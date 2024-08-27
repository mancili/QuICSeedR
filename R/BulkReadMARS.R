#' Bulk Read MARS data
#'
#' @param path Character string specifying the path to the parent directory containing the data folders.
#' @param plate_subfix Character string used to identify plate data files.
#' @param raw_subfix Character string used to identify raw data files.
#' @param helper_func Optional function to be applied to each column of the plate data (default: NULL).
#'
#' @details
#' The example dataset, located in `inst/extdata/`, consists of folders representing
#' individual experimental runs. This dataset is a subset of the data used in a
#' publication led by Dr. Stuart Lichtenberg. For detailed information about the experiments, please 
#' contact Dr. Stuart Lichtenberg at `licht213@umn.edu`.
#' 
#' Each folder contains two Excel files:
#' \itemize{
#'   \item A file with the suffix `plate`: Contains the plate setup and experimental information.
#'   \item A file with the suffix `raw`: Contains fluorescence data exported from MARS software.
#' }
#'
#' @return A list containing data from each folder, including plate, raw, and replicate data.
#' 
#' @import readxl
#' @examples
#' path = system.file("extdata", package = "QuICSeedR")
#' data <- BulkReadMARS(path = path,
#'                      plate_subfix = 'plate',
#'                      raw_subfix = 'raw')
#' str(data)
#' 
#' @export

BulkReadMARS <- function(path, plate_subfix, raw_subfix, helper_func = NULL) {
  
  folders <- list.dirs(path = path, recursive = FALSE)
  
  mylist <- vector(mode = 'list', length = length(folders))
  
  listnames <- basename(folders)  
  names(mylist) <- listnames
  
  for (i in seq_along(folders)) { 
    folder <- folders[i]
    
    files <- list.files(path = folder, pattern = "\\.xlsx$", full.names = TRUE)
    
    plate_path <- files[grepl(plate_subfix, files, fixed = TRUE)]
    raw_path <- files[grepl(raw_subfix, files, fixed = TRUE)]
    
    if (length(plate_path) == 0 || length(raw_path) == 0) {
      warning(paste("Skipping folder", folder, "due to missing files."))
      next
    }
    
    plate_data <- read_xlsx(plate_path)
    raw_data <- read_xlsx(raw_path)
    replicate_data <- GetReplicate(plate_data)
    
    mylist[[i]] <- list(
      plate = if (is.null(helper_func)) plate_data else data.frame(lapply(plate_data, helper_func)),
      raw = raw_data,
      replicate = replicate_data
    )
  }
  mylist <- Filter(Negate(is.null), mylist)
  
  return(mylist)
}
