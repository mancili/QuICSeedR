#' Bulk Read MARS data
#'
#' @param path Character string specifying the path to the parent directory containing the data folders.
#' @param plate_subfix Character string used to identify plate data files.
#' @param raw_subfix Character string used to identify raw data files.
#' @param helper_func Optional function to be applied to each column of the plate data (default: NULL).
#'
#' @return A list containing data from each folder, including plate, raw, and replicate data.
#' 
#' @import readxl
#' @examples
#' \dontrun{
#' grinder_data <- ReadMARS(path = './data/grinder/',
#'                          plate_subfix = 'plate',
#'                          raw_subfix = 'raw',
#'                          helper_func = flip_and_replace)
#' }
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
