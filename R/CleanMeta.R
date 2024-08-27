#' Get Clean Metadata
#'
#' This function processes raw data (raw), plate layout (plate), and replicate information (replicate) to create a clean metadata dataframe.
#' It can optionally split the content column into additional columns.
#'
#' @param raw A dataframe containing the raw data.
#' @param plate A dataframe containing the plate layout information.
#' @param replicate A dataframe containing the replicate information. Output of `GetReplicate()`.
#' @param split_content Logical, whether to split the content. Default is FALSE.
#' @param split_by A character string to split the content by. Default is "_".
#' @param split_into A character vector specifying names for the split columns. Required if split_content is TRUE.
#' @param del_na Logical, whether to drop rows containing NA. Default is TRUE.
#' @return A data frame containing the cleaned metadata.
#'
#' @examples
#' # Define the path to the plate data file
#' plate_path <- system.file("extdata/20240716_p3", 
#'                           file = '20240716_p3_plate.xlsx', 
#'                           package = "QuICSeedR")
#'   
#' # Read the plate data
#' plate <- readxl::read_xlsx(plate_path)
#' 
#' # Define the path to the raw data file
#' raw_path <- system.file("extdata/20240716_p3", 
#'                         file = '20240716_p3_raw.xlsx', 
#'                         package = "QuICSeedR")
#' # Read the raw data
#' raw <- readxl::read_xlsx(raw_path)
#' 
#' # Get replicate data
#' replicate <- GetReplicate(plate)
#' 
#' #Get metadata and display the few rows 
#' meta = CleanMeta(raw, plate, replicate)
#' head(meta)
#'
#' @export
CleanMeta <- function (raw, plate, replicate, split_content = FALSE, split_by = "_", split_into = c(),
                       del_na = TRUE) {
  
  if (split_content && length(split_into) == 0) {
    stop("If split_content is TRUE, split_into must be provided and cannot be empty.")
  }
  
  n_platecol <- ifelse(ncol(plate) == 13, 13, 25)
  plate_format <- ifelse(n_platecol == 13, 96, 384)
  
  replicate <- replicate[, -1]
  replicate <- c(t(replicate))
  
  generate_wells <- function(rows, cols) {
    wells <- character(length(rows) * length(cols))
    index <- 1
    for (r in rows) {
      for (c in cols) {
        wells[index] <- paste0(r, c)
        index <- index + 1
      }
    }
    return(wells)
  }
  
  if (plate_format == 96) {
    rows <- LETTERS[1:8]
    cols <- sprintf("%02d", 1:12)
    n_row <- 8
    n_col <- 12
  } else if (plate_format == 384) {
    rows <- LETTERS[1:16]
    cols <- sprintf("%02d", 1:24)
    n_row <- 16
    n_col <- 24
  } else {
    stop("Invalid format. Must be either 96 or 384.")
  }
  
  well <- generate_wells(rows, cols)  
  
  content <- plate[, 2:n_platecol]
  content <- c(t(content))
  
  if (del_na) {
    valid_well <- which(!is.na(replicate))
    well <- well[valid_well]
    content <- content[valid_well]
    replicate <- replicate[valid_well]
  }
  
  content_replicate <- paste(content, replicate, sep = "_")
  content_replicate = gsub ("NA_NA", NA, content_replicate)
  
  meta <- data.frame(
    well = well,
    content = content,
    replicate = replicate,
    content_replicate = content_replicate,
    format = plate_format,
    stringsAsFactors = FALSE
  )
  
  if (split_content) {
    split_df <- do.call(rbind, strsplit(as.character(meta$content), split_by))
    
    if (ncol(split_df) != length(split_into)) {
      stop(paste("Number of split columns (", ncol(split_df), 
                 ") does not match the length of 'split_into' (", 
                 length(split_into), ").", sep=""))
      colnames(split_df) <- paste0("split_", seq_len(ncol(split_df)))
    }
    
    colnames(split_df) <- split_into
    meta <- cbind(meta, split_df)
  }
  
  return(meta)
}


