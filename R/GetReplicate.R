#' Generate Replicate Numbers for Plate Data
#'
#' This function takes a plate layout and generates a corresponding matrix of
#' replicate numbers for each sample.
#'
#' @param plate 
#'   A matrix or data frame representing the plate layout, where each
#'   cell contains a sample identifier or NA for empty wells.
#'
#' @return A data frame with the same dimensions as the input plate, where each
#'   cell contains the replicate number for the corresponding sample in the
#'   input plate.
#'
#' @note
#' - Sample identifiers are converted to character type before processing.
#' - The function assumes that the input plate is organized such that replicate
#'   samples are encountered sequentially.
#' - The output maintains the column names from the input plate.
#'
#' @examples
#' plate <- matrix(
#'   c("A", "B", "C",
#'     "A", "B", NA,
#'     "A", "C", "D"),
#'   nrow = 3, byrow = TRUE
#' )
#' 
#' replicates <- GetReplicate(plate)
#' print(replicates)
#' 
#' 
#' @export
GetReplicate <- function(plate) {
  replicate_counts <- c()
  
  replicate_data <- matrix(nrow = nrow(plate), ncol = ncol(plate))
  
  for (col in 1:ncol(plate)) {
    for (row in 1:nrow(plate)) {
      sample_id <- plate[row, col]
      if (is.na(sample_id)) {
        replicate_data[row, col] <- NA
      } else {
        sample_id <- as.character(sample_id)
        if (!sample_id %in% names(replicate_counts)) {
          replicate_counts[sample_id] <- 1
        } else {
          replicate_counts[sample_id] <- replicate_counts[sample_id] + 1
        }
        replicate_data[row, col] <- replicate_counts[sample_id]
      }
    }
  }
  
  replicate_data_df <- as.data.frame(replicate_data)
  colnames(replicate_data_df) <- colnames(plate)
  return(replicate_data_df)
}
