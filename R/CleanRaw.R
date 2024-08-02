#' Generate Clean Raw Data
#'
#' @description This function takes metadata, raw data, and total cycle information to generate clean raw fluorescence data.
#'
#' @param meta A data frame containing the metadata. Output from CleanMeta function.
#' @param raw Raw fluorescence readings from MARS software.
#' @param plate_time Output of `ConvertTime()`.
#' @param cycle_total The total number of cycles (rows) to include in the output.
#' @return A data frame containing the cleaned raw fluorescence data. 
#' @examples
#' \dontrun{
#' meta <- data.frame(content_replicate = c("A1", "A2", "A3"))
#' raw <- data.frame(
#'   X1 = c("Header", "1", "2", "3"),
#'   X2 = c("Time", "0", "1", "2"),
#'   A1 = c("Value", "100", "110", "120"),
#'   A2 = c("Value", "200", "210", "220"),
#'   A3 = c("Value", "300", "310", "320")
#' )
#' plate_time <- c(0, 1, 2)
#' 
#' cleaned_data <- CleanRaw(meta, raw, plate_time)
#' print(cleaned_data)
#' }
#' @export
CleanRaw = function (meta, raw, plate_time, cycle_total) 
{
  if (missing(cycle_total) || is.null(cycle_total) || length(cycle_total) == 
      0) {
    cycle_total = nrow(raw) - 1
  }
  else {
    cycle_total <- cycle_total
  }
  raw = raw[-1, -c(1:2)]
  raw = raw[1:cycle_total, meta$well]
  raw = as.numeric(unlist(raw))
  raw = matrix(raw, nrow = cycle_total)
  rownames(raw) = unlist(plate_time[1:cycle_total, ])
  colnames(raw) = meta$content_replicate
  return(raw)
}