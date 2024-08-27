#' Generate Clean Raw Data
#'
#' @description This function takes metadata, raw data, and total cycle information to generate clean raw fluorescence data.
#'
#' @param meta A data frame containing the metadata. Output from CleanMeta function.
#' @param raw Raw fluorescence readings from MARS software.
#' @param plate_time Output of `ConvertTime()`.
#' @param cycle_total The total number of cycles (rows) to include in the output.
#' 
#' @return A data frame containing the cleaned raw fluorescence data. 
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
#' # Ensure time displayed as decimal hours
#' plate_time = ConvertTime(raw)
#' 
#' #Get metadata and display the few rows 
#' meta = CleanMeta(raw, plate, replicate)
#' 
#' #Clean data 
#' cleaned_data <- CleanRaw(meta, raw, plate_time)
#' 
#' cleaned_data[1:5, 1:5]
#' 
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