#' Extract and Convert Time Data to Decimal Hours
#'
#' The function extracts and converts run time information from MARS output.
#'
#' @param raw Output from MARS.
#'
#' @return A data frame containing the time information in decimal hours.
#'
#' @examples
#' # Example with "hours and minutes" format
#' raw_data <- data.frame(
#'   V1 = c("Header", "Row1", "Row2"),
#'   V2 = c("Time", "1 h 30 min", "2 h 45 min")
#' )
#' plate_time <- ConvertTime(raw_data)
#' print(plate_time)
#'
#' # Example with decimal hours format
#' raw_data2 <- data.frame(
#'   V1 = c("Header", "Row1", "Row2"),
#'   V2 = c("Time", "1.5", "2.75")
#' )
#' plate_time2 <- ConvertTime(raw_data2)
#' print(plate_time2)
#' 
#' @importFrom magrittr %>%
#' @export
ConvertTime = function (raw) {
  time = c(raw[-1, 2])
  time = unlist(time) %>% data.frame(stringsAsFactors = FALSE)
  if (grepl("min", time) == TRUE) {
    hours <- as.numeric(gsub(" h.*$", "", time$.))
    suppressWarnings({
      minutes <- ifelse(grepl("min", time$.), as.numeric(gsub("^.*?([0-9]+) min$", 
                                                              "\\1", time$.)), 0)
    })
    decimal_hours <- hours + (minutes/60)
    time = (data.frame(. = decimal_hours))
  }
  else {
    time$. = as.numeric(as.character(time$.))
  }
  return(time)
}
