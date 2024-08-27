#' Plot Time Series Data
#'
#' This function creates a faceted plot of time series data for each well in a plate layout.
#'
#' @param raw A data frame containing the raw plate data. The first row and
#'   first two columns are assumed to be metadata and are removed.
#' @param plate_time Output from `ConvertTime()`.  
#' @param format Format of plates used in the experiment. 96 or 384. 
#' @param f_size font size for subtitles.
#' @param fill Logical, whether to fill in missing wells with 0. Default is FALSE.
#'
#' @return A ggplot object representing the plate data.
#'
#' @import ggplot2
#' @importFrom dplyr %>% mutate
#' @importFrom tidyr pivot_longer
#' @importFrom rlang .data
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
#' # Ensure time displayed as decimal hours
#' plate_time = ConvertTime(raw)
#' 
#' #Plot time series data for each well in a plate layout
#' PlotPlate(raw = raw, plate_time = plate_time, fill = TRUE)
#'
#' @export
PlotPlate <- function(raw, plate_time, format = 96, f_size = 5, fill = FALSE) {
  
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
  
  if (format == 96) {
    rows <- LETTERS[1:8]
    cols <- sprintf("%02d", 1:12)
    n_row <- 8
    n_col <- 12
  } else if (format == 384) {
    rows <- LETTERS[1:16]
    cols <- sprintf("%02d", 1:24)
    n_row <- 16
    n_col <- 24
  } else {
    stop("Invalid format. Must be either 96 or 384.")
  }
  
  all_wells <- generate_wells(rows, cols)
  
  if(fill) {
    missing_wells <- setdiff(all_wells, colnames(raw)[-c(1:2)])
    for(well in missing_wells) {
      raw[[well]] <- 0
    }
  }
  
  well_order <- c(colnames(raw)[1:2], all_wells)
  raw <- raw[, well_order]
  
  rawplot <- raw[-1, -c(1:2)]
  vectors_long <- rep(colnames(rawplot), times = nrow(rawplot))
  
  rawplot <- cbind(plate_time$., rawplot)
  x <- "Time"
  colnames(rawplot)[1] <- x
  rawplot[] <- lapply(rawplot, function(x) as.numeric(as.character(x)))
  
  y <- "Value"
  long_data <- pivot_longer(rawplot, cols = -.data$Time, names_to = "Variable", values_to = y)
  long_data$Variable <- vectors_long
  long_data$Value <- as.numeric(long_data$Value)
  
  
  global_max <- max(long_data$Value, na.rm = TRUE) / 0.8
  
  base_plot <- ggplot(long_data, aes(x = .data[[x]], y = .data[[y]])) +
    geom_line() +
    facet_wrap(~.data$Variable, scales = "free", nrow = n_row, ncol = n_col) +
    labs(y = "Fluorescence") +
    theme(
      strip.background = element_blank(),
      strip.text = element_text(size = f_size, face = "bold", color = "black"),
      panel.background = element_rect(fill = "white", colour = "gray"),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    ) +
    ylim(c(0, global_max))
  
  return(base_plot)
}


