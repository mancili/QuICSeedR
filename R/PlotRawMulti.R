#' Plot Multiple Samples from the Cleaned Raw Data
#'
#' @description The function visualizes the fluorescence over time for selected samples.
#' 
#' @param raw Cleaned raw data. Output from `GetCleanRaw`.
#' @param samples The names of the samples to plot.
#' @param legend_position Position of legend. Default is "topleft".
#' Choose from "bottomright", "bottom", "bottomleft", "left", "topleft", "top", "topright", "right" and "center".
#' @param ylim Numeric vector giving the y coordinate range (relative fluorescence units). If NULL (default), limits are computed from the data.
#' @param xlim Numeric vector giving the x coordinate range (hours). If NULL (default), limits are computed from the data.
#' @param custom_colors An optional vector of colors to be used for plotting. 
#'        If NULL (default), the function will use the original color scheme.
#' @param xlab Label for x-axis.
#' @param ylab Label for y-axis.
#' @param linetypes Vector of line types to use for each line. Can be integer codes (1:6) 
#'        or character codes ("solid", "dashed", "dotted", "dotdash", "longdash", "twodash").
#'        All lines will be solid by default.
#'   
#' @importFrom graphics plot legend lines
#' 
#' @return A plot displaying the fluorescence of the selected samples over time. 
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
#' cleanraw <- CleanRaw(meta, raw, plate_time)
#' 
#' #Plot fluorescence curves from negative and positive controls
#' PlotRawMulti(cleanraw, c("Neg", "Pos"))
#' 
#' @export
PlotRawMulti <- function(raw, samples, legend_position = "topleft", 
                         xlim = NULL, ylim = NULL, custom_colors = NULL,
                         xlab = "Time (h)", ylab = "Fluorescence",
                         linetypes = NULL) {
  
  time <- as.numeric(rownames(raw))
  
  if (is.null(ylim)) {
    ymax <- max(raw, na.rm = TRUE)
    ylim <- c(0, ymax/0.8)
  }
  
  if (is.null(xlim)) {
    xlim <- range(time, na.rm = TRUE)
  }
  
  if (is.null(custom_colors)) {
    colors_to_use <- seq_len(length(samples))
  } else {
    colors_to_use <- rep_len(custom_colors, length(samples))
  }
  
  if (is.null(linetypes)) {
    linetypes <- rep(1, length(samples))
  } else {
    linetypes <- rep_len(linetypes, length(samples))
  }
  
  plot(NULL, xlim = xlim, ylim = ylim, xlab = xlab, ylab = ylab)
  
  for (i in seq_along(samples)) {
    sel <- grep(paste0('^', samples[i]), colnames(raw))
    for (j in sel) {
      not_na_indices <- !is.na(raw[, j])
      lines(x = time[not_na_indices], y = raw[not_na_indices, j], 
            type = "l", col = colors_to_use[i], lty = linetypes[i])
    }
  }
  
  legend(legend_position, legend = samples,
         col = colors_to_use, lty = linetypes, cex = 0.8)
}