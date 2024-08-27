#' Plot a Single Sample from the Cleaned Raw Data
#
#' @description The function plots fluorescence for selected sample over time.
#
#' @param raw Cleaned raw data. Output from `GetCleanRaw`
#' @param sample The name of the sample to plot.
#' @param legend_position Position of legend. Default is "topleft".
#' @param ylim Numeric vector giving the y coordinate range (relative fluorescence units). If NULL (default), limits are computed from the data.
#' @param xlim Numeric vector giving the x coordinate range (hours). If NULL (default), limits are computed from the data.
#' Choose from "bottomright", "bottom", "bottomleft", "left", "topleft", "top", "topright", "right" and "center".
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
#' @return A plot displaying the fluorescence of the selected sample over time. 
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
#' #Plot fluorescence curves from positive controls
#' PlotRawSingle(cleanraw, "Pos")
#' 
#' @export
#' 
PlotRawSingle <- function(raw, sample, legend_position = "topleft", 
                          xlim = NULL, ylim = NULL, custom_colors = NULL,
                          xlab = "Time (h)", ylab = "Fluorescence",
                          linetypes = NULL) {
  
  time <- as.numeric(rownames(raw))
  sel <- grep(paste0('^', sample), colnames(raw))
  
  if (is.null(ylim)) {
    ymax <- max(raw[, sel], na.rm = TRUE)
    ylim <- c(0, ymax/0.8)
  }
  
  if (is.null(xlim)) {
    xlim <- range(time, na.rm = TRUE)
  }
  
  if (is.null(custom_colors)) {
    colors_to_use <- seq_len(length(sel))
  } else {
    colors_to_use <- rep_len(custom_colors, length(sel))
  }
  
  if (is.null(linetypes)) {
    linetypes <- rep(1, length(sel)) 
  } else {
    linetypes <- rep_len(linetypes, length(sel))
  }
  
  not_na_indices <- !is.na(raw[, sel[1]])
  plot(x = time[not_na_indices], y = raw[not_na_indices, sel[1]], type = "l",
       col = colors_to_use[1], lty = linetypes[1], ylim = ylim, xlim = xlim, xlab = xlab, ylab = ylab)
  
  for (i in 2:length(sel)) {
    not_na_indices <- !is.na(raw[, sel[i]])
    lines(x = time[not_na_indices], y = raw[not_na_indices, sel[i]], 
          type = "l", col = colors_to_use[i], lty = linetypes[i])
  }
  
  legend(legend_position, legend = seq_len(length(sel)),
         col = colors_to_use, lty = linetypes, cex = 0.8)
}
