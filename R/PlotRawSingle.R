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
#' @param linetype The type parameter in plotting functions accepts various values to control the appearance of the plot: "p" draws only points, 
#' "l" creates lines, "b" combines both points and lines, "c" displays empty points joined by lines, "o" overplots points and lines, "s" and "S" create stair steps,
#'  "h" produces histogram-like vertical lines, and "n" sets up the plot without drawing any points or lines. For more detailed information on these options, 
#'  refer to the documentation of the plot function.       
#' @importFrom graphics plot legend lines
#' @references 
#' Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988) The New S Language. Wadsworth & Brooks/Cole.
#' @examples
#' if (interactive()) {
#' PlotRawSingle(raw = raw, sample = sample)
#' }
#' 
#' @export
#' 
PlotRawSingle <- function(raw, sample, legend_position = "bottomright", 
                          xlim = NULL, ylim = NULL, custom_colors = NULL,
                          xlab = "Time (h)", ylab = "Fluorescence",
                          linetype = "l") {
  
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
  
  not_na_indices <- !is.na(raw[, sel[1]])
  plot(x = time[not_na_indices], y = raw[not_na_indices, sel[1]], type = linetype,
       col = colors_to_use[1], ylim = ylim, xlim = xlim, xlab = xlab, ylab = ylab)
  
  for (i in 2:length(sel)) {
    not_na_indices <- !is.na(raw[, sel[i]])
    lines(x = time[not_na_indices], y = raw[not_na_indices, sel[i]], type = linetype, col = colors_to_use[i])
  }
  
  legend(legend_position, legend = seq_len(length(sel)),
         col = colors_to_use, lty = 1, cex = 0.8)
}
