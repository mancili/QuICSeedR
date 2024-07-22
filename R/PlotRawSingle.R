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
#' @importFrom graphics plot legend lines
#' @examples
#' \dontrun{
#' PlotRawSingle(raw = raw, sample = sample)
#' }
#' @export
PlotRawSingle <- function(raw, sample, legend_position = "bottomright", ylim = NULL, xlim = NULL) {
  time <- as.numeric(rownames(raw))
  sel <- grep(paste0('^', sample), colnames(raw))
  
  if (is.null(ylim)) {
    ymax <- max(raw[, sel], na.rm = TRUE)
    ylim <- c(0, ymax/0.8)
  }
  
  if (is.null(xlim)) {
    xlim <- range(time, na.rm = TRUE)
  }
  
  not_na_indices <- !is.na(raw[, sel[1]])
  plot(x = time[not_na_indices], y = raw[not_na_indices, sel[1]], type = "l",
       col = 1, ylim = ylim, xlim = xlim, xlab = "Time (h)", ylab = "Fluorescence")
  
  for (i in 2:length(sel)) {
    not_na_indices <- !is.na(raw[, sel[i]])
    lines(x = time[not_na_indices], y = raw[not_na_indices, sel[i]], type = "l", col = i)
  }
  
  legend(legend_position, legend = seq_len(length(sel)),
         col = seq_len(length(sel)), lty = 1, cex = 0.8)
}