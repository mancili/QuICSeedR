#' Create a customizable ggplot for metrics
#'
#' This is wrapper function to generate a ggplot object with default options for boxplot and jittered points. 
#' Additional ggplot2 functions can be applied to the plot.
#'
#' @param calculation A data frame containing the data to be plotted. Output of `GetCalculation`
#' @param x Character string specifying the column name for the x-axis. Default is "content".
#' @param y Character string specifying the column name for the y-axis. Default is "RAF".
#' @param fill_var The name of the column to be used for fill color, or a vector of colors.
#'   If NULL, no fill color is applied. Default is NULL.
#' @param point Logical. If TRUE (default), adds jittered points to the plot.
#' @param box Logical. If TRUE (default), adds a boxplot to the plot.
#' @param base_theme A ggplot2 theme object. Default is theme_bw().
#' @param ... Additional ggplot2 functions to be applied to the plot.
#'
#' @return A ggplot object.
#'
#' @examples
#' if (interactive()) {
#' PlotMetric(calculation_96, y = "MS", point = FALSE, box = FALSE,
#'            boxplot= geom_boxplot(color = 'gray'),
#'            scatter = geom_point(color = "blue") ,
#'            xlab = xlab("Sample ID"),
#'            ylab =ylab("Normalized Max Slope"))
#'}
#' @import ggplot2
#' @export
PlotMetric <- function(calculation, x = "content", y = "RAF", fill_var = NULL,
                       point = TRUE, box = TRUE, 
                       base_theme = theme_bw(), ...) {
  
  p <- ggplot(calculation, aes(x = .data[[x]], y = .data[[y]]))
  
  if (!is.null(fill_var)) {
    if (is.character(fill_var) && length(fill_var) == 1 && fill_var %in% names(calculation)) {
      p <- p + aes(fill = .data[[fill_var]])
    } else if (is.character(fill_var)) {
      if (box) {
        p <- p + aes(fill = factor(.data[[x]])) +
          scale_fill_manual(values = fill_var)
      } else {
        p <- p + aes(color = factor(.data[[x]])) +
          scale_color_manual(values = fill_var)
      }
    }
  }
  
  if (box) {
    p <- p + geom_boxplot(aes(x = factor(.data[[x]])))
  }
  if (point) {
    p <- p + geom_jitter(width = 0.2, height = 0)
  }
  
  p <- p + base_theme +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  for (func in list(...)) {
    p <- p + func
  }
  
  return(p)
}