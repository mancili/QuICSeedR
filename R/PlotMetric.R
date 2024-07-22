#' Create a customizable ggplot
#'
#' This function generates a ggplot object with options for boxplot and jittered points. 
#' Additional ggplot2 functions to be applied to the plot.
#'
#' @param calculation A data frame containing the data to be plotted. Output of `GetCalculation`
#' @param x Character string specifying the column name for the x-axis. Default is "content".
#' @param y Character string specifying the column name for the y-axis. Default is "RAF".
#' @param point Logical. If TRUE (default), adds jittered points to the plot.
#' @param box Logical. If TRUE (default), adds a boxplot to the plot.
#' @param base_theme A ggplot2 theme object. Default is theme_bw().
#' @param ... Additional ggplot2 functions to be applied to the plot.
#'
#' @return A ggplot object.
#'
#' @examples
#' \dontrun{
#' PlotMetric(calculation, x = "content", y = "MS",
#'            labs(title = "Max Slope by Sample"),
#'            scale_y_continuous(limits = c(0, 1600)))
#'}
#' @import ggplot2
#' @export
PlotMetric <- function(calculation, x = "content",  y = "RAF", 
                       point = TRUE, box = TRUE, 
                       base_theme = theme_bw(), ...) {
  
  p <- ggplot(calculation, aes(x = .data[[x]], y = .data[[y]]))
  if (box) {
    p <- p + geom_boxplot(aes(x = factor(.data[[x]])))
  }
  if (point) {
    p <- p + geom_jitter()
  }
  p <- p + base_theme
  p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  for (func in list(...)) {
    p <- p + func
  }
  return(p)
}