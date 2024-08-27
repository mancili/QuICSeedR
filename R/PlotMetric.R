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
#' #Get calculations using positive controls to normalize values. 
#' calculation = GetCalculation(raw = cleanraw, meta, norm = TRUE, norm_ct = 'Pos')
#' 
#' #Default plot
#' PlotMetric(calculation)
#'            
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
    p <- p + geom_boxplot(aes(x = factor(.data[[x]])), 
                          outlier.shape = NA)
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