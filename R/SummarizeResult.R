#' Summarize Analysis Results
#'
#' This function combines analysis results from multiple tests with metadata,
#' and determines overall significance based on a specified method.
#'
#' @param analysis Output of `GetAnalysis()`.
#' @param calculation Output of `GetCalculation()`.
#' @param sig_method Method to determine overall significance. 
#'        Options are "metric_count", "xth_count", or any of the names in the analysis list. Default is "metric_count".
#' @param method_threshold Threshold for count when using "metric_count" and "xth_count" method. Default is 3.
#' 
#' @importFrom stats aggregate reshape
#' @importFrom dplyr select starts_with filter group_by summarise ungroup
#' @importFrom tidyr pivot_longer
#' @importFrom rlang .data
#' 
#' @return A data frame summarizing results of the analysis and calculation, with columns:
#'
#' \itemize{
#'   \item result: Overall result based on the chosen method
#'
#'   \item position: Location of the sample replications based on plate layout
#'
#'   \item method: Method chosen for determination of the result
#'
#'   \item *_sig: Significance indicator for each metric
#'
#'   \item *_p: P-value or adjusted p-value for each metric
#'
#'   \item metric_count: Count of significant results across all analyses
#'
#'   \item xth_count: The number of reactions crossing the fluorescent threshold
#'
#'   \item total_rep: Total number of replicates
#'
#'   \item xth_percent: Percentage of replicates crossing the fluorescent threshold
#' }
#' 
#' @examples
#' \dontrun{
#' result <- SummarizeResult(analysis, calculation)
#' result_mpr <- SummarizeResult(analysis, calculation, sig_method = "MPR")
#' }
#'
#' @export
SummarizeResult <- function(analysis, calculation, sig_method = "metric_count", method_threshold = 3) {
  
  if (!is.list(analysis) || length(analysis) == 0) stop("'analysis' must be a non-empty list")
  if (!is.data.frame(calculation) || !"content" %in% names(calculation)) stop("'calculation' must be a data frame with a 'content' column")
  
  valid_sig_methods <- c("metric_count", "xth_count", "xth_percent", names(analysis))
  if (!sig_method %in% valid_sig_methods) {
    stop(paste("Invalid sig_method. Must be one of:", paste(valid_sig_methods, collapse = ", ")))
  }
  
  unique_content <- unique(calculation$content)
  result <- data.frame(content = unique_content, result = "", stringsAsFactors = FALSE)
  result$method = sig_method
  
  sub = calculation[,1:3]
  sub  = reshape(sub, direction = "wide", idvar = 'content', timevar = "replicate")
  position = sub %>%
    select(.data$content, starts_with("well.")) %>%
    pivot_longer(cols = starts_with("well."), 
                 names_to = "well_column", 
                 values_to = "well_value") %>%
    filter(!is.na(.data$well_value)) %>%
    group_by(.data$content) %>%
    summarise(wells = paste(sort(.data$well_value), collapse = "-")) %>%
    ungroup()
  result$position <- position$wells[match(result$content, position$content)]
  
  for (stat_name in names(analysis)) {
    stat <- analysis[[stat_name]]
    if (!all(c("significant", "p_value") %in% names(stat))) {
      warning(paste("Skipping", stat_name, "due to missing 'significant' or 'p_value' column"))
      next
    }
    
    content_col <- if ("content" %in% names(stat)) stat$content else rownames(stat)
    
    result[[paste0(stat_name, "_sig")]] <- stat$significant[match(result$content, content_col)]
    result[[paste0(stat_name, "_p")]] <- stat$adj_p[match(result$content, content_col)]
    if (is.null(result[[paste0(stat_name, "_p")]])) {
      result[[paste0(stat_name, "_p")]] <- stat$p_value[match(result$content, content_col)]
    }
  }
  
  sig_columns <- grep("_sig$", names(result), value = TRUE)
  result$metric_count <- rowSums(sapply(result[sig_columns], function(x) grepl("\\*", x)))
  
  xth_count <- aggregate(XTH ~ content, data = calculation, FUN = sum)
  result$xth_count <- xth_count$XTH[match(result$content, xth_count$content)]
  
  total_rep = calculation %>% 
    select(.data$content, replicate) %>% 
    group_by(.data$content) %>% 
    summarise(max_rep = max(replicate, na.rm = TRUE))
  result$total_rep <- total_rep$max_rep[match(result$content, total_rep$content)]
  
  result$xth_percent = round(result$xth_count/result$total_rep*100, 2)
  
  if (sig_method == "metric_count") {
    result$result[result$metric_count >= method_threshold] <- "*"
  } else if (sig_method == "xth_count") {
    result$result[result$xth_count >= method_threshold] <- "*"
  } else if (sig_method == "xth_percent") {
    result$result[result$xth_percent >= method_threshold] <- "*"
  } else if (sig_method %in% c("MS", "MPR", "RAF")) {
    sig_column <- paste0(sig_method, "_sig")
    if (sig_column %in% names(result)) {
      result$result[grepl("\\*", result[[sig_column]])] <- "*"
    } else {
      warning(paste("Column", sig_column, "not found in results. No overall result calculated."))
    }
  }
  
  return(result)
}