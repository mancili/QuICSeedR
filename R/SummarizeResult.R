#' Summarize Analysis Results
#'
#' This function combines analysis results from multiple tests with metadata,
#' and determines overall significance based on a specified method. By default, it 
#' evaluates sample-level result by calculating the percentage of technical replicates 
#' that exceed the pre-defined threshold.
#' 
#' @param analysis Output of `GetAnalysis()`.
#' @param calculation Output of `GetCalculation()`.
#' @param sig_method Specifies the approach for determining sample-level result.
#'        Available options include "xth_percent", "metric_count", "xth_count", 
#'        or any metric name present in the analysis list. The default is "xth_percent".
#' @param method_threshold Defines the threshold value for the "metric_count", 
#'        "xth_count", and "xth_percent" methods. This parameter defaults to 50.
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
#' calculation = GetCalculation(raw = cleanraw, meta, sd_fold = 10)
#' 
#' #Formatting calculations for analysis (also compatible with graphing softwares used in F-SAA 
#' #research)
#' calculation_spread = SpreadCalculation(calculation)
#' 
#' #Get analysis comparing samples to negative control using the one-tailed Wilcox Rank-Sum test.
#' analysis <- GetAnalysis(calculation_spread, control = "Neg", test = "wilcox", 
#'                         alternative = 'greater')
#'                         
#' #Summarization of results. Default method is rate of amyloid formation.
#' result <- SummarizeResult(analysis, calculation)
#' 
#' head(result)
#'
#' @export
SummarizeResult <- function(analysis = NULL, calculation, sig_method = "xth_percent", method_threshold = 50) {
  if (!is.data.frame(calculation) || !"content" %in% names(calculation))
    stop("'calculation' must be a data frame with a 'content' column")
  
  unique_content <- unique(calculation$content)
  result <- data.frame(content = unique_content, result = "", stringsAsFactors = FALSE)
  result$method = sig_method
  
  sub = calculation[, 1:3]
  sub = reshape(sub, direction = "wide", idvar = "content", timevar = "replicate")
  position = sub %>% 
    select(.data[['content']], starts_with("well.")) %>%
    pivot_longer(cols = starts_with("well."), names_to = "well_column", values_to = "well_value") %>% 
    filter(!is.na(.data[['well_value']])) %>%
    group_by(.data[['content']]) %>% 
    summarise(wells = paste(sort(.data[['well_value']]), collapse = "-")) %>% 
    ungroup()
  result$position <- position$wells[match(result$content, position$content)]
  
  if (!is.null(analysis) && is.list(analysis) && length(analysis) > 0) {
    valid_sig_methods <- c("metric_count", "xth_count", "xth_percent", names(analysis))
    if (!sig_method %in% valid_sig_methods) {
      stop(paste("Invalid sig_method. Must be one of:", paste(valid_sig_methods, collapse = ", ")))
    }
    
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
    
    if (sig_method == "metric_count") {
      result$result[result$metric_count >= method_threshold] <- "*"
    }
    else if (sig_method %in% c("MS", "MPR", "RAF")) {
      sig_column <- paste0(sig_method, "_sig")
      if (sig_column %in% names(result)) {
        result$result[grepl("\\*", result[[sig_column]])] <- "*"
      }
      else {
        warning(paste("Column", sig_column, "not found in results. No overall result calculated."))
      }
    }
  } else if (!is.null(analysis)) {
    warning("'analysis' is empty or not a list. Metric, metric count, and metric p-value columns will not be included.")
  }
  
  xth_count <- aggregate(XTH ~ content, data = calculation, FUN = sum)
  result$xth_count <- xth_count$XTH[match(result$content, xth_count$content)]
  total_rep = calculation %>% 
    select(.data[['content']], replicate) %>%
    group_by(.data[['content']]) %>% 
    summarise(max_rep = max(replicate, na.rm = TRUE))
  result$total_rep <- total_rep$max_rep[match(result$content, total_rep$content)]
  result$xth_percent = round(result$xth_count/result$total_rep * 100, 2)
  
  if (sig_method == "xth_count") {
    result$result[result$xth_count >= method_threshold] <- "*"
  }
  else if (sig_method == "xth_percent") {
    result$result[result$xth_percent >= method_threshold] <- "*"
  }
  
  return(result)
}