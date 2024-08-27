#' Processing and analyzing Multiple Experiments
#'
#' This function processes multiple experiments in parallel, performing a series of operations
#' including time conversion, metadata cleaning, raw data cleaning, calculations, analysis,
#' and result summarization.
#'
#' @param data Compiled data of experiments.  
#' @param do_analysis Logical. Whether statistic analysis is included. Default is `TRUE`.   
#' @param params A list of parameters for various processing steps. If a parameter is not
#'   provided, default values will be used. `control` used in `GetAnalysis()` is required.
#'   parameters can be provided for the following functions:
#'   \itemize{
#'     \item `ConvertTime`
#'     \item `CleanMeta`
#'     \item `CleanRaw`
#'     \item `GetCalculation`
#'     \item `SpreadCalculation`
#'     \item `GetAnalysis`
#'     \item `SummarizeResult`
#'   }
#' @param verbose Logical; if TRUE, prints detailed processing information for troubleshooting.
#' Default is FALSE.
#' @return A list containing three elements:
#'   \itemize{
#'     \item combined_calculation: A data frame of combined calculations from all experiments
#'     \item combined_cleanraw: Cleaned raw data for each experiment
#'     \item combined_result: A data frame of combined results from all experiments
#'   }
#' 
#' @examples
#' #Get path for example data
#' path = system.file("extdata", package = "QuICSeedR")
#' 
#' #Helper function
#' add_underscore <- function(text) {
#'   gsub("([a-zA-Z])(\\d)", "\\1_\\2", text)
#'   }
#'   
#' #Read in data
#' elkear = BulkReadMARS(path = path, 
#'                       plate_subfix = 'plate',
#'                       raw_subfix = 'raw', 
#'                       helper_func = add_underscore)
#'                       
#' #Set up parameters for batch analysis
#' params = list(
#'   CleanMeta = list(split_content = TRUE, split_into = c('region', 'sample')),
#'   GetCalculation = list(cycle_background = 5, norm = TRUE, norm_ct = 'Pos',
#'                        sd_fold = 10, time_skip = 5),
#'   GetAnalysis = list(control = "Neg")
#'   )
#'   
#' #Get results     
#' results = BulkProcessing(data =elkear, params = params)
#' 
#' str(results)
#'
#' 
#' @export
BulkProcessing = function(data, do_analysis = TRUE, params = list(), verbose = FALSE) {
  subcalculation <- list()
  subcleanraw <- list()
  subresult <- list()
  
  log <- function(...) {
    if (verbose) cat(...)
  }
  
  for (j in 1:length(data)) {
    plate <- data[[j]]$plate
    raw <- data[[j]]$raw
    replicate <- data[[j]]$replicate
    
    log("Processing plate", j, "\n")
    log("Dimensions of raw:", dim(raw), "\n")
    
    plate_time <- do.call(ConvertTime, c(list(raw), params$ConvertTime %||% list()))
    meta <- do.call(CleanMeta, c(list(raw = raw, plate = plate, 
                                      replicate = replicate), params$CleanMeta %||% list()))
    
    log("Dimensions of meta:", dim(meta), "\n")
    log("Dimensions of plate_time:", dim(plate_time), "\n")
    
    clean_raw_params <- params$CleanRaw %||% list()
    
    raw <- tryCatch({
      do.call(CleanRaw, c(list(meta = meta, raw = raw, 
                               plate_time = plate_time), clean_raw_params))
    }, error = function(e) {
      log("Error in CleanRaw for plate", j, ":", conditionMessage(e), "\n")
      return(NULL)
    })
    
    if (is.null(raw)) {
      log("Skipping further processing for plate", j, "\n")
      next
    }
    
    log("Dimensions of cleaned raw:", dim(raw), "\n")
    
    calculation <- tryCatch({
      do.call(GetCalculation, c(list(raw = raw, 
                                     meta = meta), params$GetCalculation %||% list()))
    }, error = function(e) {
      log("Error in GetCalculation for plate", j, ":", conditionMessage(e), "\n")
      return(NULL)
    })
    
    if (is.null(calculation)) {
      log("Skipping further processing for plate", j, "\n")
      next
    }
    
    if (do_analysis) {
      calculation_spread <- do.call(SpreadCalculation, 
                                    c(list(calculation), params$SpreadCalculation %||% list()))
      analysis <- do.call(GetAnalysis, c(list(calculation_spread), 
                                         params$GetAnalysis %||% list()))
    }
    
    result <- tryCatch({
      if (do_analysis) {
        do.call(SummarizeResult, c(list(analysis = analysis, 
                                        calculation = calculation), params$SummarizeResult %||% list()))
      } else {
        do.call(SummarizeResult, c(list(calculation = calculation), 
                                   params$SummarizeResult %||% list()))
      }
    }, error = function(e) {
      log("Error in SummarizeResult for plate", j, ":", conditionMessage(e), "\n")
      return(NULL)
    })
    
    if (!is.null(result)) {
      subcalculation[[j]] <- calculation
      subcleanraw[[j]] <- raw
      subresult[[j]] <- result
    }
  }
  
  subcalculation <- Filter(Negate(is.null), subcalculation)
  subcleanraw <- Filter(Negate(is.null), subcleanraw)
  subresult <- Filter(Negate(is.null), subresult)
  
  if (length(subcalculation) == 0) {
    warning("No plates were successfully processed.")
    return(NULL)
  }
  
  names(subcalculation) = names(data)[1:length(subcalculation)]
  names(subcleanraw) = names(data)[1:length(subcleanraw)]
  names(subresult) = names(data)[1:length(subresult)]
  
  subresult <- lapply(names(subresult), function(name) {
    data <- subresult[[name]]
    data$plate_name <- name
    data
  })
  fullresult = do.call(rbind, subresult)
  
  subcalculation <- lapply(names(subcalculation), function(name) {
    data <- subcalculation[[name]]
    data$plate_name <- name
    data
  })
  fullcalc = do.call(rbind, subcalculation)
  
  return(list(combined_calculation = fullcalc, combined_cleanraw = subcleanraw, 
              combined_result = fullresult))
}