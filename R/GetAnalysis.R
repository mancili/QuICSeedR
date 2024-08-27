#' Perform Statistical Analysis on Calculations
#'
#' This function performs statistical analysis on a list of calculation spreads,
#' comparing each column to a control column using various statistical tests.
#'
#' @param calculation_spread 
#'        A list of data frames, each representing a calculation spread. 
#'        
#' Output of `SpreadCaculation()`.
#' @param control The name or pattern of the control column in each data frame.
#' @param test The statistical test to use. Options are:
#'   \itemize{
#'     \item "t-test": Student's t-test, suitable for normally distributed data. 
#'            For more information, run: \code{?stats::t.test}
#'     \item "wilcox": Wilcoxon rank-sum test (also known as Mann-Whitney U test), a non-parametric test. 
#'            For more information, run: \code{?stats::wilcox.test}
#'     \item "yuen": Yuen's test for trimmed means, robust against outliers and non-normality. 
#'            For more information, run: \code{?WRS2::yuen}
#'   }
#'   Default is "wilcox".
#' @param alternative Options are "two.sided", "less", or "greater". Default is "two.sided".
#' @param adjust_p Logical. Whether to adjust p-values for multiple comparisons. Default is FALSE.
#' @param alpha The significance level for determining significance stars. Default is 0.05.
#'
#' @return A list of data frames containing the results of the statistical analysis for selected metrics.
#' @references 
#' Mair, P., & Wilcox, R. (2020). WRS2: A Collection of Robust Statistical Methods.
#' https://CRAN.R-project.org/package=WRS2
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
#' #Formatting calculations for analysis (also compatible with graphing 
#' #softwares used in F-SAA research)
#' calculation_spread = SpreadCalculation(calculation)
#' 
#' analysis <- GetAnalysis(calculation_spread, control = "Neg", test = "wilcox", 
#'                         alternative = 'greater')
#' head(analysis)
#'
#' @importFrom stats t.test wilcox.test p.adjust na.omit
#' @importFrom WRS2 yuen
#' @importFrom tidyr pivot_longer
#' @importFrom tidyselect everything
#' @importFrom methods is
#' 
#' @export
GetAnalysis <- function(calculation_spread, control, test = 'wilcox', alternative = 'two.sided', adjust_p = FALSE, alpha = 0.05) {
  if ("time_to_threshold" %in% names(calculation_spread)) {
    calculation_spread$time_to_threshold[is.na(calculation_spread$time_to_threshold)] <- 0
  }
  
  err.t.test <- function(x, y, ...) {
    x <- na.omit(x)
    y <- na.omit(y)
    obj <- try(t.test(x, y, ...), silent=TRUE)
    if (is(obj, "try-error")) return(NA) else return(obj)
  }
  
  yuen_wrapper <- function(x, y, alternative = alternative) {
    
    data <- data.frame(group1 = x, group2 = y)
    
    long_data <- pivot_longer(data, cols = everything(), names_to = "group", values_to = "value")
    
    result <- tryCatch({
      res <- WRS2::yuen(value ~ group, data = long_data, tr = 0.1)
      
      if (alternative == "greater" && res$test > 0) {
        res$p.value <- res$p.value / 2
      } else if (alternative == "less" && res$test < 0) {
        res$p.value <- res$p.value / 2
      }
      
      return(res)
      
    }, error = function(e) {
      if(grepl("missing value where TRUE/FALSE needed", e$message)) {
        return(NULL)
      } else {
        return(paste("Error:", e$message))
      }
    })
    
    return(result)
  }
  
  analysis <- vector(mode = 'list', length = length(calculation_spread))
  
  if (test == "t-test") {
    test_fun <- function(x, y) err.t.test(x, y, alternative = alternative)
  } else if (test == "wilcox") {
    test_fun <- function(x, y) wilcox.test(x, y, alternative = alternative)
  } else if (test == "yuen") {
    test_fun <- function(x, y) yuen_wrapper(x, y, alternative = alternative)
  } else {
    stop("Invalid test specified")
  }
  
  for (j in 1:length(calculation_spread)) {
    data <- calculation_spread[[j]]
    
    ct_sel <- grep(control, colnames(data))
    stat.res <- data.frame(matrix(NA, ncol = 4, nrow = ncol(data)))
    colnames(stat.res) <- c("statistic", "p_value", "adj_p","significant")
    
    for (i in 1:ncol(data)) {
      t.res <- test_fun(data[, i], data[, ct_sel])
      
      if (is.null(t.res)) {
        next
      }
      
      if (is.atomic(t.res)) {
        p_value <- t.res
        statistic <- NA
      } else {
        p_value <- t.res$p.value
        statistic <- ifelse(test == "yuen", t.res$test, t.res$statistic)
      }
      
      stat.res[i, 1] <- round(statistic, digits = 2)
      stat.res[i, 2] <- round(p_value, digits = 5)
    }
    
    if (adjust_p) {
      p.adj <- p.adjust(stat.res[, 2], method = "BH")
      stat.res[, 3] <- round(p.adj, digits = 5)
    } else {
      stat.res[, 3] <- stat.res[, 2]
    }
    
    for (i in 1:nrow(stat.res)) {
      pval <- stat.res[i, 3]
      if (!is.na(pval)) {
        if (pval <= 0.0001) {
          stat.res[i, 4] <- '****'
        } else if (pval <= 0.001) {
          stat.res[i, 4] <- '***'
        } else if (pval <= 0.01) {
          stat.res[i, 4] <- '**'
        } else if (pval <= alpha) {
          stat.res[i, 4] <- '*'
        } else {
          stat.res[i, 4] <- ''
        }
      }
    }
    
    stat.res[is.na(stat.res)] <- ""
    rownames(stat.res) <- colnames(data)
    if  (adjust_p == FALSE) {
      stat.res = stat.res[,-3]
    } else {
      stat.res = stat.res
    }
    analysis[[j]] <- stat.res
    
  }
  
  names(analysis) <- names(calculation_spread)
  return(analysis)
}