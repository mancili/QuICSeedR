#' Spread Calculation Data
#'
#' @description This function takes a data frame containing metadata and calculation results,
#' and spreads the results into a list of data frames for selected calculation term.
#'
#' @param calculation A data frame containing the metadata and results of the calculation. Output from `GetCalculation()`.
#' @param id_col The name of the column in calculation that identifies the content. Default is 'content'.
#' @param rep_col The name of the column in calculation that identifies the replicate. Default is 'replicate'.
#' @param terms A vector of column names to spread.  Defaults to 'RAF', 'MPR', and 'MS'.
#' @return A list of data frames containing the spread results of the calculation. Each data frame is compatible with 
#' various graphing software, particularly GraphPad Prism, which is the most commonly used graphing tool in F-SAA research.
#' @importFrom methods is
#' @importFrom tidyr pivot_wider
#' @importFrom tidyselect everything all_of
#' @importFrom dplyr select
#' @examples 
#' calculation <- data.frame(
#'   content = rep(c("A", "B", "C"), each = 2),
#'   replicate = rep(1:2, 3),
#'   time_to_threshold = rnorm(6),
#'   RAF = rnorm(6),
#'   MPR = rnorm(6),
#'   MS = rnorm(6)
#'   )
#' 
#' calculation_spread = SpreadCalculation(calculation)
#' print(calculation_spread)
#' 
#' @export
SpreadCalculation <- function(calculation, id_col = "content", rep_col = "replicate", 
                     terms = c('RAF', 'MPR', 'MS')) {
  if (!all(c(id_col, rep_col) %in% colnames(calculation))) {
    stop("id_col and rep_col must be present in the calculation data frame")
  }
  
  if (is.null(terms)) {
    terms <- c('time_to_threshold', 'RAF', 'MPR', 'MS')
  }
  
  if (!all(terms %in% colnames(calculation))) {
    stop("Not all specified terms are present in the calculation data frame")
  }
  
  spread_term <- function(data, term) {
    data %>%
      select(all_of(c(id_col, rep_col, term))) %>%
      pivot_wider(names_from = all_of(id_col), values_from = all_of(term)) %>%
      select(-all_of(rep_col)) %>%
      as.data.frame()
  }
  
  calculation_spread <- lapply(terms, function(term) spread_term(calculation, term))
  
  names(calculation_spread) <- terms
  
  return(calculation_spread)
}

