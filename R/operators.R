#' Null default operator
#'
#' @name null-default
#' @aliases %||%
#'
#' This operator returns the left-hand side if it's not NULL, and the right-hand side otherwise.
#'
#' @param x The primary value
#' @param y The default value to be used if x is NULL
#'
#' @return x if it's not NULL, otherwise y
#'
#' @examples
#' NULL %||% 1  # Returns 1
#' 5 %||% 1     # Returns 5
#'
#' @export
`%||%` <- function(x, y) if (is.null(x)) y else x