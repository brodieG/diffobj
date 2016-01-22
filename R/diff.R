#' Diff two character vectors
#'
#' Implementation of Myer's diff borrowed from Mike B. Allen
#'
#' @export
#' @param a character
#' @param b character
#' @return list
#' @useDynLib diffr, .registration=TRUE, .fixes="DIFFR_"

diffr <- function(a, b) {
  stopifnot(is.character(a), is.character(b), all(!is.na(c(a, b))))
  .Call(DIFFR_diffr, a, b)
}
