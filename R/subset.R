#' @include s4.R

NULL

#' Subsetting Methods for Diff Objects
#'
#' Methods to subset the character representation of the diff output.  The
#' subsetting bears no link to the line numbers in the diffs, only to the actual
#' displayed diff.
#'
#' \code{[} only supports numeric indices, and returns without error if you
#' specify out of bound indices.  If you apply multiple subsetting methods they
#' will be applied in the following order irrespective of what order you 
#' actually specify them in: \code{[}, then \code{head}, then \code{tail}.
#' If you use the same subsetting method multiple times on the same object,
#' the last call will define the outcome.
#'
#' These methods are implemented by storing the chosen indices in the 
#' \code{Diff} object and using them to subset the \code{as.character} output.
#' This mechanism explains the seemingly odd behavior documented above.
#'
#' @param x \code{Diff} object
#' @param i subsetting index, must be numeric

setMethod(
  "[", signature(x="Diff", i="numeric", j="missing", drop="missing"),
  function(x, i) {
    if(anyNA(i) || (any(i < 0) && any(i > 0)))
      stop("`i` may not contain NAs or both positive and negative indices")
    x@sub.index <- as.integer(i)
    x
} )

setMethod("head", "Diff",
  function(x, n, ...) {
    if(length(list(...)))
      stop("This method does not support arguments other than `x` or `n`")
    if(!is.int.1L(n)) stop("`n` must be integer(1L) and not NA")
    x@sub.head <- as.integer(n)
    x
} )

setMethod("tail", "Diff",
  function(x, n, ...) {
    if(length(list(...)))
      stop("This method does not support arguments other than `x` or `n`")
    if(!is.int.1L(n)) stop("`n` must be integer(1L) and not NA")
    x@sub.tail <- as.integer(n)
    x
} )
