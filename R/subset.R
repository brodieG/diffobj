# Copyright (C) 2019 Brodie Gaslam
#
# This file is part of "diffobj - Diffs for R Objects"
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# Go to <https://www.r-project.org/Licenses/GPL-2> for a copy of the license.

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
#' @export
#' @rdname extract-Diff-method
#' @param x \code{Diff} object
#' @param i subsetting index, must be numeric
#' @param n integer(1L), the size for the resulting object
#' @param ... unused, for compatibility with generics
#' @return \code{Diff} object with subsetting indices recorded for use by
#'   \code{show}
#' ## `pager="off"` for CRAN compliance; you may omit in normal use
#' diff <- diffChr(letters, LETTERS, format="raw", pager="off")
#' diff[5:15]
#' head(diff, 5)
#' tail(diff, 5)
#' head(head(diff, 5), 8)  ## note not 'typical' behavior

setMethod(
  "[", signature(x="Diff", i="numeric", j="missing", drop="missing"),
  function(x, i) {
    if(anyNA(i) || (any(i < 0) && any(i > 0)))
      stop("`i` may not contain NAs or both positive and negative indices")
    x@sub.index <- as.integer(i)
    x
} )

#' @export
#' @rdname extract-Diff-method

setMethod("head", "Diff",
  function(x, n, ...) {
    if(length(list(...)))
      stop("This method does not support arguments other than `x` or `n`")
    if(!is.int.1L(n)) stop("`n` must be integer(1L) and not NA")
    x@sub.head <- as.integer(n)
    x
} )
#' @export
#' @rdname extract-Diff-method

setMethod("tail", "Diff",
  function(x, n, ...) {
    if(length(list(...)))
      stop("This method does not support arguments other than `x` or `n`")
    if(!is.int.1L(n)) stop("`n` must be integer(1L) and not NA")
    x@sub.tail <- as.integer(n)
    x
} )
