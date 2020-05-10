# Copyright (C) 2020 Brodie Gaslam
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

#' @include styles.R

NULL

#' Attempt to Compute Console Height in Text Lines
#'
#' Returns the value of the \code{LINES} system variable if it is reasonable,
#' 48 otherwise.
#'
#' @export
#' @return integer(1L)
#' @examples
#' console_lines()

console_lines <- function() {
  LINES <- as.integer(Sys.getenv("LINES"))
  if(length(LINES) == 1L && !is.na(LINES) && LINES > 0L) LINES else 48L
}
#' Configure Automatic Context Calculation
#'
#' Helper functions to help define parameters for selecting an appropriate
#' \code{context} value.
#'
#' @export
#' @param min integer(1L), positive, set to zero to allow any context
#' @param max integer(1L), set to negative to allow any context
#' @return S4 object containing configuration parameters, for use as the
#'   \code{context} or parameter value in \code{\link[=diffPrint]{diff*}}
#'   methods
#' @examples
#' ## `pager="off"` for CRAN compliance; you may omit in normal use
#' diffChr(letters, letters[-13], context=auto_context(0, 3), pager="off")
#' diffChr(letters, letters[-13], context=auto_context(0, 10), pager="off")
#' diffChr(
#'   letters, letters[-13], context=auto_context(0, 10), line.limit=3L,
#'   pager="off"
#' )

auto_context <- function(
  min=getOption("diffobj.context.auto.min"),
  max=getOption("diffobj.context.auto.max")
){
  if(!is.int.1L(min) || min < 0L)
    stop("Argument `min` must be integer(1L) and greater than zero")
  if(!is.int.1L(max))
    stop("Argument `max` must be integer(1L) and not NA")
  new("AutoContext", min=as.integer(min), max=as.integer(max))
}
# Changes the LESS system variable to make it compatible with ANSI escape
# sequences
#
# flags is supposed to be character(1L) in form "XVF" or some such
#
# Returns the previous value of the variable, NA if it was not set

set_less_var <- function(flags) {
  LESS <- Sys.getenv("LESS", unset=NA) # NA return is NA_character_
  LESS.new <- NA
  if(is.character(LESS) && length(LESS) == 1L) {
    if(isTRUE(grepl("^\\s*$", LESS)) || is.na(LESS) || !nzchar(LESS)) {
      LESS.new <- sprintf("-%s", flags)
    } else if(
      isTRUE(grepl("^\\s*-[[:alpha:]]+(\\s+-[[:alpha:]])*\\s*$", LESS))
    ) {
      LESS.new <- sub(
        "\\s*\\K(-[[:alpha:]]+)\\b$", sprintf("\\1%s", flags), LESS, perl=TRUE
  ) } }
  if(!is.na(LESS.new)) Sys.setenv(LESS=LESS.new) else
    warning("Unable to set `LESS` system variable")
  LESS
}
reset_less_var <- function(LESS.old) {
  if(is.na(LESS.old)) {
    Sys.unsetenv("LESS")
  } else Sys.setenv(LESS=LESS.old)
}
