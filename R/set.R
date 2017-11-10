# Copyright (C) 2017  Brodie Gaslam
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
## Helper Function to Check if a File is Likely to be less Pager

file_is_less <- function(x) {
  if(is.chr.1L(x) && file_test("-x", x)) {
    res <- tryCatch(
      system2(x, "--version", stdout=TRUE, stderr=TRUE),
      warning=function(e) NULL,
      error=function(e) NULL
    )
    length(res) && grepl("^less \\d+", res[1L])
  } else FALSE
}
pager_opt_default <- function(x=getOption("pager")) {
  is.character(x) && !is.na(x[1L]) &&
  normalizePath(x[1L], mustWork=FALSE) ==
    normalizePath(file.path(R.home(), "bin", "pager"), mustWork=FALSE)
}

#' Check Whether System Has less as Pager
#'
#' If \code{getOption(pager)} is set to the default value, checks whether
#' \code{Sys.getenv("PAGER")} appears to be \code{less} by trying to run the
#' pager with the \dQuote{version} and parsing the output.  If
#' \code{getOption(pager)} is not the default value, then checks whether it
#' points to the \code{less} program by the same mechanism.
#'
#' Some systems may have \code{less} pagers installed that do not respond to the
#' \code{$LESS} environment variable.  For example, \code{more} on at least some
#' versions of OS X is \code{less}, but does not actually respond to
#' \code{$LESS}.  If such as pager is the system pager you will likely end up
#' seeing gibberish in the pager.  If this is your use case you will need to
#' set-up a custom pager configuration object that sets the correct system
#' variables (see \code{\link{Pager}}).
#'
#' @seealso \code{\link{Pager}}
#' @return TRUE or FALSE
#' @export
#' @examples
#' pager_is_less()

pager_is_less <- function() {
  pager.opt <- getOption("pager")
  if(pager_opt_default(pager.opt)) {
    file_is_less(Sys.getenv("PAGER"))
  } else if (is.character(pager.opt)) {
    file_is_less(head(pager.opt, 1L))
  } else FALSE
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
