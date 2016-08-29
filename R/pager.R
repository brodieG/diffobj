# diffobj - Diffs for R Objects
# Copyright (C) 2016  Brodie Gaslam
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# Go to <https://www.r-project.org/Licenses/GPL-3> for a copy of the license.

#' Objects for Specifying Pager Settings
#'
#' Generate pager configuration objects to use as the \code{pager} argument to
#' the \code{\link[=diffPrint]{diff*}} methods or as the \code{pager} slot for
#' \code{\link{Style}} objects.
#'
#' Several pre-defined pager configuration objects are available via
#' constructor functions:
#' \itemize{
#'   \item \code{PagerOff}: Turn off pager
#'   \item \code{PagerSystem}: Use the system pager as invoked by
#'      \code{\link{file.show}}
#'   \item \code{PagerSystemLess}: Like \code{PagerSystem}, but provides
#'      additional configuration options if it the system pager is \code{less}
#'   \item \code{PagerBrowser}: Use \code{\link{browseURL}} as the pager
#' }
#' Make sure you instantiate the pagers with the constructor functions rather
#' than with \code{new} to make sure they are properly configured.
#'
#' @section Custom Pagers:
#'
#' If you wish to define your own pager object you should do so by extending the
#' \code{Pager} virtual class.  In most cases you should be able to use one of
#' the existing objects configured with different parameters, but if your
#' pager function requires special treatment then you can define a custom pager
#' object.  At a minimum you should specify the \code{pager} slot of the object
#' (see constructor function parameter definition).  If the function you
#' use to handle the actual paging is non-blocking (i.e. allows R code
#' evaluation to continue after it is spawned, you may want to wrap it in a
#' function that pauses evaluation such as \code{\link{make_blocking}}, as
#' otherwise the temporary file that contains the diff may be deleted before the
#' pager has a chance to read it.
#'
#' @param pager a function that accepts at least one parameter and does not
#'   require a parameter other than the first parameter.  This function will be
#'   called with a file name passed as the first argument.  The referenced file
#'   will contain the text of the diff.  This is a temporary file that will be
#'   deleted as soon as the pager function completes evaluation.
#'   \code{PagerSystem} and \code{PagerSystemLess} use \code{\link{file.show}}
#'   by default, and \code{PagerBrowser} uses \code{\link{browseURL}}.
#' @param file.ext character(1L) an extension to append to file name passed to
#'   \code{pager}, \emph{without} the period.  For example, \code{PagerBrowser}
#'   uses \dQuote{html} to cause \code{\link{browseURL}} to launch the
#'   web browser.
#' @param threshold integer(1L) number of lines of output that triggers the use
#'   of the pager; negative values lead to using
#'   \code{\link{console_lines} + 1}, and zero leads to always using the pager
#'   irrespective of how many lines the output has.
#' @param flags character(1L), only for \code{PagerSystemLess}, what flags to
#'   set with the \code{LESS} system environment variable.  By default the
#'   \dQuote{R} flag is set to ensure ANSI escape sequences are interpreted if
#'   it appears your terminal supports ANSI escape sequences.  If you want to
#'   leave the output on the screen after you exit the pager you can use
#'   \dQuote{RX}.  You should only provide the flag letters (e.g. \dQuote{"RX"},
#'   not \code{"-RX"}).  The system variable is only modified for the duration
#'   of the evaluation and is reset / unset afterwards. \emph{Note:} you must
#'   specify this slot via the constructor as in the example.  If you set the
#'   slot directly it will not have any effect.
#' @param ... additional arguments to pass on to \code{new}, typically not used
#'
#' @aliases PagerOff, PagerSystem, PagerSystemLess, PagerBrowser
#' @importFrom utils browseURL
#' @rdname Pager
#' @name Pager
#' @examples
#' ## Assuming system pager is `less` and terminal supports ANSI ESC sequences
#' ## Equivalent to running `less -RFX`
#' \dontrun{
#' diffPrint(letters, LETTERS, pager=PagerSystemLess(flags="RFX"))
#' }

setClass(
  "Pager",
  contains="VIRTUAL",
  slots=c(pager="function", file.ext="character", threshold="integer"),
  prototype=list(
    file.ext="", threshold=0L,
    pager=function(x) stop("Pager object does not specify a paging function.")
  ),
  validity=function(object) {
    if(!is.chr.1L(object@file.ext)) return("Invalid `file.ext` slot")
    if(!is.int.1L(object@threshold)) return("Invalid `threshold` slot")
    TRUE
  }
)
#' @export
#' @rdname Pager

setClass("PagerOff", contains="Pager")

#' @export
#' @rdname Pager

PagerOff <- function(...) new("PagerOff", ...)

#' @export
#' @rdname Pager

setClass(
  "PagerSystem", contains="Pager",
  prototype=list(pager=file.show, threshold=-1L),
)
#' @export
#' @rdname Pager

PagerSystem <- function(pager=file.show, threshold=-1L, file.ext="", ...)
  new("PagerSystem", pager=pager, threshold=threshold, file.ext=file.ext, ...)

#' @export
#' @rdname Pager

setClass(
  "PagerSystemLess", contains="PagerSystem", slots=c("flags"),
  prototype=list(flags="R")
)
#' @export
#' @rdname Pager

PagerSystemLess <-
  function(pager=file.show, threshold=-1L, file.ext="", flags="R", ...)
    new(
      "PagerSystemLess", pager=pager, threshold=threshold, file.ext=file.ext,
      flags=flags, ...
    )

# Must use initialize so that the pager function can access the flags slot

setMethod("initialize", "PagerSystemLess",
  function(.Object, ...) {
    dots <- list(...)
    if("flags" %in% names(dots)) {
      flags <- dots$flags
      if(!is.chr.1L(flags))
        stop("Argument `flags` must be character(1L) and not NA")
    } else flags <- .Object@flags
    pager.old <- dots$pager
    pager <- function(x) {
      old.less <- set_less_var(flags)
      on.exit(reset_less_var(old.less), add=TRUE)
      pager.old(x)
    }
    dots$flags <- flags
    dots$pager <- pager
    do.call(callNextMethod, c(list(.Object), dots))
} )
#' Create a Blocking Version of a Function
#'
#' Wraps \code{fun} in a function that runs \code{fun} and then issues a
#' \code{readline} prompt to prevent further R code evaluation until user
#' presses a key.
#'
#' @export
#' @param fun a function
#' @param msg character(1L) a message to use as the \code{readline} prompt
#' @param invisible.res whether to return the result of \code{fun} invisibly
#' @return \code{fun}, wrapped in a function that does the blocking
#' make_blocking(sum, invisible.res=FALSE)(1:10)

make_blocking <- function(
  fun, msg="Press ENTER to continue...", invisible.res=TRUE
) {
  if(!is.function(fun)) stop("Argument `fun` must be a function")
  if(!is.chr.1L(msg)) stop("Argument `msg` must be character(1L) and not NA")
  if(!is.TF(invisible.res))
    stop("Argument `invisible.res` must be TRUE or FALSE")
  function(...) {
    res <- fun(...)
    readline(msg)
    if(invisible.res) invisible(res) else res
  }
}
#' Invoke IDE Viewer If Available, browseURL If Not
#'
#' Use \code{getOption("viewer")} to view HTML output if it is available as
#' per \href{https://support.rstudio.com/hc/en-us/articles/202133558-Extending-RStudio-with-the-Viewer-Pane}{RStudio}. Fallback to \code{\link{browseURL}}
#' if not available.
#'
#' @export
#' @param url character(1L) a location containing a file to display
#' @return the return vaue of \code{getOption("viewer")} if it is a function, or
#'   of \code{\link{browseURL}} if the viewer is not available

view_or_browse <- function(url) {
  viewer <- getOption("viewer")
  view.success <- FALSE
  if(is.function(viewer)) {
    view.try <- try(res <- viewer(url), silent=TRUE)
    if(inherits(view.try, "try-error")) {
      warning(
        "IDE viewer failed with error ",
        conditionMessage(attr(view.try, "condition")),
        "; falling back to `browseURL`"
      )
    } else view.success <- TRUE
  }
  if(!view.success) {
    res <- utils::browseURL(url)
  }
  res
}
setClass("PagerBrowser", contains="Pager")

#' @export
#' @rdname Pager

PagerBrowser <- function(
  pager=make_blocking(view_or_browse), threshold=0L, file.ext="html", ...
)
  new("PagerBrowser", pager=pager, threshold=threshold, file.ext=file.ext, ...)

# Helper function to determine whether pager will be used or not

use_pager <- function(pager, len) {
  if(!is(pager, "Pager"))
    stop("Logic Error: expecting `Pager` arg; contact maintainer.")
  if(!is(pager, "PagerOff")) {
    threshold <- if(pager@threshold < 0L) {
      console_lines()
    } else pager@threshold
    !threshold || len > threshold
  } else FALSE
}
