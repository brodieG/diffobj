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
#' function that pauses evaluation (e.g. with \code{\link{readline}}), as
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
#' diffPrint(letters, LETTERS, pager=PagerSystemLess(flags="RFX"))

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
    .Object@pager <- function(x) {
      old.less <- set_less_var(flags)
      on.exit(reset_less_var(old.less), add=TRUE)
      file.show(x)
    }
    callNextMethod(.Object, ...)
} )
#' @export
#' @rdname Pager

setClass(
  "PagerBrowser", contains="Pager",
  prototype=list(
    file.ext="html",
    threshold=0L,
    pager=function(x) {
      res <- browseURL(x)
      readline("Press ENTER to continue...")
      invisible(res)
} ) )
#' @export
#' @rdname Pager

PagerBrowser <- function(pager=browseURL, threshold=0L, file.ext="html", ...)
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
