# Copyright (C) 2018  Brodie Gaslam
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

#' Objects for Specifying Pager Settings
#'
#' Modify use of pager behavior with pager configuration objects to use as the
#' \code{pager} argument to the \code{\link[=diffPrint]{diff*}} methods or as
#' the \code{pager} slot for \code{\link{Style}} objects.  Note that in this
#' documentation we use the \dQuote{pager} term loosely and intend it to refer
#' to any device other than the terminal that can be used to render output.
#'
#' @section Default Output Behavior:
#'
#' \code{\link[=diffPrint]{diff*}} methods use \dQuote{pagers} to help
#' manage large outputs and also to provide an alternative colored diff when the
#' terminal does not support them directly.
#'
#' For OS X and *nix systems where \code{less} is the pager and the
#' terminal supports ANSI escape sequences, output is colored with ANSI escape
#' sequences.  If the output exceeds one screen height in size (as estimated by
#' \code{\link{console_lines}}) it is sent to the pager.
#'
#' If the terminal does not support ANSI escape sequences, or if the system
#' pager is not \code{less} as detected by \code{\link{pager_is_less}}, then the
#' output is rendered in HTML and sent to the IDE viewer
#' (\code{getOption("viewer")}) if defined, or to the browser with
#' \code{\link{browseURL}} if not.  This behavior may seem sub-optimal for
#' systems that have ANSI aware terminals and ANSI aware pagers other than
#' \code{less}, but these should be rare and it is possible to configure
#' \code{diffobj} to produce the correct output for them (see examples).
#'
#' @section Pagers and Styles:
#'
#' There is a close relationship between pagers and \code{\link{Style}}.  The
#' \code{Style} objects control whether the output is raw text, formatted
#' with ANSI escape sequences, or marked up with HTML.  In order for these
#' different types of outputs to render properly, they need to be sent to the
#' right device.  For this reason \code{\link{Style}} objects come with a
#' \code{Pager} configuration object pre-assigned so the output can render
#' correctly.  The exact \code{Pager} configuration object depends on the
#' \code{\link{Style}} as well as the system configuration.
#'
#' In any call to the \code{\link[=diffPrint]{diff*}} methods you can always
#' specify both the \code{\link{Style}} and \code{Pager} configuration object
#' directly for full control of output formatting and rendering.  We have tried
#' to set-up sensible defaults for most likely use cases, but given the complex
#' interactions involved it is possible you may need to configure things
#' explicitly.  Should you need to define explicit configurations you can save
#' them as option values with
#' \code{options(diffobj.pager=..., diffobj.style=...)} so that you do not need
#' to specify them each time you use \code{diffobj}.
#'
#' @section Pager Configuration Objects:
#'
#' The \code{Pager} configuration objects allow you to specify what device to
#' use as the pager and under what circumstances the pager should be used.
#' Several pre-defined pager configuration objects are available via
#' constructor functions:
#' \itemize{
#'   \item \code{PagerOff}: Turn off pager
#'   \item \code{PagerSystem}: Use the system pager as invoked by
#'      \code{\link{file.show}}
#'   \item \code{PagerSystemLess}: Like \code{PagerSystem}, but provides
#'      additional configuration options if the system pager is \code{less}.
#'      Note this object does not change the system pager; it only allows you to
#'      configure it via the \code{$LESS} environment variable which will have
#'      no effect unless the system pager is set to be \code{less}.
#'   \item \code{PagerBrowser}: Use \code{getOption("viewer")} if defined, or
#'     \code{\link{browseURL}} if not
#' }
#' \emph{Important}: Make sure you instantiate the pager objects with the
#' constructor functions (e.g. \code{PagerSystemLess(...)} not
#' \code{new('PagerSystemLess', ...)} as otherwise they will not be properly
#' configured.
#'
#' The default configuration for \code{PagerSystem} and \code{PagerSystemLess}
#' leads to output being sent to the pager if it exceeds the estimated window
#' size, whereas \code{PagerBrowser} always sends output to the pager.  This
#' behavior can be configured via the \code{threshold} parameter.
#'
#' \code{PagerSystemLess}'s primary role is to correctly configure the
#' \code{$LESS} system variable so that \code{less} renders the ANSI escape
#' sequences as intended.  On OS X \code{more} is a faux-alias to \code{less} of
#' sorts, except it does not appear to read the \code{$LESS} system variable.
#' Should you configure your system pager to be the \code{more} version of
#' \code{less}, \code{\link{pager_is_less}} will be tricked into thinking you
#' are using a \dQuote{normal} version of \code{less} and you will likely end up
#' seeing gibberish in the pager.  If this is your use case you will need to
#' set-up a custom pager configuration object that sets the correct system
#' variables.
#'
#' @section Custom Pager Configurations:
#'
#' In most cases the simplest way to generate new pager configurations is to
#' start with one of the existing configuration objects.  For example, if you
#' wanted to tell \code{diffobj} that your default system pager supports ANSI
#' escape sequences despite not being \code{less} you could use
#' \code{diffPrint(a, b, pager=PagerSystem(ansi=TRUE)}.
#'
#' You can change what system pager is used by changing it with
#' \code{options(pager=...} or by changing the \code{$PAGER} environment
#' variable.  You can also explicitly set a function to act as the pager when
#' you instantiate the \code{Pager} configuration object (see examples).
#'
#' If you wish to define your own pager object you should do so by extending the
#' \code{Pager} virtual class.  At a minimum you should specify the \code{pager}
#' slot of the object (see constructor function parameter definition).  If the
#' function you use to handle the actual paging is non-blocking (i.e. allows R
#' code evaluation to continue after it is spawned, you may want to wrap it in
#' a function that pauses evaluation such as \code{\link{make_blocking}}, as
#' otherwise the temporary file that contains the diff may be deleted before
#' the pager has a chance to read it.
#'
#' @param pager a function that accepts at least one parameter and does not
#'   require a parameter other than the first parameter.  This function will be
#'   called with a file name passed as the first argument.  The referenced file
#'   will contain the text of the diff.  This is a temporary file that will be
#'   deleted as soon as the pager function completes evaluation.
#'   \code{PagerSystem} and \code{PagerSystemLess} use \code{\link{file.show}}
#'   by default, and \code{PagerBrowser} uses
#'   \code{make_blocking(view_or_browse)}.  Note that
#'   \code{\link{make_blocking}} ensures that the temporary file is not deleted
#'   before the pager can access it.
#' @param file.ext character(1L) an extension to append to file name passed to
#'   \code{pager}, \emph{without} the period.  For example, \code{PagerBrowser}
#'   uses \dQuote{html} to cause \code{\link{browseURL}} to launch the web
#'   browser.
#' @param threshold integer(1L) number of lines of output that triggers the use
#'   of the pager; negative values lead to using
#'   \code{\link{console_lines} + 1}, and zero leads to always using the pager
#'   irrespective of how many lines the output has.
#' @param ansi TRUE or FALSE, whether the pager supports ANSI escape
#'   sequences.
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
#' @seealso \code{\link{Style}}, \code{\link{pager_is_less}}
#' @examples
#' ## We `dontrun` these examples as they involve pagers that should only be run
#' ## in interactive mode
#' \dontrun{
#' ## Assuming system pager is `less` and terminal supports ANSI ESC sequences
#' ## Equivalent to running `less -RFX`
#'
#' diffChr(1:200, 180:300, pager=PagerSystemLess(flags="RFX"))
#'
#' ## System pager is not less, but it supports ANSI escape sequences
#'
#' diffChr(1:200, 180:300, pager=PagerSystem(ansi=TRUE))
#'
#' ## Use a custom pager, in this case we make up a trivial one and configure it
#' ## always page (`threshold=0L`)
#'
#' page.fun <- function(x) cat(paste0("| ", readLines(x)), sep="\n")
#' page.conf <- PagerSystem(pager=page.fun, threshold=0L)
#' diffChr(1:200, 180:300, pager=page.conf, width=getOption("width") - 2)
#'
#' ## Set-up the custom pager as the default pager
#'
#' options(diffobj.pager=page.conf)
#' diffChr(1:200, 180:300)
#'
#' ## A blocking pager (this is effectively very similar to what `PagerBrowser`
#' ## does); need to block b/c otherwise temp file with diff could be deleted
#' ## before the device has a chance to read it since `browseURL` is not
#' ## blocking itself.  On OS X we need to specify the extension so the correct
#' ## program opens it (in this case `TextEdit`):
#'
#' page.fun <- make_blocking(browseURL)
#' page.conf <- PagerSystem(pager=page.fun, file.ext="txt")
#' diffChr(1:200, 180:300, pager=page.conf)
#' }

setClass(
  "Pager",
  contains="VIRTUAL",
  slots=c(
    pager="function", file.ext="character", threshold="numeric",
    ansi="logical"
  ),
  prototype=list(
    file.ext="", threshold=0L,
    pager=function(x) stop("Pager object does not specify a paging function."),
    ansi=FALSE
  ),
  validity=function(object) {
    if(!is.chr.1L(object@file.ext)) return("Invalid `file.ext` slot")
    if(!is.int.1L(object@threshold)) return("Invalid `threshold` slot")
    if(!is.TF(object@ansi)) return("Invalid `ansi` slot")
    TRUE
  }
)
#' @export
#' @rdname Pager

setClass(
  "PagerOff", contains="Pager",
  prototype=list(ansi=TRUE)    # pager off shouldn't prevent ANSI use
)

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

PagerSystem <- function(
  pager=file.show, threshold=-1L, file.ext="", ...
)
  new("PagerSystem", pager=pager, threshold=threshold, file.ext=file.ext, ...)

#' @export
#' @rdname Pager

setClass(
  "PagerSystemLess", contains="PagerSystem", slots=c("flags"),
  prototype=list(flags="R")
)
#' @export
#' @rdname Pager

PagerSystemLess <- function(
    pager=file.show, threshold=-1L, file.ext="", flags="R",
    ansi=TRUE, ...
  )
    new(
      "PagerSystemLess", pager=pager, threshold=threshold, file.ext=file.ext,
      flags=flags, ansi=ansi, ...
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
    stop("Logic Error: expecting `Pager` arg; contact maintainer.") # nocov
  if(!is(pager, "PagerOff")) {
    threshold <- if(pager@threshold < 0L) {
      console_lines()
    } else pager@threshold
    !threshold || len > threshold
  } else FALSE
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
pager_opt_default <- function(x=getOption("pager")) {
  is.character(x) && !is.na(x[1L]) &&
  normalizePath(x[1L], mustWork=FALSE) ==
    normalizePath(file.path(R.home(), "bin", "pager"), mustWork=FALSE)
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

