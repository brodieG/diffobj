# Copyright (C) 2016  Brodie Gaslam
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

#' Finalizing Methods for HTML Output
#'
#' Used as the \code{finalizer} slot to \code{\link{StyleHtml}} objects to wrap
#' character output prior to output to device.  Used primarily by styles that
#' output to HTML to properly configure HTML page structure, including injecting
#' JS, CSS, etc..
#'
#' @param x object to finalize
#' @param x.chr character text representation of \code{x}, typically generated
#'   with the \code{as.character} method for \code{x}
#' @param js character javascript code to append to HTML representation
#' @param ... arguments to pass on to methods
#' @export

setGeneric(
  "finalizeHtml", function(x, ...) standardGeneric("finalizeHtml")  # nocov
)

#' @rdname finalizeHtml

setMethod("finalizeHtml", c("ANY"),
  function(x, x.chr, js, ...) {
    if(!is.character(x.chr)) stop("Argument `x.chr` must be character")
    if(!is.chr.1L(js)) stop("Argument `js` must be character(1L) and not NA.")

    style <- x@etc@style

    html.output <- style@html.output
    pager <- style@pager

    if(html.output == "auto") {
      html.output <- if(is(pager, "PagerBrowser")) "page" else "diff.only"
    }
    if(html.output %in% c("diff.w.style", "page")) {
      css.txt <- try(paste0(readLines(style@css), collapse="\n"), silent=TRUE)
      if(inherits(css.txt, "try-error")) {
        cond <- attr(css.txt, "condition")
        warning(
          "Unable to read provided css file \"", style@css, "\" (error: ",
          paste0(conditionMessage(cond), collapse=""), ")."
        )
        css <- ""
      } else {
        css <- sprintf("<style type='text/css'>\n%s\n</style>", css.txt)
      }
    }
    if(html.output == "diff.w.style") {
      tpl <- "%s%s"
    } else if (html.output == "page") {
      tpl <- sprintf("
        <!DOCTYPE html>
        <html>
          <head>
            %%s\n
          </head>
          <body>
            <div id='diffobj_outer'>\n%%s\n</div>
            <script type='text/javascript'>\n%s\n</script>
          </body>
        </html>",
        js
      )
    } else if (html.output == "diff.only") {
      css <- ""
      tpl <- "%s%s"
    } else stop("Logic Error: unexpected html.output; contact maintainer.")
    sprintf(tpl, css, paste0(x.chr, collapse=""))
  }
)
