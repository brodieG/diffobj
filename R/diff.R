#' Compare R Objects with a Text Diff
#'
#' Highlights differences between R objects in a familiar and intuitive way.
#' This is similar to \code{\link{tools::Rdiff}} except the diff is computed
#' directly on R objects instead of text files, does not rely on the system
#' \code{diff} utility, and provides alternate display modes.
#'
#' @import crayon
#' @name diffobj-package
#' @docType package

NULL

# Because all these functions are so similar, we have construct them with a
# function factory.  This allows us to easily maintain consisten formals during
# initial development process when they have not been set in stone yet.

make_diff_fun <- function(capt_fun) {
  function(
    target, current,
    mode=gdo("mode"),
    context=gdo("context"),
    line.limit=gdo("line.limit"),
    format=gdo("format"),
    brightness=gdo("brightness"),
    color.mode=gdo("color.mode"),
    pager=gdo("pager"),
    ignore.white.space=gdo("ignore.white.space"),
    max.diffs=gdo("max.diffs"),
    align.threshold=gdo("align.threshold"),
    disp.width=gdo("disp.width"),
    hunk.limit=gdo("hunk.limit"),
    convert.hz.white.space=gdo("convert.hz.white.space"),
    tab.stops=gdo("tab.stops"),
    style=gdo("style"),
    palette.of.styles=diffObjStylePalette(),
    tar.banner=NULL,
    cur.banner=NULL,
    frame=parent.frame(),
    ...
  ) {
    # Force evaluation of dots to make sure user doesn't mess us up with
    # something like options(crayon.enabled=...)

    dots <- list(...)

    # Check args and evaluate all the auto-selection arguments

    etc.proc <- check_args(
      call=call, tar.exp=substitute(target), cur.exp=substitute(current),
      mode=mode, context=context, line.limit=line.limit, format=format,
      brightness=brightness, color.mode=color.mode, pager=pager,
      ignore.white.space=ignore.white.space, max.diffs=max.diffs,
      align.threshold=align.threshold, disp.width=disp.width,
      hunk.limit=hunk.limit, convert.hz.white.space=convert.hz.white.space,
      tab.stops=tab.stops, style=style, palette.of.styles=palette.of.styles,
      frame=frame, tar.banner=tar.banner, cur.banner=cur.banner
    )
    # Force crayon to whatever ansi status we chose; note we must do this after
    # touching vars in case someone passes `options(crayon.enabled=...)` as one
    # of the arguments

    old.crayon.opt <-
      options(crayon.enabled=is(etc.proc@style, "diffObjStyleAnsi"))
    on.exit(options(old.crayon.opt), add=TRUE)
    err <- make_err_fun(sys.call())

    # Compute gutter values so that we know correct widths to use for capture,
    # etc. Will need to update in HTML mode...

    nc_fun <- if(is(etc.proc@style, "diffObjStyleAnsi")) crayon_nchar else nchar
    etc.proc@gutter <- gutter_dat(etc.proc)
    if(is(etc.proc@style, "diffObjStyleHtml")) {
      etc.proc@line.width <- 0L
      etc.proc@text.width <- 0L
    } else {
      disp.width <- if(etc.proc@mode == "sidebyside") {
        as.integer(
          (etc.proc@disp.width - nc_fun(etc.proc@style@text@pad.col)) / 2
        )
      } else etc.proc@disp.width

      etc.proc@line.width <-
        max(disp.width, .min.width + etc.proc@gutter@width)
      etc.proc@text.width <-
        etc.proc@line.width - etc.proc@gutter@width
    }

    # Capture and diff

    diff <- capt_fun(target, current, etc=etc.proc, err=err, ...)
    diff
  }
}
#' Diff \code{print}ed Objects
#'
#' Runs the diff between the \code{print} or \code{show} output produced by
#' two objects.  If the objects are both normal atomic vectors, the function
#' will recognize the wrapped printed output and will carry out the diff
#' element by element rather than line by line.  The \code{+-} diff indicators
#' in the gutters will still reference the line diffs, but additionally the
#' element by element matches and differences will be highlighted by word diff
#' markers.
#'
#' @export
#' @param target the reference object
#' @param current the object being compared to \code{target}
#' @param mode character(1L), one of:
#'   \itemize{
#'     \item \dQuote{unified}: diff mode used by \code{git diff}, and the
#'       default here
#'     \item \dQuote{sidebyside}: line up the differences side by side
#'     \item \dQuote{context}: show the target and current hunks in their
#'       entirety; this mode takes up a lot of screen space but makes it easier
#'       to see what the objects actually look like
#'   }
#' @param context integer(1L) how many lines of context are shown on either side
#'   of differences, set to \code{-1L} to allow as many as there are.  Set to
#'   \dQuote{auto} (default) to display as many as 10 lines or as few as 1
#'   depending on whether total screen lines fit within \code{line.limit} (see
#'   \code{\link{etc}}), or alternatively pass the return value of
#'   \code{\link{auto_context}} to fine tune the parameters of the auto context
#'   calculation.
#' @param line.limit integer(2L) or integer(1L), if length 1 how many lines of
#'   output to show, where \code{-1} means no limit.  If length 2, the first
#'   value indicates the threshold of screen lines to begin truncating output,
#'   and the second the number of lines to truncate to, which should be fewer
#'   than the threshold.
#' @param hunk.limit integer(2L) or integer (1L), how many diff hunks to show.
#'   Behaves similarly to the \code{line.limit}.  How many hunks are in a
#'   particular diff is a function of how many differences, and also how much
#'   \code{context} is used since context can cause two hunks to bleed into
#'   each other and become one.
#' @param ignore.white.space TRUE or FALSE, whether to consider differences in
#'   horizontal whitespace (i.e. spaces and tabs) as differences (defaults to
#'   FALSE)
#' @param convert.hz.whitespace TRUE or FALSE, whether modify input strings
#'   that contain tabs and carriage returns in such a way that they display as
#'   they would \bold{with} those characters, but without using those
#'   characters (defaults to TRUE).  If your console uses tab stops that are not
#'   eight characters apart you may specify them with \code{tab.stops}.
#' @param tab.stops integer, what tab stops to use when converting hard tabs to
#'   spaces.  If not integer will be coerced to integer (defaults to 8L).
#' @param disp.width integer(1L) number of display columns to take up; note that
#'   in \dQuote{sidebyside} mode the effective display width is half this
#'   number (defaults to \code{getOption("width")}.
#' @param frame environment the evaluation frame for the \code{print/show/str},
#'   calls, allows user to ensure correct methods are used, not used by
#'   \code{\link{diff_chr}} or \code{\link{diff_deparse}}.
#' @param max.diffs integer(1L), number of differences after which we abandon
#'   the \code{O(n^2)} diff algorithm in favor of a linear one.  Set to
#'   \code{-1L} to always stick to the original algorithm (defaults to 10000L).
#' @param align.threshold numeric(1L) between 0 and 1, proportion of
#'   characters in a line of \code{target} that must be matched in a line of
#'   \code{current} in the same hunk for those lines to be paired up when
#'   displayed (defaults to 0.25).
#' @param tar.banner character(1L) or NULL, text to display ahead of the diff
#'   section representing the target output.  If NULL will be
#'   inferred from \code{target} and \code{current} expressions.
#' @param cur.banner character(1L) like \code{tar.banner}, but for
#'   \code{current}
#' @return a \code{diffObjSettings} S4 object for use with the
#'   \code{\link{diff_obj}} family of functions
#' @param ... additional arguments to pass on to \code{print}, \code{str}, etc.
#' @seealso \code{\link{diff_obj}} for details on output and diff algorithm,
#'   \code{\link{etc}} for more detailed control of diff settings,
#'   \code{\link{diff_str}},
#'   \code{\link{diff_chr}} to compare character vectors directly,
#'   \code{\link{diff_deparse}} to compare deparsed objects
#' @return a \code{\link{diffObjDiff}} object; this object has a \code{show}
#'   method that will display the diff to screen
#' @export

diff_print <- make_diff_fun(capt_print)

#' Diff Object Structures
#'
#' Compares the \code{str} output of \code{target} and \code{current}.  If
#' the \code{max.level} parameter to \code{str} is left unspecified, will
#' attempt to find the largest \code{max.level} that fits within
#' \code{line.limit} (see \code{\link{etc}}) and shows at least one difference.
#'
#' Due to the seemingly inconsistent nature of \code{max.level} when used with
#' objects with nested attributes, and also due to the relative slowness of
#' \code{str}, this function simulates the effect of \code{max.level} by hiding
#' nested lines instead of repeatedly calling \code{str} with varying values of
#' \code{max.level}.
#'
#' @inheritParams diff_print
#' @seealso \code{\link{diff_obj}} for details on output and diff algorithm,
#'   \code{\link{etc}} for more detailed control of diff settings,
#'   \code{\link{diff_print}},
#'   \code{\link{diff_chr}} to compare character vectors directly,
#'   \code{\link{diff_deparse}} to compare deparsed objects
#' @return a \code{\link{diffObjDiff}} object; this object has a \code{show}
#'   method that will display the diff to screen
#' @export

diff_str <- make_diff_fun(capt_str)

#' Diff Character Vectors Element By Element
#'
#' Will perform the diff on the actual string values of the character vectors
#' without displaying to screen and capturing.  Each vector element is treated
#' as a line of text.
#'
#' @inheritParams diff_print
#' @seealso \code{\link{diff_obj}} for details on output and diff algorithm,
#'   \code{\link{etc}} for more detailed control of diff settings,
#'   \code{\link{diff_print}}, \code{\link{diff_str}},
#'   \code{\link{diff_deparse}} to compare deparsed objects
#' @return a \code{\link{diffObjDiff}} object; this object has a \code{show}
#'   method that will display the diff to screen
#' @export
#' @examples
#' diff_chr(LETTERS[1:5], LETTERS[2:6])

diff_chr <- make_diff_fun(capt_chr)

#' Diff Deparsed Objects
#'
#' Perform diff on the character vectors produced by \code{\link{deparse}}ing
#' the objects.
#'
#' @export
#' @inheritParams diff_print
#' @seealso \code{\link{diff_obj}} for details on output and diff algorithm,
#'   \code{\link{etc}} for more detailed control of diff settings,
#'   \code{\link{diff_print}}, \code{\link{diff_str}},
#'   \code{\link{diff_chr}} to compare character vectors directly
#' @return a \code{\link{diffObjDiff}} object; this object has a \code{show}
#'   method that will display the diff to screen
#' @export
#' @examples
#' diff_deparse(matrix(1:9, 3), 1:9)

diff_deparse <- make_diff_fun(capt_deparse)

#' Diff Objects
#'
#' Compare either the \code{print}ed or \code{str} screen representation of
#' R objects depending on which is estimated to produce the most useful
#' diff.  The selection process tries to minimize screen lines while maximizing
#' differences shown subject to display constraints.  The decision algorithm is
#' likely to evolve over time, so do not rely on this function making a
#' a particular selection under specific circumstances.  Instead, use
#' \code{\link{diff_print}} or \code{\link{diff_str}} if you require one or the
#' other output.
#'
#'
#' @inheritParams diff_print
#' @seealso \code{\link{etc}} for more detailed control of diff settings,
#'   \code{\link{diff_print}}, \code{\link{diff_str}},
#'   \code{\link{diff_chr}} to compare character vectors directly,
#'   \code{\link{diff_deparse}} to compare deparsed objects
#' @return a \code{\link{diffObjDiff}} object; this object has a \code{show}
#'   method that will display the diff to screen
#' @export

diff_obj <- diff_print # we overwrite the body next

body(diff_obj) <- quote({
  if(length(list(...))) {
    stop("`...` argument not supported in `diff_obj`")
  }
  call.raw <- match.call()
  call.raw[["silent"]] <- TRUE
  call.str <- call.print <- call.raw
  call.str[[1L]] <- quote(diff_str)
  call.str[["max.level"]] <- "auto"
  call.print[[1L]] <- quote(diff_print)

  # Run both the print and str versions, and then decide which to use based
  # on some weighting of various factors including how many lines needed to be
  # omitted vs. how many differences were reported

  res.print <- eval(call.print, parent.frame())
  res.str <- eval(call.str, parent.frame())

  diff.p <- count_diff_hunks(res.print@diffs$hunks)
  diff.s <- count_diff_hunks(res.str@diffs$hunks)
  diff.l.p <- diff_line_len(res.print@diffs$hunks, res.print@etc)
  diff.l.s <- diff_line_len(res.str@diffs$hunks, res.str@etc)

  # How many lines of the input are in the diffs, vs how many lines of input

  diff.line.ratio.p <- lineCoverage(res.print)
  diff.line.ratio.s <- lineCoverage(res.str)

  # Only show the one with differences

  res <- if(!diff.s && diff.p) {
    res.print
  } else if(!diff.p && diff.s) {
    res.str

  # If one fits in full and the other doesn't, show the one that fits in full
  } else if(
    !res.str@trim.dat$lines[[1L]] &&
    res.print@trim.dat$lines[[1L]]
  ) {
    res.str
  } else if(
    res.str@trim.dat$lines[[1L]] &&
    !res.print@trim.dat$lines[[1L]]
  ) {
    res.print
  # Calculate the trade offs between the two options
  } else {
    s.score <- diff.s / diff.l.s * diff.line.ratio.s
    p.score <- diff.p / diff.l.p * diff.line.ratio.p
    if(p.score >= s.score) res.print else res.str
  }
  res
})
