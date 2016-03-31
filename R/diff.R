#' Compare R Objects with a Text Diff
#'
#' Compare R objects by computing a \code{diff} on their text representations
#' (e.g. the \code{print}ed console output).  This is similar to
#' \code{\link{tools::Rdiff}} except the diff is computed directly on R objects
#' instead of text files and does not rely on the system \code{diff} utility.
#'
#' @import crayon
#' @name diffobj-package
#' @docType package

NULL

#' Show Diffs Between the Screen Display Versions of Two Objects
#'
#' Highlights at a glance the \bold{display} differences between
#' two objects.  Lack of display differences is no guarantee that the objects
#' are the same.  Use \code{identical} or \code{all.equal} to confirm objects
#' are not different.  For basic usage we recommend you look at the examples.
#' If you are interested in the details, read on.
#'
#' @section Overview:
#'
#' \itemize{
#'   \item \code{diff_print} prints the objects, captures the output, and runs
#'     the diff on the captured output
#'   \item \code{diff_str} runs \code{str} on the objects, captures the output
#'     and runs the diff on the captured output.  If a \code{line.limit} is
#'     specified, it will attempt to find a \code{max.level} for which the
#'     output fits within the limit.  You can specify an explicit
#'     \code{max.level} to prevent this behavior (see \code{\link{str}}).  Note
#'     that using a \code{max.level} lower than the deepest nested level of an
#'     object may conceal display differences between objects.  You will be
#'     alerted to this if you did \bold{not} specify \code{max.level} yourself.
#'   \item \code{diff_obj} picks between \code{diff_print} and \code{diff_str}
#'     depending on which one it thinks will provide the most useful diff.
#'   \item \code{diff_chr} will run the diff directly on the actual character
#'     values provided, or those values coerced to character if they are not
#'     character.
#'   \item \code{diff_dep} will run the diff on the deparsed input objects
#' }
#' @section Output:
#'
#' The result of the diff provides the information necessary to transform the
#' \code{target} object into the \code{current} object.  This involves deletions
#' from and additions to \code{target}.  The deletions and additions are done
#' linewise.  Each deleted line will have \code{- } prepended to it, and each
#' added line will have \code{+ } prepended to it.  If your terminal supports
#' ANSI escape sequences the additions and deletions will be color coded.
#'
#' The first lines of output clarify the coding convention by showing the
#' \code{target} object with the deletion symbology, and the \code{current}
#' object with the addition symbology.  After these lines you will see the
#' first and possibly only hunk header.  The format will be \code{@@ x,y z,w @@}
#' where \code{x} and \code{z} indicate the starting line of the text in
#' the \code{target} and \code{current} objects that is shown after the hunk
#' header.  \code{y} and \code{w} indicate how many lines from each of those
#' objects are being shown.
#'
#' In addition to the primary line diff, hunks are themselves word-diffed within
#' each hunk to help quickly identify small differences.  Just keep in mind that
#' the \code{+-} symbols always relate to the original line diff.  The
#' word-diff is indicated only by the ANSI escape sequence styling and will not
#' be visible if your terminal does not support them or if you disable them.
#'
#' The output format used here is loosely based on the \code{git diff} format.
#'
#' @section Display Modes:
#'
#' You can control the diff display mode via the \code{mode} argument.  We
#' implement similar modes to those available in GNU diff:
#'
#' \itemize{
#'   \item unified: this is the diff mode used by \code{gitdiff}
#'   \item sidebyside: line up the differences side by side
#'   \item context: show the target and current hunks in their entirety; this
#'     mode takes up a lot of screen space but makes it easier to see what the
#'     objects actually look like
#' }
#' @section Atomic Vectors:
#'
#' When using \code{diff_print} the function will recognize the wrapped printed
#' output for normal atomic vectors, and will carry out the diff element by
#' element rather than line by line.  The \code{+-} diff indicators in the
#' gutters will still reference the line diffs, but additionally the element by
#' element matches and differences will be highlighted with ANSI style escape
#' sequences.
#'
#' @section Diff Algorithm:
#'
#' The diff algorithm is Myer's solution to the shortest edit script /
#' longest common sequence problem with the Hirschberg linear space refinement
#' as described in:
#' \cite{
#' E. Myers, \dQuote{An O(ND) Difference Algorithm and Its Variations},
#' Algorithmica 1, 2 (1986), 251-266.
#' \url{http://www.cs.arizona.edu/people/gene/PAPERS/diff.ps}
#' }
#' and should be the same algorithm used by GNU diff.  The implementation
#' used here is an adaptation of Michael B. Allen's diff program from the
#' \href{
#'    http://www.ioplex.com/~miallen/libmba/dl/libmba-0.9.1.tar.gz
#' }{\code{libmba}} \code{C} library.
#'
#' This algorithm scales with the \bold{square} of the number of differences
#' between compared objects so is most effective when comparing objects
#' that are mostly similar.
#'
#' @note: differences shown or reported by these functions may not be the
#'   totality of the differences between objects since display methods may not
#'   display all differences.  This is particularly true when using \code{str}
#'   for comparisons with \code{max.level} since differences inside unexpanded
#'   recursive levels will not be shown at all.
#' @export
#' @name diff_obj
#' @aliases diff_str, diff_print, diff_chr, diff_deparse
#' @param target the reference object
#' @param current the object being compared to \code{target}
#' @param context integer(1L) how many lines of context are shown on either side
#'   of differences, set to \code{-1L} to allow as many as possible.  Set to
#'   \dQuote{auto} to display as much context as possible without violating
#'   \code{line.limit}, or alternatively pass the return value of
#'   \code{link{auto_context}} to fine tune the parameters of the auto context
#'   calculation
#' @param ... additional arguments to pass on to \code{print}, etc.
#' @return a \code{\link{diffObjDiff}} object, invisibly.  This function is
#'   intended primarily to be used for its side-effects, but the return value
#'   is available so that you may display the diff differently without needing
#'   to recompute it (note: unfortunately the wrap diff is always recomputed
#'   on display)

NULL

# Because all these functions are so similar, we have constructed them in an
# odd fashion:
# - All the formals are defined in a separate dummy function `diff_tpl`
# - We copy this dummy function for each of our actual functions, and change
#   the bodies

diff_core <- function(
  call, capt, target, current, tar.exp, cur.exp, mode, context, line.limit,
  settings, ...
) {
  settings <- check_args(
    call=call, tar.exp=tar.exp, cur.exp=cur.exp, context=context,
    line.limit=line.limit, settings=settings
  )
  # Force crayon to whatever ansi status we chose; note we must do this after
  # touching vars in case someone passes `options(crayon.enabled=...)` as one
  # of the arguments

  old.crayon.opt <- options(crayon.enabled=settings@use.ansi)
  on.exit(options(old.crayon.opt), add=TRUE)
  err <- function(x) stop(simpleError(x, call=call))
  capt(target, current, settings=settings, err=err, ...)
}
# Note this one overwrites the entire body
#' @rdname diff_obj
#' @export

diff_obj <- function(
  target, current, mode=getOption("diffobj.mode"),
  context=getOption("diffobj.context"),
  line.limit=getOption("diffobj.line.limit"),
  settings=diffobj_settings(),
  ...
) {
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

  diff.p <- count_diffs(res.print@diffs$hunks)
  diff.s <- count_diffs(res.str@diffs$hunks)

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
    s.score <- with(res.str@trim.dat, {
      diff(lines) - lines[[1L]] + diff(diffs)
    })
    p.score <- with(res.print@trim.dat, {
      diff(lines) - .5 * lines[[1L]] + diff(diffs)
    })
    if(p.score >= s.score) res.print else res.str
  }
  res
}
#' @rdname diff_obj
#' @export

diff_print <- function(
  target, current, mode=getOption("diffobj.mode"),
  context=getOption("diffobj.context"),
  line.limit=getOption("diffobj.line.limit"),
  settings=diffobj_settings(),
  ...
) {
  diff_core(
    call=sys.call(), capt=capt_print, target=target, current=current,
    tar.exp=substitute(target), cur.exp=substitute(current),
    mode=mode, line.limit=line.limit, settings=settings, ...
  )
}
#' @rdname diff_obj
#' @export

diff_str <- function(
  target, current, mode=getOption("diffobj.mode"),
  context=getOption("diffobj.context"),
  line.limit=getOption("diffobj.line.limit"),
  settings=diffobj_settings(),
  ...
) {
  # Match original call and managed dots, in particular wrt to the
  # `max.level` arg

  diff_core(
    call=sys.call(), capt=capt_str, target=target, current=current,
    tar.exp=substitute(target), cur.exp=substitute(current),
    mode=mode, line.limit=line.limit, settings=settings, ...
  )
}
#' @rdname diff_obj
#' @export

diff_chr <- function(
  target, current, mode=getOption("diffobj.mode"),
  context=getOption("diffobj.context"),
  line.limit=getOption("diffobj.line.limit"),
  settings=diffobj_settings(),
  ...
) {}
  diff_core(
    call=sys.call(), capt=capt_chr, target=target, current=current,
    tar.exp=substitute(target), cur.exp=substitute(current),
    mode=mode, line.limit=line.limit, settings=settings, ...
  )
}
#' @rdname diff_obj
#' @export

diff_deparse <- diff_tpl; body(diff_deparse)[[13L]] <- quote({
  tar.capt <- strip_hz_control(deparse(target, ...), tab.stops)
  cur.capt <- strip_hz_control(deparse(current, ...), tab.stops)
})

