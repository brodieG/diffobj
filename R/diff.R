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
#'   \dQuote{"auto"} to display as much context as possible without violating
#'   \code{line.limit}, or alternatively pass the return value of
#'   \code{link{auto_context}} to fine tune the parameters of the auto context
#'   calculation
#' @param hunk.limit integer(2L) how many sections of differences to show.
#'   The first value is the maximum number of elements before we start trimming
#'   output.  The second value is how many elements to trim to.  If only one
#'   value is provided that value is used for the initial threshold as well as
#'   the limit to trim to.  If both values are provided the second must be
#'   smaller than the first.  Set to \code{-1L} or \code{c(-1L, -1L)} to run
#'   without limits
#' @param line.limit integer(2L) how many lines of screen output to show.
#'   Behaves like \code{hunk.limit}
#' @param use.ansi TRUE or FALSE, whether to use ANSI escape sequences to color
#'   differences (TRUE by default if we detect that your terminal supports it)
#' @param white.space TRUE or FALSE, whether to consider differences in
#'   horizontal whitespace (i.e. spaces and tabs) as differences (defaults to
#'   FALSE)
#' @param disp.width integer(1L) number of display columns to take up; note that
#'   in \dQuote{sidebyside} mode the effective display width is half this number
#' @param tar.banner character(1L) or NULL, used to clarify the symbology of the
#'   diff output (see the \dQuote{Output} section in the docs), if NULL will be
#'   inferred from \code{target} and \code{current} expressions
#' @param cur.banner character(1L) like \code{tar.banner}, but for \code{current}
#' @param frame environment the evaluation frame for the \code{print/show/str},
#'   calls, allows user to ensure correct methods are used, not used by
#'   \code{diff_chr} or \code{diff_deparse}
#' @param silent TRUE or FALSE, whether to display the diff (FALSE by default)
#' @param allow.in.hunk.diff TRUE or FALSE, whether to do a secondary diff on
#'   each hunk to highlight word differences (TRUE by default).  May be
#'   expensive if your objects has lots of nearby differences that end up
#'   aggregated in large hunks.
#' @param allow.wrap.diff TRUE whether to use a wrapping word diff for atomic
#'   vectors (see \dQuote{"Atomic Vectors"} section above)
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

diff_tpl <- function(
  target, current, mode=getOption("diffobj.mode"),
  context=getOption("diffobj.context"),
  hunk.limit=getOption("diffobj.hunk.limit"),
  line.limit=getOption("diffobj.line.limit"),
  ignore.white.space=getOption("diffobj.ignore.white.space"),
  use.ansi=getOption("diffobj.use.ansi"),
  disp.width=getOption("width"),
  tar.banner=NULL, cur.banner=NULL,
  silent=getOption("diffobj.silent"),
  max.diffs=getOption("diffobj.max.diffs"),
  max.diffs.in.hunk=getOption("diffobj.max.diffs.in.hunk"),
  max.diffs.wrap=getOption("diffobj.max.diffs.wrap"),
  tab.stops=getOption("diffobj.tab.stops"),
  frame=parent.frame(),
  ...
) {
  # Sub expressions before we touch any of the variables

  tar.exp <- substitute(target)
  cur.exp <- substitute(current)

  # Touch all the formals in case user passed an expression that evaluates
  # to error; then check them

  for(i in names(formals())) environment()[[i]]
  # this potentially modifies environment()
  check_args(environment(), sys.call())
  this.call <- sys.call()
  par.frame <- parent.frame()
  err <- function(...)
    stop(
      simpleError(do.call(paste0, c(list(...), collapse="")), call=this.call)
    )
  # Variables to populate by inserted code

  use.header <- FALSE
  tar.capt <- tar.capt.def <- cur.capt <- cur.capt.def <- diffs <- NULL

  # Force crayon to whatever ansi status we chose; note we must do this after
  # touching vars in case someone passes `options(crayon.enabled=...)` as one
  # of the arguments

  old.crayon.opt <- options(crayon.enabled=use.ansi)
  on.exit(options(old.crayon.opt))

  # line where we will insert code; each of the different `diff_*` functions
  # will have different logic here

  NULL

  # Finalize stuff

  if(is.null(diffs)) diffs <- char_diff(
    tar.capt, cur.capt, context=context, ignore.white.space=ignore.white.space,
    mode=mode, hunk.limit=hunk.limit, line.limit=line.limit,
    disp.width=disp.width, max.diffs=max.diffs, tab.stops=tab.stops,
    diff.mode="line", warn=TRUE, use.header=use.header
  )
  if(is.null(tar.banner)) tar.banner <- deparse(tar.exp)[[1L]]
  if(is.null(cur.banner)) cur.banner <- deparse(cur.exp)[[1L]]

  # Create the output structures, and display if requested

  vars <- as.list(environment())
  slot.args <- vars[names(vars) %in% names(getSlots("diffObjDiff"))]
  res <- do.call("new", c(list("diffObjDiff"), slot.args))
  res.chr <- as.character(res)
  slot(res, "trim.dat") <- attr(res.chr, "meta")
  if(!silent) {
    screen.lines <- as.integer(Sys.getenv("LINES"))[[1L]]
    if(is.na(screen.lines) || screen.lines < 1L) screen.lines <- 48L
    if(length(res.chr) / screen.lines > 1.5) {
      disp.f <- tempfile()
      on.exit(add=TRUE, unlink(disp.f))
      writeLines(res.chr, disp.f)
      if(pager_is_less() && use.ansi) {
        old.less <- set_less_var("R")
        on.exit(reset_less_var(old.less), add=TRUE)
      }
      file.show(disp.f)
    } else cat(res.chr, sep="\n")
  }
  invisible(res)
}
# Note this one overwrites the entire body
#' @rdname diff_obj
#' @export

diff_obj <- diff_tpl; body(diff_obj) <- quote({
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
  if(!silent) cat(as.character(res), sep="\n")
  invisible(res)
})
#' @rdname diff_obj
#' @export

diff_print <- diff_tpl; body(diff_print)[[13L]] <- quote({
  # capture normal prints, along with default prints to make sure that if we
  # do try to wrap an atomic vector print it is very likely to be in a format
  # we are familiar with and not affected by a non-default print method

  dots <- list(...)
  print.match <- try(
    match.call(
      get("print", envir=frame),
      as.call(c(list(quote(print), x=NULL), dots)),
      envir=frame
  ) )
  names(print.match)[[2L]] <- ""
  tar.call <- cur.call <- print.match

  if(length(dots)) {
    tar.call[[2L]] <- tar.exp
    cur.call[[2L]] <- cur.exp
    tar.banner <- deparse(tar.call)[[1L]]
    cur.banner <- deparse(cur.call)[[1L]]
  } else {
    tar.banner <- deparse(tar.exp)[[1L]]
    cur.banner <- deparse(cur.exp)[[1L]]
  }
  tar.call[[2L]] <- target
  cur.call[[2L]] <- current

  tar.call.def <- tar.call
  cur.call.def <- cur.call
  tar.call.def[[1L]] <- cur.call.def[[1L]] <- base::print.default

  both.at <- is.atomic(current) && is.atomic(target)
  capt.width <- calc_width(disp.width, mode) - 2L
  cur.capt <-
    strip_hz_control(capt_call(cur.call, capt.width, frame), tab.stops)
  cur.capt.def <- if(both.at)
    strip_hz_control(capt_call(cur.call.def, capt.width, frame), tab.stops)
  tar.capt <-
    strip_hz_control(capt_call(tar.call, capt.width, frame), tab.stops)
  tar.capt.def <- if(both.at)
    strip_hz_control(capt_call(tar.call.def, capt.width, frame), tab.stops)

  # For table-like objects always show first row
  use.header <- length(dim(target)) == 2L && length(dim(current)) == 2L
})
#' @rdname diff_obj
#' @export

diff_str <- diff_tpl; body(diff_str)[[13L]] <- quote({
  # Match original call and managed dots, in particular wrt to the
  # `max.level` arg

  dots <- list(...)
  if("object" %in% names(dots))
    err("You may not specify `object` as part of `...`")

  str.match <- try(
    match.call(
      str_tpl,
      call=as.call(c(list(quote(str), object=NULL), dots)), envir=frame
  ) )
  names(str.match)[[2L]] <- ""

  # Utility function; defining in body so it has access to `err`

  eval_try <- function(match.list, index, envir)
    tryCatch(
      eval(match.list[[index]], envir=envir),
      error=function(e)
        err("Error evaluating `", index, "` arg: ", conditionMessage(e))
    )
  # Setup / process extra args

  auto.mode <- FALSE
  max.level.supplied <- FALSE
  if(
    max.level.pos <- match("max.level", names(str.match), nomatch=0L)
  ) {
    # max.level specified in call; check for special 'auto' case
    res <- eval_try(str.match, "max.level", par.frame)
    if(identical(res, "auto")) {
      auto.mode <- TRUE
      str.match[["max.level"]] <- NA
    } else {
      max.level.supplied <- TRUE
    }
  } else {
    str.match[["max.level"]] <- NA
    auto.mode <- TRUE
    max.level.pos <- length(str.match)
    max.level.supplied <- FALSE
  }
  # Was wrap specified in strict width mode?

  wrap <- FALSE
  if("strict.width" %in% names(str.match)) {
    res <- eval_try(str.match, "strict.width", par.frame)
    wrap <- is.character(res) && length(res) == 1L && !is.na(res) &&
      nzchar(res) && identical(res, substr("wrap", 1L, nchar(res)))
  }
  if(auto.mode) {
    msg <-
      "Specifying `%s` may cause `str` output level folding to be incorrect"
    if("comp.str" %in% names(str.match)) warning(sprintf(msg, "comp.str"))
    if("indent.str" %in% names(str.match)) warning(sprintf(msg, "indent.str"))
  }
  # don't want to evaluate target and current more than once, so can't eval
  # tar.exp/cur.exp, so instead run call with actual object

  tar.call <- cur.call <- str.match
  tar.call[[2L]] <- target
  cur.call[[2L]] <- current

  # Run str

  capt.width <- calc_width_pad(disp.width, mode)
  has.diff <- has.diff.prev <- FALSE

  tar.capt <- strip_hz_control(
    capt_call(tar.call, capt.width, frame), tab.stops
  )
  tar.lvls <- str_levels(tar.capt, wrap=wrap)
  cur.capt <- strip_hz_control(
    capt_call(cur.call, capt.width, frame), tab.stops
  )
  cur.lvls <- str_levels(cur.capt, wrap=wrap)

  # note list_depth for some mysterious reason doesn't quite line up with
  # display of `str` when there are formulas as attributes, so just adding
  # + 2L

  prev.lvl.hi <- lvl <- max.depth <- max(tar.lvls, cur.lvls)
  prev.lvl.lo <- 0L
  first.loop <- TRUE
  safety <- 0L
  warn <- TRUE

  repeat{
    if((safety <- safety + 1L) > max.depth && !first.loop)
      stop(
        "Logic Error: exceeded list depth when comparing structures; contact ",
        "maintainer."
      )
    tar.str <- tar.capt[tar.lvls <= lvl]
    cur.str <- cur.capt[cur.lvls <= lvl]

    diffs.str <- char_diff(
      tar.str, cur.str, context=context,
      ignore.white.space=ignore.white.space, mode=mode, hunk.limit=hunk.limit,
      line.limit=line.limit, disp.width=disp.width, max.diffs=max.diffs,
      tab.stops=tab.stops, diff.mode="line", warn=warn
    )
    if(diffs.str$hit.diffs.max) warn <- FALSE
    has.diff <- any(
      !vapply(
        unlist(diffs.str$hunks, recursive=FALSE), "[[", logical(1L), "context"
    ) )
    if(first.loop) {
      tar.str.max <- tar.str
      cur.str.max <- cur.str
      diffs.max <- diffs.str
      first.loop <- FALSE

      # If there are no differences reducing levels isn't going to help to
      # find one; additionally, if not in auto.mode we should not be going
      # through this process

      if(!has.diff || !auto.mode) break
    }
    if(line.limit[[1L]] < 1L) break

    line.len <- diff_line_len(diffs.str$hunks, mode, disp.width)

    # We need a higher level if we don't have diffs

    if(!has.diff && prev.lvl.hi - lvl > 1L) {
      prev.lvl.lo <- lvl
      lvl <- lvl + as.integer((prev.lvl.hi - lvl) / 2)
      tar.call[[max.level.pos]] <- lvl
      cur.call[[max.level.pos]] <- lvl
      next
    } else if(!has.diff) {
      tar.str <- tar.str.max
      cur.str <- cur.str.max
      diffs.str <- diffs.max
      break
    }
    # If we have diffs, need to check whether we should try to reduce lines
    # to get under line limit

    if(line.len <= line.limit[[1L]]) {
      # We fit, nothing else to do
      break
    }
    if(lvl - prev.lvl.lo > 1L) {
      prev.lvl.hi <- lvl
      lvl <- lvl - as.integer((lvl - prev.lvl.lo) / 2)
      tar.call[[max.level.pos]] <- lvl
      cur.call[[max.level.pos]] <- lvl
      next
    }
    # Couldn't get under limit, so use first run results

    tar.str <- tar.str.max
    cur.str <- cur.str.max
    diffs.str <- diffs.max
    lvl <- NULL
    break
  }
  diffs <- diffs.str
  diffs$diffs.max <- count_diffs(diffs.max$hunks)

  if(auto.mode) {
    str.match[[max.level.pos]] <- lvl
  } else if (!max.level.supplied) {
    str.match[[max.level.pos]] <- NULL
  }
  tar.call <- cur.call <- str.match
  tar.call[[2L]] <- tar.exp
  cur.call[[2L]] <- cur.exp
  if(is.null(tar.banner)) tar.banner <- deparse(tar.call)[[1L]]
  if(is.null(cur.banner)) cur.banner <- deparse(cur.call)[[1L]]
} )
#' @rdname diff_obj
#' @export

diff_chr <- diff_tpl; body(diff_chr)[[13L]] <- quote({
  tar.capt <- strip_hz_control(
    if(!is.character(target)) as.character(target) else target, tab.stops
  )
  cur.capt <- strip_hz_control(
    if(!is.character(current)) as.character(current) else current, tab.stops
  )
})
#' @rdname diff_obj
#' @export

diff_deparse <- diff_tpl; body(diff_deparse)[[13L]] <- quote({
  tar.capt <- strip_hz_control(deparse(target, ...), tab.stops)
  cur.capt <- strip_hz_control(deparse(current, ...), tab.stops)
})

