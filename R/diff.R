#' @include s4.R
#' @include misc.R

NULL

#' Compare R Objects with a Text Diff
#'
#' Compare R objects by computing a \code{diff} on their text representations
#' (e.g. the \code{print}ed console output).  This is similar to
#' \code{\link{tools::Rdiff}} except the diff is computed directly on R objects
#' instead of text files and does not rely on the system \code{diff} utility.
#'
#' @import ansistyle
#' @name diffobj-package
#' @docType package

NULL

#' @rdname diffobj_s4method_doc

setMethod("any", "diffObjDiff",
  function(x, ..., na.rm = FALSE) {
    dots <- list(...)
    if(length(dots))
      stop("`any` method for `diffObjDiff` supports only one argument")
    any(x@diffs)
} )
#' @rdname diffobj_s4method_doc

setMethod("any", "diffObjDiffDiffs",
  function(x, ..., na.rm = FALSE) {
    dots <- list(...)
    if(length(dots))
      stop("`any` method for `diffObjDiff` supports only one argument")
    any(
      which(
        !vapply(unlist(x@hunks, recursive=FALSE), "[[", logical(1L), "context")
    ) )
} )
# Mostly replaced by Rdiff_x funs; tbd whether we get rid of this or update the
# Rdiff functions to use diff directly

diff_rdiff <- function(target, current) {
  stopifnot(is.character(target), is.character(current))
  a <- tempfile("diffObjRdiffa")
  writeLines(target, a)
  b <- tempfile("diffObjRdiffb")
  writeLines(current, b)
  diff <- capture.output(system(paste("diff -bw", shQuote(a), shQuote(b))))
}
# Try to use fancier word matching with vectors and matrices

.brack.pat <- "^ *\\[\\d+\\]"

# Determine if a string contains what appear to be standard index headers
#
# Returns index of elements in string that start with index headers.
# Note that it is permissible to have ouput that doesn't match brackets
# provided that it starts with brackets (e.g. attributes shown after break
# pattern)

find_brackets <- function(x) {
  stopifnot(is.character(x), all(!is.na(x)))
  matches <- regexpr(.brack.pat,  x)
  vals <- regmatches(x, matches)
  # the matching section must be uninterrupted starting from first line
  # and must have consisten formatting

  brackets <- which(cumsum(!nzchar(vals)) == 0L)
  vals.in.brk <- vals[brackets]
  nums.in.brk <- regmatches(vals.in.brk, regexpr("\\d+", vals.in.brk))

  if(
    length(brackets) && length(unique(nchar(vals.in.brk)) == 1L) &&
    length(unique(diff(as.integer(nums.in.brk)))) <= 1L
  ) {
    brackets
  } else integer(0L)
}
# Apply diff algorithm within lines
#
# For each line, splits into words, runs diffs, and colors them appropriately.
# For `across.lines=TRUE`, merges all lines into one and does the word diff on
# a single line to allow for the diff to look for matches across lines, though
# the result is then unwrapped back to the original lines.
#
# `match.quotes` will make "words" starting and ending with quotes; it should
# only be used if the objects are known to be attribute-less character vectors
# that are printed (as that is the only way we can know for sure how to match
# the quoted bits)

diff_word <- function(
  target, current, ignore.white.space, match.quotes=FALSE,
  use.ansi
) {
  stopifnot(
    is.character(target), is.character(current),
    all(!is.na(target)), all(!is.na(current)),
    is.TF(match.quotes),
    isTRUE(use.ansi) || identical(use.ansi, FALSE)
  )
  # Compute the char by char diffs for each line

  reg <- paste0(
    if(match.quotes) "((?<= )|(?<=^))\"([^\"]|\\\")*?\"((?= )|(?=$))|",
    "-?\\d+(\\.\\d+)?(e-?\\d{1,3})?",
    "|\\w+|\\d+|[^[:alnum:]_[:blank:]]+"
  )
  tar.reg <- gregexpr(reg, target, perl=TRUE)
  cur.reg <- gregexpr(reg, current, perl=TRUE)

  tar.split <- regmatches(target, tar.reg)
  cur.split <- regmatches(current, cur.reg)

  # Collapse into one line if to do the diff across lines, but record
  # item counts so we can reconstitute the lines at the end

  tar.lens <- vapply(tar.split, length, integer(1L))
  cur.lens <- vapply(cur.split, length, integer(1L))

  tar.split <- unlist(tar.split)
  cur.split <- unlist(cur.split)
  if(is.null(tar.split)) tar.split <- character(0L)
  if(is.null(cur.split)) cur.split <- character(0L)

  diffs <- char_diff(
    tar.split, cur.split, ignore.white.space=ignore.white.space,
    context=-1L, mode="context"
  )
  # Color

  diff.colored <- diffColor(diffs, use.ansi=use.ansi)
  tar.colored <- diff.colored$A
  cur.colored <- diff.colored$B

  # Reconstitute lines if needed

  tar.colored <- split(tar.colored, rep(seq_along(tar.lens), tar.lens))
  cur.colored <- split(cur.colored, rep(seq_along(cur.lens), cur.lens))

  # Merge back into original

  if(length(tar.colored)) regmatches(target, tar.reg) <- tar.colored
  if(length(cur.colored)) regmatches(current, cur.reg) <- cur.colored

  list(target=target, current=current)
}
# Apply line colors; returns a list with the A and B vectors colored,
# note that all hunks will be collapsed.
#
# Really only intended to be used for stuff that produces a single hunk

setGeneric("diffColor", function(x, ...) standardGeneric("diffColor"))
setMethod("diffColor", "diffObjDiffDiffs",
 function(x, use.ansi, ...) {
   res.l <- lapply(x@hunks,
     function(y) {
       lapply(y,
         function(z) {
           A <- z$A.chr
           B <- z$B.chr
           if(!z$context) {
             A[z$A < 0L] <- ansi_style(A[z$A < 0L], "green", use.style=use.ansi)
             A[z$A > 0L] <- ansi_style(A[z$A > 0L], "red", use.style=use.ansi)
             B[z$B < 0L] <- ansi_style(B[z$B < 0L], "green", use.style=use.ansi)
             B[z$B > 0L] <- ansi_style(B[z$B > 0L], "red", use.style=use.ansi)
           }
           list(A=A, B=B)
  } ) } )
  res.l <- unlist(res.l, recursive=FALSE)
  A <- unlist(lapply(res.l, "[[", "A"))
  B <- unlist(lapply(res.l, "[[", "B"))
  list(A=A, B=B)
} )

diff_color <- function(txt, diffs, range, color, use.ansi) {
  stopifnot(
    is.character(txt), is.logical(diffs), !any(is.na(diffs)),
    length(txt) == length(diffs), is.integer(range), !any(is.na(range)),
    all(range > 0 & range <= length(txt)), is.chr1(color)
  )
  to.color <- diffs & seq_along(diffs) %in% range
  txt[to.color] <- ansi_style(
    txt[to.color], color, use.style=use.ansi
  )
  txt
}
#' Show Diffs Between the Screen Display Versions of Two Objects
#'
#' Highlights at a glance the \bold{display} differences between
#' two objects.  Lack of display differences is no guarantee that the objects
#' are the same.  Use \code{identical} or \code{all.equal} to confirm objects
#' are not different.  For basic usage see examples.  For details read on.
#'
#' @section Overview:
#'
#' \itemize{
#'   \item \code{diff_print} prints the objects, captures the output, and runs
#'     the diff on the captured output
#'   \item \code{diff_str} runs \code{str} on the objects, captures the output
#'     and runs the diff on the captured output.  This will show as many
#'     recursive levels as possible so long as the line limits are not exeeded,
#'     and if they are, as few as possible to show at least one error, provided
#'     you do not specify a \code{max.level} argument as part of \code{...}
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
#' be visible if your terminal does not support them or if you diable them.
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
#'   of differences.
#' @param hunk.limit integer(2L) how many sections of differences to show.
#'   The first value is the maximum number of elements before we start trimming
#'   output.  The second value is how many elements to trim to.  If only one
#'   value is provided that value is used for the initial threshold as well as
#'   the limit to trim to.  If both values are provided the second must be
#'   smaller than the first.  Set to \code{-1L} or \code{c(-1L, -1L)} to run
#'   without limits.
#' @param line.limit integer(2L) how many lines of screen output to show.
#'   Behaves like \code{hunk.limit}
#' @param use.ansi TRUE or FALSE, whether to use ANSI escape sequences to color
#'   differences (TRUE by default if we detect that your terminal supports it)
#' @param white.space TRUE or FALSE, whether to consider differences in
#'   horizontal whitespace (i.e. spaces and tabs) as differences (defaults to
#'   FALSE)
#' @param disp.width integer(1L) number of display columns to take up; note that
#'   in \dQuote{sidebyside} mode the effective display width is halved from this
#'   number
#' @param tar.banner character(1L) used to clarify the symbology of the diff
#'   output (see the \dQuote{Output} section in the docs)
#' @param cur.banner character(1L) like \code{tar.banner}
#' @param frame environment the evaluation frame for the \code{print/show/str},
#'   calls, allows user to ensure correct methods are used, not used by
#'   \code{diff_chr} or \code{diff_deparse} though present as argument for
#'   simplicity
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
# - Within the bodies, make sure that the environment itself only has variables
#   defined that will be re-used in our instantiation of our diff object
#   (hence you'll see calls to `local` to avoid polluting the environment)

diff_tpl <- function(
  target, current, mode=getOption("diffobj.mode"),
  context=getOption("diffobj.context"),
  hunk.limit=getOption("diffobj.hunk.limit"),
  line.limit=getOption("diffobj.line.limit"),
  ignore.white.space=getOption("diffobj.ignore.white.space"),
  use.ansi=getOption("diffobj.use.ansi"),
  disp.width=getOption("width"),
  tar.banner=deparse(substitute(target))[[1L]],
  cur.banner=deparse(substitute(current))[[1L]],
  silent=getOption("diffobj.silent"),
  max.diffs=getOption("diffobj.max.diff"),
  max.diffs.in.hunk=getOption("diffobj.max.diff.in.hunk"),
  max.diffs.wrap=getOption("diffobj.max.diff.wrap"),
  frame=parent.frame(),
  ...
) {
  # Check arguments and update function environment with "fixed" args

  list2env(
    check_args(as.list(environment())), envir=environment()
  )
  tar.capt <- tar.capt.def <- cur.capt <- cur.capt.def <- NULL
  NULL         # line where we will insert code
  diffs <- char_diff(
    cur.capt, tar.capt, context=context, ignore.white.space=ignore.white.space,
    mode=mode
  )
  res <- do.call("new", c(list("diffObjDiff"), as.list(environment())))
  if(!silent) cat(as.character(res), sep="\n")
  invisible(res)
}
# Note this one overwrites the entire body
#' @rdname diff_obj
#' @export

diff_obj <- diff_tpl; body(diff_obj) <- quote({
  check_args()
  tar.capt <- tar.capt.def <- cur.capt <- cur.capt.def <- NULL
  vars <- as.list(environment())
  vars$silent <- TRUE
  res.print <- do.call("diff_print", vars)
  res.str <- do.call("diff_str", vars)
  len.print <- max(length(res.print@tar.capt), length(res.print@cur.capt))
  len.str <- max(length(res.str@tar.capt), length(res.str@cur.capt))
  # Choose which display to use; only favor res.str if it really is
  # substantially more compact and it does show an error and not possible to
  # show full print diff in context

  res <- if(
    (len.print <= len.max && any(res.print)) ||
    !any(res.str) ||
    (len.print < len.str * 3 && len.str > len.max)
  )
    res.print else res.str

  if(!silent) cat(as.character(res), sep="\n")
  invisible(res)
})
#' @rdname diff_obj
#' @export

diff_print <- diff_tpl; body(diff_print)[[3L]] <- quote({
  local({
    # capture normal prints, along with default prints to make sure that if we
    # do try to wrap an atomic vector print it is very likely to be in a format
    # we are familiar with and not affected by a non-default print method

    both.at <- is.atomic(current) && is.atomic(target)
    max.w <- calc_width(width, mode) - 2L
    cur.capt <<- obj_capt(current, max.w, frame, ...)
    cur.capt.def <<- if(both.at)
      obj_capt(current, max.w, frame, default=TRUE, ...)
    tar.capt <<- obj_capt(target, max.w, frame, ...)
    tar.capt.def <<- if(both.at)
      obj_capt(target, max.w, frame, default=TRUE, ...)
  })
})
#' @rdname diff_obj
#' @export

diff_str <- diff_tpl; body(diff_str)[[3L]] <- quote({
  local({
    obj.add.capt.str <- obj.rem.capt.str <- obj.add.capt.str.prev <-
      obj.rem.capt.str.prev <- character()

    prev.lvl <- 0L
    lvl <- 1L
    repeat{
      if(lvl > 100) lvl <- NA # safety valve
      obj.add.capt.str <-
        obj_capt(current, width - 3L, frame, mode="str", max.level=lvl)
      obj.rem.capt.str <-
        obj_capt(target, width - 3L, frame, mode="str", max.level=lvl)
      str.len.min <- min(length(obj.add.capt.str), length(obj.rem.capt.str))
      str.len.max <- max(length(obj.add.capt.str), length(obj.rem.capt.str))

      # Overshot full displayable size; check to see if previous iteration had
      # differences

      if(str.len.max > max.lines && lvl > 1L && any(diffs.str)) {
        obj.add.capt.str <- obj.add.capt.str.prev
        obj.rem.capt.str <- obj.rem.capt.str.prev
        break
      }
      # Other break conditions

      if(is.na(lvl) || lvl >= max.level) break
      if(
        identical(obj.add.capt.str.prev, obj.add.capt.str) &&
        identical(obj.rem.capt.str.prev, obj.rem.capt.str)
      ) {
        lvl <- prev.lvl
        break
      }
      # Run differences and iterate

      diffs.str <- char_diff(obj.rem.capt.str, obj.add.capt.str, white.space)
      obj.add.capt.str.prev <- obj.add.capt.str
      obj.rem.capt.str.prev <- obj.rem.capt.str
      prev.lvl <- lvl
      lvl <- lvl + 1
    }
    tar.capt <<- obj.rem.capt.str
    cur.capt <<- obj.add.capt.str
  })
})
#' @rdname diff_obj
#' @export

diff_chr <- diff_tpl; body(diff_chr)[[3L]] <- quote({
  tar.capt <- if(!is.character(target)) as.character(target) else target
  cur.capt <- if(!is.character(current)) as.character(current) else current
})
#' @rdname diff_obj
#' @export

diff_deparse <- diff_tpl; body(diff_deparse)[[3L]] <- quote({
  tar.capt <- deparse(target, ...)
  cur.capt <- deparse(current, ...)
})
# Capture output of print/show/str; unfortuantely doesn't have superb handling
# of errors during print/show call, though hopefully these are rare

obj_capt <- function(
  obj, width=getOption("width"), frame=parent.frame(), mode="print",
  max.level=0L, default=FALSE, ...
) {
  if(!is.numeric(width) || length(width) != 1L || is.na(width))
    stop("Argument `width` must be a one long numeric/integer.")
  if(
    !is.character(mode) || length(mode) != 1L || is.na(mode) ||
    !mode %in% c("print", "str")
  )
    stop("Argument `mode` must be one of \"print\" or \"str\"")
  # note this forces eval, which is needed
  if(!is.environment(frame))
    stop("Argument `frame` must be an environment")
  if(
    !is.na(max.level) && (
      !is.numeric(max.level) || length(max.level) != 1L ||  max.level < 0
    )
  )
    stop("Argument `max.level` must be integer(1L) and positive")

  max.level <- as.integer(max.level)
  width.old <- getOption("width")
  on.exit(options(width=width.old))
  width <- max(width, 10L)
  options(width=width)

  res <- try({
    extra <- NULL
    fun <- if(identical(mode, "print")) {
      if(isS4(obj)) quote(show) else quote(print)
    } else if(identical(mode, "str")) {
      extra <- list(max.level=max.level)
      quote(str)
    } else stop("Logic Error: unexpected mode; contact maintainer.")
    call <- as.call(c(list(fun, obj, `...`=...), extra))
  })
  res <- try(obj.out <- capture.output(eval(call, frame)))
  if(inherits(res, "try-error"))
    stop("Failed attempting to get text representation of object")

  options(width=width.old)
  on.exit(NULL)

  # remove trailing spaces; shouldn't have to do it but doing it since legacy
  # tests remove them and PITA to update those

  obj.out <- sub("\\s*$", "", obj.out)
  obj.out
}
# constructs the full diff message with additional meta information

obj_screen_chr <- function(
  obj.chr, obj.name, diffs, range, width, pad, color=NA_character_
) {
  stopifnot(is.chr1(pad))
  pre <- post <- NULL
  pad.all <- pad.pre.post <- NULL
  obj.name.dep <- deparse(obj.name)[[1L]]
  extra <- character()
  len.obj <- length(obj.chr)

  if(len.obj) {
    pad.all <- character(len.obj)
    pad.chars <- nchar(pad)
    if(!any(diffs)) {
      pad.all <- replicate(len.obj, paste0(rep(" ", pad.chars)), collapse="")
    } else {
      pad.all[diffs] <- pad
      pad.all <- format(pad.all)
    }
    pad.all[diffs] <- ansi_style(
      pad.all[diffs], style=color, use.style=getOption("diffobj.use.ansi")
    )
    pad.pre.post <- paste0(rep(" ", pad.chars), collapse="")

    omit.first <- max(min(range[[1L]] - 1L, len.obj), 0L)
    omit.last <- max(len.obj - tail(range, 1L), 0L)
    diffs.last <- sum(tail(diffs, -tail(range, 1L)))

    if(omit.first)
      pre <- paste0(
        "~~ omitted ", omit.first, " line", if(omit.first != 1L) "s",
        " w/o diffs"
      )
    if(omit.last) {
      post <- paste0(
        "~~ omitted ", omit.last, " line", if(omit.last != 1L) "s",
        if(diffs.last) paste0(" w/ ", diffs.last, " diff") else " w/o diff",
        if(diffs.last != 1L) "s"
    ) }
    if(!is.null(post)) {
      post <- ansi_style(
        paste0(pad.pre.post, paste0(post, extra, " ~~")),
        "silver", use.style=getOption("diffobj.use.ansi")
    ) }
    if (!is.null(pre)) {
      pre <- ansi_style(
        paste0(pad.pre.post, paste0(pre, if(is.null(post)) extra, " ~~")),
        "silver", use.style=getOption("diffobj.use.ansi")
    ) }
  }
  c(
    ansi_style(
      paste0("@@ ", obj.name.dep, " @@"), "cyan",
      use.style=getOption("diffobj.use.ansi")
    ),
    paste0(
      c(pre, paste0(pad.all, obj.chr)[range[range <= len.obj]], post)
    )
  )
}
