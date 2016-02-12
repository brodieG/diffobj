#' @include s4.R

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
#' @rdname diffobj_s4method_doc

setMethod("as.character", "diffObjDiff",
  function(x, hunk.limit, line.limit, width, use.ansi, mode, ...) {
    # These checks should never fail since presumably the inputs have been
    # checked earlier; here just in case we mess something up in devel or
    # testing

    context <- check_linelimit(line.limit)
    width <- check_width(width)
    mode <- check_mode(mode)
    white.space <- x@diffs@white.space

    len.max <- max(length(x@tar.capt), length(x@cur.capt))
    if(!any(x)) {
      msg <- "No visible differences between objects."
      if(!x@diffs@white.space && !identical(x@tar.capt, x@cur.capt)) {
        msg <- paste0(
          "Only visible differences between objects are horizontal white ",
          "spaces. You can re-run diff with `white.space=TRUE` to show them."
        )
      }
      return(
        ansi_style(
          msg, "silver",
          use.style=use.ansi
    ) ) }
    # Figure out which hunks we're going to try to render subject to the
    # line and hunk limits.  Remember

    # Trim hunks to the extent need to make sure we fit in lines; start by
    # dropping hunks beyond hunk limit

    hunk.grps <- trim_hunks(
      x@diffs@hunks, mode=mode, width=width, line.limit=line.limit,
      hunk.limit=hunk.limit, use.ansi=use.ansi
    )
    # Post trim, figure out max lines we could possibly be showing from capture
    # strings

    hunks.flat <- unlist(hunk.grps, recursive=FALSE)
    ranges <- vapply(
      hunks.flat, function(h.a)
        c(h.a$tar.rng.trim, h.a$cur.rng.trim),
      integer(4L)
    )
    tar.max <- max(ranges[2L, ])
    cur.max <- max(ranges[4L, ])

    # Detect whether we should attempt to deal with wrapping objects, if so
    # overwrite cur/tar.body/rest variables with the color diffed wrap word
    # diffs; note that if the tar/cur.capt.def is not NULL then the objects
    # being compared must be atomic vectors

    cur.body <- tar.body <- character(0L)
    cur.rest <- show.range.cur <- seq_len(cur.max)
    tar.rest <- show.range.tar <- seq_len(tar.max)

    if(
      identical(x@mode, "print") && identical(x@tar.capt, x@tar.capt.def) &&
      identical(x@cur.capt, x@cur.capt.def)
    ) {
      # Separate out the stuff that can wrap (starts with index headers vs. not),
      # and for that stuff only, apply the color word diff

      cur.head.raw <- find_brackets(x@cur.capt)
      tar.head.raw <- find_brackets(x@tar.capt)

      cur.head <- cur.head.raw[cur.head.raw %in% show.range.cur]
      tar.head <- tar.head.raw[tar.head.raw %in% show.range.tar]

      if(length(cur.head) && length(tar.head)) {
        # note we modify `x` here so that subsequent steps can re-use `x` with
        # modifications

        pat <- sprintf("%s\\K.*", .brack.pat)
        cur.body <- regexpr(pat, x@cur.capt[cur.head], perl=TRUE)
        tar.body <- regexpr(pat, x@tar.capt[tar.head], perl=TRUE)

        body.diff <- diff_word(
          regmatches(x@tar.capt[tar.head], tar.body),
          regmatches(x@cur.capt[cur.head], cur.body),
          across.lines=TRUE, white.space=white.space,
          match.quotes=is.chr.vec(x@tar.obj) && is.chr.vec(x@cur.obj)
        )
        regmatches(x@tar.capt[tar.head], tar.body) <- body.diff$target
        regmatches(x@cur.capt[cur.head], cur.body) <- body.diff$current

        # We also need to diff the row headers

        cur.r.h <- regexpr(.brack.pat, x@cur.capt[cur.head], perl=TRUE)
        tar.r.h <- regexpr(.brack.pat, x@tar.capt[tar.head], perl=TRUE)

        cur.r.h.txt <- regmatches(x@cur.capt[cur.head], cur.r.h)
        tar.r.h.txt <- regmatches(x@tar.capt[tar.head], tar.r.h)

        x.r.h <- new(
          "diffObjDiff", tar.capt=tar.r.h.txt, cur.capt=cur.r.h.txt,
          diffs=char_diff(tar.r.h.txt, cur.r.h.txt, white.space=white.space)
        )
        x.r.h.color <- diffColor(x.r.h, use.ansi=use.ansi)
        regmatches(x@tar.capt[tar.head], tar.r.h) <- x.r.h.color$A
        regmatches(x@cur.capt[cur.head], cur.r.h) <- x.r.h.color$B

        # Everything else gets a normal hunk by hunk word diff

        cur.rest <- show.range.cur[!show.range.cur %in% cur.head]
        tar.rest <- show.range.tar[!show.range.tar %in% tar.head]
      }
    }
    # Figure out which hunks are still eligible to be word diffed

    tar.to.wd <- min(which(ranges[1L,] %in% tar.rest), 0L)
    cur.to.wd <- min(which(ranges[3L,] %in% cur.rest), 0L)

    if(tar.to.wd && cur.to.wd) {
      wd.max <- max(tar.to.wd, cur.to.wd)

      for(i in seq_along(hunk.grps)) {
        for(j in seq_along(hunk.grps[[i]])) {
          h.a <- hunk.grps[[i]][[j]]
          if(h.a$id < wd.max || h.a$context) next
          # Do word diff on each non-context hunk

          h.a[c("A.chr", "B.chr")] <- diff_word(
            h.a$A.chr, h.a$B.chr, across.lines=TRUE, white.space=white.space
          )
          hunk.grps[[i]][[j]] <- h.a
    } } }
    # Make the object banner

    banner.A <- paste0("--- ", deparse(x@tar.exp)[[1L]])
    banner.B <- paste0("+++ ", deparse(x@cur.exp)[[1L]])

    if(mode == "sidebyside") {
      max.w <- max(floor(width / 2), 20L)
      comb.fun <- paste0
      t.fun <- rpadt
    } else {
      max.w <- max(width, 20L)
      comb.fun <- c
      t.fun <- chr_trim
    }
    banner <- comb.fun(
      ansi_style(t.fun(banner.A, max.w), "red", use.ansi),
      ansi_style(t.fun(banner.B, max.w), "green", use.ansi)
    )
    # Display hunks

    rng_as_chr <- function(range) paste0(range[[1L]], ",", diff(range))

    out <- lapply(
      hunk.grps, function(h.g) {
        # Get ranges DEVNOTE: NEED TO CHECK ATOMIC HUNK RANGES CONTIGUOUS

        tar.rng <- c(min(ranges[1L,]), max(ranges[2L,]))
        cur.rng <- c(min(ranges[3L,]), max(ranges[4L,]))

        hh.a <- paste0("-", rng_as_chr(tar.rng))
        hh.b <- paste0("+", rng_as_chr(cur.rng))

        hunk.head <- ansi_style(
          if(mode == "sidebyside") {
            paste0(rpadt(sprintf("@@ %s @@", c(hh.a, hh.b)), max.w), collapse="")
          } else {
            sprintf("@@ %s %s @@", hh.a, hh.b)
          },
          "cyan", use.ansi
        )
        # Output varies by mode

        diff.txt <- if(mode == "context") {
          # Need to get all the A data and the B data
          ctx <- vapply(h.g, "[[", logical(1L), "context")
          A <- wrap(unlist(lapply(h.g, "[[", "A.chr")), width, use.ansi=use.ansi)
          B <- wrap(unlist(lapply(h.g, "[[", "B.chr")), width, use.ansi=use.ansi)
          A[!ctx] <- sign_pad(A[!ctx], "- ", use.ansi=use.ansi)
          B[!ctx] <- sign_pad(B[!ctx], "+ ", use.ansi=use.ansi)
          A[ctx] <- sign_pad(A[ctx], "  ", use.ansi=use.ansi)
          B[ctx] <- sign_pad(B[ctx], "  ", use.ansi=use.ansi)
          unlist(A, ansi_style("----", "silver", use.ansi=use.ansi), B)
        } else if(mode == "unified") {
          unlist(
            lapply(h.g,
              function(h.a) {
                pos <- h.a$A > 0L
                A.out <- wrap(h.a$A.chr, width, use.ansi=use.ansi)
                if(!h.a$context) {
                  A.out[pos] <- sign_pad(A.out[pos], "- ", use.ansi=use.ansi)
                  A.out[!pos] <- sign_pad(A.out[!pos], "+ ", use.ansi=use.ansi)
                } else {
                  A.out <- sign_pad(A.out, "  ", use.ansi=use.ansi)
                }
                A.out
          } ) )
        } else if(mode == "sidebyside") {
          unlist(
            lapply(h.g,
              function(h.a) {
                # Ensure same number of elements in A and B

                A.out <- h.a$A.chr
                B.out <- h.a$B.chr
                len.diff <- length(A.out) - length(B.out)
                if(len.diff < 0L) {
                  A.out <- c(A.out, character(abs(len.diff)))
                } else if(len.diff) {
                  B.out <- c(B.out, character(len.diff))
                }
                A.w <- wrap(A.out, width, use.ansi=use.ansi, pad=TRUE)
                B.w <- wrap(B.out, width, use.ansi=use.ansi, pad=TRUE)

                # Same number of els post wrap

                A.lens <- vapply(A.out, length, integer(1L))
                B.lens <- vapply(B.out, length, integer(1L))
                blanks <- paste0(rep(" ", width), collapse="")

                for(i in seq_along(len.max)) {
                  if(A.lens < B.lens)
                    A.w[[i]] <- c(A.w[[i]], rep(blanks, B.lens - A.lens))
                  if(A.lens > B.lens)
                    B.w[[i]] <- c(B.w[[i]], rep(blanks, A.lens - B.lens))
                }
                paste0(
                  unlist(sign_pad(A.w, "- ", use.ansi=use.ansi, rev=TRUE)),
                  unlist(sign_pad(B.w, "+ ", use.ansi=use.ansi))
                )
        } ) ) }
        c(hunk.head, diff.txt)
    } )
    c(banner, unlist(out))
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
  target, current, across.lines=FALSE, white.space, match.quotes=FALSE,
  use.ansi
) {
  stopifnot(
    is.character(target), is.character(current),
    all(!is.na(target)), all(!is.na(current)),
    is.TF(across.lines),
    is.TF(match.quotes),
    across.lines || length(target) == length(current),
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

  # Collapse into one line if we want to do the diff across lines, but record
  # item counts so we can reconstitute the lines at the end

  if(across.lines) {
    tar.lens <- vapply(tar.split, length, integer(1L))
    cur.lens <- vapply(cur.split, length, integer(1L))

    tar.split <- list(unlist(tar.split))
    cur.split <- list(unlist(cur.split))
  }
  diffs <- mapply(
    char_diff, tar.split, cur.split, MoreArgs=list(
      white.space=white.space, context=-1L, mode="context"
    ),
    SIMPLIFY=FALSE
  )
  # Color

  diff.colored <- lapply(diffs, diffColor, use.ansi=use.ansi)
  tar.colored <- lapply(diff.colored, "[[", "A")
  cur.colored <- lapply(diff.colored, "[[", "B")

  # Reconstitute lines if needed; using across lines there should only be one
  # value

  if(across.lines) {
    tar.colored <- split(
      tar.colored[[1L]], rep(seq_along(tar.lens), tar.lens)
    )
    cur.colored <- split(
      cur.colored[[1L]], rep(seq_along(cur.lens), cur.lens)
    )
  }
  # Merge back into original

  regmatches(target, tar.reg) <- tar.colored
  regmatches(current, cur.reg) <- cur.colored

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
#' Designed to highlight at a glance the \bold{display} differences between
#' two objects.  Lack of visual differences is not guarantee that the objects
#' are the same.  These functions are designed to help you quickly understand
#' the nature of differences between objects when they are known to be different
#' (e.g. not \code{identical} or \code{all.equal}).  The diff algorithms are far
#' from perfect and in some cases will likely make seemingly odd choices on what
#' to highlight as being different.
#'
#' These functions focus on the first display difference between two objects.
#' If you want to see the full object diff try \code{\link{Rdiff_obj}}.
#'
#' \itemize{
#'   \item \code{diff_print} shows the differences in the \code{print} or
#'     \code{show} screen output of the two objects
#'   \item \code{diff_str} shows the differences in the \code{str} screen output
#'     of the two objects; will show as many recursive levels as possible so
#'     long as context lines are not exceeded, and if they are, as few as
#'     possible to show at least one error (see \code{max.level})
#'   \item \code{diff_obj} picks between \code{diff_print} and \code{diff_str}
#'     depending on which one it thinks will provide the most useful diff
#' }
#' @note: differences shown or reported by these functions may not be the
#'   totality of the differences between objects since display methods may not
#'   display all differences.  This is particularly true when using \code{str}
#'   for comparisons with \code{max.level} since differences inside unexpanded
#'   recursive levels will not be shown at all.
#' @export
#' @aliases diff_str, diff_print, diff_chr
#' @param target the reference object
#' @param current the object being compared to \code{target}
#' @param context 2 length integer vector representing how many lines of context
#'   are shown on either side of differences.  The first value is the maximum
#'   before we start trimming output.  The second value is the maximum to be
#'   shown before we start trimming.  We will always attempt to show as much as
#'   \code{2 * context + 1} lines of output so context may not be centered if
#'   objects display as less than \code{2 * context + 1} lines.
#' @param white.space TRUE or FALSE, whether to consider differences in
#'   horizontal whitespace (i.e. spaces and tabs) as differences (defaults to
#'   FALSE)
#' @param max.level integer(1L) up to how many levels to try running \code{str};
#'   \code{str} is run repeatedly starting with \code{max.level=1} and then
#'   increasing \code{max.level} until we fill the context or a difference
#'   appears or the \code{max.level} specified here is reached.  If the value is
#'   reached then will let \code{str} run with \code{max.level} unspecified.
#'   This is designed to produce the most compact screen output possible that
#'   shows the differences between objects, though obviously it comes at a
#'   performance cost; set to 0 to disable
#' @return character, invisibly, the text representation of the diff

diff_obj <- function(target, current, context=NULL, white.space=FALSE) {
  context <- check_context(context)
  frame <- parent.frame()
  width <- getOption("width")

  diff_obj_internal(
    target, current, tar.exp=substitute(target), cur.exp=substitute(current),
    context=context, frame=frame, width=width, white.space=white.space
  )
}
#' @export

diff_print <- function(target, current, context=NULL, white.space=FALSE) {
  context <- check_context(context)
  width <- getOption("width")
  frame <- parent.frame()
  res <- as.character(
    diff_print_internal(
      target, current, tar.exp=substitute(target), frame=frame,
      cur.exp=substitute(current), context=context, width=width,
      white.space=white.space
    ),
    context=context,
    width=width
  )
  cat(res, sep="\n")
  invisible(res)
}
#' @export

diff_str <- function(
  target, current, context=NULL, white.space=FALSE, max.level=10
) {
  width <- getOption("width")
  frame <- parent.frame()
  res <- as.character(
    diff_str_internal(
      target, current, tar.exp=substitute(target),
      cur.exp=substitute(current), context=context, width=width,
      frame=frame, max.lines=NULL, max.level=max.level,
      white.space=white.space
    ),
    context=context,
    width=width
  )
  cat(res, sep="\n")
  invisible(res)
}
#' @export

diff_chr <- function(
  target, current, context=getOption("diffobj.context"),
  white.space=getOption("diffobj.white.space"),
  hunk.limit=getOption("diffobj.hunk.limit"),
  line.limit=getOption("diffobj.line.limit"),
  mode=getOption("diffobj.mode"),
  use.ansi=getOption("diffobj.use.ansi")
) {
  tar.exp <- substitute(target)
  cur.exp <- substitute(current)

  if(!isTRUE(white.space) && !identical(white.space, FALSE))
    stop("Argument `white.space` must be TRUE or FALSE")
  context <- check_context(context)
  line.limit <- check_linelimit(line.limit)
  hunk.limit <- check_hunklimit(hunk.limit)
  mode <- check_mode(mode)
  width <- getOption("width")

  if(!is.character(target)) target <- as.character(target)
  if(!is.character(current)) current <- as.character(current)
  diffs <- char_diff(
    target, current, context=context, white.space=white.space, mode=mode
  )
  diffObj <- new(
    "diffObjDiff", tar.obj=target, cur.obj=current, tar.capt=target,
    cur.capt=current, tar.exp=tar.exp, cur.exp=cur.exp, diffs=diffs,
    mode="print", tar.capt.def=NULL, cur.capt.def=NULL
  )
  res <- as.character(
    diffObj, line.limit=line.limit, hunk.limit=hunk.limit, width=width,
    mode=mode, use.ansi=use.ansi
  )
  cat(res, sep="\n")
  invisible(res)
}
# Implements the diff_* functions
#
# @keywords internal
# @inheritParams diff_obj
# @param tar.exp the substituted target expression
# @param cur.exp the substituted current expression
# @param width at what width to wrap output
# @param file whether to show to stdout or stderr
# @param frame what frame to capture in, relevant mostly if looking for a print
#   method

diff_print_internal <- function(
  target, current, tar.exp, cur.exp, context, width, frame, white.space
) {
  # capture normal prints, along with default prints to make sure that if we
  # do try to wrap an atomic vector print it is very likely to be in a format
  # we are familiar with and not affected by a non-default print method
  if(!is.TF(white.space))
    stop("Argument `white.space` must be TRUE or FALSE")

  both.at <- is.atomic(current) && is.atomic(target)
  cur.capt <- obj_capt(current, width - 3L, frame)
  cur.capt.def <- if(both.at) obj_capt(current, width - 3L, frame, default=TRUE)
  tar.capt <- obj_capt(target, width - 3L, frame)
  tar.capt.def <- if(both.at) obj_capt(target, width - 3L, frame, default=TRUE)

  # Run basic diff

  diffs <- char_diff(tar.capt, cur.capt, white.space=white.space)

  new(
    "diffObjDiff", tar.obj=target, cur.obj=current, tar.capt=tar.capt,
    cur.capt=cur.capt, tar.exp=tar.exp, cur.exp=cur.exp, diffs=diffs,
    mode="print", tar.capt.def=tar.capt.def, cur.capt.def=cur.capt.def
  )
}
diff_str_internal <- function(
  target, current, tar.exp, cur.exp, context, width, frame, max.lines,
  max.level=10, white.space
) {
  context <- check_context(context)
  if(is.null(max.lines)) {
    max.lines <- context[[1L]] * 2L + 1L
  } else if(!is.int.1L(max.lines) || !max.lines)
    stop("Argument `max.lines` must be integer(1L) and not zero.")
  if(max.lines < 0) max.lines <- Inf
  if(!is.int.1L(max.level))
    stop("Argument `max.level` must be integer(1L) and GTE zero.")
  if(max.level > 100)
    stop("Argument `max.level` cannot be greater than 100")
  if(!is.TF(white.space))
    stop("Argument `white.space` must be TRUE or FALSE")

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
  diffs <- char_diff(obj.rem.capt.str, obj.add.capt.str, white.space)
  tar.exp <- call("str", tar.exp, max.level=lvl)
  cur.exp <- call("str", cur.exp, max.level=lvl)
  new(
    "diffObjDiff", tar.obj=target, cur.obj=current, tar.capt=obj.rem.capt.str,
    cur.capt=obj.add.capt.str, tar.exp=tar.exp, cur.exp=cur.exp, diffs=diffs,
    mode="str"
  )
}
# Unlike diff_print_internal and diff_str_internal, this one prints to screen
# and invisibly returns the result

diff_obj_internal <- function(
  target, current, tar.exp=substitute(target),
  cur.exp=substitute(current), context=NULL, width=NULL,
  frame=parent.frame(), max.level=10L, file=stdout(), white.space
) {
  context <- check_context(context)
  width <- check_width(width)
  if(!isTRUE(file.err <- is.open_con(file, writeable=TRUE)))
    stop("Argument `file` is not valid because: ", file.err)
  if(!is.environment(frame)) stop("Argument `frame` must be an environment.")

  res.print <- diff_print_internal(
    target, current, tar.exp=tar.exp, cur.exp=cur.exp, context=context,
    width=width, frame=frame, white.space=white.space
  )
  len.print <- max(length(res.print@tar.capt), length(res.print@cur.capt))

  res.str <- diff_str_internal(
    target, current, tar.exp=tar.exp,
    cur.exp=cur.exp, context=context, width=width,
    frame=frame, max.lines=len.print, white.space=white.space
  )
  len.max <- context[[1L]] * 2 + 1
  len.str <- max(length(res.str@tar.capt), length(res.str@cur.capt))

  # Choose which display to use; only favor res.str if it really is substantially
  # more compact and it does show an error and not possible to show full print
  # diff in context

  res <- if(
    (len.print <= len.max && any(res.print)) ||
    !any(res.str) ||
    (len.print < len.str * 3 && len.str > len.max)
  )
    res.print else res.str

  res.chr <- as.character(res, context, width=width)
  cat(res.chr, file=file, sep="\n")
  invisible(res.chr)
}


# Capture output of print/show/str; unfortuantely doesn't have superb handling
# of errors during print/show call, though hopefully these are rare

obj_capt <- function(
  obj, width=getOption("width"), frame=parent.frame(), mode="print",
  max.level=0L, default=FALSE
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
    call <- as.call(c(list(fun, obj), extra))
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
