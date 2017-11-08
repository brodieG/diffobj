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

#' @include misc.R
#' @include styles.R
#' @include pager.R

NULL

# S4 class definitions

setClassUnion("charOrNULL", c("character", "NULL"))

#' Dummy Doc File for S4 Methods with Existing Generics
#'
#' @keywords internal
#' @name diffobj_s4method_doc
#' @rdname diffobj_s4method_doc

NULL

#' Controls How Lines Within a Diff Hunk Are Aligned
#'
#' @slot threshold numeric(1L) between 0 and 1, what proportion of words
#'   in the lines must match in order to align them.  Set to 1 to effectively
#'   turn aligning off.  Defaults to 0.25.
#' @slot min.chars integer(1L) positive, minimum number of characters that must
#'   match across lines in order to align them.  This requirement is in addition
#'   to \code{threshold} and helps minimize spurious alignments.  Defaults to
#'   3.
#' @slot count.alnum.only logical(1L) modifier for \code{min.chars}, whether to
#'   count alpha numeric characters only.  Helps reduce spurious alignment
#'   caused by meta character sequences such as \dQuote{[[1]]} that would
#'   otherwise meet the \code{min.chars} limit
#' @export AlignThreshold
#' @exportClass AlignThreshold
#' @examples
#' a1 <- AlignThreshold(threshold=0)
#' a2 <- AlignThreshold(threshold=1)
#' a3 <- AlignThreshold(threshold=0, min.chars=2)
#' ## Note how "e f g" is aligned
#' diffChr(c("a b c e", "d e f g"), "D e f g", align=a1, pager="off")
#' ## But now it is not
#' diffChr(c("a b c e", "d e f g"), "D e f g", align=a2, pager="off")
#' ## "e f" are not enough chars to align
#' diffChr(c("a b c", "d e f"), "D e f", align=a1, pager="off")
#' ## Override with min.chars, so now they align
#' diffChr(c("a b c", "d e f"), "D e f", align=a3, pager="off")

AlignThreshold <- setClass("AlignThreshold",
  slots=c(
    threshold="numeric",
    min.chars="integer",
    count.alnum.only="logical"
  ),
  validity=function(object) {
    if(
      length(object@threshold) != 1L || is.na(object@threshold) ||
      !object@threshold %bw% c(0, 1)
    )
      return("Slot `threhold` must be numeric(1L) between 0 and 1")
    if(!is.int.1L(object@min.chars) || object@min.chars < 0L)
      return("Slot `min.chars` must be integer(1L) and positive")
    if(!is.TF(object@count.alnum.only))
      return("Slot `count.alnum.only` must be TRUE or FALSE")
  }
)
setMethod(
  "initialize", "AlignThreshold",
  function(
    .Object, threshold=gdo("align.threshold"), min.chars=gdo("align.min.chars"),
    count.alnum.only=gdo("align.count.alnum.only"), ...
  ) {
    if(is.numeric(min.chars)) min.chars <- as.integer(min.chars)
    callNextMethod(
      .Object, threshold=threshold, min.chars=min.chars,
      count.alnum.only=count.alnum.only, ...
    )
} )

setClass("AutoContext",
  slots=c(
    min="integer",
    max="integer"
  ),
  validity=function(object) {
    if(!is.int.1L(object@max) || object@min < 0L)
      return("Slot `max` must be integer(1L), positive, and not NA")
    if(!is.int.1L(object@max))
      return("Slot `max` must be integer(1L), and not NA")
    if(object@max > 0L && object@min > object@max)
      return("Slot `max` must be negative, or greater than slot `min`")
    TRUE
} )
setClassUnion("doAutoCOrInt", c("AutoContext", "integer"))
# pre-computed gutter data

GuideLines <- setClass(
  "GuideLines",
  slots=c(target="integer", current="integer"),
  validity=function(object) {
    vals <- c(object@target, object@current)
    if(anyNA(vals) || any(vals < 1L))
      return("Object may only contain strictly positive integer values")
    TRUE
  }
)
setClass("StripRowHead", slots=c(target="ANY", current="ANY"),
  validity=function(object) {
    if(!isTRUE(err <- is.one.arg.fun(object@target)))
      return(err)
    if(!isTRUE(err <- is.one.arg.fun(object@current)))
      return(err)
    TRUE
  }
)
setClass("Gutter",
  slots= c(
    insert="character", insert.ctd="character",
    delete="character", delete.ctd="character",
    match="character", match.ctd="character",
    guide="character", guide.ctd="character",
    fill="character", fill.ctd="character",
    context.sep="character", context.sep.ctd="character",
    pad="character", width="integer"
  )
)
setClass("Settings",
  slots=c(
    mode="character",             # diff output mode
    context="doAutoCOrInt",
    line.limit="integer",
    style="Style",
    hunk.limit="integer",
    max.diffs="integer",
    word.diff="logical",
    unwrap.atomic="logical",
    align="AlignThreshold",
    ignore.white.space="logical",
    convert.hz.white.space="logical",
    frame="environment",
    tab.stops="integer",
    tar.exp="ANY",
    cur.exp="ANY",
    tar.banner="charOrNULL",
    cur.banner="charOrNULL",
    guides="ANY",
    guide.lines="GuideLines",
    trim="ANY",
    strip.row.head="StripRowHead",
    disp.width="integer",
    line.width="integer",
    text.width="integer",
    line.width.half="integer",
    text.width.half="integer",
    gutter="Gutter"
  ),
  prototype=list(
    disp.width=0L, text.width=0L, line.width=0L,
    text.width.half=0L, line.width.half=0L,
    guides=function(obj, obj.as.chr) integer(0L),
    trim=function(obj, obj.as.chr) cbind(1L, nchar(obj.as.chr)),
    ignore.white.space=TRUE, convert.hz.white.space=TRUE,
    word.diff=TRUE, unwrap.atomic=TRUE
  ),
  validity=function(object){
    int.1L.and.pos <- c(
      "disp.width", "line.width", "text.width", "line.width.half",
      "text.width.half"
    )
    for(i in int.1L.and.pos)
      if(!is.int.1L(slot(object, i)) || slot(object, i) < 0L)
        return(sprintf("Slot `%s` must be integer(1L) and positive", i))
    TF <- c(
      "ignore.white.space", "convert.hz.white.space", "word.diff",
      "unwrap.atomic"
    )
    for(i in TF)
      if(!is.TF(slot(object, i)) || slot(object, i) < 0L)
        return(sprintf("Slot `%s` must be TRUE or FALSE", i))
    if(!is.TF(object@guides) && !is.function(object@guides))
      return("Slot `guides` must be TRUE, FALSE, or a function")
    if(
      is.function(object@guides) &&
      !isTRUE(v.g <- is.two.arg.fun(object@guides))
    )
      return(sprintf("Slot `guides` is not a valid guide function (%s)", v.g))
    if(!is.TF(object@trim) && !is.function(object@trim))
      return("Slot `trim` must be TRUE, FALSE, or a function")
    if(
      is.function(object@trim) &&
      !isTRUE(v.t <- is.two.arg.fun(object@trim))
    )
      return(sprintf("Slot `trim` is not a valid trim function (%s)", v.t))
    TRUE
  }
)
setMethod("initialize", "Settings", function(.Object, ...) {
  if(is.numeric(.Object@disp.width))
    .Object@disp.width <- as.integer(.Object@disp.width)
  if(is.null(.Object@disp.width))
    .Object@disp.width <- 80L
  return(callNextMethod(.Object, ...))
} )

setGeneric("sideBySide", function(x, ...) standardGeneric("sideBySide"))
setMethod("sideBySide", "Settings",
  function(x, ...) {
    x@mode <- "sidebyside"
    x@text.width <- x@text.width.half
    x@line.width <- x@line.width.half
    x
  }
)
.diff.dat.cols <- c(
  "orig", "raw", "trim", "trim.ind.start", "trim.ind.end", "comp", "eq", "fin",
  "fill", "word.ind", "tok.rat"
)

# Validate the *.dat slots of the Diff objects

valid_dat <- function(x) {
  char.cols <- c("orig", "raw", "trim", "eq", "comp", "fin")
  list.cols <- c("word.ind")
  zerotoone.cols <- "tok.rat"
  integer.cols <- c("trim.ind.start", "trim.ind.end")

  if(!is.list(x)) {
    "should be a list"
  } else if(!identical(names(x), .diff.dat.cols)) {
    paste0("should have names ", dep(.diff.dat.cols))
  } else if(
    length(
      unique(
        vapply(
          x[c(char.cols, list.cols, zerotoone.cols, integer.cols)],
          length, integer(1L)
        )
    ) ) != 1L
  ) {
    "should have equal length components"
  } else {
    if(
      length(
        not.char <- which(!vapply(x[char.cols], is.character, logical(1L)))
      )
    ){
      sprintf("element `%s` should be character", char.cols[not.char][[1L]])
    } else if (
      length(
        not.int <- which(!vapply(x[integer.cols], is.integer, logical(1L)))
      )
    ) {
      sprintf("element `%s` should be integer", integer.cols[not.int][[1L]])
    } else if (
      length(
        not.list <- which(!vapply(x[list.cols], is.list, logical(1L)))
      )
    ) {
      sprintf("element `%s` should be list", list.cols[not.list][[1L]])
    } else if (
      !all(
        vapply(
          x$word.ind,
          function(y)
            is.integer(y) && is.integer(attr(y, "match.length")) &&
            length(y) == length(attr(y, "match.length")),
          logical(1L)
      ) )
    ) {
      "element `word.ind` is not in expected format"
    } else if (
      !is.numeric(x$tok.rat) || anyNA(x$tok.rat) || !all(x$tok.rat %bw% c(0, 1))
    ) {
      "element `tok.rat` should be numeric with all values between 0 and 1"
    } else if (!is.logical(x$fill) || anyNA(x$fill)) {
      "element `fill` should be logical and not contain NAs"
    }
    else TRUE
  }
}
#' Diff Result Object
#'
#' Return value for the \code{\link[=diffPrint]{diff*}} methods.  Has
#' \code{show}, \code{as.character}, \code{summmary}, \code{[}, \code{head},
#' \code{tail}, and \code{any} methods.
#'
#' @export

setClass("Diff",
  slots=c(
    target="ANY",                    # Actual object
    tar.dat="list",
    current="ANY",
    cur.dat="list",
    diffs="list",
    trim.dat="list",              # result of trimmaxg
    sub.index="integer",
    sub.head="integer",
    sub.tail="integer",
    capt.mode="character",        # whether in print or str mode
    hit.diffs.max="logical",
    diff.count.full="integer",         # only really used by diffStr when folding
    hunk.heads="list",
    etc="Settings"
  ),
  prototype=list(
    capt.mode="print",
    trim.dat=list(lines=integer(2L), hunks=integer(2L), diffs=integer(2L)),
    hit.diffs.max=FALSE, diff.count.full=-1L
  ),
  validity=function(object) {
    # Most of the validation is done by `check_args`
    if(
      !is.chr.1L(object@capt.mode) ||
      ! object@capt.mode %in% c("print", "str", "chr", "deparse", "file")
    )
      return("slot `capt.mode` must be either \"print\" or \"str\"")

    not.list.3 <- !is.list(object@trim.dat) || length(object@trim.dat) != 3L
    not.names <- !identical(names(object@trim.dat), c("lines", "hunks", "diffs"))
    not.comp.1 <- !all(vapply(object@trim.dat, is.integer, logical(1L)))
    not.comp.2 <- !all(vapply(object@trim.dat, length, integer(1L)) == 2L)

    if(not.list.3)
      return(
        paste0(
          "slot `trim.dat` is not a length 3 list (", typeof(object@trim.dat),
          ", ", length(object@trim.dat)
      ) )
    if(not.names)
      return(
        paste0(
          "slot `trim.dat` has wrong names",
          deparse(names(object@trim.dat))[1]
      ) )
    if(not.comp.1)
      return(
        paste0(
          "slot `trim.dat` has non-integer components ",
          deparse(vapply(object@trim.dat, typeof, character(1L)))[1]
      ) )
    if(not.comp.2)
      return("slot `trim.dat` has components of length != 2")

    ## too expensive computationally
    # if(!isTRUE(tar.dat.val <- valid_dat(object@tar.dat)))
    #   return(paste0("slot `tar.dat` not valid: ", tar.dat.val))
    # if(!isTRUE(cur.dat.val <- valid_dat(object@cur.dat)))
    #   return(paste0("slot `cur.dat` not valid: ", cur.dat.val))
    if(!is.TF(object@hit.diffs.max))
      return("slot `hit.diffs.max` must be TRUE or FALSE")

    TRUE
} )
#' @rdname finalizeHtml

setMethod("finalizeHtml", c("Diff"),
  function(x, x.chr, ...) {
    style <- x@etc@style
    html.output <- style@html.output
    if(html.output == "auto") {
      html.output <- if(is(style@pager, "PagerBrowser"))
        "page" else "diff.only"
    }
    if(html.output == "page") {
      x.chr <- c(
        make_dummy_row(x),
        sprintf("<div id='diffobj_content'>%s</div>", x.chr),
        sprintf( "
          <script type=\"text/javascript\">
            var scale=%s;
          </script>", if(style@scale) "true" else "false"
        )
      )
      rez.fun <- if(style@scale)
        "resize_diff_out_scale" else "resize_diff_out_no_scale"
      js <- try(readLines(style@js), silent=TRUE)
      if(inherits(js, "try-error")) {
        cond <- attr(js, "condition")
        warning(
          "Unable to read provided js file \"", style@js, "\" (error: ",
          paste0(conditionMessage(cond), collapse=""), ")."
        )
        js <- ""
      } else {
        js <- paste0(
          c(
            js,
            sprintf(
              "window.addEventListener('resize', %s, true);\n %s();",
              rez.fun, rez.fun
          ) ),
          collapse="\n"
      ) }
    } else js <- ""
    callNextMethod(x, x.chr, js=js, ...)
} )
# Helper fun used by `show` for Diff and DiffSummary objects

show_w_pager <- function(txt, pager) {
  use.pager <- use_pager(pager, attr(txt, "len"))

  # Finalize and output

  if(use.pager) {
    disp.f <- paste0(tempfile(), ".", pager@file.ext)
    on.exit(add=TRUE, unlink(disp.f))
    writeLines(txt, disp.f)
    pager@pager(disp.f)
  } else {
    cat(txt, sep="\n")
  }
}
setMethod("show", "Diff",
  function(object) {
    txt <- as.character(object)
    show_w_pager(txt, object@etc@style@pager)
    invisible(NULL)
  }
)

# Compute what fraction of the lines in target and current actually end up
# in the diff; some of the complexity is driven by repeated context hunks

setGeneric("lineCoverage", function(x) standardGeneric("lineCoverage"))
setMethod("lineCoverage", "Diff",
  function(x) {
    lines_in_hunk <- function(z, ind)
      if(z[[ind]][[1L]]) z[[ind]][[1L]]:z[[ind]][[2L]]
    hunks.f <- unlist(x@diffs, recursive=FALSE)
    lines.tar <- length(
      unique(unlist(lapply(hunks.f, lines_in_hunk, "tar.rng.sub")))
    )
    lines.cur <- length(
      unique(unlist(lapply(hunks.f, lines_in_hunk, "cur.rng.sub")))
    )
    min(
      1, (lines.tar + lines.cur) / (
      length(x@tar.dat$raw) + length(x@cur.dat$raw))
    )
  }
)
#' Determine if Diff Object Has Differences
#'
#' @param x a \code{Diff} object
#' @param ... unused, for compatibility with generic
#' @param na.rm unused, for compatibility with generic
#' @return TRUE if there are differences, FALSE if not, FALSE with warning if
#'   there are no differences but objects are not \code{\link{all.equal}}
#' @examples
#' any(diffChr(letters, letters))
#' any(diffChr(letters, letters[-c(1, 5, 8)]))

setMethod("any", "Diff",
  function(x, ..., na.rm = FALSE) {
    dots <- list(...)
    if(length(dots))
      stop("`any` method for `Diff` supports only one argument")
    res <- any(
      which(
        !vapply(
          unlist(x@diffs, recursive=FALSE), "[[", logical(1L), "context"
    ) ) )
    if(!res && !isTRUE(all.equal(x@target, x@current)))
      warning("No visible differences, but objects are NOT `all.equal`.")
    res
} )
setClass(
  "MyersMbaSes",
  slots=c(
    a="character",
    b="character",
    type="factor",
    length="integer",
    offset="integer",
    diffs="integer"
  ),
  prototype=list(
    type=factor(character(), levels=c("Match", "Insert", "Delete"))
  ),
  validity=function(object) {
    if(!identical(levels(object@type), c("Match", "Insert", "Delete")))
      return("Slot `type` levels incorrect")
    if(any(is.na(c(object@a, object@b))))
      return("Slots `a` and `b` may not contain NA values")
    if(any(is.na(c(object@type, object@length, object@offset))))
      return("Slots `type`, `length`,  or `offset` may not contain NA values")
    if(any(c(object@type, object@length, object@offset)) < 0)
      return(
        paste0(
          "Slots `type`, `length`,  and `offset` must have values greater ",
          "than zero"
      ) )
    if(!is.int.1L(object@diffs))
      return("Slot `diffs` must be integer(1L) and not NA")
    TRUE
  }
)

# Run validity on S4 objects
#
# Intended for use within check_args; unfortunately can't use complete=TRUE
# because we are using ANY slots with S3 objects there-in, which causes
# the complete check to freak out with "trying to get slot 'package' from..."
#
# @param x object to test
# @param err.tpl a string used with sprintf, must contain two \dQuote{%s} for
#   respectively \code{arg.name} and the class name
# @param arg.name argument the object is supposed to come from
# @param err error reporting function

valid_object <- function(
  x, arg.name, err, err.tpl="Argument `%s` is an invalid `%s` object because:"
) {
  if(isS4(x)) {
    if(!isTRUE(test <- validObject(x, test=TRUE))) {
      err(
        paste(
          sprintf(err.tpl, arg.name, class(x)[[1L]]),
          strwrap(test, initial="- ", prefix="  "),
          collapse="\n"
) ) } } }
