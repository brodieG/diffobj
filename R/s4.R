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
#'   5.
#' @slot count.alnum.only logical(1L) modifier for \code{min.chars}, whether to
#'   count alpha numeric characters only.  Helps reduce spurious alignment
#'   caused by meta character sequences such as \dQuote{[[1]]} that would
#'   otherwise meet the \code{min.chars} limit
#' @slot ignore.row.head logical(1L) whether to ignore row / atomic vector index
#'   headers such as \code{[1]} or \code{[1,]}; the implementation is rather
#'   inelegant and strips things that look like they are row headers
#'   with not much accounting of whether they really are or just look like them.
#'   There is some overlap with \code{min.chars} and \code{count.alnum.only} to
#'   the extent removed row headers do not count towards those parameters.
#' @export AlignThreshold
#' @exportClass AlignThreshold

AlignThreshold <- setClass("AlignThreshold",
  slots=c(
    threshold="numeric",
    min.chars="integer",
    count.alnum.only="logical",
    ignore.row.head="logical"
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
    if(!is.TF(object@ignore.row.head))
      return("Slot `ignore.row.head` must be TRUE or FALSE")
  }
)
setMethod(
  "initialize", "AlignThreshold",
  function(
    .Object, threshold=gdo("align.threshold"), min.chars=gdo("align.min.chars"),
    count.alnum.only=gdo("align.count.alnum.only"),
    ignore.row.head=gdo("align.ignore.row.head"), ...
  ) {
    if(is.numeric(min.chars)) min.chars <- as.integer(min.chars)
    callNextMethod(
      .Object, threshold=threshold, min.chars=min.chars,
      count.alnum.only=count.alnum.only,
      ignore.row.head=ignore.row.head, ...
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
    trim=function(obj, obj.as.chr) cbind(1L, nchar(obj.as.chr))
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
# Classes for tracking intermediate diff obj data
#
# DiffDiffs contains a slot corresponding to each of target and current where
# a TRUE value means the corresponding value matches in both objects and FALSE
# means that it does not match
#
# Object deprecated in favor of list because to slow to instantiate and possible
# instantiated as many times as there are hunks, + 1

setClass("DiffDiffs",
  slots=c(
    hunks="list",
    count.diffs="integer"  # used for `str` with auto `max.level`
  ),
  prototype=list(count.diffs=0L),
  validity=function(object) {
    rng.names <- paste0(
      rep(c("tar.rng", "cur.rng"), 3L),
      rep(c("", ".sub", ".trim"), each=2L)
    )
    nm <- c("id", "A", "B", "A.chr", "B.chr", "context", rng.names)
    valid_rng <- function(x) length(x) == 2L && x[[2L]] - x[[1L]] >= 0L

    hunks.flat <- unlist(object$hunks, recursive=FALSE)
    hunk.check <- vapply(
      hunks.flat,
      function(y) {
        if(
          identical(names(y), nm) &&
          is.integer(ab <- unlist(y[nm[-(3:5)]])) && !any(is.na(ab)) &&
          all(vapply(y[rng.names], valid_rng, logical(1L)))
        ) y$id else 0L
      },
      integer(1L)
    )
    if(!all(hunk.check)) return("slot `hunks` contains invalid hunks")
    if(!identical(hunk.check, seq_along(hunks.flat)))
      return("atomic hunk ids invalid")
    if(!is.int.1L(object@count.diffs) || object@count.diffs < 0L)
      return("Slot `max.diffs` must be integer(1L), non-NA, and positive")
    TRUE
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
      sprintf("element `%s` should be list", char.cols[not.char][[1L]])
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
      !is.numeric(x$tok.rat) || anyNA(x$tok.rat) || !all(x$to.rat %bw% c(0, 1))
    ) {
      "element `tok.rat` should be numeric with all values between 0 and 1"
    } else if (!is.logical(x$fill) || anyNA(x$fill)) {
      "element `fill` should be logical and not contain NAs"
    }
    else TRUE
  }
}
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
    etc="Settings"
  ),
  prototype=list(
    capt.mode="print",
    trim.dat=list(lines=integer(2L), hunks=integer(2L), diffs=integer(2L)),
    hit.diffs.max=FALSE
  ),
  validity=function(object) {
    # Most of the validation is done by `check_args`
    if(!is.chr.1L(object@capt.mode) || ! object@capt.mode %in% c("print", "str"))
      return("slot `capt.mode` must be either \"print\" or \"str\"")
    if(
      !is.list(object@trim.dat) || length(object@trim.dat) != 3L ||
      !identical(names(object@trim.dat), c("lines", "hunks", "diffs")) ||
      !all(vapply(object@trim.dat, is.integer, logical(1L))) ||
      !all(vapply(object@trim.dat, length, integer(1L)) == 2L)
    )
      return("slot `trim.dat` in incorrect format")
    if(!isTRUE(tar.dat.val <- valid_dat(object@tar.dat)))
      return(paste0("slot `tar.dat` not valid: ", tar.dat.val))
    if(!isTRUE(cur.dat.val <- valid_dat(object@cur.dat)))
      return(paste0("slot `cur.dat` not valid: ", cur.dat.val))
    if(!is.TF(object@hit.diffs.max))
      return("slot `hit.diffs.max` must be TRUE or FALSE")

    TRUE
} )
setMethod("show", "Diff",
  function(object) {
    res.chr <- as.character(object)

    # Determine whether to use pager or not

    pager <- object@etc@style@pager
    use.pager <- use_pager(pager, attr(res.chr, "len"))

    # Finalize and output

    if(use.pager) {
      disp.f <- paste0(tempfile(), ".", pager@file.ext)
      on.exit(add=TRUE, unlink(disp.f))
      writeLines(res.chr, disp.f)
      pager@pager(disp.f)
    } else {
      cat(res.chr, sep="\n")
    }
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
#' @rdname diffobj_s4method_doc

setMethod("any", "DiffDiffs",
  function(x, ..., na.rm = FALSE) {
    stop("function deprecated")
    dots <- list(...)
    if(length(dots))
      stop("`any` method for `Diff` supports only one argument")
    any(
      which(
        !vapply(unlist(x$hunks, recursive=FALSE), "[[", logical(1L), "context")
    ) )
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
