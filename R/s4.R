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
#' @export AlignThreshold
#' @exportClass AlignThreshold

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
    count.alnum.only=gdo("align.count.alnum.only"), ...) {
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
setClass(
  "Gutter",
  slots= c(
    insert="character", insert.ctd="character",
    delete="character", delete.ctd="character",
    match="character", match.ctd="character",
    guide="character", guide.ctd="character",
    pad="character",
    width="integer"
  )
)
setClass(
  "Settings",
  slots=c(
    mode="character",             # diff output mode
    context="doAutoCOrInt",
    line.limit="integer",
    style="Style",
    hunk.limit="integer",
    max.diffs="integer",
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
    guides=function(obj, obj.as.chr) integer(0L)
  ),
  validity=function(object){
    int.1L.and.pos <- c(
      "disp.width", "line.width", "text.width", "line.width.half",
      "text.width.half"
    )
    for(i in int.1L.and.pos)
      if(!is.int.1L(slot(object, i)) || slot(object, i) < 0L)
        return(sprintf("Slot `%s` must be integer(1L) and positive", i))
    TF <- c("ignore.white.space", "convert.hz.white.space")
    for(i in TF)
      if(!is.TF(slot(object, i)) || slot(object, i) < 0L)
        return(sprintf("Slot `%s` must be TRUE or FALSE", i))
    if(!is.TF(object@guides) && !is.function(object@guides))
      return("Slot `guides` must be TRUE, FALSE, or a function")
    if(
      is.function(object@guides) &&
      !isTRUE(v.g <- is.valid.guide.fun(object@guides))
    )
      return(sprintf("Slot `guides` is not a valid guide function (%s)", v.g))
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

setClass(
  "DiffDiffs",
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
setClass(
 "Diff",
  slots=c(
    target="ANY",                 # Actual object
    tar.capt="character",         # The captured representation
    tar.capt.def="charOrNULL",    # ^^, but using default print method
    current="ANY",
    cur.capt="character",
    cur.capt.def="charOrNULL",
    diffs="list",
    trim.dat="list",              # result of trimmaxg
    capt.mode="character",        # whether in print or str mode
    etc="Settings"
  ),
  prototype=list(
    capt.mode="print",
    trim.dat=list(lines=integer(2L), hunks=integer(2L), diffs=integer(2L))
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
    TRUE
} )
setMethod("show", "Diff",
  function(object) {
    res.chr <- as.character(object)

    # Determine whether to use pager or not

    pager <- object@etc@style@pager

    use.pager <- if(!is(pager, "PagerOff")) {
      threshold <- if(pager@threshold < 0L) {
        console_lines()
      } else pager@threshold
      !threshold || length(res.chr) > threshold
    } else FALSE

    # Finalize and output

    fin <- object@etc@style@finalizer(res.chr, pager)

    if(use.pager) {
      disp.f <- paste0(tempfile(), ".", pager@file.ext)
      on.exit(add=TRUE, unlink(disp.f))
      writeLines(fin, disp.f)
      pager@pager(disp.f)
    } else {
      cat(fin, sep="\n")
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
    hunks.f <- unlist(x@diffs$hunks, recursive=FALSE)
    lines.tar <- length(
      unique(unlist(lapply(hunks.f, lines_in_hunk, "tar.rng.sub")))
    )
    lines.cur <- length(
      unique(unlist(lapply(hunks.f, lines_in_hunk, "cur.rng.sub")))
    )
    min(1, (lines.tar + lines.cur) / (length(x@tar.capt) + length(x@cur.capt)))
  }
)
setMethod("any", "Diff",
  function(x, ..., na.rm = FALSE) {
    dots <- list(...)
    if(length(dots))
      stop("`any` method for `Diff` supports only one argument")
    any(
      which(
        !vapply(
          unlist(x@diffs$hunks, recursive=FALSE), "[[", logical(1L), "context"
) ) ) } )
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
