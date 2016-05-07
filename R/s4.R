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

setClass(
  "diffObjAutoContext",
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
setClassUnion("doAutoCOrInt", c("diffObjAutoContext", "integer"))
# pre-computed gutter data

setClass(
  "diffObjGutter",
  slots= c(
    insert="character", insert.ctd="character",
    delete="character", delete.ctd="character",
    match="character", match.ctd="character",
    pad="character",
    width="integer"
  )
)
setClass(
  "diffObjSettings",
  slots=c(
    mode="character",             # diff output mode
    context="doAutoCOrInt",
    line.limit="integer",
    style="diffObjStyle",
    hunk.limit="integer",
    use.ansi="logical",
    max.diffs="integer",
    align.threshold="numeric",
    ignore.white.space="logical",
    frame="environment",
    silent="logical",
    tab.stops="integer",
    tar.exp="ANY",
    cur.exp="ANY",
    tar.banner="charOrNULL",
    cur.banner="charOrNULL",
    use.header="logical",
    disp.width="integer",
    line.width="integer",
    text.width="integer",
    gutter="diffObjGutter"
  ),
  prototype=list(use.header=FALSE, disp.width=0L, text.width=0L, line.width=0L),
  validity=function(object){
    int.1L.and.pos <- c("disp.width", "line.width", "text.width")
    for(i in int.1L.and.pos)
      if(!is.int.1L(slot(object, i)) || slot(object, i) < 0L)
        return(sprintf("Slot `%s` must be integer(1L) and positive"), i)
  }
)
setMethod("initialize", "diffObjSettings", function(.Object, ...) {
  if(is.numeric(.Object@disp.width))
    .Object@disp.width <- as.integer(.Object@disp.width)
  if(is.null(.Object@disp.width))
    .Object@disp.width <- 80L
  return(callNextMethod(.Object, ...))
} )
# Classes for tracking intermediate diff obj data
#
# DiffDiffs contains a slot corresponding to each of target and current where
# a TRUE value means the corresponding value matches in both objects and FALSE
# means that it does not match
#
# Object deprecated in favor of list because to slow to instantiate and possible
# instantiated as many times as there are hunks, + 1

setClass(
  "diffObjDiffDiffs",
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
 "diffObjDiff",
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
    etc="diffObjSettings"
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
setMethod("show", "diffObjDiff",
  function(object) {
    res.chr <- as.character(object)

    # Determine whether to use pager or not

    pager <- object@etc@style@pager

    use.pager <- if(!is(pager, "diffObjPagerOff")) {
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
      if(is(pager, "diffObjPagerBrowser"))
        readline("Press ENTER to continue...")
    } else {
      cat(fin, sep="\n")
    }
    invisible(NULL)
  }
)
# Compute what fraction of the lines in target and current actually end up
# in the diff; some of the complexity is driven by repeated context hunks

setGeneric("lineCoverage", function(x) standardGeneric("lineCoverage"))
setMethod("lineCoverage", "diffObjDiff",
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
setMethod("any", "diffObjDiff",
  function(x, ..., na.rm = FALSE) {
    dots <- list(...)
    if(length(dots))
      stop("`any` method for `diffObjDiff` supports only one argument")
    any(
      which(
        !vapply(
          unlist(x@diffs$hunks, recursive=FALSE), "[[", logical(1L), "context"
) ) ) } )
#' @rdname diffobj_s4method_doc

setMethod("any", "diffObjDiffDiffs",
  function(x, ..., na.rm = FALSE) {
    stop("function deprecated")
    dots <- list(...)
    if(length(dots))
      stop("`any` method for `diffObjDiff` supports only one argument")
    any(
      which(
        !vapply(unlist(x$hunks, recursive=FALSE), "[[", logical(1L), "context")
    ) )
} )

setClass(
  "diffObjMyersMbaSes",
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
