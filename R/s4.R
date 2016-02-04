# S4 class definitions

setClassUnion("charOrNULL", c("character", "NULL"))

#' Dummy Doc File for S4 Methods with Existing Generics
#'
#' @keywords internal
#' @name diffobj_s4method_doc
#' @rdname diffobj_s4method_doc

NULL

# Classes for tracking intermediate diff obj data
#
# DiffDiffs contains a slot corresponding to each of target and current where
# a TRUE value means the corresponding value matches in both objects and FALSE
# means that it does not match

setClass(
  "diffObjDiffDiffs",
  slots=c(hunks="list", white.space="logical"),
  validity=function(object) {
    if(!is.TF(object@white.space))
      return("slot `white.space` must be TRUE or FALSE")
    hunk.check <- vapply(
      object@hunks,
      function(x)
        identical(names(x), c("target", "current", "tar.pos", "cur.pos")) &&
        is.integer(x.u <- unlist(x)) && all(is.na(x.u) | x.u >= 0L) &&
        (length(x$tar.pos) == length(x$cur.pos)) == 1L,
      logical(1L)
    )
    if(!all(hunk.check))
      return("slot `hunks` contains invalid hunks")
    TRUE
  }
)
setClass(
  "diffObjDiff",
  slots=c(
    tar.obj="ANY",
    cur.obj="ANY",
    tar.capt="character",
    cur.capt="character",
    tar.exp="ANY",
    cur.exp="ANY",
    mode="character",
    diffs="diffObjDiffDiffs",
    tar.capt.def="charOrNULL",
    cur.capt.def="charOrNULL"
  ),
  prototype=list(mode="print"),
  validity=function(object) {
    if(!is.chr1(object@mode) || ! object@mode %in% c("print", "str"))
      return("slot `mode` must be either \"print\" or \"str\"")
    TRUE
} )
setClass(
  "diffObjMyersMbaSes",
  slots=c(
    a="character",
    b="character",
    type="factor",
    length="integer",
    offset="integer"
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
        "Slots `type`, `length`,  and `offset` must have values greater than zero"
      )
    TRUE
  }
)
