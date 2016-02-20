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
    hunk.check <- lapply(
      object@hunks,
      function(x) {
        vapply(x,
          function(y) {
            nm <- c(
              "id", "A", "B", "A.chr", "B.chr", "context", "tar.rng", "cur.rng",
              "tar.rng.trim", "cur.rng.trim"
            )
            identical(names(y), nm) &&
            is.integer(ab <- unlist(y[nm[-(3:5)]])) && !any(is.na(ab)) &&
            length(y$tar.rng) == 2L && diff(y$tar.rng) >= 0L &&
            length(y$cur.rng) == 2L && diff(y$cur.rng) >= 0L &&
            length(y$tar.rng.trim) == 2L && diff(y$tar.rng.trim) >= 0L &&
            length(y$cur.rng.trim) == 2L && diff(y$cur.rng.trim) >= 0L
          },
          logical(1L)
      ) }
    )
    if(!all(unlist(hunk.check)))
      return("slot `hunks` contains invalid hunks")
    hunks.flat <- unlist(object@hunks, recursive=FALSE)
    if(
      !identical(
        vapply(hunks.flat, "[[", integer(1L), "id"), seq_along(hunks.flat)
    ) ) {
      return("atomic hunk ids invalid")
    }
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
    tar.banner="character",
    cur.banner="character",
    mode="character",
    diffs="diffObjDiffDiffs",
    tar.capt.def="charOrNULL",
    cur.capt.def="charOrNULL"
  ),
  prototype=list(mode="print"),
  validity=function(object) {
    if(!is.chr1(object@mode) || ! object@mode %in% c("print", "str"))
      return("slot `mode` must be either \"print\" or \"str\"")
    if(!is.chr1(object@tar.banner))
      return("slot `tar.banner` must be either character(1L) and not NA")
    if(!is.chr1(object@cur.banner))
      return("slot `cur.banner` must be either character(1L) and not NA")
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
        paste0(
          "Slots `type`, `length`,  and `offset` must have values greater ",
          "than zero"
      ) )
    TRUE
  }
)
