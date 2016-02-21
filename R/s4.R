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
  slots=c(hunks="list"),
  validity=function(object) {
    hunk.check <- lapply(
      object@hunks,
      function(x) {
        vapply(x,
          function(y) {
            rng.names <- paste0(
              rep(c("tar.rng", "cur.rng"), 3L),
              rep(c("", ".sub", ".trim"), each=2L)
            )
            nm <- c("id", "A", "B", "A.chr", "B.chr", "context", rng.names)
            valid_rng <- function(x) length(x) == 2L && diff(x) >= 0L

            identical(names(y), nm) &&
            is.integer(ab <- unlist(y[nm[-(3:5)]])) && !any(is.na(ab)) &&
            all(vapply(y[rng.names], valid_rng, logical(1L)))
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
    target="ANY",                 # Actual object
    tar.capt="character",         # The captured representation
    tar.capt.def="charOrNULL",    # ^^, but using default print method
    tar.banner="character",       # Banner to display
    current="ANY",
    cur.capt="character",
    cur.capt.def="charOrNULL",
    cur.banner="character",
    mode="character",             # diff output mode
    context="integer",
    hunk.limit="integer",
    line.limit="integer",
    disp.width="integer",
    use.ansi="logical",
    max.diffs="integer",          # after how many differences should we give up
    max.diffs.in.hunk="integer",  # give up threshold for hunk-hunk comparison
    # give up threshold for word diff on wrapped atomic
    max.diffs.wrap="integer",
    ignore.white.space="logical",
    capt.mode="character",        # whether in print or str mode
    frame="environment",
    silent="logical",
    diffs="diffObjDiffDiffs"      # line by line diffs
  ),
  prototype=list(capt.mode="print"),
  validity=function(object) {
    # Most of the validation is done by `check_args`
    if(!is.chr.1L(object@capt.mode) || ! object@capt.mode %in% c("print", "str"))
      return("slot `capt.mode` must be either \"print\" or \"str\"")
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
