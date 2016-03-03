#' @include misc.R

NULL

# S4 class definitions

setClassUnion("charOrNULL", c("character", "NULL"))

#' Dummy Doc File for S4 Methods with Existing Generics
#'
#' @keywords internal
#' @name diffobj_s4method_doc
#' @rdname diffobj_s4method_doc

NULL

#' Configure Automatic Context Calculation
#'
#' Defines parameters for selecting an appropriate context value when using
#' \code{\link{diff_obj}} and related functions.
#'
#' @export
#' @param min integer(1L), positive, set to zero to allow any context
#' @param max integer(1L), set to negative to allow any context
#' @return S4 object containing configuration parameters, for use as the
#'   \code{context} parameter value in \code{\link{diff_obj}} and related
#'   functions

auto_context <- function(
  min=getOption("diffobj.context.auto.min"),
  max=getOption("diffobj.context.auto.max")
) {
  new("diffObjAutoContext", min=as.integer(min), max=as.integer(max))
}
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
setClassUnion("doacOrInt", c("diffObjAutoContext", "integer"))

# Classes for tracking intermediate diff obj data
#
# DiffDiffs contains a slot corresponding to each of target and current where
# a TRUE value means the corresponding value matches in both objects and FALSE
# means that it does not match

setClass(
  "diffObjDiffDiffs",
  slots=c(
    hunks="list",
    max.diffs="integer"  # used for `str` with auto `max.level`
  ),
  prototype=list(max.diffs=0L),
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
    if(!is.int.1L(object@max.diffs) || object@max.diffs < 0L) {
      return("Slot `max.diffs` must be integer(1L), non-NA, and positive")

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
    context="doacOrInt",
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
    diffs="diffObjDiffDiffs",     # line by line diffs
    trim.dat="list"               # result of trimmaxg
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
# Apply line colors; returns a list with the A and B vectors colored,
# note that all hunks will be collapsed.
#
# Really only intended to be used for stuff that produces a single hunk

setGeneric("diffColor", function(x, ...) standardGeneric("diffColor"))
setMethod("diffColor", "diffObjDiffDiffs",
 function(x, ...) {
   res.l <- lapply(x@hunks,
     function(y) {
       lapply(y,
         function(z) {
           A <- z$A.chr
           B <- z$B.chr
           if(!z$context) {
             A[z$A < 0L] <- crayon_style(A[z$A < 0L], "green")
             A[z$A > 0L] <- crayon_style(A[z$A > 0L], "red")
             B[z$B < 0L] <- crayon_style(B[z$B < 0L], "green")
             B[z$B > 0L] <- crayon_style(B[z$B > 0L], "red")
           }
           list(A=A, B=B)
  } ) } )
  res.l <- unlist(res.l, recursive=FALSE)
  A <- unlist(lapply(res.l, "[[", "A"))
  B <- unlist(lapply(res.l, "[[", "B"))
  list(A=A, B=B)
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
