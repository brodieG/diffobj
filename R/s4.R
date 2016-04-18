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
setClass(
  "diffObjPager",
  slots=c(mode="character", threshold="integer", less.flags="character"),
  validity=function(object) {
    if(!is.pager_mode(object@mode)) return("Invalid `mode` slot")
    if(!is.less_flags(object@less.flags)) return("Invalid `less.flags` slot")
    if(!is.int.1L(object@threshold)) return("Invalid `threshold` slot")
    TRUE
  }
)
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
# maybe this shouldn't be an S4 class since the function slot doesn't work
# for classed functions (e.g. the ones produced by crayon)

#' Customize Appearance of Diff
#'
#' Most of the customization is done by specifying functions that operate on
#' character vectors and return a modified character vector of the same length.
#' The intended use case is to pass \code{crayon} functions such as
#' \code{\link{crayon::red}}, although you may pass any function of your liking
#' that behaves as described.
#'
#' The visual representation of the diff has many nested components.  The
#' functions you specify here will be applied with the function corresponding to
#' the innermost element applied first.  A schematic of the various component
#' that represent an inserted line follows:
#' \preformatted{+- line ----------------------------------------+
#' |+- line.ins ----------------------------------+|
#' ||+- gutter ---------+ +- text ---------------+||
#' |||+- gutter.ins ---+| |+- text.ins ---------+|||
#' ||||                || ||      +- word.ins -+||||
#' |||| gutter.ins.txt || || DIFF | TEXT HERE  |||||
#' ||||                || ||      +------------+||||
#' |||+----------------+| |+--------------------+|||
#' ||+------------------+ +----------------------+||
#' |+---------------------------------------------+|
#' +-----------------------------------------------+
#' }
#' A similar model applies to deleted and matching lines.  The boxes represent
#' functions.  \code{gutter.ins.txt} represents the text to use in the gutter
#' and is not a function. \code{DIFF TEXT HERE} is text from the objects being
#' diffed, with the portion that has different words inside the \code{word.ins}
#' box provided word diff is enabled, and is obviously not a function either.
#'
#' Most of the functions defined here default to \code{\link{identity}}, but
#' you are given the flexibility to fully format the diff.
#'
#' @note in \dQuote{sidebyside} there are two lines per row of text, one showing
#'   deletions and one showing additions.
#' @param line function
#' @param line.ins function
#' @param line.del function
#' @param line.match function
#' @param text function
#' @param text.ins function
#' @param text.del function
#' @param text.match function
#' @param gutter function
#' @param gutter.ins function
#' @param gutter.del function
#' @param gutter.match function
#' @param hunk.header function to format each hunk header with
#' @param banner.ins function to format insertion banner
#' @param banner.del function to format deletion banner
#' @param banner function to format entire banner
#' @param meta function format meta information lines
#' @param gutter.ins.txt character(1L) text to use as visual cue to indicate
#'   whether a diff line is an insertion, defaults to \dQuote{> }
#' @param gutter.ins.txt.ctd character(1L) if a diff line is wrapped, the
#'   visual cue shifts to this character to indicate wrapping occured
#' @param gutter.del.txt character(1L) see \code{gutter.ins.txt} above
#' @param gutter.del.txt.ctd character(1L) see \code{gutter.ins.txt.ctd} above
#' @param gutter.match.txt character(1L) see \code{gutter.ins.txt} above
#' @param gutter.match.txt.ctd character(1L) see \code{gutter.ins.txt.ctd} above
#' @return diffObjStyle S4 object
#' @export diffObjStyle
#' @exportClass diffObjStyle

diffObjStyle <- setClass(
  "diffObjStyle",
  slots=c(
    line="ANY", line.insert="ANY", line.delete="ANY", line.match="ANY",
    text="ANY", text.insert="ANY", text.delete="ANY", text.match="ANY",
    gutter="ANY",
    gutter.insert="ANY", gutter.insert.ctd="ANY",
    gutter.delete="ANY", gutter.delete.ctd="ANY",
    gutter.match="ANY", gutter.match.ctd="ANY",
    gutter.pad="ANY",
    word.insert="ANY", word.delete="ANY",
    banner="ANY", banner.insert="ANY", banner.delete="ANY",
    context.sep="ANY", header="ANY", meta="ANY",
    gutter.insert.txt="character", gutter.insert.ctd.txt="character",
    gutter.delete.txt="character", gutter.delete.ctd.txt="character",
    gutter.match.txt="character", gutter.match.ctd.txt="character",
    gutter.pad.txt="character",
    context.sep.txt="character"
  ),
  prototype=list(
    line=identity,
    line.insert=identity, line.delete=identity, line.match=identity,
    text=identity,
    text.insert=identity, text.delete=identity, text.match=identity,
    gutter=identity, gutter.pad=identity,
    gutter.insert=identity, gutter.insert.ctd=identity,
    gutter.delete=identity, gutter.delete.ctd=identity,
    gutter.match=identity, gutter.match.ctd=identity,
    word.insert=identity, word.delete=identity,
    banner=identity, banner.insert=identity, banner.delete=identity,
    header=identity,
    context.sep=identity,
    meta=identity,
    gutter.insert.txt=">", gutter.insert.ctd.txt=":",
    gutter.delete.txt="<", gutter.delete.ctd.txt=":",
    gutter.match.txt=" ", gutter.match.ctd.txt=" ",
    gutter.pad.txt=" ",
    context.sep.txt="~~~~~"
  ),
  validity=function(object){
    char.slots.pat <- "\\.txt$"
    slots <- slotNames(object)
    char.slots <- grepl(char.slots.pat, slots)
    slots.fun <- slots[!char.slots]
    for(i in slots.fun) {
      if(!is.function(slot(object, i)))
        return(paste0("Argument `", i, "` should be a function."))
      frm <- formals(slot(object, i))
      non.def <- vapply(
        names(frm),
        function(x)
          is.name(frm[[x]]) && !nzchar(as.character(frm[[x]])) && x != "...",
        logical(1L)
      )
      if(sum(non.def) > 1L)
        return(
         paste0(
          "Argument `", i,
          "` may not have more than one non-default formal argument"
        ) )
    }
    for(i in slots[char.slots]) if(!is.chr.1L(slot(object, i)))
      return(paste0("Argument `", i, "` must be character(1L) and not NA."))
    TRUE
  }
)
setClass(
  "diffObjSettings",
  slots=c(
    mode="character",             # diff output mode
    context="doAutoCOrInt",
    line.limit="integer",
    style="diffObjStyle",
    pager="diffObjPager",
    hunk.limit="integer",
    disp.width="integer",         # what options(width) returns
    text.width="integer",         # non-gutter width
    line.width="integer",         # like disp, but half for sidebyside
    use.ansi="logical",
    max.diffs="integer",
    max.diffs.in.hunk="integer",
    max.diffs.wrap="integer",
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
    gutter="diffObjGutter"
  ),
  prototype=list(use.header=FALSE)
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
    # Finalize stuff

    res.chr <- as.character(object)
    # slot(res.diff, "trim.dat") <- attr(res.chr, "meta")
    use.pager <- object@etc@pager@mode
    use.pager.thresh <- identical(use.pager, "threshold")
    pager.thresh <- object@etc@pager@threshold
    threshold <- if(use.pager.thresh && pager.thresh == -1L)
      console_lines() else object@etc@pager@threshold

    if(
      identical(use.pager, "always") || (
        use.pager.thresh && length(res.chr) > threshold
      )
    ){
      disp.f <- tempfile()
      on.exit(add=TRUE, unlink(disp.f))
      writeLines(res.chr, disp.f)
      if(pager_is_less() && object@etc@use.ansi) {
        old.less <- set_less_var("R")
        on.exit(reset_less_var(old.less), add=TRUE)
      }
      file.show(disp.f)
    } else cat(res.chr, sep="\n")

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
