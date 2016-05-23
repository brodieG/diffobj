# Detect which rows are likely to be meta data rows (e.g. headers) in tabular
# data
#
# Should be raw data stripped of ANSI characters

detect_2d_guides <- function(txt) {
  stopifnot(is.character(txt))
  space.rows <- !grepl("^\\s+\\[\\d+,\\]\\s|^\\S", txt)
  head.row <- min(which(space.rows))
  first.row <- min(which(!space.rows & seq_along(space.rows) > head.row))
  last.row <- max(which(!space.rows))

  # Between first.row and last.row, look for repeating sequences of head rows
  # and non head rows; should have the same number of each for each block in
  # a wrapped 2d object

  res <- if(last.row > head.row) {
    space.bw <- space.rows[head.row:last.row]
    seq.dat <- vapply(
      split(space.bw, cumsum(space.bw)), FUN=function(x) c(sum(x), sum(!x)),
      integer(2L)
    )
    # Which of the sets of true and false head rows have the same repeating
    # sequence as the first?

    valid.rep <- max(
      which(
        cumsum(colSums(cbind(integer(2L), abs(apply(seq.dat, 1L, diff))))) == 0L
      ),
      0L
    )
    if(valid.rep) {
      which(
        rep(rep(c(TRUE, FALSE), valid.rep), seq.dat[seq_len(valid.rep * 2L)])
      ) + head.row - 1L
    } else integer(0L)
  } else integer(0L)

  res
}
# Definitely approximate matching, we are lazy in matching the `$` versions
# due to the possibility of pathological names (e.g., containing `)

detect_list_guides <- function(txt) {
  stopifnot(is.character(txt))
  if(length(txt)) {
    # match stuff like "[[1]][[2]]" or "$ab[[1]]$cd" ...
    square.brkt <- "(\\[\\[\\d+\\]\\])"
    dollar.simple <- sprintf("(\\$%s)", .reg.r.ident)
    pat <- sprintf("^(%s|%s)*(\\$`.*`.*)?$", square.brkt, dollar.simple)

    # Only keep those that are first, preceded by an empty string, or by
    # another matching pattern
    has.pat <- grepl(pat, txt) & nzchar(txt)
    has.chars <- c(FALSE, head(nzchar(txt), -1L))
    has.pat.prev <- c(FALSE, head(has.pat, -1L))
    valid.pat <- has.pat & (!has.chars | has.pat.prev)

    # For any sequence of matching patterns, only keep the last one since
    # the other ones are redundant
    if(any(valid.pat)) {
      v.p.rle <- rle(valid.pat)
      valid.pat[-with(v.p.rle, cumsum(lengths)[values])] <- FALSE
    }
    which(valid.pat)
  } else integer(0L)
}
# Here we want to get the high dimension counter as well as the column headers
# of each sub-dimension

detect_array_guides <- function(txt, dim.n) {
  n.d.n <- names(dim.n)
  stopifnot(
    is.character(txt),
    is.list(dim.n) || is.null(dim.n),
    (is.character(n.d.n) && length(n.d.n) > 2L) || is.null(n.d.n)
  )
  if(length(txt) > 2L) {
    if(!is.null(n.d.n)) {
      # try to guard against dimnames that contain regex
      n.p <- "(\\[|\\]|\\(|\\)|\\{|\\}|\\*|\\+|\\?|\\.|\\^|\\$|\\\\|\\|)"
      row.pat <- sprintf("^%s\\s+\\S+", gsub(n.p, "\\\1", n.d.n[[1L]]))
      col.pat <- sprintf("^\\s{2,}%s$", gsub(n.p, "\\\1", n.d.n[[2L]]))
      # identify which lines could be row and col headers
      r.h <- grepl(row.pat, txt)
      c.h <- grepl(col.pat, txt)
    } else {
      c.h <- rep(FALSE, length(txt))
      r.h <- grepl("^\\s+\\S+", txt)
    }
    # Classify each line depending on what pattern it matches so we can then
    # analyze sequences and determine which are valid

    row.types <- integer(length(txt))
    row.types[!nzchar(txt)] <- 1L          # blanks
    row.types[grepl("^, , ", txt)] <- 2L   # high dim header
    row.types[r.h] <- 3L                   # row meta / col headers
    row.types[c.h] <- 4L                   # col meta

    row.chain <- paste0(row.types, collapse="") # assumes all values 1 char long
    p.simple <- "2130*1"    # no dimnames names
    p.cols <- "21430*1"     # both row and col dimnames

    p.test <- if(!is.null(n.d.n)) p.cols else p.simple

    # not perfect, since we could check that matched tokens match dimensions...
    # there should only be one sequence of the repeating pattern

    if(
      length(
        unlist(
          arr.coord <- gregexpr(sprintf("(%s)+", p.test), row.chain)
      ) ) == 1L
    ) {
      arr.core <- unlist(regmatches(row.chain, arr.coord))
      if(length(arr.core) != 1L)
        stop("Logic Error: array repeat pattern problem; contact maintainer.")
      # Now replace everything except the front part of the match with zeroes
      core.match <- gregexpr(p.test, arr.core)
      core.pieces <- regmatches(arr.core, core.match)
      if(length(core.pieces) != 1L)
        stop("Logic Error: array repeat pattern problem; contact maintainer.")
      core.proc <- lapply(
        unlist(core.pieces),
        function(x) {
          x.u <- unlist(strsplit(unlist(x), ""))
          if(!length(x.u)) "" else {
            x.u[!!cumsum(x.u == "0")] <- "0" # anything after zero turned to zero
            paste0(x.u, collapse="")
          }
      } )
      regmatches(arr.core, core.match) <- core.proc

      # generate new version of chain with all zeros to sub back in

      row.chain.z <- paste0(rep("0", nchar(row.chain)), collapse="")
      regmatches(row.chain.z, arr.coord) <- arr.core

      # Convert back to vector

      which(unlist(strsplit(row.chain.z, "")) != "0")
    } else {
      integer(0L)
    }
  } else integer(0L)
}
#' Generic Methods to Implement Flexible Guide Line Computations
#'
#' Guide lines are context lines that would not normally be shown as part of a
#' diff because they are too far from any differences, but provide particularly
#' useful contextual information.  Column headers are a common example.
#'
#' \code{Diff} detects these important context lines by looking for patterns in
#' the text of the diff, and then displays these lines in addition to the
#' normal diff output.  Guide lines are marked by a tilde in the gutter, and
#' are typically styled differently than normal context lines.  Keep in mind
#' that guide lines may be far from the diff hunk they are juxtaposed to.  We
#' eschew the device of putting the guide lines in the hunk header as
#' \code{git diff} does because often the column alignment of the guide line is
#' meaningful.
#'
#' Guide lines are detected by the \code{*GuideLines} methods documented here.
#' Each of the \code{diff*} methods (e.g. \code{\link{diffPrint}}) has a
#' corresponding \code{*GuideLines} method (e.g.
#' \code{\link{printGuideLines}}), with the exception of \code{\link{diffCsv}}
#' since that method uses \code{diffPrint} internall.  The \code{*GuideLines}
#' methods expect an R object as the first parameter and the captured display
#' representation of the object in a charater vector as the second.  This allows
#' them to adapt what patterns they are looking for in the character
#' representation of the object.  For example, a \code{list} like object will
#' require a different guide finding strategy than a \code{matrix} object.
#'
#' The default method for \code{printGuideLines} has special handling for 2D
#' objects (e.g. data frames, matrices), arrays, and lists.  If you dislike the
#' default handling you can also define your own methods for matrices, arrays,
#' etc., or alternatively you can pass a guide finding function directly via
#' the \code{guides} parameter to the \code{diff*} methods.  The default method
#' for \code{strGuideLines} highlights top level objects.  The default methods
#' for \code{chrGuideLines} and \code{deparseGuideLines} don't do anything and
#' exit only as a mechanism for providing custom guide line methods.
#'
#' If you have classed objects with special patterns you can define your own
#' methods for them (see examples), though if your objects are S3 you will need
#' to use \code{\link{setOldClass}} as the \code{*GuideLines} generics are S4.
#'
#' @aliases strGuideLines, chrGuideLines, deparseGuideLines
#' @export
#' @rdname guideLines
#' @param obj an R object
#' @param obj.as.chr the character representation of \code{obj} that is used
#'   for computing the diffs
#' @return integer containing values in \code{seq_along(obj.as.chr)}
#' @examples
#' ## Roundabout way of suppressing guide lines for matrices
#' \dontrun{
#' setMethod("printGuideLines", c("matrix", "character"),
#'   function(obj, obj.as.chr) integer(0L)
#' )
#' ## Special guides for "zulu" S3 objects that match lines
#' ## starting in "zulu###" where ### is a nuber
#' setOldClass("zulu")
#' setMethod("printGuideLines", c("zulu", "character"),
#'   function(obj, obj.as.chr) {
#'     if(length(obj) > 20) grep("^zulu[0-9]*", obj.as.chr)
#'     else integer(0L)
#' } )
#' }

setGeneric(
  "printGuideLines",
  function(obj, obj.as.chr) StandardGeneric("printGuideLines")
)
setMethod(
  "printGuideLines", c("ANY", "character"),
  function(obj, obj.as.chr) {
    if(anyNA(obj.as.chr))
      stop("Cannot compute guides if `obj.as.chr` contains NAs")
    if(length(dim(obj)) == 2L) {
      detect_2d_guides(obj.as.chr)
    } else if (is.array(obj)) {
      detect_array_guides(obj.as.chr, dimnames(obj))
    } else if (is.list(obj)) {
      detect_list_guides(obj.as.chr)
    } else integer(0L)
  }
)
#' @export
#' @rdname guideLines

setGeneric(
  "strGuideLines",
  function(obj, obj.as.chr) StandardGeneric("strGuideLines")
)
setMethod("strGuideLines", c("ANY", "character"),
  function(obj, obj.as.chr) {
    if(anyNA(obj.as.chr))
      stop("Cannot compute guides if `obj.as.chr` contains NAs")
    starts.w.dollar <- grepl("^ \\$", obj.as.chr)
    which(starts.w.dollar & !c(tail(starts.w.dollar, -1L), FALSE))
} )

#' @export
#' @rdname guideLines

setGeneric(
  "chrGuideLines",
  function(obj, obj.as.chr) StandardGeneric("chrGuideLines")
)
setMethod("chrGuideLines", c("ANY", "character"),
  function(obj, obj.as.chr) integer(0L)
)
#' @export
#' @rdname guideLines

setGeneric(
  "deparseGuideLines",
  function(obj, obj.as.chr) StandardGeneric("deparseGuideLines")
)
setMethod("deparseGuideLines", c("ANY", "character"),
  function(obj, obj.as.chr) integer(0L)
)
#' @export
#' @rdname guideLines

setGeneric(
  "fileGuideLines",
  function(obj, obj.as.chr) StandardGeneric("fileGuideLines")
)
setMethod("fileGuideLines", c("ANY", "character"),
  function(obj, obj.as.chr) integer(0L)
)


# Helper function to verify guide line computation worked out

apply_guides <- function(obj, obj.as.chr, guide_fun) {
  guide <- try(guide_fun(obj, obj.as.chr))
  msg.extra <- paste0(
    "If you did not define custom `*GuideLines` methods contact maintainer."
  )
  if(inherits(guide, "try-error"))
    stop(
      "`*GuideLines` method produced an error when attempting to compute guide ",
      "lines; ", msg.extra
    )
  if(
    !is.integer(guide) || anyNA(guide) || anyDuplicated(guide) ||
    !all(guide %in% seq_along(obj.as.chr))
  )
    stop(
      "`*GuideLines` method must produce an integer vector containing unique ",
      "index values for the `obj.as.chr` vector; ", msg.extra
    )
  guide
}
make_guides <- function(target, tar.capt, current, cur.capt, guide_fun) {
  tar.guides <- apply_guides(target, tar.capt, guide_fun)
  cur.guides <- apply_guides(current, cur.capt, guide_fun)
  GuideLines(target=tar.guides, current=cur.guides)
}
