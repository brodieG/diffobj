# Copyright (C) 2018  Brodie Gaslam
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

# Split by guides; used by nested structures to retrieve contents within
# guides.  Each element has an attribute indicating the indices from the
# text element it was drawn from
#
# @param drop.leading keeps the section preceding guides; originally this was
#   always dropped, but caused problems with lists of depth > 1

split_by_guides <- function(txt, guides, drop.leading=TRUE) {
  stopifnot(
    is.character(txt), !anyNA(txt), is.integer(guides),
    all(guides %in% seq_along(txt))
  )
  empty <- list(`attr<-`(txt, "idx", seq_along(txt)))

  if(!length(guides)) {
    empty
  } else {
    guide.l <- logical(length(txt))
    guide.l[guides] <- TRUE
    sections <- cumsum(c(if(guides[1L] == 1L) 1L else 0L, diff(guide.l) == 1L))
    ids <- seq_along(txt)

    # remove actual guidelines

    ids.net <- ids[-guides]
    sec.net <- sections[-guides]
    txt.net <- txt[-guides]

    # split and drop leading stuff if it exists (those with section == 0)

    dat <- unname(split(txt.net, sec.net))
    ind <- unname(split(ids.net, sec.net))

    if(drop.leading) {
      dat <- tail(dat, max(sec.net))
      ind <- tail(ind, max(sec.net))
    }
    # Generate indices and attach them to each element of list

    Map(`attr<-`, dat, "idx", ind )
  }
}
# Detect which rows are likely to be meta data rows (e.g. headers) in tabular
# data (data.frames, timeseries with freq > 1).
#
# note due to ts use, can't use rownames, colnames, etc.
#
# Also, right now we're overloading a bunch of different formats (data.table,
# data.frame, etc.  Probably would be better to separate the regexes into
# different functions and keep the wrapping logic in here).

detect_2d_guides <- function(txt) {
  stopifnot(is.character(txt))
  # Start by looking for first row that leads spaces, this should be the
  # beginning of the actual data, typically the column headers. This ways we can
  # skip the meta data in tibbles and the like

  res <- integer(0L)
  if(any(crayon::has_style(txt))) txt <- crayon::strip_style(txt)
  first.spaces <- grep("^\\s+\\S+", txt)

  if(length(first.spaces)) {
    # Now look for data

    first.space <- min(first.spaces)
    space.rows <-
      !grepl("^\\S+|^\\s+[0-9]+|^\\s+---\\s*$", txt) &
      seq_along(txt) >= first.space

    if(!any(space.rows) || all(space.rows)) {
      if(length(space.rows)) res <- 1L
    } else {
      head.row <- min(which(space.rows))
      first.row <- min(which(!space.rows & seq_along(space.rows) > head.row))
      last.row <- max(which(!space.rows))

      # Between first.row and last.row, look for repeating sequences of head rows
      # and non head rows; should have the same number of each for each block in
      # a wrapped 2d object

      if(last.row > head.row) {
        space.bw <- space.rows[head.row:last.row]
        seq.dat <- vapply(
          split(space.bw, cumsum(c(TRUE, diff(space.bw) == 1L))),
          FUN=function(x) c(sum(x), sum(!x)),
          integer(2L)
        )
        # Which of the sets of true and false head rows have the same repeating
        # sequence as the first?  One thing to think about is what happens when
        # print gets truncated; should allow last in sequence to have fewer rows,
        # but we don't do that yet...

        seq.diffs <- abs(apply(seq.dat, 1L, diff))
        valid.rep <- max(
          which(
            cumsum(colSums(cbind(integer(2L), seq.diffs))) == 0L
          ),
          0L
        )
        if(valid.rep) {
          res <- which(
            rep(rep(c(TRUE, FALSE), valid.rep), seq.dat[seq_len(valid.rep * 2L)])
          ) + head.row - 1L
  } } } }
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
# Matrices

detect_matrix_guides <- function(txt, dim.n) {
  stopifnot(
    is.character(txt), !anyNA(txt),
    is.null(dim.n) || (is.list(dim.n) && length(dim.n) == 2L)
  )
  n.d.n <- names(dim.n)
  row.n <- n.d.n[1L]
  col.n <- n.d.n[2L]
  # try to guard against dimnames that contain regex
  # identify which lines could be row and col headers

  n.p <- "(\\[|\\]|\\(|\\)|\\{|\\}|\\*|\\+|\\?|\\.|\\^|\\$|\\\\|\\|)"
  c.h <- if(!is.null(col.n) && nchar(col.n)) {
    col.pat <- sprintf("^\\s{2,}%s$", gsub(n.p, "\\\1", col.n))
    grepl(col.pat, txt)
  } else {
    rep(FALSE, length(txt))
  }
  r.h <- if(!is.null(row.n) && nchar(row.n)) {
    # a bit lazy, should include col headers as well
    row.pat <- sprintf("^%s\\s+\\S+", gsub(n.p, "\\\1", row.n))
    grepl(row.pat, txt)
  } else {
    pat.extra <- if(!is.null(dim.n[[2L]]) && is.character(dim.n[[2L]])) {
      paste0(c("", gsub(n.p, "\\\1", dim.n[[2L]])), collapse="|")
    }
    grepl(paste0("^\\s+(\\[,[1-9]+[0-9]*\\]", pat.extra, ")(\\s|$)"), txt)
  }
  # Classify each line depending on what pattern it matches so we can then
  # analyze sequences and determine which are valid

  row.types <- integer(length(txt))
  row.types[r.h] <- 1L                   # row meta / col headers
  row.types[c.h] <- 2L                   # col meta

  mx.starts <- if(is.null(n.d.n)) {
    mx.start.num <- 1L
    which(row.types == mx.start.num)
  } else {
    mx.start.num <- 2L
    tmp <- which(row.types == mx.start.num)
    if(sum(r.h) == sum(c.h) && identical(which(c.h) + 1L, which(r.h))) {
      tmp
    } else integer(0L)
  }
  mx.start <- head(mx.starts, 1L)

  if(length(mx.start)) {
    # Now  try to see if pattern repeats to identify the full list of wrapped
    # guides, and return the indices that are part of repeating pattern

    mx.end <- head(mx.starts[which(mx.starts > mx.start)], 1L) - 1L
    if(!length(mx.end)) mx.end <- length(txt)

    pat.inds <- mx.start:(mx.end)
    template <- rep(
      row.types[pat.inds],
      floor((length(txt) - mx.start + 1L) / length(pat.inds))
    )
    which(head(row.types, length(template)) == template & !!template) +
      mx.start - 1L
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
  # Detect patterns for higher dimensions, and then use the matrix guide
  # finding functions to get additional guides

  dim.guides <- which(grepl("^, ,", txt))
  blanks <- which(txt == "")

  if(
    length(dim.guides) && length(blanks) &&
    all(dim.guides + 1L %in% blanks) &&
    (length(dim.guides) == 1L || length(unique(diff(dim.guides)) == 1L))
  ) {
    # Make sure within each array section there is a matrix representation

    dim.guide.fin <- sort(c(dim.guides, dim.guides + 1L))
    sub.dat <- split_by_guides(txt, dim.guide.fin)
    heads <- lapply(sub.dat, detect_matrix_guides, head(dim.n, 2L))

    if(
      all(vapply(heads, identical, logical(1L), heads[[1L]])) &&
      all(vapply(heads, length, integer(1L)))
    )
      dim.guide.fin else integer(0L)
  } else integer(0L)
}
# Utility fun to determin whether an object would be shown with the default show
# method

is_default_show_obj <- function(obj) {
  stopifnot(isS4(obj))
  s.m <- selectMethod("show", class(obj))
  identical(
    class(s.m),
    structure("derivedDefaultMethod", package = "methods")
  )
}
# Basic S4 guide detection, does not handle nesting or anything fancy like that
# and could easily be fooled

detect_s4_guides <- function(txt, obj) {
  stopifnot(isS4(obj))

  # Only try to do this if relying on default S4 show method

  if(is_default_show_obj(obj)) {
    # this could be an issue if they start using curly quotes or whatever...
    guides <- c(
      sprintf("An object of class \"%s\"", class(obj)),
      sprintf("Slot \"%s\":", slotNames(obj))
    )
    guides.loc <- which(txt %in% guides)
    guides.txt <- txt[guides.loc]

    if(!identical(guides, guides.txt)) {
      integer()
    } else {
      guides.loc
    }
  } else integer()
}
#' Generic Methods to Implement Flexible Guide Line Computations
#'
#' Guides are context lines that would normally be omitted from the
#' diff because they are too far from any differences, but provide particularly
#' useful contextual information.  Column headers are a common example.
#' Modifying guide finding is an advanced feature intended for package
#' developers that want special treatment for the display output of their
#' objects.
#'
#' \code{Diff} detects these important context lines by looking for patterns in
#' the text of the diff, and then displays these lines in addition to the
#' normal diff output.  Guides are marked by a tilde in the gutter, and
#' are typically styled differently than normal context lines, by default in
#' grey.  Guides may be far from the diff hunk they are juxtaposed to.  We
#' eschew the device of putting the guides in the hunk header as \code{git diff}
#' does because often the column alignment of the guide line is meaningful.
#'
#' Guides are detected by the \code{guides*} methods documented here.
#' Each of the \code{diff*} methods (e.g. \code{\link{diffPrint}}) has a
#' corresponding \code{guides*} method (e.g.
#' \code{\link{guidesPrint}}), with the exception of \code{\link{diffCsv}}
#' since that method uses \code{diffPrint} internally.  The \code{guides*}
#' methods expect an R object as the first parameter and the captured display
#' representation of the object in a character vector as the second.  The
#' function should then identify which elements in the character representation
#' should be treated as guides, and should return the numeric indices for them.
#'
#' The original object is passed as the first argument so that the generic can
#' dispatch on it, and so the methods may adjust their guide finding behavior
#' to data that is easily retrievable from the object, but less so from the
#' character representation thereof.
#'
#' The default method for \code{guidesPrint} has special handling for 2D
#' objects (e.g. data frames, matrices), arrays, time series, tables, lists, and
#' S4 objects that use the default \code{show} method.  Guide finding is on a
#' best efforts basis and may fail if your objects contain \dQuote{pathological}
#' display representations.  Since the diff will still work with failed
#' \code{guides} finding we consider this an acceptable compromise.  Guide
#' finding is more likely to fail with nested recursive structures.
#'
#' \code{guidesStr} highlights top level objects.  The default methods for the
#' other \code{guide*} generics do not do anything and exist only as a mechanism
#' for providing custom guide line methods.
#'
#' If you dislike the default handling you can also define your own methods for
#' matrices, arrays, etc., or alternatively you can pass a guide finding
#' function directly via the \code{guides} parameter to the \code{diff*}
#' methods.
#'
#' If you have classed objects with special patterns you can define your own
#' methods for them (see examples), though if your objects are S3 you will need
#' to use \code{\link{setOldClass}} as the \code{guides*} generics are S4.
#'
#' @note The mechanism for identifying guides will almost certainly change in
#'   the future to allow for better handling of nested guides, so if you do
#'   implement custom guideline methods do so with the understanding that they
#'   will likely be deprecated in one of the future releases.
#'
#' @aliases guidesPrint, guidesStr, guidesChr, guidesDeparse
#' @rdname guides
#' @name guides
#' @param obj an R object
#' @param obj.as.chr the character representation of \code{obj} that is used
#'   for computing the diffs
#' @return integer containing values in \code{seq_along(obj.as.chr)}
#' @examples
#' ## Roundabout way of suppressing guides for matrices
#' setMethod("guidesPrint", c("matrix", "character"),
#'   function(obj, obj.as.chr) integer(0L)
#' )
#' ## Special guides for "zulu" S3 objects that match lines
#' ## starting in "zulu###" where ### is a nuber
#' setOldClass("zulu")
#' setMethod("guidesPrint", c("zulu", "character"),
#'   function(obj, obj.as.chr) {
#'     if(length(obj) > 20) grep("^zulu[0-9]*", obj.as.chr)
#'     else integer(0L)
#' } )

NULL

#' @export
#' @rdname guides

setGeneric(
  "guidesPrint",
  function(obj, obj.as.chr) StandardGeneric("guidesPrint") # nocov
)
#' @rdname guides

setMethod(
  "guidesPrint", c("ANY", "character"),
  function(obj, obj.as.chr) {
    if(anyNA(obj.as.chr))
      stop("Cannot compute guides if `obj.as.chr` contains NAs")
    if(is.matrix(obj)) {
      detect_matrix_guides(obj.as.chr, dimnames(obj))
    } else if(
      length(dim(obj)) == 2L ||
      (is.ts(obj) && frequency(obj) > 1)
    ) {
      detect_2d_guides(obj.as.chr)
    } else if (is.array(obj)) {
      detect_array_guides(obj.as.chr, dimnames(obj))
    } else if (is.list(obj)) {
      detect_list_guides(obj.as.chr)
    } else if (isS4(obj)) {
      detect_s4_guides(obj.as.chr, obj)
    } else integer(0L)
  }
)
#' @export
#' @rdname guides

setGeneric(
  "guidesStr",
  function(obj, obj.as.chr) StandardGeneric("guidesStr")  # nocov
)
#' @rdname guides

setMethod("guidesStr", c("ANY", "character"),
  function(obj, obj.as.chr) {
    if(anyNA(obj.as.chr))
      stop("Cannot compute guides if `obj.as.chr` contains NAs")
    starts.w.dollar <- grepl("^ \\$", obj.as.chr)
    which(starts.w.dollar & !c(tail(starts.w.dollar, -1L), FALSE))
} )

#' @export
#' @rdname guides

setGeneric(
  "guidesChr",
  function(obj, obj.as.chr) StandardGeneric("guidesChr") # nocov
)
#' @rdname guides

setMethod("guidesChr", c("ANY", "character"),
  function(obj, obj.as.chr) integer(0L)
)
#' @export
#' @rdname guides

setGeneric(
  "guidesDeparse",
  function(obj, obj.as.chr) StandardGeneric("guidesDeparse") # nocov
)
#' @rdname guides

setMethod("guidesDeparse", c("ANY", "character"),
  function(obj, obj.as.chr) integer(0L)
)
#' @export
#' @rdname guides

setGeneric(
  "guidesFile",
  function(obj, obj.as.chr) StandardGeneric("guidesFile") # nocov
)
#' @rdname guides

setMethod("guidesFile", c("ANY", "character"),
  function(obj, obj.as.chr) integer(0L)
)
# Helper function to verify guide line computation worked out

apply_guides <- function(obj, obj.as.chr, guide_fun) {
  guide <- try(guide_fun(obj, obj.as.chr))
  msg.extra <- paste0(
    "If you did not specify a `guides` function or define custom `guides*` ",
    "methods contact maintainer (see `?guides`).  Proceeding without guides."
  )
  if(inherits(guide, "try-error")) {
    warning(
      "`guides*` method produced an error when attempting to compute guide ",
      "lines ; ", msg.extra
    )
    guide <- integer()
  }
  if(
    !is.integer(guide) || anyNA(guide) || anyDuplicated(guide) ||
    !all(guide %in% seq_along(obj.as.chr))
  )
    stop(
      "`guides*` method must produce an integer vector containing unique ",
      "index values for the `obj.as.chr` vector; ", msg.extra
    )
  guide
}
make_guides <- function(target, tar.capt, current, cur.capt, guide_fun) {
  tar.guides <- apply_guides(target, tar.capt, guide_fun)
  cur.guides <- apply_guides(current, cur.capt, guide_fun)
  GuideLines(target=tar.guides, current=cur.guides)
}
