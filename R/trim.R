# Copyright (C) 2016  Brodie Gaslam
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

# Detect and remove atomic headers

.pat.atom <- "^\\s*\\[[1-9][0-9]*\\]\\s"
.pat.mat <- "^\\s*\\[[1-9]+[0-9]*,\\]\\s"
.pat.attr <- "^attr\\(,\"(\\\\\"|[^\"])*\")$"

# Find first attribute and drop everything after it

up_to_attr <- function(x) {

  attr.id <- grep(.pat.attr, x)
  if(length(attr.id) && attr.id[1L] > 1L) {
    y <- head(x, attr.id[1L] - 1L)
  } else {
    y <- x
  }
  y
}
# Get atomic content on a best-efforts basis
# Note that functionality for named vectors is turned off since they become
# fairly pathological when wrap periodicities are not the same (Issue #43);

which_atomic_cont <- function(x.chr, x) {
  # Limit to everything before attribute

  y <- up_to_attr(x.chr)

  res <- if(!is.null(nm <- names(x))) {
    integer(0L)
    # # name mode; find all lines from output that contain only names

    # nm.tar <- unlist(strsplit(names(x), "\\s+"))
    # y.split <- strsplit(sub("^\\s+", "", y), "\\s+")
    # only.nm <- vapply(y.split, function(z) all(z %in% nm.tar), logical(1L))

    # # Look for TF pattern starting with first TRUE

    # if(any(only.nm)) {
    #   first.t <- min(which(only.nm))
    #   only.nm.sub <- if(first.t > 1L) {
    #     tail(only.nm, -(first.t - 1L))
    #   } else only.nm
    #   only.nm.check <-
    #     only.nm.sub == rep(c(TRUE, FALSE), length.out=length(only.nm.sub))
    #   last.t <- which(!only.nm.check)
    #   last.t <- if(!length(last.t)) length(only.nm.check) + 1L else min(last.t)

    #   # Modulo check makes sure we have full T,F repeats
    #   if(length(last.t) && last.t %% 2L) {
    #     # Ensure that all names are present in the order they are supposed to be
    #     tar.seq <- first.t:(last.t + first.t - 2L)

    #     if(all(unlist(y.split[tar.seq][c(TRUE, FALSE)]) == nm.tar)) {
    #       tar.seq
    #     } else integer(0L)
    #   } else integer(0L)
    # } else integer(0L)
  } else which_atomic_rh(x.chr)
  res
}
# Identify elements that contain row headers

which_atomic_rh <- function(x) {
  stopifnot(is.character(x), !anyNA(x))

  # Now find the row headers if any prior to the attributes

  y <- up_to_attr(x)
  w.pat <- grepl(.pat.atom, y)

  # Grab first set that matches for checking, there could be more particularly
  # if the object in question has attributes, but we explicitly rule out
  # attributes

  w.pat.rle <- rle(w.pat)
  if(any(w.pat.rle$values)) {
    # First get the indices of the patterns that match

    first.block <- min(which(w.pat.rle$values))
    w.pat.start <- sum(head(w.pat.rle$lengths, first.block - 1L), 0L) + 1L
    w.pat.ind <-
      seq(from=w.pat.start, length.out=w.pat.rle$lengths[first.block], by=1L)

    # Re extract those and run checks on them to make sure they truly are
    # what we think they are: width of headers is the same, and numbers
    # increment in equal increments starting at 1

    r.h.rows <- y[w.pat.ind]
    r.h.vals <- regmatches(r.h.rows, regexpr(.pat.atom, r.h.rows))
    r.h.lens.u <- length(unique(nchar(r.h.vals)))
    r.h.nums <- sub(".*?([0-9]+).*", "\\1", r.h.vals, perl=TRUE)
    r.h.nums.u <- length(unique(diff(as.numeric(r.h.nums))))

    if(r.h.nums.u <= 1L && r.h.lens.u == 1L && r.h.nums[[1L]] == "1") {
      w.pat.ind
    } else integer(0L)
  } else integer(0L)
}
strip_atomic_rh <- function(x) {
  stopifnot(is.character(x), !anyNA(x))
  w.r.h <- which_atomic_rh(x)
  x[w.r.h] <- sub(.pat.atom, "", x[w.r.h])
  x
}
# Detect table row headers; a bit lazy, combining all table like into one
# function when in reality more subtlety is warranted; also, we only care about
# numeric row headers.
#
# Matrices used to be done here as well, but then got split off so the `pat`
# argument is legacy

wtr_help <- function(x, pat) {
  # Should expect to find pattern repeated some number of times, and then whole
  # pattern possibly repeated the same number of times separated by the same
  # gap each time if the table is too wide and wraps.

  w.pat <- grepl(pat, x)
  w.pat.rle <- rle(w.pat)

  # It must be the case that the first block of matches occurs after non-matches
  # since the first header should happen first

  if(
    any(w.pat.rle$values) && length(w.pat.rle$values) > 1L &&
    w.pat.rle$values[2L]
  ) {
    tar.len <- w.pat.rle$lengths[2L]
    match.blocks <- w.pat.rle$values & w.pat.rle$lengths == tar.len

    # Only take matches they if alternate T/F

    match.break <- max(
      which(
        match.blocks != rep(c(FALSE, TRUE), length.out=length(match.blocks))
      ),
      0L
    )
    match.valid <- if(match.break) {
      head(match.blocks, match.break - 1L)
    } else match.blocks

    # Make sure that all interstitial blocks are same length and that they all
    # start with at least one space

    interstitial <- which(
      !match.valid & seq_along(match.valid) > 1L &
      seq_along(match.valid) != length(match.valid)
    )
    if(
      !length(interstitial) || (
        length(interstitial) &&
        length(unique(w.pat.rle$lengths[interstitial])) == 1L &&
        all(grepl("^\\s", x[unlist(rle_sub(w.pat.rle, interstitial))]))
      )
    ) {
      # Make sure row headers are the same for each repeating block; start by
      # extracting the actual headers; need to get a list of each sequence of
      # headers

      max.valid <- max(which(match.valid))
      ranges <- rle_sub(
        w.pat.rle, seq_along(w.pat.rle$lengths) <= max.valid & w.pat.rle$values
      )
      heads.l <- regmatches(x, regexec(pat, x))
      heads <- character(length(heads.l))
      heads[w.pat] <- as.character(heads.l[w.pat])

      heads.num <- as.integer(sub(".*?([0-9]+).*", "\\1", heads, perl=TRUE))
      head.ranges <- lapply(ranges, function(x) heads.num[x])

      all.identical <-
        vapply(head.ranges, identical, logical(1L), head.ranges[[1L]])
      all.one.apart <-
        vapply(head.ranges, function(x) all(diff(x) == 1L), logical(1L))

      if(all.identical && all.one.apart && head.ranges[[1L]][1L] == 1L) {
        unlist(ranges)
      } else integer(0L)
    } else integer(0L)
  } else integer(0L)
}
which_table_rh <- function(x) {
  stopifnot(is.character(x), !anyNA(x))
  pat.2 <- "^\\s*[1-9]+[0-9]*:?\\s"       # dfs/tables colon for data.table

  res <- wtr_help(x, pat.2)
  if(length(res)) attr(res, "pat") <- pat.2
  res
}
strip_table_rh <- function(x) {
  w <- which_table_rh(x)
  if(!length(w)) {
    x
  } else {
    pat <- attr(w, "pat")
    if(!is.chr.1L(pat))
      stop("Logic Error: unexpected row header pattern; contact maintainer.")
    x[w] <- sub(pat, "", x[w])
    x
  }
}
# Matrices; should really try to leverage logic in wtr_help, but not quite the
# same

which_matrix_rh <- function(x, dim.names.x) {
  guides <- detect_matrix_guides(x, dim.names.x)
  res <- integer(0L)
  if(length(guides)) {
    pieces <- split_by_guides(x, guides)
    if(!length(pieces)) stop("Logic Error: no matrix pieces")
    # Get all rows matching the matrix row header so long as they are adjacent;
    # this is only really different if there is an attribute in the last piece
    pat.ind <- lapply(
      pieces,
      function(y) {
        pat.match <- grep(.pat.mat, y)
        if(length(pat.match) > 1)
          pat.match[c(TRUE, !cumsum(diff(pat.match) != 1L))]
        else pat.match
      }
    )
    if(
      all(vapply(pat.ind, identical, logical(1L), pat.ind[[1L]])) &&
      (length(pat.ind[[1L]]) == 1L || all(diff(pat.ind[[1L]]) == 1L))
    ) {
      piece.nums <- as.integer(
        sub(".*?([0-9]+).*", "\\1", pieces[[1L]][pat.ind[[1L]]], perl=TRUE)
      )
      if(
        length(piece.nums) && piece.nums[1L] == 1L &&
        (length(piece.nums) == 1L || all(diff(piece.nums) == 1L))
      ) {
        res <- unlist(
          lapply(seq_along(pieces), function(i)
            attr(pieces[[i]], "idx")[pat.ind[[i]]]
  ) ) } } }
  res
}
strip_matrix_rh <- function(x, dim.names.x) {
  to.rep <- which_matrix_rh(x, dim.names.x)
  res <- x
  res[to.rep] <- sub(.pat.mat, "", x[to.rep])
  res
}
# Handle arrays

which_array_rh <- function(x, dim.names.x) {
  arr.h <- detect_array_guides(x, dim.names.x)
  dat <- split_by_guides(x, arr.h)

  # Look for the stuff between array guides; those should be matrix like
  # and have the same rows in each one

  m.h <- lapply(dat, which_matrix_rh, head(dim.names.x, 2L))

  if(length(m.h) && all(vapply(m.h, identical, logical(1L), m.h[[1L]]))) {
    unlist(Map(function(y, z) attr(y, "idx")[z], dat, m.h))
  } else integer(0L)
}
strip_array_rh <- function(x, dim.names.x) {
  inds <- which_array_rh(x, dim.names.x)
  res <- x
  res[inds] <- sub(.pat.mat, "", x[inds])
  res
}
# Lists, need to recurse through the various list components
#
# This is not done super rigorously; the main point of failure is if sub-objects
# produce patterns that match list sub-object headers which may cause confusion
#
# Super inefficient currently since we keep switching back and forth between
# index and trimmed formats so we can re-use `trimPrint`...
#
# Also, right now we are passing the list components with all the trailing
# new lines, and it isn't completely clear that is the right thing to do
#
# Note that we're not actually trimming the list headers themselves since unlike
# in atomics and matrices, etc, the list headers are on their own line and won't
# affect the matching diff of the actual contents of the list

strip_list_rh <- function(x, obj) {
  if(!length(obj)) {
    # empty list, nothing to do, and also if it is nested causes problems later
    x
  } else {
    # Split output into each list component

    list.h <- detect_list_guides(x)
    dat <- split_by_guides(x, list.h, drop.leading=FALSE)
    elements <- flatten_list(obj)

    # Special case where first element in list is deeper than one value, which
    # means there will be leading non-data elements in `dat` that we have to
    # reconstruct; note that if no len then rendered as `list()` so it doesn't
    # get a guide.

    offset <- if(
      is.list(obj[[1L]]) && !is.object(obj[[1L]]) && length(obj[[1L]])
    ) 1L else 0L

    if(length(elements) != length(dat) - offset) {
      # Something went wrong here, so return as is?
      x
    } else {
      # Use `trimPrint` to get indices, and trim back to stuff without row
      # header

      if(offset) {
        hd <- dat[[1L]]
        tl <- tail(dat, -offset)
      } else {
        hd <- NULL
        tl <- dat
      }
      dat.trim <- Map(trimPrint, elements, tl)
      dat.w.o.rh <- Map(
        function(chr, ind) substr(chr, ind[, 1], ind[, 2]), tl, dat.trim
      )
      unlist(
        c(
          list(hd),
          c(
            as.list(x[list.h]), dat.w.o.rh
          )[order(rep(seq_along(list.h), 2))]
      ) )
    }
  }
}
# Very similar logic to lists

strip_s4_rh <- function(x, obj) {
  stopifnot(isS4(obj))

  if(!length(slotNames(obj))) {
    # Nothing to do here
    x
  } else {
    # Split output into each list component

    s4.h <- detect_s4_guides(x, obj)
    dat <- split_by_guides(x, s4.h, drop.leading=FALSE)
    elements <- lapply(slotNames(obj), slot, object=obj)

    dat.trim <- Map(trimPrint, elements, dat)
    dat.w.o.rh <- unlist(
      Map(
        function(chr, ind) substr(chr, ind[, 1], ind[, 2]), dat, dat.trim
    ) )
    if(length(dat.w.o.rh) + length(s4.h) == length(x)) {
      res <- character(length(x))
      res[s4.h] <- x[s4.h]
      res[!seq_along(res) %in% s4.h] <- dat.w.o.rh
      res
    } else {
      x
  } }
}
#' Methods to Remove Unsemantic Text Prior to Diff
#'
#' \code{\link[=diffPrint]{diff*}} methods, in particular \code{diffPrint},
#' modify the text representation of an object prior to running the diff to
#' reduce the incidence of spurious mismatches caused by unsemantic differences.
#' For example, we look to remove matrix row indices and atomic vector indices
#' (i.e. the \samp{[1,]} or \samp{[1]} strings at the beginning of each display
#' line).
#'
#' Consider: \preformatted{
#' > matrix(10:12)
#'      [,1]
#' [1,]   10
#' [2,]   11
#' [3,]   12
#' > matrix(11:12)
#'      [,1]
#' [1,]   11
#' [2,]   12
#' }
#' In this case, the line by line diff would find all rows of the matrix to
#' be mismatched because where the data matches (rows containing
#' 11 and 12) the indices do not.  By trimming out the row indices before
#' the diff, the diff can recognize that row 2 and 3  from the first matrix
#' should be matched to row 1 and 2 of the second.
#'
#' These methods follow a similar interface as the \code{\link[=guides]{guide*}}
#' methods, with one available for each \code{diff*} method except for
#' \code{diffCsv} since that one uses \code{diffPrint} internally.  The
#' unsemantic differences are added back after the diff for display purposes,
#' and are colored in grey to indicate they are ignored in the diff.
#'
#' Currently only \code{trimPrint} and \code{trimStr} do anything meaningful.
#' \code{trimPrint} removes row index headers provided that they are of the
#' default un-named variety.  If you add row names, or if numeric row indices
#' are not ascending from 1, they will not be stripped as those have meaning.
#' \code{trimStr} removes the \samp{..$}, \samp{..-}, and \samp{..@} tokens
#' to minimize spurious matches.
#'
#' You can modify how text is trimmed by providing your own functions to the
#' \code{trim} argument of the \code{diff*} methods, or by defining
#' \code{trim*} methods for your objects.  Note that the return value for these
#' functions is the start and end columns of the text that should be
#' \emph{kept} and used in the diff.
#'
#' As with guides, trimming is on a best efforts basis and may fail with
#' \dQuote{pathological} display representations.  Since the diff still works
#' even with failed trimming this is considered an acceptable compromise.
#' Trimming is more likely to fail with nested recursive structures.
#'
#' @note \code{obj.as.chr} will be as processed by
#'   \code{\link{strip_hz_control}} and as such will not be identical to the
#'   captured output if it contains tabs, newlines, or carriage returns.
#' @rdname trim
#' @name trim
#' @aliases trimPrint, trimStr, trimChr, trimDeparse, trimFile
#' @param obj the object
#' @param obj.as.chr character the \code{print}ed representation of the object
#' @return a \code{length(obj.as.chr)} row and 2 column integer matrix with the
#'   start (first column) and end (second column) character positions of the sub
#'   string to run diffs on.

NULL

#' @export
#' @rdname trim

setGeneric("trimPrint",
  function(obj, obj.as.chr) StandardGeneric("trimPrint") # nocov
)
#' @rdname trim

setMethod(
  "trimPrint", c("ANY", "character"),
  function(obj, obj.as.chr) {
    # Remove the stuff we don't want

    stripped <- if(is.matrix(obj)) {
      strip_matrix_rh(obj.as.chr, dimnames(obj))
    } else if(
      length(dim(obj)) == 2L ||
      (is.ts(obj) && frequency(obj) > 1)
    ) {
      strip_table_rh(obj.as.chr)
    } else if (is.array(obj)) {
      strip_array_rh(obj.as.chr, dimnames(obj))
    } else if(is.atomic(obj)) {
      strip_atomic_rh(obj.as.chr)
    } else if(is.list(obj) && !is.object(obj)) {
      strip_list_rh(obj.as.chr, obj)
    } else if(isS4(obj) && is_default_show_obj(obj)) {
      strip_s4_rh(obj.as.chr, obj)
    } else obj.as.chr

    trim_sub(obj.as.chr, stripped)
  }
)
#' @export
#' @rdname trim

setGeneric("trimStr",
  function(obj, obj.as.chr) StandardGeneric("trimStr") # nocov
)
#' @rdname trim

setMethod(
  "trimStr", c("ANY", "character"),
  function(obj, obj.as.chr) {
    # Remove the stuff we don't want

    pat <- "^ (?: \\.\\.)*(?:\\$|-|@) "
    stripped <- gsub(pat, "", obj.as.chr, perl=TRUE)

    # Figure out the indices that correspond to what we want, knowing that all
    # removals should have occured at front of string

    trim_sub(obj.as.chr, stripped)
  }
)
# Helper function; returns untrimmed objects

trim_identity <- function(obj, obj.as.chr)
  cbind(rep(1L, length(obj.as.chr)), nchar(obj.as.chr))

#' @export
#' @rdname trim

setGeneric(
  "trimChr", function(obj, obj.as.chr) StandardGeneric("trimChr")  # nocov
)

#' @rdname trim

setMethod("trimChr", c("ANY", "character"), trim_identity)

#' @export
#' @rdname trim

setGeneric(
  "trimDeparse",
  function(obj, obj.as.chr) StandardGeneric("trimDeparse")  # nocov
)
#' @rdname trim

setMethod("trimDeparse", c("ANY", "character"), trim_identity)

#' @export
#' @rdname trim

setGeneric(
  "trimFile", function(obj, obj.as.chr) StandardGeneric("trimFile")  # nocov
)

#' @rdname trim

setMethod("trimFile", c("ANY", "character"), trim_identity)

# Helper fun used by trim functions that remove front of strings and rely on
# string comparison to determine trim indices

trim_sub <- function(obj.as.chr, obj.stripped) {
  if(length(obj.as.chr) != length(obj.stripped))
    stop(
      "Logic Error: trimmed string does not have same number of elements as ",
      "original; contact maintainer"
    )
  stripped.chars <- nchar(obj.stripped)
  char.diff <- nchar(obj.as.chr) - stripped.chars
  sub.start <- char.diff + 1L
  sub.end <- sub.start - 1L + stripped.chars

  if(!all(substr(obj.as.chr, sub.start, sub.end) == obj.stripped))
    stop(
      "Logic Error: trimmed string is not a substring of orginal, ",
      "contact maintainer"
    )
  cbind(sub.start, sub.end)
}
# Re-insert the trimmed stuff back into the original string

untrim <- function(dat, word.c, etc) {
  fun <- etc@style@funs@trim
  res <- with(
    dat,
    paste0(
      fun(substr(raw, 0, trim.ind.start - 1L)), word.c,
      fun(substr(raw, trim.ind.end + 1L, nchar(raw) + 1L))
    )
  )
  # substitute blanks

  res[!nzchar(dat$raw)] <- etc@style@blank.sub
  res
}

valid_trim_ind <- function(x)
  if(
    !is.integer(x) || !is.matrix(x) || anyNA(x) || !ncol(x) == 2L
  ) {
    "must be a two column integer matrix with no NAs"
  } else TRUE

apply_trim <- function(obj, obj.as.chr, trim_fun) {
  if(!isTRUE(two.arg <- is.two.arg.fun(trim_fun)))
    stop(
      "Invalid trim function (", two.arg, ").  If you did not customize the ",
      "trim function contact maintainer; see `?trim`"
    )
  trim <- try(trim_fun(obj, obj.as.chr))
  msg.extra <- paste0(
    "If you did not specify a `trim` function or define custom `trim*` ",
    "methods contact maintainer (see `?trim`).  Proceeding without trimming."
  )
  if(inherits(trim, "try-error")) {
    warning(
      "`trim*` method produced an error when attempting to trim ; ", msg.extra
    )
    trim <- cbind(rep(1L, length(obj.as.chr)), nchar(obj.as.chr))
  }
  if(!isTRUE(trim.check <- valid_trim_ind(trim)))
    stop("`trim*` method return value ", trim.check, "; ", msg.extra)
  if(nrow(trim) != length(obj.as.chr))
    stop(
      "`trim*` method output matrix must have as many rows as object ",
      "character representation has elements; ", msg.extra
    )
  trim
}
