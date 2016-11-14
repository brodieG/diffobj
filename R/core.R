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

#' @include s4.R

NULL

#' Generate a character representation of Shortest Edit Sequence
#'
#' @keywords internal
#' @seealso \code{\link{ses}}
#' @param x S4 object of class \code{MyersMbaSes}
#' @param ... unused
#' @return character vector

setMethod("as.character", "MyersMbaSes",
  function(x, ...) {
    dat <- as.data.frame(x)

    # Split our data into sections that have either deletes or inserts and get
    # rid of the matches

    dat <- dat[dat$type != "Match", ]
    d.s <- split(dat, dat$section)

    # For each section, compute whether we should display, change, insert,
    # delete, or both, and based on that append to the ses string

    ses_rng <- function(off, len)
      paste0(off, if(len > 1L) paste0(",", off + len - 1L))

    vapply(
      unname(d.s),
      function(d) {
        del <- sum(d$len[d$type == "Delete"])
        ins <- sum(d$len[d$type == "Insert"])
        if(del) {
          del.first <- which(d$type == "Delete")[[1L]]
          del.off <- d$off[del.first]
        }
        if(ins) {
          ins.first <- which(d$type == "Insert")[[1L]]
          ins.off <- d$off[ins.first]
        }
        if(del && ins) {
          paste0(ses_rng(del.off, del), "c", ses_rng(ins.off, ins))
        } else if (del) {
          paste0(ses_rng(del.off, del), "d", d$last.b[[1L]])
        } else if (ins) {
          paste0(d$last.a[[1L]], "a", ses_rng(ins.off, ins))
        } else {
          stop("Logic Error: unexpected edit type; contact maintainer.") # nocov
        }
      },
      character(1L)
) } )
# Used for mapping edit actions to numbers so we can use numeric matrices
.edit.map <- c("Match", "Insert", "Delete")

setMethod("as.matrix", "MyersMbaSes",
  function(x, row.names=NULL, optional=FALSE, ...) {
    # map del/ins/match to numbers

    len <- length(x@type)

    edit <- match(x@type, .edit.map)
    matches <- .edit.map[edit] == "Match"
    section <- cumsum(matches + c(0L, head(matches, -1L)))

    # Track what the max offset observed so far for elements of the `a` string
    # so that if we have an insert command we can get the insert position in
    # `a`

    last.a <- c(
      if(len) 0L,
      head(
        cummax(
          ifelse(.edit.map[edit] != "Insert", x@offset + x@length, 1L)
        ) - 1L, -1L
    ) )

    # Do same thing with `b`, complicated because the matching entries are all
    # in terms of `a`

    last.b <- c(
      if(len) 0L,
      head(cumsum(ifelse(.edit.map[edit] != "Delete", x@length, 0L)), -1L)
    )
    cbind(
      type=edit, len=x@length, off=x@offset, section=section, last.a=last.a,
      last.b = last.b
    )
} )
setMethod("as.data.frame", "MyersMbaSes",
  function(x, row.names=NULL, optional=FALSE, ...) {
    len <- length(x@type)
    mod <- c("Insert", "Delete")
    dat <- data.frame(type=x@type, len=x@length, off=x@offset)
    matches <- dat$type == "Match"
    dat$section <- cumsum(matches + c(0L, head(matches, -1L)))

    # Track what the max offset observed so far for elements of the `a` string
    # so that if we have an insert command we can get the insert position in
    # `a`

    dat$last.a <- c(
      if(nrow(dat)) 0L,
      head(
        cummax(ifelse(dat$type != "Insert", dat$off + dat$len, 1L)) - 1L, -1L
    ) )

    # Do same thing with `b`, complicated because the matching entries are all
    # in terms of `a`

    dat$last.b <- c(
      if(nrow(dat)) 0L,
      head(cumsum(ifelse(dat$type != "Delete", dat$len, 0L)), -1L)
    )
    dat
} )
#' Shortest Edit Script
#'
#' Computes shortest edit script to convert \code{a} into \code{b} by removing
#' elements from \code{a} and adding elements from \code{b}.  Intended primarily
#' for debugging or for other applications that understand that particular
#' format.  See \href{GNU diff docs}{http://www.gnu.org/software/diffutils/manual/diffutils.html#Detailed-Normal}
#' for how to interpret the symbols.
#'
#' \code{ses} will be much faster than any of the
#' \code{\link[=diffPrint]{diff*}} methods, particularly for large inputs with
#' limited numbers of differences.
#'
#' NAs are treated as the string \dQuote{NA}.  Non-character inputs are coerced
#' to character.
#'
#' @export
#' @param a character
#' @param b character
#' @return character
#' @examples
#' ses(letters[1:3], letters[2:4])

ses <- function(a, b) {
  if(!is.character(a)) {
    a <- try(as.character(a))
    if(inherits(a, "try-error"))
      stop("Argument `a` is not character and could not be coerced to such")
  }
  if(!is.character(b)) {
    b <- try(as.character(b))
    if(inherits(b, "try-error"))
      stop("Argument `b` is not character and could not be coerced to such")
  }
  if(anyNA(a)) a[is.na(a)] <- "NA"
  if(anyNA(b)) b[is.na(b)] <- "NA"
  as.character(diff_myers(a, b))
}

#' Diff two character vectors
#'
#' Implementation of Myer's Diff algorithm with linear space refinement
#' originally implemented by Mike B. Allen as part of
#' \href{libmba}{http://www.ioplex.com/~miallen/libmba/}
#' version 0.9.1.  This implementation is a heavily modified version of the
#' original C code and is not compatible with the \code{libmba} library.
#' The C code is simplified by using fixed size arrays instead of variable
#' ones for tracking the longest reaching paths and for recording the shortest
#' edit scripts.  Additionally all error handling and memory allocation calls
#' have been moved to the internal R functions designed to handle those things.
#' A failover result is provided in the case where max diffs allowed is
#' exceeded.  Ability to provide custom comparison functions is removed.
#'
#' @keywords internal
#' @param a character
#' @param b character
#' @param max.diffs integer(1L) how many differences before giving up; set to
#'   zero to allow as many as there are
#' @return list
#' @useDynLib diffobj, .registration=TRUE, .fixes="DIFFOBJ_"

diff_myers <- function(a, b, max.diffs=0L) {
  stopifnot(
    is.character(a), is.character(b), all(!is.na(c(a, b))), is.int.1L(max.diffs)
  )
  res <- .Call(DIFFOBJ_diffobj, a, b, max.diffs)
  res <- setNames(res, c("type", "length", "offset", "diffs"))
  types <- .edit.map
  res$type <- factor(types[res$type], levels=types)
  res$offset <- res$offset + 1L  # C 0-indexing originally
  res.s4 <- try(do.call("new", c(list("MyersMbaSes", a=a, b=b), res)))
  if(inherits(res.s4, "try-error"))
    stop(
      "Logic Error: unable to instantiate shortest edit script object; contact ",
      "maintainer."
    )
  res.s4
}
# Print Method for Shortest Edit Path
#
# Bare bones display of shortest edit path using GNU diff conventions
#
# @param object object to display
# @return character the shortest edit path character representation, invisibly
# @rdname diffobj_s4method_doc

#' @rdname diffobj_s4method_doc

setMethod("show", "MyersMbaSes",
  function(object) {
    res <- as.character(object)
    cat(res, sep="\n")
    invisible(res)
} )

#' Summary Method for Shortest Edit Path
#'
#' Displays the data required to generate the shortest edit path for comparison
#' between two strings.
#'
#' @export
#' @keywords internal
#' @param object the \code{diff_myers} object to display
#' @param with.match logical(1L) whether to show what text the edit command
#'   refers to
#' @param ... forwarded to the data frame print method used to actually display
#'   the data
#' @return whatever the data frame print method returns

setMethod("summary", "MyersMbaSes",
  function(object, with.match=FALSE, ...) {
    what <- vapply(
      seq_along(object@type),
      function(y) {
        t <- object@type[[y]]
        o <- object@offset[[y]]
        l <- object@length[[y]]
        vec <- if(t == "Insert") object@b else object@a
        paste0(vec[o:(o + l - 1L)], collapse="")
      },
      character(1L)
    )
    res <- data.frame(
      type=object@type, string=what, len=object@length, offset=object@offset
    )
    if(!with.match) res <- res[-2L]
    print(res, ...)
} )
# mode is display mode (sidebyside, etc.)
# diff.mode is whether we are doing the first pass line diff, or doing the
#   in-hunk or word-wrap versions
# warn is to allow us to suppress warnings after first hunk warning

char_diff <- function(x, y, context=-1L, etc, diff.mode, warn) {
  stopifnot(
    diff.mode %in% c("line", "hunk", "wrap"),
    isTRUE(warn) || identical(warn, FALSE)
  )
  max.diffs <- etc@max.diffs
  diff <- diff_myers(x, y, max.diffs)  # probably shouldn't generate S4

  hunks <- as.hunks(diff, etc=etc)
  hit.diffs.max <- FALSE
  if(diff@diffs < 0L) {
    hit.diffs.max <- TRUE
    diff@diffs <- -diff@diffs
    diff.msg <- c(
      line="overall", hunk="in-hunk word", wrap="atomic wrap-word"
    )
    if(warn)
      warning(
        "Exceeded diff limit during diff computation (",
        diff@diffs, " vs. ", max.diffs, " allowed); ",
        diff.msg[diff.mode], " diff is likely not optimal",
        call.=FALSE
      )
  }
  # used to be a `DiffDiffs` object, but too slow

  list(hunks=hunks, hit.diffs.max=hit.diffs.max)
}
# Compute the character representation of a hunk header

make_hh <- function(h.g, mode, tar.dat, cur.dat, ranges.orig) {
  h.ids <- vapply(h.g, "[[", integer(1L), "id")
  h.head <- vapply(h.g, "[[", logical(1L), "guide")

  # exclude header hunks from contributing to range, and adjust ranges for
  # possible fill lines added to the data

  h.ids.nh <- h.ids[!h.head]

  tar.rng <- find_rng(h.ids.nh, ranges.orig[1:2, , drop=FALSE], tar.dat$fill)
  tar.rng.f <- cumsum(!tar.dat$fill)[tar.rng]

  cur.rng <- find_rng(h.ids.nh, ranges.orig[3:4, , drop=FALSE], cur.dat$fill)
  cur.rng.f <- cumsum(!cur.dat$fill)[cur.rng]

  hh.a <- paste0(rng_as_chr(tar.rng.f))
  hh.b <- paste0(rng_as_chr(cur.rng.f))

  if(mode == "sidebyside") sprintf("@@ %s @@", c(hh.a, hh.b)) else {
    sprintf("@@ %s / %s @@", hh.a, hh.b)
  }
}
# Do not allow `useBytes=TRUE` if there are any matches with `useBytes=FALSE`
#
# Clean up word.ind to avoid issues where we have mixed UTF-8 and non
# UTF-8 strings in different hunks, and gregexpr is trying to optimize
# buy using useBytes=TRUE in ASCII only strings without knowing that in a
# different hunk there are UTF-8 strings

fix_word_ind <- function(x) {
  matches <- vapply(x, function(y) length(y) > 1L || y != -1L, logical(1L))
  useBytes <- vapply(x, function(y) isTRUE(attr(y, "useBytes")), logical(1L))
  if(!all(useBytes[matches])) x <- lapply(x, `attr<-`, "useBytes", NULL)
  x
}
# Variation on `char_diff` used for the overall diff where we don't need
# to worry about overhead from creating the `Diff` object

line_diff <- function(
  target, current, tar.capt, cur.capt, context, etc, warn=TRUE, strip=TRUE
) {
  if(!is.valid.guide.fun(etc@guides))
    stop(
      "Logic Error: guides are not a valid guide function; contact maintainer"
    )
  etc@guide.lines <-
    make_guides(target, tar.capt, current, cur.capt, etc@guides)

  # Need to remove new lines as the processed captures do that anyway and we
  # end up with mismatched lengths if we don't

  if(any(nzchar(tar.capt))) tar.capt <- split_new_line(tar.capt)
  if(any(nzchar(cur.capt))) cur.capt <- split_new_line(cur.capt)

  # Some debate as to whether we want to do this first, or last.  First has
  # many benefits so that everything is consistent, width calcs can work fine,
  # etc., but only issue is that user provided trim functions might not expect
  # the transformation of the data; this needs to be documented with the trim
  # docs.

  tar.capt.p <- tar.capt
  cur.capt.p <- cur.capt
  if(etc@convert.hz.white.space) {
    tar.capt.p <- strip_hz_control(tar.capt, stops=etc@tab.stops)
    cur.capt.p <- strip_hz_control(cur.capt, stops=etc@tab.stops)
  }
  # Apply trimming to remove row heads, etc, but only if something gets trimmed
  # from both elements

  tar.trim.ind <- apply_trim(target, tar.capt.p, etc@trim)
  tar.trim <- do.call(
    substr, list(tar.capt.p, tar.trim.ind[, 1L], tar.trim.ind[, 2L])
  )
  cur.trim.ind <- apply_trim(current, cur.capt.p, etc@trim)
  cur.trim <- do.call(
    substr, list(cur.capt.p, cur.trim.ind[, 1L], cur.trim.ind[, 2L])
  )
  if(identical(tar.trim, tar.capt.p) || identical(cur.trim, cur.capt.p)) {
    # didn't trim in both, so go back to original
    tar.trim <- tar.capt.p
    tar.trim.ind <- cbind(rep(1L, length(tar.capt.p)), nchar(tar.capt.p))
    cur.trim <- cur.capt.p
    cur.trim.ind <- cbind(rep(1L, length(cur.capt.p)), nchar(cur.capt.p))
  }
  # Remove whitespace if warranted

  tar.comp <- tar.trim
  cur.comp <- cur.trim

  if(etc@ignore.white.space) {
    tar.comp <- normalize_whitespace(tar.comp)
    cur.comp <- normalize_whitespace(cur.comp)
  }
  # Word diff is done in three steps: create an empty template vector structured
  # as the result of a call to `gregexpr` without matches, if dealing with
  # compliant atomic vectors in print mode, then update with the word diff
  # matches, finally, update with in-hunk word diffs for hunks that don't have
  # any existing word diffs:

  # Set up data lists with all relevant info; need to pass to diff_word so it
  # can be modified.
  # - orig: the very original string
  # - raw: the original captured text line by line, with strip_hz applied
  # - trim: as above, but with row meta data removed
  # - trim.ind: the indices used to re-insert `trim` into `raw`
  # - comp: the strings that will have the line diffs run on
  # - eq: the portion of `trim` that is equal post word-diff
  # - fin: the final character string for display to user
  # - word.ind: for use by `regmatches<-` to re-insert colored words
  # - tok.rat: for use by `align_eq` when lining up lines within hunks

  tar.dat <- list(
    orig=tar.capt, raw=tar.capt.p, trim=tar.trim,
    trim.ind.start=tar.trim.ind[, 1L], trim.ind.end=tar.trim.ind[, 2L],
    comp=tar.comp, eq=tar.comp, fin=tar.capt.p,
    fill=logical(length(tar.capt.p)),
    word.ind=replicate(length(tar.capt.p), .word.diff.atom, simplify=FALSE),
    tok.rat=rep(1, length(tar.capt.p))
  )
  cur.dat <- list(
    orig=cur.capt, raw=cur.capt.p, trim=cur.trim,
    trim.ind.start=cur.trim.ind[, 1L], trim.ind.end=cur.trim.ind[, 2L],
    comp=cur.comp, eq=cur.comp, fin=cur.capt.p,
    fill=logical(length(cur.capt.p)),
    word.ind=replicate(length(cur.capt.p), .word.diff.atom, simplify=FALSE),
    tok.rat=rep(1, length(cur.capt.p))
  )
  # Word diffs in wrapped form is atomic; note this will potentially change
  # the length of the vectors

  tar.wrap.diff <- integer(0L)
  cur.wrap.diff <- integer(0L)

  if(
    is.atomic(target) && is.atomic(current) &&
    length(tar.rh <- which_atomic_cont(tar.capt.p, target)) &&
    length(cur.rh <- which_atomic_cont(cur.capt.p, current)) &&
    etc@unwrap.atomic && etc@word.diff
  ) {
    if(!all(diff(tar.rh) == 1L) || !all(diff(cur.rh)) == 1L){
      # nocov start
      stop("Logic Error, row headers must be sequential; contact maintainer.")
      # nocov end
    }
    # Only do this for the portion of the data that actually matches up with
    # the atomic row headers (NOTE: need to check what happens with named
    # vectors without row headers)

    tar.dat.sub <- lapply(tar.dat, "[", tar.rh)
    cur.dat.sub <- lapply(cur.dat, "[", cur.rh)

    diff.word <- diff_word2(
      tar.dat.sub, cur.dat.sub, tar.ind=tar.rh, cur.ind=cur.rh,
      diff.mode="wrap", warn=warn, etc=etc
    )
    warn <- !diff.word$hit.diffs.max
    dat.up <- function(orig, new, ind) {
      if(!length(ind)) {
        orig
      } else {
        start <- orig[seq_along(orig) < min(ind)]
        end <- orig[seq_along(orig) > max(ind)]
        c(start, new, end)
      }
    }
    tar.dat <-
      Map(dat.up, tar.dat, diff.word$tar.dat, MoreArgs=list(ind=tar.rh))
    cur.dat <-
      Map(dat.up, cur.dat, diff.word$cur.dat, MoreArgs=list(ind=cur.rh))

    # Mark the lines that were wrapped diffed; necessary b/c tar/cur.rh are
    # defined even if other conditions to get in this loop are not, and also
    # because the addition of the fill lines moves everything around

    tar.wrap.diff <- seq_along(tar.dat$fill)[!tar.dat$fill][tar.rh]
    cur.wrap.diff <- seq_along(cur.dat$fill)[!cur.dat$fill][cur.rh]
  }
  # Actual line diff

  diffs <- char_diff(
    tar.dat$comp, cur.dat$comp, etc=etc, diff.mode="line", warn=warn
  )
  warn <- !diffs$hit.diffs.max

  hunks.flat <- diffs$hunks

  # For each of those hunks, run the word diffs and store the results in the
  # word.diffs list; bad part here is that we keep overwriting the overall
  # diff data for each hunk, which might be slow

  if(etc@word.diff) {
    # Word diffs on hunks, excluding all values that have already been wrap
    # diffed as in tar.rh and cur.rh

    for(h.a in hunks.flat) {
      if(h.a$context) next
      h.a.ind <- c(h.a$A, h.a$B)
      h.a.tar.ind <- setdiff(h.a.ind[h.a.ind > 0], tar.wrap.diff)
      h.a.cur.ind <- setdiff(abs(h.a.ind[h.a.ind < 0]), cur.wrap.diff)
      h.a.w.d <- diff_word2(
        tar.dat, cur.dat, h.a.tar.ind, h.a.cur.ind, diff.mode="hunk", warn=warn,
        etc=etc
      )
      tar.dat <- h.a.w.d$tar.dat
      cur.dat <- h.a.w.d$cur.dat
      warn <- !h.a.w.d$hit.diffs.max
    }
    # Compute the token ratios

    tok_ratio_compute <- function(z) vapply(
      z,
      function(y)
        if(is.null(wc <- attr(y, "word.count"))) 1
        else max(0, (wc - length(y)) / wc),
      numeric(1L)
    )
    tar.dat$tok.rat <- tok_ratio_compute(tar.dat$word.ind)
    cur.dat$tok.rat <- tok_ratio_compute(cur.dat$word.ind)

    # Deal with mixed UTF/plain strings

    tar.dat$word.ind <- fix_word_ind(tar.dat$word.ind)
    cur.dat$word.ind <- fix_word_ind(cur.dat$word.ind)

    # Remove different words to make equal strings

    tar.dat$eq <- with(tar.dat, `regmatches<-`(trim, word.ind, value=""))
    cur.dat$eq <- with(cur.dat, `regmatches<-`(trim, word.ind, value=""))
  }
  # Instantiate result

  hunk.grps.raw <- group_hunks(
    hunks.flat, etc=etc, tar.capt=tar.dat$raw, cur.capt=cur.dat$raw
  )
  gutter.dat <- etc@gutter
  max.w <- etc@text.width

  # Recompute line limit accounting for banner len, needed for correct trim

  etc.group <- etc
  if(etc.group@line.limit[[1L]] >= 0L) {
    etc.group@line.limit <-
      pmax(integer(2L), etc@line.limit - banner_len(etc@mode))
  }
  # Trim hunks to the extent needed to make sure we fit in lines

  hunk.grps <- trim_hunks(hunk.grps.raw, etc.group, tar.dat$raw, cur.dat$raw)
  hunks.flat <- unlist(hunk.grps, recursive=FALSE)

  # Compact to width of widest element, so retrieve all char values; also
  # need to generate all the hunk headers b/c we need to use them in width
  # computation as well; under no circumstances are hunk headers allowed to
  # wrap as they are always assumed to take one line.
  #
  # Note: this used to be done after trimming / subbing, which is technically
  # better since we might have trimmed away long rows, but we need to do it
  # here so that we can can record the new text width in the outgoing object;
  # also, logic a bit circuitous b/c this was originally done elsewhere; might
  # be faster to use tar.dat and cur.dat directly

  chr.ind <- unlist(lapply(hunks.flat, "[", c("A", "B")))
  chr.dat <- get_dat_raw(chr.ind, tar.dat$raw, cur.dat$raw)
  chr.size <- integer(length(chr.dat))

  ranges <- vapply(
    hunks.flat, function(h.a) c(h.a$tar.rng.trim, h.a$cur.rng.trim),
    integer(4L)
  )
  # compute ranges excluding fill lines
  rng_non_fill <- function(rng, fill) {
    if(!rng[[1L]]) rng else {
      rng.seq <- seq(rng[[1L]], rng[[2L]], by=1L)
      seq.not.fill <- rng.seq[!rng.seq %in% fill]
      if(!length(seq.not.fill)) {
        integer(2L)
      } else {
        range(seq.not.fill)
  } } }
  ranges.orig <- vapply(
    hunks.flat, function(h.a) {
      with(
        h.a, c(
          rng_non_fill(tar.rng.sub, which(tar.dat$fill)),
          rng_non_fill(cur.rng.sub, which(cur.dat$fill))
      ) )
    },
    integer(4L)
  )
  # We need a version of ranges that adjust for the fill lines that are counted
  # in the ranges but don't represent actual lines of output.  This does mean
  # that adjusted ranges are not necessarily contiguous

  hunk.heads <-
    lapply(hunk.grps, make_hh, etc@mode, tar.dat, cur.dat, ranges.orig)
  h.h.chars <- nchar(chr_trim(unlist(hunk.heads), etc@line.width))

  chr.size <- etc@style@nchar.fun(chr.dat)
  max.col.w <- max(
    max(0L, chr.size, .min.width + gutter.dat@width), h.h.chars
  )
  max.w <- if(max.col.w < max.w) max.col.w else max.w

  # future calculations should assume narrower display

  etc@text.width <- max.w
  etc@line.width <- max.w + gutter.dat@width

  new(
    "Diff", diffs=hunk.grps, target=target, current=current,
    hit.diffs.max=!warn, tar.dat=tar.dat, cur.dat=cur.dat, etc=etc,
    hunk.heads=hunk.heads
  )
}
