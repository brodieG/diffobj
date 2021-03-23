# Copyright (C) 2021 Brodie Gaslam
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

# borrowed from crayon, will lobby to get it exported

ansi_regex <- paste0("(?:(?:\\x{001b}\\[)|\\x{009b})",
                     "(?:(?:[0-9]{1,3})?(?:(?:;[0-9]{0,3})*)?[A-M|f-m])",
                     "|\\x{001b}[A-M]")

# Function to split a character vector by newlines; handles some special cases

split_new_line <- function(x, sgr.supported) {
  y <- x
  y[!nzchar(x)] <- "\n"
  unlist(strsplit2(y, "\n", sgr.supported=sgr.supported))
}
html_ent_sub <- function(x, style) {
  if(is(style, "StyleHtml") && style@escape.html.entities) {
    x <- gsub("&", "&amp;", x, fixed=TRUE)
    x <- gsub("<", "&lt;", x, fixed=TRUE)
    x <- gsub(">", "&gt;", x, fixed=TRUE)
    x <- gsub("\n", "<br />", x, fixed=TRUE)
    # x <- gsub(" ", "&#32;", x, fixed=TRUE)
  }
  x
}
# Helper function for align_eq; splits up a vector into matched elements and
# interstitial elements, including possibly empty interstitial elements when
# two matches are abutting

align_split <- function(v, m) {
  match.len <- sum(!!m)
  res.len <- match.len * 2L + 1L
  splits <- cumsum(
    c(
      if(length(m)) 1L,
      (!!diff(m) < 0L & !tail(m, -1L)) | (head(m, -1L) & tail(m, -1L))
  ) )
  m.all <- match(m, sort(unique(m[!!m])), nomatch=0L)  # normalize
  m.all[!m.all] <- -ave(m.all, splits, FUN=max)[!m.all]
  m.all[!m.all] <- -match.len - 1L  # trailing zeros
  m.fin <- ifelse(m.all < 0, -m.all * 2 - 1, m.all * 2)
  if(any(diff(m.fin) < 0L))
    stop("Logic Error: non monotonic alignments; contact maintainer") # nocov
  res <- replicate(res.len, character(0L), simplify=FALSE)
  res[unique(m.fin)] <- unname(split(v, m.fin))
  res
}
# Align lists based on equalities on other vectors
#
# This is used for hunks that are word diffed.  Once the word differences are
# accounted for, the remaining strings (A.eq/B.eq) are compared to try to align
# them with a naive algorithm on a line basis.  This works best when lines as a
# whole are equal except for a few differences.  There can be funny situations
# where matched words are on one line in e.g. A, but spread over multiple lines
# in B.  This isn't really handled well currently.
#
# See issue #37.
#
# The A/B vecs  will be split up into matchd elements, and non-matched elements.
# Each matching element will be surrounding by (possibly empty) non-matching
# elements.
#
# Need to reconcile the padding that happens as a result of alignment as well
# as the padding that happens with atomic vectors

align_eq <- function(A, B, x, context) {
  stopifnot(
    is.integer(A), is.integer(B), !anyNA(c(A, B)),
    is(x, "Diff")
  )
  A.fill <- get_dat(x, A, "fill")
  B.fill <- get_dat(x, B, "fill")
  A.fin <- get_dat(x, A, "fin")
  B.fin <- get_dat(x, B, "fin")

  if(context) {             # Nothing to align if this is context hunk
    A.chunks <- list(A.fin)
    B.chunks <- list(B.fin)
  } else {
    etc <- x@etc
    A.eq <- get_dat(x, A, "eq")
    B.eq <- get_dat(x, B, "eq")

    # Cleanup so only relevant stuff is allowed to match

    A.tok.ratio <- get_dat(x, A, "tok.rat")
    B.tok.ratio <- get_dat(x, B, "tok.rat")

    if(etc@align@count.alnum.only) {
      A.eq.trim <- gsub("[^[:alnum:]]", "", A.eq, perl=TRUE)
      B.eq.trim <- gsub("[^[:alnum:]]", "", B.eq, perl=TRUE)
    } else {
      A.eq.trim <- A.eq
      B.eq.trim <- B.eq
    }
    # TBD whether nchar here should be ansi-aware; probably if in alnum only
    # mode...

    A.valid <- which(
      nchar2(A.eq.trim, sgr.supported=etc@sgr.supported) >= etc@align@min.chars &
      A.tok.ratio >= etc@align@threshold
    )
    B.valid <- which(
      nchar2(B.eq.trim, sgr.supported=etc@sgr.supported) >= etc@align@min.chars &
      B.tok.ratio >= etc@align@threshold
    )
    B.eq.seq <- seq_along(B.eq.trim)

    align <- integer(length(A.eq))
    min.match <- 0L

    # Need to match each element in A.eq to B.eq, though each match consumes the
    # match so we can't use `match`; unfortunately this is slow; for context
    # hunks the match is one to one for each line; also, this whole matching
    # needs to be improved (see issue #37)

    if(length(A.valid) & length(B.valid)) {
      B.max <- length(B.valid)
      B.eq.val <- B.eq.trim[B.valid]

      for(i in A.valid) {
        if(min.match >= B.max) break
        B.match <- which(
          A.eq.trim[[i]] == if(min.match)
            tail(B.eq.val, -min.match) else B.eq.val
        )
        if(length(B.match)) {
          align[[i]] <- B.valid[B.match[[1L]] + min.match]
          min.match <- B.match[[1L]] + min.match
        }
      }
    }
    # Group elements together.  We number the interstitial buckest as the
    # negative of the next match.  There are always matches together, split
    # by possibly empty interstitial elements

    align.b <- seq_along(B.eq)
    align.b[!align.b %in% align] <- 0L
    A.chunks <- align_split(A.fin, align)
    B.chunks <- align_split(B.fin, align.b)
  }
  if(length(A.chunks) != length(B.chunks))
    # nocov start
    stop("Logic Error: aligned chunks unequal length; contact maintainer.")
    # nocov end

  list(A=A.chunks, B=B.chunks, A.fill=A.fill, B.fill=B.fill)
}
# Calculate how many lines of screen space are taken up by the diff hunks
#
# `disp.width` should be the available display width, this function computes
# the net real estate account for mode, padding, etc.

nlines <- function(txt, disp.width, mode, etc) {
  # stopifnot(is.character(txt), all(!is.na(txt)))
  capt.width <- calc_width_pad(disp.width, mode)
  pmax(
    1L,
    as.integer(
      ceiling(
        nchar2(txt, sgr.supported=etc@sgr.supported
        ) / capt.width
) ) ) }
# Gets rid of tabs and carriage returns
#
# Assumes each line is one screen line
# @param stops may be a single positive integer value, or a vector of values
#   whereby the last value will be repeated as many times as necessary

strip_hz_c_int <- function(txt, stops, sgr.supported) {

  # remove trailing and leading CRs (need to record if trailing remains to add
  # back at end? no, not really since by structure next thing must be a newline

  w.chr <- nzchar(txt)  # corner case with strsplit and zero length strings
  txt <- gsub("^\r+|\r+$", "", txt)
  has.tabs <- grep("\t", txt, fixed=TRUE)
  has.crs <- grep("\r", txt, fixed=TRUE)
  txt.s <- as.list(txt)
  txt.s[has.crs] <- if(!any(has.crs)) list()
    else strsplit2(txt[has.crs], "\r+", sgr.supported=sgr.supported)

  # Assume \r resets tab stops as it would on a type writer; so now need to
  # generate the set maximum set of possible tab stops; approximate here by
  # using largest stop

  if(length(has.tabs)) {
    max.stop <- max(stops)
    width.w.tabs <- max(
      vapply(
        txt.s[has.tabs], function(x) {
          # add number of chars and number of tabs times max tab length
          sum(
            nchar2(x, sgr.supported=sgr.supported) + (
              vapply(
                strsplit2(x, "\t", sgr.supported=sgr.supported),
                length, integer(1L)
              ) +
              grepl("\t$", x) - 1L
            ) * max.stop
          )
        }, integer(1L)
    ) )
    extra.chars <- width.w.tabs - sum(stops)
    extra.stops <- ceiling(extra.chars / tail(stops, 1L))
    stop.vec <- cumsum(c(stops, rep(tail(stops, 1L), extra.stops)))

    # For each line, assess effects of tabs

    txt.s[has.tabs] <- lapply(txt.s[has.tabs],
      function(x) {
        if(length(h.t <- grep("\t", x, fixed=T))) {
          # workaround for strsplit dropping trailing tabs
          x.t <- sub("\t$", "\t\t", x[h.t])
          x.s <- strsplit2(x.t, "\t", sgr.supported=sgr.supported)

          # Now cycle through each line with tabs and replace them with
          # spaces

          res <- vapply(x.s,
            function(y) {
              topad <- head(y, -1L)
              rest <- tail(y, 1L)
              chrs <- nchar2(topad, sgr.supported=sgr.supported)
              pads <- character(length(topad))
              txt.len <- 0L
              for(i in seq_along(topad)) {
                txt.len <- chrs[i] + txt.len
                tab.stop <- head(which(stop.vec > txt.len), 1L)
                if(!length(tab.stop))
                  # nocov start
                  stop(
                    "Logic Error: failed trying to find tab stop; contact ",
                    "maintainer"
                  )
                  # nocov end
                tab.len <- stop.vec[tab.stop]
                pads[i] <- paste0(rep(" ", tab.len - txt.len), collapse="")
                txt.len <- tab.len
              }
              paste0(paste0(topad, pads, collapse=""), rest)
            },
            character(1L)
          )
          x[h.t] <- res
        }
        x
  } ) }
  # Simulate the effect of \r by collapsing every \r separated element on top
  # of each other with some special handling for ansi escape seqs

  txt.fin <- txt.s
  txt.fin[has.crs] <- vapply(
    txt.s[has.crs],
    function(x) {
      if(length(x) > 1L) {
        chrs <- nchar2(x, sgr.supported=sgr.supported)
        max.disp <- c(tail(rev(cummax(rev(chrs))), -1L), 0L)
        res <- paste0(
          rev(
            substr2(x, max.disp + 1L, chrs, sgr.supported=sgr.supported)
          ),
          collapse=""
        )
        # add back every ANSI esc sequence from last line to very end
        # to ensure that we leave in correct ANSI escaped state

        if(grepl(ansi_regex, res, perl=TRUE)) {
          res <- paste0(
            res,
            gsub(paste0(".*", ansi_regex, ".*"), "\\1", tail(x, 1L), perl=TRUE)
        ) }
        res
      } else x # nocov has.cr elements can't have length zero after split...
    },
    character(1L)
  )
  # txt.fin should only have one long char vectors as elements
  if(!length(txt.fin)) txt else {
    # handle strsplit corner case where splitting empty string
    txt.fin[!nzchar(txt)] <- ""
    unlist(txt.fin)
  }
}
#' Replace Horizontal Spacing Control Characters
#'
#' Removes tabs, newlines, and manipulates the text so that
#' it looks the same as it did with those horizontal control
#' characters embedded.  Currently carriage returns are also processed, but
#' in the future they no longer will be.  This function is used when the
#' \code{convert.hz.white.space} parameter to the
#' \code{\link[=diffPrint]{diff*}} methods is active.  The term \dQuote{strip}
#' is a misnomer that remains for legacy reasons and lazyness.
#'
#' This is an internal function with exposed documentation because it is
#' referenced in an external function's documentation.
#'
#' @keywords internal
#' @param txt character to covert
#' @param stops integer, what tab stops to use
#' @param sgr.supported logical whether the current display device supports
#'   ANSI CSI SGR.  See \code{\link[=diffPrint]{diff*}}'s \code{sgr.supported}
#'   parameter.
#' @return character, `txt` with horizontal control sequences
#'   replaced.

strip_hz_control <- function(txt, stops=8L, sgr.supported) {
  # stopifnot(
  #   is.character(txt), !anyNA(txt),
  #   is.integer(stops), length(stops) >= 1L, !anyNA(stops), all(stops > 0L)
  # )

  # for speed in case no special chars, just skip; obviously this adds a penalty
  # for other cases but it is small

  if(!any(grepl("\n|\t|\r", txt, perl=TRUE))) {
    txt
  } else {
    if(length(has.n <- grep("\n", txt, fixed=TRUE))) {
      txt.l <- as.list(txt)
      txt.l.n <- strsplit2(txt[has.n], "\n", sgr.supported=sgr.supported)
      txt.l[has.n] <- txt.l.n
      txt <- unlist(txt.l)
    }
    has.ansi <- grepl(ansi_regex, txt, perl=TRUE)
    w.ansi <- which(has.ansi)
    wo.ansi <- which(!has.ansi)

    # since for the time being the crayon funs are a bit slow, only us them on
    # strings that are known to have ansi escape sequences

    strip_hz_c_int(txt, stops, sgr.supported=sgr.supported)
  }
}
# Normalize strings so whitespace differences don't show up as differences

normalize_whitespace <- function(txt)
  gsub(" ([[:punct:]])", "\\1", gsub("(\t| )+", " ", trimws(txt)))

# Simple text manip functions

chr_trim <- function(text, width, sgr.supported) {
  stopifnot(all(width > 2L))
  ifelse(
    nchar2(text, sgr.supported=sgr.supported) > width,
    paste0(substr2(text, 1L, width - 2L, sgr.supported=sgr.supported), ".."),
    text
  )
}
rpad <- function(text, width, pad.chr=" ", sgr.supported) {
  stopifnot(is.character(pad.chr), length(pad.chr) == 1L, nchar(pad.chr) == 1L)
  pad.count <- width - nchar2(text, sgr.supported=sgr.supported)
  pad.count[pad.count < 0L] <- 0L
  pad.chrs <- vapply(
    pad.count, function(x) paste0(rep(pad.chr, x), collapse=""), character(1L)
  )
  paste0(text, pad.chrs)
}
# Breaks long character vectors into vectors of length width
#
# Right pads them to full length if requested.  Only attempt to wrap if
# longer than width since wrapping is pretty expensive
#
# Returns a list of split vectors

wrap_int <- function(txt, width, sgr.supported) {
  nchars <- nchar2(txt, sgr.supported=sgr.supported)
  res <- as.list(txt)
  too.wide <- which(nchars > width)
  res[too.wide] <- lapply(
    too.wide,
    function(i) {
      split.end <- seq(
        from=width, by=width, length.out=ceiling(nchars[[i]] / width)
      )
      split.start <- split.end - width + 1L
      substr2(
        rep(txt[[i]], length(split.start)), split.start, split.end,
        sgr.supported=sgr.supported
      )
  } )
  res
}
wrap <- function(txt, width, pad=FALSE, sgr.supported) {
  if(length(grep("\n", txt, fixed=TRUE)))
    # nocov start
    stop("Logic error: wrap input contains newlines; contact maintainer.")
    # nocov end

  # If there are ansi escape sequences, account for them; either way, create
  # a vector of character positions after which we should split our character
  # vector

  has.na <- is.na(txt)
  has.chars <- nchar2(txt, sgr.supported=sgr.supported) & !has.na
  w.chars <- which(has.chars)
  wo.chars <- which(!has.chars & !has.na)

  txt.sub <- txt[has.chars]

  # Wrap differently depending on whether contains ansi or not, exclude zero
  # length char elements

  res.l <- vector("list", length(txt))
  res.l[has.na] <- NA_character_
  res.l[wo.chars] <- ""
  res.l[w.chars] <- wrap_int(txt.sub, width, sgr.supported=sgr.supported)

  # pad if requested

  if(pad) res.l[!has.na] <- 
    lapply(res.l[!has.na], rpad, width=width, sgr.supported=sgr.supported)
  res.l
}
