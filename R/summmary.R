# Copyright (C) 2020 Brodie Gaslam
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

setClass("DiffSummary",
  slots=c(
    max.lines="integer", width="integer", etc="Settings",
    diffs="matrix", all.eq="character",
    scale.threshold="numeric"
  ),
  validity=function(object) {
    if(
      !is.integer(object@diffs) &&
      !identical(rownames(object@diffs), c("match", "delete", "add"))
    )
      return("Invalid diffs object")
    TRUE
  }
)
#' Summary Method for Diff Objects
#'
#' Provides high level count of insertions, deletions, and matches, as well as a
#' \dQuote{map} of where the differences are.
#'
#' Sequences of single operations (e.g. "DDDDD") are compressed provided that
#' compressing them does not distort the relative size of the sequence relative
#' to the longest such sequence in the map by more than \code{scale.threshold}.
#' Since length 1 sequences cannot be further compressed \code{scale.threshold}
#' does not apply to them.
#'
#' @param object at \code{Diff} object
#' @param scale.threshold numeric(1L) between 0 and 1, how much distortion to
#'   allow when creating the summary map, where 0 is none and 1 is as much as
#'   needed to fit under \code{max.lines}, defaults to 0.1
#' @param max.lines integer(1L) how many lines to allow for the summary map,
#'   defaults to 50
#' @param width integer(1L) how many columns wide the output should be, defaults
#'   to \code{getOption("width")}
#' @param ... unused, for compatibility with generic
#' @return a \code{DiffSummary} object
#' ## `pager="off"` for CRAN compliance; you may omit in normal use
#' summary(diffChr(letters, letters[-c(5, 15)], format="raw", pager="off"))

setMethod("summary", "Diff",
  function(
    object, scale.threshold=0.1, max.lines=50L, width=getOption("width"), ...
  ) {
    if(!is.int.1L(max.lines) || max.lines < 1L)
      stop("Argument `max.lines` must be integer(1L) and strictly positive")
    max.lines <- as.integer(max.lines)
    if(!is.int.1L(width) || width < 0L)
      stop("Argument `width` must be integer(1L) and positive")
    if(width < 10L) width <- 10L
    if(
      !is.numeric(scale.threshold) || length(scale.threshold) != 1L ||
      is.na(scale.threshold) || !scale.threshold %bw% c(0, 1)
    )
      stop("Argument `scale.threshold` must be numeric(1L) between 0 and 1")

    diffs.c <- count_diffs_detail(object@diffs)
    # remove context hunks that are duplicated
    match.seq <- rle(!!diffs.c["match", ])
    match.keep <- unlist(
      lapply(
        match.seq$lengths,
        function(x) if(x == 2L) c(TRUE, FALSE) else TRUE
    ) )
    diffs <- diffs.c[, match.keep, drop=FALSE]
    all.eq <- all.equal(object@target, object@current)
    new(
      "DiffSummary", max.lines=max.lines, width=width, etc=object@etc,
      diffs=diffs, all.eq=if(isTRUE(all.eq)) character(0L) else all.eq,
      scale.threshold=scale.threshold
    )
  }
)
#' @rdname finalizeHtml

setMethod("finalizeHtml", c("DiffSummary"),
  function(x, x.chr, ...) {
    js <- ""
    callNextMethod(x, x.chr, js=js, ...)
} )
#' Generate Character Representation of DiffSummary Object
#'
#' @param x a \code{DiffSummary} object
#' @param ... not used, for compatibility with generic
#' @return the summary as a character vector intended to be \code{cat}ed to
#'   terminal
#' @examples
#' as.character(
#'   summary(diffChr(letters, letters[-c(5, 15)], format="raw", pager="off"))
#' )
setMethod("as.character", "DiffSummary",
  function(x, ...) {
    etc <- x@etc
    style <- etc@style
    hunks <- sum(!x@diffs["match", ])
    res <- c(apply(x@diffs, 1L, sum))
    scale.threshold <- x@scale.threshold
    # something seems wrong with next condition
    res <- if(!hunks || !sum(x@diffs[c("delete", "add"), ])) {
      style@summary@body(
        if(length(x@all.eq)) {
          eq.txt <- paste0("- ", x@all.eq)
          paste0(
            c(
              "No visible differences, but objects are not `all.equal`:",
              eq.txt
            ),
            collapse=style@text@line.break
          )
        } else {
          "Objects are `all.equal`"
      } )
    } else {
      pad <- 2L
      width <- x@width - pad

      head <- paste0(
        paste0(
          strwrap(
            sprintf(
              "Found differences in %d hunk%s:", hunks, if(hunks != 1L) "s" else ""
            ),
            width=width
          ),
          collapse=style@text@line.break
        ),
        style@summary@detail(
          paste0(
            strwrap(
              sprintf(
                "%d insertion%s, %d deletion%s, %d match%s (lines)",
                res[["add"]], if(res[["add"]] == 1L) "" else "s",
                res[["delete"]], if(res[["delete"]] == 1L) "" else "s",
                res[["match"]], if(res[["match"]] == 1L) "" else "es"
              ),
              width=width
            ),
          collapse=style@text@line.break
          )
        ),
        collapse=""
      )
      # Compute character screen display

      max.chars <- x@max.lines * width
      diffs <- x@diffs
      scale.threshold <- x@scale.threshold

      # Helper fun to determine if the scale skewed our data too much

      scale_err <- function(orig, scaled, threshold, width) {
        if((width - sum(scaled)) / width > threshold) {
          TRUE
        } else {
          zeroes <- !orig
          orig.nz <- orig[!zeroes]
          scaled.nz <- scaled[!zeroes]
          orig.norm <- orig.nz / max(orig.nz)
          scaled.norm <- scaled.nz / max(scaled.nz)
          any(abs(orig.norm - scaled.norm) > threshold)
        }
      }
      # Scale the data down as small as possible provided we don't violate
      # tolerance.

      diffs.gz <- diffs > 1L
      diffs.nz <- diffs[diffs.gz]
      safety <- 10000L
      tol <- width / 4
      diffs.scale <- diffs

      lo.bound <- lo <- length(diffs.nz)
      hi.bound <- hi <- sum(diffs.nz)

      if(sum(diffs.scale) > width) {
        repeat {
          mp <- ceiling((hi.bound - lo.bound) / 2) + lo.bound
          safety <- safety - 1L
          if(safety < 0L)
            # nocov start
            stop("Logic Error: likely infinite loop; contact maintainer.")
            # nocov end

          # Need to scale down; we know we need at least one char per value
          diffs.nz.s <- pmax(
            round(diffs.nz * (mp - lo) / (hi - lo)), 1L
          )
          diffs.scale[diffs.gz] <- diffs.nz.s
          scale.err <- scale_err(diffs, diffs.scale, scale.threshold, width)
          break.cond <- floor(mp / width) <= floor(lo.bound  / width) ||
            mp >= hi.bound

          if(scale.err) {
            # error, keep increasing lines
            lo.bound <- mp
          } else {
            # no error, check if we can generate an error with a smaller value
            # note hi.bound is always guaranteed to not produce error
            if(break.cond) break
            hi.bound <- mp
          }
        }
      }
      diffs.fin <- diffs.scale

      # Compute scaling factors for display to user

      scale.one <- diffs.scale == 1
      scale.gt.one <- diffs.scale > 1
      s.o.txt <- if(any(scale.one)) {
        s.o.r <- unique(range(diffs[scale.one]))
        if(length(s.o.r) == 1L)
          sprintf("%d:1 for single chars", s.o.r)
        else
          sprintf("%d-%d:1 for single chars", s.o.r[1L], s.o.r[2L])
      }

      s.gt.o.txt <- if(any(scale.gt.one)) {
        s.gt.o.r <- unique(
          range(round(diffs[scale.gt.one] / diffs.scale[scale.gt.one]))
        )
        if(length(s.gt.o.r) == 1L)
          sprintf("%d:1 for char seqs", s.gt.o.r)
        else
          sprintf("%d-%d:1 for char seqs", s.gt.o.r[1L], s.gt.o.r[2L])
      }

      map.txt <- sprintf(
        "Diff map (line:char scale is %s%s%s):",
        if(!is.null(s.o.txt)) s.o.txt else "",
        if(is.null(s.o.txt) && !is.null(s.gt.o.txt)) "" else ", ",
        if(!is.null(s.gt.o.txt)) s.gt.o.txt else ""
      )
      body <- if(style@wrap) strwrap(map.txt, width=x@width) else map.txt

      # Render actual map

      diffs.txt <- character(length(diffs.fin))
      attributes(diffs.txt) <- attributes(diffs.fin)
      symb <- c(match=".", add="I", delete="D")
      use.ansi <- FALSE

      for(i in names(symb)) {
        test <- diffs.txt[i, ] <- vapply(
          diffs.fin[i, ],
          function(x) paste0(rep(symb[[i]], x), collapse=""),
          character(1L)
        )
      }
      # Trim text down to what is displayable in the allowed lines

      txt <- do.call(paste0, as.list(c(diffs.txt)))
      txt <- substr2(txt, 1, max.chars, sgr.supported=etc@sgr.supported)
      txt.w <- unlist(
        if(style@wrap) wrap(txt, width, sgr.supported=etc@sgr.supported)
        else txt
      )
      # Apply ansi styles if warranted

      if(is(style, "StyleAnsi")) {
        old.crayon.opt <- options(crayon.enabled=TRUE)
        on.exit(options(old.crayon.opt), add=TRUE)
      }
      s.f <- style@funs
      txt.w <- gsub(
        symb[["add"]], s.f@word.insert(symb[["add"]]),
        gsub(
          symb[["delete"]], s.f@word.delete(symb[["delete"]]),
          txt.w, fixed=TRUE
        ),
        fixed=TRUE
      )
      extra <- if(sum(diffs.fin) > max.chars) {
        diffs.omitted <- diffs.fin
        diffs.under <- cumsum(diffs.omitted) <= max.chars
        diffs.omitted[diffs.under] <- 0L
        res.om <- apply(diffs.omitted, 1L, sum)
        sprintf(
          paste0(
            "omitting %d deletion%s, %d insertion%s, and %d matche%s; ",
            "increase `max.lines` to %d to show full map"
          ),
          res.om[["delete"]], if(res.om[["delete"]] != 1L) "s" else "",
          res.om[["add"]], if(res.om[["add"]] != 1L) "s" else "",
          res.om[["match"]], if(res.om[["match"]] != 1L) "s" else "",
          ceiling(sum(diffs.scale) / width)
        )
      } else character(0L)

      map <- txt.w
      if(length(extra) && style@wrap) extra <- strwrap(extra, width=width)
      c(
        style@summary@body(
          paste0(
            c(head, body),
            collapse=style@text@line.break
        ) ),
        style@summary@map(c(map, extra))
      )
    }
    fin <- style@funs@container(style@summary@container(res))
    finalize(
      fin, x,
      length(unlist(gregexpr(style@text@line.break, fin, fixed=TRUE))) +
      length(fin)
    )
  }
)
#' Display DiffSummary Objects
#'
#' @param object a \code{DiffSummary} object
#' @return NULL, invisbly
#' show(
#'   summary(diffChr(letters, letters[-c(5, 15)], format="raw", pager="off"))
#' )

setMethod("show", "DiffSummary",
  function(object) {
    show_w_pager(as.character(object), object@etc@style@pager)
    invisible(NULL)
  }
)
