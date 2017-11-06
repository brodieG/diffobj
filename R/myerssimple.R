# Copyright (C) 2017  Brodie Gaslam
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

# These are deprecated legacy functions from before we incorporated the
# libmba versions of the myers algo

# Alternate implementation of Myers algorithm in R, without linear space
# modification.  Included here mostly for reference purposes and not intended
# for use since the MBA myers implemenation should be far superior

myers_simple <- function(target, current) {
  path <- myers_simple_int(target, current)
  diff_path_to_diff(path, target, current)
}
myers_simple_int <- function(A, B) {
  N <- length(A)
  M <- length(B)
  MAX <- M + N + 1L
  if(!MAX) return(matrix(integer(0L), ncol=2))
  OFF <- MAX + 1L  # offset to adjust to R indexing
  Vl <- vector("list", MAX)
  for(D in seq_len(MAX) - 1L) {
    Vl[[D + 1L]] <- if(!D) integer(2L * MAX + 1L) else Vl[[D]]
    for(k in seq(-D, D, by=2L)) {
      # not sure of precendence for || vs &&
      # k == -D means x == 0
      V <- Vl[[D + 1L]]
      if(k == -D || (k != D && V[k - 1L + OFF] < V[k + 1L + OFF])) {
        x <- V[k + 1L + OFF]
      } else {
        x <- V[k - 1L + OFF] + 1L
      }
      y <- x - k

      # Move on diagonal
      while (x < N && y < M && A[x + 1L] == B[y + 1L]) {
        x <- x + 1L
        y <- y + 1L
      }
      # Record last match or end; if a mismatch no longer increment

      Vl[[D + 1L]][k + OFF] <- x
      if(x >= N && y >= M) {
        # Create matrix to hold entire result path; should be longest of
        # A and B plus recorded differences

        path.len <- D + max(N, M)
        res <- matrix(integer(1L), nrow=path.len, ncol=2)
        res[path.len, ] <- c(x, y)
        path.len <- path.len - 1L

        for(d in rev(seq_len(D))) {
          Vp <- Vl[[d]]
          break.out <- FALSE
          repeat {
            # can't match to zero since that is the initialized value
            shift.up <- Vp[k + 1L + OFF] == x && x
            shift.left <- Vp[k - 1L + OFF] == x - 1L && x > 1L
            if(x <= 0L && y <= 0L) {
              break
            } else if(!shift.up && !shift.left) {
              # must be on snake or about to hit 0,0
              x <- max(x - 1L, 0L)
              y <- max(y - 1L, 0L)
            } else {
              if(shift.up) {
                y <- y - 1L
                k <- k + 1L
              } else {
                x <- x - 1L
                k <- k - 1L
              }
              break.out <- TRUE
            }
            res[path.len, ] <- c(x, y)
            path.len <- path.len - 1L
            if(break.out) break
          }
        }
        if(any(res < 0L)) {
          # nocov start
          stop(
            "Logic Error: diff generated illegal coords; contact maintainer."
          )
          # nocov end
        }
        return(res)
      }
    }
  }
  stop("Logic Error, should not get here") # nocov
}
# Translates a diff path produced by the simple Myers Algorithm into the
# standard format we use in the rest of the package

diff_path_to_diff <- function(path, target, current) {
  stopifnot(
    is.character(target), is.character(current),
    is.matrix(path), is.integer(path), ncol(path) == 2,
    all(path[, 1L] %in% c(0L, seq_along(target))),
    all(path[, 2L] %in% c(0L, seq_along(current)))
  )
  # Path specifies 0s as well as duplicate coordinates, which we don't use
  # in our other formats.  For dupes, find first value for each index that is
  # lined up with a real value in the other column

  get_dupe <- function(x) {
    base <- !logical(length(x))
    if(!length(y <- which(x != 0L)))
      base[[1L]] <- FALSE else base[[min(y)]] <- FALSE
    base
  }
  cur.dup <- as.logical(ave(path[, 1L], path[, 2L], FUN=get_dupe))
  tar.dup <- as.logical(ave(path[, 2L], path[, 1L], FUN=get_dupe))

  path[!path] <- NA_integer_
  path[tar.dup, 1L] <- NA_integer_
  path[cur.dup, 2L] <- NA_integer_

  # Now create the character equivalents of the path matrix

  tar.path <- target[path[, 1L]]
  cur.path <- current[path[, 2L]]

  # Mark the equalities in the path matrix by setting them negative

  path[which(tar.path == cur.path), ] <- -path[which(tar.path == cur.path), ]

  # Remaining numbers are the mismatches which we will arbitrarily assign to
  # each other; to do so we first split our data into groups of matches and
  # mismatches and do the mapping there-in.  We also get rid of non-matching
  # entries.

  matched <- ifelse(!is.na(path[, 1]) & path[, 1] < 0L, 1L, 0L)
  splits <- cumsum(abs(diff(c(0, matched))))
  chunks <- split.data.frame(path, splits)
  res.tar <- res.cur <- vector("list", length(chunks))
  mm.count <- 0L  # for tracking matched mismatches

  for(i in seq_along(chunks)) {
    x <- chunks[[i]]
    if((neg <- any(x < 0L, na.rm=TRUE)) && !all(x < 0L, na.rm=TRUE))
      stop("Logic Error: match group error; contact maintainer")
    if(neg) {
      # Matches, so equal length and set to zero
      res.tar[[i]] <- res.cur[[i]] <- integer(nrow(x))
    } else {
      # Mismatches
      tar.mm <- Filter(Negate(is.na), x[, 1L])
      cur.mm <- Filter(Negate(is.na), x[, 2L])

      x.min.len <- min(length(tar.mm), length(cur.mm))
      res.tar[[i]] <- res.cur[[i]] <- seq_len(x.min.len) + mm.count
      mm.count <- x.min.len + mm.count
      length(res.tar[[i]]) <- length(tar.mm)
      length(res.cur[[i]]) <- length(cur.mm)
    }
  }
  if(!length(res.tar)) res.tar <- integer()
  if(!length(res.cur)) res.cur <- integer()

  return(list(target=unlist(res.tar), current=unlist(res.cur)))
}
