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


# Convert ses data into raw hunks that include both match hunks as well as
# actual hunks
#
# These hunks are then processed into hunk groups in a separate step
# (see `group_hunks`).
#
# @return a list of atomic hunks, each containing integer vectors A and B where
#   positive numbers reference character lines from target and negative ones
#   from current.  For "context" and "sidebyside" mode the A vector will contain
#   the lines from target, and the B vector the lines from current.  For
#   "unified" only the A vector is populated.  In addition to the A and B
#   vectors some other meta data is tracked, such as the range of the hunks is
#   also stored as tar.rng and cur.rng; mostly inferrable from the actual data
#   in the hunks, except that in unified mode we no longer have the actual
#   context strings from the `current` vector.
#
# starting to have second thoughts about removing all the non index data from
# hunks, particularly because it makes the line length calc a pita.

setGeneric("as.hunks", function(x, etc, ...) standardGeneric("as.hunks"))
setMethod("as.hunks", c("MyersMbaSes", "Settings"),
  function(
    x, etc, ...
  ) {
    # Split our data into sections that have either deletes/inserts or matches

    dat <- as.matrix(x)
    sects <- unique(dat[, "section"])
    j <- 0L

    res.l <- if(!nrow(dat)) {
      # Minimum one empty hunk if nothing; make this a context hunk to indicate
      # that there are no differences.  This used to be a non-context hunk

      list(
        list(
          id=1L, A=integer(0L), B=integer(0L),
          context=TRUE, guide=FALSE, tar.rng=integer(2L), cur.rng=integer(2L),
          tar.rng.sub=integer(2L), cur.rng.sub=integer(2L),
          tar.rng.trim=integer(2L), cur.rng.trim=integer(2L),
          completely.empty=TRUE
        )
      )
    } else {
      lapply(
        seq_along(sects),
        function(i) {
          s <- sects[i]
          d <- dat[which(dat[, "section"] == s), , drop=FALSE]
          d.del <- d[which(.edit.map[d[, "type"]] == "Delete"), ,drop=FALSE]
          d.ins <- d[which(.edit.map[d[, "type"]] == "Insert"), ,drop=FALSE]
          d.mtc <- d[which(.edit.map[d[, "type"]] == "Match"), ,drop=FALSE]
          del.len <- sum(d.del[, "len"])
          ins.len <- sum(d.ins[, "len"])
          mtc.len <- sum(d.mtc[, "len"])
          tar.len <- del.len + mtc.len
          cur.len <- ins.len + mtc.len

          # atomic hunks may only be del/ins or match, not both

          if((del.len || ins.len) && mtc.len || !(del.len + ins.len + mtc.len))
            stop("Logic Error: unknown edit types; contact maintainer.") # nocov

          # Figure out where previous hunk left off

          del.last <- if(nrow(d.del)) d.del[1L, "last.a"] else d[1L, "last.a"]
          ins.last <- if(nrow(d.ins)) d.ins[1L, "last.b"] else d[1L, "last.b"]
          A.start <- unname(del.last)
          B.start <- unname(ins.last)

          # record `cur` indices as negatives

          tar <- seq_len(tar.len) + A.start
          cur <- -(seq_len(cur.len) + B.start)

          context <- !!mtc.len

          A <- switch(
            etc@mode, context=tar,
            unified=c(tar, if(!context) cur), sidebyside=tar,
            stop("Logic Error: unknown mode; contact maintainer.")
          )
          B <- switch(
            etc@mode, context=cur, unified=integer(), sidebyside=cur,
            stop("Logic Error: unknown mode; contact maintainer.")
          )
          # compute ranges

          tar.rng <- cur.rng <- integer(2L)
          if(tar.len) tar.rng <- c(A.start + 1L, A.start + tar.len)
          if(cur.len) cur.rng <- c(B.start + 1L, B.start + cur.len)

          list(
            id=i, A=A, B=B, context=context, guide=FALSE,
            tar.rng=tar.rng, cur.rng=cur.rng,
            tar.rng.sub=tar.rng, cur.rng.sub=cur.rng,
            tar.rng.trim=tar.rng, cur.rng.trim=cur.rng,
            completely.empty=FALSE
          )
    } ) }
    res.l
} )

# Group hunks together based on context, in "auto" mode we find the context
# that maximizes lines displayed while adhering to line and hunk limits
# Definitely not very efficient since we re-run code multiple times we
# probably don't need to.
#
#   Important: context atomic hunks are duplicated anytime there is enough
#   context that we only show part of the context hunk.
#
# @return a list containing lists of atomic hunks.  Each of these sub-lists
#   of atomic hunks is treated as a "hunk", but is really a combination of
#   context and hunks which we will refer to as "hunk group".  In each hunk
#   group, There may be as little as one hunk with no context, or many hunks and
#   context if the context between hunks is not sufficient to meet the requested
#   context, in which case the hunks bleed together forming these hunk groups.

group_hunks <- function(hunks, etc, tar.capt, cur.capt) {
  context <- etc@context
  line.limit <- etc@line.limit
  ctx.val <- if(is(context, "AutoContext")) {
    len <- diff_line_len(
      p_and_t_hunks(hunks, ctx.val=context@max, etc=etc),
      etc=etc, tar.capt=tar.capt, cur.capt=cur.capt
    )
    len.min <- diff_line_len(
      p_and_t_hunks(hunks, ctx.val=context@min, etc=etc),
      etc=etc, tar.capt=tar.capt, cur.capt=cur.capt
    )
    if(line.limit[[1L]] < 0L) {
      context@max
    } else if(len.min > line.limit[[1L]]) {
      context@min
    } else {
      ctx.max <- ctx.hi <- ctx <- context@max
      ctx.lo <- context@min
      safety <- 0L

      repeat {
        if((safety <- safety + 1L) > ctx.max)
          # nocov start
          stop(
            "Logic Error: stuck trying to find auto-context; contact ",
            "maintainer."
          )
          # nocov end
        if(len > line.limit[[1L]] && ctx - ctx.lo > 1L) {
          ctx.hi <- ctx
          ctx <- as.integer((ctx - ctx.lo) / 2)
        } else if (len < line.limit[[1L]] && ctx.hi - ctx > 1L) {
          ctx.lo <- ctx
          ctx <- ctx + as.integer(ceiling(ctx.hi - ctx) / 2)
        } else if (len > line.limit[[1L]]) {
          # unable to get something small enough, but we know min context
          # works from inital test
          ctx <- context@min
          break
        } else if (len <= line.limit[[1L]]) {
          break
        }
        len <- diff_line_len(
          p_and_t_hunks(hunks, ctx.val=ctx, etc=etc),
          etc=etc, tar.capt=tar.capt, cur.capt=cur.capt
        )
      }
      ctx
    }
  } else context

  res <- process_hunks(hunks, ctx.val=ctx.val, etc=etc)
  res
}
# process the hunks and also drop off groups that exceed limit
#
# used exclusively when we are trying to auto-calculate context

p_and_t_hunks <- function(hunks.raw, ctx.val, etc) {
  c.all <- process_hunks(hunks.raw, ctx.val, etc)
  hunk.limit <- etc@hunk.limit
  if(hunk.limit[[1L]] >= 0L && length(c.all) > hunk.limit[[1L]])
    c.all <- c.all[seq_along(hunk.limit[[2L]])]
  c.all
}

# Subset hunks; should only ever be subsetting context hunks

hunk_sub <- function(hunk, op, n) {
  stopifnot(
    op %in% c("head", "tail"),
    hunk$context, all(hunk$tar.rng.sub),
    length(hunk$tar.rng.sub) == length(hunk$cur.rng.sub),
    diff(hunk$tar.rng.sub) == diff(hunk$cur.rng.sub),
    length(hunk$tar.rng.sub) == 2L
  )
  hunk.len <- diff(hunk$tar.rng.sub) + 1L
  len.diff <- hunk.len - n
  if(len.diff >= 0) {
    nm <- c("A", "B", "A.tok.ratio", "B.tok.ratio")
    hunk[nm] <- lapply(hunk[nm], op, n)

    # Need to recompute ranges

    if(n) {
      if(op == "tail") {
        hunk$tar.rng.trim[[1L]] <- hunk$tar.rng.sub[[1L]] <-
          hunk$tar.rng.sub[[1L]] + len.diff
        hunk$cur.rng.trim[[1L]] <- hunk$cur.rng.sub[[1L]] <-
          hunk$cur.rng.sub[[1L]] + len.diff
      } else {
        hunk$tar.rng.trim[[2L]] <- hunk$tar.rng.sub[[2L]] <-
          hunk$tar.rng.sub[[2L]] - len.diff
        hunk$cur.rng.trim[[2L]] <- hunk$cur.rng.sub[[2L]] <-
          hunk$cur.rng.sub[[2L]] - len.diff
      }
    } else {
      hunk$tar.rng.trim <- hunk$cur.rng.trim <- hunk$tar.rng.sub <-
        hunk$cur.rng.sub <- integer(2L)
    }
  }
  hunk
}
# Figure Out Context for Each Chunk
#
# If a hunk bleeds into another due to context then it becomes part of the
# other hunk.
#
# This will group atomic hunks into hunk groups with matching line in excess of
# context removed.

process_hunks <- function(x, ctx.val, etc) {
  context <- ctx.val
  ctx.vec <- vapply(x, "[[", logical(1L), "context")
  if(!all(abs(diff(ctx.vec)) == 1L))
    # nocov start
    stop(
      "Logic Error: atomic hunks not interspersing context; contact maintainer."
    )
    # nocov end

  hunk.len <- length(x)

  # Special cases, including only one hunk or forcing only one hunk group, or
  # no differences

  if(context < 0L || hunk.len < 2L || !any(ctx.vec)) {
    res.l <- list(x)
  } else {
    # Normal cases; allocate maximum possible number of elements, may need fewer
    # if hunks bleed into each other

    res.l <- vector("list", sum(!ctx.vec))

    # Jump through every second value as those are the mismatch hunks, though
    # first figure out if first hunk is mismatching, and merge hunks.  This
    # is likely not super efficient as we keep growing a list, though the only
    # thing we are actually re-allocating is the list index really, at least if
    # R is being smart about not copying the list contents (which as of 3.1 I
    # think it is...)

    i <- if(ctx.vec[[1L]]) 2L else 1L
    j <- 1L
    while(i <= hunk.len) {
      # Merge left

      res.l[[j]] <- if(i - 1L)
        list(hunk_sub(x[[i - 1L]], "tail", context), x[[i]]) else x[i]

      # Merge right

      if(i < hunk.len) {
        # Hunks bleed into next hunk due to context; note that i + 1L will always
        # be a context hunk, so $A is fully representative

        while(
          i < hunk.len && length(x[[i + 1L]]$A) <= context * 2 &&
          i + 1L < length(x)
        ) {
          res.l[[j]] <- append(res.l[[j]], x[i + 1L])
          if(i < hunk.len - 1L)
            res.l[[j]] <- append(res.l[[j]], x[i + 2L])
          i <- i + 2L
        }
        # Context enough to cause a break

        if(i < hunk.len) {
          res.l[[j]] <- append(
            res.l[[j]], list(hunk_sub(x[[i + 1L]], "head", context))
      ) } }
      j <- j + 1L
      i <- i + 2L
    }
    length(res.l) <- j - 1L
  }
  # Add back the guide hunks if needed they didn't make it in as part of the
  # context or differences.  It should be the case that the only spot that could
  # have missing hunk guides is the first hunk in a hunk group if it is a
  # context hunk

  # First, determine which guides if any need to be added back; need to do it
  # first because it is possible that a guide is present at the end context
  # of the prior hunk group

  # Helper fun to pull out indices of guide.lines

  get_guides <- function(hunk, rows, mode) {
    stopifnot(hunk$context)
    rng <- hunk[[sprintf("%s.rng", mode)]]
    rng.sub <- hunk[[sprintf("%s.rng.sub", mode)]]
    h.rows <- rows[which(!rows %bw% rng.sub & rows %bw% rng)]

    # If context hunk already contains guide row and there is a non guide at
    # beginning of hunk, then we don't need to return a guide row

    if(any(rows %bw% rng.sub) && !rng.sub[[1L]] %in% rows) {
      integer(0L)
    } else {
      # special case where the first row in the subbed hunk is a context row;
      # note we need to look at the first non-blank row; since this has to be
      # a context hunk we can just look at A.chr

      first.is.guide <- FALSE

      if(rng.sub[[1L]] %in% rows) {
        first.is.guide <- TRUE
        h.rows <- c(h.rows, rng.sub[[1L]])
      }
      # we want all guide.lines that abut the last matched guide row

      if(length(h.rows)) {
        h.fin <-
          h.rows[seq(to=max(h.rows), length.out=length(h.rows)) == h.rows]
        if(first.is.guide) h.fin <- head(h.fin, -1L)
        # convert back to indeces relative to hunk
        h.fin - rng[[1L]] + 1L
      } else integer(0L)
    }
  }
  for(k in seq_along(res.l)) {
    if(length(res.l[[k]]) && res.l[[k]][[1L]]$context) {
      h <- res.l[[k]][[1L]]
      h.o <- x[[res.l[[k]][[1L]]$id]]  # retrieve original untrimmed hunk
      if(!
        identical(
          h$tar.rng.sub,
          h$cur.rng.sub - h$cur.rng.sub[1L] + h$tar.rng.sub[1L]
      ) )
        stop("Logic Error: unequal context hunks; contact mainainer") # nocov

      # since in a context hunk, everything in tar and cur is the same, so
      # we just need to recompute the `cur` guidelines relative to tar indices
      # since the guidelines need not be the same (e.g., in lists that are
      # mostly the same, but deeper in one object, guideline will be deepest
      # index entry, which will be different.

      tar.cand.guides <- intersect(
        etc@guide.lines@target, seq(h$tar.rng[1L], h$tar.rng[2L], by=1L)
      )
      cur.cand.guides <- intersect(
        etc@guide.lines@current, seq(h$cur.rng[1L], h$cur.rng[2L], by=1L)
      ) - h$cur.rng[1L] + h$tar.rng[1L]
      h.guides <- get_guides(
        h, unique(c(tar.cand.guides, cur.cand.guides)), "tar"
      )
      if(length(h.guides)) {
        h.h <- hunk_sub(h.o, "head", max(h.guides))
        tail.ind <- if(length(h.guides) == 1L) 1L else
          diff(range(h.guides)) + 1L
        h.fin <- hunk_sub(h.h, "tail", tail.ind)
        h.fin$guide <- TRUE
        res.l[[k]] <- c(list(h.fin), res.l[[k]])
  } } }
  # Finalize, including sizing correctly, and setting the ids to the right
  # values since we potentially duplicated some context hunks

  res.fin <- res.l
  k <- 1L
  for(i in seq_along(res.fin)) {
    for(j in seq_along(res.fin[[i]])) {
      res.fin[[i]][[j]][["id"]] <- k
      k <- k + 1L
    }
  }
  res.fin
}
# Account for overhead / side by sideness in width calculations
# Internal funs

hunk_len <- function(hunk.id, hunks, tar.capt, cur.capt, etc) {
  disp.width <- etc@disp.width
  mode <- etc@mode
  hunk <- hunks[[hunk.id]]
  A.lines <-
    nlines(get_dat_raw(hunk$A, tar.capt, cur.capt), disp.width, mode, etc)
  B.lines <-
    nlines(get_dat_raw(hunk$B, tar.capt, cur.capt), disp.width, mode, etc)

  # Depending on each mode, figure out how to set up the lines;
  # straightforward except for context where we need to account for the
  # fact that all the A of a hunk group are shown first, and then all
  # the B are shown

  lines.out <- switch(
    mode,
    context=c(A.lines, if(!hunk$guide) -B.lines),
    unified=c(A.lines),
    sidebyside={
      max.len <- max(length(A.lines), length(B.lines))
      length(A.lines) <- length(B.lines) <- max.len
      c(pmax(A.lines, B.lines, na.rm=TRUE))
    },
    stop("Logic Error: unknown mode '", mode, "' contact maintainer")
  )
  # Make sure that line.id refers to the position of the line in either
  # original A or B vector

  l.o.len <- length(lines.out)
  line.id <- integer(l.o.len)
  l.gt.z <- lines.out > 0L
  l.gt.z.w <- which(l.gt.z)
  line.id[l.gt.z.w] <- seq_along(l.gt.z.w)
  l.lt.z.w <- which(!l.gt.z)
  line.id[l.lt.z.w] <- seq_along(l.lt.z.w)
  cbind(
    hunk.id=if(length(lines.out)) hunk.id else integer(),
    line.id=unname(line.id), len=lines.out
  )
}
hunk_grp_len <- function(
  hunk.grp.id, hunk.grps, etc, tar.capt, cur.capt
) {
  mode <- etc@mode
  hunks <- hunk.grps[[hunk.grp.id]]
  hunks.proc <- lapply(
    seq_along(hunks), hunk_len, hunks=hunks, etc=etc,
    tar.capt=tar.capt, cur.capt=cur.capt
  )
  res.tmp <- do.call(rbind, hunks.proc)
  res <- cbind(grp.id=if(nrow(res.tmp)) hunk.grp.id else integer(0L), res.tmp)

  # Need to make sure all positives are first, and all negatives second, if
  # there are negatives (context mode); also, if the first hunk in a hunk
  # group, add a line for the hunk header, though hunk header itself is added
  # later

  extra <- if(length(hunks)) 1L else 0L
  if(identical(mode, "context"))
    res <- res[order(res[, "len"] < 0L), , drop=FALSE]
  if(
    identical(mode, "context") &&
    length(negs <- which(res[, "len"] < 0L)) &&
    length(poss <- which(res[, "len"] > 0L))
  ) {
    # Add one for hunk header, one for context separator; remember, that lengths
    # in the B hunk are counted negatively
    res[1L, "len"] <- res[1L, "len"] + extra
    res[negs[[1L]], "len"] <- res[negs[[1L]], "len"] - extra
  } else if(nrow(res)) {
    res[1L, "len"] <- res[1L, "len"] + extra
  }
  res
}
# Compute how many lines the display version of the diff will take, meta
# lines (used for hunk guides) are denoted by negatives
#
# count lines for each remaining hunk and figure out if we need to cut some
# hunks off; note that "negative" lengths indicate the lines being counted
# originated from the B hunk in context mode

get_hunk_chr_lens <- function(hunk.grps, etc, tar.capt, cur.capt) {
  mode <- etc@mode
  disp.width <- etc@disp.width
  # Generate a matrix with hunk group id, hunk id, and wrapped length of each
  # line that we can use to figure out what to show

  do.call(
    rbind,
    lapply(
      seq_along(hunk.grps), hunk_grp_len, etc=etc, tar.capt=tar.capt,
      cur.capt=cur.capt, hunk.grps=hunk.grps
  ) )
}
# Compute total diff length in lines

diff_line_len <- function(hunk.grps, etc, tar.capt, cur.capt) {
  max(
    0L,
    cumsum(
      get_hunk_chr_lens(
        hunk.grps, etc=etc, tar.capt=tar.capt, cur.capt=cur.capt
      )[, "len"]
    )
  ) + banner_len(etc@mode)
}
# completely.empty used to highlight difference between hunks that technically
# contain a header and no data vs those that can't even contain a header;
# unfortunately a legacy of poor design choice in how headers are handled

empty_hunk_grp <- function(h.g) {
  for(j in seq_along(h.g)) {
    h.g[[j]][c("tar.rng.trim", "cur.rng.trim")] <-
      list(integer(2L), integer(2L))
    h.g[[j]]$completely.empty <- TRUE
  }
  h.g
}
# Remove hunk groups and atomic hunks that exceed the line limit
#
# Return value is a hunk group list, with an attribute indicating how many
# hunks and lines were  trimmed

trim_hunk <- function(hunk, type, line.id) {
  stopifnot(type %in% c("tar", "cur"))
  rng.idx <- sprintf("%s.rng.trim", type)
  hunk[[rng.idx]] <- if(!line.id) integer(2L) else {
    if(all(hunk[[rng.idx]])) {
      c(
        hunk[[rng.idx]][[1L]],
        min(hunk[[rng.idx]][[1L]] + line.id - 1L, hunk[[rng.idx]][[2L]])
      )
    } else integer(2L)
  }
  hunk
}
trim_hunks <- function(hunk.grps, etc, tar.raw, cur.raw) {
  stopifnot(is(etc, "Settings"))

  mode <- etc@mode
  disp.width <- etc@disp.width
  hunk.limit <- etc@hunk.limit
  line.limit <- etc@line.limit
  diffs.orig <- count_diffs(hunk.grps)

  hunk.grps.count <- length(hunk.grps)
  if(hunk.limit[[1L]] < 0L) hunk.limit <- rep(hunk.grps.count, 2L)
  hunk.limit.act <- if(hunk.grps.count > hunk.limit[[1L]]) hunk.limit[[2L]]

  hunk.grps.omitted <- max(0L, hunk.grps.count - hunk.limit.act)
  hunk.grps.used <- min(hunk.grps.count, hunk.limit.act)
  hunk.grps <- hunk.grps[seq_len(hunk.grps.used)]

  lines <- get_hunk_chr_lens(
    hunk.grps, etc=etc, tar.capt=tar.raw, cur.capt=cur.raw
  )
  cum.len <- cumsum(abs(lines[, "len"]))
  cut.off <- -1L
  lines.omitted <- 0L
  lines.total <- max(0L, tail(cum.len, 1L))
  if(line.limit[[1L]] < 0L) {
    cut.off <- max(0L, cum.len)
  } else if(any(cum.len > line.limit[[1L]])) {
    cut.off <- max(0L, cum.len[cum.len <= line.limit[[2L]]])
  }
  if(cut.off > 0) {
    lines.omitted <- lines.total - cut.off
    cut.dat <- lines[max(which(cum.len <= cut.off)), ]
    grp.cut <- cut.dat[["grp.id"]]
    hunk.cut <- cut.dat[["hunk.id"]]
    line.cut <- cut.dat[["line.id"]]
    line.neg <- cut.dat[["len"]] < 0

    # completely trim hunks that will not be shown

    grps.to.cut <- setdiff(seq_along(hunk.grps), seq_len(grp.cut))
    for(i in grps.to.cut) hunk.grps[[i]] <- empty_hunk_grp(hunk.grps[[i]])

    hunk.grps.used <- grp.cut
    hunk.grps.omitted <- max(0L, hunk.grps.count - grp.cut)

    # Remove excess lines from the atomic hunks based on the limits; we don't
    # update the ranges as those should still indicate what the original
    # untrimmed range was

    # special case for first hunk in group since we need to account for hunk
    # header that takes up a line; this is not ideal, hunk header should be
    # made part of hunks eventually

    if(mode == "context") {
      # Context tricky because every atomic hunk B data is displayed after all
      # the A data

      for(i in seq_along(hunk.grps[[grp.cut]])) {
        hunk.atom <- hunk.grps[[grp.cut]][[i]]
        if(!line.neg) {  # means all B blocks must be dropped
          hunk.atom <- trim_hunk(hunk.atom, "cur", 0L)
          if(i > hunk.cut) {
            hunk.atom <- trim_hunk(hunk.atom, "tar", 0L)
          } else if (i == hunk.cut) {
            hunk.atom <- trim_hunk(hunk.atom, "tar", line.cut)
          }
        } else {
          if(i > hunk.cut) {
            hunk.atom <- trim_hunk(hunk.atom, "cur", 0L)
          } else if (i == hunk.cut) {
            hunk.atom <- trim_hunk(hunk.atom, "cur", line.cut)
          }
        }
        hunk.grps[[grp.cut]][[i]] <- hunk.atom
      }
    } else {
      hunk.atom <- hunk.grps[[grp.cut]][[hunk.cut]]
      hunk.atom <- trim_hunk(hunk.atom, "tar", line.cut)
      if(mode == "unified") {
        # Need to share lines between tar and cur in unified mode
        line.cut <- max(
          0L, line.cut - if(any(hunk.atom$tar.rng))
            diff(hunk.atom$tar.rng) + 1L else 0L
        )
      }
      hunk.atom <- trim_hunk(hunk.atom, "cur", line.cut)
      hunk.grps[[grp.cut]][[hunk.cut]] <- hunk.atom
      null.hunks <- seq_len(length(hunk.grps[[grp.cut]]) - hunk.cut) + hunk.cut
      hunk.grps[[grp.cut]][null.hunks] <- lapply(

        hunk.grps[[grp.cut]][null.hunks],
        function(h.a) {
          h.a <- trim_hunk(h.a, "cur", 0L)
          h.a <- trim_hunk(h.a, "tar", 0L)
          h.a
    } ) }
  } else if (!cut.off && length(cum.len)) {
    lines.omitted <- lines.total
    hunk.grps.omitted <- hunk.grps.count

    for(i in seq_along(hunk.grps))
      hunk.grps[[i]] <- empty_hunk_grp(hunk.grps[[i]])
  }
  diffs.trim <- count_diffs(hunk.grps)
  attr(hunk.grps, "meta") <- list(
    lines=as.integer(c(lines.omitted, lines.total)),
    hunks=as.integer(c(hunk.grps.omitted, hunk.grps.count)),
    diffs=as.integer(c(diffs.orig - diffs.trim, diffs.orig))
  )
  hunk.grps
}
# Helper fun

line_count <- function(rng) if(rng[[1L]]) rng[[2L]] - rng[[1L]] + 1L else 0L

# Count how many "lines" of differences there are in the  hunks
#
# Counts original diff lines, not lines left after trim.  This is because
# we are checking for 'str' folding, and 'str' folding should only happen
# if the folded results fits fully within limit.
#
# param x should be a hunk group list

count_diffs <- function(x) {
  sum(
    vapply(
      unlist(x, recursive=FALSE),
      function(y)
        if(y$context) 0L else line_count(y$tar.rng) + line_count(y$cur.rng),
      integer(1L)
) ) }
# More detailed counting of differences; note that context counting is messed
# up b/c context's are duplicated around each hunk.  This is primarily used for
# the summary method

count_diffs_detail <- function(x) {
  x.flat <- unlist(x, recursive=FALSE)
  guides <- vapply(x.flat, "[[", logical(1L), "guide")
  vapply(
    x.flat[!guides],
    function(y)
      if(y$context) c(match=line_count(y$tar.rng), delete=0L, add=0L)
      else c(match=0L, delete=line_count(y$tar.rng), add=line_count(y$cur.rng)),
    integer(3L)
) }

count_diff_hunks <- function(x)
  sum(!vapply(unlist(x, recursive=FALSE), "[[", logical(1L), "context"))

