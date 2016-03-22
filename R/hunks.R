
# Convert ses data into raw hunks that include both match hunks as well as
# actual hunks
#
# @return a list containing lists of atomic hunks.  Each of these sub-lists
#   of atomic hunks is treated as a "hunk", but is really a combination of
#   context and hunks which we will refer to as "hunk group".  In each hunk
#   group, There may be as little as one hunk with no context, or many hunks and
#   context if the context between hunks is not sufficient to meet the requested
#   context, in which case the hunks bleed together forming these hunk groups.
#
#   The hunks within the groups contain integer vectors A and B, as well as
#   an indication of whether this is a context hunk or a diff hunk.  Vectors
#   A and B are computed based on what type of diff mode we're in.  Positive
#   values reference strings from the original target string, and negative ones
#   from the current string.
#
#   In addition to the indices referencing the original character vectors, the
#   actual character values are also retained, though these may be modified
#   over the course of processing by being wrapped, having colors added, etc.
#
#   Important: context atomic hunks are duplicated anytime there is enough
#   context that we only show part of the context hunk.
#
#   Notes for rendering:
#   1. determine if chunk is context or not
#   2. if not, positive values are "target" strings, negative "current strings"
#   3. the range of the hunks is also stored as tar.rng and cur.rng; mostly
#      inferrable from the actual data in the hunks, except that in unified
#      mode we no longer have the actual context strings from the `current`
#      vector.

setGeneric("as.hunks", function(x, ...) standardGeneric("as.hunks"))
setMethod("as.hunks", "diffObjMyersMbaSes",
  function(
    x, mode, context, disp.width, line.limit, hunk.limit, tab.stops,
    use.header, ...
  ) {
    stopifnot(
      is.character(mode), length(mode) == 1L, !is.na(mode),
      mode %in% c("context", "unified", "sidebyside")
    )
    # Split our data into sections that have either deletes/inserts or matches

    dat <- as.matrix(x)
    sects <- unique(dat[, "section"])
    j <- 0L

    # For each section, figure out how to represent target and current where
    # 0 means match, 1:n is a matched mismatch (change in edit script parlance),
    # and NA is a full mismatch (d or i).  Note we start the hunk ids at 2 so
    # we can add a header hunk later

    res.l <- if(!nrow(dat)) {
      # Minimum one empty hunk if nothing; arbitrarily chose to make it a
      # non context hunk; ideally would figure out a way to integrate this
      # code in the lapply...

      list(
        list(
          id=2L, A=integer(0L), B=integer(0L), A.chr=character(0L),
          B.chr=character(0L), A.eq.chr=character(0L), B.eq.chr=character(0L),
          context=FALSE, header=FALSE, tar.rng=integer(2L), cur.rng=integer(2L),
          tar.rng.sub=integer(2L), cur.rng.sub=integer(2L),
          tar.rng.trim=integer(2L), cur.rng.trim=integer(2L)
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
            stop("Logic Error: unexpected edit types; contact maintainer.")

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
            mode, context=tar, unified=c(tar, if(!context) cur), sidebyside=tar,
            stop("Logic Error: unknown mode; contact maintainer.")
          )
          B <- switch(
            mode, context=cur, unified=integer(), sidebyside=cur,
            stop("Logic Error: unknown mode; contact maintainer.")
          )
          # Retrieve the character values

          get_chr <- function(ids) {
            chr <- character(length(ids))
            chr[ids > 0L] <- x@a[ids[ids > 0]]
            chr[ids < 0L] <- x@b[abs(ids[ids < 0])]
            chr[ids == 0L] <- NA_character_
            chr
          }
          A.chr <- get_chr(A)
          B.chr <- get_chr(B)

          # compute ranges

          tar.rng <- cur.rng <- integer(2L)
          if(tar.len) tar.rng <- c(A.start + 1L, A.start + tar.len)
          if(cur.len) cur.rng <- c(B.start + 1L, B.start + cur.len)

          list(
            id=i + 1L, A=A, B=B, A.chr=A.chr, B.chr=B.chr,
            A.eq.chr=A.chr, B.eq.chr=B.chr,
            context=context, header=FALSE,
            tar.rng=tar.rng, cur.rng=cur.rng,
            tar.rng.sub=tar.rng, cur.rng.sub=cur.rng,
            tar.rng.trim=tar.rng, cur.rng.trim=cur.rng
          )
    } ) }
    # Group hunks together based on context, in "auto" mode we find the context
    # that maximizes lines displayed while adhering to line and hunk limits
    # Definitely not very efficient since we re-run code multiple times we
    # probably don't need to.

    if(is(context, "diffObjAutoContext")) {
      len <- diff_line_len(
        p_and_t_hunks(
          res.l, context=context@max, hunk.limit=hunk.limit,
          use.header=use.header
        ),
        mode, disp.width
      )
      len.min <- diff_line_len(
        p_and_t_hunks(
          res.l, context=context@min, hunk.limit=hunk.limit,
          use.header=use.header
        ),
        mode, disp.width
      )
      context <- if(len <= line.limit[[1L]] || line.limit[[1L]] < 0L) {
        -1L
      } else if(len.min > line.limit[[1L]]) {
        context@min
      } else {
        # compute max context size

        ctx.max <- ctx.hi <- ctx <- as.integer(
          ceiling(
            max(
              vapply(
                res.l,
                function(x) if(x$context) length(x$A.chr) else 0L, integer(1L)
            ) ) / 2L
        ) )
        ctx.lo <- context@min
        safety <- 0L

        repeat {
          if((safety <- safety + 1L) > ctx.max)
            stop(
              "Logic Error: stuck trying to find auto-context; contact ",
              "maintainer."
            )
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
            p_and_t_hunks(
              res.l, context=ctx, hunk.limit=hunk.limit, use.header=use.header
            ),
            mode, disp.width
          )
        }
        ctx
    } }
    process_hunks(res.l, context=context, use.header=use.header)
} )
# process the hunks and also drop off groups that exceed limit
#
# used exclusively when we are trying to auto-calculate context

p_and_t_hunks <- function(hunks.raw, context, hunk.limit, use.header) {
  c.all <- process_hunks(hunks.raw, context=context, use.header=use.header)
  if(hunk.limit[[1L]] >= 0L && length(c.all) > hunk.limit)
    c.all <- c.all[seq_along(hunk.limit[[2L]])]
  c.all
}

# Subset hunks; should only ever be subsetting context hunks

hunk_sub <- function(hunk, op, n) {
  stopifnot(
    op %in% c("head", "tail"), hunk$context, all(hunk$tar.rng),
    length(hunk$tar.rng) == length(hunk$cur.rng),
    diff(hunk$tar.rng) == diff(hunk$cur.rng),
    length(hunk$tar.rng) == 2L
  )
  hunk.len <- diff(hunk$tar.rng) + 1L
  len.diff <- hunk.len - n
  if(len.diff >= 0) {
    nm <- c("A", "B", "A.chr", "B.chr", "A.eq.chr", "B.eq.chr")
    hunk[nm] <- lapply(hunk[nm], op, n)

    # Need to recompute ranges

    if(n) {
      if(op == "tail") {
        hunk$tar.rng.trim[[1L]] <- hunk$tar.rng.sub[[1L]] <-
          hunk$tar.rng[[1L]] + len.diff
        hunk$cur.rng.trim[[1L]] <- hunk$cur.rng.sub[[1L]] <-
          hunk$cur.rng[[1L]] + len.diff
      } else {
        hunk$tar.rng.trim[[2L]] <- hunk$tar.rng.sub[[2L]] <-
          hunk$tar.rng[[2L]] - len.diff
        hunk$cur.rng.trim[[2L]] <- hunk$cur.rng.sub[[2L]] <-
          hunk$cur.rng[[2L]] - len.diff
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
# This will group atomic hunks into hunk groups

process_hunks <- function(x, context, use.header) {
  ctx.vec <- vapply(x, "[[", logical(1L), "context")
  if(!all(abs(diff(ctx.vec)) == 1L))
    stop(
      "Logic Error: atomic hunks not interspersing context; contact maintainer."
    )

  hunk.len <- length(x)

  # Special cases, including only one hunk or forcing only one hunk group, or
  # no differences

  if(context < 0L || hunk.len < 2L) return(list(x))
  if(!any(ctx.vec)) return(list()) # is this right?

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

      while(i < hunk.len && length(x[[i + 1L]]$A) <= context * 2) {
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
  # Add back the header hunk if needed.

  missing.first <- res.l[[1L]][[1L]]$tar.rng.trim[[1L]] != 1L &&
    res.l[[1L]][[1L]]$cur.rng.trim[[1L]] != 1L
  header <- if(use.header && missing.first) {
    header <- hunk_sub(x[[1L]], "head", 1L)
    header$header <- TRUE
    list(list(header))
  }
  # Finalize, including sizing correctly, and setting the ids to the right
  # values since we potentially duplicated some context hunks

  length(res.l) <- j - 1L
  res.fin <- c(header, res.l)
  k <- 1L
  for(i in seq_along(res.fin)) {
    for(j in seq_along(res.fin[[i]])) {
      res.fin[[i]][[j]][["id"]] <- k
      k <- k + 1L
    }
  }
  res.fin
}
# Compute how many lines the display version of the diff will take, meta
# lines (used for hunk headers) are denoted by negatives
#
# count lines for each remaining hunk and figure out if we need to cut some
# hunks off; note that "negative" lengths indicate the lines being counted
# originated from the B hunk in context mode

get_hunk_chr_lens <- function(hunk.grps, mode, disp.width) {
  # Account for overhead / side by sideness in width calculations
  # Internal funs
  hunk_len <- function(hunk.id, hunks) {
    hunk <- hunks[[hunk.id]]
    A.lines <- nlines(hunk$A.chr, disp.width, mode)
    B.lines <- nlines(hunk$B.chr, disp.width, mode)

    # Depending on each mode, figure out how to set up the lines;
    # straightforward except for context where we need to account for the
    # fact that all the A of a hunk group are shown first, and then all
    # the B are shown

    lines.out <- switch(
      mode,
      context=c(A.lines, -B.lines),
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
  hunk_grp_len <- function(hunk.grp.id) {
    hunks <- hunk.grps[[hunk.grp.id]]
    hunks.proc <- lapply(seq_along(hunks), hunk_len, hunks=hunks)
    res.tmp <- do.call(rbind, hunks.proc)
    res <- cbind(grp.id=if(nrow(res.tmp)) hunk.grp.id else integer(0L), res.tmp)
    # Need to make sure all positives are first, and all negatives second, if
    # there are negatives (context mode); we also add 1 to the first line in
    # each section to account for the group hunkheader info

    if(identical(mode, "context")) res <- res[order(res[, "len"] < 0L),]
    if(
      identical(mode, "context") &&
      length(negs <- which(res[, "len"] < 0L)) &&
      length(poss <- which(res[, "len"] > 0L))
    ) {
      if(length(poss)) res[1L, "len"] <- res[1L, "len"] + 1L
      res[negs[[1L]], "len"] <- res[negs[[1L]], "len"] - 1L
    } else if(nrow(res)) {
      res[1L, "len"] <- res[1L, "len"] + 1L
    }
    res
  }
  # Generate a matrix with hunk group id, hunk id, and wrapped length of each
  # line that we can use to figure out what to show

  do.call(rbind, lapply(seq_along(hunk.grps), hunk_grp_len))
}
# Compute total diff length in lines

diff_line_len <- function(hunk.grps, mode, disp.width) {
  max(
    0L,
    cumsum(get_hunk_chr_lens(hunk.grps, mode, disp.width)[, "len"])
  ) + banner_len(mode)
}
# Remove hunk groups and atomic hunks that exceed the line limit
#
# Return value is a hunk group list, with an attribute indicating how many
# hunks and lines were  trimmed

trim_hunk <- function(hunk, type, line.id) {
  stopifnot(type %in% c("tar", "cur"))
  rng.idx <- sprintf("%s.rng.trim", type)
  dat.idx <- sprintf(c("%s", "%s.chr"), if(type == "tar") "A" else "B")
  hunk[[rng.idx]] <- if(!line.id) integer(2L) else {
    if(all(hunk[[rng.idx]])) {
      c(
        hunk[[rng.idx]][[1L]],
        min(hunk[[rng.idx]][[1L]] + line.id - 1L, hunk[[rng.idx]][[2L]])
      )
    } else integer(2L)
  }
  hunk[dat.idx] <- lapply(hunk[dat.idx], head, n=line.id)
  hunk
}
trim_hunks <- function(hunk.grps, mode, disp.width, hunk.limit, line.limit) {
  diffs.orig <- count_diffs(hunk.grps)
  hunk.grps.count <- length(hunk.grps)
  if(hunk.limit[[1L]] < 0L) hunk.limit <- rep(hunk.grps.count, 2L)
  hunk.limit.act <- if(hunk.grps.count > hunk.limit[[1L]]) hunk.limit[[2L]]

  hunk.grps.omitted <- max(0L, hunk.grps.count - hunk.limit.act)
  hunk.grps.used <- min(hunk.grps.count, hunk.limit.act)
  hunk.grps <- hunk.grps[seq_len(hunk.grps.used)]

  lines <- get_hunk_chr_lens(hunk.grps, mode, disp.width)
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
    for(i in grps.to.cut)
      for(j in seq_along(hunk.grps[[i]]))
        hunk.grps[[i]][[j]][c("tar.rng.trim", "cur.rng.trim")] <-
          list(integer(2L), integer(2L))

    hunk.grps.used <- grp.cut
    hunk.grps.omitted <- max(0L, hunk.grps.count - grp.cut)

    # Remove excess lines from the atomic hunks based on the limits; we don't
    # update the ranges as those should still indicate what the original
    # untrimmed range was

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
  } else if (!cut.off) {
    lines.omitted <- lines.total
    hunk.grps.omitted <- hunk.grps.count
    hunk.grps <- list()
  }
  diffs.trim <- count_diffs(hunk.grps)
  attr(hunk.grps, "meta") <- list(
    lines=c(lines.omitted, lines.total),
    hunks=c(hunk.grps.omitted, hunk.grps.count),
    diffs=c(diffs.orig - diffs.trim, diffs.orig)
  )
  hunk.grps
}
# Modify Character Values of Hunks
#
# Used to highlight words in wrap diffs after the main diff has been done.  This
# is quite a hack job and should probably be handled more elegantly, but at this
# point it is the simplest way to get old functionality back up and running
# under the hunk view of the world.

update_hunks <- function(hunk.grps, A.chr, B.chr) {
  lapply(
    hunk.grps,
    function(h.g)
      lapply(
        h.g,
        function(h.a) {
          a.neg <- h.a$A < 0
          b.neg <- h.a$B < 0
          h.a$A.chr[a.neg] <- B.chr[abs(h.a$A[a.neg])]
          h.a$A.chr[!a.neg] <- A.chr[h.a$A[!a.neg]]
          h.a$B.chr[b.neg] <- B.chr[abs(h.a$B[b.neg])]
          h.a$B.chr[!b.neg] <- A.chr[h.a$B[!b.neg]]
          h.a
        }
  ) )
}

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
      function(y) {
        if(y$context) 0L else {
          (if(y$tar.rng[[1L]]) y$tar.rng[[2L]] - y$tar.rng[[1L]] + 1L else 0L) +
          (if(y$cur.rng[[1L]]) y$cur.rng[[2L]] - y$cur.rng[[1L]] + 1L else 0L)
      } },
      integer(1L)
) ) }
