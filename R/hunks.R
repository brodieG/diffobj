
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
#   Important: context atomic hunks are duplicated anytime there is enough
#   context that we only show part of the context hunk.
#
#   Notes for rendering:
#   1. determine if chunk is context or not
#   2. if not, positive values are "target" strings, negative "current strings"
#   3. the range of the hunks is also stored as tar.rng and cur.rng; mostly
#      inferrable from the actual data in the hunks, except that in unified
#      mode we no longer have the actual context strings from the current
#      vector.

setGeneric("as.hunks", function(x, ...) standardGeneric("as.hunks"))
setMethod("as.hunks", "diffObjMyersMbaSes",
  function(x, mode, context, ...) {
    stopifnot(
      is.character(mode), length(mode) == 1L, !is.na(mode),
      mode %in% c("context", "unified", "sidebyside")
    )
    # Split our data into sections that have either deletes/inserts or matches

    dat <- as.data.frame(x)
    d.s <- split(dat, dat$section)
    j <- 0L

    # For each section, figure out how to represent target and current where
    # 0 means match, 1:n is a matched mismatch (change in edit script parlance),
    # and NA is a full mismatch (d or i).

    res.l <- lapply(
      seq_along(d.s),
      function(i) {
        d <- d.s[[i]]
        d.del <- d[which(d$type == "Delete"), ]
        d.ins <- d[which(d$type == "Insert"), ]
        d.mtc <- d[which(d$type == "Match"), ]
        del.len <- sum(d.del$len)
        ins.len <- sum(d.ins$len)
        mtc.len <- sum(d.mtc$len)
        tar.len <- del.len + mtc.len
        cur.len <- ins.len + mtc.len

        # atomic hunks may only be del/ins or match, not both

        if((del.len || ins.len) && mtc.len || !(del.len + ins.len + mtc.len))
          stop("Logic Error: unexpected edit types; contact maintainer.")

        # Figure out where previous hunk left off

        del.last <- if(nrow(d.del)) d.del$last.a[[1L]] else d$last.a[[1L]]
        ins.last <- if(nrow(d.ins)) d.ins$last.b[[1L]] else d$last.b[[1L]]
        A.start <- del.last - del.len - mtc.len
        B.start <- ins.last - ins.len - mtc.len

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
          chr[ids == 0L] <- ""
          chr
        }
        A.chr <- get_chr(A)
        B.chr <- get_chr(B)

        # compute ranges

        tar.rng <- cur.rng <- integer(2L)
        if(tar.len) tar.rng <- c(A.start + 1L, A.start + tar.len)
        if(cur.len) cur.rng <- c(B.start + 1L, B.start + cur.len)

        list(
          id=i, A=A, B=B, A.chr=A.chr, B.chr=B.chr, context=context,
          tar.rng=tar.rng, cur.rng=cur.rng, tar.rng.trim=tar.rng,
          cur.rng.trim=cur.rng
        )
    } )
    # group hunks together based on context

    process_hunks(res.l, context)
} )
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
    nm <- c("A", "B", "A.chr", "B.chr")
    hunk[nm] <- lapply(hunk[nm], op, n)

    # Need to recompute ranges

    if(!n) {
      # hunk$tar.rng <- hunk$cur.rng <-
      hunk$tar.rng.trim <- hunk$cur.rng.trim <- integer(2L)
    } else if(op == "tail") {
      # hunk$tar.rng[[1L]] <-
      hunk$tar.rng.trim[[1L]] <-
        hunk$tar.rng.trim[[1L]] + len.diff
      # hunk$cur.rng[[1L]] <-
      hunk$cur.rng.trim[[1L]] <-
        hunk$cur.rng.trim[[1L]] + len.diff
    } else {
      # hunk$tar.rng[[2L]] <-
      hunk$tar.rng.trim[[2L]] <-
        hunk$tar.rng.trim[[2L]] - len.diff
      # hunk$cur.rng[[2L]] <-
      hunk$cur.rng.trim[[2L]] <-
        hunk$cur.rng.trim[[2L]] - len.diff
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

process_hunks <- function(x, context) {
  stopifnot(
    is.integer(context), length(context) == 1L, !is.na(context),
    # assuming hunk list is more or less in correct format, checks not
    # comprehensive here
    is.list(x)
  )
  ctx.vec <- vapply(x, "[[", logical(1L), "context")
  if(!all(abs(diff(ctx.vec)) == 1L))
    stop(
      "Logic Error: atomic hunks not interspersing context; contact maintainer."
    )

  hunk.len <- length(x)

  # Special cases, including only one hunk or forcing only one hunk group, or
  # no differences

  if(context < 0L || hunk.len < 2L) return(list(x))
  if(!any(ctx.vec)) return(list())

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
  # Finalize, including sizing correctly, and setting the ids to the right
  # values since we potentially duplicated some context hunks

  length(res.l) <- j - 1L
  k <- 1L
  for(i in seq_along(res.l)) {
    for(j in seq_along(res.l[[i]])) {
      res.l[[i]][[j]][["id"]] <- k
      k <- k + 1L
    }
  }
  res.l
}
# Compute how many lines the display version of the diff will take, meta
# lines (used for hunk headers) are denoted by negatives
#
# count lines for each remaining hunk and figure out if we need to cut some
# hunks off; note that "negative" lengths indicate the lines being counted
# originated from the B hunk in context mode
#
# NOTE: need to account for multi-space characters and escape sequences

get_hunk_chr_lens <- function(hunk.grps, mode, width, use.ansi) {
  # Account for overhead / side by sideness in width calculations
  if(mode == "sidebyside")
    width <- max(floor(width - 3L / 2L), 20L) else width <- width - 2L
  # Internal funs
  hunk_len <- function(hunk.id, hunks) {
    hunk <- hunks[[hunk.id]]
    A.lines <- as.integer(
      ceiling(ansi_style_nchar(hunk$A.chr, use.ansi) / width)
    )
    B.lines <- as.integer(
      ceiling(ansi_style_nchar(hunk$B.chr, use.ansi) / width)
    )
    # Depending on each mode, figure out how to set up the lines;
    # straightforward except for context where we need to account for the
    # fact that all the A of a hunk group are shown first, and then all
    # the B are shown

    lines.out <- switch(
      mode,
      context=c(A.lines, -B.lines),
      unified=c(A.lines),
      sidebyside=c(pmax(A.lines, B.lines)),
      stop("Logic Error: unknown mode '", mode, "' contact maintainer")
    )
    # Make sure that line.id refers to the position of the line in either
    # original A or B vector

    line.id <- unlist(lapply(split(lines.out, lines.out > 0L), seq_along))
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

    if(
      identical(mode, "context") &&
      length(negs <- which(res[, "len"] < 0L)) &&
      length(poss <- which(res[, "len"] > 0L))
    ) {
      res <- res[order(res[, "len"] < 0L),]
      if(length(poss)) res[1L, "len"] <- res[1L, "len"] + 1L
      res[negs[[1L]], "len"] <- res[negs[[1L]], "len"] - 1L
    } else {
      res[1L, "len"] <- res[1L, "len"] + 1L
    }
    res
  }
  # Generate a matrix with hunk group id, hunk id, and wrapped length of each
  # line that we can use to figure out what to show

  do.call(rbind, lapply(seq_along(hunk.grps), hunk_grp_len))
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
trim_hunks <- function(
  hunk.grps, mode, width, hunk.limit, line.limit, use.ansi
) {
  hunk.grps.count <- length(hunk.grps)
  if(hunk.limit < 0L) hunk.limit <- hunk.grps.count
  hunk.grps.omitted <- max(0L, hunk.grps.count - hunk.limit)
  hunk.grps.used <- min(hunk.grps.count, hunk.limit)
  hunk.grps <- hunk.grps[seq_len(hunk.grps.used)]

  lines <- get_hunk_chr_lens(hunk.grps, mode, width, use.ansi)
  cum.len <- cumsum(abs(lines[, "len"]))
  cut.off <- -1L
  lines.omitted <- 0L
  if(line.limit[[1L]] < 0L) {
    cut.off <- max(0L, cum.len)
  } else if(any(cum.len > line.limit[[1L]])) {
    cut.off <- max(0L, cum.len[cum.len < line.limit[[2L]]])
  }
  if(cut.off > 0) {
    lines.omitted <- tail(cum.len, 1L) - cut.off
    cut.dat <- lines[max(which(cum.len <= cut.off)), ]
    grp.cut <- cut.dat[["grp.id"]]
    hunk.cut <- cut.dat[["hunk.id"]]
    line.cut <- cut.dat[["line.id"]]
    line.neg <- cut.dat[["len"]] < 0

    hunk.grps <- hunk.grps[seq_len(grp.cut)]
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
          hunk.atom <- trim_hunk(hunk.atom, "cur", line.cut)
        }
        hunks.grp[[grp.cut]][[i]] <- hunk.atom
      }
    } else {
      hunk.atom <- hunk.grps[[grp.cut]][[hunk.cut]]
      hunk.atom <- trim_hunk(hunk.atom, "tar", line.cut)
      hunk.atom <- trim_hunk(hunk.atom, "cur", line.cut)
      hunk.grps[[grp.cut]][[hunk.cut]] <- hunk.atom
    }
  } else if (!cut.off) {
    hunk.grps <- list()
  }
  attr(hunk.grps, "omitted") <- c(lines=lines.omitted, hunks=hunk.grps.omitted)
  hunk.grps
}
