
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
          mode, context=tar, unified=c(tar, cur), sidebyside=tar,
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
          chr[ids < 0L] <- x@b[ids[ids < 0]]
          chr[ids == 0L] <- ""
          chr
        }
        A.chr <- get_chr(A)
        B.chr <- get_chr(B)

        # compute ranges

        tar.rng <- if(tar.len) c(A.start + 1L, A.start + tar.len) else integer()
        cur.rng <- if(cur.len) c(B.start + 1L, B.start + cur.len) else integer()

        list(
          A=A, B=B, A.chr=A.chr, B.chr=B.chr, context=context, tar.rng=tar.rng,
          cur.rng=cur.rng
        )
    } )
    # group hunks together based on context

    process_hunks(res.l, context)
} )
# Subset hunks; should only ever be subsetting context hunks

hunk_sub <- function(hunk, op, n) {
  stopifnot(
    op %in% c("head", "tail"), hunk$context, !!length(hunk$tar.rng),
    length(hunk$tar.rng) == length(hunk$cur.rng),
    diff(hunk$tar.rng) == diff(hunk$tar.rng),
    length(hunk$tar.rng) == 2L
  )
  hunk.len <- diff(hunk$tar.rng) + 1L
  len.diff <- hunk.len - n
  if(len.diff >= 0) {
    nm <- c("A", "B", "A.chr", "B.chr")
    hunk[nm] <- lapply(hunk[nm], op, n)

    # Need to recompute ranges

    if(op == "tail") {
      hunk$tar.rng[[1L]] <- hunk$tar.rng[[1L]] + len.diff
      hunk$cur.rng[[1L]] <- hunk$cur.rng[[1L]] + len.diff
    } else {
      hunk$tar.rng[[2L]] <- hunk$tar.rng[[2L]] - len.diff
      hunk$cur.rng[[2L]] <- hunk$cur.rng[[2L]] - len.diff
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
      # Hunks bleed into next hunk due to context

      while(i < hunk.len && length(x[[i + 1L]]$target) <= context * 2) {
        res.l[[j]] <- append(res.l[[j]], x[i + 1L])
        if(i < hunk.len - 1L)
          res.l[[j]] <- append(res.l[[j]], x[i + 2L])
        i <- i + 2L
      }
      # Context enough to cause a break

      if(i < hunk.len) {
        res.l[[j]] <- append(
          res.l[[j]], list(hunk_sub(x[[i - 1L]], "head", context))
    ) } }
    j <- j + 1L
    i <- i + 2L
  }
  length(res.l) <- j - 1L
  res.l
}
