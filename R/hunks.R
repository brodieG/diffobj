
# Convert ses data into raw hunks that include both match hunks as well as
# actual hunks
#
# @return a list with the hunks, as well as several descriptive vectors the same
#   length as the hunk list.  Hunks are just lists with the target and current
#   match status encoded as 0, a number, or NA (match, mismach aligned with
#   a mismatch in the other vector, or delete/add respectively).  This should
#   probably be done with S4 objects, but given potential large number of these
#   we are using plain lists instead.  In addition to the match encoding, the
#   position to insert the vector after is provided.

setGeneric("as.hunks", function(x, ...) standardGeneric("as.hunks"))
setMethod("as.hunks", "diffObjMyersMbaSes",
  function(x, context, ...) {
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

        del.len <- sum(d$len[which(d$type == "Delete")])
        ins.len <- sum(d$len[which(d$type == "Insert")])
        mtc.len <- sum(d$len[which(d$type == "Match")])

        type <- NA_integer_

        if(del.len && ins.len) { # must have delete/insert
          if(!all(d$type %in% c("Delete", "Insert")))
            stop("Logic Error: unexpected edit types; contact maintainer.")
          # Idea here is to number all the insert/deletes so that we can then
          # line them up later; we use `j` to ensure we produce unique indices

          min.len <- min(ins.len, del.len)
          match.seq <- seq_len(min.len) + j
          type <- "Change"
          j <<- j + min.len

          tar <- c(match.seq, rep(NA_integer_, del.len - min.len))
          cur <- c(match.seq, rep(NA_integer_, ins.len - min.len))
        } else {
          # can only have one type

          if(sum(as.logical(c(del.len, ins.len, mtc.len))) != 1L)
            stop("Logic Error: unexpected edit types 2; contact maintainer.")

          type <- as.character(d$type[[1L]])
          if(mtc.len) {
            tar <- cur <- rep(0L, d$len)
          } else if (ins.len) {
            tar <- integer()
            cur <- rep(NA_integer_, d$len)
          } else if (d$type == "Delete") {
            cur <- integer()
            tar <- rep(NA_integer_, d$len)
          }
        }
        list(
          target=tar, current=cur,
          tar.pos=d$last.a[[1L]] - del.len - mtc.len,
          cur.pos=d$last.b[[1L]] - ins.len - mtc.len,
          type=type
    ) } )
    # Restructure data in result list

    types <- factor(
      vapply(res.l, "[[", character(1L), "type"),
      levels=c("Match", "Delete", "Insert", "Change")
    )
    if(any(is.na(types)))
      stop("Logic Error: invalid hunks extracted; contact maintainer.")
    if(length(types) > 1L && !all(abs(diff(types == "Match")) == 1L))
      stop(
        "Logic Error: match and mismatch chunks not interspersed; contact ",
        "maintainer."
      )
    hunks <- lapply(res.l, function(z) z[names(z) != "type"])
    process_hunks(list(hunks=hunks, types=types), context)
} )
# Combine two hunks together

merge_hunks <- function(a, b) {
  nm <- c("target", "current")
  res <- setNames(lapply(nm, function(x) c(a[[x]], b[[x]])), nm)
  # always inherit left hunk position
  res$tar.pos <- a$tar.pos
  res$cur.pos <- a$cur.pos
  res
}
# Subset hunks

hunk_sub <- function(hunk, op, n) {
  stopifnot(op %in% c("head", "tail"))
  nm <- c("target", "current")
  hunk.dat <- setNames(lapply(hunk[nm], op, n), nm)

  # Need to reset the start position if we partially subset when merging left
  # 'tail' must mean a left merge

  if(op == "tail") {
    hunk.lens <- vapply(hunk[nm], length, integer(1L))
    hunk.lens.trim <- vapply(hunk.dat, length, integer(1L))

    pos.delt <- hunk.lens - hunk.lens.trim
    hunk.dat$tar.pos <- hunk$tar.pos + pos.delt[[1L]]
    hunk.dat$cur.pos <- hunk$cur.pos + pos.delt[[2L]]
  } else {
    hunk.dat$tar.pos <- hunk$tar.pos
    hunk.dat$cur.pos <- hunk$cur.pos
  }
  hunk.dat
}
# Figure Out Context for Each Chunk
#
# If a hunk bleeds into another due to context then it becomes part of the
# other hunk.  Note that the input `x` is not hunks yet since it has the
# additional meta information in x$types that we eventually strip off

process_hunks <- function(x, context) {
  stopifnot(
    is.integer(context), length(context) == 1L, !is.na(context),
    # assuming hunk list is more or less in correct format, checks not
    # comprehensive here
    is.list(x), is.list(x$hunks),
    is.factor(x$types), length(x$types) == length(x$hunks)
  )
  hunk.len <- length(x$hunks)

  # Special cases, including only one hunk or forcing only one hunk

  if(context < 0L) {
    return(list(Reduce(merge_hunks, x$hunks)))
  }
  if(hunk.len < 2L) return(x)

  # Normal cases

  res.l <- vector("list", hunk.len)

  # Jump through every second value as those are the mismatch hunks, though
  # first figure out if first hunk is mismatching

  i <- if(x$types[[1L]] == "Match") 2L else 1L
  j <- 1L
  while(i <= hunk.len) {
    # Merge left

    res.l[[j]] <- if(i - 1L) merge_hunks(
      hunk_sub(x$hunks[[i - 1L]], "tail", context), x$hunks[[i]]
    ) else x$hunks[[i]]

    # Merge right

    if(i < hunk.len) {
      # Hunks bleed into next hunk due to context

      while(i < hunk.len && length(x$hunks[[i + 1L]]$target) <= context * 2) {
        res.l[[j]] <- merge_hunks(res.l[[j]], x$hunks[[i + 1L]])
        if(i < hunk.len - 1L)
          res.l[[j]] <- merge_hunks(res.l[[j]], x$hunks[[i + 2L]])
        i <- i + 2L
      }
      # Context enough to cause a break

      if(i < hunk.len) {
        res.l[[j]] <- merge_hunks(
          res.l[[j]], hunk_sub(x$hunks[[i - 1L]], "head", context)
    ) } }
    j <- j + 1L
    i <- i + 2L
  }
  length(res.l) <- j - 1L
  res.l
}
# Reduce hunks so the total number of text;

setGeneric("trimHunks", function(x, ...) standardGeneric("trimHunks"))
setMethod("trimHunks", "diffObjDiff",
  function(x, limit, width, mode, ...) {
    # Different modes have different per hunk # of lines calculations
    mode %in% c("context", "unified", "sidebyside")

    if(width < 20 || (mode == "sidebyside" && width < 40)) {
      width <- if(mode == "sidebyside") 40L else 20L
      warning("Setting width minimum to ", width, " for diff display")
    }
    # Subtract required margin from width

    if(mode == "sidebyside") {
      width <- floor(width / 2) - 3
    } else {
      width <- width - 2
    }
    # Given width, compute number of screen lines taken by each hunk; we need
    # to go through each hunk, find the strings corresponding to each element,
    # match up target/current, and then figure out number of screen lines.

    count_lines <- function(vec, matches, pos) {
      vec.sub <- vec[seq_along(matches) + pos]
      lines <- ceiling(nchar(vec.sub) / width)
      types <- ifelse(is.na(matches), 2, ifelse(matches, 1, 0))
      list(lines=lines, type=types)
    }
    res <- lapply(
      x@hunks, function(y) Map(
        count_lines, list(x@tar.capt, x@cur.capt),
        y[c("target", "current")], y[c("tar.pos", "cur.pos")]
    ) )
    NULL
} )
