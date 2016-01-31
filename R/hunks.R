
# Convert ses data into raw hunks that include both match hunks as well as
# actual hunks
#
# @return a list with the hunks, as well as several descriptive vectors the same
#   length as the hunk list.  Hunks are just lists with the target and current
#   match status encoded as 0, a number, or NA (match, mismach aligned with
#   a mismatch in the other vector, or delete/add respectively).  This should
#   probably be done with S4 objects, but given potential large number of these
#   we are using plain lists instead.

setGeneric("as.hunks", function(x, ...) standardGeneric("as.hunks"))
setMethod("as.hunks", "diffObjMyersMbaSes",
  function(x, ...) {
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
        un.type <- length(unique(d$type))
        type <- NA_integer_
        if(un.type == 2L) { # must have delete/insert
          if(!all(d$type %in% c("Delete", "Insert")))
            stop("Logic Error: unexpected edit types; contact maintainer.")
          # Idea here is to number all the insert/deletes so that we can then
          # line them up later; we use `j` to ensure we produce unique indices

          ins.len <- d$len[[which(d$type == "Insert")]]
          del.len <- d$len[[which(d$type == "Delete")]]
          min.len <- min(ins.len, del.len)
          match.seq <- seq_len(min.len) + j
          type <- "Change"
          j <<- j + min.len

          tar <- c(match.seq, rep(NA_integer_, del.len - min.len))
          cur <- c(match.seq, rep(NA_integer_, ins.len - min.len))
        } else if (un.type == 1L) {
          type <- as.character(d$type[[1L]])
          if(d$type == "Match") {
            tar <- cur <- rep(0L, d$len)
          } else if (d$type == "Insert") {
            tar <- integer()
            cur <- rep(NA_integer_, d$len)
          } else if (d$type == "Delete") {
            cur <- integer()
            tar <- rep(NA_integer_, d$len)
          } else
            stop("Logic Error: unexpected edit types 2; contact maintainer.")
        }
        list(
          target=tar, current=cur, tar.pos=d$last.a[[1L]],
          cur.pos=d$last.b[[1L]], tar.len=del.len, cur.len=ins.len, type=type
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
    int.scalars <- c("tar.pos", "cur.pos", "tar.len", "cur.len")
    sclr.l <- Map(function(z) vapply(res.l, "[[", integer(1L), z), int.scalars)

    hunks <- lapply(res.l, "[", c("target", "current"))

    c(list(hunks=hunks, types=types), sclr.l)
} )
# subset hunks; would be better as S4, but again trying to be at least
# partially efficient; also not usoing S3 b/c we don't want to export these
# methods


# Combine two hunks together

merge_hunks <- function(a, b) {
  nm <- names(a)
  setNames(lapply(nm, function(x) c(a[[x]], b[[x]])), nm)
}
# Figure Out Context for Each Chunk
#
# If a hunk bleeds into another due to context then it becomes part of the
# other hunk.

process_hunks <- function(x, context) {
  stopifnot(
    is.integer(context), length(context) == 1L, !is.na(context), context >= 0L,
    is.list(x)  # assuming hunk list in correct format...
  )
  hunk.len <- length(x)
  if(hunk.len < 2L) return(x)

  res.l <- vector("list", length(x))

  # Jump through every second value as those are the mismatch hunks, though
  # first figure out if first hunk is mismatching

  i <- if(x$types[[1L]] == "Match") 2L else 1L
  j <- 1L
  while(i <= hunk.len) {
    # Merge left

    res.l[[j]] <- if(i - 1L) merge_hunks(
      lapply(x$hunks[[i - 1L]], tail, context), x$hunks[[i]]
    ) else x$hunks[[i]]

    # Merge right

    res.l[[j]] <- if(i < hunk.len) {
      # Hunks bleed into next hunk due to context

      while(length(x$hunks[[i + 1L]]$target <= context * 2)) {
        res.l[[j]] <- merge_hunks(res.l[[j]], x$hunks[[i + 1L]])
        if(i < hunk.len - 1L)
          res.l[[j]] <- merge_hunks(res.l[[j]], x$hunks[[i + 2L]])
        i <- i + 2L
      }
      # Context enough to cause a break

      if(i < hunk.len) {
        res.l[[j]] <- merge_hunks(
          res.l[[j]], lapply(x$hunks[[i - 1L]], head, context)
    ) } }
    j <- j + 1L
    i <- i + 2L
  }
  length(res.l) <- j - 1L
  res.ls
}
