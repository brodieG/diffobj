# Detect and remove atomic headers

.pat.atom <- "^\\s*\\[[1-9][0-9]*\\]\\s"
.pat.mat <- "^\\s*\\[[1-9]+[0-9]*,\\]\\s"

which_atomic_rh <- function(x) {
  stopifnot(is.character(x), !anyNA(x))
  w.pat <- grepl(.pat.atom, x)

  # Grab first set that matches for checking, there could be more particularly
  # if the object in question has attributes, but we won't bother with the
  # attributes

  w.pat.rle <- rle(w.pat)
  if(any(w.pat.rle$values)) {
    # First get the indices of the patterns that match

    first.block <- min(which(w.pat.rle$values))
    w.pat.start <- sum(head(w.pat.rle$lengths, first.block - 1L), 0L) + 1L
    w.pat.ind <-
      seq(from=w.pat.start, length=w.pat.rle$lengths[first.block], by=1L)

    # Re extract those and run checks on them to make sure they truly are
    # what we think they are: width of headers is the same, and numbers
    # increment in equal increments starting at 1

    r.h.rows <- x[w.pat.ind]
    r.h.vals <- regmatches(r.h.rows, regexpr(.pat.atom, r.h.rows))
    r.h.lens.u <- length(unique(nchar(r.h.vals)))
    r.h.nums <- sub(".*?([0-9]+).*", "\\1", r.h.vals, perl=TRUE)
    r.h.nums.u <- length(unique(diff(as.numeric(r.h.nums))))

    if(r.h.nums.u == 1L && r.h.lens.u == 1L && r.h.nums[[1L]] == "1") {
      w.pat.ind
    } else integer(0L)
  } else integer(0L)
}
strip_atomic_rh <- function(x) {
  stopifnot(is.character(x), !anyNA(x))
  w.r.h <- which_atomic_rh(x)
  x[w.r.h] <- sub(.pat.atom, "", x[w.r.h])
  x
}
# Detect table row headers; a bit lazy, combining all table like into one
# function when in reality more subtlety is warranted; also, we only care about
# numeric row headers.
#
# Matrices used to be done here as well, but then got split off so the `pat`
# argument is legacy

wtr_help <- function(x, pat) {
  # Should expect to find pattern repeated some number of times, and then whole
  # pattern possibly repeated the same number of times separated by the same
  # gap each time if the table is too wide and wraps.

  w.pat <- grepl(pat, x)
  w.pat.rle <- rle(w.pat)

  # It must be the case that the first block of matches occurs after non-matches
  # since the first header should happen first

  if(
    any(w.pat.rle$values) && length(w.pat.rle$values) > 1L &&
    w.pat.rle$values[2L]
  ) {
    tar.len <- w.pat.rle$lengths[2L]
    match.blocks <- w.pat.rle$values & w.pat.rle$lengths == tar.len

    # Only take matches they if alternate T/F

    match.break <- max(
      which(
        match.blocks != rep(c(FALSE, TRUE), length.out=length(match.blocks))
      ),
      0L
    )
    match.valid <- if(match.break) {
      head(match.blocks, match.break - 1L)
    } else match.blocks

    # Make sure that all interstitial blocks are same length and that they all
    # start with at least one space

    interstitial <- which(
      !match.valid & seq_along(match.valid) > 1L &
      seq_along(match.valid) != length(match.valid)
    )
    if(
      !length(interstitial) || (
        length(interstitial) &&
        length(unique(w.pat.rle$lengths[interstitial])) == 1L &&
        all(grepl("^\\s", x[unlist(rle_sub(w.pat.rle, interstitial))]))
      )
    ) {
      # Make sure row headers are the same for each repeating block; start by
      # extracting the actual headers; need to get a list of each sequence of
      # headers

      max.valid <- max(which(match.valid))
      ranges <- rle_sub(
        w.pat.rle, seq_along(w.pat.rle$lengths) <= max.valid & w.pat.rle$value
      )
      heads.l <- regmatches(x, regexec(pat, x))
      heads <- character(length(heads.l))
      heads[w.pat] <- as.character(heads.l[w.pat])

      heads.num <- as.integer(sub(".*?([0-9])+.*", "\\1", heads))
      head.ranges <- lapply(ranges, function(x) heads.num[x])

      all.identical <-
        vapply(head.ranges, identical, logical(1L), head.ranges[[1L]])
      all.one.apart <-
        vapply(head.ranges, function(x) all(diff(x) == 1L), logical(1L))

      if(all.identical && all.one.apart && head.ranges[[1L]][1L] == 1L) {
        unlist(ranges)
      } else integer(0L)
    } else integer(0L)
  } else integer(0L)
}
which_table_rh <- function(x) {
  stopifnot(is.character(x), !anyNA(x))
  pat.2 <- "^\\s*[1-9]+[0-9]*:?\\s"       # dfs/tables colon for data.table

  res <- wtr_help(x, pat.2)
  if(length(res)) attr(res, "pat") <- pat.2
  res
}
strip_table_rh <- function(x) {
  w <- which_table_rh(x)
  if(!length(w)) {
    x
  } else {
    pat <- attr(w, "pat")
    if(!is.chr.1L(pat))
      stop("Logic Error: unexpected row header pattern; contact maintainer.")
    x[w] <- sub(pat, "", x[w])
    x
  }
}
# Matrices; should really try to leverage logic in wtr_help, but not quite the
# same

which_matrix_rh <- function(x, dim.names.x) {
  guides <- detect_matrix_guides(x, dim.names.x)
  res <- integer(0L)
  if(length(guides)) {
    pieces <- split_by_guides(x, guides)
    if(!length(pieces)) stop("Logic Error: no matrix pieces")
    # Get all rows matching the matrix row header so long as they are adjacent;
    # this is only really different if there is an attribute in the last piece
    pat.ind <- lapply(
      pieces,
      function(y) {
        pat.match <- grep(.pat.mat, y)
        if(length(pat.match) > 1)
          pat.match[c(TRUE, !cumsum(diff(pat.match) != 1L))]
        else pat.match
      }
    )
    if(
      all(vapply(pat.ind, identical, logical(1L), pat.ind[[1L]])) &&
      (length(pat.ind[[1L]]) == 1L || all(diff(pat.ind[[1L]]) == 1L))
    ) {
      piece.nums <- as.integer(
        sub(".*?([0-9]+).*", "\\1", pieces[[1L]][pat.ind[[1L]]], perl=TRUE)
      )
      if(
        length(piece.nums) && piece.nums[1L] == 1L &&
        (length(piece.nums) == 1L || all(diff(piece.nums) == 1L))
      ) {
        res <- unlist(
          lapply(seq_along(pieces), function(i)
            attr(pieces[[i]], "idx")[pat.ind[[i]]]
  ) ) } } }
  res
}
strip_matrix_rh <- function(x, dim.names.x) {
  to.rep <- which_matrix_rh(x, dim.names.x)
  res <- x
  res[to.rep] <- sub(.pat.mat, "", x[to.rep])
  res
}
# Handle arrays

which_array_rh <- function(x, dim.names.x) {
  arr.h <- detect_array_guides(x, dim.names.x)
  dat <- split_by_guides(x, arr.h)

  # Look for the stuff between array guides; those should be matrix like
  # and have the same rows in each one

  m.h <- lapply(dat, which_matrix_rh, head(dim.names.x, 2L))

  if(length(m.h) && all(vapply(m.h, identical, logical(1L), m.h[[1L]]))) {
    unlist(Map(function(y, z) attr(y, "idx")[z], dat, m.h))
  } else integer(0L)
}
strip_array_rh <- function(x, dim.names.x) {
  inds <- which_array_rh(x, dim.names.x)
  res <- x
  res[inds] <- sub(.pat.mat, "", x[inds])
  res
}
#' Trim Method for Printed Objects
#'
#' @note \code{obj.as.chr} will be post \code{strip_hz_control}
#' @param obj the object
#' @param obj.as.chr charcter the \code{print}ed representation of the object
#' @rdname trim
#' @name trim
#' @aliases printTrim
#' @export
#' @return a \code{length(obj.as.chr) * 2} integer matrix with the start (first
#'   column and end (second column) character positions of the sub string to
#'   run diffs on.

setGeneric("printTrim",
  function(obj, obj.as.chr) StandardGeneric("printTrim")
)
setMethod(
  "printTrim", c("ANY", "character"),
  function(obj, obj.as.chr) {
    # Remove the stuff we don't want

    stripped <- if(is.matrix(obj)) {
      strip_matrix_rh(obj.as.chr, dimnames(obj))
    } else if(
      length(dim(obj)) == 2L ||
      (is.ts(obj) && frequency(obj) > 1)
    ) {
      strip_table_rh(obj.as.chr)
    } else if (is.array(obj)) {
      strip_array_rh(obj.as.chr, dimnames(obj))
    } else if(is.atomic(obj)) {
      strip_atomic_rh(obj.as.chr)
    } else obj.as.chr

    # Figure out the indices that correspond to what we want, knowing that all
    # removals should have occured at front of string

    if(length(obj.as.chr) != length(stripped))
      stop(
        "Logic Error: trimmed string does not have same number of elements as ",
        "original; contact maintainer"
      )
    stripped.chars <- nchar(stripped)
    char.diff <- nchar(obj.as.chr) - stripped.chars
    sub.start <- char.diff + 1L
    sub.end <- sub.start - 1L + stripped.chars

    if(!all(substr(obj.as.chr, sub.start, sub.end) == stripped))
      stop(
        "Logic Error: trimmed string is not a substring of orginal, ",
        "contact maintainer"
      )
    cbind(sub.start, sub.end)
  }
)
# Re-insert the trimmed stuff back into the original string

untrim <- function(raw, trim, ind)
  paste0(
    substr(raw, 0, ind[, 1L] - 1L), trim,
    substr(raw, ind[, 2L] + 1L, nchar(raw) + 1L)
  )

valid_trim_ind <- function(x)
  if(
    !is.integer(x) || !is.matrix(x) || anyNA(x) || !ncol(x) == 2L
  ) {
    "must be a two column integer matrix with no NAs"
  } else TRUE

apply_trim <- function(obj, obj.as.chr, trim_fun) {
  trim <- try(trim_fun(obj, obj.as.chr))
  msg.extra <- paste0(
    "If you did not define custom `*trim` methods contact maintainer ",
    "(see `?trim`)."
  )
  if(inherits(trim, "try-error"))
    stop(
      "`*trim` method produced an error when attempting to trim ; ", msg.extra
    )
  if(!isTRUE(trim.check <- valid_trim_ind(trim)))
    stop("`*trim` method ", trim.check, "; ", msg.extra)
  if(nrow(trim) != length(obj.as.chr))
    stop(
      "`*trim` method output matrix must have as many rows as object ",
      "character representation has elements; ", msg.extra
    )
  trim
}
