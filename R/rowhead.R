# Detect and remove atomic headers

.pat.atom <- "^\\s*\\[[1-9][0-9]*\\]\\s"

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
# numeric row headers.  This will also do arrays, but not arrays that require
# wrapping of matrix display components as that gets even more complicated

wtr_help <- function(x, pat) {
  # Should expect to find pattern repeated some number of times, and then whole
  # pattern possibly repeated the same number of times separated by the same
  # gap

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

    match.break <- min(
      which(
        match.blocks != rep(c(FALSE, TRUE), length.out=length(match.blocks))
      ),
      0L
    )
    match.valid <- if(match.break) {
      head(match.blocks, match.break - 1L)
    } else match.blocks

    # Make sure that all interstitial blocks are same length

    interstitial <- which(
      !match.valid & seq_along(match.valid) > 1L &
      seq_along(match.valid) != length(match.valid)
    )
    if(
      length(interstitial) &&
      length(unique(w.pat.rle$lengths[interstitial])) == 1L
    ) {
      # Make sure row headers are the same for each repeating block; start by
      # extracting the actual headers; need to get a list of each sequence of
      # headers

      max.valid <- max(which(match.valid))
      lens <- w.pat.rle$lengths[seq.int(max.valid)]
      vals <- w.pat.rle$values[seq.int(max.valid)]
      seq.start <- cumsum(lens)[!vals] + 1L
      seq.end <- cumsum(lens)[vals]
      ranges <- Map(seq, seq.start, seq.end, 1L)

      heads.l <- regmatches(x, regexec(pat, x))
      heads <- character(length(heads.l))
      heads[w.pat] <- as.character(heads.l[w.pat])

      heads.num <- as.integer(sub(".*?([0-9])+.*", "\\1", heads))
      head.ranges <- lapply(ranges, function(x) heads.num[x])

      all.identical <-
        vapply(head.ranges, identical, logical(1L), head.ranges[[1L]])
      all.one.apart <-
        vapply(head.ranges, function(x) all(diff(x) == 1L), logical(1L))

      if(all.identical && all.one.apart) {
        unlist(ranges)
      } else integer(0L)
    } else integer(0L)
  } else integer(0L)
}
which_table_rh <- function(x) {
  stopifnot(is.character(x), !anyNA(x))

  pat.1 <- "^\\s*\\[[1-9]+[0-9]*,\\]\\s"  # matrix / arrays
  pat.2 <- "^\\s*[1-9]+[0-9]*:?\\s"       # dfs/tables colon for data.table

  res <- wtr_help(x, pat.1)
  if(length(res)) {
    attr(res, "pat") <- pat.1
    res
  } else if(length(res <- wtr_help(x, pat.2))) {
    attr(res, "pat") <- pat.2
  } else integer(0L)
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
    sub(pat, "", x)
  }
}
setGeneric("stripRowHead",
  function(obj, obj.as.chr) StandardGeneric("stripRowHead")
)
setMethod(
  "stripRowHead", c("ANY", "character"),
  function(obj, obj.as.chr) {
    if(anyNA(obj.as.chr))
      stop("Cannot compute guides if `obj.as.chr` contains NAs")
    if(length(dim(obj)) == 2L) {
      detect_2d_guides(obj.as.chr)
    } else if (is.array(obj)) {
      detect_array_guides(obj.as.chr, dimnames(obj))
    } else if (is.atomic(obj)) {
      detect_list_guides(obj.as.chr)
    } else obj.as.chr
  }
)
