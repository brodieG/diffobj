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
