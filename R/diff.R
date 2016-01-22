#' Diff two character vectors
#'
#' Implementation of Myer's diff borrowed from Mike B. Allen
#'
#' @export
#' @param a character
#' @param b character
#' @return list
#' @useDynLib diffr, .registration=TRUE, .fixes="DIFFR_"

diffr <- function(a, b) {
  stopifnot(is.character(a), is.character(b), all(!is.na(c(a, b))))
  res <- .Call(DIFFR_diffr, a, b)
  res <- setNames(res, c("type", "length", "offset"))
  types <- c("M", "I", "D")
  res$type <- factor(
    types[res$type], levels=types, labels=c("Match", "Insert", "Delete")
  )
  res$offset <- res$offset + 1L  # C 0-indexing originally
  class(res) <- "diffr"
  res
}
#' @export

print.diffr <- function(x, with.match=FALSE, ...) {
  stopifnot(
    is.list(x), length(unique(unlist(lapply(x, length)))) == 1L,
    identical(
      vapply(x, class, ""),
      c(type="factor", length="integer", offset="integer")
    ),
    identical(levels(x$type), c("Match", "Insert", "Delete")),
    all(!is.na(unlist(x))),
    all(unlist(x) > 0L),
    isTRUE(with.match) || identical(with.match, FALSE)
  )
  what <- vapply(
    seq_along(x$type),
    function(y) {
      with(
        x, {
          vec <- if(type[[y]] == "Insert") b else a
          paste0(vec[offset[[y]]:(offset[[y]] + length[[y]] - 1L)], collapse="")
    } ) },
    character(1L)
  )
  print(
    with(
      x,
      data.frame(type, what, length, offset)
  ) )
}
