# Run \code{tools::Rdiff} On Contents of Char Vectors
#
# Each cell in the vector is treated as a line in the file.

base_diff <- function(A, B, minimal=TRUE) {
  af <- tempfile()
  bf <- tempfile()
  writeLines(A, af)
  writeLines(B, bf)

  try(res <- tools::Rdiff(af, bf, useDiff=TRUE, Log=TRUE)$out)
  unlink(c(af, bf))
  if(!is.null(res)) {
    res <- if(minimal) res[!grepl("^[<>-]", res)] else res
    cat(res, sep="\n")
  }
  invisible(res)
}
#' a \code{tools::Rdiff} Between R Objects
#'
#' Just a wrapper that saves the \code{print} / \code{show} representation of an
#' object to a temp file and then runs \code{tools::Rdiff} on them.  For
#' each of \code{from}, \code{to}, will check if they are 1 length character
#' vectors referencing an RDS file, and will use the contents of that RDS file
#' as the object to compare.
#'
#' @export
#' @seealso \code{tools::Rdiff}
#' @param from an R object (see details)
#' @param to another R object (see details)
#' @param ... passed on to \code{Rdiff}
#' @return whatever \code{Rdiff} returns

Rdiff_obj <- function(from, to, ...) {
  dummy.env <- new.env()  # used b/c unique object
  files <- try(
    vapply(
      list(from, to),
      function(x) {
        if(
          is.character(x) && length(x) == 1L && !is.na(x) && file_test("-f", x)
        ) {
          rdstry <- tryCatch(readRDS(x), error=function(x) dummy.env)
          if(!identical(rdstry, dummy.env)) x <- rdstry
        }
        f <- tempfile()
        capture.output(if(isS4(x)) show(x) else print(x), file=f)
        f
      },
      character(1L)
  ) )
  if(inherits(files, "try-error"))
    stop("Unable to store text representation of objects")
  res <- tools::Rdiff(files[[1L]], files[[2L]], ...)
  unlink(files)
  invisible(res)
}
