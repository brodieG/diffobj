# nocov start
# these functions may rely on using the system diff utility, which may not be
# available on some systems, particularly basic windows installations without
# the development tools.  This tools are also not part of the core functionality
# of this package.  For these reasons, these functions do have unit tests and
# are excluded from coverage computations.

#' Run \code{tools::Rdiff} Directly on R Objects
#'
#' These functions are here for reference and testing purposes.  You should be
#' using \code{\link{diff_ses}} instead of \code{Rdiff_chr} and
#' \code{\link{diff_obj}} instead of \code{Rdiff_obj}.  See limitations in note.
#'
#' \code{Rdiff_chr} runs diffs on character vectors or objects coerced to
#' character vectors, where each value in the vectors is treated as a line in a
#' file.  \code{Rdiff_chr} always runs with the \code{useDiff} and \code{Log}
#' parameters set to \code{TRUE}.
#'
#' \code{Rdiff_obj} runs diffs on the \code{print}ed representation of
#' the provided objects.  For each of \code{from}, \code{to}, will check if they
#' are 1 length character vectors referencing an RDS file, and will use the
#' contents of that RDS file as the object to compare.
#'
#' @note \code{Rdiff_chr} will try to use the system \code{diff} utility, and
#'   \code{Rdiff_obj} might try as well.  This will fail in systems that do
#'   not have that utility available (e.g. windows installation without Rtools).
#' @importFrom tools Rdiff
#' @export
#' @aliases Rdiff_obj
#' @seealso \code{\link{diff_ses}}, \code{\link{diff_obj}}
#' @param from character or object coercible to character
#' @param to character or object coercible to character
#' @param ... additional arguments to pass on to \code{tools::Rdiff}
#' @param silent TRUE or FALSE, whether to display output to screen, only for
#'   \code{Rdiff_chr}
#' @param minimal TRUE or FALSE, whether to exclude the lines that show the
#'   actual differences, only for \code{Rdiff_chr}
#' @return for \code{Rdiff_chr} character vector, invisibly, the diff script,
#'   for \code{Rdiff_obj} the return value of \code{tools::Rdiff}

Rdiff_chr <- function(from, to, silent=FALSE, minimal=TRUE) {
  A <- try(as.character(from))
  if(inherits(A, "try-error")) stop("Unable to coerce `target` to character.")
  B <- try(as.character(to))
  if(inherits(b, "try-error")) stop("Unable to coerce `current` to character.")

  af <- tempfile()
  bf <- tempfile()
  writeLines(A, af)
  writeLines(B, bf)

  try(res <- Rdiff(af, bf, useDiff=TRUE, Log=TRUE)$out)
  unlink(c(af, bf))
  if(!is.null(res)) {
    res <- if(minimal) res[!grepl("^[<>-]", res)] else res
    cat(res, sep="\n")
  }
  invisible(res)
}
#' @export
#' @rdname Rdiff_chr

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
  res <- Rdiff(files[[1L]], files[[2L]], ...)
  unlink(files)
  invisible(res)
}

# nocov end
