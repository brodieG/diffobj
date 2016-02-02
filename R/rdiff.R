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
#' @note These functions will try to use the system \code{diff} utility. This
#'   will fail in systems that do not have that utility available (e.g. windows
#'   installation without Rtools).
#' @importFrom tools Rdiff
#' @export
#' @seealso \code{\link{diff_ses}}, \code{\link{diff_obj}}
#' @param from character or object coercible to character for \code{Rdiff_chr},
#'   any R object with \code{Rdiff_obj}
#' @param to character same as \code{from}
#' @param nullPointers passed to \code{tools::Rdiff}
#' @param silent TRUE or FALSE, whether to display output to screen
#' @param minimal TRUE or FALSE, whether to exclude the lines that show the
#'   actual differences or only the actual edit script commands
#' @return the Rdiff output, invisibly if \code{silent} is FALSE

Rdiff_chr <- function(from, to, silent=FALSE, minimal=FALSE, nullPointers=TRUE) {
  A <- try(as.character(from))
  if(inherits(A, "try-error")) stop("Unable to coerce `target` to character.")
  B <- try(as.character(to))
  if(inherits(b, "try-error")) stop("Unable to coerce `current` to character.")

  af <- tempfile()
  bf <- tempfile()
  writeLines(A, af)
  writeLines(B, bf)
  on.exit(unlink(c(af, bf)))

  Rdiff_run(
    silent=silent, minimal=minimal, from=af, to=bf, nullPointers=nullPointers
  )
}
#' @export
#' @rdname Rdiff_chr

Rdiff_obj <- function(from, to, silent=FALSE, minimal=FALSE, nullPointers=TRUE) {
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
        on.exit(unlink(f))
        capture.output(if(isS4(x)) show(x) else print(x), file=f)
        on.exit()
        f
      },
      character(1L)
  ) )
  if(inherits(files, "try-error"))
    stop("Unable to store text representation of objects")
  on.exit(unlink(files))
  Rdiff_run(
    from=files[[1L]], to=files[[2L]], silent=silent, minimal=minimal,
    nullPointers=nullPointers
  )
}
# Internal use only: BEWARE, will unlink from, to

Rdiff_run <- function(from, to, nullPointers, silent, minimal) {
  stopifnot(
    isTRUE(silent) || identical(silent, FALSE),
    isTRUE(minimal) || identical(minimal, FALSE)
  )
  res <- Rdiff(
    from=from, to=to, useDiff=TRUE, Log=TRUE, nullPointers=nullPointers
  )$out
  if(!is.character(res)) stop("Unexpected tools::Rdiff output")

  res <- if(minimal) res[!grepl("^[<>-]", res)] else res
  if(silent) res else {
    cat(res, sep="\n")
    invisible(res)
  }
}
