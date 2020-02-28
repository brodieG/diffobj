# Copyright (C) 2019 Brodie Gaslam
#
# This file is part of "diffobj - Diffs for R Objects"
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# Go to <https://www.r-project.org/Licenses/GPL-2> for a copy of the license.

#' Run Rdiff Directly on R Objects
#'
#' These functions are here for reference and testing purposes.  They are
#' wrappers to \code{tools::Rdiff} and rely on an existing system diff utility.
#' You should be using \code{\link{ses}} or \code{\link{diffChr}} instead of
#' \code{Rdiff_chr} and \code{\link{diffPrint}} instead of \code{Rdiff_obj}.
#' See limitations in note.
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
#' @seealso \code{\link{ses}}, \code{\link[=diffPrint]{diff*}}
#' @param from character or object coercible to character for \code{Rdiff_chr},
#'   any R object with \code{Rdiff_obj}, or a file pointing to an RDS object
#' @param to character same as \code{from}
#' @param nullPointers passed to \code{tools::Rdiff}
#' @param silent TRUE or FALSE, whether to display output to screen
#' @param minimal TRUE or FALSE, whether to exclude the lines that show the
#'   actual differences or only the actual edit script commands
#' @return the Rdiff output, invisibly if \code{silent} is FALSE
#' Rdiff_chr(letters[1:5], LETTERS[1:5])
#' Rdiff_obj(letters[1:5], LETTERS[1:5])

Rdiff_chr <- function(from, to, silent=FALSE, minimal=FALSE, nullPointers=TRUE) {
  A <- try(as.character(from))
  if(inherits(A, "try-error")) stop("Unable to coerce `target` to character.")
  B <- try(as.character(to))
  if(inherits(B, "try-error")) stop("Unable to coerce `current` to character.")

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
  res <- tryCatch(
    Rdiff(
      from=from, to=to, useDiff=TRUE, Log=TRUE, nullPointers=nullPointers
    )$out,
    warning=function(e)
      stop(
        "`tools::Rdiff` returned a warning; this likely means you are running ",
        "without a `diff` utility accessible to R"
      )
  )
  if(!is.character(res))
    # nocov start
    stop("Internal Error: Unexpected tools::Rdiff output, contact maintainer")
    # nocov end

  res <- if(minimal) res[!grepl("^[<>-]", res)] else res
  if(silent) res else {
    cat(res, sep="\n")
    invisible(res)
  }
}
#' Attempt to Detect Whether diff Utility is Available
#'
#' Checks whether \code{\link[=Rdiff]{tools::Rdiff}} issues a warning when
#' running with \code{useDiff=TRUE} and if it does assumes this is because the
#' diff utility is not available.  Intended primarily for testing purposes.
#'
#' @export
#' @return TRUE or FALSE
#' @param test.with function to test for diff presence with, typically Rdiff
#' @examples
#' has_Rdiff()

has_Rdiff <- function(test.with=tools::Rdiff) {
  f.a <- tempfile()
  f.b <- tempfile()
  on.exit(unlink(c(f.a, f.b)))
  writeLines(letters[1:3], f.a)
  writeLines(LETTERS, f.b)
  tryCatch(
    {
      test.with(
        from=f.a, to=f.b, useDiff=TRUE, Log=TRUE, nullPointers=FALSE
      )
      TRUE
    }, warning=function(e) FALSE
  )
}
