library(testthat)
library(diffobj)

context("capture")

test_that("capture width issues", {
  old.opt <- options(width=40L)
  on.exit(options(old.opt))
  etc <- new("Settings", style=StyleRaw(), text.width=5L)  # impossible width
  expect_warning(
    res <-
      diffobj:::capture(letters, etc, function(...) do.call(cat, list(...))),
    "Unable to set desired "
  )
  expect_equal(nchar(res), c(40L, 40L, 36L))
})
test_that("errors in capture", {
  etc <- new("Settings", style=StyleRaw())
  expect_error(diffobj:::capture(stop('boom'), etc, function(...) stop(...)),
    "Failed attempting.*boom"
  )
  print <- function() NULL
  str <- function() NULL
  etc@mode <- "auto"
  etc@frame <- environment()
  expect_error(
    diffobj:::capt_print(1, 2, etc, function(...) stop(...), list()),
    "Unable to compose"
  )
  expect_error(
    diffobj:::capt_str(1, 2, etc, function(...) stop(...), list(object=1)),
    "specify `object`"
  )
  expect_error(
    diffobj:::capt_deparse(
      stop('a'), stop('b'), etc,  function(...) stop(...), list()
    ),
    "attempting to deparse"
  )
  expect_error(
    suppressWarnings(
      diffobj:::capt_file(
        tempfile(), tempfile(), etc,  function(...) stop(...), list()
    ) ),
    "`target`"
  )
  f <- tempfile()
  on.exit(unlink(f), add=TRUE)
  writeLines(letters, f)
  expect_error(
    suppressWarnings(
      diffobj:::capt_file(f, tempfile(), etc,  function(...) stop(...), list())
    ),
    "`current`"
  )
  expect_error(
    suppressWarnings(
      diffobj:::capt_csv(
        tempfile(), tempfile(), etc,  function(...) stop(...), list()
    ) ),
    "`target`"
  )
  expect_error(
    suppressWarnings(
      diffobj:::capt_csv(
        f, tempfile(), etc,  function(...) stop(...), list()
    ) ),
    "`current`"
  )
  bad_obj <- structure(list(NULL), class='diffobj_ogewlhgiadfl3')
  expect_error(
    diffobj:::capt_chr(bad_obj, letters, etc,  function(...) stop(...), list()),
    "Coercion of `target`"
  )
  expect_error(
    diffobj:::capt_chr(letters, bad_obj, etc,  function(...) stop(...), list()),
    "Coercion of `current`"
  )
})
