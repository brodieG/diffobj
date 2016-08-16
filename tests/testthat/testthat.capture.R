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
