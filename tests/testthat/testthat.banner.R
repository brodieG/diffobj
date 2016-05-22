library(testthat)

test_that("Banner Capture", {
  ref <- as.character(
      diffPrint(1 + 2, letters, tar.banner="1 + 2", cur.banner="letters")
    )
  expect_identical(as.character(diffPrint(1 + 2, letters)), ref)
  setMethod(
    "diffPrint", c("numeric", "character"),
    function(target, current, ...) callNextMethod()
  )
  expect_identical(as.character(diffPrint(1 + 2, letters)), ref)
  expect_true(
    !identical(as.character(diffPrint(1 + 2, LETTERS)), ref)
  )
} )
