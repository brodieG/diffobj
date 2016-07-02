context("summary")

if(!identical(basename(getwd()), "testthat"))
  stop("Working dir does not appear to be /testthat, is ", getwd())

rdsf <- function(x)
  file.path(getwd(), "helper", "summary", sprintf("%s.rds", x))

# Note, atomic prints happen in different test file

test_that("Any", {
  expect_false(any(diffPrint(iris.s, iris.s)))
  expect_warning(res <- any(diffPrint(iris.s, iris.c)), "objects are NOT")
  expect_false(res)
  expect_true(any(diffPrint(iris.s, iris.4)))
})

test_that("Small Summary", {
  expect_equal_to_reference(
    as.character(summary(diffPrint(iris.s, iris.4))), rdsf(100)
  )
  expect_equal_to_reference(
    as.character(summary(diffPrint(iris.s, iris.2))), rdsf(200)
  )
  expect_equal_to_reference(
    as.character(summary(diffPrint(iris.s, iris.3))), rdsf(300)
  )
  expect_equal_to_reference(
    as.character(summary(diffPrint(iris.s, iris.c))), rdsf(400)
  )
})
test_that("Big Summary", {
  # Make sure we test summary reduction, wrapping

  expect_equal_to_reference(
    as.character(summary(diffChr(chr.7, chr.8))), rdsf(500)
  )
  expect_equal_to_reference(
    as.character(summary(diffChr(chr.7, chr.8), scale.threshold=1)), rdsf(600)
  )
  expect_equal_to_reference(
    as.character(summary(diffChr(chr.7, chr.8), scale.threshold=0)), rdsf(700)
  )
})
