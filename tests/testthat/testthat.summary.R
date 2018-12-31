context("summary")

if(!identical(basename(getwd()), "testthat"))
  stop("Working dir does not appear to be /testthat, is ", getwd())

rdsf <- function(x)
  file.path(getwd(), "helper", "summary", sprintf("%s.rds", x))
txtf <- function(x)
  file.path(getwd(), "helper", "summary", sprintf("%s.txt", x))

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
  # All equal

  expect_equal_to_reference(
    as.character(summary(diffChr(letters, letters))), rdsf(450)
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
  # Force truncation of summary
  expect_equal_to_reference(
    as.character(
      summary(diffChr(chr.7, chr.8), scale.threshold=0, max.lines=2)
    ),
    rdsf(800)
  )
})
test_that("Show", {
  expect_true(
    paste0(capture.output(summary(diffChr(chr.7, chr.8))), collapse="\n") ==
    as.character(summary(diffChr(chr.7, chr.8)))
  )
})
test_that("HTML summary", {
  expect_equal_to_reference(
    as.character(
      summary(
        diffPrint(
          iris.s, iris.4, format="html", style=list(html.output="page")
    ) ) ),
    rdsf(900)
  )
})
test_that("errors", {
  diff <- diffChr("hello green world", "hello red world")
  expect_error(summary(diff, max.lines=0), "strictly positive")
  expect_error(summary(diff, width=1:3), "integer\\(1L\\)")
  expect_error(summary(diff, scale.threshold=5), "between 0 and 1")
})
test_that("width wrap", {
  diff <- diffChr("hello green world", "hello red world", format='raw')
  expect_known_output(show(summary(diff, width=5)), txtf(100))
})
