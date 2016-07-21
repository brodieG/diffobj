context("diffDeparse")

if(!identical(basename(getwd()), "testthat"))
  stop("Working dir does not appear to be /testthat, is ", getwd())

rdsf <- function(x)
  file.path(getwd(), "helper", "diffDeparse", sprintf("%s.rds", x))

test_that("deparse", {
  # First one will be done in unified mode since `deparse` disregards
  # option(width=), second will be done side by side

  expect_equal_to_reference(
    as.character(diffDeparse(letters, LETTERS)), rdsf(100)
  )
  expect_equal_to_reference(
    as.character(
      diffDeparse(letters, LETTERS, extra=list(width.cutoff=20))
    ),
    rdsf(200)
  )
})
