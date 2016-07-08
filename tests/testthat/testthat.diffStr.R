context("diffStr")

if(!identical(basename(getwd()), "testthat"))
  stop("Working dir does not appear to be /testthat, is ", getwd())

rdsf <- function(x)
  file.path(getwd(), "helper", "diffStr", sprintf("%s.rds", x))

library(diffobj)

test_that("lm models", {

  expect_equal_to_reference(as.character(diffStr(mdl1, mdl2)), rdsf(100))
  # Too strict a line limit, can't get under
  expect_equal_to_reference(
    as.character(diffStr(mdl1[7], mdl2[7], line.limit=10)), rdsf(200)
  )
  # Now we can get under
  expect_equal_to_reference(
    as.character(diffStr(mdl1[7], mdl2[7], line.limit=15)), rdsf(300)
  )
})
test_that("Simple structure", {
  # Character types

  expect_equal_to_reference(as.character(diffStr(iris.c, iris.s)), rdsf(400))
})
test_that("Strict width", {
  expect_equal_to_reference(
    as.character(
      diffStr(mdl1, mdl2, extra=list(strict.width="wrap"), line.limit=30)
    ),
    rdsf(400)
  )
})
