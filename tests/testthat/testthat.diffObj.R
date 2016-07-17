context("diffObj")

if(!identical(basename(getwd()), "testthat"))
  stop("Working dir does not appear to be /testthat, is ", getwd())

rdsf <- function(x)
  file.path(getwd(), "helper", "diffObj", sprintf("%s.rds", x))

test_that("simple diffobj", {
  expect_equal_to_reference(as.character(diffObj(iris.s, iris.c)), rdsf(100))
})
