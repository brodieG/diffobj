context("diffObj")

if(!identical(basename(getwd()), "testthat"))
  stop("Working dir does not appear to be /testthat, is ", getwd())

rdsf <- function(x)
  file.path(getwd(), "helper", "diffObj", sprintf("%s.rds", x))

test_that("simple diffobj", {
  # no diff for print
  expect_equal_to_reference(as.character(diffObj(iris.s, iris.c)), rdsf(100))
  # no diff for str
  expect_equal_to_reference(
    as.character(diffObj(1:100, c(1:99, 200L))), rdsf(200)
  )
  # diffs for both and must pick one, first one is str, second is print
  expect_equal_to_reference(
    as.character(diffObj(mdl1[7], mdl2[7])), rdsf(300)
  )
  expect_equal_to_reference(as.character(diffObj(mdl1, mdl2)), rdsf(400))
})
