context("diffStr")

if(!identical(basename(getwd()), "testthat"))
  stop("Working dir does not appear to be /testthat, is ", getwd())

rdsf <- function(x)
  file.path(getwd(), "helper", "diffStr", sprintf("%s.rds", x))

library(diffobj)

test_that("lm models", {

  expect_equal_to_reference(as.character(diffStr(mdl1, mdl2)), rdsf(100))
  diffStr(mdl1[7], mdl2[7], line.limit=10)
  

  # interesting example below where the in-hunk word diff is too aggressive
  # preventing the eq-lines from atching

  diffStr(mdl1[7], mdl2[7], mode="sidebyside")
  diffPrint(mdl1, mdl2)
  diffStr(
    mdl1, mdl2, mode="sideby", bright="dark",
    pager=PagerSystemLess(flags="RX")
  )
  # Check that `max.level` shows up when using it


})
diffStr(cars, mtcars)
