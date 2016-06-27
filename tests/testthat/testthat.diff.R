context("diffPrint")

if(!identical(basename(getwd()), "tests"))
  stop("Working dir does not appear to be /tests")


rdsf <- function(x)
  file.path(
    getwd(), "testthat", "helper", "diffPrint", sprintf("%s.rds", x)
  )
# Note, atomic prints happen in different test file


test_that("Matrices", {
  mx.2 <- matrix(1:100, ncol=2)
  mx.4 <- mx.3 <- mx.2
  mx.3[31, 2] <- 111L

  mx.3a <- mx.3[-31, ]
  set.seed(2)
  mx.4[cbind(sample(1:50, 6), sample(1:2, 6, replace=TRUE))] <-
    sample(-(1:50), 6)

  mx.5 <- matrix(1:9, 3)
  mx.6 <- matrix(12:1, 4)
  mx.6[4,] <- c(3L, 6L, 9L)

  # single value difference
  expect_equal_to_reference(as.character(diffPrint(mx.2, mx.3)), rdsf(100))
  # single value unified
  expect_equal_to_reference(
    as.character(diffPrint(mx.2, mx.3, mode="unified")), rdsf(150)
  )
  # single value context
  expect_equal_to_reference(
    as.character(diffPrint(mx.2, mx.3, mode="context")), rdsf(175)
  )
  # missing row
  expect_equal_to_reference(as.character(diffPrint(mx.2, mx.3a)), rdsf(200))
  expect_equal_to_reference(
    as.character(diffPrint(mx.2, mx.3a, mode="unified")), rdsf(300)
  )
  # More differences

  expect_equal_to_reference(as.character(diffPrint(mx.2, mx.4)), rdsf(400))
  expect_equal_to_reference(
    as.character(diffPrint(mx.2, mx.4, mode="unified")), rdsf(500)
  )
  # Testing alignments
  expect_equal_to_reference(as.character(diffPrint(mx.5, mx.6)), rdsf(600))
  expect_equal_to_reference(
    as.character(diffPrint(mx.5, mx.6, mode="unified")), rdsf(700)
  )
  expect_equal_to_reference(
    as.character(diffPrint(mx.5, mx.6, mode="context")), rdsf(800)
  )
  # More complex matrix
  set.seed(2)
  A <- B <- matrix(sample(1:80), nrow=16)
  B[cbind(sample(5:16, 4), sample(1:5, 4))] <- sample(30:80, 4)

  expect_equal_to_reference(diffPrint(A, B), rdsf(900))
  expect_equal_to_reference(
    as.character(diffPrint(A, B, mode="unified")), rdsf(1000)
  )
  expect_equal_to_reference(
    as.character(diffPrint(A, B, mode="context")), rdsf(1100)
  )
  # Style matrices

  expect_equal_to_reference(
    as.character(diffPrint(diffobj:::.mx1, diffobj:::.mx2)), rdsf(1200)
  )
})
test_that("Lists", {
  diffPrint(lst.1, lst.3)
  diffPrint(lst.1, lst.3, mode="sidebyside", context=1)
  diffPrint(lst.1, lst.3, mode="unified")

  diffPrint(lst.4, lst.5)

  diffPrint(unclass(mdl1), unclass(mdl2))
})
test_that("Data Frames", {
  # Shows that line shifts within hunks are matched

  diffPrint(iris, iris.2) # no round
  diffPrint(iris, iris.2, mode="sidebyside")
  diffPrint(iris, iris.c)
  diffPrint(iris, iris.3)
  diffPrint(iris, iris.3, mode="sidebyside")
  diffPrint(iris, iris.4, mode="unified")
  diffPrint(iris, iris.4, mode="sidebyside")

  diffPrint(iris, iris.4, mode="sidebyside", guides=function(x, y) integer())
  diffPrint(iris, iris.4, mode="sidebyside", guides=FALSE)

  diffPrint(iris.5, iris.4, mode="sidebyside")
  diffPrint(iris.3a, iris.4a)

  diffPrint(cars, mtcars, mode="s")
  diffPrint(iris, iris[-2])
})

test_that("Arrays", {
  diffPrint(arr.1, arr.2, mode="s", context=1)
  diffPrint(arr.1, arr.2, mode="context", context=1)

  # Large DF (WARNING: a bit slow)
})
test_that("Mixed", {
  diffPrint(list(1, 2, 3), matrix(1:9, 3))
  diffPrint(list(25, 2, 3), matrix(1:9, 3))
  diffPrint(list(c(1, 4, 7), c(2, 5, 8), c(3, 6, 9)), matrix(1:9, 3))

  # Arrays
})

  # library(ggplot2)
  # head(diamonds)
  # d2 <- diamonds
  # d2$x[sample(seq_len(nrow(d2)), 100)] <- sample(d2$x, 100)
  # diffPrint(diamonds, d2)
  # diffPrint(diamonds, d2, context=1)
  # diffPrint(diamonds, d2, context=1, mode="s")
