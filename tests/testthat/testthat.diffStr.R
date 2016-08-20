context("diffStr")

if(!identical(basename(getwd()), "testthat"))
  stop("Working dir does not appear to be /testthat, is ", getwd())

rdsf <- function(x)
  file.path(getwd(), "helper", "diffStr", sprintf("%s.rds", x))

library(diffobj)

test_that("lm models", {
  # formula display changed
  if(R.Version()$major >= 3 && R.Version()$minor >= "3.1")
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
  # formula display changed
  if(R.Version()$major >= 3 && R.Version()$minor >= "3.1") {
    expect_equal_to_reference(
      as.character(
        diffStr(mdl1, mdl2, extra=list(strict.width="wrap"), line.limit=30)
      ),
      rdsf(500)
    )
  }
})
test_that("max.level", {
  expect_equal_to_reference(
    as.character(diffStr(mdl1[7], mdl2[7], extra=list(max.level="auto"))),
    rdsf(600)
  )
  expect_equal_to_reference(
    as.character(diffStr(mdl1[7], mdl2[7], extra=list(max.level=2))),
    rdsf(700)
  )
  # Has a difference, but can't get under; the second is just for reference

  lst.1 <- lst.2 <- lst.3 <- list(a=list(b=list(c=list(d=list(e=list(25))))))
  names(lst.2) <- "A"

  expect_equal_to_reference(
    as.character(diffStr(lst.1, lst.2, line.limit=2)), rdsf(800)
  )
  expect_equal_to_reference(
    as.character(diffStr(lst.1, lst.2, line.limit=2)), rdsf(900)
  )
  # Test that initial run shows difference, but too big, but next one down
  # doesn't so have to increase level

  names(lst.3$a$b$c$d) <- "E"
  expect_equal_to_reference(
    as.character(diffStr(lst.1, lst.3, line.limit=6)), rdsf(1000)
  )
})

