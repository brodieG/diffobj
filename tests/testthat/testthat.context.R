
context("Context")

if(!identical(basename(getwd()), "testthat"))
  stop("Working dir does not appear to be /testthat, is ", getwd())

rdsf <- function(x)
  file.path(getwd(), "helper", "context", sprintf("%s.rds", x))

test_that("interesting context values", {
  expect_equal_to_reference(
    as.character(diffChr(chr.9, chr.10, context=0)),
    rdsf(100)
  )
  expect_equal_to_reference(
    as.character(diffChr(chr.9, chr.10, context=-1L)),
    rdsf(150)
  )
  expect_equal_to_reference(
    as.character(diffChr(chr.9, chr.10, context="auto")),
    rdsf(200)
  )
  expect_equal_to_reference(
    as.character(diffChr(chr.9, chr.10, context=0, mode="context")), rdsf(300)
  )
})

test_that("with line limit", {
  expect_equal_to_reference(
    as.character(diffChr(chr.9, chr.10, context="auto", line.limit=18)),
    rdsf(400)
  )
  expect_equal_to_reference(
    as.character(diffChr(chr.9, chr.10, context="auto", line.limit=25)),
    rdsf(500)
  )
})
