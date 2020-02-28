
context("Context")

if(!identical(basename(getwd()), "testthat"))
  stop("Working dir does not appear to be /testthat, is ", getwd())

rdsf <- function(x)
  file.path(getwd(), "helper", "context", sprintf("%s.rds", x))
txtf <- function(x)
  file.path(getwd(), "helper", "context", sprintf("%s.txt", x))

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
  # default to min context

  a <- b <- letters
  b[c(3, 20)] <- LETTERS[c(3,20)]
  expect_known_output(
    show(diffChr(a, b, line.limit=c(20, 10), context='auto', format='raw')),
    txtf(100)
  )
  # trim hunks in auto-context mode

  a <- b <- letters
  b[c(3, 10, 20)] <- LETTERS[c(3,10,20)]
  expect_known_output(
    show(
      diffChr(
        a, b, hunk.limit=c(2, 1), context=auto_context(1, 5), line.limit=20,
        format='raw'
      )
    ),
    txtf(200)
  )
})
test_that("error handling", {
  expect_error(auto_context(min=-1, max=1:3), "`min` must be")
  expect_error(auto_context(min=1, max=1:3), "`max` must be")
})
