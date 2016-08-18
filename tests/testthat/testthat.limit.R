context("mis")

if(!identical(basename(getwd()), "testthat"))
  stop("Working dir does not appear to be /testthat, is ", getwd())

rdsf <- function(x)
  file.path(getwd(), "helper", "limit", sprintf("%s.rds", x))

library(diffobj)

test_that("Simple limit", {
  A <- B <- letters[1:5]
  B[2] <- "B"
  B[6] <- "F"
  # diffChr(A, B)
  expect_equal_to_reference(
    as.character(diffChr(A, B, line.limit=2)), rdsf(100)
  )
  expect_equal_to_reference(
    as.character(diffChr(A, B, line.limit=3)), rdsf(200)
  )
})
test_that("More Extensive Limits", {
  Puromycin2 <- Puromycin
  set.seed(1)
  Puromycin2$conc[c(8, 15:19, 22)] <- round(runif(7), 2)
  Puromycin2$state[17] <- "treated"

  expect_equal_to_reference(
    as.character(
      diffPrint(Puromycin, Puromycin2, line.limit=15, mode="sidebyside")
    ),
    rdsf(300)
  )

  # # Not working right
  # diffPrint(Puromycin, Puromycin2, line.limit=15, mode="context")
  expect_equal_to_reference(
    as.character(
      diffPrint(Puromycin, Puromycin2, line.limit=15, mode="unified")
    ),
    rdsf(500)
  )

  expect_equal_to_reference(
    as.character(
      diffPrint(Puromycin, Puromycin2, line.limit=5, mode="sidebyside")
    ),
    rdsf(600)
  )
  expect_equal_to_reference(
    as.character(
      diffPrint(Puromycin, Puromycin2, line.limit=5, mode="context")
    ),
    rdsf(700)
  )
  expect_equal_to_reference(
    as.character(
      diffPrint(Puromycin, Puromycin2, line.limit=5, mode="unified")
    ),
    rdsf(800)
  )

  Puromycin3 <- Puromycin2
  names(Puromycin3)[3L] <- "blargh"
  expect_equal_to_reference(
    as.character(
      diffPrint(Puromycin, Puromycin3, line.limit=7, mode="sidebyside")
    ),
    rdsf(900)
  )
  expect_equal_to_reference(
    as.character(
      diffPrint(Puromycin, Puromycin3, line.limit=6, mode="context")
    ),
    rdsf(1000)
  )
} )
test_that("Dual limit values", {
  A <- letters[1:10]
  B <- LETTERS[1:10]
  expect_equal_to_reference(
    as.character(diffChr(A, B, line.limit=c(10, 3))), rdsf(1100)
  )
  expect_equal_to_reference(
    as.character(diffChr(A, B, line.limit=c(13, 3))), rdsf(1200)
  )
  expect_error(diffChr(A, B, line.limit=c(3, 13)), "larger than or")
})

test_that("Cause errors", {
  expect_error(diffChr(letters, LETTERS, line.limit=1:3), "vector of length")
})




