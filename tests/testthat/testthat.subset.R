
context("subset")

A <- B <- letters[1:5]
B[2] <- "B"
B[6] <- "F"

test_that("subset", {
  old.opt <- options(diffobj.style=StyleRaw())
  on.exit(options(old.opt))
  expect_equal(
    c(as.character(diffChr(A, B)[1:3])),
    c("< A          > B        ", "@@ 1,5 @@    @@ 1,6 @@  ", "  a            a        ")

  )
  expect_equal(
    c(as.character(diffChr(A, B)[1])), c(as.character(head(diffChr(A, B), 1)))
  )
  expect_equal(
    c(as.character(diffChr(A, B)[7:8])), c(as.character(tail(diffChr(A, B), 2)))
  )
})
test_that("subset errors", {
  diff <- diffChr(A, B)
  expect_error(diff[NA_real_], "contain NAs or both positive")
  expect_error(diff[c(-1, 1)], "contain NAs or both positive")
  expect_error(head(diff, 1, 2), "does not support arguments")
  expect_error(head(diff, NA), "must be integer")
  expect_error(head(diff, 1:3), "must be integer")
  expect_error(tail(diff, 1:3), "must be integer")
  expect_error(tail(diff, 1, 2), "does not support arguments")
})

