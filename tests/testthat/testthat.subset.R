
context("subset")

test_that("subset", {
  old.opt <- options(diffobj.style=StyleRaw())
  on.exit(options(old.opt))
  A <- B <- letters[1:5]
  B[2] <- "B"
  B[6] <- "F"
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

