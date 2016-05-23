library(diffobj)

context("Limits")

test_that("limits", {
  old.opt <- options(diffobj.format="raw", disp.width=80L)
  on.exit(options(old.opt))
  A <- B <- letters[1:5]
  B[2] <- "B"
  B[6] <- "F"
  diffChr(A, B, line.limit=1, context=1L)
})
