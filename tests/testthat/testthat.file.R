library(diffobj)

test_that("RDS", {
  f1 <- tempfile()
  f2 <- tempfile()
  on.exit(unlink(c(f1, f2)))

  mx1 <- mx2 <- matrix(1:9, 3)
  mx2[5] <- 99
  saveRDS(mx1, f1)
  saveRDS(mx2, f2)

  expect_is(diffobj:::get_rds(f1), "matrix")
  expect_is(diffobj:::get_rds(f2), "matrix")

  diffPrint(mx1, f2)
  diffPrint(f1, f2)
  diffPrint(f1, mx2)
  diffPrint(f1, f2, rds=FALSE)
})
