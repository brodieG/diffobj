library(diffobj)

context("Limits / Subset")

test_that("subset", {
  old.opt <- options(diffobj.format="raw", disp.width=80L)
  on.exit(options(old.opt))
  A <- B <- letters[1:5]
  B[2] <- "B"
  B[6] <- "F"
  expect_equal(
    c(as.character(diffChr(A, B)[1:3])),
    c("< A                 > B                ", "@@ 1,5 @@           @@ 1,6 @@          ", "  a                   a                ")
  )
  expect_equal(
    c(as.character(diffChr(A, B)[1])), c(as.character(head(diffChr(A, B), 1)))
  )
  expect_equal(
    c(as.character(diffChr(A, B)[7:8])), c(as.character(tail(diffChr(A, B), 2)))
  )
  diffChr(A, B, line.limit=2, context=1L)
  diffChr(A, B, line.limit=3, context=1L)
  diffChr(A, B)
})

# ## Old tests that need to be formalized
#
# Puromycin2 <- Puromycin
# set.seed(1)
# Puromycin2$conc[c(8, 15:19, 22)] <- round(runif(7), 2)
# Puromycin2$state[17] <- "treated"
# diffPrint(Puromycin, Puromycin2, line.limit=15)
# diffPrint(Puromycin, Puromycin2, line.limit=15, mode="sidebyside")
# diffPrint(Puromycin, Puromycin2, line.limit=15, mode="context")
#
# # line limit issues
# diffPrint(Puromycin, Puromycin2, line.limit=6)
# diffPrint(Puromycin, Puromycin2, line.limit=6, mode="sidebyside")
# diffPrint(Puromycin, Puromycin2, line.limit=6, mode="context")
#
# diffPrint(Puromycin, Puromycin2, line.limit=3)
# diffPrint(Puromycin, Puromycin2, line.limit=3)
# diffPrint(Puromycin, Puromycin2, line.limit=4)
#
# Puromycin3 <- Puromycin2
# names(Puromycin3)[3L] <- "blargh"
# diffPrint(Puromycin, Puromycin3, line.limit=6, mode="context")
#
