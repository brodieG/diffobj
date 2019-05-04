library(diffobj)

context("diffFile")

if(!identical(basename(getwd()), "testthat"))
  stop("Working dir does not appear to be /testthat, is ", getwd())

rdsf <- function(x)
  file.path(getwd(), "helper", "diffFile", sprintf("%s.rds", x))

test_that("Code File", {
  # # compare two crayon file versions
  # # These should eventually just be downloaded and made into diffFile tests

  f.p.1 <- file.path("helper", "diffFile", "s.o.3f1f68.R")
  f.p.2 <- file.path("helper", "diffFile", "s.o.30dbe0.R")

  # url.1 <- "https://raw.githubusercontent.com/gaborcsardi/crayon/3f1f68ab177b82a27e754a58264af801f7194820/R/string_operations.r"
  # url.2 <- "https://raw.githubusercontent.com/gaborcsardi/crayon/30dbe0d4d92157350af3cb3aeebd6d9a9cdf5c0e/R/string_operations.r"
  # f.1 <- readLines(url.1)
  # f.2 <- readLines(url.2)
  # writeLines(f.1, f.p.1)
  # writeLines(f.2, f.p.2)

  expect_equal_to_reference(as.character(diffFile(f.p.1, f.p.2)), rdsf(100))
})
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

  ref <- as.character(diffPrint(mx1, mx2))
  expect_identical(as.character(diffPrint(mx1, f2, cur.banner="mx2")), ref)
  expect_identical(as.character(diffPrint(f1, mx2, tar.banner="mx1")), ref)
  expect_identical(
    as.character(diffPrint(f1, f2, tar.banner="mx1", cur.banner="mx2")), ref
  )
  expect_true(!identical(as.character(diffPrint(mx1, f2, rds=FALSE)), ref))
})

test_that("file", {
  f1 <- tempfile()
  f2 <- tempfile()
  on.exit(unlink(c(f1, f2)))
  letters2 <- letters
  letters2[15] <- "HELLO"

  writeLines(letters, f1)
  writeLines(letters2, f2)

  expect_identical(
    as.character(diffChr(letters, letters2, tar.banner="f1", cur.banner="f2")),
    as.character(diffFile(f1, f2))
  )

  # issue 133 h/t Noam Ross, thanks for the test

  library(diffobj)
  x <- tempfile()
  y <- tempfile()
  on.exit(unlink(c(x, y)))
  cat("Hello\nthere\n", file = x)
  file.copy(x, y)
  expect_identical(
    as.character(diffFile(x, y, format = "raw")),
    structure(
      c("No visible differences between objects.", 
        "< x          > y        ", 
        "@@ 1,2 @@    @@ 1,2 @@  ", 
        "  Hello        Hello    ", 
        "  there        there    "), len = 5L)
  )
})

test_that("CSV", {
  f1 <- tempfile()
  f2 <- tempfile()
  on.exit(unlink(c(f1, f2)))

  iris2 <- iris
  iris2$Sepal.Length[25] <- 9.9

  write.csv(iris, f1, row.names=FALSE)
  write.csv(iris2, f2, row.names=FALSE)

  expect_identical(
    as.character(diffPrint(iris, iris2, tar.banner="f1", cur.banner="f2")),
    as.character(diffCsv(f1, f2))
  )
})
