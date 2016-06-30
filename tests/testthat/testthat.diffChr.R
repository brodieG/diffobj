context("diffChr")

if(!identical(basename(getwd()), "testthat"))
  stop("Working dir does not appear to be /testthat, is ", getwd())

rdsf <- function(x)
  file.path(getwd(), "helper", "diffChr", sprintf("%s.rds", x))

test_that("Corner Cases", {
  # Corner cases from https://neil.fraser.name/writing/diff/
  # Both of these appear handled correctly by the algorithm here
  # first one: suboptimal edit script due to two sided approach

  A1 <- c("X", "A", "X", "C", "X", "A", "B", "C")
  B1 <- c("A", "B", "C", "Y")
  expect_equal_to_reference(as.character(diffChr(A1, B1)), rdsf(100))

  # second one: failure to find intersection at ends of paths (paths run into
  # each other eventually)

  A2 <- c("A", "B", "X", "A", "B")
  B2 <- c("A", "Y", "B")
  expect_equal_to_reference(as.character(diffChr(A2, B2)), rdsf(200))

  # Simple corner cases

  expect_equal_to_reference(
    as.character(diffChr(character(), character())), rdsf(225)
  )
  expect_equal_to_reference(as.character(diffChr("", "")), rdsf(250))
})
test_that("Larger strings", {
  X <- do.call(paste0, expand.grid(LETTERS, LETTERS, LETTERS, LETTERS))
  # diffChr(X[1:2000], X[2001:4000])

  set.seed(1)
  n <- 500
  A3 <- B3 <- X[1:n]
  A3 <- A3[
    -unlist(
      replicate(25, seq(from=sample(n, 1), by=1L, length.out=sample(10, 1)))
    )
  ]
  B3 <- B3[
    -unlist(
      replicate(25, seq(from=sample(n, 1), by=1L, length.out=sample(10, 1)))
    )
  ]
  expect_equal_to_reference(as.character(diffChr(A3, B3)), rdsf(300))

  # Too slow to run; useful for benchmarking though

  # res <- diffChr(X[1:10000], X[7500:17500])
  # res <- ses(X[1:10000], X[7500:17500])
  # res <- diffChr(X[1:25000], X[10001:50000], max.diffs=65000)
})
test_that("Sentences", {
  chr.5 <- c(
    "hello there how are you doing",
    "humpty dumpty took a big fall",
    "lorem ipsum dolor sic est boom",
    "a computer once wrote a phrase"
  )
  chr.6 <- c(
    "hello THERE how are you doing",
    "and another SENTENCE blah blah",
    "humpty dumpty TOOK a big fall",
    "a COMPUTER once wrote a phrase"
  )
  expect_equal_to_reference(as.character(diffChr(chr.5, chr.6)), rdsf(400))
  expect_equal_to_reference(
    as.character(diffChr(chr.5, chr.6, mode="unified")), rdsf(500)
  )
  expect_equal_to_reference(
    as.character(diffChr(chr.5, chr.6, mode="context")), rdsf(600)
  )
})

# # compare two crayon file versions
# # These should eventually just be downloaded and made into diffFile tests
#
# url.1 <- "https://raw.githubusercontent.com/gaborcsardi/crayon/3f1f68ab177b82a27e754a58264af801f7194820/R/string_operations.r"
# url.2 <- "https://raw.githubusercontent.com/gaborcsardi/crayon/30dbe0d4d92157350af3cb3aeebd6d9a9cdf5c0e/R/string_operations.r"
# f.1 <- readLines(url.1)
# f.2 <- readLines(url.2)
# diffChr(f.1, f.2)
# diffChr(f.1, f.2, mode="s")
#


