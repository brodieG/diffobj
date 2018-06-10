context("ses")

# These tests are intended to be run under valgrind so we can make sure there
# are no compiled code issues.  It's basically impossible to run the full test
# suite under valgrind because there are lots of false positives from the PCRE
# library.

test_that("basic", {
  expect_equal(ses(letters[1:10], letters[1:10]), character())
  expect_equal(ses(letters[1:10], LETTERS[1:10]), "1,10c1,10")
  expect_equal(ses(letters[1:5], LETTERS[1:10]), "1,5c1,10")
  expect_equal(ses(letters[1:10], LETTERS[1:5]), "1,10c1,5")
  expect_equal(ses(letters[2:10], letters[1:7]), c("0a1", "7,9d7"))
  expect_equal(
    ses(letters[c(1:5, 1:5, 1:5)], c("e", "d", "a", "b", "c")),
    c("1,4d0", "6,8d1", "10d2", "14,15d5")
  )
  expect_equal(
    ses(c("e", "d", "a", "b", "c"), letters[c(1:5, 1:5, 1:5)]),
    c("0a1,4", "1a6,8", "2a10", "5a14,15")
  )
  # edit distance = 1


})
test_that("trigger edit distance 1 branches", {
  expect_equal(ses("a", c("a", "b")), "1a2")
  expect_equal(ses(c("a", "b"), "a"), "2d1")
  expect_equal(ses("c", c("b", "c")), "0a1")
  expect_equal(ses(c("b", "c"), "c"), "1d0")

  expect_equal(ses("a", character()), "1d0")
  expect_equal(ses(character(), "a"), "0a1")
  expect_equal(ses(character(), character()), character())

  ## this is from the atomic tests, haven't dug into why they actually trigger
  ## the desired branches, but it is fairly complex
  set.seed(2)
  w1 <- sample(
    c(
    "carrot", "cat", "cake", "eat", "rabbit", "holes", "the", "a", "pasta",
    "boom", "noon", "sky", "hat", "blah", "paris", "dog", "snake"
    ), 25, replace=TRUE
  )
  w4 <- w3 <- w2 <- w1
  w2[sample(seq_along(w1), 5)] <- LETTERS[1:5]
  w3 <- w1[8:15]
  w4 <- c(w1[1:5], toupper(w1[1:5]), w1[6:15], toupper(w1[1:5]))

  expect_equal(ses(w1, w4), c("5a6,10", "15,21d19", "23,25c21,25"))

})
test_that("longer strings", {

  # A bigger string

  string <- do.call(paste0, expand.grid(LETTERS, LETTERS, LETTERS))

  expect_equal(
    ses(string, c("hello", string[-c(5, 500, 1000)], "goodbye")),
    c("0a1", "5d5", "500d499", "1000d998", "17576a17575")
  )
  expect_equal(
    ses(c(string[200:500], "hello", string[-(1:400)][-c(5, 500, 1000)]), string),
    c("0a1,199", "207,306d405", "800a900", "1299a1400")
  )
})
test_that("max diffs", {
  expect_warning(
    ses(letters[1:10], LETTERS[1:10], max.diffs=5),
    "Exceeded `max.diffs`"
  )
  expect_equal(
    ses(letters[1:10], LETTERS[1:10], max.diffs=5, warn=FALSE),
    "1,10c1,10"
  )
  expect_equal(
    ses(
      letters[1:10],
      c(letters[1], LETTERS[2:5], letters[6:10]), max.diffs=5, warn=FALSE
    ),
    "2,5c2,5"
  )
  expect_equal(
    ses(
      letters[1:10],
      c(letters[1], LETTERS[2:5], letters[6:8], LETTERS[9], letters[10]),
      max.diffs=5, warn=FALSE
    ),
    c("2,5c2,5", "9c9")
  )
})
