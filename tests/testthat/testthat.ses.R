context("ses")

# Any tests added here should also be added to the valgrind test file

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
test_that("corner cases?", {
  expect_equal(ses(letters[1:4], letters[1:3]), "4d3")
  expect_equal(ses(letters[1:3], letters[1:4]), "3a4")
})
test_that("errors", {
  expect_error(ses('a', 'b', max.diffs='hello'), "must be scalar integer")
  expect_error(ses('a', 'b', warn='hello'), "must be TRUE or FALSE")

  a <- structure(1, class='diffobj_ogewlhgiadfl2')
  expect_error(ses(a, 1), "could not be coerced")
  expect_error(ses(1, a), "could not be coerced")
})

# We want to have a test file that fully covers the C code in order to run
# valgrind with just that one.  We were unable to isolate simple diffs that
# triggered all the code, but we were able to do it with the below in addition
# to the above.

test_that("Repeat tests for full coverage in SES file", {
  # From test.diffStr.R
  # formula display changed
  if(R.Version()$major >= 3 && R.Version()$minor >= "3.1") {
    rdsf1 <- function(x)
      file.path(getwd(), "helper", "diffStr", sprintf("%s.rds", x))
    expect_equal_to_reference(
      as.character(
        diffStr(mdl1, mdl2, extra=list(strict.width="wrap"), line.limit=30)
      ),
      rdsf1(500)
    )
  }
  # from testthat.warnings.R

  A3 <- c("a b c", "d e f A B C D", "g h i", "f")
  B3 <- c("a b c", "xd e f E Q L S", "g h i", "q")

  expect_warning(diffChr(A3, B3, max.diffs=2), "Exceeded diff")
})

test_that("ses_dat", {
  a <- b <- do.call(paste0, expand.grid(LETTERS, LETTERS))
  set.seed(2)
  b <- b[-sample(length(b), 100)]
  a <- a[-sample(length(b), 100)]

  dat <- ses_dat(a, b)
  expect_equal(dat[['val']][dat[['op']] != 'Delete'], b)
  expect_equal(dat[['val']][dat[['op']] != 'Insert'], a)
  expect_equal(a[dat[['id.a']][!is.na(dat[['id.a']])]], a)

  dat2 <- ses_dat(a, b, extra=FALSE)
  expect_equal(dat[1:2], dat2)
  expect_equal(length(dat2), 2L)

  expect_error(ses_dat(a, b, extra=NA), 'TRUE or FALSE')
})
test_that("encoding agnostic #144", {
  # h/t @hadley, these are different in string cache, but should compare equal
  # as per ?identical
  x <- c("fa\xE7ile", "fa\ue7ile")
  Encoding(x) <- c("latin1", "UTF-8")
  y <- rev(x)
  expect_equal(diffobj::ses(x, y), character())
})
