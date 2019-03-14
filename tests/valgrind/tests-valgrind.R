# These tests are intended to be run under valgrind so we can make sure there
# are no compiled code issues.  It's basically impossible to run the full test
# suite under valgrind because there are lots of false positives from the PCRE
# library.
#
# Orinally these were the ses tests, but even the testthat overhead caused too
# many issues so we're just running the code without checking results.

writeLines("basic")

    # expect_equal(ses(letters[1:10], letters[1:10]), character())
    ses(letters[1:10], letters[1:10])
    # expect_equal(ses(letters[1:10], LETTERS[1:10]), "1,10c1,10")
    ses(letters[1:10], LETTERS[1:10])
    # expect_equal(ses(letters[1:5], LETTERS[1:10]), "1,5c1,10")
    ses(letters[1:5], LETTERS[1:10])
    # expect_equal(ses(letters[1:10], LETTERS[1:5]), "1,10c1,5")
    ses(letters[1:10], LETTERS[1:5])
    # expect_equal(ses(letters[2:10], letters[1:7]), c("0a1", "7,9d7"))
    ses(letters[2:10], letters[1:7])
    # expect_equal(ses(letters[c(1:5, 1:5, 1:5)], c("e", "d", "a",
    #     "b", "c")), c("1,4d0", "6,8d1", "10d2", "14,15d5"))
    ses(letters[c(1:5, 1:5, 1:5)], c("e", "d", "a", "b", "c"))
    # expect_equal(ses(c("e", "d", "a", "b", "c"), letters[c(1:5, 1:5,
    #     1:5)]), c("0a1,4", "1a6,8", "2a10", "5a14,15"))
    ses(c("e", "d", "a", "b", "c"), letters[c(1:5, 1:5, 1:5)])

writeLines("trigger edit distance 1 branches")

    # expect_equal(ses("a", c("a", "b")), "1a2")
    ses("a", c("a", "b"))
    # expect_equal(ses(c("a", "b"), "a"), "2d1")
    ses(c("a", "b"), "a")
    # expect_equal(ses("c", c("b", "c")), "0a1")
    ses("c", c("b", "c"))
    # expect_equal(ses(c("b", "c"), "c"), "1d0")
    ses(c("b", "c"), "c")
    # expect_equal(ses("a", character()), "1d0")
    ses("a", character())
    # expect_equal(ses(character(), "a"), "0a1")
    ses(character(), "a")
    # expect_equal(ses(character(), character()), character())
    ses(character(), character())
    ## this is from the atomic tests, haven't dug into why they actually trigger
    ## the desired branches, but it is fairly complex
    set.seed(2)
    w1 <- sample(c("carrot", "cat", "cake", "eat", "rabbit", "holes",
        "the", "a", "pasta", "boom", "noon", "sky", "hat", "blah",
        "paris", "dog", "snake"), 25, replace = TRUE)
    w4 <- w3 <- w2 <- w1
    w2[sample(seq_along(w1), 5)] <- LETTERS[1:5]
    w3 <- w1[8:15]
    w4 <- c(w1[1:5], toupper(w1[1:5]), w1[6:15], toupper(w1[1:5]))
    # expect_equal(ses(w1, w4), c("5a6,10", "15,21d19", "23,25c21,25"))
    ses(w1, w4)

writeLines("longer strings")

    # A bigger string
    string <- do.call(paste0, expand.grid(LETTERS, LETTERS, LETTERS))
    # expect_equal(ses(string, c("hello", string[-c(5, 500, 1000)],
    #     "goodbye")), c("0a1", "5d5", "500d499", "1000d998", "17576a17575"))
    ses(string, c("hello", string[-c(5, 500, 1000)], "goodbye"))
    # expect_equal(ses(c(string[200:500], "hello", string[-(1:400)][-c(5,
    #     500, 1000)]), string), c("0a1,199", "207,306d405", "800a900",
    #     "1299a1400"))
    ses(c(string[200:500], "hello", string[-(1:400)][-c(5, 500, 1000)]),
        string)

writeLines("max diffs")
    # expect_warning(ses(letters[1:10], LETTERS[1:10], max.diffs = 5),
    #     "Exceeded `max.diffs`")
    suppressWarnings(ses(letters[1:10], LETTERS[1:10], max.diffs = 5))
    # expect_equal(ses(letters[1:10], LETTERS[1:10], max.diffs = 5,
    #     warn = FALSE), "1,10c1,10")
    ses(letters[1:10], LETTERS[1:10], max.diffs = 5, warn = FALSE)
    # expect_equal(ses(letters[1:10], c(letters[1], LETTERS[2:5], letters[6:10]),
    #     max.diffs = 5, warn = FALSE), "2,5c2,5")
    ses(letters[1:10], c(letters[1], LETTERS[2:5], letters[6:10]),
        max.diffs = 5, warn = FALSE)
    # expect_equal(ses(letters[1:10], c(letters[1], LETTERS[2:5], letters[6:8],
    #     LETTERS[9], letters[10]), max.diffs = 5, warn = FALSE), c("2,5c2,5",
    #     "9c9"))
    ses(letters[1:10], c(letters[1], LETTERS[2:5], letters[6:8],
        LETTERS[9], letters[10]), max.diffs = 5, warn = FALSE)

writeLines("corner cases?")
    # expect_equal(ses(letters[1:4], letters[1:3]), "4d3")
    ses(letters[1:4], letters[1:3])
    # expect_equal(ses(letters[1:3], letters[1:4]), "3a4")
    ses(letters[1:3], letters[1:4])
    #

writeLines("errors")
    # expect_error(ses("a", "b", max.diffs = "hello"), "must be scalar integer")
    try(ses("a", "b", max.diffs = "hello"), silent=TRUE)
    # expect_error(ses("a", "b", warn = "hello"), "must be TRUE or FALSE")
    try(ses("a", "b", warn = "hello"), silent=TRUE)

# We want to have a test file that fully covers the C code in order to run
# valgrind with just that one.  We were unable to isolate simple diffs that
# triggered all the code, but we were able to do it with the below in addition
# to the above.
# test_that("Repeat tests for full coverage in SES file", {
  #
    # From test.diffStr.R
    # formula display changed
writeLines("model prep")
    frm1 <- as.formula("Sepal.Length ~ Sepal.Width", env=.GlobalEnv)
    frm2 <- as.formula("Sepal.Length ~ Sepal.Width + Species", env=.GlobalEnv)
    mdl1 <- lm(frm1, iris)
    mdl2 <- lm(frm2, iris)


writeLines("diff str")
    # as.character(
    #   diffStr(mdl1, mdl2,
    #     extra = list(strict.width = "wrap"), line.limit = 30)
    # )
    ## we captured the text being diffed above at the actual level, and
    ## also at the highest level
    ses(
      readLines('valgrind/mdl-tar.txt'), readLines('valgrind/mdl-cur.txt')
    )
    ses(
      readLines('valgrind/mdl-tar-all.txt'),
      readLines('valgrind/mdl-cur-all.txt')
    )
    # from testthat.warnings.R
writeLines("exceeded diff")
    A3 <- c("a b c", "d e f A B C D", "g h i", "f")
    B3 <- c("a b c", "xd e f E Q L S", "g h i", "q")
    suppressWarnings(ses(A3, B3, max.diffs = 2))

writeLines("done")
