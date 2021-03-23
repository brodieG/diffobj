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
    ses(1, 2:9, max.diffs = 8)

    # h/t @gadenbui, data is extracted from palmerpenguins@0.1.0::penguins
    #
    # comparison <- subset(penguins, year == 2007 | flipper_length_mm > 220)
    # test <- subset(penguins, year == 2008)
    # a <- test$bill_length_mm
    # b <- comparison$bill_length_mm

    a <- c(39.6, 40.1, 35, 42, 34.5, 41.4, 39, 40.6, 36.5, 37.6, 35.7, 
    41.3, 37.6, 41.1, 36.4, 41.6, 35.5, 41.1, 35.9, 41.8, 33.5, 39.7, 
    39.6, 45.8, 35.5, 42.8, 40.9, 37.2, 36.2, 42.1, 34.6, 42.9, 36.7, 
    35.1, 37.3, 41.3, 36.3, 36.9, 38.3, 38.9, 35.7, 41.1, 34, 39.6, 
    36.2, 40.8, 38.1, 40.3, 33.1, 43.2, 49.1, 48.4, 42.6, 44.4, 44, 
    48.7, 42.7, 49.6, 45.3, 49.6, 50.5, 43.6, 45.5, 50.5, 44.9, 45.2, 
    46.6, 48.5, 45.1, 50.1, 46.5, 45, 43.8, 45.5, 43.2, 50.4, 45.3, 
    46.2, 45.7, 54.3, 45.8, 49.8, 46.2, 49.5, 43.5, 50.7, 47.7, 46.4, 
    48.2, 46.5, 46.4, 48.6, 47.5, 51.1, 45.2, 45.2, 50.5, 49.5, 46.4, 
    52.8, 40.9, 54.2, 42.5, 51, 49.7, 47.5, 47.6, 52, 46.9, 53.5, 
    49, 46.2, 50.9, 45.5)
    b <- c(39.1, 39.5, 40.3, NA, 36.7, 39.3, 38.9, 39.2, 34.1, 42, 37.8, 
    37.8, 41.1, 38.6, 34.6, 36.6, 38.7, 42.5, 34.4, 46, 37.8, 37.7, 
    35.9, 38.2, 38.8, 35.3, 40.6, 40.5, 37.9, 40.5, 39.5, 37.2, 39.5, 
    40.9, 36.4, 39.2, 38.8, 42.2, 37.6, 39.8, 36.5, 40.8, 36, 44.1, 
    37, 39.6, 41.1, 37.5, 36, 42.3, 46.1, 50, 48.7, 50, 47.6, 46.5, 
    45.4, 46.7, 43.3, 46.8, 40.9, 49, 45.5, 48.4, 45.8, 49.3, 42, 
    49.2, 46.2, 48.7, 50.2, 45.1, 46.5, 46.3, 42.9, 46.1, 44.5, 47.8, 
    48.2, 50, 47.3, 42.8, 45.1, 59.6, 49.6, 50.5, 50.5, 50.1, 50.4, 
    46.2, 54.3, 49.8, 49.5, 50.7, 46.4, 48.2, 48.6, 45.2, 52.5, 50, 
    50.8, 52.1, 52.2, 49.5, 50.8, 46.9, 51.1, 55.9, 49.1, 49.8, 51.5, 
    55.1, 48.8, 50.4, 46.5, 50, 51.3, 45.4, 52.7, 45.2, 46.1, 51.3, 
    46, 51.3, 46.6, 51.7, 47, 52, 45.9, 50.5, 50.3, 58, 46.4, 49.2, 
    42.4, 48.5, 43.2, 50.6, 46.7, 52)

    # In <0.3.4: Exceeded buffer for finding fake snake
    ses(a[-c(15:38, 50:90)], b[-c(40:85, 100:125)], max.diffs=80)

    # In <0.3.4: Faux Snake Process Failed
    ses(a[-(18:38)], b[-(50:80)], max.diffs=115)

    # issue 157
    # Arguably could match on 'A' instead of 'X' and be more comparct
    a <- c('a', 'b', 'c', 'A', 'X', 'Y', 'Z', 'W')
    b <- c('X', 'C', 'A', 'U', 1, 2, 3)
    ses(a, b, max.diffs=13)

    # segfault (but may have beend debugging code)
    ses(letters[1:2], LETTERS[1:2], max.diffs = 4)

    # snake overrun
    ses(c("G", "C", "T", "C", "A", "C", "G", "C"), c("T", "G"), max.diffs=2)

    # effect of max.diffs on compactness (waldo logical comparison)
    ses(c('A','A','A','A','A'), c('B','A','B','A','B'), max.diffs=0)
    ses(c('A','A','A','A','A'), c('B','A','B','A','B'), max.diffs=1)
    ses(c('A','A','A','A','A'), c('B','A','B','A','B'), max.diffs=2)

    # back snake all matches before faux snake triggered
    ses_dat(
      a=c("T", "A", "A", "C", "C", "A"),
      b=c("A", "G", "A", "A"), max.diffs = 0
    )

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
