NAME <- "s4"
source(file.path('_helper', 'init.R'))

# - diff data validation works
#
# These are not currently in use
# expect_match(diffobj:::valid_dat("hello"), "should be a list")
# D0 <- D1 <- D2 <- D3 <- D4 <- D5 <- D6 <- D7 <-
#   diffPrint(letters, LETTERS)@tar.dat

# expect_match(diffobj:::valid_dat(unname(D0)), "should have names")

# length(D1[[1L]]) <- 1L
# expect_match(diffobj:::valid_dat(D1), "should have equal length")

# D2$orig <- integer(length(D2$orig))
# expect_match(diffobj:::valid_dat(D2), "should be character")

# D3$trim.ind.start <- character(length(D3$trim.ind.start))
# expect_match(diffobj:::valid_dat(D3), "should be integer")

# D4$word.ind <- integer(length(D4$word.ind))
# expect_match(diffobj:::valid_dat(D4), "should be list")

# D5$word.ind <- vector("list", length(D5$word.ind))
# expect_match(diffobj:::valid_dat(D5), "not in expected format")

# D6$tok.rat <- D6$tok.rat + 2
# expect_match(diffobj:::valid_dat(D6), "with all values between")

# D7$fill <- integer(length(D7$fill))
# expect_match(diffobj:::valid_dat(D7), "should be logical")

# - any ------------------------------------------------------------------------

isTRUE(any(diffChr('a', 'b')))
identical(any(diffChr('a', 'a')), FALSE)
try(any(diffChr('a', 'a'), 2)) # "supports only one argument"
