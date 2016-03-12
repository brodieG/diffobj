library(diffobj)

stop("Add tests for following strings described in https://neil.fraser.name/writing/diff/")
# Corner cases from https://neil.fraser.name/writing/diff/
# Both of these appear handled correctly by the algorithm here
# first one: suboptimal edit script due to two sided approach
A1 <- c("X", "A", "X", "C", "X", "A", "B", "C")
B1 <- c("A", "B", "C", "Y")
diff_chr(A1, B1)

# second one: failure to find intersection at ends of paths (paths run into
# each other eventually)

A2 <- c("A", "B", "X", "A", "B")
B2 <- c("A", "Y", "B")
diff_chr(A2, B2)

set.seed(1)
X <- do.call(paste0, expand.grid(LETTERS, LETTERS, LETTERS))

diff_chr(X[1:2000], X[2001:4000])
diff_chr(X[1:5000], X[5001:10000])

# Max limit warnings work properly; these are not fully fleshed out

A3 <- c("a b c", "d e f A B C D", "g h i", "f")
B3 <- c("a b c", "xd e f E Q L S", "g h i", "q")

expect_warning(diff_chr(A3, B3, max.diffs=2), "max.diffs")
expect_warning(diff_chr(A3, B3, max.diffs.in.hunk=2), "max.diffs.in")
expect_warning(diff_print(A3, B3, max.diffs.wrap=2), "max.diffs.wrap")

local({
  # The Myers paper strings

  A <- c("a", "b", "c", "a", "b", "b", "a")
  B <- c("c", "b", "a", "b", "a", "c")

  test_that("diff myers simple", {
    expect_identical(
      diffobj:::char_diff_myers_simple(character(), character()),
      list(target = integer(0), current = integer(0))
    )
    expect_identical(
      diffobj:::char_diff_myers_simple("a", character()),
      list(target = NA_integer_, current = integer(0))
    )
    expect_identical(
      diffobj:::char_diff_myers_simple(character(), "a"),
      list(target = integer(0), current = NA_integer_)
    )
    expect_identical(
      diffobj:::char_diff_myers_simple("a", "a"), list(target = 0L, current = 0L)
    )
    expect_identical(
      diffobj:::char_diff_myers_simple("a", "b"),
      list(target = 1L, current = 1L)
    )
    expect_identical(
      diffobj:::char_diff_myers_simple(c("a", "b"), "b"),
      list(target = c(NA, 0L), current = 0L)
    )
    expect_identical(
      diffobj:::char_diff_myers_simple(c("a", "b"), "a"),
      list(target = c(0L, NA), current = 0L)
    )
    expect_identical(
      diffobj:::char_diff_myers_simple("a", c("a", "b")),
      list(target = 0L, current = c(0L, NA))
    )
    expect_identical(
      diffobj:::char_diff_myers_simple("b", c("a", "b")),
      list(target = 0L, current = c(NA, 0L))
    )
    expect_identical(
      diffobj:::char_diff_myers_simple(c("a", "b"), c("b", "c")),
      list(target = c(NA, 0L), current = c(0L, NA))
    )
    expect_identical(
      diffobj:::char_diff_myers_simple(c("a", "b", "c", "d"), c("a", "c", "d", "b")),
      list(target = c(0L, NA, 0L, 0L), current = c(0L, 0L,  0L, NA))
    )
    # Actual Myers sample string
    expect_identical(
      diffobj:::char_diff_myers_simple(A, B),
      list(target = c(NA, NA, 0L, 0L, 0L, NA, 0L), current = c(0L,  NA, 0L, 0L, 0L, NA))
    )
  } )
  test_that("diff myers mba", {
    expect_identical(diff_ses(character(), character()), character())
    expect_identical(diff_ses("a", character()), "1d0")
    expect_identical(diff_ses(character(), "a"), "0a1")
    expect_identical(diff_ses("a", "a"), character())
    expect_identical(diff_ses("a", "b"), "1c1")
    expect_identical(diff_ses(c("a", "b"), "b"), "1d0")
    expect_identical(diff_ses(c("a", "b"), "a"), "2d1")
    expect_identical(diff_ses("a", c("a", "b")), "1a2")
    expect_identical(diff_ses("b", c("a", "b")), "0a1")
    expect_identical(diff_ses(c("a", "b"), c("b", "c")), c("1d0", "2a2"))
    expect_identical(
      diff_ses(c("a", "b", "c", "d"), c("a", "c", "d", "b")), c("2d1", "4a4")
    )
    # Actual Myers sample string
    expect_identical(diff_ses(A, B), c("1,2d0", "4d1", "5a3", "7a6"))
    # This used to cause errors due to under-allocated buffer vector
    expect_identical(diff_ses(letters[1:10], LETTERS[1:2]), "1,10c1,2")

    # A little more complex with changes, this was a problem at some point

    A2 <- c("A", "B", "C")
    B2 <- c("X", "A", "Y", "C")
    expect_identical(diff_ses(A2, B2), c("0a1", "2c3"))

    # More complicated strings; intended for use with contexts for hunks,
    # but making sure the diffs are correct

    A1 <- c("A", "AA", "B", "C", "D", "E", "F", "G", "H")
    B1 <- c("A", "B", "X", "W", "D", "DD", "E", "Y", "Z")
    C1 <- c("X", "D", "E", "Y", "Z", "H")

    expect_identical(diff_ses(A1, B1), c("2d1", "4c3,4", "5a6", "7,9c8,9"))
    expect_identical(diff_ses(A1, C1), c("1,4c1", "7,8c4,5"))

    A5 <- c("A", "AA", "B", "C", "D", "E", "F", "G", "H")
    B5 <- c("A", "B",  "X", "W", "D", "E", "F", "W", "G")
    expect_identical(diff_ses(A5, B5), c("2d1", "4c3,4", "7a8", "9d9"))

  } )
  test_that("print/summary", {
    expect_identical(
      summary(diffobj:::diff_myers_mba(A, B), with.match=TRUE),
      structure(list(type = structure(c(3L, 1L, 3L, 1L, 2L, 1L, 2L), .Label = c("Match", "Insert", "Delete"), class = "factor"), string = structure(c(2L, 5L, 1L, 3L, 1L, 4L, 5L), .Label = c("a", "ab", "b", "ba", "c"), class = "factor"), len = c(2L, 1L, 1L, 1L, 1L, 2L, 1L), offset = c(1L, 3L, 4L, 5L, 3L, 6L, 6L)), .Names = c("type", "string", "len", "offset"), row.names = c(NA, -7L), class = "data.frame")
    )
    expect_identical(
      summary(diffobj:::diff_myers_mba(A, B), with.match=FALSE),
      structure(list(type = structure(c(3L, 1L, 3L, 1L, 2L, 1L, 2L), .Label = c("Match", "Insert", "Delete"), class = "factor"), len = c(2L, 1L, 1L, 1L, 1L, 2L, 1L), offset = c(1L, 3L, 4L, 5L, 3L, 6L, 6L)), .Names = c("type", "len", "offset"), row.names = c(NA, -7L), class = "data.frame")
    )
    expect_identical(
      capture.output(print(diffobj:::diff_myers_mba(A, B))), diff_ses(A, B)
    )
  })
  #  test_that("translate", {
  #    aa <- c("a", "b", "b", "c", "e")
  #    bb <- c("x", "y", "c", "f", "e")
  #    expect_identical(
  #      diffobj:::diffObjCompact(diffobj:::diff_myers_mba(A, B)),
  #      list(target = c(NA, NA, 0L, NA, 0L, 0L, 0L), current = c(0L, 0L, NA, 0L, 0L, NA))
  #    )
  #    expect_identical(
  #      diffobj:::diffObjCompact(diffobj:::diff_myers_mba(aa, bb)),
  #      list(target = c(1L, 2L, NA, 0L, 0L), current = c(1L, 2L, 0L, NA, 0L))
  #    )
  #
  #  } )
})
