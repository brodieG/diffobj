library(diffobj)
context("core")

local({
  # The Myers paper strings

  A <- c("a", "b", "c", "a", "b", "b", "a")
  B <- c("c", "b", "a", "b", "a", "c")

  test_that("diff myers simple", {
    expect_identical(
      diffobj:::myers_simple(character(), character()),
      list(target = integer(0), current = integer(0))
    )
    expect_identical(
      diffobj:::myers_simple("a", character()),
      list(target = NA_integer_, current = integer(0))
    )
    expect_identical(
      diffobj:::myers_simple(character(), "a"),
      list(target = integer(0), current = NA_integer_)
    )
    expect_identical(
      diffobj:::myers_simple("a", "a"), list(target = 0L, current = 0L)
    )
    expect_identical(
      diffobj:::myers_simple("a", "b"),
      list(target = 1L, current = 1L)
    )
    expect_identical(
      diffobj:::myers_simple(c("a", "b"), "b"),
      list(target = c(NA, 0L), current = 0L)
    )
    expect_identical(
      diffobj:::myers_simple(c("a", "b"), "a"),
      list(target = c(0L, NA), current = 0L)
    )
    expect_identical(
      diffobj:::myers_simple("a", c("a", "b")),
      list(target = 0L, current = c(0L, NA))
    )
    expect_identical(
      diffobj:::myers_simple("b", c("a", "b")),
      list(target = 0L, current = c(NA, 0L))
    )
    expect_identical(
      diffobj:::myers_simple(c("a", "b"), c("b", "c")),
      list(target = c(NA, 0L), current = c(0L, NA))
    )
    expect_identical(
      diffobj:::myers_simple(c("a", "b", "c", "d"), c("a", "c", "d", "b")),
      list(target = c(0L, NA, 0L, 0L), current = c(0L, 0L,  0L, NA))
    )
    # Actual Myers sample string
    expect_identical(
      diffobj:::myers_simple(A, B),
      list(target = c(NA, NA, 0L, 0L, 0L, NA, 0L), current = c(0L,  NA, 0L, 0L, 0L, NA))
    )
  } )
  test_that("diff myers mba", {
    expect_identical(ses(character(), character()), character())
    expect_identical(ses("a", character()), "1d0")
    expect_identical(ses(character(), "a"), "0a1")
    expect_identical(ses("a", "a"), character())
    expect_identical(ses("a", "b"), "1c1")
    expect_identical(ses(c("a", "b"), "b"), "1d0")
    expect_identical(ses(c("a", "b"), "a"), "2d1")
    expect_identical(ses("a", c("a", "b")), "1a2")
    expect_identical(ses("b", c("a", "b")), "0a1")
    expect_identical(ses(c("a", "b"), c("b", "c")), c("1d0", "2a2"))
    expect_identical(
      ses(c("a", "b", "c", "d"), c("a", "c", "d", "b")), c("2d1", "4a4")
    )
    # Actual Myers sample string
    expect_identical(ses(A, B), c("1,2d0", "4d1", "5a3", "7a6"))
    # This used to cause errors due to under-allocated buffer vector
    expect_identical(ses(letters[1:10], LETTERS[1:2]), "1,10c1,2")

    # A little more complex with changes, this was a problem at some point

    A2 <- c("A", "B", "C")
    B2 <- c("X", "A", "Y", "C")
    expect_identical(ses(A2, B2), c("0a1", "2c3"))

    # More complicated strings; intended for use with contexts for hunks,
    # but making sure the diffs are correct

    A1 <- c("A", "AA", "B", "C", "D", "E", "F", "G", "H")
    B1 <- c("A", "B", "X", "W", "D", "DD", "E", "Y", "Z")
    C1 <- c("X", "D", "E", "Y", "Z", "H")

    expect_identical(ses(A1, B1), c("2d1", "4c3,4", "5a6", "7,9c8,9"))
    expect_identical(ses(A1, C1), c("1,4c1", "7,8c4,5"))

    A5 <- c("A", "AA", "B", "C", "D", "E", "F", "G", "H")
    B5 <- c("A", "B",  "X", "W", "D", "E", "F", "W", "G")
    expect_identical(ses(A5, B5), c("2d1", "4c3,4", "7a8", "9d9"))

  } )
  test_that("print/summary", {
    capture.output(
      res.1 <- summary(diffobj:::diff_myers(A, B), with.match=TRUE)
    )
    expect_identical(
      res.1,
      structure(list(type = structure(c(3L, 1L, 3L, 1L, 2L, 1L, 2L), .Label = c("Match", "Insert", "Delete"), class = "factor"), string = structure(c(2L, 5L, 1L, 3L, 1L, 4L, 5L), .Label = c("a", "ab", "b", "ba", "c"), class = "factor"), len = c(2L, 1L, 1L, 1L, 1L, 2L, 1L), offset = c(1L, 3L, 4L, 5L, 3L, 6L, 6L)), .Names = c("type", "string", "len", "offset"), row.names = c(NA, -7L), class = "data.frame")
    )
    capture.output(
      res.2 <- summary(diffobj:::diff_myers(A, B), with.match=FALSE)
    )
    expect_identical(
      res.2,
      structure(list(type = structure(c(3L, 1L, 3L, 1L, 2L, 1L, 2L), .Label = c("Match", "Insert", "Delete"), class = "factor"), len = c(2L, 1L, 1L, 1L, 1L, 2L, 1L), offset = c(1L, 3L, 4L, 5L, 3L, 6L, 6L)), .Names = c("type", "len", "offset"), row.names = c(NA, -7L), class = "data.frame")
    )
    expect_identical(
      capture.output(print(diffobj:::diff_myers(A, B))), ses(A, B)
    )
  })
  #  test_that("translate", {
  #    aa <- c("a", "b", "b", "c", "e")
  #    bb <- c("x", "y", "c", "f", "e")
  #    expect_identical(
  #      diffobj:::diffObjCompact(diffobj:::diff_myers(A, B)),
  #      list(target = c(NA, NA, 0L, NA, 0L, 0L, 0L), current = c(0L, 0L, NA, 0L, 0L, NA))
  #    )
  #    expect_identical(
  #      diffobj:::diffObjCompact(diffobj:::diff_myers(aa, bb)),
  #      list(target = c(1L, 2L, NA, 0L, 0L), current = c(1L, 2L, 0L, NA, 0L))
  #    )
  #
  #  } )
})
