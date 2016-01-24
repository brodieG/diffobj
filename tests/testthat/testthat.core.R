library(diffobj)

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
      diffobj:::char_diff_myers_simple(
        c("a", "b", "c", "a", "b", "b", "a"),
        c("c", "b", "a", "b", "a", "c")
      ),
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
  test_that("translate", {
    aa <- c("a", "b", "b", "c", "e")
    bb <- c("x", "y", "c", "f", "e")
    expect_identical(
      diffobj:::diffObjCompact(diffobj:::diff_myers_mba(A, B)),
      list(target = c(NA, NA, 0L, NA, 0L, 0L, 0L), current = c(0L, 0L, NA, 0L, 0L, NA))
    )
    expect_identical(
      diffobj:::diffObjCompact(diffobj:::diff_myers_mba(aa, bb)),
      list(target = c(1L, 2L, NA, 0L, 0L), current = c(1L, 2L, 0L, NA, 0L))
    )

  } )
})
