library(diffobj)

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
  expect_identical(
    diff_ses(
      c("a", "b", "c", "a", "b", "b", "a"), c("c", "b", "a", "b", "a", "c")
    ),
    c("1,2d0", "4d1", "5a3", "7a6")
  )
} )
