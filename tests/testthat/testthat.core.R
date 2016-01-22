
test_that("diff myers simple", {
  expect_identical(
    diffr:::char_diff_myers_simple(character(), character()),
    list(target = integer(0), current = integer(0))
  )
  expect_identical(
    diffr:::char_diff_myers_simple("a", character()),
    list(target = NA_integer_, current = integer(0))
  )
  expect_identical(
    diffr:::char_diff_myers_simple(character(), "a"),
    list(target = integer(0), current = NA_integer_)
  )
  expect_identical(
    diffr:::char_diff_myers_simple("a", "a"), list(target = 0L, current = 0L)
  )
  expect_identical(
    diffr:::char_diff_myers_simple("a", "b"),
    list(target = 1L, current = 1L)
  )
  expect_identical(
    diffr:::char_diff_myers_simple(c("a", "b"), "b"),
    list(target = c(NA, 0L), current = 0L)
  )
  expect_identical(
    diffr:::char_diff_myers_simple(c("a", "b"), "a"),
    list(target = c(0L, NA), current = 0L)
  )
  expect_identical(
    diffr:::char_diff_myers_simple("a", c("a", "b")),
    list(target = 0L, current = c(0L, NA))
  )
  expect_identical(
    diffr:::char_diff_myers_simple("b", c("a", "b")),
    list(target = 0L, current = c(NA, 0L))
  )
  expect_identical(
    diffr:::char_diff_myers_simple(c("a", "b"), c("b", "c")),
    list(target = c(NA, 0L), current = c(0L, NA))
  )
  expect_identical(
    diffr:::char_diff_myers_simple(c("a", "b", "c", "d"), c("a", "c", "d", "b")),
    list(target = c(0L, NA, 0L, 0L), current = c(0L, 0L,  0L, NA))
  )
  # Actual Myers sample string
  expect_identical(
    diffr:::char_diff_myers_simple(
      c("a", "b", "c", "a", "b", "b", "a"),
      c("c", "b", "a", "b", "a", "c")
    ),
    list(target = c(NA, NA, 0L, 0L, 0L, NA, 0L), current = c(0L,  NA, 0L, 0L, 0L, NA))
  )
} )
