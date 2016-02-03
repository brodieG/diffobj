library(diffobj)

test_that("hunks", {
  # Need to add simpler tests
})
test_that("processed hunks", {
  # Should only be one break in context with 1L
  A5 <- c("A", "AA", "B", "C", "D", "E", "F", "G", "H")
  B5 <- c("A", "B",  "X", "W", "D", "E", "F", "W", "G")

  hunks <- diffobj:::as.hunks(diffobj:::diff_myers_mba(A5, B5))
  expect_equal(
    diffobj:::process_hunks(hunks, 1L),
    list(list(target = c(0, NA_integer_, 0, 1, 0), current = c(0, 0, 1, NA_integer_, 0), tar.pos = 0, cur.pos = 0), list(target = c(0, 0, NA_integer_), current = c(0, NA_integer_, 0), tar.pos = 6, cur.pos = 6))
  )
  # Many more breaks without context
  expect_equal(
    diffobj:::process_hunks(hunks, 0L),
    list(list(target = NA_integer_, current = integer(0), tar.pos = 1, cur.pos = 1), list(target = 1, current = c(1, NA_integer_), tar.pos = 3, cur.pos = 2), list(target = integer(0), current = NA_integer_, tar.pos = 7, cur.pos = 7), list(target = NA_integer_, current = integer(0), tar.pos = 8, cur.pos = 9))
  )
  # No matching stretch long enough to allow a break, equivalently, force one
  # hunk by using negative index

  hunk.no.brk <- list(list(target = c(0, NA_integer_, 0, 1, 0, 0, 0, 0, NA_integer_), current = c(0, 0, 1, NA_integer_, 0, 0, 0, NA_integer_, 0), tar.pos = 0, cur.pos = 0))
  expect_equal(diffobj:::process_hunks(hunks, 2L),hunk.no.brk)
  expect_equal(diffobj:::process_hunks(hunks, -1L),hunk.no.brk)
})
