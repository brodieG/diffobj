NAME <- "core"
source(file.path('_helper', 'init.R'))

# The Myers paper strings

A <- c("a", "b", "c", "a", "b", "b", "a")
B <- c("c", "b", "a", "b", "a", "c")

# - diff myers simple ----------------------------------------------------------

identical(
  diffobj:::myers_simple(character(), character()),
  list(target = integer(0), current = integer(0))
)
identical(
  diffobj:::myers_simple("a", character()),
  list(target = NA_integer_, current = integer(0))
)
identical(
  diffobj:::myers_simple(character(), "a"),
  list(target = integer(0), current = NA_integer_)
)
identical(
  diffobj:::myers_simple("a", "a"), list(target = 0L, current = 0L)
)
identical(
  diffobj:::myers_simple("a", "b"),
  list(target = 1L, current = 1L)
)
identical(
  diffobj:::myers_simple(c("a", "b"), "b"),
  list(target = c(NA, 0L), current = 0L)
)
identical(
  diffobj:::myers_simple(c("a", "b"), "a"),
  list(target = c(0L, NA), current = 0L)
)
identical(
  diffobj:::myers_simple("a", c("a", "b")),
  list(target = 0L, current = c(0L, NA))
)
identical(
  diffobj:::myers_simple("b", c("a", "b")),
  list(target = 0L, current = c(NA, 0L))
)
identical(
  diffobj:::myers_simple(c("a", "b"), c("b", "c")),
  list(target = c(NA, 0L), current = c(0L, NA))
)
identical(
  diffobj:::myers_simple(c("a", "b", "c", "d"), c("a", "c", "d", "b")),
  list(target = c(0L, NA, 0L, 0L), current = c(0L, 0L,  0L, NA))
)
# Actual Myers sample string
identical(
  diffobj:::myers_simple(A, B),
  list(target = c(NA, NA, 0L, 0L, 0L, NA, 0L), current = c(0L,  NA, 0L, 0L, 0L, NA))
)
# - diff myers mba -------------------------------------------------------------

identical(ses(character(), character()), character())
identical(ses("a", character()), "1d0")
identical(ses(character(), "a"), "0a1")
identical(ses("a", "a"), character())
identical(ses("a", "b"), "1c1")
identical(ses(c("a", "b"), "b"), "1d0")
identical(ses(c("a", "b"), "a"), "2d1")
identical(ses("a", c("a", "b")), "1a2")
identical(ses("b", c("a", "b")), "0a1")
identical(ses(c("a", "b"), c("b", "c")), c("1d0", "2a2"))
identical(
  ses(c("a", "b", "c", "d"), c("a", "c", "d", "b")), c("2d1", "4a4")
)
# Actual Myers sample string
identical(ses(A, B), c("1,2d0", "4d1", "5a3", "7a6"))
# This used to cause errors due to under-allocated buffer vector
identical(ses(letters[1:10], LETTERS[1:2]), "1,10c1,2")

# A little more complex with changes, this was a problem at some point

A2 <- c("A", "B", "C")
B2 <- c("X", "A", "Y", "C")
identical(ses(A2, B2), c("0a1", "2c3"))

# More complicated strings; intended for use with contexts for hunks,
# but making sure the diffs are correct

A1 <- c("A", "AA", "B", "C", "D", "E", "F", "G", "H")
B1 <- c("A", "B", "X", "W", "D", "DD", "E", "Y", "Z")
C1 <- c("X", "D", "E", "Y", "Z", "H")

identical(ses(A1, B1), c("2d1", "4c3,4", "5a6", "7,9c8,9"))
identical(ses(A1, C1), c("1,4c1", "7,8c4,5"))

A5 <- c("A", "AA", "B", "C", "D", "E", "F", "G", "H")
B5 <- c("A", "B",  "X", "W", "D", "E", "F", "W", "G")
identical(ses(A5, B5), c("2d1", "4c3,4", "7a8", "9d9"))

# NAs treated as strings

identical(ses(c(NA, "a", "b"), c("a", "b", NA)), c("1d0", "3a3"))

# Coersion to character

identical(ses(1:5, 4:6), c("1,3d0", "5a3"))

# - print/summary --------------------------------------------------------------
capture.output(
  res.1 <- summary(diffobj:::diff_myers(A, B), with.match=TRUE)
)
identical(
  res.1,
  structure(list(type = structure(c(3L, 1L, 3L, 1L, 2L, 1L, 2L), .Label = c("Match", 
"Insert", "Delete"), class = "factor"), string = c("ab", "c", 
"a", "b", "a", "ba", "c"), len = c(2L, 1L, 1L, 1L, 1L, 2L, 1L
), offset = c(1L, 3L, 4L, 5L, 3L, 6L, 6L)), class = "data.frame", row.names = c(NA, 
-7L))
)
capture.output(
  res.2 <- summary(diffobj:::diff_myers(A, B), with.match=FALSE)
)
identical(
  res.2,
  structure(list(type = structure(c(3L, 1L, 3L, 1L, 2L, 1L, 2L), .Label = c("Match", "Insert", "Delete"), class = "factor"), len = c(2L, 1L, 1L, 1L, 1L, 2L, 1L), offset = c(1L, 3L, 4L, 5L, 3L, 6L, 6L)), .Names = c("type", "len", "offset"), row.names = c(NA, -7L), class = "data.frame")
)
identical(
  capture.output(print(diffobj:::diff_myers(A, B))), ses(A, B)
)
#  # - translate
#    aa <- c("a", "b", "b", "c", "e")
#    bb <- c("x", "y", "c", "f", "e")
#    identical(
#      diffobj:::diffObjCompact(diffobj:::diff_myers(A, B)),
#      list(target = c(NA, NA, 0L, NA, 0L, 0L, 0L), current = c(0L, 0L, NA, 0L, 0L, NA))
#    )
#    identical(
#      diffobj:::diffObjCompact(diffobj:::diff_myers(aa, bb)),
#      list(target = c(1L, 2L, NA, 0L, 0L), current = c(1L, 2L, 0L, NA, 0L))
#    )
#
#  } )
