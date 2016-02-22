library(diffobj)

# For now we're just listing out all the tests we want to run
test_that("hunks", {
  # Need to add simpler tests
})
A5 <- c("A", "AA", "B", "C", "D", "E", "F", "G", "H")
B5 <- c("A", "B",  "X", "W", "D", "E", "F", "W", "G")

diff.raw <- diffobj:::diff_myers_mba(A5, B5)
old.opt <- options(diffobj.use.ansi=TRUE)

# These will eventually need to be turned into tests; for now just recording
# the variations we need to test

h0 <- diffobj:::as.hunks(A5, B5)
h1 <- diffobj:::as.hunks(A5, B5, context=0L)
h2 <- diffobj:::as.hunks(A5, B5, context=0L, mode="context")
h3 <- diffobj:::as.hunks(A5, B5, context=0L, mode="sidebyside")
h4 <- diffobj:::as.hunks(A5, B5, context=1L)
h5 <- diffobj:::as.hunks(A5, B5, context=1L, mode="context")
h6 <- diffobj:::as.hunks(A5, B5, context=1L, mode="sidebyside")
h7 <- diffobj:::as.hunks(A5, B5, context=2L)
h8 <- diffobj:::as.hunks(A5, B5, context=2L, mode="context")
h9 <- diffobj:::as.hunks(A5, B5, context=2L, mode="sidebyside")

test_that("generate hunks", {
  # fill in equal to reference for h0-h9 above

} )
test_that("trim hunks", {
  diffobj:::trim_hunks(h4, mode="unified", line.limit=10L, hunk.limit=-1L)
  diffobj:::trim_hunks(h5, mode="context", line.limit=10L, hunk.limit=-1L)
  diffobj:::trim_hunks(h6, mode="sidebyside", line.limit=10L, hunk.limit=-1L)

  diffobj:::trim_hunks(h4, mode="unified", line.limit=2L, hunk.limit=-1L)
  diffobj:::trim_hunks(h5, mode="context", line.limit=2L, hunk.limit=-1L)
  diffobj:::trim_hunks(h6, mode="sidebyside", line.limit=2L, hunk.limit=-1L)

  diffobj:::trim_hunks(h4, mode="unified", line.limit=-1L, hunk.limit=1L)
  diffobj:::trim_hunks(h5, mode="context", line.limit=-1L, hunk.limit=1L)
  diffobj:::trim_hunks(h6, mode="sidebyside", line.limit=-1L, hunk.limit=1L)
} )
options(old.opt)
