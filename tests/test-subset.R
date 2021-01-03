NAME <- "subset"
source(file.path('_helper', 'init.R'))

A <- B <- letters[1:5]
B[2] <- "B"
B[6] <- "F"

# - subset ---------------------------------------------------------------------

local({
  old.opt <- options(diffobj.style=StyleRaw())
  on.exit(options(old.opt))
  a0 <- all.equal(
    c(as.character(diffChr(A, B)[1:3])),
    c("< A          > B        ", "@@ 1,5 @@    @@ 1,6 @@  ", "  a            a        ")

  )
  a <- all.equal(
    c(as.character(diffChr(A, B)[1])), c(as.character(head(diffChr(A, B), 1)))
  )
  b <- all.equal(
    c(as.character(diffChr(A, B)[7:8])), c(as.character(tail(diffChr(A, B), 2)))
  )
  c(a0, a, b)
})
# - subset errors --------------------------------------------------------------

diff <- diffChr(A, B)
try(diff[NA_real_]) # "contain NAs or both positive"
try(diff[c(-1, 1)]) # "contain NAs or both positive"
try(head(diff, 1, 2)) # "does not support arguments"
try(head(diff, NA)) # "must be integer"
try(head(diff, 1:3)) # "must be integer"
try(tail(diff, 1:3)) # "must be integer"
try(tail(diff, 1, 2)) # "does not support arguments"


