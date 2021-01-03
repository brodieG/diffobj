NAME <- "limit"
source(file.path('_helper', 'init.R'))

# - Simple limit ---------------------------------------------------------------

A <- B <- letters[1:5]
B[2] <- "B"
B[6] <- "F"
# diffChr(A, B)
all.equal(as.character(diffChr(A, B, line.limit=2)), rdsf(100))
all.equal(as.character(diffChr(A, B, line.limit=3)), rdsf(200))

# - More Extensive Limits ------------------------------------------------------

Puromycin2 <- Puromycin
set.seed(1)
Puromycin2$conc[c(8, 15:19, 22)] <- round(runif(7), 2)
Puromycin2$state[17] <- "treated"

all.equal(
  as.character(
    diffPrint(Puromycin, Puromycin2, line.limit=15, mode="sidebyside")
  ),
  rdsf(300)
)

# # Not working right
# diffPrint(Puromycin, Puromycin2, line.limit=15, mode="context")
all.equal(
  as.character(
    diffPrint(Puromycin, Puromycin2, line.limit=15, mode="unified")
  ),
  rdsf(500)
)

all.equal(
  as.character(
    diffPrint(Puromycin, Puromycin2, line.limit=5, mode="sidebyside")
  ),
  rdsf(600)
)
all.equal(
  as.character(
    diffPrint(Puromycin, Puromycin2, line.limit=5, mode="context")
  ),
  rdsf(700)
)
all.equal(
  as.character(
    diffPrint(Puromycin, Puromycin2, line.limit=5, mode="unified")
  ),
  rdsf(800)
)

Puromycin3 <- Puromycin2
names(Puromycin3)[3L] <- "blargh"
all.equal(
  as.character(
    diffPrint(Puromycin, Puromycin3, line.limit=7, mode="sidebyside")
  ),
  rdsf(900)
)
all.equal(
  as.character(
    diffPrint(Puromycin, Puromycin3, line.limit=6, mode="context")
  ),
  rdsf(1000)
)
# - Dual limit values ----------------------------------------------------------

A <- letters[1:10]
B <- LETTERS[1:10]
all.equal(
  as.character(diffChr(A, B, line.limit=c(10, 3))), rdsf(1100)
)
all.equal(
  as.character(diffChr(A, B, line.limit=c(13, 3))), rdsf(1200)
)
try(diffChr(A, B, line.limit=c(3, 13))) # "larger than or"

# - Cause errors ---------------------------------------------------------------

try(diffChr(letters, LETTERS, line.limit=1:3)) # "vector of length"

# - Vanishing header -----------------------------------------------------------

# issue 64
all.equal(
  as.character(
    diffChr(
      letters, letters[-13], context=auto_context(0, 10), line.limit=1L,
      pager="off"
  ) ),
  rdsf(1300)
)

