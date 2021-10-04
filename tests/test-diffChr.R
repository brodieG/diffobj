NAME <- "diffChr"
source(file.path('_helper', 'init.R'))

# - Corner Cases ---------------------------------------------------------------

# Corner cases from https://neil.fraser.name/writing/diff/
# Both of these appear handled correctly by the algorithm here
# first one: suboptimal edit script due to two sided approach

A1 <- c("X", "A", "X", "C", "X", "A", "B", "C")
B1 <- c("A", "B", "C", "Y")
all.equal(as.character(diffChr(A1, B1)), rdsf(100))

# second one: failure to find intersection at ends of paths (paths run into
# each other eventually)

A2 <- c("A", "B", "X", "A", "B")
B2 <- c("A", "Y", "B")
all.equal(as.character(diffChr(A2, B2)), rdsf(200))

# Simple corner cases

all.equal(
  as.character(diffChr(character(), character())), rdsf(225)
)
all.equal(as.character(diffChr("", "")), rdsf(250))

# - Larger strings -------------------------------------------------------------

# diffChr(X[1:2000], X[2001:4000])

all.equal(as.character(diffChr(chr.7, chr.8)), rdsf(300))

# Too slow to run; useful for benchmarking though

# X1 <- X[1:2e4]
# X2 <- X1[-sample(seq_along(X1), 2e3)]
# X2[sample(seq_along(X2), 4e3)] <- "XXXXXX"
# res <- diffChr(X1, X2)
# res <- diffChr(X[1:10000], X[7500:17500])
# res <- ses(X[1:10000], X[7500:17500])
# res <- diffChr(X[1:25000], X[10001:50000], max.diffs=65000)

# - Sentences
chr.5 <- c(
  "hello there how are you doing",
  "humpty dumpty took a big fall",
  "lorem ipsum dolor sic est boom",
  "a computer once wrote a phrase"
)
chr.6 <- c(
  "hello THERE how are you doing",
  "and another SENTENCE blah blah",
  "humpty dumpty TOOK a big fall",
  "a COMPUTER once wrote a phrase"
)
all.equal(as.character(diffChr(chr.5, chr.6)), rdsf(400))
all.equal(
  as.character(diffChr(chr.5, chr.6, mode="unified")), rdsf(500)
)
all.equal(
  as.character(diffChr(chr.5, chr.6, mode="context")), rdsf(600)
)
# - Whitespace -----------------------------------------------------------------

all.equal(
  as.character(diffChr(c("a", "b", "c"), c("a ", "b", "c"))), rdsf(800)
)
all.equal(
  as.character(
    diffChr(c("a", "b", "c"), c("a ", "b", "c"), ignore.white.space=FALSE)
  ),
  rdsf(900)
)
# New lines count as new elements
all.equal(
  as.character(diffChr("woo\nhoo\nfoo", c("woo", "foo"))), rdsf(1000)
)
all.equal(
  capture.output(diffChr("hello . world", "hello.  world", format='raw')),
  txtf(100)
)
# - SGR ------------------------------------------------------------------------

a <- c("hello \033[31mworld\033[m", "umbrellas", "tomatoes")
b <- c("hello world", "umbrellas", "tomatoes")

local({
  old.opt <- options(diffobj.sgr.supported=TRUE)
  on.exit(options(old.opt))
  diff <- diffChr(a, b) # warn: 'contained ANSI CSI SGR'
  try(diffChr(a, b, strip.sgr=1:3)) # "TRUE, FALSE, or NULL"
  try(diffChr(a, b, sgr.supported=1:3)) # "TRUE, FALSE, or NULL"

  c(
    all.equal(capture.output(show(diff)), txtf(200)),
    all.equal(capture.output(show(diffChr(a, b, strip.sgr=FALSE))), txtf(300)),
    all.equal(capture.output(show(diffChr(a, b, format='raw'))), txtf(400))
  )
})
# - Alignment ------------------------------------------------------------------

chr.7 <- c("a b c d e", "F G h i j k", "xxx", "yyy", "k l m n o")
chr.8 <- c("f g h i j k", "hello", "goodbye", "yo", "k l m n o")

all.equal(as.character(diffChr(chr.7, chr.8)), rdsf(1100))
all.equal(
  as.character(diffChr(chr.7, chr.8, align=4/6)), rdsf(1100) # same as above
)
# No longer aligns
all.equal(
  as.character(diffChr(chr.7, chr.8, align=4.01/6)), rdsf(1200)
)
all.equal(
  as.character(diffChr(chr.7, chr.8, align=AlignThreshold(min.chars=4))),
  rdsf(1100)  # same as earlier
)
all.equal(
  as.character(diffChr(chr.7, chr.8, align=AlignThreshold(min.chars=5))),
  rdsf(1200)  # same as above
)

## Normally this would not align, but we allow symbols to count towards
## alignment
chr.7a <- c("a b c e", "d [ f g")
chr.7b <- "D [ f g"
a1 <- AlignThreshold(threshold=0, min.chars=2, count.alnum.only=FALSE)
all.equal(
  as.character(diffChr(chr.7a, chr.7b, align=a1, format='raw')),
  structure(
    c("< chr.7a     > chr.7b   ", "@@ 1,2 @@    @@ 1 @@    ",
      "< a b c e    ~          ", "< d [ f g    > D [ f g  "), len = 4L)
)
# corner case where alignment alog exits early because it runs out of B values
# to match A values to.

b <- c('a b c e', 'x w z f', 'e f g h')
a <- c('z o o o', 'p o o o', 'A b c e')
al <- AlignThreshold(threshold=0, min.chars=0)
all.equal(
  capture.output(show(diffChr(b, a, align=al, format='raw'))), txtf(500)
)
# - NAs ------------------------------------------------------------------------

all.equal(
  as.character(
    diffChr(c(NA, letters[1:3]), c(letters[1:3], LETTERS[1:2], NA))
  ),
  rdsf(1300)
)
all.equal(
  as.character(
    diffChr(c(letters[1:3]), c(letters[1:3], LETTERS[1:2], NA))
  ),
  rdsf(1400)
)
all.equal(
  as.character(
    diffChr(c(NA, letters[1:3]), c(letters[1:3], LETTERS[1:2]))
  ),
  rdsf(1500)
)
# - Nested dots issue 134, h/t Noam Ross ---------------------------------------

fn <- function(target, current, ...) {
  diffChr(target, current, ...)
}
all.equal(
  as.character(fn("a", "b", format = "raw")),
  structure(
    c(
      "< target    > current ",
      "@@ 1 @@     @@ 1 @@   ",
      "< a         > b       "), len = 3L
  )
)

# - Newlines in input, issue 135, h/t Flying Sheep -----------------------------

a <-     'A Time Series:\n[1] 1 2 3 4'
b <-     'A Time Series:\n[1] 9 4 1 4'
all.equal(
  c(as.character(diffobj::diffChr(a, b, format = 'raw'))),
  c("< a               > b             ",
    "@@ 1,2 @@         @@ 1,2 @@       ",
    "  A Time Series:    A Time Series:",
    "< [1] 1 2 3 4     > [1] 9 4 1 4   ")
)

# - Attributes causing dispatch in guides, issue 142 ---------------------------

zlold <- c("0x0000, 0x001F", "0x007F, 0x009F", "0x0300, 0x036F")
zlnew <- structure(
  c("0x0000, 0x001F", "0x008F, 0x009F", "0x0300, 0x036F"), .Dim = 3L
)
diffChr(zlold, zlnew) # no warning

# - do.call, issue 158 ---------------------------------------------------------

do.call(diffChr, list(1:2, 3:4, format='raw'))

