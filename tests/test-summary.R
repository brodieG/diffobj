NAME <- "summary"
source(file.path('_helper', 'init.R'))

# Note, atomic prints happen in different test file

# - Any ------------------------------------------------------------------------

identical(any(diffPrint(iris.s, iris.s)), FALSE)
res <- any(diffPrint(iris.s, iris.c)) # warn:  "objects are NOT"
identical(res, FALSE)
isTRUE(any(diffPrint(iris.s, iris.4)))


# - Small Summary --------------------------------------------------------------

all.equal(
  as.character(summary(diffPrint(iris.s, iris.4))), rdsf(100)
)
all.equal(
  as.character(summary(diffPrint(iris.s, iris.2))), rdsf(200)
)
all.equal(
  as.character(summary(diffPrint(iris.s, iris.3))), rdsf(300)
)
all.equal(
  as.character(summary(diffPrint(iris.s, iris.c))), rdsf(400)
)
# All equal

all.equal(
  as.character(summary(diffChr(letters, letters))), rdsf(450)
)

# - Big Summary ----------------------------------------------------------------

# Make sure we test summary reduction, wrapping

all.equal(
  as.character(summary(diffChr(chr.7, chr.8))), rdsf(500)
)
all.equal(
  as.character(summary(diffChr(chr.7, chr.8), scale.threshold=1)), rdsf(600)
)
all.equal(
  as.character(summary(diffChr(chr.7, chr.8), scale.threshold=0)), rdsf(700)
)
# Force truncation of summary
all.equal(
  as.character(
    summary(diffChr(chr.7, chr.8), scale.threshold=0, max.lines=2)
  ),
  rdsf(800)
)

# - Show -----------------------------------------------------------------------

isTRUE(
  paste0(capture.output(summary(diffChr(chr.7, chr.8))), collapse="\n") ==
  as.character(summary(diffChr(chr.7, chr.8)))
)

# - HTML summary ---------------------------------------------------------------

all.equal(
  as.character(
    summary(
      diffPrint(
        iris.s, iris.4, format="html", style=list(html.output="page")
  ) ) ),
  rdsf(900)
)

# - errors ---------------------------------------------------------------------

diff <- diffChr("hello green world", "hello red world")
try(summary(diff, max.lines=0)) # "strictly positive"
try(summary(diff, width=1:3)) # "integer\\(1L\\)"
try(summary(diff, scale.threshold=5)) # "between 0 and 1"

# - width wrap -----------------------------------------------------------------

diff <- diffChr("hello green world", "hello red world", format='raw')
all.equal(capture.output(show(summary(diff, width=5))), txtf(100))

