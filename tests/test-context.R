NAME <- "context"
source(file.path('_helper', 'init.R'))

# - interesting context values -------------------------------------------------

all.equal(
  as.character(diffChr(chr.9, chr.10, context=0)),
  rdsf(100)
)
all.equal(
  as.character(diffChr(chr.9, chr.10, context=-1L)),
  rdsf(150)
)
all.equal(
  as.character(diffChr(chr.9, chr.10, context="auto")),
  rdsf(200)
)
all.equal(
  as.character(diffChr(chr.9, chr.10, context=0, mode="context")), rdsf(300)
)
# - with line limit ------------------------------------------------------------

all.equal(
  as.character(diffChr(chr.9, chr.10, context="auto", line.limit=18)),
  rdsf(400)
)
all.equal(
  as.character(diffChr(chr.9, chr.10, context="auto", line.limit=25)),
  rdsf(500)
)
# default to min context

a <- b <- letters
b[c(3, 20)] <- LETTERS[c(3,20)]
all.equal(
  capture.output(
    show(diffChr(a, b, line.limit=c(20, 10), context='auto', format='raw'))
  ),
  txtf(100)
)
# trim hunks in auto-context mode

a <- b <- letters
b[c(3, 10, 20)] <- LETTERS[c(3,10,20)]
all.equal(
  capture.output(show(
    diffChr(
      a, b, hunk.limit=c(2, 1), context=auto_context(1, 5), line.limit=20,
      format='raw'
    )
  )),
  txtf(200)
)
# - error handling -------------------------------------------------------------

try(auto_context(min=-1, max=1:3)) # "`min` must be"
try(auto_context(min=1, max=1:3)) # "`max` must be"

