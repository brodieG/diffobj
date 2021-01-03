NAME <- "diffDeparse"
source(file.path('_helper', 'init.R'))

# - deparse --------------------------------------------------------------------

# First one will be done in unified mode since `deparse` disregards
# option(width=), second will be done side by side

all.equal(as.character(diffDeparse(letters, LETTERS)), rdsf(100))
all.equal(
  as.character(
    diffDeparse(letters, LETTERS, extra=list(width.cutoff=20))
  ),
  rdsf(200)
)
