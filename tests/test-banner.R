NAME <- "banner"
source(file.path('_helper', 'init.R'))

# - Banner Capture ------------------------------------------------------------

ref <- as.character(
  diffPrint(1 + 2, letters, tar.banner="1 + 2", cur.banner="letters")
)
identical(as.character(diffPrint(1 + 2, letters)), ref)
invisible(
  setMethod(
    "diffPrint", c("numeric", "character"),
    function(target, current, ...) callNextMethod()
) )
identical(as.character(diffPrint(1 + 2, letters)), ref)
isTRUE(
  !identical(as.character(diffPrint(1 + 2, LETTERS)), ref)
)
