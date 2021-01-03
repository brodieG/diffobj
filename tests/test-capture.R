NAME <- "capture"
source(file.path('_helper', 'init.R'))

# - capture width issues -------------------------------------------------------

local({
  old.opt <- options(width=40L)
  on.exit(options(old.opt))
  etc <- new("Settings", style=StyleRaw(), text.width=5L)  # impossible width
  # warn: "Unable to set desired "
  res <- diffobj:::capture(letters, etc, function(...) do.call(cat, list(...)))
  all.equal(nchar(res), c(40L, 40L, 36L))
})

# - errors in capture ----------------------------------------------------------

etc <- new("Settings", style=StyleRaw())
try(diffobj:::capture(stop('boom'), etc, function(...) stop(...))) # boom
print <- function() NULL
str <- function() NULL
etc@mode <- "auto"
etc@frame <- environment()
try(diffobj:::capt_print(1, 2, etc, function(...) stop(...), list())) # compose
# spec object
try(diffobj:::capt_str(1, 2, etc, function(...) stop(...), list(object=1)))
try(  # attempting to deparse
  diffobj:::capt_deparse(
    stop('a'), stop('b'), etc,  function(...) stop(...), list()
  )
)
try(  # target
  suppressWarnings(
    diffobj:::capt_file(
      tempfile(), tempfile(), etc,  function(...) stop(...), list()
  ) )
)
local({
  f <- tempfile()
  on.exit(unlink(f), add=TRUE)
  writeLines(letters, f)
  try( # "`current`"
    suppressWarnings(
      diffobj:::capt_file(f, tempfile(), etc,  function(...) stop(...), list())
    )
  )
  try( # "`target`"
    suppressWarnings(
      diffobj:::capt_csv(
        tempfile(), tempfile(), etc,  function(...) stop(...), list()
    ) )
  )
  try( # "`current`"
    suppressWarnings(
      diffobj:::capt_csv(
        f, tempfile(), etc,  function(...) stop(...), list()
    ) )
  )
})
bad_obj <- structure(list(NULL), class='diffobj_ogewlhgiadfl3')
try( # "Coercion of `target`"
  diffobj:::capt_chr(bad_obj, letters, etc,  function(...) stop(...), list())
)
try( # "Coercion of `current`"
  diffobj:::capt_chr(letters, bad_obj, etc,  function(...) stop(...), list())
)
