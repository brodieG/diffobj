NAME <- "check"
source(file.path('_helper', 'init.R'))

# - is.less_flags --------------------------------------------------------------

isTRUE(diffobj:::is.less_flags("RVXF"))
isTRUE(diffobj:::is.less_flags("rvxF"))
identical(diffobj:::is.less_flags(c("rvxF", "RVXF")), FALSE)
identical(diffobj:::is.less_flags(23), FALSE)
identical(diffobj:::is.less_flags("rv xF"), FALSE)

# - is.int.2L ------------------------------------------------------------------

isTRUE(diffobj:::is.int.2L(1:2))
isTRUE(diffobj:::is.int.2L(as.numeric(1:2)))
identical(diffobj:::is.int.2L(c(1.3, 2.2)), FALSE)
identical(diffobj:::is.int.2L(1:3), FALSE)
identical(diffobj:::is.int.2L(c(1, NA)), FALSE)

# - arg.funs -------------------------------------------------------------------

isTRUE(diffobj:::is.one.arg.fun(function(x) NULL))
isTRUE(diffobj:::is.one.arg.fun(function(x, y=5) NULL))

diffobj:::is.one.arg.fun(function(..., x) NULL) # "cannot have `...` as "
diffobj:::is.one.arg.fun(NULL) # "is not a fun"
diffobj:::is.one.arg.fun(function() NULL) # "have at least"
diffobj:::is.one.arg.fun(function(x, y) NULL) # "cannot have any"

isTRUE(diffobj:::is.two.arg.fun(function(x, y) NULL))
isTRUE(diffobj:::is.two.arg.fun(function(x, y=5) NULL))

diffobj:::is.two.arg.fun(function(x, ..., y) NULL) # "cannot have `...` as "
diffobj:::is.two.arg.fun(NULL) # "is not a fun"
diffobj:::is.two.arg.fun(function(x) NULL) # "have at least")
diffobj:::is.two.arg.fun(function(x, y, z) NULL) # "cannot have any"

# - valid_object ---------------------------------------------------------------

s.h <- StyleHtml()
s.h@wrap <- TRUE
try(diffobj:::valid_object(s.h, "style", stop)) #an invalid `StyleHtml` object

pal <- PaletteOfStyles()
pal["html", "light", "yb"] <- list(s.h)
try(# "`palette.of.styles` is an invalid"
  diffChr(
    "A", "B", palette.of.styles=pal, style="auto", format="html",
    brightness="light", color.mode="yb"
  )
)
# - brightness -----------------------------------------------------------------

try(diffPrint(1:3, 3:6, brightness=NA)) # "must be character"
try(diffPrint(1:3, 3:6, brightness="red")) # "may only contain values"
try(diffPrint(1:3, 3:6, brightness=c(raw='light'))) # "one empty name"
try(diffPrint(1:3, 3:6, brightness=c('light', 'dark'))) # have names

# - misc -----------------------------------------------------------------------

diffobj:::is.one.file.name(1) # "must be character"
try(diffPrint(1:3, 2:6, extra="hello")) # "must be a list"
try(diffPrint(1:3, 2:6, context=TRUE)) # "Argument `context` must"
try(diffPrint(1:3, 2:6, mode=1)) # "must be character"
try(diffPrint(1:3, 2:6, tab.stops=-1)) # "strictly positive"
try(diffPrint(1:3, 2:6, hunk.limit='hello')) # "integer vector"
try(diffPrint(1:3, 2:6, guides='hello')) # "or a function"
try(diffPrint(1:3, 2:6, guides=function(x, y, z) NULL))# "cannot have any non"
try(diffPrint(1:3, 2:6, trim='hello')) # "TRUE, FALSE, or a function"
try(diffPrint(1:3, 2:6, trim=function(x, y, z) NULL)) # "cannot have any non"
try(diffPrint(1:3, 2:6, interactive='hello')) # "must be TRUE or"
try(diffPrint(1:3, 2:6, max.diffs=1:10)) # "must be integer"
try(diffPrint(1:3, 2:6, tar.banner=1:10)) # "must be atomic"
try(diffPrint(1:3, 2:6, style=1:10)) # "must be \"auto\", a"
try(diffPrint(1:3, 2:6, pager=1:10)) # "must be one of"
try(diffPrint(1:3, 2:6, format=1:10)) # "must be character"
try(diffPrint(1:3, 2:6, palette.of.styles=1:10)) # "must be a `PaletteOfStyles`"
try(diffChr(letters, LETTERS, context=NA)) # "must be integer"


