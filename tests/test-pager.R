NAME <- "pager"
source(file.path('_helper', 'init.R'))
source(file.path('_helper', 'tools.R'))

# void pager, doesn't do anything, just to test side effect of writing to file

void <- function(x) NULL

# - Specifying pager -----------------------------------------------------------

style <- gdo("diffobj.style")
if(is.null(style)) style <- StyleAnsi8NeutralYb()
style@pager@file.ext <- "xyz"  # make pager identifiable
all.equal(
  diffChr(
    letters, LETTERS, style=style, pager="auto", interactive=TRUE
  )@etc@style@pager@file.ext,
  "xyz"
)
all.equal(
  diffChr(
    letters, LETTERS, style=style, pager="off", interactive=TRUE
  )@etc@style@pager,
  PagerOff()
)
identical(
  diffChr(
    letters, LETTERS, style=style, pager="auto", interactive=FALSE
  )@etc@style@pager,
  PagerOff()
)

# - System Pagers --------------------------------------------------------------

less.orig <- Sys.getenv("LESS")
pager_mock <- function(...) {
  warning(Sys.getenv("LESS"))
  42
}
is(PagerSystem(), "PagerSystem")
is(
  pg.less <- PagerSystemLess(pager=pager_mock, flags="VWF"),
  "PagerSystemLess"
)
res <- pg.less@pager() # warning: "VWF$"
all.equal(res, 42)
all.equal(less.orig, Sys.getenv("LESS"))
all.equal(PagerSystemLess(pager=pager_mock)@flags, "R")

try(PagerSystemLess(pager=pager_mock, flags=letters))

# - use_pager ------------------------------------------------------------------

local({
  suppressMessages(mock(diffobj:::console_lines, 10L))
  on.exit(suppressMessages(untrace(diffobj:::console_lines)))
  c(
    isTRUE(diffobj:::use_pager(PagerSystem(threshold=0L), 1L)),
    identical(diffobj:::use_pager(PagerSystem(threshold=50L), 25L), FALSE),
    isTRUE(diffobj:::use_pager(PagerSystem(threshold=-1L), 25L))
  )
})

# - Setting LESS var -----------------------------------------------------------

local({
  less.orig <- Sys.getenv("LESS", unset=NA)
  old.opt <- options(crayon.enabled=FALSE)  # problems with crayon and LESS
  on.exit({
    diffobj:::reset_less_var(less.orig) # should be tested..., but super simple
    options(old.opt)
  })

  # Here we change the LESS variable even though we're mocking getenv

  Sys.unsetenv("LESS")
  a0 <- isTRUE(is.na(diffobj:::set_less_var("XF")))
  a <- all.equal(Sys.getenv("LESS"), "-XF")
  Sys.setenv(LESS="-X -F")
  b <- all.equal(diffobj:::set_less_var("VP"), "-X -F")
  c <- all.equal(Sys.getenv("LESS"), "-X -FVP")
  diffobj:::reset_less_var("-XF")
  d <- all.equal(Sys.getenv("LESS"), "-XF")
  diffobj:::reset_less_var(NA_character_)
  e <- all.equal(Sys.getenv("LESS"), "")
  Sys.setenv(LESS="-XF")
  f <- all.equal(diffobj:::set_less_var("V"), "-XF")
  g <- all.equal(Sys.getenv("LESS"), "-XFV")
  c(a0, a, b, c, d, e, f, g)
})

# - viewer vs browser ----------------------------------------------------------

local({
  viewer <- function(x) "viewer"
  old.external <- options(viewer=viewer, browser=function(url) "browser")
  on.exit(options(old.external))
  suppressMessages(mock(diffobj::make_blocking, quote(fun)))
  on.exit(suppressMessages(untrace(diffobj::make_blocking)), add=TRUE)
  pager <- PagerBrowser()
  a <- all.equal(pager@pager("blah"), "viewer")
  options(viewer=NULL)
  b <- all.equal(pager@pager("blah"), "browser")
  options(viewer=function(x) stop("viewer error"))
  res <- pager@pager("blah") # warning: "IDE viewer"
  c <- all.equal(res, "browser")
  c(a, b, c)
})

# - blocking -------------------------------------------------------------------

# Note that readline just proceeds in non-interactive mode, which is why we
# need the mock here

local({
  suppressMessages(mock(diffobj:::interactive, FALSE))
  on.exit(suppressMessages(untrace(diffobj:::interactive)))
  suppressMessages(mock(diffobj:::readline, quote(warning("readline"))))
  on.exit(suppressMessages(untrace(diffobj:::readline)), add=TRUE)
  try(make_blocking("hello")) # "must be a function"
  try(make_blocking(identity, letters)) # "must be character\\(1L")
  try(make_blocking(identity, "a", "a")) # "must be TRUE"

  res <- make_blocking(sum)(1:10) # warn: "readline"
  a <- all.equal(sum(1:10), res)
  b <- isTRUE(
    withVisible(
      suppressWarnings(make_blocking(sum, invisible=FALSE)(1:10))
    )[['visible']]
  )
  c(a, b)
})
local({
  suppressMessages(mock(diffobj:::interactive, TRUE))
  on.exit(suppressMessages(untrace(diffobj:::interactive)))
  suppressMessages(mock(diffobj:::readline, quote(warning("readline"))))
  on.exit(suppressMessages(untrace(diffobj:::readline)), add=TRUE)
  show( # warn "readline"
    diffChr(
      "a", "b", format='raw',
      pager=list(pager=void, make.blocking=TRUE, threshold=0)
    )
  )
  show( # warn "readline"
    diffChr(
      "a", "b", format='html',
      pager=list(pager=void, make.blocking=NA, threshold=0)
  ) )
  show(diffChr("a", "b", format='html', pager=list(pager=void)))
})
# There should be no warnings in this lot

local({
  suppressMessages(mock(diffobj:::interactive, TRUE))
  on.exit(suppressMessages(untrace(diffobj:::interactive)))
  suppressMessages(mock(diffobj:::readline, quote(warning("readline"))))
  on.exit(suppressMessages(untrace(diffobj:::readline)), add=TRUE)
  f <- tempfile()
  on.exit(unlink(f), add=TRUE)
  show(  # no warning
    diffChr(
      "a", "b", format='html',
      pager=list(pager=void, make.blocking=NA, file.path=f)
  ) )
  show(  # no warning
    diffChr(
      "a", "b", format='html',
      pager=list(pager=void, make.blocking=FALSE, file.path=f)
  ) )
  show(  # no warning
    diffChr("a", "b", format='html', pager=list(pager=void, file.path=f))
  )
})

# - html page output -----------------------------------------------------------

pager <- PagerBrowser(
  pager=function(x) cat(readLines(x), sep="\n"), make.blocking=FALSE
)
all.equal(
  capture.output(show(diffChr("A", "B", pager=pager, style=StyleRaw()))),
  c("< \"A\"       > \"B\"     ", "@@ 1 @@     @@ 1 @@   ", "< A         > B       ")
)
pager.warn <- PagerBrowser(
  pager=function(x) cat(readLines(x), sep="\n"), make.blocking=FALSE
)
try( # "Unable to instantiate `Style` object: Argument `js` .* is not a file"
  diffChr(
    "A", "B", pager=pager.warn, format="html", style=list(js="notafile")
) )
try( # "Unable to instantiate `Style` object: Argument `css` .* is not a file"
  diffChr(
    "A", "B", pager=pager.warn, format="html", style=list(css="notafile")
  )
)
# Create objects that bypass the validation

style.obj.1 <- style.obj.2 <- StyleHtmlLightYb()
style.obj.1@css <- "notafile"
style.obj.2@js <- "notafile"

invisible(
  capture.output( # warn: "Unable to read provided css file"
    show(diffChr("A", "B", pager=pager.warn, style=style.obj.1))
) )
invisible(
  capture.output( # "Unable to read provided js file"
    show(diffChr("A", "B", pager=pager.warn, style=style.obj.2))
) )
# - pager_is_less --------------------------------------------------------------

is.less <- pager_is_less()
isTRUE(diffobj:::is.TF(is.less))

less <- tryCatch(
  system2("which", "less", stdout=TRUE, stderr=TRUE),
  error=function(e) NULL, warning=function(e) NULL
)
sys.cat <- tryCatch(
  system2("which", "cat", stdout=TRUE, stderr=TRUE),
  error=function(e) NULL, warning=function(e) NULL
)
if(diffobj:::is.chr.1L(less) && file_test("-x", less)) {
  local({
    old.opt <- options(pager=less)
    on.exit(options(old.opt))

    # has to be stopifnot as we can't return TRUE for systems that don't
    # meet these requirements
    stopifnot(
      identical(diffobj:::pager_opt_default(), FALSE),
      isTRUE(pager_is_less())
    )
  })
}
if(diffobj:::is.chr.1L(sys.cat) && file_test("-x", sys.cat)) {
  local({
    old.opt <- options(pager=sys.cat)
    on.exit(options(old.opt))

    # has to be stopifnot as we can't return TRUE for systems that don't
    # meet these requirements
    stopifnot(
      identical(diffobj:::pager_opt_default(), FALSE),
      identical(pager_is_less(), FALSE)
    )
  })
}
## force some checks

local({
  old.opt <- options(pager=NULL)
  on.exit(options(old.opt))
  identical(pager_is_less(), FALSE)
})
identical(diffobj:::file_is_less(tempfile()), FALSE)

# - file.path ------------------------------------------------------------------

f <- tempfile()
show(
  diffChr(
    "A", "B", format='raw',
    pager=list(pager=void, file.path=f, threshold=0L)
) )
all.equal(
  readLines(f),
  c("< \"A\"       > \"B\"     ", "@@ 1 @@     @@ 1 @@   ",
    "< A         > B       ")
)
show(  # No error on this one
  diffChr(
    "A", "B", format='raw',
    pager=list(pager=void, file.path=NA, threshold=0L)
) )
try(Pager(file.path=letters)) # "must be length 1"
try(Pager(file.path=1)) # "must be character"

# - basic pager ----------------------------------------------------------------

local({
  f <- tempfile()
  on.exit(unlink(f))
  c(
    all.equal(
      capture.output(
        show(
          diffChr(
            1, 2, pager=Pager(file.path=f, threshold=0L),
            format='raw'
          )
      ) ),
      txtf(100)
    ),
    all.equal(txtf(100), readLines(f))
  )
})

# - format-pager interaction ---------------------------------------------------

local({
  old.opt <- options(crayon.colors=7)
  crayon::num_colors(TRUE)
  on.exit({
    options(old.opt)
    crayon::num_colors(TRUE)
  })
  c(
    is(
      diffChr(1, 2, format='auto', pager="on", interactive=TRUE)@etc@style,
      "StyleHtml"
    ),
    is(
      diffChr(1, 2, format='auto', pager="on", interactive=FALSE)@etc@style,
      "StyleRaw"
    ),
    is(
      diffChr(
        1, 2, format='auto', pager=PagerBrowser(), interactive=FALSE
      )@etc@style,
      "StyleHtml"
    )
  )
})
# - format-pager interaction 2 -------------------------------------------------

local({
  old.rs <- Sys.getenv('RSTUDIO', unset=NA)
  old.rsterm <- Sys.getenv('RSTUDIO_TERM', unset=NA)
  on.exit({
    if(is.na(old.rs)) {
      Sys.unsetenv('RSTUDIO')
    } else Sys.setenv('RSTUDIO'=old.rs)

    if(is.na(old.rsterm)) {
      Sys.unsetenv('RSTUDIO_TERM')
    } else Sys.setenv('RSTUDIO_TERM'=old.rsterm)
  })
  Sys.unsetenv('RSTUDIO')
  Sys.unsetenv('RSTUDIO_TERM')
  old.opt <- options(crayon.colors=8)
  crayon::num_colors(TRUE)
  on.exit({options(old.opt); crayon::num_colors(TRUE)}, add=TRUE)

  Sys.setenv(RSTUDIO='1')

  a <- c(
    is(
      diffChr(1, 2, format='auto', pager='on', interactive=TRUE)@etc@style,
      "StyleHtml"
    ),
    is(
      diffChr(1, 2, format='auto', interactive=FALSE)@etc@style,
      "StyleAnsi"
  ) )
  Sys.setenv(RSTUDIO_TERM='HELLO')
  crayon::num_colors(TRUE)

  c(
    a,
    is(
      diffChr(1, 2, format='auto', pager='on', interactive=TRUE)@etc@style,
      "StyleAnsi"
  ) )
})

# - format-pager interaction 3 -------------------------------------------------

is(
  diffPrint(1:3, 3:1, format='auto', interactive=FALSE, term.colors=1)@etc@style,
  "StyleRaw"
)
is(
  diffPrint(1:3, 3:1, format='auto', interactive=FALSE, term.colors=8)@etc@style,
  "StyleAnsi"
)

# - Default pager writes to screen ---------------------------------------------

# issue132 thanks Bill Dunlap

local({
  f <- tempfile()
  on.exit(unlink(f))
  writeLines("hello world", f)

  all.equal(capture.output(new("Pager")@pager(f)), "hello world")
})

