NAME <- "style"
source(file.path('_helper', 'init.R'))

## - Style Palette ------------------------------------------------------------

all.equal(
  capture.output(diffobj:::display_ansi_256_styles()),
  rdsf(100)
)
## - crayon settings -----------------------------------------------------------

  # make sure crayon options are appropriately overriden
local({
  old.opt <- options(crayon.enabled=FALSE)
  on.exit(options(old.opt))
  print(identical(crayon::green("green"), "green"))
  # should have ANSI coloring despite crayon disabled
  print(
    all.equal(
      as.character(diffChr(letters[1:3], LETTERS[1:3])), rdsf(200)
    )
  )
  identical(crayon::green("green"), "green")
})
## - Palette of Styles ---------------------------------------------------------

pos <- PaletteOfStyles()
identical(
  pos[["ansi256", "light", "rgb"]],
  getClassDef("StyleAnsi256LightRgb", package="diffobj", inherits=FALSE)
)
all.equal(
  capture.output(show(pos)), rdsf(300)
)
all.equal(
  capture.output(summary(pos)), rdsf(400)
)

pos["ansi256", "light", "yb"] <- list(StyleRaw())
all.equal(
  c(pos["ansi256", "light", "yb"]@data), list(StyleRaw()),
  check.environment=FALSE
)
all.equal(
  pos[["ansi256", "light", "yb"]], StyleRaw(),
  check.environment=FALSE
)

## - Auto Styles ---------------------------------------------------------------

try(diffChr(letters, LETTERS, style="auto", format="xml"))
is(
  diffChr(
    letters, LETTERS, style="auto", format="auto", brightness="light",
    term.colors=256
  )@etc@style,
  "StyleAnsi256LightYb"
)
is(
  diffChr(
    letters, LETTERS, style="auto", format="auto", brightness="light",
    term.colors=8
  )@etc@style,
  "StyleAnsi8NeutralYb"
)
is(
  diffChr(
    letters, LETTERS, style="auto", format="auto", interactive=FALSE,
    term.colors=1
  )@etc@style,
  "StyleRaw"
)
is(
  diffChr(
    letters, LETTERS, style="auto", format="auto", interactive=TRUE,
    term.colors=1  # note pager off by default in tests
  )@etc@style,
  "StyleRaw"
)
is(
  diffChr(
    letters, LETTERS, style="auto", format="auto", interactive=TRUE,
    pager="auto", term.colors=1
  )@etc@style,
  "StyleHtml"
)
is(
  diffChr(
    letters, LETTERS, style="auto", format="auto", interactive=TRUE,
    pager="auto", term.colors=9
  )@etc@style,
  "StyleAnsi8NeutralYb"
)
is(
  diffChr(
    letters, LETTERS, style="auto", format="auto", interactive=TRUE,
    pager="auto", brightness='light', term.colors=500
  )@etc@style,
  "StyleAnsi256LightYb"
)
is(
  diffChr(
    letters, LETTERS, style="auto", format="html", interactive=TRUE,
    pager="auto", color.mode=c("rgb", ansi8="yb")
  )@etc@style,
  "StyleHtmlLightRgb"
)
is(
  diffChr(
    letters, LETTERS, style="auto", format="html", interactive=TRUE,
    pager="auto", color.mode=c("rgb", html="yb")
  )@etc@style,
  "StyleHtmlLightYb"
)
## - Palette Params ------------------------------------------------------------

all.equal(
  as.character(
    diffChr(
      letters, LETTERS, style="auto", format="ansi256",
      brightness=c("light", ansi256="dark")
  ) ),
  rdsf(500)
)
all.equal(
  as.character(
    diffChr(
      letters, LETTERS, style="auto", format="ansi256", brightness=c("dark")
  ) ),
  rdsf(500)
)
## - Style Validation ----------------------------------------------------------

s.f <- StyleFuns()
isTRUE(validObject(s.f))
s.f@word.insert <- function(x, y) NULL
try(validObject(s.f)) # word.insert

try(diffChr(1,2, format='html', style=list(scale=1:3)))
try(diffChr(1,2, format='html', style=list(html.output="a")))

## - Pallette w/ Objs ----------------------------------------------------------

pal <- PaletteOfStyles()
pal["raw", "neutral", "rgb"] <- list(new(pal[["raw", "neutral", "rgb"]]))

suppressWarnings(
  withCallingHandlers(
    invisible(diffChr(
      letters, LETTERS, format="raw", brightness="neutral", color.mode="rgb",
      palette.of.styles=pal, style=list(na.sub="NA")
    )),
    warning=function(e) writeLines(conditionMessage(e))
  )
)
## - External Files ------------------------------------------------------------

isTRUE(file_test("-f", diffobj_css()))
isTRUE(file_test("-f", diffobj_js()))
