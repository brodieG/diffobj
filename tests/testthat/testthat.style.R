context("style")

if(!identical(basename(getwd()), "testthat"))
  stop("Working dir does not appear to be /testthat, is ", getwd())

rdsf <- function(x)
  file.path(getwd(), "helper", "style", sprintf("%s.rds", x))

test_that("style palette", {
  expect_equal_to_reference(
    capture.output(diffobj:::display_ansi_256_styles()),
    rdsf(100)
  )
})
test_that("crayon settings", {
  # make sure crayon options are appropriately overriden
  old.opt <- options(crayon.enabled=FALSE)
  on.exit(options(old.opt))
  expect_identical(crayon::green("green"), "green")
  # should have ANSI coloring despite crayon disabled
  expect_equal_to_reference(
    as.character(diffChr(letters[1:3], LETTERS[1:3])), rdsf(200)
  )
  expect_identical(crayon::green("green"), "green")
})
test_that("palette of styles", {
  pos <- PaletteOfStyles()
  expect_identical(
    pos[["ansi256", "light", "rgb"]],
    getClassDef("StyleAnsi256LightRgb", package="diffobj", inherits=FALSE)
  )
  expect_equal_to_reference(
    capture.output(show(pos)), rdsf(300)
  )
  expect_equal_to_reference(
    capture.output(summary(pos)), rdsf(400)
  )
})
test_that("Palette Subset", {
  p.o.s <- PaletteOfStyles()
  p.o.s["ansi256", "light", "yb"] <- list(StyleRaw())
  expect_equal(c(p.o.s["ansi256", "light", "yb"]@data), list(StyleRaw()))
  expect_equal(p.o.s[["ansi256", "light", "yb"]], StyleRaw())
})
test_that("auto style selection", {
  expect_error(
    diffChr(letters, LETTERS, style="auto", format="xml"),
    "`format` must be one of"
  )
  expect_is(
    diffChr(
      letters, LETTERS, style="auto", format="auto", brightness="light",
      term.colors=256
    )@etc@style,
    "StyleAnsi256LightYb"
  )
  expect_is(
    diffChr(
      letters, LETTERS, style="auto", format="auto", brightness="light",
      term.colors=8
    )@etc@style,
    "StyleAnsi8NeutralYb"
  )
  expect_is(
    diffChr(
      letters, LETTERS, style="auto", format="auto", interactive=FALSE,
      term.colors=1
    )@etc@style,
    "StyleRaw"
  )
  expect_is(
    diffChr(
      letters, LETTERS, style="auto", format="auto", interactive=TRUE,
      term.colors=1  # note pager off by default in tests
    )@etc@style,
    "StyleRaw"
  )
  expect_is(
    diffChr(
      letters, LETTERS, style="auto", format="auto", interactive=TRUE,
      pager="auto", term.colors=1
    )@etc@style,
    "StyleHtml"
  )
  expect_is(
    diffChr(
      letters, LETTERS, style="auto", format="auto", interactive=TRUE,
      pager="auto", term.colors=9
    )@etc@style,
    "StyleAnsi8NeutralYb"
  )
  expect_is(
    diffChr(
      letters, LETTERS, style="auto", format="auto", interactive=TRUE,
      pager="auto", brightness='light', term.colors=500
    )@etc@style,
    "StyleAnsi256LightYb"
  )
  expect_is(
    diffChr(
      letters, LETTERS, style="auto", format="html", interactive=TRUE,
      pager="auto", color.mode=c("rgb", ansi8="yb")
    )@etc@style,
    "StyleHtmlLightRgb"
  )
  expect_is(
    diffChr(
      letters, LETTERS, style="auto", format="html", interactive=TRUE,
      pager="auto", color.mode=c("rgb", html="yb")
    )@etc@style,
    "StyleHtmlLightYb"
  )
})
test_that("palette param selection", {
  expect_equal_to_reference(
    as.character(
      diffChr(
        letters, LETTERS, style="auto", format="ansi256",
        brightness=c("light", ansi256="dark")
    ) ),
    rdsf(500)
  )
  expect_equal_to_reference(
    as.character(
      diffChr(
        letters, LETTERS, style="auto", format="ansi256", brightness=c("dark")
    ) ),
    rdsf(500)
  )
})
test_that("style fun validation", {
  s.f <- StyleFuns()
  expect_true(validObject(s.f))
  s.f@word.insert <- function(x, y) NULL
  expect_error(validObject(s.f), "word.insert")
})
test_that("palette with objects", {
  pal <- PaletteOfStyles()
  pal["raw", "neutral", "rgb"] <- list(new(pal[["raw", "neutral", "rgb"]]))

  expect_warning(
    diffChr(
      letters, LETTERS, format="raw", brightness="neutral", color.mode="rgb",
      palette.of.styles=pal, style=list(na.sub="NA")
    ),
    "arguments cannot be applied"
  )
})
test_that("external files", {
  expect_true(file_test("-f", diffobj_css()))
  expect_true(file_test("-f", diffobj_js()))
})
