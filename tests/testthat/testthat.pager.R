library(diffobj)
context("pager")

txtf <- function(x)
  file.path(getwd(), "helper", "pager", sprintf("%s.txt", x))

# void pager, doesn't do anything, just to test side effect of writing to file
void <- function(x) NULL
test_that("Specifying pager", {
  style <- gdo("diffobj.style")
  if(is.null(style)) style <- StyleAnsi8NeutralYb()
  style@pager@file.ext <- "xyz"  # make pager identifiable
  expect_equal(
    diffChr(
      letters, LETTERS, style=style, pager="auto", interactive=TRUE
    )@etc@style@pager@file.ext,
    "xyz"
  )
  expect_equal(
    diffChr(
      letters, LETTERS, style=style, pager="off", interactive=TRUE
    )@etc@style@pager,
    PagerOff()
  )
  expect_identical(
    diffChr(
      letters, LETTERS, style=style, pager="auto", interactive=FALSE
    )@etc@style@pager,
    PagerOff()
  )
})
test_that("System Pagers", {
  less.orig <- Sys.getenv("LESS")
  pager_mock <- function(...) {
    warning(Sys.getenv("LESS"))
    42
  }
  expect_is(PagerSystem(), "PagerSystem")
  expect_is(
    pg.less <- PagerSystemLess(pager=pager_mock, flags="VWF"),
    "PagerSystemLess"
  )
  expect_warning(res <- pg.less@pager(), "VWF$")
  expect_equal(res, 42)
  expect_equal(less.orig, Sys.getenv("LESS"))
  expect_equal(PagerSystemLess(pager=pager_mock)@flags, "R")
  expect_error(PagerSystemLess(pager=pager_mock, flags=letters))
})
test_that("use_pager", {
  with_mock(
    "diffobj:::console_lines"=function(...) 10L, {
      expect_true(diffobj:::use_pager(PagerSystem(threshold=0L), 1L))
      expect_false(diffobj:::use_pager(PagerSystem(threshold=50L), 25L))
      expect_true(diffobj:::use_pager(PagerSystem(threshold=-1L), 25L))
    }
  )
})
test_that("Setting LESS var", {
  less.orig <- Sys.getenv("LESS")
  old.opt <- options(crayon.enabled=FALSE)  # problems with crayon and LESS
  on.exit({
    diffobj:::reset_less_var(less.orig) # should be tested..., but super simple
    options(old.opt)
  })
  # Here we change the LESS variable even though we're mocking getenv
  with_mock(
    Sys.getenv=function(...) NA_character_,
    expect_true(is.na(diffobj:::set_less_var("XF")))
  )
  expect_equal(Sys.getenv("LESS"), "-XF")
  with_mock(
    Sys.getenv=function(...) "-X -F",
    expect_equal(diffobj:::set_less_var("VP"), "-X -F")
  )
  expect_equal(Sys.getenv("LESS"), "-X -FVP")
  diffobj:::reset_less_var("-XF")
  expect_equal(Sys.getenv("LESS"), "-XF")
  diffobj:::reset_less_var(NA_character_)
  expect_equal(Sys.getenv("LESS"), "")

  with_mock(
    Sys.getenv=function(...) "-XF",
    expect_equal(diffobj:::set_less_var("V"), "-XF")
  )
  expect_equal(Sys.getenv("LESS"), "-XFV")

  with_mock(
    Sys.getenv=function(...) NULL,
    expect_warning(diffobj:::set_less_var("V"), "Unable to set")
  )
})
test_that("viewer vs browser", {
  viewer <- function(x) "viewer"
  old.external <- options(viewer=viewer, browser=function(url) "browser")
  on.exit(options(old.external))
  with_mock(
    "diffobj::make_blocking"=identity, {
      pager <- PagerBrowser()
      expect_equal(pager@pager("blah"), "viewer")
      options(viewer=NULL)
      expect_equal(pager@pager("blah"), "browser")
      options(viewer=function(x) stop("viewer error"))
      expect_warning(res <- pager@pager("blah"), "IDE viewer")
      expect_equal(res, "browser")
    }
  )
})
test_that("blocking", {
  # Note that readline just proceeds in non-interactive mode, which is why we
  # need the mock here

  with_mock(
    "diffobj:::interactive"=function() FALSE,
    "diffobj:::readline"=function(...) warning("readline"),
    {
      expect_error(make_blocking("hello"), "must be a function")
      expect_error(make_blocking(identity, letters), "must be character\\(1L")
      expect_error(make_blocking(identity, "a", "a"), "must be TRUE")

      expect_warning(res <- make_blocking(sum)(1:10), "readline")
      expect_equal(sum(1:10), res)
      expect_true(
        withVisible(
          suppressWarnings(make_blocking(sum, invisible=FALSE)(1:10))
        )[['visible']]
      )
  })
  with_mock(
    "diffobj:::interactive"=function() TRUE,
    "diffobj:::readline"=function(...) warning("readline"),
    {
      expect_warning(
        show(
          diffChr(
            "a", "b", format='raw',
            pager=list(pager=void, make.blocking=TRUE, threshold=0)
          )
        ),
        "readline"
      )
      expect_warning(
        show(
          diffChr(
            "a", "b", format='html',
            pager=list(pager=void, make.blocking=NA, threshold=0)
        ) ),
        "readline"
      )
      expect_warning(
        show(diffChr("a", "b", format='html', pager=list(pager=void))),
        "readline"
      )
      f <- tempfile()
      on.exit(unlink(f))
      expect_warning(
        show(
          diffChr(
            "a", "b", format='html',
            pager=list(pager=void, make.blocking=NA, file.path=f)
        ) ),
        NA
      )
      expect_warning(
        show(
          diffChr(
            "a", "b", format='html',
            pager=list(pager=void, make.blocking=FALSE, file.path=f)
        ) ),
        NA
      )
      expect_warning(
        show(
          diffChr("a", "b", format='html', pager=list(pager=void, file.path=f))
        ),
        NA
      )
} )})
test_that("html page output", {
  pager <- PagerBrowser(
    pager=function(x) cat(readLines(x), sep="\n"), make.blocking=FALSE
  )
  expect_equal(
    capture.output(show(diffChr("A", "B", pager=pager, style=StyleRaw()))),
    c("< \"A\"       > \"B\"     ", "@@ 1 @@     @@ 1 @@   ", "< A         > B       ")
  )
  pager.warn <- PagerBrowser(
    pager=function(x) cat(readLines(x), sep="\n"), make.blocking=FALSE
  )
  expect_error(
    diffChr(
      "A", "B", pager=pager.warn, format="html", style=list(js="notafile")
    ),
    "Unable to instantiate `Style` object: Argument `js` .* is not a file"
  )
  expect_error(
    diffChr(
      "A", "B", pager=pager.warn, format="html", style=list(css="notafile")
    ),
    "Unable to instantiate `Style` object: Argument `css` .* is not a file"
  )
  # Create objects that bypass the validation

  style.obj.1 <- style.obj.2 <- StyleHtmlLightYb()
  style.obj.1@css <- "notafile"
  style.obj.2@js <- "notafile"

  expect_warning(
    capture.output(
      show(diffChr("A", "B", pager=pager.warn, style=style.obj.1))
    ),
    "Unable to read provided css file"
  )
  expect_warning(
    capture.output(
      show(diffChr("A", "B", pager=pager.warn, style=style.obj.2))
    ),
    "Unable to read provided js file"
  )
})
test_that("pager_is_less", {
  is.less <- pager_is_less()
  expect_true(diffobj:::is.TF(is.less))

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

      expect_false(diffobj:::pager_opt_default())
      expect_true(pager_is_less())
    })
  }
  if(diffobj:::is.chr.1L(sys.cat) && file_test("-x", sys.cat)) {
    local({
      old.opt <- options(pager=sys.cat)
      on.exit(options(old.opt))

      expect_false(diffobj:::pager_opt_default())
      expect_false(pager_is_less())
    })
  }
  ## force some checks

  local({
    old.opt <- options(pager=NULL)
    on.exit(options(old.opt))
    expect_false(pager_is_less())
  })
  expect_false(diffobj:::file_is_less(tempfile()))
})
test_that("file.path", {
  f <- tempfile()
  show(
    diffChr(
      "A", "B", format='raw',
      pager=list(pager=void, file.path=f, threshold=0L)
  ) )
  expect_equal(
    readLines(f),
    c("< \"A\"       > \"B\"     ", "@@ 1 @@     @@ 1 @@   ",
      "< A         > B       ")
  )
  expect_error(
    show(
      diffChr(
        "A", "B", format='raw',
        pager=list(pager=void, file.path=NA, threshold=0L)
    ) ),
    NA
  )
  expect_error(Pager(file.path=letters), "must be length 1")
  expect_error(Pager(file.path=1), "must be character")
})
test_that("basic pager", {
  f <- tempfile()
  on.exit(unlink(f))
  expect_known_output(
    show(
      diffChr(
        1, 2, pager=Pager(file.path=f, threshold=0L),
        format='raw'
      )
    ),
    txtf(100)
  )
  expect_equal(readLines(txtf(100)), readLines(f))
  unlink(f)
})
test_that("format-pager interaction", {
  old.opt <- options(crayon.colors=7)
  crayon::num_colors(TRUE)
  on.exit({
    options(old.opt)
    crayon::num_colors(TRUE)
  })
  expect_is(
    diffChr(1, 2, format='auto', pager="on", interactive=TRUE)@etc@style,
    "StyleHtml"
  )
  expect_is(
    diffChr(1, 2, format='auto', pager="on", interactive=FALSE)@etc@style,
    "StyleRaw"
  )
  expect_is(
    diffChr(
      1, 2, format='auto', pager=PagerBrowser(), interactive=FALSE
    )@etc@style,
    "StyleHtml"
  )
})
test_that("format-pager interaction 2", {
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

  expect_is(
    diffChr(1, 2, format='auto', pager='on', interactive=TRUE)@etc@style,
    "StyleHtml"
  )
  expect_is(
    diffChr(1, 2, format='auto', interactive=FALSE)@etc@style,
    "StyleAnsi"
  )
  Sys.setenv(RSTUDIO_TERM='HELLO')
  crayon::num_colors(TRUE)

  expect_is(
    diffChr(1, 2, format='auto', pager='on', interactive=TRUE)@etc@style,
    "StyleAnsi"
  )
})
test_that("format-pager interaction 3", {
  expect_is(
    diffPrint(1:3, 3:1, format='auto', interactive=FALSE, term.colors=1)@etc@style,
    "StyleRaw"
  )
  expect_is(
    diffPrint(1:3, 3:1, format='auto', interactive=FALSE, term.colors=8)@etc@style,
    "StyleAnsi"
  )
})
test_that("Default pager writes to screen", {
  # issue132 thanks Bill Dunlap

  f <- tempfile()
  on.exit(unlink(f))
  writeLines("hello world", f)

  expect_equal(capture.output(new("Pager")@pager(f)), "hello world")
})
