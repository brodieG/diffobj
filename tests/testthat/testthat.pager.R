library(diffobj)
context("pager")

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
  old.viewer <- options(viewer=viewer)
  on.exit(options(old.viewer))
  with_mock(
    "utils::browseURL"=function(url) "browser",
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
  # Note that readline just proceeds in non-interactive mode

  with_mock(
    "diffobj:::interactive"=function() FALSE,
    "diffobj:::readline"=function(...) warning("readline"),
    {
      expect_error(make_blocking("hello"), "must be a function")
      expect_error(make_blocking(identity, letters), "must be character\\(1L")
      expect_error(make_blocking(identity, "a", "a"), "must be TRUE")

      expect_warning(res <- make_blocking(sum)(1:10), "readline")
      expect_equal(sum(1:10), res)
    }
  )
})

test_that("html page output", {
  pager <- PagerBrowser(pager=function(x) cat(readLines(x), sep="\n"))
  expect_equal(
    capture.output(show(diffChr("A", "B", pager=pager, style=StyleRaw()))),
    c("< \"A\"       > \"B\"     ", "@@ 1 @@     @@ 1 @@   ", "< A         > B       ")
  )
  pager.warn <- PagerBrowser(
    pager=function(x) cat(readLines(x), sep="\n"),
  )
  expect_warning(
    capture.output(
      show(
        diffChr(
          "A", "B", pager=pager.warn, format="html", style=list(js="notafile")
    ) ) ),
    "Unable to read"
  )
})
