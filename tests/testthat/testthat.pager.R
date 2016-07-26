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
