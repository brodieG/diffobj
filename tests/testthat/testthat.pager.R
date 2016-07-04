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
