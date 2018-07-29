context("check")

test_that("is.less_flags", {
  expect_true(diffobj:::is.less_flags("RVXF"))
  expect_true(diffobj:::is.less_flags("rvxF"))
  expect_false(diffobj:::is.less_flags(c("rvxF", "RVXF")))
  expect_false(diffobj:::is.less_flags(23))
  expect_false(diffobj:::is.less_flags("rv xF"))
})
test_that("is.int.2L", {
  expect_true(diffobj:::is.int.2L(1:2))
  expect_true(diffobj:::is.int.2L(as.numeric(1:2)))
  expect_false(diffobj:::is.int.2L(c(1.3, 2.2)))
  expect_false(diffobj:::is.int.2L(1:3))
  expect_false(diffobj:::is.int.2L(c(1, NA)))
})
test_that("arg.funs", {
  expect_true(diffobj:::is.one.arg.fun(function(x) NULL))
  expect_true(diffobj:::is.one.arg.fun(function(x, y=5) NULL))
  expect_match(
    diffobj:::is.one.arg.fun(function(..., x) NULL),
    "cannot have `...` as "
  )
  expect_match(diffobj:::is.one.arg.fun(NULL), "is not a fun")
  expect_match(diffobj:::is.one.arg.fun(function() NULL), "have at least")
  expect_match(diffobj:::is.one.arg.fun(function(x, y) NULL), "cannot have any")

  expect_true(diffobj:::is.two.arg.fun(function(x, y) NULL))
  expect_true(diffobj:::is.two.arg.fun(function(x, y=5) NULL))
  expect_match(
    diffobj:::is.two.arg.fun(function(x, ..., y) NULL),
    "cannot have `...` as "
  )
  expect_match(diffobj:::is.two.arg.fun(NULL), "is not a fun")
  expect_match(diffobj:::is.two.arg.fun(function(x) NULL), "have at least")
  expect_match(
    diffobj:::is.two.arg.fun(function(x, y, z) NULL), "cannot have any"
  )
})
test_that("valid_object", {
  s.h <- StyleHtml()
  s.h@wrap <- TRUE
  expect_error(
    diffobj:::valid_object(s.h, "style", stop), "an invalid `StyleHtml` object"
  )
  pal <- PaletteOfStyles()
  pal["html", "light", "yb"] <- list(s.h)
  expect_error(
    diffChr(
      "A", "B", palette.of.styles=pal, style="auto", format="html",
      brightness="light", color.mode="yb"
    ),
    "`palette.of.styles` is an invalid"
  )
})
test_that("brightness", {
  expect_error(diffPrint(1:3, 3:6, brightness=NA), "must be character")
  expect_error(diffPrint(1:3, 3:6, brightness="red"), "may only contain values")
  expect_error(diffPrint(1:3, 3:6, brightness=c(raw='light')), "one empty name")
  expect_error(
    diffPrint(1:3, 3:6, brightness=c('light', 'dark')), "have names"
  )
})
test_that("misc", {
  expect_match(diffobj:::is.one.file.name(1), "must be character")
  expect_error(diffPrint(1:3, 2:6, extra="hello"), "must be a list")
  expect_error(diffPrint(1:3, 2:6, context=TRUE), "Argument `context` must")
  expect_error(diffPrint(1:3, 2:6, mode=1), "must be character")
  expect_error(diffPrint(1:3, 2:6, tab.stops=-1), "strictly positive")
  expect_error(diffPrint(1:3, 2:6, hunk.limit='hello'), "integer vector")
  expect_error(diffPrint(1:3, 2:6, guides='hello'), "or a function")
  expect_error(
    diffPrint(1:3, 2:6, guides=function(x, y, z) NULL), "cannot have any non"
  )
  expect_error(
    diffPrint(1:3, 2:6, trim='hello'), "TRUE, FALSE, or a function"
  )
  expect_error(
    diffPrint(1:3, 2:6, trim=function(x, y, z) NULL), "cannot have any non"
  )
  expect_error(diffPrint(1:3, 2:6, interactive='hello'), "must be TRUE or")
  expect_error(diffPrint(1:3, 2:6, max.diffs=1:10), "must be integer")
  expect_error(diffPrint(1:3, 2:6, tar.banner=1:10), "must be atomic")
  expect_error(diffPrint(1:3, 2:6, style=1:10), "must be \"auto\", a")
  expect_error(diffPrint(1:3, 2:6, pager=1:10), "must be one of")
  expect_error(diffPrint(1:3, 2:6, format=1:10), "must be character")
  expect_error(
    diffPrint(1:3, 2:6, palette.of.styles=1:10),
    "must be a `PaletteOfStyles`"
  )

})

