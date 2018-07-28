library(diffobj)
context("misc")

test_that("trim_str", {
  a <- structure("hello", class="A", xx="B")
  b <- structure(1:10, yy=a)
  long.string <- "I'm a string long enough to force wrapping under most cases so that I may be useful for tests andiamareallylongwordtoseehowwrappingbreakslongwordsthatexceed"
  obj <- list(
    a=a, b=b, c=1:50,
    d=long.string,
    e=list(1, structure(2, zz=list(a=1, b=list("a", ls=long.string))), e=letters)
  )
  # conditional because of issue113
  str.txt <- capture.output(str(obj))
  str.txt.w <- capture.output(str(obj, width=30L, strict.width="wrap"))

  if(
    getRversion() >= '3.5.0' && as.numeric(R.Version()[['svn rev']]) >= 73780
  ) {
    expect_equal(
      diffobj:::str_levels(str.txt, wrap=FALSE),
      c(0L, 1L, 2L, 1L, 2L, 3L, 1L, 1L, 1L, 2L, 2L, 3L, 4L, 4L, 5L, 5L, 2L)
    )
    expect_equal(
      diffobj:::str_levels(str.txt.w, wrap=TRUE),
      c(0L, 1L, 2L, 1L, 1L, 2L, 2L, 3L, 1L, 1L, 1L, 1L, 1L, 1L, 1L,
        1L, 1L, 2L, 2L, 3L, 3L, 4L, 4L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L,
        2L, 2L
      )
    )
  } else {
    expect_equal(
      diffobj:::str_levels(str.txt, wrap=FALSE),

      c(0L, 1L, 3L, 1L, 2L, 4L, 1L, 1L, 1L, 2L, 2L, 3L, 4L, 4L, 5L,  5L, 2L)
    )
    expect_equal(
      diffobj:::str_levels(str.txt.w, wrap=TRUE),
      c(0L, 1L, 1L, 3L, 1L, 1L, 2L, 2L, 4L, 4L, 1L, 1L, 1L, 1L, 1L,  1L, 1L, 1L, 1L, 2L, 2L, 3L, 3L, 4L, 4L, 5L, 5L, 5L, 5L, 5L, 5L,  5L, 5L, 2L, 2L)
    )
  }
  # cat(
  #   paste(
  #     format(substr(str.txt.w, 1, 20)), diffobj:::str_levels(str.txt.w, TRUE),
  #     sep=": "
  #   ),
  #   sep="\n"
  # )
})

test_that("rle_sub", {
  x <- c(1, 1, 1, 2, 2, 1, 1, 3, 3, 4, 4, 4, 5, 2, 2)
  r <- rle(x)
  expect_equal(diffobj:::rle_sub(r, r$values == 1L), list(1:3, 6:7))
  expect_equal(diffobj:::rle_sub(r, r$values == 2L), list(4:5, 14:15))
  expect_true(all(x[unlist(diffobj:::rle_sub(r, r$values == 1L))] == 1))
  expect_true(all(x[unlist(diffobj:::rle_sub(r, r$values == 2L))] == 2))
  expect_true(all(x[unlist(diffobj:::rle_sub(r, r$values == 3L))] == 3))
})

test_that("myers_simple", {

})

test_that("call funs", {
  # Failure case; assumes no S4 dispatch in testthat
  calls <- list(quote(a()), quote(b()), quote(notafunctionblah()))
  expect_equal(diffobj:::which_top(calls), length(calls))
  expect_warning(diffobj:::extract_call(calls, new.env()), "Unable to find")
})

test_that("lines", {
  old.val <- Sys.getenv("LINES", unset=NA)
  on.exit(
    if(is.na(old.val)) Sys.unsetenv("LINES") else Sys.setenv(LINES=old.val)
  )
  Sys.setenv(LINES="25")
  expect_equal(console_lines(), 25L)
  Sys.setenv(LINES="-25")
  expect_equal(console_lines(), 48L)
  Sys.unsetenv("LINES")
  expect_equal(console_lines(), 48L)
})

test_that("get_funs", {
  expect_identical(
    diffobj:::get_fun(quote(diffobj::diffPrint), .BaseNamespaceEnv),
    diffobj::diffPrint
  )
  expect_identical(
    diffobj:::get_fun(quote(diffobj:::diffPrint), .BaseNamespaceEnv),
    diffobj::diffPrint
  )
  expect_identical(
    diffobj:::get_fun(quote(diffPrint), getNamespace("diffobj")),
    diffobj::diffPrint
  )
  expect_warning(
    gf <- diffobj:::get_fun(quote(notAFunction), getNamespace("diffobj")),
    "Unable to find function"
  )
  expect_identical(gf, NULL)
})
test_that("trimws2", {
  expect_equal(diffobj:::trimws2("hello world"),  "hello world")
  expect_equal(diffobj:::trimws2("  hello world"),  "hello world")
  expect_equal(diffobj:::trimws2("  hello world  "),  "hello world")
  expect_equal(diffobj:::trimws2("  hello world  ", 'left'), "hello world  ")
  expect_equal(diffobj:::trimws2("  hello world  ", 'right'), "  hello world")

  expect_error(diffobj:::trimws2("  hello world  ", 'banana'), "is wrong")
})
