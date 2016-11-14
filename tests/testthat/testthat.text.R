library(diffobj)

context("text")
test_that("simple wrap", {
  txt1 <- c(
    "humpty dumpty sat on a wall and had a big fall",
    "humpty sat on a wall and dumped a big fall"
  )
  res1 <- diffobj:::wrap(txt1, 10, TRUE)

  expect_identical(
    gsub(" *$", "", vapply(res1, paste0, character(1L), collapse="")), txt1
  )
  expect_equal(lapply(res1, nchar), list(rep(10L, 5L), rep(10L, 5L)))

  txt2 <- "hello world!"
  expect_identical(
    unlist(diffobj:::wrap(txt2, nchar(txt2), TRUE)),
    txt2
  )
  expect_identical(
    paste0(unlist(diffobj:::wrap(txt2, nchar(txt2) / 2, TRUE)), collapse=""),
    txt2
  )
})
test_that("wrap with escape sequences", {
  txt3 <- c(
    paste0(
      "humpty dumpty ", crayon::style("sat on a wall", "red"),
      " and had a big fall",
      crayon::style(
        crayon::style(
          "humpty sat on a wall and dumped a big fall",
          "green"
        ),
        "bgRed"
      ), "woohoo"
    ),
    paste0(
      crayon::style("hello ", "inverse"), "beautiful ",
      crayon::style("world", "blue")
    )
  )
  res3 <- diffobj:::wrap(txt3, 10, TRUE)

  expect_identical(
    crayon::strip_style(
      gsub(" *$", "", vapply(res3, paste0, character(1L), collapse=""))
    ),
    crayon::strip_style(txt3)
  )
  expect_equal(
    lapply(res3, crayon::col_nchar),
    list(rep(10L, 10L), rep(10L, 3L))
  )
})
test_that("strip hz whitespace", {
  old.opt <- options(crayon.enabled=FALSE)
  on.exit(options(old.opt))
  expect_equal(diffobj:::strip_hz_control("a\tb", stops=4L), "a   b")
  expect_equal(diffobj:::strip_hz_control("ab\t", stops=4L), "ab  ")
  expect_equal(diffobj:::strip_hz_control("a\tb\t", stops=4L), "a   b   ")
  expect_equal(diffobj:::strip_hz_control("\ta\tb\t", stops=4L), "    a   b   ")
  expect_equal(
    diffobj:::strip_hz_control("\ta\tb\t", stops=c(2L, 4L)), "  a   b   "
  )
  expect_equal(
    diffobj:::strip_hz_control(c("ab\t", "\ta\tb\t"), stops=4L),
    c("ab  ", "    a   b   ")
  )
  # recall that nchar("\033") == 1
  expect_equal(
    diffobj:::strip_hz_control("\033[31ma\t\033[39mhello\tb", stops=10L),
    "\033[31ma    \033[39mhello          b"
  )
  # carriage returns

  expect_equal(
    diffobj:::strip_hz_control("hellothere\rHELLO"),
    "HELLOthere"
  )
  expect_equal(
    diffobj:::strip_hz_control(
      c("hellothere\rHELLO", "000\r12345678\rabcdef\rABC")
    ),
    c("HELLOthere", "ABCdef78")
  )
  # newlines

  expect_equal(
    diffobj:::strip_hz_control(c("a", "", "\n", "a\nb")),
    c("a", "", "", "a", "b")
  )
  # with colors

  options(crayon.enabled=TRUE)

  expect_equal(
    crayon::strip_style(
      diffobj:::strip_hz_control("\033[31ma\t\033[39mhello\tb", stops=10L)
    ),
    "a         hello     b"
  )
  test.chr <- paste0(
    crayon::red(crayon::`%+%`("000",  crayon::bgBlue("\r12345678"))),
    "\rabcdef", crayon::green("\rABC")
  )
  # visually inspect these

  # cat("\n")
  # cat(test.chr, sep="\n")
  res <- diffobj:::strip_hz_control(test.chr)
  # cat(res, sep="\n")
  expect_equal(crayon::strip_style(res), "ABCdef78")

  # Mix tabs and carriage returns, visual inspection assumes terminal tab
  # stops at 8L; note output not exactly the same since it seems tabs don't
  # ovewrite prior screen state whereas spaces do

  test.chr.2 <- paste0(
    crayon::red(crayon::`%+%`("000", crayon::bgBlue("\r123\t456\t78"))),
    "\rab\tcd f", crayon::green("\rABC")
  )
  # cat("\n")
  # cat(test.chr.2, sep="\n")
  res.2 <- diffobj:::strip_hz_control(test.chr.2, stops=8L)
  # cat(res.2, sep="\n")

  expect_equal(crayon::strip_style(res.2), "ABC     cd f    78")

  # multi line

  test.chr.3 <- c(test.chr, test.chr.2)
  # cat("\n")
  res.3 <- diffobj:::strip_hz_control(test.chr.3)
  # cat(res.3, sep="\n")
  # cat(test.chr.3, sep="\n")

  expect_equal(crayon::strip_style(res.3), c("ABCdef78", "ABC     cd f    78"))
})
