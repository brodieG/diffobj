library(diffobj)
library(ansistyle)

test_that("simple wrap", {
  txt1 <- c(
    "humpty dumpty sat on a wall and had a big fall",
    "humpty sat on a wall and dumped a big fall"
  )
  res1 <- diffobj:::wrap(txt, 10, TRUE)

  expect_identical(vapply(res1, paste0, character(1L), collapse=""), txt1)
  expect_equal(lapply(res1, nchar), list(c(10, 10, 10, 10, 6), c(10, 10, 10, 10, 2)))

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
      "humpty dumpty ", ansi_style("sat on a wall", "red"),
      " and had a big fall",
      ansi_style(
        ansi_style(
          "humpty sat on a wall and dumped a big fall",
          "green"
        ),
        "bgRed"
      ), "woohoo"
    ),
    paste0(
      ansi_style("hello ", "inverse"), "beautiful ", ansi_style("world", "blue")
    )
  )
  res3 <- diffobj:::wrap(txt3, 10, TRUE)

  expect_identical(vapply(res3, paste0, character(1L), collapse=""), txt3)
  expect_equal(
    lapply(res3, nchar),
    list(c(10, 15, 15, 10, 20, 10, 10, 10, 20, 4), c(19, 15, 6))
  )
  res4 <- diffobj:::wrap(txt3, 10, FALSE)
  expect_identical(vapply(res4, paste0, character(1L), collapse=""), txt3)
  expect_equal(
    lapply(res4, nchar),
    list(c(10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 4), c(10, 10, 10, 10))
  )
})
test_that("pad sign", {
  txt1 <- list(c("hello", "there"), c("how", "are you"), "sir!")
  expect_identical(
    diffobj:::sign_pad(txt1, "  ", FALSE, TRUE),
    list(c("  hello", "  there"), c("  how", "  are you"), "  sir!")
  )
  expect_identical(
    diffobj:::sign_pad(txt1, "+ ", FALSE, TRUE),
    list(c("\033[32m+ \033[39mhello", "\033[32m: \033[39mthere"), c("\033[32m+ \033[39mhow", "\033[32m: \033[39mare you"), "\033[32m+ \033[39msir!")
  )
  expect_identical(
    diffobj:::sign_pad(txt1, "- ", FALSE, TRUE),
    list(c("\033[31m- \033[39mhello", "\033[31m: \033[39mthere"), c("\033[31m- \033[39mhow", "\033[31m: \033[39mare you"), "\033[31m- \033[39msir!")
  )
  expect_error(diffobj:::sign_pad(txt1, "* ", FALSE, TRUE), "pad %in%")
} )
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
  options(crayon.enabled=TRUE)

  expect_equal(
    crayon::strip_style(
      diffobj:::strip_hz_control("\033[31ma\t\033[39mhello\tb", stops=10L)
    ),
    "a         hello     b"
  )


})
