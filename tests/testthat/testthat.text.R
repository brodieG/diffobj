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
