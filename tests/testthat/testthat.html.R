context("html")

if(!identical(basename(getwd()), "testthat"))
  stop("Working dir does not appear to be /testthat, is ", getwd())

rdsf <- function(x)
  file.path(getwd(), "helper", "html", sprintf("%s.rds", x))

# Tests need to fleshed out

# Verify that internal css works

test_that("HTML Output Modes", {
  expect_equal_to_reference(
    as.character(
      diffPrint(
        letters[1:3], LETTERS[1:3],
        style=StyleHtmlLightYb(html.output="diff.only")
    ) ),
    rdsf(100)
  )
  expect_equal_to_reference(
    as.character(
      diffPrint(
        letters[1:6], LETTERS[1:6],
        style=StyleHtmlLightYb(html.output="diff.w.style")
    ) ),
    rdsf(200)
  )
  expect_equal_to_reference(
    as.character(
      diffPrint(
        letters[1:6], LETTERS[1:6],
        style=StyleHtmlLightYb(html.output="page")
    ) ),
    rdsf(300)
  )
  expect_equal_to_reference(
    as.character(
      diffPrint(
        letters[1:6], LETTERS[1:6], mode="unified",
        style=StyleHtmlLightYb(html.output="page")
    ) ),
    rdsf(350)
  )
})
test_that("Sub CSS", {
  # Mess up the CSS to test that we can change CSS file

  f <- tempfile()
  on.exit(unlink(f))
  cat("div.row {background-color: red;}\n", file=f)
  expect_equal_to_reference(
    as.character(
      diffPrint(
        letters, LETTERS,
        style=StyleHtmlLightYb(css=f, html.output="diff.w.style")
      )
    ),
    rdsf(400)
  )
})
test_that("Tag funs", {
  div_a <- div_f("A", c(color="red"))
  expect_equal(
    div_a(c("a", "b")),
    c(
      "<div class='A' style='color: red;'>a</div>",
      "<div class='A' style='color: red;'>b</div>"
    )
  )
  span_a <- span_f()
  expect_equal(span_a(c("a", "b")), c("<span>a</span>", "<span>b</span>"))
})

test_that("nchar", {
  expect_equal(nchar_html("<a href='blahblah'>25</a>"), 2)
  expect_equal(nchar_html("<a href='blahblah'>25&nbsp;</a>"), 3)
})
