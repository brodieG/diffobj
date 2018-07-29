# tests designed to produce warnings

context("warnings")

test_that("Extra args for `str`", {
  a <- "hello"
  b <- "goodbye"

  expect_warning(diffStr(a, b, extra=list(comp.str="^")), "Specifying")
  expect_warning(diffStr(a, b, extra=list(comp="^")), "Specifying")
  expect_warning(diffStr(a, b, extra=list(indent.str="...")), "Specifying")
  expect_warning(diffStr(a, b, extra=list(indent="...")), "Specifying")
})
test_that("Max diffs", {
  # Max limit warnings work properly; these are not fully fleshed out

  A3 <- c("a b c", "d e f A B C D", "g h i", "f")
  B3 <- c("a b c", "xd e f E Q L S", "g h i", "q")

  expect_warning(diffChr(A3, B3, max.diffs=2), "Exceeded diff")
})
test_that("Overriden formals", {
  expect_warning(
    diffChr(letters, LETTERS, style=StyleRaw(), format="ansi8"),
    "Provided `style` argument will override the provided `format` argument"
  )
  expect_warning(
    diffChr(
      letters, LETTERS, style=StyleRaw(), format="ansi8", color.mode="rgb"
    ),
    "Provided `style` .* `format` and `color.mode` arguments"
  )
})
