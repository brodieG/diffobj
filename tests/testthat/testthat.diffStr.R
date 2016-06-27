library(diffobj)

context("structure")

test_that("lm models", {
  mdl1 <- lm(Sepal.Length ~ Sepal.Width, iris)
  mdl2 <- lm(Sepal.Length ~ Sepal.Width + Species, iris.3)
  diffStr(mdl1, mdl2, mode="sidebyside")
  diffStr(mdl1, mdl2, mode="sidebyside", brightness="light")
  # make sure that notice of suppressed stuff shows up
  diffStr(mdl1, mdl2, mode="sidebyside", line.limit=15)
  # interesting example below where the in-hunk word diff is too aggressive
  # preventing the eq-lines from atching
  diffStr(mdl1[7], mdl2[7], mode="sidebyside")
  diffPrint(mdl1, mdl2)
  diffStr(
    mdl1, mdl2, mode="sideby", bright="dark",
    pager=PagerSystemLess(flags="RX")
  )
  # Check that `max.level` shows up when using it

  diffStr(mdl1, mdl2, line.limit=40)

})
diffStr(cars, mtcars)
