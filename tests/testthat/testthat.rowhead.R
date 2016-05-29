library(testthat)

context("Row Head")

test_that("Atomic", {
  old.opt <- options(width=80L)
  on.exit(options(old.opt))
  set.seed(1)
  x <- capture.output(1:50)
  y <- capture.output(factor(sample(letters, 50, replace=TRUE)))

  expect_equal(
    diffobj:::strip_atomic_rh(x),
    c(" 1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25", "26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50")
  )
  expect_equal(
    diffobj:::strip_atomic_rh(y),
    c("g j o x f x y r q b f e r j u m s z j u y f q d g k a j w i m p m e v r u c", "s k v q u o n u a m t s", "Levels: a b c d e f g i j k m n o p q r s t u v w x y z")
  )
})


