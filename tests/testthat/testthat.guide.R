
test_that("detect_2d_guides", {
   iris.dply <- c("Source: local data frame [150 x 5]", "Groups: Species [3]", "", "   Sepal.Length Sepal.Width", "          (dbl)       (dbl)", "1           5.1         3.5", "2           4.9         3.0", "3           4.7         3.2", "4           4.6         3.1", "5           5.0         3.6", "6           5.4         3.9", "7           4.6         3.4", "8           5.0         3.4", "9           4.4         2.9", "10          4.9         3.1", "..          ...         ...", "Variables not shown: Petal.Length", "  (dbl), Petal.Width (dbl), Species", "  (fctr)")
   expect_equal(diffobj:::detect_2d_guides(iris.dply), 3:5)
   old.opt <- options(width=40)
   on.exit(options(old.opt))
   expect_equal(diffobj:::detect_2d_guides(capture.output(iris)), c(1, 152))
   expect_equal(
     diffobj:::detect_2d_guides(
       capture.output(matrix(1:100, nrow=5)), c(1, 7, 13, 19)
   ) )
})
test_that("detect_list_guides", {
  l.1 <- list(1, 1:3, matrix(1:3, 1))
  l.2 <- list(a=1, list(1:3, b=4, c=list(1, b=2)), matrix(1:3, 1))
  c.l.2 <- capture.output(l.2)
  c.l.2 <- capture.output(lst.1)
  # cbind(c.l.2, seq_along(c.l.2) %in% diffobj:::detect_list_guides(c.l.2))
  expect_equal(diffobj:::detect_list_guides(capture.output(l.1)), c(1, 4, 7))
  expect_equal(
    diffobj:::detect_list_guides(capture.output(l.2)),
    c(1, 5, 8, 12, 15, 20)
  )
})
test_that("detect_array_guides", {
  a.1 <- array(1:6, dim=c(2, 1, 3))
  a.2 <- array(1:6, dim=c(2, 1, 3), dimnames=list(NULL, "X", LETTERS[1:3]))
  a.3 <- array(
    1:6, dim=c(2, 1, 3),
    dimnames=list(rows=NULL, cols="X", LETTERS[1:3])
  )
  a.4 <- `attr<-`(a.3, "hello", "random attribute")
  a.5 <- array(1:36, dim=c(6, 2, 3))
  c.a.1 <- capture.output(a.1)
  c.a.2 <- capture.output(a.2)
  c.a.3 <- capture.output(a.3)
  c.a.4 <- capture.output(a.4)
  c.a.5 <- capture.output(a.5)
  # # helper funs to vizualize the guide line detection
  # viz_dag <- function(capt, obj)
  #   cbind(
  #     capt,
  #     seq_along(capt) %in% diffobj:::detect_array_guides(capt, dimnames(obj))
  #   )
  # viz_dag(c.a.1, a.1)
  # viz_dag(c.a.2, a.2)
  # viz_dag(c.a.3, a.3)
  # viz_dag(c.a.4, a.4)
  viz_dag(c.a.5, a.5)
  expect_equal(
    diffobj:::detect_array_guides(c.a.1, dimnames(a.1)),
    c(1L, 2L, 3L, 7L, 8L, 9L, 13L, 14L, 15L)
  )
  expect_equal(
    diffobj:::detect_array_guides(c.a.2, dimnames(a.2)),
    c(1L, 2L, 3L, 7L, 8L, 9L, 13L, 14L, 15L)
  )
  expect_equal(
    diffobj:::detect_array_guides(c.a.3, dimnames(a.3)),
    c(1L, 2L, 3L, 4L, 8L, 9L, 10L, 11L, 15L, 16L, 17L, 18L)
  )
  expect_equal(
    diffobj:::detect_array_guides(c.a.4, dimnames(a.4)),
    c(1L, 2L, 3L, 4L, 8L, 9L, 10L, 11L, 15L, 16L, 17L, 18L)
  )
})
