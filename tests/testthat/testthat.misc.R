library(diffobj)

test_that("list_depth", {
  lst.0 <- lst.1 <- list(list(list(1)))
  expect_identical(diffobj:::list_depth(lst.0), 3L)
  expect_identical(diffobj:::list_depth(list(list(list()))), 3L)
  expect_identical(diffobj:::list_depth(list(1, 2, lst.0)), 4L)
  expect_identical(diffobj:::list_depth(list(list(lst.0), 2, lst.0)), 5L)
  attr(lst.1, "boo") <- lst.0
  expect_identical(diffobj:::list_depth(lst.1), 4L)
  lst.2 <- lst.1
  attr(lst.2, "boo")[[1L]][[2L]] <- lst.0
  expect_identical(diffobj:::list_depth(lst.2), 6L)
  lst.3 <- lst.0
  attributes(lst.3) <- list(xx=a ~ b, zz=c("a", "b"))
  attributes(lst.3) <- list(xx=1:3, zz=c("a", "b"))
  attr(attr(lst.3, "xx"), "yy") <- list(1:2)

})

test_that("trim_str", {
  a <- structure("hello", class="A", xx="B")
  b <- structure(1:10, yy=a)
  long.string <- "I'm a string long enough to force wrapping under most cases so that I may be useful for tests andiamareallylongwordtoseehowwrappingbreakslongwordsthatexceed"
  obj <- list(
    a=a, b=b, c=1:50,
    d=long.string,
    e=list(1, structure(2, zz=list(a=1, b=list("a", ls=long.string))), e=letters)
  )
  str.txt <- capture.output(str(obj))
  expect_equal(
    diffobj:::str_levels(str.txt, wrap=FALSE),
    c(0L, 1L, 3L, 1L, 2L, 4L, 1L, 1L, 1L, 2L, 2L, 3L, 4L, 4L, 5L,  5L, 2L)
  )
  str.txt.w <- capture.output(str(obj, width=30L, strict.width="wrap"))
  expect_equal(
    diffobj:::str_levels(str.txt.w, wrap=TRUE),
    c(0L, 1L, 1L, 3L, 1L, 1L, 2L, 2L, 4L, 4L, 1L, 1L, 1L, 1L, 1L,  1L, 1L, 1L, 1L, 2L, 2L, 3L, 3L, 4L, 4L, 5L, 5L, 5L, 5L, 5L, 5L,  5L, 5L, 2L, 2L)
  )
  # cat(
  #   paste(
  #     format(substr(str.txt.w, 1, 20)), diffobj:::str_levels(str.txt.w, TRUE),
  #     sep=": "
  #   ),
  #   sep="\n"
  # )
})

