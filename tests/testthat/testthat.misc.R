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
