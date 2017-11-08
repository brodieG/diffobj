context("diffObj")

if(!identical(basename(getwd()), "testthat"))
  stop("Working dir does not appear to be /testthat, is ", getwd())

rdsf <- function(x)
  file.path(getwd(), "helper", "diffObj", sprintf("%s.rds", x))

test_that("simple diffobj", {
  # no diff for print
  expect_equal_to_reference(as.character(diffObj(iris.s, iris.c)), rdsf(100))
  # no diff for str
  expect_equal_to_reference(
    as.character(diffObj(1:100, c(1:99, 200L))), rdsf(200)
  )
  # diffs for both and must pick one, first one is str, second is print
  expect_equal_to_reference(
    as.character(diffObj(mdl1[7], mdl2[7])), rdsf(300)
  )
  expect_equal_to_reference(as.character(diffObj(mdl1, mdl2)), rdsf(400))
})
test_that("fits or doesn't", {
  # Note, the first test used to favor str until we handicapped print
  expect_equal(
    diffObj(matrix(1:20, ncol=2), matrix(2:21, ncol=2), line.limit=5)@capt.mode,
    "str"
  )
  # test kinda slow, would be better to have one with smaller objects with print
  # methods

  expect_equal(
    diffObj(mdl1, mdl2, line.limit=15, mode='unified')@capt.mode, "print"
  )
  expect_equal(diffObj(1:1000, 1000:1, line.limit=5)@capt.mode, "str")
})

# Random exmaples to think through `diffObj` output

diffObj(
  pairlist("`logical(2L)` should be length 2 (is 3)"),
  pairlist("be length 2 (is 3)")
)

diffObj(
  pairlist("`matrix(integer(), nrow = 3)` should be matrix (is list)", "`list(character(1L), 1L)[[2]]` should be type \"integer-like\" (is \"character\")"),
  pairlist("be class \"matrix\" (is \"list\")", "be type \"integer-like\" (is \"character\") at index [[2]]")
)
