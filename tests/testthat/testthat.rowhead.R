library(testthat)

context("Row Head")

.mx.base <- matrix(
  c(
    "averylongwordthatcanlahblah", "causeasinglewidecolumnblah",
    "matrixtowrapseveraltimes", "inarrowscreen", "onceuponatime",
    "agreenduckflew", "overthemountains", "inalongofantelopes",
    "ineedthreemore", "entriesactually", "nowonlytwomore", "iwaswrongearlier"
  ),
  nrow=3, ncol=4
)
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
test_that("Matrix", {
  mx1 <- mx2 <- matrix(1:3, 3)
  expect_equal(
    diffobj:::strip_matrix_rh(capture.output(mx1), dimnames(mx1)),
    c("     [,1]", "   1", "   2", "   3")
  )
  # shouldn't strip headers from attributes
  attr(mx2, "blah") <- matrix(1:2, 2)
  expect_equal(
    diffobj:::strip_matrix_rh(capture.output(mx2), dimnames(mx2)),
    c("     [,1]", "   1", "   2", "   3", "attr(,\"blah\")", "     [,1]", "[1,]    1", "[2,]    2")
  )
  # Matrices that wrap

  mx3 <- mx4 <- mx5 <- mx6 <- .mx.base
  old.opt <- options(width=30)
  on.exit(options(old.opt))

  expect_equal(
    diffobj:::strip_matrix_rh(capture.output(mx3), dimnames(mx3)),
    c("     [,1]                         ", "\"averylongwordthatcanlahblah\"", "\"causeasinglewidecolumnblah\" ", "\"matrixtowrapseveraltimes\"   ", "     [,2]            ", "\"inarrowscreen\" ", "\"onceuponatime\" ", "\"agreenduckflew\"", "     [,3]                ", "\"overthemountains\"  ", "\"inalongofantelopes\"", "\"ineedthreemore\"    ", "     [,4]              ", "\"entriesactually\" ", "\"nowonlytwomore\"  ", "\"iwaswrongearlier\"")
  )
  # Add rownames; should no longer strip

  rownames(mx4) <- 2:4
  expect_equal(
    diffobj:::strip_matrix_rh(capture.output(mx4), dimnames(mx4)),
    capture.output(mx4)
  )
  # Attributes don't have stuff stripped

  attr(mx6, "blah") <- letters[1:15]

  expect_equal(
    diffobj:::strip_matrix_rh(capture.output(mx6), dimnames(mx6)),
    c("     [,1]                         ", "\"averylongwordthatcanlahblah\"", "\"causeasinglewidecolumnblah\" ", "\"matrixtowrapseveraltimes\"   ", "     [,2]            ", "\"inarrowscreen\" ", "\"onceuponatime\" ", "\"agreenduckflew\"", "     [,3]                ", "\"overthemountains\"  ", "\"inalongofantelopes\"", "\"ineedthreemore\"    ", "     [,4]              ", "\"entriesactually\" ", "\"nowonlytwomore\"  ", "\"iwaswrongearlier\"", "attr(,\"blah\")", " [1] \"a\" \"b\" \"c\" \"d\" \"e\" \"f\"", " [7] \"g\" \"h\" \"i\" \"j\" \"k\" \"l\"", "[13] \"m\" \"n\" \"o\"")
  )
})
test_that("Table", {
  old.opt <- options(width=30)
  on.exit(options(old.opt))

  # Data frames

  df1 <- as.data.frame(.mx.base)
  expect_equal(
    diffobj:::strip_table_rh(capture.output(df1)),
    c("                           V1", "averylongwordthatcanlahblah", " causeasinglewidecolumnblah", "   matrixtowrapseveraltimes", "              V2", " inarrowscreen", " onceuponatime", "agreenduckflew", "                  V3", "  overthemountains", "inalongofantelopes", "    ineedthreemore", "                V4", " entriesactually", "  nowonlytwomore", "iwaswrongearlier")
  )
  df2 <- df1[c(2, 1, 3), ]

  expect_equal(
    diffobj:::strip_table_rh(capture.output(df2)),
    capture.output(df2)
  )
  # Rownames that start from one and sequential, should get stripped; also,
  # colon allowed

  df3 <- df1
  rownames(df3) <- paste0(1:3, ":")
  expect_equal(
    diffobj:::strip_table_rh(capture.output(df3), dimnames(df3)),
    c("   [,1]                         ", "\"averylongwordthatcanlahblah\"", "\"causeasinglewidecolumnblah\" ", "\"matrixtowrapseveraltimes\"   ", "   [,2]            ", "\"inarrowscreen\" ", "\"onceuponatime\" ", "\"agreenduckflew\"", "   [,3]                ", "\"overthemountains\"  ", "\"inalongofantelopes\"", "\"ineedthreemore\"    ", "   [,4]              ", "\"entriesactually\" ", "\"nowonlytwomore\"  ", "\"iwaswrongearlier\"")
  )
})
test_that("Array", {
  a <- array(1:6, c(3, 1, 2))
  a.c <- capture.output(a)
  expect_equal(
    diffobj:::which_array_rh(a.c, dimnames(a)),
  )

})


