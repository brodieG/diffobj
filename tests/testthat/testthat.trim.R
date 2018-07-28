context("trim")

if(!identical(basename(getwd()), "testthat"))
  stop("Working dir does not appear to be /testthat, is ", getwd())

rdsf <- function(x)
  file.path(getwd(), "helper", "trim", sprintf("%s.rds", x))

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
  expect_equal(diffobj:::which_atomic_rh(capture.output(1:5)), 1)

  expect_equal_to_reference(
    as.character(diffPrint(1:3, 2:6, trim=FALSE)), rdsf(50)
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
  # Single row matrix

  expect_equal(
    diffobj:::which_matrix_rh(capture.output(matrix(1:2, nrow=1)), NULL), 2
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
    diffobj:::strip_table_rh(capture.output(df3)),
    c("                            V1", "averylongwordthatcanlahblah", " causeasinglewidecolumnblah", "   matrixtowrapseveraltimes", "               V2", " inarrowscreen", " onceuponatime", "agreenduckflew", "                   V3", "  overthemountains", "inalongofantelopes", "    ineedthreemore", "                 V4", " entriesactually", "  nowonlytwomore", "iwaswrongearlier")
  )
  # Try ts

  expect_equal(
    diffobj:::strip_table_rh(capture.output(USAccDeaths)),
    capture.output(USAccDeaths)
  )
  # Set it so first year is 1

  USAD2 <- USAccDeaths
  tsp(USAD2)[1:2] <- tsp(USAD2)[1:2] - 1972

  expect_equal(
    diffobj:::strip_table_rh(capture.output(USAD2)),
    c("    Jan   Feb   Mar   Apr", " 9007  8106  8928  9137", " 7750  6981  8038  8422", " 8162  7306  8124  7870", " 7717  7461  7767  7925", " 7792  6957  7726  8106", " 7836  6892  7791  8192", "    May   Jun   Jul   Aug", "10017 10826 11317 10744", " 8714  9512 10120  9823", " 9387  9556 10093  9620", " 8623  8945 10078  9179", " 8890  9299 10625  9302", " 9115  9434 10484  9827", "    Sep   Oct   Nov   Dec", " 9713  9938  9161  8927", " 8743  9129  8710  8680", " 8285  8466  8160  8034", " 8037  8488  7874  8647", " 8314  8850  8265  8796", " 9110  9070  8633  9240")
  )
  # single row data frame

  expect_equal(c(diffobj:::which_table_rh(capture.output(data.frame(1, 2)))), 2)

  # More than 10 rows data.frame

  expect_equal(
    c(diffobj:::which_table_rh(capture.output(head(Puromycin, 10L)))),
    2:11
  )
})
test_that("Array", {
  a <- array(1:6, c(3, 1, 2))
  a.c <- capture.output(a)
  expect_equal(
    diffobj:::strip_array_rh(a.c, dimnames(a)),
    c(", , 1", "", "     [,1]", "   1", "   2", "   3", "", ", , 2", "", "     [,1]", "   4", "   5", "   6", "")
  )
  viz_sarh <- function(capt, obj)
    cbind(
      capt,
      as.integer(
        seq_along(capt) %in% diffobj:::which_array_rh(capt, dimnames(obj))
      )
    )
  a1 <- a2 <- a3 <- a4 <- array(
    "averylongphrasethatwillforcemytwocolumnarraytowrapblahblah", c(2, 2, 2)
  )
  ca1 <- capture.output(a1)
  viz_sarh(ca1, a1)
  expect_equal(
    diffobj:::which_array_rh(ca1, dimnames(a1)),
    c(4L, 5L, 7L, 8L, 13L, 14L, 16L, 17L)
  )
  colnames(a2) <- c("ABC", "DEF")
  ca2 <- capture.output(a2)
  viz_sarh(ca2, a2)
  expect_equal(
    diffobj:::which_array_rh(ca2, dimnames(a2)),
    c(4L, 5L, 7L, 8L, 13L, 14L, 16L, 17L)
  )
  rownames(a3) <- 1:2
  ca3 <- capture.output(a3)
  viz_sarh(ca3, a3)
  expect_equal(diffobj:::which_array_rh(ca3, dimnames(a3)), integer(0L))

  attr(a4, "blahblah") <- matrix(1:4, 2)
  ca4 <- capture.output(a4)
  viz_sarh(ca4, a4)
  expect_equal(
    diffobj:::which_array_rh(ca4, dimnames(a4)),
    c(4L, 5L, 7L, 8L, 13L, 14L, 16L, 17L)
  )
})
test_that("List", {
  l1 <- list(
    matrix(1:4, 2), b=list(abc=c(letters, LETTERS), list(matrix(4:1, 2)))
  )
  l1.c <- capture.output(l1)
  expect_equal(
    diffobj:::strip_list_rh(l1.c, l1),
    c("[[1]]", "     [,1] [,2]", "   1    3", "   2    4", "", "$b", "$b$abc", "\"a\" \"b\" \"c\" \"d\" \"e\" \"f\" \"g\" \"h\" \"i\" \"j\" \"k\" \"l\" \"m\" \"n\" \"o\" \"p\" \"q\" \"r\" \"s\"", "\"t\" \"u\" \"v\" \"w\" \"x\" \"y\" \"z\" \"A\" \"B\" \"C\" \"D\" \"E\" \"F\" \"G\" \"H\" \"I\" \"J\" \"K\" \"L\"", "\"M\" \"N\" \"O\" \"P\" \"Q\" \"R\" \"S\" \"T\" \"U\" \"V\" \"W\" \"X\" \"Y\" \"Z\"", "", "$b[[2]]", "$b[[2]][[1]]", "     [,1] [,2]", "   4    2", "   3    1", "", "", "")
  )

  a <- list(list())
  aa <- list(list(), "a")
  b <- list("a", list())
  c <- list(list("a"), "b")
  d <- list("a", "b", "c")

  expect_identical(
    diffobj:::strip_list_rh(capture.output(d), d),
    c("[[1]]", "\"a\"", "", "[[2]]", "\"b\"", "", "[[3]]", "\"c\"", "")
  )
  expect_identical(
    diffobj:::strip_list_rh(capture.output(a), a),
    c("[[1]]", "list()", "")
  )
  expect_identical(
    diffobj:::strip_list_rh(capture.output(aa), aa),
    c("[[1]]", "list()", "", "[[2]]", "\"a\"", "")
  )
  expect_identical(
    diffobj:::strip_list_rh(capture.output(b), b),
    c("[[1]]", "\"a\"", "", "[[2]]", "list()", "")
  )
  expect_identical(
    diffobj:::strip_list_rh(capture.output(c), c),
    c("[[1]]", "[[1]][[1]]", "\"a\"", "", "", "[[2]]", "\"b\"", "")
  )

})
test_that("custom trim fun", {
  a <- matrix(100:102)
  b <- matrix(101:103)
  fun1 <- function(x, y) cbind(rep(1L, 4), rep(5L, 4))

  expect_equal_to_reference(as.character(diffPrint(a, b, trim=fun1)), rdsf(100))
  if(getRversion() >= "3.2") {
    expect_warning(
      capture.output(
        trim.err <-
          as.character(diffPrint(a, b, trim=function(x, y) stop("boom"))),
        type="message"
      ),
      "If you did not specify a `trim`"
    )
    expect_equal_to_reference(trim.err, rdsf(200))
  }
  # purposefully bad trim fun

  expect_error(
    diffPrint(1:100, 2:100, trim=function(x, y) TRUE),
    "method return value must be a two "
  )
})

test_that("s4", {
  setClass("DOTrimTest", slots=c(a="numeric", b="list", c="matrix"))
  obj <- new(
    "DOTrimTest", a=1:40, b=list(a=1, letters, NULL), c=matrix(1:9, 3)
  )
  expect_equal_to_reference(
    diffobj:::strip_s4_rh(capture.output(obj), obj), rdsf(300)
  )
})
