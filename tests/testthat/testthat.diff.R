context("Diff")
local({
  test_that("diff", {
    mx.1 <- matrix(1:9, nrow=3)
    mx.2 <- matrix(1:100, ncol=2)
    set.seed(1, "Mersenne-Twister")
    mx.3 <- matrix(runif(100), ncol=2)
    mx.4 <- mx.3 <- mx.2
    mx.3[31, 2] <- 111L
    mx.3a <- mx.3[-31, ]

    mx.4[cbind(sample(1:50, 6), sample(1:2, 6, replace=TRUE))] <-
      sample(-(1:50), 6)

    diffPrint(mx.2, mx.3)
    diffPrint(mx.2, mx.3a)
    diffPrint(mx.2, mx.3a, mode="unified")
    diffPrint(mx.2, mx.3, mode="unified")
    diffPrint(mx.2, mx.4)
    diffPrint(mx.2, mx.4, mode="unified")

    mx.5 <- matrix(1:9, 3)
    mx.6 <- matrix(12:1, 4)
    mx.6[4,] <- c(3L, 6L, 9L)
    diffPrint(mx.5, mx.6)
    diffPrint(mx.5, mx.6, mode="unified")
    diffPrint(mx.5, mx.6, mode="context")

    A <- B <- matrix(sample(1:80), nrow=16)
    B[cbind(sample(5:16, 4), sample(1:5, 4))] <- sample(30:80, 4)
    diffPrint(A, B)
    diffPrint(A, B, mode="unified")
    diffPrint(A, B, mode="context")
    diffPrint(A, B, mode="context", context=0)

    # Style matrices

    diffPrint(diffobj:::.mx1, diffobj:::.mx2, mode="unified")

    lst.1 <- list(
      NULL,
      z=list(
        list(letters[1:3]), list(NULL),
        z=list(1:3, 1, 2, z=list(1, z=list(z=5))),
        matrix(1:9, 3)
    ) )
    lst.2 <- lst.1
    lst.2$z$z$z$z$z <- 6
    lst.2$z[[1L]][[1L]][2L] <- "bananas"
    lst.2$z[[4L]] <- matrix(12:1, ncol=3)
    lst.2$z[[4L]][4, ] <- c(3L, 6L, 9L)
    lst.3 <- lst.2
    lst.3[[1]] <- "hello"

    diffPrint(lst.1, lst.3, mode="sidebyside")
    diffPrint(lst.1, lst.3, mode="sidebyside", context=1)
    diffPrint(lst.1, lst.3, mode="unified")
    diffObj(lst.1, lst.3)
    diffObj(lst.1, lst.2)

    lst.4 <- list(NULL, z=list(z=list(z=list(z=list(matrix(1:3))))))
    lst.5 <- list(NULL, z=list(z=list(z=list(z=list(matrix(2:4))))))
    diffPrint(lst.4, lst.5)

    chr.1 <- c(
      "hello world",
      "I ran into a rather bizarre bug involving memoise that made it impossible to forget the cached version of crayon:::i_num_colors. Somehow, the binary version of crayon on CRAN has a corrupted copy of the memoised crayon:::i_num_colors function",
      "goodbye"
    )
    chr.2 <- c(
      "hello world",
      "I ran blah a rather bizarre bug involving memoise that made it"
    )

    diffPrint(chr.1, chr.2)
    diffObj(chr.1, chr.2, mode="sidebyside")
    diffPrint(chr.1, chr.2, mode="unified")
    diffPrint(chr.1, chr.2, mode="context")
    diffPrint(chr.1[2:3], chr.2[2], mode="sidebyside")

    # make sure blanks line up correctly
    chr.3 <- letters[1:20]
    chr.4 <- c(
      "a phrase long enough to wrap a few lines when looked at on a side by side basis",
      "lorem ipsum dolor something or other I don't remember what the whole thing was anyway"
    )
    diffPrint(chr.3, chr.4)
    diffPrint(chr.3, chr.4, mode="unified")

    # Shows that line shifts within hunks are matched

    iris.2 <- iris.c <- transform(iris, Species=as.character(Species))
    # without rounding this is a bit wild, but good corner case to test
    iris.2$Sepal.Length[sample(nrow(iris.2), 5)] <-
      rnorm(5, mean(iris.2$Sepal.Length), sd(iris.2$Sepal.Length))

    iris.3 <- iris.2
    iris.3$Sepal.Length <- round(iris.3$Sepal.Length, 1L)
    iris.4 <- iris.3
    iris.4$Petal.Width[sample(1:nrow(iris.4), 6)] <- round(runif(6), 1)

    diffPrint(iris, iris.2) # no round
    diffPrint(iris, iris.2, mode="sidebyside")
    diffPrint(iris, iris.c)
    diffPrint(iris, iris.3)
    diffPrint(iris, iris.3, mode="sidebyside")
    diffPrint(iris, iris.4, mode="unified")
    diffPrint(iris, iris.4, mode="sidebyside")
    diffObj(iris, iris.c)
    diffObj(iris, iris.2)
    diffObj(iris, iris.3)
    diffObj(iris, iris.3, mode="sidebyside")

    diffPrint(iris, iris.4, mode="sidebyside", guides=function(x, y) integer())
    diffPrint(iris, iris.4, mode="sidebyside", guides=FALSE)

    iris.5 <- iris
    attr(iris.5, "test.attr") <- letters

    diffPrint(iris.5, iris.4, mode="sidebyside")

    # Narrow versions to fit side by side

    iris.3a <- setNames(iris.3, c("S.L", "S.W", "P.L", "P.W", "Sp"))
    iris.4a <- setNames(iris.4, c("S.L", "S.W", "P.L", "P.W", "Sp"))

    diffPrint(iris.3a, iris.4a)

    diffStr(cars, mtcars)
    diffPrint(cars, mtcars, mode="s")

    diffPrint(iris, iris[-2])
    diffPrint(list(1, 2, 3), matrix(1:9, 3))
    diffPrint(list(25, 2, 3), matrix(1:9, 3))
    diffPrint(list(c(1, 4, 7), c(2, 5, 8), c(3, 6, 9)), matrix(1:9, 3))

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

    diffPrint(letters[1:3], LETTERS[1:3])

    Puromycin2 <- Puromycin
    set.seed(1)
    Puromycin2$conc[c(8, 15:19, 22)] <- round(runif(7), 2)
    Puromycin2$state[17] <- "treated"
    diffPrint(Puromycin, Puromycin2, line.limit=15)
    diffPrint(Puromycin, Puromycin2, line.limit=15, mode="sidebyside")
    diffPrint(Puromycin, Puromycin2, line.limit=15, mode="context")

    # line limit issues
    diffPrint(Puromycin, Puromycin2, line.limit=6)
    diffPrint(Puromycin, Puromycin2, line.limit=6, mode="sidebyside")
    diffPrint(Puromycin, Puromycin2, line.limit=6, mode="context")

    diffPrint(Puromycin, Puromycin2, line.limit=3)
    diffPrint(Puromycin, Puromycin2, line.limit=3)
    diffPrint(Puromycin, Puromycin2, line.limit=4)

    Puromycin3 <- Puromycin2
    names(Puromycin3)[3L] <- "blargh"
    diffPrint(Puromycin, Puromycin3, line.limit=6, mode="context")

    # Arrays

    arr.1 <- arr.2 <- array(1:36, dim=c(6, 2, 3))
    arr.2[c(4, 12, 20)] <- 99
    diffPrint(arr.1, arr.2, mode="s", context=1)
    diffPrint(arr.1, arr.2, mode="context", context=1)

    # Large DF (WARNING: a bit slow)

    library(ggplot2)
    head(diamonds)
    d2 <- diamonds
    d2$x[sample(seq_len(nrows(d2)), 100)] <- sample(d2$x, 100)
    d2$x[sample(seq_len(nrow(d2)), 100)] <- sample(d2$x, 100)
    diffPrint(diamonds, d2)
    diffPrint(diamonds, d2, context=1)
    diffPrint(diamonds, d2, context=1, mode="s")
  } )

  test_that("RdiffObj", {
    a <- matrix(1:3, ncol=1)
    b <- matrix(c(1, 3, 2), ncol=1)
    expect_identical(
      capture.output(res <- RdiffObj(a, b)),
      c("", "3c3", "< [2,]    2", "---", "> [2,]    3", "4c4", "< [3,]    3",  "---", "> [3,]    2")
    )
    expect_equal(res, 1)
    expect_identical(capture.output(RdiffObj(a, a)), character())
    expect_equal(RdiffObj(a, a), 0)

    # Try with RDS object

    f <- tempfile()
    saveRDS(a, f)
    expect_identical(
      capture.output(res <- RdiffObj(f, b)),
      c("", "3c3", "< [2,]    2", "---", "> [2,]    3", "4c4", "< [3,]    3",  "---", "> [3,]    2")
    )
    expect_equal(res, 1)
    expect_identical(capture.output(RdiffObj(f, f)), character())
    expect_equal(RdiffObj(a, a), 0)
    unlink(f)
  })}
)
