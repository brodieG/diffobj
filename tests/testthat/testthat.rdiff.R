# Only run tests on machines that are likely to have diff utility

if(identical(.Platform$OS.type, "unix")) {
  library(diffobj)
  A2 <- c("A", "B", "C")
  B2 <- c("X", "A", "Y", "C")
  A3 <- 1:3
  B3 <- c(100L, 1L, 200L, 3L)

  test_that("Rdiff_chr", {
    ref.res <- c("0a1", "2c3")
    ref.res.1 <- c("0a1", "> X", "2c3", "< B", "---", "> Y")

    expect_identical(Rdiff_chr(A2, B2, silent=TRUE, minimal=TRUE), ref.res)
    capt <- capture.output(res <- Rdiff_chr(A2, B2, silent=FALSE, minimal=TRUE))
    expect_identical(res, ref.res)
    expect_identical(capt, res)
    capt.1 <- capture.output(
      res.1 <- Rdiff_chr(A2, B2, silent=FALSE, minimal=FALSE)
    )
    expect_identical(capt.1, ref.res.1)
    expect_identical(res.1, ref.res.1)

    # test coersion
    expect_identical(Rdiff_chr(A3, B3), ref.res)
  })
  test_that("Rdiff_obj", {
    ref.res2 <- c("1c1", "< [1] \"A\" \"B\" \"C\"", "---", "> [1] \"X\" \"A\" \"Y\" \"C\"" )
    ref.res3 <- c("1c1")
    expect_identical(Rdiff_obj(A2, B2), ref.res2)
    expect_identical(Rdiff_obj(A2, B2, minimal=TRUE), ref.res3)
  })
}
