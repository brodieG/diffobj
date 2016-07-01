library(diffobj)

test_that("align with headers", {
  mx.5 <- matrix(11:19, 3)
  mx.6 <- matrix(22:11, 4)
  mx.6[4,] <- c(13L, 16L, 19L)
  raw <- StyleRaw()
  expect_identical(
    c(as.character(diffPrint(mx.5, mx.6, style=raw))),
    c("< mx.5                  > mx.6                 ", "@@ 1,4 @@               @@ 1,5 @@              ", "       [,1] [,2] [,3]          [,1] [,2] [,3]  ", "< [1,]   11   14   17   > [1,]   22   18   14  ", "< [2,]   12   15   18   > [2,]   21   17   13  ", "                        > [3,]   20   16   12  ", "< [3,]   13   16   19   > [4,]   13   16   19  ")
  )
  expect_identical(
    c(
      as.character(
        diffPrint(
          mx.5, mx.6, style=raw,
          align=AlignThreshold(ignore.row.head=FALSE)
    ) ) ),
    c("< mx.5                  > mx.6                 ", "@@ 1,4 @@               @@ 1,5 @@              ", "       [,1] [,2] [,3]          [,1] [,2] [,3]  ", "< [1,]   11   14   17   > [1,]   22   18   14  ", "< [2,]   12   15   18   > [2,]   21   17   13  ", "< [3,]   13   16   19   > [3,]   20   16   12  ", "                        > [4,]   13   16   19  ")
  )
  df.1 <- as.data.frame(mx.5)
  df.2 <- as.data.frame(mx.6)

  expect_identical(
    c(as.character(diffPrint(df.1, df.2, format="raw", disp.width=80))),
    c("< df.1              > df.2             ", "@@ 1,4 @@           @@ 1,5 @@          ", "    V1 V2 V3            V1 V2 V3       ", "< 1 11 14 17        > 1 22 18 14       ", "< 2 12 15 18        > 2 21 17 13       ", "                    > 3 20 16 12       ", "< 3 13 16 19        > 4 13 16 19       ")
  )
})
# List of things that used to be tested before the interface became too
# complicated to easily test spearately:
# - simple alignment
# - edge cases with empty strings
# - empty first matches
# - thresholds
# - ignoring whitespace

