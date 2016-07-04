library(diffobj)

context("align")

if(!identical(basename(getwd()), "testthat"))
  stop("Working dir does not appear to be /testthat, is ", getwd())

rdsf <- function(x)
  file.path(getwd(), "helper", "align", sprintf("%s.rds", x))

stop("Tests not working currently")

# All these tests need to be updated to be consistent with the final structure
# whereby we trim row headers, etc.  Our original matrix tests don't make sense
# any more because post-trimming what used to be part of the hunk that needed
# alignment is now actually just a straight-up line match

test_that("align with headers", {
  mx.5 <- matrix(11:19, 3)
  mx.6 <- matrix(22:11, 4)
  mx.6[4,] <- c(13L, 16L, 19L)
  raw <- StyleRaw()
  expect_equal_to_reference(
    as.character(diffPrint(mx.5, mx.6, style=raw)), rdsf(100)
  )
  expect_equal_to_reference(
    as.character(
      diffPrint(
        mx.5, mx.6, style=raw, trim=FALSE
    ) ),
    rdsf(200)
  )
  expect_equal_to_reference(
    as.character(
      diffPrint(
        mx.5, mx.6, style=raw, align=1
    ) ),
    rdsf(300)
  )


  expect_identical(
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
