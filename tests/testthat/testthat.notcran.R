library(testthat)
context('notcran')

txtf <- function(x)
  file.path(getwd(), "helper", "notcran", sprintf("%s.txt", x))

test_that("tibble", {
   TB1 <- try(tibble::tibble(a=1:10), silent=TRUE)
   if(!inherits(TB1, "try-error")) {
     TB2 <- TB1
     TB2[5,"a"] <- 99L

     # not ideal, have to strip styles now that tibble started adding them as
     # they clash with ones used by `diffobj`, need to resolve more
     # systematically with #114

     expect_warning(
       tb.diff <- as.character(diffPrint(TB1, TB2, context=1)), 'ANSI CSI'
     )
     expect_known_output(writeLines(tb.diff), txtf(100))


     expect_equal(
       crayon::strip_style(
         suppressWarnings(as.character())
       ),
       crayon::strip_style(
         structure(
           c(
             "\033[33m<\033[39m \033[33mTB1\033[39m        \033[34m>\033[39m \033[34mTB2\033[39m      ",
             "\033[36m@@ 7,3 @@  \033[39m  \033[36m@@ 7,3 @@  \033[39m",
             "\033[90m\033[90m~\033[90m \033[90m\033[90m       a\033[90m\033[90m \033[39m  \033[90m\033[90m~\033[90m \033[90m\033[90m       a\033[90m\033[90m \033[39m",
             "\033[90m\033[90m~\033[90m \033[90m\033[90m   <int>\033[90m\033[90m \033[39m  \033[90m\033[90m~\033[90m \033[90m\033[90m   <int>\033[90m\033[90m \033[39m",
             "  \033[90m 4 \033[39m    4\033[90m\033[39m     \033[90m 4 \033[39m    4\033[90m\033[39m ",
             "\033[33m<\033[39m \033[90m 5 \033[39m    \033[33m5\033[39m\033[90m\033[39m   \033[34m>\033[39m \033[90m 5 \033[39m   \033[34m99\033[39m\033[90m\033[39m ",
             "  \033[90m 6 \033[39m    6\033[90m\033[39m     \033[90m 6 \033[39m    6\033[90m\033[39m "
          ), len = 7L
      ) )
    )
  }
})
test_that("data.table", {
  # data table, but only if available

  DT1 <- try(data.table::data.table(a=1:10), silent=TRUE)
  if(!inherits(DT1, "try-error")) {
    DT2 <- data.table::copy(DT1)
    DT2[5, a:=99]
    expect_equal(
      suppressWarnings(as.character(diffPrint(DT1, DT2, context=1))),
      structure(
        c(
          "\033[33m<\033[39m \033[33mDT1\033[39m        \033[34m>\033[39m \033[34mDT2\033[39m      ", "\033[36m@@ 5,3 @@  \033[39m  \033[36m@@ 5,3 @@  \033[39m", "\033[90m\033[90m~\033[90m \033[90m\033[90m     a\033[90m\033[90m   \033[39m  \033[90m\033[90m~\033[90m \033[90m\033[90m     a\033[90m\033[90m   \033[39m", "  \033[90m 4: \033[39m 4\033[90m\033[39m       \033[90m 4: \033[39m 4\033[90m\033[39m   ", "\033[33m<\033[39m \033[90m 5: \033[39m \033[33m5\033[39m\033[90m\033[39m     \033[34m>\033[39m \033[90m 5: \033[39m\033[34m99\033[39m\033[90m\033[39m   ",
"  \033[90m 6: \033[39m 6\033[90m\033[39m       \033[90m 6: \033[39m 6\033[90m\033[39m   "
        ),
        len = 6L
  ) ) }
})
