source(file.path('_helper', 'init.R'))

# This one should NOT have an associated .Rout.save file.  Unlike all the other
# files it only fails if an error is thrown.

if(nchar(Sys.getenv('NOT_CRAN'))) {
  rdsf <- function(x)
    readRDS(file.path("testthat", "helper", "notcran", sprintf("%s.rds", x)))
  txtf <- function(x)
    readLines(file.path("testthat", "helper", "notcran", sprintf("%s.txt", x)))

  # - tibble ---------------------------------------------------------------------

  local({
    old.opt <- options(
      diffobj.sgr.supported=TRUE, crayon.enabled=TRUE, crayon.colors=256
    )
    crayon::num_colors(TRUE)
    on.exit({
      options(old.opt)
      crayon::num_colors(TRUE)
    })
    TB1 <- try(tibble::tibble(a=1:10), silent=TRUE)
    if(!inherits(TB1, "try-error")) {
      TB2 <- TB1
      TB2[5,"a"] <- 99L

      tb.diff <- as.character(diffPrint(TB1, TB2, context=1)) # warn:  'ANSI CSI'

      stopifnot(all.equal(tb.diff, txtf(100), check.attributes = FALSE))
      tb.diff.2 <- diffPrint(TB1, TB2[-2,], context=1) # warn: 'ANSI CSI'
      stopifnot(all.equal(capture.output(show(tb.diff.2)), txtf(200)))
      if(packageVersion('pillar') > '1.3.0')
        # Due to pillar #129
        stopifnot(
          all.equal(
            capture.output(
              show(diffPrint(TB1, TB2[-2,], context=1, strip.sgr=FALSE))
            ),
            txtf(300)
        ) )
    }
  })

  # - data.table -----------------------------------------------------------------
  #
  # data table, but only if available

  DT1 <- try(data.table::data.table(a=1:10), silent=TRUE)
  if(!inherits(DT1, "try-error")) {
    DT2 <- data.table::copy(DT1)
    DT2[5, a:=99]
    stopifnot(
      all.equal(
        suppressWarnings(as.character(diffPrint(DT1, DT2, context=1))),
        structure(
          c(
              "\033[33m<\033[39m \033[33mDT1\033[39m        \033[34m>\033[39m \033[34mDT2\033[39m      ", "\033[36m@@ 5,3 @@  \033[39m  \033[36m@@ 5,3 @@  \033[39m", "\033[90m\033[90m~\033[90m \033[90m\033[90m     a\033[90m\033[90m   \033[39m  \033[90m\033[90m~\033[90m \033[90m\033[90m     a\033[90m\033[90m   \033[39m", "  \033[90m 4: \033[39m 4\033[90m\033[39m       \033[90m 4: \033[39m 4\033[90m\033[39m   ", "\033[33m<\033[39m \033[90m 5: \033[39m \033[33m5\033[39m\033[90m\033[39m     \033[34m>\033[39m \033[90m 5: \033[39m\033[34m99\033[39m\033[90m\033[39m   ",
    "  \033[90m 6: \033[39m 6\033[90m\033[39m       \033[90m 6: \033[39m 6\033[90m\033[39m   "
            ),
            len = 6L
    ) ) )
  }
}

