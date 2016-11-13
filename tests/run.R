# Run tests

library(testthat)
library(covr)
library(diffobj)

local({                                         # so we can use `on.exit`
  # options that can't be reset to NULL...

  no.null.opts <- c(
    "warnPartialMatchArgs", "warnPartialMatchAttr", "warnPartialMatchDollar"
  )
  no.null.opt.list <- Map(getOption, no.null.opts)
  no.null.nulls <- vapply(no.null.opt.list, is.null, logical(1L))
  no.null.opt.list[no.null.nulls] <- FALSE
  all.opts <- c(
    list(
      useFancyQuotes=FALSE,   # all.equals uses fancy quotes
      diffobj.format="ansi8", # force ANSI colors
      diffobj.color.mode="yb",# force yb
      diffobj.pager="off",    # run tests without pager
      width=80L,
      encoding="UTF-8"        # so Gabor's name renders properly on win...
    )
  )
  old.opts <- options(c(diffobj_set_def_opts(), all.opts))
  options(
    warnPartialMatchArgs=TRUE,
    warnPartialMatchAttr=TRUE,
    warnPartialMatchDollar=TRUE
  )
  old.opts <- c(old.opts, no.null.opt.list)

  # covr options have no effect here; just recorded so we can use them ahead
  # of calling package_coverage() when running tests manually
  if(FALSE) {
    options(covr.exclude_start="(?://|#)[[:space:]]*nocov[[:space:]]*start")
    options(covr.exclude_end="(?://|#)[[:space:]]*nocov[[:space:]]*end")
    options(covr.exclude_pattern="(?://|#)[[:space:]]*nocov")
  }
  on.exit(options(old.opts))
  test.res <- test_dir(
    "testthat",
    filter=paste0(                              # so we can run subset of files
      c(
        "atomic",
        "banner",
        "capture",
        "check",
        "context",
        "core",
        "diffChr",
        "diffDeparse",
        "diffObj",
        "diffPrint",
        "diffStr",
        "file",
        "guide",
        "html",
        "limit",
        "methods",
        "misc",
        "pager",
        "rdiff",
        "s4",
        "style",
        "subset",
        "summary",
        "text",
        "trim",
        "warning"
      ), collapse="|"
    )
  )
  with(
    as.data.frame(test.res), {
      fail <- sum(failed)
      err <- sum(error)
      if(fail != 0 || err) stop("Errors: ", err, " Failures: ", fail)
  })
})
