# Run tests

library(testthat)
library(covr)
library(diffobj)

local({                                         # so we can use `on.exit`
  old.opts <- diffobj_set_def_opts()
  options(diffobj.style=StyleAnsi8NeutralYb())  # force ANSI colors
  options(diffobj.pager="off")                  # run tests without pager
  options(width=80L)

  # # covr options have no effect here; just recorded so we can use them ahead
  # # of calling package_coverage()

  # options(covr.exclude_start="(?://|#)[[:space:]]*nocov[[:space:]]*start")
  # options(covr.exclude_end="(?://|#)[[:space:]]*nocov[[:space:]]*end")
  # options(covr.exclude_pattern="(?://|#)[[:space:]]*nocov")

  on.exit(options(old.opts))
  test_dir(
    "testthat",
    filter=paste0(                              # so we can run subset of files
      c(
        "atomic",
        "banner",
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
})
