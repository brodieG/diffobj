# Run tests

library(testthat)
library(diffobj)

local({
  old.opts <- diffobj_set_def_opts()
  options(diffobj.style=StyleAnsi8NeutralYb())  # force ANSI colors
  options(diffobj.pager="off")                  # run tests without pager
  options(width=80L)
  on.exit(options(old.opts))
  test_dir(
    "testthat",
    filter=paste0(
      c(
        # "atomic",
        # "banner",
        # "check",
        # # "context",
        # "core",
        # "diffChr",
        # "diffDeparse",
        # "diffObj",
        "diffPrint"
        # "diffStr",
        # "file",
        # "guide",
        # "html",
        # "limit",
        # "misc",
        # "pager",
        # "rdiff",
        # "style",
        # "subset",
        # "summary",
        # "text",
        # "trim",
        # "warning"
      ), collapse="|"
    )
  )
})
