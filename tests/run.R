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
        "diffPrint", "diffStr", "diffChr", "diffObj",
        "diffDeparse",
        "guide", "trim", "atomic", "file",
        "summary", "style", "rdiff", "html", "core", "warning", "pager", "text",
        "check"
      ), collapse="|"
    )
  )
})
