# Run tests

library(testthat)
library(diffobj)

local({
  old.opts <- diffobj_set_def_opts()
  options(diffobj.style=StyleAnsi8NeutralYb())  # force ANSI colors
  options(diffobj.pager="off")                  # run tests without pager
  on.exit(options(old.opts))
  test_dir("testthat", filter="diffPrint|guide|trim")
})
