# Run tests

library(testthat)
library(diffobj)

local({
  old.opts <- diffobj_set_def_opts()
  options(diffobj.style=StyleAnsi8NeutralYb())  # force ANSI colors
  on.exit(options(old.opts))
  test_dir("testthat", filter="diffPrint")
})
