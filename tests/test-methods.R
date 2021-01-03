NAME <- "methods"
source(file.path('_helper', 'init.R'))

# try implementing methods that change default behavior outside of package

# - Force unified --------------------------------------------------------------

par.env <- new.env()
local(
  envir=par.env, {
  suppressWarnings(
    setClass(
      "testdiffobj", slots=c(a="integer"), where=par.env
  ) )
  # First check that we do actually output in side by side mode

  print(
    all.equal(
      as.character(diffObj(new("testdiffobj", a=1L), new("testdiffobj", a=2L))),
      rdsf(100)
  ) )
  # Now verify that with our new method, we get unified

  setMethod("diffObj", c("testdiffobj", "testdiffobj"),
    function(target, current, ...) {
      dots <- match.call(expand.dots=FALSE)[["..."]]
      if("mode" %in% names(dots))
        callNextMethod()
      else
        callNextMethod(target=target, current=current, ..., mode="unified")
    },
    where=par.env
  )
  on.exit(
    removeMethod("diffObj", c("testdiffobj", "testdiffobj"), where=par.env)
  )
  print(
    all.equal(
      as.character(diffObj(new("testdiffobj", a=1L), new("testdiffobj", a=2L))),
      rdsf(200)
    ) )
  # Make sure we can still get side by side?
  print(
    all.equal(
      as.character(
        diffObj(
          new("testdiffobj", a=1L), new("testdiffobj", a=2L), mode="sidebyside"
      ) ),
      rdsf(100)
  ) )
  try( #"Argument `mode` must be"
    diffObj(new("testdiffobj", a=1L), new("testdiffobj", a=2L), mode="hello")
  )
})
