context("methods")
library(testthat)
library(diffobj)

if(!identical(basename(getwd()), "testthat"))
  stop("Working dir does not appear to be /testthat, is ", getwd())

rdsf <- function(x)
  file.path(getwd(), "helper", "methods", sprintf("%s.rds", x))

# try implementing methods that change default behavior outside of package

test_that("Force unified", {
  par.env <- new.env()
  local(
    envir=par.env, {
    suppressWarnings(
      setClass(
        "testdiffobj", slots=c(a="integer"), where=par.env
    ) )
    # First check that we do actually output in side by side mode

    expect_equal_to_reference(
      as.character(diffObj(new("testdiffobj", a=1L), new("testdiffobj", a=2L))),
      rdsf(100)
    )
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
    expect_equal_to_reference(
      as.character(diffObj(new("testdiffobj", a=1L), new("testdiffobj", a=2L))),
      rdsf(200)
    )
    # Make sure we can still get side by side?
    expect_equal_to_reference(
      as.character(
        diffObj(
          new("testdiffobj", a=1L), new("testdiffobj", a=2L), mode="sidebyside"
      ) ),
      rdsf(100)
    )
    expect_error(
      diffObj(new("testdiffobj", a=1L), new("testdiffobj", a=2L), mode="hello"),
      "Argument `mode` must be"
    )
  })
})
