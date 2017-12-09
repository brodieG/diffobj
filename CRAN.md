This is a minor release to resolve failures under
the more recent R-devel releases that change the
output of `str` (as per Martin Mäechler). Also
resolves another minor bug.

## R CMD check --as-cran

Completes with 'Status: OK'

## Test Environments

I have tested this package against the following
environments:

* Travis Ubuntu 14.04.5 LTS
    * R devel (2017-12-08 r73867)
    * R version 3.4.2 (2017-01-27)
    * R version 3.3.3 (2017-01-27)
* Winbuilder
    * R devel (2017-09-12 r73242), unfortunately
      this fails because the R-devel version on
      winbuilder is out of date and predates
      the change to `str`.  The test runs
      on more recent R-devel on Travis.
    * R version 3.4.3 (2017-11-30)
      https://win-builder.r-project.org/pSHPbe31iLEx
  * Locally Mac OS 10.12.6
    * R Version 3.4.1 (2017-06-30)

