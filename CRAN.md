This is a minor release to address the undeclared
dependencies in packages raised by Professor Hornik.

I have removed the tests in question completely
from the built package.  Previously, these tests
were run conditionally.

## R CMD check --as-cran

Completes with 'Status: OK'

## Test Environments

I have tested this package against the following
environments:

* Travis Ubuntu 14.04.5 LTS
    * R devel (2018-06-08 r74873)
    * R version 3.5.0 (2017-01-27)
    * R version 3.4.4 (2017-01-27)
* Winbuilder
    * R devel (2018-06-07 r74865)
      https://win-builder.r-project.org/ns6HAd6m8oUD
* Locally Mac OS 10.13.4
    * R Version 3.4.3 (2017-11-30)

