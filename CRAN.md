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
    * R devel (2017-12-09 r73876)
    * R version 3.4.2 (2017-01-27)
    * R version 3.3.3 (2017-01-27)
* Winbuilder
    * R devel (2017-09-12 r73242)
      https://win-builder.r-project.org/b0rAP0qfLHE5
    * R version 3.4.3 (2017-11-30)
      https://win-builder.r-project.org/xA07Gd3bS0vo
  * Locally Mac OS 10.12.6
    * R Version 3.4.1 (2017-06-30)

