This is an early re-submission of a recently
accepted package.  I hope it falls under the
"[before] a package is established" exemption in
the submission guidelines.

There is a substantial bug in the published
version of the package.  It only manifests in the
binaries when they are run on a machine other than
the one that built them, which is how I missed it.
The bug prevents the binaries from working correctly
in a common use case (Rstudio, terminals without
ANSI color support).

## R CMD check --as-cran

The only output is the NOTE corresponding to this
early re-submission.  I apologize for wasting your
time with my oversight.

    Maintainer: ‘Brodie Gaslam <brodie.gaslam@yahoo.com>’

    Days since last update: 2

## Test Environments

I have tested this package against the following
environments:

* Travis Ubuntu 12.04.5 LTS
    * R devel (2016-08-31 r71183)
    * R version 3.3.1 (2016-06-21)
    * R version 3.2.5 (2016-04-14)
* Winbuilder
    * R devel (2016-08-30 r71176)
    * R version 3.3.1 (2016-06-21)
* Locally on Mac OS 10.9.5
    * R version 3.3.1 (2016-06-21)

Additionally, I have tested the winbuilder binaries
on a different windows system.

## Bug Details

I set a default option value to `system.file(...)`
at install time instead of load time, which
creates a dependency to the building machine
library location.
