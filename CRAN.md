This is a minor release to resolve failures caused
by changes to `tibble`, as well as some changes in
how UTF8 bytes are parsed on the win-builder machines.

## R CMD check --as-cran

Completes with 'Status: OK'

## Test Environments

I have tested this package against the following
environments:

* Travis Ubuntu 14.04.5 LTS
    * R devel (2018-01-26 r74168)
    * R version 3.4.2 (2017-01-27)
    * R version 3.3.3 (2017-01-27)
* Winbuilder
    * R devel (2018-01-26 r74162)
      https://win-builder.r-project.org/9Qq1vESgk754
  * Locally Mac OS 10.13.2
    * R Version 3.4.3 (2017-11-30)

