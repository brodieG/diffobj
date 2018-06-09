# diffobj - Diffs for R Objects

[![](https://travis-ci.org/brodieG/diffobj.svg?branch=master)](https://travis-ci.org/brodieG/diffobj)
[![](https://codecov.io/github/brodieG/diffobj/coverage.svg?branch=master)](https://codecov.io/github/brodieG/diffobj?branch=master)
[![](http://www.r-pkg.org/badges/version/diffobj)](https://cran.r-project.org/package=diffobj)

Generate a colorized diff of two R objects for an intuitive visualization of their differences.

See vignettes for [details](http://htmlpreview.github.io/?https://raw.githubusercontent.com/brodieG/diffobj/master/inst/doc/diffobj.html), and for [comparisons with standard comparison functions](http://htmlpreview.github.io/?https://raw.githubusercontent.com/brodieG/diffobj/master/inst/doc/metacomp.html).

## Output

If your terminal supports formatting through ANSI escape sequences, `diffobj` will output colored diffs to the terminal.  Otherwise, output will be colored with HTML/CSS and sent to the IDE viewport or to your browser.  `diffobj` comes with several built-in color schemes that can be further customized.  Some examples:

![Output Examples](https://raw.githubusercontent.com/brodieG/diffobj/master/cliandrstudio.png)

## Installation

This package is available on [CRAN](https://cran.r-project.org/package=diffobj).

```
install.packages("diffobj")
browseVignettes("diffobj")
```

## Related Software

* [tools::Rdiff](https://stat.ethz.ch/R-manual/R-devel/library/tools/html/Rdiff.html)
* [Daff](https://cran.r-project.org/package=daff) diff, patch and merge for data.frames
* [GNU diff](https://www.gnu.org/software/diffutils)

## Acknowledgements

* R Core for developing and maintaining such a wonderful language.
* CRAN maintainers, for patiently shepherding packages onto CRAN and maintaining
  the repository, and Uwe Ligges in particular for maintaining
  [Winbuilder](http://win-builder.r-project.org/).
* [Jim Hester](https://github.com/jimhester) because
  [covr](https://cran.r-project.org/package=covr) rocks.
* [Dirk Eddelbuettel](https://github.com/eddelbuettel) and [Carl
  Boettiger](https://github.com/cboettig) for the
  [rocker](https://github.com/rocker-org/rocker) project, and [Gábor
  Csárdi](https://github.com/gaborcsardi) and the
  [R-consortium](https://www.r-consortium.org/) for
  [Rhub](https://github.com/r-hub), without which testing bugs on R-devel and
  other platforms would be a nightmare.
* [Hadley Wickham](https://github.com/hadley/) for
  [devtools](https://cran.r-project.org/package=devtools) and with [Peter
  Danenberg](https://github.com/klutometis) for
  [roxygen2](https://cran.r-project.org/package=roxygen2).
* [Yihui Xie](https://github.com/yihui) for
  [knitr](https://cran.r-project.org/package=knitr) and  [J.J.
  Allaire](https://github.com/jjallaire) etal for
  [rmarkdown](https://cran.r-project.org/package=rmarkdown), and by extension
  John MacFarlane for [pandoc](http://pandoc.org/).
* Olaf Mersmann for
  [microbenchmark](https://cran.r-project.org/package=microbenchmark), because
  microsecond matter, and [Joshua Ulrich](https://github.com/joshuaulrich) for
  making it lightweight and maintaining it.
* [Tomas Kalibera](https://github.com/kalibera) for
  [rchk](https://github.com/kalibera/rchk) and the accompanying vagrant image,
  and rcnst to help detect errors in compiled code.
* [Winston Chang](https://github.com/wch) for the
  [r-debug](https://hub.docker.com/r/wch1/r-debug/) docker container, in
  particular because of the valgrind level 2 instrumented version of R.
* All open source developers out there that make their work freely available
  for others to use.
* [Github](https://github.com/), [Travis-CI](https://travis-ci.org/),
  [Codecov](https://codecov.io/), [Vagrant](https://www.vagrantup.com/),
  [Docker](https://www.docker.com/), [Ubuntu](https://www.ubuntu.com/),
  [Brew](https://brew.sh/) for providing infrastructure that greatly simplifies
  open source development.
* [Free Software Foundation](http://fsf.org/) for developing the GPL license and
  promotion of the free software movement.

