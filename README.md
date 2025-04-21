# diffobj - Diffs for R Objects

[![R build
status](https://github.com/brodieG/diffobj/workflows/R-CMD-check/badge.svg)](https://github.com/brodieG/diffobj/actions)
[![](https://codecov.io/github/brodieG/diffobj/coverage.svg?branch=rc)](https://app.codecov.io/gh/brodieG/diffobj?branch=rc)

Generate a colorized diff of two R objects for an intuitive visualization of their differences.

> See the [introductory vignette for details][1].

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

* [tools::Rdiff][2].
* [Daff](https://cran.r-project.org/package=daff) diff, patch and merge for
  data.frames.
* [GNU diff](https://www.gnu.org/software/diffutils/).
* [waldo](https://cran.r-project.org/package=waldo), which internally uses
  `diffobj` for diffs but takes a more hands-on approach to detailing object
  differences.

## Acknowledgements

* R Core for developing and maintaining such a wonderful language.
* CRAN maintainers, for patiently shepherding packages onto CRAN and maintaining
  the repository, and Uwe Ligges in particular for maintaining
  [Winbuilder](https://win-builder.r-project.org/).
* The users who have reported bugs and possible fixes, and/or made feature
  requests (see NEWS.md).
* [Gábor Csárdi](https://github.com/gaborcsardi) for
  [crayon](https://github.com/r-lib/crayon).
* [Jim Hester](https://github.com/jimhester) for
  [covr](https://cran.r-project.org/package=covr), and with Rstudio for
  [r-lib/actions](https://github.com/r-lib/actions).
* [Dirk Eddelbuettel](https://github.com/eddelbuettel) and [Carl
  Boettiger](https://github.com/cboettig) for the
  [rocker](https://github.com/rocker-org/rocker) project, and [Gábor
  Csárdi](https://github.com/gaborcsardi) and the
  [R-consortium](https://r-consortium.org/) for
  [Rhub](https://github.com/r-hub), without which testing bugs on R-devel and
  other platforms would be a nightmare.
* [Hadley Wickham](https://github.com/hadley/) and [Peter
  Danenberg](https://github.com/klutometis) for
  [roxygen2](https://cran.r-project.org/package=roxygen2).
* [Yihui Xie](https://github.com/yihui) for
  [knitr](https://cran.r-project.org/package=knitr) and  [J.J.
  Allaire](https://github.com/jjallaire) etal for
  [rmarkdown](https://cran.r-project.org/package=rmarkdown), and by extension
  John MacFarlane for [pandoc](https://pandoc.org/).
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
* [Gábor Csárdi](https://github.com/gaborcsardi), the
  [R-consortium](https://r-consortium.org/), etal for
  [revdepcheck](https://github.com/r-lib/revdepcheck) to simplify reverse
  dependency checks.
* All open source developers out there that make their work freely available
  for others to use.
* [Github](https://github.com/), [Codecov](https://about.codecov.io/),
  [Vagrant](https://www.vagrantup.com/), [Docker](https://www.docker.com/),
  [Ubuntu](https://ubuntu.com/), [Brew](https://brew.sh/) for providing
  infrastructure that greatly simplifies open source development.
* [Free Software Foundation](https://www.fsf.org/) for developing the GPL
  license and promotion of the free software movement.

[1]: https://cran.r-project.org/package=diffobj/vignettes/diffobj.html
[2]: https://stat.ethz.ch/R-manual/R-devel/library/tools/html/Rdiff.html
