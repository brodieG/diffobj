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
vignette("diffobj", package="diffobj")
```

## Development

Go to [Github](https://github.com/brodieG/diffobj) to report issues or if you
are interested in development versions of this package.  The `master` branch is
considered stable.  If you want to submit a PR please do so against the
`development` branch.

## Related Software

* [tools::Rdiff](https://stat.ethz.ch/R-manual/R-devel/library/tools/html/Rdiff.html)
* [Daff](https://cran.r-project.org/package=daff) diff, patch and merge for data.frames
* [GNU diff](https://www.gnu.org/software/diffutils)


