# diffobj - Compare R Objects with a Diff

## Overview

Diffs provide a clear and intuitive accounting of the differences between two objects because they present the objects in usual way except modified to highlight the differences.  This package contains a collection of functions for carrying out and displaying colorized diffs on R objects for display in a terminal or in a browser.  This is inspired by `tools::Rdiff`, except the diff is computed directly on R objects instead of text files, the diff computation is independent of the system `diff` utility, and the diff display is colorized and optimized to handle common R objects.

See [vignette](http://htmlpreview.github.io/?https://raw.githubusercontent.com/brodieG/diffobj/rc/inst/doc/diffobj.html) for details.

## Examples

Output can be colorized with ANSI escape sequences, or in HTML.  Some examples or terminal output:

![samples](/vignettes/ans256brightness.jpg)

## Installation

Currently this package is only available on Github:

```
devtools::install_github("brodieG/diffobj")
vignette("diffobj", package="diffobj")
```
