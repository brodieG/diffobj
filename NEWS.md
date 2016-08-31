# diffobj

## v0.1.4

* [#67](https://github.com/brodieG/diffobj/issues/67) Fix CRAN Binaries
* Clarified that C code is heaviliy modified and incompatible with original
  `libmba` implementation

## v0.1.3

* First version on CRAN
* [#51](https://github.com/brodieG/diffobj/issues/51) use RStudio viewport to display HTML diffs when running in RStudio, as per [Noam Ross](https://twitter.com/noamross/status/760115813559009280)
* [#54](https://github.com/brodieG/diffobj/issues/54), [#55](https://github.com/brodieG/diffobj/issues/55), scale HTML output to viewport width (see `?Style`)
* [#53](https://github.com/brodieG/diffobj/issues/53), default term colors computed on run instead of on package load
* [#56](https://github.com/brodieG/diffobj/issues/56), disable wrap for HTML output
* HTML output now captured with default width 80 since there is no explicit relationship between HTML viewport width and `getOption("width")`
* The `style` parameter now accepts lists to use as instantiation arguments for `Style` objects (see `?Style`)
* Fix subtle rendering and formatting application flaws
* Switch Travis shields to SVG per [Gábor Csárdi](https://github.com/gaborcsardi/diffobj/commit/710251f2cd663bfdadcab9aea6a37f9eb4a87599)
* Improve in-hunk alignment of partially matching lines
* Compile with `-pedantic`, fix related warnings [Arun](http://stackoverflow.com/users/559784/arun)
* Improved coverage and more robust testing
* Several internal structure changes to accomodate improvements

## v0.1.2

* [#46](https://github.com/brodieG/diffobj/issues/46): Guide and Trim Problems with Lists
* [#47](https://github.com/brodieG/diffobj/issues/47): Output Format in non-ANSI Terminals Without Browser (reported by [Frank](https://github.com/brodieG/diffobj/issues/47))
* [#48](https://github.com/brodieG/diffobj/issues/48): `make_blocking` Default prompt Confusing (reported by [Frank](https://github.com/brodieG/diffobj/issues/47))
* [#49](https://github.com/brodieG/diffobj/issues/49): In-Hunk Word Diffs Issues when Unwrap-diffing Atomics
* [#50](https://github.com/brodieG/diffobj/issues/50): CSS Lost in Rstudio Server Sessions (reported by [Steven Beaupré](https://chat.stackoverflow.com/users/4064778/steven-beaupre))

## v0.1.1

* Turn off unwrapping for _named_ atomic vectors (see [#43](https://github.com/brodieG/diffobj/issues/43))
* [#44](https://github.com/brodieG/diffobj/issues/44): Proper handling of NULL objects in `diffStr`
* [#41](https://github.com/brodieG/diffobj/issues/41): Compilation Issues in Winbuilder

## v0.1.0

* Initial Release
