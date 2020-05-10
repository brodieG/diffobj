# Copyright (C) 2020 Brodie Gaslam
#
# This file is part of "diffobj - Diffs for R Objects"
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# Go to <https://www.r-project.org/Licenses/GPL-2> for a copy of the license.

#' @include styles.R

NULL


# nocov start
.onLoad <- function(libname, pkgname) {
  # Scheme defaults are fairly complex...

  existing.opts <- options()
  options(.default.opts[setdiff(names(.default.opts), names(existing.opts))])
  trimws <<- if(getRversion() < "3.2.0") trimws2 else base::trimws
}
# Remove DLLs when package is unloaded

.onUnload <- function(libpath) {
  library.dynam.unload("diffobj", libpath)
}
# nocov end

