# Copyright (C) 2016  Brodie Gaslam
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

# Retrieves data from the data elements of the Diff object based on the index
# values provided in ind.  Positive values draw from `tar` elements, and
# negative draw from `cur` elements.
#
# returns a list with the elements.  If type is length 1, you will probably
# want to unlist the return value

get_dat <- function(x, ind, type) {
  stopifnot(
    is(x, "Diff"), is.integer(ind),
    is.chr.1L(type) && type %in% .diff.dat.cols
  )
  # Need to figure out what zero indices are; previously would return
  # NA_character_, but now since we're getting a whole bunch of different
  # stuff not sure what the right return value should be, or even if we produce
  # zero indices anymore
  get_dat_raw(ind, x@tar.dat[[type]], x@cur.dat[[type]])
}
get_dat_raw <- function(ind, tar, cur) {
  template <- tar[0L]
  length(template) <- length(ind)
  template[which(ind < 0L)] <-  cur[abs(ind[ind < 0L])]
  template[which(ind > 0L)] <-  tar[abs(ind[ind > 0L])]
  template
}
