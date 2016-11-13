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

# Check Whether Input Could Be Reference to RDS File and Load if it Is

get_rds <- function(x) {
  if(is.chr.1L(x) && file_test("-f", x) || inherits(x, "connection")) {
    tryCatch(suppressWarnings(readRDS(x)), error=function(e) x)
  } else x
}
