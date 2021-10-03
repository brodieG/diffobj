# Copyright (C) 2021 Brodie Gaslam

# This file is part of "aammrtf - An Almost Most Minimal R Test Framework"
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 or 3 of the License.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# Go to <https://www.r-project.org/Licenses/GPL-2> for a copy of the license.

flist <- function(x, y) paste0(x, paste0("'", basename(y), "'", collapse=", "))
report <- function(x) {writeLines(character(13)); stop(x, call.=FALSE)}

test.out <- list.files(pattern="\\.Rout$")
non.ascii <- which(lengths(lapply(test.out, tools::showNonASCIIfile)) > 0)
if(length(non.ascii))
  warning(flist("Some outputs contain non-ASCII:\n", test.out[non.ascii]))

tar <- list.files(pattern='\\.Rout\\.save$', full.names=TRUE)
cur <- file.path(dirname(tar), sub('\\.save$', '', basename(tar)))
awol <- !file.exists(cur)

if(any(awol)) report(flist(".Rout files missing (failed?):\n", cur[awol]))

diff.dat <- Map(tools::Rdiff, tar[!awol], cur[!awol], useDiff=TRUE, Log=TRUE)
diffs <- vapply(diff.dat, '[[', 1, 'status')
if(any(!!diffs)) report(flist("Test output differences:\n", cur[!!diffs]))
