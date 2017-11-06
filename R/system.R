# Copyright (C) 2017  Brodie Gaslam
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

# Default options; beware of defining default options that may have different
# values during package install, which is when this list is contructed, and
# function runtime

.default.opts <- list(
  diffobj.context=2L,
  diffobj.context.auto.min=1L,
  diffobj.context.auto.max=10L,
  diffobj.ignore.white.space=TRUE,
  diffobj.convert.hz.white.space=TRUE,
  diffobj.line.limit=-1L,
  diffobj.pager="auto",
  diffobj.pager.mode="threshold",
  diffobj.pager.threshold=-1L,
  diffobj.less.flags="R",
  diffobj.word.diff=TRUE,
  diffobj.unwrap.atomic=TRUE,
  diffobj.rds=TRUE,
  diffobj.hunk.limit=-1L,
  diffobj.mode="auto",
  diffobj.silent=FALSE,
  diffobj.max.diffs=50000L,
  diffobj.align=NULL,           # NULL == AlignThreshold()
  diffobj.align.threshold=0.25,
  diffobj.align.min.chars=3L,
  diffobj.align.count.alnum.only=TRUE,
  diffobj.style="auto",
  diffobj.format="auto",
  diffobj.interactive=NULL,     # NULL == interactive()
  diffobj.color.mode="yb",
  diffobj.term.colors=NULL,
  diffobj.brightness="neutral",
  diffobj.tab.stops=8L,
  diffobj.disp.width=0L,        # 0L == use style width, see param docs
  diffobj.palette=NULL,         # NULL == PaletteOfStyles()
  diffobj.guides=TRUE,
  diffobj.trim=TRUE,
  diffobj.html.escape.html.entities=TRUE,
  diffobj.html.js=NULL,         # NULL == diffobj_js()
  diffobj.html.css=NULL,        # NULL == diffobj_css()
  diffobj.html.scale=TRUE,
  diffobj.html.output="auto"
)

# nocov start
.onLoad <- function(libname, pkgname) {
  # Scheme defaults are fairly complex...

  existing.opts <- options()
  options(.default.opts[setdiff(names(.default.opts), names(existing.opts))])
}
#' Remove DLLs when package is unloaded

.onUnload <- function(libpath) {
  library.dynam.unload("diffobj", libpath)
}
# nocov end

#' Shorthand Function for Accessing diffobj Options
#'
#' \code{gdo(x)} is equivalent to \code{getOption(sprintf("diffobj.\%s", x))}.
#'
#' @export
#' @param x character(1L) name off \code{diffobj} option to retrieve, without
#'   the \dQuote{diffobj.} prefix
#' @examples
#' gdo("format")

gdo <- function(x) getOption(sprintf("diffobj.%s", x))

#' Set All diffobj Options to Defaults
#'
#' Used primarily for testing to ensure all options are set to default values.
#'
#' @export
#' @return list for use with \code{options} that contains values of
#'   \code{diffob} options before they were forced to defaults
#' @examples
#' \dontrun{
#'   diffobj_set_def_opts()
#' }

diffobj_set_def_opts <- function() options(.default.opts)
