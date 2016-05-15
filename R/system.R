#' @include styles.R

.onLoad <- function(libname, pkgname) {
  # Scheme defaults are fairly complex...

  default.opts <- list(
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
    diffobj.hunk.limit=-1L,
    diffobj.mode="unified",
    diffobj.silent=FALSE,
    diffobj.max.diffs=50000L,
    diffobj.align.threshold=0.25,
    diffobj.style="auto",
    diffobj.format="auto",
    diffobj.color.mode="yb",
    diffobj.brightness="neutral",
    diffobj.tab.stops=8L,
    diffobj.disp.width=NULL,  # NULL == getOption("width")
    diffobj.palette=NULL,     # NULL == PaletteOfStyles()
    diffobj.guides=TRUE,
    diffobj.html.escape.html.entities=TRUE,
    diffobj.html.css=
      file.path(system.file(package="diffobj"), "css", "diffobj.css"),
    diffobj.html.css.mode="auto"
  )
  existing.opts <- options()
  options(default.opts[setdiff(names(default.opts), names(existing.opts))])
}
#' Remove DLLs when package is unloaded

.onUnload <- function(libpath) {
  library.dynam.unload("diffobj", libpath)
}

#' Shorthand Function for Accessing diffobj Options
#'
#' \code{gdo(x)} is equivalent to \code{getOption(sprintf("diffobj.\%s", x))}.
#'
#' @export
#' @param x character(1L) name off \code{diffobj} option to retrieve, without
#'   the \dQuote{diffobj.} prefix

gdo <- function(x) getOption(sprintf("diffobj.%s", x))
