#' @include styles.R


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
  diffobj.color.mode="yb",
  diffobj.brightness="neutral",
  diffobj.tab.stops=8L,
  diffobj.disp.width=0L,        # 0L == use style width, see param docs
  diffobj.palette=PaletteOfStyles(),
  diffobj.guides=TRUE,
  diffobj.trim=TRUE,
  diffobj.html.escape.html.entities=TRUE,
  diffobj.html.css=diffobj_css(),
  diffobj.html.output="auto"
)

.onLoad <- function(libname, pkgname) {
  # Scheme defaults are fairly complex...

  existing.opts <- options()
  options(.default.opts[setdiff(names(.default.opts), names(existing.opts))])
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

#' Set All diffobj Options to Defaults
#'
#' Used primarily for testing to ensure all options are set to default values.
#'
#' @export
#' @return list for use with \code{options} that contains values of
#'   \code{diffob} options before they were forced to defaults

diffobj_set_def_opts <- function() options(.default.opts)
