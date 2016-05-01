
.onLoad <- function(libname, pkgname) {
  # Scheme defaults are fairly complex...

  default.opts <- list(
    diffobj.context="auto",
    diffobj.context.auto.min=1L,
    diffobj.context.auto.max=10L,
    diffobj.ignore.white.space=TRUE,
    diffobj.convert.hz.white.space=TRUE,
    diffobj.line.limit=-1L,
    diffobj.pager.mode="threshold",
    diffobj.pager.threshold=-1L,
    diffobj.less.flags="R",
    diffobj.hunk.limit=-1L,
    diffobj.use.ansi=crayon::has_color(),
    diffobj.mode="unified",
    diffobj.silent=FALSE,
    diffobj.max.diffs=50000L,
    diffobj.max.diffs.in.hunk=50000L,
    diffobj.max.diffs.wrap=50000L,
    diffobj.align.threshold=0.25,
    diffobj.style="basic",
    diffobj.tab.stops=8L,
    diffobj.disp.width=getOption("width"),
    diffobj.html.escape.html.entities=TRUE,
    diffobj.html.css=
      file.path(system.file(package="diffobj"), "css", "diffobj.css"),
    diffobj.html.css.mode="auto",
    diffobj.html.use.browser="auto",
    diffobj.html.as.page="auto"
  )
  existing.opts <- options()
  options(default.opts[setdiff(names(default.opts), names(existing.opts))])
}
#' Remove DLLs when package is unloaded

.onUnload <- function(libpath) {
  library.dynam.unload("diffobj", libpath)
}

gdO <- function(x) getOption(sprintf("diffobj.%s", x))
