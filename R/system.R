
.onLoad <- function(libname, pkgname) {
  default.opts <- list(
    diffobj.context="auto",
    diffobj.context.auto.min=1L,
    diffobj.context.auto.max=-1L,
    diffobj.ignore.white.space=TRUE,
    diffobj.line.limit=c(50L, 25L),
    diffobj.hunk.limit=-1L,
    diffobj.use.ansi=crayon::has_color(),
    diffobj.mode="unified",
    diffobj.silent=FALSE,
    diffobj.max.diffs=10000L,
    diffobj.max.diffs.in.hunk=100L,
    diffobj.max.diffs.wrap=10000L,
    diffobj.tab.stops=8L
  )
  existing.opts <- options()
  options(default.opts[setdiff(names(default.opts), names(existing.opts))])
}
#' Remove DLLs when package is unloaded

.onUnload <- function(libpath) {
  library.dynam.unload("diffobj", libpath)
}

