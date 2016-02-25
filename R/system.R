
.onLoad <- function(libname, pkgname) {
  default.opts <- list(
    diffobj.context=c(2L),
    diffobj.ignore.white.space=TRUE,
    diffobj.line.limit=-1L,
    diffobj.hunk.limit=-1L,
    diffobj.use.ansi=ansistyle::ansi_available(),
    diffobj.mode="unified",
    diffobj.silent=FALSE,
    diffobj.max.diffs=10000L,
    diffobj.max.diffs.in.hunk=100L,
    diffobj.max.diffs.wrap=10000L
  )
  existing.opts <- options()
  options(default.opts[setdiff(names(default.opts), names(existing.opts))])
}
#' Remove DLLs when package is unloaded

.onUnload <- function(libpath) {
  library.dynam.unload("diffobj", libpath)
}

