#' Remove DLLs when package is unloaded

.onLoad <- function(libname, pkgname) {
  default.opts <- list(
    diffobj.context=c(2L),
    diffobj.white.space=FALSE,
    diffobj.line.limit=-1L,
    diffobj.hunk.limit=-1L,
    diffobj.use.ansi=ansistyle::ansi_available(),
    diffobj.mode="unified"
  )
  existing.opts <- options()
  options(default.opts[setdiff(names(default.opts), names(existing.opts))])
}
.onUnload <- function(libpath) {
  library.dynam.unload("diffobj", libpath)
}

