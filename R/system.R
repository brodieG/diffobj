#' Remove DLLs when package is unloaded

.onLoad <- function(libname, pkgname) {
  default.opts <- list(
    diffobj.context=c(2L),
    diffobj.white.space=FALSE,
    diffobj.line.limit=c(50L, 20L)
  )
  existing.opts <- options()
  options(default.opts[setdiff(names(default.opts), names(existing.opts))])
}
.onUnload <- function(libpath) {
  library.dynam.unload("diffobj", libpath)
}

