#' Remove DLLs when package is unloaded

.onLoad <- function(libname, pkgname) {
  options(diffobj.test.context=c(10L, 5L))
}
.onUnload <- function(libpath) {
  library.dynam.unload("diffobj", libpath)
}

