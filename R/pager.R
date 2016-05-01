setClass(
  "diffObjPager",
  contains="virtual",
  slots=c(pager="function", mode="character", file.ext="character"),
  prototype=list(file.ext=""),
  validity=function(object) {
    if(!is.chr.1L(object@file.ext)) return("Invalid `file.ext` slot")
    if(!is.pager_mode(object@mode)) return("Invalid `mode` slot")
    if(!is.int.1L(object@threshold)) return("Invalid `threshold` slot")
    TRUE
  }
)
setClass(
  "diffObjPagerSystem", contains="diffObjPager", slots=c(threshold="integer"),
  prototype=list(pager=file.show),
  validity=function(object) {
    if(!is.int.1L(object@threshold)) return("Invalid `threshold` slot")
    TRUE
  }
)
setClass(
  "diffObjPagerSystemLess", contains="diffObjPagerSystem", slots=c("flags"),
  prototype=list(
    pager=function(x) {
      old.less <- set_less_var("R")
      on.exit(reset_less_var(old.less), add=TRUE)
      file.show(x)
  } )
)
setClass("diffObjPagerBrowser", contains="diffObjPager",
  prototype=list(file.ext="html", pager=browseURL),
  validity=function(object) {
    if(!object@mode %in% c("always", "never"))
      return("Invalid `mode` slot.")
    TRUE
} )
