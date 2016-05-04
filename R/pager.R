setClass(
  "diffObjPager",
  contains="VIRTUAL",
  slots=c(pager="function", file.ext="character", threshold="integer"),
  prototype=list(file.ext="", threshold=0L),
  validity=function(object) {
    if(!is.chr.1L(object@file.ext)) return("Invalid `file.ext` slot")
    if(!is.int.1L(object@threshold)) return("Invalid `threshold` slot")
    TRUE
  }
)
diffObjPagerOff <- setClass("diffObjPagerOff", contains="diffObjPager")
diffObjPagerSystem <- setClass(
  "diffObjPagerSystem", contains="diffObjPager",
  prototype=list(pager=file.show),
)
diffObjPagerSystemLess <- setClass(
  "diffObjPagerSystemLess", contains="diffObjPagerSystem", slots=c("flags"),
  prototype=list(
    pager=function(x) {
      old.less <- set_less_var("R")
      on.exit(reset_less_var(old.less), add=TRUE)
      file.show(x)
  } )
)
diffObjPagerBrowser <- setClass(
  "diffObjPagerBrowser", contains="diffObjPager",
  prototype=list(file.ext="html", pager=browseURL)
)
