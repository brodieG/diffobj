#' @include html.R

NULL

# maybe this shouldn't be an S4 class since the function slot doesn't work
# for classed functions (e.g. the ones produced by crayon)

#' Customize Appearance of Diff
#'
#' Most of the customization is done by specifying functions that operate on
#' character vectors and return a modified character vector of the same length.
#' The intended use case is to pass \code{crayon} functions such as
#' \code{\link{crayon::red}}, although you may pass any function of your liking
#' that behaves as described.
#'
#' The visual representation of the diff has many nested components.  The
#' functions you specify here will be applied with the function corresponding to
#' the innermost element applied first.  A schematic of the various component
#' that represent an inserted line follows:
#' \preformatted{+- line ----------------------------------------+
#' |+- line.ins ----------------------------------+|
#' ||+- gutter ---------+ +- text ---------------+||
#' |||+- gutter.ins ---+| |+- text.ins ---------+|||
#' ||||                || ||      +- word.ins -+||||
#' |||| gutter.ins.txt || || DIFF | TEXT HERE  |||||
#' ||||                || ||      +------------+||||
#' |||+----------------+| |+--------------------+|||
#' ||+------------------+ +----------------------+||
#' |+---------------------------------------------+|
#' +-----------------------------------------------+
#' }
#' A similar model applies to deleted and matching lines.  The boxes represent
#' functions.  \code{gutter.ins.txt} represents the text to use in the gutter
#' and is not a function. \code{DIFF TEXT HERE} is text from the objects being
#' diffed, with the portion that has different words inside the \code{word.ins}
#' box provided word diff is enabled, and is obviously not a function either.
#'
#' Most of the functions defined here default to \code{\link{identity}}, but
#' you are given the flexibility to fully format the diff.
#'
#' @note in \dQuote{sidebyside} there are two lines per row of text, one showing
#'   deletions and one showing additions.
#' @param line function
#' @param line.ins function
#' @param line.del function
#' @param line.match function
#' @param text function
#' @param text.ins function
#' @param text.del function
#' @param text.match function
#' @param gutter function
#' @param gutter.ins function
#' @param gutter.del function
#' @param gutter.match function
#' @param hunk.header function to format each hunk header with
#' @param banner.ins function to format insertion banner
#' @param banner.del function to format deletion banner
#' @param banner function to format entire banner
#' @param meta function format meta information lines
#' @param gutter.ins.txt character(1L) text to use as visual cue to indicate
#'   whether a diff line is an insertion, defaults to \dQuote{> }
#' @param gutter.ins.txt.ctd character(1L) if a diff line is wrapped, the
#'   visual cue shifts to this character to indicate wrapping occured
#' @param gutter.del.txt character(1L) see \code{gutter.ins.txt} above
#' @param gutter.del.txt.ctd character(1L) see \code{gutter.ins.txt.ctd} above
#' @param gutter.match.txt character(1L) see \code{gutter.ins.txt} above
#' @param gutter.match.txt.ctd character(1L) see \code{gutter.ins.txt.ctd} above
#' @return diffObjStyle S4 object
#' @export diffObjStyle
#' @exportClass diffObjStyle

diffObjStyleFuns <- setClass(
  "diffObjStyleFuns",
  slots=c(
    container="ANY", row="ANY",
    line="ANY", line.insert="ANY", line.delete="ANY", line.match="ANY",
    text="ANY", text.insert="ANY", text.delete="ANY", text.match="ANY",
    gutter="ANY",
    gutter.insert="ANY", gutter.insert.ctd="ANY",
    gutter.delete="ANY", gutter.delete.ctd="ANY",
    gutter.match="ANY", gutter.match.ctd="ANY",
    gutter.pad="ANY",
    word.insert="ANY", word.delete="ANY",
    banner="ANY", banner.insert="ANY", banner.delete="ANY",
    context.sep="ANY", header="ANY", meta="ANY"
  ),
  prototype=list(
    container=identity, row=identity, line=identity,
    line.insert=identity, line.delete=identity, line.match=identity,
    text=identity,
    text.insert=identity, text.delete=identity, text.match=identity,
    gutter=identity, gutter.pad=identity,
    gutter.insert=identity, gutter.insert.ctd=identity,
    gutter.delete=identity, gutter.delete.ctd=identity,
    gutter.match=identity, gutter.match.ctd=identity,
    word.insert=identity, word.delete=identity,
    banner=identity, banner.insert=identity, banner.delete=identity,
    header=identity,
    context.sep=identity,
    meta=identity
  ),
  validity=function(object){
    for(i in slotNames(object)) {
      if(!is.function(slot(object, i)))
        return(paste0("Argument `", i, "` should be a function."))
      frm <- formals(slot(object, i))
      non.def <- vapply(
        names(frm),
        function(x)
          is.name(frm[[x]]) && !nzchar(as.character(frm[[x]])) && x != "...",
        logical(1L)
      )
      if(sum(non.def) > 1L)
        return(
         paste0(
          "Argument `", i,
          "` may not have more than one non-default formal argument"
        ) )
    }
    TRUE
  }
)
diffObjStyleText <- setClass(
  "diffObjStyleText",
  slots=c(
    gutter.insert="character", gutter.insert.ctd="character",
    gutter.delete="character", gutter.delete.ctd="character",
    gutter.match="character", gutter.match.ctd="character",
    gutter.pad="character",
    context.sep="character",
    pad.col="character"
  ),
  prototype=list(
    gutter.insert=">", gutter.insert.ctd=":",
    gutter.delete="<", gutter.delete.ctd=":",
    gutter.match=" ", gutter.match.ctd=" ",
    gutter.pad=" ",
    context.sep="~~~~~",
    pad.col=" "
  ),
  validity=function(object){
    for(i in slotNames(object)) if(!is.chr.1L(slot(object, i)))
      return(paste0("Argument `", i, "` must be character(1L) and not NA."))
    TRUE
  }
)
diffObjStyle <- setClass(
  "diffObjStyle",
  slots=c(
    funs="diffObjStyleFuns",
    text="diffObjStyleText",
    wrap="logical",
    pad="logical",
    disp.width="integer",
    line.width="integer",
    text.width="integer"
  ),
  prototype=list(
    funs=diffObjStyleFuns(),
    text=diffObjStyleText(),
    disp.width=getOption("diffobj.disp.width"),
    text.width=0L,
    line.width=0L,
    wrap=TRUE,
    pad=TRUE
  ),
  validity=function(object){
    int.1L.and.pos <- c("disp.width", "line.width", "text.width")
    for(i in int.1L.and.pos)
      if(!is.int.1L(slot(object, i)) || slot(object, i) < 0L)
        return(sprintf("Slot `%s` must be integer(1L) and positive"), i)
    if(!is.TF(object@wrap))
      return("Slot `wrap` must be TRUE or FALSE")
    if(!is.TF(object@pad))
      return("Slot `pad` must be TRUE or FALSE")
    TRUE
  }
)
setMethod("initialize", "diffObjStyle", function(.Object, ...) {
  if(is.numeric(.Object@disp.width))
    .Object@disp.width <- as.integer(disp.width)
  if(is.null(.Object@disp.width))
    .Object@disp.width <- 80L
  return(callNextMethod(.Object, ...))
} )
diffObjStyleDefault <- setClass(
  "diffObjStyleDefault", contains="diffObjStyle",
  prototype=list(
    funs=diffObjStyleFuns(
      word.insert=crayon::green, word.delete=crayon::red,
      gutter.insert=crayon::green,
      gutter.insert.ctd=crayon::green,
      gutter.delete=crayon::red,
      gutter.delete.ctd=crayon::red,
      header=crayon::cyan,
      meta=crayon::silver,
      context.sep=crayon::silver
  ) )
)
diffObjStyleHtml <- setClass(
  "diffObjStyleHtml", contains="diffObjStyle",
  slots=c(css="character"),
  prototype=list(
    funs=diffObjStyleFuns(
      container=function(x) c("<div class='diffobj_container'>", x, "</div>"),
      row=div_f("row"),
      line.insert=div_f("insert"),
      line.delete=div_f("delete"),
      line.match=div_f("match"),
      line=div_f("line"),
      text.insert=div_f("insert"),
      text.delete=div_f("delete"),
      text.match=div_f("match"),
      text=div_f("text"),
      gutter.insert=div_f("insert"),
      gutter.delete=div_f("delete"),
      gutter.match=div_f("match"),
      gutter=div_f("gutter"),
      word.insert=span_f(c("word", "insert")),
      word.delete=span_f(c("word", "delete")),
      header=div_f(c("header"))
    ),
    text=diffObjStyleText(
      gutter.insert="&gt;",
      gutter.delete="&lt;",
      gutter.match="&nbsp;"
    ),
    wrap=FALSE,
    pad=FALSE,
    css=file.path(system.file(package="diffobj"), "css", "diffobj.css")
  ),
  validity=function(object) {
    if(!is.chr.1L(css))
      return("slot `css` must be character(1L)")
    TRUE
  }
)


# # if(theme == "core") {
#     diffObjStyle(disp.width=getOption("width"))
#   } else if(theme == "default") {
#     diffObjStyle(
#       word.insert=crayon::green, word.delete=crayon::red,
#       gutter.insert=crayon::green,
#       gutter.insert.ctd=crayon::green,
#       gutter.delete=crayon::red,
#       gutter.delete.ctd=crayon::red,
#       header=crayon::cyan,
#       meta=crayon::silver,
#       context.sep=crayon::silver
#     )
#   } else if(theme == "html") {
#     diffObjStyle(
#       line.insert=div_f(c("line", "insert")),
#       line.delete=div_f(c("line", "delete")),
#       line.match=div_f(c("line")),
#       word.insert=span_f(c("word", "insert")),
#       word.delete=span_f(c("word", "delete")),
#       header=div_f(c("line", "header"))
#     )
#   } else if(theme == "git") {
#     ins <- crayon::make_style(rgb(0, 5, 0, maxColorValue=5))
#     word.insert.fg <- identity
#     word.insert.bg <- crayon::make_style(rgb(0, 1, 0, maxColorValue=5), bg=TRUE)
#     word.insert <- function(x) word.insert.bg(word.insert.fg(x))
# 
#     del <- crayon::make_style(rgb(5, 0, 0, maxColorValue=5))
#     word.delete.fg <- identity
#     word.delete.bg <- crayon::make_style(rgb(1, 0, 0, maxColorValue=5), bg=TRUE)
#     word.delete <- function(x) word.delete.bg(word.delete.fg(x))
# 
#     diffObjStyle(
#       line.insert=ins,
#       line.delete=del,
#       word.insert=word.insert,
#       word.delete=word.delete,
#       banner.insert=crayon::green,
#       banner.delete=crayon::red,
#       header=crayon::cyan,
#       meta=crayon::silver
#     )
#   } else if(theme == "git.2") {
#     ins <- crayon::make_style(rgb(0, 5, 0, maxColorValue=5))
#     word.insert.fg <- crayon::reset
#     word.insert.bg <- crayon::make_style(rgb(0, 1, 0, maxColorValue=5), bg=TRUE)
#     word.insert <- function(x) word.insert.fg(word.insert.bg(x))
# 
#     del <- crayon::make_style(rgb(5, 0, 0, maxColorValue=5))
#     word.delete.fg <- crayon::reset
#     word.delete.bg <- crayon::make_style(rgb(1, 0, 0, maxColorValue=5), bg=TRUE)
#     word.delete <- function(x) word.delete.fg(word.delete.bg(x))
# 
#     diffObjStyle(
#       line.insert=ins,
#       line.delete=del,
#       word.insert=word.insert,
#       word.delete=word.delete,
#       banner.insert=crayon::green,
#       banner.delete=crayon::red,
#       header=crayon::cyan,
#       meta=crayon::silver
#     )
#   } else if(theme == "git.3") {
#     ins <- crayon::make_style(rgb(0, 5, 0, maxColorValue=5))
#     word.insert.fg <- crayon::reset
#     word.insert.bg <- crayon::make_style(rgb(0, 1, 0, maxColorValue=5), bg=TRUE)
#     word.insert <- crayon::green
# 
#     del <- crayon::make_style(rgb(5, 0, 0, maxColorValue=5))
#     word.delete.fg <- crayon::reset
#     word.delete.bg <- crayon::make_style(rgb(1, 0, 0, maxColorValue=5), bg=TRUE)
#     word.delete <- crayon::red
# 
#     diffObjStyle(
#       line.insert=identity,
#       line.delete=identity,
#       word.insert=word.insert,
#       word.delete=word.delete,
#       banner.insert=crayon::green,
#       banner.delete=crayon::red,
#       header=crayon::cyan,
#       meta=crayon::silver
#     )
#   } else if(theme == "stripes") {
#     ins <- crayon::make_style(rgb(0, 1, 0, maxColorValue=5), bg=TRUE)
#     word.insert.fg <- crayon::make_style(rgb(0, 5, 0, maxColorValue=5))
#     word.insert <- function(x) crayon::bold(word.insert.fg(x))
#     del <- crayon::make_style(rgb(1, 0, 0, maxColorValue=5), bg=TRUE)
#     word.delete.fg <- crayon::make_style(rgb(5, 0, 0, maxColorValue=5))
#     word.delete <- function(x) crayon::bold(word.delete.fg(x))
#     diffObjStyle(
#       line.insert=ins,
#       line.delete=del,
#       word.insert=word.insert.fg,
#       word.delete=word.delete.fg,
#       banner.insert=crayon::green,
#       banner.delete=crayon::red,
#       header=crayon::cyan,
#       meta=crayon::silver
#     )
#   } else if(theme == "stripes.light") {
#     ins <- crayon::make_style(rgb(4, 5, 4, maxColorValue=5), bg=TRUE)
#     word.insert.fg <- crayon::make_style(rgb(0, 3, 0, maxColorValue=5))
#     word.insert <- function(x) crayon::bold(word.insert.fg(x))
#     del <- crayon::make_style(rgb(5, 4, 4, maxColorValue=5), bg=TRUE)
#     word.delete.fg <- crayon::make_style(rgb(3, 0, 0, maxColorValue=5))
#     word.delete <- function(x) crayon::bold(word.delete.fg(x))
#     diffObjStyle(
#       line.insert=ins,
#       line.delete=del,
#       word.insert=word.insert,
#       word.delete=word.delete,
#       banner.insert=crayon::green,
#       banner.delete=crayon::red,
#       header=crayon::cyan,
#       meta=crayon::silver
#     )
#   } else if(theme == "checkers.light") {
#     ins <- crayon::make_style(rgb(4, 5, 4, maxColorValue=5), bg=TRUE)
#     word.insert <- crayon::make_style(rgb(2, 4, 2, maxColorValue=5), bg=TRUE)
#     ins.fg <- crayon::make_style(rgb(0, 3, 0, maxColorValue=5))
#     del <- crayon::make_style(rgb(5, 4, 4, maxColorValue=5), bg=TRUE)
#     del.fg <- crayon::make_style(rgb(3, 0, 0, maxColorValue=5))
#     word.delete <- crayon::make_style(rgb(4, 2, 2, maxColorValue=5), bg=TRUE)
#     diffObjStyle(
#       text.insert=ins,
#       text.delete=del,
#       word.insert=word.insert,
#       word.delete=word.delete,
#       gutter.insert=ins.fg,
#       gutter.delete=del.fg,
#       header=crayon::cyan,
#       meta=crayon::silver
#     )
#   } else if(theme == "checkers.dark") {
#     ins <- crayon::make_style(rgb(0, 1, 0, maxColorValue=5), bg=TRUE)
#     word.insert <- crayon::make_style(rgb(1, 4, 1, maxColorValue=5), bg=TRUE)
#     word.insert.fg <- crayon::make_style(rgb(0, 1, 0, maxColorValue=5))
#     del <- crayon::make_style(rgb(1, 0, 0, maxColorValue=5), bg=TRUE)
#     word.delete <- crayon::make_style(rgb(4, 1, 1, maxColorValue=5), bg=TRUE)
#     word.delete.fg <- crayon::make_style(rgb(1, 0, 0, maxColorValue=5))
#     diffObjStyle(
#       text.insert=ins,
#       text.delete=del,
#       word.insert=function(x) word.insert(word.insert.fg(x)),
#       word.delete=function(x) word.delete(word.delete.fg(x)),
#       gutter.insert=word.insert.fg,
#       gutter.delete=word.delete.fg,
#       banner.insert=crayon::green,
#       banner.delete=crayon::red,
#       header=crayon::cyan,
#       meta=crayon::silver
#     )
#   } else if(theme == "text") {
#     diffObjStyle(
#       word.insert=function(x) paste0(">|", x, "|>"),
#       word.delete=function(x) paste0("<|", x, "|<")
#     )
#   }
