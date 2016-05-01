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
#' @rdname diffObjStyle
#' @name diffObjStyle
#' @export diffObjStyle
#' @exportClass diffObjStyle

NULL

diffObjStyleFuns <- setClass(
  "diffObjStyleFuns",
  slots=c(
    container="ANY", row="ANY",
    line="ANY", line.insert="ANY", line.delete="ANY", line.match="ANY",
    text="ANY", text.insert="ANY", text.delete="ANY", text.match="ANY",
    banner="ANY", banner.insert="ANY", banner.delete="ANY",
    gutter="ANY",
    gutter.insert="ANY", gutter.insert.ctd="ANY",
    gutter.delete="ANY", gutter.delete.ctd="ANY",
    gutter.match="ANY", gutter.match.ctd="ANY",
    gutter.pad="ANY",
    word.insert="ANY", word.delete="ANY",
    context.sep="ANY", header="ANY", meta="ANY"
  ),
  prototype=list(
    container=identity, row=identity,
    banner=identity, banner.insert=identity, banner.delete=identity,
    line=identity,
    line.insert=identity, line.delete=identity, line.match=identity,
    line.insert=identity, line.delete=identity, line.match=identity,
    text=identity,
    text.insert=identity, text.delete=identity, text.match=identity,
    gutter=identity, gutter.pad=identity,
    gutter.insert=identity, gutter.insert.ctd=identity,
    gutter.delete=identity, gutter.delete.ctd=identity,
    gutter.match=identity, gutter.match.ctd=identity,
    word.insert=identity, word.delete=identity,
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
    disp.width=0L,
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
    .Object@disp.width <- as.integer(.Object@disp.width)
  if(is.null(.Object@disp.width))
    .Object@disp.width <- 80L
  return(callNextMethod(.Object, ...))
} )
#' @export diffObjStyleAnsi8NeutralRgb
#' @exportClass diffObjStyleAnsi8NeutralRgb
#' @rdname diffObjStyle

diffObjStyleAnsi8NeutralRgb <- setClass(
  "diffObjStyleAnsi8NeutralRgb", contains="diffObjStyle",
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
#' @export diffObjStyleAnsi8NeutralYb
#' @exportClass diffObjStyleAnsi8NeutralYb
#' @rdname diffObjStyle

diffObjStyleAnsi8NeutralYb <- setClass(
  "diffObjStyleAnsi8NeutralYb", contains="diffObjStyle",
  prototype=list(
    funs=diffObjStyleFuns(
      word.insert=crayon::blue, word.delete=crayon::yellow,
      gutter.insert=crayon::blue,
      gutter.insert.ctd=crayon::blue,
      gutter.delete=crayon::yellow,
      gutter.delete.ctd=crayon::yellow,
      header=crayon::cyan,
      meta=crayon::silver,
      context.sep=crayon::silver
  ) )
)
#' @export diffObjStyleAnsi256LightRgb
#' @exportClass diffObjStyleAnsi256LightRgb
#' @rdname diffObjStyle

diffObjStyleAnsi256LightRgb <- setClass(
  "diffObjStyleAnsi256LightRgb", contains="diffObjStyle",
  prototype=list(
    funs=diffObjStyleFuns(
      text.insert=crayon::make_style(rgb(4, 5, 4, maxColorValue=5), bg=TRUE),
      text.delete=crayon::make_style(rgb(5, 4, 4, maxColorValue=5), bg=TRUE),
      word.insert=crayon::make_style(rgb(2, 4, 2, maxColorValue=5), bg=TRUE),
      word.delete=crayon::make_style(rgb(4, 2, 2, maxColorValue=5), bg=TRUE),
      gutter.insert=crayon::make_style(rgb(0, 3, 0, maxColorValue=5)),
      gutter.insert.ctd=crayon::make_style(rgb(0, 3, 0, maxColorValue=5)),
      gutter.delete=crayon::make_style(rgb(3, 0, 0, maxColorValue=5)),
      gutter.delete.ctd=crayon::make_style(rgb(3, 0, 0, maxColorValue=5)),
      header=crayon::make_style(rgb(0, 3, 3, maxColorValue=5)),
      meta=crayon::silver
) ) )
#' @export diffObjStyleAnsi256LightYb
#' @exportClass diffObjStyleAnsi256LightYb
#' @rdname diffObjStyle

diffObjStyleAnsi256LightYb <- setClass(
  "diffObjStyleAnsi256LightYb", contains="diffObjStyle",
  prototype=list(
    funs=diffObjStyleFuns(
      text.insert=crayon::make_style(rgb(3, 3, 5, maxColorValue=5), bg=TRUE),
      text.delete=crayon::make_style(rgb(4, 4, 2, maxColorValue=5), bg=TRUE),
      word.insert=crayon::make_style(rgb(2, 2, 4, maxColorValue=5), bg=TRUE),
      word.delete=crayon::make_style(rgb(3, 3, 1, maxColorValue=5), bg=TRUE),
      gutter.insert=crayon::make_style(rgb(0, 0, 3, maxColorValue=5)),
      gutter.insert.ctd=crayon::make_style(rgb(0, 0, 3, maxColorValue=5)),
      gutter.delete=crayon::make_style(rgb(2, 1, 0, maxColorValue=5)),
      gutter.delete.ctd=crayon::make_style(rgb(2, 1, 0, maxColorValue=5)),
      header=crayon::make_style(rgb(0, 3, 3, maxColorValue=5)),
      meta=crayon::silver
) ) )
#' @export diffObjStyleAnsi256DarkRgb
#' @exportClass diffObjStyleAnsi256DarkRgb
#' @rdname diffObjStyle

diffObjStyleAnsi256DarkRgb <- setClass(
  "diffObjStyleAnsi256DarkRgb", contains="diffObjStyle",
  prototype=list(
    funs=diffObjStyleFuns(
      text.insert=crayon::make_style(rgb(0, 1, 0, maxColorValue=5), bg=TRUE),
      text.delete=crayon::make_style(rgb(1, 0, 0, maxColorValue=5), bg=TRUE),
      word.insert=crayon::make_style(rgb(0, 3, 0, maxColorValue=5), bg=TRUE),
      word.delete=crayon::make_style(rgb(3, 0, 0, maxColorValue=5), bg=TRUE),
      gutter.insert=crayon::make_style(rgb(0, 2, 0, maxColorValue=5)),
      gutter.insert.ctd=crayon::make_style(rgb(0, 2, 0, maxColorValue=5)),
      gutter.delete=crayon::make_style(rgb(2, 0, 0, maxColorValue=5)),
      gutter.delete.ctd=crayon::make_style(rgb(2, 0, 0, maxColorValue=5)),
      header=crayon::cyan,
      meta=crayon::silver
) ) )
#' @export diffObjStyleAnsi256DarkYb
#' @exportClass diffObjStyleAnsi256DarkYb
#' @rdname diffObjStyle

diffObjStyleAnsi256DarkYb <- setClass(
  "diffObjStyleAnsi256DarkYb", contains="diffObjStyle",
  prototype=list(
    funs=diffObjStyleFuns(
      text.insert=crayon::make_style(rgb(0, 0, 1, maxColorValue=5), bg=TRUE),
      text.delete=crayon::make_style(rgb(1, 1, 0, maxColorValue=5), bg=TRUE),
      word.insert=crayon::make_style(rgb(0, 0, 4, maxColorValue=5), bg=TRUE),
      word.delete=crayon::make_style(rgb(3, 2, 0, maxColorValue=5), bg=TRUE),
      gutter.insert=crayon::make_style(rgb(0, 0, 3, maxColorValue=5)),
      gutter.insert.ctd=crayon::make_style(rgb(0, 0, 3, maxColorValue=5)),
      gutter.delete=crayon::make_style(rgb(1, 1, 0, maxColorValue=5)),
      gutter.delete.ctd=crayon::make_style(rgb(1, 1, 0, maxColorValue=5)),
      header=crayon::make_style(rgb(0, 3, 3, maxColorValue=5)),
      meta=crayon::silver
) ) )
#' @export diffObjStyleHtml
#' @exportClass diffObjStyleHtml
#' @rdname diffObjStyle

setClass(
  "diffObjStyleHtml", contains="diffObjStyle",
  slots=c(css="character"),
  prototype=list(
    funs=diffObjStyleFuns(
      container=cont_f(),
      row=div_f("row"),
      banner.insert=div_f("insert"),
      banner.delete=div_f("delete"),
      banner=div_f("line banner"),
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
    pad=FALSE
  ),
  validity=function(object) {
    if(!is.chr.1L(object@css))
      return("slot `css` must be character(1L)")
    TRUE
  }
)
# construct with default values specified via options; would this work with
# initialize?  Depends on whether this is run by package installation process

diffObjStyleHtml <- function(...) {
  args <- list(...)
  args.def <- list(css=getOption("diffobj.html.css"))
  args.comb <- c(args, args.def[!names(args.def) %in% names(args)])
  do.call("new", c("diffObjStyleHtml", args.comb))
}
#' @export diffObjStyleHtmlLightRgb
#' @exportClass diffObjStyleHtmlLightRgb
#' @rdname diffObjStyle

diffObjStyleHtmlLightRgb <- setClass(
  "diffObjStyleHtmlLightRgb", contains="diffObjStyleHtml"
)
setMethod("initialize", "diffObjStyleHtmlLightYb",
  function(.Object, ...) {
    .Object@funs@container <- cont_f(c("light", "rgb"))
    callNextMethod(.Object, ...)
  }
)
#' @export diffObjStyleHtmlLightYb
#' @exportClass diffObjStyleHtmlLightYb
#' @rdname diffObjStyle

diffObjStyleHtmlLightYb <- setClass(
  "diffObjStyleHtmlLightYb", contains="diffObjStyleHtml",
)
setMethod("initialize", "diffObjStyleHtmlLightYb",
  function(.Object, ...) {
    .Object@funs@container <- cont_f(c("light", "yb"))
    callNextMethod(.Object, ...)
  }
)
# Helper structure for constructing our defaults array

.dfs.dims <- list(
  format=c("raw", "ansi8", "ansi256", "html"),
  brightness=c("neutral", "light", "dark"),
  color=c("full", "yb")  # add b/w?
)
.dfs.dims.sizes <- vapply(.dfs.dims, length, integer(1L))
.dfs.arr <- array(
  vector("list", prod(.dfs.dims.sizes)), dim=.dfs.dims.sizes, dimnames=.dfs.dims
)

#' Class for Tracking Default Styles
#'
#' Provides a mechanism for complying with style defaults that constrain styles
#' along one of the style dimensions without actually specifying a style.
#' For example, one might require that a style be \dQuote{light} in tone, but
#' not care about what particular style is used.  See examples for usage.
#'
#' @section Dimensions:
#'
#' There are three general orthogonal dimensions of styles that can be used when
#' rendering diffs: the type of format, the \dQuote{luminosity} of the output,
#' and whether the colors used are distinguishable if you assume reds and greens
#' are not distinguishable.  Defaults for the intersections each of these
#' dimensions are encoded as a three dimensional list.
#'
#' The list dimensions are:
#' \itemize{
#'   \item format: the format type, typically \dQuote{raw}, \dQuote{ansi8},
#'     \dQuote{ansi256}, \dQuote{html}
#'   \item brightness: whether the colors are bright or not, which allows user to
#'     chose a scheme that is compatible with their console, typically:
#'     \dQuote{light}, \dQuote{dark}, \dQuote{normal}
#'   \item color: whether to use full color schemes or not for compatibility with
#'     people with reduced sensitivy to differences between red and green colors,
#'     typically \dQuote{full} or \dQuote{yb} for Yellow Blue.
#' }
#' @section Structural Details:
#'
#' The list must be fully populated with objects that are or extend
#' \code{diffObjStyle}.  There is no explicit check that the objects in the list
#' comply with the descriptions implied by their coordinates, although the
#' default object provided by the package does comply for the most part.  One
#' check that is carried out is that any element that has a \dQuote{html}
#' value in the \code{format} dimension extends \code{diffObjStyleHtml}.
#'
#' Every cell in the list must be populated.  If there is a particular
#' combination of coordinates that does not have a corresponding defined style
#' a reasonable substitution must be provided.  For example, this package
#' only defines \dQuote{light} HTML styles, so it simply uses that style for
#' all the possible \code{brightness} values.
#'
#' While the list may only have the three dimensions described, you can add
#' values to the dimensions provided the values described above also exist.
#' For example, if you wanted to allow for styles that would render in
#' \code{grid} graphics, you could genarate a default list with \dQuote{"grid"}
#' value for the \code{format} dimension.
#'
#' @export diffObjStylePalette
#' @exportClass diffObjStylePalette
#' @examples
#' ## Create a new style based on existing style by changing
#' ## gutter symbols
#' my.style <- diffObjStyleAnsi256LightRgb()
#' my.style@text@gutter.ins <- "+"
#' my.style@text@gutter.del <- "-"
#' ## Generate the default style object palette, and replace
#' ## the ansi256 / light / rgb style with our modified one
#' defs <- diffObjStylePalette()
#' defs["ansi256", "light", "rgb"] <- list(my.style) # note `list()`
#' ## If so desired, set our new style palette as the default
#' ## one; could also pass directly as argument to `diff*` funs
#' \dontrun{
#' options(diffobj.style.defaults) <- defs
#' }

diffObjStylePalette <- setClass(
  "diffObjStylePalette",
  slots=c(data="array"),
  validity=function(object) {
    dat <- object@data
    valid.names <- names(.dfs.dims)
    if(!is.list(dat))
      return("Slot `data` must be a dimensioned list")
    if(
      !is.list(dimnames(dat)) ||
      !identical(names(dimnames(dat)), valid.names) ||
      !all(vapply(dimnames(dat), is.character, logical(1L))) ||
      anyNA(unlist(dat))
    )
      return(
        paste0(
          "`dimnames` for default styles must be a list with names `",
          paste0(deparse(valid.names), collapse=""), "` and contain only ",
          "character vectors with no NA values."
      ) )

    if(
      !all(
        vapply(
          valid.names,
          function(x) all(.dfs.dims[[x]] %in% dimnames(dat)[[x]]),
          logical(1L)
    ) ) )
      return("Style dimension names do not contain all required values")

    if(!all(vapply(dat, is, logical(1L), "diffObjStyle")))
      return("Styles may only contain objects that extend `diffObjStyle`")
    if(!all(vapply(dat["html", ,], is, logical(1L), "diffObjStyleHtml")))
      return("Styles classifed as HTML must extend `diffObjStyleHtml`")
    TRUE
  }
)
setMethod("initialize", "diffObjStylePalette",
  function(.Object, ...) {
    .dfs.arr["raw", , ] <- list(diffObjStyle())

    .dfs.arr["ansi8", , "full"] <- list(diffObjStyleAnsi8NeutralRgb())
    .dfs.arr["ansi8", , "yb"] <- list(diffObjStyleAnsi8NeutralYb())

    .dfs.arr["ansi256", "neutral", "full"] <- list(diffObjStyleAnsi8NeutralRgb())
    .dfs.arr["ansi256", "neutral", "yb"] <- list(diffObjStyleAnsi8NeutralYb())
    .dfs.arr["ansi256", "light", "full"] <- list(diffObjStyleAnsi256LightRgb())
    .dfs.arr["ansi256", "light", "yb"] <- list(diffObjStyleAnsi256LightYb())
    .dfs.arr["ansi256", "dark", "full"] <- list(diffObjStyleAnsi256DarkRgb())
    .dfs.arr["ansi256", "dark", "yb"] <- list(diffObjStyleAnsi256DarkYb())

    .dfs.arr["html", , "full"] <- list(diffObjStyleHtmlLightRgb())
    .dfs.arr["html", , "yb"] <- list(diffObjStyleHtmlLightYb())

    .Object@data <- .dfs.arr
    callNextMethod(.Object, ...)
  }
)
setReplaceMethod(
  "[", signature=c(x="diffObjStylePalette"),
  function(x, i, j, ..., value) {
    x@data[i, j, ...] <- value
    validObject(x)
    x
} )
setMethod(
  "[", signature=c(x="diffObjStylePalette"),
  function(x, i, j, ..., drop=TRUE) {
    x@data[i, j, ..., drop]
  }
)
setMethod(
  "[[", signature=c(x="diffObjStylePalette"),
  function(x, i, j, ..., exact=TRUE) {
    x@data[[i, j, ..., exact]]
  }
)
