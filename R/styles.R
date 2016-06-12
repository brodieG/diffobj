#' @include html.R

NULL

# maybe this shouldn't be an S4 class since the function slot doesn't work
# for classed functions (e.g. the ones produced by crayon)

#' Functions Used for Styling Diff Components
#'
#' Except for \code{container} every function specified here should be
#' vectorized and apply formatting to each element in a character vectors.  The
#' functions must accept at least one argument and require no more than one
#' argument.  The text to be formatted will be passed as a character vector
#' as the first argument to each function.
#'
#' These functions are applied in post processing steps.  The \code{diff*}
#' methods do not do any of the formatting.  Instead, the formatting is done
#' only if the user requests to \code{show} the object.  Internally, \code{show}
#' first converts the object to a character vector using \code{as.character},
#' which applies every formatting function defined here except for
#' \code{container}.  Then \code{show} applies \code{container} before
#' forwarding the result to the screen or pager.
#'
#' @note the slots are set to class \dQuote{ANY} to allow classed functions
#'   such as those defined in the \code{crayon} package.  Despite this seemingly
#'   permissive slot definition, only functions are allowed in the slots by
#'   the validation functions.
#' @param container function used primarily by HTML styles to generate an
#'   outermost \code{DIV} that allows for CSS targeting of its contents
#'   (see \code{\link{cont_f}} for a function generator appropriate for use
#'   here)
#' @param line function
#' @param line.insert function
#' @param line.delete function
#' @param line.match function
#' @param line.guide function formats guide lines (see \code{\link{guideLines}})
#' @param text function
#' @param text.insert function
#' @param text.delete function
#' @param text.match function
#' @param text.guide function formats guide lines (see \code{\link{guideLines}})
#' @param gutter function
#' @param gutter.insert function
#' @param gutter.delete function
#' @param gutter.match function
#' @param gutter.guide function
#' @param gutter.pad function
#' @param header function to format each hunk header with
#' @param banner function to format entire banner
#' @param banner.insert function to format insertion banner
#' @param banner.delete function to format deletion banner
#' @param meta function format meta information lines
#' @param context.sep function to format the separator used to visually
#'   distinguish the A and B hunks in \dQuote{context} \code{mode}
#' @return a StyleFuns S4 object
#' @seealso \code{\link{Style}}
#' @rdname StyleFuns
#' @export StyleFuns
#' @exportClass StyleFuns

StyleFuns <- setClass(
  "StyleFuns",
  slots=c(
    container="ANY", row="ANY",
    line="ANY", line.insert="ANY", line.delete="ANY", line.match="ANY",
    line.guide="ANY", line.fill="ANY",
    text="ANY", text.insert="ANY", text.delete="ANY", text.match="ANY",
    text.guide="ANY", text.fill="ANY",
    banner="ANY", banner.insert="ANY", banner.delete="ANY",
    gutter="ANY",
    gutter.insert="ANY", gutter.insert.ctd="ANY",
    gutter.delete="ANY", gutter.delete.ctd="ANY",
    gutter.match="ANY", gutter.match.ctd="ANY",
    gutter.guide="ANY", gutter.guide.ctd="ANY",
    gutter.fill="ANY", gutter.fill.ctd="ANY",
    gutter.pad="ANY",
    word.insert="ANY", word.delete="ANY",
    context.sep="ANY", header="ANY", meta="ANY"
  ),
  prototype=list(
    container=identity, row=identity,
    banner=identity, banner.insert=identity, banner.delete=identity,
    line=identity, line.insert=identity, line.delete=identity,
    line.match=identity, line.guide=identity, line.fill=identity,
    text=identity, text.insert=identity, text.delete=identity,
    text.match=identity, text.guide=identity, text.fill=identity,
    gutter=identity, gutter.pad=identity,
    gutter.insert=identity, gutter.insert.ctd=identity,
    gutter.delete=identity, gutter.delete.ctd=identity,
    gutter.match=identity, gutter.match.ctd=identity,
    gutter.guide=identity, gutter.guide.ctd=identity,
    gutter.fill=identity, gutter.fill.ctd=identity,
    word.insert=identity, word.delete=identity,
    header=identity,
    context.sep=identity,
    meta=identity
  ),
  validity=function(object){
    for(i in slotNames(object)) {
      if(!is.function(slot(object, i)))
        return(paste0("Argument `", i, "` should be a function."))
      if(has_non_def_formals(tail(formals(slot(object, i)), -1L)))
        return(
          paste0(
            "Argument `", i,
            "` may not have non-default formals argument after the first."
          ) )
    }
    TRUE
  }
)
StyleFunsAnsi <- setClass(
  "StyleFunsAnsi", contains="StyleFuns",
  prototype=list(
    word.insert=crayon::green, word.delete=crayon::red,
    gutter.insert=crayon::green, gutter.insert.ctd=crayon::green,
    gutter.delete=crayon::red, gutter.delete.ctd=crayon::red,
    gutter.guide=crayon::silver, gutter.guide.ctd=crayon::silver,
    header=crayon::cyan,
    meta=crayon::silver,
    line.guide=crayon::silver,
    context.sep=crayon::silver
  )
)
#' Character Tokens Used in Diffs
#'
#' Various character tokens are used throughout diffs to provide visual cues.
#' For example, gutters will contain characters that denote deletions and
#' insertions (\code{<} and \code{>} by default).
#'
#' @param gutter.insert character(1L) text to use as visual cue to indicate
#'   whether a diff line is an insertion, defaults to \dQuote{> }
#' @param gutter.insert.ctd character(1L) if a diff line is wrapped, the
#'   visual cue shifts to this character to indicate wrapping occured
#' @param gutter.delete character(1L) see \code{gutter.insert} above
#' @param gutter.delete.ctd character(1L) see \code{gutter.insert.ctd} above
#' @param gutter.match character(1L) see \code{gutter.insert} above
#' @param gutter.match.ctd character(1L) see \code{gutter.insert.ctd} above
#' @param gutter.guide character(1L) see \code{gutter.insert} above
#' @param gutter.guide.ctd character(1L) see \code{gutter.insert.ctd} above
#' @param gutter.fill character(1L) see \code{gutter.insert} above
#' @param gutter.fill.ctd character(1L) see \code{gutter.insert.ctd} above
#' @param gutter.pad character(1L) separator between gutter characters and the
#'   rest of a line in a diff
#' @param pad.col character(1L) separator between columns in side by side mode
#' @return a StyleText S4 object
#' @seealso \code{\link{Style}}
#' @rdname StyleText
#' @export StyleText
#' @exportClass StyleText

StyleText <- setClass(
  "StyleText",
  slots=c(
    gutter.insert="character", gutter.insert.ctd="character",
    gutter.delete="character", gutter.delete.ctd="character",
    gutter.match="character", gutter.match.ctd="character",
    gutter.guide="character", gutter.guide.ctd="character",
    gutter.fill="character", gutter.fill.ctd="character",
    gutter.pad="character", context.sep="character",
    pad.col="character"
  ),
  prototype=list(
    gutter.insert=">", gutter.insert.ctd=":",
    gutter.delete="<", gutter.delete.ctd=":",
    gutter.match=" ", gutter.match.ctd=" ",
    gutter.guide="~", gutter.guide.ctd=":",
    gutter.fill=" ", gutter.fill.ctd=" ",
    gutter.pad=" ", context.sep="~~~~~",
    pad.col=" "
  ),
  validity=function(object){
    for(i in slotNames(object)) if(!is.chr.1L(slot(object, i)))
      return(paste0("Argument `", i, "` must be character(1L) and not NA."))
    TRUE
  }
)
#' Customize Appearance of Diff
#'
#' S4 objects that expose the formatting controls for \code{\link{Diff}}
#' objects.  Many predifined formats are defined as classes that extend the
#' base \code{Style} class.  You may fine tune styles by either extending
#' the pre-defined classes, or modifying an instance thereof.
#'
#' @section Pre-defined Classes:
#'
#' Pre-defined classes are used to populate the \code{\link{PaletteOfStyles}}
#' object, which in turn allows the \code{diff*} methods to pick the
#' appropriate \code{Style} for each combination of the \code{format},
#' \code{color.mode}, and \code{brightness} parameters when the \code{style}
#' parameter is set to \dQuote{auto}.  The following classes are pre-defined:
#'
#' \itemize{
#'   \item \code{Style}: No styles applied
#'   \item \code{StyleAnsi8NeutralRgb}
#'   \item \code{StyleAnsi8NeutralYb}
#'   \item \code{StyleAnsi256LightRgb}
#'   \item \code{StyleAnsi256LightYb}
#'   \item \code{StyleAnsi256DarkRgb}
#'   \item \code{StyleAnsi256DarkYb}
#'   \item \code{StyleHtmlLightRgb}
#'   \item \code{StyleHtmlLightYb}
#' }
#' Each of these classes has an associated constructor function with the
#' same name (see examples).  Objects instantiated from these classes
#' may also be used directly as the value for the \code{style} parameter to the
#' \code{diff*} methods. This will override the automatic selection process
#' that uses \code{\link{PaletteOfStyles}}.
#'
#' There are predefined classes for most combinations of
#' \code{format/color.mode/brightness}, but not all.  For example, there are
#' only \dQuote{light} \code{brightness} defined for the \dQuote{html}
#' \code{format}, and that class is re-used for all possible
#' \code{brightness} values.  \code{\link{PaletteOfStyles}} substitutes an
#' appropriate class when necessary (e.g. \code{StyleAnsi8NeutralYb} for the
#' neutral yellow-blue Ansi256 entry).
#'
#' To get a preview of what a style looks like just instantiate
#' an object; the \code{show} method will output a trivial diff to screen with
#' styles applied.  Note that for ANSI styles of the dark and light variety
#' the show method colors the terminal background and foregrounds in compatible
#' colors.  In normal usage the terminal background and foreground colors are
#' left untouched so you should not expect light styles to look good on dark
#' background and vice versa even if they render correctly when showing the
#' style object.
#'
#' @section Style Structure:
#'
#' Most of the customization is done by specifying functions that operate on
#' character vectors and return a modified character vector of the same length.
#' The intended use case is to pass \code{crayon} functions such as
#' \code{\link{crayon::red}}, although you may pass any function of your liking
#' that behaves as described.
#'
#' The visual representation of the diff has many nested components.  The
#' functions you specify here will be applied starting with the innermost ones.
#' A schematic of the various component that represent an inserted line follows
#' (note dQuote{insert} abbreviated to \dQuote{ins}, and \dQuote{gutter}
#' abbreviated to \dQuote{gtr}):
#' \preformatted{+- line ---------------------------------------------------+
#' |+- line.ins ---------------------------------------------+|
#' ||+- gtr ------------------------++- text ---------------+||
#' |||+- gtr.ins ---++- gtr.pad ---+||+- text.ins ---------+|||
#' ||||             ||             ||||      +- word.ins -+||||
#' |||| gtr.ins.txt || gtr.pad.txt |||| DIFF | TEXT HERE  |||||
#' ||||             ||             ||||      +------------+||||
#' |||+-------------++-------------+||+--------------------+|||
#' ||+------------------------------++----------------------+||
#' |+--------------------------------------------------------+|
#' +----------------------------------------------------------+
#' }
#' A similar model applies to deleted and matching lines.  The boxes represent
#' functions.  \code{gutter.insert.txt} represents the text to use in the gutter
#' and is not a function. \code{DIFF TEXT HERE} is text from the objects being
#' diffed, with the portion that has different words inside the
#' \code{word.insert} and is obviously not a function either.
#' \code{gutter.pad} and \code{gutter.pad.txt} are used to separate the gutter
#' from the text and usually end up resolving to a space.
#'
#' Most of the functions defined here default to \code{\link{identity}}, but
#' you are given the flexibility to fully format the diff.  See
#' \code{\link{StyleFuns}} and \code{\link{StyleText}} for a full listing of
#' the adjustable elements.
#'
#' In side-by-side mode there are two \dQuote{lines} per screen line, each with
#' the structure described here.
#'
#' The structure described here may change in the future.
#'
#' @section HTML Styles:
#'
#' Styling functions can just as easily wrap \code{Diff} components in HTML
#' tags as anything else; however, if you wish to apply your own custom styles
#' we recommend that you do so via CSS styles as opposed to by modifying the
#' default styling functions defined for the HTML styles.  For the most part
#' these functions apply structural HTML tags that follow the outline descibed
#' in the previous section.  The styling is then done via style sheets.
#'
#' See \code{file.path(system.file(package="diffobj"), "css", "diffobj.css")}
#' for the predefined styles.  The styles are structured so that they are
#' applied to any element within a container of a particular class.  The
#' predefined HTML styles use the function returned \code{\link{cont_f}} as the
#' value for slot \code{@funs@container}.  For example, \code{StyleHtmlLightRgb}
#' uses \code{@funs@container <- cont_f("light", "rgb")}.  This wraps the entire
#' diff in a \code{DIV} block with class \dQuote{"diffobj_container light rgb"}
#' which then allows the CSS style sheet to target the \code{Diff} elements.
#'
#' @rdname Style
#' @export Style
#' @exportClass Style
#' @param funs a \code{\link{StyleFuns}} object that contains all the functions
#'   represented above
#' @param text a \code{\link{StyleText}} object that contains the non-content
#'   text used by the diff (e.g. \code{gutter.insert.txt})
#' @param wrap TRUE or FALSE, whether the text should be hard wrapped to fit in
#'   the console
#' @param pad TRUE or FALSE, whether text should be right padded
#' @param pager what type of \code{\link{Pager}} to use
#' @param finalizer function that accepts at least two parameters and requires
#'   no more than two parameters, will receive as the first parameter the
#'   full text of the diff as a character vector, and the active
#'   \code{\link{Pager}} as the second argument.  This allows final
#'   modifications to the character output so that it is displayed correctly
#'   by the pager.  For example, \code{StyleHtml} objects use it to generate
#'   HTML headers if the \code{Diff} is destined to be displayed in a browser.
#'   The \code{Pager} object is passed along to provide information about the
#'   paging device to the function.
#' @return Style S4 object
#' @examples
#' ## Create a new style based on existing style by changing
#' ## gutter symbols and guide color; see `?StyleFuns` and
#' ## `?StyleText` for a full list of adjustable elements
#' my.style <- StyleAnsi8NeutralYb()
#' my.style   ## `show` method gives you a preview of the style
#' my.style@text@gutter.insert <- "+++"
#' my.style@text@gutter.delete <- "---"
#' my.style@funs@text.guide <- crayon::green
#' my.style   ## Notice gutters and guide color

Style <- setClass(
  "Style",
  slots=c(
    funs="StyleFuns",
    text="StyleText",
    wrap="logical",
    pad="logical",
    finalizer="function",
    pager="Pager"
  ),
  prototype=list(
    funs=StyleFuns(),
    text=StyleText(),
    wrap=TRUE,
    pad=TRUE,
    pager=PagerOff(),
    finalizer=function(x, y) x
  ),
  validity=function(object){
    if(!is.TF(object@wrap))
      return("Slot `wrap` must be TRUE or FALSE")
    if(!is.TF(object@pad))
      return("Slot `pad` must be TRUE or FALSE")
    fin.args <- formals(object@finalizer)
    if(length(fin.args) < 2L)
      return(
        "Slot `finalizer` must be a function with at least two parameters."
      )
    if(length(fin.args) > 2L && has_non_def_formals(tail(fin.args, -2L)))
      return(
        paste0(
          "Slot `finalizer` must be a function with no non-default parameters ",
          "other than the first two."
    ) )
  }
)
setClass("Light", contains="VIRTUAL")
setClass("Dark", contains="VIRTUAL")
setClass("Neutral", contains="VIRTUAL")

setClass("Ansi", contains="VIRTUAL")
setClass("Html", contains="VIRTUAL")

setClass("Rgb", contains="VIRTUAL")
setClass("Yb", contains="VIRTUAL")

#' @export StyleAnsi
#' @exportClass StyleAnsi
#' @rdname Style

StyleAnsi <- setClass(
  "StyleAnsi", contains=c("Style", "Ansi"),
  prototype=list(funs=StyleFunsAnsi()),
)
setMethod(
  "initialize", "StyleAnsi",
  function(.Object, ...) {
    .Object@pager <- if(pager_is_less())
      PagerSystemLess() else PagerSystem()
    callNextMethod(.Object, ...)
})
#' @export StyleAnsi8NeutralRgb
#' @exportClass StyleAnsi8NeutralRgb
#' @rdname Style

StyleAnsi8NeutralRgb <- setClass(
  "StyleAnsi8NeutralRgb", contains=c("StyleAnsi", "Neutral", "Rgb")
)
#' @export StyleAnsi8NeutralYb
#' @exportClass StyleAnsi8NeutralYb
#' @rdname Style

StyleAnsi8NeutralYb <- setClass(
  "StyleAnsi8NeutralYb", contains=c("StyleAnsi", "Neutral", "Yb"),
  prototype=list(
    funs=StyleFunsAnsi(
      word.insert=crayon::blue, word.delete=crayon::yellow,
      gutter.insert=crayon::blue,
      gutter.insert.ctd=crayon::blue,
      gutter.delete=crayon::yellow,
      gutter.delete.ctd=crayon::yellow
  ) )
)
#' @export StyleAnsi256LightRgb
#' @exportClass StyleAnsi256LightRgb
#' @rdname Style

StyleAnsi256LightRgb <- setClass(
  "StyleAnsi256LightRgb", contains=c("StyleAnsi", "Light", "Rgb"),
  prototype=list(
    funs=StyleFunsAnsi(
      text.insert=crayon::make_style(rgb(4, 5, 4, maxColorValue=5), bg=TRUE),
      text.delete=crayon::make_style(rgb(5, 4, 4, maxColorValue=5), bg=TRUE),
      text.fill=crayon::make_style(
        rgb(22, 22, 22, maxColorValue=23), bg=TRUE, grey=TRUE
      ),
      word.insert=crayon::make_style(rgb(2, 4, 2, maxColorValue=5), bg=TRUE),
      word.delete=crayon::make_style(rgb(4, 2, 2, maxColorValue=5), bg=TRUE),
      gutter.insert=crayon::make_style(rgb(0, 3, 0, maxColorValue=5)),
      gutter.insert.ctd=crayon::make_style(rgb(0, 3, 0, maxColorValue=5)),
      gutter.delete=crayon::make_style(rgb(3, 0, 0, maxColorValue=5)),
      gutter.delete.ctd=crayon::make_style(rgb(3, 0, 0, maxColorValue=5)),
      header=crayon::make_style(rgb(0, 3, 3, maxColorValue=5))
) ) )
#' @export StyleAnsi256LightYb
#' @exportClass StyleAnsi256LightYb
#' @rdname Style

StyleAnsi256LightYb <- setClass(
  "StyleAnsi256LightYb", contains=c("StyleAnsi", "Light", "Yb"),
  prototype=list(
    funs=StyleFunsAnsi(
      text.insert=crayon::make_style(rgb(3, 3, 5, maxColorValue=5), bg=TRUE),
      text.delete=crayon::make_style(rgb(4, 4, 2, maxColorValue=5), bg=TRUE),
      text.fill=crayon::make_style(
        rgb(22, 22, 22, maxColorValue=23), bg=TRUE, grey=TRUE
      ),
      word.insert=crayon::make_style(rgb(2, 2, 4, maxColorValue=5), bg=TRUE),
      word.delete=crayon::make_style(rgb(3, 3, 1, maxColorValue=5), bg=TRUE),
      gutter.insert=crayon::make_style(rgb(0, 0, 3, maxColorValue=5)),
      gutter.insert.ctd=crayon::make_style(rgb(0, 0, 3, maxColorValue=5)),
      gutter.delete=crayon::make_style(rgb(2, 1, 0, maxColorValue=5)),
      gutter.delete.ctd=crayon::make_style(rgb(2, 1, 0, maxColorValue=5)),
      header=crayon::make_style(rgb(0, 3, 3, maxColorValue=5))
) ) )
#' @export StyleAnsi256DarkRgb
#' @exportClass StyleAnsi256DarkRgb
#' @rdname Style

StyleAnsi256DarkRgb <- setClass(
  "StyleAnsi256DarkRgb", contains=c("StyleAnsi", "Dark", "Rgb"),
  prototype=list(
    funs=StyleFunsAnsi(
      text.insert=crayon::make_style(rgb(0, 1, 0, maxColorValue=5), bg=TRUE),
      text.delete=crayon::make_style(rgb(1, 0, 0, maxColorValue=5), bg=TRUE),
      text.fill=crayon::make_style(
        rgb(2, 2, 2, maxColorValue=23), bg=TRUE, grey=TRUE
      ),
      word.insert=crayon::make_style(rgb(0, 3, 0, maxColorValue=5), bg=TRUE),
      word.delete=crayon::make_style(rgb(3, 0, 0, maxColorValue=5), bg=TRUE),
      gutter.insert=crayon::make_style(rgb(0, 2, 0, maxColorValue=5)),
      gutter.insert.ctd=crayon::make_style(rgb(0, 2, 0, maxColorValue=5)),
      gutter.delete=crayon::make_style(rgb(2, 0, 0, maxColorValue=5)),
      gutter.delete.ctd=crayon::make_style(rgb(2, 0, 0, maxColorValue=5))
) ) )
#' @export StyleAnsi256DarkYb
#' @exportClass StyleAnsi256DarkYb
#' @rdname Style

StyleAnsi256DarkYb <- setClass(
  "StyleAnsi256DarkYb", contains=c("StyleAnsi", "Dark", "Yb"),
  prototype=list(
    funs=StyleFunsAnsi(
      text.insert=crayon::make_style(rgb(0, 0, 1, maxColorValue=5), bg=TRUE),
      text.delete=crayon::make_style(rgb(1, 1, 0, maxColorValue=5), bg=TRUE),
      text.fill=crayon::make_style(
        rgb(2, 2, 2, maxColorValue=23), bg=TRUE, grey=TRUE
      ),
      word.insert=crayon::make_style(rgb(0, 0, 4, maxColorValue=5), bg=TRUE),
      word.delete=crayon::make_style(rgb(3, 2, 0, maxColorValue=5), bg=TRUE),
      gutter.insert=crayon::make_style(rgb(0, 0, 3, maxColorValue=5)),
      gutter.insert.ctd=crayon::make_style(rgb(0, 0, 3, maxColorValue=5)),
      gutter.delete=crayon::make_style(rgb(1, 1, 0, maxColorValue=5)),
      gutter.delete.ctd=crayon::make_style(rgb(1, 1, 0, maxColorValue=5)),
      header=crayon::make_style(rgb(0, 3, 3, maxColorValue=5))
) ) )
#' @export StyleHtml
#' @exportClass StyleHtml
#' @rdname Style

StyleHtml <- setClass(
  "StyleHtml", contains=c("Style", "Html"),
  slots=c(
    css="character", css.mode="character", escape.html.entities="logical"
  ),
  prototype=list(
    funs=StyleFuns(
      container=cont_f(),
      row=div_f("row"),
      banner.insert=div_f("insert"),
      banner.delete=div_f("delete"),
      banner=div_f("line banner"),
      line.insert=div_f("insert"),
      line.delete=div_f("delete"),
      line.match=div_f("match"),
      line.guide=div_f("guide"),
      line=div_f("line"),
      text.insert=div_f("insert"),
      text.delete=div_f("delete"),
      text.match=div_f("match"),
      text.guide=div_f("guide"),
      text=div_f("text"),
      gutter.insert=div_f("insert"),
      gutter.delete=div_f("delete"),
      gutter.match=div_f("match"),
      gutter.guide=div_f("guide"),
      gutter=div_f("gutter"),
      word.insert=span_f(c("word", "insert")),
      word.delete=span_f(c("word", "delete")),
      header=div_f(c("header"))
    ),
    text=StyleText(
      gutter.insert="&gt;",
      gutter.delete="&lt;",
      gutter.match="&nbsp;"
    ),
    pager=PagerBrowser(),
    wrap=FALSE,
    pad=FALSE,
    escape.html.entities=TRUE
  ),
  validity=function(object) {
    if(!is.chr.1L(object@css))
      return("slot `css` must be character(1L)")
    if(!is.chr.1L(object@css.mode))
      return("slot `css.mode` must be \"internal\" or \"external\"")
    if(!is.TF(object@escape.html.entities))
      return("slot `escape.html.entities` must be TRUE or FALSE.")
    TRUE
  }
)
# construct with default values specified via options; would this work with
# initialize?  Depends on whether this is run by package installation process

setMethod("initialize", "StyleHtml",
  function(
    .Object, css=getOption("diffobj.html.css"),
    css.mode=getOption("diffobj.html.css.mode"),
    escape.html.entities=getOption("diffobj.html.escape.html.entities"),
    ...
  ) {
    if(!is.chr.1L(css))
      stop("Argument `css` must be character(1L) and not NA")
    valid.css.modes <- c("auto", "internal", "external")
    if(!string_in(css.mode, valid.css.modes))
      stop("Argument `css.mode` must be in `", dep(valid.css.modes), "`.")

    # Generate finalizer function

    .Object@finalizer <- function(txt, pager) {
      stopifnot(is(pager, "Pager"))

      use.pager <- !is(pager, "PagerOff")
      header <- footer <- NULL
      txt.flat <- paste0(txt, sep="")

      css.mode <- if(css.mode == "auto" && is(pager, "PagerBrowser"))
        "external" else "internal"

      css <- if(css.mode == "internal") {
        css.txt <- try(paste0(readLines(css), collapse=""))
        if(inherits(css.txt, "try-error"))
        stop("Cannot read css file ", css)
        sprintf("<style type='text/css'>%s</style>", css.txt)
      } else if (use.pager) {
        sprintf("<link rel='stylesheet' type='text/css' href='%s'>", css)
      } else ""
      template <- if(use.pager) {
        "<!DOCTYPE html><html><head>%s</head><body>%s</body><html>"
      } else "%s%s"
      sprintf(template, css, txt.flat)
    }
    callNextMethod(.Object, css=css, css.mode=css.mode, ...)
} )
#' @export StyleHtmlLightRgb
#' @exportClass StyleHtmlLightRgb
#' @rdname Style

StyleHtmlLightRgb <- setClass(
  "StyleHtmlLightRgb", contains=c("StyleHtml", "Light", "Rgb")
)
setMethod("initialize", "StyleHtmlLightRgb",
  function(.Object, ...) {
    .Object@funs@container <- cont_f(c("light", "rgb"))
    callNextMethod(.Object, ...)
  }
)
#' @export StyleHtmlLightYb
#' @exportClass StyleHtmlLightYb
#' @rdname Style

StyleHtmlLightYb <- setClass(
  "StyleHtmlLightYb", contains=c("StyleHtml", "Light", "Yb"),
)
setMethod("initialize", "StyleHtmlLightYb",
  function(.Object, ...) {
    .Object@funs@container <- cont_f(c("light", "yb"))
    callNextMethod(.Object, ...)
  }
)
# Helper structure for constructing our defaults array

.dfs.dims <- list(
  format=c("raw", "ansi8", "ansi256", "html"),
  brightness=c("neutral", "light", "dark"),
  color.mode=c("rgb", "yb")  # add b/w?
)
.dfs.dims.sizes <- vapply(.dfs.dims, length, integer(1L))
.dfs.arr <- array(
  vector("list", prod(.dfs.dims.sizes)), dim=.dfs.dims.sizes, dimnames=.dfs.dims
)

#' Class for Tracking Default Styles by Style Type
#'
#' Provides a mechanism for specifying a style based on the style properties
#' along dimensions of format, brightness, and color.  This allows a user to
#' request a style that meets a certain description (e.g. a \dQuote{light}
#' scheme in \dQuote{ansi256} format), without having to provide a specific
#' \code{\link{Style}} object.
#'
#' @section Dimensions:
#'
#' There are three general orthogonal dimensions of styles that can be used when
#' rendering diffs: the type of format, the \dQuote{brightness} of the output,
#' and whether the colors used are distinguishable if you assume reds and greens
#' are not distinguishable.  Defaults for the intersections each of these
#' dimensions are encoded as a three dimensional list.  This list is just an
#' atomic vector of type \dQuote{list} with a length 3 \code{dim} attribute.
#'
#' The array/list dimensions are:
#' \itemize{
#'   \item format: the format type, typically \dQuote{raw}, \dQuote{ansi8},
#'     \dQuote{ansi256}, or \dQuote{html}
#'   \item brightness: whether the colors are bright or not, which allows user to
#'     chose a scheme that is compatible with their console, typically:
#'     \dQuote{light}, \dQuote{dark}, \dQuote{normal}
#'   \item color.mode: \dQuote{rgb} for full color or \dQuote{yb} for
#'     dichromats (yb stands for Yellow Blue).
#' }
#' @section Methods:
#'
#' The following methods are implemented:
#' \itemize{
#'   \item \code{[}, \code{[<-}, \code{[[}
#'   \item show
#'   \item summary
#'   \item dimnames
#' }
#' @section Structural Details:
#'
#' The array/list is stored in the \code{data} slot of
#' \code{PaletteOfStyles} objects.  Subsetting methods are provided so you
#' may operate directly on the S4 object as you would on a regular array.
#'
#' The array/list must be fully populated with objects that are or extend
#' \code{Style}.  There is no explicit check that the objects in the list
#' comply with the descriptions implied by their coordinates, although the
#' default object provided by the package does comply for the most part.  One
#' check that is carried out is that any element that has a \dQuote{html}
#' value in the \code{format} dimension extends \code{StyleHtml}.  The example
#' below purposefully subverts this for illustrative purposes.
#'
#' Every cell in the list must be populated.  If there is a particular
#' combination of coordinates that does not have a corresponding defined style
#' a reasonable substitution must be provided.  For example, this package
#' only defines \dQuote{light} HTML styles, so it simply uses that style for
#' all the possible \code{brightness} values.
#'
#' While the list may only have the three dimensions described, you can add
#' values to the dimensions provided the values described above are the first
#' ones in each of their corresponding dimensions.  For example, if you wanted
#' to allow for styles that would render in \code{grid} graphics, you could
#' genarate a default list with \dQuote{"grid"} value appended to the values of
#' the \code{format} dimension.
#'
#' @export PaletteOfStyles
#' @exportClass PaletteOfStyles
#' @examples
#' ## Look at all "ansi256" styles (assumes compatible terminal)
#' PaletteOfStyles()["ansi256",,]
#' ## Generate the default style object palette, and replace
#' ## the ansi256 / light / rgb style with our modified one
#' ## which for illustrative purposes is the raw style
#' defs <- PaletteOfStyles()
#' my.style <- Style()   # See `?Style` for custom styles
#' defs["ansi256", "light", "rgb"] <- list(my.style) # note `list()`
#' ## Output has no format now for format/color.mode/brightness
#' ## we modified ...
#' diffPrint(1:3, 2:5, format="ansi256", color.mode="rgb", brightness="light")
#' ## If so desired, set our new style palette as the default
#' ## one; could also pass directly as argument to `diff*` funs
#' \dontrun{
#' options(diffobj.palette=defs)
#' }

PaletteOfStyles <- setClass(
  "PaletteOfStyles",
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
          function(x) !identical(
            .dfs.dims[[x]], head(dimnames(dat)[[x]], length(.dfs.dims[[x]]))
          ),
          logical(1L)
    ) ) )
      return("Style dimension names do not contain all required values")

    if(!all(vapply(dat, is, logical(1L), "Style")))
      return("Styles may only contain objects that extend `Style`")
    if(!all(vapply(dat["html", ,], is, logical(1L), "StyleHtml")))
      return("Styles classifed as HTML must extend `StyleHtml`")
    TRUE
  }
)
setMethod("initialize", "PaletteOfStyles",
  function(.Object, ...) {
    .dfs.arr["raw", , ] <- list(Style())

    .dfs.arr["ansi8", , "rgb"] <- list(StyleAnsi8NeutralRgb())
    .dfs.arr["ansi8", , "yb"] <- list(StyleAnsi8NeutralYb())

    .dfs.arr["ansi256", "neutral", "rgb"] <- list(StyleAnsi8NeutralRgb())
    .dfs.arr["ansi256", "neutral", "yb"] <- list(StyleAnsi8NeutralYb())
    .dfs.arr["ansi256", "light", "rgb"] <- list(StyleAnsi256LightRgb())
    .dfs.arr["ansi256", "light", "yb"] <- list(StyleAnsi256LightYb())
    .dfs.arr["ansi256", "dark", "rgb"] <- list(StyleAnsi256DarkRgb())
    .dfs.arr["ansi256", "dark", "yb"] <- list(StyleAnsi256DarkYb())

    .dfs.arr["html", , "rgb"] <- list(StyleHtmlLightRgb())
    .dfs.arr["html", , "yb"] <- list(StyleHtmlLightYb())

    .Object@data <- .dfs.arr
    callNextMethod(.Object, ...)
  }
)
setReplaceMethod(
  "[", signature=c(x="PaletteOfStyles"),
  function(x, i, j, ..., value) {
    x@data[i, j, ...] <- value
    validObject(x)
    x
} )
setMethod(
  "[", signature=c(x="PaletteOfStyles"),
  function(x, i, j, ..., drop=FALSE) {
    x@data <- x@data[i, j, ..., drop=drop]
    x
  }
)
setMethod(
  "[[", signature=c(x="PaletteOfStyles"),
  function(x, i, j, ..., exact=TRUE) {
    x@data[[i, j, ..., exact=exact]]
  }
)
setMethod("dimnames", "PaletteOfStyles", function(x) dimnames(x@data))

#' Show Method for Style Objects
#'
#' Display a small sample diff with the Style object styles applied.  For
#' ANSI light and dark styles, will also temporarily set the background and
#' foreground colors to ensure they are compatible with the style, even though
#' this is not done in normal output (i.e. if you intend on using a
#' \dQuote{light} style, you should set your terminal background color to be
#' light or expect sub-optimal rendering).
#'
#' @param object a \code{Style} S4 object
#' @return NULL, invisibly
#' @examples
#' StyleAnsi256LightYb()  # assumes ANSI colors supported

setMethod("show", "Style",
  function(object) {
    cat(sprintf("Object of class `%s`:\n\n", class(object)))
    mx1 <- mx2 <- matrix(1:50, ncol=2)
    mx2[c(6, 40)] <- 99L
    d.p <- diffPrint(
      mx1, mx2, context=1, line.limit=6, style=object, pager=PagerOff()
    )
    d.txt <- capture.output(show(d.p))
    if(is(object, "Ansi")) {
      old.crayon.opt <-
        options(crayon.enabled=TRUE)
      on.exit(options(old.crayon.opt), add=TRUE)
      pad.width <- max(crayon_nchar(d.txt))
      d.txt <- rpad(d.txt, width=pad.width)
      bgWhite <- crayon::make_style(rgb(1, 1, 1), bg=TRUE)
      white <- crayon::make_style(rgb(1, 1, 1))
      if(is(object, "Light")) {
        d.txt <- bgWhite(crayon::black(d.txt))
      } else if (is(object, "Dark")) {
        d.txt <- crayon::bgBlack(white(d.txt))
      }
      if(is(object, "Light") || is(object, "Dark")) {
        d.txt <- c(
          d.txt, "",
          strwrap(
            paste0(
              "Default bg and fg colors forced to appropriate colors for ",
              "scheme; this does not happen in actual use."
            ),
            width=pad.width + 20L
    ) ) } }
    cat(d.txt, sep="\n")
    invisible(NULL)
} )
setMethod("show", "StyleHtml",
  function(object) {
    cat(sprintf("Class `%s` sample output:\n\n", class(object)))
    cat("[Object Renders in HTML]\n")
    invisible(NULL)
} )
setMethod("show", "PaletteOfStyles",
  function(object) {
    fmt <- dimnames(object)$format
    brt <- dimnames(object)$brightness
    clr <- dimnames(object)$color.mode

    for(f in fmt) {
      for(b in brt) {
        for(c in clr) {
          txt <- capture.output(show(object[[f, b, c]]))
          cat(
            sprintf("\nformat: %s, brightness: %s, color.mode: %s\n\n", f, b, c)
          )
          cat(paste0("  ", txt), sep="\n")
} } } } )
setMethod("summary", "PaletteOfStyles",
  function(object, ...) apply(object@data, 1:3, function(x) class(x[[1L]]))
)

# Helper function to render output for vignette

display_ansi_256_styles <- function() {
  styles <- lapply(
    list(
      StyleAnsi8NeutralYb(), StyleAnsi8NeutralRgb(),
      StyleAnsi256DarkYb(), StyleAnsi256DarkRgb(),
      StyleAnsi256LightYb(), StyleAnsi256LightRgb()
    ),
    function(x) capture.output(show(x))[3:8]
  )
  names <- c("Neutral", "Dark", "Light")
  cat("\n")
  lapply(
    1:3,
    function(x) {
      cat(paste0(" ", names[x]), "\n\n")
      cat(paste("   ", styles[[x * 2 - 1]], " ", styles[[x * 2]]), sep="\n")
      cat("\n")
    }
  )
  invisible(NULL)
}
