# diffobj - Compare R Objects with a Diff
# Copyright (C) 2016  Brodie Gaslam
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# Go to <https://www.r-project.org/Licenses/GPL-3> for a copy of the license.

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
#' @param line.guide function formats guide lines (see \code{\link{guides}})
#' @param text function
#' @param text.insert function
#' @param text.delete function
#' @param text.match function
#' @param text.guide function formats guide lines (see \code{\link{guides}})
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
    gutter.context.sep="ANY", gutter.context.sep.ctd="ANY",
    gutter.pad="ANY",
    word.insert="ANY", word.delete="ANY",
    context.sep="ANY", header="ANY", meta="ANY", trim="ANY"
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
    gutter.context.sep=identity, gutter.context.sep.ctd=identity,
    word.insert=identity, word.delete=identity,
    header=identity,
    context.sep=identity,
    meta=identity,
    trim=identity
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
    gutter.fill=crayon::silver, gutter.fill.ctd=crayon::silver,
    gutter.context.sep=crayon::silver, gutter.context.sep.ctd=crayon::silver,
    header=crayon::cyan,
    meta=crayon::silver,
    line.guide=crayon::silver,
    context.sep=crayon::silver,
    trim=crayon::silver
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
    gutter.context.sep="character", gutter.context.sep.ctd="character",
    gutter.pad="character",
    context.sep="character",
    pad.col="character",
    line.break="character"
  ),
  prototype=list(
    gutter.insert=">", gutter.insert.ctd=":",
    gutter.delete="<", gutter.delete.ctd=":",
    gutter.match=" ", gutter.match.ctd=" ",
    gutter.guide="~", gutter.guide.ctd="~",
    gutter.fill="~", gutter.fill.ctd="~",
    gutter.context.sep="~", gutter.context.sep.ctd="~",
    gutter.pad=" ", context.sep="----------",
    pad.col=" ",
    line.break="\n"
  ),
  validity=function(object){
    for(i in slotNames(object)) if(!is.chr.1L(slot(object, i)))
      return(paste0("Argument `", i, "` must be character(1L) and not NA."))
    TRUE
  }
)
#' Customize Appearance of Diff
#'
#' S4 objects that expose the formatting controls for \code{Diff}
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
#'   \item \code{StyleRaw}: No styles applied
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
#' \code{crayon::red}, although you may pass any function of your liking
#' that behaves as described.
#'
#' The visual representation of the diff has many nested components.  The
#' functions you specify here will be applied starting with the innermost ones.
#' A schematic of the various component that represent an inserted line follows
#' (note \dQuote{insert} abbreviated to \dQuote{ins}, and \dQuote{gutter}
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
#' \code{word.insert}.  \code{gutter.pad} and \code{gutter.pad.txt} are used to
#' separate the gutter from the text and usually end up resolving to a space.
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
#' If you use a \code{Style} that inherits from \code{StyleHtml} the
#' diff will be wrapped in HTML tags, styled with CSS, and output to
#' a web browser by the pager.  The HTML output will be a full stand-alone
#' HTML page with references to the built-in cascading style sheet.  If the
#' pager is disabled or is not \code{\link{PagerBrowser}} then only the raw
#' HTML for the diff is output.
#'
#' Should you want to capture the HTML output for use elsewhere, you can do
#' so by using \code{as.character} on the return value of the \code{diff*}
#' methods.  If you want the raw HTML without any of the headers and
#' css in \code{<style>} tags use \code{html.ouput="diff.only"} when you
#' instantiate the \code{Style} object (see examples), or disable the
#' \code{\link{Pager}}.  Another option is \code{html.output="diff.w.style"}
#' which will add \code{<style>} tags with the CSS, but without wrapping those
#' in \code{<head>} tags. This last option results in illegal HTML with a
#' \code{<style>} block inside the \code{<body>} block, but appears to work and
#' is useful if you want to embed HTML someplace but do not have access to the
#' headers.
#'
#' For compatibility with RStudio server sessions the styles are always included
#' directly within \code{style} tags rather than in an external style sheet.
#'
#' Unlike with ANSI styles, you should not modify the styling functions in the
#' \code{@funs} slot of the \code{Style} object.  Instead, provide your own
#' styles.  See \code{diffobj_css()} for the predefined styles.  The styles are
#' structured so that they are applied to any element within a container of a
#' particular class.
#'
#' To provide your own custom CSS style sheet you may:
#' \itemize{
#'   \item specify it through the \code{css} slot of a \code{StyleHtml} object,
#'     and pass that object as the value for the \code{style} parameter for the
#'     \code{diff*} methods (see example)
#'   \item as above, but asign the object to the active \code{PaletteOfStyles}
#'   \item  set the \dQuote{diffobj.html.css} option
#' }
#'
#' If you define your own custom \code{StyleHtml} object you may want to modify
#' the slot \code{@funs@container}.  This slot contains a function that is
#' applied to the entire diff output.  For example, \code{StyleHtmlLightRgb}
#' uses \code{@funs@container <- cont_f("light", "rgb")}.  \code{cont_f} returns
#' a function that accepts a character vector as an argument and returns
#' that value wrapped in a \code{DIV} block with class
#' \dQuote{"diffobj_container light rgb"}.  This allows the CSS style sheet to
#' target the \code{Diff} elements with the correct styles.
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
#' @param na.sub what character value to substitute for NA elements; NA elements
#'   are generated when lining up side by side diffs by adding padding rows; by
#'   default the text styles replace these with a blank character string, and
#'   the HTML styles leave them as NA for the HTML formatting functions to deal
#'   with
#' @param blank sub what character value to replace blanks with; needed in
#'   particular for HTML rendering (uses \code{"&nbsp;"}) to prevent lines from
#'   collapsing
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
#'
#' ## Provide a custom style sheet; here we assume there is a style sheet at
#' ## `HOME/web/mycss.css`
#' \dontrun{
#' my.css <- file.path(path.expand("~"), "web", "mycss.css")
#' diffPrint(1:5, 2:6, style=StyleHtmlLightYb(css=my.css))
#' }
#' ## Return only the raw HTML without any of the headers
#' as.character(
#'   diffPrint(1:5, 2:6, style=StyleHtmlLightYb(html.output="diff.only"))
#' )

Style <- setClass("Style", contains="VIRTUAL",
  slots=c(
    funs="StyleFuns",
    text="StyleText",
    wrap="logical",
    pad="logical",
    finalizer="function",
    pager="Pager",
    na.sub="character",
    blank.sub="character",
    disp.width="integer"
  ),
  prototype=list(
    funs=StyleFuns(),
    text=StyleText(),
    wrap=TRUE,
    pad=TRUE,
    pager=PagerOff(),
    finalizer=function(x, y) x,
    na.sub="",
    blank.sub="",
    disp.width=0L
  ),
  validity=function(object){
    if(!is.TF(object@wrap))
      return("Slot `wrap` must be TRUE or FALSE")
    if(!is.TF(object@pad))
      return("Slot `pad` must be TRUE or FALSE")
    if(length(object@na.sub) != 1L)
      return("Slot `na.sub` must be character(1L)")
    if(length(object@blank.sub) != 1L)
      return("Slot `na.sub` must be character(1L)")
    if(!is.int.1L(object@disp.width) || object@disp.width < 0L)
      return("Slot `disp.width` must be integer(1L), positive, and not NA")
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

setClass("Raw", contains="VIRTUAL")
setClass("Ansi", contains="VIRTUAL")
setClass("Html", contains="VIRTUAL")

setClass("Rgb", contains="VIRTUAL")
setClass("Yb", contains="VIRTUAL")

#' @export StyleRaw
#' @exportClass StyleRaw
#' @rdname Style

StyleRaw <- setClass("StyleRaw", contains=c("Style", "Raw"))

#' @export StyleAnsi
#' @exportClass StyleAnsi
#' @rdname Style

StyleAnsi <- setClass(
  "StyleAnsi", contains=c("StyleRaw", "Ansi"),
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
        rgb(20, 20, 20, maxColorValue=23), bg=TRUE, grey=TRUE
      ),
      word.insert=crayon::make_style(rgb(2, 4, 2, maxColorValue=5), bg=TRUE),
      word.delete=crayon::make_style(rgb(4, 2, 2, maxColorValue=5), bg=TRUE),
      gutter.insert=crayon::make_style(rgb(0, 3, 0, maxColorValue=5)),
      gutter.insert.ctd=crayon::make_style(rgb(0, 3, 0, maxColorValue=5)),
      gutter.delete=crayon::make_style(rgb(3, 0, 0, maxColorValue=5)),
      gutter.delete.ctd=crayon::make_style(rgb(3, 0, 0, maxColorValue=5)),
      header=crayon::make_style(rgb(0, 3, 3, maxColorValue=5))
) ) )

darkGray <- crayon::make_style(rgb(13, 13, 13, maxColorValue=23), grey=TRUE)
darkGrayBg <- crayon::make_style(
  rgb(2, 2, 2, maxColorValue=23), bg=TRUE, grey=TRUE
)
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
        rgb(20, 20, 20, maxColorValue=23), bg=TRUE, grey=TRUE
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
      word.insert=crayon::make_style(rgb(0, 3, 0, maxColorValue=5), bg=TRUE),
      word.delete=crayon::make_style(rgb(3, 0, 0, maxColorValue=5), bg=TRUE),
      gutter.insert=crayon::make_style(rgb(0, 2, 0, maxColorValue=5)),
      gutter.insert.ctd=crayon::make_style(rgb(0, 2, 0, maxColorValue=5)),
      gutter.delete=crayon::make_style(rgb(2, 0, 0, maxColorValue=5)),
      gutter.delete.ctd=crayon::make_style(rgb(2, 0, 0, maxColorValue=5)),
      gutter.guide=darkGray, gutter.guide.ctd=darkGray, line.guide=darkGray,
      gutter.fill=darkGray, gutter.fill.ctd=darkGray, text.fill=darkGrayBg,
      gutter.context.sep=darkGray, gutter.context.sep.ctd=darkGray,
      context.sep=darkGray, meta=darkGray, trim=darkGray
) ) )
#' @export StyleAnsi256DarkYb
#' @exportClass StyleAnsi256DarkYb
#' @rdname Style

StyleAnsi256DarkYb <- setClass(
  "StyleAnsi256DarkYb", contains=c("StyleAnsi", "Dark", "Yb"),
  prototype=list(
    funs=StyleFunsAnsi(
      text.insert=crayon::make_style(rgb(0, 0, 2, maxColorValue=5), bg=TRUE),
      text.delete=crayon::make_style(rgb(1, 1, 0, maxColorValue=5), bg=TRUE),
      word.insert=crayon::make_style(rgb(0, 0, 5, maxColorValue=5), bg=TRUE),
      word.delete=crayon::make_style(rgb(3, 2, 0, maxColorValue=5), bg=TRUE),
      gutter.insert=crayon::make_style(rgb(0, 0, 3, maxColorValue=5)),
      gutter.insert.ctd=crayon::make_style(rgb(0, 0, 3, maxColorValue=5)),
      gutter.delete=crayon::make_style(rgb(1, 1, 0, maxColorValue=5)),
      gutter.delete.ctd=crayon::make_style(rgb(1, 1, 0, maxColorValue=5)),
      header=crayon::make_style(rgb(0, 3, 3, maxColorValue=5)),
      gutter.guide=darkGray, gutter.guide.ctd=darkGray, line.guide=darkGray,
      gutter.fill=darkGray, gutter.fill.ctd=darkGray, text.fill=darkGrayBg,
      gutter.context.sep=darkGray, gutter.context.sep.ctd=darkGray,
      context.sep=darkGray, meta=darkGray, trim=darkGray
) ) )
#' @export StyleHtml
#' @exportClass StyleHtml
#' @rdname Style

StyleHtml <- setClass(
  "StyleHtml", contains=c("Style", "Html"),
  slots=c(
    css="character", html.output="character", escape.html.entities="logical",
    js="character"
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
      line.fill=div_f("fill"),
      line=div_f("line"),
      text.insert=div_f("insert"),
      text.delete=div_f("delete"),
      text.match=div_f("match"),
      text.guide=div_f("guide"),
      text.fill=div_f("fill"),
      text=div_f("text"),
      gutter.insert=div_f("insert"),
      gutter.delete=div_f("delete"),
      gutter.match=div_f("match"),
      gutter.guide=div_f("guide"),
      gutter.fill=div_f("fill"),
      gutter=div_f("gutter"),
      context.sep=div_f("context_sep"),
      word.insert=span_f(c("word", "insert")),
      word.delete=span_f(c("word", "delete")),
      trim=span_f("trim"),
      header=div_f(c("header"))
    ),
    text=StyleText(
      gutter.insert="&gt;",
      gutter.delete="&lt;",
      gutter.match="&nbsp;",
      line.break="<br />"
    ),
    pager=PagerBrowser(),
    wrap=FALSE,
    pad=FALSE,
    escape.html.entities=TRUE,
    na.sub="&nbsp;",
    blank.sub="&nbsp;",
    disp.width=120L
  ),
  validity=function(object) {
    if(!is.chr.1L(object@css))
      return("slot `css` must be character(1L)")
    if(!is.chr.1L(object@js))
      return("slot `js` must be character(1L)")
    if(!is.chr.1L(object@html.output))
      return("slot `html.output` must be character(1L)")
    if(!is.TF(object@escape.html.entities))
      return("slot `escape.html.entities` must be TRUE or FALSE.")
    TRUE
  }
)
#' Return Location of Default HTML Support Files
#'
#' File location for default CSS and JS files.  Note that these files are read
#' and injected into the output HTML rather than referenced to simplify serving.
#'
#' @rdname webfiles
#' @export
#' @return path to the default CSS or JS file

NULL

#' @export
#' @rdname webfiles

diffobj_css <- function()
  file.path(system.file(package="diffobj"), "css", "diffobj.css")

#' @export
#' @rdname webfiles

diffobj_js <- function()
  file.path(system.file(package="diffobj"), "script", "diffobj.js")

# construct with default values specified via options; would this work with
# initialize?  Depends on whether this is run by package installation process

setMethod("initialize", "StyleHtml",
  function(
    .Object, css=getOption("diffobj.html.css"),
    js=getOption("diffobj.html.js"),
    html.output=getOption("diffobj.html.output"),
    escape.html.entities=getOption("diffobj.html.escape.html.entities"),
    ...
  ) {
    if(!is.chr.1L(css))
      stop("Argument `css` must be character(1L) and not NA")
    if(!is.chr.1L(js))
      stop("Argument `js` must be character(1L) and not NA")
    valid.html.output <- c("auto", "page", "diff.only", "diff.w.style")
    if(!string_in(html.output, valid.html.output))
      stop("Argument `html.output` must be in `", dep(valid.html.output), "`.")

    # Generate finalizer function

    .Object@finalizer <- function(txt, Diff) {
      stopifnot(is(Diff, "Diff"))
      pager <- Diff@etc@style@pager

      # Note this might conflict with threshold computations as we don't really
      # know whether we are truly going to use the pager

      use.pager <- !is(pager, "PagerOff")
      header <- footer <- NULL
      txt.flat <- paste0(txt, sep="")

      if(html.output == "auto") {
        html.output <- if(is(pager, "PagerBrowser")) "page" else "diff.only"
      }
      if(html.output %in% c("diff.w.style", "page")) {
        css.txt <- try(paste0(readLines(css), collapse="\n"))
        if(inherits(css.txt, "try-error")) stop("Cannot read css file ", css)
        css <- sprintf("<style type='text/css'>\n%s\n</style>", css.txt)
      }
      if(html.output == "diff.w.style") {
        tpl <- "%s%s"
      } else if (html.output == "page") {
        js.txt <- try(paste0(readLines(js), collapse="\n"))
        if(inherits(js.txt, "try-error")) stop("Cannot read js file ", js)
        tpl <- sprintf( "
          <!DOCTYPE html>
          <html>
            <head>
              %%s
              <script type=\"text/javascript\">
                %s
              </script>
            </head>
            <body>
            <div id='diff_content'>
            %%s
            </div>
            <div id='diff_meta' style='display: none;'>
            %s
            </div>
            </body>
            <script type=\"text/javascript\">resize();</script>
          </html>",
          make_dummy_row(Diff),
          js.txt
        )
      } else if (html.output == "diff.only") {
        css <- ""
        tpl <- "%s%s"
      } else stop("Logic Error: unexpected html.output; contact maintainer.")
      sprintf(tpl, css, txt.flat)
    }
    callNextMethod(.Object, css=css, html.output=html.output, js=js, ...)
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
#' The array/list must be fully populated with objects that are or inherit
#' \code{Style}, or are \dQuote{classRepresentation} objects (i.e. those of
#' the type returned by \code{\link{getClassDef}}) that extend \code{Style}.
#' There is no explicit check that the objects in the list comply with the
#' descriptions implied by their coordinates, although the default object
#' provided by the package does comply for the most part.  One check that is
#' carried out is that any element that has a \dQuote{html} value in the
#' \code{format} dimension extends \code{StyleHtml}.
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
#' my.pal <- PaletteOfStyles()
#' my.style <- StyleRaw()   # See `?Style` for custom styles
#' my.style@funs@word.delete <- crayon::bgBlue
#' my.pal["ansi256", "light", "rgb"] <- list(my.style) # note `list()`
#' ## Output has no format now for format/color.mode/brightness
#' ## we modified ...
#' diffPrint(
#'    1:3, 2:5, format="ansi256", color.mode="rgb", brightness="light",
#'    palette.of.styles=my.pal
#' )
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
          function(x) identical(
            .dfs.dims[[x]], head(dimnames(dat)[[x]], length(.dfs.dims[[x]]))
          ),
          logical(1L)
    ) ) )
      return("Style dimension names do not contain all required values")

    # May be either style objects or Style Class definitions

    style.def <- getClassDef("Style", package="diffobj")
    are.styles <- vapply(dat, is, logical(1L), "Style")
    are.styles.def <- logical(length(are.styles))
    are.styles.def[!are.styles] <- vapply(
      dat[!are.styles],
      function(x) is(x, "classRepresentation") && extends(x, style.def),
      logical(1L)
    )
    if(!all(are.styles | are.styles.def))
      return(
        paste0(
          "Styles may only contain objects that inherit from `Style` or class ",
          "definitions that extend `Style`"
      ) )
    if(
      !all(
        vapply(
          dat["html", ,],
          function(x)
            is(x, "classRepresentation") && extends(x, "StyleHtml") ||
            is(x, "StyleHtml"),
          logical(1L)
        )
      )
    )
      return("Styles classifed as HTML must extend `StyleHtml`")
    TRUE
  }
)
setMethod("initialize", "PaletteOfStyles",
  function(.Object, ...) {
    .dfs.arr["raw", , ] <- list(
      getClassDef("StyleRaw", package="diffobj", inherits=FALSE)
    )
    .dfs.arr["ansi8", , "rgb"] <- list(
      getClassDef("StyleAnsi8NeutralRgb", package="diffobj", inherits=FALSE)
    )
    .dfs.arr["ansi8", , "yb"] <- list(
      getClassDef("StyleAnsi8NeutralYb", package="diffobj", inherits=FALSE)
    )
    .dfs.arr["ansi256", "neutral", "rgb"] <- list(
      getClassDef("StyleAnsi8NeutralRgb", package="diffobj", inherits=FALSE)
    )
    .dfs.arr["ansi256", "neutral", "yb"] <- list(
      getClassDef("StyleAnsi8NeutralYb", package="diffobj", inherits=FALSE)
    )
    .dfs.arr["ansi256", "light", "rgb"] <- list(
      getClassDef("StyleAnsi256LightRgb", package="diffobj", inherits=FALSE)
    )
    .dfs.arr["ansi256", "light", "yb"] <- list(
      getClassDef("StyleAnsi256LightYb", package="diffobj", inherits=FALSE)
    )
    .dfs.arr["ansi256", "dark", "rgb"] <- list(
      getClassDef("StyleAnsi256DarkRgb", package="diffobj", inherits=FALSE)
    )
    .dfs.arr["ansi256", "dark", "yb"] <- list(
      getClassDef("StyleAnsi256DarkYb", package="diffobj", inherits=FALSE)
    )
    .dfs.arr["html", , "rgb"] <- list(
      getClassDef("StyleHtmlLightRgb", package="diffobj", inherits=FALSE)
    )
    .dfs.arr["html", , "yb"] <- list(
      getClassDef("StyleHtmlLightYb", package="diffobj", inherits=FALSE)
    )
    .Object@data <- .dfs.arr
    callNextMethod(.Object, ...)
  }
)
#' @rdname Extract_PaletteOfStyles

setReplaceMethod(
  "[", signature=c(x="PaletteOfStyles"),
  function(x, i, j, ..., value) {
    x@data[i, j, ...] <- value
    validObject(x)
    x
} )
#' @rdname Extract_PaletteOfStyles

setMethod(
  "[", signature=c(x="PaletteOfStyles"),
  function(x, i, j, ..., drop=FALSE) {
    x@data <- x@data[i, j, ..., drop=drop]
    x
  }
)
#' Extract/Replace a Style Class or Object from PaletteOfStyles
#'
#' @rdname Extract_PaletteOfStyles
#' @seealso \code{\link{diffPrint}} for explanations of \code{format},
#'   \code{brightness}, and \code{color.mode}
#' @param x a \code{\link{PaletteOfStyles}} object
#' @param i numeric, or character corresponding to a valid style \code{format}
#' @param j numeric, or character corresponding to a valid style
#'   \code{brightness}
#' @param ... pass a numeric or character corresponding to a valid
#'   \code{color.mode}
#' @param exact passed on to generic
#' @param drop TRUE or FALSE, whether to drop dimensions, defaults to FALSE,
#'   which is different than generic
#' @param value a \emph{list} of \code{\link{Style}} class or
#'   \code{\link{Style}} objects
#' @return a \code{\link{Style}} \code{ClassRepresentation} object or
#'    \code{\link{Style}} object for \code{[[}, and a list of the same for
#'    \code{[}

setMethod(
  "[[", signature=c(x="PaletteOfStyles"),
  function(x, i, j, ..., exact=TRUE) {
    x@data[[i, j, ..., exact=exact]]
  }
)
#' Retrieve Dimnames for PaletteOfStyles Objects
#'
#' @param x a \code{\link{PaletteOfStyles}} object
#' @return list the dimension names

setMethod("dimnames", "PaletteOfStyles", function(x) dimnames(x@data))

# Matrices used for show methods for styles

#' Unexported Matrix Used for Sample Display
#'
#' @aliases .mx2

.mx1 <- .mx2 <- matrix(1:50, ncol=2)
.mx2[c(6L, 40L)] <- 99L
.mx2 <- .mx2[-7L,]

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
#' show(StyleAnsi256LightYb())  # assumes ANSI colors supported

setMethod("show", "Style",
  function(object) {
    cat(sprintf("Object of class `%s`:\n\n", class(object)))
    d.p <- diffPrint(
      .mx1, .mx2, context=1, line.limit=7L,
      style=object, pager=PagerOff(),
      tar.banner="diffobj:::.mx1", cur.banner="diffobj:::.mx2"
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
#' @rdname show-Style-method

setMethod("show", "StyleHtml",
  function(object) {
    cat(sprintf("Class `%s` sample output:\n\n", class(object)))
    cat("[Object Renders in HTML]\n")
    invisible(NULL)
} )
#' Display a PaletteOfStyles
#'
#' @param object a \code{\link{PaletteOfStyles}} object
#' @return NULL, invisibly

setMethod("show", "PaletteOfStyles",
  function(object) {
    fmt <- dimnames(object)$format
    brt <- dimnames(object)$brightness
    clr <- dimnames(object)$color.mode

    for(f in fmt) {
      for(b in brt) {
        for(c in clr) {
          obj <- object[[f, b, c]]
          if(is(obj, "classRepresentation")) obj <- new(obj)
          txt <- capture.output(show(obj))
          cat(
            sprintf("\nformat: %s, brightness: %s, color.mode: %s\n\n", f, b, c)
          )
          cat(paste0("  ", txt), sep="\n")
} } } } )
#' Display a Summarized Version of a PaletteOfStyles
#'
#' @param object a \code{\link{PaletteOfStyles}} object
#' @param ... unused, for compatibility with generic
#' @return character representation showing classes and/or objects in
#'   PaletteOfStyles

setMethod("summary", "PaletteOfStyles",
  function(object, ...)
    apply(
      object@data,
      1:3,
      function(x)
        if(is(x[[1L]], "classRepresentation"))
          paste0("class: ", x[[1L]]@className) else
          paste0("object: ", class(x[[1L]]))
    )
)

# Helper function to render output for vignette

display_ansi_256_styles <- function() {
  styles <- lapply(
    list(
      StyleAnsi8NeutralYb(), StyleAnsi8NeutralRgb(),
      StyleAnsi256DarkYb(), StyleAnsi256DarkRgb(),
      StyleAnsi256LightYb(), StyleAnsi256LightRgb()
    ),
    function(x) capture.output(show(x))[3:9]
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
