#' @include misc.R

NULL

#' Make Functions That Wrap Text in HTML Tags
#'
#' Helper functions to generate functions to use as slots for the
#' \code{StyleHtml@funs} classes.  These are functions that return
#' \emph{functions}.
#'
#' \code{tag_f} and related functions (\code{div_f}, \code{span_f}) produce
#' functions that are  vectorized and will apply opening and closing tags to
#' each element of a character vector. \code{container_f} on the other hand
#' produces a function will collapse a character vector into length 1, and only
#' then applies the tags.  Additionally, \code{container_f} already comes with
#' the \dQuote{diffobj_container} class specified.
#'
#' @note inputs are assumed to be valid class names or CSS styles.
#'
#' @keywords export
#' @param character class the CSS class(es)
#' @param named character style inline styles, where the name is the CSS
#'   property and the value the value.
#' @param blank.val character(1L) what value to substitute for empty character
#'   elements; defaults to \code{"&nbsp;"} so that empty HTML elements do not
#'   collapse, though note \code{span_f} uses \code{""}.
#' @param na.val character(1L) what value to substitute for NA character
#'   elements; defaults to \code{""}.  NA elements typically arise when
#'   aligning diff components in side by side mode by adding empty rows.
#' @return a function that accepts a character parameter.  If applied, each
#'   element in the character vector will be wrapped in the div tags
#' @aliases div_f, span_f, cont_f
#' @examples
#' ## Assuming class 'ex1' has CSS styles defined elsewhere
#' tag_f("div", "ex1")(LETTERS[1:5])
#' ## Use convenience function, and add some inline styles
#' div_f("ex2", c(color="green", `font-family`="arial"))(LETTERS[1:5])
#' ## Notice how this is a div with pre-specifed class,
#' ## and only one div is created around the entire data
#' cont_f()(LETTERS[1:5])

tag_f <- function(
  tag, class=character(), style=character(), blank.val="&nbsp;", na.val=""
) {
  stopifnot(is.chr.1L(tag), is.character(class), is.character(style))
  function(x) {
    if(!is.character(x)) stop("Argument `x` must be character.")
    if(!length(x)) character(0L) else
      paste0(
        "<", tag,
        if(length(class)) paste0(" class='", paste0(class, collapse=" "), "'"),
        if(length(style))
          paste0(
            " style='",
            paste(names(style), style, sep=": ", collapse="; "), ";'"
          ),
        ">",
        ifelse(is.na(x),na.val, ifelse(!nchar(x), blank.val, x)),
        "</", tag, ">"
      )
} }
#' @export
#' @rdname tag_f

div_f <- function(
  class=character(), style=character(), blank.val="&nbsp;", na.val=""
)
  tag_f("div", class=class, style=style, blank.val=blank.val, na.val=na.val)

#' @export
#' @rdname tag_f

span_f <- function(
  class=character(), style=character(), blank.val="&nbsp;", na.val=""
)
  tag_f("span", class=class, style=style, blank.val=blank.val, na.val=na.val)

#' @export
#' @rdname tag_f

cont_f <- function(class=character()) {
  stopifnot(is.character(class))
  function(x) {
    if(!is.character(x)) stop("Argument `x` must be character.")
    sprintf(
      "<div class='diffobj_container%s'><pre>%s</pre></div>",
      if(length(class)) paste0(" ", class, collapse="") else "",
      paste0(x, collapse="")
    )
} }
