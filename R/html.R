#' @include misc.R

NULL

#' Make Functions That Wrap Text in HTML Tags
#'
#' See examples
#'
#' @note inputs are assumed to be valid class names or CSS styles.
#'
#' @keywords export
#' @param character class the CSS class(es)
#' @param named character style inline styles, where the name is the CSS
#'   property and the value the value.
#' @return a function that accepts a character parameter.  If applied, each
#'   element in the character vector will be wrapped in the div tags
#' @examples
#' ## Assuming class 'ex1' has CSS styles defined elsewhere
#' tag_f("div", "ex1")(LETTERS[1:5])
#' ## Use convenience function, and add some inline styles
#' div_f("ex2", c(color="green", `font-family`="arial"))(LETTERS[1:5])

tag_f <- function(tag, class=character(), style=character()) {
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
        ">", x, "</", tag, ">"
      )
} }
#' @export
#' @rdname tag_f

div_f <- function(class=character(), style=character())
  tag_f("div", class, style)

#' @export
#' @rdname tag_f

span_f <- function(class=character(), style=character())
  tag_f("span", class, style)

diffobj_css <- function() {
  css <- list(
    DIV.row=c(width="100%", overflow="hidden"),
    DIV.line=c(width="50%", float="left"),
    SPAN.word.insert=c(`background-color`="#a6f3a6"),
    SPAN.word.delete=c(`background-color`="#f8cbcb"),
    DIV.text=c(overflow="hidden"),
    DIV.text.insert=c(`background-color`="#eaffea"),
    DIV.text.delete=c(`background-color`="#ffecec"),
    DIV.gutter=c(width="1em")
  )
  res <- character(length(css) + length(unlist(css)) + 2 * length(css))
  j <- 0
  for(i in names(css)) {
    res[[(j <- j + 1)]] <- i
    res[[(j <- j + 1)]] <- "{"
    res[seq_len(length(css[[i]])) + j] <-
      paste0("  ", names(css[[i]]), ": ", css[[i]], ";")
    j <- j + length(css[[i]])
    res[[(j <- j + 1)]] <- "}"
  }
  res
}
