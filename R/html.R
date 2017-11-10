# Copyright (C) 2017  Brodie Gaslam
#
# This file is part of "diffobj - Diffs for R Objects"
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# Go to <https://www.r-project.org/Licenses/GPL-2> for a copy of the license.

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
#' each element of a character vector.  \code{container_f} on the other hand
#' produces a function will collapse a character vector into length 1, and only
#' then applies the tags.  Additionally, \code{container_f} already comes with
#' the \dQuote{diffobj_container} class specified.
#'
#' @note inputs are assumed to be valid class names or CSS styles.
#'
#' @export
#' @param tag character(1L) a name of an HTML tag
#' @param class character the CSS class(es)
#' @param style named character inline styles, where the name is the CSS
#'   property and the value the value.
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
#' Count Text Characters in HTML
#'
#' Very simple implementation that will fail if there are any \dQuote{>} in the
#' HTML that are not closing tags, and assumes that HTML entities are all one
#' character wide.  Also, spaces are counted as one width each because the
#' HTML output is intended to be displayed inside \code{<PRE>} tags.
#'
#' @export
#' @param x character
#' @return integer(length(x)) with number of characters of each element
#' @examples
#' nchar_html("<a href='http:www.domain.com'>hello</a>")

nchar_html <- function(x) {
  stopifnot(is.character(x) && !anyNA(x))
  tag.less <- gsub("<[^>]*>", "", x) 
  # Thanks ridgerunner for html entity removal regex
  # http://stackoverflow.com/users/433790/ridgerunner
  # http://stackoverflow.com/a/8806462/2725969
  ent.less <-
    gsub("&(?:[a-z\\d]+|#\\d+|#x[a-f\\d]+);", "X", tag.less, perl=TRUE)
  nchar(ent.less)
}
