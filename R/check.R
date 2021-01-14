# Copyright (C) 2021 Brodie Gaslam
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

is.less_flags <-
  function(x) is.chr.1L(x) && isTRUE(grepl("^[[:alpha:]]*$", x))

# for checking the limits, if successful returns an integer(2L) vector,
# otherwise a character vector to sprintf as an error

#' @include pager.R

check_limit <- function(limit) {
  if(
    !is.numeric(limit) || any(is.na(limit)) ||
    !length(limit) %in% 1:2 ||
    !all(is.finite(limit)) ||
    any(round(limit) != limit) ||
    (length(limit) == 2L && diff(limit) > 0)
  ) {
    return(
      paste0(
        "Argument `%s` must be an integer vector of length 1 or 2 ",
        "and if length 2, with the first value larger than or equal to ",
        "the second%s"
  ) ) }
  limit <- as.integer(limit)
  if(length(limit) == 1L) limit <- rep(limit, 2L)
  limit
}
# requires a value to be a scalar character and match one of the provided
# options

string_in <- function(x, valid.x) is.chr.1L(x) && x %in% valid.x

# Simple validation functions

is.int.1L <- function(x)
  is.numeric(x) && length(x) == 1L && !is.na(x) && all(x == round(x)) &&
  is.finite(x)

is.int.2L <- function(x)
  is.numeric(x) && length(x) == 2L && !anyNA(x) && all(x == round(x)) &&
  all(is.finite(x))

is.TF <- function(x) isTRUE(x) || identical(x, FALSE)

is.chr.1L <- function(x) is.character(x) && length(x) == 1L && !is.na(x)

is.valid.palette.param <- function(x, param, palette) {
  stopifnot(is(palette, "PaletteOfStyles"))
  stopifnot(isTRUE(param %in% c("brightness", "color.mode")))
  valid.formats <- dimnames(palette@data)$format
  valid.params <- dimnames(palette@data)[[param]]

  if(!is.character(x) || anyNA(x))
    paste0("Argument `", param, "` must be character and not contain NAs")
  else if(!all(x %in% valid.params))
    paste0(
      "Argument `", param, "` may only contain values in `", dep(valid.params),
      "`"
    )
  else if(
    (length(x) > 1L && is.null(names(x))) ||
    !all(names(x) %in% c("", valid.formats))
  )
    paste0(
      "Argument `", param, "` must have names if it has length > 1, and those ",
      "names must include at least an empty name `\"\"` as well as names only ",
      "from `", dep(valid.formats), "`."
    )
  else if ((!is.null(names(x)) && !"" %in% names(x)))
    paste0(
      "Argument `", param, "` must include at least one empty name `\"\"` if ",
      "it has names."
    )
  else TRUE
}
is.one.arg.fun <- function(x) {
  if(!is.function(x)) {
    "is not a function"
  } else if(length(formals(x)) < 1L) {
    "does not have at least one arguments"
  } else if("..." %in% names(formals(x))[1]) {
    "cannot have `...` as the first argument"
  } else {
    nm.forms <- vapply(formals(x), is.name, logical(1L))
    forms.chr <- character(length(nm.forms))
    forms.chr[nm.forms] <- as.character(formals(x)[nm.forms])
    forms.names <- names(formals(x))
    if(any(tail(!nzchar(forms.chr) & nm.forms & forms.names != "...", -1L)))
      "cannot have any non-optional arguments other than first one" else TRUE
  }
}
is.valid.guide.fun <- is.two.arg.fun <- function(x) {
  if(!is.function(x)) {
    "is not a function"
  } else if(length(formals(x)) < 2L) {
    "does not have at least two arguments"
  } else if("..." %in% names(formals(x))[1:2]) {
    "cannot have `...` as one of the first two arguments"
  } else {
    nm.forms <- vapply(formals(x), is.name, logical(1L))
    forms.chr <- character(length(nm.forms))
    forms.chr[nm.forms] <- as.character(formals(x)[nm.forms])
    if(any(tail(!nzchar(forms.chr) & nm.forms, -2L)))
      "cannot have any non-optional arguments other than first two" else TRUE
} }
is.valid.width <- function(x)
  if(!is.int.1L(x) || (x != 0L && (x < 10L || x > 10000))) {
    "must be integer(1L) and 0, or between 10 and 10000"
  } else TRUE

is.one.file.name <- function(x) {
  if(!is.chr.1L(x)) {
    "must be character(1L) and not NA"
  } else if(!file_test("-f", x)) {
    sprintf("(\"%s\") is not a file", x)
  } else TRUE
}
is.non.obj.style <- function(x)
  string_in(x, "auto") || (is.list(x) && !is.object(x))

# Things that could possibly be output by substitute

is.possibly.substituted <- function(x)
  (is.atomic(x) && length(x) == 1L) || is.null(x) || is.name(x) || is.call(x)

# Checks common arguments across functions

check_args <- function(
  call, tar.exp, cur.exp, mode, context, line.limit, format, brightness,
  color.mode, pager, ignore.white.space, max.diffs, align, disp.width,
  hunk.limit, convert.hz.white.space, tab.stops, style, palette.of.styles,
  frame, tar.banner, cur.banner, guides, rds, trim, word.diff, unwrap.atomic,
  extra, interactive, term.colors, strip.sgr, sgr.supported, call.match
) {
  err <- make_err_fun(call)
  warn <- make_warn_fun(call)

  # Check for conflicting arguments

  formals <- tail(names(call.match), -1L)
  style.overrideable <- c("format", "brightness", "color.mode")
  if(
    "style" %in% formals && !is.non.obj.style(style) &&
    any(s.ov <- style.overrideable %in% formals)
  )
    warn(
      "Provided `style` argument will override the provided ",
      if(sum(s.ov) < 2L) {
        sprintf("`%s` argument", style.overrideable[s.ov])
      } else {
        paste0(
          paste0(
            sprintf("`%s`", head(style.overrideable[s.ov], -1L)),
            collapse=", "
          ),
          " and `", tail(style.overrideable[s.ov], 1L), "` arguments."
    ) } )

  # Check extra

  if(!is.list(extra)) err("Argument `extra` must be a list.")

  # Check context

  msg.base <- paste0(
    "Argument `%s` must be integer(1L) and not NA, an object produced ",
    "by `auto_context`, or \"auto\"."
  )
  if(
    !is.int.1L(context) && !is(context,"AutoContext") &&
    !identical(context, "auto")
  )
    err(sprintf(msg.base, "context"))

  if(!is(context, "AutoContext")) {
    context <- if(identical(context, "auto")) auto_context() else {
      if(is.int.1L(context) && context < 0) {
        min.cont <- 0
        max.cont <- -1
      } else if (is.int.1L(context)) {
        min.cont <- max.cont <- as.integer(context)
      } else {
        err("Argument `context` must be integer(1L) and not NA.")
      }
      cont <- try(auto_context(min.cont, max.cont))
      if(inherits(cont, "try-error"))
        # nocov start
        # should not be possible to get here given prior checks
        err(
          "Unable to instantiate an `AutoContext` object from provided ",
          "`context` argument.  Value should be integer(1L) and not NA, or ",
          "an `AutoContext` object as generated by `auto_context()`."
        )
        # nocov end
      cont
    }
  }
  # any 'substr' of them otherwise these checks fail

  val.modes <- c("auto", "unified", "context", "sidebyside")
  fail.mode <- FALSE
  if(!is.character(mode) || length(mode) != 1L || is.na(mode) || !nzchar(mode))
    fail.mode <- TRUE
  if(!fail.mode && !any(mode.eq <- substr(val.modes, 1, nchar(mode)) == mode))
    fail.mode <- TRUE
  if(fail.mode)
    err(
      "Argument `mode` must be character(1L) and in `", deparse(val.modes), "`."
    )

  # Tab stops

  tab.stops <- as.integer(tab.stops)
  if(
    !is.integer(tab.stops) || !length(tab.stops) >= 1L || anyNA(tab.stops) ||
    !all(tab.stops > 0L)
  )
    err(
      "Argument `tab.stops` must be integer containing at least one value and ",
      "with all values strictly positive"
    )
  # Limit vars

  hunk.limit <- check_limit(hunk.limit)
  if(!is.integer(hunk.limit)) err(sprintf(hunk.limit, "hunk.limit", "."))
  if(!is.integer(line.limit <- check_limit(line.limit)))
    err(
      sprintf(
        line.limit, "line.limit",
        ", or \"auto\" or the result of calling `auto_line_limit`"
    ) )
  # guides

  if(!is.TF(guides) && !is.function(guides))
    err("Argument `guides` must be TRUE, FALSE, or a function")
  if(is.function(guides) && !isTRUE(g.f.err <- is.two.arg.fun(guides)))
    err("Argument `guides` ", g.f.err)
  if(!is.function(guides) && !guides)
    guides <- function(obj, obj.as.chr) integer(0L)

  if(!is.TF(trim) && !is.function(trim))
    err("Argument `trim` must be TRUE, FALSE, or a function")
  if(is.function(trim) && !isTRUE(t.f.err <- is.two.arg.fun(trim)))
    err("Argument `trim` ", t.f.err)
  if(!is.function(trim) && !trim) trim <- trim_identity

  # check T F args

  if(is.null(interactive)) interactive <- interactive()
  TF.vars <- c(
    "ignore.white.space", "convert.hz.white.space", "rds", "word.diff",
    "unwrap.atomic", "interactive"
  )
  msg.base <- "Argument `%s` must be TRUE or FALSE."
  for(x in TF.vars) if(!is.TF(get(x, inherits=FALSE))) err(sprintf(msg.base, x))

  # int 1L vars

  if(is.null(term.colors)) term.colors <- crayon::num_colors()
  msg.base <- "Argument `%s` must be integer(1L) and not NA."
  int.1L.vars <- c("max.diffs", "term.colors")
  for(x in int.1L.vars) {
    if(!is.int.1L(int.val <- get(x, inherits=FALSE)))
      err(sprintf(msg.base, "max.diffs"))
    assign(x, as.integer(int.val))
  }
  # Banners; convolution here is to accomodate `diffObj` and have it be able
  # to pass captured target/current expressions

  chr1LorNULLorLanguage.vars <- c("tar.banner", "cur.banner")
  msg.base <-
    "Argument `%s` must be atomic and length(1L), NULL, a symbol, or a call"
  for(x in chr1LorNULLorLanguage.vars ) {
    y <- get(x, inherits=FALSE)
    if(!is.possibly.substituted(y)) err(sprintf(msg.base, x))
  }
  if(!is.chr.1L(tar.banner) && !is.null(tar.banner)) {
    tar.exp <- tar.banner
    tar.banner <- NULL
  }
  if(!is.chr.1L(cur.banner) && !is.null(cur.banner)) {
    cur.exp <- cur.banner
    cur.banner <- NULL
  }
  # Align threshold

  if(!is(align, "AlignThreshold")) {
    align <- if(
      is.numeric(align) && length(align) == 1L &&
      !is.na(align) && align %bw% c(0, 1)
    ) {
      AlignThreshold(threshold=align)
    } else if(is.null(align)) {
      AlignThreshold()
    } else err(
      "Argument `align` must be an \"AlignThreshold\" object or numeric(1L) ",
      "and between 0 and 1."
    )
  }
  # style

  valid_object(style, "style", err)
  if(
    !is(style, "Style") && !string_in(style, "auto") &&
    !(is.list(style) && !is.object(style))
  )
    err("Argument `style` must be \"auto\", a `Style` object, or a list.")

  # pager; 'on' just means use pager already associated with style

  valid_object(pager, "pager", err)
  valid.pagers <- c("auto", "off", "on")
  if(
    !is(pager, "Pager") && !string_in(pager, valid.pagers) &&
    !(is.list(pager) && !is.object(pager))
  )
    err(
      "Argument `pager` must be one of `", dep(valid.pagers),
      "`, a `Pager` object, or a list."
    )
  pager.args <- list()
  if(!is(pager, "Pager")) {
    if(string_in(pager, "off")) {
      pager <- PagerOff()
    } else if (is.list(pager)) {
      pager.args <- pager
      pager <- "on"
    }
  }
  # palette and arguments that reference palette dimensions

  if(is.null(palette.of.styles)) palette.of.styles <- PaletteOfStyles()
  if(!is(palette.of.styles, "PaletteOfStyles"))
    err("Argument `palette.of.styles` must be a `PaletteOfStyles` object.")

  palette.params <- c("brightness", "color.mode")
  for(x in palette.params)
    if(
      !isTRUE(
        msg <- is.valid.palette.param(
          get(x, inherits=FALSE), x, palette.of.styles
      ) )
    ) err(msg)

  # Figure out whether pager is allowable or not; note that "auto" pager just
  # means let the pager that comes built into the style be the pager

  if(is.character(pager))
    pager <- if(
      (pager == "auto" && interactive) || pager == "on"
    ) {
      "on"
    } else PagerOff()

  # format; decide what format to use

  if(
    !is(style, "Style") &&
    (
      string_in(style, "auto") || (is.list(style) && !is.object(style))
    )
  ) {
    if(is.list(style)) {
      style.args <- style
      style <- "auto"
    } else style.args <- list()

    # We only want to allow ansi styles if the pager supports them too;
    # unfortuantely we cannot have different styles depending on whether the
    # output is paged or not, at least not at this time

    pager.could.be.ansi <- if(is(pager, "Pager")) pager@ansi else FALSE

    if(!is.chr.1L(format))
      err("Argument `format` must be character(1L) and not NA")
    valid.formats <- c("auto", dimnames(palette.of.styles@data)$format)
    if(!format %in% valid.formats)
      err("Argument `format` must be one of `", dep(valid.formats) , "`.")
    if(format == "auto") {
      if(!is.int.1L(term.colors))
        # nocov start
        err(
          "Logic Error: unexpected return from `crayon::num_colors()`; ",
          "contact maintainer."
        )
        # nocov end
      # No recognized color alternatives, try to use HTML if we can

      format <- if(
        nzchar(Sys.getenv('RSTUDIO')) && !nzchar(Sys.getenv('RSTUDIO_TERM')) &&
        interactive
      ) {
        "html"
      } else if(
        term.colors < 8
      ) {
        if(!pager.could.be.ansi) {
          if(
            (interactive && identical(pager, "on")) || is(pager, "PagerBrowser")
          ) "html" else "raw"
        } else {
          if(!pager@threshold) "ansi8" else "raw"
        }
      } else if (term.colors < 256) {
        "ansi8"
      } else if (term.colors >= 256) {
        "ansi256"
      } else stop("Logic error: unhandled format; contact maintainer.") # nocov
    }
    style <- palette.of.styles[[
      format, get_pal_par(format, brightness), get_pal_par(format, color.mode)
    ]]
    if(is(style, "classRepresentation")) {
      style <- try(do.call("new", c(list(style), style.args)), silent=TRUE)
      if(inherits(style, "try-error")) {
        msg <- conditionMessage(attr(style, "condition"))
        err("Unable to instantiate `Style` object: ", msg)
      }
    } else {
      if(length(style.args)) {
        warn(
          "Extra `style` arguments cannot be applied because selected object ",
          "`palette.of.styles` is a `Style` instance rather than a `Style` ",
          "\"classRepresentation\".  See documentation for the `style` ",
          "parameter for details."
      ) }
      valid_object(
        style, "palette.of.styles", err,
        paste0(
          "Argument `%s` is an invalid `%s` because it contains and invalid ",
          "`Style` object:"
    ) ) }
  } else if(!is(style, "Style"))
    stop("Logic Error: unexpected style state; contact maintainer.") # nocov

  # Attach specific pager if it was requested generated; if "on" just let the
  # existing pager on the style be, which is done by not modifying @pager

  if(is(pager, "Pager")) {
    style@pager <- pager
  } else if (length(pager.args)) {
    ## this is a bit gnarly, and
    pager.s <- style@pager
    old.slots <-
      sapply(slotNames(pager.s), slot, object=pager.s, simplify=FALSE)
    pager.args <-
      c(pager.args, old.slots[setdiff(names(old.slots), names(pager.args))])
    style@pager <- do.call("new", c(list(class(pager.s)), pager.args))
  } else if(!identical(pager, "on"))
    stop("Logic Error: Unexpected pager state; contact maintainer.") # nocov

  # Check display width

  if(!isTRUE(d.w.err <- is.valid.width(disp.width)))
    err("Arugment `disp.width` ", d.w.err)
  disp.width <- as.integer(disp.width)
  if(disp.width) {
    style@disp.width <- disp.width
  } else if(!style@disp.width) {
    d.w <- getOption("width")
    if(!is.valid.width(d.w)) {
      # nocov start this should never happen
      warn("`getOption(\"width\") returned an invalid width, using 80L")
      d.w <- 80L
      # nocov end
    }
    style@disp.width <- d.w
  }
  disp.width <- style@disp.width

  # check strip.sgr

  if(!is.TF(strip.sgr) && !is.null(strip.sgr))
    err("Argument `strip.sgr` must be TRUE, FALSE, or NULL")
  if(is.null(strip.sgr)) strip.sgr <- is(style, "Ansi")

  # check strip.sgr

  if(!is.TF(sgr.supported) && !is.null(sgr.supported))
    err("Argument `sgr.supported` must be TRUE, FALSE, or NULL")
  if(is.null(sgr.supported))
    sgr.supported <- is(style, "Ansi") || crayon::has_color()

  # instantiate settings object

  etc <- new(
    "Settings", mode=val.modes[[which(mode.eq)]], context=context,
    line.limit=line.limit, ignore.white.space=ignore.white.space,
    max.diffs=max.diffs, align=align, disp.width=disp.width,
    hunk.limit=hunk.limit, convert.hz.white.space=convert.hz.white.space,
    tab.stops=tab.stops, style=style, frame=frame,
    tar.exp=tar.exp, cur.exp=cur.exp, guides=guides, tar.banner=tar.banner,
    cur.banner=cur.banner, trim=trim, word.diff=word.diff,
    unwrap.atomic=unwrap.atomic, strip.sgr=strip.sgr,
    sgr.supported=sgr.supported, err=err, warn=warn
  )
  etc
}
