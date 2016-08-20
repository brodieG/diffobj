# diffobj - Diffs for R Objects
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

# Capture output of print/show/str; unfortunately doesn't have superb handling
# of errors during print/show call, though hopefully these are rare
#
# x is a quoted call to evaluate

capture <- function(x, etc, err) {
  capt.width <- etc@text.width
  if(capt.width) {
    opt.set <- try(width.old <- options(width=capt.width), silent=TRUE)
    if(inherits(opt.set, "try-error")) {
      warning(
        "Unable to set desired width ", capt.width, ", (",
        conditionMessage(attr(opt.set, "condition")), ");",
        "proceeding with existing setting."
      )
    } else on.exit(options(width.old))
  }
  res <- try(obj.out <- capture.output(eval(x, etc@frame)))
  if(inherits(res, "try-error"))
    err(
      "Failed attempting to get text representation of object: ",
      conditionMessage(attr(res, "condition"))
    )
  html_ent_sub(res, etc@style)
}
# capture normal prints, along with default prints to make sure that if we
# do try to wrap an atomic vector print it is very likely to be in a format
# we are familiar with and not affected by a non-default print method

capt_print <- function(target, current, etc, err, extra){
  dots <- extra
  # What about S4?
  print.match <- try(
    match.call(
      get("print", envir=etc@frame),
      as.call(c(list(quote(print), x=NULL), dots)),
      envir=etc@frame
  ) )
  if(inherits(print.match, "try-error"))
    err("Unable to compose `print` call")

  names(print.match)[[2L]] <- ""
  tar.call <- cur.call <- print.match

  if(length(dots)) {
    if(!is.null(etc@tar.exp)) tar.call[[2L]] <- etc@tar.exp
    if(!is.null(etc@cur.exp)) cur.call[[2L]] <- etc@cur.exp
    tar.call[[2L]] <- etc@tar.exp
    cur.call[[2L]] <- etc@cur.exp
    etc@tar.banner <- deparse(tar.call)[[1L]]
    etc@cur.banner <- deparse(cur.call)[[1L]]
  }
  if(!is.null(target)) tar.call[[2L]] <- target
  if(!is.null(current)) cur.call[[2L]] <- current

  # If dimensioned object, and in auto-mode, switch to side by side if stuff is
  # narrow enough to fit

  if((!is.null(dim(target)) || !is.null(dim(current)))) {
    cur.capt <- capture(cur.call, etc, err)
    tar.capt <- capture(tar.call, etc, err)
    etc <- set_mode(etc, tar.capt, cur.capt)
  } else {
    etc <- if(etc@mode == "auto") sideBySide(etc) else etc
    cur.capt <- capture(cur.call, etc, err)
    tar.capt <- capture(tar.call, etc, err)
  }
  if(isTRUE(etc@guides)) etc@guides <- guidesPrint
  if(isTRUE(etc@trim)) etc@trim <- trimPrint

  diff.out <- line_diff(target, current, tar.capt, cur.capt, etc=etc, warn=TRUE)
  diff.out@capt.mode <- "print"
  diff.out
}
# Tries various different `str` settings to get the best possible output

capt_str <- function(target, current, etc, err, extra){
  # Match original call and managed dots, in particular wrt to the
  # `max.level` arg
  dots <- extra
  frame <- etc@frame
  line.limit <- etc@line.limit
  if("object" %in% names(dots))
    err("You may not specify `object` as part of `extra`")

  str.match <- try(
    match.call(
      str_tpl,
      call=as.call(c(list(quote(str), object=NULL), dots)), envir=etc@frame
  ) )
  if(inherits(str.match, "try-error"))
    err("Unable to compose `str` call")

  names(str.match)[[2L]] <- ""

  # Handle auto mode (side by side always for `str`)

  if(etc@mode == "auto") etc <- sideBySide(etc)

  # Utility function; defining in body so it has access to `err`

  eval_try <- function(match.list, index, envir)
    tryCatch(
      eval(match.list[[index]], envir=envir),
      error=function(e)
        err("Error evaluating `", index, "` arg: ", conditionMessage(e))
    )
  # Setup / process extra args

  auto.mode <- FALSE
  max.level.supplied <- FALSE
  if(
    max.level.pos <- match("max.level", names(str.match), nomatch=0L)
  ) {
    # max.level specified in call; check for special 'auto' case
    max.level.eval <- eval_try(str.match, "max.level", etc@frame)
    if(identical(max.level.eval, "auto")) {
      auto.mode <- TRUE
      str.match[["max.level"]] <- NA
    } else {
      max.level.supplied <- TRUE
    }
  } else {
    str.match[["max.level"]] <- NA
    auto.mode <- TRUE
    max.level.pos <- length(str.match)
    max.level.supplied <- FALSE
  }
  # Was wrap specified in strict width mode?

  wrap <- FALSE
  if("strict.width" %in% names(str.match)) {
    res <- eval_try(str.match, "strict.width", etc@frame)
    wrap <- is.character(res) && length(res) == 1L && !is.na(res) &&
      nzchar(res) && identical(res, substr("wrap", 1L, nchar(res)))
  }
  if(auto.mode) {
    msg <-
      "Specifying `%s` may cause `str` output level folding to be incorrect"
    if("comp.str" %in% names(str.match)) warning(sprintf(msg, "comp.str"))
    if("indent.str" %in% names(str.match)) warning(sprintf(msg, "indent.str"))
  }
  # don't want to evaluate target and current more than once, so can't eval
  # tar.exp/cur.exp, so instead run call with actual object

  tar.call <- cur.call <- str.match
  if(!is.null(target)) tar.call[[2L]] <- target
  if(!is.null(current)) cur.call[[2L]] <- current

  # Run str

  capt.width <- etc@text.width
  has.diff <- has.diff.prev <- FALSE

  # we used to strip_hz_control here, but shouldn't have to since handled by
  # line_diff

  tar.capt <- capture(tar.call, etc, err)
  tar.lvls <- str_levels(tar.capt, wrap=wrap)
  cur.capt <- capture(cur.call, etc, err)
  cur.lvls <- str_levels(cur.capt, wrap=wrap)

  prev.lvl.hi <- lvl <- max.depth <- max(tar.lvls, cur.lvls)
  prev.lvl.lo <- 0L
  first.loop <- TRUE
  safety <- 0L
  warn <- TRUE

  if(isTRUE(etc@guides)) etc@guides <- guidesStr
  if(isTRUE(etc@trim)) etc@trim <- trimStr

  tar.str <- tar.capt
  cur.str <- cur.capt

  diff.obj <- diff.obj.full <- line_diff(
    target, current, tar.str, cur.str, etc=etc, warn=warn
  )
  if(!max.level.supplied) {
    repeat{
      if((safety <- safety + 1L) > max.depth && !first.loop)
        stop(
          "Logic Error: exceeded list depth when comparing structures; contact ",
          "maintainer."
        )
      if(!first.loop) {
        tar.str <- tar.capt[tar.lvls <= lvl]
        cur.str <- cur.capt[cur.lvls <= lvl]

        diff.obj <- line_diff(
          target, current, tar.str, cur.str, etc=etc, warn=warn
        )
      }
      if(diff.obj@hit.diffs.max) warn <- FALSE
      has.diff <- suppressWarnings(any(diff.obj))

      # If there are no differences reducing levels isn't going to help to
      # find one; additionally, if not in auto.mode we should not be going
      # through this process

      if(first.loop && !has.diff) break
      first.loop <- FALSE

      if(line.limit[[1L]] < 1L) break

      line.len <- diff_line_len(
        diff.obj@diffs, etc=etc, tar.capt=tar.str, cur.capt=cur.str
      )
      # We need a higher level if we don't have diffs

      if(!has.diff && prev.lvl.hi - lvl > 1L) {
        prev.lvl.lo <- lvl
        lvl <- lvl + as.integer((prev.lvl.hi - lvl) / 2)
        tar.call[[max.level.pos]] <- lvl
        cur.call[[max.level.pos]] <- lvl
        next
      } else if(!has.diff) {
        diff.obj <- diff.obj.full
        lvl <- NULL
        break
      }
      # If we have diffs, need to check whether we should try to reduce lines
      # to get under line limit

      if(line.len <= line.limit[[1L]]) {
        # We fit, nothing else to do
        break
      }
      if(lvl - prev.lvl.lo > 1L) {
        prev.lvl.hi <- lvl
        lvl <- lvl - as.integer((lvl - prev.lvl.lo) / 2)
        tar.call[[max.level.pos]] <- lvl
        cur.call[[max.level.pos]] <- lvl
        next
      }
      # Couldn't get under limit, so use first run results

      diff.obj <- diff.obj.full
      lvl <- NULL
      break
    }
  } else {
    tar.str <- tar.capt[tar.lvls <= max.level.eval]
    cur.str <- cur.capt[cur.lvls <= max.level.eval]

    lvl <- max.level.eval
    diff.obj <- line_diff(target, current, tar.str, cur.str, etc=etc, warn=warn)
  }
  if(auto.mode && !is.null(lvl) && lvl < max.depth) {
    str.match[[max.level.pos]] <- lvl
  } else if (!max.level.supplied || is.null(lvl)) {
    str.match[[max.level.pos]] <- NULL
  }
  tar.call <- cur.call <- str.match
  if(!is.null(etc@tar.exp)) tar.call[[2L]] <- etc@tar.exp
  if(!is.null(etc@cur.exp)) cur.call[[2L]] <- etc@cur.exp
  if(is.null(etc@tar.banner))
    diff.obj@etc@tar.banner <- deparse(tar.call)[[1L]]
  if(is.null(etc@cur.banner))
    diff.obj@etc@cur.banner <- deparse(cur.call)[[1L]]

  # Track total differences in fully expanded view so we can report hidden
  # diffs when folding levels

  diff.obj@diff.count.full <- count_diffs(diff.obj.full@diffs)
  diff.obj@capt.mode <- "str"
  diff.obj
}
capt_chr <- function(target, current, etc, err, extra){
  tar.capt <- if(!is.character(target))
    do.call(as.character, c(list(target), extra)) else target
  cur.capt <- if(!is.character(current))
    do.call(as.character, c(list(current), extra)) else current

  etc <- set_mode(etc, tar.capt, cur.capt)
  if(isTRUE(etc@guides)) etc@guides <- guidesChr
  if(isTRUE(etc@trim)) etc@trim <- trimChr

  diff.out <- line_diff(
    target, current, html_ent_sub(tar.capt, etc@style),
    html_ent_sub(cur.capt, etc@style), etc=etc
  )
  diff.out@capt.mode <- "chr"
  diff.out
}
capt_deparse <- function(target, current, etc, err, extra){
  dep.try <- try({
    tar.capt <- do.call(deparse, c(list(target), extra))
    cur.capt <- do.call(deparse, c(list(current), extra))
  })
  if(inherits(dep.try, "try-error"))
    err("Error attempting to deparse object(s)")

  etc <- set_mode(etc, tar.capt, cur.capt)
  if(isTRUE(etc@guides)) etc@guides <- guidesDeparse
  if(isTRUE(etc@trim)) etc@trim <- trimDeparse

  diff.out <- line_diff(
    target, current, html_ent_sub(tar.capt, etc@style),
    html_ent_sub(cur.capt, etc@style), etc=etc
  )
  diff.out@capt.mode <- "deparse"
  diff.out
}
capt_file <- function(target, current, etc, err, extra) {
  tar.capt <- try(do.call(readLines, c(list(target), extra)))
  if(inherits(tar.capt, "try-error")) err("Unable to read `target` file.")
  cur.capt <- try(do.call(readLines, c(list(current), extra)))
  if(inherits(cur.capt, "try-error")) err("Unable to read `current` file.")

  etc <- set_mode(etc, tar.capt, cur.capt)
  if(isTRUE(etc@guides)) etc@guides <- guidesFile
  if(isTRUE(etc@trim)) etc@trim <- trimFile

  diff.out <- line_diff(
    target, current, html_ent_sub(tar.capt, etc@style),
    html_ent_sub(cur.capt, etc@style), etc=etc
  )
  diff.out@capt.mode <- "file"
  diff.out
}
capt_csv <- function(target, current, etc, err, extra){
  tar.df <- try(do.call(read.csv, c(list(target), extra)))
  if(inherits(tar.df, "try-error")) err("Unable to read `target` file.")
  if(!is.data.frame(tar.df))
    err("`target` file did not produce a data frame when read")
  cur.df <- try(do.call(read.csv, c(list(current), extra)))
  if(inherits(cur.df, "try-error")) err("Unable to read `current` file.")
  if(!is.data.frame(cur.df))
    err("`current` file did not produce a data frame when read")

  capt_print(tar.df, cur.df, etc, err, extra)
}
# Sets mode to "unified" if stuff is too wide to fit side by side without
# wrapping otherwise sets it in "sidebyside"

set_mode <- function(etc, tar.capt, cur.capt) {
  stopifnot(is(etc, "Settings"), is.character(tar.capt), is.character(cur.capt))
  nc_fun <- etc@style@nchar.fun
  if(etc@mode == "auto") {
    if(
      any(nc_fun(cur.capt) > etc@text.width.half) ||
      any(nc_fun(tar.capt) > etc@text.width.half)
    ) {
      etc@mode <- "unified"
  } }
  if(etc@mode == "auto") etc <- sideBySide(etc)
  etc
}
