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

# Used so that `with_mock` will work since these are primitives, for testing

interactive <- function() base::interactive()
readline <- function(...) if(interactive()) base::readline(...) # nocov

# Returns the indices of the original rle object that correspond to the
# ind rle values

rle_sub <- function(rle, ind) {
  ind <- if(is.numeric(ind)) {
    as.integer(ind)
  } else if(is.logical(ind)) {
    which(ind)
  } else stop("Logic Error: unexpected `ind` input")
  if(!all(ind) > 0 || !all(diff(ind) > 0))
    stop("Logic Error: `ind` should be monotonically increasing")

  len.cum <- cumsum(rle$lengths)
  all.ind <- Map(
    seq, from=c(1L, head(len.cum, -1L) + 1L), to=len.cum, by=1L
  )
  all.ind[ind]
}
# concatenate method for factors

c.factor <- function(..., recursive=FALSE) {
  dots <- list(...)
  dots.n.n <- dots[!vapply(dots, is.null, logical(1L))]
  if(!length(dots)) factor(0L) else {
    if(
      !all(vapply(dots.n.n, is, logical(1L), "factor")) ||
      length(unique(lapply(dots.n.n, levels))) != 1L
    ) {
      NextMethod()
    } else {
      int.f <- unlist(lapply(dots.n.n, as.integer))
      lvl <- levels(dots[[1L]])
      factor(lvl[int.f], levels=lvl)
    }
  }
}
# Pull out the names of the functions in a sys.call stack

stack_funs <- function(s.c) {
  if(!length(s.c)) stop("Logic Error: call stack empty; contact maintainer.")
  vapply(
    s.c, function(call) paste0(deparse(call), collapse="\n"), character(1L)
  )
}

.internal.call <- quote(.local(target, current, ...))

# Pull out the first call reading back from sys.calls that is likely to be
# be the top level call to the diff* methods.  This is somewhat fragile
# unfortunately, but there doesn't seem to be a systematic way to figure this
# out

which_top <- function(s.c) {
  if(!length(s.c))
    stop("Logic Error: stack should have at least one call, contact maintainer")
  funs <- stack_funs(s.c)
  fun.ref <- stack_funs(list(.internal.call))  # find .local call
  fun.ref.loc <- match(fun.ref, funs, nomatch=0L)

  f.rle <- rle(funs)
  val.calls <- f.rle$lengths == 2

  if(any(val.calls) && fun.ref.loc) {
    # return first index of last pairs of identical calls in the call stack
    # that is followed by a correct .internal call, and also that are not
    # calls to `eval`.

    rle.elig <- rle_sub(f.rle, which(val.calls))
    rle.elig.max <- vapply(rle.elig, max, integer(1L))
    rle.followed <- which(
      rle.elig.max < max(fun.ref.loc) & !grepl("eval\\(", funs[rle.elig.max])
    )
    if(!length(rle.followed)) {  # can't find correct one
      length(s.c)
    } else {
      rle.elig[[max(rle.followed)]][1L]
    }
  } else {
    # failed to find a value, so just return last call on stack
    length(s.c)
  }
}
get_fun <- function(name, env) {
  get.fun <- if(is.name(name) || (is.character(name) && length(name) == 1L)) {
    try(get(as.character(name), envir=env), silent=TRUE)
  } else if(
    is.call(name) && (
      identical(as.character(name[[1L]]), "::") ||
      identical(as.character(name[[1L]]), ":::")
    ) && length(name) == 3L
  ) {
    get.fun <- try(eval(name, env))
  }
  if(is.function(get.fun)) get.fun else {
    warning(
      "Unable to find function `", deparse(name), "` to ",
      "match call with."
    )
    NULL
  }
}
extract_call <- function(s.c, par.env) {
  idx <- which_top(s.c)
  found.call <- s.c[[idx]]
  no.match <- list(call=NULL, tar=NULL, cur=NULL)
  get.fun <- get_fun(found.call[[1L]], env=par.env)
  res <- no.match
  if(is.function(get.fun)) {
    found.call.m <- try(match.call(definition=get.fun, call=found.call))
    if(!inherits(found.call.m, "try-error")) {
      if(length(found.call.m) < 3L) length(found.call.m) <- 3L
      res <-
        list(call=found.call.m, tar=found.call.m[[2L]], cur=found.call.m[[3L]])
    }
  }
  res
}
#' Get Parent Frame of S4 Call Stack
#'
#' Implementation of the \code{function(x=parent.frame()) ...} pattern for the
#' \code{\link[=diffPrint]{diff*}} methods since the normal pattern does not
#' work with S4 methods.  Works by looking through the call stack and
#' identifying what call likely initiated the S4 dispatch.
#'
#' The function is not exported and intended only for use as the default value
#' for the \code{frame} argument for the \code{\link[=diffPrint]{diff*}}
#' methods.
#'
#' Matching is done purely by looking for the last repeated call followed
#' by \code{.local(target, current, ...)} that is not a call to \code{eval}.
#' This pattern seems to match the correct call most of the time.
#' Since methods can be renamed by the user we make no attempt to verify method
#' names.  This method could potentially be tricked if you implement custom
#' \code{\link[=diffPrint]{diff*}} methods that somehow
#' issue two identical sequential calls before calling \code{callNextMethod}.
#' Failure in this case means the wrong \code{frame} will be returned.
#'
#' @return an environment

par_frame <- function() {
  s.c <- head(sys.calls(), -1L)
  top <- which_top(s.c)
  par <- sys.parents()[top]
  if(par) {
    sys.frames()[[par]]
  } else .GlobalEnv
}

# check whether running in knitr
# in_knitr <- function() isTRUE(getOption('knitr.in.progress'))

make_err_fun <- function(call)
  function(...) stop(simpleError(do.call(paste0, list(...)), call=call))

make_warn_fun <- function(call)
  function(...) warning(simpleWarning(do.call(paste0, list(...)), call=call))

# Function used to match against `str` calls since the existing function
# does not actually define `max.level`; note it never is actually called
# nocov start

str_tpl <- function(object, max.level, comp.str, indent.str, ...) NULL

# nocov end

# utility fun to deparse into chr1L

dep <- function(x) paste0(deparse(x, width.cutoff=500L), collapse="")

# Reports how many levels deep each line of a `str` screen output is

str_levels <- function(str.txt, wrap=FALSE) {
  if(length(str.txt) < 2L) {
    integer(length(str.txt))
  } else {
    # annoying `wrap` kills leading whitespace, so we need separate patterns

    sub.pat <- if(wrap) {
      "^(\\.\\. )*\\.\\.[@$\\-]"
    } else {
      "^ ( \\.\\.)*[@$\\-]"
    }
    tl.pat <- if(wrap) "^(\\$|-)" else "^ (\\$|-)"
    subs <- character(length(str.txt))
    subs.rg <- regexpr(sub.pat, str.txt, perl=TRUE)
    subs[subs.rg > 0] <- regmatches(str.txt, subs.rg)
    subs.fin <- regmatches(subs, gregexpr("\\.\\.", subs, perl=TRUE))

    level <- vapply(subs.fin, length, integer(1L))
    top.level <- grepl(tl.pat, str.txt)
    level[!!level & !top.level] <- level[!!level & !top.level] + 1L
    level[1L] <- 0L
    level[top.level] <- 1L

    # handle potential wrapping; need to detect which sections of the text
    # are at level 0, and if they are, give them the depth of the previous
    # section
    if(wrap) {
      sects <- c(
        0L, cumsum(xor(head(level, -1L) == 0L, tail(level, -1L) == 0L))
      )
      level.s <- split(level, sects)
      if(length(level.s) > 1L) {
        for(i in 2L:length(level.s)){
          if(!any(level.s[[i]])) level.s[[i]][] <- tail(level.s[[i - 1L]], 1L)
        }
        # could just unlist since sections are supposed to be monotonic in vec
        level <- unsplit(level.s, sects)
      }
    }
    level
  }
}
# Calculate how many lines the banner will take up

banner_len <- function(mode) if(mode == "sidebyside") 1L else 2L

# Compute display width in characters
#
# Note this does not account for the padding required

.pad <- list(context=2L, sidebyside=2L, unified=2L)
.min.width <- 6L

calc_width <- function(width, mode) {
  # stopifnot(
  #   is.numeric(width), length(width) == 1L, !is.na(width), is.finite(width),
  #   width >= 0L,
  #   is.character(mode), mode %in% c("context", "unified", "sidebyside")
  # )
  width <- as.integer(width)
  width.tmp <- if(mode == "sidebyside")
    as.integer(floor((width - 2)/ 2)) else width
  as.integer(max(.min.width, width.tmp))
}
calc_width_pad <- function(width, mode) {
  # stopifnot(
  #   is.character(mode), mode %in% c("context", "unified", "sidebyside")
  # )
  width.tmp <- calc_width(width, mode)
  width.tmp - .pad[[mode]]
}
# Helper function to retrieve a palette parameter

get_pal_par <- function(format, param) {
  if(is.chr.1L(param) && is.null(names(param))) {
    param
  } else if(format %in% names(param)) {
    param[format]
  } else if (wild.match <- match("", names(param), nomatch=0L)) {
    param[wild.match]
  } else stop("Logic Error: malformed palette parameter; contact maintainer.")
}
# check whether argument list contains non-default formals

has_non_def_formals <- function(arg.list) {
  stopifnot(is.pairlist(arg.list) || is.list(arg.list))
  any(
    vapply(
      arg.list,
      function(x) is.name(x) && !nzchar(as.character(x)),
      logical(1L)
  ) )
}

# Between

`%bw%` <- function(x, y) {
  stopifnot(length(y) == 2L)
  if(y[[1L]] < y[[2L]]) {
    low <- y[[1L]]
    hi <- y[[2L]]
  } else {
    hi <- y[[1L]]
    low <- y[[2L]]
  }
  x >= low & x <= hi
}

flatten_list <- function(l)
  if(is.list(l) && !is.object(l) && length(l))
    do.call(c, lapply(l, flatten_list)) else list(l)
