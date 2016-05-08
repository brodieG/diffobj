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
# check whether running in knitr

in_knitr <- function() isTRUE(getOption('knitr.in.progress'))

make_err_fun <- function(call)
  function(...) stop(simpleError(do.call(paste0, list(...)), call=call))

# Function used to match against `str` calls since the existing function
# does not actually define `max.level`

str_tpl <- function(object, max.level, ...) NULL

# Reduce `str` output to a level

trim_str <- function(str.txt, level, kj) {
  NULL
}
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

# Compute list depth including attributes
#
# These should line up with the max.level param of `str`

list_depth <- function(x, depth=0L) {
  max.lvl <- max.lvl.l <- max.lvl.a <- depth
  depth <- depth + 1L
  if(is.list(x)) {
    max.lvl.l <- max(
      unlist(lapply(x, list_depth, depth=depth), recursive=TRUE), depth
    )
  }
  if(length(attrs <- attributes(x))) {
    max.lvl.a <- max(
      unlist(lapply(attrs, list_depth, depth=depth), recursive=TRUE), depth
    )
  }
  max(max.lvl.l, max.lvl.a)
}
# Compute display width in characters
#
# Note this does not account for the padding required

.pad <- list(context=2L, sidebyside=2L, unified=2L)
.min.width <- 15L

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
calc_width_unpad <- function(capt.width, mode) {
  stopifnot(
    is.character(mode), mode %in% c("context", "unified", "sidebyside")
  )
  capt.width + .pad[[mode]]
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
