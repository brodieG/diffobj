# check whether argument list contains non-default formals

has_non_def_formals <- function(arg.list) {
  stopifnot(is.pairlist(arg.list))
  any(
    vapply(
      arg.list,
      function(x) is.name(x) && !nzchar(as.character(x))
      logical(1L)
  ) )
}

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
# for checking the limits, if successful returns an integer(2L) vector,
# otherwise a character vector to sprintf as an error

check_limit <- function(limit) {
  if(
    !is.numeric(limit) || any(is.na(limit)) ||
    !length(limit) %in% 1:2 ||
    !is.finite(limit) ||
    round(limit) != limit ||
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
# Checks common arguments across functions

check_args <- function(call, tar.exp, cur.exp, mode, context, etc) {
  err <- make_err_fun(call)

  if(!is(etc, "diffObjSettings"))
    err("Argument `etc` must be a `diffObjSettings` S4 object")

  msg.base <- paste0(
    "Argument `%s` must be integer(1L) and not NA, an object produced ",
    "by `auto_context`, or \"auto\"."
  )
  if(
    !is.int.1L(context) && !is(context,"diffObjAutoContext") &&
    !identical(context, "auto")
  )
    err(sprintf(msg.base, "context"))

  if(!is(context, "diffObjAutoContext")) {
    context <- if(identical(context, "auto")) auto_context() else
      auto_context(as.integer(context), as.integer(context))
  }
  # any 'substr' of them otherwise these checks fail

  val.modes <- c("unified", "context", "sidebyside")
  fail.mode <- FALSE
  if(!is.character(mode) || length(mode) != 1L || is.na(mode) || !nzchar(mode))
    fail.mode <- TRUE
  if(!fail.mode && !any(mode.eq <- substr(val.modes, 1, nchar(mode)) == mode))
    fail.mode <- TRUE
  if(fail.mode)
    err(
      "Argument `mode` must be character(1L) and in `", deparse(val.modes), "`."
    )

  # Update the etc object

  etc@mode <- val.modes[[which(mode.eq)]]
  etc@context <- context
  etc@tar.exp <- tar.exp
  etc@cur.exp <- cur.exp
  etc
}

is.int.1L <- function(x)
  is.numeric(x) && length(x) == 1L && !is.na(x) && x ==  round(x) &&
  is.finite(x)

is.int.2L <- function(x)
  is.numeric(x) && length(x) == 2L && !anyNA(x) && x ==  round(x) &&
  is.finite(x)

is.TF <- function(x) isTRUE(x) || identical(x, FALSE)

is.chr.1L <- function(x) is.character(x) && length(x) == 1L && !is.na(x)

is.diffs <- function(x)
  is.list(x) && length(x) == 4L &&
  identical(names(x), c("hunks", "diffs", "diffs.max", "hit.diffs.max")) &&
  is.list(x$hunks) && is.int.1L(x$diffs) && is.int.1L(x$diffs.max) &&
  is.TF(x$hit.diffs.max)

is.valid.palette.param <- function(x, param, palette) {
  stopifnot(is(palettes, "diffObjStylePalette"))
  stopifnot(isTRUE(param %in% c("brightness", "color.mode")))
  valid.formats <- dimnames(palette@data)$format
  valid.params <- dimnames(palette@data)[[param]]

  if(!is.character(x) || anyNA(x))
    stop("Argument `", param, "` must be character and not contain NAs")

  if(!all(x %in% valid.params))
    stop(
      "Argument `", param, "` may only contain values in `", dep(valid.params),
      "`"
    )

  if(
    (length(x) > 1L && is.null(names(x))) ||
    (!is.null(names(x)) && !"" %in% names(x)) ||
    !all(names(x) %in% valid.formats)
  )
    stop(
      "Argument `", param, "` must have names if it has length > 1, and those ",
      "names must include at least an empty name `\"\"` as well as names only ",
      "from `", dep(valid.formats), "`."
    )
  TRUE
}
# Helper function to retrieve a palette parameter

get_pal_param <- function(format, param) {
  if(format %in% names(param)) {
    param[format]
  } else if (wild.match <- match("", names(param), no.match=0L)) {
    param[wild.match]
  } else stop("Logic Error: malformed palette parameter; contact maintainer.")
}
