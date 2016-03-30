
# Function used to match against `str` calls since the existing function
# does not actually define `max.level`

str_tpl <- function(object, max.level, ...) NULL

# Reduce `str` output to a level

trim_str <- function(str.txt, level, kj) {
  NULL
}

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
# for checking the limits
#
# run exclusively for side effects (throwing an error, or assigning value in
# parent env of 'check_args').

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
#
# Note: this will modify the value of some of the arguments in the parent
# environment

check_args <- function(
  call, tar.exp, cur.exp, mode, context, line.limit, settings
) {
  err <- function(txt) stop(simpleError(txt, call=call))

  if(!is(settings, "diffObjSettings"))
    err("Argument `settings` must be a `diffObjSettings` S4 object")

  msg.base <- paste0(
    "Argument `%s` must be integer(1L) and not NA, an object produced ",
    "by `auto_context`, or \"auto\"."
  )
  if(
    !is.int.1L(context) && !is(context,"diffObjAutoContext") &&
    !identical(context, "auto")
  )
    err(sprintf(msg.base, "context"))

  context <- if(identical(context, "auto")) auto_context() else
    as.integer(context)

  # check line limit

  if(
    !identical(line.limit, "auto") && !is(line.limit, "diffObjAutoLineLimit")
  ) {
    if(!is.integer(ll.check <- check_limit(line.limit)))
      err(
        sprintf(
          ll.check, "line.limit",
          ", or \"auto\" or the result of calling `auto_line_limit`"
      ) )
    line.limit <- ll.check
  } else if (identical(line.limit, "auto")) {
    line.limit <- auto_line_limit()
  }
  # check modes

  val.modes <- c("unified", "context", "sidebyside")
  if(
    !is.character(mode) || length(mode) != 1L || is.na(mode) || 
    !mode %in% val.modes
  ) {
    msg <- paste0(
      "Argument `mode` must be character(1L) and in `", deparse(val.modes), "`."
    )
    stop(simpleError(msg, call=call))
  }
  # Update the settings object

  settings@line.limit <- line.limit
  settings@context <- context
  settings@mode <- mode
  settings@tar.exp <- tar.exp
  settings@cur.exp <- cur.exp
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

