# Function used to match against `str` calls since the existing function
# does not actually define `max.level`

str_tpl <- function(object, max.level, ...) NULL

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

calc_width <- function(width, mode) {
  stopifnot(
    is.numeric(width), length(width) == 1L, !is.na(width), is.finite(width),
    width >= 0L,
    is.character(mode), mode %in% c("context", "unified", "sidebyside")
  )
  width <- as.integer(width)
  width.tmp <- if(mode == "sidebyside")
    as.integer(floor((width - 2)/ 2)) else width
  as.integer(max(20L, width.tmp))
}
calc_width_pad <- function(width, mode) {
  stopifnot(
    is.character(mode), mode %in% c("context", "unified", "sidebyside")
  )
  width.tmp <- calc_width(width, mode)
  width.tmp - if(mode == "sidebyside") 2L else 2L # happens to be same now
}
# Common argument check functions; note that the `stop` message reports as the
# parent system call

check_linelimit <- function(line.limit) {
  if(
    !is.numeric(line.limit) || any(is.na(line.limit)) ||
    !length(line.limit) %in% 1:2 ||
    round(line.limit) != line.limit ||
    (length(line.limit) == 2L && diff(line.limit) > 0)
  ) {
    msg <- paste0(
      "Argument `line.limit` must be an integer vector of length 1 or 2 ",
      "and if length 2, with the first ",
      "value larger than or equal to the second."
    )
    stop(simpleError(msg, call=sys.call(-1L)))
  }
  line.limit <- as.integer(line.limit)
  if(length(line.limit) == 1L) line.limit <- rep(line.limit, 2L)
  line.limit
}
check_context <- function(context) {
  if(
    !is.numeric(context) || length(context) != 1L || is.na(context) ||
    round(context) != context
  )
    stop(
      simpleError(
        "Argument `context` must be integer(1L) and not NA.",
        call=sys.call(-1L)
    ) )
  as.integer(context)
}
check_width <- function(width) {
  err.msg <- "must be integer(1L) and strictly positive"
  if(is.null(width)) {
    width <- getOption("width")
    if(!is.int.pos.1L(width))
      stop("`getOption(\"width\")` ", err.msg)
  }
  if(!is.int.pos.1L(width)) stop("Argument `width` ", err.msg)
  width
}
check_hunklimit <- function(hunk.limit) {
  if(
    !is.numeric(hunk.limit) || length(hunk.limit) != 1L || is.na(hunk.limit) ||
    round(hunk.limit) != hunk.limit
  )
    stop(
      simpleError(
        "Argument `hunk.limit` must be integer(1L) and not NA.",
        call=sys.call(-1L)
    ) )
  as.integer(hunk.limit)
}
check_mode <- function(mode) {
  val.modes <- c("context", "unified", "sidebyside")
  if(
    !is.character(mode) || length(mode) != 1L || is.na(mode) ||
    !mode %in% val.modes
  ) {
    msg <- paste0(
      "Argument `mode` must be character(1L) and in `", deparse(val.modes), "`."
    )
    stop(simpleError(msg, call=sys.call(-1L)))
  }
  mode
}
# for checking the limits; for use exclusively within `check_args`
#
# run exclusively for side effects (throwing an error, or assigning value in
# parent env of 'check_args').

check_limit <- function(limit, type) {
  if(
    !is.numeric(limit) || any(is.na(limit)) ||
    !length(limit) %in% 1:2 ||
    !is.finite(limit) ||
    round(limit) != limit ||
    (length(limit) == 2L && diff(limit) > 0)
  ) {
    msg <- paste0(
      "Argument `%s` must be an integer vector of length 1 or 2 ",
      "and if length 2, with the first ",
      "value larger than or equal to the second."
    )
    stop(sprintf(msg, type), call=sys.call(-2L))
  }
  limit <- as.integer(limit)
  if(length(limit) == 1L) limit <- rep(limit, 2L)
  limit
}
# Checks common arguments across functions
#
# Note: this will modify the value of some of the arguments in the parent
# environment

check_args <- function(env.to.check, call) {

  # Define variables to check

  limit.vars <- c("line.limit", "hunk.limit")
  int1L.vars <- c(
    "context", "disp.width", "max.diffs", "max.diffs.in.hunk", "max.diffs.wrap"
  )
  TF.vars <- c("use.ansi", "ignore.white.space", "silent")
  chr1LorNULL.vars <- c("tar.banner", "cur.banner")

  # check modes

  val.modes <- c("unified", "context", "sidebyside")
  if(
    !is.character(env.to.check$mode) || length(env.to.check$mode) != 1L ||
    is.na(env.to.check$mode) || !env.to.check$mode %in% val.modes
  ) {
    msg <- paste0(
      "Argument `mode` must be character(1L) and in `", deparse(val.modes), "`."
    )
    stop(simpleError(msg, call=call))
  }
  # check limit.vars (has side effects)

  for(i in limit.vars) env.to.check[[i]] <- check_limit(env.to.check[[i]], i)

  # check integer 1L args

  msg.base <- "Argument `%s` must be integer(1L) and not NA."
  for(i in int1L.vars) env.to.check[[i]] <-
    if(!is.int.1L(env.to.check[[i]])) {
      stop(simpleError(sprintf(msg.base, i), call=call))
    } else as.integer(env.to.check[[i]])

  # check T F args

  msg.base <- "Argument `%s` must be TRUE or FALSE."
  lapply(
    TF.vars,
    function(x)
      if(!is.TF(env.to.check[[x]]))
        stop(simpleError(sprintf(msg.base, x), call=call))
  )
  # check char 1L

  msg.base <- "Argument `%s` must be character(1L) and not NA, or NULL"
  lapply(
    chr1LorNULL.vars,
    function(x)
      if(!is.chr.1L(env.to.check[[x]]) && !is.null(env.to.check[[x]]))
        stop(simpleError(sprintf(msg.base, x), call=call))
  )
  # frame

  if(!is.environment(env.to.check$frame))
    stop(simpleError("Argument `frame` must be an environment.", call=call))

  # Function does not return as we modify env.to.check as needed

  NULL
}

is.int.1L <- function(x)
  is.numeric(x) && length(x) == 1L && !is.na(x) && x ==  round(x) &&
  is.finite(x)

is.TF <- function(x) isTRUE(x) || identical(x, FALSE)

is.chr.1L <- function(x) is.character(x) && length(x) == 1L && !is.na(x)

