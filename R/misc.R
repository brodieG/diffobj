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

check_args <- function(vals) {
  call <- sys.call(-2L)

  # check modes

  val.modes <- c("context", "unified", "sidebyside")
  if(
    !is.character(vals$mode) || length(vals$mode) != 1L || is.na(vals$mode) ||
    !vals$mode %in% val.modes
  ) {
    msg <- paste0(
      "Argument `mode` must be character(1L) and in `", deparse(val.modes), "`."
    )
    stop(simpleError(msg, call=call))
  }
  # check limits (has side effects)

  limits <- c("line.limit", "hunk.limit")
  vals[limits] <- Map(check_limit, vals[limits], limits)

  # check integer 1L args

  int1L.vars <- c(
    "context", "disp.width", "max.diffs", "max.diffs.in.hunk", "max.diffs.wrap"
  )
  msg.base <- "Argument `%s` must be integer(1L) and not NA."
  vals[int1L.vars] <- lapply(
    int1L.vars,
    function(x)
      if(!is.int.1L(vals[[x]])) {
        stop(simpleError(sprintf(msg.base, x), call=call))
      } else as.integer(vals[[x]])
  )
  # check T F args

  msg.base <- "Argument `%s` must be TRUE or FALSE"
  lapply(
    c("use.ansi", "ignore.white.space", "silent"),
    function(x)
      if(!is.TF(vals[[x]])) stop(simpleError(sprintf(msg.base, x), call=call))
  )
  # check char 1L

  msg.base <- "Argument `%s` must be character(1L) and not NA"
  lapply(
    c("tar.banner", "cur.banner"),
    function(x)
      if(!is.chr.1L(vals[[x]]))
        stop(simpleError(sprintf(msg.base, x), call=call))
  )
  # frame

  if(!is.environment(vals$frame))
    stop(simpleError("Argument `frame` must be an environment.", call=call))

  # Return modified args

  vals
}

is.int.1L <- function(x)
  is.numeric(x) && length(x) == 1L && !is.na(x) && x ==  round(x) &&
  is.finite(x)

is.TF <- function(x) isTRUE(x) || identical(x, FALSE)

is.chr.1L <- function(x) is.character(x) && length(x) == 1L && !is.na(x)

