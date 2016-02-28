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

.pad <- list(context=2L, sidebyside=2L, unified=2L)
.min.width <- 15L

calc_width <- function(width, mode) {
  stopifnot(
    is.numeric(width), length(width) == 1L, !is.na(width), is.finite(width),
    width >= 0L,
    is.character(mode), mode %in% c("context", "unified", "sidebyside")
  )
  width <- as.integer(width)
  width.tmp <- if(mode == "sidebyside")
    as.integer(floor((width - 2)/ 2)) else width
  as.integer(max(.min.width, width.tmp))
}
calc_width_pad <- function(width, mode) {
  stopifnot(
    is.character(mode), mode %in% c("context", "unified", "sidebyside")
  )
  width.tmp <- calc_width(width, mode)
  width.tmp - .pad[[mode]]
}
calc_width_unpad <- function(capt.width, mode) {
  stopifnot(
    is.character(mode), mode %in% c("context", "unified", "sidebyside")
  )
  capt.width + .pad[[mode]]
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
    "disp.width", "max.diffs", "max.diffs.in.hunk", "max.diffs.wrap"
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

  msg.base <- paste0(
    "Argument `%s` must be integer(1L) and not NA, an object produced ",
    "by `auto_context`, or \"auto\"."
  )
  if(
    !is.int.1L(env.to.check$context) &&
    !is(env.to.check$context,"diffObjAutoContext") &&
    !identical(env.to.check$context, "auto")
  )
    stop(simpleError(sprintf(msg.base, "context"), call=call))

  if(identical(env.to.check$context, "auto"))
    env.to.check$context <- auto_context()

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

