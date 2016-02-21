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
# parent env of 'check_args'.

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
  } else {
    limit <- as.integer(limit)
    if(length(limit) == 1L) limit <- rep(limit, 2L)
    assign(type, limit, pos=-2L, inherit=FALSE)
  }
  NULL
}
# Checks common arguments across functions
#
# Note: this will modify the value of some of the arguments in the parent
# environment

check_args <- function() {
  args <- c(
    "line.limit", "context", "width", "hunk.limit", "mode", "use.ansi",
    "white.space"
  )
  call <- sys.call(-1L)
  vals <- try(mget(args, envir=as.environment(-1L), inherits=FALSE))
  if(inherits(vals, "try-error"))
    stop(
      simpleError(
        "Logic Error: unexpected missing argument; contact maintainer.",
        call=call
    ) )
  # check modes

  val.modes <- c("context", "unified", "sidebyside")
  if(
    !is.character(val$mode) || length(val$mode) != 1L || is.na(val$mode) ||
    !val$mode %in% val.modes
  ) {
    msg <- paste0(
      "Argument `mode` must be character(1L) and in `", deparse(val.modes), "`."
    )
    stop(simpleError(msg, call=call))
  }
  # check limits (has side effects)

  limits <- c("line.limit", "hunk.limit")
  Map(check_limit, vals[limits], limits)

  # check integer 1L args

  msg <- "Argument `%s` must be integer(1L) and not NA."

  if(!is.int.1L(vals$context)) stop(sprintf(msg, "context"), call=call)
  if(!is.int.1L(vals$width)) stop(sprintf(msg, "width"), call=call)

  # check T F args

  msg <- "Argument `%s` must be TRUE or FALSE"

  if(!is.TF(vals$use.nsi)) stop(sprintf(msg, "use.ansi"), call=call)
  if(!is.TF(vals$white.space)) stop(sprintf(msg, "white.space"), call=call)
}

is.int.1L <- function(x)
  is.numeric(x) && length(x) == 1L && !is.na(x) && x ==  round(x) &&
  is.finite(x)

is.TF <- function(x) isTRUE(x) || identical(x, FALSE)

is.chr1 <- function(x) is.character(x) && length(x) == 1L && !is.na(x)

is.valid_con <- function(x, file.name=NULL, readable=NA, writeable=NA) {
  if(!is.null(file.name) && !is.chr1(file.name))
      stop("Argument `file.name` must be NULL or a one length character vector.")
  if(!is.lgl.1L(readable) || !is.lgl.1L(writeable))
    stop("Arguments `readable` and `writeable` must be logical(1L)")

  # Basic checks

  if(!inherits(x, c("file", "connection")))
    return("must inherit from \"file\" and \"connection\"")
  if(!is.integer(x)) return("must be an integer")
  if(inherits(try(x.chr <- as.character(x)), "try-error"))
    return("cannot retrieve connection name to test")
  cons <- showConnections(all=TRUE)
  if(!isTRUE(x.chr %in% rownames(cons)))
    return("connection does not exist in `showConnections`")

  # Check r/w status

  rw <- list(writeable=writeable, readable=readable)
  rw.s <- list(writeable="write", readable="read")
  for(i in names(rw))
    if(!is.na(rw[[i]]))
      if(
        (cons[x.chr, sprintf("can %s", rw.s[[i]])] == "yes") != rw[[i]]
      )
        return(
          cc(
            "connection is ", if(rw[[i]]) "not ", i, " but should ",
            if(!rw[[i]]) "not ", "be ", i
        ) )

  # Match file name

  if(!is.null(file.name)) {
    if(!identical(file.name, cons[x.chr, "description"]))
      return("file name does not match connection description")
  }
  return(TRUE)
}
is.open_con <- function(x, file=NULL, readable=NA, writeable=NA) {
  if(!isTRUE(msg <- is.valid_con(x, file, readable, writeable))) return(msg)
  if(!isOpen(x)) return("must be an open connection")
  return(TRUE)
}
