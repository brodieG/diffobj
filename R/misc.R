# Common argument check functions; note that the `stop` message reports as the
# parent system call

check_linelimit <- function(line.limit) {
  if(
    !is.numeric(line.limit) || any(is.na(line.limit)) ||
    !length(line.limit) %in% 1:2 || any(line.limit) < 1L ||
    round(line.limit) != line.limit ||
    (length(line.limit) == 2L && diff(line.limit) > 0)
  ) {
    msg <- paste0(
      "Argument `line.limit` must be an integer vector of length 1 or 2 ",
      "with only strictly positive values and if length 2, with the first ",
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

# Functions copied over from `unitizer`, will be deleted for the most
# part

is.chr.vec <- function(x) is.character(x) && is.null(attributes(x))

is.chr1 <- function(x) is.character(x) && length(x) == 1L && !is.na(x)

is.TF <- function(x) isTRUE(x) || identical(x, FALSE)

is.lgl.1L <- function(x) is.logical(x) && length(x) == 1L

is.int.1L <- function(x)
  is.numeric(x) && length(x) == 1L && !any(is.na(x)) && all.equal(x, round(x))

is.context.out.vec <- function(x)
  is.numeric(x) && length(x) == 2L && !any(is.na(x)) && all(x > 0) &&
  x[1] >= x[2] && all.equal(round(x), x)

is.int.pos.1L <- function(x)
  is.numeric(x) && length(x) == 1L && !any(is.na(x)) &&
  all.equal(x, round(x)) && all(x > 0L)

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
