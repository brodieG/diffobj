
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
