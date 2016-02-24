# Capture output of print/show/str; unfortunately doesn't have superb handling
# of errors during print/show call, though hopefully these are rare

capt_call <- function(x, capt.width, frame) {
  width.old <- getOption("width")
  on.exit(options(width=width.old))
  width <- max(capt.width, 20L)
  options(width=width)

  res <- try(obj.out <- capture.output(eval(x, frame)))
  if(inherits(res, "try-error"))
    stop(
      simpleError(
        paste0(
          "Failed attempting to get text representation of object: ",
          conditionMessage(attr(res, "condition"))
        ),
        call=sys.call(-1L)
    ) )
  res
}
obj_capt <- function(
  obj, width=getOption("width"), frame=parent.frame(), mode="print",
  max.level=0L, default=FALSE, ...
) {
  if(!is.numeric(width) || length(width) != 1L || is.na(width))
    stop("Argument `width` must be a one long numeric/integer.")
  if(
    !is.character(mode) || length(mode) != 1L || is.na(mode) ||
    !mode %in% c("print", "str")
  )
    stop("Argument `mode` must be one of \"print\" or \"str\"")
  # note this forces eval, which is needed
  if(!is.environment(frame))
    stop("Argument `frame` must be an environment")
  if(
    !is.na(max.level) && (
      !is.numeric(max.level) || length(max.level) != 1L ||  max.level < 0
    )
  )
    stop("Argument `max.level` must be integer(1L) and positive")

  max.level <- as.integer(max.level)
  width.old <- getOption("width")
  on.exit(options(width=width.old))
  width <- max(width, 10L)
  options(width=width)

  res <- try({
    extra <- NULL
    fun <- if(identical(mode, "print")) {
      if(isS4(obj)) quote(show) else quote(print)
    } else if(identical(mode, "str")) {
      extra <- list(max.level=max.level)
      quote(str)
    } else stop("Logic Error: unexpected mode; contact maintainer.")
    call <- as.call(c(list(fun, obj, `...`=...), extra))
  })
  res <- try(obj.out <- capture.output(eval(call, frame)))
  if(inherits(res, "try-error"))
    stop("Failed attempting to get text representation of object")

  options(width=width.old)
  on.exit(NULL)

  # remove trailing spaces; shouldn't have to do it but doing it since legacy
  # tests remove them and PITA to update those

  obj.out <- sub("\\s*$", "", obj.out)
  obj.out
}
