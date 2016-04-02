# Capture output of print/show/str; unfortunately doesn't have superb handling
# of errors during print/show call, though hopefully these are rare
#
# x is a quoted call to evaluate

capture <- function(x, capt.width, frame, err) {
  width.old <- getOption("width")
  on.exit(options(width=width.old))
  width <- max(capt.width, 20L)
  options(width=width)

  res <- try(obj.out <- capture.output(eval(x, frame)))
  if(inherits(res, "try-error"))
    err(
      "Failed attempting to get text representation of object: ",
      conditionMessage(attr(res, "condition"))
    )
  res
}
# DEPRECATED?

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
# capture normal prints, along with default prints to make sure that if we
# do try to wrap an atomic vector print it is very likely to be in a format
# we are familiar with and not affected by a non-default print method

capt_print <- function(target, current, settings, err, ...){
  dots <- list(...)
  frame <- settings@frame
  print.match <- try(
    match.call(
      get("print", envir=frame), as.call(c(list(quote(print), x=NULL), dots)),
      envir=frame
  ) )
  if(inherits(print.match, "try-error"))
    err("Unable to compose `print` call")

  names(print.match)[[2L]] <- ""
  tar.call <- cur.call <- print.match

  if(length(dots)) {
    tar.call[[2L]] <- settings@tar.exp
    cur.call[[2L]] <- settings@cur.exp
    settings@tar.banner <- deparse(tar.call)[[1L]]
    settings@cur.banner <- deparse(cur.call)[[1L]]
  } else {
    settings@tar.banner <- deparse(settings@tar.exp)[[1L]]
    settings@cur.banner <- deparse(settings@cur.exp)[[1L]]
  }
  tar.call[[2L]] <- target
  cur.call[[2L]] <- current

  tar.call.def <- tar.call
  cur.call.def <- cur.call
  tar.call.def[[1L]] <- cur.call.def[[1L]] <- base::print.default

  both.at <- is.atomic(current) && is.atomic(target)
  capt.width <- calc_width(settings@disp.width, settings@mode) - 2L
  cur.capt <- capture(cur.call, capt.width, frame, err)
  cur.capt.def <- if(both.at) capture(cur.call.def, capt.width, frame, err)
  tar.capt <- capture(tar.call, capt.width, frame)
  tar.capt.def <- if(both.at) capture(tar.call.def, capt.width, frame, err)

  use.header <- length(dim(target)) == 2L && length(dim(current)) == 2L

  diff <- line_diff(
    target, current, tar.capt, cur.capt, settings=settings,
    warn=TRUE, use.header=use.header
  )
  diff@tar.capt.def <- tar.capt.def
  diff@cur.capt.def <- cur.capt.def
  diff
}
# Tries various different `str` settings to get the best possible output

capt_str <- function(target, current, settings, err, ...){
  # Match original call and managed dots, in particular wrt to the
  # `max.level` arg
  dots <- list(...)
  frame <- settings@frame
  line.limit <- settings@line.limit
  if("object" %in% names(dots))
    err("You may not specify `object` as part of `...`")

  str.match <- try(
    match.call(
      str_tpl,
      call=as.call(c(list(quote(str), object=NULL), dots)), envir=frame
  ) )
  if(inherits(str.match, "try-error"))
    err("Unable to compose `str` call")

  names(str.match)[[2L]] <- ""

  # Utility function; defining in body so it has access to `err`

  eval_try <- function(match.list, index, envir)
    tryCatch(
      eval(match.list[[index]], envir=envir),
      error=function(e)
        err("Error evaluating `", index, "` arg: ", conditionMessage(e))
    )
  # Setup / process extra args

  auto.mode <- FALSE
  max.level.supplied <- FALSE
  if(
    max.level.pos <- match("max.level", names(str.match), nomatch=0L)
  ) {
    # max.level specified in call; check for special 'auto' case
    res <- eval_try(str.match, "max.level", settings@frame)
    if(identical(res, "auto")) {
      auto.mode <- TRUE
      str.match[["max.level"]] <- NA
    } else {
      max.level.supplied <- TRUE
    }
  } else {
    str.match[["max.level"]] <- NA
    auto.mode <- TRUE
    max.level.pos <- length(str.match)
    max.level.supplied <- FALSE
  }
  # Was wrap specified in strict width mode?

  wrap <- FALSE
  if("strict.width" %in% names(str.match)) {
    res <- eval_try(str.match, "strict.width", settings@frame)
    wrap <- is.character(res) && length(res) == 1L && !is.na(res) &&
      nzchar(res) && identical(res, substr("wrap", 1L, nchar(res)))
  }
  if(auto.mode) {
    msg <-
      "Specifying `%s` may cause `str` output level folding to be incorrect"
    if("comp.str" %in% names(str.match)) warning(sprintf(msg, "comp.str"))
    if("indent.str" %in% names(str.match)) warning(sprintf(msg, "indent.str"))
  }
  # don't want to evaluate target and current more than once, so can't eval
  # tar.exp/cur.exp, so instead run call with actual object

  tar.call <- cur.call <- str.match
  tar.call[[2L]] <- target
  cur.call[[2L]] <- current

  # Run str

  capt.width <- calc_width_pad(settings@disp.width, settings@mode)
  has.diff <- has.diff.prev <- FALSE

  tar.capt <- strip_hz_control(
    capture(tar.call, capt.width, frame, err), stops=settings@tab.stops
  )
  tar.lvls <- str_levels(tar.capt, wrap=wrap)
  cur.capt <- strip_hz_control(
    capture(cur.call, capt.width, frame, err), stops=settings@tab.stops
  )
  cur.lvls <- str_levels(cur.capt, wrap=wrap)

  prev.lvl.hi <- lvl <- max.depth <- max(tar.lvls, cur.lvls)
  prev.lvl.lo <- 0L
  first.loop <- TRUE
  safety <- 0L
  warn <- TRUE

  repeat{
    if((safety <- safety + 1L) > max.depth && !first.loop)
      stop(
        "Logic Error: exceeded list depth when comparing structures; contact ",
        "maintainer."
      )
    tar.str <- tar.capt[tar.lvls <= lvl]
    cur.str <- cur.capt[cur.lvls <= lvl]

    diff.obj <- line_diff(
      target, current, tar.str, cur.str, settings=settings, warn=warn,
      strip=FALSE
    )
    diffs.str <- diff.obj@diffs

    if(diffs.str$hit.diffs.max) warn <- FALSE
    has.diff <- any(
      !vapply(
        unlist(diffs.str$hunks, recursive=FALSE), "[[", logical(1L), "context"
    ) )
    if(first.loop) {
      diff.obj.first <- diff.obj
      first.loop <- FALSE

      # If there are no differences reducing levels isn't going to help to
      # find one; additionally, if not in auto.mode we should not be going
      # through this process

      if(!has.diff || !auto.mode) break
    }
    if(line.limit[[1L]] < 1L) break

    line.len <-
      diff_line_len(diffs.str$hunks, settings=settings)

    # We need a higher level if we don't have diffs

    if(!has.diff && prev.lvl.hi - lvl > 1L) {
      prev.lvl.lo <- lvl
      lvl <- lvl + as.integer((prev.lvl.hi - lvl) / 2)
      tar.call[[max.level.pos]] <- lvl
      cur.call[[max.level.pos]] <- lvl
      next
    } else if(!has.diff) {
      diff.obj <- diff.obj.first
      break
    }
    # If we have diffs, need to check whether we should try to reduce lines
    # to get under line limit

    if(line.len <= line.limit[[1L]]) {
      # We fit, nothing else to do
      break
    }
    if(lvl - prev.lvl.lo > 1L) {
      prev.lvl.hi <- lvl
      lvl <- lvl - as.integer((lvl - prev.lvl.lo) / 2)
      tar.call[[max.level.pos]] <- lvl
      cur.call[[max.level.pos]] <- lvl
      next
    }
    # Couldn't get under limit, so use first run results

    diff.obj <- diff.obj.first
    lvl <- NULL
    break
  }
  diff.obj@diffs$diffs.max <- count_diffs(diff.obj@diffs$hunks)

  if(auto.mode) {
    str.match[[max.level.pos]] <- lvl
  } else if (!max.level.supplied) {
    str.match[[max.level.pos]] <- NULL
  }
  tar.call <- cur.call <- str.match
  tar.call[[2L]] <- settings@tar.exp
  cur.call[[2L]] <- settings@cur.exp
  if(is.null(settings@tar.banner))
    diff.obj@settings@tar.banner <- deparse(tar.call)[[1L]]
  if(is.null(settings@cur.banner))
    diff.obj@settings@cur.banner <- deparse(cur.call)[[1L]]

  diff.obj
}
capt_chr <- function(target, current, settings, err, ...){
  tar.capt <- if(!is.character(target)) as.character(target, ...) else target
  cur.capt <- if(!is.character(current)) as.character(current, ...) else current
  line_diff(target, current, tar.capt, cur.capt, settings=settings)
}
capt_deparse <- function(target, current, settings, err, ...){
  tar.capt <- deparse(target, ...)
  cur.capt <- deparse(current, ...)
  line_diff(target, current, tar.capt, cur.capt, settings=settings)
}
