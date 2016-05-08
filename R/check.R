is.less_flags <-
  function(x) is.chr.1L(x) && isTRUE(grepl("^[[:alpha:]]*$", x))

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
# requires a value to be a scalar character and match one of the provided
# options

string_in <- function(x, valid.x) is.chr.1L(x) && x %in% valid.x

# Simple validation functions

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
  stopifnot(is(palette, "StylePalette"))
  stopifnot(isTRUE(param %in% c("brightness", "color.mode")))
  valid.formats <- dimnames(palette@data)$format
  valid.params <- dimnames(palette@data)[[param]]

  if(!is.character(x) || anyNA(x))
    paste0("Argument `", param, "` must be character and not contain NAs")
  else if(!all(x %in% valid.params))
    paste0(
      "Argument `", param, "` may only contain values in `", dep(valid.params),
      "`"
    )
  else if(
    (length(x) > 1L && is.null(names(x))) ||
    (!is.null(names(x)) && !"" %in% names(x)) ||
    !all(names(x) %in% valid.formats)
  )
    paste0(
      "Argument `", param, "` must have names if it has length > 1, and those ",
      "names must include at least an empty name `\"\"` as well as names only ",
      "from `", dep(valid.formats), "`."
    )
  else TRUE
}
# Checks common arguments across functions

check_args <- function(
  call, tar.exp, cur.exp, mode, context, line.limit, format, brightness,
  color.mode, pager, ignore.white.space, max.diffs, align.threshold, disp.width,
  hunk.limit, convert.hz.white.space, tab.stops, style, palette.of.styles,
  frame, tar.banner, cur.banner
) {
  err <- make_err_fun(call)

  # Check display width

  if(is.null(disp.width)) disp.width <- getOption("width")
  if(is.null(disp.width)) disp.width <- 80L
  if(!is.int.1L(disp.width) || disp.width < 1L)
    err(
      "Argument `disp.width` must be integer(1L) and positive, or NULL."
    )

  # Check context

  msg.base <- paste0(
    "Argument `%s` must be integer(1L) and not NA, an object produced ",
    "by `auto_context`, or \"auto\"."
  )
  if(
    !is.int.1L(context) && !is(context,"AutoContext") &&
    !identical(context, "auto")
  )
    err(sprintf(msg.base, "context"))

  if(!is(context, "AutoContext")) {
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

  # Tab stops

  tab.stops <- as.integer(tab.stops)
  if(
    !is.integer(tab.stops) || !length(tab.stops) >= 1L || anyNA(tab.stops) ||
    !all(tab.stops > 0L)
  )
    stop(
      "Argument `tab.stops` must be integer containing at least one value and ",
      "with all values strictly positive"
    )
  # Limit vars

  hunk.limit <- check_limit(hunk.limit)
  if(!is.integer(hunk.limit)) err(sprintf(hunk.limit, "hunk.limit", "."))
  if(!is.integer(line.limit <- check_limit(line.limit)))
    err(
      sprintf(
        line.limit, "line.limit",
        ", or \"auto\" or the result of calling `auto_line_limit`"
    ) )
  # check T F args

  TF.vars <- c("ignore.white.space", "convert.hz.white.space")
  msg.base <- "Argument `%s` must be TRUE or FALSE."
  for(x in TF.vars) if(!is.TF(get(x, inherits=FALSE))) err(sprintf(msg.base, x))

  # int 1L vars

  msg.base <- "Argument `%s` must be integer(1L) and not NA."
  if(!is.int.1L(max.diffs)) err(sprintf(msg.base, "max.diffs"))
  max.diffs <- as.integer(max.diffs)

  # char or NULL vars

  chr1LorNULL.vars <- c("tar.banner", "cur.banner")
  msg.base <- "Argument `%s` must be character(1L) and not NA, or NULL"
  for(x in chr1LorNULL.vars) {
    y <- get(x, inherits=FALSE)
    if(!is.chr.1L(y) && !is.null(y)) err(sprintf(msg.base, x))
  }
  # 0-1 vars

  if(
    !is.numeric(align.threshold) || length(align.threshold) != 1L ||
    !align.threshold >= 0 || !align.threshold <= 1
  )
    err("Argument `align.threshold` must be between 0 and 1")

  # style

  if(!is(style, "Style") && !string_in(style, "auto"))
    err("Argument `style` must be \"auto\" or a `Style` object.")

  # pager

  valid.pagers <- c("auto", "off")
  if(!is(pager, "Pager") && !string_in(pager, valid.pagers))
    err(
      "Argument `pager` must be one of `", dep(valid.pagers),
      "` or a `Pager` object."
    )
  if(!is(pager, "Pager") && string_in(pager, "off"))
    pager <- PagerOff()

  # palette and arguments that reference palette dimensions

  if(is.null(palette.of.styles)) palette.of.styles <- StylePalette()
  if(!is(palette.of.styles, "StylePalette"))
    err("Argument `palette.of.styles` must be a `StylePalette` object.")

  palette.params <- c("brightness", "color.mode")
  for(x in palette.params)
    if(
      !isTRUE(
        msg <- is.valid.palette.param(
          get(x, inherits=FALSE), x, palette.of.styles
      ) )
    ) err(msg)

  # Figure out whether pager is allowable or not; note that "auto" pager just
  # means let the pager that comes built into the style be the pager

  if(!is(pager, "Pager")) {
    pager <- if(pager == "auto" && interactive() && !in_knitr()) {
      "auto"
    } else PagerOff()
  }
  # format; decide what format to use

  if(!is(style, "Style") && string_in(style, "auto")) {
    if(!is.chr.1L(format))
      err("Argument `format` must be character(1L) and not NA")
    valid.formats <- c("auto", dimnames(palette.of.styles@data)$format)
    if(!format %in% valid.formats)
      err("Argument `format` must be one of `", dep(valid.formats) , "`.")
    if(format == "auto") {
      clrs <- crayon::num_colors()
      if(!is.int.1L(clrs))
        err(
          "Logic Error: unexpected return from `crayon::num_colors()`; ",
          "contact maintainer."
        )
      # No recognized color alternatives, try to use HTML if we can

      format <- if(!clrs %in% c(8, 256) || in_knitr()) {
        if(in_knitr() || interactive()) {
          "html"
        } else {
          "raw"
        }
      } else if (clrs == 8) {
        "ansi8"
      } else if (clrs == 256) {
        "ansi256"
      } else stop("Logic error: unhandled format; contact maintainer.")
    }
    style <- palette.of.styles[[
      format, get_pal_par(format, brightness), get_pal_par(format, color.mode)
    ]]
  } else stop("Logic Error: unexpected style state; contact maintainer.")

  # Attach specific pager if it was requested generated; if "auto" just let the
  # existing pager on the style be

  if(is(pager, "Pager")) style@pager <- pager
  else if(pager != "auto")
    stop("Logic Error: Unexpected pager state; contact maintainer.")

  # instantiate settings object

  etc <- new(
    "Settings", mode=val.modes[[which(mode.eq)]], context=context,
    line.limit=line.limit, ignore.white.space=ignore.white.space,
    max.diffs=max.diffs, align.threshold=align.threshold, disp.width=disp.width,
    hunk.limit=hunk.limit, convert.hz.white.space=convert.hz.white.space,
    tab.stops=tab.stops, style=style, frame=frame,
    tar.exp=tar.exp, cur.exp=cur.exp
  )
  etc
}
