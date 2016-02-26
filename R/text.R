# Calculate how many lines of screen space are taken up by the diff hunks
#
# `disp.width` should be the available display width, this function computes
# the net real estate account for mode, padding, etc.

nlines <- function(txt, disp.width, mode) {
  use.ansi <- crayon::has_color()
  stopifnot(is.character(txt), all(!is.na(txt)))
  net.width <- calc_width_pad(disp.width, mode)
  nc_fun <- if(use.ansi) crayon::col_nchar else nchar
  pmax(1L, as.integer(ceiling(nc_fun(txt) / net.width)))
}
# Simple text manip functions

chr_trim <- function(text, width) {
  stopifnot(all(width > 2L))
  ifelse(
    nchar(text) > width,
    paste0(substr(text, 1L, width - 2L), ".."),
    text
  )
}
rpad <- function(text, width, pad.chr=" ") {
  use.ansi <- crayon::has_color()
  stopifnot(is.character(pad.chr), length(pad.chr) == 1L, nchar(pad.chr) == 1L)
  nchar_fun <- if(use.ansi) crayon::col_nchar else nchar
  pad.count <- width - nchar_fun(text)
  pad.count[pad.count < 0L] <- 0L
  pad.chrs <- vapply(
    pad.count, function(x) paste0(rep(pad.chr, x), collapse=""), character(1L)
  )
  paste0(text, pad.chrs)
}
# trim and right pad

rpadt <- function(text, width, pad.chr=" ")
  rpad(chr_trim(text, width), width, pad.chr)

# Breaks long character vectors into vectors of length width
#
# Right pads them to full length if requested
#
# Returns a list of split vectors

wrap <- function(txt, width, pad=FALSE) {
  # Get rid of newlines (NOTE: how does this even work??? must be wrong)

  txt[!!nchar(txt)] <- unlist(strsplit(txt[!!nchar(txt)], "\n"))

  # If there are ansi escape sequences, account for them; either way, create
  # a vector of character positions after which we should split our character
  # vector

  use.ansi <- crayon::has_color()
  ss_fun <- if(use.ansi) crayon::col_substr else substr
  nc_fun <- if(use.ansi) crayon::col_nchar else nchar

  # Map each character to a length of 1 or zero depending on whether it is
  # part of an ANSI escape sequence or not

  res.l <- lapply(
    seq_along(txt),
    function(i) {
      nchars <- nc_fun(txt[[i]])
      if(!nchars) return("")

      split.end <- seq(
        from=width, by=width, length.out=ceiling(nchars / width)
      ) + 1L
      split.start <- split.end - width

      unlist(
        Map(ss_fun, rep(txt[[i]], length(split.start)), split.start, split.end)
      )
    }
  )
  if(!length(res.l)) res.l <- list(txt)
  if(pad) lapply(res.l, rpad, width=width) else res.l
}
# Add the +/- in front of a text line and color accordingly
#
# Input is expected to be a list with character vectors of length one or more,
# more when a vector is wrapped.  We use the `:` symbol to indicate the wrapping
#
# returns a list containing padded char vectors

sign_pad <- function(txt, pad, rev=FALSE) {
  use.ansi <- crayon::has_color()
  if(!length(txt)) return(txt)
  stopifnot(
    is.list(txt), all(vapply(txt, is.character, logical(1L))),
    !any(is.na(unlist(txt))),
    is.integer(pad), length(pad) == 1L || length(pad) == length(txt),
    all(pad %in% 1:3)
  )
  pads <- if(!rev) c("  ", "+ ", "- ") else c("  ", " +", " -")
  pad.ex <- if(!rev) ": " else " :"
  if(length(pad) == 1L) pad <- rep(pad, length(txt))

  lines <- vapply(txt, length, integer(1L))
  pad.out <- lapply(
    seq_along(txt),
    function(x) {
      len <- length(txt[[x]])
      if(!len) character(0L) else {
        color <- pad[[x]] > 1L
        extras <- rep(if(color) pad.ex else "  ", len - 1L)
        res <- c(pads[pad[[x]]], extras)
        if(use.ansi && color) {
          crayon:::style(res, if(pad[[x]] == 2L) "green" else "red")
        } else res
  } } )
  Map(paste0, if(rev) txt else pad.out, if(!rev) txt else pad.out)
}
