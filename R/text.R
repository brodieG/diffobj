# Simple text manip functions

chr_trim <- function(text, width) {
  stopifnot(all(width > 2L))
  ifelse(
    nchar(text) > width,
    paste0(substr(text, 1L, width - 2L), ".."),
    text
  )
}
rpad <- function(text, width, pad.chr=" ", use.ansi=TRUE) {
  stopifnot(is.character(pad.chr), length(pad.chr) == 1L, nchar(pad.chr) == 1L)
  nchar_fun <- if(use.ansi) ansi_style_nchar else nchar
  pad.count <- width - nchar_fun(text)
  pad.count[pad.count < 0L] <- L
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

wrap <- function(txt, width, use.ansi, pad=FALSE) {
  # Get rid of newlines

  if(!length(txt)) return(list(txt))
  txt <- unlist(strsplit(txt, "\n"))

  # If there are ansi escape sequences, account for them; either way, create
  # a vector of character positions after which we should split our character
  # vector

  esc.loc <- if(use.ansi) {
    gregexpr(.ansistyle_ansi_regex, txt)
  } else {
    # Equivalent to no-match
    replicate(length(txt), structure(-1L, match.length=-1L), simplify=FALSE)
  }
  # Map each character to a length of 1 or zero depending on whether it is
  # part of an ANSI escape sequence or not

  lapply(
    seq_along(txt),
    function(i) {
      nchars <- nchar(txt[[i]])
      if(!nchars) return("")
      char.len <- rep(1L, nchars)
      esi <- esc.loc[[i]]
      zero.len <- unlist(
        Map(
          function(x, y) if(x > 0L) seq_len(y) + x - 1L else 0L,
          esi, attr(esi, "match.length")
      ) )
      char.len[zero.len] <- 0L

      # Determine locations to split at; we are greedy, grabbing as many zero
      # width chars as we can

      cum.len <- cumsum(char.len)
      max.len <- tail(cum.len, 1L)

      if(max.len <= width) return(txt[[i]])

      split.chr.pos <- seq(
        from=width, by=width, length.out=ceiling(max.len / width)
      )
      # match the locations in reversed string (reverse so `match` matches)
      # last instance of a length

      split.locs <-
        length(cum.len) - c(na.omit(match(split.chr.pos, rev(cum.len)))) + 1L
      split.locs <- split.locs[which(split.locs < nchars)]

      res <- substr(
        rep(txt[[i]], length(split.locs) + 1L),
        c(1L, split.locs + 1L), c(split.locs, nchars)
      )
      if(pad) rpad(res, width, use.ansi=use.ansi) else res
    }
  )
}
# Add the +/- in front of a text line and color accordingly
#
# Input is expected to be a list with character vectors of length one or more,
# more when a vector is wrapped.  We use the `:` symbol to indicate the wrapping
#
# returns a list containing padded char vectors

sign_pad <- function(txt, pad, rev=FALSE, use.ansi) {
  if(!length(txt)) return(txt)
  stopifnot(
    is.list(txt), all(vapply(txt, is.character, logical(1L))),
    !any(is.na(unlist(txt))),
    is.character(pad), length(pad) == 1L, pad %in% c("+ ", "- ", "  "),
    isTRUE(use.ansi) || identical(use.ansi, FALSE)
  )
  neut <- pad == "  "
  pad.extra <- "  "
  if(rev && !neut) {
    pad.chr <- paste0(rev(strsplit(pad, "")[[1L]]), collapse="")
    pad.extra <- " :"
  } else {
    pad.chr <- pad
    pad.extra <- ": "
  }
  lines <- vapply(txt, length, integer(1L))
  pad.out <- lapply(
    lines,
    function(x) if(x) c(pad.chr, rep(pad.extra, x - 1L)) else character(0L)
  )
  if(use.ansi && !neut) {
    pad.out <- lapply(
      pad.out, ansi_style,
      if(pad == "- ") "red" else "green", use.style=use.ansi
    )
  }
  Map(paste0, if(rev) txt else pad.out, if(!rev) txt else pad.out)
}
