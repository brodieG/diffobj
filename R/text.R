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

wrap <- function(txt, width, use.ansi, pad=FALSE) {
  # Get rid of newlines

  txt[!!nchar(txt)] <- unlist(strsplit(txt[!!nchar(txt)], "\n"))

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

  res.l <- lapply(
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
      res
    }
  )
  if(!length(res.l)) res.l <- list(txt)
  if(pad) lapply(res.l, rpad, width=width, use.ansi=use.ansi) else res.l
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
    is.integer(pad), length(pad) == 1L || length(pad) == length(txt),
    all(pad %in% 1:3),  isTRUE(use.ansi) || identical(use.ansi, FALSE)
  )
  pads <- if(!rev) c("  ", "+ ", "- ") else c("  ", " +", " -")
  pad.ex <- if(!rev) ": " else " :"
  if(length(pad) == 1L) pad <- rep(pad, length(txt))

  lines <- vapply(txt, length, integer(1L))
  pad.out <- lapply(
    seq_along(txt),
    function(x) {
      len <- length(txt[[x]])
      color <- pad[[x]] > 1L
      extras <- if(len)
        rep(if(color) pad.ex else "  ", len - 1L) else character(0L)
      res <- c(pads[pad[[x]]], extras)
      if(use.ansi && color) {
        ansi_style(
          res, if(pad[[x]] == 2L) "green" else "red",
          use.style=use.ansi
        )
      } else res
    }
  )
  Map(paste0, if(rev) txt else pad.out, if(!rev) txt else pad.out)
}
