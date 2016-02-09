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
  stopifnot(is.character(pad.chr), length(pad.chr) == 1L, nchar(pad.chr) == 1L)
  pad.count <- width - nchar(text)
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
# Returns a list of split vectors

wrap <- function(txt, width, ansi_escape) {
  # Get rid of newlines

  txt <- unlist(strsplit(txt, "\n"))

  # If there are ansi escape sequences, account for them; either way, create
  # a vector of character positions after which we should split our character
  # vector

  esc.loc <- if(ansi_escape) {
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

      substr(
        rep(txt[[i]], length(split.locs) + 1L),
        c(1L, split.locs + 1L), c(split.locs, nchars)
    ) }
  )
}
