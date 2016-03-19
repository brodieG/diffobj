# Remap crayon funs so they show up in profiling

crayon_nchar <- crayon::col_nchar
crayon_substr <- crayon::col_substr
crayon_style <- crayon::style
crayon_hascolor <- crayon::has_color
crayon_split <- crayon::col_strsplit
crayon_strip <- crayon::strip_style

# borrowed from crayon, will lobby to get it exported
ansi_regex <- paste0("(?:(?:\\x{001b}\\[)|\\x{009b})",
                     "(?:(?:[0-9]{1,3})?(?:(?:;[0-9]{0,3})*)?[A-M|f-m])",
                     "|\\x{001b}[A-M]")

# Align to vectors based on the values of two other vectors
#
# This will group mismatches with the first match preceding them.  There will
# be as many groups as there are matches.  The matches are assessed on
# `A.eq` and `B.eq`

align_eq <- function(A, B, A.eq, B.eq, ignore.white.space=FALSE) {
  stopifnot(
    is.character(A.eq), is.character(B.eq), !anyNA(A.eq), !anyNA(B.eq),
    length(A) == length(A.eq), length(B) == length(B.eq),
    is.TF(ignore.white.space)
  )
  # to simplify logic, force the first value to match

  A.l <- c("<match>", A)
  B.l <- c("<match>", B)

  A.eq <- c("<match>", if(ignore.white.space) gsub("\\s+", "", A.eq) else A.eq)
  B.eq <- c("<match>", if(ignore.white.space) gsub("\\s+", "", B.eq) else B.eq)

  align <- match(A.eq, B.eq, nomatch=0L)
  align[align < cummax(align)] <- 0L
  A.splits <- cumsum(!!align)

  B.align <- c(align[!!align], length(B.eq) + 1L)
  B.splits <-
    rep(seq_len(length(B.align) - 1L), tail(B.align, -1L) - head(B.align, -1L))

  # now chunk; we need to drop the first match from splits since we added it

  A.chunks <- unname(split(A.l, A.splits))
  B.chunks <- unname(split(B.l, B.splits))
  A.chunks[[1L]] <- tail(A.chunks[[1L]], -1L)
  B.chunks[[1L]] <- tail(B.chunks[[1L]], -1L)

  if(length(A.chunks) != length(B.chunks))
    stop("Logic Error: aligned chunks unequal length; contact maintainer.")

  list(A=A.chunks, B=B.chunks)
}
# if last char matches, repeat, but only if not in use.ansi mode
#
# needed b/c col_strsplit behaves differently than strisplit when delimiter
# is at end.  Will break if you pass a regex reserved character as pad

pad_end <- function(x, pad, use.ansi) {
  stopifnot(
    is.character(x), !anyNA(x), is.character(pad), length(pad) == 1L,
    !is.na(pad), nchar(pad) == 1L, is.TF(use.ansi)
  )
  if(!use.ansi || packageVersion("crayon") >= "1.3.2")
    sub(paste0(pad, "$"), paste0(pad, pad), x) else x
}
# Calculate how many lines of screen space are taken up by the diff hunks
#
# `disp.width` should be the available display width, this function computes
# the net real estate account for mode, padding, etc.

nlines <- function(txt, disp.width, mode) {
  use.ansi <- crayon_hascolor()
  # stopifnot(is.character(txt), all(!is.na(txt)))
  capt.width <- calc_width_pad(disp.width, mode)
  if(use.ansi) txt <- crayon_strip(txt)
  pmax(1L, as.integer(ceiling(nchar(txt) / capt.width)))
}
# Gets rid of tabs and carriage returns
#
# Assumes each line is one screen line
# @param stops may be a single positive integer value, or a vector of values
#   whereby the last value will be repeated as many times as necessary

strip_hz_c_int <- function(txt, stops, use.ansi, nc_fun, sub_fun, split_fun) {

  # remove trailing and leading CRs (need to record if trailing remains to add
  # back at end? no, not really since by structure next thing must be a newline

  w.chr <- nzchar(txt)  # corner case with strsplit and zero length strings
  txt <- gsub("^\r+|\r+$", "", txt)
  has.tabs <- grep("\t", txt, fixed=TRUE)
  has.crs <- grep("\r", txt, fixed=TRUE)
  txt.s <- as.list(txt)
  txt.s[has.crs] <- if(!any(has.crs)) list() else split_fun(txt[has.crs], "\r+")

  # Assume \r resets tab stops as it would on a type writer; so now need to
  # generate the set maximum set of possible tab stops; approximate here by
  # using largest stop

  if(length(has.tabs)) {
    max.stop <- max(stops)
    width.w.tabs <- max(
      vapply(
        txt.s[has.tabs], function(x) {
          # add number of chars and number of tabs times max tab length
          sum(
            nc_fun(x) + (
              vapply(strsplit(x, "\t"), length, integer(1L)) +
              grepl("\t$", x) - 1L
            ) * max.stop
          )
        }, integer(1L)
    ) )
    extra.chars <- width.w.tabs - sum(stops)
    extra.stops <- ceiling(extra.chars / tail(stops, 1L))
    stop.vec <- cumsum(c(stops, rep(tail(stops, 1L), extra.stops)))

    # For each line, assess effects of tabs

    txt.s[has.tabs] <- lapply(txt.s[has.tabs],
      function(x) {
        if(length(h.t <- grep("\t", x, fixed=T))) {
          # workaround for strsplit dropping trailing tabs
          x.t <- pad_end(x[h.t], "\t", use.ansi)
          x.s <- split_fun(x.t, "\t")

          # Now cycle through each line with tabs and replace them with
          # spaces

          res <- vapply(x.s,
            function(y) {
              topad <- head(y, -1L)
              rest <- tail(y, 1L)
              chrs <- nc_fun(topad)
              pads <- character(length(topad))
              txt.len <- 0L
              for(i in seq_along(topad)) {
                txt.len <- chrs[i] + txt.len
                tab.stop <- head(which(stop.vec > txt.len), 1L)
                if(!length(tab.stop))
                  stop(
                    "Logic Error: failed trying to find tab stop; contact ",
                    "maintainer"
                  )
                tab.len <- stop.vec[tab.stop]
                pads[i] <- paste0(rep(" ", tab.len - txt.len), collapse="")
                txt.len <- tab.len
              }
              paste0(paste0(topad, pads, collapse=""), rest)
            },
            character(1L)
          )
          x[h.t] <- res
        }
        x
  } ) }
  # Simulate the effect of \r by collapsing every \r separated element on top
  # of each other with some special handling for ansi escape seqs

  txt.fin <- txt.s
  txt.fin[has.crs] <- vapply(
    txt.s[has.crs],
    function(x) {
      if(length(x) > 1L) {
        chrs <- nc_fun(x)
        max.disp <- c(tail(rev(cummax(rev(chrs))), -1L), 0L)
        res <- paste0(rev(sub_fun(x, max.disp + 1L, chrs)), collapse="")
        # add back every ANSI esc sequence from last line to very end
        # to ensure that we leave in correct ANSI escaped state

        if(use.ansi && grepl(ansi_regex, res, perl=TRUE)) {
          res <- paste0(
            res,
            gsub(paste0(".*", ansi_regex, ".*"), "\\1", tail(x, 1L), perl=TRUE)
        ) }
        res
      } else x
    },
    character(1L)
  )
  # txt.fin should only have one long char vectors as elements
  if(!length(txt.fin)) txt else {
    # handle strsplit corner case where splitting empty string
    txt.fin[!nzchar(txt)] <- ""
    unlist(txt.fin)
  }
}
strip_hz_control <- function(txt, stops=8L) {
  # stopifnot(
  #   is.character(txt), !anyNA(txt),
  #   is.integer(stops), length(stops) >= 1L, !anyNA(stops), all(stops > 0L)
  # )
  if(any(grepl("\n", txt)))
    stop("Logic Error: input may not contain newlines; contact maintainer")
  use.ansi <- crayon_hascolor()
  has.ansi <- grepl(ansi_regex, txt, perl=TRUE) & use.ansi
  w.ansi <- which(has.ansi)
  wo.ansi <- which(!has.ansi)

  # since for the time being the crayon funs are a bit slow, only us them on
  # strings that are known to have ansi escape sequences

  res <- character(length(txt))
  res[w.ansi] <- strip_hz_c_int(
    txt[w.ansi], stops, use.ansi, crayon_nchar, crayon_substr, crayon_split
  )
  res[wo.ansi] <- strip_hz_c_int(
    txt[wo.ansi], stops, use.ansi, nchar, substr, strsplit
  )
  res
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
  use.ansi <- crayon_hascolor()
  stopifnot(is.character(pad.chr), length(pad.chr) == 1L, nchar(pad.chr) == 1L)
  nchar_fun <- if(use.ansi) crayon_nchar else nchar
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

wrap_int <- function(txt, width, sub_fun, nc_fun) {
  lapply(
    seq_along(txt),
    function(i) {
      nchars <- nc_fun(txt[[i]])
      split.end <- seq(
        from=width, by=width, length.out=ceiling(nchars / width)
      )
      split.start <- split.end - width + 1L
      sub_fun(rep(txt[[i]], length(split.start)), split.start, split.end)
  } )
}
wrap <- function(txt, width, pad=FALSE) {
  if(length(grep("\n", txt, fixed=TRUE)))
    stop("Logic error: wrap input contains newlines; contact maintainer.")

  # If there are ansi escape sequences, account for them; either way, create
  # a vector of character positions after which we should split our character
  # vector

  use.ansi <- crayon_hascolor()
  has.chars <- nzchar(txt)
  w.chars <- which(has.chars)
  wo.chars <- which(!has.chars)

  txt.sub <- txt[has.chars]
  w.ansi.log <- grepl(ansi_regex, txt.sub, perl=TRUE) & use.ansi
  w.ansi <- which(w.ansi.log)
  wo.ansi <- which(!w.ansi.log)

  # Wrap differently depending on whether contains ansi or not, exclude zero
  # length char elements

  res.l <- vector("list", length(txt))
  res.l[wo.chars] <- ""
  res.l[w.chars][w.ansi] <-
    wrap_int(txt.sub[w.ansi], width, crayon_substr, crayon_nchar)
  res.l[w.chars][wo.ansi] <-
    wrap_int(txt.sub[wo.ansi], width, substr, nchar)

  # pad if requested

  if(pad) lapply(res.l, rpad, width=width) else res.l
}
# Add the +/- in front of a text line and color accordingly
#
# Input is expected to be a list with character vectors of length one or more,
# more when a vector is wrapped.  We use the `:` symbol to indicate the wrapping
#
# returns a list containing padded char vectors

sign_pad <- function(txt, pad, rev=FALSE) {
  use.ansi <- crayon_hascolor()
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
          crayon_style(res, if(pad[[x]] == 2L) "green" else "red")
        } else res
  } } )
  Map(paste0, if(rev) txt else pad.out, if(!rev) txt else pad.out)
}
