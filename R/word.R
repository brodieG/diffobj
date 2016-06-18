# Used to initialize the word difference index lists; represents a non matching
# result for use with `regmatches`

.word.diff.atom <- -1L
attr(.word.diff.atom, "match.length") <- -1L

# Try to use fancier word matching with vectors and matrices

.brack.pat <- "^ *\\[\\d+\\]"

# Determine if a string contains what appear to be standard index headers
#
# Returns index of elements in string that start with index headers.
# Note that it is permissible to have ouput that doesn't match brackets
# provided that it starts with brackets (e.g. attributes shown after break
# pattern)

find_brackets <- function(x) {
  stopifnot(is.character(x), all(!is.na(x)))
  matches <- regexpr(.brack.pat,  x)
  vals <- regmatches(x, matches)
  # the matching section must be uninterrupted starting from first line
  # and must have consisten formatting

  brackets <- which(cumsum(!nzchar(vals)) == 0L)
  vals.in.brk <- vals[brackets]
  nums.in.brk <- regmatches(vals.in.brk, regexpr("\\d+", vals.in.brk))

  if(
    length(brackets) && length(unique(nchar(vals.in.brk)) == 1L) &&
    length(unique(diff(as.integer(nums.in.brk)))) <= 1L
  ) {
    brackets
  } else integer(0L)
}
# Matches syntactically valid R variable names

.reg.r.ident <- "(?:\\.[[:alpha:]]|[[:alpha:]])[[:alnum:]_.]*"

# Helper function when lining up word in a word diff to the lines they came from
#
# lines: is a list of what lines are in each hunk,
# cont: is a logical vector of same length as lines denoting whether a
#   particular value in lines is context or diff

reassign_lines <- function(lines, cont) {
  h.c <- length(cont)
  # Loop through all elements, assuming there are at least two as otherwise
  # nothing to do; `head` also guarantees always one more element left in
  # lines at any point in loop

  for(i in head(seq_along(lines), -1L)) {
    two.left <- h.c > i + 1L # at least two lines left
    if(cont[[i]]) {
      if(length(lines[[i]])) {
        if(
          two.left && length(lines[[i + 2L]]) &&
          min(lines[[i + 2L]]) == max(lines[[i]])
        ) {
          # Line spans a diff; assign the line to the diff hunk

          if(!length(lines[[i + 1L]]))
            lines[[i + 1L]] <- min(lines[[i + 2L]])
          lines[[i]] <- head(lines[[i]], -1L)
          lines[[i + 2L]] <- tail(lines[[i + 2L]], -1L)
        } else if (
          length(lines[[i + 1L]]) &&
          max(lines[[i]]) == min(lines[[i + 1]])
        ) {
          # Line shared between context and next diff

          lines[[i]] <- head(lines[[i]], -1L)
        }
        # If next diff hunk is still empty after all this, move a line over

        if(!length(lines[[i + 1L]]) && length(lines[[i]])) {
          lines[[i + 1L]] <- tail(lines[[i]], 1L)
          lines[[i]] <- head(lines[[i]], -1L)
        }
      }
    } else if(
      length(lines[[i]]) && length(lines[[i + 1]]) &&
      max(lines[[i]]) == min(lines[[i + 1L]])
    ) {
      # Non context hunk; handle case where non-context and next hunk overlap,
      # but prior context hunk doesn't

      lines[[i + 1L]] <- tail(lines[[i + 1L]], -1L)
    } else if(
      !length(lines[[i]]) && length(lines[[i + 1L]]) && (
        !two.left || !length(lines[[i + 2L]]) ||
        length(lines[[i + 1L]]) > 1L ||
        lines[[i + 1L]] != head(lines[[i + 2L]], 1L)
      )
    ) {
      # Empty first diff hunk, steal a line from next context hunk provided that
      # line is not shared with the next diff hunk

      lines[[i]] <- head(lines[[i + 1L]], 1L)
      lines[[i + 1L]] <- tail(lines[[i + 1L]], -1L)
    }
  }
  lines
}
# Helper Function for Mapping Word Diffs to Lines
#
# Used when we're doing a wrapped diff for atomic vectors.

word_to_line_map <- function(
  hunks, tar.dat, cur.dat, tar.ends, cur.ends, tar.ind, cur.ind
) {
  find_word_line <- function(h, pos, ends) {
    inds <- c(h$A, h$B)
    inds.lookup <- if(pos) inds[inds > 0] else abs(inds[inds < 0])
    ints <- c(1L, head(ends, -1L) + 1L)
    sort(unique(findInterval(inds.lookup, ints)))
  }
  tar.lines <- lapply(hunks, find_word_line, TRUE, tar.ends)
  cur.lines <- lapply(hunks, find_word_line, FALSE, cur.ends)

  # Since it is possible that a hunk is in the middle of a line, we need to
  # decide what to do with lines that show up in multiple hunks.  Rule is
  # that all overlapping lines should be assigned to the non-context hunk
  # in-between

  h.cont <- vapply(hunks, "[[", logical(1L), "context")
  tar.lines.p <- reassign_lines(tar.lines, h.cont)
  cur.lines.p <- reassign_lines(cur.lines, h.cont)

  # Now need to make sure that the context hunks are actually the same length
  # which need not be the case on a line basis.  To do so we must insert
  # blanks in the match hunks if they are unequal length.  We also need to
  # adjust the original character vectors so they have those blanks show up.

  fill_cont <- function(cont, len, i, max.i) {
    len.c <- length(cont)
    len.diff <- len - len.c
    NAs <- rep(NA, len.diff)
    if(i == 1L && len.c < 2L)           # spaces in front
      c(NAs, cont)
    else if(i == max.i || len.c < 2L)   # spaces in end
      c(cont, NAs)
    else {                      # spaces in middle
      c(head(cont, floor(len.c / 2)), NAs, tail(cont, -floor(len.c / 2)))
  } }
  h.c <- length(hunks)
  for(i in seq.int(h.c)) {
    if(h.cont[[i]]) {
      t.len <- length(tar.lines.p[[i]])
      c.len <- length(cur.lines.p[[i]])
      len.diff <- abs(t.len - c.len)
      if(t.len > c.len) {
        cur.lines.p[[i]] <- fill_cont(cur.lines.p[[i]], t.len, i, h.c)
      } else if (t.len < c.len){
        tar.lines.p[[i]] <- fill_cont(tar.lines.p[[i]], c.len, i, h.c)
  } } }
  # Augment the input vectors by the blanks we added; these blanks are
  # represented by NAs in our index vector so should be easy to do

  augment <- function(dat, lines, ind) {
    lines.u <- unlist(lines)
    lines.len <- length(lines.u)
    for(i in names(dat)) {
      i.vec <- dat[[i]]
      hd.ind <- seq_along(i.vec) < min(ind)
      tl.ind <- seq_along(i.vec) > max(ind)
      hd <- i.vec[hd.ind]
      tl <- i.vec[tl.ind]
      bod <- vector(typeof(i.vec), length(lines.u))
      bod[!is.na(lines.u)] <- i.vec
      if(i == "word.ind") {
        bod[is.na(lines.u)] <- list(.word.diff.atom)
      } else if (i == "fill") {
        # warning: this is also used/subverted for augmenting the original
        # indices so think before you change it
        bod[is.na(lines.u)] <- TRUE
      }
      dat[[i]] <- c(hd, bod, tl)
    }
    dat
  }
  browser()
  tar.dat <- augment(tar.dat, tar.lines.p, tar.ind)
  cur.dat <- augment(cur.dat, cur.lines.p, cur.ind)

  # Also need to augment the indices so we can re-insert properly; we subvert
  # the fill logic since that will make sure

  tar.ind.a <-
    augment(list(fill=!logical(length(tar.ind))), tar.lines.p, tar.ind)
  tar.ind.a.l <- unname(unlist(tar.ind.a))
  cur.ind.a <-
    augment(list(fill=!logical(length(cur.ind))), cur.lines.p, cur.ind)
  cur.ind.a.l <- unname(unlist(cur.ind.a))

  # Generate the final vectors to do the diffs on; these should be unique
  # and matching for the matches, and unique and mismatching for the
  # mismatches

  hunk_match <- function(i, l) rep(h.cont[i], length(l[[i]]))
  tar.match <- unlist(lapply(seq_along(h.cont), hunk_match, l=tar.lines.p))
  cur.match <- unlist(lapply(seq_along(h.cont), hunk_match, l=cur.lines.p))

  pos.nums <- sum(tar.match)
  if(pos.nums != length(unlist(cur.lines.p[h.cont])))
    stop("Logic Error: pos nums incorrect; contact maintainer")
  neg.nums <- sum(!tar.match, !cur.match)

  strings <-
    make_unique_strings(pos.nums + neg.nums, c(tar.dat$raw, cur.dat$raw))
  strings.pos <- strings[seq.int(pos.nums)]
  strings.neg <- tail(strings, neg.nums)
  if(neg.nums + pos.nums != length(strings))
    stop("Logic Error: num-string maping failed; contact maintainer")

  tar.dat$comp[tar.ind.a.l][tar.match] <- strings.pos
  cur.dat$comp[cur.ind.a.l][cur.match] <- strings.pos
  tar.dat$comp[tar.ind.a.l][!tar.match] <- head(strings.neg, sum(!tar.match))
  cur.dat$comp[cur.ind.a.l][!cur.match] <- tail(strings.neg, sum(!cur.match))
  list(tar.dat=tar.dat, cur.dat=cur.dat)
}
# Pull out mismatching words from the word regexec; helper functions

reg_pull <- function(ind, reg) {
  reg.out <- reg[ind]
  attr(reg.out, "match.length") <- attr(reg, "match.length")[ind]
  attr(reg.out, "useBytes") <- attr(reg, "useBytes")
  attr(reg.out, "word.count") <- length(reg)
  reg.out
}
# Generate the indices in each row and apply the pulling functions
# - reg list produced by `gregexpr` and such
# - ends length of each line in words
# - mismatch index of mismatching words
#

reg_apply <- function(reg, ends, mismatch) {
  if(!length(reg)) {
    reg
  } else {
    use.bytes <- attr(reg[[1L]], "useBytes") # assume useBytes value unchanging
    regs.fin <- reg
    buckets <- head(c(0L, ends) + 1L, -1L)
    mism.lines <- findInterval(mismatch, buckets)
    mism.lines.u <- unique(mism.lines)
    mtch.lines.u <- which(!seq_along(ends) %in% mism.lines.u )
    # These don't have any mismatches
    attr(.word.diff.atom, "useBytes") <- use.bytes
    regs.fin[mtch.lines.u] <-
      replicate(length(mtch.lines.u), .word.diff.atom, simplify=FALSE)
    # These do have mismatches, we need to split them up in list elements and
    # substract the starting index to identify position within each sub-list

    if(length(mism.lines.u)) {
      inds.msm <- Map(
        "-", unname(split(mismatch, mism.lines)), buckets[mism.lines.u] - 1L
      )
      regs.fin[mism.lines.u] <- Map(reg_pull, inds.msm, reg[mism.lines.u])
    }
    regs.fin
  }
}
# Modify `tar.dat` and `cur.dat` by generating `regmatches` indices for the
# words that are different
#
# If `diff.mode` is "wrap", then line up lines based on the word matches and
# mismatches contained there-in.  This is done by generating new strings
# that match or don't depending on the word contents, and then passing those
# back as the `comp` component of the `tar.dat` and `cur.dat` returned.  The
# subsequent line diff will cause the relevant lines to be lined up.
#
# Note that in "word" mode the returned values may be longer than the input ones
# as it may be necessary to add lines to get things to match-up.  Added lines
# are indicated by TRUE values in teh `fill` component of the `*.dat` return
# values
#
# `match.quotes` will make "words" starting and ending with quotes; it should
# only be used with atomic character vectors or possibly deparsed objects.

diff_word2 <- function(
  tar.dat, cur.dat, tar.ind, cur.ind, etc, match.quotes=FALSE, diff.mode,
  warn=TRUE
) {
  stopifnot(
    is.TF(match.quotes), is.TF(warn)
    # isTRUE(valid_dat(tar.dat)), isTRUE(valid_dat(cur.dat)) # too expensive
  )
  # Compute the char by char diffs for each line

  reg <- paste0(
    # grab leading spaces for each word; these will be stripped before actual
    # word diff, but we want them to be part of mismatch so they are removed
    # when we construct the equal strings as that allows better matching b/w
    # strings with differences removed; could do trailing spaces instead
    "\\s*(?:",
    # Some attempt at matching R identifiers; note we explicitly chose not to
    # match `.` or `..`, etc, since those could easily be punctuation
    sprintf("%s|", .reg.r.ident),
    # Not whitespaces that doesn't include quotes
    "[^ \"]+|",
    # Quoted phrases as structured in atomic character vectors
    if(match.quotes) "(?:(?<= )|(?<=^))\"(?:[^\"]|\\\")*?\"(?:(?= )|(?=$))|",
    # Other quoted phrases we might see in expressions or deparsed chr vecs,
    # this is a bit lazy currently b/c we're not forcing precise matching b/w
    # starting and ending delimiters
    "(?:(?<=[ ([,{])|(?<=^))\"(?:[^\"]|\\\"|\"(?=[^ ]))*?",
    "\"(?:(?=[ ,)\\]}])|(?=$))|",
    # Other otherwise 'illegal' quotes that couldn't be matched to one of the
    # known valid quote structures
    "\")"
  )
  tar.chr <- tar.dat$trim[tar.ind]
  cur.chr <- cur.dat$trim[cur.ind]
  tar.reg <- gregexpr(reg, tar.chr, perl=TRUE)
  cur.reg <- gregexpr(reg, cur.chr, perl=TRUE)

  tar.split <- regmatches(tar.chr, tar.reg)
  cur.split <- regmatches(cur.chr, cur.reg)

  # Collapse into one line if to do the diff across lines, but record
  # item counts so we can reconstitute the lines at the end

  tar.lens <- vapply(tar.split, length, integer(1L))
  cur.lens <- vapply(cur.split, length, integer(1L))

  tar.unsplit <- unlist(tar.split)
  cur.unsplit <- unlist(cur.split)
  if(is.null(tar.unsplit)) tar.unsplit <- character(0L)
  if(is.null(cur.unsplit)) cur.unsplit <- character(0L)

  # Remove the leading spaces we grabbed for each word

  tar.unsplit <- sub("^\\s*", "", tar.unsplit)
  cur.unsplit <- sub("^\\s*", "", cur.unsplit)

  # Run the word diff as a line diff configured in a manner compatible for the
  # word diff

  etc@line.limit <- etc@hunk.limit <- etc@context <- -1L
  etc@mode <- "context"

  diffs <- char_diff(
    tar.unsplit, cur.unsplit, etc=etc, diff.mode=diff.mode, warn=warn
  )
  # Need to figure out which elements match, and which ones do not

  hunks.flat <- diffs$hunks
  tar.mism <- unlist(lapply(hunks.flat, function(x) if(!x$context) x$A))
  cur.mism <- abs(unlist(lapply(hunks.flat, function(x) if(!x$context) x$B)))

  # Figure out which line each of these elements came from, and what index
  # in each of those lines they are; we use the recorded lengths in words of
  # each line to reconstruct this; also record original line length so we
  # can compute token ratios

  tar.ends <- cumsum(tar.lens)
  cur.ends <- cumsum(cur.lens)

  tar.dat$word.ind[tar.ind] <- reg_apply(tar.reg, tar.ends, tar.mism)
  cur.dat$word.ind[cur.ind] <- reg_apply(cur.reg, cur.ends, cur.mism)

  # If in wrap mode (which is really atomic mode), generate a spoofed
  # `comp` vector that will force the line diff to align in a way that respects
  # the word differences.  This is inefficient and round-about, but has the
  # huge benefit of allowing us to plug in the wrapped diff into our existing
  # line diff infrastructure

  if(diff.mode == "wrap") {
    word.line.mapped <- word_to_line_map(
      hunks.flat, tar.dat, cur.dat, tar.ends, cur.ends, tar.ind, cur.ind
    )
    tar.dat <- word.line.mapped$tar.dat
    cur.dat <- word.line.mapped$cur.dat
  }
  list(tar.dat=tar.dat, cur.dat=cur.dat, hit.diffs.max=diffs$hit.diffs.max)
}
# Make unique strings
#
# Makes gibberish strings that are 16 characters long, are unique, and don't
# overlap with `invalid`.  This allows us to generate strings we can use to
# cause a specific diff outcome.
#
# n: how long the character vector should be
# invalid: what values cannot be contained in the returned values

make_unique_strings <- function(n, invalid) {
  pool <- c(
    letters, LETTERS, 0:9, "_", ".", "*", "+", "-", "=", "(", ")", "{",
    "}", "~", "`", "!", "@", "#", "$", "%", "^", "&", ";", ":", "<", ">", "?",
    ",", "/"
  )
  cols <- 16 # use 16 character samples, should be more than big enough
  dat <- matrix("", ncol=16, nrow=n)
  rows <- 1:n
  safety <- 0
  repeat {
    dat[rows, ] <-
      matrix(sample(pool, cols * length(rows), replace=TRUE), ncol=cols)
    dat.chr <- do.call(paste0, split(dat, col(dat)))
    rows <- which(duplicated(dat.chr) | dat.chr %in% invalid)
    if(!length(rows)) break
    if(safety <- safety + 1 > 100)
      stop(
        "Logic Error: unable to generate unique strings; this should be ",
        "incredibly rare as we are sampling from 10^31 elements, so try ",
        "again and if it happens again contact maintainer"
      )
  }
  dat.chr
}
# Apply line colors; returns a list with the A and B vectors colored,
# note that all hunks will be collapsed.
#
# Really only intended to be used for stuff that produces a single hunk

diff_color <- function(x, ins.fun, del.fun) {
  if(!is.diffs(x))
    stop("Logic Error: unexpected input; contact maintainer.")
  h.flat <- unlist(x$hunks, recursive=FALSE)
  # the & !logical(...) business is to ensure we get zero row matrices when
  # the id vector is length zero

  bind_hunks <- function(hunk, val)
    do.call(
      rbind,
      lapply(
        hunk,
        function(y)
          cbind(id=y[[val]], ctx=y$context & !logical(length(y[[val]])))
    ) )

  A.num <- bind_hunks(h.flat, "A")
  B.num <- bind_hunks(h.flat, "B")
  A.chr <- unlist(lapply(h.flat, "[[", "A.chr"))
  B.chr <- unlist(lapply(h.flat, "[[", "B.chr"))

  # The following contortions are to minimize number of calls to
  # `crayon_style`

  A.green <- which(A.num[, "id"] < 0 & !A.num[, "ctx"])
  A.red <- which(A.num[, "id"] > 0 & !A.num[, "ctx"])
  B.green <- which(B.num[, "id"] < 0 & !B.num[, "ctx"])
  B.red <- which(B.num[, "id"] > 0 & !B.num[, "ctx"])

  AB.green.in <- c(A.chr[A.green], B.chr[B.green])
  AB.red.in <- c(A.chr[A.red], B.chr[B.red])
  color.try <- try({
    AB.green <- ins.fun(AB.green.in)
    AB.red <- del.fun(AB.red.in)
    NULL
  })
  if(inherits(color.try, "try-error"))
    stop("Styling functions failed; see prior errors")
  if(
    !is.character(AB.green) || anyNA(AB.green) ||
    length(AB.green) != length(AB.green.in)
  )
    stop("Insert styling function produced unexpected output")

  # Make a version where the differences are replaced with blank strings; this
  # will then allow us to line up the hunk lines

  A.eq <- A.chr
  B.eq <- B.chr
  A.eq[c(A.green, A.red)] <- ""
  B.eq[c(B.green, B.red)] <- ""

  # Color the diffs

  A.chr[A.green] <- head(AB.green, length(A.green))
  A.chr[A.red] <- head(AB.red, length(A.red))
  B.chr[B.green] <- tail(AB.green, length(B.green))
  B.chr[B.red] <- tail(AB.red, length(B.red))

  list(A=A.chr, B=B.chr, A.eq=A.eq, B.eq=B.eq)
}
# Add word diff highlighting

word_color <- function(txt, inds, fun) {
  word.list <- regmatches(txt, inds)
  word.lens <- vapply(word.list, length, integer(1L))

  # remove leading space before coloring
  words.u <- unlist(word.list)
  words.u.trim.ind <- regexpr("\\S.*", words.u)
  words.u.trim <- regmatches(words.u, words.u.trim.ind)

  # color and re-insert back into space
  words.c.trim <- fun(words.u.trim)
  regmatches(words.u, words.u.trim.ind) <- words.c.trim

  # split back into original lines
  words.res <- vector("list", length(word.list))
  words.res[!!word.lens] <- split(
    words.u, rep(seq_along(word.lens), times=word.lens)
  )
  words.res[!word.lens] <- list(character(0L))
  regmatches(txt, inds) <- words.res
  txt
}
