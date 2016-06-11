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

# Apply diff algorithm within lines
#
# For each line, splits into words, runs diffs, and colors them appropriately.
# For `across.lines=TRUE`, merges all lines into one and does the word diff on
# a single line to allow for the diff to look for matches across lines, though
# the result is then unwrapped back to the original lines.
#
# `match.quotes` will make "words" starting and ending with quotes; it should
# only be used if the objects are known to be attribute-less character vectors
# that are printed (as that is the only way we can know for sure how to match
# the quoted bits): note, we relaxed this constraint a little bit so that we
# can better match deparsed things and so on

diff_word <- function(
  tar.chr, cur.chr, etc, match.quotes=FALSE, diff.mode, warn=TRUE
) {
  stopifnot(
    is.character(tar.chr), is.character(cur.chr),
    all(!is.na(tar.chr)), all(!is.na(cur.chr)),
    is.TF(match.quotes), is.TF(warn)
  )
  # Compute the char by char diffs for each line

  reg <- paste0(
    # Some attempt at matching R identifiers; note we explicitly chose not to
    # match `.` or `..`, etc, since those could easily be punctuation
    sprintf("%s|", .reg.r.ident),
    # Not whitespaces that doesn't include quotes
    "[^ \"]+|",
    # Quoted phrases as structured in atomic character vectors
    if(match.quotes) "((?<= )|(?<=^))\"([^\"]|\\\")*?\"((?= )|(?=$))|",
    # Other quoted phrases we might see in expressions or deparsed chr vecs,
    # this is a bit lazy currently b/c we're not forcing precise matching b/w
    # starting and ending delimiters
    "((?<=[ ([,{])|(?<=^))\"([^\"]|\\\"|\"(?=[^ ]))*?\"((?=[ ,)\\]}])|(?=$))|",
    # Other otherwise 'illegal' quotes that couldn't be matched to one of the
    # known valid quote structures
    "\""
  )
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

  etc@line.limit <- etc@hunk.limit <- etc@context <- -1L
  etc@mode <- "context"
  diffs <- char_diff(
    tar.unsplit, cur.unsplit, etc=etc, diff.mode=diff.mode, warn=warn
  )
  # Color

  diff.colored <- diff_color(
    diffs, ins.fun=etc@style@funs@word.insert,
    del.fun=etc@style@funs@word.delete
  )
  tar.colored <- diff.colored$A
  cur.colored <- diff.colored$B
  tar.eq <- diff.colored$A.eq
  cur.eq <- diff.colored$B.eq

  # Reconstitute lines, but careful since some lines may be empty

  tar.fin <- tar.fin.eq <- replicate(length(tar.chr), character(0L))
  cur.fin <- cur.fin.eq <- replicate(length(cur.chr), character(0L))

  tar.w.len <- tar.lens[tar.lens > 0]
  cur.w.len <- cur.lens[cur.lens > 0]

  tar.fin[tar.lens > 0] <-
    split(tar.colored, rep(seq_along(tar.w.len), tar.w.len))
  cur.fin[cur.lens > 0] <-
    split(cur.colored, rep(seq_along(cur.w.len), cur.w.len))

  # These are with differences suppressed

  tar.fin.eq[tar.lens > 0] <-
    split(tar.eq, rep(seq_along(tar.w.len), tar.w.len))
  cur.fin.eq[cur.lens > 0] <-
    split(cur.eq, rep(seq_along(cur.w.len), cur.w.len))

  # Merge back into original

  tar.cpy <- tar.cpy.eq <- tar.chr
  cur.cpy <- cur.cpy.eq <- cur.chr
  if(length(tar.colored)) regmatches(tar.cpy, tar.reg) <- tar.fin
  if(length(cur.colored)) regmatches(cur.cpy, cur.reg) <- cur.fin
  if(length(tar.colored)) regmatches(tar.cpy.eq, tar.reg) <- tar.fin.eq
  if(length(cur.colored)) regmatches(cur.cpy.eq, cur.reg) <- cur.fin.eq

  # Compute token counts; this allows us to check how close to a match a line
  # with word differences is

  tar.toks <- vapply(tar.split, function(x) sum(nzchar(x)), integer(1L))
  cur.toks <- vapply(cur.split, function(x) sum(nzchar(x)), integer(1L))

  tar.toks.eq <- vapply(tar.fin.eq, function(x) sum(nzchar(x)), integer(1L))
  cur.toks.eq <- vapply(cur.fin.eq, function(x) sum(nzchar(x)), integer(1L))

  # Record info; a lot of the extra stuff is so we can then align lines in
  # hunks.  tar/cur.tok.ratio what proportion of the tokens in a line is
  # matched

  list(
    tar.chr=tar.cpy, cur.chr=cur.cpy,
    tar.eq.chr=tar.cpy.eq, cur.eq.chr=cur.cpy.eq,
    tar.tok.ratio=ifelse(tar.lens, tar.toks.eq / tar.lens, 0),
    cur.tok.ratio=ifelse(cur.lens, cur.toks.eq / cur.lens, 0),
    hit.diffs.max=diffs$hit.diffs.max
  )
}
# Helper function when lining up word in a word diff to the lines they came from
# lines is a list of what lines are in each hunk, cont is a vector of same
# length as lines denoting whether a particular value in lines is context or
# diff

reassign_lines <- function(lines, cont) {
  for(i in lines) {
    if(cont[[i]]) {
      if(hunk.count > i && length(lines[[i]])) {
        if(
          hunk.count > i + 1L && length(lines[[i + 2L]]) &&
          min(lines[[i + 2L]]) == max(lines[[i]])
        ) {
          # Line spans a diff; assign the line to the diff hunk

          if(!length(lines[[i + 1L]]))
            lines[[i + 1L]] <- max(lines[[i + 2L]])
          lines[[i]] <- head(lines[[i]], -1L)
          lines[[i + 2L]] <- tail(lines[[i]], -1L)
        } else if (
          length(lines[[i + 1L]]) &&
          max(lines[[i]]) == min(lines[[i + 1]])
        ) {
          # Line shared between context and next diff

          lines[[i]] <- head(lines[[i]], -1L)
  } } } }
  lines
}
# Helper Function for Mapping Word Diffs to Lines
#
# Used when we're doing a wrapped diff for atomic vectors.

word_to_line_map <- function(hunks, tar.dat, cur.dat, tar.ends, cur.ends) {
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

  pad_cont <- function(cont, len, i, max.i) {
    len.diff <- len - length(cont)
    NAs <- rep(NA, len.diff)
    if(i == 1L || len < 2L)     # spaces in front
      c(NAs, cont)
    else if(i == max.i)         # spaces in end
      c(cont, NAs)
    else {                      # spaces in middle
      c(head(cont, floor(len / 2)), NAs, tail(cont, -floor(len / 2)))
  } }
  h.c <- length(hunks)
  for(i in seq.int(h.c)) {
    if(h.cont[[i]]) {
      t.len <- length(tar.lines.p[[i]])
      c.len <- length(cur.lines.p[[i]])
      len.diff <- abs(t.len - c.len)
      if(t.len > c.len) {
        cur.lines.p[[i]] <- pad_cont(cur.lines.p[[i]], t.len, i, hunk.count)
      } else if (t.len < c.len){
        tar.lines.p[[i]] <- pad_cont(tar.lines.p[[i]], c.len, i, hunk.count)
  } } }
  # Augment the input vectors by the blanks we added; these blanks are
  # represented by NAs in our index vector so should be easy to do

  word.diff.atom <- -1L
  attr(word.diff.atom, "match.length") <- -1L

  augment <- function(dat, lines, ind) {
    lines.u <- unlist(lines)
    lines.len <- length(lines.u)
    for(i in names(dat)) {
      i.vec <- dat[[i]]
      hd <- i.vec[seq_along(i.vec) < min(ind)]
      tl <- i.vec[seq_along(i.vec) > max(ind)]
      bod <- vector(typeof(i.vec), length(ind))
      bod[!is.na(ind)] <- i.vec
      if(i == "word.diff") {
        bod[is.na(ind)] <- list(word.diff.atom)
      } else if (i == "pad") {
        bod[is.na(ind)] <- TRUE
  } } }
  tar.dat <- augment(tar.dat, tar.lines.p, tar.ind)
  cur.dat <- augment(cur.dat, cur.lines.p, cur.ind)

  # Generate the final vectors to do the diffs on; these should be unique
  # and matching for the matches, and unique and mismatching for the
  # mismatches

  tar.nums <- unlist(tar.lines.p)
  cur.nums <- unlist(cur.lines.p)
  nums <- unique(tar.nums, cur.nums)
  strings <- make_unique_strings(nums, c(tar.chr, cur.chr))
  pos.nums <- nums[nums > 0L]
  if(pos.nums != seq_along(pos.nums))
    stop("Logic Error: pos nums incorrect; contact maintainer")
  strings.pos <- strings[seq_along(pos.nums)]
  strings.neg <- tail(strings, -length(pos.nums))
  if(length(strings.pos) + length(strings.neg) != length(strings))
    stop("Logic Error: num-string maping failed; contact maintainer")
  tar.dat$comp[tar.ind][tar.nums > 0] <- strings.pos
  cur.dat$comp[cur.ind][cur.nums > 0] <- strings.pos
  tar.dat$comp[tar.ind][tar.nums < 0] <-
    head(strings.neg, length(which(tar.nums < 0)))
  cur.dat$comp[cur.ind][cur.nums < 0] <-
    tail(strings.neg, length(which(cur.nums < 0)))
  list(tar.dat=tar.dat, cur.dat=cur.dat)
}
# Pull out mismatching words from the word regexec; helper functions

reg_pull <- function(reg, start, end, mismatch) {
  ind <- mismatch[mismatch %bw% c(start, end)] - start + 1L
  if(length(ind)) {
    reg.out <- reg[ind]
    match.len <- attr(reg, "match.length")[ind]
  } else {
    reg.out <- -1L
    match.len <- -1L
  }
  attr(reg.out, "match.length") <- match.len
  attr(reg.out, "useBytes") <- attr(reg, "useBytes")
  attr(reg.out, "word.count") <- end - start + 1L
  reg.out
}
# Generate the indices in each row and apply the pulling functions

reg_apply <- function(reg, ends, mismatch, fun) {
  if(length(ends)) {
    Map(
      fun, reg, c(1L, head(ends, -1L) + 1L), ends,
      MoreArgs=list(mismatch=mismatch)
    )
  } else list()
}
# Modify `tar.dat` and `cur.dat` by generating `regmatches` indices for the
# words that are different
#
# If `diff.mode` is "word", then line up lines based on the word matches and
# mismatches contained there-in.  This is done by generating new strings
# that match or don't depending on the word contents, and then passing those
# back as the `comp` component of the `tar.dat` and `cur.dat` returned.  The
# subsequent line diff will cause the relevant lines to be lined up.
#
# Note that in "word" mode the returned values may be longer than the input ones
# as it may be necessary to add lines to get things to match-up.  Added lines
# are indicated by TRUE values in teh `pad` component of the `*.dat` return
# values

diff_word2 <- function(
  tar.dat, cur.dat, tar.ind, cur.ind, etc, match.quotes=FALSE, diff.mode,
  warn=TRUE
) {
  stopifnot(
    is.TF(match.quotes), is.TF(warn),
    isTRUE(valid.dat(tar.dat)), isTRUE(valid.dat(cur.dat)) # expensive validation?
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
  tar.dat$word.ind[tar.ind] <- reg_apply(tar.reg, tar.ends, tar.mism, reg_pull)
  cur.dat$word.ind[tar.ind] <- reg_apply(cur.reg, cur.ends, cur.mism, reg_pull)

  # If in word mode (which is really atomic mode), generate a spoofed
  # `comp` vector that will force the line diff to align in a way that respects
  # the word differences.  This is inefficient and round-about, but has the
  # huge benefit of allowing us to plug in the wrapped diff into our existing
  # line diff infrastructure

  if(diff.mode == "word") {
    word.line.mapped <-
      word_to_line_map(hunks.flat, tar.dat, cur.dat, tar.ends, cur.ends)
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
    dat.chr <- do.call(paste0, split(dat, row(dat)))
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
