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
# working on alternate to diff_word that returns the coordinates of the word
# differences instead of the colored words
#
# Should return a list with, for each of tar and cur, a list of the
# `gregexpr` elements that constitute the word differences.  There is some
# inefficiency here since we junk the text we use to do the word differences
# and we'll re-retrieve them later, but oh well

diff_word2 <- function(
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
  # Need to figure out which elements match, and which ones do not

  hunks.flat <- diffs$hunks
  tar.mism <- unlist(lapply(hunks.flat, function(x) if(!x$context) x$A))
  cur.mism <- abs(unlist(lapply(hunks.flat, function(x) if(!x$context) x$B)))

  # Figure out which line each of these elements came from, and what index
  # in each of those lines they are; we use the recorded lengths in words of
  # each line to reconstruct this; also record original line length so we
  # can compute token ratios

  reg.pull <-  function(reg, start, end, mismatch) {
    ind <- mismatch[mismatch %bw% c(start, end)] - start + 1L
    reg.out <- reg[ind]
    attr(reg.out, "match.length") <- attr(reg, "match.length")[ind]
    attr(reg.out, "useBytes") <- attr(reg, "useBytes")
    attr(reg.out, "word.count") <- end - start + 1L
    reg.out
  }
  tar.reg.fin <- Map(
    reg.pull, tar.reg, c(1L, head(tar.lens, -1L) + 1L),
    tar.lens, MoreArgs=list(mismatch=tar.mism)
  )
  cur.reg.fin <- Map(
    reg.pull, cur.reg, c(1L, head(cur.lens, -1L) + 1L),
    cur.lens, MoreArgs=list(mismatch=cur.mism)
  )
  list(tar=tar.reg.fin, cur=cur.reg.fin, hit.diffs.max=diffs$hit.diffs.max)
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
