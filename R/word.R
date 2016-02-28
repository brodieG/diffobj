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
  target, current, ignore.white.space, match.quotes=FALSE,
  disp.width
) {
  stopifnot(
    is.character(target), is.character(current),
    all(!is.na(target)), all(!is.na(current)),
    is.TF(match.quotes)
  )
  # Compute the char by char diffs for each line

  reg <- paste0(
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
  tar.reg <- gregexpr(reg, target, perl=TRUE)
  cur.reg <- gregexpr(reg, current, perl=TRUE)

  tar.split <- regmatches(target, tar.reg)
  cur.split <- regmatches(current, cur.reg)

  # Collapse into one line if to do the diff across lines, but record
  # item counts so we can reconstitute the lines at the end

  tar.lens <- vapply(tar.split, length, integer(1L))
  cur.lens <- vapply(cur.split, length, integer(1L))

  tar.split <- unlist(tar.split)
  cur.split <- unlist(cur.split)
  if(is.null(tar.split)) tar.split <- character(0L)
  if(is.null(cur.split)) cur.split <- character(0L)

  diffs <- char_diff(
    tar.split, cur.split, ignore.white.space=ignore.white.space,
    context=-1L, mode="context", line.limit=-1L, hunk.limit=-1L,
    disp.width=disp.width
  )
  # Color

  diff.colored <- diffColor(diffs)
  tar.colored <- diff.colored$A
  cur.colored <- diff.colored$B

  # Reconstitute lines if needed

  tar.colored <- split(tar.colored, rep(seq_along(tar.lens), tar.lens))
  cur.colored <- split(cur.colored, rep(seq_along(cur.lens), cur.lens))

  # Merge back into original

  if(length(tar.colored)) regmatches(target, tar.reg) <- tar.colored
  if(length(cur.colored)) regmatches(current, cur.reg) <- cur.colored

  list(target=target, current=current)
}
