#' @include s4.R

NULL

# Generate a character representation of Shortest Edit Sequence
#
# @seealso \code{\link{diff_ses}}
# @param x S4 object of class \code{MyersMbaSes}
# @param ... unused
# @return character vector

setMethod("as.character", "MyersMbaSes",
  function(x, ...) {
    dat <- as.data.frame(x)

    # Split our data into sections that have either deletes or inserts and get
    # rid of the matches

    dat <- dat[dat$type != "Match", ]
    d.s <- split(dat, dat$section)

    # For each section, compute whether we should display, change, insert,
    # delete, or both, and based on that append to the ses string

    ses_rng <- function(off, len)
      paste0(off, if(len > 1L) paste0(",", off + len - 1L))

    vapply(
      unname(d.s),
      function(d) {
        del <- sum(d$len[d$type == "Delete"])
        ins <- sum(d$len[d$type == "Insert"])
        if(del) {
          del.first <- which(d$type == "Delete")[[1L]]
          del.off <- d$off[del.first]
        }
        if(ins) {
          ins.first <- which(d$type == "Insert")[[1L]]
          ins.off <- d$off[ins.first]
        }
        if(del && ins) {
          paste0(ses_rng(del.off, del), "c", ses_rng(ins.off, ins))
        } else if (del) {
          paste0(ses_rng(del.off, del), "d", d$last.b[[1L]])
        } else if (ins) {
          paste0(d$last.a[[1L]], "a", ses_rng(ins.off, ins))
        } else {
          stop("Logic Error: unexpected edit type; contact maintainer.")
        }
      },
      character(1L)
) } )
# Used for mapping edit actions to numbers so we can use numeric matrices
.edit.map <- c("Match", "Insert", "Delete")

setMethod("as.matrix", "MyersMbaSes",
  function(x, row.names=NULL, optional=FALSE, ...) {
    # map del/ins/match to numbers

    len <- length(x@type)

    edit <- match(x@type, .edit.map)
    matches <- .edit.map[edit] == "Match"
    section <- cumsum(matches + c(0L, head(matches, -1L)))

    # Track what the max offset observed so far for elements of the `a` string
    # so that if we have an insert command we can get the insert position in
    # `a`

    last.a <- c(
      if(len) 0L,
      head(
        cummax(
          ifelse(.edit.map[edit] != "Insert", x@offset + x@length, 1L)
        ) - 1L, -1L
    ) )

    # Do same thing with `b`, complicated because the matching entries are all
    # in terms of `a`

    last.b <- c(
      if(len) 0L,
      head(cumsum(ifelse(.edit.map[edit] != "Delete", x@length, 0L)), -1L)
    )
    cbind(
      type=edit, len=x@length, off=x@offset, section=section, last.a=last.a,
      last.b = last.b
    )
} )
setMethod("as.data.frame", "MyersMbaSes",
  function(x, row.names=NULL, optional=FALSE, ...) {
    len <- length(x@type)
    mod <- c("Insert", "Delete")
    dat <- data.frame(type=x@type, len=x@length, off=x@offset)
    matches <- dat$type == "Match"
    dat$section <- cumsum(matches + c(0L, head(matches, -1L)))

    # Track what the max offset observed so far for elements of the `a` string
    # so that if we have an insert command we can get the insert position in
    # `a`

    dat$last.a <- c(
      if(nrow(dat)) 0L,
      head(
        cummax(ifelse(dat$type != "Insert", dat$off + dat$len, 1L)) - 1L, -1L
    ) )

    # Do same thing with `b`, complicated because the matching entries are all
    # in terms of `a`

    dat$last.b <- c(
      if(nrow(dat)) 0L,
      head(cumsum(ifelse(dat$type != "Delete", dat$len, 0L)), -1L)
    )
    dat
} )
#' Shortest Edit Script
#'
#' Computes shortest edit script to convert \code{a} into \code{b} by removing
#' elements from \code{a} and adding elements from \code{b}.  Intended primarily
#' for debugging or for other applications that understand that particular
#' format.  See \href{GNU diff docs}{http://www.gnu.org/software/diffutils/manual/diffutils.html#Detailed-Normal}
#' for how to interpret the symbols.
#'
#' @export
#' @param a character
#' @param b character
#' @return character

ses <- function(a, b) as.character(diff_myers_mba(a, b))

#' Diff two character vectors
#'
#' Implementation of Myer's with linear space refinement originally implemented
#' by Mike B. Allen as part of
#' \href{libmba}{http://www.ioplex.com/~miallen/libmba/}
#' version 0.9.1.  This implementation uses the exact same algorithm, except
#' that the C code is simplified by using fixed size arrays instead of variable
#' ones for tracking the longest reaching paths and for recording the shortest
#' edit scripts.  Additionally all error handling and memory allocation calls
#' have been moved to the internal R functions designed to handle those things.
#'
#' @keywords internal
#' @param a character
#' @param b character
#' @param max.diffs integer(1L) how many differences before giving up; set to
#'   zero to allow as many as there are
#' @return list
#' @useDynLib diffobj, .registration=TRUE, .fixes="DIFFOBJ_"

diff_myers_mba <- function(a, b, max.diffs=0L) {
  stopifnot(
    is.character(a), is.character(b), all(!is.na(c(a, b))), is.int.1L(max.diffs)
  )
  res <- .Call(DIFFOBJ_diffobj, a, b, max.diffs)
  res <- setNames(res, c("type", "length", "offset", "diffs"))
  types <- .edit.map
  res$type <- factor(types[res$type], levels=types)
  res$offset <- res$offset + 1L  # C 0-indexing originally
  res.s4 <- try(do.call("new", c(list("MyersMbaSes", a=a, b=b), res)))
  if(inherits(res.s4, "try-error"))
    stop(
      "Logic Error: unable to instantiate shortest edit script object; contact ",
      "maintainer."
    )
  res.s4
}
# Print Method for Shortest Edit Path
#
# Bare bones display of shortest edit path using GNU diff conventions
#
# @param object object to display
# @return character the shortest edit path character representation, invisibly
# @rdname diffobj_s4method_doc

#' @rdname diffobj_s4method_doc

setMethod("show", "MyersMbaSes",
  function(object) {
    res <- as.character(object)
    cat(res, sep="\n")
    invisible(res)
} )

# Summary Method for Shortest Edit Path
#
# Displays the data required to generate the shortest edit path for comparison
# between two strings.
#
# @param object the \code{diff_myers_mba} object to display
# @param with.match logical(1L) whether to show what text the edit command
#   refers to
# @param ... forwarded to the data frame print method used to actually display
#   the data
# @return whatever the data frame print method returns
# @export

#' @rdname diffobj_s4method_doc

setMethod("summary", "MyersMbaSes",
  function(object, with.match=FALSE, ...) {
    what <- vapply(
      seq_along(object@type),
      function(y) {
        t <- object@type[[y]]
        o <- object@offset[[y]]
        l <- object@length[[y]]
        vec <- if(t == "Insert") b else a
        paste0(vec[o:(o + l - 1L)], collapse="")
      },
      character(1L)
    )
    res <- data.frame(
      type=object@type, string=what, len=object@length, offset=object@offset
    )
    if(!with.match) res <- res[-2L]
    print(res, ...)
} )
# mode is display mode (sidebyside, etc.)
# diff.mode is whether we are doing the first pass line diff, or doing the
#   in-hunk or word-wrap versions
# warn is to allow us to suppress warnings after first hunk warning

char_diff <- function(x, y, context=-1L, etc, diff.mode, warn) {
  stopifnot(
    diff.mode %in% c("line", "hunk", "wrap"),
    isTRUE(warn) || identical(warn, FALSE)
  )
  if(etc@ignore.white.space) {
    x.w <- normalize_whitespace(x)
    y.w <- normalize_whitespace(y)
  } else {
    x.w <- x
    y.w <- y
  }
  max.diffs <- etc@max.diffs
  diff <- diff_myers_mba(x.w, y.w, max.diffs)  # probably shouldn't generate S4

  # Reset given whitespace and other modifications

  diff@a <- x
  diff@b <- y
  hunks <- as.hunks(diff, etc=etc)
  hit.diffs.max <- FALSE
  if(diff@diffs < 0L) {
    hit.diffs.max <- TRUE
    diff@diffs <- -diff@diffs
    diff.msg <- c(
      line="overall", hunk="in-hunk word", wrap="atomic wrap-word"
    )
    if(warn)
      warning(
        "Exceeded diff limit during diff computation (",
        diff@diffs, " vs. ", max.diffs, " allowed); ",
        diff.msg[diff.mode], " diff is likely not optimal",
        call.=FALSE
      )
  }
  # used to be a `DiffDiffs` object, but too slow

  list(hunks=hunks, hit.diffs.max=hit.diffs.max)
}
# Variation on `char_diff` used for the overall diff where we don't need
# to worry about overhead from creating the `Diff` object

line_diff <- function(
  target, current, tar.capt, cur.capt, context, etc, warn=TRUE, strip=TRUE
) {
  if(!is.valid.guide.fun(etc@guides))
    stop(
      "Logic Error: guides are not a valid guide function; contact maintainer"
    )
  etc@guide.lines <-
    make_guides(target, tar.capt, current, cur.capt, etc@guides)

  # Some debate as to whether we want to do this first, or last.  First has
  # many benefits so that everything is consistent, width calcs can work fine,
  # etc., but only issue is that user provided trim functions might not expect
  # the transformation of the data; this needs to be documented with the trim
  # docs.

  if(strip) {
    tar.capt <- strip_hz_control(tar.capt, stops=etc@tab.stops)
    cur.capt <- strip_hz_control(cur.capt, stops=etc@tab.stops)
  }
  # Apply trimming to remove row heads, etc

  tar.trim.ind <- apply_trim(target, tar.capt, etc@trim)
  tar.trim <- do.call(
    substr, list(tar.capt, tar.trim.ind[, 1L], tar.trim.ind[, 2L])
  )
  cur.trim.ind <- apply_trim(current, cur.capt, etc@trim)
  cur.trim <- do.call(
    substr, list(cur.capt, cur.trim.ind[, 1L], cur.trim.ind[, 2L])
  )
  # Word diff is done in three steps: create an empty template vector structured
  # as the result of a call to `gregexpr` without matches, if dealing with
  # compliant atomic vectors in print mode, then update with the word diff
  # matches, finally, update with in-hunk word diffs for hunks that don't have
  # any existing word diffs:

  # Set up data lists with all relevant info; need to pass to diff_word so it
  # can be modified.
  # - raw: the original captured text line by line
  # - trim: as above, but with row meta data removed
  # - trim.ind: the indices used to re-insert `trim` into `raw`
  # - comp: the strings that will have the line diffs run on
  # - eq: the portion of `trim` that is equal post word-diff
  # - fin: the final character string for display to user
  # - word.ind: for use by `regmatches<-` to re-insert colored words
  # - tok.rat: for use by `align_eq` when lining up lines within hunks

  tar.dat <- list(
    raw=tar.capt, trim=tar.trim,
    trim.ind.start=tar.trim.ind[, 1L], trim.ind.end=tar.trim.ind[, 2L],
    comp=tar.trim, eq=tar.trim, fin=tar.capt, fill=logical(length(tar.capt)),
    word.ind=replicate(length(tar.capt), .word.diff.atom, simplify=FALSE),
    tok.rat=rep(1, length(tar.capt))
  )
  cur.dat <- list(
    raw=cur.capt, trim=cur.trim,
    trim.ind.start=cur.trim.ind[, 1L], trim.ind.end=cur.trim.ind[, 2L],
    comp=cur.trim, eq=cur.trim, fin=cur.capt, fill=logical(length(cur.capt)),
    word.ind=replicate(length(cur.capt), .word.diff.atom, simplify=FALSE),
    tok.rat=rep(1, length(cur.capt))
  )
  # Word diffs in wrapped form is atomic; note this will potentially change
  # the length of the vectors

  if(
    is.atomic(target) && is.atomic(current) &&
    length(tar.rh <- which_atomic_rh(tar.capt)) &&
    length(cur.rh <- which_atomic_rh(cur.capt))
  ) {
    diff.word <- diff_word2(
      tar.dat, cur.dat, tar.ind=tar.rh, cur.ind=cur.rh,
      diff.mode="wrap", warn=warn, etc=etc
    )
    warn <- !diff.word$hit.diffs.max
    tar.dat <- diff.word$tar.dat
    cur.dat <- diff.word$cur.dat
  }
  # Actual line diff

  diffs <- char_diff(
    tar.dat$comp, cur.dat$comp, etc=etc, diff.mode="line", warn=warn
  )
  warn <- !diffs$hit.diffs.max

  # Word diffs on hunks; check first which lines already have diffs and identify
  # the diff hunks that don't contain any of those lines

  tar.l.w.d <- which(vapply(tar.dat$word.ind, "[", integer(1L), 1L) != -1L)
  cur.l.w.d <- which(vapply(cur.dat$word.ind, "[", integer(1L), 1L) != -1L)
  all.l.w.d <- c(tar.l.w.d, -cur.l.w.d)

  hunks.flat <- diffs$hunks
  hunks.w.o.w.diff <- vapply(
    hunks.flat,
    function(y) !y$context && !any(unlist(y[c("A", "B")]) %in% all.l.w.d),
    logical(1L)
  )
  # For each of those hunks, run the word diffs and store the results in the
  # word.diffs list; bad part here is that we keep overwriting the overall
  # diff data for each hunk, which might be slow

  for(i in which(hunks.w.o.w.diff)) {
    h.a <- hunks.flat[[i]]
    h.a.ind <- c(h.a$A, h.a$B)
    h.a.tar.ind <- h.a.ind[h.a.ind > 0]
    h.a.cur.ind <- abs(h.a.ind[h.a.ind < 0])
    h.a.w.d <- diff_word2(
      tar.dat, cur.dat, h.a.tar.ind, h.a.cur.ind, diff.mode="hunk", warn=warn,
      etc=etc
    )
    tar.dat <- h.a.w.d$tar.dat
    cur.dat <- h.a.w.d$cur.dat
    warn <- !h.a.w.d$hit.diffs.max
  }
  # Compute the token ratios

  tok_ratio_compute <- function(z) vapply(
    z,
    function(y)
      if(is.null(wc <- attr(y, "word.count"))) 1
      else max(0, (wc - length(y)) / wc),
    numeric(1L)
  )
  tar.dat$tok.rat <- tok_ratio_compute(tar.dat$word.ind)
  cur.dat$tok.rat <- tok_ratio_compute(cur.dat$word.ind)
  tar.dat$eq <- `regmatches<-`(tar.dat$trim, tar.dat$word.ind, value="")
  cur.dat$eq <- `regmatches<-`(cur.dat$trim, cur.dat$word.ind, value="")

  # Instantiate result

  hunk.grps <- group_hunks(
    hunks.flat, etc=etc, tar.capt=tar.dat$raw, cur.capt=cur.dat$raw
  )
  new(
    "Diff", diffs=hunk.grps, target=target, current=current,
    hit.diffs.max=!warn, tar.dat=tar.dat, cur.dat=cur.dat, etc=etc
  )
}
# Helper function encodes matches within mismatches so that we can later word
# diff the mismatches

match_mismatch <- function(x, y) {
  mis.overlap <- min(x, y)
  mis.extra <- max(x, y) - mis.overlap
  mis.seq <- seq_len(mis.overlap)
  mis.x <- x > y

  # construct final match vector, any additional mismatches in one or
  # other vector are mismatched and encoded as NAs

  x.d <- c(mis.seq, rep(NA_integer_, if(mis.x) mis.extra else 0L))
  y.d <- c(mis.seq, rep(NA_integer_, if(!mis.x) mis.extra else 0L))

  list(target=x.d, current=y.d)
}
