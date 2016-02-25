#' @include s4.R

NULL

#' Generate a character representation of Shortest Edit Sequence
#'
#' @seealso \code{\link{diff_ses}}
#' @param x S4 object of class \code{diffObjMyersMbaSes}
#' @param ... unused
#' @return character vector

setMethod("as.character", "diffObjMyersMbaSes",
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
setMethod("as.data.frame", "diffObjMyersMbaSes",
  function(x, row.names=NULL, optional=FALSE, ...) {
    len <- length(x@type)
    mod <- c("Insert", "Delete")
    dat <- data.frame(type=x@type, len=x@length, off=x@offset)
    matches <- dat$type == "Match"
    dat$section <- cumsum(matches + c(0L, head(matches, -1L)))

    # Track what the max offset observed so far for elements of the `a` string
    # so that if we have an insert command we can get the insert position in
    # `a`

    dat$last.a <-
      cummax(ifelse(dat$type != "Insert", dat$off + dat$len, 1L)) - 1L

    # Do same thing with `b`, complicated because the matching entries are all
    # in terms of `a`

    dat$last.b <- cumsum(ifelse(dat$type != "Delete", dat$len, 0L))
    dat
} )
#' Produce Shortest Edit Script
#'
#' Intended primarily for debugging or for other applications that understand
#' that particular format.  See \href{GNU diff docs}{http://www.gnu.org/software/diffutils/manual/diffutils.html#Detailed-Normal}
#' for how to interpret the symbols.
#'
#' @export
#' @param a character
#' @param b character
#' @return character

diff_ses <- function(a, b) as.character(diff_myers_mba(a, b))

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
#' @return list
#' @useDynLib diffobj, .registration=TRUE, .fixes="DIFFOBJ_"

diff_myers_mba <- function(a, b) {
  stopifnot(is.character(a), is.character(b), all(!is.na(c(a, b))))
  res <- .Call(DIFFOBJ_diffobj, a, b)
  res <- setNames(res, c("type", "length", "offset"))
  types <- c("Match", "Insert", "Delete")
  res$type <- factor(types[res$type], levels=types)
  res$offset <- res$offset + 1L  # C 0-indexing originally
  res.s4 <- try(do.call("new", c(list("diffObjMyersMbaSes", a=a, b=b), res)))
  if(inherits(res.s4, "try-error"))
    stop(
      "Logic Error: unable to instantiate shortest edit script object; contact ",
      "maintainer."
    )
  res.s4
}
#' Print Method for Shortest Edit Path
#'
#' Bare bones display of shortest edit path using GNU diff conventions
#'
#' @param object object to display
#' @return character the shortest edit path character representation, invisibly

setMethod("show", "diffObjMyersMbaSes",
  function(object) {
    res <- as.character(object)
    cat(res, sep="\n")
    invisible(res)
} )
#' Summary Method for Shortest Edit Path
#'
#' Displays the data required to generate the shortest edit path for comparison
#' between two strings.
#'
#' @param object the \code{diff_myers_mba} object to display
#' @param with.match logical(1L) whether to show what text the edit command
#'   refers to
#' @param ... forwarded to the data frame print method used to actually display
#'   the data
#' @return whatever the data frame print method returns
#' @export

setMethod("summary", "diffObjMyersMbaSes",
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
# Carries out the comparison between two character vectors and returns the
# elements that match and those that don't as a unitizerDiffDiffs object

char_diff <- function(
  x, y, context=-1L, ignore.white.space, mode, hunk.limit, line.limit,
  disp.width, use.ansi
) {
  if(ignore.white.space) {
    sub.pat <- "(\t| )"
    pat.1 <- sprintf("^%s*|%s*$", sub.pat, sub.pat)
    pat.2 <- sprintf("%s+", sub.pat)
    x.w <- gsub(pat.2, " ", gsub(pat.1, "", x))
    y.w <- gsub(pat.2, " ", gsub(pat.1, "", y))
  }
  diff <- diff_myers_mba(x.w, y.w)
  if(ignore.white.space) {
    diff@a <- x
    diff@b <- y
  }
  hunks <- as.hunks(
    diff, context=context, mode=mode, hunk.limit=hunk.limit,
    line.limit=line.limit, disp.width=disp.width, use.ansi=use.ansi
  )
  new("diffObjDiffDiffs", hunks=hunks)
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
char_diff_int <- function(x, y) {
  stopifnot(
    is.character(x), is.character(y), !any(is.na(c(x, y)))
  )
  # find first difference

  x.d <- y.d <- logical()
  len.match <- min(length(x), length(y))
  eq <- head(x, len.match) == head(y, len.match)
  diffs <- which(!eq)
  if(!length(diffs)) {
    # extras at end
    x.d <- c(rep(0L, len.match), c(rep(NA_integer_, length(x) - len.match)))
    y.d <- c(rep(0L, len.match), c(rep(NA_integer_, length(y) - len.match)))
  } else {
    first.diff <- diffs[[1L]]
    eq.so.far <- rep(0L, first.diff - 1L)
    eq.extra <- 0L

    # Try to see if difference exists in y, and if not see if any subsequent
    # line does exist, indicating deletions from x.  However, make sure that
    # we don't have the same number of matches in x as well as in y, as that
    # would suggest the match in y is just a coincidence

    # This grows vectors, but doesn't seem to be a huge performance issue at
    # the normal scale we run this.

    diff.found <- FALSE
    for(i in seq(first.diff, length(x), by=1L)) {
      n.match.self <- which(x[[i]] == tail(x, -i))
      n.match <- which(
        x[[i]] == tail(y, if(first.diff == 1L) Inf else  -first.diff + 1L)
      )
      if(length(n.match) && length(n.match) > length(n.match.self)) {
        tmp.res <- Recall(
          x[i:length(x)], y[(n.match[[1L]] + first.diff - 1L):length(y)]
        )
        # compute matched mismatches and line them up so they have the same
        # non-zero non-NA integer value in both x and y

        m.match <- match_mismatch(eq.extra, n.match[[1L]] - 1L)

        # re-adjust recursion values by how many matched mismatches we have
        m.max <- max(0L, unlist(m.match), na.rm=TRUE)
        tmp.res <- lapply(
          tmp.res, function(y) ifelse(!is.na(y) & !!y, y + m.max, y)
        )
        # construct final match vector, any additional mismatches in one or
        # other vector are mismatched and encoded as NAs
        x.d <- c(eq.so.far, m.match[[1L]], tmp.res[[1L]])
        y.d <- c(eq.so.far, m.match[[2L]], tmp.res[[2L]])
        diff.found <- TRUE
        break
      }
      eq.extra <- eq.extra + 1L
    }
    if(!diff.found) {  # Difference did not exist in y
      m.match <- match_mismatch(eq.extra, length(y) - first.diff + 1L)
      x.d <- c(eq.so.far, m.match[[1L]])
      y.d <- c(eq.so.far, m.match[[2L]])
    }
  }
  list(target=x.d, current=y.d)
}
# Alternate implementation of Myers algorithm in R, without linear space
# modification.  Included here mostly for reference purposes and not intended
# for use since the MBA myers implemenation should be far superior

char_diff_myers_simple <- function(target, current) {
  path <- char_diff_myers_simple_int(target, current)
  diff_path_to_diff(path, target, current)
}
char_diff_myers_simple_int <- function(A, B) {
  N <- length(A)
  M <- length(B)
  MAX <- M + N + 1L
  if(!MAX) return(matrix(integer(0L), ncol=2))
  OFF <- MAX + 1L  # offset to adjust to R indexing
  Vl <- vector("list", MAX)
  for(D in seq_len(MAX) - 1L) {
    Vl[[D + 1L]] <- if(!D) integer(2L * MAX + 1L) else Vl[[D]]
    for(k in seq(-D, D, by=2L)) {
      # not sure of precendence for || vs &&
      # k == -D means x == 0
      V <- Vl[[D + 1L]]
      if(k == -D || (k != D && V[k - 1L + OFF] < V[k + 1L + OFF])) {
        x <- V[k + 1L + OFF]
      } else {
        x <- V[k - 1L + OFF] + 1L
      }
      y <- x - k

      # Move on diagonal
      while (x < N && y < M && A[x + 1L] == B[y + 1L]) {
        x <- x + 1L
        y <- y + 1L
      }
      # Record last match or end; if a mismatch no longer increment

      Vl[[D + 1L]][k + OFF] <- x
      if(x >= N && y >= M) {
        # Create matrix to hold entire result path; should be longest of
        # A and B plus recorded differences

        path.len <- D + max(N, M)
        res <- matrix(integer(1L), nrow=path.len, ncol=2)
        res[path.len, ] <- c(x, y)
        path.len <- path.len - 1L

        for(d in rev(seq_len(D))) {
          Vp <- Vl[[d]]
          break.out <- FALSE
          repeat {
            # can't match to zero since that is the initialized value
            shift.up <- Vp[k + 1L + OFF] == x && x
            shift.left <- Vp[k - 1L + OFF] == x - 1L && x > 1L
            if(x <= 0L && y <= 0L) {
              break
            } else if(!shift.up && !shift.left) {
              # must be on snake or about to hit 0,0
              x <- max(x - 1L, 0L)
              y <- max(y - 1L, 0L)
            } else {
              if(shift.up) {
                y <- y - 1L
                k <- k + 1L
              } else {
                x <- x - 1L
                k <- k - 1L
              }
              break.out <- TRUE
            }
            res[path.len, ] <- c(x, y)
            path.len <- path.len - 1L
            if(break.out) break
          }
        }
        if(any(res < 0L))
          stop("Logic Error: diff generated illegal coords; contact maintainer.")
        return(res)
      }
    }
  }
  stop("Logic Error, should not get here")
}
# Translates a diff path produced by the simple Myers Algorithm into the
# standard format we use in the rest of the package

diff_path_to_diff <- function(path, target, current) {
  stopifnot(
    is.character(target), is.character(current),
    is.matrix(path), is.integer(path), ncol(path) == 2,
    all(path[, 1L] %in% c(0L, seq_along(target))),
    all(path[, 2L] %in% c(0L, seq_along(current)))
  )
  # Path specifies 0s as well as duplicate coordinates, which we don't use
  # in our other formats.  For dupes, find first value for each index that is
  # lined up with a real value in the other column

  get_dupe <- function(x) {
    base <- !logical(length(x))
    if(!length(y <- which(x != 0L)))
      base[[1L]] <- FALSE else base[[min(y)]] <- FALSE
    base
  }
  cur.dup <- as.logical(ave(path[, 1L], path[, 2L], FUN=get_dupe))
  tar.dup <- as.logical(ave(path[, 2L], path[, 1L], FUN=get_dupe))

  path[!path] <- NA_integer_
  path[tar.dup, 1L] <- NA_integer_
  path[cur.dup, 2L] <- NA_integer_

  # Now create the character equivalents of the path matrix

  tar.path <- target[path[, 1L]]
  cur.path <- current[path[, 2L]]

  # Mark the equalities in the path matrix by setting them negative

  path[which(tar.path == cur.path), ] <- -path[which(tar.path == cur.path), ]

  # Remaining numbers are the mismatches which we will arbitrarily assign to
  # each other; to do so we first split our data into groups of matches and
  # mismatches and do the mapping there-in.  We also get rid of non-matching
  # entries.

  matched <- ifelse(!is.na(path[, 1]) & path[, 1] < 0L, 1L, 0L)
  splits <- cumsum(abs(diff(c(0, matched))))
  chunks <- split.data.frame(path, splits)
  res.tar <- res.cur <- vector("list", length(chunks))
  mm.count <- 0L  # for tracking matched mismatches

  for(i in seq_along(chunks)) {
    x <- chunks[[i]]
    if((neg <- any(x < 0L, na.rm=TRUE)) && !all(x < 0L, na.rm=TRUE))
      stop("Logic Error: match group error; contact maintainer")
    if(neg) {
      # Matches, so equal length and set to zero
      res.tar[[i]] <- res.cur[[i]] <- integer(nrow(x))
    } else {
      # Mismatches
      tar.mm <- Filter(Negate(is.na), x[, 1L])
      cur.mm <- Filter(Negate(is.na), x[, 2L])

      x.min.len <- min(length(tar.mm), length(cur.mm))
      res.tar[[i]] <- res.cur[[i]] <- seq_len(x.min.len) + mm.count
      mm.count <- x.min.len + mm.count
      length(res.tar[[i]]) <- length(tar.mm)
      length(res.cur[[i]]) <- length(cur.mm)
    }
  }
  if(!length(res.tar)) res.tar <- integer()
  if(!length(res.cur)) res.cur <- integer()

  return(list(target=unlist(res.tar), current=unlist(res.cur)))
}
