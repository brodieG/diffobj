# @include S4.R

NULL

# Compute the ranges of a hunk group based on atomic hunk ids
#
# rng.o is a matrix where each column represents `c(tar.rng, cur.rng)`
# and rng.o has the original untrimmed values

find_rng <- function(ids, rng.o) {
  with.rng <- ids[which(rng.o[1L, ids] > 0L)]
  if(!length(with.rng)) {
    # Find previous earliest originally existing item we want to insert
    # after; note we need to look at the non-trimmed ranges, and we include
    # the first context atomic hunk in the group as a potential match
    prev <- rng.o[
      2L, seq_len(ncol(rng.o)) <= max(ids[[1L]], 0L) &
        rng.o[1L, ] > 0L
    ]
    if(!length(prev)) integer(2L) else c(max(prev), 0L)
  } else {
    c(min(rng.o[1L, intersect(ids, with.rng)]), max(rng.o[2L, ids]))
  }
}
# Create a text representation of a file line range

rng_as_chr <- function(range) {
  a <- range[[1L]]
  b <- if(diff(range))
    paste0(",", if(range[[2L]]) diff(range) + 1L else 0)
  paste0(a, b)
}
# Convert a hunk group into text representation

hunk_as_char <- function(
  h.g, ranges.orig, mode, disp.width, ignore.white.space
) {
  # First check that the hunk group hasn't been completely trimmed

  all.lines <- sum(
    unlist(
      lapply(h.g, function(h.a) unlist(h.a[c("tar.rng.trim", "cur.rng.trim")]))
  ) )
  if(!all.lines) {
    character(0L)
  } else {
    max.w <- calc_width(disp.width, mode)
    capt.width <- calc_width_pad(disp.width, mode)
    h.ids <- vapply(h.g, "[[", integer(1L), "id")
    tar.rng <- find_rng(h.ids, ranges.orig[1:2, , drop=FALSE])
    cur.rng <- find_rng(h.ids, ranges.orig[3:4, , drop=FALSE])

    hh.a <- paste0("-", rng_as_chr(tar.rng))
    hh.b <- paste0("+", rng_as_chr(cur.rng))

    hunk.head <- crayon_style(
      if(mode == "sidebyside") {
        paste0(
          rpadt(sprintf("@@ %s @@", hh.a), max.w),
          "  ",
          rpadt(sprintf("@@ %s @@", hh.b), max.w),
          collapse=""
        )
      } else {
        sprintf("@@ %s %s @@", hh.a, hh.b)
      },
      "cyan"
    )
    # Get trimmed character ranges; positives are originally from target, and
    # negatives from current

    get_chrs <- function(h.a, mode, eq=FALSE) {
      ab <- LETTERS[1:2]
      stopifnot(mode %in% LETTERS[1:2], length(mode) == 1L, is.TF(eq))
      rng <- c(
        seq(h.a$tar.rng.trim[[1L]], h.a$tar.rng.trim[[2L]]),
        -seq(h.a$cur.rng.trim[[1L]], h.a$cur.rng.trim[[2L]])
      )
      chr.ind <- sprintf("%s%s.chr", mode, if(eq) ".eq" else "")
      h.a[[chr.ind]][match(rng, h.a[[mode]], nomatch=0L)]
    }
    # Output varies by mode

    diff.txt <- if(mode == "context") {
      # Need to get all the A data and the B data

      A.ctx <- unlist(
        lapply(h.g, function(h.a) rep(h.a$context, length(h.a$A.chr)))
      )
      B.ctx <- unlist(
        lapply(h.g, function(h.a) rep(h.a$context, length(h.a$B.chr)))
      )
      A <- wrap(unlist(lapply(h.g, get_chrs, mode="A")), width=capt.width)
      B <- wrap(unlist(lapply(h.g, get_chrs, mode="B")), width=capt.width)
      A[!A.ctx] <- sign_pad(A[!A.ctx], 3L)
      B[!B.ctx] <- sign_pad(B[!B.ctx], 2L)
      A[A.ctx] <- sign_pad(A[A.ctx], 1L)
      B[B.ctx] <- sign_pad(B[B.ctx], 1L)
      unlist(
        c(
          A,
          if(length(B)) crayon_style("~~~~", "silver"),
          B
      ) )
    } else if(mode == "unified") {
      unlist(
        lapply(h.g,
          function(h.a) {
            pos <- h.a$A > 0L
            A.out <- wrap(get_chrs(h.a, mode="A"), capt.width)
            if(!h.a$context) {
              A.out[pos] <- sign_pad(A.out[pos], 3L)
              A.out[!pos] <- sign_pad(A.out[!pos], 2L)
            } else {
              A.out <- sign_pad(A.out, 1L)
            }
            A.out
      } ) )
    } else if(mode == "sidebyside") {
      unlist(
        lapply(h.g,
          function(h.a) {
            # Ensure same number of elements in A and B

            A.out <- get_chrs(h.a, "A")
            B.out <- get_chrs(h.a, "B")
            A.present <- rep(TRUE, length(A.out))
            B.present <- rep(TRUE, length(B.out))
            len.diff <- length(A.out) - length(B.out)

            A.w <- wrap(A.out, capt.width, pad=TRUE)
            B.w <- wrap(B.out, capt.width, pad=TRUE)

            # Same number of els post wrap

            if(length(unlist(A.w)) || length(unlist(B.w))) {
              A.w.pad <- sign_pad(A.w, ifelse(!h.a$context & A.present, 3L, 1L))
              B.w.pad <- sign_pad(B.w, ifelse(!h.a$context & B.present, 2L, 1L))

              blanks <- paste0(rep(" ", max.w), collapse="")

              # Match up the "equal" versions of the strings; we want to split
              # in chunks that start with a match and end in mismatches

              A.eq <- get_chrs(h.a, "A", TRUE)
              B.eq <- get_chrs(h.a, "B", TRUE)

              AB.aligned <- align_eq(
                A.w.pad, B.w.pad, A.eq, B.eq, ignore.white.space
              )
              A.chunks <- AB.aligned$A
              B.chunks <- AB.aligned$B

              # Make everything same length by adding blanks as needed

              for(i in seq_along(A.chunks)) {
                A.ch <- A.chunks[[i]]
                B.ch <- B.chunks[[i]]
                A.l <- length(A.ch)
                B.l <- length(B.ch)
                max.l <- max(A.l, B.l)
                length(A.ch) <- length(B.ch) <- max.l

                for(j in seq_along(A.ch)) {
                    l.diff <- length(A.ch[[j]]) - length(B.ch[[j]])
                    if(l.diff < 0L)
                      A.ch[[j]] <- c(A.ch[[j]], rep(blanks, abs(l.diff)))
                    if(l.diff > 0L)
                      B.ch[[j]] <- c(B.ch[[j]], rep(blanks, l.diff))
                }
                A.chunks[[i]] <- A.ch
                B.chunks[[i]] <- B.ch
              }
              paste0(unlist(A.chunks), "  ", unlist(B.chunks))
            }
    } ) ) }
    c(hunk.head, diff.txt)
  }
}
#' @rdname diffobj_s4method_doc

setMethod("as.character", "diffObjDiff",
  function(x, ...) {
    # These checks should never fail since presumably the inputs have been
    # checked earlier; here just in case we mess something up in devel or
    # testing

    hunk.limit <- x@hunk.limit
    line.limit <- x@line.limit
    hunk.limit <- x@hunk.limit
    disp.width <- x@disp.width
    max.diffs <- x@max.diffs
    max.diffs.in.hunk <- x@max.diffs.in.hunk
    max.diffs.wrap <- x@max.diffs.wrap
    mode <- x@mode
    tab.stops <- x@tab.stops
    ignore.white.space <- x@ignore.white.space

    len.max <- max(length(x@tar.capt), length(x@cur.capt))
    no.diffs <- if(!any(x)) {
      msg <- "No visible differences between objects."
      if(!ignore.white.space && !identical(x@tar.capt, x@cur.capt)) {
        msg <- paste0(
          "Only visible differences between objects are horizontal white ",
          "spaces. You can re-run diff with `ignore.white.space=FALSE` to show ",
          "them."
      ) }
      res <- crayon_style(msg, "silver")
    }
    # Basic width computation and banner size

    banner.len <- banner_len(mode)
    max.w <- calc_width(disp.width, mode)

    if(line.limit[[1L]] >= 0L)
      line.limit <- pmax(integer(2L), line.limit - banner.len)

    # Trim hunks to the extent need to make sure we fit in lines; start by
    # dropping hunks beyond hunk limit

    hunk.grps <- trim_hunks(
      x@diffs$hunks, mode=mode, disp.width=disp.width, line.limit=line.limit,
      hunk.limit=hunk.limit
    )
    hunks.flat <- unlist(hunk.grps, recursive=FALSE)

    # Make the object banner and compute more detailed widths post trim

    banner.A <- paste0("--- ", x@tar.banner)
    banner.B <- paste0("+++ ", x@cur.banner)

    if(mode == "sidebyside") {
      # If side by side we want stuff close together if reasonable
      max.col.w <- calc_width_unpad(
        max(
          unlist(lapply(hunks.flat, function(x) nchar(c(x$A.chr, x$B.chr))))
        ), mode=mode
      )
      max.w <- if(max.col.w < max.w) max(.min.width, max.col.w) else max.w
      # future calculations should assume narrower display
      disp.width <- max.w * 2L + 2L
      comb.fun <- paste0
      t.fun <- rpadt
    } else {
      comb.fun <- c
      t.fun <- chr_trim
    }
    banner <- comb.fun(
      crayon_style(t.fun(banner.A, max.w), "red"),
      if(mode == "sidebyside") "  ",
      crayon_style(t.fun(banner.B, max.w), "green")
    )
    # Trim banner if exceeds line limit, and adjust line limit for banner size

    if(line.limit[[1L]] >= 0 && line.limit[[1L]] < banner.len)
      length(banner) <- line.limit[[2L]]

    # Post trim, figure out max lines we could possibly be showing from capture
    # strings; careful with ranges,

    trim.meta <- attr(hunk.grps, "meta")
    lim.line <- trim.meta$lines
    lim.hunk <- trim.meta$hunks
    ll <- !!lim.line[[1L]]
    lh <- !!lim.hunk[[1L]]
    diff.count <- count_diffs(hunk.grps)
    str.fold.out <- if(x@diffs$diffs.max > diff.count) {
      crayon_style(
        paste0(
          x@diffs$diffs.max - diff.count,
          " differences are hidden by our use of `max.level`"
        ),
        "silver"
      )
    }
    limit.out <- if(ll || lh) {
      if(!is.null(str.fold.out)) {
        stop(
          "Logic Error: should not be str folding when limited; contact ",
          "maintainer."
        )

      }
      crayon_style(
        paste0(
          "... omitted ",
          if(ll) sprintf("%d/%d lines", lim.line[[1L]], lim.line[[2L]]),
          if(ll && lh) ", ",
          if(lh) sprintf("%d/%d hunks", lim.hunk[[1L]], lim.hunk[[2L]])
        ),
        "silver"
      )
    }
    ranges <- vapply(
      hunks.flat, function(h.a)
        c(h.a$tar.rng.trim, h.a$cur.rng.trim),
      integer(4L)
    )
    ranges.orig <- vapply(
      hunks.flat, function(h.a) c(h.a$tar.rng.sub, h.a$cur.rng.sub), integer(4L)
    )
    tar.max <- max(ranges[2L, ], 0L)
    cur.max <- max(ranges[4L, ], 0L)

    # Detect whether we should attempt to deal with wrapping objects, if so
    # overwrite cur/tar.body/rest variables with the color diffed wrap word
    # diffs; note that if the tar/cur.capt.def is not NULL then the objects
    # being compared must be atomic vectors

    cur.body <- tar.body <- character(0L)
    cur.rest <- show.range.cur <- seq_len(cur.max)
    tar.rest <- show.range.tar <- seq_len(tar.max)

    if(
      identical(x@capt.mode, "print") &&
      identical(x@tar.capt, x@tar.capt.def) &&
      identical(x@cur.capt, x@cur.capt.def)
    ) {
      # Separate out the stuff that can wrap (starts with index headers vs. not),
      # and for that stuff only, apply the color word diff

      cur.head.raw <- find_brackets(x@cur.capt)
      tar.head.raw <- find_brackets(x@tar.capt)

      cur.head <- cur.head.raw[cur.head.raw %in% show.range.cur]
      tar.head <- tar.head.raw[tar.head.raw %in% show.range.tar]

      if(length(cur.head) && length(tar.head)) {
        # note we modify `x` here so that subsequent steps can re-use `x` with
        # modifications

        pat <- sprintf("%s\\K.*", .brack.pat)
        cur.body <- regexpr(pat, x@cur.capt[cur.head], perl=TRUE)
        tar.body <- regexpr(pat, x@tar.capt[tar.head], perl=TRUE)

        body.diff <- diff_word(
          regmatches(x@tar.capt[tar.head], tar.body),
          regmatches(x@cur.capt[cur.head], cur.body),
          ignore.white.space=ignore.white.space,
          match.quotes=is.character(x@target) && is.character(x@current),
          max.diffs=max.diffs, tab.stops=tab.stops, diff.mode="wrap"
        )
        regmatches(x@tar.capt[tar.head], tar.body) <- body.diff$target
        regmatches(x@cur.capt[cur.head], cur.body) <- body.diff$current

        # We also need to diff the row headers

        cur.r.h <- regexpr(.brack.pat, x@cur.capt[cur.head], perl=TRUE)
        tar.r.h <- regexpr(.brack.pat, x@tar.capt[tar.head], perl=TRUE)

        cur.r.h.txt <- regmatches(x@cur.capt[cur.head], cur.r.h)
        tar.r.h.txt <- regmatches(x@tar.capt[tar.head], tar.r.h)

        x.r.h <- new(
          "diffObjDiff", tar.capt=tar.r.h.txt, cur.capt=cur.r.h.txt,
          diffs=char_diff(
            tar.r.h.txt, cur.r.h.txt, ignore.white.space=ignore.white.space,
            mode="context", hunk.limit=hunk.limit, line.limit=line.limit,
            disp.width=disp.width, max.diffs=max.diffs.wrap,
            tab.stops=tab.stops, diff.mode="wrap", warn=TRUE
          )
        )
        x.r.h.color <- diff_color(x.r.h@diffs)
        regmatches(x@tar.capt[tar.head], tar.r.h) <- x.r.h.color$A
        regmatches(x@cur.capt[cur.head], cur.r.h) <- x.r.h.color$B

        # Update the character values in the hunks proper

        hunk.grps <- update_hunks(hunk.grps, x@tar.capt, x@cur.capt)

        # Everything else gets a normal hunk by hunk word diff

        cur.rest <- show.range.cur[!show.range.cur %in% cur.head]
        tar.rest <- show.range.tar[!show.range.tar %in% tar.head]
      }
    }
    # Figure out which hunks are still eligible to be word diffed; note we
    # will word-diff even if other hunk is completely missing (most commonly
    # in context mode where we line.limit and the B portion is lost)

    tar.to.wd <- which(ranges[1L,] %in% tar.rest)
    cur.to.wd <- which(ranges[3L,] %in% cur.rest)

    if(length(tar.to.wd) || length(cur.to.wd)) {
      wd.max <- min(head(tar.to.wd, 1L), head(cur.to.wd, 1L), 0L)
      warn.hit.diffs.max <- TRUE

      # We need to compare the +s to the -s, and then reconstruct back into
      # the original A and B vectors

      for(i in seq_along(hunk.grps)) {
        for(j in seq_along(hunk.grps[[i]])) {
          h.a <- hunk.grps[[i]][[j]]
          # Skip context or those that have been wrap diffed
          if(
            h.a$id < wd.max || h.a$context || (!length(h.a$A) && !length(h.a$B))
          ) next
          # Do word diff on each non-context hunk; real messy because the
          # stuff from `tar` and `cur` are mixed in in A and B (well, really
          # only in unified mode) so we have to separate it back out before
          # we do the diff

          A.pos <- which(h.a$A > 0L)
          B.pos <- which(h.a$B > 0L)
          A.neg <- which(h.a$A < 0L)
          B.neg <- which(h.a$B < 0L)

          A.new <- c(h.a$A.chr[A.pos], h.a$B.chr[B.pos])
          B.new <- c(h.a$A.chr[A.neg], h.a$B.chr[B.neg])

          new.diff <- diff_word(
            A.new, B.new, ignore.white.space=ignore.white.space,
            max.diffs=max.diffs.in.hunk, tab.stops=tab.stops,
            diff.mode="hunk", warn=warn.hit.diffs.max
          )
          if(new.diff$hit.diffs.max) warn.hit.diffs.max <- FALSE
          h.a$A.chr[A.pos] <- new.diff$target[seq_along(A.pos)]
          h.a$A.chr[A.neg] <- new.diff$current[seq_along(A.neg)]
          h.a$B.chr[B.pos] <- new.diff$target[seq_along(B.pos) + length(A.pos)]
          h.a$B.chr[B.neg] <- new.diff$current[seq_along(B.neg) + length(A.neg)]

          # Do the same with the versions with all differences removed

          h.a$A.eq.chr[A.pos] <- new.diff$tar.eq[seq_along(A.pos)]
          h.a$A.eq.chr[A.neg] <- new.diff$cur.eq[seq_along(A.neg)]
          h.a$B.eq.chr[B.pos] <-
            new.diff$tar.eq[seq_along(B.pos) + length(A.pos)]
          h.a$B.eq.chr[B.neg] <-
            new.diff$cur.eq[seq_along(B.neg) + length(A.neg)]

          # Update the hunk

          hunk.grps[[i]][[j]] <- h.a
    } } }
    # Process the actual hunks into character

    out <- lapply(
      hunk.grps, hunk_as_char, ranges.orig=ranges.orig,
      mode=mode, disp.width=disp.width, ignore.white.space=ignore.white.space
    )
    # Finalize

    fin <- c(banner, unlist(out), limit.out, str.fold.out, no.diffs)
    attr(fin, "meta") <- trim.meta
    fin
} )
