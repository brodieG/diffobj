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

hunk_as_char <- function(h.g, ranges.orig, etc) {
  mode <- etc@mode
  disp.width <- etc@disp.width
  ignore.white.space <- etc@ignore.white.space

  # First check that the hunk group hasn't been completely trimmed

  all.lines <- sum(
    unlist(
      lapply(h.g, function(h.a) unlist(h.a[c("tar.rng.trim", "cur.rng.trim")]))
  ) )
  diff.list <- if(!all.lines) {
    list()
  } else {
    max.w <- calc_width(disp.width, mode)
    capt.width <- calc_width_pad(disp.width, mode)
    h.ids <- vapply(h.g, "[[", integer(1L), "id")
    tar.rng <- find_rng(h.ids, ranges.orig[1:2, , drop=FALSE])
    cur.rng <- find_rng(h.ids, ranges.orig[3:4, , drop=FALSE])

    hh.a <- paste0("-", rng_as_chr(tar.rng))
    hh.b <- paste0("+", rng_as_chr(cur.rng))

    hunk.head <- if(!h.g[[1L]]$header) {
      crayon_style(
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
    ) }
    # Generate hunk contents

    hunk.res <- lapply(h.g,
      function(h.a) {
        if(mode=="context") {
          ghd.mode.1 <- "A"
          ghd.mode.2 <- "B"
          ghd.type.1 <- ghd.type.2 <- "both"
          fin_fun <- fin_fun_context
        } else if(mode == "unified") {
          ghd.mode.1 <- ghd.mode.2 <-"A"
          ghd.type.1 <- "pos"
          ghd.type.2 <- "neg"
          fin_fun <- fin_fun_unified
        } else if(mode == "sidebyside") {
          ghd.mode.1 <- "A"
          ghd.mode.2 <- "B"
          ghd.type.1 <- "pos"
          ghd.type.2 <- "neg"
          fin_fun <- fin_fun_sidebyside
        }
        A.dat <- get_hunk_dat(h.a, mode=ghd.mode.1, ghd.type.1)
        B.dat <- get_hunk_dat(h.a, mode=ghd.mode.2, ghd.type.2)
        dat.align <- align_eq(
          A.dat, B.dat, ignore.white.space=etc@ignore.white.space,
          threshold=etc@align.threshold, context=h.a$context
        )
        del.fun <- function(x) etc@style@line(etc@style@line.del(x))
        ins.fun <- function(x) etc@style@line(etc@style@line.ins(x))

        # Wrap, add gutters, and style lines

        A.fin <- wrap_and_sign_pad(
          dat.align$A, capt.width,
          pad.type=if(h.a$context) 1L else 3L,
          wrap.pad=mode == "sidebyside",
          style=if(h.a$context) etc@style@line else del.fun
        )
        B.fin <- wrap_and_sign_pad(
          dat.align$B, capt.width,
          pad.type=if(h.a$context) 1L else 2L,
          wrap.pad=mode == "sidebyside",
          style=if(h.a$context) etc@style@line else ins.fun
        )
        fin_fun(A.fin, B.fin, max.w)
    } )
    c(hunk.head, hunk.res)
  }
  # Context mode is a bit weird because we need to re-order across atomic hunks
  # unlike with the other modes

  res <- if(mode == "context") {
    ctx.A <- unlist(lapply(diff.list, "[[", 1L))
    ctx.B <- unlist(lapply(diff.list, "[[", 2L))
    unlist(c(ctx.A, if(length(ctx.B)) crayon_style("~~~~", "silver"), ctx.B))
  } else {
    unlist(diff.list)
  }
  res
}
# helper for in_hunk_diffs
#
# `nd` stands for new.diff.  Here we are just remapping the word diff data
# from tar/cur to our A/B data.

update_hunk_atom <- function(h.a, new.diff, A.pos, A.neg, B.pos, B.neg, ind) {
  A.ind <- sprintf("A.%s", ind)
  B.ind <- sprintf("B.%s", ind)
  tar.ind <- sprintf("tar.%s", ind)
  cur.ind <- sprintf("cur.%s", ind)

  h.a[[A.ind]][A.pos] <- new.diff[[tar.ind]][seq_along(A.pos)]
  h.a[[A.ind]][A.neg] <- new.diff[[cur.ind]][seq_along(A.neg)]

  h.a[[B.ind]][B.pos] <- new.diff[[tar.ind]][seq_along(B.pos)]
  h.a[[B.ind]][B.neg] <- new.diff[[cur.ind]][seq_along(B.neg)]

  h.a
}
# Compute diffs in hunks

in_hunk_diffs <- function(hunk.grps, etc, tar.to.wd, cur.to.wd) {
  if(length(tar.to.wd) || length(cur.to.wd)) {
    wd.max <- min(head(tar.to.wd, 1L), head(cur.to.wd, 1L), 0L)
    warn.hit.diffs.max <- TRUE

    # We need to compare the +s to the -s, and then reconstruct back into
    # the original A and B vectors

    for(i in seq_along(hunk.grps)) {
      for(j in seq_along(hunk.grps[[i]])) {
        h.a <- hunk.grps[[i]][[j]]
        # Skip context or those that have been wrap diffed or non-context
        # hunks that are being trimmed
        if(
          h.a$id < wd.max || h.a$context ||
          (!length(h.a$A) && !length(h.a$B))
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
          A.new, B.new, etc=etc, diff.mode="hunk",
          warn=warn.hit.diffs.max
        )
        if(new.diff$hit.diffs.max) warn.hit.diffs.max <- FALSE

        # Update hunk atom with the word diff info; we need to remap from
        # tar/cur back to A/B

        ind.sub <- c("chr", "eq.chr", "tok.ratio")
        for(k in seq_along(ind.sub)) {
          h.a <- update_hunk_atom(
            h.a, new.diff, A.pos, A.neg, B.pos, B.neg, ind.sub[[k]]
        ) }
        # Update the hunk group with the modified hunk atom

        hunk.grps[[i]][[j]] <- h.a
  } } }
  hunk.grps
}
# Helper functions for 'as.character'

# Get trimmed character ranges; positives are originally from target, and
# negatives from current

.valid_sub <- c("chr", "eq.chr", "raw.chr", "tok.ratio")
get_hunk_dat <- function(h.a, mode, type="both", sub=.valid_sub) {
  stopifnot(
    mode %in% LETTERS[1:2], length(mode) == 1L,
    is.character(sub), !anyNA(sub),
    all(sub %in% .valid_sub),
    is.chr.1L(type), type %in% c("both", "pos", "neg")
  )
  rng <- c(
    if(type %in% c("pos", "both"))
      seq(h.a$tar.rng.trim[[1L]], h.a$tar.rng.trim[[2L]]),
    if(type %in% c("neg", "both"))
      -seq(h.a$cur.rng.trim[[1L]], h.a$cur.rng.trim[[2L]])
  )
  chr.ind <- sprintf("%s.%s", mode, sub)
  setNames(
    lapply(h.a[chr.ind], function(x) x[match(rng, h.a[[mode]], nomatch=0L)]),
    sub
  )
}
fin_fun_context <- function(A, B, max.w) list(A, B)

fin_fun_unified <- function(A, B, max.w)
  c(A, B)[order(c(seq_along(A), seq_along(B)))]

fin_fun_sidebyside <- function(A, B, max.w) {
  for(i in seq_along(A)) {
    A.ch <- A[[i]]
    B.ch <- B[[i]]
    A.l <- length(A.ch)
    B.l <- length(B.ch)
    max.l <- max(A.l, B.l)
    length(A.ch) <- length(B.ch) <- max.l
    blanks <- paste0(rep(" ", max.w), collapse="")

    for(j in seq_along(A.ch)) {
      l.diff <- length(A.ch[[j]]) - length(B.ch[[j]])
      if(l.diff < 0L)
      A.ch[[j]] <- c(A.ch[[j]], rep(blanks, abs(l.diff)))
      if(l.diff > 0L)
      B.ch[[j]] <- c(B.ch[[j]], rep(blanks, l.diff))
    }
    A[[i]] <- A.ch
    B[[i]] <- B.ch
  }
  paste0(unlist(A), "  ", unlist(B))
}

#' @rdname diffobj_s4method_doc

setMethod("as.character", "diffObjDiff",
  function(x, ...) {
    old.crayon.opt <- options(crayon.enabled=x@etc@use.ansi)
    on.exit(options(old.crayon.opt), add=TRUE)

    # These checks should never fail since presumably the inputs have been
    # checked earlier; here just in case we mess something up in devel or
    # testing

    hunk.limit <- x@etc@hunk.limit
    line.limit <- x@etc@line.limit
    hunk.limit <- x@etc@hunk.limit
    disp.width <- x@etc@disp.width
    max.diffs <- x@etc@max.diffs
    max.diffs.in.hunk <- x@etc@max.diffs.in.hunk
    max.diffs.wrap <- x@etc@max.diffs.wrap
    mode <- x@etc@mode
    tab.stops <- x@etc@tab.stops
    ignore.white.space <- x@etc@ignore.white.space

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

    line.limit.a <- if(line.limit[[1L]] >= 0L)
      pmax(integer(2L), line.limit - banner.len) else line.limit

    # Trim hunks to the extent need to make sure we fit in lines

    hunk.grps <- trim_hunks(x@diffs$hunks, x@etc)
    hunks.flat <- unlist(hunk.grps, recursive=FALSE)

    # Make the object banner and compute more detailed widths post trim

    tar.banner <- if(!is.null(x@etc@tar.banner)) x@etc@tar.banner else
      deparse(x@etc@tar.exp)[[1L]]
    cur.banner <- if(!is.null(x@etc@cur.banner)) x@etc@cur.banner else
      deparse(x@etc@cur.exp)[[1L]]
    banner.A <- paste0("--- ", tar.banner)
    banner.B <- paste0("+++ ", cur.banner)

    if(mode == "sidebyside") {
      # If side by side we want stuff close together if reasonable
      max.col.w <- calc_width_unpad(
        max(
          unlist(lapply(hunks.flat, function(x) nchar(c(x$A.chr, x$B.chr))))
        ), mode=mode
      )
      max.w <- if(max.col.w < max.w) max(.min.width, max.col.w) else max.w
      # future calculations should assume narrower display
      x@etc@disp.width <- disp.width <- max.w * 2L + 2L
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
    # Trim banner if exceeds line limit, and adjust line limit for banner size;
    # note we add back

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
      hunks.flat, function(h.a) c(h.a$tar.rng.trim, h.a$cur.rng.trim),
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
          x@etc, diff.mode="wrap",
          match.quotes=is.character(x@target) && is.character(x@current)
        )
        regmatches(x@tar.capt[tar.head], tar.body) <- body.diff$tar.chr
        regmatches(x@cur.capt[cur.head], cur.body) <- body.diff$cur.chr

        # We also need to diff the row headers

        cur.r.h <- regexpr(.brack.pat, x@cur.capt[cur.head], perl=TRUE)
        tar.r.h <- regexpr(.brack.pat, x@tar.capt[tar.head], perl=TRUE)

        cur.r.h.txt <- regmatches(x@cur.capt[cur.head], cur.r.h)
        tar.r.h.txt <- regmatches(x@tar.capt[tar.head], tar.r.h)

        word.set <- x@etc
        word.set@line.limit <- -1L
        word.set@mode <- "context"

        x.r.h <- new(
          "diffObjDiff", tar.capt=tar.r.h.txt, cur.capt=cur.r.h.txt,
          diffs=char_diff(
            tar.r.h.txt, cur.r.h.txt, etc=word.set, diff.mode="wrap",
            warn=TRUE
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
    hunk.grps <-
      in_hunk_diffs(hunk.grps=hunk.grps, etc=x@etc, tar.to.wd, cur.to.wd)

    # Process the actual hunks into character

    out <- lapply(
      hunk.grps, hunk_as_char, ranges.orig=ranges.orig, etc=x@etc
    )
    # Finalize

    fin <- c(banner, unlist(out), limit.out, str.fold.out, no.diffs)
    attr(fin, "meta") <- trim.meta
    fin
} )
