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
# Finalization function should return a list with two character vectors for
# diff contents, and two factor vectors denoting the type of content for
# each of the character vectors where valid data types are ins, del, mtc, hdr,
# ctx; chrt is just a helper function to generate factors with those possible
# values

chrt <- function(...)
  factor(
    c(...),
    levels=c("insert", "delete", "match", "header", "context.sep")
  )
hunkl <- function(col.1=NULL, col.2=NULL, type.1=NULL, type.2=NULL)
  c(
    list(list(dat=col.1, type=type.1)),
    if(!is.null(col.2)) list(list(dat=col.2, type=type.2))
  )

fin_fun_context <- function(A, B, context, max.w) {
  A.ul <- unlist(A)
  B.ul <- unlist(B)
  hunkl(
    col.1=c(A.ul, if(context) NA, B.ul),
    type.1=chrt(
      rep(if(context) "match" else "delete", length(A.ul)),
      if(context) "context.sep",
      rep(if(context) "match" else "insert", length(B.ul))
    ),
  )
}
fin_fun_unified <- function(A, B, context, max.w) {
  ord <- order(c(seq_along(A), seq_along(B)))
  types <- c(
    lapply(A, function(x) rep(if(context) "match" else "delete", length(x))),
    lapply(B, function(x) rep(if(context) "match" else "insert", length(x)))
  )
  hunkl(
    col.1=unlist(c(A, B)[ord]),
    type.1=chrt(unlist(types[ord]))
  )
}
fin_fun_sidebyside <- function(A, B, context, max.w) {
  for(i in seq_along(A)) {
    A.ch <- A[[i]]
    B.ch <- B[[i]]
    A.l <- length(A.ch)
    B.l <- length(B.ch)
    max.l <- max(A.l, B.l)
    length(A.ch) <- length(B.ch) <- max.l

    A[[i]] <- A.ch
    B[[i]] <- B.ch
  }
  A.ul <- unlist(A)
  B.ul <- unlist(B)
  hunkl(
    col.1=ifelse(is.na(A.ul), "", A.ul),
    col.2=ifelse(is.na(B.ul), "", B.ul),
    type.1=chrt(ifelse(context | is.na(A.ul), "match", "delete")),
    type.2=chrt(ifelse(context | is.na(B.ul), "match", "insert"))
  )
}
# Convert a hunk group into text representation

hunk_atom_as_char <- function(h.a, mode, etc) {
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

  # Align the lines accounting for partial matching post word-diff,
  # each diff style has a different finalization function

  dat.align <- align_eq(
    A.dat, B.dat, ignore.white.space=etc@ignore.white.space,
    threshold=etc@align.threshold
  )
  fin_fun(dat.align$A, dat.align$B, h.a$context, max.w=etc@text.width)
}

hunk_as_char <- function(h.g, ranges.orig, etc) {
  mode <- etc@mode
  disp.width <- etc@disp.width
  ignore.white.space <- etc@ignore.white.space

  # First check that the hunk group hasn't been completely trimmed

  all.lines <- sum(
    unlist(
      lapply(h.g, function(h.a) unlist(h.a[c("tar.rng.trim", "cur.rng.trim")]))
  ) )
  if(!all.lines) {
    list(hunkl())
  } else {
    max.w <- calc_width(disp.width, mode)
    capt.width <- calc_width_pad(disp.width, mode)
    h.ids <- vapply(h.g, "[[", integer(1L), "id")
    tar.rng <- find_rng(h.ids, ranges.orig[1:2, , drop=FALSE])
    cur.rng <- find_rng(h.ids, ranges.orig[3:4, , drop=FALSE])

    hh.a <- paste0("-", rng_as_chr(tar.rng))
    hh.b <- paste0("+", rng_as_chr(cur.rng))

    hunk.head <- list(
      if(!h.g[[1L]]$header) {
        if(mode == "sidebyside") {
          hunkl(
            col.1=sprintf("@@ %s @@", hh.a), col.2=sprintf("@@ %s @@", hh.b),
            type.1=chrt("header"), type.2=chrt("header")
          )
        } else {
          hunkl(col.1=sprintf("@@ %s %s @@", hh.a, hh.b), type.1=chrt("header"))
        }
      } else hunkl()
    )
    # Generate hunk contents in pre-rendered form.  These will be lists with
    # four vectors.

    hunk.res <- lapply(h.g, hunk_atom_as_char, mode=mode, etc=etc)

    # Add header and return; this a list of lists, though all sub-lists should
    # have same format

    c(hunk.head, hunk.res)
  }
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
      res <- x@etc@style@meta(msg)
    }
    # Basic width computation and banner size; start by computing gutter so we
    # can figure out what's left

    gutter.dat <- x@etc@gutter
    banner.len <- banner_len(mode)
    max.w <- x@etc@text.width

    line.limit.a <- if(line.limit[[1L]] >= 0L)
      pmax(integer(2L), line.limit - banner.len) else line.limit

    # Trim hunks to the extent need to make sure we fit in lines

    hunk.grps <- trim_hunks(x@diffs$hunks, x@etc)
    hunks.flat <- unlist(hunk.grps, recursive=FALSE)

    if(mode == "sidebyside") {
      # If side by side we want stuff close together if reasonable

      chr.dat <- unlist(lapply(hunks.flat, "[", c("A.chr", "B.chr")))
      chr.size <- integer(length(chr.dat))
      is.ansi <- x@etc@use.ansi & grepl(ansi_regex, chr.dat, perl=TRUE)
      if(any(is.ansi)) chr.size[is.ansi] <- crayon_nchar(chr.dat)
      chr.size[!is.ansi] <- nchar(chr.dat)
      max.col.w <- max(0L, chr.size, .min.width) + gutter.dat@width
      max.w <- if(max.col.w < max.w) max.col.w else max.w

      # future calculations should assume narrower display

      x@etc@line.width <- max.w
      x@etc@text.width <- max.w - gutter.dat@width
    }
    # Make the object banner and compute more detailed widths post trim

    tar.banner <- if(!is.null(x@etc@tar.banner)) x@etc@tar.banner else
      deparse(x@etc@tar.exp)[[1L]]
    cur.banner <- if(!is.null(x@etc@cur.banner)) x@etc@cur.banner else
      deparse(x@etc@cur.exp)[[1L]]
    banner.A <- x@etc@style@word.delete(chr_trim(tar.banner, x@etc@text.width))
    banner.B <- x@etc@style@word.insert(chr_trim(cur.banner, x@etc@text.width))

    # Trim banner if exceeds line limit, currently we're implicitly assuming
    # that each banner line does not exceed 1 in length; may change in future

    if(line.limit[[1L]] >= 0) {
      if(line.limit[[2L]] < 2L && mode != "sidebyside") banner.A <- NULL
      if(line.limit[[2L]] < 1L) banner.B <- banner.A <- NULL
    }
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
      x@etc@style@meta(
        paste0(
          "... omitted ",
          if(ll) sprintf("%d/%d lines", lim.line[[1L]], lim.line[[2L]]),
          if(ll && lh) ", ",
          if(lh) sprintf("%d/%d hunks", lim.hunk[[1L]], lim.hunk[[2L]])
      ) )
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

    cur.body <- tar.body <- character(0L)
    cur.rest <- show.range.cur <- seq_len(cur.max)
    tar.rest <- show.range.tar <- seq_len(tar.max)

    # Figure out which hunks are still eligible to be word diffed; note we
    # will word-diff even if other hunk is completely missing (most commonly
    # in context mode where we line.limit and the B portion is lost); this is
    # legacy from when we used to do the wrap diff and can probably be
    # simplified since we will do this for every hunk

    tar.to.wd <- which(ranges[1L,] %in% tar.rest)
    cur.to.wd <- which(ranges[3L,] %in% cur.rest)
    hunk.grps.d <-
      in_hunk_diffs(hunk.grps=hunk.grps, etc=x@etc, tar.to.wd, cur.to.wd)

    # Generate the pre-rendered hunk data as text columns; a bit complicated
    # as we need to unnest stuff; use rbind to make it a little easier.

    pre.render.raw <- unlist(
      lapply(hunk.grps.d, hunk_as_char, ranges.orig=ranges.orig, etc=x@etc),
      recursive=FALSE
    )
    pre.render.mx <- do.call(rbind, pre.render.raw)
    pre.render.mx.2 <- lapply(
      split(pre.render.mx, col(pre.render.mx)), do.call, what="rbind"
    )
    pre.render <- lapply(
      unname(pre.render.mx.2),
      function(mx) list(
        dat=unlist(mx[, 1L]),
        type=unlist(mx[, 2L], recursive=FALSE)
    ) )
    # Apply text level styles

    es <- x@etc@style
    pre.render.s <- lapply(
      pre.render,
      function(z) {
        res <- character(length(z$dat))
        res[z$type == "insert"] <-
          es@text(es@text.insert(z$dat[z$type == "insert"]))
        res[z$type == "delete"] <-
          es@text(es@text.delete(z$dat[z$type == "delete"]))
        res[z$type == "match"] <-
          es@text(es@text.match(z$dat[z$type == "match"]))
        res[z$type == "header"] <- es@header(z$dat[z$type == "header"])
        if(mode == "context") {
          ctx.sep <- es@context.sep(es@context.sep.txt)
          res[z$type == "context.sep"] <- ctx.sep
        }
        res
      }
    )
    # Generate wrapped version of the text; if in sidebyside, make sure that
    # all elements are same length

    pre.render.w <- replicate(
      2L, vector("list", length(pre.render.s[[1L]])), simplify=FALSE
    )
    for(i in seq_along(pre.render.s)) {
      hdr <- pre.render[[i]]$type == "header"
      pre.render.w[[i]][hdr] <- wrap(pre.render.s[[i]][hdr], x@etc@line.width)
      pre.render.w[[i]][!hdr] <- wrap(pre.render.s[[i]][!hdr], x@etc@text.width)
    }
    line.lens <- lapply(pre.render.w, vapply, length, integer(1L))
    types <- lapply(pre.render, "[[", "type")

    if(mode == "sidebyside") {
      line.lens.max <- replicate(2L, do.call(pmax, line.lens), simplify=FALSE)
      pre.render.w <- lapply(
        pre.render.w, function(y) {
          Map(
            function(dat, len) {
              length(dat) <- len
              dat[is.na(dat)] <- ""
              dat
            },
            y, line.lens.max[[1L]]
    ) } ) }
    # Compute gutter, padding, and continuations

    pads <- lapply(
      line.lens, function(y) lapply(y, rep, x=gutter.dat@pad)
    )
    gutters <- render_gutters(
      cols=pre.render, lens=line.lens, lens.max=line.lens.max, etc=x@etc
    )

    # Apply layout; output should be character with each element representing
    # a row of output

    out <- table_ascii(
      banner.del=banner.A, banner.ins=banner.B, cols=pre.render.w,
      gutters=gutters, pads=pads, types=types, etc=x@etc
    )
    # Finalize

    fin <- c(unlist(out), limit.out, str.fold.out, no.diffs)
    attr(fin, "meta") <- trim.meta
    fin
} )
