#' @include s4.R

setClass("DiffSummary",
  slots=c(
    max.lines="integer", width="integer", style="Style",
    diffs="matrix", all.eq="character",
    scale.threshold="numeric"
  ),
  validity=function(object) {
    if(
      !is.integer(object@diffs) &&
      !identical(rownames(object@diffs), c("match", "delete", "add"))
    )
      return("Invalid diffs object")
    TRUE
  }
)

#' @export

setMethod("summary", "Diff",
  function(
    object, scale.threshold=0.1, max.lines=50L, width=getOption("width"), ...
  ) {
    if(!is.int.1L(max.lines) || max.lines < 1L)
      stop("Argument `max.lines` must be integer(1L) and strictly positive")
    max.lines <- as.integer(max.lines)
    if(!is.int.1L(width) || width < 0L)
      stop("Argument `width` must be integer(1L) and positive")
    if(width < 10L) width <- 10L
    if(
      !is.numeric(scale.threshold) || length(scale.threshold) != 1L ||
      is.na(scale.threshold) || !scale.threshold %bw% c(0, 1)
    )
      stop("Argument `scale.threshold` must be numeric(1L) between 0 and 1")

    diffs.c <- count_diffs_detail(object@diffs)
    # remove context hunks that are duplicated
    match.seq <- rle(!!diffs.c["match", ])
    match.keep <- unlist(
      lapply(
        match.seq$lengths,
        function(x) if(x == 2L) c(TRUE, FALSE) else TRUE
    ) )
    diffs <- diffs.c[, match.keep, drop=FALSE]
    all.eq <- all.equal(object@target, object@current)
    new(
      "DiffSummary", max.lines=max.lines, width=width, style=object@etc@style,
      diffs=diffs, all.eq=if(isTRUE(all.eq)) character(0L) else all.eq,
      scale.threshold=scale.threshold
    )
  }
)
#' @export

setMethod("as.character", "DiffSummary",
  function(x, ...) {
    hunks <- sum(!x@diffs["match", ])
    res <- c(apply(x@diffs, 1L, sum))
    scale.threshold <- x@scale.threshold

    res <- if(!hunks) {
      if(length(x@all.eq)) {
        eq.txt <- paste0("- ", x@all.eq)
        c("No visible differences, but objects are not `all.equal`:", eq.txt)
      } else {
        "Objects are `all.equal`\n"
      }
    } else {
      head <- sprintf(
        paste0(
           "Found differences in %d hunks:\n  %d insertions, %d deletions, ",
           "%d matches (lines)\n"
        ),
        hunks, res[["add"]], res[["delete"]], res[["match"]]
      )
      # Compute character screen display

      pad <- 2L
      width <- x@width - pad

      max.chars <- x@max.lines * width
      diffs <- x@diffs
      scale.threshold <- x@scale.threshold

      # Helper fun to determine if the scale skewed our data too much

      scale_err <- function(orig, scaled, threshold, width) {
        if((width - sum(scaled)) / width > threshold) {
          TRUE
        } else {
          zeroes <- !orig
          orig.nz <- orig[!zeroes]
          scaled.nz <- scaled[!zeroes]
          orig.norm <- orig.nz / max(orig.nz)
          scaled.norm <- scaled.nz / max(scaled.nz)
          any(abs(orig.norm - scaled.norm) > threshold)
        }
      }
      # Scale the data down as small as possible provided we don't violate
      # tolerance.

      diffs.gz <- diffs > 1L
      diffs.nz <- diffs[diffs.gz]
      safety <- 10000L
      tol <- width / 4
      diffs.scale <- diffs

      lo.bound <- lo <- length(diffs.nz)
      hi.bound <- hi <- sum(diffs.nz)

      if(sum(diffs.scale) > width) {
        repeat {
          mp <- round((hi.bound - lo.bound) / 2) + lo.bound
          safety <- safety - 1L
          if(safety < 0L)
            stop("Logic Error: likely infinite loop; contact maintainer.")

          # Need to scale down; we know we need at least one char per value
          diffs.nz.s <- pmax(
            round(diffs.nz * (mp - lo) / (hi - lo)), 1L
          )
          diffs.scale[diffs.gz] <- diffs.nz.s
          scale.err <- scale_err(diffs, diffs.scale, scale.threshold, width)
          break.cond <- floor(mp / width) <= floor(lo.bound  / width) ||
            mp >= hi.bound

          if(scale.err) {
            # error, keep increasing lines
            lo.bound <- mp
          } else {
            # no error, check if we can generate an error with a smaller value
            # note hi.bound is always guaranteed to not produce error
            if(break.cond) break
            hi.bound <- mp
          }
        }
      }
      diffs.fin <- diffs.scale

      # Compute scaling factors for display to user

      scale.one <- diffs.scale == 1
      scale.gt.one <- diffs.scale > 1
      s.o.txt <- if(any(scale.one)) {
        s.o.r <- unique(range(diffs[scale.one]))
        if(length(s.o.r) == 1L)
          sprintf("%d:1 for single chars", s.o.r)
        else
          sprintf("%d-%d:1 for single chars", s.o.r[1L], s.o.r[2L])
      }

      s.gt.o.txt <- if(any(scale.gt.one)) {
        s.gt.o.r <- unique(
          range(round(diffs[scale.gt.one] / diffs.scale[scale.gt.one]))
        )
        if(length(s.gt.o.r) == 1L)
          sprintf("%d:1 for char seqs", s.gt.o.r)
        else
          sprintf("%d-%d:1 for char seqs", s.gt.o.r[1L], s.gt.o.r[2L])
      }

      map.txt <- sprintf(
        "Diff map (line:char scale is %s%s%s):\n",
        if(!is.null(s.o.txt)) s.o.txt else "",
        if(is.null(s.o.txt) && !is.null(s.gt.o.txt)) "" else ", ",
        if(!is.null(s.gt.o.txt)) s.gt.o.txt else ""
      )
      body <- strwrap(map.txt, width=x@width)

      # Render actual map

      diffs.txt <- character(length(diffs.fin))
      attributes(diffs.txt) <- attributes(diffs.fin)
      symb <- c(match=".", add="I", delete="D")
      use.ansi <- FALSE

      for(i in names(symb)) {
        test <- diffs.txt[i, ] <- vapply(
          diffs.fin[i, ],
          function(x) paste0(rep(symb[[i]], x), collapse=""),
          character(1L)
        )
      }
      # Trim text down to what is displayable in the allowed lines

      txt <- do.call(paste0, as.list(c(diffs.txt)))
      txt <- substr(txt, 1, max.chars)
      txt.w <- unlist(wrap(txt, width))

      # Apply ansi styles if warranted

      if(is(x@style, "StyleAnsi")) {
        old.crayon.opt <-
          options(crayon.enabled=is(x@style, "StyleAnsi"))
        on.exit(options(old.crayon.opt), add=TRUE)
        s.f <- x@style@funs
        txt.w <- gsub(
          symb[["match"]], s.f@text.match(symb[["match"]]),
          gsub(
            symb[["add"]],
            s.f@text.insert(s.f@word.insert(symb[["add"]])),
            gsub(
              symb[["delete"]],
              s.f@text.delete(s.f@word.delete(symb[["delete"]])),
              txt.w, fixed=TRUE
            ),
            fixed=TRUE
          ),
          fixed=TRUE
        )
      }
      extra <- if(sum(diffs.fin) > max.chars) {
        diffs.omitted <- diffs.fin
        diffs.under <- cumsum(diffs.omitted) <= max.chars
        diffs.omitted[diffs.under] <- 0L
        res.om <- apply(diffs.omitted, 1L, sum)
        sprintf(
          paste0(
            "omitting %d deletion%s, %d insertion%s, and %d matche%s; ",
            "increase `max.lines` to %d to show full map"
          ),
          res.om[["delete"]], if(res.om[["delete"]] != 1L) "s" else "",
          res.om[["add"]], if(res.om[["add"]] != 1L) "s" else "",
          res.om[["match"]], if(res.om[["match"]] != 1L) "s" else "",
          ceiling(sum(diffs.scale) / width)
        )
      } else character(0L)

      map <- paste0("  ", txt.w)
      if(length(extra))
        extra <- strwrap(extra, indent=2L, exdent=2L, width=width)
      c(head, body, map, extra)
    }
    c("", res, "")
  }
)
#' @export

setMethod("show", "DiffSummary",
  function(object) {
    cat(as.character(object), sep="\n")
    invisible(NULL)
  }
)
