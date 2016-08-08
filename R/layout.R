# diffobj - Compare R Objects with a Diff
# Copyright (C) 2016  Brodie Gaslam
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# Go to <https://www.r-project.org/Licenses/GPL-3> for a copy of the license.

# Compute all the different gutter components and report max width

gutter_dat <- function(etc) {
  stopifnot(is(etc, "Settings"))
  funs <- etc@style@funs
  text <- etc@style@text

  # get every slot except the pad slot; we'll then augment them so they have
  # all the same number of characters if the style class inherits from
  # Raw, which should be the case for raw, ansi8 and ansi255.  Finally apply
  # functions; note we assume the provided gutter characters don't contain
  # ANSI escapes.  We're a bit sloppy here with how we pull the relevant stuff

  slot.nm <- slotNames(text)
  slots <- slot.nm[grepl("^gutter\\.", slot.nm) & slot.nm != "gutter.pad"]
  gutt.txt <- vapply(slots, slot, character(1L), object=text)
  gutt.dat <- if(is(etc@style, "Raw")) format(gutt.txt) else gutt.txt

  gutt.format.try <- try({
    gutt.dat.format <- vapply(
      slots,
      function(x) funs@gutter(slot(funs, sprintf("%s", x))(gutt.dat[x])),
      character(1L)
    )
    gutt.pad <- funs@gutter(funs@gutter.pad(text@gutter.pad))
  })
  if(inherits(gutt.format.try, "try-error"))
    stop(
      "Failed attempting to apply gutter formatting functions; if you did not ",
      "customize them, contact maintainer.  See `?StyleFuns`."
    )

  names(gutt.dat.format) <- sub("^gutter\\.", "", names(gutt.dat.format))
  nc_fun <- etc@style@nchar.fun
  gutt.max.w <- max(nc_fun(gutt.pad) + nc_fun(gutt.dat.format))
  gutt.args <- c(
    list("Gutter"), as.list(gutt.dat.format),
    list(pad=gutt.pad, width=gutt.max.w)
  )
  do.call("new", gutt.args)
}
# Based on the type of each row in a column, render the correct gutter

render_gutters <- function(types, lens, lens.max, etc) {
  gutter.dat <- etc@gutter
  Map(
    function(dat, lens, lens.max) {
      Map(
        function(type, len, len.max) {
          if(
            type %in% c(
              "insert", "delete", "match", "guide", "fill", "context.sep"
            )
          ) {
            c(
              if(len) slot(gutter.dat, as.character(type)),
              rep(
                slot(gutter.dat, paste0(type, ".", "ctd")), max(len - 1L, 0L)
              ),
              rep(slot(gutter.dat, "fill"), max(len.max - len, 0L))
            )
          } else character(len)
        },
        dat, lens, lens.max
      )
    },
    types, lens, lens.max
  )
}

render_col <- function(gutter, pad, col, type, etc) {
  lens <- vapply(col, length, integer(1L))
  gutt.ul <- unlist(gutter)
  col.txt <- paste0(
    gutt.ul, ifelse(nchar(gutt.ul), unlist(pad), ""), unlist(col)
  )
  type.ul <- unlist(type)
  es <- etc@style@funs

  # line formats

  col.txt[type.ul == "banner.insert"] <-
    es@banner(es@banner.insert(col.txt[type.ul == "banner.insert"]))
  col.txt[type.ul == "banner.delete"] <-
    es@banner(es@banner.delete(col.txt[type.ul == "banner.delete"]))
  col.txt[type.ul == "insert"] <-
    es@line(es@line.insert(col.txt[type.ul == "insert"]))
  col.txt[type.ul == "delete"] <-
    es@line(es@line.delete(col.txt[type.ul == "delete"]))
  col.txt[type.ul == "match"] <-
    es@line(es@line.match(col.txt[type.ul == "match"]))
  col.txt[type.ul == "guide"] <-
    es@line(es@line.guide(col.txt[type.ul == "guide"]))
  col.txt[type.ul == "fill"] <-
    es@line(es@line.fill(col.txt[type.ul == "fill"]))
  col.txt[type.ul == "context.sep"] <-
    es@line(es@context.sep(col.txt[type.ul == "context.sep"]))
  col.txt[type.ul == "header"] <- es@line(col.txt[type.ul == "header"])
  col.txt
}
render_cols <- function(cols, gutters, pads, types, etc) {
  Map(render_col, gutters, pads, cols, types, MoreArgs=list(etc=etc))
}
render_rows <- function(cols, etc) {
  col.txt <- do.call(paste, c(cols, list(sep=etc@style@text@pad.col)))
  etc@style@funs@row(col.txt)
}

# Create a dummy row so we can compute display width for scaling display in
# HTML mode
#
# @param x a `Diff` object

make_dummy_row <- function(x) {
  dummy.text <- paste0(rep("a", x@etc@text.width), collapse="")
  fns <- x@etc@style@funs
  txt <- x@etc@style@text

  txt <- if(x@etc@mode == "sidebyside") {
    sprintf(
      "%s%s%s",
      fns@line(
        fns@line.delete(
          sprintf(
            "%s%s%s", x@etc@gutter@delete, x@etc@gutter@pad,
            fns@text(fns@text.delete(dummy.text))
      ) ) ),
      txt@pad.col,
      fns@line(
      fns@line.insert(
          sprintf(
            "%s%s%s", x@etc@gutter@insert, x@etc@gutter@pad,
            fns@text(fns@text.insert(dummy.text))
    ) ) ) )
  } else {
    fns@line(
      fns@line.delete(
        sprintf(
          "%s%s%s", x@etc@gutter@delete, x@etc@gutter@pad,
          fns@text(fns@text.insert(dummy.text))
    ) ) )
  }
  sprintf(
    "<div id='diffobj_size_meta' style='%s'>%s</div>",
    "display: none; position: absolute; top: 0px; z-index: -1;",
    x@etc@style@funs@container(fns@row(txt))
  )
}
