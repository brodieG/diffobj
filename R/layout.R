# Compute all the different gutter components and report max width

gutter_dat <- function(etc) {
  stopifnot(is(etc, "Settings"))
  funs <- etc@style@funs
  text <- etc@style@text

  gutt.insert <- funs@gutter(funs@gutter.insert(text@gutter.insert))
  gutt.insert.ctd <- funs@gutter(funs@gutter.insert.ctd(text@gutter.insert.ctd))
  gutt.delete <- funs@gutter(funs@gutter.delete(text@gutter.delete))
  gutt.delete.ctd <- funs@gutter(funs@gutter.delete.ctd(text@gutter.delete.ctd))
  gutt.match <- funs@gutter(funs@gutter.match(text@gutter.match))
  gutt.match.ctd <- funs@gutter(funs@gutter.match.ctd(text@gutter.match.ctd))
  gutt.guide <- funs@gutter(funs@gutter.guide(text@gutter.guide))
  gutt.guide.ctd <- funs@gutter(funs@gutter.guide.ctd(text@gutter.guide.ctd))
  gutt.fill <- funs@gutter(funs@gutter.fill(text@gutter.fill))
  gutt.fill.ctd <- funs@gutter(funs@gutter.fill.ctd(text@gutter.fill.ctd))

  gutt.pad <- funs@gutter(funs@gutter.pad(text@gutter.pad))
  nc_fun <- if(is(etc@style, "StyleAnsi")) crayon_nchar else nchar

  gutt.max.w <- max(
    nc_fun(gutt.pad) + nc_fun(
      c(
        gutt.insert, gutt.insert.ctd, gutt.delete, gutt.delete.ctd, gutt.match,
        gutt.match.ctd
  ) ) )
  new(
    "Gutter",
    insert=gutt.insert, insert.ctd=gutt.insert.ctd, delete=gutt.delete,
    delete.ctd=gutt.delete.ctd, match=gutt.match, match.ctd=gutt.match.ctd,
    guide=gutt.guide, guide.ctd=gutt.guide.ctd,
    fill=gutt.fill, fill.ctd=gutt.fill.ctd,
    pad=gutt.pad, width=gutt.max.w
  )
}
# Based on the type of each row in a column, render the correct gutter

render_gutters <- function(types, lens, lens.max, etc) {
  gutter.dat <- etc@gutter
  Map(
    function(dat, lens, lens.max) {
      Map(
        function(type, len, len.max) {
          if(type %in% c("insert", "delete", "match", "guide", "fill")) {
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
  es <- etc@style@funs

  # line formats

  col.txt[type == "banner.insert"] <-
    es@banner(es@banner.insert(col.txt[type == "banner.insert"]))
  col.txt[type == "banner.delete"] <-
    es@banner(es@banner.delete(col.txt[type == "banner.delete"]))
  col.txt[type == "insert"] <-
    es@line(es@line.insert(col.txt[type == "insert"]))
  col.txt[type == "delete"] <-
    es@line(es@line.delete(col.txt[type == "delete"]))
  col.txt[type == "match"] <-
    es@line(es@line.match(col.txt[type == "match"]))
  col.txt[type == "guide"] <-
    es@line(es@line.guide(col.txt[type == "guide"]))
  col.txt[type == "pad"] <-
    es@line(es@line.guide(col.txt[type == "pad"]))
  col.txt[type == "context.sep"] <-
    es@line(es@context.sep(col.txt[type == "context.sep"]))
  col.txt[type == "header"] <- es@line(col.txt[type == "header"])
  col.txt
}
render_cols <- function(cols, gutters, pads, types, etc) {
  Map(render_col, gutters, pads, cols, types, MoreArgs=list(etc=etc))
}
render_rows <- function(cols, etc) {
  col.txt <- do.call(paste, c(cols, list(sep=etc@style@text@pad.col)))
  etc@style@funs@row(col.txt)
}
