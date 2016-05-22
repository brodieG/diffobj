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
          if(type %in% c("insert", "delete", "match", "guide")) {
            c(
              if(len) slot(gutter.dat, as.character(type)),
              rep(slot(gutter.dat, paste0(type, ".", "ctd")), max(len - 1L, 0L)),
              rep(slot(gutter.dat, "match"), max(len.max - len, 0L))
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
  type.r <- rep(type, lens)
  gutt.ul <- unlist(gutter)
  col.txt <- paste0(
    gutt.ul, ifelse(nchar(gutt.ul), unlist(pad), ""), unlist(col)
  )
  es <- etc@style@funs

  # line formats

  col.txt[type.r == "banner.insert"] <-
    es@banner(es@banner.insert(col.txt[type.r == "banner.insert"]))
  col.txt[type.r == "banner.delete"] <-
    es@banner(es@banner.delete(col.txt[type.r == "banner.delete"]))
  col.txt[type.r == "insert"] <-
    es@line(es@line.insert(col.txt[type.r == "insert"]))
  col.txt[type.r == "delete"] <-
    es@line(es@line.delete(col.txt[type.r == "delete"]))
  col.txt[type.r == "match"] <-
    es@line(es@line.match(col.txt[type.r == "match"]))
  col.txt[type.r == "guide"] <-
    es@line(es@line.guide(col.txt[type.r == "guide"]))
  col.txt[type.r == "context.sep"] <-
    es@line(es@context.sep(col.txt[type.r == "context.sep"]))
  col.txt[type.r == "header"] <- es@line(col.txt[type.r == "header"])
  col.txt
}
render_cols <- function(cols, gutters, pads, types, etc) {
  Map(render_col, gutters, pads, cols, types, MoreArgs=list(etc=etc))
}
render_rows <- function(cols, etc) {
  col.txt <- do.call(paste, c(cols, list(sep=etc@style@text@pad.col)))
  etc@style@funs@row(col.txt)
}
