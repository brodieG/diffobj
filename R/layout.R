# Compute all the different gutter components and report max width

gutter_dat <- function(etc) {
  stopifnot(is(etc, "diffObjSettings"))
  es <- etc@style

  gutt.insert <- es@gutter(es@gutter.insert(es@gutter.insert.txt))
  gutt.insert.ctd <- es@gutter(es@gutter.insert.ctd(es@gutter.insert.ctd.txt))
  gutt.delete <- es@gutter(es@gutter.delete(es@gutter.delete.txt))
  gutt.delete.ctd <- es@gutter(es@gutter.delete.ctd(es@gutter.delete.ctd.txt))
  gutt.match <- es@gutter(es@gutter.match(es@gutter.match.txt))
  gutt.match.ctd <- es@gutter(es@gutter.match.ctd(es@gutter.match.ctd.txt))

  gutt.pad <- es@gutter(es@gutter.pad(es@gutter.pad.txt))

  gutt.max.w <- max(
    nchar(
      c(
        gutt.insert, gutt.insert.ctd, gutt.delete, gutt.delete.ctd, gutt.match,
        gutt.match.ctd, gutt.pad
  ) ) )
  new(
    "diffObjGutter",
    insert=gutt.insert, insert.ctd=gutt.insert.ctd, delete=gutt.delete,
    delete.ctd=gutt.delete.ctd, match=gutt.match, match.ctd=gutt.match.ctd,
    pad=gutt.pad, width=gutt.max.w
  )
}
# Based on the type of each row in a column, render the correct gutter

render_gutters <- function(cols, lens, etc) {
  gutter.dat <- etc@gutter
  Map(
    function(dat, lens) {
      Map(
        function(type, len) {
          if(type %in% c("insert", "delete", "match")) c(
            if(len) slot(gutter.dat, as.character(type)),
            rep(slot(gutter.dat, paste0(type, ".", "ctd")), max(len - 1L, 0L))
          ) else character(len)
        },
        dat$type, lens
      )
    },
    cols, lens
  )
}
row_ascii <- function(gutter, pad, text)
  if(anyNA(gutter)) text else paste0(gutter, pad, text)

cols_ascii <- function(gutter, pad, col, type, etc) {
  stopifnot(is(etc, "diffObjSettings"))
  lens <- vapply(col, length, integer(1L))
  type.r <- rep(type, lens)
  gutt.ul <- unlist(gutter)
  col.txt <- paste0(
    gutt.ul, ifelse(nchar(gutt.ul), unlist(pad), ""), unlist(col)
  )
  es <- etc@style

  col.txt[type.r == "insert"] <-
    es@line(es@line.insert(col.txt[type.r == "insert"]))
  col.txt[type.r == "delete"] <-
    es@line(es@line.delete(col.txt[type.r == "delete"]))
  col.txt[type.r == "match"] <-
    es@line(es@line.match(col.txt[type.r == "match"]))
  col.txt[type.r == "context.sep"] <-
    es@line(es@context.sep(col.txt[type.r == "context.sep"]))
  col.txt[type.r == "header"] <- es@line(es@header(col.txt[type.r == "header"]))
  col.txt
}
table_ascii <- function(
  banner.ins, banner.del, gutters, pads, cols, types, etc
) {
  if(etc@mode == "sidebyside") {
    banner.del <- rpad(banner.del, etc@text.width)
    banner.ins <- rpad(banner.ins, etc@text.width)
  }
  banner.del <- cols_ascii(
    etc@gutter@delete, etc@gutter@pad, banner.del, type="delete", etc
  )
  banner.ins <- cols_ascii(
    etc@gutter@insert, etc@gutter@pad, banner.ins, type="insert", etc
  )
  cols.proc <- Map(
    cols_ascii, gutters, pads, cols, types, MoreArgs=list(etc=etc)
  )
  if(etc@mode == "sidebyside") {
    paste0(
      c(banner.del, unlist(cols.proc[[1L]])),
      c(banner.ins, unlist(cols.proc[[2L]]))
    )
  } else {
    c(banner.del, banner.ins, unlist(cols.proc[[1L]]))
  }
}
