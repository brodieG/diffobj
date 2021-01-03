library(diffobj)
no.null.opts <- c(
  "warnPartialMatchArgs", "warnPartialMatchAttr", "warnPartialMatchDollar"
)
no.null.opt.list <- Map(getOption, no.null.opts)
no.null.nulls <- vapply(no.null.opt.list, is.null, logical(1L))
no.null.opt.list[no.null.nulls] <- FALSE
all.opts <- c(
  list(
    useFancyQuotes=FALSE,   # all.equals uses fancy quotes
    diffobj.format="ansi8", # force ANSI colors
    diffobj.color.mode="yb",# force yb
    diffobj.pager="off",    # run tests without pager
    width=80L,
    encoding="UTF-8"        # so Gabor's name renders properly on win...
  )
)
OLD.OPTS <- options(c(diffobj_set_def_opts(), all.opts))
options(
  warnPartialMatchArgs=TRUE,
  warnPartialMatchAttr=TRUE,
  warnPartialMatchDollar=TRUE
)
# tests predate 3.5.

if(R.version$major >= 4 || R.version$major >= 3 && R.version$minor >= "5.0")
  suppressWarnings(RNGversion("3.5.2"));

source("_helper/commonobjects.R")
