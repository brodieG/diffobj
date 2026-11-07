# Tests intended to be run with tools:::.runPackageTests() (i.e. R CMD check)
# Note, need to be loose with the directory check

if(nzchar(Sys.getenv('NOT_CRAN')))
  stopifnot(grepl('tests', basename(getwd())), exists("NAME"))

# str() output changed in R-devel r90805 (PR#18719); unknown svn rev is new
str.legacy <- getRversion() < "4.7.0" ||
  isTRUE(suppressWarnings(as.integer(R.version[["svn rev"]]) < 90805L))

rdsf <- function(x, name=NAME) {
  f <- sprintf("%s.rds", x)
  new <- file.path("_helper", "objs-str-r90805", name, f)
  readRDS(
    if(!str.legacy && file.exists(new)) new else
      file.path("_helper", "objs", name, f)
  )
}
txtf <-
  function(x) readLines(file.path("_helper", "objs", NAME, sprintf("%s.txt", x)))

srdsf <- function(x, i)
  saveRDS(x, file.path("_helper", "objs", NAME, sprintf("%s.rds", i)), version=2)

stxtf <- function(x, i)
  writeLines(x, file.path("_helper", "objs", NAME, sprintf("%s.txt", i)))

library(diffobj)

all.opts <- c(
  list(
    useFancyQuotes=FALSE,   # all.equals uses fancy quotes
    diffobj.format="ansi8", # force ANSI colors
    diffobj.color.mode="yb",# force yb
    diffobj.pager="off",    # run tests without pager
    width=80L,
    encoding="UTF-8",        # so Gabor's name renders properly on win...
    warnPartialMatchArgs=TRUE,
    warnPartialMatchAttr=TRUE,
    warnPartialMatchDollar=TRUE
  )
)
options(c(diffobj_set_def_opts(), all.opts))

# tests predate 3.5.

if(R.version$major >= 4 || R.version$major >= 3 && R.version$minor >= "5.0")
  suppressWarnings(RNGversion("3.5.2"));

source("_helper/commonobjects.R")
