NAME <- "misc"
source(file.path('_helper', 'init.R'))

# - trim_str -------------------------------------------------------------------

a <- structure("hello", class="A", xx="B")
b <- structure(1:10, yy=a)
long.string <- "I'm a string long enough to force wrapping under most cases so that I may be useful for tests andiamareallylongwordtoseehowwrappingbreakslongwordsthatexceed"
obj <- list(
  a=a, b=b, c=1:50,
  d=long.string,
  e=list(1, structure(2, zz=list(a=1, b=list("a", ls=long.string))), e=letters)
)
# conditional because of issue113
str.txt <- capture.output(str(obj))
str.txt.w <- capture.output(str(obj, width=30L, strict.width="wrap"))

if(
  getRversion() >= '3.5.0' && as.numeric(R.Version()[['svn rev']]) >= 73780
) {
  c(
    all.equal(
      diffobj:::str_levels(str.txt, wrap=FALSE),
      c(0L, 1L, 2L, 1L, 2L, 3L, 1L, 1L, 1L, 2L, 2L, 3L, 4L, 4L, 5L, 5L, 2L)
    ),
    all.equal(
      diffobj:::str_levels(str.txt.w, wrap=TRUE),
      c(0L, 1L, 2L, 1L, 1L, 2L, 2L, 3L, 1L, 1L, 1L, 1L, 1L, 1L, 1L,
        1L, 1L, 2L, 2L, 3L, 3L, 4L, 4L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L,
        2L, 2L
      )
  ) )
} else {
  c(
    all.equal(
      diffobj:::str_levels(str.txt, wrap=FALSE),
      c(0L, 1L, 3L, 1L, 2L, 4L, 1L, 1L, 1L, 2L, 2L, 3L, 4L, 4L, 5L,  5L, 2L)
    ),
    all.equal(
      diffobj:::str_levels(str.txt.w, wrap=TRUE),
      c(0L, 1L, 1L, 3L, 1L, 1L, 2L, 2L, 4L, 4L, 1L, 1L, 1L, 1L, 1L,  1L, 1L, 1L, 1L, 2L, 2L, 3L, 3L, 4L, 4L, 5L, 5L, 5L, 5L, 5L, 5L,  5L, 5L, 2L, 2L)
  ) )
}
# cat(
#   paste(
#     format(substr(str.txt.w, 1, 20)), diffobj:::str_levels(str.txt.w, TRUE),
#     sep=": "
#   ),
#   sep="\n"
# )

# - rle_sub --------------------------------------------------------------------

x <- c(1, 1, 1, 2, 2, 1, 1, 3, 3, 4, 4, 4, 5, 2, 2)
r <- rle(x)
all.equal(diffobj:::rle_sub(r, r$values == 1L), list(1:3, 6:7))
all.equal(diffobj:::rle_sub(r, r$values == 2L), list(4:5, 14:15))
isTRUE(all(x[unlist(diffobj:::rle_sub(r, r$values == 1L))] == 1))
isTRUE(all(x[unlist(diffobj:::rle_sub(r, r$values == 2L))] == 2))
isTRUE(all(x[unlist(diffobj:::rle_sub(r, r$values == 3L))] == 3))

# - call funs ------------------------------------------------------------------

# Failure case; assumes no S4 dispatch in testthat
calls <- list(quote(a()), quote(b()), quote(notafunctionblah()))
all.equal(diffobj:::which_top(calls), length(calls))
diffobj:::extract_call(calls, new.env()) # warn:  "Unable to find")

# missing param works

calls2 <- pairlist(
  quote(diffChr("a")), quote(diffChr("a")), quote(.local(target, current, ...))
)
all.equal(
  diffobj:::extract_call(calls2, new.env()),
  list(call = quote(diffChr(target = "a", NULL)), tar = "a", cur = NULL)
)
# fallback parent frame; can't think of a good way to actually cause this to
# happen

# all.equal(diffobj:::par_frame(), .GlobalEnv)

# - lines ----------------------------------------------------------------------

old.val <- Sys.getenv("LINES", unset=NA)
Sys.setenv(LINES="25")
all.equal(console_lines(), 25L)
Sys.setenv(LINES="-25")
all.equal(console_lines(), 48L)
Sys.unsetenv("LINES")
all.equal(console_lines(), 48L)

# - get_funs -------------------------------------------------------------------

identical(
  diffobj:::get_fun(quote(diffobj::diffPrint), .BaseNamespaceEnv),
  diffobj::diffPrint
)
identical(
  diffobj:::get_fun(quote(diffobj:::diffPrint), .BaseNamespaceEnv),
  diffobj::diffPrint
)
identical(
  diffobj:::get_fun(quote(diffPrint), getNamespace("diffobj")),
  diffobj::diffPrint
)
gf <- diffobj:::get_fun(quote(notAFunction), getNamespace("diffobj")) # warn

identical(gf, NULL)

# - trimws2 --------------------------------------------------------------------

all.equal(diffobj:::trimws2("hello world"),  "hello world")
all.equal(diffobj:::trimws2("  hello world"),  "hello world")
all.equal(diffobj:::trimws2("  hello world  "),  "hello world")
all.equal(diffobj:::trimws2("  hello world  ", 'left'), "hello world  ")
all.equal(diffobj:::trimws2("  hello world  ", 'right'), "  hello world")

try(diffobj:::trimws2("  hello world  ", 'banana')) # "is wrong"

# - string ---------------------------------------------------------------------

try(diffobj:::substr2("hello world", 1, 1:2)) # "same length"

# - Gutters --------------------------------------------------------------------

etc <- new("Settings")
etc@style <- StyleRaw()
etc@style@funs@gutter <- function(x) stop("bad gutters")
try(diffobj:::gutter_dat(etc)) # "Failed attempting to apply gutter."

# - Finalizer error handling ---------------------------------------------------

try(finalizeHtml(letters, NULL)) # "must be character"
try(finalizeHtml(letters, letters, letters)) # "must be character\\(1L"

# - c.factor -------------------------------------------------------------------

all.equal(diffobj:::c.factor(), factor(character()))

# - strip_hz -------------------------------------------------------------------

# Can't trigger this directly because wrapper doesn't let this case through
diffobj:::strip_hz_c_int(character(), 8L, TRUE)


