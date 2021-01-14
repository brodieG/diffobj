NAME <- "diffPrint"
source(file.path('_helper', 'init.R'))

# Note, atomic prints happen in different test file

# - Matrices -------------------------------------------------------------------

mx.2 <- matrix(1:100, ncol=2)
mx.4 <- mx.3 <- mx.2
mx.3[31, 2] <- 111L

mx.3a <- mx.3[-31, ]
set.seed(2)
mx.4[cbind(sample(1:50, 6), sample(1:2, 6, replace=TRUE))] <-
  sample(-(1:50), 6)

mx.5 <- matrix(1:9, 3)
mx.6 <- matrix(12:1, 4)
mx.6[4,] <- c(3L, 6L, 9L)

# single value difference
all.equal(as.character(diffPrint(mx.2, mx.3)), rdsf(100))
# single value unified
all.equal(as.character(diffPrint(mx.2, mx.3, mode="unified")), rdsf(150))
# single value context
all.equal(as.character(diffPrint(mx.2, mx.3, mode="context")), rdsf(175))
# missing row
all.equal(as.character(diffPrint(mx.2, mx.3a)), rdsf(200))
all.equal(as.character(diffPrint(mx.2, mx.3a, mode="unified")), rdsf(300))
# More differences

all.equal(as.character(diffPrint(mx.2, mx.4)), rdsf(400))
all.equal(as.character(diffPrint(mx.2, mx.4, mode="unified")), rdsf(500))
# Testing alignments
all.equal(as.character(diffPrint(mx.5, mx.6)), rdsf(600))
all.equal(as.character(diffPrint(mx.5, mx.6, mode="unified")), rdsf(700))
all.equal(as.character(diffPrint(mx.5, mx.6, mode="context")), rdsf(800))
# More complex matrix
set.seed(2)
A <- B <- matrix(sample(1:80), nrow=16)
B[cbind(sample(5:16, 4), sample(1:5, 4))] <- sample(30:80, 4)

all.equal(as.character(diffPrint(A, B)), rdsf(900))
all.equal(as.character(diffPrint(A, B, mode="unified")), rdsf(1000))
all.equal(as.character(diffPrint(A, B, mode="context")), rdsf(1100))
# Style matrices

all.equal(as.character(diffPrint(diffobj:::.mx1, diffobj:::.mx2)), rdsf(1200))

# - Lists ----------------------------------------------------------------------

all.equal(as.character(diffPrint(lst.1, lst.3)), rdsf(1300))
all.equal(as.character(diffPrint(lst.1, lst.3, mode="unified")), rdsf(1400))
all.equal(as.character(diffPrint(lst.4, lst.5)), rdsf(1500))
all.equal(as.character(diffPrint(lst.4, lst.5, mode="context")), rdsf(1600))
# Nested first element (https://github.com/brodieG/diffobj/issues/46)
all.equal(
  as.character(diffPrint(list(1, list(2, list(1:3))), list(list(list(1:3))))),
  rdsf(1650)
)
# Interesting but relatively slow example so we don't actually run it in
# tests

# diffPrint(unclass(mdl1), unclass(mdl2))
# diffPrint(unclass(mdl1), unclass(mdl2), mode="unified")

# - Data Frames ----------------------------------------------------------------

all.equal(as.character(diffPrint(iris.s, iris.2)), rdsf(1700))
all.equal(
  as.character(diffPrint(iris.s, iris.2, mode="sidebyside")), rdsf(1800)
)
all.equal(as.character(diffPrint(iris.s, iris.c)), rdsf(1900))
all.equal(as.character(diffPrint(iris.s, iris.3)), rdsf(2000))

all.equal(
  as.character(diffPrint(iris.s, iris.3, mode="sidebyside")), rdsf(2100)
)
all.equal(as.character(diffPrint(iris.s, iris.4, mode="unified")), rdsf(2150))
all.equal(
  as.character(diffPrint(iris.s, iris.4, mode="sidebyside")), rdsf(2200)
)
all.equal(
  as.character(diffPrint(iris.5, iris.4, mode="sidebyside")), rdsf(2250)
)
all.equal(as.character(diffPrint(iris.3a, iris.4a)), rdsf(2300))
all.equal(
  as.character(diffPrint(iris.s, iris.3, mode="sidebyside")), rdsf(2350)
)
all.equal(as.character(diffPrint(iris.s, iris.s[-2])), rdsf(2370))
# This one is interesting because the input is pathological because there
# is one line that matches exactly between the two and as such creates a
# matching hunk, but it really is matching by coincidence.

all.equal(
  as.character(diffPrint(iris.s, iris.s[-2], mode="sidebyside")), rdsf(2383)
)
# Possible example where we may not want to trim the row headers (Issue #39)
all.equal(
  as.character(diffPrint(cars[1:5,], mtcars[1:5,], mode="sidebyside")),
  rdsf(2380)
)

# - Guides ---------------------------------------------------------------------

# Most guides tests are in the guides file, but this confirms interface works
# when starting at `diffPrint` instead of internally

all.equal(
  as.character(
    diffPrint(
      iris.s, iris.4, mode="sidebyside", guides=function(x, y) integer()
  ) ),
  rdsf(2400)
)
all.equal(
  as.character(diffPrint(iris.s, iris.4, mode="sidebyside", guides=FALSE)),
  rdsf(2500)
)

# - Arrays
arr.1 <- arr.2 <- array(1:24, c(4, 2, 3))
arr.2[c(3, 20)] <- 99L
all.equal(as.character(diffPrint(arr.1, arr.2)), rdsf(2600))

# - Mixed
all.equal(
  as.character(diffPrint(list(1, 2, 3), matrix(1:9, 3))),
  rdsf(2700)
)
all.equal(
  as.character(diffPrint(list(25, 2, 3), matrix(1:9, 3))),
  rdsf(2800)
)
all.equal(
  as.character(
    diffPrint(list(c(1, 4, 7), c(2, 5, 8), c(3, 6, 9)), matrix(1:9, 3))
  ),
  rdsf(2900)
)
# - `unitizer` corner case -----------------------------------------------------

res1 <- structure(
  c(-1717, 101, 0.938678984853783),
  .Names = c("intercept", "slope", "rsq"), class = "fastlm"
)
res2 <- structure(
  c(-3.541306e+13, 701248600000, 0.938679),
  .Names = c("intercept", "slope", "rsq"), class = "fastlm"
)
all.equal(as.character(diffPrint(res1, res2)), rdsf(3000))
all.equal(
  as.character(diffPrint(unname(res1), unname(res2))), rdsf(3100)
)

# - factors and other meta -----------------------------------------------------

# Thanks Frank

all.equal(
  as.character(diffPrint(factor(1:100), factor(c(1:99, 101)))), rdsf(3200)
)
f1 <- factor(1:100)
f2 <- factor(c(1:20, 22:99, 101))
all.equal(capture.output(diffPrint(f1, f2)), txtf(100))

f3 <- factor(letters[1:10])
f4 <- factor(letters[1:10], levels=letters[1:11])
all.equal(capture.output(diffPrint(f3, f4)), txtf(150))

# time series

nhtemp2 <- nhtemp
nhtemp2[c(5, 30)] <- -999
all.equal(capture.output(diffPrint(nhtemp, nhtemp2)), txtf(175))

# Meta on both sides

print.diffobj_test_c1 <- function(x, ...) {
  writeLines(c("Header row 1", "header row 2"))
  print(c(x))
  writeLines(c("", "Footer row 1", "", "footer row2"))
}
m1 <- structure(1:30, class='diffobj_test_c1')
m2 <- structure(2:51, class='diffobj_test_c1')
all.equal(capture.output(diffPrint(m1, m2)), txtf(200), print=TRUE)

# - Raw output -----------------------------------------------------------------

all.equal(
  as.character(diffPrint(letters, LETTERS, format="raw", pager="off")),
  rdsf(3300)
)
# - Varying Widths -------------------------------------------------------------

all.equal(
  as.character(diffPrint(letters, LETTERS, format="raw", disp.width=40)),
  rdsf(3400)
)
try(diffPrint(letters, LETTERS, disp.width=5))

# - covr workaround ------------------------------------------------------------

# Needed so that the function definition stuff is marked as covered; really
# it shouldn't even be eligible for coverage, need to discuss further with
# jhester

invisible(diffobj:::make_diff_fun())

# - Encoding Issues ------------------------------------------------------------

# issue81, mixed UTF-8 ASCII, encoding a-acute in hex to avoid cross platform
# issues

a <- "G\xc3\xa1bor Cs\xc3\xa1rdi"
b <- sprintf("%s wow", a)
Encoding(a) <- 'UTF-8'
Encoding(b) <- 'UTF-8'

# No error

new <- (as.character(diffPrint(list(hell=a, b=NULL), list(hell=b, b=list()))))

# can't store this in RDS b/c otherwise won't run properly on oses with
# different encoding (e.g. windows)

ref <- structure(
  c("\033[33m<\033[39m \033[33mlist(hell = a, b = N..\033[39m  \033[34m>\033[39m \033[34mlist(hell = b, b = l..\033[39m",
    "\033[36m@@ 1,6 @@               \033[39m  \033[36m@@ 1,6 @@               \033[39m",
    "  \033[90m\033[39m$hell\033[90m\033[39m                     \033[90m\033[39m$hell\033[90m\033[39m                 ",
    "\033[33m<\033[39m \033[90m[1] \033[39m\033[33m\"G\xc3\xa1bor Cs\xc3\xa1rdi\"\033[39m\033[90m\033[39m      \033[34m>\033[39m \033[90m[1] \033[39m\033[34m\"G\xc3\xa1bor Cs\xc3\xa1rdi wow\"\033[39m\033[90m\033[39m",
    "                                                  ", "  \033[90m\033[39m$b\033[90m\033[39m                        \033[90m\033[39m$b\033[90m\033[39m                    ",
    "\033[33m<\033[39m \033[90m\033[39m\033[33mNULL\033[39m\033[90m\033[39m                    \033[34m>\033[39m \033[90m\033[39m\033[34mlist\033[39m\033[34m()\033[39m\033[90m\033[39m                ",
    "                                                  "
  ),
  len = 8L
)
Encoding(ref) <- 'UTF-8'
all.equal(new, ref)

# issue 106, this used to fail when trying to check for an RDS with a bytes
# encoded file name

bytes <- "\x81"
Encoding(bytes) <- "bytes"
isTRUE(!any(diffPrint(bytes, bytes)))

# - Quoted Objects -------------------------------------------------------------

all.equal(
  as.character(diffPrint(quote(zz + 1), quote(zz + 3))),
  structure(
    c("\033[33m<\033[39m \033[33mquote(..\033[39m  \033[34m>\033[39m \033[34mquote(..\033[39m", "\033[36m@@ 1 @@   \033[39m  \033[36m@@ 1 @@   \033[39m", "\033[33m<\033[39m \033[90m\033[39mzz + \033[33m1\033[39m\033[90m\033[39m    \033[34m>\033[39m \033[90m\033[39mzz + \033[34m3\033[39m\033[90m\033[39m  "
    ), len = 3L
  )
)
all.equal(
  as.character(diffPrint(quote(x), quote(y))),
  structure(
    c("\033[33m<\033[39m \033[33mquote(x)\033[39m  \033[34m>\033[39m \033[34mquote(y)\033[39m", "\033[36m@@ 1 @@   \033[39m  \033[36m@@ 1 @@   \033[39m", "\033[33m<\033[39m \033[90m\033[39m\033[33mx\033[39m\033[90m\033[39m         \033[34m>\033[39m \033[90m\033[39m\033[34my\033[39m\033[90m\033[39m       "),
    len = 3L
  )
)
# - par_frame ------------------------------------------------------------------

# check that par_frame is retrieved correctly
env <- new.env()
env$print <- function(x, ...) stop('boom')
try(evalq(diffPrint(1:3, 1:4), env)) # "Failed attempting .*: boom"

f <- function(a, b, ...) {
  print <- function(x, ...) stop('boom2')
  diffPrint(a, b, ...)
}
try(f(1:3, 1:4, format='raw')) # "Failed attempting .*: boom2"

