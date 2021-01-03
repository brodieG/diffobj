NAME <- "diffStr"
source(file.path('_helper', 'init.R'))

# - lm models ------------------------------------------------------------------

# formula display changed
if(
  R.Version()$major >= 3 && R.Version()$minor >= "3.1" || R.Version()$major > 3
)
  all.equal(as.character(diffStr(mdl1, mdl2)), rdsf(100))

# Too strict a line limit, can't get under
all.equal(
  as.character(diffStr(mdl1[7], mdl2[7], line.limit=10)), rdsf(200)
)
# Now we can get under
all.equal(
  as.character(diffStr(mdl1[7], mdl2[7], line.limit=15)), rdsf(300)
)

# - Simple structure -----------------------------------------------------------
#
# Character types

all.equal(as.character(diffStr(iris.c, iris.s)), rdsf(400))

# - Strict width ---------------------------------------------------------------
# formula display changed
if(
  R.Version()$major >= 3 && R.Version()$minor >= "3.1" || R.Version()$major > 3
) {
  c(
    all.equal(
      as.character(
        diffStr(mdl1, mdl2, extra=list(strict.width="wrap"), line.limit=30)
      ),
      rdsf(500)
    ),
    all.equal(
      as.character(
        diffStr(mdl1, mdl2, extra=list(strict.width="cut"), line.limit=30)
      ),
      rdsf(550)
  ) )
}
# - max.diffs ------------------------------------------------------------------

invisible(diffStr(iris, mtcars, max.diffs=2)) # warn: "Exceeded diff limit"

# - max.level ------------------------------------------------------------------

all.equal(
  as.character(diffStr(mdl1[7], mdl2[7], extra=list(max.level="auto"))),
  rdsf(600)
)
all.equal(
  as.character(diffStr(mdl1[7], mdl2[7], extra=list(max.level=2))),
  rdsf(700)
)
# Has a difference, but can't get under; the second is just for reference

lst.1 <- lst.2 <- lst.3 <- list(a=list(b=list(c=list(d=list(e=list(25))))))
names(lst.2) <- "A"

all.equal(
  as.character(diffStr(lst.1, lst.2, line.limit=2)), rdsf(800)
)
all.equal(
  as.character(diffStr(lst.1, lst.2, line.limit=2)), rdsf(900)
)
# Test that initial run shows difference, but too big, but next one down
# doesn't so have to increase level

names(lst.3$a$b$c$d) <- "E"
all.equal(
  as.character(diffStr(lst.1, lst.3, line.limit=6)), rdsf(1000)
)

# - No visible differences -----------------------------------------------------

all.equal(
  as.character(diffStr(1:100, c(1:99, 101L))), rdsf(1100)
)

# - Quoted Objects -------------------------------------------------------------

all.equal(
  as.character(diffStr(quote(zz + 1), quote(zz + 3))),
  structure(
    c("\033[33m<\033[39m \033[33mstr(quote(zz +..\033[39m  \033[34m>\033[39m \033[34mstr(quote(zz +..\033[39m", "\033[36m@@ 1 @@           \033[39m  \033[36m@@ 1 @@           \033[39m", "\033[33m<\033[39m \033[90m\033[39m language zz + \033[33m1\033[39m\033[90m\033[39m  \033[34m>\033[39m \033[90m\033[39m language zz + \033[34m3\033[39m\033[90m\033[39m"
    ), len = 3L
) )

all.equal(
  as.character(diffStr(quote(x), quote(y))),
  structure(c("\033[33m<\033[39m \033[33mstr(quo..\033[39m  \033[34m>\033[39m \033[34mstr(quo..\033[39m", "\033[36m@@ 1 @@    \033[39m  \033[36m@@ 1 @@    \033[39m", "\033[33m<\033[39m \033[90m\033[39m symbol \033[33mx\033[39m\033[90m\033[39m  \033[34m>\033[39m \033[90m\033[39m symbol \033[34my\033[39m\033[90m\033[39m"), len = 3L)
)

# - Spaces with punctuation ----------------------------------------------------

all.equal(
  capture.output(show(diffStr(list(a=1), list(a=1, cabera=3), format='raw'))),
  txtf(100)
)

