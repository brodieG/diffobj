# Automated string generation and comparison
# Used to verify no errors and focus on max.diffs limited ones.

M <- 10
n <- seq_len(M+1)-1
d <- 0:5 * 2
i <- 50
alph <- c('G','A','T','C')

dat <- expand.grid(n, d)
dat <- do.call(rbind, replicate(i, dat, simplify=FALSE))

set.seed(1)
res <- lapply(
  seq_len(nrow(dat)),
  function(i) {
    m <- dat[i,1]
    n <- M - m
    max.diffs <- dat[i,2]
    a <- sample(alph, m, replace=TRUE)
    b <- sample(alph, n, replace=TRUE)
    warn.dat <- ""
    res <- try(withCallingHandlers(
      ses_dat(a, b, max.diffs=max.diffs),
      warning=function(e) {
        warn.dat <<- paste0(
          sub(".*?(\\d+ vs \\d+).*", "\\1", conditionMessage(e), perl=TRUE),
          collapse=""
        )
      }
    ))
    if(inherits(res, 'try-error')) {
      writeLines(deparse(list(a=a, b=b, max.diffs=max.diffs)))
      stop('failed at ', i)
    }
    list(res, param=c(m=m, n=n, max.diffs=max.diffs),  warn=warn.dat, a=a, b=b)
  }
)

diffdisp <- function(x) {
  ses <- x[[1]]
  print(ses)
  writeLines(
    c(
      "",
      paste0("a: ", paste0(deparse(x[['a']]), collape="\n")),
      paste0("b: ", paste0(deparse(x[['b']]), collape="\n")),
      x[['warn']]
  ) )
}
set.seed(1)

res.warn <- vapply(res, function(x) nzchar(x[['warn']]), TRUE)
# res2 <- res[res.warn]
res2 <- res
i <- sample(seq_along(res2))

for(j in i) {
  readline("-------------------------------------")
  diffdisp(res2[[j]])
}
