NAME <- "scaling"
source(file.path('_helper', 'init.R'))

# These tests are not actually run since they require manual intervention to
# check for browser rendering

# These tests should be run on as many browsers as possible as well as in
# RStudio, and consist of running the code and resizing the windows to see
# what happens

if(FALSE) { # prevent running
  # Text should be allowed to unfurl beyond native width

  diffStr(mdl1, mdl2, format="html")
  diffStr(mdl1, mdl2, format="html", style=list(scale=FALSE))

  diffPrint(c(mdl1), c(mdl2), format="html")

  # Revealed problems with pixel rounding in scaling

  diffPrint(letters[1:6], LETTERS[1:6], format="html")
  diffPrint(letters[1:6], LETTERS[1:6], format="html", style=list(scale=FALSE))

  # Revealed problems with gutter width computations; and scaling

  mx.2 <- matrix(1:100, ncol=4)
  mx.4 <- mx.3 <- mx.2
  mx.3[15, 2] <- 111L
  mx.3a <- mx.3[-5, ]
  diffPrint(mx.2, mx.3a, format="html")

  # summary stuff

  summary(diffPrint(letters, LETTERS, format="html"))
  summary(diffStr(mdl1, mdl2, format="html"))
  summary(diffPrint(c(mdl1), c(mdl2), format="html"))

  # Long banners

  diffPrint(
    1:20 + 100 + 100 + 100 + 100 + 100 + 100 + 100,
    2:20 + 100 + 100 + 100 + 100 + 100 + 100 + 100,
    format="html", style=list(scale=FALSE)
  )
  diffPrint(
    1:20 + 100 + 100 + 100 + 100 + 100 + 100 + 100,
    2:20 + 100 + 100 + 100 + 100 + 100 + 100 + 100,
    format="html"
  )
  # context

  diffPrint(
    1:20 + 100 + 100 + 100 + 100 + 100 + 100 + 100,
    2:20 + 100 + 100 + 100 + 100 + 100 + 100 + 100,
    format="html", mode="context"
  )
}
