is.less_flags <- function(x)
  !is.chr.1L(less.flags) && !isTRUE(grepl("^[[:alpha:]]$", less.flags))

is.pager_mode <- function(x) {
}
  mode.valid <- c("threshold", "always", "never")

