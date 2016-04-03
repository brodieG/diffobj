is.less_flags <- 
  function(x) is.chr.1L(x) && isTRUE(grepl("^[[:alpha:]]*$", x))

is.pager_mode <- function(x) {
  mode.valid <- c("threshold", "always", "never")
  is.chr.1L(x) && x %in% mode.valid
}

