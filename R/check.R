is.less_flags <- 
  function(x) is.chr.1L(x) && isTRUE(grepl("^[[:alpha:]]*$", x))

