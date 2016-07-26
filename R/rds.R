# Check Whether Input Could Be Reference to RDS File and Load if it Is

get_rds <- function(x) {
  if(is.chr.1L(x) && file_test("-f", x) || inherits(x, "connection")) {
    tryCatch(suppressWarnings(readRDS(x)), error=function(e) x)
  } else x
}
