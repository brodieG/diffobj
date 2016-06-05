# Retrieves data from the data elements of the Diff object based on the index
# values provided in ind.  Positive values draw from `tar` elements, and
# negative draw from `cur` elements.
#
# returns a list with the elements.  If type is length 1, you will probably
# want to unlist the return value

get_dat <- function(x, ind, type) {
  stopifnot(
    is(x, "Diff"), is.integer(ind),
    is.character(type) && all(type %in% .diff.dat.cols)
  )
  # Need to figure out what zero indices are; previously would return
  # NA_character_, but now since we're getting a whole bunch of different
  # stuff not sure what the right return value should be, or even if we produce
  # zero indices anymore

  setNames(
    lapply(type, function(y) get_dat_raw(ind, x@tar.dat[[y]], x@cur.dat[[y]])),
    type
  )
}
get_dat_raw <- function(ind, tar, cur) {
  if(any(ind == 0L)) stop("Zero indices detected")
  template <- tar[0L]
  length(template) <- length(ind)
  template[which(ind < 0L)] <-  cur[abs(ind[ind < 0L])]
  template[which(ind > 0L)] <-  tar[abs(ind[ind > 0L])]
  template
}
