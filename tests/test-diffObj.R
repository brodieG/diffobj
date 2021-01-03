NAME <- "diffObj"
source(file.path('_helper', 'init.R'))

# - simple diffobj -------------------------------------------------------------

# no diff for print
all.equal(as.character(diffObj(iris.s, iris.c)), rdsf(100))
# no diff for str
all.equal(
  as.character(diffObj(1:100, c(1:99, 200L))), rdsf(200)
)
# diffs for both and must pick one, first one is str, second is print
all.equal(
  as.character(diffObj(mdl1[7], mdl2[7])), rdsf(300)
)
all.equal(as.character(diffObj(mdl1, mdl2)), rdsf(400))

# - fits or doesn't ------------------------------------------------------------

# Note, the first test used to favor str until we handicapped print
all.equal(
  diffObj(matrix(1:20, ncol=2), matrix(2:21, ncol=2), line.limit=5)@capt.mode,
  "str"
)
# test kinda slow, would be better to have one with smaller objects with print
# methods

all.equal(
  diffObj(mdl1, mdl2, line.limit=15, mode='unified')@capt.mode, "print"
)
all.equal(diffObj(1:1000, 1000:1, line.limit=5)@capt.mode, "str")

# - misc -----------------------------------------------------------------------

try(diffObj(1, 2, extra=list(TRUE))) # "extra"

# - print error ----------------------------------------------------------------

x <- structure("hello", class="diffobj_ogewlhgiadfl")
y <- structure("goodbye", class="diffobj_ogewlhgiadfl")
try(diffObj(x, y)) # "Error in calling .diffPrint."

# Random exmaples to think through `diffObj` output

diffObj(
  pairlist("`logical(2L)` should be length 2 (is 3)"),
  pairlist("be length 2 (is 3)")
)

diffObj(
  pairlist("`matrix(integer(), nrow = 3)` should be matrix (is list)", "`list(character(1L), 1L)[[2]]` should be type \"integer-like\" (is \"character\")"),
  pairlist("be class \"matrix\" (is \"list\")", "be type \"integer-like\" (is \"character\") at index [[2]]")
)
