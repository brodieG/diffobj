
targets <- list.files(pattern='.Rout.save', full.names=TRUE)
current <- file.path(dirname(targets), sub('.save$', '', basename(targets)))
missing <- !file.exists(current)

if(any(missing))
  stop(
    "Following test output files are missing:\n",
    paste0("* ", basename(current[missing]), collapse="\n")
  )

diff.dat <- Map(
  tools::Rdiff, targets[!missing], current[!missing], useDiff=TRUE, Log=TRUE
)
diffs <- vapply(diffs, '[', 1, 'status')
if(any(diffs))
  stop(
    "Following test output files have differences:\n",
    paste0("* ", basename(current[!!diffs]), collapse="\n")
  )

