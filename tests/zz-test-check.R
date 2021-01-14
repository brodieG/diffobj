filelist <- function(x, y) paste0(x, paste0("* ", basename(y), collapse="\n"))

test.out <- list.files(pattern="\\.Rout$")
if(any(lengths(lapply(test.out, tools::showNonASCIIfile))))
  warning(bullets("Some test output files contain non-ASCII:\n", test.out))

targets <- list.files(pattern='\\.Rout\\.save', full.names=TRUE)
current <- file.path(dirname(targets), sub('\\.save$', '', basename(targets)))
missing <- !file.exists(current)
writeLines(getwd())

if(any(missing))
  stop(filelist("Test output files are missing (failed?):\n", current[missing]))

diff.dat <- Map(
  tools::Rdiff, targets[!missing], current[!missing], useDiff=TRUE, Log=TRUE
)
diffs <- vapply(diff.dat, '[[', 1, 'status')
if(any(!!diffs))
  stop(filelist("Test output files have differences:\n", current[!!diffs]))

