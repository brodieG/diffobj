
# Corner cases from https://neil.fraser.name/writing/diff/
# Both of these appear handled correctly by the algorithm here
# first one: suboptimal edit script due to two sided approach
A1 <- c("X", "A", "X", "C", "X", "A", "B", "C")
B1 <- c("A", "B", "C", "Y")
diffChr(A1, B1)

# second one: failure to find intersection at ends of paths (paths run into
# each other eventually)

A2 <- c("A", "B", "X", "A", "B")
B2 <- c("A", "Y", "B")
diffChr(A2, B2)

set.seed(1)
X <- do.call(paste0, expand.grid(LETTERS, LETTERS, LETTERS, LETTERS))

diffChr(X[1:2000], X[2001:4000])
A3 <- B3 <- X[1:2000]
A3 <- A3[
  -unlist(
    replicate(50, seq(from=sample(2000, 1), by=1L, length.out=sample(15, 1)))
  )
]
B3 <- B3[
  -unlist(
    replicate(50, seq(from=sample(2000, 1), by=1L, length.out=sample(15, 1)))
  )
]
diffChr(A3, B3)
res <- diffChr(X[1:10000], X[7500:17500])
res <- diffChr(X[1:25000], X[10001:50000], max.diffs=65000)

diffChr(letters[1:10], LETTERS[1:10])

# compare two crayon file versions

url.1 <- "https://raw.githubusercontent.com/gaborcsardi/crayon/3f1f68ab177b82a27e754a58264af801f7194820/R/string_operations.r"
url.2 <- "https://raw.githubusercontent.com/gaborcsardi/crayon/30dbe0d4d92157350af3cb3aeebd6d9a9cdf5c0e/R/string_operations.r"
f.1 <- readLines(url.1)
f.2 <- readLines(url.2)
diffChr(f.1, f.2)
diffChr(f.1, f.2, mode="s")

chr.5 <- c(
  "hello there how are you doing",
  "humpty dumpty took a big fall",
  "lorem ipsum dolor sic est boom",
  "a computer once wrote a phrase"
)
chr.6 <- c(
  "hello THERE how are you doing",
  "and another SENTENCE blah blah",
  "humpty dumpty TOOK a big fall",
  "a COMPUTER once wrote a phrase"
)
diffChr(chr.5, chr.6)
diffChr(chr.5, chr.6, mode="unified")

