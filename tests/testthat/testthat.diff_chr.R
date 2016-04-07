
# Corner cases from https://neil.fraser.name/writing/diff/
# Both of these appear handled correctly by the algorithm here
# first one: suboptimal edit script due to two sided approach
A1 <- c("X", "A", "X", "C", "X", "A", "B", "C")
B1 <- c("A", "B", "C", "Y")
diff_chr(A1, B1)

# second one: failure to find intersection at ends of paths (paths run into
# each other eventually)

A2 <- c("A", "B", "X", "A", "B")
B2 <- c("A", "Y", "B")
diff_chr(A2, B2)

set.seed(1)
X <- do.call(paste0, expand.grid(LETTERS, LETTERS, LETTERS, LETTERS))

diff_chr(X[1:2000], X[2001:4000])
res <- diff_chr(X[1:25000], X[10001:50000], etc=etc(max.diffs=65000, max.diffs.in.hunk=65000)

diff_chr(letters[1:10], LETTERS[1:10])
