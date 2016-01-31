A <- c("A", "AA", "B", "C", "D", "E", "F", "G", "H")
B <- c("A", "B", "X", "W", "D", "DD", "E", "Y", "Z")
C <- c("X", "D", "E", "Y", "Z", "H")

library(diffobj)

diffobj:::as.hunks(diffobj:::diff_myers_mba(A, B))
