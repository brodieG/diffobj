library(diffobj)

A <- B <- c(letters, LETTERS)
B[15] <- "Alabama"
diffPrint(A[-(1:8)], A)  # at 80 cols, happens to correspond to row insertion
diffPrint(A, B)
C <- A[-15]
diffPrint(C, B)

D <- C
E <- B[-45]

diffPrint(D, E)
diffPrint(E, D)
