library(diffobj)

A <- B <- c(letters, LETTERS)
B[15] <- "Alabama"
diffPrint(A[-(1:8)], A)  # at 80 cols, happens to correspond to row insertion
diffPrint(A, A[-(1:8)])
diffPrint(A, B)
diffPrint(A[-1], A[-2])

# Make sure turning off word.diff also turns of unwrapping, but that we can turn
# off unwrapping without turning off word diff

diffPrint(A, B, word.diff=FALSE)
diffPrint(A, B, unwrap.atomic=FALSE)

diffPrint(B, A)
C <- A[-15]
diffPrint(C, B)

D <- C
E <- B[-45]

diffPrint(D, E)
diffPrint(E, D)

state.abb.2 <- state.abb
state.abb.2[38] <- "Pennsylvania"

diffPrint(state.abb, state.abb.2)

diffPrint(1:100, 2:101, mode="sidebyside")
diffPrint(2:101, 1:100)
diffPrint(2:101, (1:100)[-9])

int.1 <- int.2 <- 1:100
int.2[c(8, 20, 60)] <- 99
int.2 <- c(50:1, int.2)
diffPrint(int.1, int.2)

A.1 <- A
A.1[5] <- "Ee"
diffPrint(A, A.1[-(13:18)])
