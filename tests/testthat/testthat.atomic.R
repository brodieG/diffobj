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
diffPrint((2:101)[-98], (1:100)[-9])

int.1 <- int.2 <- 1:100
int.2[c(8, 20, 60)] <- 99
int.2 <- c(50:1, int.2)
diffPrint(int.1, int.2)

A.1 <- A
A.1[5] <- "Ee"
diffPrint(A, A.1[-(13:18)])

set.seed(2)
w1 <- sample(
  c(
  "carrot", "cat", "cake", "eat", "rabbit", "holes", "the", "a", "pasta",
  "boom", "noon", "sky", "hat", "blah", "paris", "dog", "snake"
  ), 25, replace=TRUE
)
w4 <- w3 <- w2 <- w1
w2[sample(seq_along(w1), 5)] <- LETTERS[1:5]
w3 <- w1[8:15]
w4 <- c(w1[1:5], toupper(w1[1:5]), w1[6:15], toupper(w1[1:5]))

diffPrint(w1, w2)
diffPrint(w1, w3)
diffPrint(w1, w4)

nums <- runif(5, -1e9, 1e9)
scinums <- format(c(nums, 1/nums), scientific=TRUE)
other <- c(paste0(sample(1:200, 5), "%"), "5.34e-8", "-2.534e6")
wl <- c(words, nums, scinums, other)

# Initial sample

s1 <- s2 <- s3 <- s4 <- sample(wl, 20, replace=T)
s2 <- s1[5:20]                             # subset
s3[sample(seq_along(s1), 10)] <- sample(s1, 10)     # change some
s4 <- c(s1[1:5], sample(s1, 2), s1[6:15], sample(s1, 2), s1[16:20])

a <- c("a", "b", "c", "d")
b <- c("b" , "c", "d", "e")
diffPrint(a, b)

a <- c("x", "a", "b", "c", "d", "z")
b <- c("x", "b", "c", "d", "e", "z")
diffPrint(a, b)

a <- c("x", "a", "b", "c", "d", "z")
b <- c("z", "b", "c", "d", "e", "x")
diffPrint(a, b)
