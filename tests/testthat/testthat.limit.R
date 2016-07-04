library(diffobj)

context("Limit")

# ## Old tests that need to be formalized
#
A <- B <- letters[1:5]
B[2] <- "B"
B[6] <- "F"

diffChr(A, B, line.limit=2, context=1L)
diffChr(A, B, line.limit=3, context=1L)
diffChr(A, B)

Puromycin2 <- Puromycin
set.seed(1)
Puromycin2$conc[c(8, 15:19, 22)] <- round(runif(7), 2)
Puromycin2$state[17] <- "treated"

diffPrint(Puromycin, Puromycin2, line.limit=15, mode="sidebyside")
diffPrint(Puromycin, Puromycin2, line.limit=15, mode="context")
diffPrint(Puromycin, Puromycin2, line.limit=15, mode="unified")

diffPrint(Puromycin, Puromycin2, line.limit=5, mode="sidebyside")
diffPrint(Puromycin, Puromycin2, line.limit=5, mode="context")
diffPrint(Puromycin, Puromycin2, line.limit=5, mode="unified")

# line limit issues

diffPrint(Puromycin, Puromycin2, line.limit=8, mode="sidebyside")
diffPrint(Puromycin, Puromycin2, line.limit=8, mode="sidebyside")
diffPrint(Puromycin, Puromycin2, line.limit=8, mode="context")

Puromycin3 <- Puromycin2
names(Puromycin3)[3L] <- "blargh"
diffPrint(Puromycin, Puromycin3, line.limit=6, mode="sidebyside")
diffPrint(Puromycin, Puromycin3, line.limit=6, mode="context")

