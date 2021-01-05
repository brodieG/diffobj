# Helper objects to use across multiple test files

## Matrices -----------

## Lists -------------------

lst.1 <- list(
  NULL,
  letters,
  z=list(
    list(letters[1:3]), list(NULL),
    z=list(1:3, 1, 2, z=list(1, z=list(z=5))),
    matrix(1:9, 3)
) )
lst.2 <- lst.1
lst.2$z$z$z$z$z <- 6
lst.2$z[[1L]][[1L]][2L] <- "bananas"
lst.2$z[[4L]] <- matrix(12:1, ncol=3)
lst.2$z[[4L]][4, ] <- c(3L, 6L, 9L)
lst.3 <- lst.2
lst.3[[1]] <- "hello"
lst.3[[2]] <- NULL

lst.4 <- list(NULL, z=list(z=list(z=list(z=list(matrix(1:3))))))
lst.5 <- list(NULL, z=list(z=list(z=list(z=list(matrix(2:4))))))

## Character ----------------

chr.1 <- c(
  "hello world",
  "I ran into a rather bizarre bug involving memoise that made it impossible to forget the cached version of crayon:::i_num_colors. Somehow, the binary version of crayon on CRAN has a corrupted copy of the memoised crayon:::i_num_colors function",
  "goodbye"
)
chr.2 <- c(
  "hello world",
  "I ran blah a rather bizarre bug involving memoise that made it"
)
chr.3 <- letters[1:20]
chr.4 <- c(
  "a phrase long enough to wrap a few lines when looked at on a side by side basis",
  "lorem ipsum dolor something or other I don't remember what the whole thing was anyway"
)
# X <- do.call(paste0, expand.grid(LETTERS, LETTERS, LETTERS, LETTERS))
n <- 500  # this n is used
# saveRDS(X[seq_len(n)], file.path('_helper', 'objs', 'common', 'aaaa.RDS'),
# version=2)
set.seed(1)

chr.7 <- chr.8 <- readRDS(file.path('_helper', 'objs', 'common', 'aaaa.RDS'))
chr.7 <- chr.7[
  -unlist(
    replicate(25, seq(from=sample(n, 1), by=1L, length.out=sample(10, 1)))
  )
]
chr.8 <- chr.8[
  -unlist(
    replicate(25, seq(from=sample(n, 1), by=1L, length.out=sample(10, 1)))
  )
]

chr.9 <- chr.10 <- letters
ind <- c(4, 10, 18, 20, 26)
chr.10[ind] <- LETTERS[ind]

## Data Frames ----------------

set.seed(2)
iris.s <- `row.names<-`(iris[c(1:5, 50:55, 100:105), ], NULL)
iris.2 <- iris.c <- transform(iris.s, Species=as.character(Species))
# without rounding this is a bit wild, but good corner case to test
iris.2$Sepal.Length[sample(nrow(iris.2), 5)] <-
  rnorm(5, mean(iris.2$Sepal.Length), sd(iris.2$Sepal.Length))

iris.3 <- iris.2
iris.3$Sepal.Length <- round(iris.3$Sepal.Length, 1L)
iris.4 <- iris.3
iris.4$Petal.Width[sample(1:nrow(iris.4), 6)] <- round(runif(6), 1)

iris.5 <- iris.s
attr(iris.5, "test.attr") <- letters

# Narrow versions to fit side by side

iris.3a <- setNames(iris.3, c("S.L", "S.W", "P.L", "P.W", "Sp"))
iris.4a <- setNames(iris.4, c("S.L", "S.W", "P.L", "P.W", "Sp"))

## Arrays -----------------

## Models -----------------

frm1 <- as.formula("Sepal.Length ~ Sepal.Width", env=.GlobalEnv)
frm2 <- as.formula("Sepal.Length ~ Sepal.Width + Species", env=.GlobalEnv)
mdl1 <- lm(frm1, iris)
mdl2 <- lm(frm2, iris)
