any(diffPrint(iris, iris))
any(diffPrint(iris, iris.c))
any(diffPrint(iris, iris.4))

summary(diffPrint(iris, iris.4))
summary(diffPrint(iris, iris.2))
summary(diffPrint(iris, iris.3))
summary(diffPrint(iris, iris.c))

d1 <- d2 <- ggplot2::diamonds[sample(seq_len(nrow(ggplot2::diamonds)), 10000),]

d1 <- d1[-sample(seq_len(nrow(d1)), 200), ]
d1$clarity[sample(seq_len(nrow(d1)), 50)] <-
  d1$clarity[sample(seq_len(nrow(d1)), 50)]
d1$x[sample(seq_len(nrow(d1)), 50)] <- d1$x[sample(seq_len(nrow(d1)), 50)]
d1$y[sample(seq_len(nrow(d1)), 50)] <- d1$y[sample(seq_len(nrow(d1)), 50)]

d2 <- d2[-sample(seq_len(nrow(d2)), 200), ]
summary(diffPrint(d1, d2))

