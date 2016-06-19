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

    a <- c("a b", "c d")
    b <- c("b c", "d e")
    expect_identical(
      diffobj:::diff_word(a, b, across.lines=TRUE, white.space=FALSE, use.ansi=TRUE),
      structure(list(target = c("\033[31ma\033[39m b", "c d"), current = c("b c", "d \033[32me\033[39m")), .Names = c("target", "current"))
    )
    a <- c("x a b", "c d z")
    b <- c("x b c", "d e z")
    expect_identical(
      diffobj:::diff_word(a, b, across.lines=TRUE, white.space=FALSE, use.ansi=TRUE),
      structure(list(target = c("x \033[31ma\033[39m b", "c d z"), current = c("x b c", "d \033[32me\033[39m z")), .Names = c("target", "current"))
    )
    a <- c("x a b", "c d z")
    b <- c("z b c", "d e x")
    expect_identical(
      diffobj:::diff_word(a, b, across.lines=TRUE, white.space=FALSE, use.ansi=TRUE),
      list(target = c("\033[31mx\033[39m \033[31ma\033[39m b", "c d \033[31mz\033[39m"), current = c("\033[32mz\033[39m b c", "d \033[32me\033[39m \033[32mx\033[39m"))
    )
    # lapply(
    #   diffobj:::diff_word(a, b, across.lines=TRUE, white.space=FALSE, use.ansi=TRUE),
    #   cat, sep="\n"
    # )
    stop("test diff word with quotes, including quotes in names, etc")
  })
  options(old.opt)
  test_that("char_diff", {
    expect_identical(
      unitizer:::char_diff(c("a", "b", "c"), c("a", "b", "c")),
      new(
        "unitizerDiffDiffs", target=integer(3L),
        current=integer(3L), white.space=FALSE
    ) )
    expect_identical(
      unitizer:::char_diff(c("a", "b"), c("a", "b", "c")),
      new(
        "unitizerDiffDiffs", target=integer(2L),
        current=c(0L, 0L, NA), white.space=FALSE
    ) )
    expect_identical(
      unitizer:::char_diff(c("a", "b", "c"), c("a", "b")),
      new(
        "unitizerDiffDiffs", target=c(0L, 0L, NA),
        current=integer(2L), white.space=FALSE
    ) )
    expect_identical(
      unitizer:::char_diff(c("b", "c"), c("a", "b")),
      new(
        "unitizerDiffDiffs", target=c(0L, NA),
        current=c(NA, 0L), white.space=FALSE
    ) )
    expect_identical(
      unitizer:::char_diff(letters[1:3], letters[2:4]),
      new(
        "unitizerDiffDiffs", target=c(NA, 0L, 0L),
        current=c(0L, 0L, NA), white.space=FALSE
    ) )
    expect_identical(
      unitizer:::char_diff(c("a", "b", "c", "d"), c("a", "b", "b", "d", "e")),
      new(
        "unitizerDiffDiffs", target=c(0L, 0L, 1L, 0L),
        current=c(0L, 0L, 1L, 0L, NA), white.space=FALSE
    ) )
    expect_identical(
      unitizer:::char_diff(c("a", "b", "c"), c("a", "b", "d")),
      new(
        "unitizerDiffDiffs", target=c(0L, 0L, 1L),
        current=c(0L, 0L, 1L), white.space=FALSE
    ) )
    expect_identical(
      unitizer:::char_diff(
        c("a", "b", "c", "d", "f", "g", "h", "i", "j"),
        c("b", "C", "D", "E", "f", "G", "H", "j", "K"), white.space=FALSE
      ),
      new(
        "unitizerDiffDiffs", target=c(NA, 0L, 1L, 2L, 0L, 3L, 4L, NA, 0L),
        current=c(0L, 1L, 2L, NA, 0L, 3L, 4L, 0L, NA), white.space=FALSE
      )
    )
