# tests designed to produce warnings

context("warnings")

# Special args to `str`

a <- "hello"
b <- "goodbye"

expect_warning(diff_str(a, b, comp.str="^"), "Specifying")
expect_warning(diff_str(a, b, comp="^"), "Specifying")
expect_warning(diff_str(a, b, indent.str="..."), "Specifying")
expect_warning(diff_str(a, b, indent="..."), "Specifying")

# Max limit warnings work properly; these are not fully fleshed out

A3 <- c("a b c", "d e f A B C D", "g h i", "f")
B3 <- c("a b c", "xd e f E Q L S", "g h i", "q")

expect_warning(diff_chr(A3, B3, max.diffs=2), "max.diffs")
expect_warning(diff_chr(A3, B3, max.diffs.in.hunk=2), "max.diffs.in")
expect_warning(diff_print(A3, B3, max.diffs.wrap=2), "max.diffs.wrap")
