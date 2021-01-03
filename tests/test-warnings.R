NAME <- "warnings"
source(file.path('_helper', 'init.R'))

# tests designed to produce warnings

# - Extra args for `str` -------------------------------------------------------

a <- "hello"
b <- "goodbye"

invisible(diffStr(a, b, extra=list(comp.str="^"))) # "Specifying"
invisible(diffStr(a, b, extra=list(comp="^"))) #  "Specifying")
invisible(diffStr(a, b, extra=list(indent.str="..."))) #  "Specifying"
invisible(diffStr(a, b, extra=list(indent="..."))) # "Specifying"

# - Max diffs ------------------------------------------------------------------

# Max limit warnings work properly; these are not fully fleshed out

A3 <- c("a b c", "d e f A B C D", "g h i", "f")
B3 <- c("a b c", "xd e f E Q L S", "g h i", "q")

invisible(diffChr(A3, B3, max.diffs=2)) # warn: "Exceeded diff"

# - Overriden formals ----------------------------------------------------------

# warn "Provided `style` argument will override the provided `format` argument"
invisible(diffChr(letters, LETTERS, style=StyleRaw(), format="ansi8"))

# warn: "Provided `style` .* `format` and `color.mode` arguments"
invisible(
  diffChr(letters, LETTERS, style=StyleRaw(), format="ansi8", color.mode="rgb")
)

