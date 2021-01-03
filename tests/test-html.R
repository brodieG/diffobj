NAME <- "html"
source(file.path('_helper', 'init.R'))

# Verify that internal css works

# - HTML Output Modes ----------------------------------------------------------

all.equal(
  as.character(
    diffPrint(
      letters[1:3], LETTERS[1:3],
      style=StyleHtmlLightYb(html.output="diff.only")
  ) ),
  rdsf(100)
)
all.equal(
  as.character(
    diffPrint(
      letters[1:6], LETTERS[1:6],
      style=StyleHtmlLightYb(html.output="diff.w.style")
  ) ),
  rdsf(200)
)
all.equal(
  as.character(
    diffPrint(
      letters[1:6], LETTERS[1:6],
      style=StyleHtmlLightYb(html.output="page")
  ) ),
  rdsf(300)
)
all.equal(
  as.character(
    diffPrint(
      letters[1:6], LETTERS[1:6], mode="unified",
      style=StyleHtmlLightYb(html.output="page")
  ) ),
  rdsf(350)
)
# - Sub CSS --------------------------------------------------------------------

# Mess up the CSS to test that we can change CSS file

local({
  f <- tempfile()
  on.exit(unlink(f))
  cat("div.row {background-color: red;}\n", file=f)
  all.equal(
    as.character(
      diffPrint(
        letters, LETTERS,
        style=StyleHtmlLightYb(css=f, html.output="diff.w.style")
      )
    ),
    rdsf(400)
  )
})
# - Tag funs -------------------------------------------------------------------

div_a <- div_f("A", c(color="red"))
all.equal(
  div_a(c("a", "b")),
  c(
    "<div class='A' style='color: red;'>a</div>",
    "<div class='A' style='color: red;'>b</div>"
  )
)
span_a <- span_f()
all.equal(span_a(c("a", "b")), c("<span>a</span>", "<span>b</span>"))

try(div_a(TRUE)) # "must be character"
all.equal(div_a(character()),character())

# - nchar ----------------------------------------------------------------------

all.equal(nchar_html("<a href='blahblah'>25</a>"), 2)
all.equal(nchar_html("<a href='blahblah'>25&nbsp;</a>"), 3)

# - cont_f ---------------------------------------------------------------------

try(cont_f("hello")(1:3)) # "must be character"
