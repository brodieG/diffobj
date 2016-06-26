# Tests need to fleshed out

# Verify that internal css works

x <- as.character(
  diffPrint(mdl1, mdl2, style=StyleHtmlLightYb(html.output="diff.only"))
)

x <- as.character(
  diffPrint(mdl1, mdl2, style=StyleHtmlLightYb(html.output="diff.w.style"))
)
f <- paste0(tempfile(), ".html") 
cat(
  file=f, "<html><head><title>A Test</title></head><body>", x, "</body></html>"
)
browseURL(f)
unlink(f)

# Mess up the CSS to test that we can change CSS file

f <- tempfile()
cat("div.row {background-color: red;}\nk", file=f)
diffPrint(mdl1, mdl2, style=StyleHtmlLightYb(css=f))
unlink(f)
