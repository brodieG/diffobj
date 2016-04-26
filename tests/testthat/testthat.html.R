# Tests need to fleshed out

# Verify that internal css works

st <- diffObjStyleHtml(css.mode="internal")
res <- diff_str(mdl1, mdl2, mode="s", context=1, etc=etc(style=st))
as.character(res)
res
