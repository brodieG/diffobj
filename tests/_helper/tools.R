# Mock a function by tracing it's guts out.  Untrace to unmock.

# Quick and dirty, not thoroughly tested.

mock <- function(f, tracer, where=f, print=FALSE)  {
  editor <- function(name, file, title) {body(name) <- tracer; name}
  old.edit <- options(editor=editor)
  on.exit(options(old.edit))
  invisible(
    eval(
      bquote(trace(.(substitute(f)), edit=TRUE, print=FALSE, where=.(where))),
      parent.frame()
) ) }

