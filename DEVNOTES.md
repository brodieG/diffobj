
## Optim Notes

Using
```
profvis(for(i in 1:50) as.character(diffPrint(iris, iris.2)))
```

Getting a bit of time in `crayon_strip`, surprisingly, but only on the order of
10-15% of total eval time so of limited leverage.  Related, `nchar_fun` as well
since it also calls the strip fun.

Some overhead from parameter checks for `get_dat` that could probably be
avoided.

Calls to split/ave also start adding up a bit.

`check_args` non trivial at about 5% of total exec time.

Calls to `head` via `standardGeneric` -> `head.default`

`diff_myers` using S4 objects not fantastic.

A bit surprising that the only obvious source of overhead from ansi stuff is the
stripping of ansi tags.

Initial conclusion is that there isn't anything super obvious to do here.
Faster strip is probably the main improvement, but the rest is just going to be
a hard slog of going through a lot of code.
