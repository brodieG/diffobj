## Compiled Testing

Run the 'ses' tests with valgrind.  They should have 100% coverage of the C
code.

We also tracked down these:

```
==204== Conditional jump or move depends on uninitialised value(s)
==204==    at 0x41E87D7: ???
==204==    by 0xBC983A7: ???
==204==    by 0xBC983A7: ???
==204==    by 0xBC983F9: ???
==204==    by 0x1FFEFF046F: ???
==204==    by 0x2F: ???
```

To this call:

```
debug: base::nchar(strip_style(x), ...)
Browse[3]> strip_style(x)
==204== Conditional jump or move depends on uninitialised value(s)
==204==    at 0x41E87D7: ???
==204==    by 0xC6C0E77: ???
==204==    by 0xC6C0E77: ???
==204==    by 0xC6C0E81: ???
==204==    by 0x1FFEFF046F: ???
==204==    by 0x2F: ???
```

Seems like something funny going on with the perl grep.

That said, we were unable to reproduce by calling `gsub` directly.

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
