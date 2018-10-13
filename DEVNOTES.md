## Crayon or Not Crayon

### ANSI detection And Settings

Do we want our substring functions to be ansi blind if we detect that the system
does not support ansi?

One problem right now is that when in 'raw' mode, we set `crayon.enabled=FALSE`,
but we might still be in a system that supports ANSI and the input itself may
have ANSI in it.

Ideally we move away from crayon for ansi detection and rely solely on the
`style` parameter.  So probably do that now.

But what about the special case where we are on a system that does not support
ANSI escapes, and we want to see the diffs in the ANSI escapes?  Do we just not
support this?

Probably better to add a separate "ansi.sgr.supported" that for now can default
to crayon::has_color()?  Then what do we do when someone uses and ANSI style but
this is off?  Just use it anyway?  And provide global nchar and substr functions
that are aware of this setting?  Yes.

## Fansi Problems

Would be great to use fansi instead of crayon for substr as it will be much
faster, and also support wide characters, but there are two major issues:

* We still need a terminal ansi-support test, which is well developed and
  tested in crayon.
* The fansi tests use `unitizer`, which suggests `diffobj`; need to check if
  this actually would cause a circular dependency or not.  So diffobj would
  import fansi, which suggests unitizer.  Currently unitizer imports diffobj.
  So there does seem to be build time circular dependency.  We could either
  change all the fansi tests to not be unitizer (or move the unitizer tests to
  `extra`, and have another version of them that tests output with .Rout files).
  This seems like a massive PITA, and also annoying because we'll need to make
  sure both sets of tests are run on travis, but not on CRAN, which then makes
  it hard to make sure we're submitting the same tarball.  The more realistic
  solution might be to remove `diffobj` as an `imports` and `suggests` so that
  it is possible to build unitizer without diffobj.

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
