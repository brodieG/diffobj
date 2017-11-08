# Contributing

Thank you for your interest in contributing to this package.  To make sure you
do not waste your time or mine, please read and follow the guidelines here.

## Reporting Issues

Please create a:

* minimal:        as little code as possible.
* reproducible:   include the **minimal** input data.
* example:        include the expected output.

Additionally:

* Include output of `sessionInfo()`
* Format the example code so that it can be copy-pasted into an R console

## Submitting PRs

### Before you Start

Create an issue that highlights the problem, and describe how you hope to solve
it.  Do not be offended if your offer for help is refused.  Accepting a PR
creates a maintenance burden that I might not be willing to take on.

I realize the requirements I lay out here are annoying.  If they prevent you
from making a contribution I am sorry and sympathize, having been on the
opposite side of such requirements myself.  Nonetheless the requirements stand
to ensure your contribution does not end up creating more work than it
saves.

### Requirements

* Check the diff prior to submitting the PR and make sure there are no
  unnecessary changes (e.g. meaningless white space changes, etc.).
* All PRs should be made as a new branch off of the "development" branch.
* Every line of code you contribute should be tested as shown by `covr`.
* Unit tests should be done in `unitizer`.
* You should license all contributions you make with a license compatible with
  that of the package, and you should ensure you are the copyright holder for
  all the contributions.

### Style Guide

Strict:

* Wrap text at 80 columns
* Indentations are 2 spaces
* Strip trailing whitespace

Suggested:

* function_name()
* object.name
* FormalClassName
* formalMethodName()

## Thank You!

For taking the time to read these contribution guidelines.  I apologize if they
seem a little hostile, but time, yours and mine, is precious and it would be a
shame to waste any of it.
