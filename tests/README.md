# Tests

## Structure of a test suite

A test suite is a subdirectory of this directory.  The name of the
directory is the source language of `flap` to be tested.  We name this
top-level directory `src-lang/` below.

In `src-lang/`, there is a file `extension` that contains the file
extension used for the source programs to test.  The syntax of this file
is:

```
EXT=<src-ext>
```

Then, there are subdirectories in `src-lang/` that represents a subset
of tests for testing a specific feature.  For example, there may be
a subdirectory for tests of the interpreter of the language, another for
tests of a compiler for the language, etc.  We name `feature/` one of
these subdirectories.

In `feature/`, there are two subdirectories (namely `good/` and `bad/`),
representing respectively positive and negative tests.  In these
directories, the file `options` contains the options passed to `flap`
for the tests.

## How to add a new test?

If you want to add a new positive (resp. negative) test to an existing
test suite, just create a file
`src-lang/feature/good/new_test.<src-ext>`
(resp. `src-lang/feature/bad/new_test.<src-ext>`) containing the source
program to be tested.  For positive tests, you must also add a file
`src-lang/feature/good/new_test.expected` containing the expected
output/result to mark the test as OK.
