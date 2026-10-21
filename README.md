# Fortmacs

A rewrite of GNU Emacs 31.1 in modern Fortran (2018+), for Linux on x86-64
and aarch64. Configuration is Fortran too: your `init.f90` is compiled into
the binary, xmonad-style. There is no Elisp.

**Status:** early porting. `fortmacs FILE` opens a file in the terminal
where you can type, delete, move within lines and save (`C-x C-s`, with a
`FILE~` backup), but most of Emacs is still missing: moving between lines,
the minibuffer, undo, search and much more. See [PORTING.md](PORTING.md)
for the roadmap and progress.

**Progress:** Done 427 of 56,864 (~0.75%): 388 of 8,547 upstream C
functions and 39 of 48,317 Elisp definitions. A rough estimate from
`tools/progress.sh`.

## Building

Requires gfortran ≥ 13, [fpm](https://fpm.fortran-lang.org) and, for
development, [fprettify](https://github.com/fortran-lang/fprettify) 0.3.7
and [Fortitude](https://fortitude.readthedocs.io) 0.9.2
(`pip install fprettify==0.3.7 fortitude-lint==0.9.2`).

```sh
fpm build
fpm run -- --version
fpm run -- FILE         # edit FILE in this terminal (C-x C-s saves, C-x C-c exits)
tools/check.sh          # format + lint check, debug build, tests (what CI runs)
tools/check.sh --fix    # reformat and apply safe lint fixes in place
```

CI runs the same check on Ubuntu and Fedora (x86-64 and aarch64) and on
Arch Linux (x86-64), and builds and tests on Guix weekly.

## Reference source

Ports follow GNU Emacs tag `emacs-31.1`. `tools/fetch-upstream.sh` places a
verified shallow checkout in `upstream/emacs/` (git-ignored).

## Why Fortran?

It started as a way to have fun while learning the language. Along the way
it became clear that Fortran brings real benefits over Emacs Lisp:

- **Compiled ahead of time to fast native code.** Elisp normally runs as
  byte-code. Since Emacs 28 it can be natively compiled through libgccjit,
  which the manual puts at roughly 2.5–5× faster than byte-code. Fortran is
  compiled ahead of time by optimizing compilers built for exactly that
  kind of speed.
- **Real parallelism.** Elisp threads are cooperative: only one runs at a
  time, and they switch only at certain points, such as while waiting for
  input. Fortmacs keeps one thread for editing but can run background work
  (file I/O, searching large files) on true parallel threads.
- **Less to wait for.** These are design properties, not benchmarks
  (Fortmacs hasn't been measured against Emacs yet):
  - No garbage-collection pauses. Emacs stops now and then to collect
    garbage, which is why so many configs raise `gc-cons-threshold`.
    Fortmacs frees objects explicitly, so it has no collector to pause for.
  - Nothing to load at startup. Emacs reads and evaluates your init files
    every time it starts; in Fortmacs your config is already compiled into
    the binary.
  - Native data. Elisp values are tagged objects, and floating-point
    numbers are allocated on the heap. Fortran works with plain machine
    integers, reals and arrays.
- **Errors caught at compile time.** Fortran is statically typed. With
  `implicit none` and declared argument intents, many mistakes are compile
  errors instead of runtime surprises.
- **A stable, standardized language.** Fortran is an ISO standard (most
  recently Fortran 2023) with many independent compilers: GNU, LLVM Flang,
  Intel, NAG and others. Code from decades ago still compiles. Elisp exists
  only inside GNU Emacs.
- **Configuration is real code.** Your `init.f90` is compiled with the
  editor and type-checked, and every rebuild is kept so you can roll back.

The trade-off: you can't evaluate code in a running session. Changing the
config means rebuilding (`fortmacs --recompile`).

## Why AI assistance?

As the progress line shows, tens of thousands of definitions need
rewriting in Fortran. That isn't only Elisp but Emacs's internal C as
well. One person would have to master three languages, and even then it
would be a monumental undertaking. Emacs is niche as it is; Emacs in
Fortran would probably never happen.

An AI assistant changes the math:

- **It already reads all three languages.** It can study an upstream C or
  Elisp function and write the Fortran port with the source cited, instead
  of someone learning each codebase first.
- **It handles the mechanical bulk.** Much of the work is faithful,
  repetitive translation that follows a fixed set of conventions. An
  assistant applies them consistently across thousands of functions.
- **It ports the tests too.** Upstream's test suites come over alongside
  the code, so each port is checked against Emacs's own expectations.
- **The safeguards stay human.** Every change is built, tested and checked
  for memory leaks in CI on two architectures, and a human reviews and
  merges every pull request.

GNU Emacs does not accept AI-generated contributions. Fortmacs respects
that: upstream is used as a read-only reference, and nothing from this
project is ever sent back to it.

## License

GPLv3 or later, matching upstream; see [LICENSE](LICENSE). Files ported
from GNU Emacs keep the Free Software Foundation copyright notice alongside
ours.
