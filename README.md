# Fortmacs

A rewrite of GNU Emacs 31.1 in modern Fortran (2018+), for Linux on x86-64
and aarch64. Configuration is Fortran too: your `init.f90` is compiled into
the binary, xmonad-style. There is no Elisp.

**Status:** early porting. `fortmacs FILE` opens a file in the terminal
where you can type, delete, move within lines and save (`C-x C-s`, with a
`FILE~` backup), but most of Emacs is still missing: moving between lines,
the minibuffer, undo, search and much more. See [PORTING.md](PORTING.md)
for the roadmap and progress.

<!-- progress:start -->
**Progress:**

*Main*

Total translations: 427 of 56,864\
`[░         ]` (≆0.75%)

Upstream C functions: 388 of 8,547\
`[▒         ]` (≆4.50%)

Elisp definitions: 39 of 48,317\
`[░         ]` (≆0.25%)

*Packages*

Total packages: 0 of 7,135\
`[          ]` (0.00%)

GNU ELPA: 0 of 509\
`[          ]` (0.00%)

NonGNU ELPA: 0 of 293\
`[          ]` (0.00%)

MELPA: 0 of 6,333\
`[          ]` (0.00%)

A rough estimate from `tools/progress.sh`.
<!-- progress:end -->

## Building

Requires gfortran ≥ 13, [fpm](https://fpm.fortran-lang.org) and, for
development, [fprettify](https://github.com/fortran-lang/fprettify) 0.3.7
and [Fortitude](https://fortitude.readthedocs.io) 0.9.2.

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
- **Readable syntax.** Lisp's parentheses have devoted fans, but for many
  people the sea of them is where they give up on Elisp: meaning hides in
  nesting depth, and a misplaced paren changes what code does. Fortran
  spells its structure out in words. Blocks say where they end (`end if`,
  `end do`, `end subroutine name`), variables are declared with their types
  and how they are used, and arguments can be passed by name. A command
  that underlines the current line, in Elisp:

  ```elisp
  (defun underline-line ()
    "Insert a line of dashes as long as the current line."
    (interactive)
    (let ((width (- (line-end-position) (line-beginning-position))))
      (end-of-line)
      (insert "\n" (make-string width ?-))))
  ```

  and in Fortran, written as Fortmacs's own commands are:

  ```fortran
  !> Insert a line of dashes as long as the current line.
  subroutine underline_line(status)
      type(status_type), intent(out) :: status
      integer(int64) :: width

      width = line_end_position() - line_beginning_position()
      call end_of_line()
      call insert(new_line("a")//repeat("-", width), status)
  end subroutine underline_line
  ```
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

**And why not?** Rewriting Emacs in another language used to be the kind
of idea people joked about and never tried, because it meant years of
volunteered evenings for an experiment few would use. New tools have
changed that: experiments that once stayed hypothetical can now become
real without anyone giving up years of their time. Fortmacs is one of
them, and finding out how far it goes is half the fun.

## Why AI assistance?

As the progress block shows, tens of thousands of definitions need
rewriting in Fortran. That isn't only Elisp but Emacs's internal C as
well. One person would have to master three languages, and even then it
would be a monumental undertaking. Emacs is niche as it is; Emacs in
Fortran would probably never happen.

And that is only Emacs itself. Much of what people love about Emacs lives
in its packages: GNU ELPA, NonGNU ELPA and MELPA hold over 7,000 of them
(509, 293 and 6,333), every one written in Elisp. For Fortmacs to be
usable, everyone's favorite package has to be available too, so they all
need translating as well.

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
