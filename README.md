<h1 align="center"><img src="docs/logo/logo.svg" alt="Fortmacs" width="448"></h1>

> **Not usable yet.** This is an early, experimental port; it builds and
> runs, but most of Emacs is missing.

GNU Emacs 31.1 rewritten in modern Fortran (2018+), for Linux on x86-64 and
aarch64. There is no Elisp: your configuration is a Fortran module compiled
into the binary, and every rebuild is a numbered generation you can roll
back to, as in Guix.

<!-- progress:start -->
**Progress:** early porting. Fortmacs runs as a bare-bones terminal editor; most of Emacs is still
missing. See [PORTING.md](PORTING.md) for the roadmap and what works so far.

<picture>
<source media="(prefers-color-scheme: dark)" srcset="docs/progress/pie-dark.svg">
<img src="docs/progress/pie.svg" width="520" height="180" alt="Ported: ≅3.92% of Emacs's C functions and Elisp definitions and the packages in GNU ELPA, NonGNU ELPA and MELPA">
</picture>
<!-- progress:end -->

The project is funded entirely by one person. Every agent run costs tokens,
so token usage and its cost set the pace.

## Building

Requires gfortran ≥ 13 and [fpm](https://fpm.fortran-lang.org); development
also needs [fprettify](https://github.com/fortran-lang/fprettify) 0.3.7 and
[Fortitude](https://fortitude.readthedocs.io) 0.9.2.

```sh
fpm build
fpm run -- FILE         # edit FILE (C-x C-s saves, C-x C-c exits)
tools/check.sh          # format, lint, debug build and tests, as CI runs them
tools/check.sh --fix    # reformat and apply safe lint fixes
```

CI runs the same check on Ubuntu and Fedora (x86-64 and aarch64) and Arch
Linux (x86-64) for every pull request and after each merge, and on Guix
weekly.

## Configuring

Write your configuration in `~/.config/fortmacs/init.f90`
([docs/init.f90](docs/init.f90) is an example): a module `user_config` whose
`configure` subroutine runs at startup. Settings are variables
(`tab_width = 4`), built-in commands are objects
(`commands%upcase_region%disabled = .false.`), and keys are bound with
`keymap_global_set("C-c d", "my_command", status)`.

```sh
fpm install --profile release   # fortmacs, its library and modules, in ~/.local
fortmacs --recompile            # build init.f90 into a new generation
fortmacs -q                     # start without your configuration
fortmacs --list-generations     # every build, the current one marked
fortmacs --rollback             # back to the generation before
fortmacs --switch-generation 3  # or to any generation
```

A failed build shows the compiler's errors and leaves your last good build
running. Each generation is kept with the `init.f90` it was built from in
`~/.local/state/fortmacs/generations`.

## Why Fortran?

1. **Real parallel threads.** Emacs runs one thread at a time under a global
   lock. Fortmacs keeps one thread for buffers and the screen, as Emacs does,
   and will run heavy work (searching large files, file I/O, syntax parsing,
   language servers) on other cores, each worker with its own copy of the
   data. Not implemented yet; see "Threads" in [PORTING.md](PORTING.md).
2. **Fast native code.** Compiled ahead of time by optimizing compilers,
   with no garbage-collection pauses (objects are freed explicitly) and no
   init files to evaluate at startup.
3. **Errors caught at compile time.** Static types, `implicit none` and
   argument intents turn many mistakes into compile errors, in your
   configuration too.
4. **A stable standard.** Fortran is an ISO standard with many independent
   compilers; code from decades ago still compiles.
5. **Syntax.** Blocks say where they end (`end if`, `end do`) instead of
   ending where parentheses balance.
   [Compare the two side by side](https://vermi-culous.github.io/Fortmacs/syntax.html).
   That's a preference, not a verdict on Elisp.

6. **Help that speaks Fortran.** Emacs is at its best as an environment for
   the language it's written in: it documents every function and variable,
   shows their current values and jumps to their source. Fortmacs aims to
   bring that to Fortran: help shows Fortran names, values and source files,
   so exploring and configuring the editor feels like working in Fortran.

## Differences from Emacs

Fortmacs follows Emacs 31.1 exactly, except where being Fortran requires
otherwise:

- Names use underscores: `M-x find_file`, `C-h v fill_column`,
  `text_mode_hook`. Keys and mode-line text are unchanged.
- There is no Lisp to evaluate at run time: no `M-:`, `eval-buffer` or
  `ielm`, and `*scratch*` starts in Fundamental mode. Changing the
  configuration means rebuilding.
- Help shows Fortran values (`.true.`, `8`, `"text"`, named constants) and
  names the Fortran file a definition lives in.
- Configuration and packages are Fortran modules compiled into the binary;
  commands that save settings write Fortran into `init.f90`.

## Why AI assistance?

Emacs has tens of thousands of C and Elisp definitions, plus over 7,000
Elisp packages (509 in GNU ELPA, 293 in NonGNU ELPA, 6,333 in MELPA). A
handful of porters could never keep up, and maintainers shouldn't be asked
to port their work to a niche experiment, so without help Emacs in Fortran
would never happen. This is for fun. Let's see where it goes.

AI has made breakthroughs once out of reach, from AlphaFold's 200 million
protein structures (part of the 2024 Nobel Prize in Chemistry) to the
antibiotic candidate halicin, yet AI-assisted coding still carries a
stigma. Fortmacs judges its code like any other: it must build, pass
Emacs's own tests and survive human review.

Fortmacs is co-written with Claude Code, Anthropic's coding agent. It needs
constant direction: left alone it drifted toward Lisp written in Fortran,
such as `call put("upcase-region", "disabled", .false., status)` where
Fortmacs now has `commands%upcase_region%disabled = .false.`. In return it:

- reads C, Elisp and Fortran, porting each function with a citation to its
  source and porting Emacs's tests with it;
- applies the conventions consistently across thousands of functions, with
  up to eight agents working in parallel;
- wrote about 65,000 lines of Fortran and tests, not counting generated
  tables, in the first three days; token cost, not typing, sets the pace.

Every change is built, tested and checked for memory leaks in CI on two
architectures, and a human reviews and merges every pull request. GNU Emacs
does not accept AI-generated contributions, so upstream stays a read-only
reference and nothing is sent back to it.

## Reference source

Ports follow GNU Emacs tag `emacs-31.1`. `tools/fetch-upstream.sh` places a
verified shallow checkout in `upstream/emacs/` (git-ignored).

## License

GPLv3 or later, matching upstream; see [LICENSE](LICENSE). Files ported from
GNU Emacs keep the Free Software Foundation copyright notice alongside ours.
