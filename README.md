# Fortmacs

A rewrite of GNU Emacs 31.1 in modern Fortran (2018+), for Linux on x86-64
and aarch64. Configuration is Fortran too: your `init.f90` is compiled into
the binary, xmonad-style. There is no Elisp.

**Status:** early porting; not usable as an editor yet. See
[PORTING.md](PORTING.md) for the roadmap and progress.

**Progress:** Done 297 of 56,864 (~0.52%): 278 of 8,547 upstream C
functions and 19 of 48,317 Elisp definitions. A rough estimate from
`tools/progress.sh`.

## Building

Requires gfortran ≥ 13, [fpm](https://fpm.fortran-lang.org) and, for
development, [fprettify](https://github.com/fortran-lang/fprettify) 0.3.7.

```sh
fpm build
fpm run -- --version
tools/check.sh          # format check + debug build + tests (what CI runs)
tools/check.sh --fix    # reformat sources in place
```

## Reference source

Ports follow GNU Emacs tag `emacs-31.1`. `tools/fetch-upstream.sh` places a
verified shallow checkout in `upstream/emacs/` (git-ignored).

## License

GPLv3 or later, matching upstream; see [LICENSE](LICENSE). Files ported
from GNU Emacs keep the Free Software Foundation copyright notice alongside
ours.
