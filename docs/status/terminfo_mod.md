# terminfo_mod

- Module: terminfo_mod
- Upstream: terminfo.c (`tparam`), plus the libtinfo interface Emacs uses, reimplemented
- State: done

## Ported

compiled entry parser (16- and 32-bit numbers, extended capabilities),
ncurses search path,
`tgetent`,
`tgetstr`/`tgetnum`/`tgetflag` (termcap codes),
`tigetstr`/`tigetnum`/`tigetflag`,
`tparm` (full %-language and printf formats),
`tgoto`,
padding removal,
built-in xterm-256color

## Deferred

string parameters (%s),
padding delays,
the termcap database

## Tests

test_terminfo_mod (own; checked against infocmp)
