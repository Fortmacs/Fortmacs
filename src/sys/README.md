# src/sys

The only place ISO_C_BINDING may appear. Everything here wraps Linux system
interfaces (termios, read/write, signals, file I/O) behind plain Fortran
procedures so the rest of `src/` stays pure Fortran.

Rules:

- Linux only, x86-64 and aarch64. Struct layouts (`struct termios`,
  `struct winsize`, ...) are mirrored as `bind(C)` derived types and
  constants (`TCSANOW`, `ECHO`, `ICANON`, ...) are hardcoded, each with a
  comment naming the glibc/kernel header it comes from.
- Where a layout or constant differs between x86-64 and aarch64, say so in
  a comment and select it in one place.
- Prefer non-variadic calls (`tcgetattr`/`tcsetattr` over `ioctl`). Where a
  variadic call is unavoidable, document why it is safe on both targets.
- Signal handlers only set a flag; all work happens in the main loop.
- No `.c` files, ever.

Module names follow the upstream file they serve (`sysdep.c` → `fm_sysdep`,
`term.c` → `fm_term`), plus small `fm_sys_*` modules for raw bindings that
have no single upstream counterpart.
