# terminal_colors_mod

- Module: terminal_colors_mod (+ submodule terminal_colors_startup_mod)
- Upstream: term/tty-colors.el, xfaces.c (`tty-defined-color-alist`, `color-values-from-color-spec` / `parse_color_spec`), startup.el and faces.el (the color part of terminal setup)
- State: partial

## Ported

`tty-defined-color-alist` (one terminal),
`tty-standard-colors`,
`tty-color-alist`,
`tty-modify-color-alist`,
`tty-register-default-colors`,
`tty-color-canonicalize`,
`tty-color-24bit`,
`tty-color-define`,
`tty-color-clear`,
`tty-color-off-gray-diag`,
`tty-color-approximate`,
`tty-color-standard-values` (#RGB, rgb:, rgbi:, names),
`tty-color-translate`,
`tty-color-by-index`,
`tty-color-values`,
`tty-color-desc`,
`tty-color-gray-shades`;
`tty_register_colors` (startup.el's tty-register-default-colors, then xterm's colors) at terminal setup;
`clear_face_cache_hook` where upstream calls clear-face-cache

## Deferred

`tty-color-mode-alist` and the tty-color-mode frame parameter;
FRAME/DISPLAY arguments;
an xterm color error does not cut command-line short

## Tests

test_terminal_colors_mod (tty-colors-tests.el + own)
