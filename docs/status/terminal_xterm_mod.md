# terminal_xterm_mod

- Module: terminal_xterm_mod
- Upstream: term/xterm.el
- State: partial

## Ported

`xterm-rxvt-function-map`,
`xterm-function-map` (cursor, editing, F1–F12, keypad, all modifier forms, modifyOtherKeys/formatOtherKeys),
`terminal-init-xterm` (input-decode-map and color parts),
`xterm-standard-colors`,
`xterm-rgb-convert-to-16bit`,
`xterm-register-default-colors` (8, 16, 88, 256 and 24-bit)

## Deferred

bracketed paste,
focus events,
terminal queries (background color, OSC 52),
rxvt (`rxvt-standard-colors`),
`xterm-alternatives-map`,
`terminal-init-xterm-hook`

## Tests

test_terminal_xterm_mod (own),
test_terminal_colors_mod (own)
