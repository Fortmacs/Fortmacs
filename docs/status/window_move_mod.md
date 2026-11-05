# window_move_mod

- Module: window_move_mod
- Upstream: windmove.el
- State: partial

## Ported

`windmove-left`,
`windmove-right`,
`windmove-up`,
`windmove-down` (the prefix argument as SIGN),
`windmove-find-other-window`,
`windmove-do-window-select` ("No window DIR from selected window", "Minibuffer is inactive"),
`windmove-wrap-around`,
`windmove-create-window` (`.true.`/`.false.`, a function as `windmove_create_window%procedure`, R6-37),
`windmove-allow-all-windows` (no window has a `no-other-window` parameter yet),
`windmove-mode` (global, on: its :init-value) with `windmove-mode-map`,
`windmove-install-defaults` (old bindings found with where-is-internal and unbound),
`windmove-default-keybindings` and `windmove-delete-default-keybindings` (MODIFIERS an optional `key_modifiers_type`, shift by default, `key_modifiers_type()` for none; PREFIX a key description, C-x by default),
`windmove-delete-in-direction` with `windmove-delete-left`,
`-right`,
`-up`,
`-down` (C-u also kills the buffer, M-0 deletes the selected window)

## Deferred

repeating a command to override `no-other-window` (`windmove-allow-repeated-command-override`, with window parameters),
the buffer display commands (`windmove-display-in-direction`, `windmove-display-left`, `-right`, `-up`, `-down`, `-same-window`, `-new-frame`, `-new-tab`, `windmove-display-default-keybindings`, `windmove-display-no-select`; they need `display-buffer-override-next-command`, and frames and tabs, step 44),
swapping window states (`windmove-swap-states-in-direction`, `windmove-swap-states-left`, `-right`, `-up`, `-down`, `windmove-swap-states-default-keybindings`; they need `window-swap-states` and window states),
the options set through their :set functions (`windmove-default-keybindings`, `windmove-display-default-keybindings`, `windmove-delete-default-keybindings`, `windmove-swap-states-default-keybindings`; Customize, step 26),
the warnings of `windmove-install-defaults` (warnings.el is not ported),
the obsolete `windmove-window-distance-delta`,
`windmove-coord-add`,
`windmove-constrain-to-range`,
`windmove-constrain-around-range`,
`windmove-frame-edges`,
`windmove-constrain-loc-for-movement`,
`windmove-wrap-loc-for-movement`,
`windmove-reference-loc`,
`windmove-other-window-loc`

## Tests

test_window_move_mod (own; upstream has no ERT tests for windmove.el: `window-in-direction`, the key bindings, moving, wrapping, creating and deleting windows)
