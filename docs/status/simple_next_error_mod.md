# simple_next_error_mod

- Module: simple_next_error_mod (submodule of simple_mod)
- Upstream: simple.el (next-error)
- State: partial

## Ported

`next-error` (M-g n, M-g M-n, C-x `; C-u resets), `previous-error` (M-g p, M-g M-p), `first-error`, `next-error-no-select`, `previous-error-no-select`, `next-error-function` (a buffer-local procedure setting: `%procedure`, `%set_local`, `%get_current`, cleared by kill-all-local-variables), `next-error-last-buffer`, `next-error-buffer-p`, `next-error-find-buffer`, `next-error-internal`, `next-error-found`, `next-error-hook`, `next-error-verbose`

## Deferred

highlighting the locus (`next-error-highlight`, overlays),
the fringe arrow,
`next-error-recenter`,
`next-error-find-buffer-function`,
`next-error-found-function`,
`next-error-move-function`,
`next-error-select-buffer`,
`next-error-follow-minor-mode`,
`recenter-current-error`,
the repeat map

## Tests

test_compile_mod, test_buffer_local_setting_mod (own: `next_error_function` per buffer)
