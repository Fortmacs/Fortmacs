# font_core_mod

- Module: font_core_mod
- Upstream: font-core.el
- State: partial

## Ported

`font-lock-mode` (with its :after-hook `font-lock-initial-fontify`, `change-major-mode-hook` handling, hidden buffers),
`font-lock-defaults` (a typed buffer-local `font_lock_defaults_type`: keyword levels, KEYWORDS-ONLY, CASE-FOLD, SYNTAX-ALIST),
`font-lock-function`,
`font-lock-default-function` (the `font-lock-face` alias in `char-property-alias-alist`),
`font-lock-change-mode`,
`font-lock-defontify`,
`turn-on-font-lock`,
`font-lock-global-modes`,
`turn-on-font-lock-if-desired`,
`global-font-lock-mode` (define-globalized-minor-mode written out: `-enable-in-buffer`, `--set-explicitly`);
the keyword types of `font-lock-keywords`

## Deferred

OTHER-VARS of `font-lock-defaults` other than `font-lock-syntactic-face-function`,
keywords as variables to evaluate,
`define-globalized-minor-mode` in general

## Tests

test_font_lock_mod (own; font-lock-tests.el needs Lisp modes)
