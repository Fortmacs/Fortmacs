# conf_mode_mod

- Module: conf_mode_mod
- Upstream: textmodes/conf-mode.el (+ files.el `conf-mode-maybe`)
- State: partial

## Ported

`conf-mode` (guessing the sub-mode from the contents when called directly, `conf-mode--recursing`),
`conf-unix-mode`,
`conf-windows-mode`,
`conf-javaprop-mode`,
`conf-space-mode` (`conf-space-keywords`, `conf-space-keywords-alist`, `conf-space-mode-internal`),
`conf-colon-mode`,
`conf-ppd-mode`,
`conf-xdefaults-mode`,
`conf-toml-mode` (`conf-toml-recognize-section`),
`conf-desktop-mode`,
`conf-npmrc-mode`,
`conf-mode-initialize`,
the syntax tables,
keywords and `conf-mode-map`,
`conf-align-assignments` (C-c C-a; `conf-assignment-column`, `-sign`, `-space`, `-regexp`),
`conf-quote-normal` (C-c C-q),
`conf-space-keywords` (C-c C-s),
`conf-mode-maybe`;
Imenu's `imenu-generic-expression` of each mode (its changes in `conf-space-mode-internal`, `conf-colon-mode` and `conf-ppd-mode` too) (step 49c)

## Deferred

Outline (`outline-regexp`, `outline-heading-end-regexp`, `conf-outline-level`),
the menu,
`text-conversion-style`,
the modes' abbrev tables,
file-local `conf-space-keywords` (`hack-local-variables-hook`),
XML mode in `conf-mode-maybe`

## Tests

test_conf_mode_mod (ports conf-mode-tests: all 11; own: guessing, auto-mode-alist);
test_imenu_mod (the indexes)
