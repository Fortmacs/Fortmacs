# dabbrev_mod

- Module: dabbrev_mod
- Upstream: dabbrev.el
- State: partial

## Ported

`dabbrev-expand` (M-/: backward, forward, then other buffers with a progress reporter and the file name parts; repeated M-/ cycling; SPC M-/ copying the following words; minibuffer origin),
`dabbrev--reset-global-variables`,
`dabbrev--select-buffers`,
`dabbrev--filter-buffer-modes`,
`dabbrev--same-major-mode-p`,
`dabbrev--goto-start-of-abbrev`,
`dabbrev--abbrev-at-point`,
`dabbrev--try-find`,
`dabbrev--ignore-buffer-p`,
`dabbrev--find-expansion`,
`dabbrev--make-friend-buffer-list`,
`dabbrev--substitute-expansion`,
`dabbrev--search`,
every setting (`dabbrev-case-fold-search`, `dabbrev-case-replace`, `dabbrev-case-distinction`, `dabbrev-abbrev-char-regexp`, `dabbrev-check-other-buffers`, `dabbrev-check-all-buffers`, `dabbrev-ignored-buffer-names/regexps/modes`, `dabbrev-limit`, `dabbrev-backward-only`, ...),
`dabbrev-completion` (C-M-/),
`dabbrev-capf`,
`dabbrev--find-all-expansions`

## Deferred

picture-mode replacement;
hippie-exp.el

## Tests

test_dabbrev_mod (ports dabbrev-expand-test, dabbrev-completion-test, -with-argument, -same-buffer-1 to -4, -other-buffer-1, -2, -4, dabbrev-expand-after-killing-buffer; own: cycling, case replacement, case-sensitive search)
