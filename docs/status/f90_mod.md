# f90_mod

- Module: f90_mod
- Upstream: progmodes/f90.el
- State: partial

## Ported

`f90-mode` (derived from Prog mode; the syntax table, `f90-mode-map`, the abbrev table with its `:regexp`, four levels of keywords with `f90-typedef-matcher` and `f90-typedec-matcher`, `comment-start`, `comment-indent-function`, `indent-line-function`, `indent-region-function`, `normal-auto-fill-function`, `fill-paragraph-function`, `abbrev-all-caps`),
indentation (`f90-indent-line`, `f90-indent-new-line` C-j, `f90-indent-region`, `f90-indent-subprogram` C-M-q, `f90-calculate-indent`, `f90-get-correct-indent`, `f90-comment-indent`, continuation lines, line numbers, preprocessor lines),
motion (`f90-previous-statement`/`-next-statement` C-c C-p/C-n, `f90-beginning-of-subprogram`/`-end-of-subprogram` C-M-a/C-M-e, `f90-mark-subprogram` C-M-h, `f90-end-of-block`/`-beginning-of-block` C-M-n/C-M-p with label checks, `f90-next-block`/`-previous-block` C-c C-e/C-a),
smart END statements (`f90-match-end`, `f90-block-match`, `f90-insert-end` C-c ],
`f90-smart-end`,
`f90-smart-end-names`),
`f90-comment-region` (C-c ;),
`f90-break-line` (C-c RET),
`f90-join-lines` (C-c C-d),
`f90-do-auto-fill`,
`f90-fill-region` (C-c C-f),
`f90-fill-paragraph`,
`f90-electric-insert`,
keyword case (`f90-change-keywords`, `f90-auto-keyword-case`, the up/down/capitalize commands),
`f90-abbrev-start` (`) and `f90-abbrev-help`, `f90-font-lock-1`…`-4`, all the user options and regexps; auto-mode-alist `.f90`, `.f95`, `.f03`, `.f08`; Imenu: `f90-imenu-generic-expression`, `f90-imenu-type-matcher`, `imenu-case-fold-search`; `f90-current-defun` as `add-log-current-defun-function` (step 49c)

## Deferred

the F90 menu,
`f90-add-imenu-menu` (the menu bar),
Hideshow,
`f90-backslash-not-special` (obsolete),
the `delete-selection` property,
`f90-cache-position` as a buffer-local variable

## Tests

test_f90_mod (ports f90-tests: all but the expected failure f90-test-bug3729; own: comment filling);
test_index_menu_mod (the index)
