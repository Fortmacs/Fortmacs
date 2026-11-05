# make_mode_mod

- Module: make_mode_mod
- Upstream: progmodes/make-mode.el
- State: partial

## Ported

`makefile-mode` and its variants `makefile-automake-mode`,
`makefile-gmake-mode`,
`makefile-makepp-mode`,
`makefile-bsdmake-mode`,
`makefile-imake-mode` (keywords from `makefile-make-font-lock-keywords`, `makefile-syntax-propertize-function`, the syntax tables, `makefile-mode-map`, `makefile-mode-abbrev-table`),
the faces,
all the options,
the electric keys (`makefile-electric-keys`: `$`, `:`, `=`, `.`, bound when the first Makefile mode runs, as loading make-mode.el does),
`makefile-next-dependency` (M-n),
`makefile-previous-dependency` (M-p),
`makefile-insert-target`,
`makefile-insert-target-ref` (C-c :),
`makefile-insert-macro`,
`makefile-insert-macro-ref`,
`makefile-insert-special-target`,
`makefile-pickup-targets`,
`makefile-pickup-macros`,
`makefile-pickup-everything` (C-c C-p),
`makefile-pickup-filenames-as-targets` (C-c C-f),
the buffer-local target and macro tables,
`makefile-completions-at-point` (C-M-i, with its exit function),
`makefile-backslash-region` (C-c C-\),
`makefile-fill-paragraph` (M-q),
`makefile-create-up-to-date-overview` (C-c C-u, `makefile-query-by-make-minus-q`),
`makefile-insert-gmake-function` (C-c TAB),
`makefile-cleanup-continuations`,
`makefile-warn-suspicious-lines` and `makefile-warn-continuations` on `write-file-functions`,
`makefile-add-log-defun` (Makefile mode's `add-log-current-defun-function`),
`makefile-match-dependency`,
`makefile-match-action`,
`makefile-match-function-end`;
dabbrev's `dabbrev-abbrev-skip-leading-regexp` becomes a setting (buffer-local here);
`makefile-imenu-generic-expression` (with `makefile-previous-dependency` as a matcher) and makepp's "Functions" (step 49c)

## Deferred

the obsolete browser (C-c C-b, obsolete since 30.1),
the menu,
`cpp-font-lock-keywords` in the Imake keywords,
the obsolete `makefile-generate-temporary-filename`;
the overview sorts its lines itself (sort.el is not ported)

## Tests

test_make_mode_mod (ports make-mode-tests--bug17400; own tests of the keywords, motion, pickup, insertion, completion, backslashes, filling, the save checks, `makefile-add-log-defun`, auto-mode-alist and `make-temp-name`);
test_index_menu_mod (the index)
