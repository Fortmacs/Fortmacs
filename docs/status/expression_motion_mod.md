# expression_motion_mod

- Module: expression_motion_mod
- Upstream: emacs-lisp/lisp.el (named for what it does)
- State: partial

## Ported

`forward-sexp` (C-M-f, C-M-<right>),
`backward-sexp` (C-M-b, C-M-<left>),
`forward-list` (C-M-n),
`backward-list` (C-M-p),
`down-list` (C-M-d, C-M-<down>),
`backward-up-list` (C-M-u, C-M-<up>),
`up-list` (ESCAPE-STRINGS, NO-SYNTAX-CROSSING),
`kill-sexp` (C-M-k),
`backward-kill-sexp` (C-M-<backspace>, C-M-<delete>),
`mark-sexp` (C-M-@, C-M-SPC; extending),
the `-default-function`s they use,
`forward-sexp-function` (buffer-local when set; `forward-sexp` delegates to it, `up-list` steps with it when it is not the default),
`scan-error` as `user-error` interactively;
the subprogram commands: `beginning-of-defun` (C-M-a, C-M-<home>),
`beginning-of-defun-raw`,
`beginning-of-defun-comments`,
`end-of-defun` (C-M-e, C-M-<end>),
`mark-defun` (C-M-h),
`narrow-to-defun` (C-x n d),
`beginning-of-defun-function` (buffers' own),
`end-of-defun-function`,
`defun-prompt-regexp` (buffer-local when set),
`end-of-defun-moves-to-eol`,
`narrow-to-defun-include-comments`,
`buffer-end`;
`insert-pair` (`insert-pair-alist`, `parens-require-spaces`),
`insert-parentheses` (M-(), `delete-pair` (`delete-pair-blink-delay`, `delete-pair-push-mark`), `raise-sexp`, `check-parens`,
`forward-sexp-function` and `beginning-of-defun-function` as buffer-local procedure settings (`%procedure`, `%set_local`, `%get_current`, `%is_local`, `%clear_local`)

## Deferred

`forward-list-function`,
`down-list-function`,
`up-list-function`,
shift selection,
`kill-backward-up-list` (thingatpt.el's `thing-at-point`),
`move-past-close-and-reindent` (M-);
lisp-mode.el's `calculate-lisp-indent`);
not applicable: `field-complete`,
`lisp-complete-symbol`

## Tests

test_syntax_scan_mod (ports lisp-tests: lisp-forward-sexp-1-empty-parens, -1-error-mismatch, -1-eobp, -2-eobp, -2-eobp-and-subsequent, lisp-backward-sexp-1-empty-parens, -1-error-mismatch, -1-eobp, -2-bobp, -2-bobp-and-subsequent; own tests through the command loop),
test_expression_motion_mod (ports end-of-defun-twice, mark-defun-no-arg-region-inactive, lisp-delete-pair-parens, -quotation-marks, -quotes-in-text-mode, -quotes-text-mode-syntax-table, with a `;` comment syntax table for Emacs Lisp mode; own tests of `defun-prompt-regexp`, `narrow-to-defun`, `insert-parentheses`, `raise-sexp`, `check-parens`)
