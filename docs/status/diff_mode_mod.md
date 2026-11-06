# diff_mode_mod

- Module: diff_mode_mod, diff_mode_apply_mod, diff_mode_add_log_mod (submodules)
- Upstream: vc/diff-mode.el (+ emacs-lisp/easy-mmode.el `easy-mmode--next`/`--prev` in easy_minor_mode_mod)
- State: partial

## Ported

`diff-mode` (`diff-mode-shared-map`, `diff-mode-map`, `diff-mode-read-only-map` and `diff-mode-read-only` for read-only buffers, `diff-default-read-only`, `diff-mode-hook`, `diff-setup-buffer-type`, `diff-buffer-type`, `next-error-function`), `diff-default-directory` and `diff-vc-backend` as forwarded string settings (`%set_local`, `%value`), `diff-vc-revisions`,
the faces and `diff-font-lock-keywords` (git preamble and email signature),
hunk and file motion (n, p, N, P, TAB, <backtab>, {, } as easy-mmode navigation keeping `diff-restrict-view` narrowing; M-n etc. through the ESC prefix to the shared map),
`diff-beginning-of-hunk`,
`diff-end-of-hunk`,
`diff-bounds-of-hunk`/`-file`,
`diff-restrict-view` (C-c C-n, r, W widens),
`diff-hunk-kill` (k),
`diff-file-kill` (K),
`diff-kill-junk`,
`diff-delete-other-hunks` (C-c RET n),
`diff-split-hunk` (C-c C-s, s),
`diff-fixup-modifs`,
`diff-hunk-file-names`,
`diff-find-file-name` (`diff-remembered-files-alist`, `diff-filename-drop-dir`),
`diff-hunk-text`,
`diff-find-text`,
`diff-find-approx-text`,
`diff-find-source-location`,
`diff-sanity-check-hunk`,
`diff-apply-hunk` (C-c C-a, C-u for reverse, the dwim undo, backups),
`diff-test-hunk` (C-c C-t),
`diff-apply-buffer` (C-c RET a),
`diff-revert-and-kill-hunk` (C-c M-u, u, @),
`diff-goto-source` (RET, o, C-c C-c),
`diff-jump-to-old-file`,
`diff-advance-after-apply-hunk`,
`diff-ask-before-revert-and-kill-hunk`,
`diff-valid-unified-empty-line`;
in diff_mode_add_log_mod: `diff-add-log-current-defuns` (`diff_add_log_current_subprograms`),
`diff-add-log-use-relative-names`,
`diff-current-defun` (`diff_current_subprogram`, Diff mode's `add-log-current-defun-function`) and its `add-log-buffer-file-name-function`,
`diff-add-change-log-entries-other-window` (`add_diff_change_log_entries_other_window`, C-x 4 A)

## Deferred

the Deferred list in diff_mode_mod: refinement,
syntax highlighting and prettifying,
unified/context conversion and reversing,
on-the-fly header updates,
`diff-minor-mode`,
Ediff,
whitespace commands,
`diff-kill-ring-save`,
outline,
imenu,
the menu,
mouse,
VC revisions,
`diff-wiggle`

## Tests

test_diff_mode_mod (ports diff-mode-test-ignore-trailing-dashes, -hunk-text-no-newline, the text-property halves of -font-lock and -font-lock-syntax-one-line, test-hunk-file-names, -fixups-added-lines, -fixups-empty-hunks, -git-patch, -git-patch/before-first-hunk (first check), -git-patch/signature, -topmost-addition-undo, -bottommost-addition-undo, -setup-buffer-type),
test_add_log_mod (own: `diff-add-log-current-defuns` and `diff-current-defun`),
test_buffer_local_setting_mod (own: `diff-default-directory` and `diff-vc-backend` per buffer, kill-all-local-variables)
