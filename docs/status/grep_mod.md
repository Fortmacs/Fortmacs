# grep_mod

- Module: grep_mod
- Upstream: progmodes/grep.el (+ find-dired.el `find-name-arg`)
- State: partial

## Ported

`grep` (M-x grep, the prompt with `grep-default-command` and C-u, `grep-command-position`),
`grep-find` (`find-grep`),
`lgrep` (C-u: confirm, C-u C-u: edit `grep-command`),
`rgrep`,
`zrgrep` (`rzgrep`),
`kill-grep`,
`grep-mode` (a Compilation mode: `grep-mode-map`, `grep-regexp-alist` with the `--null` separator shown as ":" and the match columns, `grep-mode-font-lock-keywords`, the "Grep finished" transforms, `grep-last-buffer`, `grep-hit-face`, `grep-error`, the mode line's match count),
`grep-process-setup` (GREP_COLORS),
`grep-exit-message`,
`grep-filter` (the `match` face, escape sequences removed),
`grep-use-headings` (`grep--heading-filter`, `grep-heading`, `grep-heading-regexp`),
`grep-compute-defaults` (probing grep, find and xargs: `grep-use-null-device`, `grep-use-null-filename-separator`, `grep-highlight-matches`, `grep-find-use-xargs`, `grep-command`, `grep-template`, `grep-find-command`, `grep-find-template`; `grep-host-defaults-alist` for the local host),
`grep-apply-setting`,
`grep-tag-default`,
`grep-expand-template`,
`grep-read-regexp`,
`grep-read-files` (`grep-files-aliases`, `grep-read-files-function`, the merged completion table),
`grep-find-ignored-files` (function and variable, the latter `grep_find_ignored_files_variable`),
`rgrep-find-ignored-directories`,
`grep-find-ignored-directories`,
`rgrep-default-command`,
`rgrep-find-ignores-in-<f>` (`rgrep_find_ignores_in_f`),
`grep-find-abbreviate` and `grep-find-toggle-abbreviation`,
`grep-save-buffers` (one choice setting, its function value in `grep_save_buffers%procedure`, R6-37),
`grep-setup-hook`,
`grep-file-at-point`,
Grep-Edit mode (`grep-change-to-grep-edit-mode` e, `grep-edit--prepare-buffer`, `grep-edit-save-changes` C-c C-c, `grep-edit-mode-hook`)

## Deferred

C-c C-f (`next-error-follow-minor-mode`),
menu and tool bar,
the `keymap` of the abbreviated command,
`kill-transform-function`,
the outline functions of `grep-use-headings`,
`grep-window-height`,
`grep-search-path`,
`find-tag-default-function`,
`file-name-at-point-functions`,
the case-fold toggle of `read-regexp`,
remote directories (`grep-quoting-style` posix)

## Tests

test_grep_mod (ports grep-tests--rgrep-abbreviate-properties-gnu-linux (the darwin and windows-nt cases are not applicable), grep-tests--grep-heading-regexp-without-null, -with-null and compile-tests.el's compile-test-grep-regexps; own: `wildcard-to-regexp`, `grep-expand-template`, a grep run with next-error);
own test of Grep-Edit mode
