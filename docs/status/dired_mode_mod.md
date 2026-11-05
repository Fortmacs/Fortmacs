# dired_mode_mod

- Module: dired_mode_mod (named so as not to clash with dired_mod), dired_aux_mod (submodule)
- Upstream: dired.el, dired-aux.el
- State: partial

## Ported

`dired` (C-x d),
`dired-other-window` (C-x 4 d),
`dired-noselect`,
`dired-internal-noselect` (one buffer per directory, `dired-auto-revert-buffer`, `dired-directory-changed-p`),
`dired-mode` (major mode, read-only, `dired-mode-map` with all the ported keys, `dired-mode-hook`, `dired-before-readin-hook`, `dired-after-readin-hook`, `dired-initial-position-hook`),
`dired-readin`,
`dired-insert-directory` (`dired-use-ls-dired` with `dired--check-use-ls-dired`, `dired-listing-switches`, wildcards, `dired-free-space` with `get-free-disk-space`),
`dired-align-file`,
`dired-insert-set-properties` (`dired-filename`, `mouse-face`, `invisible` for hide-details),
the *ls error* buffer,
`dired-build-subdir-alist`,
`dired-revert` with `dired-save-positions`/`dired-restore-positions` and remembered marks,
`dired-buffers-for-dir`,
`dired-goto-file`,
`dired-get-filename` (LOCALP, `ls -b` escapes),
`dired-move-to-filename` (`directory-listing-before-filename-regexp`),
`dired-move-to-end-of-filename`,
`dired-file-name-at-point`,
`dired-current-directory`,
`dired-make-relative`;
moving (n, p, SPC, <down>, <up>, C-n, C-p, >, <, ^, C-M-n, C-M-p, j, `dired-movement-style`),
visiting (RET, f, e, o, C-o, `dired-find-file`, `dired-display-file`),
marks (m, u, U, DEL, M-DEL, d, t, `* *`, `* /`, `* @`, `* %`, `* s`, `* m`, `* u`, `* t`, `* N`, `* DEL`, `* !`, `* ?`, `* c`, `* C-n`, `* C-p`, M-}, M-{, `% m`, `% d`, `% g`, `% &`, #, ~, the region with `dired-mark-region`),
deleting (x, D with `dired-mark-pop-up`'s *Deletions* window, `dired-deletion-confirmer`, `dired-recursive-deletes` with `read-answer`, `dired-no-confirm`, *Dired log*),
`dired-why`,
`dired-summary` (?),
`dired-undo` (C-x u, C-_),
`dired-copy-filename-as-kill` (w),
`dired-sort-toggle-or-edit` (s, the mode name "Dired by name"/"Dired by date"),
`dired-switches-in-mode-line` (a choice setting: DIRED_SWITCHES_IN_MODE_LINE_SORT_ORDER, _AS_IS, `%characters = N`, `%procedure => f`), + (`dired-create-directory`),
l (`dired-do-redisplay`),
`dired-add-file`,
`dired-update-file-line`,
`dired-kill-subdir`,
`dired-goto-subdir`,
`dired-mark-subdir-files`,
q (`quit-window`),
g (`revert-buffer`),
the Dired faces and `dired-font-lock-keywords` (marks, flagged and marked files, permissions, directories, symbolic links with `dired-check-symlinks`, special files, ignored extensions);
the dired-aux.el file operations (in dired_aux_mod): C,
R,
H,
S,
Y (`dired-do-copy`, `-rename`, `-hardlink`, `-symlink`, `-relsymlink` through `dired-do-create-files` and `dired-create-files`; `dired-dwim-target` with next, next-visible and recent; overwrite queries with `dired-query` (y, n, !, q and the *Dired help*); `dired-copy-preserve-time`, `dired-copy-dereference`, `dired-recursive-copies` with top as default; `dired-create-destination-dirs` and `-on-trailing-dirsep`; the `dired-keep-marker-*` options; `dired-do-revert-buffer`),
`% R`,
`% r`,
`% C`,
`% H`,
`% S`,
`% Y`,
`% u`,
`% l`,
M (symbolic and octal modes),
G and O (through `chgrp` and `dired-chown-program`),
T (`dired-touch-program`, with the date read),
Z (`dired-compress-file` with `dired-compress-file-suffixes` and directories as archives),
c (`dired-do-compress-to` with `dired-compress-files-alist`),
k (`dired-do-kill-lines`, with a prefix the subdirectory lines),
`dired-create-empty-file`,
v (`dired-view-file`),
y (`dired-show-file-type`),
`dired-unmark-all-files` with a prefix (`dired-query`);
the listing updates `dired-add-file`,
`dired-remove-file`,
`dired-rename-file` (renaming visiting buffers and subdirectory headers),
`dired-relist-file`,
`dired-unadvertise`,
`dired-check-process`,
`dired-bunch-files`,
`dired-mark-read-file-name`,
`dired-mark-confirm`,
`dired-log-summary`;
`=` (`dired-diff`) and `dired-backup-diff`;
step 44a: `dired-other-frame` (C-x 5 d),
`dired-other-tab` (C-x t d)

## Deferred

the list of files as the directory argument,
inserted subdirectories (i, $, `dired-switches-alist`),
`dired-hide-details-mode` (the properties are set; redisplay shows invisible text),
a (`dired-find-alternate-file`),
the dired-aux.el commands Deferred in dired_aux_mod (P, !, &, X, E, A, Q, `dired-compare-directories`, `.`, the VC commands, wdired's C-x C-q, HOW-TO functions),
function values of `dired-auto-revert-buffer`,
`dired-movement-style` t,
mouse and drag-and-drop,
file names with newlines and `dired-auto-toggle-b-switch`,
`dired-maybe-use-globstar`,
auto-revert,
directory-local variables,
dired-x,
trash,
`combine-change-calls`,
the free space shown by a `display` property (redisplay does not draw them yet)

## Tests

test_dired_mode_mod (ports dired-test-bug22694, -bug27243-01, -bug27243-02, -bug27968, -bug27940, -ls-error-message without the shell's sample message; own tests of the buffer, marks, deletion, keys, C-x d, + and ^, the values of `dired_switches_in_mode_line`);
test_dired_aux_mod (ports dired-test-bug28834, dired-test-bug30624; own tests of copy, rename with a visiting buffer, links, chmod, touch, k, `% u`, `% R`, compression, v, the overwrite query and `dired-unmark-all-files`)
