# vc_dir_mod

- Module: vc_dir_mod
- Upstream: vc/vc-dir.el
- State: partial

## Ported

`vc-dir` (C-x v d, C-u for the backend),
`vc-dir-root`,
`vc-dir-mode` and its keymap,
`vc-dir-prepare-status-buffer`,
`vc-dir-update`,
`vc-dir-refresh` (g),
`vc-dir-refresh-files`,
`vc-dir-headers`,
`vc-dir-busy`,
`vc-dir-kill-dir-status-process` (C-c C-c),
motion (n, p, SPC, S-SPC, TAB, <backtab>, M-}, M-{),
marking (m, u, M, U, DEL, M-DEL, % m, * %, * r, `vc-dir-mark-files`, `vc-dir-mark-state-files`, the mass-mark rules and `vc-dir-allow-mass-mark-changes`),
`vc-dir-root-next-action` (V),
the VC keys + (`vc-pull`),
P,
I,
O,
E L,
b c,
b l,
b L and b s,
`vc-dir-delete-file` (d),
`vc-dir-find-file` (f, e, RET),
`vc-dir-find-file-other-window` (o),
`vc-dir-display-file` (C-o),
`vc-dir-view-file`,
`vc-dir-ignore` (G),
`vc-dir-hide-state` (x),
`vc-dir-kill-line` (C-k),
`vc-dir-show-fileentry`,
`vc-dir-current-file`,
`vc-dir-marked-files`,
`vc-dir-marked-only-files-and-states`,
`vc-dir-child-files-and-states`,
`vc-dir-resynch-file` (after saves, reverts, checkins),
`vc-dir-deduce-fileset`,
`vc-dir-auto-hide-up-to-date`,
`vc-dir-save-some-buffers-on-revert`,
`vc-default-dir-printer`,
`vc-default-dir-extra-headers`,
the faces

## Deferred

search and replace in marked files (S, Q, M-s a C-s),
`vc-dir-clean-files`,
the menu,
tool bar and mouse maps,
the keys of commands not ported yet (unintegrated: T, E =, E D),
the outgoing count,
`vc-dir-kill-query`,
the backend's VC-Dir minor mode,
the `display-state` of asynchronous checkins,
desktop,
bookmarks,
project buffers,
directory-local variables

## Tests

test_vc_mod (own: C-x v d on a Git repository, n, m, M, C-u U, C-x v v from VC-Dir, x)
