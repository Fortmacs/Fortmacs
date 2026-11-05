# simple_merge_mode_mod

- Module: simple_merge_mode_mod
- Upstream: vc/smerge-mode.el
- State: partial

## Ported

`smerge-mode` (" SMerge", `smerge-mode-map` with `smerge-command-prefix` (C-c ^; bound when the mode is first turned on, as upstream binds it when smerge-mode.el is loaded) to `smerge-basic-map` and its = map (prompt "Diff"), `smerge-mode-hook`, the font-lock keyword and `paragraph-separate` (`smerge-parsep-re`) while on, the empty C-c ^ map bound globally at startup),
the faces (`smerge-upper`, `-lower`, `-base`, `-markers`, `-refined-changed`, `-removed`, `-added`) and the face variables,
`smerge-begin-re`,
`smerge-end-re`,
`smerge-base-re`,
`smerge-lower-re`,
`smerge-match-names`,
`smerge-conflict-style`,
`smerge-match-conflict`,
`smerge-find-conflict` (conflict overlays kept in a list of handles, with subr.el's `remove-overlays` splitting for them),
`smerge-conflict-overlay`,
`smerge-remove-props`,
`smerge-check` (its cache),
`smerge-ensure-match`,
`smerge-auto-leave`,
`smerge-next`/`-prev` (n, p),
`smerge-keep-n`,
`smerge-keep-all` (a),
`-base` (b),
`-lower` (l, o),
`-upper` (u, m),
`-current` (RET),
`smerge-get-current`,
`smerge-kill-current`,
`smerge-combine-with-next` (C),
`smerge-auto-combine` (`smerge-auto-combine-max-separation`),
`smerge-swap`,
`smerge-extend`,
`smerge-resolve` (r: `smerge-resolve-function` with a buffer-local value, diff -b refinement through `smerge-apply-resolution-patch`, whitespace-only changes, diff | patch both ways, comment-only conflicts with `smerge-resolve--extract-comment` and `-normalize`),
`smerge-resolve-all`,
`smerge-diff` with `smerge-diff-base-upper` (= <),
`-base-lower` (= >),
`-upper-lower` (= =),
`smerge-diff-buffer-name`,
`smerge-diff-switches`,
`smerge-auto-leave`,
`smerge-start-session`,
`smerge-repeat-map`

## Deferred

refinement (`smerge-refine` R, `smerge-refine-regions` and the `smerge--refine-*` helpers, `smerge-refine-exchange-point`, the shadow cursor and its face and option, `smerge-refine-ignore-whitespace`, `-weight-hack`, `-forward-function`, and the refinement `smerge-next` does after moving when `diff-refine` asks),
`smerge-ediff` (E) and the Ediff hook,
`smerge-makeup-conflict` (it ends by refining),
`smerge-vc-next-conflict` and `smerge-change-buffer-confirm` (with vc.el's `vc-find-conflicted-file`),
`smerge-batch-resolve`,
the menus,
`smerge-popup-context-menu` and the overlays' `help-echo` and mouse keymap (`smerge-text-properties`),
the undo entry `smerge-auto-leave` records to turn the mode on again,
removing the font-lock keyword when the mode is turned off (font-lock-remove-keywords),
the menu item names of the = map,
the obsolete aliases

## Tests

test_simple_merge_mode_mod (ports smerge-mode-test-empty-hunk; own: matching, the keep, combine, swap and delete commands, n and p, auto-leave, C-c ^ u and the repeat map, sessions, `paragraph-separate`, = =, resolving with diff and patch, a Git conflict visited and resolved)
