# vc_annotate_mod

- Module: vc_annotate_mod
- Upstream: vc/vc-annotate.el
- State: partial

## Ported

`vc-annotate` (C-x v g; C-u reads the revision and the span in days; BUFFER, MOVE-POINT-TO and BACKEND),
the options `vc-annotate-display-mode` (a `vc_annotate_display_mode_type`: color map range, scale, fullscale or days),
`vc-annotate-background-mode`,
`vc-annotate-color-map` (an array of `vc_annotate_color_type`; the light, dark and 8-color TTY defaults),
`vc-annotate-very-old-color`,
`vc-annotate-background`,
`vc-annotate-menu-elements`,
`vc-annotate-use-short-revision` (the three frame-dependent defaults are set at the first use when the configuration left them unallocated, as loading vc-annotate.el does),
`vc-annotate-ratio`,
`vc-annotate-mode` and `vc-annotate-mode-map` (a, d, =, D, f, j, l, n, p, w, v, RET),
`vc-annotate-toggle-annotation-visibility`,
`vc-annotate-display-default`,
`vc-annotate-oldest-in-map`,
`vc-annotate-get-time-set-line-props`,
`vc-annotate-display-autoscale`,
`vc-annotate-display-select`,
`vc-annotate-prev-revision`,
`vc-annotate-next-revision`,
`vc-annotate-working-revision`,
`vc-annotate-extract-revision-at-line`,
`vc-annotate-revision-at-line`,
`vc-annotate-find-revision-at-line`,
`vc-annotate-revision-previous-to-line`,
`vc-annotate-show-log-revision-at-line`,
`vc-annotate-show-diff-revision-at-line` (and -internal),
`vc-annotate-show-changeset-diff-revision-at-line`,
`vc-annotate-warp-revision` (REVSPEC as COUNT or REVISION),
`vc-annotate-compcar` (`findloc` on the color map's ages, inline),
`vc-annotate-convert-time`,
`vc-annotate-difference`,
`vc-default-annotate-current-time`,
`vc-annotate-display`,
`vc-annotate-lines` (the font-lock matcher) with the faces made per color (`vc_annotate_face_RRGGBB`),
`vc-annotate-goto-line`,
`vc-annotate--file-name-change`;
log-view.el `log-view-annotate-version` (a) is in log_view_mod

## Deferred

the menu (`vc-annotate-mode-menu`, its span entries),
`truncate-lines` in the buffer (not ported),
directory-local variables,
clearing the buffer's own `revert-buffer-function` (files_mod has no clear_local for it),
`force-window-update`,
the :set of `vc-annotate-background-mode` that recomputes the color map,
a backend's own `annotate-current-time` (no ported backend has one),
the coding system of the output (UTF-8) and the DOS line ends of Windows;
"Already at revision REV of FILE" at a line (R6-34; upstream prints the (REV . FILE) pair)

## Tests

test_vc_annotate_mod (own; upstream has no vc-annotate tests: C-x v g output and line colors, p and n, d and l at a line, `vc-git-annotate-time`, a in Log View)
