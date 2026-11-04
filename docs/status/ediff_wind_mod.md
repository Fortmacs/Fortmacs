# ediff_wind_mod

- Module: ediff_wind_mod
- Upstream: vc/ediff-wind.el
- State: partial

## Ported

`ediff-window-setup-function` and `ediff-split-window-function` (procedure pointers; `split_window_below` and `split_window_right`),
`ediff-setup-windows`,
`ediff-setup-windows-default`,
`ediff-setup-windows-plain`,
`ediff-setup-windows-plain-compare` (the control buffer in a window across the bottom of the frame),
`ediff-refresh-mode-lines` (the control buffer's identification),
`ediff-make-wide-control-buffer-id`,
`ediff-get-visible-buffer-window`,
`ediff-keep-window-config`

## Deferred

the multiframe setup and control frames (`ediff-setup-windows-multiframe` and its merge and compare variants, `ediff-setup-control-frame`, `ediff-destroy-control-frame`, `ediff-make-frame-position`, `ediff-control-frame-parameters` and the frame options, `ediff-floating-control-frame`, `ediff-prefer-iconified-control-frame`, `ediff-refresh-control-frame`, `ediff-make-narrow-control-buffer-id`, `ediff-make-base-title`),
the merge layout (`ediff-setup-windows-plain-merge`, `ediff-merge-split-window-function`, `ediff-select-lowest-window`),
`ediff-make-wide-display`,
`window-combination-resize` (in 3-way jobs, B and C share what the first split gave B),
the mode lines of buffers A,
B and C,
windows jobs,
`ediff-skip-unsuitable-frames`,
`ediff-get-window-by-clicking`

## Tests

test_ediff_util_mod (own: the window layout)
