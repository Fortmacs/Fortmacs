# rectangle_mod

- Module: rectangle_mod
- Upstream: rect.el
- State: partial

## Ported

`apply-on-rectangle` (a `rectangle_line_action_type` extension per FUNCTION and its ARGS; point kept),
`rectangle-position-as-coordinates`,
`rectangle-intersect-p`,
`rectangle-dimensions`,
`delete-rectangle-line`,
`delete-extract-rectangle-line`,
`extract-rectangle-line` (TABs as spaces, padding),
`spaces-string`,
`delete-rectangle` (C-x r d),
`delete-extract-rectangle`,
`extract-rectangle`,
`extract-rectangle-bounds`,
`killed-rectangle`,
`kill-rectangle` (C-x r k; read-only buffers and `kill-read-only-ok`),
`copy-rectangle-as-kill` (C-x r M-w),
`yank-rectangle` (C-x r y),
`insert-rectangle`,
`open-rectangle` (C-x r o),
`open-rectangle-line`,
`delete-whitespace-rectangle-line`,
`delete-whitespace-rectangle`,
`string-rectangle-line`,
`string-rectangle` (C-x r t; prompt with the `string-rectangle-history` default, point recorded for undo),
`string-insert-rectangle`,
`clear-rectangle` (C-x r c),
`clear-rectangle-line`,
`rectangle-number-line-callback`,
`rectangle--default-line-number-format`,
`rectangle-number-lines` (C-x r N; with C-u, `read-number` and the format string),
the prefix argument as FILL;
editing_functions_mod's `styled_format` with the counter for the line numbers,
`rectangle-mark-mode` (C-x SPC) with `rectangle-mark-mode-map` (C-o, C-t, the remaps),
the crutches (`rectangle--pos-cols`, `rectangle--col-pos`, `rectangle--point-col`, also used by `apply-on-rectangle`),
`rectangle-exchange-point-and-mark`,
`rectangle-right-char`,
`rectangle-left-char`,
`rectangle-forward-char`,
`rectangle-backward-char`,
`rectangle-next-line`,
`rectangle-previous-line`,
the advice on `region-beginning` and `region-end`,
`rectangle--extract-region`,
`rectangle--insert-region`,
`rectangle--insert-for-yank` (undone by `yank-pop`),
`rectangle--highlight-for-redisplay` and `rectangle--unhighlight-for-redisplay` (an overlay per line for the window, before- and after-strings of `(space :align-to COL)`, the cursor through `cursor`)

## Deferred

the `string-rectangle` preview (`rectangle-preview`, a minibuffer post-command hook),
`rectangle-indicate-zero-width-rectangle` (a face height of 0.2),
`rectangle--duplicate-right` (no `duplicate-dwim`),
the obsolete `operate-on-rectangle`,
`close-rectangle`,
`replace-rectangle`,
input methods,
%e %f %g in a number format

## Tests

test_rectangle_mod (own; upstream has no rect-tests.el: extraction with TABs, deletion splitting a TAB and across wide characters, FILL, kill and paste, read-only buffers, open, clear, string, number lines, the C-x r keys; Rectangle-Mark mode: C-x SPC, the remapped motion keys and a crutch past the EOL, C-w and C-y of a rectangle, `region-beginning`, `region-end`, `region-bounds` and C-x C-x cycling the corners)
