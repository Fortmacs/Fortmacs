# register_mod

- Module: register_mod
- Upstream: register.el
- State: partial

## Ported

`register-alist` as an array of typed `register_type` (markers and window configurations owned, freed when set again),
`get-register`,
`set-register` (text, numbers, rectangles),
`register-separator` (-1 for nil),
`register-describe-oneline`,
`register-preview-default`,
`register-of-type-alist`,
`register-preview` (*Register Preview* at the bottom, fitted),
`register-read-with-preview` and `register-read-with-preview-traditional` (`register-use-preview` `traditional`: one key; C-h previews; C-g, ESC and `escape` quit),
`point-to-register` (C-x r SPC, C-SPC, C-@),
`window-configuration-to-register` (C-x r w),
`jump-to-register` (C-x r j; markers, window configurations, files, buffers, file-query references),
`file-to-register` (C-x r F),
`buffer-to-register` (C-x r B),
`register--jumpable-p`,
`register--insertable-p`,
the `register-val-jump-to`,
`register-val-describe` and `register-val-insert` methods,
`number-to-register` (C-x r n),
`increment-register` (C-x r +),
`view-register`,
`list-registers` (*Output*),
`describe-register-1`,
`insert-register` (C-x r i, g),
`copy-to-register` (C-x r s, x),
`append-to-register`,
`prepend-to-register`,
`copy-rectangle-to-register` (C-x r r)

## Deferred

frame configurations and framesets (C-u C-x r SPC is an error; C-x r f),
the other `register-use-preview` values (`register-read-with-preview-fancy`, `register-preview-next`/`-previous`, `register-preview-default-keys`),
`register-preview-delay` (no timers),
`register-preview-function`,
`register-preview-display-buffer-alist`,
`help-event-list`,
`register-swap-out` and `register-buffer-to-file-query` (need `kill-buffer-hook`),
`registerv` values,
keyboard macro,
bookmark and other packages' register values,
the Dired directory of `file-to-register`,
`yank-excluded-properties`

## Tests

test_register_mod (ports register-test-bug27634, register--jumpable-p (the ported kinds); own tests for each command, descriptions, the preview and the C-x r keys)
