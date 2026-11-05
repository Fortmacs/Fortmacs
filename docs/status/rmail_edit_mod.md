# rmail_edit_mod

- Module: rmail_edit_mod
- Upstream: mail/rmailedit.el
- State: partial

## Ported

`rmail-edit-mode`,
`rmail-edit-current-message`,
`rmail-cease-edit` (escaping "From " lines, rewriting the charset, adding a lost Content-Type field, encoding the body by its coding system and content transfer encoding, the edited attribute),
`rmail-abort-edit`,
`rmail-edit-headers-alist` (`rmail_edit_headers`),
`rmail-edit-diff-headers`,
`rmail-edit-update-headers`,
`rmail-edit-mode-hook`,
`rmail-edit-map`

## Deferred

the MIME state of an edited message (`rmail-old-mime-state`, rmailmm.el),
`rmail-old-pruned` and `rmail-old-headers` as variables (kept per buffer by the module),
the mode line's modified flag reset to its default (`mode-line-modified`)

## Tests

test_rmail_edit_mod (own; upstream has no rmailedit-tests.el: finishing an edit, aborting one, applying changed header fields)
