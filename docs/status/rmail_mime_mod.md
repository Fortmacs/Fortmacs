# rmail_mime_mod

- Module: rmail_mime_mod
- Upstream: mail/rmailmm.el
- State: partial

## Ported

`rmail-mime-media-type-handlers-alist` (`rmail_mime_media_type_handlers`, records of a regular expression and a handler; upstream calls only the first function of an element, so each record has one),
`rmail-mime-attachment-dirs-alist` (`rmail_mime_attachment_directories`, records of a regular expression and one directory, in order of preference),
`rmail-mime-render-html-function` (`render_rmail_mime_html_lynx` when lynx is installed, else null),
`rmail-mime-prefer-html`,
`rmail-mime-mbox-buffer`,
`rmail-mime-view-buffer`,
`rmail-mime-coding-system`,
`rmail-mime-searching`,
`rmail-mime-entity` (a record of a registry, its index the text property `rmail_mime_entity`; the entities of an Rmail buffer are destroyed when it shows the next message),
`rmail-mime-display` (`RMAIL_MIME_DISPLAY_HIDDEN`, `_DECODED`, `_RAW`),
`rmail-mime-save` (the button action: the file name read, the body decoded, written with no conversion),
the button types `rmail-mime-save` and `rmail-mime-toggle` (`rmail_mime_save_button`, `rmail_mime_toggle_button`),
`rmail-mime-entity-segment`,
`rmail-mime-shown-mode`,
`rmail-mime-hidden-mode`,
`rmail-mime-raw-mode`,
`rmail-mime-toggle-raw`,
`rmail-mime-toggle-hidden` (RET),
TAB and <backtab> in Rmail mode (`forward_button`, `backward_button`),
`rmail-mime-insert-tagline`,
`rmail-mime-update-tagline`,
`rmail-mime-insert-header`,
`rmail-mime-find-header-encoding`,
`rmail-mime-text-handler`,
`rmail-mime-insert-decoded-text`,
`rmail-mime-insert-text`,
`rmail-mime-insert-html`,
`rmail-mime-render-html-lynx`,
`rmail-mime-fix-inserted-faces`,
`rmail-mime-toggle-button`,
`rmail-mime-bulk-handler`,
`rmail-mime-set-bulk-data` (no image type is supported),
`rmail-mime-insert-bulk`,
`rmail-mime-multipart-handler`,
`rmail-mime-process-multipart` (multipart/alternative shows the best text part),
`rmail-mime-insert-multipart`,
`rmail-mime-handle`,
`rmail-mime-show` (`show_rmail_mime_buffer`),
`rmail-mime-process`,
`rmail-mime-parse`,
`rmail-mime-insert`,
`rmail-mime-mode` (RMIME),
`rmail-mime` (v, `toggle_rmail_mime`; its ignored ARG is left out),
`rmail-mm-get-boundary-error-message` (`set_rmail_mime_boundary_error`),
`rmail-show-mime` (the value of `rmail_show_mime_function`),
`rmail-search-mime-message` (the value of `rmail_search_mime_message_function`)

## Deferred

images (`rmail-mime-show-images`, `rmail-mime-insert-image`, `image-supported-file-p`: Fortmacs shows no images, so, as in an Emacs built without images, an image attachment is a tag line whose body is shown as text),
`rmail-mime-render-html-shr` (shr.el and libxml),
`rmail-mime-save-action` (only its default, nothing done after saving: visiting the file, Dired and opening the file with another program wait for it),
font lock in RMIME mode (`rmail-font-lock-keywords`, with Rmail's),
`rmail-insert-mime-forwarded-message` and `rmail-insert-mime-resent-message` (with `rmail-forward` and `rmail-resend`, Deferred in rmail_mod),
`rmail-mime-entity-set-truncated` (obsolete; a plain assignment);
Not applicable: rmail.el's `rmail-mime-feature` and `rmail-require-mime-maybe` (rmail_mime_mod is built in, so there is no feature to load)

## Tests

test_rmail_mime_mod (ports rmailmm-tests.el: rmailmm-test-handler; rmailmm-test-bulk-handler and rmailmm-test-multipart-handler, which upstream marks unstable with no condition for success, check what the code does; own: an attachment's tag line, a multipart/alternative message shown in Rmail and its hidden part shown, toggling the raw message, searching the decoded parts)
