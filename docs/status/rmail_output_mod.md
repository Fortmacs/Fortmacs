# rmail_output_mod

- Module: rmail_output_mod
- Upstream: mail/rmailout.el
- State: partial

## Ported

`rmail-output-read-file-name`,
`rmail-delete-unwanted-fields`,
`rmail-output-as-mbox` (to the file or to the buffer visiting it),
`rmail-output-to-rmail-buffer`,
`rmail-output` (mbox files),
`rmail-output-as-seen`,
`rmail-output-body-to-file`,
`rmail-output-decode-coding`,
`rmail-fields-not-to-output`,
`rmail-output-reset-deleted-flag`

## Deferred

Babyl files (`rmail-output-as-babyl`, `rmail-output-to-babyl-buffer`, `rmail-convert-to-babyl-format`, `rmail-nuke-pinhead-header`: a `void_function` error condition),
a message without a "From " line (mail-utils.el's `mail-mbox-from`, which needs time-date.el's `date-to-time`: a `void_function` error condition; so \[output_rmail_message_as_seen] of a shown message waits for it),
encrypted output files (NAME.gpg),
`rmail-output-file-alist`,
Auto Save mode in an Rmail buffer given a message

## Tests

test_rmail_output_mod (own; upstream has no rmailout-tests.el: output to an mbox file, the body to a file, output as seen waiting for mail-mbox-from)
