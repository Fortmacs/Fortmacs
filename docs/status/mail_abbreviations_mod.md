# mail_abbreviations_mod

- Module: mail_abbreviations_mod
- Upstream: mail/mailabbrev.el
- State: partial

## Ported

`mail-abbrevs-mode` (global),
`mail-abbrevs-only`,
`mail-abbrevs` (an abbrev table; `mail_abbreviations_built` for upstream's nil),
`mail-abbrev-modtime`,
`mail-abbrevs-sync-aliases`,
`mail-abbrevs-setup` (`set_up_mail_abbreviations`; `mail_abbreviation_expand_wrapper` takes the place of upstream's wrapper around the buffer-local `abbrev-expand-function`),
`mail-abbrevs-enable`,
`mail-abbrevs-disable`,
`build-mail-abbrevs` (comments, continuation lines, alias, group and source lines),
`mail-alias-separator-string`,
`define-mail-abbrev` (a command; mailrc and comma syntax, quoting full names),
`mail-resolve-all-aliases` and `-1` (the loop error),
`mail-abbrev-expand-hook`,
`mail-abbrev-mode-regexp`,
`mail-abbrev-syntax-table`,
`mail-abbrev-in-expansion-header-p`,
`mail-abbrev-expand-wrapper`,
`merge-mail-abbrevs`,
`rebuild-mail-abbrevs`,
`mail-abbrev-insert-alias` (C-c C-a),
`mail-abbrev-complete-alias` (M-TAB),
`mail-abbrev-next-line`,
`mail-abbrev-end-of-buffer`

## Deferred

the non-ASCII characters of identifier syntax in `mail-abbrev-syntax-table` (map-char-table)

## Tests

test_mail_abbreviations_mod (own: reading a .mailrc file, quoting in `define_mail_abbreviation`, the loop error, expansion in the address fields only)
