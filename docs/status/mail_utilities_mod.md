# mail_utilities_mod

- Module: mail_utilities_mod
- Upstream: mail/mail-utils.el
- State: partial

## Ported

`mail-dont-reply-to-names`,
`mail-file-babyl-p` (`is_mail_file_babyl`),
`mail-quote-printable` (`quote_mail_printable`),
`mail-quote-printable-region` (a command),
`mail-unquote-printable-hexdigit`,
`mail-unquote-printable`,
`mail-unquote-printable-region` (a command; NOERROR, UNIBYTE),
`mail-strip-quoted-names` (`strip_mail_quoted_names`),
`mail-dont-reply-to`,
`mail-fetch-field` (`get_mail_field`: LAST, ALL, DELETE; no value is unallocated),
`mail-parse-comma-list` (`parse_mail_comma_list`, a blank-padded array),
`mail-comma-list-regexp`,
`mail-rfc822-date`,
`mail-mbox-from`

## Deferred

`mail-use-rfc822` (with rfc822.el's `rfc822-addresses`),
`mail-re-regexps` and `mail--wrap-re-regexp` (for Rmail and Message),
the LIST argument of `mail-fetch-field`,
the obsolete `mail-string-delete` and `mail-rfc822-time-zone`

## Tests

test_mail_utilities_mod (ports mail-utils-tests.el but mail-utils-tests-mail-rfc822-time-zone; own tests of the field options and of comments in addresses)
