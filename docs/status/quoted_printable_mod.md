# quoted_printable_mod

- Module: quoted_printable_mod
- Upstream: mail/qp.el
- State: partial

## Ported

`quoted-printable-decode-region` (a command; CODING-SYSTEM),
`quoted-printable-decode-string` (CODING-SYSTEM),
`quoted-printable-encode-region` (a command; FOLD, CLASS),
`quoted-printable-encode-string`

## Deferred

`mm-use-ultra-safe-encoding` (Gnus's mm-bodies.el; unbound without it, so `quoted-printable-encode-region` folds only with FOLD and never encodes "From " and "-" at the start of a line),
a multibyte STRING with characters other than ASCII for `quoted-printable-decode-string` (upstream inserts their low 8 bits into its unibyte buffer; the string here is bytes)

## Tests

test_quoted_printable_mod (ports qp-tests.el: all four)
