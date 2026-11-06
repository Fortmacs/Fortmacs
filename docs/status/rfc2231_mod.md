# rfc2231_mod

- Module: rfc2231_mod
- Upstream: mail/rfc2231.el (in part: parsing; mail/mail-parse.el's aliases mail-header-parse-content-type, mail-header-parse-content-disposition and mail-content-type-get collapse to these names)
- State: partial

## Ported

`rfc2231-get-value` (`get_rfc2231_value`),
`rfc2231-parse-qp-string` (`parse_rfc2231_quoted_printable_string`),
`rfc2231-parse-string` (`parse_rfc2231_string`: an `rfc2231_header_type`, the name and the parameters; continued and encoded values; SIGNAL-ERROR as REPORT_ERROR; the value is scanned as a string with the rules of upstream's syntax table),
`rfc2231-decode-encoded-string` (`decode_rfc2231_encoded_string`)

## Deferred

`rfc2231-encode-string` (with mm-bodies.el and rfc2047-encode-parameter),
the fallback of `rfc2231-parse-string` for an error in removing comments and whitespace (neither can fail: both end at an unterminated string or comment)

## Tests

test_rfc2231_mod (own: upstream has no rfc2231 tests)
