# rfc2047_mod

- Module: rfc2047_mod
- Upstream: mail/rfc2047.el (+ mail/mail-prsvr.el; mail/mail-parse.el's aliases of rfc2047.el functions collapse to these names)
- State: partial

## Ported

`rfc2047-header-encoding-alist` (`rfc2047_header_encodings`, records of `rfc2047_header_encoding_type`: a header regular expression or `all_headers`, and an `RFC2047_HEADER_ENCODING_` method),
`rfc2047-charset-encoding-alist` (`rfc2047_charset_encodings`),
`rfc2047-encode-encoded-words`,
`rfc2047-allow-irregular-q-encoded-words`,
`rfc2047-encoded-word-regexp` and `rfc2047-encoded-word-regexp-loose`,
`rfc2047-qp-or-base64`,
`rfc2047-narrow-to-field` (`narrow_to_rfc2047_field`; mail-header-narrow-to-field),
`rfc2047-field-value` (mail-header-field-value),
`rfc2047-encoding-type` (`RFC2047_ENCODING_TYPE_MIME`, `RFC2047_ENCODING_TYPE_ADDRESS_MIME`),
`rfc2047-encode-message-header` (a command; mail-encode-encoded-word-buffer),
`rfc2047-encodable-p` (`is_rfc2047_encodable`),
`rfc2047-syntax-table`,
`rfc2047-encode-region` (mail-encode-encoded-word-region; addresses, quoted strings, comments, the `mime` type),
`rfc2047-encode-string` (mail-encode-encoded-word-string),
`rfc2047-encode-max-chars` (0 for none),
`rfc2047-encode-1`,
`rfc2047-encode`,
`rfc2047-fold-field` (mail-header-fold-field),
`rfc2047-fold-region`,
`rfc2047--break-line`,
`rfc2047-unfold-field` (mail-header-unfold-field),
`rfc2047-unfold-region`,
`rfc2047-b-encode-string` and `rfc2047-q-encode-string` (rfc2047-encode-function-alist is the choice of the two in the code),
`rfc2047-quote-decoded-words-containing-tspecials`,
`rfc2047-allow-incomplete-encoded-text`,
`rfc2047-strip-backslashes-in-quoted-strings`,
`rfc2047-charset-to-coding-system`,
`rfc2047-decode-encoded-words`,
`rfc2047-decode-region` (a command; mail-decode-encoded-word-region),
`rfc2047-decode-address-region` (mail-decode-encoded-address-region),
`rfc2047-decode-string` (mail-decode-encoded-word-string),
`rfc2047-decode-address-string` (mail-decode-encoded-address-string),
`rfc2047-pad-base64`;
mail-prsvr.el: `mail-parse-charset` ("" for none),
`mail-parse-ignored-charsets`

## Deferred

`message-posting-charset` (message.el binds it only through Gnus's gnus-setup-posting-charset, so it is nil without Gnus: a field that needs no encoding is left alone, and the 8-bit branch of `rfc2047-encode-message-header` and of `rfc2047-encodable-p` waits for Gnus with mm-bodies.el's `mm-body-7-or-8`),
`rfc2047-encode-parameter` (mail-header-encode-parameter; needs rfc2045.el's `rfc2045-encode-string`),
the obsolete `rfc2047-quote-special-characters-in-quoted-strings`,
a unibyte STRING for `rfc2047-decode-string` (upstream decodes one by us-ascii; strings here are multibyte text),
the other mail-parse.el functions (the aliases of rfc2231.el, ietf-drums.el and rfc2045.el functions, `mail-header-parse-addresses-lax`, `mail-header-parse-address-lax`) with those files;
Not applicable: the `debug-on-error` test in `rfc2047-encode-region`'s error handler (no debugger)

## Tests

test_rfc2047_mod (ports rfc2047-tests.el: the four folding tests; own tests of unfolding, the field value, decoding, address decoding, encoding strings and a message header, and padding)
