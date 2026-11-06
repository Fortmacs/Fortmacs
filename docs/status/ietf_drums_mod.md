# ietf_drums_mod

- Module: ietf_drums_mod
- Upstream: mail/ietf-drums.el (in part: what parsing MIME parameters needs; mail/mail-parse.el's aliases mail-header-remove-comments and mail-header-remove-whitespace collapse to these names)
- State: partial

## Ported

`ietf-drums-tspecials` (`IETF_DRUMS_TSPECIALS`),
`ietf-drums--skip-comment` (`ietf_drums_comment_end`, on a string),
`ietf-drums-unfold-fws` (`unfold_ietf_drums_folding_whitespace`, on a string),
`ietf-drums-remove-comments` (`remove_ietf_drums_comments`),
`ietf-drums-remove-whitespace` (`remove_ietf_drums_whitespace`)

## Deferred

the other token variables (`ietf-drums-no-ws-ctl-token`, `ietf-drums-text-token`, `ietf-drums-specials-token`, `ietf-drums-quote-token`, `ietf-drums-wsp-token`, `ietf-drums-fws-regexp`, `ietf-drums-atext-token`, `ietf-drums-dot-atext-token`, `ietf-drums-qtext-token`),
`ietf-drums-syntax-table` and `ietf-drums-comment-syntax-table` (their rules are code here),
`ietf-drums-token-to-list`,
`ietf-drums-init`,
`ietf-drums-get-comment`,
`ietf-drums-strip`,
`ietf-drums-remove-garbage`,
`ietf-drums-strip-cte`,
`ietf-drums-parse-address`,
`ietf-drums-parse-addresses`,
`ietf-drums-parse-date` (ietf-drums-date.el),
`ietf-drums-narrow-to-header`,
`ietf-drums-quote-string`,
`ietf-drums-make-address`

## Tests

test_ietf_drums_mod (ports ietf-drums-tests.el in part: the cases of `ietf-drums-remove-comments` and `ietf-drums-remove-whitespace`; own: unfolding)
