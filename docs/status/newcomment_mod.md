# newcomment_mod

- Module: newcomment_mod
- Upstream: newcomment.el (+ simple.el `comment-line-break-function` in `default-indent-new-line`)
- State: partial

## Ported

`comment-dwim` (M-;),
`comment-region`,
`uncomment-region`,
`comment-or-uncomment-region`,
`comment-box`,
`comment-indent` (`indent-for-comment`),
`comment-set-column` (C-x ;),
`comment-kill`,
`comment-line` (C-x C-;),
`comment-indent-new-line` (through `comment-line-break-function` from M-j and auto-fill's line breaks);
`comment-normalize-vars` (the `undecided` syntax test in a temporary buffer, derived `comment-continue`, `comment-start-skip` and `comment-end-skip`),
`comment-search-forward`/`-backward`,
`comment-beginning`,
`comment-forward`,
`comment-enter-backward`,
`comment-padright`/`-padleft`,
`comment-quote-nested` with `comment-quote-nested-function`,
`comment-region-internal` and `comment-make-extra-lines` for every `comment-style`,
`comment-choose-indent` (aligning with neighboring comments, `comment-inline-offset`, `comment-fill-column`),
`comment-indent-default` with a buffer-local `comment-indent-function` (column, range or none),
`comment-insert-comment-function`,
`comment-add`,
`comment-padding`,
`comment-empty-lines`,
`comment-multi-line`,
`comment-auto-fill-only-comments`,
`comment-valid-prefix-p`,
`comment-only-p`,
`comment-indent-function` as a buffer-local procedure setting (`%procedure`, `%set_local`, `%get_current`, `%is_local`, `%clear_local`)

## Deferred

`combine-change-calls`,
inheriting text properties,
buffer-local values of `comment-region-function`,
`uncomment-region-function` and `comment-quote-nested-function`,
hard newlines,
`string-width` of non-ASCII fill prefixes,
auto-fill's comment handling in `do-auto-fill` (`comment-auto-fill-only-comments`, the comment fill prefix),
the obsolete aliases as separate commands

## Tests

test_newcomment_mod (ports newcomment-tests: test-uncomment-space-comment-continue, with C mode's comment settings by hand; own: `#` comments)
