# page_mod

- Module: page_mod
- Upstream: textmodes/page.el
- State: done

## Ported

`forward-page` (C-x ]),
`backward-page` (C-x [), `mark-page` (C-x C-p), `narrow-to-page` (C-x n p, disabled), `count-lines-page` (C-x l), `what-page`, `page--count-lines-page`, `page--what-page`

## Deferred


## Tests

test_page_mod (ports page-tests.el: all 6),
test_log_edit_mod (through the ChangeLog entry search)
