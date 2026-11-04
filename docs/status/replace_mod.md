# replace_mod

- Module: replace_mod
- Upstream: replace.el (query replace part)
- State: partial

## Ported

`perform-replace` (adjacent and empty matches, the region, BACKWARD, word-delimited matches through `word-search-regexp`, lax whitespace, `case-replace`, `query-replace-skip-read-only`, list REPLACEMENTS with REPEAT-COUNT, a Fortran replacement function in place of (FUNCTION . DATA), the answers act, skip, act-and-show, act-and-exit, exit, automatic, backup, undo, undo-all, edit (C-r), delete-and-edit (C-w), edit-replacement (e, E), recenter, help, scroll-up, quit, and any other key ending the loop and being reread, the stack, "Replaced N occurrences"),
`replace-search`,
`replace-match-maybe-edit` (`\?`),
`replace-quote`,
`query-replace-descr`,
`query-replace-read-from` (defaults and history pairs with `query-replace-from-to-separator`, \n and \t warnings),
`query-replace-read-to`,
`query-replace-read-args`,
`query-replace-compile-replacement` (`\#`),
`query-replace`,
`query-replace-regexp`,
`map-query-replace-regexp`,
`replace-string`,
`replace-regexp`;
M-%,
C-M-%,
the `match` face;
Occur: `occur` (M-s o, `list-matching-lines`; NLINES context lines, the collect operation with C-u, the region),
`multi-occur`,
`multi-occur-in-matching-buffers`,
`occur-read-primary-args`,
`occur-1`,
`occur-engine` (the prefixes, multi-line matches, context lines and separators, `list-matching-lines-jump-to-current-line`, the faces, `occur-excluded-properties`, the headers and totals),
`occur-mode` (`occur-mode-map`: RET, C-c C-c, o, C-o, n, p, M-n, M-p, r, e; `occur-next-error` for next-error, revert),
`occur-edit-mode` (C-c C-c, `occur-after-change-function`),
`occur-cease-edit`,
`occur-mode-goto-occurrence`,
`-other-window`,
`occur-mode-display-occurrence`,
`occur-next`,
`occur-prev`,
`occur-rename-buffer`,
`occur-hook`,
`occur-mode-hook`,
`occur-mode-find-occurrence-hook`;
the keep-lines family: `keep-lines`,
`flush-lines` (command),
`kill-matching-lines`,
`copy-matching-lines`,
`how-many` (with their aliases)

## Deferred

`\,` (Lisp expressions),
highlighting and lazy highlighting,
invisible text and `isearch-filter-predicate`,
`replace-char-fold`,
noncontiguous regions,
multi-buffer answers (Y, N),
the diff answer,
`replace-search-function`,
separators as text properties,
Occur's overlay arrow,
the highlighting of the visited occurrence,
mouse events and the context menu,
C-c C-f,
l,
c (`clone-buffer`),
the coding system of the buffers searched,
`isearch-occur`,
`read-regexp-case-fold-search`

## Tests

test_replace_mod (ports query-replace-tests except the `\,` case, perform-replace-tests, query-replace--undo, query-replace-undo-bug31073, -bug31492, -bug31538, -bug37073, -bug37287, replace-regexp-bug45973; own tests);
occur-test-00 to occur-test-13,
replace-occur-revert-bug32543, -bug32987,
test-count-matches;
own tests of the keep-lines family,
Occur mode and Occur-Edit mode
