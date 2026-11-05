# incremental_search_mod

- Module: incremental_search_mod
- Upstream: isearch.el
- State: partial

## Ported

`isearch-forward`,
`isearch-backward`,
`isearch-mode`,
`isearch-done`,
`isearch-update`,
`isearch-message` (prefix and suffix),
`isearch-search`,
`isearch-search-string`,
`isearch-search-and-update`,
`isearch-repeat` (+ `-forward`, `-backward`, wrapping),
`isearch-exit` (nonincremental search on an empty string),
`isearch-cancel`,
`isearch-abort`,
`isearch-delete-char`,
`isearch-del-char`,
`isearch-printing-char`,
`isearch-quote-char`,
`isearch-yank-char`,
`isearch-yank-word-or-char` (C-w),
`isearch-yank-line`,
`isearch-yank-string`,
`isearch-yank-kill`,
`isearch-yank-pop-only`,
the mark where the search started,
`isearch-beginning-of-buffer`,
`isearch-end-of-buffer`,
`isearch-toggle-case-fold`,
`isearch-edit-string` (with-isearch-suspended),
the search ring (`isearch-update-ring`, `isearch-ring-advance`/`-retreat`),
`isearch-pre-command-hook` (prefix arguments allowed),
`isearch-mode-hook`,
`isearch-mode-end-hook` (`isearch-mode-end-hook-quit`),
`isearch-search-fun-function` (`isearch-search-fun`, `isearch-search-fun-default`),
`isearch-wrap-function`,
`isearch-push-state-function` (each state's pop function),
`isearch-filter-predicate` (null by default: `isearch-filter-visible` waits for invisible text),
`isearch-message-function` (`isearch-message`),
`isearch-message-prefix-add`,
search-failed errors (their message shown as the error),
`isearch-no-upper-case-p` (with REGEXP-FLAG),
`search-upper-case`,
`search-exit-option` (t or nil),
`isearch-mode-map`;
regexp search (`isearch-forward-regexp`, `isearch-backward-regexp`, C-u C-s, `isearch-toggle-regexp`, `isearch-fallback` for `*` `?` `\}` `\|`, invalid-regexp messages, the regexp search ring),
lax whitespace (`search-whitespace-regexp`, `isearch-lax-whitespace`, `isearch-regexp-lax-whitespace`, `isearch-toggle-lax-whitespace`);
`word-search-regexp`;
C-s,
C-r,
C-M-s,
C-M-r,
M-r,
M-s r,
M-s SPC,
the `lazy-highlight` face (for Occur's current line),
the `incremental_search` face (for Ispell's highlighting),
`isearch-search-fun-function`, `isearch-wrap-function`, `isearch-push-state-function`, `isearch-filter-predicate` and `isearch-message-function` as buffer-local procedure settings (`%procedure`, `%set_local`, `%get_current`, `%clear_local`)

## Deferred

word,
symbol and char-fold isearch (`isearch-regexp-function`, `search-default-mode`),
the `word-search-*` commands,
highlighting and lazy highlighting,
C-M-w (forward-symbol),
reading a kill in the minibuffer (C-u M-y),
invisible text,
scrolling and motion during search,
`isearch-query-replace` and occur,
help map,
input methods,
completion,
minibuffer-local-isearch-map

## Tests

test_incremental_search_mod (ports isearch--test-done; own tests through the command loop)
