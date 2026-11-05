# fortmacs_tags_mod

- Module: fortmacs_tags_mod
- Upstream: progmodes/etags.el
- State: partial

## Ported

the options `tags-file-name`,
`tags-case-fold-search` (a choice setting: `.true.`, `.false.`, `TAGS_CASE_FOLD_SEARCH_DEFAULT`),
`tags-table-list`,
`tags-add-tables` (a choice setting: `.true.`, `.false.`, `TAGS_ADD_TABLES_ASK`),
`tags-revert-without-query`,
`find-tag-hook` (the global hook and a buffer's own),
`find-tag-default-function`,
`tags-tag-face`,
`tags-apropos-verbose`,
`default-tags-table-function`,
`etags-xref-prefer-current-file`,
`etags-xref-find-definitions-tag-order` (`fortmacs_tags_find_cross_reference_definitions_tag_order`, records of `tag_match_test_type`),
`etags--xref-limit`;
the state: `last-tag`,
`find-tag-history`,
`tags-location-ring`,
`tags-table-list-pointer`,
`tags-table-list-started-at`,
`tags-table-set-list`,
`tags-table-computed-list` and `-for`;
`tags-table-mode` (`tags-table-mode-hook`; `tags-file-name` buffer-local in the table),
`visit-tags-table` (the prompt with `tags-file-name` or `default-tags-table-function`, the `tags-add-tables` question, `tags-expand-table-name`),
`visit-tags-table-buffer` (CONT: the first, same, next or reset table as `VISIT_*` constants; `tags-table-check-computed-list`, `tags-table-including`, `tags-table-list-member`, `tags-next-table`, `initialize-new-tags-table`, `tags-verify-table` and reverting a table that changed on disk with `tags-revert-without-query`; "File FILE is not a valid tags table"),
`tags-reset-tags-tables` (`reset_tags_tables`),
`tags-table-files`,
`tags-included-tables`,
`tags-completion-table` (the "Making tags completion table" message, a buffer with no file named by its buffer name, R6-42),
`tags-lazy-completion-table`,
`tags-completion-at-point-function` (on `completion_at_point_functions` in Xref-Etags mode's buffers),
the etags format (the only one: `etags-recognize-tags-table`'s functions are the module's own: `etags-verify-tags-table`, `etags-file-of-tag`, `etags-tags-completion-table`, `etags-snarf-tag` (`snarf_fortmacs_tags_tag`, `tag_info_type`), `etags-goto-tag-location` (`goto_fortmacs_tags_tag_location`: the search widening around the recorded position), `etags-tags-table-files`, `etags-tags-included-tables`, `etags-list-tags`, `etags-tags-apropos`),
the match tests `tag-exact-file-name-match-p`,
`tag-file-name-match-p`,
`tag-exact-match-p`,
`tag-implicit-name-match-p`,
`tag-symbol-match-p` (`is_tag_identifier_match`),
`tag-word-match-p`,
`tag-partial-file-name-match-p`,
`tag-any-match-p`,
`tag-re-match-p` (`is_tag_regular_expression_match`) (as `is_tag_*`),
`find-tag-tag` (`read_tag`),
`find-tag--default`,
`find-tag-interactive` (NEXT-P as `FIND_TAG_NEXT_NO`/`_YES`/`_POP_BACK`),
`find-tag-noselect` (`find-tag-in-order` with `find-tag-tag-order`, `find-tag-regexp-tag-order`, `find-tag-search-function`, `find-tag-next-line-after-failure-p` as the etags format's; the location pushed on the xref history),
`find-tag`,
`find-tag-other-window`,
`find-tag-regexp`,
`pop-tag-mark` (`go_back_in_cross_reference_history`),
`file-of-tag`,
`tag-find-file-of-tag-noselect` (`find_file_of_tag_noselect`),
`list-tags` (the tags as text),
`tags-apropos`,
`complete-tag`;
the xref backend: `etags--xref-backend` (`fortmacs_tags_cross_reference_backend`, the default of `xref-backend-functions`),
`fortmacs_tags_backend_type`'s `xref-backend-identifier-at-point`,
`-identifier-completion-table`,
`-identifier-completion-ignore-case`,
`-definitions` (`etags--xref-find-definitions`, `etags-xref-prefer-current-file`),
`-apropos`,
`xref-etags-location` (`cross_reference_fortmacs_tags_location_type`: marker, group, line)

## Deferred

`tags-search` (M-x tags-search),
`tags-query-replace`,
`tags-loop-continue` (M-,),
`tags-next-file`,
`next-file`,
`tags-loop-scan`,
`tags-loop-operate`,
`tags-loop-eval`,
`tags--last-search-operate-function`,
`tags--all-files` (the loop over the tables' files: the next-file machinery and `fileloop.el`),
`select-tags-table` (its mode, map, `select-tags-table-select`, `select-tags-table-quit`),
`find-tag-other-frame` (C-x 5 .),
the hooks for other tags formats (`tags-table-format-functions`, `file-of-tag-function`, `tags-table-files-function`, `tags-completion-table-function`, `snarf-tag-function`, `goto-tag-location-function`, `find-tag-regexp-search-function`, `find-tag-regexp-next-line-after-failure-p`, `find-tag-search-function`, `find-tag-next-line-after-failure-p`, `list-tags-function`, `tags-apropos-function`, `tags-included-tables-function`, `verify-tags-table-function`, `etags-case-fold-search`, `local-find-tag-hook`, `etags-syntax-table`),
the buttons of `list-tags` and `tags-apropos` and `tags-apropos-additional-actions` (`etags-tags-apropos-additional`, `etags--xref-apropos-additional`),
`tags-with-face`,
the progress reporter of `etags-tags-apropos`,
`tags-compression-info-list` (compressed tags tables: jka-compr),
`selective-display` in `etags-goto-tag-location`,
a buffer-local `tags-table-list`,
`tags-location-ring-length` (the xref history's length),
the obsolete `find-tag-marker-ring`,
the compatibility files of the old `tags-table-files` (`tags--compat-files`, `tags--compat-initialize`),
the `find-tag-default-function` symbol property (the setting only)

## Tests

test_fortmacs_tags_mod (ports etags-tests.el: etags-bug-158, etags-bug-23164, etags-buffer-local-tags-table-list (on tables of the same shape in test/etags-resources, test/manual/etags's not being in the tree; the buffer-local `tags-table-list` part waits for it); own: find-tag, list-tags, tags-apropos, the xref apropos)
