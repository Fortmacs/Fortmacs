# tabulated_list_mod

- Module: tabulated_list_mod
- Upstream: emacs-lisp/tabulated-list.el
- State: partial

## Ported

`tabulated-list-mode` (derived from Special mode; `tabulated-list-mode-map` but the button keys),
the buffer-local `tabulated-list-format` (an array of `tabulated_list_column_type`: name, width, sort (a test or by the column's strings), `:right-align`, `:pad-right`),
`tabulated-list-entries` (an array of `tabulated_list_entry_type`: a string ID and the column descriptors, or a function),
`tabulated-list-padding`,
`tabulated-list-sort-key`,
`tabulated-list-use-header-line`,
`tabulated-list-printer`,
`tabulated-list-revert-hook`;
the buffer-local variables as setting objects (`tabulated_list_format`, `tabulated_list_entries`, `tabulated_list_sort_key`, `tabulated_list_printer`: `%set_local`, `%get_current`, `%is_local`, `%clear_local`; `tabulated_list_entries%set_local` takes an array or a procedure, R6-37) and forwarded settings (`tabulated_list_padding`, `tabulated_list_use_header_line`); format, entries, padding and sort key permanent-local;
`tabulated-list-init-header` (the header line's segments, the sort indicator of the frame),
`tabulated-list-print` (sorting, REMEMBER-POS, UPDATE),
`tabulated-list-print-entries`,
`tabulated-list-print-entry`,
`tabulated-list-print-col` (truncation with an ellipsis property, right alignment, `(space :align-to N)` padding, the available space before a right-aligned column),
`tabulated-list-get-id`,
`tabulated-list-get-entry`,
`tabulated-list-put-tag`,
`tabulated-list-clear-all-tags`,
`tabulated-list-delete-entry`,
`tabulated-list-set-col`,
`tabulated-list-revert`,
`tabulated-list-sort` (S; -1 restores the original order),
`tabulated-list-widen-current-column` and `-narrow-` (`}` and `{`),
`tabulated-list-next-column` and `-previous-column` (M-<right>, M-<left>),
the sort indicators,
`tabulated-list-fake-header`

## Deferred

the fake header (`tabulated-list-use-header-line` nil),
images and buttons in columns,
the sort buttons of the header line,
groups (`tabulated-list-groups` and their functions),
`truncate-lines`,
help-echo and mouse-face,
`header-line-indent-mode`,
bidi

## Tests

test_tabulated_list_mod (ports tabulated-list-print and tabulated-list-sort; own tests of the header, entries made by a procedure and the permanent-local variables)
