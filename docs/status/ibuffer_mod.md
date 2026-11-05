# ibuffer_mod

- Module: ibuffer_mod, ibuffer_extensions_mod (submodule)
- Upstream: ibuffer.el, ibuf-ext.el
- State: partial

## Ported

`ibuffer` (OTHER-WINDOW-P, NAME, QUALIFIERS, NOSELECT, SHRINK, FILTER-GROUPS, FORMATS),
`ibuffer-other-window` and `ibuffer-list-buffers` (FILES-ONLY),
`ibuffer-jump`,
Ibuffer mode (`ibuffer-mode-map` with the `/` filter map, the mode line's sorting state and filters, the header line with the filters, `revert-buffer-function`);
`ibuffer-formats` drawn by interpreting the formats (MIN-SIZE, MAX-SIZE, functions for them, ALIGN, ELIDE with `ibuffer-eliding-string`, the summary line, the title line or `ibuffer-use-header-line` title),
the columns mark,
modified,
read-only,
locked,
name (`ibuffer-buffer-name-face`, `ibuffer-fontification-alist`),
size (`ibuffer-human-readable-size`),
recency,
mode,
process,
filename,
filename-and-process with their summarizers;
`ibuffer-update` (`ibuffer-maybe-show-predicates`, `ibuffer-never-show-predicates`, `ibuffer-always-show-predicates`, the temporary hide and show regexps, `ibuffer-always-show-last-buffer`, `ibuffer-view-ibuffer`),
`ibuffer-redisplay`,
`ibuffer-switch-format`,
`ibuffer-map-lines` and the line properties,
movement (`ibuffer-forward-line`, `-backward-line`, `ibuffer-movement-cycle`, the next-marked commands),
marks (m, u, DEL, t, d, C-d, `* *`, `* c`, U and the `* `, `%` and `.` marking commands, `ibuffer-save-marks`),
operations through `define-ibuffer-op`'s driver (`ibuffer_operate`: confirmation, OPSTRING message, modification flag): save,
toggle modified and read-only,
delete,
kill on deletion marks,
kill lines,
shell commands (pipe, pipe replace, file),
rename uniquely,
revert,
replace regexp,
query replace (regexp),
occur;
visiting (RET, o, C-o, M-o, v, C-x v, A, C-x C-f, b),
sorting (recency, alphabetic, size, file name/process, major mode, mode name; `,`, `s i`),
`ibuffer-jump-to-buffer`,
`ibuffer-diff-with-file`,
`ibuffer-copy-filename-as-kill`,
`ibuffer-copy-buffername-as-kill`,
the filters (`ibuffer-filtering-qualifiers` as nested function calls; `ibuffer-included-in-filters-p` and -filter-p, `ibuffer-unary-operand`, every `define-ibuffer-filter` filter with its `ibuffer-filter-by-*` command and reader: mode, used-mode, derived-mode with `completing-read-multiple`, name, process, starred-name, filename, basename, file-extension, directory, size-gt, size-lt, modified, visiting-file, content, test; the filter stack: pop, decompose, exchange, negate, or, and, disable; `ibuffer-filter-chosen-by-completion`, `ibuffer-interactive-filter-by-mode`, `ibuffer-format-qualifier`; `ibuffer-saved-filters` with save, add, switch, delete; `ibuffer-filter-format-alist`),
the filter groups (`ibuffer-filter-groups`, `ibuffer-generate-filter-groups`, the hidden groups, from the filters or by mode, pop, decompose, clear, kill and paste with C-k and C-y, jump, `ibuffer-saved-filter-groups` with save, switch, delete),
`ibuffer-bs-show`,
`ibuffer-bs-toggle-all`,
`ibuffer-auto-mode`,
the faces and all the options

## Deferred

saving the filters and groups with Customize (step 26),
`ibuffer-repair-saved-filters` (the old format),
fractional sizes for `/ <` and `/ >`,
menus and mouse commands,
the mode name and header keymaps,
help-echo,
frames,
etags,
emacs-lock (locked buffers),
misearch,
printing,
cus-edit,
`truncate-lines`,
user-defined sorting or operation commands,
inline columns,
remote files in diffs;
not applicable: `ibuffer-do-eval`,
`ibuffer-do-view-and-eval`,
reading a test form (`/ e`)

## Tests

test_ibuffer_mod (ports ibuffer-test-Bug25000; own tests of the list, marks, kills, sorting and formats),
test_ibuffer_extensions_mod (ports ibuffer-test-Bug24997, -Bug25058, ibuffer-save-filters, ibuffer-filter-inclusion-1 to -8, ibuffer-decompose-filter, ibuffer-and-filter, ibuffer-or-filter, ibuffer-format-qualifier, ibuffer-unary-operand, with ported modes in place of the others; own tests of filter groups)
