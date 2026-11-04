# abbrev_mod

- Module: abbrev_mod, abbrev_cmds_mod (submodule)
- Upstream: abbrev.el, buffer.c `abbrev-mode` and `local-abbrev-table`, files.el `save-abbrevs`
- State: partial

## Ported

abbrev tables as `abbrev_table_type` (`define` = define-abbrev with :count, :system, force, :case-fixed, :enable-function and a hook with `no-self-insert`; `define_table` = define-abbrev-table; components `parents`, `case_fixed`, `regexp`, `enable_function`, `modiff`),
`abbrev-table-name-list`,
`global-abbrev-table`,
`fundamental-mode-abbrev-table`,
per-mode tables (text, prog, special, help, edit-abbrevs),
`abbrev-mode` (minor mode, " Abbrev", first in the mode line),
`kill-all-abbrevs`,
`copy-abbrev-table`,
`insert-abbrevs`,
`list-abbrevs`,
`prepare-abbrev-list-buffer`,
`edit-abbrevs`,
Edit-Abbrevs mode,
`abbrev-table-name`,
`write-abbrev-file` (into the init file, see Design decisions),
`add-mode-abbrev`,
`add-global-abbrev`,
`add-abbrev`,
the inverse ones,
`abbrev-prefix-mark`,
`expand-region-abbrevs`,
`clear-abbrev-table`,
`abbrev--check-chars`,
`define-global-abbrev`,
`define-mode-abbrev`,
`abbrev--active-tables`,
`abbrev--symbol`,
`abbrev-symbol`,
`abbrev-expansion`,
`abbrev--before-point`,
`abbrev-insert` (case handling, `abbrev-all-caps`),
`expand-abbrev`,
`abbrev-expand-function`,
`abbrev--default-expand`,
`unexpand-abbrev`,
`abbrev--describe`,
`insert-abbrev-table-description`,
`abbrev--table-symbols`,
`abbrev-table-empty-p`,
`abbrev--possibly-save`,
`only-global-abbrevs`,
`abbrevs-changed` (reset after the init file),
`last-abbrev`,
`last-abbrev-text`,
`last-abbrev-location`,
`abbrev-start-location`;
C-x a l/C-a/g/+/i g/i l/-/e/',
C-x ',
M-';
abbrevs expand in insert-tab,
indent-relative and tab-to-tab-stop;
open-line binds abbrev-mode off

## Deferred

non-string expansions,
list-valued `local-abbrev-table`,
`abbrev-minor-mode-table-alist`,
`abbrev-expand-functions`,
other table and abbrev properties (`abbrev-table-get/put`, `abbrev-get/put`),
`abbrev-suggest`,
`abbrev-table-menu`,
`define-abbrevs`,
`edit-abbrevs-redefine` and the edit-abbrevs-mode-map keys,
Edit-Abbrevs font-lock and its face,
hooks in *Abbrevs*,
writing abbrevs with hooks or enable functions;
`read-abbrev-file` and `quietly-read-abbrev-file` are not applicable

## Tests

test_abbrev_mod (ports abbrev--table-symbols-test, copy-abbrev-table-test, abbrev-table-empty-p-test, kill-all-abbrevs-test, abbrev-table-name-test, clear-abbrev-table-test, list-abbrevs-test, prepare-abbrev-list-buffer-test, insert-abbrevs-test, read-write-abbrev-file-test(-with-props) as writing, the inverse-add-abbrev tests, abbrev--possibly-save-test; own tests through the command loop)
