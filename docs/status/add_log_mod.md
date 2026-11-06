# add_log_mod

- Module: add_log_mod, add_log_diff_mod (submodule)
- Upstream: vc/add-log.el
- State: partial

## Ported

`find-change-log` (a Diff buffer's source buffer through `diff-find-source-location`, in add_log_diff_mod),
`change-log-name`,
`add-log-find-changelog-buffer`,
`add-log--pseudo-changelog-buffer-name`,
`add-log--changelog-buffer-p` (`is_add_log_changelog_buffer`),
`add-log-full-name`,
`add-log-mailing-address`,
`add-log-time-format` (`add_log_time_procedure` returns the date and a status: `format_date` reports its errors through it),
`add-log-iso8601-time-string` (a `date_format_type`),
`add-log-dont-create-changelog-file`,
`add-change-log-entry` (WHOAMI, CHANGELOG-FILE-NAME, OTHER-WINDOW, NEW-ENTRY, PUT-NEW-ENTRY-ON-NEW-LINE; the copyright notice, merging with the previous item, `fill-column`),
`add-change-log-entry-other-window` (C-x 4 a),
`prompt-for-change-log-name` (`read_change_log_name`),
`add-log-file-name`,
`add-log-current-defun` (`add_log_current_subprogram`, with `add-log-current-defun-header-regexp`),
`add-log-current-defun-function` (buffer-local: Shell-script, Makefile, Python, CSS, HTML, F90 and Diff modes set it),
`add-log-buffer-file-name-function`,
`add-log-file-name-function`,
`add-log-keep-changes-together`,
`add-log-always-start-new-record`,
`change-log-version-info-enabled`,
`change-log-version-number-regexp-list` (`change_log_version_number_regular_expressions`) and `change-log-version-number-search` (`search_change_log_version_number`),
`change-log-mode` (syntax table, faces, `change-log-font-lock-keywords`, `page-delimiter`, C-c C-f),
`change-log-find-file` (`find_change_log_entry_file`, C-c C-f),
`change-log-search-file-name`,
`change-log-file-names-re`,
`change-log-start-entry-re`,
`change-log-read-defuns`,
`change-log-insert-entries`,
`change-log-unindented-file-names-re`,
`change-log-tag-re`,
`add-log-current-defun-function` and `add-log-buffer-file-name-function` as buffer-local procedure settings (`%procedure`, `%set_local`, `%get_current`, `%clear_local`)

## Deferred

`add-log-edit-prev-comment` and `add-log-edit-next-comment` (C-c C-p, C-c C-n),
`change-log-goto-source` (C-c C-c) and next-error support (`change-log-next-error`, `change-log-search-tag-name`, `change-log-goto-source-1`),
`change-log-merge`,
`change-log-resolve-conflict`,
`change-log-read-entries`,
`change-log-sortable-date-at`,
`change-log-next-buffer`;
Change Log mode's filling and indentation (`change-log-indent`, `change-log-fill-parenthesized-list`, `change-log-fill-file-list`, `change-log-fill-forward-paragraph`, `fill-nobreak-predicate`),
`change-log-beginning-of-defun` and `-end-of-defun`,
`change-log-get-method-definition`,
and its `show-trailing-whitespace`,
`adaptive-fill-regexp`,
`comment-start`,
`tab-always-indent`,
`copyright-at-end-flag`,
`version-control` settings;
the menu;
`add-log-mailing-address` as a list (one address chosen at random),
`add-log-time-zone-rule`,
`add-log-iso8601-with-time-zone`,
`use-hard-newlines`;
the language support upstream keeps in add-log-current-defun's docstring (C, Lisp, LaTeX, Texinfo, Perl) comes with those modes

## Tests

test_add_log_mod (ports add-log-tests.el: the four sh tests; own: `add-change-log-entry`, `change-log-insert-entries`, `change-log-search-file-name`),
test_log_edit_mod (through `log-edit-fill-entry`)
