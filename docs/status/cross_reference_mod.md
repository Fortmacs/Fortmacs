# cross_reference_mod

- Module: cross_reference_mod
- Upstream: progmodes/xref.el (+ subr.el `add-hook`/`remove-hook` for a buffer's own backend functions)
- State: partial

## Ported

the items: `xref_item_type` (xref-item and xref-match-item: summary, location, length; `make_xref`, `make_xref_match`, `append_xref_item`),
the locations as extensions of `xref_location_type` (`xref_file_location_type` with `make_xref_file_location`, `xref_buffer_location_type`, `xref_bogus_location_type`; the generics `xref-location-marker`, `xref-location-group`, `xref-location-line` as type-bound procedures),
the backends: `xref_backend_type` (the generics `xref-backend-definitions`, `-references`, `-apropos`, `-identifier-at-point`, `-identifier-completion-table`, `-identifier-completion-ignore-case` as type-bound procedures with upstream's defaults),
`xref-backend-functions` (the global value and a buffer's own),
`xref-find-backend`,
`xref--no-backend-available`;
the history (`xref--history`, the global storage: `push_xref_marker_stack`, `clear_xref_marker_stack`, `is_xref_marker_stack_empty`, `is_xref_forward_history_empty`, `xref--push-backward`, `xref--push-forward`),
`xref-go-back` (M-,, `go_back_in_xref_history`),
`xref-go-forward` (C-M-,, `go_forward_in_xref_history`),
`xref-pop-marker-stack` (obsolete),
`xref-after-jump-hook`,
`xref-after-return-hook`,
`xref-after-update-hook`,
`xref-pop-to-location` (`pop_to_xref_location`: window, other window; `xref--show-pos-in-buf`, `widen-automatically`),
`xref-current-item`;
the *xref* buffer: `xref--xref-buffer-mode` (`xref_xref_buffer_mode`, derived from Special mode: n, p, N, P, RET, TAB, C-o, ., ,, M-,, e and g through `revert_xref_buffer`),
`xref--transient-buffer-mode`,
`xref-edit-mode` (`change_to_xref_edit_mode` e, `save_xref_edit_changes` C-c C-c, `xref-edit--prepare-buffer`, `xref-edit--before-change-function`),
`xref-next-line`,
`xref-prev-line` (`previous_xref_line`),
their `-no-show` forms,
`xref-next-group`,
`xref-prev-group`,
`xref-goto-xref`,
`xref-quit-and-goto-xref`,
`xref-quit-and-pop-marker-stack`,
`xref-show-location-at-point`,
`xref--item-at-point`,
`xref--search-property`,
`xref--insert-xrefs` (the file headers and line numbers, the faces `xref-file-header`, `xref-line-number`, `xref-match`; `xref-file-name-display` with `xref--group-name-for-display`, `xref--analyze`),
`xref-truncation-width` (`xref--apply-truncation` on redisplay),
the mode line's match count (`xref-num-matches-found`, `xref-match-count-face` as `xref_match_count_face`),
`xref--show-xrefs`,
`xref--show-defs`,
`xref-show-xrefs-function`,
`xref-show-definitions-function` (`xref--show-xref-buffer`, `xref-show-definitions-buffer`, `xref-show-definitions-completing-read`),
the fetchers (`xref_fetcher_type`, `xref--create-fetcher`),
`xref--show-common-initialize`,
`xref--auto-jump-first` (`xref-auto-jump-to-first-definition`, `xref-auto-jump-to-first-xref`: choice settings: `.false.`, `.true.`, `XREF_AUTO_JUMP_SHOW`, `XREF_AUTO_JUMP_MOVE`),
`xref--switch-to-buffer`,
`xref-prompt-for-identifier` (a choice setting: `.true.`, `.false.`, `XREF_PROMPT_FOR_IDENTIFIER_LISTED`, `_EXCEPT_LISTED`, with the list of commands in its `%commands`, R6-39; C-h v shows the commands after the state that uses them, `XREF_PROMPT_FOR_IDENTIFIER_EXCEPT_LISTED, %commands = ["find_xref_definitions", ...]`, as upstream shows its whole list, without making `.false.` count as on),
`xref--prompt-p`,
`xref--read-identifier` (`read_xref_identifier`, `xref--read-identifier-history`),
`xref-find-definitions` (M-., `find_xref_definitions`),
`xref-find-definitions-other-window` (C-x 4 .),
`xref-find-references` (M-?, `find_xref_references`),
`xref-find-apropos` (C-M-., `find_xref_apropos`, `xref-apropos-regexp`, `xref--read-pattern-history`),
`xref-etags-mode`;
the search: `xref-matches-in-directory`,
`xref-matches-in-files`,
`xref-references-in-directory` (`xref-references-in-directory-function`: `xref--references-in-directory-grep` the default),
`xref-search-program` (`XREF_SEARCH_PROGRAM_GREP`/`_RIPGREP`/`_UGREP`) and `xref-search-program-alist` (`xref_search_programs`, records of `xref_search_program_type`),
`xref--rgrep-command`,
`xref--find-ignores-arguments`,
`xref--regexp-to-extended`,
`xref--parse-hits`,
`xref--sort-hits`,
`xref--convert-hits`,
`xref--collect-matches`,
`xref--collect-matches-1` (`syntax-propertize` for the match's identifier bounds);
the current project (project_mod, step 49b): `xref--project-root`,
the default `xref-backend-references` searching the project root and external roots,
`xref--analyze` relative to the project root,
the project's ignores in `xref-references-in-directory-grep`;
`init_xref` binds the keys in `esc_map` and `ctl_x_4_map`

## Deferred

the mouse: `xref-find-definitions-at-mouse`,
`xref-find-references-at-mouse`,
`xref-select-and-show-xref`,
`xref--button-map` and the mouse bindings of the modes;
`xref-find-definitions-other-frame` (C-x 5 .);
`xref-query-replace-in-results` (r),
`xref-find-references-and-replace` (with `xref--query-replace-1`, `xref--buf-pairs-iterator`, `xref--outdated-p`, `xref-revert-buffer-restore-point`);
`xref-pulse-momentarily` (pulse.el) and the overlay arrow (`xref--set-arrow`);
`xref-history-storage` with `xref-window-local-history` (the global history only; `xref--get-history`, `xref--make-xref-history`);
`xref-show-definitions-buffer-at-bottom` (`xref--show-defs-buffer-at-bottom`: `display-buffer-in-direction`);
the `group-function` of the completion table's metadata (`xref--completing-read-group`);
`xref--ensure-default-directory` (the directory is `default-directory`);
remote files (`xref--hits-remote-id`, `xref--with-connection-local-variables`, `xref--process-file-region`: process-file);
Imenu in the *xref* buffer (`xref--imenu-prev-index-position`, `xref--imenu-extract-index-name`, step 49c) and `xref--add-log-current-defun`;
Semantic and EDE (`xref-references-in-directory-semantic`, `ede-minor-mode`);
the obsolete `xref-marker-ring-length` and `xref-set-marker-ring-length`;
Not applicable: `xref--alistify` (the groups are arrays),
`xref--insert-propertized`,
`xref--defstruct`,
`xref--fetcher` (the fetcher object),
`xref--original-window` and `xref--original-window-intent` (components of the show parameters)

## Tests

test_cross_reference_mod (ports xref-tests.el: xref-matches-in-directory-finds-none-for-some-regexp, -finds-some-for-bar, -filters-with-ignores, -finds-two-matches-on-the-same-line, -finds-an-empty-line-regexp-match, xref-matches-in-files-includes-matches-from-all-the-files, -trims-summary-for-matches-on-same-line, xref--xref-file-name-display-is-abs, -is-nondirectory, -is-relative-to-project-root; the three xref--buf-pairs-iterator tests wait for `xref-query-replace-in-results`; own: the *xref* buffer, the history, `xref--regexp-to-extended`)
