# project_mod

- Module: project_mod, project_cross_reference_mod (submodule)
- Upstream: progmodes/project.el (+ vc.el `vc-default-ignore-completion-table`)
- State: partial

## Ported

the project object: `project_type` (the generics `project-root`, `project-external-roots`, `project-name`, `project-ignores`, `project-files` as `list_files`, `project-buffers` as type-bound procedures with upstream's defaults),
`project-current` (`get_current_project`, with `project-current-directory-override` and `project-find-functions` as `project_find_functions`, a list of procedures with `%append`),
`project--find-in-directory`,
the transient project (`transient_project_type`),
`project-prompter` (`read_project_dir`, `read_project_name`, `project-read-project`, `project-prompt-project-dir`, `project-prompt-project-name`, `project--dir-history`, `project--name-history`),
`project--files-in-directory`,
`project--dir-ignores`,
`project-files-relative-names`,
`project-combine-directories`,
`project-subtract-directories`;
the VC-aware backend: `project-try-vc` (`try_vc_project`, `project-try-vc--search`, `project--get-cached`, `project--set-cached`, `project--clear-cache`, `project-vc-cache-timeout`, `project-vc-non-essential-cache-timeout`),
`vc_project_type` (`project-vc--backend`),
`project-vc-backend-markers-alist` (`project_vc_backend_markers`),
`project-vc-extra-root-markers`,
`project-vc-ignores` (the global value),
`project-vc-include-untracked`,
`project-vc-merge-submodules` (`project--git-submodules`, `project--submodule-p`),
`project-vc-name`,
`project-vc-external-roots-function`,
`project--vc-ignores` (`project_vc_ignores_of`, with vc.el's `vc-default-ignore-completion-table`),
`vc-git-project-list-files` (`git ls-files`);
the known projects: `project-list-file` (namelist syntax, namelist_mod),
`project--read-project-list`,
`project--ensure-read-project-list`,
`project--write-project-list`,
`project-remember-project`,
`project--remember-dir`,
`project--remove-from-project-list`,
`project-forget-project`,
`project-known-project-roots`,
`project-list-exclude`,
`project-prune-zombie-projects` (`project-prune-zombies-default` as `is_local_project`, `project--delete-zombie-projects`),
`project-remember-projects-under`,
`project-forget-zombie-projects`,
`project-forget-projects-under`;
the commands,
on C-x p (`project-prefix-map`): `project-find-file` (f, `find_project_file`),
`project-or-external-find-file` (F),
`project-find-file-in`,
`project-root-find-file`,
`project-find-dir` (d),
`project-dired` (D),
`project-vc-dir` (v),
`project-shell` (s),
`project-shell-command` (!),
`project-async-shell-command` (&),
`project-compile` (c),
`project-recompile`,
`project-find-regexp` (g) and `project-or-external-find-regexp` (G) with `project--find-regexp-in-files` and `project--read-regexp` (in project_cross_reference_mod, which uses cross_reference_mod),
`project-switch-to-buffer` (b, `project--read-project-buffer`, `project--buffer-check`, `project-ignore-buffer-conditions`),
`project-display-buffer`,
`project-list-buffers` (C-b, `project-buffers-viewer`, `project-list-buffers-buffer-menu`),
`project-kill-buffers` (k, `close_project_buffers`, `project-kill-buffer-conditions` as `project_close_buffer_conditions`, `project--buffers-to-kill`),
`project-save-some-buffers` (C-x s),
`project-execute-extended-command` (x),
`project-any-command` (o),
`project-prefix-or-any-command`,
`project-switch-project` (p, `project-switch-commands`, `project-switch-use-entire-map`, `project--switch-project-command`, `project--keymap-prompt`, `project--menu-prompt`),
`project-prefixed-buffer-name`,
`project-read-file-name-function` (`project--read-file-cpd-relative`, `project--read-file-absolute`, `project--read-file-name`, `project--completing-read-strict`, `project--find-default-from`)

## Deferred

`project-query-replace-regexp` (r) and `project-search` (fileloop.el);
`project-eshell` (e, Eshell);
the other-place commands and maps (`project-other-window-command` C-x 4 p, `project-other-frame-command` C-x 5 p, `project-other-tab-command` C-x t p, `project-other-window-map`, `project-other-frame-map`, `project--other-place-command`, `project--other-place-prefix`, `project-display-buffer-other-frame`);
`project-find-matching-file-or-directory`,
`project-find-matching-buffer` and `project-find-matching-buffer-function`,
`project-change-to-matching-directory` (`project--transplant-file-name`);
directory-local variables (`project-customize-dirlocals`, `project--read-dir-locals`, `project--value-in-dir`: `project-vc-ignores`, `project-vc-name` and the rest have only their global values; the project-name-<vc> tests);
the mode line (`project-mode-line`, `project-mode-line-face`, `project-mode-line-format`, `project-mode-line-map`);
`project-compilation-buffer-name-function`;
`project-file-history-behavior` (relativizing the history, with the `project` text property of its entries);
`project-key-prompt-style` (the faces of the dispatch menu);
`project-kill-buffers-display-buffer-list`;
`project-list-buffers-ibuffer`;
`project-uniquify-dirname-transform` and uniquify's buffer names in `project--read-project-buffer` (uniquify.el);
hiding the internal buffers from the completions of an empty input in `project--read-project-buffer`;
the file name at point as the default of the find-file commands (`thing-at-point` filename);
`project-vc-cache-timeout` by directory test;
`minibuffer-default-add-completions` in the file prompts;
remote files (`project--remote-file-names`, `project--within-roots-fallback`);
`vc-hg-project-list-files` and `vc-default-project-list-files` (the other backends list with `find`);
`project-regexp-history-variable` (the history is `grep_regexp_history`);
the obsolete `project-roots`;
Not applicable: `project--buffers-completion-table`,
`project--completion-table-with-category` and `project--file-completion-table` (completion tables are `completion_table_type` with a category),
`project--vc-merge-submodules-p` (the setting is read directly)

## Tests

test_project_mod (ports project-tests.el: project-ignores, project-ignores-bug-50240, project-vc-recognizes-git, project-vc-extra-root-markers-supports-wildcards, project-vc-supports-project-in-different-dir, project-vc-supports-files-in-subdirectory, project-vc-ignores-in-external-directory, project-vc-nonexistent-directory-no-error, project-try-vc-uses-cache, project-try-vc-invalidates-cache, project-find-regexp; project/quoted-directory waits for quoted file names, project-find-regexp-with-prefix stubs prompts with cl-letf, the project-name-<vc> tests wait for directory-local variables; own: a project from `project_find_functions`, the known projects, the buffer conditions, `combine_project_directories`, the VC ignores, C-x p)
