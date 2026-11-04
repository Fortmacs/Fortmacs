# shell_mod

- Module: shell_mod
- Upstream: shell.el
- State: partial

## Ported

`shell` (M-x shell: "*shell*", `explicit-shell-file-name`, `shell-file-name`, ESHELL, `explicit-bash-args`/`explicit-csh-args`, the ~/.emacs_SHELL and ~/.emacs.d/init_SHELL.sh startup files),
`shell-mode` (from comint-mode: prompt, delimiters, paragraphs, font-lock keywords, `shell-get-old-input`, history files with HISTFILE/HISTSIZE, `shell-dirstack-query`, the bash ^A/^B filter),
`shell-mode-hook`,
`shell-mode-map` (C-c C-f, C-c C-b, TAB, M-?, C-x n d),
`shell-command-mode`,
`shell-dirtrack-mode` with `shell-directory-tracker` (cd, pushd, popd),
`shell-cd`,
`split-string-shell-command`,
`shell-unquote-argument`,
`shell-eval-command`,
`shell-kill-buffer-on-exit`,
`list-buffers-directory` ;
`shell-apply-ansi-color` and `shell-reapply-ansi-color` (`ansi-color-apply-face-function` in Shell mode and Shell Command mode) ;
completion (`shell-completion-vars`, `shell-dynamic-complete-functions`, `shell--unquote&requote-argument`, `shell--parse-pcomplete-arguments` as pcomplete's parser, `shell-command-completion-function`, `shell-command-completion` and shell--command-completion-data with `shell-completion-execonly`, `shell-dynamic-complete-command`, `shell-filename-completion`, `shell-match-partial-variable`, `shell-environment-variable-completion`, `shell-dynamic-complete-environment-variable`, `shell-c-a-p-replace-by-expanded-directory`, `shell-replace-by-expanded-directory`, `shell-completion-fignore`, `shell-file-name-chars`, `shell-file-name-quote-list`, `shell-input-autoexpand`)

## Deferred

`shell-dynamic-complete-filename` (upstream passes the completion properties to `completion-in-region` as extra arguments),
`shell-resync-dirs`,
fontification of input,
`shell-highlight-undef-mode`,
remote shells

## Tests

test_shell_mod (ports shell-tests.el; own tests of command and environment variable completion)
