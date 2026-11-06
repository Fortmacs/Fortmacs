# shell_script_mod

- Module: shell_script_mod
- Upstream: progmodes/sh-script.el
- State: partial

## Ported

`sh-mode` and `sh-base-mode` (`shell-script-mode`; the syntax table and its rpm variant, `sh-mode-map` with the remaps, the abbrev table, three highlighting levels as functions, `sh-font-lock-syntactic-face-function`, the faces `sh-heredoc`, `sh-quoted-exec` and `sh-escaped-newline`),
the feature tables (`sh-feature` with `sh-append` and `sh-ancestor-alist`, `sh-alias-alist`, `sh-canonicalize-shell`, `sh-builtins`, `sh-leading-keywords`, `sh-other-keywords`, `sh-variables`, `sh-assignment-regexp`, `sh-shell-arg`, `sh-indent-supported`),
`sh-set-shell` (C-c :),
`shell--guess-shell`,
`sh-syntax-propertize-function` (here documents, `#`, case patterns, subshells in strings),
SMIE indentation for sh and rc (both grammars, lexers and rules, `sh-smie--indent-continuation` with `sh-indent-after-continuation`, the hanging-eolp advice, `sh-var-value` and all the indentation variables as choice settings: `%columns = N` or the constants SHELL_PLUS … SHELL_SLASH, buffer-local values included),
`sh-basic-indent-line` for the other shells,
`sh-make-vars-local`,
`sh-reset-indent-vars-to-global-values`,
`sh-execute-region` (C-M-x),
`sh-add` (C-c +),
`sh-shell-initialize-variables`,
`sh-remember-variable`,
`sh-assignment` (obsolete),
`sh-electric-here-document-mode` (in `sh-mode-hook`),
`sh-beginning-of-command`,
`sh-end-of-command`,
`sh-backslash-region` (C-c C-\),
`sh-current-defun-name` (Shell-script mode's `add-log-current-defun-function`),
`defun-prompt-regexp`,
completion at point (`sh-completion-at-point-function` with `shell--vars-before-point`, `shell--cmd-completion-table-gen` and its cache; `sh-dynamic-complete-functions` through `comint-dynamic-complete-functions`),
the inferior shell (`sh-shell-process`, `sh-show-shell` C-c C-z, `sh-send-text`, `sh-cd-here` C-c C-d, `sh-send-line-or-region-and-step` C-c C-n),
the indentation styles (`sh-styles-alist` as an array of `shell_style_type`, `sh-name-style`, `sh-load-style`, `sh-save-styles-to-buffer` writing a Fortran assignment),
the statement skeletons (`sh-case` C-c C-c, `sh-for` C-c C-f, `sh-indexed-loop` C-c C-l, `sh-function` C-c (, `sh-if` C-c C-i, `sh-repeat` C-c C-r, `sh-select` C-c C-s, `sh-tmp-file` C-c C-t, `sh-until` C-c C-u, `sh-while` C-c C-w, `sh-while-getopts` C-c C-o; one `skeleton_variants_type` per command, chosen by `shell_skeleton_feature` (sh-feature as `skeleton_filter_function`), `sh-modify` on the typed skeletons, `sh-test`, `sh-non-closing-paren`, sh-mode's further element `<` as a value element), the skeleton settings of `sh-base-mode` (`sh-skeleton-pair-default-alist`, the backquote pair, `sh-quoted-p` as the pair filter, `skeleton-newline-indent-rigidly`), smie-config's keys (C-c ?, C-c =, C-c <, C-c >); auto-mode-alist and interpreter-mode-alist entries; `sh-imenu-generic-expression` (a feature table of `index_menu_pattern_type` arrays, `shell_index_menu_feature_type`), `imenu-case-fold-search`, the Imenu candidates of command completion (`shell--cmd-completion-table-gen`) (step 49c)

## Deferred

`executable-interpret` (C-c C-x; `compilation-start`'s NAME-FUNCTION and rule specifications),
`outline-regexp`,
`escaped-string-quote` (with `yank-in-context`),
the Flymake backend,
the `sh-shell` file-local variable,
the menu,
`bash-ts-mode`

## Tests

test_shell_script_mod (ports sh-script-tests: all 6, with the cases of sh-indents.erts; own: feature tables, here documents, `sh-canonicalize-shell`, the statement skeletons for bash and csh as GNU Emacs inserts them, `sh-modify`, smie-config, the indentation variables' `%columns`, named constants and buffer-local values);
test_index_menu_mod (ports imenu-simple-scan-deftest-sh)
