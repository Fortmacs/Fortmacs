# compile_mod

- Module: compile_mod
- Upstream: progmodes/compile.el (+ files.el `cd`, simple.el `forward-to-indentation`, startup.el `command-line-normalize-file-name`)
- State: partial

## Ported

`compile` (M-x compile; C-u: Comint mode with `compilation-shell-minor-mode`),
`recompile`,
`compilation-start` (as before, plus MODE t, "[Compiling] " and `mode-line-process` ":run"/":exit [N]" with the error/warning/info counts),
`compilation-mode` (`compilation-mode-map`: RET, C-c C-c, C-o, C-c C-k, M-n, M-p, M-{, M-}, n, p, TAB, <backtab>, g),
error parsing: `compilation-error-regexp-alist-alist` (all 67 rules, the `rx` ones as regexps; HIGHLIGHTs, makepp's and omake's forms, javac's `current-column`),
`compilation-error-regexp-alist`,
functions as FILE/LINE/COLUMN,
`compilation-directory-matcher`,
`compilation-transform-file-match-alist`,
`compilation-error-case-fold-search`,
`compilation-parse-errors`,
`compilation--ensure-parse` (from the filter, motion and font-lock),
the file structures and locations,
`compilation-error-screen-columns`,
`compilation-first-column`;
the faces `compilation-error`,
`-warning`,
`-info`,
`-mode-line-fail`,
`-mode-line-run`,
`-mode-line-exit`,
`-line-number`,
`-column-number` and the face variables;
`compilation-mode-font-lock-keywords`;
`compilation-next-error`,
`-previous-error`,
`-next-file`,
`-previous-file`,
`-display-error`,
`compile-goto-error`,
`compilation-next-error-function`,
`compilation-goto-locus`,
`compilation-find-file` (with `compilation-search-all-directories` and the prompt),
`compilation-skip-threshold`,
`compilation-set-skip-threshold`,
`compilation-skip-visited`,
`compilation-skip-to-next-location`,
`compilation-context-lines` (`compilation_context`: `%lines = N`, COMPILATION_CONTEXT_TOP, COMPILATION_CONTEXT_NO_SCROLL),
`compilation-forget-errors`,
`compilation-minor-mode`,
`compilation-shell-minor-mode`,
`kill-compilation`,
`compilation-find-buffer`,
modes made by `define-compilation-mode` (the MODE argument of `compilation-start` and `recompile`, the *NAME* buffers, `compilation_set_mode_locals` for a mode's own `compilation-error-regexp-alist` rules, directory matcher, transforms, `compilation-error`, `compilation-mode-font-lock-keywords`, `compilation-mode-line-errors`, `compilation-process-setup-function` with the environment and `compilation-exit-message-function`),
HIGHLIGHTs (face FACE display DISPLAY),
`compilation_message_location` (the location markers for Grep-Edit mode)

## Deferred

`compilation-auto-jump-to-first-error` (timers),
highlighting the locus and the overlay arrow,
the `keymap` text property (`compilation-button-map`),
mouse,
menus,
tool bar,
global table elements given as specifications,
FILE formats,
other HIGHLIGHT property lists,
`compilation-parse-errors-filename-function`,
`compilation-search-path`,
`compilation-fake-loc`,
the face-nil parts of the "Compilation finished/exited" keywords,
removing keywords when the minor modes are turned off,
visiting directory messages,
`selective-display`,
`compilation-max-output-line-length`,
`compilation-hidden-output`,
`compilation-window-height`,
`compilation-page-delimiter`,
`compilation-environment`,
the hooks and functions with arguments (but a mode's setup and exit message functions),
`compilation-save-buffers-predicate`,
NAME-FUNCTION/HIGHLIGHT-REGEXP/CONTINUE,
wildcards after `cd`,
the variables `define-compilation-mode` sets from the mode's name (the mode's body sets them)

## Tests

test_compile_mod (ports compile-test-error-regexps (all 179 cases and the counts) and compile-test-functions; own: compilations, directory tracking, next-error and RET, Comint compilations);
compile-test-grep-regexps is in test_grep_mod
