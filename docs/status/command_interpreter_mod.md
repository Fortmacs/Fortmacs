# command_interpreter_mod

- Module: command_interpreter_mod
- Upstream: comint.el (+ comint-carriage-motion moved from compile_mod)
- State: partial

## Ported

`comint-mode` (`comint-mode-map` with M-p, M-n, C-<up>, C-<down>, C-c M-r, C-c M-s, C-M-l, RET, C-d, <delete>, C-c SPC, C-c C-x, C-c C-a, C-c C-u, C-c C-w, C-c C-c, C-c C-\, C-c RET, C-c C-o, C-c M-o, C-c C-r, C-c C-e, C-c C-n, C-c C-p, C-c C-d, C-c C-s; `comint-mode-hook`, `mode-line-process` ":%s"),
`make-comint`,
`make-comint-in-buffer`,
`comint-run`,
`comint-exec` (`comint-exec-hook`, startfile, the TERM/INSIDE_EMACS/PAGER environment),
`comint-output-filter` (`comint-preoutput-filter-functions`, `comint-output-filter-functions`, carriage motion, output fields, the prompt in `comint-highlight-prompt`, `comint-prompt-read-only`),
`comint-send-input` (`comint-input-sender`, `comint-input-filter`, `comint-input-filter-functions`, `comint-process-echoes`, `comint-eol-on-send`, `comint-highlight-input`),
the input ring (`comint-input-ring-size`, `comint-input-ignoredups`, history files with `comint-input-ring-file-name`, M-p/M-n, matching input),
`comint-bol`,
`comint-delete-output`,
`comint-write-output`,
`comint-show-output`,
`comint-show-maximum-output`,
`comint-clear-buffer`,
`comint-truncate-buffer`,
`comint-strip-ctrl-m`,
the interrupt,
kill,
quit and EOF commands (CURRENT-GROUP through the pty's VINTR/VQUIT characters),
`comint-kill-input`,
`comint-delchar-or-maybe-eof`,
prompt motion,
`comint-arguments`,
`comint-delim-arg`,
`comint-move-point-for-output`,
`comint-scroll-show-maximum-output`,
`comint-postoutput-scroll-to-bottom`;
history expansion (`comint-input-autoexpand` with input and history, `comint-replace-by-expanded-history`, `comint-c-a-p-replace-by-expanded-history`, `comint-replace-by-expanded-history-before-point`, `comint-magic-space`, `comint-args`, `comint-within-quotes`, `comint-how-many-region`, `comint-previous-input-string`),
`comint-insert-previous-argument` (C-c .) with `comint-insert-previous-argument-from-end`,
Isearch in the input history (M-r `comint-history-isearch-backward-regexp`, `comint-history-isearch-backward`, `comint-history-isearch`, the setup, end, search, message, wrap and push-state functions, `comint-goto-input`),
`comint-stop-subjob` (C-c C-z),
`comint-continue-subjob`,
`comint-scroll-to-bottom-on-input` (`comint-preinput-scroll-to-bottom` on the buffer's `pre-command-hook`) ;
password prompts (`comint-password-prompt-regexp` from mule-conf.el's words and colons, `comint-password-prompt-max-length`, `comint-watch-for-password-prompt` on `comint-output-filter-functions` through a timer, `comint-send-invisible`, `comint-password-function`, `comint-read-noecho`) ;
ANSI colors (`ansi-color-process-output` first in the default `comint-output-filter-functions`) ;
completion (`comint-completion-at-point` on `completion-at-point-functions`, `comint-dynamic-complete-functions` with `comint-c-a-p-replace-by-expanded-history` and `comint-filename-completion`, `comint-word`, `comint-match-partial-filename`, `comint-quote-filename`, comint--unquote&requote-argument through `completion-table-with-quoting`, `comint-completion-file-name-table` with `comint-file-name-prefix`, the `:exit-function` adding `comint-completion-addsuffix`, `comint-completion-fignore`, `comint-file-name-chars`, `comint-file-name-quote-list`, `comint-dynamic-complete-filename`, `comint-replace-by-expanded-filename`, `comint-dynamic-list-filename-completions`, `comint-complete-input-ring` on C-x <up>),
`comint-directory`,
`comint-get-old-input`, `comint-input-sender`, `comint-input-filter`, `comint-password-function`, `comint-unquote-function` and `comint-requote-function` as buffer-local procedure settings (`%procedure`, `%set_local`, `%get_current`, `%clear_local`), `comint-completion-fignore` as a buffer-local setting object (`=`, `%set_local`, `%get_current`)

## Deferred

`comint-dynamic-list-input-ring` (C-c C-l) and `comint-dynamic-list-completions` (need `choose-completion-string`),
the status of a stopped process,
redirection,
fontification of input,
mouse,
menus,
text property hooks (`insert-in-front-hooks`),
`window-point-insertion-type`

## Tests

test_command_interpreter_mod (ports comint-tests.el: comint-test-password-regexp, comint-test-no-password-function, comint-test-password-function-with-value, comint-test-password-function-with-nil; own tests)
