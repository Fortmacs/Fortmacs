# simple_shell_mod

- Module: simple_shell_mod (submodule of simple_mod)
- Upstream: simple.el (shell commands; + subr.el `call-process-shell-command`, `call-shell-region`; window.el `count-screen-lines`; buffer.c `set-buffer-major-mode`)
- State: partial

## Ported

`shell-command` (M-!: the prompt with `shell-command-prompt-show-cwd`, the visited file as default, prefix argument inserting at point with the mark after, output to another buffer by handle or name),
`shell-command-on-region` (M-|; C-u M-| replacing the region with point and mark around it, NO_MARK, output buffer reuse of the current buffer, the exit messages "(Shell command succeeded with no output)", "(Shell command failed with code N and no output)", "(Shell command killed by signal S)", "(Shell command failed with error)", "some error output to the \"B\" buffer"),
`shell-command-default-error-buffer` and error buffers (\f separators, DISPLAY-ERROR-BUFFER),
`shell-command-dont-erase-buffer` with `shell-command-save-pos-or-erase` and `shell-command-set-point-after-cmd`,
`display-message-or-buffer` (a buffer),
`read-shell-command` with `minibuffer-local-shell-command-map` and `shell-command-history`,
`shell-command-switch`,
the buffer names;
asynchronous shell commands (a trailing `&`, `async-shell-command` M-&, "*Async Shell Command*" in `shell-command-mode`, `async-shell-command-buffer`, `async-shell-command-display-buffer`, `async-shell-command-width`, `shell-command-sentinel`);
files.el `file-relative-name` (in files_read_mod),
INITIAL of the form (STRING . POSITION) in `read-shell-command`

## Deferred

shell completion in the minibuffer,
the dired default,
noncontiguous regions,
coding systems,
remote directories,
`mode-line-process` and `revert-buffer-function` of the output buffer,
a resizable echo area (output longer than one screen line goes to the buffer),
the save-window-excursion display in set-point-after-cmd,
default major modes other than Fundamental

## Tests

test_simple_shell_mod (ports simple-tests-shell-command-39067, simple-tests-shell-command-dont-erase-buffer; simple-tests-async-shell-command-30280; own: messages, echo area or buffer, insertion at point, C-u M-|, error buffers, asynchronous commands, M-! and C-u M-| through the command loop)
