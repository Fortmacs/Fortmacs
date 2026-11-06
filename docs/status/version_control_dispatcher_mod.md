# version_control_dispatcher_mod

- Module: version_control_dispatcher_mod
- Upstream: vc/vc-dispatcher.el
- State: partial

## Ported

`vc-do-command` (OKSTATUS nil/t/async/0, BUFFER, the command messages, `vc-command-messages`, `vc-display-failed-async-commands`),
`vc-process-filter`,
`vc-setup-buffer`,
`vc-exec-after` and `vc-process-sentinel` (queued actions per process),
`vc-delistify`,
`vc-parent-buffer` and its mode-line name,
`vc-resynch-window`,
`vc-resynch-buffer`,
`vc-resynch-buffers-in-directory`,
`vc-buffer-sync`,
`vc-revert-buffer-internal`,
`vc-suppress-confirm`,
`vc-allow-async-diff`/`-revert`,
the log entry: `vc-start-logentry`,
`vc-log-edit`,
`vc-finish-logentry`,
`vc-log-fileset`,
`vc-log-operation`,
`vc-logentry-check-hook`,
`vc-delete-logbuf-window`;
`vc-do-async-command` ("Running '...'...", "Finished in N seconds", `vc-display-failed-async-commands`; EDIT for `vc-filter-command-function` bound to `vc-user-edit-command`),
`vc-user-edit-command`,
`version_control--display-async-command-buffer`,
`vc-compilation-mode` (VC-Compilation, derived from Compilation mode, with the backend's error regexps as an argument) and `vc-compilation-mode-hook`,
`vc-set-async-update` (VC-Dir, Dired and file buffers)

## Deferred

`vc-filter-command-function` as an option,
`vc-edit-next-command` (C-x v !),
`vc-post-command-functions`,
`vc-tor`,
separate standard error of asynchronous commands,
`version_control--inhibit-async-window`,
View mode after `vc-resynch-window`

## Tests

test_version_control_mod (ports vc-test-misc.el vc-test-do-command-1, -2, -3, -5, -6, -7 and vc-test-exec-after-1, -2, -3)
