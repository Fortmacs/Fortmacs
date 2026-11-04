# process_mod

- Module: process_mod
- Upstream: process.c (+ subr.el `start-process`, `process-live-p`; gnulib nproc.c)
- State: partial

## Ported

process objects as registry handles with generations (`destroy_process`; not garbage collected),
`make-process` (:name, :buffer, :command, :noquery, :connection-type pipe or pty, :filter, :sentinel; plus `environment` entries),
`start-process`,
`is_process`,
`process-live-p`,
`get-process`,
`get-buffer-process`,
`process-list`,
`process-status` (run, exit, signal),
`process-exit-status`,
`process-id`,
`process-name`,
`process-command`,
`process-tty-name`,
`set-process-window-size` (TOLD as an `intent(out)` logical),
`process-buffer`,
`set-process-buffer`,
`process-mark`,
`set-process-filter`,
`set-process-sentinel`,
`set-process-query-on-exit-flag`,
`process-query-on-exit-flag`,
`status_message` ("finished\n", "exited abnormally with code N\n", signal descriptions downcased, core dumps),
`status_notify` and `exec_sentinel` (errors as "error in process sentinel: "),
`read_process_output` (decoded by the process's coding system, with the carryover of incomplete UTF-8 sequences, odd UTF-16 bytes and DOS CRs; a detected coding system becomes the process's; errors as "error in process filter: "),
`set-process-coding-system`,
`process-coding-system` (:coding, else `coding-system-for-read`/`-write`, else `default-process-coding-system`),
`internal-default-process-filter` (insert before markers at the process mark, read-only and narrowing as upstream),
`internal-default-process-sentinel` ("\nProcess NAME MSG"),
`delete-process` (M-x: "Deleted process"; SIGKILL to the group, "killed\n"),
`interrupt-process`,
`stop-process`,
`continue-process`,
`kill-process`,
`quit-process`,
`signal-process` (numbers),
`kill_buffer_processes` (SIGHUP on kill-buffer and on exit),
`process-send-string`,
`process-send-region`,
`process-send-eof` (closing the pipe, or ^D on a pty),
`accept-process-output` (PROCESS, SECONDS),
`num-processors`,
`process-running-child-p` (`process_running_child`: the foreground process group, 0 when the process has its terminal, `PROCESS_RUNNING_CHILD_UNKNOWN` when the system cannot tell; R6-6),
`process-connection-type`,
`delete-exited-processes`;
the main thread takes the workers' messages while waiting for input (keyboard_mod `process_output_hook`),
in `sit-for` and in `accept-process-output`

## Deferred

network,
serial and pipe processes,
:stderr,
(INPUT . OUTPUT) connection types,
`process-coding-system-alist`,
`exec-path`,
remote directories,
stopped processes (`stop-process`, `continue-process`),
a filter of t,
a CURRENT-GROUP of `lambda`,
signal names,
`interrupt-process-functions`,
the process plist,
thread locking,
`window-adjust-process-window-size` (window.el, with the window change hooks),
`signal-process` (`signal-process-functions`),
adaptive read buffering,
`inhibit-sentinels`,
saving the match data around filters,
quitting in `accept-process-output`,
JUST-THIS-ONE,
`list-processes`,
`read-process-name`,
`process-kill-buffer-query-function` and the query about active processes on exit;
exec failures of pty processes are reported as errors

## Tests

test_process_mod (set-process-window-size; ports process-tests: process-test-sentinel-accept-process-output, -sit-for, start-process-should-not-modify-arguments, process-tests/sentinel-called and /sentinel-with-multiple-processes (pipe and pty), make-process/connection-type/pty and /pipe; own: process-running-child, default filter and sentinel, exit codes, kill, delete, output handled on the main thread only, UTF-8 split across reads, input and EOF, unique names, kill-buffer, missing programs)
