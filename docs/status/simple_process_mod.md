# simple_process_mod

- Module: simple_process_mod (submodule of simple_mod)
- Upstream: simple.el (the process menu)
- State: partial

## Ported

`list-processes` (QUERY-ONLY, BUFFER; *Process List*),
`process-menu-mode` (from Tabulated List mode: Process, PID, Status, Buffer, TTY, Thread, Command),
`process-menu-delete-process` (d),
`list-processes--refresh` (exited and signaled processes deleted),
`process-menu-query-only`

## Deferred

the Buffer column's button (`process-menu-visit-buffer`, help-echo; tabulated lists have no buttons),
threads (Thread shows "--"),
network,
serial and pipe connections in the Command column

## Tests

test_process_mod (own: list-processes)
