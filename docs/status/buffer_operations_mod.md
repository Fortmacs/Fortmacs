# buffer_operations_mod

- Module: buffer_operations_mod (submodule of buffer_mod)
- Upstream: buffer.c
- State: done

## Ported

`kill-buffer` (unchains markers, clears the position cache, runs `buffer-list-update-hook`),
`erase-buffer` (with inline `widen`),
`find-buffer`,
`execute_buffer_list_update_hook`,
`buffer-enable-undo` as a command,
`syms_of_buffer` (its commands)

## Deferred

hooks other than `buffer-list-update-hook` and `kill-buffer-hook`,
windows,
processes,
labeled restrictions

## Tests

test_buffer_mod,
test_marker_mod
