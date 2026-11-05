# buffer_local_setting_mod

- Module: buffer_local_setting_mod
- Upstream: — (the buffer-local values of data.c's SYMBOL_LOCALIZED, for settings that are not forwarded)
- State: done

## Ported

`buffer_local_setting_type`,
the abstract parent of the buffer-local setting objects (the serial numbers of the buffers with their own value, entries of closed buffers reused),
`get_local_index`,
`make_local_entry`,
`is_local`,
`clear_local`,
the deferred `append_local_entry` and `clear_local_entry` each extension binds beside `set_local` and `get_current`

## Deferred

—

## Tests

test_buffer_local_setting_mod (own: `set_local`, `get_current`, `is_local` and `clear_local` in two buffers, a local null value, the reuse of a closed buffer's entry, an array setting)
