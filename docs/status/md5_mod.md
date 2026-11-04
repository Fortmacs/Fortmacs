# md5_mod

- Module: md5_mod
- Upstream: Gnulib lib/md5.c
- State: done

## Ported

`md5_context_type` (md5_init_ctx as default initialization; `%process_bytes`, `%finish`: md5_process_bytes, md5_finish_ctx, md5_read_ctx),
`md5_process_block`,
`compute_md5` (md5_buffer);
32-bit words in int64 masked to 32 bits,
`ishftc` rotations

## Deferred

lib/md5-stream.c's md5_stream (files; fns.c does not use it)

## Tests

test_md5_mod (RFC 1321 known answers, a million "a"s, pieces)
