# sha3_mod

- Module: sha3_mod
- Upstream: Gnulib lib/sha3.c
- State: done

## Ported

`sha3_context_type(digest_size=...)` (sha3_224_init_ctx … sha3_512_init_ctx),
`sha3_process_bytes`,
`sha3_finish_ctx`,
`sha3_process_block` (Keccak-f[1600], Rho and Pi from tables),
`compute_sha3` (sha3_224_buffer … sha3_512_buffer)

## Deferred

sha3_free_ctx (nothing to free),
the OpenSSL variant

## Tests

test_sha3_mod (FIPS 202 known answers, pieces)
