# sha256_mod

- Module: sha256_mod
- Upstream: Gnulib lib/sha256.c
- State: done

## Ported

`sha256_context_type` (SHA-224 as `sha256_context_type(state=SHA224_INITIAL_STATE, digest_size=SHA224_DIGEST_SIZE)`),
`sha256_process_block`,
`compute_sha224`,
`compute_sha256`

## Deferred

—

## Tests

test_sha256_mod (FIPS 180 known answers, pieces)
