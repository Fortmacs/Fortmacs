# sha512_mod

- Module: sha512_mod
- Upstream: Gnulib lib/sha512.c (+ lib/u64.h `u64plus`)
- State: done

## Ported

`sha512_context_type` (SHA-384 as `sha512_context_type(state=SHA384_INITIAL_STATE, digest_size=SHA384_DIGEST_SIZE)`),
`sha512_process_block`,
`compute_sha384`,
`compute_sha512`;
64-bit sums modulo 2**64 by `add_unsigned` (two 32-bit halves, no signed overflow)

## Deferred

lengths beyond int64 (the 128-bit count)

## Tests

test_sha512_mod (FIPS 180 known answers, pieces)
