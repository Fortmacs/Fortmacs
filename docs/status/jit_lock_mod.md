# jit_lock_mod

- Module: jit_lock_mod
- Upstream: jit-lock.el
- State: partial

## Ported

`jit-lock-mode` (buffer-local),
`jit-lock-chunk-size`,
`jit-lock-functions` (with bounds),
`jit-lock-register`,
`jit-lock-unregister`,
`jit-lock-refontify`,
`jit-lock-function` (on `fontification-functions`),
`jit-lock--run-functions`,
`jit-lock-fontify-now`,
`jit-lock-after-change` with `jit-lock-after-change-extend-region-functions`

## Deferred

stealth,
deferred and contextual fontification (idle timers),
the antiblink grace,
`jit-lock-force-redisplay` (the next redisplay shows earlier changes),
`jit-lock-debug-mode`,
indirect buffers

## Tests

test_font_lock_mod (own)
