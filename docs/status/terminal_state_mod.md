# terminal_state_mod

- Module: terminal_state_mod
- Upstream: termchar.h, cm.h (`struct cm`)
- State: partial

## Ported

`terminal_display_info_type` (capabilities, modes, cursor-motion state),
the output buffer (`terminal_output`, `terminal_flush`)

## Deferred

termscript,
mouse highlight,
terminal parameters

## Tests

test_terminal_output_mod
