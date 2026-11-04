# marker_mod

- Module: marker_mod
- Upstream: marker.c (+ alloc.c `Fmake_marker`)
- State: done

## Ported

conversion with cache and marker hints,
`make-marker`,
`marker-buffer`,
`marker-position` (`Fmarker_position`),
`marker_byte_position`,
`marker-last-position`,
`set-marker` (integer and marker positions),
`set_marker_restricted(_both)`,
`set_marker_both`,
`detach_marker`,
`unchain_marker`,
`copy-marker`,
insertion types,
`count_markers`,
CHECK_MARKER (`resolve_marker`);
`destroy_marker` (ours: no GC)

## Deferred

`build_marker` after long scans (would leak without GC),
`need_adjustment`;
Not applicable: the debugging checks `verify_bytepos` and `byte_char_debug_check`,
`syms_of_marker` (symbol definitions);
waits for the GUI: nothing

## Tests

test_marker_mod (marker-tests.el needs windows and *Help*; its Bug#24368 point is ported as set-marker-from-other-buffer)
