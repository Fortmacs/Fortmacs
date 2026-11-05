# face_realization_mod

- Module: face_realization_mod
- Upstream: xfaces.c
- State: partial

## Ported

overlays' `face` properties merged by priority with the region in `face_at_buffer_position`,
the face cache (`realize_basic_faces` with every basic face id, `realize_default_face`, `realize_named_face`, `realize_tty_face`, `map_tty_color`, `tty_defined_color`, `tty_lookup_color`, `color_distance`),
`merge_face_attributes` (inheritance, relative heights),
`merge_named_face` and `merge_face_ref` (face names, attribute property lists with Fortran keywords, lists; the `:extend` filter with `face_inherited_attr`),
`lookup_face`,
`lookup_named_face`,
`merge_faces`,
`face_at_buffer_position` (text property and the region),
`is_tty_supports_face_attributes` (display-supports-face-attributes-p),
`clear-face-cache`,
`tty-suppress-bold-inverse-default-colors`;
frame.el `frame-set-background-mode` for terminals (light for xterm-like terminals),
overlays' `window` property,
`face_at_string_position` (an overlay string's `face`),
faces.el's `face-differs-from-default-p`

## Deferred

overlays' `window` property,
`face-remapping-alist`,
mouse faces,
`:filter` face references,
(FOREGROUND-COLOR . COLOR) references,
the `reset` value,
float and functional heights in references,
fonts and window-system faces,
the background color query (xterm.el)

## Tests

test_face_realization_mod (own; xfaces-tests.el tests graphical colors and fonts),
test_display_engine_mod
