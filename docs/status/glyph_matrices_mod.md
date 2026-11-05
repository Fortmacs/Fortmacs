# glyph_matrices_mod

- Module: glyph_matrices_mod
- Upstream: dispextern.h
- State: partial

## Ported

`glyph_type` (character, face id, padding, `type` CHARACTER_GLYPH/STRETCH_GLYPH, `pixel_width`, `ascent`, `descent`),
`glyph_row_type` (`x`, `y`, `pixel_width`, `ascent`, `height`, `visible_height`, `continued_p`, `ends_at_end_of_accessible_portion_p`),
`glyph_matrix_type`,
`GLYPH_EQUAL_P` (with type and pixel width),
`face_type` (terminal attributes and colors, the font id),
face color constants,
basic face ids (`enum face_id`),
`enum text_cursor_kinds`,
`TTY_CAP_*`,
the realized face's `lface`,
`face_tty_specified_color`,
`struct redisplay_interface` as the abstract `redisplay_interface_type` (clear_frame, write_glyphs, clear_end_of_line, draw_vertical_window_border, draw_window_cursor, flush_display; frame pixel coordinates)

## Deferred

the other glyph types and fields (images, compositions, glyphless, slices, boxes, voffset, bidi),
phys_ascent/phys_height,
the other redisplay interface hooks (fringes, glyph strings, overhangs, scroll runs, mouse face, hourglass)

## Tests

test_terminal_output_mod,
test_fake_graphical_user_interface_mod
