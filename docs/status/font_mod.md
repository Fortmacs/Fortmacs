# font_mod

- Module: font_mod
- Upstream: font.h
- State: partial

## Ported

`font_metrics_type` (struct font_metrics),
`font_type` (the metrics of struct font: sizes, ascent, descent, space and average widths, baseline offset),
`font_driver_type` (the abstract font driver: `load_for_lface` (font.c font_load_for_lface folded in), `font`, `text_extents` with encode_char's missing-glyph case)

## Deferred

font specs and entities,
font Lisp functions,
fontsets,
real backends (FreeType/HarfBuzz with the GUI),
vertical centering,
underline metrics

## Tests

test_fake_gui_mod
