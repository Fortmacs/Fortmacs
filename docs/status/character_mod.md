# character_mod

- Module: character_mod
- Upstream: character.c, character.h
- State: done

## Ported

encoding table,
`char_string`,
`multibyte_length`,
`string_char_and_length`,
`raw_prev_char_len`,
raw-byte (eight-bit) conversions,
`multibyte_chars_in_text`,
`parse_str_as_multibyte`,
`str_as_multibyte`,
`count_size_as_multibyte`,
`str_to_multibyte`,
`str_as_unibyte`,
`char_resolve_modifier_mask`,
small tests,
`is_alphabetic`,
`is_alphanumeric`,
`is_graphic`,
`is_printable`,
`is_blank` (general categories from uni_category_mod),
`glyph_width` (an approximation of `char_width`, moved from xdisp_mod for editfns_mod's `format`; xdisp_mod re-exports it)

## Deferred

`chars_in_text` (needs current buffer → buffer_mod);
`char_width`/`string-width` (display phase);
`translate_char`;
Lisp-level DEFUNs

## Tests

test_character_mod (own; character-tests.el only tests `string-width`, deferred)
