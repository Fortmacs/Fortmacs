# character_mod

- Module: character_mod
- Upstream: character.c, character.h
- State: done

## Ported

encoding table,
`character_string`,
`multibyte_length`,
`string_character_and_length`,
`raw_previous_character_length`,
raw-byte (eight-bit) conversions,
`multibyte_characters_in_text`,
`parse_string_as_multibyte`,
`string_as_multibyte`,
`count_size_as_multibyte`,
`string_to_multibyte`,
`string_as_unibyte`,
`character_resolve_modifier_mask`,
small tests,
`is_alphabetic`,
`is_alphanumeric`,
`is_graphic`,
`is_printable`,
`is_blank` (general categories from unicode_category_mod),
`glyph_width` (an approximation of `char_width`, moved from display_engine_mod for editing_functions_mod's `format`; display_engine_mod re-exports it)

## Deferred

`characters_in_text` (needs current buffer → buffer_mod);
`char_width`/`string-width` (display phase);
`translate_char`;
Lisp-level DEFUNs

## Tests

test_character_mod (own; character-tests.el only tests `string-width`, deferred)
