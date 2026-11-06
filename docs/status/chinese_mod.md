# chinese_mod

- Module: chinese_mod, cyrillic_mod, english_mod, european_mod, romanian_mod, greek_mod, hebrew_mod, japanese_mod, korean_mod, lao_mod, thai_mod, tibetan_mod, vietnamese_mod, miscellaneous_language_mod, georgian_mod (submodules of coding_mod)
- Upstream: language/*.el
- State: partial

## Ported

the coding systems of type `character_set` (80),
`shift-jis` (2),
`big5` (1) and `iso-2022` (13),
with their aliases,
defined in loadup.el's order (`initialize_chinese`, ...);
their language environments (`define_chinese_language_environments`, ...)

## Deferred

eucjp-ms and in-is13194-devanagari (indian.el),
the other coding systems (CCL, UTF-7),
input methods (41i)

## Tests

test_coding_mod
