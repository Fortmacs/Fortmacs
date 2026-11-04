# casefiddle_mod

- Module: casefiddle_mod
- Upstream: casefiddle.c
- State: done

## Ported

`upcase`,
`downcase`,
`capitalize`,
`upcase-initials` on characters and strings (`casify_object`; in a unibyte buffer 128..255 are raw bytes: `make_char_unibyte`),
`upcase-region`,
`downcase-region`,
`capitalize-region`,
`upcase-initials-region` with REGION-NONCONTIGUOUS-P (`casify_pnc_region` over `region-bounds`),
unibyte buffers cased byte by byte (`do_casify_unibyte_region`),
`upcase-word` (M-u),
`downcase-word` (M-l),
`capitalize-word` (M-c),
C-x C-u and C-x C-l (disabled, as upstream),
title case and the unconditional special casings (ß → SS, ﬁ → FI),
the Greek final sigma rule,
`case-symbols-as-words`,
the change hooks of region changes

## Deferred

compositions;
Not applicable: unibyte strings (`do_casify_unibyte_string`, `ascii_casify_character`: strings are always multibyte here),
the extractor's `Copyright` line;
waits for the GUI: nothing

## Tests

test_casefiddle_mod (ports casefiddle-tests-casing-character, -casing-word, -casing, -casing-byte8, -char-casing, casefiddle-case-symbols-as-words; own: noncontiguous regions, unibyte buffers; -casing-byte8-with-changes and casefiddle-turkish need case-table.el, casefiddle-invalid-region-extract-function a region-extract-function returning non-lists)
