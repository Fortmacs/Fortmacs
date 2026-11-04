# chartab_mod

- Module: chartab_mod
- Upstream: chartab.c
- State: done

## Ported

char-tables as registry objects with integer values (`NO_VALUE` for nil) and handles for parents and table-valued extra slots: the four-level trie with its sub char-tables in a pool (`make_sub_char_table`, `copy_sub_char_table`, released to a free list instead of GC),
`make-char-table` (extra slots from the purpose: case_table 3, category_table 2, or given),
`char-table-subtype`,
`char-table-parent`,
`set-char-table-parent` (`set_char_table_parent`, loops refused),
`char-table-extra-slot`,
`set-char-table-extra-slot`,
`char-table-range`,
`set-char-table-range` (t, nil, a character or a range),
`optimize-char-table` (`optimize_sub_char_table`; values compare by ==),
`map-char-table` (`map_char_table`, `map_sub_char_table`, runs of equal values with the parents consulted),
`char_table_ref`,
`sub_char_table_ref`,
`char_table_ref_and_range`,
`sub_char_table_ref_and_range`,
`char_table_ref_simple`,
`char_table_set`,
`sub_char_table_set`,
`char_table_set_range`,
`sub_char_table_set_range`,
`char_table_ascii`,
`set_char_table_ascii`,
`copy_char_table`,
`CHECK_CHAR_TABLE`

## Deferred

`unicode-property-table-internal`,
`get-unicode-property-internal`,
`put-unicode-property-internal`,
`get_unicode_property` (the properties are generated Fortran tables: uni_case_mod, uni_category_mod, charscript_mod; nothing needs them as char-tables yet),
`map_char_table_for_charset`,
`map_sub_char_table_for_charset` (charsets with mapping tables, step 41);
Not applicable: the compressed form of the uni-*.el tables and their loading (`uniprop_table`, `uniprop_table_uncompress`, `uniprop_decode_value_run_length`, `uniprop_encode_value_character`, `uniprop_encode_value_run_length`, `uniprop_encode_value_numeric`, `uniprop_get_decoder`, `uniprop_get_encoder`),
`syms_of_chartab` (symbol definitions);
waits for the GUI: nothing

## Tests

test_chartab_mod (ports chartab-tests.el; own: inheritance, ranges, mapping, optimizing and copying)
