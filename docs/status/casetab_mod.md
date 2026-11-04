# casetab_mod

- Module: casetab_mod, uni_case_mod (generated)
- Upstream: casetab.c, buffer.h, characters.el (case pairs), admin/unidata/UnicodeData.txt
- State: done

## Ported

case tables as char-tables (chartab_mod) of subtype case_table with the UPCASE,
CANONICALIZE and EQUIVALENCES extra slots: `case-table-p`,
`check_case_table`,
`current-case-table`,
`standard-case-table`,
`set-case-table`,
`set-standard-case-table` (`set_case_table` filling empty slots with `set_identity`, `shuffle` and `set_canon`; buffers on the old standard table keep it),
per-buffer tables (BVAR `downcase_table`, `upcase_table`, `case_canon_table`, `case_eqv_table`, reset by `kill-all-local-variables`),
`downcase`,
`upcase`,
`is_uppercase`,
`is_lowercase`,
the canonicalize lookup (`char_table_translate`);
the standard table from `init_casetab_once` (ASCII) and characters.el's Unicode entries,
identity entries included (`tools/gen-unicase.sh`),
its canonicalize and equivalence tables computed by `set_case_table`

## Deferred

characters.el's `set-case-syntax-pair` and friends (case-table.el);
Not applicable: `syms_of_casetab` (symbol definitions);
waits for the GUI: nothing

## Tests

test_casetab_mod (ports casefiddle-tests-case-table, the upcase/downcase part of casefiddle-tests-char-casing; own: the computed canonicalize table against the generated one, `case-table-p`, buffer-local tables)
