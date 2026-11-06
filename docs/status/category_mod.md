# category_mod

- Module: category_mod
- Upstream: category.c, category.h
- State: done

## Ported

category sets (`category_set_type`; `make-category-set`, `category-set-mnemonics`, `set_category_set`, `CATEGORY_MEMBER`),
category tables as char-tables of subtype category_table whose values number the table's distinct sets (`hash_get_category_set`),
with the category docstrings: `define-category`,
`category-docstring`,
`get-unused-category`,
`category-table-p`,
`check_category_table`,
`category-table`,
`standard-category-table` (`initialize_category_once`, then characters_mod's categories),
`copy-category-table` (`copy_category_table`, `copy_category_entry`),
`make-category-table`,
`set-category-table` (per buffer: BVAR `category_table`, `bset_category_table`, reset by `kill-all-local-variables`),
`char-category-set` (`character_category_set`),
`modify-category-entry`,
`CHARACTER_HAS_CATEGORY` (used by regexps, filling, indentation and word boundaries);
`is_word_boundary` is in syntax_mod

## Deferred

the categories of characters.el that are not ported (see characters_mod);
Not applicable: `syms_of_category` (symbol definitions),
the extractor's `Copyright` and `word` lines;
waits for the GUI: nothing

## Tests

test_category_mod (own: there is no category-tests.el)
