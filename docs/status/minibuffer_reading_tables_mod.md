# minibuffer_reading_tables_mod

- Module: minibuffer_reading_tables_mod
- Upstream: minibuf.c, minibuffer.el (split from minibuffer_reading_mod so that hooks_mod's hooks can hold completion-at-point functions)
- State: done

## Ported

completion tables (`completion_table_type`: lists, the obarray, functions; `completion-boundaries`' actions; the table's category),
`capf_data_type` (START, END, COLLECTION, `:test`, `:exclusive`)

## Deferred

the other completion-at-point properties

## Tests

(through test_minibuffer_reading_mod, test_minibuffer_mod)
