# ibuf_macs_mod

- Module: ibuf_macs_mod
- Upstream: ibuf-macs.el
- State: partial

## Ported

the records `define-ibuffer-column` (`ibuffer_column_type`),
`define-ibuffer-sorter` (`ibuffer_sorter_type`) and `define-ibuffer-op` (`ibuffer_op_type`) define;
`ibuffer-formats` entries (`ibuffer_column`, `ibuffer_text`, `ibuffer_formats%append`),
`define-ibuffer-filter`'s records (`ibuffer_filter_def_type`),
the filter specifications (`ibuffer_filter_type`: `ibuffer_and`, `ibuffer_or`, `ibuffer_not`, `ibuffer_saved`, `ibuffer_filter` and a constructor per filter),
the saved filters,
filter groups and filter formats

## Deferred

`ibuffer-aif`,
`ibuffer-awhen` (Lisp control macros)

## Tests

test_ibuffer_mod
