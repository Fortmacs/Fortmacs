# completing_read_multiple_mod

- Module: completing_read_multiple_mod
- Upstream: emacs-lisp/crm.el
- State: done

## Ported

`completing-read-multiple` (the elements split at `crm-separator`, the prompt with `crm-prompt` as `completing_read_multiple_prompt%format`, a Fortran format, and `%items`, the description, separator and original prompt it writes in order (owner, 2026-11-06), completion of the element at point through `crm--collection-fn` and `completion-table-with-context`),
`crm-complete`,
`crm-complete-word`,
`crm-completion-help`,
`crm-complete-and-exit`,
`crm-local-completion-map` and `crm-local-must-match-map` (remapping the minibuffer completion commands)

## Deferred

`crm-separator`'s `:separator` property (the description and the inserted separator are the variables `completing_read_multiple_separator_description` and `completing_read_multiple_separator_string`)

## Tests

test_ibuffer_extensions_mod (own: crm.el has no tests)
