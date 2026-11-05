# editing_commands_mod

- Module: editing_commands_mod
- Upstream: cmds.c
- State: partial

## Ported

`undo-auto-amalgamate` in `self-insert-command` and `delete-char`,
`forward-char`,
`backward-char` (`move_point`),
`forward-line` (incl. excessive counts),
`beginning-of-line`,
`end-of-line`,
`delete-char`,
`self-insert-command` (expanding an abbrev before a non-word character in Abbrev mode (`expand-abbrev`, with a `no-self-insert` hook stopping the insertion), insertion and auto-fill after a space or newline, through `internal-auto-fill`),
`post-self-insert-hook` (run at the end of `self-insert-command`, which sets `last-command-event`),
command registration (`syms_of_cmds`)

## Deferred

overwrite mode,
`translation-table-for-input`,
intangible text;
Not applicable: the scanner's doc-line artifact `Copyright`;
waits for the GUI: nothing

## Tests

test_editing_commands_mod (ports forward-line-with-bignum from cmds-tests.el),
test_bindings_mod
