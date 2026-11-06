# execute_interactively_mod

- Module: execute_interactively_mod
- Upstream: callint.c (+ eval.c `is_command`)
- State: done

## Ported

`this-command` (here for `call-interactively`, re-exported by keyboard_mod),
`current-minibuffer-command` (bound by `call-interactively`),
`mark-even-if-inactive`,
command registry (Fortran procedures under symbols, standing in for function cells),
`call-interactively` (generic: a command object, or a key's binding resolved with `command_of`),
`is_command`,
`prefix-numeric-value`,
`current-prefix-arg`,
`prefix-arg` (`type(prefix_argument_type)`, A7; `this-command` and `current-minibuffer-command` are `type(command_type)` pointers);
callint.c `check_mark` (the `r` code gives point and the mark as they are, after its checks)

## Deferred

`command-history` and `repeat-complex-command` (`fix_command`, `quotify_arg`, `quotify_args`: the arguments commands read are not recorded as forms yet);
Not applicable: interactive specs (`Finteractive`, `read_file_name`: commands read their own arguments in their `cmd_` procedures),
`Ffuncall_interactively` (a command run through its `cmd_` procedure is the interactive call; there is no Lisp backtrace for called-interactively-p),
`syms_of_callint` (symbol definitions);
waits for the GUI: nothing

## Tests

test_keyboard_mod,
test_editing_functions_mod (own: the `r` code);
callint-tests.el needs interactive specs and command-history
