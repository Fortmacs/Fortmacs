# hooks_mod

- Module: hooks_mod
- Upstream: eval.c (hooks), subr.el (`add-hook`, `remove-hook`)
- State: partial

## Ported

`run-hooks` and `run_hook` (a `t` in a local value runs the default value),
calling hook functions (`defcommand` procedures) with no arguments,
`run-hook-with-args` for the change hooks;
three kinds of hook sharing `hook_base_type` (functions, buffer-local values, depths, `add-hook`/`remove-hook` including the depth info removal and the "make-local-variable was used on a hook" case, `empty`, `clear`): `hook_type` (normal),
`change_hook_type` (`change_procedure`: BEGINNING, END) `after_change_hook_type` (`after_change_procedure`: BEGINNING, END, OLD-LEN),
`position_hook_type` (`position_procedure`: POS; safe runs that ignore errors, as redisplay's) `region_hook_type` (`region_procedure`: BEGINNING, END, giving the bounds done; listed for `run-hook-wrapped`) `capf_hook_type` (`capf_procedure`: the completion data at point, for `completion-at-point-functions`) and `until_success_hook_type` (`until_success_procedure`: run until one succeeds, `run-hook-with-args-until-success`, for `write-file-functions`),
`window_position_hook_type` (`window_position_procedure`: WINDOW, POS, `run_hook_with_args_2`, optionally safe as keyboard.c's safe_execute_hooks_2, for `window-scroll-functions`);
property functions: `property_function_type` names a procedure for the symbols that text and overlay properties hold as functions (`modification-hooks`, `insert-in-front-hooks`, `insert-behind-hooks`, `point-entered`, `point-left`),
called with `execute_property_function` and `execute_overlay_change_function` (eval.c `Ffuncall`);
`buffer-list-update-hook` and `change-major-mode-hook` (buffer.c);
`hook_type`'s `execute_local` and `execute_default` (only a buffer's own functions, or only the default ones, as window.c's run_funs runs them);
`string_result_hook_type` (run-hook-with-args-until-success for functions giving a string, `which-func-functions`) (step 49c)

## Deferred

`run-hook-with-args-until-failure`,
the evaluator

## Tests

test_derived_mod (ports subr-tests-add-hook-depth),
test_insert_delete_mod
