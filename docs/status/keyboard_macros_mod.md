# keyboard_macros_mod

- Module: keyboard_macros_mod
- Upstream: kmacro.el
- State: partial

## Ported

macros as `keyboard_macro_type` (keys, counter, format; upstream's `kmacro` oclosure) run with their own counter (`keyboard_macro`, funcall on one),
the counter (`kmacro-counter`, `kmacro-insert-counter` C-x C-k TAB, `kmacro-set-counter` C-x C-k C-c, `kmacro-add-counter` C-x C-k C-a, `kmacro-set-format` C-x C-k C-f (the counter formats are Fortran formats, "(i0)" by default, written with editing_functions_mod's `write_with_format`; panel, 2026-11-06), `kmacro-display-counter`, the initial value, value at start and last counter, `kmacro-loop-setup-function`),
the register counters (`kmacro-reg-load-counter`, `kmacro-reg-save-counter`, `kmacro-reg-add-counter-equal`/`-less`/`-greater` on C-x C-k C-r),
`kmacro-quit-counter-equal`/`-less`/`-greater` (C-x C-k C-q),
the ring (`kmacro-ring`, `kmacro-ring-max`, `kmacro-ring-head`, `kmacro-push-ring`, `kmacro-pop-ring(1)`, `kmacro-split-ring-element`, `kmacro-ring-empty-p`, `kmacro-display`, `kmacro-call-ring-2nd(-repeat)` C-x C-k C-l, `kmacro-view-ring-2nd`, `kmacro-cycle-ring-next`/`-previous` C-x C-k C-n/C-p, `kmacro-swap-ring` C-x C-k C-t, `kmacro-delete-ring-head` C-x C-k C-d, `kmacro-view-macro(-repeat)` C-x C-k C-v),
repeating on the last key (`kmacro-repeat-on-last-key`, `kmacro-get-repeat-prefix`, `kmacro-repeat-no-prefix`, the `kmacro-repeat` properties),
`kmacro-start-macro` (C-x (, C-x C-k s, C-x C-k C-s; `kmacro-execute-before-append`), `kmacro-end-macro` (C-x ), "Ignore empty macro"),
`kmacro-call-macro` (the repeat key in a transient map, its hint; `kmacro-call-repeat-key`, `kmacro-call-repeat-with-arg`),
`kmacro-start-macro-or-insert-counter` (<f3>),
`kmacro-end-or-call-macro` (<f4>),
`kmacro-end-or-call-macro-repeat` (C-x C-k C-k),
`kmacro-end-and-call-macro` (C-x e),
`kmacro-redisplay` (C-x C-k d),
`kmacro-keyboard-quit`,
`kmacro-keymap`

## Deferred

editing and step editing (C-x C-k C-e, RET, e, l, SPC),
`kmacro-name-last-macro`,
`kmacro-bind-to-key`,
`kmacro-to-register` and macros as register values,
`apply-macro-to-region-lines` and `kbd-macro-query` (macros.el),
the Keyboard Macro Menu,
mouse events (`kmacro-call-mouse-event`),
a key symbol as `kmacro-call-repeat-key`,
repeat-mode,
the obsolete functions and list form;
the repeat key runs a named command,
`keyboard_macro_execute_macro_repeat` (upstream an anonymous one)

## Tests

test_keyboard_macros_mod (ports kmacro-tests: test-insert-counter-01 to -04, test-start-format-counter, test-start-macro-when-defining-macro, set-macro-counter-while-defining, start-insert-counter-appends-to-macro, test-reg-load, test-reg-save, test-reg-add-counter-equal-01 to -04, -less, -greater, test-quit-counter-equal-01 to -04, -less, -greater, end-call-macro-prefix-args, end-and-call-macro, call-macro-hint-and-repeat, test-ring-2nd-commands, fill-ring-and-rotate, test-ring-commands-when-no-macros, repeat-view-and-run; own: recording, prefix arguments, C-g and errors ending a definition, F3/F4, the termination hook, the mode line, format-kbd-macro, the keys)
