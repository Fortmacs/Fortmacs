# simple_goto_mod

- Module: simple_goto_mod (submodule of simple_mod), thing_at_point_mod
- Upstream: simple.el (going to lines, counting words; + editfns.c `goto-char`, `narrow-to-region` and `widen`, indent.c `move-to-column`, subr.el `goto-char--read-natnum-interactive` as commands, bindings.el `narrow-map`), thingatpt.el
- State: partial

## Ported

`goto-line` (M-g g, M-g M-g; C-u for the other buffer, `widen-automatically`),
`goto-line-relative` (C-x n g),
`goto-line-read-args` (the number at point as default, `goto-line-history`),
`goto-char` (M-g c),
`move-to-column` (M-g TAB),
`narrow-to-region` (C-x n n, disabled),
`widen` (C-x n w),
`count-words` (with `count-words--format`, `count-words--buffer-format`),
`count-words-region` (M-=);
basic_subprograms_read_mod's `read-number` with a nil first default;
thingatpt.el's `number-at-point` (decimal and hexadecimal),
`thing-at-point-looking-at`,
`thing-at-point-decimal-regexp`,
`thing-at-point-hexadecimal-regexp`

## Deferred

`goto-line-history-local` (buffer-local histories),
`selective-display`;
the rest of thingatpt.el (`thing-at-point` and the other things),
floating-point numbers at point

## Tests

test_simple_goto_mod (ports simple-test-count-words-bug-41761, thingatpt-tests' test-numbers-none, -decimal, -hex-lisp, -hex-c; own tests of `goto-line`, `goto-line-relative`, `count-words--format` and the keys)
