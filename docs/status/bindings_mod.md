# bindings_mod

- Module: bindings_mod
- Upstream: bindings.el, subr.el (`global-map`, `esc-map`, `ctl-x-map`), international/mule-conf.el (self-insert range)
- State: partial

## Ported

C-x m and C-x 4 m (`compose-mail`, `compose-mail-other-window`),
`ctl-x-4-map` (C-x 4: C-x 4 f, C-x 4 C-f, C-x 4 d),
`ctl-x-r-map` (C-x r: the rect.el and register.el keys),
`ctl-x-x-map` (C-x x f),
`right-word`,
`left-word`,
M-f,
M-b,
M-d,
M-DEL,
M-@,
M-{,
M-},
M-h,
C-<left>,
C-<right>,
C-<up>,
C-<down>,
C-<delete>,
C-<backspace>,
the help map on C-h, <help> and <f1>,
C-a,
C-e, <home>, <end> (move-beginning/end-of-line), <menu>, <execute>, <find>, <begin>,
S-/C-<insert(char)>, <copy>, <paste>, <cut>,
S-<delete>, <undo>, <open>, <insertline>, <deleteline>,
C-_,
C-/,
C-x u,
C-?,
C-M-_,
C-SPC,
C-@,
C-x C-x,
C-x h,
C-w,
M-w,
C-k,
C-y,
M-y, <C-S-backspace>,
the expression and group keys (C-M-f, C-M-b, C-M-n, C-M-p, C-M-d, C-M-u, C-M-k, C-M-@, C-M-SPC and the C-M-arrow, C-M-<delete>, C-M-<backspace> forms),
the default keymaps with every binding whose command is ported (incl. minibuffer-local-map history keys),
`function-key-map` (backspace/delete → DEL),
`right-char`,
`left-char`;
C-n,
C-p, <down>, <up>,
`search-map` (M-s; o);
step 44a: subr.el `ctl-x-5-map` (C-x 5) with frame.el's,
files.el's and window.el's keys,
subr.el's `tab-prefix-map` (C-x t)

## Deferred

bindings to unported commands (C-x r f, the bookmark keys C-x r b/m/M/l, C-x SPC),
bidi-aware left/right,
menus

## Tests

test_bindings_mod
