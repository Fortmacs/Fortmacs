# winner_mod

- Module: winner_mod
- Upstream: winner.el
- State: partial

## Ported

`winner-mode` (global; `winner-mode-map` with C-c <left> and C-c <right>, bound the first time the mode runs unless `winner-dont-bind-my-keys`, as loading winner.el binds them; `winner-mode-hook`, `winner-mode-off-hook` as the mode's `off_hook`),
`winner-repeat-map`,
`winner-undo` (with `winner-undo-this`, "Winner undo (N / M)"),
`winner-redo`,
`winner-ring-size`,
`winner-boring-buffers`,
`winner-boring-buffers-regexp`,
saving on `window-configuration-change-hook` (`winner-change-fun`),
`post-command-hook` (`winner-save-old-configurations`) and `minibuffer-setup-hook` (`winner-save-unconditionally`, `winner-save-conditionally`),
`winner-insert-if-new`,
`winner-remember`,
`winner-configuration`,
`winner-ring`,
`winner-equal`,
`winner-conf`,
`winner-win-data`,
`winner-sorted-window-list`,
restoring (`winner-set`: points through `winner-make-point-alist` and `winner-get-point`, marks and their activation, boring and dead buffers' windows deleted; `winner-set-conf`, `winner-active-region`);
one frame,
so the per-frame alists are one ring and one configuration;
the ring (ring.el's `make-ring`, `ring-insert`, `ring-remove`, `ring-ref`, `ring-length` on Lisp objects) is an array of configurations,
newest first

## Deferred

the :set function of `winner-dont-bind-my-keys` after Winner mode first ran (Customize, step 26),
`select-enable-primary` (not ported: marks are restored as when it is nil),
restoring the minibuffer window's height (`winner-set-conf`; resizing the minibuffer window is Deferred),
a `winner-ring-size` of 0 (upstream's ring-insert fails with an arith-error; here nothing is saved),
other frames (step 44)

## Tests

test_winner_mod (ports winner-dont-bind-my-keys, its first check: setting the option with setopt waits for its :set function; own: undo and redo, boring buffers, `window-configuration-equal-p`, `window-configuration-change-hook`)
