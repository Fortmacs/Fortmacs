# easy_minor_mode_mod

- Module: easy_minor_mode_mod
- Upstream: emacs-lisp/easy-mmode.el
- State: partial

## Ported

a state expression (:variable (GETTER . SETTER), `getter`),
global modes' state as the default value (setq-default, default-value),
`define-minor-mode` (buffer-local or :global, :lighter, `MODE-map`, a body with the new state, :variable),
the mode function (`set_minor_mode`: nil enables, numbers, toggle),
`MODE-hook`/`-on-hook`/`-off-hook`,
the enabled/disabled message,
:after-hook (delayed while mode hooks are),
`easy-mmode-pretty-mode-name`

## Deferred

`define-globalized-minor-mode` in general (Global Font Lock mode is written out as it expands),
`local-minor-modes`/`global-minor-modes`,
custom integration,
`easy-mmode-define-keymap`,
a `MODE-map` defined before the mode without :keymap

## Tests

test_easy_minor_mode_mod (ports easy-mmode--minor-mode)
