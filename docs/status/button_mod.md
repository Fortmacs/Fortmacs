# button_mod

- Module: button_mod
- Upstream: button.el
- State: partial

## Ported

the `button` face,
`button-buffer-map` (TAB, ESC TAB, <backtab>),
`button-map` (RET),
`button-mode` (its body unbuttonizes the whole buffer),
`define-button-type` (a `button_kind_type` object and its `%define(name, supertype)`; the properties as components: action and mouse-action procedure pointers, face, mouse-face, keymap, help-echo text or procedure, follow-link, skip),
`button-type-subtype-p` (`is_button_subtype`),
`button-start`,
`button-end`,
`button-get` and `button-put` (type-bound accessors of `button_type`, one `get_X`/`set_X` pair per property: kind, action, mouse_action, face, mouse_face, keymap, help_echo, help_echo_function, follow_link, skip, button_data; R6-28),
`button-activate` (`activate_button`),
`button-label`,
`button-has-type-p` (`has_button_type`),
`make-button`,
`insert-button` (overlays, evaporating),
`make-text-button`,
`insert-text-button` (text properties, each button its own `button` value),
`button-at`,
`next-button`,
`previous-button`,
`push-button` (RET),
`button--help-echo`,
`forward-button` (TAB, wrapping, skip, the help-echo message),
`backward-button` (S-TAB),
`button-describe` (`describe_button`: the type line),
`button--properties`,
`buttonize-region`,
`unbuttonize-region`

## Deferred

mouse events (`push-button`'s event, mouse-2, follow-link, mode-line, header-line and touch screen bindings of `button-map`),
buttons in strings and the mode line or header line (area buttons, `make-text-button` on a string, `buttonize`),
a marker as an action,
the Action and Mouse-action sections of `button-describe` (a procedure pointer carries no name) and its help-setup-xref,
widgets (bug#34506);
a form as help-echo is Not applicable

## Tests

test_button_mod (ports button-tests.el: button-at, button--help-echo-string, button--help-echo-function, button--preserve-help-echo (its buttonize-region part); button--help-echo-form Not applicable; own tests for motion, skip, pushing, types, describe, button-mode, the Help mode keys)
