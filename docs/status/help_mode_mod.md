# help_mode_mod

- Module: help_mode_mod, help_mode_cross_reference_mod (submodule)
- Upstream: help-mode.el
- State: partial

## Ported

`help-mode`,
derived from `special-mode` (its keys come from special-mode-map; the keys of button-buffer-map bound in help-mode-map itself, as make-composed-keymap is not ported),
the button types `help-xref`,
`help-function`,
`help-variable`,
`help-symbol` (`help_name`),
`help-back`,
`help-forward` (button_mod objects, a name as the button data),
`help-button-action`,
`help-do-xref`,
`help-xref-following`,
`help-xref-stack`,
`help-xref-forward-stack`,
`help-xref-stack-item` (items of `help_xref_item_type`: position, procedure pointer, argument),
`help-setup-xref` (from C-h f, C-h v, C-h o and C-h k),
`help-make-xrefs` (from `with-help-window`: the first paragraph skipped, quoted names after variable/option, function/command/call, or none, M-x commands, `\\=` escapes; `help-xref-symbol-regexp` as `help_xref_name_regexp`),
`help-xref-button`,
`help-insert-xref-button`,
`help-xref--navigation-buttons` ([back], [forward], `help-back-label`, `help-forward-label`; with ensure-empty-lines),
`help-xref-go-back`,
`help-xref-go-forward`,
`help-go-back` (l, C-c C-b, <XF86Back>),
`help-go-forward` (r, C-c C-f, <XF86Forward>),
`help-follow-symbol` (C-c C-c, `help_follow_name`),
`help-goto-next-page` (n),
`help-goto-previous-page` (p)

## Deferred

s (help-view-source),
i,
I,
c (customize),
g (revert),
h,
?, <, >;
`help-buffer` returning a help-mode current buffer while following (one set of stacks, for *Help*, not buffer-local);
the Info node,
man page,
customization group,
URL and multilingual references,
and the face,
type and `source of` (help-function-def) buttons of help-make-xrefs,
`help-xref-on-pp`,
`help-clean-buttons`;
help-setup-xref in describe-key (describe-key--helper looks the keys up again in their buffer),
describe-mode,
describe-bindings,
the mule describe commands and button-describe;
help-make-xrefs from `with-output-to-temp-buffer` (help-mode-finish);
the menu,
tool bar,
context menu,
bookmarks

## Tests

test_help_mode_mod (ports help-mode-tests.el: make-xrefs (the name button), xref-button, xref-go-back, xref-go-forward, go-back, go-back-no-stack, go-forward, go-forward-no-stack, do-xref, follow-symbol, follow-symbol-no-symbol; own tests for [back] and [forward]),
test_help_mod
