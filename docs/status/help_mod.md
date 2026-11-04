# help_mod

- Module: help_mod
- Upstream: help.el (+ help-macro.el `make-help-screen`, keymap.c `describe-buffer-bindings`/`describe_vector`, newcomment.el `comment-indent` for the lossage, simple.el `make-separator-line`)
- State: partial

## Ported

`substitute-command-keys` (\[COMMAND], \<MAP>, \{MAP} summaries, \`KEY', \=, curved quotes; a FORD-style `x` closes with a right quote), the help map on C-h and <f1> (the ported commands: C-h, ?, <help>, <f1>, a, b, c, d, e, f, k, l, m, o, q, u, v, w, x), `describe-key` (C-h k, "found in MAP" by help--binding-locus), `describe-key-briefly` (C-h c), `where-is` (C-h w), `describe-bindings` (C-h b: key translations, overriding, minor mode, major mode and global bindings, function key and input decoding translations; `help--describe-map-tree`, `describe-map` with keymap-canonicalize, shadowing, "(this binding is currently shadowed)", <remap> sections, char-table runs, the natural sort of function keys, the 16/24/32-column alignment), `view-lossage` (C-h l, comments aligned at column 24 as comment-indent does), `view-echo-area-messages` (C-h e), `help-for-help` (C-h C-h, C-h ?; upstream's screen text, saying Fortmacs where it means this program (R6-25) and on the Manuals lines (R6-32), the help screen loop with scrolling, "No help command is bound to"), `help-quit`, `with-help-window` (*Help* in the selected window, "Type q to restore previous buffer", trailing newlines as help-make-xrefs leaves them), subr.el `with-output-to-temp-buffer` (with window.c `temp_output_buffer_show` and help.el's help-mode-setup and help-mode-finish), names for keymaps that docstrings refer to

## Deferred

the help-map commands not ported (Info, manuals, NEWS/FAQ/tutorial files, describe-syntax, coding systems and input methods, display-local-help, help-quick, finder, packages, help-find-source; the help screen lists them under their upstream keys, which stay unbound),
`three-step-help`,
the `help-key-binding` and `separator-line` faces,
Outline minor mode in *Help* (`describe-bindings-outline`),
PREFIX/BUFFER of describe-bindings,
keyboard-translate-table,
text-property maps,
menus,
character names of translations,
`view-lossage-auto-refresh`,
changing `lossage-size`,
other `text-quoting-style`s,
key translations in describe-key,
mouse and menu events,
`function-called-at-point` defaults,
INSERT arguments,
keyboard-macro bindings in describe-key

## Tests

test_help_mod (ports help-tests-substitute-command-keys/no-change, /commands, /literal-key-sequence, /literal-key-sequence-ignore-invalid, /quotes (curve), /literals, /no-change-2, /keymaps, /undefined-map, help-tests-describe-map-tree/mention-shadow-t, /mention-shadow-nil, /partial-t, /partial-nil; own tests through the command loop)
