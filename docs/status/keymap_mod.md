# keymap_mod

- Module: keymap_mod
- Upstream: keymap.c (+ keymap.el `key-parse`, `key-valid-p`, `keymap-set`, `keymap-global-set`, keyboard.c `reorder_modifiers` subset)
- State: done

## Ported

`where-is-internal` (all or the first preferred binding, advertised bindings, char-table runs as FROM..TO intervals, shadowed bindings left out; KEYMAP and the global map, or the active maps; a map reached again is searched again unless through a cycle, as accessible-keymaps does),
registry with explicit lifetimes,
`make-keymap`,
`make-sparse-keymap`,
`is_keymap`,
`keymap-parent`,
`set-keymap-parent` (cycle check),
`copy-keymap`,
`access_keymap` (meta→ESC, t defaults, parent and composite prefix maps),
`define-key` (with REMOVE, `define_as_prefix`),
character ranges (the char-table part of full keymaps),
overriding-terminal-local-map,
`lookup-key` (lists of keymaps, too-long),
`current-global-map`,
`use-global-map`,
`current-local-map`,
`use-local-map`,
`current-active-maps` (with the `keymap` and `local-map` properties at point, intervals.c `get_local_map`),
`key-binding` (local + global),
keyboard.c `parse_modifiers`/`apply_modifiers`/`reorder_modifiers` (symbols),
`keyboard`,
`key-description`,
`single-key-description`,
`push_key_description`,
`key-valid-p`,
`keymap-set` (a character definition names a command),
`keymap-global-set`,
minor-mode maps (`minor-mode-map-alist`, via `append_minor_mode_map`; the buffer-local `minor-mode-overriding-map-alist`, via `push_minor_mode_overriding_map`);
`accessible-keymaps` (prefix maps breadth first, ESC maps meta-ized),
`map-keymap` items (char-table runs, then entries, with the parents),
`current_minor_maps`;
keyboard.c `parse_modifiers_uncached` with the click modifiers (`down-`, `drag-`, `double-`, `triple-`, `up-`, click for `mouse-N` and `wheel-`) and `apply_modifiers_uncached` with their prefixes;
`keymap-prompt` (through the parents),
`get_keymap`,
`keymap_parent`,
`lookup_key_1`,
`text-char-description` (`push_text_character_description`),
`current-minor-mode-maps`,
`minor-mode-key-binding` (the pairs as two arrays),
keys given as ["C-c a"] to `define-key` and `lookup-key` (`possibly_translate_key_sequence`),
`silly_event_symbol_error`,
`where_is_internal` and `shadow_lookup`,
`append_key` and `map_keymap_canonical` in accessible-keymaps,
the map-keymap family (`Fmap_keymap`, `Fmap_keymap_internal`, `map_keymap_internal`, `map_keymap_item`, `map_keymap_call`, `map_keymap_char_table_item`),
`copy_keymap_item` and `copy_keymap_set_char_table`,
`help--describe-vector` (help_mod)

## Deferred

`keymap-local-set` and the other keymap.el functions,
char-tables/vectors (full keymap is a flag),
menu items,
remapping in `where-is-internal`,
symbol function cells as prefix maps,
event ranges,
text-property / overriding maps,;
after step 42c: menu items (`get_keyelt`, `keymap--get-keyelt`),
mouse events (`click_position`),
`describe-vector` of an arbitrary vector or char-table with `describe_vector_basic` and `describe_vector_princ` (no Lisp vectors; char-tables hold integers),
faces in *Help* text (`describe_key_maybe_fontify`);
Not applicable: `CHECK_VECTOR_OR_CHAR_TABLE` (type checks of Lisp vectors),
`initial_define_lispy_key` (bindings are defined in Fortran by bindings_mod),
the scanner's artifact `static_assert`;
waits for the GUI: menu bars and tool bars

## Tests

test_keymap_mod (own: char-table ranges, printing, equal; ports keymap-tests: make-keymap, is_keymap, keymap-parent, copy-keymap, define-key/undefined, /keyboard-macro, /keymap, lookup-key, /list-of-keymaps, /too-long, removal-inherit, unset-test-remove-and-inheritance, keymap--key-description, keymap-set-consistency (the `keymap-set` part); subr-tests: subr-test-key-valid-p);
own: keymap-string-keys-and-silly-symbols,
keymap-prompt-and-minor-mode-key-binding;
ports keymap-text-char-description
