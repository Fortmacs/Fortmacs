# text_properties_mod

- Module: text_properties_mod, text_properties_modify_mod (submodule)
- Upstream: textprop.c (+ intervals.c point motion)
- State: partial

## Ported

`text-properties-at`,
`get-text-property`,
`put-text-property`,
`add-text-properties`,
`set-text-properties` (`set_text_properties`, `set_text_properties_1`, `set_properties`),
`remove-text-properties`,
`remove-list-of-text-properties`,
`next/previous(-single)-property-change`,
`text-property-any`,
`text-property-not-all`,
`verify_interval_modification` (read-only, inhibit-read-only, stickiness on insertion, the `modification-hooks` of changed text and the `insert-behind-hooks`/`insert-in-front-hooks` recorded for an insertion),
`report_interval_modification`,
`execute_mod_hooks`,
`modify_text_properties` (with insdel.c's `prepare_to_modify_buffer_1`: the before-change hooks) and the after-change hooks of property changes,
starting over when a before-change function changed the intervals,
`text_property_stickiness`,
`next/previous-single-char-property-change` (without overlays);
`get-char-property` and `get-char-property-and-overlay` (overlays by priority, then text properties),
`get-pos-property` (editfns.c: stickiness and overlay insertion types),
`next-char-property-change`,
`previous-char-property-change`,
`next-single-char-property-change` and `previous-single-char-property-change` with overlays,
get_property_and_range (intervals.c),
TEXT_PROPERTY_MEANS_INVISIBLE (xdisp.c:invisible_prop),
the text properties of strings (`text-properties-at`, `get-text-property`, `add-text-properties`, `put-text-property`, `next-single-property-change` with a string OBJECT; `propertize`; `substring` and `concat` keeping them; CHECK_STRING_OR_BUFFER),
`add-face-text-property` (in the buffer),
`text_property_list`,
`add_text_properties_from_list`,
`extend_property_ranges` and `copy_text_properties` (buffers; the elements are `text_property_range_type` records),
`inhibit-point-motion-hooks` and intervals.c's point motion (`set_point`, `set_point_both`: `intangible` text and the `point-left`/`point-entered` functions; `move_if_not_intangible`, `temporary_set_point`);
property functions are symbols naming hooks_mod's `property_function_type`

## Deferred

string OBJECTs of the property-list helpers (with `format`, 42f),
the `window` property of overlays,
undo records,
LIMIT t,
`add-face-text-property` on strings,
`text-property-default-nonsticky` as a (buffer-local) variable (its table needs dotted pairs; the standard value is built in),
SET_PT in the primitives other than `goto-char` (they keep `temporary_set_point_both`, the same while `inhibit-point-motion-hooks` is t, its default; 42c, 42f);
Not applicable: `interval_of` (an index into the run array),
`syms_of_textprop` (symbol and variable definitions),
the scanner's doc-line artifact `returns`;
waits for the GUI: nothing

## Tests

test_text_properties_mod (ports textprop-interval-immutability; own: modification and insert hooks, intangible text and point motion hooks, property lists and copying, string intervals, `keymap` and `local-map` properties)
