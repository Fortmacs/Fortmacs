# editing_functions_mod

- Module: editing_functions_mod
- Upstream: editfns.c (+ buffer.c `validate_region`, intervals.c `set_point_from_marker`)
- State: partial

## Ported

`mark-marker` (made on first use),
`region-beginning`,
`region-end` (`region_limit`, `mark-inactive`),
`point`,
`point-min/max`,
`pos-bol`,
`pos-eol`,
`line-beginning/end-position` (constrained to fields),
fields (`find_field`, `field-beginning`, `field-end`, `field-string-no-properties`, `delete-field`, `constrain-to-field`, `get-pos-property`; text properties only),
`inhibit-field-text-motion`,
`buffer-size`,
`gap-position/size`,
`goto-char` (integer and marker, through intervals.c `set_point_both`),
`point(-min/-max)-marker`,
`position-bytes`,
`byte-to-position`,
`following-char`,
`preceding-char`,
`is_beginning_of_buffer`,
`is_end_of_buffer`,
`is_beginning_of_line`,
`is_end_of_line`,
`char-after/before`,
`insert-char` (count),
`narrow-to-region`,
`widen`,
`buffer-substring` (generic: with the text properties into a string value, the text alone into a character variable; `make_buffer_string`, `make_buffer_string_both`),
`buffer-substring-no-properties`,
`buffer-string`,
`insert-buffer-substring` (with text properties, through insdel.c `insert_from_buffer`),
`delete-region`,
`delete-and-extract-region`,
`subst-char-in-region` (in place, as upstream; NOUNDO, the undo list and file name restored also after an error, `subst_char_in_region_unwind(_1)`; starting over after the before-change hooks, then the after-change hooks) (advised by Rectangle-Mark mode through procedure pointers),
callint.c's `r` interactive code (`interactive_region`);
step 42f: `insert`,
`insert-before-markers`,
`insert-and-inherit`,
`insert-before-markers-and-inherit` with two to eight strings (`general_insert_function`: each string as insdel.c's `insert_from_string(_before_markers)` inserts it, the change hooks once per string; they extend insert_delete_mod's one-string procedures, which are `insert1`, as generic interfaces: `call insert(status, "Hello, ", name, "!")`),
`field-string` (with the text properties; the text alone is `field-string-no-properties`, comint's and shell's input),
`compare-buffer-substrings` (absent arguments for the defaults; `case-fold-search` through the current buffer's case canon table),
`char-equal` (`is_character_equal`),
`char-to-string`,
`string-to-char`,
`ngettext` (dired-aux's copy removed),
`emacs-pid`,
`group-name` (unallocated for a gid with no group; a gid outside gid_t is "Not an in-range integer"),
`group-real-gid`,
`current-message` (in display_engine_mod beside `message`, which keeps the echo area's text);
step 42f chunk 2: `transpose-regions` (`transpose_markers`; undo through undo.c `record_change`, now in undo_mod; text properties moved with their text; LEAVE-MARKERS correcting only byte positions, insdel.c `adjust_markers_byte_position` with TO; the M-x command reading point, the mark and the mark ring, in simple_goto_mod),
`translate-region-internal` (a string, or a char-table whose purpose is `translation_table`; a one-character replacement of another byte length through insdel.c `replace_range`),
save-excursion as `save_excursion_begin`/`save_excursion_end` with `type(excursion_type)` (`save_excursion_save`, `save_excursion_restore`: the selected window's point too),
save-restriction as `save_restriction_begin`/`save_restriction_end` with `type(restriction_type)` (`save_restriction_save(_1)`, `save_restriction_restore(_1)`, `labeled_restrictions_save`/`_restore`: copies of the labeled restrictions with markers of their own),
the labeled restrictions (`labeled_restrictions` as an array of `labeled_restriction_type` records with a character label and an `outermost` flag; `labeled_restrictions_add`, `_remove`, `_get_bound`, `_peek_label` (the index of the innermost), `_push`, `_pop`, `_remove_in_current_buffer`, `internal--labeled-narrow-to-region`, `internal--labeled-widen`, `labeled_narrow_to_region` as `labeled_narrow_to_region_begin`/`_end` (with fileio.c `restore_point_unwind`), `reset_outermost_restrictions` and `unwind_reset_outermost_restriction` as `reset_outermost_restrictions_begin`/`_end` around redisplay_internal; `narrow-to-region` and `widen` keep within them);
`write_with_format`: an integer, or strings in order, written with a Fortran format the user types (skeleton prompts, `completing_read_multiple_prompt`, `css_lookup_url_format`, the dired compression commands, keyboard_macros_mod's counter formats, rectangle_mod's line-number format; a malformed format is an `error` through STATUS; panel, 2026-11-06, not a port);
`message` (in display_engine_mod) shows its text as it is;
Not applicable: `format`, `format-message` and `message`'s format arguments: text is built with `write` (owner, 2026-11-06; the port of step 42f chunk 3 is gone; quotes are curved with error_mod's `substitute_quotes`, help.el:substitute-quotes),
`propertize` (text_properties_mod) reports an odd number of properties as `wrong-number-of-arguments` (new error code) through an optional STATUS

## Deferred

`check_translation` and the char-table values it reads (arrays of characters and lists of translation records: character_table_mod's values are integers),
subr.el `with-restriction` and `without-restriction` (and `internal--with-restriction`),
buffer.c `erase-buffer` and the other callers of `labeled_restrictions_remove_in_current_buffer` (buffer_mod is below editing_functions_mod),
long-line optimizations (the labels upstream uses),
the private save-excursion and save-restriction copies in other modules (to migrate in a follow-up),
`text-quoting-style` (`substitute_quotes` always curves),
the message's text properties in the echo area,
`inhibit-message`,
overlays in fields,
INHIBIT-CAPTURE-PROPERTY,
characters as arguments of the insert family (`insert_character`, or `character_to_string`),
the text properties of `buffer-string` (its function form gives the text; `buffer_substring` over the accessible text gives them),
`buffer-access-fontify-functions` and `buffer-access-fontified-property` (`update_buffer_properties`),
`message-box` (on a terminal `x-popup-dialog` shows a TTY menu: no TTY menus yet),
`message-or-box` (with `use-dialog-box` and `last-nonmenu-event`, it calls `message-box`)

## Tests

test_editing_functions_mod (ports find-fields, test-group-name, transpose-ascii-regions-test, transpose-nonascii-regions-test-1 and -2, editfns-tests--transpose-equal-but-not, editfns-tests--transpose-regions (the undo checked on the text: undo records keep no text properties), editfns-tests--bug76124, test-translate-region-internal; own: the insert family, buffer-substring's properties, field-string, compare-buffer-substrings, the character functions, ngettext, emacs-pid, group-real-gid, current-message, transpose errors, string translation tables, save-excursion, save-restriction, labeled restrictions, write-with-format, substitute-quotes; also ports propertize/error-even-number-of-args),
test_insert_delete_mod (the change-hook cases),
test_editing_commands_mod (own);
the format tests (format-properties, format-c-float, format-binary-*, format-sharp-0-x, format-%d-float, format-%d-large-float, format-%x-large-float, format-%o-negative-float, format-with-field, editfns-tests-styled-print, format-bignum) are not applicable (text is built with write),
delete-region-undo-markers-1 needs the undo marker adjustments,
the rest `replace-region-contents`
