# fortmacs

- Module: fortmacs
- Upstream: (none: the config API)
- State: partial

## Ported

the module an init.f90 uses: re-exports of the status type,
values,
the faces (`faces`, `face_underline`, `face_box`, the `FACE_STYLE_*` and `FACE_BOX_*` constants; the individual comint and compilation face objects are no longer exported),
the keyboard macro settings (`keyboard_macro_ring_maximum`, `keyboard_macro_execute_before_append`, `keyboard_macro_repeat_no_prefix`, `keyboard_macro_execute_repeat_key`, `keyboard_macro_execute_repeat_with_argument`, `keyboard_macro_editor_eight_bits`) and `keyboard_macro_termination_hook`,
`defcommand`,
`keymap-global-set`,
`global-set-key`,
editing and file functions,
word and paragraph motion,
syntax tables,
comments and expressions (`forward_comment`, `scan_groups`, `scan_expressions`, `parse_partial_expression`, the lisp.el expression commands),
help (`describe-function`, `describe-variable`, `substitute-command-keys`),
rectangles (`apply_on_rectangle` with `rectangle_line_action_type` extensions, a rectangle as an array of `string_type`, `last_cut_rectangle`, the rect.el functions),
registers (`registers` (protected), `register_type`, `register_contents_type` and the REGISTER_ kinds, `get_register`, the generic `set_register` for text, numbers and rectangles, the register.el functions, `register_separator`),
`count_lines`,
`line_number_at_position`,
and the settings (`tab-width`, `make-backup-files`, `line-move-visual`, `goal-column`, `history-length`, `enable-recursive-minibuffers`, `history-add-new-input`, `insert-default-directory`, `inhibit-field-text-motion`, `visible-bell`),
which now carry their upstream docstrings;
app/main.f90's default `configure` is written against it (which also keeps fpm from pruning it out of an install);
processes (`make_process`, `start_process`, filters and sentinels, signals, input),
shell commands and their settings,
`compile` and its settings;
the Dired file-operation options (`directory_editor_copy_preserve_time`, `directory_editor_recursive_copies`, `directory_editor_do_what_i_mean_target`, `directory_editor_create_destination_directories`, the keep-marker options and the rest),
`copy_file`,
`rename_file`,
`add_name_to_file`,
`create_symbolic_link`,
`set_file_modes`,
`set_file_times`,
`copy_directory`,
`copy_directory_create_symlink`,
`create_empty_file`,
the file-modes conversions,
`find_file_read_only`,
`read_only_mode`,
`read_only_mode_hook`,
`what_line`,
View mode (`view_mode`, `view_mode_map`, `view_mode_hook`, `view_file`, `view_buffer` and their other-window forms, `view_read_only`, `view_scroll_auto_exit`, `view_exits_all_viewing_windows`, `view_inhibit_help_message`, `view_no_disable_on_exit`)

## Deferred

hooks (`add-hook`),
buffer-local settings

## Tests

test_fortmacs_mod (a `configure` written against it)
