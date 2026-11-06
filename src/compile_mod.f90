! compile_mod.f90 -- run compiler as inferior of Emacs, parse error messages.
! Ported from GNU Emacs 31.1 lisp/progmodes/compile.el, with
! lisp/files.el cd, lisp/simple.el forward-to-indentation and
! lisp/startup.el command-line-normalize-file-name.
!
! Copyright (C) 1985-1987, 1993-1999, 2001-2026 Free Software
! Foundation, Inc.
! Copyright (C) 2026 The Fortmacs authors
!
! This file is part of Fortmacs.
!
! Fortmacs is free software: you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! Fortmacs is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with Fortmacs.  If not, see <https://www.gnu.org/licenses/>.

!> Compilation: `compile' runs a shell command asynchronously with its
!> output in *compilation*, in Compilation mode (or, with C-u, in Comint
!> mode with `compilation_shell_minor_mode'), with the "Compilation
!> started at ..." header and the "Compilation finished at ..., duration
!> ..." line when it ends; `recompile' (g) runs it again and
!> `stop_compilation' (C-c C-k) interrupts it.  While it runs the mode
!> line shows ":run" and "[Compiling] ", and the counts of errors,
!> warnings and informational messages found so far.
!>
!> The output is parsed with the rules of `compilation_error_regular_expression_names'
!> (all of `compilation_error_regular_expressions' but `omake'), following
!> the "Entering directory" messages of `compilation_directory_matcher';
!> each message gets a `compilation_message' text property and faces.
!> `next_error' (M-g n, C-x `) and `compile_goto_error' (RET) visit the
!> source of a message, `compilation_next_error' (M-n, TAB) and friends
!> move between messages.  `compilation_minor_mode' and
!> `compilation_shell_minor_mode' do the same in other buffers.
!>
!> A mode made by define-compilation-mode (such as grep_mod's
!> `grep_mode') has `compilation_mode' as its parent and gives its buffers
!> their own compilation variables with `compilation_set_mode_locals'
!> and the settings' `set_local'; `compilation_start' runs the command in
!> that mode, in the buffer *NAME* (NAME being the mode's name without
!> "_mode").
!>
!> Not ported yet (Deferred): `compilation_auto_jump_to_first_error' and
!> the `first-error' value of `compilation_scroll_output' beyond scrolling
!> (need timers), highlighting the locus (`next_error_highlight',
!> `compilation_highlight_regular_expression': need overlays), the overlay arrow and
!> margins (`compilation_context' COMPILATION_CONTEXT_NO_SCROLL, the
!> fringe), the `keymap'
!> text property (`compilation_button_map'), the mouse, menus and tool
!> bar, `compilation_goto_in_progress_buffer'; a global
!> `compilation_error_regular_expression_names' with rule specifications rather than
!> names (add the rule to `compilation_error_regular_expressions'; a mode's
!> own value is rules), FILE given as (FILE FORMAT...), HIGHLIGHT faces
!> given as text properties other than `face' and `display',
!> `compilation_parse_errors_filename_function',
!> `command_interpreter_file_name_prefix' (remote files), `compilation_search_path'
!> and `compilation_search_extra_path' (only the directory of the message
!> and, with `compilation_search_all_directories', the directories
!> entered before it are searched), `compilation_fake_location',
!> `compilation_debug'; the text properties (no `face', no `compilation_message', ...)
!> of the font-lock keywords of the "Compilation finished" and
!> "Compilation exited" lines, and removing the font-lock keywords when
!> the minor modes are turned off (`font_lock_remove_keywords'); visiting
!> a directory message with `compile_goto_error' (`directory_editor_other_window');
!> `selective_display' in `compilation_beginning_of_line';
!> `compilation_maximum_output_line_length' (long lines are not abbreviated:
!> needs buttons and display properties), `compilation_hidden_output',
!> `compilation_window_height', `compilation_page_delimiter'
!> (`page_delimiter' is not buffer-local), `compilation_environment',
!> `compilation_buffer_name_function', the global values of
!> `compilation_process_setup_procedure' and
!> `compilation_exit_message_procedure' (modes set their own),
!> `compilation_start_hook', `compilation_finish_functions',
!> `compilation_save_buffers_test_function' (all modified file buffers are
!> offered), the NAME-FUNCTION, HIGHLIGHT-REGEXP and CONTINUE arguments
!> of `compilation_start', `revert_buffer_function' (g is `recompile'
!> itself), wildcards in a leading "cd" of the command
!> (`file-expand-wildcards'), the variables define-compilation-mode sets
!> from the mode's name (the mode's body sets them),
!> `next_error_follow_minor_mode' (C-c C-f) and `recenter_current_error'
!> (l).
module compile_mod
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use execute_interactively_mod, only: PREFIX_NONE, PREFIX_RAW
    use commands_mod, only: commands
    use error_mod, only: status_type, set_error, substitute_quotes, GENERIC_ERROR_CODE, USER_ERROR_CODE
    use handle_mod, only: handle_type, null_handle
    use value_mod, only: value_type, list, xfixnum, vtag, V_INTEGER, V_STRING
    use data_mod, only: string_setting_type, logical_setting_type, integer_setting_type, symbol_value, set, &
                        append_locals_clearer, forward_setting, choice_setting_type, string_to_number
    use buffer_mod, only: buffers, current, current_buffer, set_buffer, get_or_make_buffer, is_buffer_live, &
                          buffer_list, erase_buffer, restore_buffer_modified
    use marker_mod, only: copy_marker_at, destroy_marker, marker_position, set_marker, marker_buffer, make_marker
    use insert_delete_mod, only: insert, before_change_functions
    use editing_functions_mod, only: point, point_minimum, point_maximum, goto_character, widen, narrow_to_region, &
                                     line_beginning_position, line_end_position, is_end_of_buffer, is_beginning_of_buffer, &
                                     is_beginning_of_line, buffer_size, buffer_substring, point_marker
    use text_properties_mod, only: put_text_property, get_text_property, add_text_properties, &
                                   remove_list_of_text_properties, next_single_property_change, &
                                   previous_single_property_change, text_property_not_all
    use editing_commands_mod, only: forward_line, beginning_of_line, end_of_line
    use indent_mod, only: move_to_column, current_column
    use syntax_mod, only: skip_characters_forward
    use search_mod, only: string_match, regular_expression_search_forward, match_end, match_data, set_match_data, replace_match, &
                          case_fold_search, SEARCH_NOERROR
    use basic_subprograms_mod, only: match_string, silent_modifications_type, silent_modifications_begin, silent_modifications_end
    use environment_mod, only: substitute_environment_variables, getenv
    use file_input_output_mod, only: expand_file_name, file_name_as_directory, is_file_name_absolute, is_file_directory, &
                                     file_exists, is_file_accessible_directory, file_name_directory, file_name_nondirectory
    use files_mod, only: abbreviate_file_name, save_some_buffers, find_file_noselect, file_truename
    use print_mod, only: readable_text
    use utility_functions_mod, only: read_yes_or_no
    use hooks_mod, only: hook_type, change_major_mode_hook
    use keyboard_mod, only: sit_for
    use execute_interactively_mod, only: current_prefix_argument, prefix_numeric_value
    use display_engine_mod, only: message, mode_line_part_type, set_mode_line_process, append_mode_line_modes_entry, tab_width
    use window_mod, only: windows, display_buffer, set_window_start, set_window_point, pop_to_buffer, switch_to_buffer, &
                          get_buffer_window, select_window, display_action_parameters_type, pop_up_windows
    use frame_mod, only: selected_frame
    use keymap_mod, only: set_keymap_parent, keymap_set
    use modes_mod, only: major_modes, minor_modes
    use derived_mod, only: major_mode_type
    use simple_mod, only: special_mode_map, read_shell_command, shell_command_switch, next_error_last_buffer, &
                          set_next_error_function, next_error_find_buffer, next_error_internal, next_error, push_mark
    use execute_process_mod, only: shell_file_name, initialize_callproc
    use process_mod, only: make_process, get_buffer_process, process_status, &
                           process_query_on_exit_flag, interrupt_process, delete_process, process_buffer, &
                           process_exit_status, set_process_buffer, set_process_query_on_exit_flag, process_mark, &
                           process_send_eof, number_processors
    use time_functions_mod, only: current_time_string
    use version_mod, only: upstream_version
    use command_interpreter_mod, only: command_interpreter_carriage_motion, command_interpreter_inhibit_carriage_motion, &
                                       command_interpreter_mode, command_interpreter_exec
    use faces_mod, only: named_face_type, display_when, face_attributes, FACE_CLASS_COLOR
    use font_core_mod, only: font_lock_defaults, font_lock_keyword_type, font_lock_highlight_type, &
                             FONT_LOCK_OVERRIDE_REPLACE
    use font_lock_mod, only: font_lock_append_text_property, font_lock_add_keywords, font_lock_flush
    use string_type_mod, only: string_type, append_string
    implicit none(type, external)
    private

    public :: compile, recompile, compilation_start, compilation_mode, stop_compilation, compilation_filter, &
              compilation_sentinel, compilation_handle_exit, command_interpreter_carriage_motion, initialize_compile, cd
    public :: compilation_parse_errors, compilation_ensure_parse, compilation_next_error, compilation_previous_error, &
              compilation_next_file, compilation_previous_file, compilation_display_error, compile_goto_error, &
              compilation_next_error_function, compilation_find_buffer, compilation_forget_errors, &
              compilation_set_skip_threshold, compilation_minor_mode, compilation_shell_minor_mode, &
              compilation_setup, compilation_face, is_compilation_buffer_internal, is_compilation_buffer, &
              compilation_message_info, compilation_message_counts, compilation_find_file, compilation_set_mode_locals, &
              compilation_update_mode_line, compilation_message_location

    abstract interface
        !> A rule's LINE, END-LINE, COLUMN or END-COLUMN given as a function
        !> of no arguments: the number, or FOUND false for none.  It can
        !> assume the rule's regexp has just been matched (point is at the
        !> end of the match), and should preserve the match data.  Not a port.
        subroutine compilation_number_procedure(number, found)
            import :: int64
            implicit none(type, external)
            integer(int64), intent(out) :: number
            logical, intent(out) :: found
        end subroutine compilation_number_procedure

        !> A rule's FILE given as a function: FILENAME (relative to
        !> DIRECTORY if that is allocated), or FOUND false for none.
        !> Not a port.
        subroutine compilation_file_procedure(filename, directory, found)
            implicit none(type, external)
            character(len=:), allocatable, intent(out) :: filename, directory
            logical, intent(out) :: found
        end subroutine compilation_file_procedure

        !> A HIGHLIGHT's FACE given as a function called after each
        !> match: the face's name (FACE unallocated for none).  Not a port.
        recursive subroutine compilation_face_procedure(face, status)
            import :: status_type
            implicit none(type, external)
            character(len=:), allocatable, intent(out) :: face
            type(status_type), intent(out) :: status
        end subroutine compilation_face_procedure

        !> `compilation_process_setup_procedure': called by
        !> `compilation_start' just before the process starts, with the
        !> compilation buffer current.  ENVIRONMENT holds the "NAME=VALUE"
        !> strings the process gets in front of `process_environment'; the
        !> function may add to it (as `setenv' does inside the binding of
        !> `process_environment' in compilation-start).  Not a port.
        recursive subroutine compilation_process_setup_procedure(environment, status)
            import :: string_type, status_type
            implicit none(type, external)
            type(string_type), allocatable, intent(inout) :: environment(:)
            type(status_type), intent(out) :: status
        end subroutine compilation_process_setup_procedure

        !> `compilation_exit_message_procedure': the message MESSAGE and
        !> the mode line string CODE for a compilation that ended with
        !> PROCESS_STATUS ("exit" or "signal") and EXIT_STATUS, MESSAGE being
        !> the default message.  Not a port.
        subroutine compilation_exit_message_procedure(process_status, exit_status, message_text, message, code)
            implicit none(type, external)
            character(len=*), intent(in) :: process_status, message_text
            integer, intent(in) :: exit_status
            character(len=:), allocatable, intent(out) :: message, code
        end subroutine compilation_exit_message_procedure

        !> `compilation_mode_line_errors': the parts of the mode line that
        !> follow the process state.  Not a port.
        subroutine compilation_mode_line_errors_procedure(parts)
            import :: mode_line_part_type
            implicit none(type, external)
            type(mode_line_part_type), allocatable, intent(out) :: parts(:)
        end subroutine compilation_mode_line_errors_procedure
    end interface

    public :: compilation_number_procedure, compilation_file_procedure, compilation_face_procedure, &
              compilation_process_setup_procedure, compilation_exit_message_procedure, &
              compilation_mode_line_errors_procedure

    !> A HIGHLIGHT of a rule: the text of SUBMATCH gets the face FACE (a
    !> face name), or the one FACE_FUNCTION gives.  DISPLAY, if allocated,
    !> makes the text properties `face' FACE and `display' DISPLAY: the text
    !> also gets that `display' property.
    type, public :: compilation_highlight_type
        integer :: submatch = 0
        character(len=:), allocatable :: face
        procedure(compilation_face_procedure), pointer, nopass :: face_function => null()
        character(len=:), allocatable :: display
    end type compilation_highlight_type

    !> A rule for matching messages: an element of
    !> `compilation_error_regular_expressions' (see
    !> `compilation_error_regular_expression_names').  Subexpression numbers are 0 for
    !> none; WARNING and INFO, if either is not 0, make the type
    !> depend on which of them matched, otherwise TYPE
    !> is the type (2 error, 1 warning, 0 info).
    type, public :: compilation_rule_type
        !> The rule's name (`compilation_error_regular_expression_names' refers to it).
        character(len=:), allocatable :: name
        character(len=:), allocatable :: regular_expression
        integer :: file = 0
        procedure(compilation_file_procedure), pointer, nopass :: file_function => null()
        integer :: line = 0, end_line = 0, column = 0, end_column = 0
        procedure(compilation_number_procedure), pointer, nopass :: line_function => null()
        procedure(compilation_number_procedure), pointer, nopass :: end_line_function => null()
        procedure(compilation_number_procedure), pointer, nopass :: column_function => null()
        procedure(compilation_number_procedure), pointer, nopass :: end_column_function => null()
        integer :: type = 2
        integer :: warning = 0, info = 0
        integer :: hyperlink = 0
        type(compilation_highlight_type), allocatable :: highlights(:)
    end type compilation_rule_type

    !> The value of `compilation_directory_matcher': REGULAR_EXPRESSION (unallocated
    !> for none) and the subexpressions DIRECTORY(i), with LEAVE(i) (0 for none).
    type, public :: compilation_directory_matcher_type
        character(len=:), allocatable :: regular_expression
        integer, allocatable :: directory(:), leave(:)
    end type compilation_directory_matcher_type

    !> An element of `compilation_transform_file_matches': REGULAR_EXPRESSION and
    !> REPLACEMENT (unallocated for none).
    type, public :: compilation_transform_type
        character(len=:), allocatable :: regular_expression, replacement
    end type compilation_transform_type

    !> What `compilation_message_info' tells about a message: its RULE
    !> (unallocated for an anonymous rule), TYPE, and the location (FILE,
    !> DIRECTORY, LINE, COLUMN; HAS_LOCATION false for none) and end location.
    type, public :: compilation_message_info_type
        character(len=:), allocatable :: rule
        integer :: type = 2
        logical :: has_location = .false.
        character(len=:), allocatable :: file, directory
        logical :: has_line = .false., has_column = .false.
        integer(int64) :: line = 0, column = 0
        logical :: has_end = .false., has_end_line = .false., has_end_column = .false.
        integer(int64) :: end_line = 0, end_column = 0
    end type compilation_message_info_type

    !> Last shell command used to do a compilation; default for next
    !> compilation.
    !>
    !> Sometimes it is useful for files to supply local values for this
    !> variable.  You might also use mode hooks to specify it in certain
    !> modes.
    !>
    !> It's often useful to leave a space at the end of the value.  Port of
    !> compile.el compile-command ("make -k -jN " with N the number of
    !> processors divided by 1.5, rounded up).
    type(string_setting_type), public :: compile_command = string_setting_type(name="compile_command")

    !> True means \[compile] reads the compilation command to use.
    !> Otherwise, \[compile] just uses the value of `compile_command'.
    !>
    !> Note that changing this to false may be a security risk, because a
    !> file might define a malicious `compile_command' as a file local
    !> variable, and you might not notice.  Therefore, `compile_command'
    !> is considered unsafe if this variable is false.  Port of compile.el
    !> compilation-read-command.
    type(logical_setting_type), public :: compilation_read_command = &
                                          logical_setting_type(name="compilation_read_command", value=.true.)

    !> True means \[compile] asks which buffers to save before compiling.
    !> Otherwise, it saves all modified buffers without asking.  Port of
    !> compile.el compilation-ask-about-save.
    type(logical_setting_type), public :: compilation_ask_about_save = &
                                          logical_setting_type(name="compilation_ask_about_save", value=.true.)

    !> If true, always stop a running compilation process before starting
    !> a new one.  If false, ask to stop it.  Port of compile.el
    !> compilation-always-kill.
    type(logical_setting_type), public :: compilation_always_stop = &
                                          logical_setting_type(name="compilation_always_stop", value=.false.)

    !> If true, send end-of-file as compilation process input.
    !> This only affects platforms that support asynchronous processes (see
    !> `start_process'); synchronous compilation processes never accept
    !> input.  Port of compile.el compilation-disable-input.
    type(logical_setting_type), public :: compilation_disable_input = &
                                          logical_setting_type(name="compilation_disable_input", value=.false.)

    !> Values of `compilation_scroll_output' besides .true. and .false.
    integer, parameter, public :: COMPILATION_SCROLL_OUTPUT_FIRST_ERROR = 2

    !> True to scroll the *compilation* buffer window as output appears.
    !>
    !> Setting it causes the Compilation mode commands to put point at the
    !> end of their output window so that the end of the output is always
    !> visible rather than the beginning.
    !>
    !> COMPILATION_SCROLL_OUTPUT_FIRST_ERROR stops scrolling at the first error,
    !> and leaves point on its location in the *compilation* buffer.  Port of
    !> compile.el compilation-scroll-output.
    type(choice_setting_type), public :: compilation_scroll_output = &
                                         choice_setting_type(name="compilation_scroll_output")

    !> Hook run after `compilation_filter' has inserted a string into the
    !> buffer.  It is called with the variable `compilation_filter_start'
    !> bound to the position of the start of the inserted text, and point at
    !> its end.
    !>
    !> If Fortmacs lacks asynchronous process support, this hook is run
    !> after `execute_process' inserts the grep output into the buffer.  Port
    !> of compile.el compilation-filter-hook.
    type(hook_type), target, public :: compilation_filter_hook = hook_type(name="compilation_filter_hook")

    !> Position of the start of the text inserted by `compilation_filter'.
    !> This is bound before running `compilation_filter_hook'.  Port of
    !> compile.el compilation-filter-start.
    integer(int64), public :: compilation_filter_start = 0

    !> Hook run after entering Compilation mode.  Port of compile.el
    !> compilation-mode-hook.
    type(hook_type), target, public :: compilation_mode_hook = hook_type(name="compilation_mode_hook")

    !> Hook run after entering or leaving `compilation_minor_mode'.  Port
    !> of compile.el compilation-minor-mode-hook.
    type(hook_type), target, public :: compilation_minor_mode_hook = hook_type(name="compilation_minor_mode_hook")

    !> Hook run after entering or leaving `compilation_shell_minor_mode'.
    !> Port of compile.el compilation-shell-minor-mode-hook.
    type(hook_type), target, public :: compilation_shell_minor_mode_hook = &
                                       hook_type(name="compilation_shell_minor_mode_hook")

    !> Keymap for compilation log buffers.  Port of compile.el
    !> compilation-mode-map.
    type(handle_type), public :: compilation_mode_map

    !> Keymap for `compilation_minor_mode'.  Port of compile.el
    !> compilation-minor-mode-map.
    type(handle_type), public :: compilation_minor_mode_map

    !> Keymap for `compilation_shell_minor_mode'.  Port of compile.el
    !> compilation-shell-minor-mode-map.
    type(handle_type), public :: compilation_shell_minor_mode_map

    !> Directory to restore to when doing `recompile'.  Port of compile.el
    !> compilation-directory (this is its default value; a compilation
    !> buffer has a value of its own, the directory it was started in).
    type(string_setting_type), public :: compilation_directory = string_setting_type(name="compilation_directory")

    !> Table of values for `compilation_error_regular_expression_names'.  Port of
    !> compile.el compilation-error-regexp-alist-alist (the rules are named
    !> with underscores for hyphens: `gcc_include', `edg_1').
    type(compilation_rule_type), allocatable, public :: compilation_error_regular_expressions(:)

    !> Table that specifies how to match errors in compiler output.
    !> On GNU and Unix, any string is a valid filename, so these
    !> matchers must make some common sense assumptions, which handle
    !> normal cases.  A shorter list will be lighter on resource usage.
    !>
    !> Each element is the name of a rule of
    !> `compilation_error_regular_expressions'.
    !>
    !> A rule has the form (REGULAR_EXPRESSION FILE [LINE COLUMN TYPE HYPERLINK
    !> HIGHLIGHT...]).  If REGULAR_EXPRESSION matches, the FILE'th subexpression
    !> gives the file name, and the LINE'th subexpression gives the line
    !> number.  The COLUMN'th subexpression gives the column number on
    !> that line.
    !>
    !> If FILE, LINE or COLUMN are 0 or that index didn't match, that
    !> information is not present on the matched line.  In that case the
    !> file name is assumed to be the same as the previous one in the
    !> buffer, line number defaults to 1 and column defaults to
    !> beginning of line's indentation.
    !>
    !> FILE can also be a function that returns FILENAME or
    !> RELATIVE-FILENAME and DIRECTORY_NAME.
    !>
    !> LINE can also be given with END-LINE, meaning a range of lines.
    !> COLUMN can also be given with END-COLUMN, meaning a range of columns
    !> starting on LINE and ending on END-LINE, if that matched.
    !>
    !> LINE, END-LINE, COLUMN, and END-COL can also be functions of no
    !> argument that return the corresponding line or column number.  They
    !> can assume REGULAR_EXPRESSION has just been matched, and should correspondingly
    !> preserve this match data.
    !>
    !> TYPE is 2 for a real error or 1 for warning or 0 for info.
    !> TYPE can also be given as WARNING and INFO.  In that case this will
    !> be equivalent to 1 if the WARNING'th subexpression matched or else
    !> equivalent to 0 if the INFO'th subexpression matched, or else
    !> equivalent to 2 if neither of them matched.  See
    !> `compilation_error_face', `compilation_warning_face',
    !> `compilation_info_face' and `compilation_skip_threshold'.
    !>
    !> What matched the HYPERLINK'th subexpression has `mouse_face' and
    !> `compilation_message_face' applied.  If this is 0, the text
    !> matched by the whole REGULAR_EXPRESSION becomes the hyperlink.
    !>
    !> Additional HIGHLIGHTs take the shape (SUBMATCH FACE), where
    !> SUBMATCH is the number of a submatch and FACE is a face name, or a
    !> function giving one.
    !>
    !> After identifying compilation errors and warnings determined by this
    !> variable, the `compilation_transform_file_matches' variable
    !> is then consulted.  It allows further transformations of the
    !> matched file names, and ignoring false positives.  Port of
    !> compile.el compilation-error-regexp-alist.
    character(len=64), allocatable, public :: compilation_error_regular_expression_names(:)

    !> If true, use case-insensitive matching of compilation errors.
    !> If false, matching is case-sensitive.
    !>
    !> Compilation errors are given by the regexps in
    !> `compilation_error_regular_expression_names' and
    !> `compilation_error_regular_expressions'.
    !>
    !> This variable should only be set for backward compatibility as a
    !> temporary measure.  The proper solution is to use a regexp that
    !> matches the messages without case-folding.  Port of compile.el
    !> compilation-error-case-fold-search.
    type(logical_setting_type), public :: compilation_error_case_fold_search = &
                                          logical_setting_type(name="compilation_error_case_fold_search", value=.false.)

    !> A value for tracking when directories are entered or left.
    !> If its regexp is unallocated, do not track directories, e.g. if all
    !> file names are absolute.  The REGULAR_EXPRESSION matches these messages.  It can
    !> match any number of variants, e.g. different languages.  The values
    !> DIRECTORY and LEAVE say: if the DIRECTORY'th subexpression matches, that is a
    !> directory name.  If LEAVE is 0 or the corresponding LEAVE'th
    !> subexpression doesn't match, this message is about going into
    !> another directory.  If it does match anything, this message is
    !> about going back to the directory we were in before the last
    !> entering message.  Port of compile.el compilation-directory-matcher.
    type(compilation_directory_matcher_type), public :: compilation_directory_matcher

    !> Table of regexp/replacements to alter file names in compiler
    !> messages.  If the replacement is unallocated, the matching
    !> message will not be considered an error or warning.  If not, it
    !> should be a replacement string for the matched regexp.
    !>
    !> If a replacement is specified, the value of the matched file name
    !> used to locate the warning or error is modified using the
    !> replacement, but the compilation buffer still displays the original
    !> value.  Port of compile.el compilation-transform-file-match-alist.
    type(compilation_transform_type), allocatable, public :: compilation_transform_file_matches(:)

    !> If true, column numbers in error messages are screen columns.
    !> Otherwise they are interpreted as character positions, with
    !> each character occupying one column.
    !> The default is to use screen columns, which requires that the
    !> compilation program and Fortmacs agree about the display width of the
    !> characters, especially the TAB character.
    !> If this is buffer-local in the destination buffer, Fortmacs obeys
    !> that value, otherwise it uses the value in the *compilation*
    !> buffer.  This enables a major mode to specify its own value.  Port
    !> of compile.el compilation-error-screen-columns.
    type(logical_setting_type), public :: compilation_error_screen_columns = &
                                          logical_setting_type(name="compilation_error_screen_columns", value=.true.)

    !> This is how compilers number the first column, usually 1 or 0.
    !> If this is buffer-local in the destination buffer, Fortmacs obeys
    !> that value, otherwise it uses the value in the *compilation*
    !> buffer.  This enables a major mode to specify its own value.  Port
    !> of compile.el compilation-first-column.
    type(integer_setting_type), public :: compilation_first_column = &
                                          integer_setting_type(name="compilation_first_column", value=1)

    !> Compilation motion commands skip less important messages.
    !> The value can be either 2 -- skip anything less than error, 1 --
    !> skip anything less than warning or 0 -- don't skip any messages.
    !> Note that all messages not positively identified as warning or
    !> info, are considered errors.  Port of compile.el
    !> compilation-skip-threshold.
    type(integer_setting_type), public :: compilation_skip_threshold = &
                                          integer_setting_type(name="compilation_skip_threshold", value=1)

    !> Compilation motion commands skip visited messages if this is true.
    !> Visited messages are ones for which the file, line and column have
    !> been jumped to from the current content in the current compilation
    !> buffer, even if it was from a different message.  Port of
    !> compile.el compilation-skip-visited.
    type(logical_setting_type), public :: compilation_skip_visited = &
                                          logical_setting_type(name="compilation_skip_visited", value=.false.)

    !> If true, skip multiple error messages for the same source location.
    !> Port of compile.el compilation-skip-to-next-location.
    type(logical_setting_type), public :: compilation_skip_to_next_location = &
                                          logical_setting_type(name="compilation_skip_to_next_location", value=.true.)

    !> True means that `compilation_find_file' searches all the
    !> directories that were entered before the message, not only its
    !> own.  Port of compile.el compilation-search-all-directories.
    type(logical_setting_type), public :: compilation_search_all_directories = &
                                          logical_setting_type(name="compilation_search_all_directories", value=.true.)

    !> Values of `compilation_context' besides a number of lines
    !> (`%lines'); it takes no .true. or .false..
    integer, parameter, public :: COMPILATION_CONTEXT_TOP = 2, COMPILATION_CONTEXT_NO_SCROLL = 3

    !> The setting `compilation_context': a number of lines (R6-39:
    !> `compilation_context%lines = 3'; negative when no number is set) or
    !> one of its named constants.  Not a port.
    type, extends(choice_setting_type), public :: compilation_context_choice_setting_type
        integer(int64) :: lines = -1
    contains
        procedure :: clear_kinds => compilation_context_clear_kinds
        procedure :: other_value => compilation_context_other_value
        procedure :: kind_hint => compilation_context_kind_hint
    end type compilation_context_choice_setting_type

    !> Display this many lines of leading context before the current
    !> message (`compilation_context%lines').  If COMPILATION_CONTEXT_TOP or
    !> COMPILATION_CONTEXT_NO_SCROLL, and the left fringe is displayed,
    !> don't scroll the compilation output window; an arrow in the left
    !> fringe points to the current message.  With no left fringe, if
    !> COMPILATION_CONTEXT_TOP, the message scrolls to the top of the
    !> window; there is no arrow.  If COMPILATION_CONTEXT_NO_SCROLL, don't
    !> scroll the compilation output window at all; an arrow before column
    !> zero points to the current message.  Port of compile.el
    !> compilation-context-lines.
    type(compilation_context_choice_setting_type), public :: compilation_context = &
                                                             compilation_context_choice_setting_type(name="compilation_context", &
                                                            true_valid=.false., false_valid=.false., choice=COMPILATION_CONTEXT_TOP)

    !> Face name to use for whole messages.
    !> Faces `compilation_error_face', `compilation_warning_face',
    !> `compilation_info_face', `compilation_line_face' and
    !> `compilation_column_face' get prepended to this, when applicable.
    !> Port of compile.el compilation-message-face.
    type(string_setting_type), public :: compilation_message_face = string_setting_type(name="compilation_message_face")

    !> Face name to use for file name in error messages.  Port of
    !> compile.el compilation-error-face.
    type(string_setting_type), public :: compilation_error_face = string_setting_type(name="compilation_error_face")

    !> Face name to use for file name in warning messages.  Port of
    !> compile.el compilation-warning-face.
    type(string_setting_type), public :: compilation_warning_face = string_setting_type(name="compilation_warning_face")

    !> Face name to use for file name in informational messages.  Port of
    !> compile.el compilation-info-face.
    type(string_setting_type), public :: compilation_info_face = string_setting_type(name="compilation_info_face")

    !> Face name to use for line numbers in compiler messages.  Port of
    !> compile.el compilation-line-face.
    type(string_setting_type), public :: compilation_line_face = string_setting_type(name="compilation_line_face")

    !> Face name to use for column numbers in compiler messages.  Port of
    !> compile.el compilation-column-face.
    type(string_setting_type), public :: compilation_column_face = string_setting_type(name="compilation_column_face")

    !> Face name to use for entering directory messages.  Port of
    !> compile.el compilation-enter-directory-face.
    type(string_setting_type), public :: compilation_enter_directory_face = &
                                         string_setting_type(name="compilation_enter_directory_face")

    !> Face name to use for leaving directory messages.  Port of
    !> compile.el compilation-leave-directory-face.
    type(string_setting_type), public :: compilation_leave_directory_face = &
                                         string_setting_type(name="compilation_leave_directory_face")

    !> The number of errors found so far (the value of buffers without a
    !> count of their own; compilation buffers have one).  Port of
    !> compile.el compilation-num-errors-found.
    integer(int64), public :: compilation_error_count_found = 0

    !> The number of warnings found so far.  Port of compile.el
    !> compilation-num-warnings-found.
    integer(int64), public :: compilation_warning_count_found = 0

    !> The number of informational messages found so far.  Port of
    !> compile.el compilation-num-infos-found.
    integer(int64), public :: compilation_info_count_found = 0

    !> Face used to highlight compiler errors.  Port of
    !> compile.el:compilation-error.
    type(named_face_type), target, public :: compilation_error

    !> Face used to highlight compiler warnings.  Port of
    !> compile.el:compilation-warning.
    type(named_face_type), target, public :: compilation_warning

    !> Face used to highlight compiler information.  Port of
    !> compile.el:compilation-info.
    type(named_face_type), target, public :: compilation_info

    !> Face for Compilation mode's "error" mode line indicator.  Port of
    !> compile.el:compilation-mode-line-fail.
    type(named_face_type), target, public :: compilation_mode_line_fail

    !> Face for Compilation mode's "running" mode line indicator.  Port of
    !> compile.el:compilation-mode-line-run.
    type(named_face_type), target, public :: compilation_mode_line_run

    !> Face for Compilation mode's "exit" mode line indicator.  Port of
    !> compile.el:compilation-mode-line-exit.
    type(named_face_type), target, public :: compilation_mode_line_exit

    !> Face for displaying line numbers in compiler messages.  Port of
    !> compile.el:compilation-line-number.
    type(named_face_type), target, public :: compilation_line_number

    !> Face for displaying column numbers in compiler messages.  Port of
    !> compile.el:compilation-column-number.
    type(named_face_type), target, public :: compilation_column_number

    ! The default stem of messages when no matches are found (compile.el
    ! compilation-error); see `error_stem'.
    character(len=*), parameter :: COMPILATION_ERROR_STEM = "error"

    ! A location (compile.el's LOCATION): COLUMN and LINE of FILE (a file
    ! structure), the MARKER pointing there once it is known, and whether
    ! it was VISITED.  FILLED is false while only the column is known (the
    ! loc just made by compilation-assq).
    type :: location_type
        logical :: has_column = .false.
        integer(int64) :: column = 0
        logical :: filled = .false.
        logical :: has_line = .false.
        integer(int64) :: line = 0
        integer :: file = 0
        type(handle_type) :: marker
        logical :: visited = .false.
    end type location_type

    ! The locations of a line of a file structure (LINE LOCATION...), the locs
    ! ordered by column, none first and then decreasing.
    type :: line_node_type
        logical :: has_line = .false.
        integer(int64) :: line = 0
        integer, allocatable :: locations(:)
    end type line_node_type

    ! A file structure ((FILENAME DIRECTORY) FORMATS (LINE LOCATION...) ...),
    ! the lines ordered by number, none first and then decreasing.
    type :: file_struct_type
        character(len=:), allocatable :: filename, directory
        type(line_node_type), allocatable :: lines(:)
    end type file_struct_type

    ! A key of compilation-locs: FILENAME and DIRECTORY as given (directory
    ! unallocated for none), and its file structure.
    type :: file_key_type
        character(len=:), allocatable :: filename, directory
        integer :: file = 0
    end type file_key_type

    ! A message (compilation--message): LOCATION, TYPE, END_LOCATION (0 for none)
    ! and RULE (unallocated for none).  `compilation_message' text
    ! properties hold its index.
    type :: message_type
        integer :: location = 0
        integer :: type = 2
        integer :: end_location = 0
        character(len=:), allocatable :: rule
    end type message_type

    ! An element of the stack of directories a `compilation_directory'
    ! text property holds: DIRECTORY (unallocated for nil) and the rest of the
    ! stack, PARENT (0 for the empty stack).
    type :: directory_node_type
        character(len=:), allocatable :: directory
        integer :: parent = 0
    end type directory_node_type

    ! What a compilation buffer keeps (its buffer-local variables): forgotten
    ! when its local variables are killed.
    type :: compilation_buffer_type
        type(handle_type) :: buffer
        ! compilation-arguments.
        logical :: has_arguments = .false.
        character(len=:), allocatable :: command
        logical :: command_interpreter = .false.
        ! compilation--start-time.
        integer(int64) :: start_count = 0
        ! compilation-directory, if the buffer has a value of its own.
        character(len=:), allocatable :: directory
        ! mode-line-process: 0 none, 1 running, 2 exited (EXIT_TEXT in
        ! EXIT_FACE).
        integer :: mode_line = 0
        character(len=:), allocatable :: exit_text, exit_face
        ! compilation-locs is local (compilation-setup).
        logical :: setup = .false.
        ! Its own compilation-num-*-found.
        logical :: has_counts = .false.
        integer(int64) :: error_count = 0, warning_count = 0, info_count = 0
        ! compilation--parsed: a marker, or none (-1).
        type(handle_type) :: parsed
        ! compilation-current-error: a marker, a position, or none.
        type(handle_type) :: current_error_marker
        logical :: has_current_error = .false.
        integer(int64) :: current_error_position = 0
        ! compilation-messages-start: a marker, a position, or none.
        type(handle_type) :: messages_start_marker
        logical :: has_messages_start = .false.
        integer(int64) :: messages_start_position = 0
        ! compilation-auto-jump-to-next.
        logical :: auto_jump_to_next = .false.
        ! The messages, locations, file structures, the keys of
        ! compilation-locs and the directory stacks.
        type(message_type), allocatable :: messages(:)
        integer :: nmessages = 0
        type(location_type), allocatable :: locations(:)
        integer :: nlocs = 0
        type(file_struct_type), allocatable :: files(:)
        integer :: nfiles = 0
        type(file_key_type), allocatable :: keys(:)
        integer :: nkeys = 0
        type(directory_node_type), allocatable :: directories(:)
        integer :: ndirs = 0
        ! The stack '(nil).
        integer :: false_directory = 0
        ! The MODE of compilation-arguments (null for compilation_mode).
        type(major_mode_type), pointer :: mode => null()
        ! What a mode made by define-compilation-mode set buffer-locally:
        ! compilation-error-regexp-alist (as rules),
        ! compilation-directory-matcher, compilation-transform-file-match-alist,
        ! compilation-error, compilation-mode-line-errors,
        ! compilation-process-setup-function and
        ! compilation-exit-message-function.
        logical :: has_rules = .false.
        type(compilation_rule_type), allocatable :: rules(:)
        logical :: has_matcher = .false.
        type(compilation_directory_matcher_type) :: matcher
        logical :: has_transforms = .false.
        type(compilation_transform_type), allocatable :: transforms(:)
        character(len=:), allocatable :: error
        procedure(compilation_mode_line_errors_procedure), pointer, nopass :: mode_line_errors => null()
        procedure(compilation_process_setup_procedure), pointer, nopass :: process_setup => null()
        procedure(compilation_exit_message_procedure), pointer, nopass :: exit_message => null()
    end type compilation_buffer_type

    type(compilation_buffer_type), allocatable :: compilation_buffers(:)

    ! compile.el compilation-in-progress: the running compilations.
    type(handle_type), allocatable :: compilation_in_progress(:)

    ! A rule spec read during a match: what compilation-error-properties
    ! computes.
    type :: error_specification_type
        character(len=:), allocatable :: filename, directory
        logical :: has_line = .false., has_end_line = .false., has_column = .false., has_end_column = .false.
        integer(int64) :: line = 0, end_line = 0, column = 0, end_column = 0
        integer :: type = 2
        character(len=:), allocatable :: rule
    end type error_specification_type

    character(len=*), parameter :: NEWLINE = achar(10), TB = achar(9), CR = achar(13)

contains

    ! ------------------------------------------------------------------
    ! The rules
    ! ------------------------------------------------------------------

    !> Add a rule NAME to `compilation_error_regular_expressions'.  Not a port.
    subroutine append_rule(name, regular_expression, file, line, end_line, column, end_column, type, warning, info, hyperlink)
        character(len=*), intent(in) :: name, regular_expression
        integer, intent(in), optional :: file, line, end_line, column, end_column, type, warning, info, hyperlink
        type(compilation_rule_type) :: rule

        if (.not. allocated(compilation_error_regular_expressions)) allocate (compilation_error_regular_expressions(0))
        rule%name = name
        rule%regular_expression = regular_expression
        if (present(file)) rule%file = file
        if (present(line)) rule%line = line
        if (present(end_line)) rule%end_line = end_line
        if (present(column)) rule%column = column
        if (present(end_column)) rule%end_column = end_column
        if (present(type)) rule%type = type
        if (present(warning)) rule%warning = warning
        if (present(info)) rule%info = info
        if (present(hyperlink)) rule%hyperlink = hyperlink
        allocate (rule%highlights(0))
        compilation_error_regular_expressions = [compilation_error_regular_expressions, rule]
    end subroutine append_rule

    !> Add a HIGHLIGHT to RULE: SUBMATCH in FACE (a name) or the face
    !> FACE_FUNCTION gives.  Not a port.
    subroutine append_highlight(rule, submatch, face, face_function)
        type(compilation_rule_type), intent(inout) :: rule
        integer, intent(in) :: submatch
        character(len=*), intent(in), optional :: face
        procedure(compilation_face_procedure), optional :: face_function
        type(compilation_highlight_type) :: highlight

        if (.not. allocated(rule%highlights)) allocate (rule%highlights(0))
        highlight%submatch = submatch
        if (present(face)) highlight%face = face
        if (present(face_function)) highlight%face_function => face_function
        rule%highlights = [rule%highlights, highlight]
    end subroutine append_highlight

    !> The last rule added.  Not a port.
    integer function last_rule()
        last_rule = size(compilation_error_regular_expressions)
    end function last_rule

    !> Set `compilation_error_regular_expressions' to the built-in rules, in
    !> the default matching order.  Port of the value of compile.el
    !> compilation-error-regexp-alist-alist (the `rx' forms written as
    !> regexps).
    subroutine initialize_compilation_error_regular_expressions()
        if (allocated(compilation_error_regular_expressions)) deallocate (compilation_error_regular_expressions)
        call append_rule("absoft", "^\(?:[Ee]rror on \|[Ww]arning on\( \)\)?[Ll]ine["//TB//" ]+\([0-9]+\)["//TB// &
                         " ]+of["//TB//' ]+"?\([a-zA-Z]?:?[^":'//NEWLINE//']+\)"?:', 3, 2, warning=1)
        call append_rule("ada", "\(warning: .*\)? at \([^ "//NEWLINE//"]+\):\([0-9]+\)$", 2, 3, warning=1)
        call append_rule("aix", " in line \([0-9]+\) of file \([^ "//NEWLINE//"]+[^. "//NEWLINE//"]\)\.? ", 2, 1)
        call append_rule("ant", "^[ "//TB//"]*\(?:\[[^] "//NEWLINE//"]+\][ "//TB//"]*\)\{1,2\}\(\(?:[A-Za-z]:\)?[^: "//NEWLINE// &
                         "]+\):\([0-9]+\):\(?:\([0-9]+\):\([0-9]+\):\([0-9]+\):\)?\( warning\)?", 1, 2, 4, 3, 5, warning=6)
        call append_rule("bash", "^\([^: "//NEWLINE//TB//"]+\): line \([0-9]+\):", 1, 2)
        call append_rule("borland", "^\(?:Error\|Warnin\(g\)\) \(?:[FEW][0-9]+ \)?\([a-zA-Z]?:?[^:( "//TB//NEWLINE// &
                         "]+\) \([0-9]+\)\(?:[) "//TB//"]\|:[^0-9"//NEWLINE//"]\)", 2, 3, warning=1)
        call append_rule("python_tracebacks_and_caml", "^[ "//TB//']*File \("?\)\([^," '//NEWLINE//TB// &
                         "<>]+\)\1, lines? \([0-9]+\)-?\([0-9]+\)?\(?:$\|,\(?: characters? \([0-9]+\)-?\([0-9]+\)?:\)?\([ " &
                         //NEWLINE//"]Warning\(?: [0-9]+\)?:\)?\)", 2, 3, 4, 5, 6, warning=7)
        call append_rule("cmake", "^CMake \(?:Error\|\(Warning\)\) at \(.*\):\([1-9][0-9]*\) ([^)]+):$", 2, 3, warning=1)
        call append_rule("cmake_info", "^  \(?: \*\)?\(.*\):\([1-9][0-9]*\) ([^)]+)$", 1, 2, type=0)
        call append_rule("comma", '^"\([^," '//NEWLINE//TB//']+\)", line \([0-9]+\)\(?:[(. pos]+\([0-9]+\))?\)?[:.,; (-]\( ' &
                         //"warning:\|[-0-9 ]*(W)\)?", 1, 2, column=3, warning=4)
        call append_rule("msft", "^ *\([0-9]+>\)?\(\(?:[a-zA-Z]:\)?[^ :("//TB//NEWLINE//"][^:("//TB//NEWLINE// &
                         "]*\)(\([0-9]+\)\(?:,\([0-9]+\)\)?) ?: \(?:see declaration\|\(?:warnin\(g\)\|[a-z ]+\) C[0-9]+:\)", &
                         2, 3, column=4, warning=5)
        call append_rule("edg_1", "^\([^ "//NEWLINE//"]+\)(\([0-9]+\)): \(?:error\|warnin\(g\)\|remar\(k\)\)", 1, 2, &
                         warning=3, info=4)
        call append_rule("edg_2", 'at line \([0-9]+\) of "\([^ '//NEWLINE//']+\)"$', 2, 1, type=0)
        call append_rule("epc", "^Error [0-9]+ at (\([0-9]+\):\([^)"//NEWLINE//"]+\))", 2, 1)
        call append_rule("ftnchek", &
                         "\(^Warning .*\)? line[ "//NEWLINE//"]\([0-9]+\)[ "//NEWLINE//"]\(?:col \([0-9]+\)[ "//NEWLINE// &
                         "]\)?file \([^ :;"//NEWLINE//"]+\)", 4, 2, column=3, warning=1)
        call append_rule("gradle_kotlin", "^\(?:\(w\)\|\([iv]\)\|e\): file://\(\(?:[A-Za-z]:\)?[^"//NEWLINE// &
                         ":]+\):\([[:digit:]]+\):\([[:digit:]]+\) ", 3, 4, column=5, warning=1, info=2)
        call append_rule("gradle_kotlin_legacy", "^\(?:\(w\)\|\([iv]\)\|e\): \(\(?:[A-Za-z]:\)?[^"//NEWLINE// &
                         ":]+\): (\([[:digit:]]+\), \([[:digit:]]+\)): ", 3, 4, column=5, warning=1, info=2)
        call append_rule("gradle_android", "^ *ERROR:\(?1:[^:"//NEWLINE//"]+\):\(?2:[[:digit:]]+\): ", 1, 2)
        call append_rule("iar", '^"\(.*\)",\([0-9]+\)\s-+\(?:Error\|Warnin\(g\)\)\[[0-9]+\]:', 1, 2, warning=3)
        call append_rule("ibm", "^\([^( "//NEWLINE//TB//"]+\)(\([0-9]+\):\([0-9]+\)) : \(?:warnin\(g\)\|informationa\(l\)\)?", &
                         1, 2, column=3, warning=4, info=5)
        call append_rule("irix", "^[-[:alnum:]_/ ]+: \(?:\(?:[sS]evere\|[eE]rror\|[wW]arnin\(g\)\|[iI]nf\(o\)\)[0-9 ]*: " &
                         //'\)?\([^," '//NEWLINE//TB//"]+\)\(?:, line\|:\) \([0-9]+\):", 3, 4, warning=1, info=2)
        call append_rule("java", "^\(?:[ "//TB//"]+at \|==[0-9]+== +\(?:at\|b\(y\)\)\).+(\([^()"//NEWLINE// &
                         "]+\):\([0-9]+\))$", 2, 3, warning=1)
        call append_rule("javac", "^\(\(?:[A-Za-z]:\)?[^"//NEWLINE//":]+\):\([0-9]+\): \(warning: \)?.*"//NEWLINE//".*"//NEWLINE// &
                         " *\^$", 1, 2, warning=3)
        compilation_error_regular_expressions(last_rule())%column_function => current_column_function
        call append_rule("jikes_file", '^\(?:Found\|Issued\) .* compiling "\(.+\)":$', 1, type=0)
        call append_rule("maven", "^\(?:\[\(?:ERROR\|\(?1:WARNING\)\|\(?2:INFO\)\)] \)?\(?3:[^"//NEWLINE//" []\(?:[^"//NEWLINE// &
                         " :]\| [^"//NEWLINE//"/-]\|:[^"//NEWLINE//" []\)*\):\[\(?4:[[:digit:]]+\),\(?5:[[:digit:]]+\)] ", 3, 4, &
                         column=5, warning=1, info=2)
        call append_rule("jikes_line", "^ *\([0-9]+\)\.[ "//TB//"]+.*"//NEWLINE//" +\(<-*>"//NEWLINE// &
                         "\*\*\* \(?:Error\|Warnin\(g\)\)\)", 0, 1, type=2, hyperlink=0)
        call append_highlight(compilation_error_regular_expressions(last_rule()), 2, face_function=face_of_type_3)
        call append_rule("clang_include", "^In file included from \([^"//NEWLINE//":]+\):\([0-9]+\):$", 1, 2, type=0)
        call append_rule("gcc_include", "^\(?:In file included \|                 \|"//TB//"\)from \([0-9]*[^0-9"//NEWLINE// &
                         "]\(?:[^"//NEWLINE//" :]\| [^-/"//NEWLINE//"]\|:[^ "//NEWLINE &
                         //"]\)*?\):\([0-9]+\)\(?::\([0-9]+\)\)?\(?:\([:,]\|$\)\)?", &
                         1, 2, column=3, info=4)
        call append_rule("ruby_Test::Unit", "^    [[ ]?\([^ (].*\):\([1-9][0-9]*\)\(\]\)?:in ", 1, 2)
        call append_rule("rust_panic", "^\(?:    \)?thread '.+' \(?:([[:digit:]]+) \)?\(?1:panicked\) at \(?2:.+\):" &
                         //"\(?3:[[:digit:]]+\):\(?4:[[:digit:]]+\):$", 2, 3, column=4)
        call append_highlight(compilation_error_regular_expressions(last_rule()), 1, face_function=error_face_value)
        call append_rule("rust", "^\(?:\(?1:error\)\|\(?2:warning\)\|\(?3:note\)\)\(?:\[[0-9A-Z]+]\)?:.*"//NEWLINE// &
                         " +--> \(?4:.+\):\(?5:[0-9]+\):\(?6:[0-9]+\)", 4, 5, column=6, warning=2, info=3)
        call append_highlight(compilation_error_regular_expressions(last_rule()), 1, face_function=error_face_value)
        call append_highlight(compilation_error_regular_expressions(last_rule()), 2, face_function=warning_face_value)
        call append_highlight(compilation_error_regular_expressions(last_rule()), 3, face_function=info_face_value)
        call append_rule("lua", &
                   "^[^"//TB//NEWLINE//"]+?: \([^"//TB//NEWLINE//"]+?\):\([0-9]+\): .+"//NEWLINE//"stack traceback:"//NEWLINE//TB, &
                         1, 2, type=2, hyperlink=1)
        call append_rule("lua_stack", "^"//TB//"\(?:\[C]:\|\([^"//TB//NEWLINE//"]+?\):\(?:\([0-9]+\):\)?\) in ", 1, 2, type=0, &
                         hyperlink=1)
        call append_rule("gmake", ": \*\*\* \[\(\(.+?\):\([0-9]+\): .+\)\]", 2, 3, type=0, hyperlink=1)
        call append_rule("gnu", "^\(?:[[:alpha:]][.[:alnum:]-]+: ?\| +|?\)?\(?1:\(?:[^0-9"//NEWLINE//TB//" ]\|[0-9]+[^0-9"// &
                         NEWLINE//"]\)\(?:[^"//NEWLINE//" :]\| [^/"//NEWLINE//"-]\|:[^ "//NEWLINE &
                         //"]\)*?\): ?\(?2:[0-9]+\)\(?:-\(?4:[0-9]+\)" &
                         //"\(?:\.\(?5:[0-9]+\)\)?\|[.:]\(?3:[0-9]+\)\(?:-\(?:\(?4:[0-9]+\)\.\)?\(?5:[0-9]+\)\)?\)?:\(?: *" &
                         //"\(?6:FutureWarning\|RuntimeWarning\|Warning\|warning\|W:\)\| *\(?7:Informational\|" &
                         //"informational\|Information\|information\|Info\|info\|I:\|instantiated from\|required from\|" &
                         //"Note\|note\|\[ skipping .+ ]\)\| *\(?:Error\|error\)\|[0-9]?\(?:[^0-9"//NEWLINE//"]\|$\)\|" &
                         //"[0-9][0-9][0-9]\)", 1, 2, 4, 3, 5, warning=6, info=7)
        call append_rule("cucumber", "\(?:^\(?:cucumber\(?: -p [^[:space:]]+\)?\|     \)\|#\) \([^(].*\):" &
                         //"\([1-9][0-9]*\)", 1, 2)
        call append_rule("lcc", "^\(?:E\|\(W\)\), \([^("//NEWLINE//"]+\)(\([0-9]+\),["//TB//" ]*\([0-9]+\)", 2, 3, column=4, &
                         warning=1)
        call append_rule("makepp", "^makepp\(?:\(?:: warning\(:\).*?\|\(: Scanning\|: [LR]e?l?oading makefile\|: " &
                         //"Imported\|log:.*?\) \|: .*?\)`\(\(\S +?\)\(?::\([0-9]+\)\)?\)['(]\)", 4, 5, warning=1, &
                         info=2, hyperlink=3)
        call append_highlight(compilation_error_regular_expressions(last_rule()), 0, face_function=makepp_highlight)
        call append_rule("mips_1", " (\([0-9]+\)) in \([^ "//NEWLINE//"]+\)", 2, 1)
        call append_rule("mips_2", " in \([^()"//NEWLINE//" ]+\)(\([0-9]+\))$", 1, 2)
        call append_rule("omake", "^\*\*\* omake: file \(.*\) changed", 1)
        call append_highlight(compilation_error_regular_expressions(last_rule()), 0, face_function=omake_highlight)
        call append_rule("oracle", "^\(?:Semantic error\|Error\|PCC-[0-9]+:\).* line \([0-9]+\)\(?:\(?:,\| at\)? column " &
                         //"\([0-9]+\)\)?\(?:,\| in\| of\)? file \(.*?\):?$", 3, 1, column=2)
        call append_rule("perl", " at \([^ "//NEWLINE//"]+\) line \([0-9]+\)\(?:[,.]\|$\| during global destruction\.$\)", 1, 2)
        call append_rule("php", "\(?:Parse\|Fatal\) error: \(.*\) in \(.*\) on line \([0-9]+\)", 2, 3)
        call append_rule("rxp", "^\(?:Error\|Warnin\(g\)\):.*"//NEWLINE//".* line \([0-9]+\) char \([0-9]+\) of " &
                         //"file://\(.+\)", 4, 2, column=3, warning=1)
        call append_rule("shellcheck", "^In \(.+\) line \([0-9]+\):", 1, 2)
        call append_rule("sparc_pascal_file", "^\w\w\w \w\w\w +[0-3]?[0-9] +[0-2][0-9]:[0-5][0-9]:[0-5][0-9] " &
                         //"[12][09][0-9][0-9] +\(.*\):$", 1, type=0)
        call append_rule("sparc_pascal_line", "^\(\(?:E\|\(w\)\) +[0-9]+\) line \([0-9]+\) -  ", 0, 3, warning=2)
        call append_highlight(compilation_error_regular_expressions(last_rule()), 1, face_function=face_of_type_2)
        call append_rule("sparc_pascal_example", "^ +\([0-9]+\) +.*"//NEWLINE//"\(\(?:e\|\(w\)\) [0-9]+\)-+", 0, 1, warning=3)
        call append_highlight(compilation_error_regular_expressions(last_rule()), 2, face_function=face_of_type_3)
        call append_rule("sun", ": \(?:ERROR\|WARNIN\(G\)\|REMAR\(K\)\) \(?:[[:alnum:] ]+, \)?File = \(.+\), Line = " &
                         //"\([0-9]+\)\(?:, Column = \([0-9]+\)\)?", 3, 4, column=5, warning=1, info=2)
        call append_rule("sun_ada", "^\([^, "//NEWLINE//TB//"]+\), line \([0-9]+\), char \([0-9]+\)[:., (-]", 1, 2, column=3)
        call append_rule("watcom", "^[ "//TB//"]*\(\(?:[a-zA-Z]:\)?[^ :("//TB//NEWLINE//"][^:("//TB//NEWLINE// &
                         "]*\)(\([0-9]+\)): ?\(?:\(Error! E[0-9]+\)\|\(Warning! W[0-9]+\)\):", 1, 2, warning=4)
        call append_rule("4bsd", "\(?:^\|::  \|\S ( \)\(/[^ "//NEWLINE//TB//"()]+\)(\([0-9]+\))\(?:: \(warning:\)?\|$\| ),\)", &
                         1, 2, warning=3)
        call append_rule("gcov_file", "^ *-: *\(0\):Source:\(.+\)$", 2, 1, type=0)
        call append_rule("gcov_header", "^ *-: *\(0\):\(?:Object\|Graph\|Data\|Runs\|Programs\):.+$", 0, 1, type=0)
        call append_rule("gcov_nomark", "^ *-: *\([1-9]\|[0-9]\{2,\}\):.*$", 0, 1, type=0)
        call append_highlight(compilation_error_regular_expressions(last_rule()), 0, face="default")
        call append_highlight(compilation_error_regular_expressions(last_rule()), 1, face_function=line_face_value)
        call append_rule("gcov_called_line", "^ *\([0-9]+\): *\([0-9]+\):.*$", 0, 2, type=0)
        call append_highlight(compilation_error_regular_expressions(last_rule()), 0, face="default")
        call append_highlight(compilation_error_regular_expressions(last_rule()), 1, face_function=info_face_value)
        call append_highlight(compilation_error_regular_expressions(last_rule()), 2, face_function=line_face_value)
        call append_rule("gcov_never_called", "^ *\(#####\): *\([0-9]+\):.*$", 0, 2, type=2)
        call append_highlight(compilation_error_regular_expressions(last_rule()), 0, face="default")
        call append_highlight(compilation_error_regular_expressions(last_rule()), 1, face_function=error_face_value)
        call append_highlight(compilation_error_regular_expressions(last_rule()), 2, face_function=line_face_value)
        call append_rule("perl__Pod::Checker", "^\*\*\* \(?:ERROR\|\(WARNING\)\).* \(?:at\|on\) line \([0-9]+\) " &
                         //"\(?:.* \)?in file \([^ "//TB//NEWLINE//"]+\)", 3, 2, warning=1)
        call append_rule("perl__Test", "^# Failed test [0-9]+ in \([^ "//TB//CR//NEWLINE//"]+\) at line \([0-9]+\)", 1, 2)
        call append_rule("perl__Test2", "^\(.*NOK.*\)?# Test [0-9]+ got:.* (\([^ "//TB//CR//NEWLINE// &
                         "]+\) at line \([0-9]+\)\( fail #[0-9]+\)?)", 2, 3)
        call append_rule("perl__Test::Harness", "^.*NOK.* \([^ "//TB//CR//NEWLINE//"]+\) at line \([0-9]+\)", 1, 2)
        call append_rule("weblint", "^\([^ "//TB//CR//NEWLINE//"(]+\) (\([0-9]+\):\([0-9]+\)) ", 1, 2, column=3)
        call append_rule("guile_file", "^In \(.+\..+\):"//NEWLINE, 1, type=0)
        call append_rule("guile_line", "^ *\([0-9]+\): *\([0-9]+\)", 0, 1, column=2)
        call append_rule("typescript_tsc_plain", "^\([^ "//TB//NEWLINE//"()][^"//NEWLINE//"()]*\)(\([0-9]+\),\([0-9]+\)): error " &
                         //"[0-9A-Z]+: ", 1, 2, column=3, type=2)
        call append_rule("typescript_tsc_pretty", "^\([^ "//TB//NEWLINE//"()][^"//NEWLINE//"()]*\):\([0-9]+\):\([0-9]+\) - error " &
                         //"[0-9A-Z]+: ", 1, 2, column=3, type=2)
    end subroutine initialize_compilation_error_regular_expressions

    !> The default `compilation_error_regular_expression_names': every rule but
    !> `omake' (its mere presence triggers special processing and modifies
    !> the regexps of other rules, see `compilation_parse_errors').
    !> Not a port.
    subroutine initialize_compilation_error_regular_expression_names()
        integer :: k, n

        n = 0

        do k = 1, size(compilation_error_regular_expressions)
            if (compilation_error_regular_expressions(k)%name /= "omake") n = n + 1
        end do

        if (allocated(compilation_error_regular_expression_names)) deallocate (compilation_error_regular_expression_names)
        allocate (compilation_error_regular_expression_names(n))
        n = 0

        do k = 1, size(compilation_error_regular_expressions)
            if (compilation_error_regular_expressions(k)%name == "omake") cycle
            n = n + 1
            compilation_error_regular_expression_names(n) = compilation_error_regular_expressions(k)%name
        end do
    end subroutine initialize_compilation_error_regular_expression_names

    !> The column of point (the javac rule's COLUMN: #'current-column).
    !> Not a port.
    subroutine current_column_function(number, found)
        integer(int64), intent(out) :: number
        logical, intent(out) :: found

        number = current_column()
        found = .true.
    end subroutine current_column_function

    !> (compilation-face '(2)) for a HIGHLIGHT.  Not a port.
    recursive subroutine face_of_type_2(face, status)
        character(len=:), allocatable, intent(out) :: face
        type(status_type), intent(out) :: status

        status = status_type()
        face = compilation_face(2, 0)
    end subroutine face_of_type_2

    !> (compilation-face '(3)) for a HIGHLIGHT.  Not a port.
    recursive subroutine face_of_type_3(face, status)
        character(len=:), allocatable, intent(out) :: face
        type(status_type), intent(out) :: status

        status = status_type()
        face = compilation_face(3, 0)
    end subroutine face_of_type_3

    !> `compilation_error_face' for a HIGHLIGHT.  Not a port.
    recursive subroutine error_face_value(face, status)
        character(len=:), allocatable, intent(out) :: face
        type(status_type), intent(out) :: status

        status = status_type()
        if (allocated(compilation_error_face%value)) face = compilation_error_face%value
    end subroutine error_face_value

    !> `compilation_warning_face' for a HIGHLIGHT.  Not a port.
    recursive subroutine warning_face_value(face, status)
        character(len=:), allocatable, intent(out) :: face
        type(status_type), intent(out) :: status

        status = status_type()
        if (allocated(compilation_warning_face%value)) face = compilation_warning_face%value
    end subroutine warning_face_value

    !> `compilation_info_face' for a HIGHLIGHT.  Not a port.
    recursive subroutine info_face_value(face, status)
        character(len=:), allocatable, intent(out) :: face
        type(status_type), intent(out) :: status

        status = status_type()
        if (allocated(compilation_info_face%value)) face = compilation_info_face%value
    end subroutine info_face_value

    !> `compilation_line_face' for a HIGHLIGHT.  Not a port.
    recursive subroutine line_face_value(face, status)
        character(len=:), allocatable, intent(out) :: face
        type(status_type), intent(out) :: status

        status = status_type()
        if (allocated(compilation_line_face%value)) face = compilation_line_face%value
    end subroutine line_face_value

    !> The HIGHLIGHT of the makepp rule: parse the other file names of the
    !> line (all but the first), then go to the end of the line; no face.
    !> Not a port.
    recursive subroutine makepp_highlight(face, status)
        character(len=:), allocatable, intent(out) :: face
        type(status_type), intent(out) :: status
        type(compilation_rule_type) :: rules(1)
        integer(int64), allocatable :: saved(:)
        type(status_type) :: st
        integer(int64) :: from

        saved = match_data()
        rules(1)%regular_expression = "`\(\(\S +?\)\(?::\([0-9]+\)\)?\)['(]"
        rules(1)%file = 2
        rules(1)%line = 3

        if (match_end(1, st) /= 0) then
            rules(1)%type = 1
        else if (match_end(2, st) /= 0) then
            rules(1)%type = 0
        else
            rules(1)%type = 2
        end if

        rules(1)%hyperlink = 1
        allocate (rules(1)%highlights(0))
        from = match_end(0, st)
        call compilation_parse_errors(from, line_end_position(), status, rules)
        call set_match_data(saved)
        call end_of_line()
    end subroutine makepp_highlight

    !> The HIGHLIGHT of the omake rule: forget the locations of the file
    !> about to be recompiled; no face.  Not a port.
    recursive subroutine omake_highlight(face, status)
        character(len=:), allocatable, intent(out) :: face
        type(status_type), intent(out) :: status

        status = status_type()
        call compilation_flush_file_structure(match_string(1))
    end subroutine omake_highlight

    ! ------------------------------------------------------------------
    ! Compilation buffers
    ! ------------------------------------------------------------------

    ! Not a port.
    logical function same(a, b)
        type(handle_type), intent(in) :: a, b

        same = a%index == b%index .and. a%generation == b%generation
    end function same

    !> The entry of buffer `h' in `compilation_buffers' (0 for none).
    !> Not a port.
    integer function entry_of(h) result(k)
        type(handle_type), intent(in) :: h

        if (allocated(compilation_buffers) .and. h%index /= 0) then
            do k = 1, size(compilation_buffers)
                if (same(compilation_buffers(k)%buffer, h)) return
            end do
        end if

        k = 0
    end function entry_of

    !> Find or make the entry of the current buffer (in the slot of a dead
    !> buffer if there is one), its index K.  Not a port.
    subroutine make_current_entry(k)
        integer, intent(out) :: k
        type(compilation_buffer_type) :: entry

        k = entry_of(current_buffer())
        if (k /= 0) return
        if (.not. allocated(compilation_buffers)) allocate (compilation_buffers(0))

        do k = 1, size(compilation_buffers)
            if (.not. is_buffer_live(compilation_buffers(k)%buffer)) then
                call reset_entry(k)
                compilation_buffers(k)%buffer = current_buffer()
                return
            end if
        end do

        entry%buffer = current_buffer()
        compilation_buffers = [compilation_buffers, entry]
        k = size(compilation_buffers)
    end subroutine make_current_entry

    !> Free what entry K holds and make it unused.  Not a port.
    subroutine reset_entry(k)
        integer, intent(in) :: k
        type(compilation_buffer_type) :: empty
        type(status_type) :: st
        integer :: i

        do i = 1, compilation_buffers(k)%nlocs
            if (compilation_buffers(k)%locations(i)%marker%index /= 0) call destroy_marker(compilation_buffers(k)%locations( &
                                                                                           i)%marker, st)
        end do

        if (compilation_buffers(k)%parsed%index /= 0) call destroy_marker(compilation_buffers(k)%parsed, st)

        if (compilation_buffers(k)%current_error_marker%index /= 0) then
            call destroy_marker(compilation_buffers(k)%current_error_marker, st)
        end if

        if (compilation_buffers(k)%messages_start_marker%index /= 0) then
            call destroy_marker(compilation_buffers(k)%messages_start_marker, st)
        end if

        compilation_buffers(k) = empty
    end subroutine reset_entry

    !> Forget what the current buffer had as a compilation buffer, when
    !> `clear_all_local_variables' runs.  Not a port.
    subroutine clear_compilation_locals()
        integer :: k

        k = entry_of(current_buffer())
        if (k /= 0) call reset_entry(k)
    end subroutine clear_compilation_locals

    !> Give the current buffer its own values of the compilation variables,
    !> as the body of a mode made by define-compilation-mode does with
    !> `set_local' (after `compilation_mode' has run as its parent):
    !> `compilation_error_regular_expression_names' as the rules ERROR_REGULAR_EXPRESSIONS,
    !> `compilation_directory_matcher', `compilation_transform_file_matches',
    !> `compilation_error' (the word for a message, "error" by default),
    !> `compilation_mode_font_lock_keywords' (MODE_FONT_LOCK_KEYWORDS: what
    !> Font Lock highlights after the parse),
    !> `compilation_mode_line_errors', `compilation_process_setup_procedure'
    !> and `compilation_exit_message_procedure'.  The settings
    !> (`compilation_error_face', `compilation_scroll_output',
    !> `compilation_first_column' and so on) take `set_local' directly.
    !> Cf. compile.el:define-compilation-mode.  Not a port.
    subroutine compilation_set_mode_locals(error_regular_expressions, directory_matcher, transform_file_match_table, &
                                           error, mode_font_lock_keywords, mode_line_errors, process_setup_function, &
                                           exit_message_function)
        type(compilation_rule_type), intent(in), optional :: error_regular_expressions(:)
        type(compilation_directory_matcher_type), intent(in), optional :: directory_matcher
        type(compilation_transform_type), intent(in), optional :: transform_file_match_table(:)
        character(len=*), intent(in), optional :: error
        type(font_lock_keyword_type), intent(in), optional :: mode_font_lock_keywords(:)
        procedure(compilation_mode_line_errors_procedure), optional :: mode_line_errors
        procedure(compilation_process_setup_procedure), optional :: process_setup_function
        procedure(compilation_exit_message_procedure), optional :: exit_message_function
        type(font_lock_keyword_type), allocatable :: keywords(:)
        integer :: k, i

        call make_current_entry(k)

        if (present(error_regular_expressions)) then
            compilation_buffers(k)%has_rules = .true.
            allocate (compilation_buffers(k)%rules(size(error_regular_expressions)))

            do i = 1, size(error_regular_expressions)
                compilation_buffers(k)%rules(i) = error_regular_expressions(i)
            end do
        end if

        if (present(directory_matcher)) then
            compilation_buffers(k)%has_matcher = .true.
            compilation_buffers(k)%matcher = directory_matcher
        end if

        if (present(transform_file_match_table)) then
            compilation_buffers(k)%has_transforms = .true.
            allocate (compilation_buffers(k)%transforms(size(transform_file_match_table)))

            do i = 1, size(transform_file_match_table)
                compilation_buffers(k)%transforms(i) = transform_file_match_table(i)
            end do
        end if

        if (present(error)) compilation_buffers(k)%error = error

        if (present(mode_line_errors)) then
            compilation_buffers(k)%mode_line_errors => mode_line_errors
            call update_mode_line_process(k)
        end if

        if (present(process_setup_function)) compilation_buffers(k)%process_setup => process_setup_function
        if (present(exit_message_function)) compilation_buffers(k)%exit_message => exit_message_function

        if (present(mode_font_lock_keywords)) then
            ! (compilation-mode-font-lock-keywords): parsing, then these.
            allocate (keywords(size(mode_font_lock_keywords) + 1))
            keywords(1)%matcher => ensure_parse_matcher
            allocate (keywords(1)%highlights(0))

            do i = 1, size(mode_font_lock_keywords)
                keywords(i + 1) = mode_font_lock_keywords(i)
            end do

            call font_lock_defaults%set_local(keywords=keywords, keywords_only=.true.)
        end if
    end subroutine compilation_set_mode_locals

    !> The current buffer's `compilation_error': the word for a message.
    !> Not a port.
    function error_stem() result(word)
        character(len=:), allocatable :: word
        integer :: k

        word = COMPILATION_ERROR_STEM
        k = entry_of(current_buffer())
        if (k == 0) return
        if (allocated(compilation_buffers(k)%error)) word = compilation_buffers(k)%error
    end function error_stem

    !> Test if inside a compilation buffer.  Port of
    !> compile.el:compilation-buffer-internal-p.
    logical function is_compilation_buffer_internal()
        integer :: k

        is_compilation_buffer_internal = .false.
        k = entry_of(current_buffer())
        if (k /= 0) is_compilation_buffer_internal = compilation_buffers(k)%setup
    end function is_compilation_buffer_internal

    !> Test if BUFFER is a compilation buffer.  Port of
    !> compile.el:compilation-buffer-p.
    logical function is_compilation_buffer(buffer)
        type(handle_type), intent(in) :: buffer
        integer :: k

        is_compilation_buffer = .false.
        k = entry_of(buffer)
        if (k /= 0) is_compilation_buffer = compilation_buffers(k)%setup
    end function is_compilation_buffer

    !> Return a compilation buffer.
    !> If AVOID_CURRENT is false, and the current buffer is a compilation
    !> buffer, return it.  If AVOID_CURRENT is true, return the current
    !> buffer only as a last resort.  Port of
    !> compile.el:compilation-find-buffer.
    recursive subroutine compilation_find_buffer(buffer, status, avoid_current)
        type(handle_type), intent(out) :: buffer
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: avoid_current
        logical :: avoid

        avoid = .false.
        if (present(avoid_current)) avoid = avoid_current
        status = status_type()

        if (is_compilation_buffer_internal() .and. .not. avoid) then
            buffer = current_buffer()
            return
        end if

        call next_error_find_buffer(buffer, status, avoid, is_compilation_buffer_internal)
    end subroutine compilation_find_buffer

    !> The counts of errors, warnings and informational messages found so
    !> far in the current buffer (`compilation_error_count_found' and
    !> friends).  Not a port.
    subroutine compilation_message_counts(errors, warnings, infos)
        integer(int64), intent(out) :: errors, warnings, infos
        integer :: k

        errors = compilation_error_count_found
        warnings = compilation_warning_count_found
        infos = compilation_info_count_found
        k = entry_of(current_buffer())
        if (k == 0) return
        if (.not. compilation_buffers(k)%has_counts) return
        errors = compilation_buffers(k)%error_count
        warnings = compilation_buffers(k)%warning_count
        infos = compilation_buffers(k)%info_count
    end subroutine compilation_message_counts

    ! ------------------------------------------------------------------
    ! Messages, locations and file structures
    ! ------------------------------------------------------------------

    !> A new message in entry K.  Not a port.
    integer function make_message(k, location, type, end_location, rule) result(id)
        integer, intent(in) :: k, location, type, end_location
        character(len=*), intent(in), optional :: rule
        type(message_type), allocatable :: grown(:)
        integer :: i, n

        if (.not. allocated(compilation_buffers(k)%messages)) allocate (compilation_buffers(k)%messages(16))
        n = compilation_buffers(k)%nmessages

        if (n == size(compilation_buffers(k)%messages)) then
            allocate (grown(2*n))

            do i = 1, n
                grown(i)%location = compilation_buffers(k)%messages(i)%location
                grown(i)%type = compilation_buffers(k)%messages(i)%type
                grown(i)%end_location = compilation_buffers(k)%messages(i)%end_location

                if (allocated(compilation_buffers(k)%messages(i)%rule)) then
                    call move_alloc(compilation_buffers(k)%messages(i)%rule, grown(i)%rule)
                end if
            end do

            call move_alloc(grown, compilation_buffers(k)%messages)
        end if

        id = n + 1
        compilation_buffers(k)%nmessages = id
        compilation_buffers(k)%messages(id)%location = location
        compilation_buffers(k)%messages(id)%type = type
        compilation_buffers(k)%messages(id)%end_location = end_location
        if (allocated(compilation_buffers(k)%messages(id)%rule)) deallocate (compilation_buffers(k)%messages(id)%rule)
        if (present(rule)) compilation_buffers(k)%messages(id)%rule = rule
    end function make_message

    !> A new location of column COLUMN (none unless HAS_COLUMN) in entry K.
    !> Not a port.
    integer function make_location(k, has_column, column) result(id)
        integer, intent(in) :: k
        logical, intent(in) :: has_column
        integer(int64), intent(in) :: column
        type(location_type), allocatable :: grown(:)
        integer :: n

        if (.not. allocated(compilation_buffers(k)%locations)) allocate (compilation_buffers(k)%locations(16))
        n = compilation_buffers(k)%nlocs

        if (n == size(compilation_buffers(k)%locations)) then
            allocate (grown(2*n))
            grown(1:n) = compilation_buffers(k)%locations(1:n)
            call move_alloc(grown, compilation_buffers(k)%locations)
        end if

        id = n + 1
        compilation_buffers(k)%nlocs = id
        compilation_buffers(k)%locations(id) = location_type()
        compilation_buffers(k)%locations(id)%has_column = has_column
        compilation_buffers(k)%locations(id)%column = column
    end function make_location

    !> A new directory stack node in entry K: DIRECTORY (if present) on PARENT.
    !> Not a port.
    integer function create_directory(k, parent, directory) result(id)
        integer, intent(in) :: k, parent
        character(len=*), intent(in), optional :: directory
        type(directory_node_type), allocatable :: grown(:)
        integer :: i, n

        if (.not. allocated(compilation_buffers(k)%directories)) allocate (compilation_buffers(k)%directories(16))
        n = compilation_buffers(k)%ndirs

        if (n == size(compilation_buffers(k)%directories)) then
            allocate (grown(2*n))

            do i = 1, n
                grown(i)%parent = compilation_buffers(k)%directories(i)%parent

                if (allocated(compilation_buffers(k)%directories(i)%directory)) then
                    call move_alloc(compilation_buffers(k)%directories(i)%directory, grown(i)%directory)
                end if
            end do

            call move_alloc(grown, compilation_buffers(k)%directories)
        end if

        id = n + 1
        compilation_buffers(k)%ndirs = id
        compilation_buffers(k)%directories(id)%parent = parent
        if (allocated(compilation_buffers(k)%directories(id)%directory)) deallocate (compilation_buffers(k)%directories( &
                                                                                     id)%directory)
        if (present(directory)) compilation_buffers(k)%directories(id)%directory = directory
    end function create_directory

    !> The message whose index the `compilation_message' property at POSITION
    !> holds (0 for none).  Not a port.
    integer function message_at(position) result(id)
        integer(int64), intent(in) :: position

        id = property_id_at(position, "compilation_message")

        if (id /= 0) then
            if (id > compilation_buffers(entry_of(current_buffer()))%nmessages) id = 0
        end if
    end function message_at

    !> The directory stack the `compilation_directory' property at POSITION
    !> holds (0 for none).  Not a port.
    integer function directory_at(position) result(id)
        integer(int64), intent(in) :: position

        id = property_id_at(position, "compilation_directory")

        if (id /= 0) then
            if (id > compilation_buffers(entry_of(current_buffer()))%ndirs) id = 0
        end if
    end function directory_at

    !> The index the property PROPERTY at POSITION holds, 0 for none (or when the
    !> current buffer has no entry).  Not a port.
    integer function property_id_at(position, property) result(id)
        integer(int64), intent(in) :: position
        character(len=*), intent(in) :: property
        type(value_type) :: v
        type(status_type) :: st

        id = 0
        if (entry_of(current_buffer()) == 0) return
        if (position < 1) return
        call get_text_property(position, value_type(name=property), v, st)
        if (.not. st%ok()) return
        if (vtag(v) /= V_INTEGER) return
        if (xfixnum(v) < 1 .or. xfixnum(v) > huge(0)) return
        id = int(xfixnum(v))
    end function property_id_at

    !> The directory stack in effect at POSITION: the `compilation_directory'
    !> property at the previous change of it (0 for none).  Port of
    !> compile.el:compilation--previous-directory (without its cache).
    integer function directory_before(position) result(id)
        integer(int64), intent(in) :: position
        integer(int64) :: previous
        logical :: found
        type(status_type) :: st

        id = 0
        call previous_single_property_change(position, value_type(name="compilation_directory"), previous, found, st)
        if (.not. found) return
        id = directory_at(previous - 1)
        if (id == 0) id = directory_at(previous)
    end function directory_before

    !> Retrieve FILE's file-structure or create a new one.  FILE is
    !> FILENAME, relative to DIRECTORY if present; FILENAME may be relative
    !> or absolute.  The file structure's directory is the truename of
    !> DIRECTORY.  Port of compile.el:compilation-get-file-structure.
    recursive integer function compilation_get_file_structure(k, filename, directory) result(fs)
        integer, intent(in) :: k
        character(len=*), intent(in) :: filename
        character(len=*), intent(in), optional :: directory
        character(len=:), allocatable :: name, specification
        type(status_type) :: st
        logical :: has_specification

        fs = find_key(k, filename, directory)
        if (fs /= 0) return
        ! File was not previously encountered, at least not in the form
        ! passed.  Let's normalize it and look again.
        name = filename
        has_specification = present(directory)

        if (has_specification) then
            call file_truename(directory, specification, st)
            if (.not. st%ok()) specification = directory
        end if

        ! Some compilers produce bogus file names like "./bar//foo.c" for
        ! file "bar/foo.c"; expand_file_name would collapse these into
        ! "/foo.c" and fail to find the appropriate file.  So we look for
        ! doubled slashes in the file name and fix them.
        name = command_line_normalize_file_name(name)

        ! Retrieve or create file-structure for normalized name.
        if (has_specification) then
            fs = find_key(k, name, specification)
        else
            fs = find_key(k, name)
        end if

        if (fs == 0) then
            fs = make_file(k, name, has_specification, specification)

            if (has_specification) then
                call append_key(k, name, fs, specification)
            else
                call append_key(k, name, fs)
            end if
        end if

        ! Store it for the possibly unnormalized name.
        if (present(directory)) then
            call append_key(k, filename, fs, directory)
        else
            call append_key(k, filename, fs)
        end if
    end function compilation_get_file_structure

    !> The file structure of the key FILENAME . DIRECTORY (0 for none).
    !> Not a port.
    integer function find_key(k, filename, directory) result(fs)
        integer, intent(in) :: k
        character(len=*), intent(in) :: filename
        character(len=*), intent(in), optional :: directory
        integer :: i

        fs = 0

        do i = 1, compilation_buffers(k)%nkeys
            associate (key => compilation_buffers(k)%keys(i))
                if (.not. same_string(key%filename, filename)) cycle
                if (present(directory) .neqv. allocated(key%directory)) cycle

                if (present(directory)) then
                    if (.not. same_string(key%directory, directory)) cycle
                end if

                fs = key%file
                return
            end associate
        end do
    end function find_key

    ! Not a port.
    logical function same_string(a, b)
        character(len=*), intent(in) :: a, b

        same_string = len(a) == len(b)
        if (same_string) same_string = a == b
    end function same_string

    ! Not a port.
    subroutine append_key(k, filename, fs, directory)
        integer, intent(in) :: k, fs
        character(len=*), intent(in) :: filename
        character(len=*), intent(in), optional :: directory
        type(file_key_type), allocatable :: grown(:)
        integer :: i, n

        if (present(directory)) then
            if (find_key(k, filename, directory) /= 0) return
        else
            if (find_key(k, filename) /= 0) return
        end if

        if (.not. allocated(compilation_buffers(k)%keys)) allocate (compilation_buffers(k)%keys(0))
        n = compilation_buffers(k)%nkeys

        if (n == size(compilation_buffers(k)%keys)) then
            allocate (grown(max(16, 2*n)))

            do i = 1, n
                call move_alloc(compilation_buffers(k)%keys(i)%filename, grown(i)%filename)

                if (allocated(compilation_buffers(k)%keys(i)%directory)) then
                    call move_alloc(compilation_buffers(k)%keys(i)%directory, grown(i)%directory)
                end if

                grown(i)%file = compilation_buffers(k)%keys(i)%file
            end do

            call move_alloc(grown, compilation_buffers(k)%keys)
        end if

        n = n + 1
        compilation_buffers(k)%nkeys = n
        compilation_buffers(k)%keys(n)%filename = filename
        if (allocated(compilation_buffers(k)%keys(n)%directory)) deallocate (compilation_buffers(k)%keys(n)%directory)
        if (present(directory)) compilation_buffers(k)%keys(n)%directory = directory
        compilation_buffers(k)%keys(n)%file = fs
    end subroutine append_key

    ! Not a port.
    integer function make_file(k, filename, has_directory, directory) result(fs)
        integer, intent(in) :: k
        character(len=*), intent(in) :: filename
        logical, intent(in) :: has_directory
        character(len=:), allocatable, intent(in) :: directory
        type(file_struct_type), allocatable :: grown(:)
        integer :: i, n

        if (.not. allocated(compilation_buffers(k)%files)) allocate (compilation_buffers(k)%files(0))
        n = compilation_buffers(k)%nfiles

        if (n == size(compilation_buffers(k)%files)) then
            allocate (grown(max(8, 2*n)))

            do i = 1, n
                call move_alloc(compilation_buffers(k)%files(i)%filename, grown(i)%filename)

                if (allocated(compilation_buffers(k)%files(i)%directory)) then
                    call move_alloc(compilation_buffers(k)%files(i)%directory, grown(i)%directory)
                end if

                call move_alloc(compilation_buffers(k)%files(i)%lines, grown(i)%lines)
            end do

            call move_alloc(grown, compilation_buffers(k)%files)
        end if

        fs = n + 1
        compilation_buffers(k)%nfiles = fs
        compilation_buffers(k)%files(fs)%filename = filename
        if (allocated(compilation_buffers(k)%files(fs)%directory)) deallocate (compilation_buffers(k)%files(fs)%directory)
        if (has_directory) compilation_buffers(k)%files(fs)%directory = directory
        if (allocated(compilation_buffers(k)%files(fs)%lines)) deallocate (compilation_buffers(k)%files(fs)%lines)
        allocate (compilation_buffers(k)%files(fs)%lines(0))
    end function make_file

    !> Forget the file structure of FILE (a name), so that later messages
    !> about it get new locations.  Port of
    !> compile.el:compilation--flush-file-structure.
    recursive subroutine compilation_flush_file_structure(file)
        character(len=*), intent(in) :: file
        integer :: k, fs, i, n

        call make_current_entry(k)
        fs = compilation_get_file_structure(k, file)
        n = 0

        do i = 1, compilation_buffers(k)%nkeys
            if (compilation_buffers(k)%keys(i)%file == fs) cycle
            n = n + 1

            if (n /= i) then
                call move_alloc(compilation_buffers(k)%keys(i)%filename, compilation_buffers(k)%keys(n)%filename)

                if (allocated(compilation_buffers(k)%keys(n)%directory)) then
                    deallocate (compilation_buffers(k)%keys(n)%directory)
                end if

                if (allocated(compilation_buffers(k)%keys(i)%directory)) then
                    call move_alloc(compilation_buffers(k)%keys(i)%directory, compilation_buffers(k)%keys(n)%directory)
                end if

                compilation_buffers(k)%keys(n)%file = compilation_buffers(k)%keys(i)%file
            end if
        end do

        compilation_buffers(k)%nkeys = n
    end subroutine compilation_flush_file_structure

    !> Find or make, in its place, the line node NODE of LINE (none unless
    !> HAS_LINE) in file structure FS of entry K: compilation-assq on the
    !> loc tree.  Port of compile.el:compilation-assq, in part.
    subroutine make_line_node(k, fs, has_line, line, node)
        integer, intent(in) :: k, fs
        logical, intent(in) :: has_line
        integer(int64), intent(in) :: line
        integer, intent(out) :: node
        type(line_node_type), allocatable :: grown(:)
        integer :: n, i

        n = size(compilation_buffers(k)%files(fs)%lines)
        node = 1

        if (.not. has_line) then
            if (n > 0) then
                if (.not. compilation_buffers(k)%files(fs)%lines(1)%has_line) return
            end if
        else
            do while (node <= n)
                associate (x => compilation_buffers(k)%files(fs)%lines(node))
                    if (.not. x%has_line) then
                        node = node + 1
                        cycle
                    end if

                    if (line < x%line) then
                        node = node + 1
                        cycle
                    end if

                    if (x%line == line) return
                    exit
                end associate
            end do
        end if

        ! Insert a new node at NODE.
        allocate (grown(n + 1))

        do i = 1, node - 1
            call move_line_node(compilation_buffers(k)%files(fs)%lines(i), grown(i))
        end do

        grown(node)%has_line = has_line
        grown(node)%line = line
        allocate (grown(node)%locations(0))

        do i = node, n
            call move_line_node(compilation_buffers(k)%files(fs)%lines(i), grown(i + 1))
        end do

        call move_alloc(grown, compilation_buffers(k)%files(fs)%lines)
    end subroutine make_line_node

    ! Not a port.
    subroutine move_line_node(from, to)
        type(line_node_type), intent(inout) :: from
        type(line_node_type), intent(inout) :: to

        to%has_line = from%has_line
        to%line = from%line
        call move_alloc(from%locations, to%locations)
    end subroutine move_line_node

    !> Find or make, in its place, the loc LOCATION of column COLUMN (none unless
    !> HAS_COLUMN) in line node NODE of file structure FS of entry K:
    !> compilation-assq on a line.
    !> Port of compile.el:compilation-assq, in part.
    subroutine make_column_location(k, fs, node, has_column, column, location)
        integer, intent(in) :: k, fs, node
        logical, intent(in) :: has_column
        integer(int64), intent(in) :: column
        integer, intent(out) :: location
        integer, allocatable :: locations(:)
        integer :: n, j, id

        locations = compilation_buffers(k)%files(fs)%lines(node)%locations
        n = size(locations)
        j = 1

        if (.not. has_column) then
            if (n > 0) then
                if (.not. compilation_buffers(k)%locations(locations(1))%has_column) then
                    location = locations(1)
                    return
                end if
            end if
        else
            do while (j <= n)
                id = locations(j)

                if (.not. compilation_buffers(k)%locations(id)%has_column) then
                    j = j + 1
                    cycle
                end if

                if (column < compilation_buffers(k)%locations(id)%column) then
                    j = j + 1
                    cycle
                end if

                if (compilation_buffers(k)%locations(id)%column == column) then
                    location = id
                    return
                end if

                exit
            end do
        end if

        location = make_location(k, has_column, column)
        compilation_buffers(k)%files(fs)%lines(node)%locations = [locations(1:j - 1), location, locations(j:n)]
    end subroutine make_column_location

    !> Collapse multiple slashes to one, to handle non-Emacs file names.
    !> Port of startup.el:command-line-normalize-file-name.
    function command_line_normalize_file_name(file_in) result(file)
        character(len=*), intent(in) :: file_in
        character(len=:), allocatable :: file
        integer :: k, j

        file = file_in

        ! /// at the beginning is supposed to mean just /, not //.
        if (len(file) >= 3) then
            if (file(1:3) == "///") then
                j = 1

                do while (j < len(file))
                    if (file(j + 1:j + 1) /= "/") exit
                    j = j + 1
                end do

                file = "/"//file(j + 1:)
            end if
        end if

        ! Don't collapse // at the start of the file name.
        k = 2

        do while (k < len(file))
            if (file(k:k) == "/" .and. file(k + 1:k + 1) == "/") then
                j = verify(file(k + 1:), "/")
                j = merge(len(file) + 1, k + j, j == 0)
                file = file(1:k)//file(j:)
            end if

            k = k + 1
        end do
    end function command_line_normalize_file_name

    ! ------------------------------------------------------------------
    ! Parsing
    ! ------------------------------------------------------------------

    !> The type of a message whose type depends on which of the
    !> subexpressions WARNING and INFO of the last match matched.  Port of
    !> compile.el:compilation-type.
    integer function compilation_type(warning, info) result(type)
        integer, intent(in) :: warning, info
        type(status_type) :: st

        type = 2

        if (warning /= 0) then
            if (match_end(warning, st) /= 0) then
                type = 1
                return
            end if
        end if

        if (info /= 0) then
            if (match_end(info, st) /= 0) type = 0
        end if
    end function compilation_type

    !> The face of a message whose type depends on which of the
    !> subexpressions WARNING and INFO of the last match matched:
    !> `compilation_warning_face', `compilation_info_face' or
    !> `compilation_error_face'.  Port of compile.el:compilation-face.
    function compilation_face(warning, info) result(face)
        integer, intent(in) :: warning, info
        character(len=:), allocatable :: face

        select case (compilation_type(warning, info))
        case (1)
            face = setting_string(compilation_warning_face)
        case (0)
            face = setting_string(compilation_info_face)
        case default
            face = setting_string(compilation_error_face)
        end select
    end function compilation_face

    ! Not a port.
    function setting_string(s) result(v)
        type(string_setting_type), intent(in) :: s
        character(len=:), allocatable :: v

        v = ""
        if (allocated(s%value)) v = s%value
    end function setting_string

    !> The face for messages of TYPE (2 error, 1 warning, 0 info).  Not a port.
    function type_face(type) result(face)
        integer, intent(in) :: type
        character(len=:), allocatable :: face

        select case (type)
        case (0)
            face = setting_string(compilation_info_face)
        case (1)
            face = setting_string(compilation_warning_face)
        case default
            face = setting_string(compilation_error_face)
        end select
    end function type_face

    !> Match position of subexpression G in the match data MD (0 when it
    !> didn't match): its beginning, or its end if END.  Not a port.
    integer(int64) function md_position(md, g, end) result(position)
        integer(int64), intent(in) :: md(:)
        integer, intent(in) :: g
        logical, intent(in) :: end

        position = 0
        if (g < 0) return
        if (2*g + 2 > size(md)) return

        if (end) then
            position = md(2*g + 2)
        else
            position = md(2*g + 1)
        end if
    end function md_position

    !> The text of subexpression G of the match data MD.  Not a port.
    function md_string(md, g) result(s)
        integer(int64), intent(in) :: md(:)
        integer, intent(in) :: g
        character(len=:), allocatable :: s
        type(status_type) :: st

        call buffer_substring(md_position(md, g, .false.), md_position(md, g, .true.), s, st)
        if (.not. st%ok()) s = ""
    end function md_string

    !> Put PROPERTY VALUE on the text of subexpression MATCHNUM, if it matched.
    !> Port of compile.el:compilation--put-prop.
    subroutine compilation_put_property(md, matchnum, property, value)
        integer(int64), intent(in) :: md(:)
        integer, intent(in) :: matchnum
        character(len=*), intent(in) :: property, value
        type(status_type) :: st

        if (matchnum == 0) return
        if (md_position(md, matchnum, .false.) == 0) return
        call put_text_property(md_position(md, matchnum, .false.), md_position(md, matchnum, .true.), value_type(name=property), &
                               value_type(name=value), st)
    end subroutine compilation_put_property

    !> Like `beginning_of_line'.  Port of
    !> compile.el:compilation-beginning-of-line (`selective_display' is
    !> Deferred).
    subroutine compilation_beginning_of_line(n)
        integer(int64), intent(in) :: n

        call beginning_of_line(n)
    end subroutine compilation_beginning_of_line

    !> Go to column COLUMN on the current line.
    !> If SCREEN is true, columns are screen columns, otherwise, they are
    !> just char-counts.  FIRST is the first column's number.  Port of
    !> compile.el:compilation-move-to-column.
    subroutine compilation_move_to_column(column_in, screen, first)
        integer(int64), intent(in) :: column_in, first
        logical, intent(in) :: screen
        integer(int64) :: column, reached, saved

        column = column_in - first

        if (screen) then
            ! Presumably, the compilation tool doesn't know about our
            ! current `tab_width' setting, so it probably assumed 8-wide
            ! TABs.
            saved = tab_width%value
            tab_width%value = 8
            call move_to_column(max(column, 0_int64), reached)
            tab_width%value = saved
        else
            call goto_character(min(line_beginning_position() + column, line_end_position()))
        end if
    end subroutine compilation_move_to_column

    !> Move forward ARGUMENT lines and position at first nonblank character.
    !> Port of simple.el:forward-to-indentation.
    subroutine forward_to_indentation(argument)
        integer(int64), intent(in) :: argument
        integer(int64) :: moved
        type(status_type) :: st

        moved = forward_line(argument)
        moved = skip_characters_forward(" "//TB, st)
    end subroutine forward_to_indentation

    !> Parse errors between START and END.
    !> The errors recognized are the ones specified in RULES (anonymous
    !> rules) which default to `compilation_error_regular_expression_names' if RULES
    !> is absent.  Port of compile.el:compilation-parse-errors.
    recursive subroutine compilation_parse_errors(start, end, status, rules)
        integer(int64), intent(in) :: start, end
        type(status_type), intent(out) :: status
        type(compilation_rule_type), intent(in), optional :: rules(:)
        type(compilation_rule_type) :: rule
        logical :: saved_fold, omake_included
        integer :: r, n, j, k

        status = status_type()
        saved_fold = case_fold_search%value
        case_fold_search = compilation_error_case_fold_search%value
        omake_included = .false.
        ! The buffer's own compilation-error-regexp-alist (rules).
        k = entry_of(current_buffer())

        if (k /= 0) then
            if (.not. compilation_buffers(k)%has_rules) k = 0
        end if

        if (present(rules)) then
            n = size(rules)
        else if (k /= 0) then
            n = size(compilation_buffers(k)%rules)
        else
            n = 0
            if (allocated(compilation_error_regular_expression_names)) n = size(compilation_error_regular_expression_names)
        end if

        if (k == 0 .and. allocated(compilation_error_regular_expression_names)) then
            omake_included = any(compilation_error_regular_expression_names == "omake")
        end if

        do r = 1, n
            if (present(rules)) then
                rule = rules(r)
                if (allocated(rule%name)) deallocate (rule%name)
            else if (k /= 0) then
                rule = compilation_buffers(k)%rules(r)
                if (allocated(rule%name)) deallocate (rule%name)
            else
                j = rule_index(trim(compilation_error_regular_expression_names(r)))
                if (j == 0) cycle
                rule = compilation_error_regular_expressions(j)
            end if

            call parse_rule(rule, start, end, omake_included, status)
            if (.not. status%ok()) exit
        end do

        case_fold_search = saved_fold
    end subroutine compilation_parse_errors

    !> The index of the rule NAME in `compilation_error_regular_expressions'
    !> (0 for none).  Not a port.
    integer function rule_index(name) result(j)
        character(len=*), intent(in) :: name

        if (allocated(compilation_error_regular_expressions)) then
            do j = 1, size(compilation_error_regular_expressions)
                if (allocated(compilation_error_regular_expressions(j)%name)) then
                    if (same_string(compilation_error_regular_expressions(j)%name, name)) return
                end if
            end do
        end if

        j = 0
    end function rule_index

    !> Parse the messages RULE matches between START and END (the body of
    !> the dolist of compile.el:compilation-parse-errors).
    !> Port of compile.el:compilation-parse-errors, in part.
    recursive subroutine parse_rule(rule, start, end, omake_included, status)
        type(compilation_rule_type), intent(in) :: rule
        integer(int64), intent(in) :: start, end
        logical, intent(in) :: omake_included
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: pattern, face, help
        integer(int64), allocatable :: md(:)
        integer(int64) :: found, b, e
        type(status_type) :: st
        type(value_type) :: properties
        logical :: has_properties, changed, file_given
        integer :: message, this_type, h, mn

        status = status_type()
        if (.not. allocated(rule%regular_expression)) return
        pattern = rule%regular_expression

        ! omake reports some error indented, so skip the indentation.
        if (omake_included .and. len(pattern) > 0) then
            if (string_match("\`\([^^]\|\^\( \*\|\[\)\)", pattern, st) == 0) then
                pattern = "^\(?:      \)?"//pattern(2:)
            end if
        end if

        file_given = rule%file /= 0 .or. associated(rule%file_function)
        call goto_character(start)

        do
            call regular_expression_search_forward(pattern, found, status, bound=end, noerror=SEARCH_NOERROR)
            if (.not. status%ok()) return
            if (found == 0) exit
            md = match_data()
            call compilation_error_properties(rule, md, has_properties, message, help, status)
            if (.not. status%ok()) return
            call set_match_data(md)
            if (.not. has_properties) cycle

            if (file_given) then
                if (rule%warning /= 0 .or. rule%info /= 0) then
                    this_type = compilation_type(rule%warning, rule%info)
                else
                    this_type = rule%type
                end if

                call compilation_note_type(this_type)
                call compilation_put_property(md, rule%file, "font_lock_face", type_face(this_type))
            end if

            call compilation_put_property(md, rule%line, "font_lock_face", setting_string(compilation_line_face))
            call compilation_put_property(md, rule%end_line, "font_lock_face", setting_string(compilation_line_face))
            call compilation_put_property(md, rule%column, "font_lock_face", setting_string(compilation_column_face))
            call compilation_put_property(md, rule%end_column, "font_lock_face", setting_string(compilation_column_face))

            ! Obey HIGHLIGHT.
            if (allocated(rule%highlights)) then
                do h = 1, size(rule%highlights)
                    mn = rule%highlights(h)%submatch
                    if (md_position(md, mn, .false.) == 0) cycle
                    if (allocated(face)) deallocate (face)

                    if (associated(rule%highlights(h)%face_function)) then
                        call rule%highlights(h)%face_function(face, status)
                        if (.not. status%ok()) return
                    else if (allocated(rule%highlights(h)%face)) then
                        face = rule%highlights(h)%face
                    end if

                    if (allocated(rule%highlights(h)%display)) then
                        ! (face FACE display DISPLAY).
                        if (.not. allocated(face)) face = ""

                        if (len(face) == 0) then
                            call put_text_property(md_position(md, mn, .false.), md_position(md, mn, .true.), &
                                                   value_type(name="font_lock_face"), value_type(), st)
                        else
                            call put_text_property(md_position(md, mn, .false.), md_position(md, mn, .true.), &
                                                   value_type(name="font_lock_face"), value_type(name=face), st)
                        end if

                        call put_text_property(md_position(md, mn, .false.), md_position(md, mn, .true.), &
                                               value_type(name="display"), &
                                               value_type(text=rule%highlights(h)%display), st)
                        cycle
                    end if

                    if (.not. allocated(face)) cycle
                    if (len(face) == 0) cycle
                    call put_text_property(md_position(md, mn, .false.), md_position(md, mn, .true.), &
                                           value_type(name="font_lock_face"), &
                                           value_type(name=face), st)
                end do
            end if

            mn = rule%hyperlink
            b = md_position(md, mn, .false.)
            e = md_position(md, mn, .true.)
            if (b == 0) cycle
            properties = list(value_type(name="compilation_message"), value_type(int(message, int64)), &
                              value_type(name="help_echo"), &
                              value_type(text=help), &
                              value_type(name="mouse_face"), value_type(name="highlight"))
            call add_text_properties(b, e, properties, changed, st)
            call font_lock_append_text_property(b, e, value_type(name="font_lock_face"), &
                                                value_type(name=setting_string(compilation_message_face)), st)
        end do
    end subroutine parse_rule

    !> The current buffer's `compilation_transform_file_matches'.
    !> Not a port.
    function transform_file_match_table() result(transforms)
        type(compilation_transform_type), allocatable :: transforms(:)
        integer :: k, i

        k = entry_of(current_buffer())

        if (k /= 0) then
            if (compilation_buffers(k)%has_transforms) then
                allocate (transforms(size(compilation_buffers(k)%transforms)))

                do i = 1, size(transforms)
                    transforms(i) = compilation_buffers(k)%transforms(i)
                end do

                return
            end if
        end if

        if (.not. allocated(compilation_transform_file_matches)) then
            allocate (transforms(0))
            return
        end if

        allocate (transforms(size(compilation_transform_file_matches)))

        do i = 1, size(transforms)
            transforms(i) = compilation_transform_file_matches(i)
        end do
    end function transform_file_match_table

    !> Note that a new message with severity TYPE was seen.
    !> This updates the appropriate variable used by the mode-line.  Port of
    !> compile.el:compilation--note-type.
    subroutine compilation_note_type(type)
        integer, intent(in) :: type
        integer :: k

        k = entry_of(current_buffer())

        if (k /= 0) then
            if (compilation_buffers(k)%has_counts) then
                select case (type)
                case (0)
                    compilation_buffers(k)%info_count = compilation_buffers(k)%info_count + 1
                case (1)
                    compilation_buffers(k)%warning_count = compilation_buffers(k)%warning_count + 1
                case (2)
                    compilation_buffers(k)%error_count = compilation_buffers(k)%error_count + 1
                case default
                    ! (Other types are not counted.)
                end select

                call update_mode_line_process(k)
                return
            end if
        end if

        select case (type)
        case (0)
            compilation_info_count_found = compilation_info_count_found + 1
        case (1)
            compilation_warning_count_found = compilation_warning_count_found + 1
        case (2)
            compilation_error_count_found = compilation_error_count_found + 1
        case default
            ! (Other types are not counted.)
        end select
    end subroutine compilation_note_type

    !> The central driver, called when parsing to gather all information
    !> needed to later jump to corresponding source code: the message of
    !> the match of RULE (whose match data is MD) in MESSAGE and the help echo
    !> HELP, HAS_PROPERTIES false for none.  Port of
    !> compile.el:compilation-error-properties
    !> (`compilation_auto_jump_to_first_error' is Deferred).
    recursive subroutine compilation_error_properties(rule, md, has_properties, message, help, status)
        type(compilation_rule_type), intent(in) :: rule
        integer(int64), intent(in) :: md(:)
        logical, intent(out) :: has_properties
        integer, intent(out) :: message
        character(len=:), allocatable, intent(out) :: help
        type(status_type), intent(out) :: status
        type(error_specification_type) :: specification
        character(len=:), allocatable :: filename, directory, file_name, replaced
        integer(int64), allocatable :: saved(:)
        integer(int64) :: position, ec
        type(status_type) :: st
        type(compilation_transform_type), allocatable :: transforms(:)
        logical :: found, has_file, has_directory, has_ec
        integer :: k, d, previous, location, fs, j

        status = status_type()
        has_properties = .false.
        message = 0
        call text_property_not_all(md_position(md, 0, .false.), point(), value_type(name="compilation_message"), value_type(), &
                                   position, &
                                   found, st)
        if (found) return
        call make_current_entry(k)
        has_file = .false.
        has_directory = .false.

        if (associated(rule%file_function)) then
            call rule%file_function(filename, directory, has_file)
            has_directory = allocated(directory)
            if (.not. has_file) has_directory = .false.
        else if (rule%file /= 0) then
            if (md_position(md, rule%file, .false.) /= 0) then
                filename = md_string(md, rule%file)
                has_file = .true.

                if (.not. is_file_name_absolute(filename)) then
                    d = directory_before(md_position(md, 0, .false.))

                    if (d /= 0) then
                        if (allocated(compilation_buffers(k)%directories(d)%directory)) then
                            directory = compilation_buffers(k)%directories(d)%directory
                            has_directory = .true.
                        end if
                    end if
                end if
            end if
        else
            ! This message didn't mention one, get it from previous.
            call previous_single_property_change(point(), value_type(name="compilation_message"), position, found, st)

            if (found) then
                previous = message_at(position - 1)
                if (previous == 0) previous = message_at(position)

                if (previous /= 0) then
                    location = compilation_buffers(k)%messages(previous)%location

                    if (location /= 0) then
                        fs = compilation_buffers(k)%locations(location)%file

                        if (fs /= 0) then
                            filename = compilation_buffers(k)%files(fs)%filename
                            has_file = .true.

                            if (allocated(compilation_buffers(k)%files(fs)%directory)) then
                                directory = compilation_buffers(k)%files(fs)%directory
                                has_directory = .true.
                            end if
                        end if
                    end if
                end if
            end if

            if (.not. has_file) then
                filename = "*unknown*"
                has_file = .true.
            end if
        end if

        ! All of these fields are optional, get them only if we have an
        ! index, and it matched some part of the message.
        call get_number(rule%line_function, rule%line, specification%has_line, specification%line)
        call get_number(rule%end_line_function, rule%end_line, specification%has_end_line, specification%end_line)
        call get_number(rule%column_function, rule%column, specification%has_column, specification%column)
        call get_number(rule%end_column_function, rule%end_column, has_ec, ec)

        if (has_ec) then
            ! Add one to get an exclusive upper bound.
            specification%end_column = ec + 1
            specification%has_end_column = .true.
        else if (specification%has_end_line) then
            specification%end_column = -1
            specification%has_end_column = .true.
        end if

        if (rule%warning /= 0 .or. rule%info /= 0) then
            specification%type = compilation_type(rule%warning, rule%info)
        else
            specification%type = rule%type
        end if

        ! Remove matches like /bin/sh and do other file name transforms.
        transforms = transform_file_match_table()

        if (has_file .and. size(transforms) > 0) then
            saved = match_data()

            if (has_directory) then
                call expand_file_name(filename, file_name, st, directory)
            else
                file_name = filename
            end if

            do j = 1, size(transforms)
                if (.not. allocated(transforms(j)%regular_expression)) cycle
                if (string_match(transforms(j)%regular_expression, file_name, st) == 0) cycle

                if (allocated(transforms(j)%replacement)) then
                    call replace_match(transforms(j)%replacement, st, &
                                       fixedcase=.false., literal=.false., string=file_name, result=replaced)
                    filename = replaced
                    has_directory = .false.
                else
                    has_file = .false.
                end if

                exit
            end do

            call set_match_data(saved)
        end if

        ! If we ignored all the files with errors on this line, then return
        ! nothing.
        if (.not. has_file) return
        specification%filename = filename
        if (has_directory) specification%directory = directory
        if (allocated(rule%name)) specification%rule = rule%name
        call compilation_internal_error_properties(specification, message, help, status)
        has_properties = status%ok()

    contains

        subroutine get_number(fn, group, has, value)
            procedure(compilation_number_procedure), pointer, intent(in) :: fn
            integer, intent(in) :: group
            logical, intent(out) :: has
            integer(int64), intent(out) :: value

            has = .false.
            value = 0

            if (associated(fn)) then
                call fn(value, has)
            else if (group /= 0) then
                if (md_position(md, group, .false.) /= 0) then
                    call string_to_number(md_string(md, group), value)
                    has = .true.
                end if
            end if
        end subroutine get_number
    end subroutine compilation_error_properties

    !> Get the meta-info that will be added as text-properties: the message
    !> MESSAGE and the help echo HELP for the location SPECIFICATION.  Port of
    !> compile.el:compilation-internal-error-properties.
    recursive subroutine compilation_internal_error_properties(specification_in, message, help, status)
        type(error_specification_type), intent(in) :: specification_in
        integer, intent(out) :: message
        character(len=:), allocatable, intent(out) :: help
        type(status_type), intent(out) :: status
        type(error_specification_type) :: specification
        type(handle_type) :: marker, end_marker, mbuf, comp, new_marker
        type(status_type) :: st
        integer(int64) :: position, mline, first, original_point, beginning_of_accessible_portion, end_of_accessible_portion, target
        logical :: screen, valid
        integer :: k, fs, marker_node, x, n, location, end_location, node, enode

        status = status_type()
        specification = specification_in
        comp = current_buffer()
        call make_current_entry(k)

        if (allocated(specification%directory)) then
            fs = compilation_get_file_structure(k, specification%filename, specification%directory)
        else
            fs = compilation_get_file_structure(k, specification%filename)
        end if

        ! Get first already existing marker (if any has one, all have one).
        marker = null_handle()
        marker_node = 0
        n = size(compilation_buffers(k)%files(fs)%lines)

        if (n > 0) then
            marker_node = 1

            if (size(compilation_buffers(k)%files(fs)%lines(1)%locations) > 0) then
                marker = compilation_buffers(k)%locations(compilation_buffers(k)%files(fs)%lines(1)%locations(1))%marker
            end if
        end if

        valid = .false.

        if (marker%index /= 0) then
            call marker_buffer(marker, mbuf, st)
            valid = st%ok() .and. mbuf%index /= 0
        end if

        new_marker = null_handle()
        end_marker = null_handle()

        if (valid) then
            ! Normalize no linenumber to line 1.
            if (.not. specification%has_line) then
                specification%line = 1
                specification%has_line = .true.
            end if

            ! Find nearest loc, at least one exists.
            do x = 2, n
                associate (xl => compilation_buffers(k)%files(fs)%lines(x)%line)
                    if (xl > specification%line) then
                        marker_node = x
                    else
                        mline = 1

                        if (compilation_buffers(k)%files(fs)%lines(marker_node)%has_line) then
                            mline = compilation_buffers(k)%files(fs)%lines(marker_node)%line
                        end if

                        if (mline - specification%line > specification%line - xl) marker_node = x
                        exit
                    end if
                end associate
            end do

            marker = compilation_buffers(k)%locations(compilation_buffers(k)%files(fs)%lines(marker_node)%locations(1))%marker
            mline = 1

            if (compilation_buffers(k)%files(fs)%lines(marker_node)%has_line) then
                mline = compilation_buffers(k)%files(fs)%lines(marker_node)%line
            end if

            screen = compilation_error_screen_columns%value
            first = compilation_first_column%value
            call marker_buffer(marker, mbuf, st)
            call set_buffer(mbuf, st)
            ! Obey the compilation_error_screen_columns of the target
            ! buffer if its major mode set it buffer-locally.
            if (compilation_error_screen_columns%is_local()) screen = compilation_error_screen_columns%value
            if (compilation_first_column%is_local()) first = compilation_first_column%value
            original_point = point()
            beginning_of_accessible_portion = point_minimum()
            end_of_accessible_portion = point_maximum()
            call widen()
            call marker_position(marker, position, st)
            call goto_character(position)

            ! Set end-marker if appropriate and go to line.
            if (.not. (specification%has_end_column .or. specification%has_end_line)) then
                call compilation_beginning_of_line(specification%line - mline + 1)
            else
                target = specification%line
                if (specification%has_end_line) target = specification%end_line
                call compilation_beginning_of_line(target - mline + 1)

                if (.not. specification%has_end_column) then
                    call end_of_line()
                else if (specification%end_column < 0) then
                    call end_of_line()
                else
                    call compilation_move_to_column(specification%end_column, screen, first)
                end if

                call point_marker(end_marker)
                if (specification%has_end_line) call compilation_beginning_of_line(specification%line - specification%end_line + 1)
            end if

            if (specification%has_column) then
                call compilation_move_to_column(specification%column, screen, first)
            else
                call forward_to_indentation(0_int64)
            end if

            call point_marker(new_marker)
            call narrow_to_region(beginning_of_accessible_portion, end_of_accessible_portion, st)
            call goto_character(original_point)
            call set_buffer(comp, st)
            k = entry_of(comp)
        end if

        marker = new_marker
        call make_line_node(k, fs, specification%has_line, specification%line, node)
        end_location = 0

        if (specification%has_end_line) then
            call make_line_node(k, fs, .true., specification%end_line, enode)
            call make_column_location(k, fs, enode, specification%has_end_column, specification%end_column, end_location)
            call make_line_node(k, fs, specification%has_line, specification%line, node)
        else if (specification%has_end_column) then
            call make_column_location(k, fs, node, .true., specification%end_column, end_location)
        end if

        call make_column_location(k, fs, node, specification%has_column, specification%column, location)

        ! If they are new, make the loc(s) reference the file they point
        ! to.
        if (.not. compilation_buffers(k)%locations(location)%filled) then
            call fill_location(compilation_buffers(k)%locations(location), specification%has_line, specification%line, fs, marker)
            marker = null_handle()
        end if

        if (end_location /= 0) then
            if (.not. compilation_buffers(k)%locations(end_location)%filled) then
                if (specification%has_end_line) then
                    call fill_location(compilation_buffers(k)%locations(end_location), .true., specification%end_line, fs, &
                                       end_marker)
                else
                    call fill_location(compilation_buffers(k)%locations(end_location), specification%has_line, specification%line, &
                                       fs, end_marker)
                end if

                end_marker = null_handle()
            end if
        end if

        ! The markers not kept are garbage.
        if (marker%index /= 0) call destroy_marker(marker, st)
        if (end_marker%index /= 0) call destroy_marker(end_marker, st)

        if (allocated(specification%rule)) then
            message = make_message(k, location, specification%type, end_location, specification%rule)
        else
            message = make_message(k, location, specification%type, end_location)
        end if

        if (specification%has_column) then
            help = "mouse-2: visit this file, line and column"
        else if (specification%has_line) then
            help = "mouse-2: visit this file and line"
        else
            help = "mouse-2: visit this file"
        end if
    end subroutine compilation_internal_error_properties

    ! Not a port.
    subroutine fill_location(location, has_line, line, fs, marker)
        type(location_type), intent(inout) :: location
        logical, intent(in) :: has_line
        integer(int64), intent(in) :: line
        integer, intent(in) :: fs
        type(handle_type), intent(in) :: marker

        location%filled = .true.
        location%has_line = has_line
        location%line = line
        location%file = fs
        location%marker = marker
    end subroutine fill_location

    !> Remove the properties parsing put between START and END.  Port of
    !> compile.el:compilation--remove-properties.
    subroutine compilation_remove_properties(start_in, end_in)
        integer(int64), intent(in), optional :: start_in, end_in
        type(silent_modifications_type) :: silent
        type(status_type) :: st
        integer(int64) :: start, end, next
        logical :: found, changed
        type(value_type) :: v, properties

        start = point_minimum()
        if (present(start_in)) start = start_in
        end = point_maximum()
        if (present(end_in)) end = end_in
        call silent_modifications_begin(silent)
        properties = list(value_type(name="compilation_debug"), value_type(name="compilation_directory"), &
                          value_type(name="compilation_message"), &
                          value_type(name="font_lock_face"), value_type(name="help_echo"), value_type(name="mouse_face"))

        do
            call next_single_property_change(start, value_type(name="compilation_message"), next, found, st, end)
            if (.not. found) next = end
            call get_text_property(start, value_type(name="compilation_message"), v, st)
            if (st%ok() .and. allocated(v%a)) call remove_list_of_text_properties(start, next, properties, changed, st)
            if (next >= end) exit
            start = next
        end do

        call silent_modifications_end(silent)
    end subroutine compilation_remove_properties

    !> change-major-mode-hook: remove the properties of the whole buffer.
    !> Not a port.
    subroutine remove_properties_hook(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call compilation_remove_properties()
    end subroutine remove_properties_hook

    !> Parse the text between START and END: the directory messages first,
    !> then the error messages.  Port of compile.el:compilation--parse-region.
    recursive subroutine compilation_parse_region(start, end_in, status)
        integer(int64), intent(in) :: start, end_in
        type(status_type), intent(out) :: status
        integer(int64), allocatable :: md(:)
        integer(int64) :: end, found
        type(handle_type) :: process
        character(len=:), allocatable :: state
        type(status_type) :: st
        type(value_type) :: properties, more
        type(compilation_directory_matcher_type) :: dm
        logical :: changed
        integer :: i, k

        status = status_type()
        dm = compilation_directory_matcher
        k = entry_of(current_buffer())

        if (k /= 0) then
            if (compilation_buffers(k)%has_matcher) dm = compilation_buffers(k)%matcher
        end if

        end = end_in
        call goto_character(end)

        if (.not. is_beginning_of_line()) then
            ! We generally don't like to parse partial lines.
            process = get_buffer_process(current_buffer())

            if (process%index /= 0) then
                state = process_status(process)
                if (state == "run" .or. state == "open") end = line_beginning_position()
            end if
        end if

        call compilation_remove_properties(start, end)
        call goto_character(start)

        if (allocated(dm%regular_expression)) then
            do
                call regular_expression_search_forward(dm%regular_expression, found, status, bound=end, &
                                                       noerror=SEARCH_NOERROR)
                if (.not. status%ok()) return
                if (found == 0) exit
                md = match_data()
                if (.not. allocated(dm%directory)) cycle

                do i = 1, size(dm%directory)
                    if (md_position(md, dm%directory(i), .false.) == 0) then
                        call set_error(status, GENERIC_ERROR_CODE, &
                                       "Wrong type argument: expected integer or type(marker_type), got .false.")
                        return
                    end if

                    call compilation_directory_properties(md, dm%directory(i), &
                                                          dm%leave(i), properties, more)
                    call add_text_properties(md_position(md, dm%directory(i), .false.), &
                                             md_position(md, dm%directory(i), .true.), properties, changed, st)
                    call add_text_properties(md_position(md, dm%directory(i), .false.), &
                                             md_position(md, dm%directory(i), .true.), more, changed, st)
                end do
            end do
        end if

        call compilation_parse_errors(start, end, status)
    end subroutine compilation_parse_region

    !> The text properties of a directory change message.  The
    !> `compilation_directory' property is important, because it is the
    !> stack of nested enter-messages.  Relative filenames on the following
    !> lines are relative to the top of the stack.  Port of
    !> compile.el:compilation-directory-properties.
    subroutine compilation_directory_properties(md, index, leave, properties, more)
        integer(int64), intent(in) :: md(:)
        integer, intent(in) :: index, leave
        type(value_type), intent(out) :: properties, more
        integer :: k, directory, value, message
        logical :: leaving
        character(len=:), allocatable :: face

        call make_current_entry(k)
        leaving = .false.
        if (leave /= 0) leaving = md_position(md, leave, .true.) /= 0
        ! Find previous stack, and push onto it, or if `leave' pop it.
        directory = directory_before(md_position(md, 0, .false.))

        if (leaving) then
            face = setting_string(compilation_leave_directory_face)
            value = 0
            if (directory /= 0) value = compilation_buffers(k)%directories(directory)%parent

            if (value == 0) then
                ! '(nil): nil only isn't a property change.
                if (compilation_buffers(k)%false_directory == 0) compilation_buffers(k)%false_directory = create_directory(k, 0)
                value = compilation_buffers(k)%false_directory
            end if
        else
            face = setting_string(compilation_enter_directory_face)
            value = create_directory(k, directory, md_string(md, index))
        end if

        ! Place a `compilation_message' everywhere we change
        ! text-properties so compilation--remove-properties can know what
        ! to remove.
        message = make_message(k, 0, 0, 0)
        properties = list(value_type(name="font_lock_face"), value_type(name=face), value_type(name="compilation_directory"), &
                          value_type(int(value, int64)), &
                          value_type(name="compilation_message"), value_type(int(message, int64)), value_type(name="mouse_face"), &
                          value_type(name="highlight"))
        more = list(value_type(name="help_echo"), value_type(text="mouse-2: visit destination directory"))
    end subroutine compilation_directory_properties

    !> Make sure the text has been parsed up to LIMIT.  Port of
    !> compile.el:compilation--ensure-parse.
    recursive subroutine compilation_ensure_parse(limit_in)
        integer(int64), intent(in) :: limit_in
        type(silent_modifications_type) :: silent
        type(status_type) :: st
        type(value_type) :: v
        integer(int64) :: original_point, limit, parsed, start, moved
        integer :: k

        call make_current_entry(k)
        original_point = point()
        call goto_character(limit_in)
        limit = line_beginning_position(2_int64)

        if (compilation_buffers(k)%parsed%index == 0) then
            ! We use a marker for compilation--parsed so that users (such as
            ! grep.el) don't need to flush-parse when they modify the buffer
            ! in a way that impacts buffer positions but does not require
            ! re-parsing.
            call copy_marker_at(annotation_end(), compilation_buffers(k)%parsed, .false., st)
        end if

        call marker_position(compilation_buffers(k)%parsed, parsed, st)

        if (parsed < limit) then
            start = max(parsed, point_minimum())
            call set_marker(compilation_buffers(k)%parsed, limit, st)
            call goto_character(start)
            ! Not line-beginning-position: ignore (comint) fields.
            moved = forward_line(0_int64)

            do while (.not. is_beginning_of_buffer())
                call get_text_property(point() - 1, value_type(name="compilation_multiline"), v, st)
                if (.not. allocated(v%a)) exit
                moved = forward_line(-1_int64)
            end do

            call marker_position(compilation_buffers(k)%parsed, parsed, st)
            call silent_modifications_begin(silent)
            call compilation_parse_region(point(), parsed, st)
            call silent_modifications_end(silent)
        end if

        call goto_character(original_point)
    end subroutine compilation_ensure_parse

    !> Where (text-property-search-forward 'compilation-annotation) from
    !> the start of the buffer leaves point: the end of the first region
    !> with a `compilation_annotation' property, or point-min.  Not a port.
    integer(int64) function annotation_end() result(position)
        type(value_type) :: v
        type(status_type) :: st
        integer(int64) :: next
        logical :: found

        position = point_minimum()
        if (position >= point_maximum()) return

        do
            call get_text_property(position, value_type(name="compilation_annotation"), v, st)

            if (allocated(v%a)) then
                call next_single_property_change(position, value_type(name="compilation_annotation"), next, found, st)
                if (.not. found) next = point_maximum()
                position = next
                return
            end if

            call next_single_property_change(position, value_type(name="compilation_annotation"), next, found, st)

            if (.not. found) then
                position = point_minimum()
                return
            end if

            position = next
        end do
    end function annotation_end

    !> Mark the region between START and END for re-parsing.  Port of
    !> compile.el:compilation--flush-parse.
    subroutine compilation_flush_parse(start, end, status)
        integer(int64), intent(in) :: start, end
        type(status_type), intent(out) :: status
        integer(int64) :: parsed
        integer :: k

        status = status_type()
        if (end < 0) return
        k = entry_of(current_buffer())
        if (k == 0) return
        if (compilation_buffers(k)%parsed%index == 0) return
        call marker_position(compilation_buffers(k)%parsed, parsed, status)
        if (.not. status%ok()) return
        call set_marker(compilation_buffers(k)%parsed, min(start, parsed), status)
    end subroutine compilation_flush_parse

    !> The font-lock MATCHER (compilation--ensure-parse): parse up to
    !> LIMIT; it never matches.  Not a port.
    subroutine ensure_parse_matcher(limit, found, status)
        integer(int64), intent(in) :: limit
        logical, intent(out) :: found
        type(status_type), intent(out) :: status

        status = status_type()
        found = .false.
        if (.not. is_compilation_buffer_internal()) return
        call compilation_ensure_parse(limit)
    end subroutine ensure_parse_matcher

    !> Forget the locations found so far: messages later in the buffer get
    !> new locations, and the next error is searched after what the buffer
    !> has now.  Port of compile.el:compilation-forget-errors.
    subroutine compilation_forget_errors()
        type(handle_type) :: process
        type(status_type) :: st
        integer(int64) :: position
        integer :: k

        call make_current_entry(k)
        ! In case we hit the same file/line specs, we want to recompute a
        ! new marker for them, so flush our cache.
        compilation_buffers(k)%nkeys = 0

        if (compilation_buffers(k)%current_error_marker%index /= 0) then
            call destroy_marker(compilation_buffers(k)%current_error_marker, st)
        end if

        compilation_buffers(k)%current_error_marker = null_handle()
        compilation_buffers(k)%has_current_error = .false.
        process = get_buffer_process(current_buffer())

        if (process%index /= 0) then
            call marker_position(process_mark(process), position, st)
        else
            position = point_maximum()
        end if

        if (compilation_buffers(k)%messages_start_marker%index /= 0) then
            call destroy_marker(compilation_buffers(k)%messages_start_marker, st)
        end if

        compilation_buffers(k)%messages_start_marker = null_handle()
        compilation_buffers(k)%has_messages_start = .true.

        ! In the future, ignore the text already present in the buffer.
        if (position > point_minimum()) then
            call copy_marker_at(position - 1, compilation_buffers(k)%messages_start_marker, .false., st)
        else
            compilation_buffers(k)%messages_start_position = position
        end if

        compilation_buffers(k)%auto_jump_to_next = compilation_scroll_output%choice == COMPILATION_SCROLL_OUTPUT_FIRST_ERROR
    end subroutine compilation_forget_errors

    !> What the `compilation_message' property at POSITION of the current
    !> buffer says (FOUND false for none).  Not a port.
    subroutine compilation_message_info(position, info, found)
        integer(int64), intent(in) :: position
        type(compilation_message_info_type), intent(out) :: info
        logical, intent(out) :: found
        integer :: k, message, location, fs

        found = .false.
        message = message_at(position)
        if (message == 0) return
        found = .true.
        k = entry_of(current_buffer())
        info%type = compilation_buffers(k)%messages(message)%type
        if (allocated(compilation_buffers(k)%messages(message)%rule)) info%rule = compilation_buffers(k)%messages(message)%rule
        location = compilation_buffers(k)%messages(message)%location

        if (location /= 0) then
            info%has_location = .true.
            info%has_line = compilation_buffers(k)%locations(location)%has_line
            info%line = compilation_buffers(k)%locations(location)%line
            info%has_column = compilation_buffers(k)%locations(location)%has_column
            info%column = compilation_buffers(k)%locations(location)%column
            fs = compilation_buffers(k)%locations(location)%file

            if (fs /= 0) then
                info%file = compilation_buffers(k)%files(fs)%filename

                if (allocated(compilation_buffers(k)%files(fs)%directory)) then
                    info%directory = compilation_buffers(k)%files(fs)%directory
                end if
            end if
        end if

        location = compilation_buffers(k)%messages(message)%end_location

        if (location /= 0) then
            info%has_end = .true.
            info%has_end_line = compilation_buffers(k)%locations(location)%has_line
            info%end_line = compilation_buffers(k)%locations(location)%line
            info%has_end_column = compilation_buffers(k)%locations(location)%has_column
            info%end_column = compilation_buffers(k)%locations(location)%column
        end if
    end subroutine compilation_message_info

    ! ------------------------------------------------------------------
    ! Moving between messages
    ! ------------------------------------------------------------------

    !> Like next_single_property_change, but parse the text first.  Port of
    !> compile.el:compilation-next-single-property-change.
    recursive subroutine compilation_next_single_property_change(position_in, property, res, found, limit)
        integer(int64), intent(in) :: position_in
        character(len=*), intent(in) :: property
        integer(int64), intent(out) :: res
        logical, intent(out) :: found
        integer(int64), intent(in), optional :: limit
        integer(int64) :: position, parsed, limit_position
        type(status_type) :: st
        integer :: k
        logical :: again

        position = position_in

        do
            call make_current_entry(k)
            parsed = -1
            if (compilation_buffers(k)%parsed%index /= 0) call marker_position(compilation_buffers(k)%parsed, parsed, st)
            limit_position = point_maximum()
            if (present(limit)) limit_position = limit
            parsed = max(parsed, limit_position)
            call compilation_ensure_parse(parsed)

            if (present(limit)) then
                call next_single_property_change(position, value_type(name=property), res, found, st, limit)
            else
                call next_single_property_change(position, value_type(name=property), res, found, st)
            end if

            again = .not. found
            if (found .and. present(limit)) again = res == limit
            if (.not. (again .and. position < limit_position)) exit
            position = parsed
        end do
    end subroutine compilation_next_single_property_change

    !> Move point to the next error in the compilation buffer.
    !> This function does NOT find the source line like \[next_error],
    !> but you can use \[compilation_display_error] to find and display
    !> the corresponding source code.  Prefix arg N says how many error
    !> messages to move forwards (or backwards, if negative).  Where the
    !> current error ends and the next one begins is determined by the
    !> rules from `compilation_error_regular_expression_names' that matched the
    !> compilation messages; this function moves point to where the
    !> `compilation_message' text property changes its value.  In general,
    !> all the messages that have the same line and column numbers are
    !> considered parts of a single compilation message.
    !>
    !> Optional arg DIFFERENT_FILE, if true, means find next error for a
    !> file that is different from the current one.  Optional arg POINT, if
    !> present, specifies the value of point to start looking for the next
    !> message.  In interactive invocations, DIFFERENT_FILE and POINT are
    !> always absent.  Port of compile.el:compilation-next-error.
    recursive subroutine compilation_next_error(n, status, different_file, point)
        integer(int64), intent(in) :: n
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: different_file
        integer(int64), intent(in), optional :: point
        integer :: message

        call next_error_1(n, status, message, different_file, point)
    end subroutine compilation_next_error

    !> compilation-next-error, giving the message MESSAGE found.
    !> Port of compile.el:compilation-next-error.
    recursive subroutine next_error_1(n_in, status, message, different_file, point_in)
        integer(int64), intent(in) :: n_in
        type(status_type), intent(out) :: status
        integer, intent(out) :: message
        logical, intent(in), optional :: different_file
        integer(int64), intent(in), optional :: point_in
        integer(int64) :: point_value, n, option, limit, res
        type(status_type) :: st
        logical :: different, found, forward
        integer :: location, last, k

        status = status_type()
        message = 0
        different = .false.
        if (present(different_file)) different = different_file

        if (.not. is_compilation_buffer(current_buffer())) then
            call set_error(status, GENERIC_ERROR_CODE, "Not in a compilation buffer")
            return
        end if

        point_value = point()
        if (present(point_in)) point_value = point_in
        n = n_in
        call compilation_ensure_parse(point_value)
        k = entry_of(current_buffer())
        message = message_at(point_value)
        location = 0
        if (message /= 0) location = compilation_buffers(k)%messages(message)%location
        last = 0

        if (n == 0) then
            ! Find message near here.
            if (message == 0) then
                message = message_at(max(point_value - 1, point_minimum()))

                if (message == 0) then
                    call previous_single_property_change(point_value, value_type(name="compilation_message"), res, found, st, &
                                                         line_beginning_position())
                    point_value = res
                    message = message_at(max(point_value - 1, point_minimum()))

                    if (message == 0) then
                        call compilation_next_single_property_change(point_value, "compilation_message", res, found, &
                                                                     line_end_position())
                        point_value = res
                        message = message_at(point_value)
                        if (message == 0) point_value = point()
                    end if
                end if
            end if
        else
            if (location /= 0) last = compilation_buffers(k)%locations(location)%file
            forward = n >= 0

            if (forward) then
                limit = point_maximum()
            else
                ! Don't move "back" to message at or before point.  Pass an
                ! explicit (point-min) to make sure pt has a value.
                call previous_single_property_change(point_value, value_type(name="compilation_message"), res, found, st, &
                                                     point_minimum())
                point_value = res
                limit = point_minimum()
            end if

            ! compilation-loop.
            do while ((forward .and. n > 0) .or. (.not. forward .and. n < 0))
                option = point_value
                call property_change(point_value, res, found)

                if (found) then
                    point_value = res
                else
                    ! Handle the case where the first error message is at
                    ! the start of the buffer, and n < 0.
                    if (message_at(limit) == message_at(option)) then
                        call loop_error()
                        return
                    end if

                    point_value = limit
                end if

                ! prop 'compilation-message usually has 2 changes, on and
                ! off, so re-search if off.
                message = message_at(point_value)

                if (message == 0) then
                    call property_change(point_value, res, found, limit)

                    if (found) then
                        point_value = res
                        message = message_at(point_value)
                    end if

                    if (message == 0) then
                        call loop_error()
                        return
                    end if
                end if

                k = entry_of(current_buffer())
                if (compilation_buffers(k)%messages(message)%type < compilation_skip_threshold%value) cycle

                if (different) then
                    if (location_file(k, message) == last) cycle
                    last = location_file(k, message)
                end if

                if (compilation_skip_visited%value) then
                    if (compilation_buffers(k)%messages(message)%location /= 0) then
                        if (compilation_buffers(k)%locations(compilation_buffers(k)%messages(message)%location)%visited) cycle
                    end if
                end if

                if (compilation_skip_to_next_location%value) then
                    if (compilation_buffers(k)%messages(message)%location == location) cycle
                end if

                ! Count this message only if none of the above are true.
                if (forward) then
                    n = n - 1
                else
                    n = n + 1
                end if
            end do
        end if

        call goto_character(point_value)
        if (message == 0) call set_error(status, USER_ERROR_CODE, "No "//error_stem()//" here")

    contains

        subroutine property_change(from, res, found, limit_position)
            integer(int64), intent(in) :: from
            integer(int64), intent(out) :: res
            logical, intent(out) :: found
            integer(int64), intent(in), optional :: limit_position
            type(status_type) :: st

            if (forward) then
                call compilation_next_single_property_change(from, "compilation_message", res, found, limit_position)
            else if (present(limit_position)) then
                call previous_single_property_change(from, value_type(name="compilation_message"), res, found, st, limit_position)
            else
                call previous_single_property_change(from, value_type(name="compilation_message"), res, found, st)
            end if
        end subroutine property_change

        subroutine loop_error()
            type(handle_type) :: process

            process = get_buffer_process(current_buffer())

            if (forward) then
                if (process%index /= 0) then
                    call set_error(status, USER_ERROR_CODE, "No more "//error_stem()//"s yet")
                else
                    call set_error(status, USER_ERROR_CODE, "Past last "//error_stem())
                end if
            else
                call set_error(status, USER_ERROR_CODE, "Moved back before first "//error_stem())
            end if
        end subroutine loop_error
    end subroutine next_error_1

    !> The file structure of the location of message MESSAGE of entry K (0
    !> for none).  Not a port.
    integer function location_file(k, message) result(fs)
        integer, intent(in) :: k, message

        fs = 0

        if (compilation_buffers(k)%messages(message)%location /= 0) then
            fs = compilation_buffers(k)%locations(compilation_buffers(k)%messages(message)%location)%file
        end if
    end function location_file

    !> The command: move by the prefix argument.
    !>
    !> Move point to the next error in the compilation buffer.
    recursive subroutine execute_compilation_next_error(status)
        type(status_type), intent(out) :: status

        call compilation_next_error(prefix_numeric_value(current_prefix_argument), status)
    end subroutine execute_compilation_next_error

    !> Move point to the previous error in the compilation buffer.
    !> Prefix arg N says how many error messages to move backwards (or
    !> forwards, if negative).  Does NOT find the source line like
    !> \[previous_error].  This is like `compilation_next_error', but moves
    !> in the other direction.  Port of compile.el:compilation-previous-error.
    recursive subroutine compilation_previous_error(n, status)
        integer(int64), intent(in) :: n
        type(status_type), intent(out) :: status

        call compilation_next_error(-n, status)
    end subroutine compilation_previous_error

    !> The command: move back by the prefix argument.
    !>
    !> Move point to the previous error in the compilation buffer.
    recursive subroutine execute_compilation_previous_error(status)
        type(status_type), intent(out) :: status

        call compilation_previous_error(prefix_numeric_value(current_prefix_argument), status)
    end subroutine execute_compilation_previous_error

    !> Move point to the next error for a different file than the current
    !> one.  Prefix arg N says how many files to move forwards (or
    !> backwards, if negative).  Port of compile.el:compilation-next-file.
    recursive subroutine compilation_next_file(n, status)
        integer(int64), intent(in) :: n
        type(status_type), intent(out) :: status

        call compilation_next_error(n, status, .true.)
    end subroutine compilation_next_file

    !> The command: move by the prefix argument.
    !>
    !> Move point to the next error for a different file than the current
    !> one.
    recursive subroutine execute_compilation_next_file(status)
        type(status_type), intent(out) :: status

        call compilation_next_file(prefix_numeric_value(current_prefix_argument), status)
    end subroutine execute_compilation_next_file

    !> Move point to the previous error for a different file than the
    !> current one.  Prefix arg N says how many files to move backwards (or
    !> forwards, if negative).  Port of compile.el:compilation-previous-file.
    recursive subroutine compilation_previous_file(n, status)
        integer(int64), intent(in) :: n
        type(status_type), intent(out) :: status

        call compilation_next_file(-n, status)
    end subroutine compilation_previous_file

    !> The command: move back by the prefix argument.
    !>
    !> Move point to the previous error for a different file than the
    !> current one.
    recursive subroutine execute_compilation_previous_file(status)
        type(status_type), intent(out) :: status

        call compilation_previous_file(prefix_numeric_value(current_prefix_argument), status)
    end subroutine execute_compilation_previous_file

    !> Display the source for current error in another window.  Port of
    !> compile.el:compilation-display-error.
    recursive subroutine compilation_display_error(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: selected
        type(status_type) :: st

        call set_current_error_position(point())
        ! (next-error-no-select 0)
        selected = selected_frame%selected_window
        call next_error(status, 0_int64)
        call select_window(selected, st, norecord=.true.)
    end subroutine compilation_display_error

    !> Make compilation-current-error the position POSITION of the current
    !> buffer.  Not a port.
    subroutine set_current_error_position(position)
        integer(int64), intent(in) :: position
        type(status_type) :: st
        integer :: k

        call make_current_entry(k)

        if (compilation_buffers(k)%current_error_marker%index /= 0) then
            call destroy_marker(compilation_buffers(k)%current_error_marker, st)
        end if

        compilation_buffers(k)%current_error_marker = null_handle()
        compilation_buffers(k)%has_current_error = .true.
        compilation_buffers(k)%current_error_position = position
    end subroutine set_current_error_position

    !> Switch the `compilation_skip_threshold' level.  Port of
    !> compile.el:compilation-set-skip-threshold.
    subroutine compilation_set_skip_threshold(level)
        integer, intent(in) :: level

        compilation_skip_threshold = level

        select case (compilation_skip_threshold%value)
        case (0)
            call message("Skipping Nothing")
        case (1)
            call message("Skipping Info messages")
        case (2)
            call message("Skipping Warnings and info")
        case default
            call message("Skipping .false.")
        end select
    end subroutine compilation_set_skip_threshold

    !> The command: the level is the prefix argument, or the next one,
    !> modulo 3.
    !>
    !> Switch the `compilation_skip_threshold' level.
    subroutine execute_compilation_set_skip_threshold(status)
        type(status_type), intent(out) :: status
        integer(int64) :: level

        status = status_type()

        if (current_prefix_argument%kind /= PREFIX_NONE) then
            level = prefix_numeric_value(current_prefix_argument)
        else
            level = compilation_skip_threshold%value + 1
        end if

        call compilation_set_skip_threshold(int(modulo(level, 3_int64)))
    end subroutine execute_compilation_set_skip_threshold

    !> Visit the source for the error message at point.
    !> Use this command in a compilation log buffer.  Port of
    !> compile.el:compile-goto-error (visiting a directory message is
    !> Deferred: needs dired).
    recursive subroutine compile_goto_error(status)
        type(status_type), intent(out) :: status

        status = status_type()

        if (.not. is_compilation_buffer(current_buffer())) then
            call set_error(status, GENERIC_ERROR_CODE, "Not in a compilation buffer")
            return
        end if

        call compilation_ensure_parse(point())

        if (directory_at(point()) /= 0) then
            call set_error(status, GENERIC_ERROR_CODE, "Visiting directories is not supported yet")
            return
        end if

        call set_current_error_position(point())
        call next_error_internal(status)
    end subroutine compile_goto_error

    !> Update markers in LOCATION, and set MARKER to location pointed by LOCATION.
    !> SCREEN_COLUMNS and FIRST_COLUMN are the value of
    !> `compilation_error_screen_columns' and `compilation_first_column' to
    !> use if they are not set buffer-locally in the target buffer.  Port of
    !> compile.el:compilation--update-markers.
    recursive subroutine compilation_update_markers(location, marker, screen_columns, first_column, status)
        integer, intent(in) :: location
        type(handle_type), intent(in) :: marker
        logical, intent(in) :: screen_columns
        integer(int64), intent(in) :: first_column
        type(status_type), intent(out) :: status
        type(handle_type) :: comp, target, m
        type(status_type) :: st
        character(len=:), allocatable :: filename, directory
        logical :: screen
        integer(int64) :: first, last, original_point, beginning_of_accessible_portion, end_of_accessible_portion, moved
        integer :: k, fs, node, i, id

        comp = current_buffer()
        k = entry_of(comp)
        fs = compilation_buffers(k)%locations(location)%file
        filename = compilation_buffers(k)%files(fs)%filename

        if (allocated(compilation_buffers(k)%files(fs)%directory)) then
            directory = compilation_buffers(k)%files(fs)%directory
            call compilation_find_file(marker, filename, target, status, directory)
        else
            call compilation_find_file(marker, filename, target, status)
        end if

        if (.not. status%ok()) return
        k = entry_of(comp)
        call set_buffer(target, status)
        if (.not. status%ok()) return
        screen = screen_columns
        first = first_column
        ! Obey the compilation-error-screen-columns of the target buffer if
        ! its major mode set it buffer-locally.
        if (compilation_error_screen_columns%is_local()) screen = compilation_error_screen_columns%value
        if (compilation_first_column%is_local()) first = compilation_first_column%value
        last = 1
        original_point = point()
        beginning_of_accessible_portion = point_minimum()
        end_of_accessible_portion = point_maximum()
        call widen()
        call goto_character(point_minimum())

        ! Treat file's found lines in forward order, 1 by 1.
        do node = size(compilation_buffers(k)%files(fs)%lines), 1, -1
            associate (line => compilation_buffers(k)%files(fs)%lines(node))
                ! Else this is a filename without a line#.
                if (line%has_line) then
                    call compilation_beginning_of_line(line%line - last + 1)
                    last = line%line
                end if

                ! Treat line's found columns and store/update a marker for
                ! each.
                do i = 1, size(line%locations)
                    id = line%locations(i)

                    if (compilation_buffers(k)%locations(id)%has_column) then
                        if (compilation_buffers(k)%locations(id)%column == -1) then
                            ! Special case for range end.
                            call end_of_line()
                        else
                            call compilation_move_to_column(compilation_buffers(k)%locations(id)%column, screen, first)
                        end if
                    else
                        call beginning_of_line()
                        moved = skip_characters_forward(" "//TB, st)
                    end if

                    if (compilation_buffers(k)%locations(id)%marker%index /= 0) then
                        call set_marker(compilation_buffers(k)%locations(id)%marker, point(), st)

                        if (.not. st%ok()) then
                            call point_marker(m)
                            compilation_buffers(k)%locations(id)%marker = m
                        end if
                    else
                        call point_marker(m)
                        compilation_buffers(k)%locations(id)%marker = m
                    end if
                end do
            end associate
        end do

        call narrow_to_region(beginning_of_accessible_portion, end_of_accessible_portion, st)
        call goto_character(original_point)
        call set_buffer(comp, st)
    end subroutine compilation_update_markers

    !> The BUFFER and POSITION of the location of the message at POSITION of
    !> the current compilation buffer (FOUND false for none), its markers
    !> updated (the file visited) if they are not set yet; what
    !> grep-edit--prepare-buffer computes with compilation--update-markers.
    !> Not a port.
    recursive subroutine compilation_message_location(position_value, buffer, position, found, status)
        integer(int64), intent(in) :: position_value
        type(handle_type), intent(out) :: buffer
        integer(int64), intent(out) :: position
        logical, intent(out) :: found
        type(status_type), intent(out) :: status
        type(handle_type) :: dummy
        type(status_type) :: st
        integer :: k, message, location

        status = status_type()
        found = .false.
        position = 0
        buffer = null_handle()
        k = entry_of(current_buffer())
        if (k == 0) return
        message = message_at(position_value)
        if (message == 0) return
        location = compilation_buffers(k)%messages(message)%location
        if (location == 0) return

        ! Update the markers if necessary.
        if (compilation_buffers(k)%locations(location)%marker%index /= 0) then
            call marker_buffer(compilation_buffers(k)%locations(location)%marker, buffer, st)
        end if

        if (compilation_buffers(k)%locations(location)%marker%index == 0 .or. .not. is_buffer_live(buffer)) then
            call make_marker(dummy)
            call compilation_update_markers(location, dummy, compilation_error_screen_columns%value, &
                                            compilation_first_column%value, status)
            call destroy_marker(dummy, st)
            if (.not. status%ok()) return
            k = entry_of(current_buffer())
        end if

        if (compilation_buffers(k)%locations(location)%marker%index == 0) return
        call marker_buffer(compilation_buffers(k)%locations(location)%marker, buffer, st)
        call marker_position(compilation_buffers(k)%locations(location)%marker, position, st)
        found = st%ok() .and. is_buffer_live(buffer)
    end subroutine compilation_message_location

    !> Advance to the next error message and visit the file where the
    !> error was.  This is the value of `next_error_function' in
    !> Compilation buffers.  Port of
    !> compile.el:compilation-next-error-function.
    recursive subroutine compilation_next_error_function(n, reset, status)
        integer(int64), intent(in) :: n
        logical, intent(in) :: reset
        type(status_type), intent(out) :: status
        type(handle_type) :: marker, comp, location_marker, end_marker, mbuf
        type(status_type) :: st
        logical :: screen, valid
        integer(int64) :: first, point
        integer :: k, message, location, end_location

        comp = current_buffer()
        call make_current_entry(k)

        if (reset) then
            if (compilation_buffers(k)%current_error_marker%index /= 0) then
                call destroy_marker(compilation_buffers(k)%current_error_marker, st)
            end if

            compilation_buffers(k)%current_error_marker = null_handle()
            compilation_buffers(k)%has_current_error = .false.
        end if

        screen = compilation_error_screen_columns%value
        first = compilation_first_column%value

        if (compilation_buffers(k)%has_current_error) then
            if (compilation_buffers(k)%current_error_marker%index /= 0) then
                call marker_position(compilation_buffers(k)%current_error_marker, point, st)
            else
                point = compilation_buffers(k)%current_error_position
            end if
        else if (compilation_buffers(k)%has_messages_start) then
            if (compilation_buffers(k)%messages_start_marker%index /= 0) then
                call marker_position(compilation_buffers(k)%messages_start_marker, point, st)
            else
                point = compilation_buffers(k)%messages_start_position
            end if
        else
            point = point_minimum()
        end if

        call next_error_1(n, status, message, point_in=point)
        if (.not. status%ok()) return
        k = entry_of(comp)
        location = compilation_buffers(k)%messages(message)%location
        end_location = compilation_buffers(k)%messages(message)%end_location

        if (location == 0) then
            call set_error(status, USER_ERROR_CODE, "No next error")
            return
        end if

        call point_marker(marker)

        if (compilation_buffers(k)%current_error_marker%index /= 0) then
            call destroy_marker(compilation_buffers(k)%current_error_marker, st)
        end if

        call point_marker(compilation_buffers(k)%current_error_marker)
        compilation_buffers(k)%has_current_error = .true.
        ! (overlay-arrow-position is Deferred.)
        ! If loc contains no marker, no error in that file has been
        ! visited.  If the marker is invalid the buffer has been closed.
        ! So, recalculate all markers for that file.
        valid = .false.

        if (compilation_buffers(k)%locations(location)%marker%index /= 0) then
            call marker_buffer(compilation_buffers(k)%locations(location)%marker, mbuf, st)
            valid = st%ok() .and. mbuf%index /= 0
        end if

        if (.not. valid) then
            call compilation_update_markers(location, marker, screen, first, status)

            if (.not. status%ok()) then
                call destroy_marker(marker, st)
                return
            end if
        end if

        k = entry_of(comp)
        location_marker = compilation_buffers(k)%locations(location)%marker
        end_marker = null_handle()
        if (end_location /= 0) end_marker = compilation_buffers(k)%locations(end_location)%marker
        call compilation_goto_locus(marker, location_marker, end_marker, status)
        call destroy_marker(marker, st)
        if (.not. status%ok()) return
        k = entry_of(comp)
        if (k /= 0) compilation_buffers(k)%locations(location)%visited = .true.
    end subroutine compilation_next_error_function

    !> The command: visit the error the prefix argument says.
    !>
    !> Advance to the next error message and visit the file where the
    !> error was.
    recursive subroutine execute_compilation_next_error_function(status)
        type(status_type), intent(out) :: status

        call compilation_next_error_function(prefix_numeric_value(current_prefix_argument), .false., status)
    end subroutine execute_compilation_next_error_function

    !> Maybe align the compilation output window W with marker MK near
    !> top.  Port of compile.el:compilation-set-window (the terminal has no
    !> fringe).
    subroutine compilation_set_window(w, mk)
        type(handle_type), intent(in) :: w, mk
        type(status_type) :: st
        integer(int64) :: position, original_point, moved

        call marker_position(mk, position, st)
        original_point = point()

        if (compilation_context%lines >= 0) then
            call goto_character(position)
            call compilation_beginning_of_line(1 - compilation_context%lines)
            call set_window_start(windows(w%index), point(), st)
            call goto_character(original_point)
        else if (compilation_context%choice == COMPILATION_CONTEXT_TOP) then
            ! There is no left fringe.
            call goto_character(position)
            moved = forward_line(0_int64)
            call set_window_start(windows(w%index), line_beginning_position(), st)
            call goto_character(original_point)
        end if

        call set_window_point(windows(w%index), position, st)
    end subroutine compilation_set_window

    !> Jump to an error corresponding to MESSAGE at MK.
    !> All arguments are markers.  If END_MK is not the null handle, mark
    !> is set there.  Port of compile.el:compilation-goto-locus
    !> (highlighting the locus is Deferred).
    recursive subroutine compilation_goto_locus(message, mk, end_mk, status)
        type(handle_type), intent(in) :: message, mk, end_mk
        type(handle_type) :: message_buffer, mk_buffer, pre_existing, w, selected
        type(display_action_parameters_type) :: table
        type(status_type) :: st
        type(status_type), intent(out) :: status
        logical :: from_compilation_buffer, saved_pop_up
        integer(int64) :: position

        status = status_type()
        call marker_buffer(message, message_buffer, st)
        call marker_buffer(mk, mk_buffer, st)

        if (mk_buffer%index == 0) then
            call set_error(status, GENERIC_ERROR_CODE, "Marker does not point anywhere")
            return
        end if

        ! Show compilation buffer in other window, scrolled to this error.
        selected = selected_frame%selected_window
        from_compilation_buffer = same(windows(selected%index)%buffer, message_buffer)
        ! Use an existing window if it is in a visible frame.
        pre_existing = get_buffer_window(message_buffer)

        if (from_compilation_buffer .and. pre_existing%index /= 0) then
            w = pre_existing
        else
            call display_buffer(message_buffer, w, st)
        end if

        call set_buffer(message_buffer, st)
        call marker_position(message, position, st)
        call goto_character(position)
        if (w%index /= 0) call compilation_set_window(w, message)

        if (from_compilation_buffer .or. same(w, selected)) then
            ! If the compilation buffer window is selected, keep the
            ! compilation buffer in this window; display the source in
            ! another window.
            saved_pop_up = pop_up_windows
            pop_up_windows = .true.
            table%inhibit_same_window = .true.
            call pop_to_buffer(mk_buffer, status, table=table)
            pop_up_windows = saved_pop_up
        else
            call switch_to_buffer(buffers(mk_buffer%index)%name, status)
        end if

        if (.not. status%ok()) return
        call set_buffer(mk_buffer, st)
        call marker_position(mk, position, st)
        ! If narrowing gets in the way of going to the right place, widen.
        call goto_character(position)

        if (point() /= position) then
            call widen()
            call goto_character(position)
        end if

        if (end_mk%index /= 0) then
            call marker_position(end_mk, position, st)
            if (st%ok()) call push_mark(position, nomsg=.true.)
        else
            if (buffers(current)%mark_active) buffers(current)%mark_active = .false.
        end if
    end subroutine compilation_goto_locus

    !> Expand FILENAME in DIRECTORY, or resolve its true name.  Port of
    !> compile.el:compilation--expand-fn.
    function compilation_expand_fn(directory, filename) result(name)
        character(len=*), intent(in) :: directory, filename
        character(len=:), allocatable :: name
        type(status_type) :: st

        call expand_file_name(filename, name, st, directory)
        if (file_exists(name)) return
        call file_truename(file_name_as_directory(directory)//filename, name, st)
        if (.not. st%ok()) call expand_file_name(filename, name, st, directory)
    end function compilation_expand_fn

    !> Search for FILENAME: in DIRECTORY (or `default_directory'), then
    !> when `compilation_search_all_directories', in the directories
    !> entered before MARKER.  The buffer found (null if none) and the
    !> directory searched.  Port of compile.el:compilation-find-file-1
    !> (`compilation_search_path' is Deferred: only its default, the
    !> directory, is searched).
    recursive subroutine compilation_find_file_1(marker, filename_in, buffer, specification_directory, status, directory)
        type(handle_type), intent(in) :: marker
        character(len=*), intent(in) :: filename_in
        type(handle_type), intent(out) :: buffer
        character(len=:), allocatable, intent(out) :: specification_directory
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: directory
        character(len=:), allocatable :: filename, name, absolute, thisdir
        type(handle_type) :: mbuf, old
        type(status_type) :: st
        integer(int64) :: position, original_point
        integer :: d, k
        logical :: found

        status = status_type()
        buffer = null_handle()

        if (present(directory)) then
            call expand_file_name(directory, specification_directory, st, default_directory())
        else
            specification_directory = default_directory()
        end if

        filename = filename_in
        thisdir = specification_directory

        if (is_file_name_absolute(filename)) then
            ! The file name is absolute.  Use its explicit directory as the
            ! first in the search path, and strip it from FILENAME.
            call expand_file_name(filename, absolute, st, default_directory())
            filename = abbreviate_file_name(absolute)
            thisdir = file_name_directory(filename)
            filename = file_name_nondirectory(filename)
            name = compilation_expand_fn(thisdir, filename)

            if (file_exists(name)) then
                call find_file_noselect(name, buffer, status)
                return
            end if

            ! Then the nil of compilation-search-path.
            thisdir = specification_directory
        end if

        name = compilation_expand_fn(thisdir, filename)

        if (file_exists(name)) then
            call find_file_noselect(name, buffer, status)
            return
        end if

        ! If we haven't found it, this might be a parallel build.  Search
        ! the directories further up the buffer.
        if (.not. compilation_search_all_directories%value) return
        call marker_buffer(marker, mbuf, st)
        if (mbuf%index == 0) return
        old = current_buffer()
        call set_buffer(mbuf, st)
        k = entry_of(mbuf)
        original_point = point()
        call marker_position(marker, position, st)
        call goto_character(position)
        call previous_single_property_change(point(), value_type(name="compilation_directory"), position, found, st)
        if (found) call goto_character(position)
        d = directory_at(point() - 1)
        if (d == 0) d = directory_at(point())
        call goto_character(original_point)
        ! (cdr dirs): the directories before the innermost.
        if (d /= 0) d = compilation_buffers(k)%directories(d)%parent

        do while (d /= 0)
            if (allocated(compilation_buffers(k)%directories(d)%directory)) then
                thisdir = compilation_buffers(k)%directories(d)%directory
                name = compilation_expand_fn(thisdir, filename)

                if (file_exists(name)) then
                    call set_buffer(old, st)
                    call find_file_noselect(name, buffer, status)
                    return
                end if
            end if

            k = entry_of(mbuf)
            d = compilation_buffers(k)%directories(d)%parent
        end do

        call set_buffer(old, st)
    end subroutine compilation_find_file_1

    !> The current buffer's default directory.  Not a port.
    function default_directory() result(directory)
        character(len=:), allocatable :: directory

        directory = ""
        if (current == 0) return
        if (allocated(buffers(current)%directory)) directory = buffers(current)%directory
    end function default_directory

    !> Find a buffer for file FILENAME.
    !> If FILENAME is not found at all, ask the user where to find it.
    !> Pop up the buffer containing MARKER and scroll to MARKER if we ask
    !> the user where to find the file.  The directory searched is
    !> DIRECTORY; if it is relative, it is combined with
    !> `default_directory'; if absent, `default_directory' is used.  Port of
    !> compile.el:compilation-find-file (FORMATS and
    !> `compilation_search_path' are Deferred).
    recursive subroutine compilation_find_file(marker, filename, buffer, status, directory)
        use minibuffer_mod, only: read_file_name
        use display_update_mod, only: ding
        type(handle_type), intent(in) :: marker
        character(len=*), intent(in) :: filename
        type(handle_type), intent(out) :: buffer
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: directory
        character(len=:), allocatable :: specification_directory, name, origname
        type(handle_type) :: mbuf, w, old
        type(status_type) :: st
        integer(int64) :: position, original_point
        logical :: saved_pop_up

        call compilation_find_file_1(marker, filename, buffer, specification_directory, status, directory)
        if (.not. status%ok()) return

        do while (buffer%index == 0)
            ! The file doesn't exist.  Ask the user where to find it.
            call marker_buffer(marker, mbuf, st)
            old = current_buffer()
            original_point = point()

            if (mbuf%index /= 0) then
                saved_pop_up = pop_up_windows
                pop_up_windows = .true.
                call display_buffer(mbuf, w, st)
                pop_up_windows = saved_pop_up
                call set_buffer(mbuf, st)
                call marker_position(marker, position, st)
                call goto_character(position)
                if (w%index /= 0) call compilation_set_window(w, marker)
                call set_buffer(old, st)
                call goto_character(original_point)
            end if

            call read_file_name("Find this "//error_stem()//" in (default "//filename//"): ", name, status, &
                                directory=specification_directory, default_filename=filename, mustmatch=1)
            if (.not. status%ok()) return
            origname = name

            if (.not. file_exists(name)) then
                call message(substitute_quotes("Cannot find file `")//name//substitute_quotes("'"))
                call ding(status=st)
                call sit_for(2000_int32)
            else if (is_file_directory(name)) then
                name = compilation_expand_fn(name, filename)

                if (.not. file_exists(name)) then
                    call message(substitute_quotes("No `")//filename//substitute_quotes("' in directory ")//origname)
                    call ding(status=st)
                    call sit_for(2000_int32)
                else
                    call find_file_noselect(name, buffer, status)
                    if (.not. status%ok()) return
                end if
            else
                call find_file_noselect(name, buffer, status)
                if (.not. status%ok()) return
            end if
        end do
    end subroutine compilation_find_file

    ! ------------------------------------------------------------------
    ! Compilation mode and the minor modes
    ! ------------------------------------------------------------------

    !> Major mode for compilation log buffers.
    !> \<compilation_mode_map>To visit the source for a line-numbered
    !> error, move point to the error message line and type
    !> \[compile_goto_error].  To kill the compilation, type
    !> \[stop_compilation].
    !>
    !> Runs `compilation_mode_hook' with `execute_mode_hooks' (which see).
    !> Port of compile.el:compilation-mode (NAME-OF-MODE "Compilation").
    recursive subroutine compilation_mode(status)
        type(status_type), intent(out) :: status
        logical :: saved

        if (.not. associated(major_modes%compilation_mode%execute)) call initialize_compile()
        ! kill-all-local-variables, use-local-map, major-mode, mode-name.
        call major_modes%compilation_mode%begin(saved, status)
        if (.not. status%ok()) return
        call compilation_setup(status)
        if (.not. status%ok()) return
        buffers(current)%read_only = .true.
        call major_modes%compilation_mode%end(saved, status)
    end subroutine compilation_mode

    !> Prepare the buffer for the compilation parsing commands to work.
    !> Optional argument MINOR indicates this is called from
    !> `compilation_minor_mode'.  Port of compile.el:compilation-setup.
    recursive subroutine compilation_setup(status, minor)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: minor
        type(font_lock_keyword_type), allocatable :: keywords(:)
        logical :: is_minor
        integer :: k

        status = status_type()
        is_minor = .false.
        if (present(minor)) is_minor = minor
        call make_current_entry(k)
        compilation_buffers(k)%has_counts = .true.
        compilation_buffers(k)%error_count = 0
        compilation_buffers(k)%warning_count = 0
        compilation_buffers(k)%info_count = 0
        ! Note that compilation-next-error-function is for interfacing
        ! with the next-error function in simple.el, and it's only
        ! coincidentally named similarly to compilation-next-error.
        call set_next_error_function(compilation_next_error_function)
        ! compilation-locs.
        compilation_buffers(k)%setup = .true.
        compilation_buffers(k)%nkeys = 0
        call before_change_functions%prepend(compilation_flush_parse, local=.true.)
        ! Also for minor mode, since it's not permanent-local.
        call change_major_mode_hook%prepend(remove_properties_hook, local=.true.)
        call compilation_mode_font_lock_keywords(keywords)

        if (is_minor) then
            call font_lock_add_keywords("", keywords, status)
            if (.not. status%ok()) return
            call font_lock_flush(status)
        else
            call font_lock_defaults%set_local(keywords=keywords, keywords_only=.true.)
        end if
    end subroutine compilation_setup

    !> Undo `compilation_setup' for the minor modes.  Port of
    !> compile.el:compilation--unsetup (removing the font-lock keywords is
    !> Deferred).
    recursive subroutine compilation_unsetup(status)
        type(status_type), intent(out) :: status
        type(status_type) :: st
        integer :: k

        status = status_type()
        call before_change_functions%remove(compilation_flush_parse, local=.true.)
        k = entry_of(current_buffer())

        if (k /= 0) then
            if (compilation_buffers(k)%parsed%index /= 0) call destroy_marker(compilation_buffers(k)%parsed, st)
            compilation_buffers(k)%parsed = null_handle()
        end if

        call compilation_remove_properties()
        call font_lock_flush(status)
    end subroutine compilation_unsetup

    !> Return expressions to highlight in Compilation mode: parsing, then
    !> what compile.el compilation-mode-font-lock-keywords highlights (the
    !> face variables are read when the keywords are made).  Port of
    !> compile.el:compilation-mode-font-lock-keywords.
    subroutine compilation_mode_font_lock_keywords(keywords)
        type(font_lock_keyword_type), allocatable, intent(out) :: keywords(:)

        allocate (keywords(6))
        keywords(1)%matcher => ensure_parse_matcher
        allocate (keywords(1)%highlights(0))
        ! configure output lines.  (compilation-face '(4 . 3)) on 2: the
        ! error face, unless 3 (info) or 4 (warning), which are all of 2,
        ! matched.
        keywords(2)%regular_expression = "^[Cc]hecking \(?:[Ff]or \|[Ii]f \|[Ww]hether \(?:to \)?\)?\(.+\)\.\.\. *" &
                                         //"\(?:(cached) *\)?\(\(yes\(?: .+\)?\)\|no\|\(.*\)\)$"
        allocate (keywords(2)%highlights(4))
        call set_highlight(keywords(2)%highlights(1), 1, "font_lock_variable_name_face")
        call set_highlight(keywords(2)%highlights(2), 2, setting_string(compilation_error_face))
        call set_highlight(keywords(2)%highlights(3), 3, setting_string(compilation_info_face), .true.)
        call set_highlight(keywords(2)%highlights(4), 4, setting_string(compilation_warning_face), .true.)
        ! Command output lines.  Recognize `make[n]:' lines too.
        keywords(3)%regular_expression = "^\([[:alnum:]_/.+-]+\)\(\[\([0-9]+\)\]\)?["//TB//" ]*:"
        allocate (keywords(3)%highlights(2))
        call set_highlight(keywords(3)%highlights(1), 1, "font_lock_function_name_face")
        call set_highlight(keywords(3)%highlights(2), 3, setting_string(compilation_line_face), laxmatch=.true.)
        keywords(4)%regular_expression = " --?o\(?:utfile\|utput\)?[= ]\(\S +\)"
        allocate (keywords(4)%highlights(1))
        call set_highlight(keywords(4)%highlights(1), 1, "font_lock_keyword_face")
        keywords(5)%regular_expression = "^Compilation \(finished\).*"
        allocate (keywords(5)%highlights(1))
        call set_highlight(keywords(5)%highlights(1), 1, setting_string(compilation_info_face))
        keywords(6)%regular_expression = "^Compilation \(exited abnormally\|interrupt\|killed\|terminated\|" &
                                         //"segmentation fault\)\(?:.*with code \([0-9]+\)\)?.*"
        allocate (keywords(6)%highlights(2))
        call set_highlight(keywords(6)%highlights(1), 1, setting_string(compilation_error_face))
        call set_highlight(keywords(6)%highlights(2), 2, setting_string(compilation_error_face), laxmatch=.true.)
    end subroutine compilation_mode_font_lock_keywords

    !> Make H the MATCH-HIGHLIGHT of SUBEXP in FACE; OVERRIDE true for an
    !> optional (lax) highlight replacing the faces there, LAXMATCH true
    !> for one that need not match.  Not a port.
    subroutine set_highlight(h, subexp, face, override, laxmatch)
        type(font_lock_highlight_type), intent(inout) :: h
        integer, intent(in) :: subexp
        character(len=*), intent(in) :: face
        logical, intent(in), optional :: override, laxmatch

        h%match%subexp = subexp
        h%match%face = face
        h%match%laxmatch = .false.
        if (present(laxmatch)) h%match%laxmatch = laxmatch

        if (present(override)) then
            if (override) then
                h%match%override = FONT_LOCK_OVERRIDE_REPLACE
                h%match%laxmatch = .true.
            end if
        end if
    end subroutine set_highlight

    !> Toggle Compilation minor mode.
    !>
    !> When Compilation minor mode is enabled, all the error-parsing
    !> commands of Compilation major mode are available.  See
    !> `compilation_mode'.  Port of compile.el:compilation-minor-mode.
    recursive subroutine compilation_minor_mode(status)
        type(status_type), intent(out) :: status

        call minor_modes%compilation_minor_mode%command(status)
    end subroutine compilation_minor_mode

    !> The body of Compilation minor mode.  Port of
    !> compile.el:compilation-minor-mode (its body).
    recursive subroutine compilation_minor_mode_body(enable, status)
        logical, intent(in) :: enable
        type(status_type), intent(out) :: status

        if (enable) then
            call compilation_setup(status, .true.)
        else
            call compilation_unsetup(status)
        end if
    end subroutine compilation_minor_mode_body

    !> Toggle Compilation Shell minor mode.
    !>
    !> When Compilation Shell minor mode is enabled, all the error-parsing
    !> commands of the Compilation major mode are available but bound to
    !> keys that don't collide with Shell mode.  See `compilation_mode'.
    !> Port of compile.el:compilation-shell-minor-mode.
    recursive subroutine compilation_shell_minor_mode(status)
        type(status_type), intent(out) :: status

        call minor_modes%compilation_shell_minor_mode%command(status)
    end subroutine compilation_shell_minor_mode

    ! ------------------------------------------------------------------
    ! Starting
    ! ------------------------------------------------------------------

    !> The default value of `compile_command': "make -k -jN ", N being
    !> the number of processors divided by 1.5, rounded up.  Not a port.
    function default_compile_command() result(command)
        character(len=:), allocatable :: command
        character(len=16) :: n
        integer :: p

        p = number_processors()
        write (n, "(i0)") (2*p + 2)/3
        command = "make -k -j"//trim(n)//" "
    end function default_compile_command

    !> Read the compilation command, COMMAND as initial input.  Port of
    !> compile.el:compilation-read-command.
    subroutine compilation_read_command_from_user(command, result, status)
        use value_mod, only: xstring, list_element, value_type
        character(len=*), intent(in) :: command
        character(len=:), allocatable, intent(out) :: result
        type(status_type), intent(out) :: status
        type(value_type) :: history, first
        logical :: same_first

        same_first = .false.
        call symbol_value(value_type(name="compile_history"), history, status)
        if (status%ok()) first = list_element(history, 1_int64)

        if (status%ok()) then
            if (vtag(first) == V_STRING) same_first = xstring(first) == command .and. len(xstring(first)) == len(command)
        end if

        if (same_first) then
            call read_shell_command("Compile command: ", result, status, initial_input=command, history="compile_history", &
                                    history_position=1)
        else
            call read_shell_command("Compile command: ", result, status, initial_input=command, history="compile_history")
        end if
    end subroutine compilation_read_command_from_user

    !> Compile the program including the current buffer.  Default: run
    !> `make'.
    !> Runs COMMAND, a shell command, in a separate process asynchronously
    !> with output going to the buffer `*compilation*'.
    !>
    !> You can then use the command \[next_error] to find the next error
    !> message and move to the source code that caused it.
    !>
    !> If optional second arg COMMAND_INTERPRETER is true the buffer will be in Comint
    !> mode with `compilation_shell_minor_mode'.
    !>
    !> Interactively, prompts for the command if the variable
    !> `compilation_read_command' is true; otherwise uses
    !> `compile_command'.  With prefix arg, always prompts.
    !> Additionally, with universal prefix arg, compilation buffer will be
    !> in comint mode, i.e. interactive.
    !>
    !> To run more than one compilation at once, start one then rename
    !> the `*compilation*' buffer to some other name with
    !> \[rename_buffer].  Then _switch buffers_ and start the new
    !> compilation.  It will create a new `*compilation*' buffer.
    !>
    !> On most systems, termination of the main compilation process kills
    !> its subprocesses.  Port of compile.el:compile.
    recursive subroutine compile(command, status, command_interpreter)
        character(len=*), intent(in) :: command
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: command_interpreter
        character(len=:), allocatable :: directory

        if (command /= compile_command_value() .or. len(command) /= len(compile_command_value())) then
            compile_command = command
        end if

        call save_some_buffers(.not. compilation_ask_about_save%value, .false., status)
        if (.not. status%ok()) return
        directory = ""
        if (allocated(buffers(current)%directory)) directory = buffers(current)%directory
        ! setq-default: the value of buffers without their own.
        compilation_directory = directory
        call compilation_start(command, status, command_interpreter)
    end subroutine compile

    !> `compile_command', set to its default value first if needed.
    !> Not a port.
    function compile_command_value() result(command)
        character(len=:), allocatable :: command

        if (.not. allocated(compile_command%value)) compile_command = default_compile_command()
        command = compile_command%value
    end function compile_command_value

    !> The command: interactively, read the command (if
    !> `compilation_read_command' or with a prefix argument), then compile,
    !> in Comint mode with C-u.  Port of the command of compile.el:compile.
    !>
    !> Compile the program including the current buffer.  Default: run
    !> `make'.
    recursive subroutine execute_compile(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: command, initial_input

        command = compile_command_value()

        if (compilation_read_command%value .or. current_prefix_argument%kind /= PREFIX_NONE) then
            initial_input = command
            call compilation_read_command_from_user(initial_input, command, status)
            if (.not. status%ok()) return
        end if

        call compile(command, status, current_prefix_argument%kind == PREFIX_RAW)
    end subroutine execute_compile

    !> Re-compile the program including the current buffer.
    !> If this is run in a Compilation mode buffer, reuse the arguments from
    !> the original use.  Otherwise, recompile using `compile_command'.
    !> If the optional argument `edit_command' is true, the command can be
    !> edited.  Port of compile.el:recompile.
    recursive subroutine recompile(status, edit_command)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: edit_command
        character(len=:), allocatable :: command, saved_directory, directory, saved_command
        logical :: edit, had_directory, command_interpreter
        type(handle_type) :: old
        type(major_mode_type), pointer :: mode
        integer :: k

        edit = .false.
        if (present(edit_command)) edit = edit_command
        call save_some_buffers(.not. compilation_ask_about_save%value, .false., status)
        if (.not. status%ok()) return
        old = current_buffer()
        ! (let ((default-directory (or compilation-directory default-directory)))
        had_directory = allocated(buffers(current)%directory)
        if (had_directory) saved_directory = buffers(current)%directory
        directory = compilation_directory_value()
        if (len(directory) > 0) buffers(current)%directory = directory
        command = compile_command_value()
        k = entry_of(current_buffer())

        if (k /= 0) then
            if (.not. compilation_buffers(k)%has_arguments) k = 0
        end if

        if (edit) then
            if (k /= 0) then
                call compilation_read_command_from_user(compilation_buffers(k)%command, command, status)
            else
                saved_command = command
                call compilation_read_command_from_user(saved_command, command, status)
            end if

            if (.not. status%ok()) then
                call restore_directory()
                return
            end if

            k = entry_of(current_buffer())

            if (k /= 0) then
                if (compilation_buffers(k)%has_arguments) compilation_buffers(k)%command = command
            end if
        end if

        command_interpreter = .false.
        mode => null()

        if (k /= 0) then
            command = compilation_buffers(k)%command
            command_interpreter = compilation_buffers(k)%command_interpreter
            mode => compilation_buffers(k)%mode
        end if

        if (associated(mode)) then
            call compilation_start(command, status, command_interpreter, mode)
        else
            call compilation_start(command, status, command_interpreter)
        end if

        call restore_directory()

    contains

        subroutine restore_directory()
            type(status_type) :: st

            ! Restore the saved value in the buffer that was current.
            if (.not. is_buffer_live(old)) return
            call set_buffer(old, st)

            if (had_directory) then
                buffers(current)%directory = saved_directory
            else if (allocated(buffers(current)%directory)) then
                deallocate (buffers(current)%directory)
            end if
        end subroutine restore_directory
    end subroutine recompile

    !> The current buffer's `compilation_directory' ("" for none).  Not a port.
    function compilation_directory_value() result(directory)
        character(len=:), allocatable :: directory
        integer :: k

        directory = ""
        k = entry_of(current_buffer())

        if (k /= 0) then
            if (allocated(compilation_buffers(k)%directory)) then
                directory = compilation_buffers(k)%directory
                return
            end if
        end if

        if (allocated(compilation_directory%value)) directory = compilation_directory%value
    end function compilation_directory_value

    !> The command: recompile, editing the command with a prefix argument.
    !>
    !> Re-compile the program including the current buffer.
    recursive subroutine execute_recompile(status)
        type(status_type), intent(out) :: status

        call recompile(status, current_prefix_argument%kind /= PREFIX_NONE)
    end subroutine execute_recompile

    !> Insert TEXT at point, adding the `compilation_annotation' text
    !> property.  This property is used to distinguish output of the
    !> compilation process from additional information inserted by Fortmacs.
    !> Port of compile.el:compilation-insert-annotation.
    subroutine compilation_insert_annotation(text)
        character(len=*), intent(in) :: text
        type(status_type) :: st
        integer(int64) :: start

        start = point()
        call insert(text, st)
        call put_text_property(start, point(), value_type(name="compilation_annotation"), value_type(.true.), st)
    end subroutine compilation_insert_annotation

    !> The first 19 characters of `current_time_string': "Sun Sep 16
    !> 01:03:52", as compile.el takes them with (substring
    !> (current-time-string) 0 19).  Not a port.
    function current_time_19() result(s)
        character(len=:), allocatable :: s

        s = current_time_string()
        s = s(1:19)
    end function current_time_19

    !> The name of MODE (its mode-name) in lower case.  Port of
    !> compile.el:compilation--downcase-mode-name.
    function downcase_name(name) result(s)
        character(len=*), intent(in) :: name
        character(len=:), allocatable :: s
        integer :: i

        s = name

        do i = 1, len(s)
            if (s(i:i) >= "A" .and. s(i:i) <= "Z") s(i:i) = achar(iachar(s(i:i)) + 32)
        end do
    end function downcase_name

    !> Run compilation command COMMAND (low level interface).
    !> If COMMAND starts with a cd command, that becomes the
    !> `default_directory'.
    !>
    !> MODE is the major mode to set in the compilation buffer
    !> (`compilation_mode' if absent).  COMMAND_INTERPRETER true means use
    !> `compilation_shell_minor_mode' under `command_interpreter_mode' instead (MODE t).
    !>
    !> The buffer is the current buffer if it has the proper major mode,
    !> else a buffer with a name based on the major mode (*compilation*,
    !> *grep*).  Port of compile.el:compilation-start (NAME-FUNCTION,
    !> HIGHLIGHT-REGEXP and CONTINUE are Deferred).
    recursive subroutine compilation_start(command, status, command_interpreter, mode)
        character(len=*), intent(in) :: command
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: command_interpreter
        type(major_mode_type), intent(in), target, optional :: mode
        character(len=:), allocatable :: thisdir, cd_directory, origdir, name_of_mode
        type(handle_type) :: outbuf, comp_process, outwin, old, process
        type(string_type), allocatable :: arguments(:), switches(:), environment_variables(:)
        type(value_type) :: inhibit
        type(status_type) :: st
        logical :: answer, scroll, use_command_interpreter
        integer(int64) :: desired
        integer :: k

        use_command_interpreter = .false.
        if (present(command_interpreter)) use_command_interpreter = command_interpreter

        if (use_command_interpreter .or. .not. present(mode)) then
            name_of_mode = "compilation"
        else
            name_of_mode = trim(mode%name)

            if (len(name_of_mode) > 5) then
                if (name_of_mode(len(name_of_mode) - 4:) == "_mode") name_of_mode = name_of_mode(:len(name_of_mode) - 5)
            end if
        end if

        old = current_buffer()
        thisdir = ""
        if (allocated(buffers(current)%directory)) thisdir = buffers(current)%directory

        ! compilation--default-buffer-name.
        if (buffers(current)%major_mode == name_of_mode .or. buffers(current)%major_mode == name_of_mode//"_mode") then
            outbuf = current_buffer()
        else
            call get_or_make_buffer("*"//downcase_name(name_of_mode)//"*", outbuf, status)
            if (.not. status%ok()) return
        end if

        call set_buffer(outbuf, status)
        if (.not. status%ok()) return
        comp_process = get_buffer_process(outbuf)

        if (comp_process%index /= 0) then
            answer = process_status(comp_process) /= "run" .or. .not. process_query_on_exit_flag(comp_process)

            if (.not. answer) then
                call read_yes_or_no("A "//name_of_mode//" process is running; kill it? ", answer, status)
                if (.not. status%ok()) return
            end if

            if (answer) then
                call interrupt_process(st, comp_process)

                if (st%ok()) then
                    call sit_for(1000_int32)
                    call delete_process(comp_process, st)
                end if
            else
                call set_error(status, GENERIC_ERROR_CODE, substitute_quotes("Cannot have two processes in `")// &
                               buffers(current)%name//substitute_quotes("' at once"))
                call set_buffer(old, st)
                return
            end if
        end if

        ! First transfer directory from where M-x compile was called.
        buffers(current)%directory = thisdir
        ! Make compilation buffer read-only.  The filter can still write it.
        ! Clear out the compilation buffer.
        call symbol_value(value_type(name="inhibit_read_only"), inhibit, st)
        call set(value_type(name="inhibit_read_only"), value_type(.true.), st)
        ! Then evaluate a cd command if any, but don't perform it yet, else
        ! start-command would do it again through the shell: (cd "..") AND
        ! sh -c "cd ..; make"
        call command_cd_directory(command, thisdir, cd_directory)
        call cd(cd_directory, status)

        if (.not. status%ok()) then
            call set(value_type(name="inhibit_read_only"), inhibit, st)
            buffers(current)%directory = thisdir
            return
        end if

        call erase_buffer(st)
        ! Select the desired mode.
        origdir = buffers(current)%directory

        if (.not. use_command_interpreter) then
            buffers(current)%undo_enabled = .false.
            if (allocated(buffers(current)%undo_entries)) deallocate (buffers(current)%undo_entries)

            if (present(mode)) then
                call mode%execute(status)
            else
                call compilation_mode(status)
            end if
        else
            buffers(current)%read_only = .false.
            call command_interpreter_mode(status)
            if (status%ok()) call minor_modes%compilation_shell_minor_mode%enable(status)
        end if

        if (.not. status%ok()) then
            call set(value_type(name="inhibit_read_only"), inhibit, st)
            return
        end if

        ! Remember the original dir, so we can use it when we recompile.
        call make_current_entry(k)
        compilation_buffers(k)%directory = thisdir

        if (buffer_size() == 0) then
            ! Output a mode setter, for saving and later reloading this
            ! buffer.
            call compilation_insert_annotation("-*- mode: "//name_of_mode//"; default-directory: "// &
                                               readable_text(value_type(text=abbreviate_file_name(origdir)))// &
                                               " -*-"//NEWLINE)
        end if

        call compilation_insert_annotation(buffers(current)%mode_name//" started at "//current_time_19()//NEWLINE//NEWLINE// &
                                           command//NEWLINE)
        call make_current_entry(k)
        call system_clock(compilation_buffers(k)%start_count)
        call set(value_type(name="inhibit_read_only"), inhibit, st)
        ! The let binding of default-directory ends here.
        buffers(current)%directory = thisdir
        call restore_buffer_modified(.false.)
        ! Pop up the compilation buffer.
        call display_buffer(outbuf, outwin, st)
        scroll = .not. compilation_scroll_output%is_off()
        call make_current_entry(k)
        compilation_buffers(k)%has_arguments = .true.
        compilation_buffers(k)%command = command
        compilation_buffers(k)%command_interpreter = use_command_interpreter
        compilation_buffers(k)%mode => null()
        if (present(mode) .and. .not. use_command_interpreter) compilation_buffers(k)%mode => mode
        if (outwin%index /= 0 .and. .not. scroll) call set_window_start(windows(outwin%index), point_minimum(), st)

        ! Position point as the user will see it.
        if (scroll) then
            desired = point_maximum()
        else
            desired = point_minimum()
        end if

        call goto_character(desired)

        if (outwin%index /= 0 .and. .not. same(outwin, selected_frame%selected_window)) then
            call set_window_point(windows(outwin%index), desired, st)
        end if

        ! Some external programs (like "git grep") use a pager; defeat that.
        call append_string(environment_variables, "INSIDE_EMACS="//upstream_version//",compile", "PAGER=")
        ! The setup function is called before compilation-set-window-height
        ! so it can set the compilation-window-height buffer locally.
        call make_current_entry(k)

        if (associated(compilation_buffers(k)%process_setup)) then
            call compilation_buffers(k)%process_setup(environment_variables, status)

            if (.not. status%ok()) then
                call set_buffer(old, st)
                return
            end if
        end if

        ! Start the compilation.
        if (.not. allocated(shell_file_name%value)) call initialize_callproc()

        if (use_command_interpreter) then
            call append_string(switches, shell_command_switch%value, command)
            call command_interpreter_exec(outbuf, downcase_name(buffers(current)%mode_name), shell_file_name%value, switches, &
                                          status, &
                                          environment=environment_variables)
            if (status%ok()) process = get_buffer_process(outbuf)
        else
            call append_string(arguments, shell_file_name%value, shell_command_switch%value, command)
            call make_process(downcase_name(buffers(current)%mode_name), arguments, process, status, buffer=outbuf, &
                              environment=environment_variables)
        end if

        if (.not. status%ok()) then
            call set_buffer(old, st)
            return
        end if

        ! Make the buffer's mode line show process state.
        call make_current_entry(k)
        compilation_buffers(k)%mode_line = 1
        call update_mode_line_process(k)
        ! Set the process as killable without query by default.
        if (compilation_always_stop%value) call set_process_query_on_exit_flag(process, .false., st)
        call set_process_sentinel_and_filter(process,.not. use_command_interpreter)
        ! Use (point-max) here so that output comes in after the initial
        ! text, regardless of where the user sees point.
        call set_marker(process_mark(process), point_maximum(), st, outbuf)
        if (compilation_disable_input%value) call process_send_eof(st, process)
        call prepend_in_progress(process)
        ! Now finally cd to where the shell started make/grep/...
        buffers(current)%directory = origdir
        if (scroll) call goto_character(point_maximum())
        ! Make it so the next C-x ` will use this buffer.
        next_error_last_buffer = outbuf
        call set_buffer(old, st)
    end subroutine compilation_start

    ! Not a port.
    subroutine set_process_sentinel_and_filter(process, filter)
        use process_mod, only: set_process_sentinel, set_process_filter
        type(handle_type), intent(in) :: process
        logical, intent(in) :: filter
        type(status_type) :: st

        call set_process_sentinel(process, st, compilation_sentinel)
        ! Keep the comint filter, since it's needed for proper handling of
        ! the prompts.
        if (filter) call set_process_filter(process, st, compilation_filter)
    end subroutine set_process_sentinel_and_filter

    ! Not a port.
    subroutine prepend_in_progress(process)
        type(handle_type), intent(in) :: process

        if (.not. allocated(compilation_in_progress)) allocate (compilation_in_progress(0))
        compilation_in_progress = [process, compilation_in_progress]
    end subroutine prepend_in_progress

    !> Whether compilations are in progress (`compilation_in_progress' is
    !> true), for the "[Compiling] " of the mode line.  Not a port.
    logical function is_compilation_in_progress()
        is_compilation_in_progress = .false.
        if (allocated(compilation_in_progress)) is_compilation_in_progress = size(compilation_in_progress) > 0
    end function is_compilation_in_progress

    !> Give entry K's buffer (the current buffer) the `mode_line_process'
    !> of its state: ":%s" or the exit status, then
    !> `compilation_mode_line_errors' (the counts by default).  Not a port.
    subroutine update_mode_line_process(k)
        integer, intent(in) :: k
        type(mode_line_part_type), allocatable :: parts(:), errors(:)
        integer :: i

        if (compilation_buffers(k)%mode_line == 0) return

        if (associated(compilation_buffers(k)%mode_line_errors)) then
            call compilation_buffers(k)%mode_line_errors(errors)
        else
            call compilation_mode_line_errors(k, errors)
        end if

        allocate (parts(size(errors) + 1))

        if (compilation_buffers(k)%mode_line == 1) then
            parts(1)%text = ":%s"
            parts(1)%face = "compilation_mode_line_run"
        else
            parts(1)%text = compilation_buffers(k)%exit_text
            parts(1)%face = compilation_buffers(k)%exit_face
        end if

        do i = 1, size(errors)
            parts(i + 1) = errors(i)
        end do

        call set_mode_line_process(parts)
    end subroutine update_mode_line_process

    !> The default `compilation_mode_line_errors' of entry K: the numbers
    !> of errors, warnings and informational messages so far.  Not a port.
    subroutine compilation_mode_line_errors(k, parts)
        integer, intent(in) :: k
        type(mode_line_part_type), allocatable, intent(out) :: parts(:)
        character(len=20) :: count_text

        allocate (parts(7))
        parts(1)%text = " ["
        parts(1)%face = ""
        write (count_text, "(i0)") compilation_buffers(k)%error_count
        parts(2)%text = trim(count_text)
        parts(2)%face = "compilation_error"
        parts(3)%text = " "
        parts(3)%face = ""
        write (count_text, "(i0)") compilation_buffers(k)%warning_count
        parts(4)%text = trim(count_text)
        parts(4)%face = "compilation_warning"
        parts(5)%text = " "
        parts(5)%face = ""
        write (count_text, "(i0)") compilation_buffers(k)%info_count
        parts(6)%text = trim(count_text)
        parts(6)%face = "compilation_info"
        parts(7)%text = "]"
        parts(7)%face = ""
    end subroutine compilation_mode_line_errors

    !> Show the current state of the current compilation buffer in its
    !> mode line again, after what `compilation_mode_line_errors' shows
    !> has changed (upstream the mode line evaluates it at each
    !> redisplay).  Not a port.
    subroutine compilation_update_mode_line()
        integer :: k

        k = entry_of(current_buffer())
        if (k /= 0) call update_mode_line_process(k)
    end subroutine compilation_update_mode_line

    !> The directory a leading "cd DIRECTORY;" of COMMAND changes to (DEFAULT if
    !> there is none): the cond of compile.el:compilation-start.  Not a port.
    subroutine command_cd_directory(command, default, directory)
        character(len=*), intent(in) :: command, default
        character(len=:), allocatable, intent(out) :: directory
        character(len=:), allocatable :: argument, out
        type(status_type) :: st
        logical :: matched
        integer :: k
        integer(int64) :: index

        index = string_match("\`\s *cd\(?:\s +\(\S +?\|'[^']*'\|""\(?:[^""`$\\]\|\\.\)*""\)\)?\s *[;&"//NEWLINE// &
                             "]", command, st)

        if (index == 0) then
            directory = default
            return
        end if

        argument = match_string(1, command, matched)

        if (.not. matched) then
            directory = "~"
        else if (argument(1:1) == "'") then
            directory = argument(2:len(argument) - 1)
        else if (argument(1:1) == """") then
            ! (replace-regexp-in-string "\\\\\\(.\\)" "\\1" ...)
            out = ""
            k = 2

            do while (k <= len(argument) - 1)
                if (argument(k:k) == "\" .and. k < len(argument) - 1) then
                    k = k + 1
                end if

                out = out//argument(k:k)
                k = k + 1
            end do

            directory = out
        else
            ! Globbing (file-expand-wildcards) is Deferred.
            directory = substitute_environment_variables(argument, .false.)
        end if
    end subroutine command_cd_directory

    !> Make DIRECTORY become the current buffer's default directory.
    !> If your environment includes a `CDPATH' variable, try each one of
    !> that list of directories (separated by occurrences of
    !> `path_separator') when resolving a relative directory name.  Port of
    !> files.el:cd and cd-absolute.
    subroutine cd(directory, status)
        character(len=*), intent(in) :: directory
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: cdpath, entry, candidate, absolute, base, cdir
        logical :: found
        integer :: b, e

        base = ""
        if (allocated(buffers(current)%directory)) base = buffers(current)%directory

        if (is_file_name_absolute(directory)) then
            call expand_file_name(directory, absolute, status, base)
            if (.not. status%ok()) return
        else
            ! cd-path: CDPATH's directories, or "./".
            call getenv("CDPATH", cdpath, found)
            if (.not. found) cdpath = "./"
            absolute = ""
            b = 1

            do while (b <= len(cdpath) + 1)
                e = index(cdpath(b:), ":")

                if (e == 0) then
                    e = len(cdpath) + 1
                else
                    e = b + e - 1
                end if

                entry = cdpath(b:e - 1)
                if (len(entry) == 0) entry = "./"
                call expand_file_name(entry, cdir, status, base)
                if (.not. status%ok()) cdir = base
                call expand_file_name(directory, candidate, status, file_name_as_directory(cdir))

                if (status%ok()) then
                    if (is_file_directory(candidate)) then
                        absolute = candidate
                        exit
                    end if
                end if

                b = e + 1
            end do

            if (len(absolute) == 0) then
                call set_error(status, GENERIC_ERROR_CODE, "No such directory found via CDPATH environment variable")
                return
            end if
        end if

        ! cd-absolute.
        absolute = abbreviate_file_name(file_name_as_directory(absolute))

        if (.not. is_file_directory(absolute)) then
            if (file_exists(absolute)) then
                call set_error(status, GENERIC_ERROR_CODE, absolute//" is not a directory")
            else
                call set_error(status, GENERIC_ERROR_CODE, absolute//": no such directory")
            end if

            return
        end if

        if (.not. is_file_accessible_directory(absolute)) then
            call set_error(status, GENERIC_ERROR_CODE, "Cannot cd to "//absolute//":  Permission denied")
            return
        end if

        buffers(current)%directory = absolute
    end subroutine cd

    ! ------------------------------------------------------------------
    ! Output and exit
    ! ------------------------------------------------------------------

    !> Process filter for compilation buffers.
    !> Just inserts the text, handles carriage motion (see
    !> `command_interpreter_inhibit_carriage_motion') and runs
    !> `compilation_filter_hook'.  Port of compile.el:compilation-filter
    !> (`compilation_maximum_output_line_length' and
    !> `compilation_hidden_output' are Deferred).
    recursive subroutine compilation_filter(process, string, status)
        type(handle_type), intent(in) :: process
        character(len=*), intent(in) :: string
        type(status_type), intent(out) :: status
        type(handle_type) :: buffer, position, min, max, mark
        type(value_type) :: inhibit
        type(status_type) :: st
        integer(int64) :: p, minimum_position, maximum_position

        status = status_type()
        buffer = process_buffer(process)
        if (.not. is_buffer_live(buffer)) return
        call set_buffer(buffer, status)
        if (.not. status%ok()) return
        call symbol_value(value_type(name="inhibit_read_only"), inhibit, st)
        call set(value_type(name="inhibit_read_only"), value_type(.true.), st)
        ! `save-excursion' doesn't use the right insertion-type for us.
        call copy_marker_at(point(), position, .true., st)
        call copy_marker_at(point_minimum(), min, .false., st)
        ! If we are inserting at the end of the accessible part of the
        ! buffer, keep the inserted text visible.
        call copy_marker_at(point_maximum(), max, .true., st)
        mark = process_mark(process)
        call marker_position(mark, compilation_filter_start, st)
        call widen()
        call goto_character(compilation_filter_start)
        call insert(string, status)

        if (status%ok()) then
            if (.not. command_interpreter_inhibit_carriage_motion%value) then
                call marker_position(mark, p, st)
                call command_interpreter_carriage_motion(p, point())
            end if

            call set_marker(mark, point(), st)
            ! Update the number of errors in compilation-mode-line-errors.
            call compilation_ensure_parse(point())
            call compilation_filter_hook%execute(status)
        end if

        call marker_position(position, p, st)
        call goto_character(p)
        call marker_position(min, minimum_position, st)
        call marker_position(max, maximum_position, st)
        call narrow_to_region(minimum_position, maximum_position, st)
        call destroy_marker(position, st)
        call destroy_marker(min, st)
        call destroy_marker(max, st)
        call set(value_type(name="inhibit_read_only"), inhibit, st)
    end subroutine compilation_filter

    !> Sentinel for compilation buffers.  Port of
    !> compile.el:compilation-sentinel.
    recursive subroutine compilation_sentinel(process, message, status)
        type(handle_type), intent(in) :: process
        character(len=*), intent(in) :: message
        type(status_type), intent(out) :: status
        type(handle_type) :: buffer, old
        type(status_type) :: st
        character(len=:), allocatable :: state
        integer :: k

        status = status_type()
        state = process_status(process)
        if (state /= "exit" .and. state /= "signal") return
        buffer = process_buffer(process)

        if (.not. is_buffer_live(buffer)) then
            ! Buffer killed.
            call set_process_buffer(process, null_handle(), st)
        else
            old = current_buffer()
            call set_buffer(buffer, st)
            ! Write something in the compilation buffer and hack its mode
            ! line.
            call compilation_handle_exit(state, process_exit_status(process), message)
            if (is_buffer_live(old)) call set_buffer(old, st)
        end if

        if (allocated(compilation_in_progress)) then
            compilation_in_progress = pack(compilation_in_progress, &
                                           [(.not. same(compilation_in_progress(k), process), &
                                             k=1, size(compilation_in_progress))])
        end if

        ! Since the buffer and mode line will show that the process is dead,
        ! we can delete it now.
        call delete_process(process, st)
    end subroutine compilation_sentinel

    !> Write MESSAGE in the current buffer and hack its `mode_line_process'.
    !> Port of compile.el:compilation-handle-exit.
    subroutine compilation_handle_exit(process_status_name, exit_status, message_text)
        character(len=*), intent(in) :: process_status_name, message_text
        integer, intent(in) :: exit_status
        type(value_type) :: inhibit
        type(status_type) :: st
        integer(int64) :: omax, original_point, now, rate
        integer :: k
        real :: elapsed
        character(len=32) :: duration
        character(len=16) :: code_text
        character(len=:), allocatable :: line, text, code

        k = entry_of(current_buffer())

        if (k /= 0) then
            if (associated(compilation_buffers(k)%exit_message)) then
                call compilation_buffers(k)%exit_message(process_status_name, exit_status, message_text, text, code)
            else
                text = message_text
                write (code_text, "(i0)") exit_status
                code = trim(code_text)
            end if
        else
            text = message_text
            write (code_text, "(i0)") exit_status
            code = trim(code_text)
        end if

        call symbol_value(value_type(name="inhibit_read_only"), inhibit, st)
        call set(value_type(name="inhibit_read_only"), value_type(.true.), st)
        omax = point_maximum()
        original_point = point()
        ! Record where we put the message, so we can ignore it later on.
        call goto_character(omax)
        call compilation_insert_annotation(NEWLINE//buffers(current)%mode_name//" "//text)
        if (is_beginning_of_line() .and. point() > point_minimum()) call goto_character(point() - 1)
        k = entry_of(current_buffer())
        call system_clock(now, rate)
        elapsed = 0
        if (k /= 0) elapsed = real(now - compilation_buffers(k)%start_count)/real(max(rate, 1_int64))

        if (elapsed < 10) then
            write (duration, "(f0.2, ' s')") elapsed
        else if (elapsed < 60) then
            write (duration, "(f0.1, ' s')") elapsed
        else
            write (duration, "(i0, ':', i2.2, ':', i2.2)") int(elapsed)/3600, mod(int(elapsed)/60, 60), mod(int(elapsed), 60)
        end if

        if (duration(1:1) == ".") duration = "0"//duration(1:len(duration) - 1)
        call compilation_insert_annotation(" at "//current_time_19()//", duration "//trim(duration))
        call goto_character(point_maximum())
        ! Prevent that message from being recognized as a compilation error.
        call put_text_property(omax, point(), value_type(name="compilation_handle_exit"), value_type(.true.), st)
        line = text

        if (len(line) > 0) then
            if (line(len(line):len(line)) == NEWLINE) line = line(1:len(line) - 1)
        end if

        k = entry_of(current_buffer())

        if (k /= 0) then
            compilation_buffers(k)%mode_line = 2
            compilation_buffers(k)%exit_text = ":"//process_status_name//" ["//code//"]"

            if (exit_status > 0) then
                compilation_buffers(k)%exit_face = "compilation_mode_line_fail"
            else
                compilation_buffers(k)%exit_face = "compilation_mode_line_exit"
            end if

            call update_mode_line_process(k)
        end if

        call message(buffers(current)%mode_name//" "//line)
        if (original_point < omax) call goto_character(original_point)
        call set(value_type(name="inhibit_read_only"), inhibit, st)
    end subroutine compilation_handle_exit

    !> Stop the process made by the \[compile] or \[grep] commands.  Port of
    !> compile.el:kill-compilation.
    recursive subroutine stop_compilation(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: buffer, process, old
        type(status_type) :: st

        call compilation_find_buffer(buffer, status)
        if (.not. status%ok()) return
        process = get_buffer_process(buffer)

        if (process%index /= 0) then
            call interrupt_process(status, process)
        else
            old = current_buffer()
            call set_buffer(buffer, st)
            call set_error(status, GENERIC_ERROR_CODE, "The "//downcase_name(buffers(current)%mode_name)// &
                           " process is not running")
            call set_buffer(old, st)
        end if
    end subroutine stop_compilation

    ! ------------------------------------------------------------------
    ! Definitions
    ! ------------------------------------------------------------------

    !> The faces of compile.el.  Not a port.
    subroutine define_faces()
        type(named_face_type), pointer :: f
        integer, parameter :: C = FACE_CLASS_COLOR

        f => compilation_error
        call f%define("compilation_error", "Face used to highlight compiler errors.")
        call f%specification_entry(display_when(), face_attributes(inherit="error"))
        f => compilation_warning
        call f%define("compilation_warning", "Face used to highlight compiler warnings.")
        call f%specification_entry(display_when(), face_attributes(inherit="warning"))
        f => compilation_info
        call f%define("compilation_info", "Face used to highlight compiler information.")
        call f%specification_entry(display_when(), face_attributes(inherit="success"))
        f => compilation_mode_line_fail
        call f%define("compilation_mode_line_fail", "Face for Compilation mode's ""error"" mode line indicator.")
        call f%specification_entry(display_when(is_default=.true.), face_attributes(inherit="compilation_error"))
        call f%specification_entry(display_when(classes=[C], minimum_colors=16), face_attributes(foreground="Red1", weight="bold"))
        call f%specification_entry(display_when(classes=[C], minimum_colors=8), face_attributes(foreground="red"))
        call f%specification_entry(display_when(), face_attributes(inverse_video=.true., weight="bold"))
        f => compilation_mode_line_run
        call f%define("compilation_mode_line_run", "Face for Compilation mode's ""running"" mode line indicator.")
        call f%specification_entry(display_when(), face_attributes(inherit="compilation_warning"))
        f => compilation_mode_line_exit
        call f%define("compilation_mode_line_exit", "Face for Compilation mode's ""exit"" mode line indicator.")
        call f%specification_entry(display_when(is_default=.true.), face_attributes(inherit="compilation_info"))
        call f%specification_entry(display_when(classes=[C], minimum_colors=16), face_attributes(foreground="ForestGreen", &
                                                                                                 weight="bold"))
        call f%specification_entry(display_when(classes=[C]), face_attributes(foreground="green", weight="bold"))
        call f%specification_entry(display_when(), face_attributes(weight="bold"))
        f => compilation_line_number
        call f%define("compilation_line_number", "Face for displaying line numbers in compiler messages.")
        call f%specification_entry(display_when(), face_attributes(inherit="font_lock_keyword_face"))
        f => compilation_column_number
        call f%define("compilation_column_number", "Face for displaying column numbers in compiler messages.")
        call f%specification_entry(display_when(), face_attributes(inherit="font_lock_doc_face"))
    end subroutine define_faces

    !> Define Compilation mode and the minor modes, and register the
    !> commands.  Not a port.
    subroutine initialize_compile()
        type(status_type) :: st

        call define_faces()
        call major_modes%compilation_mode%define("compilation_mode", "Compilation", compilation_mode, &
                                                 compilation_mode_hook)
        major_modes%compilation_mode%special = .true.
        compilation_mode_map = major_modes%compilation_mode%map
        call set_keymap_parent(compilation_mode_map, special_mode_map, st)
        call keymap_set(compilation_mode_map, "C-c C-c", "compile_goto_error", st)
        call keymap_set(compilation_mode_map, "RET", "compile_goto_error", st)
        call keymap_set(compilation_mode_map, "C-o", "compilation_display_error", st)
        call keymap_set(compilation_mode_map, "C-c C-k", "stop_compilation", st)
        call keymap_set(compilation_mode_map, "M-n", "compilation_next_error", st)
        call keymap_set(compilation_mode_map, "M-p", "compilation_previous_error", st)
        call keymap_set(compilation_mode_map, "M-{", "compilation_previous_file", st)
        call keymap_set(compilation_mode_map, "M-}", "compilation_next_file", st)
        call keymap_set(compilation_mode_map, "n", "next_error_no_select", st)
        call keymap_set(compilation_mode_map, "p", "previous_error_no_select", st)
        call keymap_set(compilation_mode_map, "TAB", "compilation_next_error", st)
        call keymap_set(compilation_mode_map, "<backtab>", "compilation_previous_error", st)
        call keymap_set(compilation_mode_map, "g", "recompile", st)
        call minor_modes%compilation_minor_mode%define("compilation_minor_mode", lighter=" Compilation", &
                                                       body=compilation_minor_mode_body, &
                                                       hook=compilation_minor_mode_hook, keymap=.true.)
        compilation_minor_mode_map = minor_modes%compilation_minor_mode%map
        call set_keymap_parent(compilation_minor_mode_map, special_mode_map, st)
        call keymap_set(compilation_minor_mode_map, "C-c C-c", "compile_goto_error", st)
        call keymap_set(compilation_minor_mode_map, "RET", "compile_goto_error", st)
        call keymap_set(compilation_minor_mode_map, "C-o", "compilation_display_error", st)
        call keymap_set(compilation_minor_mode_map, "C-c C-k", "stop_compilation", st)
        call keymap_set(compilation_minor_mode_map, "M-n", "compilation_next_error", st)
        call keymap_set(compilation_minor_mode_map, "M-p", "compilation_previous_error", st)
        call keymap_set(compilation_minor_mode_map, "M-{", "compilation_previous_file", st)
        call keymap_set(compilation_minor_mode_map, "M-}", "compilation_next_file", st)
        call keymap_set(compilation_minor_mode_map, "n", "next_error_no_select", st)
        call keymap_set(compilation_minor_mode_map, "p", "previous_error_no_select", st)
        call keymap_set(compilation_minor_mode_map, "g", "recompile", st)
        call minor_modes%compilation_shell_minor_mode%define("compilation_shell_minor_mode", lighter=" Shell-Compile", &
                                                             body=compilation_minor_mode_body, &
                                                             hook=compilation_shell_minor_mode_hook, keymap=.true.)
        compilation_shell_minor_mode_map = minor_modes%compilation_shell_minor_mode%map
        call keymap_set(compilation_shell_minor_mode_map, "C-M-m", "compile_goto_error", st)
        call keymap_set(compilation_shell_minor_mode_map, "C-M-n", "compilation_next_error", st)
        call keymap_set(compilation_shell_minor_mode_map, "C-M-p", "compilation_previous_error", st)
        call keymap_set(compilation_shell_minor_mode_map, "M-{", "compilation_previous_file", st)
        call keymap_set(compilation_shell_minor_mode_map, "M-}", "compilation_next_file", st)
        commands%compilation_mode%execute => compilation_mode
        commands%compile%execute => execute_compile
        commands%recompile%execute => execute_recompile
        commands%stop_compilation%execute => stop_compilation
        commands%compile_goto_error%execute => compile_goto_error
        commands%compilation_next_error%execute => execute_compilation_next_error
        commands%compilation_previous_error%execute => execute_compilation_previous_error
        commands%compilation_next_file%execute => execute_compilation_next_file
        commands%compilation_previous_file%execute => execute_compilation_previous_file
        commands%compilation_display_error%execute => compilation_display_error
        commands%compilation_set_skip_threshold%execute => execute_compilation_set_skip_threshold
        commands%compilation_next_error_function%execute => execute_compilation_next_error_function
        commands%compilation_minor_mode%execute => compilation_minor_mode
        commands%compilation_shell_minor_mode%execute => compilation_shell_minor_mode
        call append_locals_clearer(clear_compilation_locals)
        call append_mode_line_modes_entry("compilation_in_progress", "[Compiling] ", is_compilation_in_progress)
        call forward_setting(compilation_error_screen_columns)
        call forward_setting(compilation_first_column)
        if (.not. allocated(compile_command%value)) compile_command = default_compile_command()
        if (.not. allocated(compilation_error_regular_expressions)) call initialize_compilation_error_regular_expressions()

        if (.not. allocated(compilation_error_regular_expression_names)) then
            call initialize_compilation_error_regular_expression_names()
        end if

        if (.not. allocated(compilation_directory_matcher%regular_expression)) then
            compilation_directory_matcher%regular_expression = "\(?:Entering\|Leavin\(g\)\) directory [`']\(.+\)'$"
            compilation_directory_matcher%directory = [2]
            compilation_directory_matcher%leave = [1]
        end if

        if (.not. allocated(compilation_transform_file_matches)) then
            allocate (compilation_transform_file_matches(1))
            compilation_transform_file_matches(1)%regular_expression = "/bin/[a-z]*sh\'"
        end if

        if (.not. allocated(compilation_message_face%value)) compilation_message_face = "underline"
        if (.not. allocated(compilation_error_face%value)) compilation_error_face = "compilation_error"
        if (.not. allocated(compilation_warning_face%value)) compilation_warning_face = "compilation_warning"
        if (.not. allocated(compilation_info_face%value)) compilation_info_face = "compilation_info"
        if (.not. allocated(compilation_line_face%value)) compilation_line_face = "compilation_line_number"
        if (.not. allocated(compilation_column_face%value)) compilation_column_face = "compilation_column_number"

        if (.not. allocated(compilation_enter_directory_face%value)) then
            compilation_enter_directory_face = "font_lock_function_name_face"
        end if

        if (.not. allocated(compilation_leave_directory_face%value)) then
            compilation_leave_directory_face = "font_lock_builtin_face"
        end if
    end subroutine initialize_compile

    !> Forget the lines of `compilation_context', as any assignment to it
    !> does.  Not a port.
    subroutine compilation_context_clear_kinds(self)
        class(compilation_context_choice_setting_type), intent(inout) :: self

        self%lines = -1
    end subroutine compilation_context_clear_kinds

    !> The lines of `compilation_context' as help shows it (`%lines = N');
    !> "" when none is set.  Not a port.
    function compilation_context_other_value(self) result(text)
        class(compilation_context_choice_setting_type), intent(in) :: self
        character(len=:), allocatable :: text
        character(len=40) :: digits

        text = ""

        if (self%lines >= 0) then
            write (digits, "(i0)") self%lines
            text = "%lines = "//trim(digits)
        end if
    end function compilation_context_other_value

    !> The assignment that sets the number or procedure `compilation_context' takes, for the error of a rejected
    !> value.  Not a port.
    function compilation_context_kind_hint(self, number) result(text)
        class(compilation_context_choice_setting_type), intent(in) :: self
        character(len=*), intent(in) :: number
        character(len=:), allocatable :: text

        text = "a number of lines is `"//trim(self%name)//"%lines = "//number//"'"
    end function compilation_context_kind_hint
end module compile_mod
