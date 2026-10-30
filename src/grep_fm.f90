! grep_fm.f90 -- run `grep' and display the results.
! Ported from GNU Emacs 31.1 lisp/progmodes/grep.el, with
! lisp/find-dired.el find-name-arg.
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

!> Grep: `grep' runs a grep command with its output in *grep*, in Grep
!> mode (a Compilation mode, see compile_fm), so that `next_error' (M-g
!> n, C-x `) and RET visit the matches.  `grep_find' (`find_grep') runs a
!> find command, `lgrep' builds the command from `grep_template' for the
!> files of a directory, `rgrep' from `grep_find_template' for a
!> directory tree, `zrgrep' (`rzgrep') for gzipped files.
!>
!> The defaults are computed by probing the grep, find and xargs
!> programs (`grep_compute_defaults'); matches are highlighted with the
!> color escape sequences grep emits (`grep_highlight_matches'), which
!> `grep_filter' turns into the `match' face; `grep_use_headings'
!> groups the matches under a heading per file.
!>
!> Grep-Edit mode (e) applies edits of the matches to the files, as
!> Occur-Edit mode does (replace_fm).
!>
!> Not ported yet (Deferred): C-c C-f
!> (`next_error_follow_minor_mode'), the menu and tool bar, the `keymap'
!> of the abbreviated part of a command (RET and mouse-2 on it; M-x
!> `grep_find_toggle_abbreviation' works), `kill_transform_function'
!> (the NUL of `--null' is copied as is), the outline functions of
!> `grep_use_headings', `grep_window_height' and `grep_search_path'
!> (compile_fm defers `compilation_window_height' and
!> `compilation_search_path'), `find_tag_default_function', the
!> functions of `file_name_at_point_functions', the case-fold toggle of
!> `read_regexp' (`read_regexp_case_fold_search' is `case_fold_search'),
!> remote directories (`file_remote_p': the defaults of the local host
!> only, `grep_quoting_style' stays false).
module grep_fm
    use, intrinsic :: iso_fortran_env, only: int64
    use commands_fm, only: commands
    use error_fm, only: status_type, set_error, FM_GENERIC_ERROR, FM_USER_ERROR
    use handle_fm, only: handle_type, null_handle
    use lisp_fm, only: value_type, make_string, make_list, list, nil, t_value, nilp, consp, stringp, xstring, fixnump, &
                       xfixnum, list_elements, make_fixnum
    use lread_fm, only: sym
    use data_fm, only: symbol_value, add_local_variables_killer
    use eval_fm, only: hook_type
    use buffer_fm, only: buffers, cur, current_buffer, set_buffer, get_buffer_create, generate_new_buffer_name, &
                         kill_buffer, buffer_modified_p, buffer_live_p, transient_mark_mode
    use marker_fm, only: copy_marker_at, free_marker, marker_position, set_marker
    use insdel_fm, only: insert, insert_before_markers
    use editfns_fm, only: point, point_min, point_max, goto_char, line_beginning_position, line_end_position, &
                          buffer_substring, mark_position, char_after
    use cmds_fm, only: forward_line
    use textprop_fm, only: put_text_property, get_text_property, add_text_properties, text_property_any, &
                           next_single_property_change, remove_list_of_text_properties
    use search_fm, only: re_search_forward, match_beginning, match_end, match_data, set_match_data, replace_match, &
                         string_match, looking_at, case_fold_search, string_index_to_offset, SEARCH_NOERROR, SEARCH_MOVE
    use subr_fm, only: match_string, shell_quote_argument, add_to_history, defvar_nil, find_tag_default, &
                       silent_modifications_type, silent_modifications_begin, silent_modifications_end
    use fileio_fm, only: expand_file_name, file_name_as_directory, file_name_nondirectory, file_directory_p, &
                         file_accessible_directory_p
    use files_fm, only: save_some_buffers, file_name_extension, wildcard_to_regexp, make_temp_file, delete_file
    use callproc_fm, only: call_process
    use simple_fm, only: read_shell_command, next_error_last_buffer, shell_command_switch
    use callproc_fm, only: shell_file_name, init_callproc
    use minibuf_fm, only: read_from_minibuffer, read_string, minibuffer_string_type
    use minibuf_table_fm, only: completion_table_type, completion_function_table, completion_list_table, &
                                completion_predicate, COMPLETE_TRY, COMPLETE_ALL, COMPLETE_TEST
    use minibuf_fm, only: try_completion, all_completions, test_completion
    use minibuffer_fm, only: completing_read, read_directory_name, completion_file_name_table, &
                             read_file_name_completion_ignore_case
    use replace_fm, only: read_regexp, format_prompt
    use isearch_fm, only: isearch_no_upper_case_p
    use callint_fm, only: current_prefix_arg, prefix_numeric_value
    use keymap_fm, only: keymap_set, set_keymap_parent, make_sparse_keymap, current_local_map, use_local_map
    use text_mode_fm, only: text_mode_map
    use derived_fm, only: derived_mode_p
    use subr_fm, only: run_mode_hooks
    use insdel_fm, only: after_change_functions
    use data_fm, only: set
    use help_fm, only: substitute_command_keys
    use buffer_fm, only: restore_buffer_modified_p
    use textprop_fm, only: text_property_not_all
    use replace_fm, only: occur_add_target, occur_after_change_function
    use modes_fm, only: major_modes
    use process_fm, only: process_arg_type, get_buffer_process
    use font_core_fm, only: font_lock_keyword_type, font_lock_highlight_type, font_lock_mode_enabled, &
                            FONT_LOCK_OVERRIDE_T
    use font_lock_fm, only: font_lock_highlight
    use faces_fm, only: named_face_type, display_when, face_attrs
    use vc_hooks_fm, only: vc_directory_exclusion_list
    use dired_fm, only: completion_ignored_extensions
    use xdisp_fm, only: message, mode_line_part_type
    use compile_fm, only: compilation_start, compilation_set_mode_locals, compilation_update_mode_line, &
                          compilation_rule_type, compilation_highlight_type, compilation_directory_matcher_type, &
                          compilation_transform_type, compilation_transform_file_match_alist, &
                          compilation_error_face, compilation_scroll_output, compilation_first_column, &
                          compilation_error_screen_columns, compilation_disable_input, compilation_filter_hook, &
                          compilation_filter_start, compilation_info_face, compilation_warning_face, &
                          compilation_minor_mode_map, compilation_message_info, compilation_message_info_type, &
                          kill_compilation, syms_of_compile, compilation_message_location, COMPILATION_SCROLL_NIL, &
                          COMPILATION_SCROLL_T
    implicit none(type, external)
    private

    public :: grep_edit_mode, grep_change_to_grep_edit_mode, grep_edit_save_changes
    public :: grep, grep_find, lgrep, rgrep, zrgrep, grep_mode, grep_compute_defaults, grep_default_command, &
              grep_expand_template, rgrep_default_command, grep_find_toggle_abbreviation, grep_file_at_point, &
              grep_process_setup, grep_exit_message, grep_filter, grep_find_ignored_files, &
              rgrep_find_ignored_directories, grep_read_files, grep_apply_setting, grep_tag_default, syms_of_grep

    !> The values of the tri-state options (`grep_use_null_device',
    !> `grep_use_null_filename_separator', `grep_highlight_matches'):
    !> false (nil), true (t), not yet probed (`auto-detect'), and for
    !> `grep_highlight_matches' `--color=always' (`always') and
    !> `--color=auto' (`auto').
    integer, parameter, public :: GREP_NIL = 0, GREP_T = 1, GREP_AUTO_DETECT = 2, GREP_ALWAYS = 3, GREP_AUTO = 4

    !> The values of `grep_find_use_xargs': not yet probed (nil), `find
    !> -exec {} ;' (`exec'), `find -exec {} +' (`exec-plus'), `find
    !> -print0 | xargs -0' (`gnu'), `find -print0 | sort -z | xargs -0'
    !> (`gnu-sort'), and `find -print | xargs' (any other value).
    integer, parameter, public :: GREP_FIND_USE_XARGS_NIL = 0, GREP_FIND_USE_XARGS_EXEC = 1, &
                                  GREP_FIND_USE_XARGS_EXEC_PLUS = 2, GREP_FIND_USE_XARGS_GNU = 3, &
                                  GREP_FIND_USE_XARGS_GNU_SORT = 4, GREP_FIND_USE_XARGS_OTHER = 5

    !> The values of `grep_save_buffers': don't save (nil), save all
    !> buffers (t), ask before saving (`ask').  A function given as
    !> `grep_save_buffers_function' takes precedence.
    integer, parameter, public :: GREP_SAVE_BUFFERS_NIL = 0, GREP_SAVE_BUFFERS_T = 1, GREP_SAVE_BUFFERS_ASK = 2

    !> An element of `grep_files_aliases': the alias NAME for the file
    !> name patterns FILES.
    type, public :: grep_files_alias_type
        character(len=16) :: name = ""
        character(len=128) :: files = ""
    end type grep_files_alias_type

    abstract interface
        !> The car of a cons element of `grep_find_ignored_files_variable' and
        !> `grep_find_ignored_directories': whether the element applies to
        !> the search directory DIR.  Not a port.
        logical function grep_ignore_predicate(dir)
            implicit none(type, external)
            character(len=*), intent(in) :: dir
        end function grep_ignore_predicate

        !> `grep_read_files_function': the file name whose extension gives
        !> the default for `grep_read_files' (FOUND false for none).
        !> Not a port.
        subroutine grep_read_files_proc(name, found)
            implicit none(type, external)
            character(len=:), allocatable, intent(out) :: name
            logical, intent(out) :: found
        end subroutine grep_read_files_proc

        !> A function value of `grep_save_buffers': called with each buffer
        !> current, whether that buffer should be saved.  Not a port.
        logical function grep_save_buffers_proc()
            implicit none(type, external)
        end function grep_save_buffers_proc
    end interface

    public :: grep_ignore_predicate, grep_read_files_proc, grep_save_buffers_proc

    !> An element of `grep_find_ignored_files_variable' and
    !> `grep_find_ignored_directories': the file or directory NAME, which
    !> applies to a search directory only if APPLIES (when given) says so
    !> (the cons form (FUNCTION . NAME)).
    type, public :: grep_ignore_type
        character(len=64) :: name = ""
        procedure(grep_ignore_predicate), pointer, nopass :: applies => null()
    end type grep_ignore_type

    !> Use special markers to highlight grep matches.
    !>
    !> Some grep programs are able to surround matches with special
    !> markers in grep output.  Such markers can be used to highlight
    !> matches in grep mode.  This requires `font_lock_mode' to be active
    !> in grep buffers, so if you have globally disabled `font_lock_mode',
    !> you will not get highlighting.
    !>
    !> This option sets the environment variable GREP_COLORS to specify
    !> markers for highlighting and adds the --color option in front of
    !> any explicit grep options before starting the grep.
    !>
    !> When this option is GREP_AUTO, grep uses `--color=auto' to
    !> highlight matches only when it outputs to a terminal (when `grep'
    !> is the last command in the pipe), thus avoiding the use of any
    !> potentially-harmful escape sequences when standard output goes to a
    !> file or pipe.
    !>
    !> To make grep highlight matches even into a pipe, you need the
    !> option GREP_ALWAYS that forces grep to use `--color=always' to
    !> unconditionally output escape sequences.
    !>
    !> If the value is GREP_AUTO_DETECT (the default), `grep' will call
    !> `grep_compute_defaults' to compute the value.  To change the
    !> default value, use \[customize] or call the function
    !> `grep_apply_setting'.  Port of grep.el grep-highlight-matches.
    integer, public :: grep_highlight_matches = GREP_AUTO_DETECT

    !> Regular expression matching grep markers to highlight.
    !> It matches SGR ANSI escape sequences which are emitted by grep to
    !> color its output.  This variable is used in `grep_filter'.  Port of
    !> grep.el grep-match-regexp.
    character(len=:), allocatable, public :: grep_match_regexp

    !> True to scroll the *grep* buffer window as output appears.
    !>
    !> Setting it causes the grep commands to put point at the end of their
    !> output window so that the end of the output is always visible rather
    !> than the beginning.  Port of grep.el grep-scroll-output.
    logical, public :: grep_scroll_output = .false.

    !> The default grep command for \[grep].
    !> If the grep program used supports an option to always include file
    !> names in its output (such as the `-H' option to GNU grep), it's a
    !> good idea to include it when specifying `grep_command'.
    !>
    !> In interactive usage, the actual value of this variable is set up
    !> by `grep_compute_defaults'; to change the default value, use
    !> \[customize] or call the function `grep_apply_setting'.
    !>
    !> Also see `grep_command_position'.  Port of grep.el grep-command.
    character(len=:), allocatable, public :: grep_command

    !> Where to put point when prompting for a grep command.
    !> This controls the placement of point in the minibuffer when Fortmacs
    !> prompts for the grep command.  If 0, put point at the end of the
    !> suggested command.  Otherwise, this should be the one-based
    !> position in the minibuffer where to place point.  Port of grep.el
    !> grep-command-position.
    integer, public :: grep_command_position = 0

    !> The default command to run for \[lgrep].
    !> The following place holders should be present in the string:
    !>  <C> - place to put the options like -i and --color.
    !>  <F> - file names and wildcards to search.
    !>  <X> - file names and wildcards to exclude.
    !>  <R> - the regular expression searched for.
    !>  <N> - place to insert `null_device'.
    !>
    !> In interactive usage, the actual value of this variable is set up
    !> by `grep_compute_defaults'; to change the default value, use
    !> \[customize] or call the function `grep_apply_setting'.  Port of
    !> grep.el grep-template.
    character(len=:), allocatable, public :: grep_template

    !> If GREP_T, append the value of `null_device' to `grep' commands.
    !> This is done to ensure that the output of grep includes the filename
    !> of any match in the case where only a single file is searched, and
    !> is not necessary if the grep program used supports the `-H' option.
    !>
    !> In interactive usage, the actual value of this variable is set up
    !> by `grep_compute_defaults'; to change the default value, use
    !> \[customize] or call the function `grep_apply_setting'.  Port of
    !> grep.el grep-use-null-device.
    integer, public :: grep_use_null_device = GREP_AUTO_DETECT

    !> If true (GREP_T), use `grep''s `--null' option.
    !> This is done to disambiguate file names in `grep''s output.  Port of
    !> grep.el grep-use-null-filename-separator.
    integer, public :: grep_use_null_filename_separator = GREP_AUTO_DETECT

    !> The default find command for \[grep_find].
    !> In interactive usage, the actual value of this variable is set up
    !> by `grep_compute_defaults'; to change the default value, use
    !> \[customize] or call the function `grep_apply_setting'.
    !>
    !> If `grep_find_command_position' is not 0, point will be placed at
    !> that position for easier editing (the form (COMMAND . POSITION)).
    !> Port of grep.el grep-find-command.
    character(len=:), allocatable, public :: grep_find_command

    !> The POSITION of `grep_find_command' when it has the form (COMMAND
    !> . POSITION), 0 for none.
    integer, public :: grep_find_command_position = 0

    !> The default command to run for \[rgrep].
    !> The following place holders should be present in the string:
    !>  <D> - base directory for find
    !>  <X> - find options to restrict or expand the directory list
    !>  <F> - find options to limit the files matched
    !>  <C> - place to put the grep options like -i and --color
    !>  <R> - the regular expression searched for.
    !> In interactive usage, the actual value of this variable is set up
    !> by `grep_compute_defaults'; to change the default value, use
    !> \[customize] or call the function `grep_apply_setting'.  Port of
    !> grep.el grep-find-template.
    character(len=:), allocatable, public :: grep_find_template

    !> If false, when `rgrep' expands `grep_find_template', file ignores
    !> go in <X>.
    !>
    !> By default, the <X> placeholder contains find options for affecting
    !> the directory list, and the <F> placeholder contains the find
    !> options which affect which files are matched, both
    !> `grep_find_ignored_files_variable' and the FILES argument to `rgrep'.
    !>
    !> This separation allows the two sources of file matching in <F> to be
    !> optimized together into a set of options which are overall faster
    !> for "find" to evaluate.
    !>
    !> If false, <X> contains ignores both for directories and files, and
    !> <F> contains only the FILES argument.  This is the old behavior.
    !> Port of grep.el rgrep-find-ignores-in-<f>.
    logical, public :: rgrep_find_ignores_in_f = .true.

    !> Whether to use POSIX-like shell argument quoting.  Port of grep.el
    !> grep-quoting-style.
    logical, public :: grep_quoting_style = .false.

    !> Alist of aliases for the FILES argument to `lgrep' and `rgrep'.
    !> Port of grep.el grep-files-aliases.
    type(grep_files_alias_type), allocatable, public :: grep_files_aliases(:)

    !> List of names of sub-directories which `rgrep' shall not recurse
    !> into.  If an element has a predicate (APPLIES), it is called on the
    !> search directory to determine whether NAME should not be recursed
    !> into.
    !>
    !> The default value is inherited from `vc_directory_exclusion_list'.
    !> Port of grep.el grep-find-ignored-directories.
    type(grep_ignore_type), allocatable, public :: grep_find_ignored_directories(:)

    !> List of file names which `rgrep' and `lgrep' shall exclude.
    !> If an element has a predicate (APPLIES), it is called on the search
    !> directory to determine whether NAME should not be excluded.  Port of
    !> grep.el grep-find-ignored-files (renamed: the function
    !> `grep_find_ignored_files' has its name).
    type(grep_ignore_type), allocatable, public :: grep_find_ignored_files_variable(:)

    !> If not GREP_SAVE_BUFFERS_NIL, save buffers before running the grep
    !> commands.  If GREP_SAVE_BUFFERS_ASK, ask before saving.  If
    !> `grep_save_buffers_function' is set, call it with no arguments with
    !> each buffer current, as a predicate to determine whether that buffer
    !> should be saved or not.  Port of grep.el grep-save-buffers.
    integer, public :: grep_save_buffers = GREP_SAVE_BUFFERS_ASK

    !> The function value of `grep_save_buffers' (null for none).
    procedure(grep_save_buffers_proc), pointer, public :: grep_save_buffers_function => null()

    !> If true, column numbers in grep hits are screen columns.
    !> See `compilation_error_screen_columns'.  Port of grep.el
    !> grep-error-screen-columns.
    logical, public :: grep_error_screen_columns = .false.

    !> List of hook functions run by `grep_process_setup' (see
    !> `run_hooks').  Port of grep.el grep-setup-hook.
    type(hook_type), target, public :: grep_setup_hook = hook_type(name="grep_setup_hook")

    !> Hook run after entering Grep mode.  Port of grep.el grep-mode-hook.
    type(hook_type), target, public :: grep_mode_hook = hook_type(name="grep_mode_hook")

    !> Keymap for grep buffers.
    !> This keymap inherits from `compilation_minor_mode_map'.  Port of
    !> grep.el grep-mode-map.
    type(handle_type), public :: grep_mode_map

    !> The most recent grep buffer.
    !> A grep buffer becomes most recent when you select Grep mode in it.
    !> Notice that using \[next_error] or \[compile_goto_error] modifies
    !> `next_error_last_buffer' rather than `grep_last_buffer'.  Port of
    !> grep.el grep-last-buffer.
    type(handle_type), public :: grep_last_buffer

    !> Face name to use for grep matches.  Port of grep.el grep-match-face.
    character(len=:), allocatable, public :: grep_match_face

    !> Regexp used to match grep hits.
    !> See `compilation_error_regexp_alist' for format details.  Port of
    !> grep.el grep-regexp-alist.
    type(compilation_rule_type), allocatable, public :: grep_regexp_alist(:)

    !> Value to use for `compilation_first_column' in grep buffers.  Port
    !> of grep.el grep-first-column.
    integer, public :: grep_first_column = 0

    !> Message to print when no matches are found.  Port of grep.el
    !> grep-error.
    character(len=:), allocatable, public :: grep_error

    !> Face name to use for grep hits.  Port of grep.el grep-hit-face.
    character(len=:), allocatable, public :: grep_hit_face

    !> Face name to use for grep error messages.  Port of grep.el
    !> grep-error-face.
    character(len=:), allocatable, public :: grep_error_face

    !> Face name to use for grep context lines.  Port of grep.el
    !> grep-context-face.
    character(len=:), allocatable, public :: grep_context_face

    !> If true, hide part of rgrep/lgrep/zrgrep command line.
    !> The hidden part contains a list of ignored directories and files.
    !> Clicking on the button-like ellipsis unhides the abbreviated part
    !> and reveals the entire command line.  The visibility of the
    !> abbreviated part can also be toggled with
    !> `grep_find_toggle_abbreviation'.  Port of grep.el
    !> grep-find-abbreviate.
    logical, public :: grep_find_abbreviate = .true.

    !> If true, subdivide grep output into sections, one per file.  Port of
    !> grep.el grep-use-headings.
    logical, public :: grep_use_headings = .false.

    !> Face of headings when `grep_use_headings' is true.  Port of grep.el
    !> grep-heading.
    type(named_face_type), target, public :: grep_heading

    !> Regexp used to create headings from grep output lines.
    !> It should be anchored at beginning of line.  The first capture
    !> group, if present, should match the heading associated to the
    !> line.  The buffer range of the second capture, if present, is
    !> made invisible (presumably because displaying it would be
    !> redundant).  Port of grep.el grep-heading-regexp.
    character(len=:), allocatable, public :: grep_heading_regexp

    !> Additional things to highlight in grep output.
    !> This gets tacked on the end of the generated expressions.  Port of
    !> grep.el grep-mode-font-lock-keywords.
    type(font_lock_keyword_type), allocatable, public :: grep_mode_font_lock_keywords(:)

    !> Rules added to `compilation_transform_file_match_alist' in
    !> `grep_mode'.  These prevent the "Grep finished" lines from being
    !> misinterpreted as matches (bug#77732).  Port of grep.el
    !> grep-compilation-transform-finished-rules.
    type(compilation_transform_type), allocatable, public :: grep_compilation_transform_finished_rules(:)

    !> The default grep program for `grep_command' and
    !> `grep_find_command'.  This variable's value takes effect when
    !> `grep_compute_defaults' is called.  Port of grep.el grep-program.
    character(len=:), allocatable, public :: grep_program

    !> The default find program.
    !> This is used by commands like `grep_find_command', `find_dired' and
    !> others.  Port of grep.el find-program.
    character(len=:), allocatable, public :: find_program

    !> The default xargs program for `grep_find_command'.
    !> See `grep_find_use_xargs'.
    !> This variable's value takes effect when `grep_compute_defaults' is
    !> called.  Port of grep.el xargs-program.
    character(len=:), allocatable, public :: xargs_program

    !> How to invoke find and grep.
    !> If GREP_FIND_USE_XARGS_EXEC, use `find -exec {} ;'.
    !> If GREP_FIND_USE_XARGS_EXEC_PLUS use `find -exec {} +'.
    !> If GREP_FIND_USE_XARGS_GNU, use `find -print0' and `xargs -0'.
    !> If GREP_FIND_USE_XARGS_GNU_SORT, use `find -print0', `sort -z' and
    !> `xargs -0'.  GREP_FIND_USE_XARGS_OTHER means to use `find -print'
    !> and `xargs'.
    !>
    !> This variable's value takes effect when `grep_compute_defaults' is
    !> called.  Port of grep.el grep-find-use-xargs.
    integer, public :: grep_find_use_xargs = GREP_FIND_USE_XARGS_NIL

    !> Keymap for `grep_edit_mode'.  Port of grep.el grep-edit-mode-map.
    type(handle_type), public :: grep_edit_mode_map

    !> Hooks run when changing to Grep-Edit mode.  Port of grep.el
    !> grep-edit-mode-hook.
    type(hook_type), target, public :: grep_edit_mode_hook = hook_type(name="grep_edit_mode_hook")

    !> The function giving the default file name of `grep_read_files'.
    procedure(grep_read_files_proc), pointer, public :: grep_read_files_function => null()

    !> Whether `lgrep' passes "--directories=skip" (GREP_T), not (GREP_NIL)
    !> or does not know yet (GREP_AUTO_DETECT).
    integer :: grep_use_directories_skip = GREP_AUTO_DETECT

    !> Option to `find' to match the base name of files.  Port of
    !> find-dired.el find-name-arg (it moves to find-dired's module when
    !> that is ported).
    character(len=:), allocatable, public :: find_name_arg

    ! The values grep-compute-defaults computes (an entry of
    ! grep-host-defaults-alist).
    type :: grep_defaults_type
        character(len=:), allocatable :: command, template, find_command, find_template
        integer :: command_position = 0, find_command_position = 0
        integer :: use_null_device = GREP_AUTO_DETECT, use_null_filename_separator = GREP_AUTO_DETECT
        integer :: find_use_xargs = GREP_FIND_USE_XARGS_NIL, highlight_matches = GREP_AUTO_DETECT
        logical :: quoting_style = .false.
    end type grep_defaults_type

    ! grep-host-defaults-alist: the values given by the user (the entry
    ! nil) and those computed for the local host (localhost).
    type :: grep_host_defaults_type
        logical :: has_defaults = .false., has_localhost = .false.
        type(grep_defaults_type) :: defaults, localhost
    end type grep_host_defaults_type

    type(grep_host_defaults_type) :: grep_host_defaults_alist

    ! The buffer-local variables of grep buffers: grep-num-matches-found
    ! and grep--heading-state (LIMIT, a marker, and the last HEADING).
    type :: grep_local_type
        type(handle_type) :: buffer
        integer(int64) :: num_matches_found = 0
        logical :: has_heading_state = .false.
        type(handle_type) :: heading_limit
        character(len=:), allocatable :: heading
        ! grep-edit-original-mode-map.
        type(handle_type) :: edit_original_map
    end type grep_local_type

    type(grep_local_type), allocatable :: locals(:)

    ! The defaults of grep-read-files' completion: the table
    ! completion-table-merge makes of them and completion-file-name-table.
    type(minibuffer_string_type), allocatable :: read_files_defaults(:)

    character(len=*), parameter :: NL = achar(10), TB = achar(9), NUL = achar(0), ESC = achar(27)
    character(len=*), parameter :: NULL_DEVICE = "/dev/null"

contains

    ! ------------------------------------------------------------------
    ! Buffer-local state
    ! ------------------------------------------------------------------

    !> The entry of the current buffer in `locals', made if needed.
    !> Not a port.
    integer function local_entry() result(k)
        type(grep_local_type), allocatable :: grown(:)
        type(handle_type) :: h
        integer :: n

        h = current_buffer()
        if (.not. allocated(locals)) allocate (locals(0))

        do k = 1, size(locals)
            if (locals(k)%buffer%index == h%index .and. locals(k)%buffer%generation == h%generation) return
        end do

        do k = 1, size(locals)
            if (.not. buffer_live_p(locals(k)%buffer)) then
                call reset_local(k)
                locals(k)%buffer = h
                return
            end if
        end do

        n = size(locals)
        allocate (grown(n + 1))

        do k = 1, n
            grown(k) = locals(k)
        end do

        grown(n + 1)%buffer = h
        call move_alloc(grown, locals)
        k = n + 1
    end function local_entry

    !> Free what entry K holds and make it unused.  Not a port.
    subroutine reset_local(k)
        integer, intent(in) :: k
        type(grep_local_type) :: empty
        type(status_type) :: st

        if (locals(k)%heading_limit%index /= 0) call free_marker(locals(k)%heading_limit, st)
        locals(k) = empty
    end subroutine reset_local

    !> Forget the current buffer's grep variables, when
    !> `kill_all_local_variables' runs.  Not a port.
    subroutine kill_grep_locals()
        type(handle_type) :: h
        integer :: k

        if (.not. allocated(locals)) return
        h = current_buffer()

        do k = 1, size(locals)
            if (locals(k)%buffer%index == h%index .and. locals(k)%buffer%generation == h%generation) then
                call reset_local(k)
                return
            end if
        end do
    end subroutine kill_grep_locals

    ! ------------------------------------------------------------------
    ! Settings
    ! ------------------------------------------------------------------

    !> Set the given settings, and update `grep_host_defaults_alist'.
    !> The settings are `grep_command', `grep_template',
    !> `grep_use_null_device', `grep_find_command', `grep_find_template',
    !> `grep_find_use_xargs', `grep_use_null_filename_separator',
    !> `grep_highlight_matches', or `grep_quoting_style'.  Port of
    !> grep.el:grep-apply-setting (one call can set several).
    subroutine grep_apply_setting(command, template, use_null_device, find_command, find_command_position, &
                                  find_template, find_use_xargs, use_null_filename_separator, highlight_matches, &
                                  quoting_style)
        character(len=*), intent(in), optional :: command, template, find_command, find_template
        integer, intent(in), optional :: use_null_device, find_command_position, find_use_xargs, &
                                         use_null_filename_separator, highlight_matches
        logical, intent(in), optional :: quoting_style

        if (grep_host_defaults_alist%has_defaults) then
            call apply(grep_host_defaults_alist%defaults)
            if (grep_host_defaults_alist%has_localhost) call apply(grep_host_defaults_alist%localhost)
        end if

        if (present(command)) grep_command = command
        if (present(template)) grep_template = template
        if (present(use_null_device)) grep_use_null_device = use_null_device

        if (present(find_command)) then
            grep_find_command = find_command
            grep_find_command_position = 0
        end if

        if (present(find_command_position)) grep_find_command_position = find_command_position
        if (present(find_template)) grep_find_template = find_template
        if (present(find_use_xargs)) grep_find_use_xargs = find_use_xargs
        if (present(use_null_filename_separator)) grep_use_null_filename_separator = use_null_filename_separator
        if (present(highlight_matches)) grep_highlight_matches = highlight_matches
        if (present(quoting_style)) grep_quoting_style = quoting_style

    contains

        subroutine apply(d)
            type(grep_defaults_type), intent(inout) :: d

            if (present(command)) d%command = command
            if (present(template)) d%template = template
            if (present(use_null_device)) d%use_null_device = use_null_device

            if (present(find_command)) then
                d%find_command = find_command
                d%find_command_position = 0
            end if

            if (present(find_command_position)) d%find_command_position = find_command_position
            if (present(find_template)) d%find_template = find_template
            if (present(find_use_xargs)) d%find_use_xargs = find_use_xargs
            if (present(use_null_filename_separator)) d%use_null_filename_separator = use_null_filename_separator
            if (present(highlight_matches)) d%highlight_matches = highlight_matches
            if (present(quoting_style)) d%quoting_style = quoting_style
        end subroutine apply
    end subroutine grep_apply_setting

    !> The current values of the settings `grep_compute_defaults' computes.
    !> Not a port.
    function current_defaults() result(d)
        type(grep_defaults_type) :: d

        if (allocated(grep_command)) d%command = grep_command
        d%command_position = grep_command_position
        if (allocated(grep_template)) d%template = grep_template
        d%use_null_device = grep_use_null_device
        if (allocated(grep_find_command)) d%find_command = grep_find_command
        d%find_command_position = grep_find_command_position
        if (allocated(grep_find_template)) d%find_template = grep_find_template
        d%use_null_filename_separator = grep_use_null_filename_separator
        d%find_use_xargs = grep_find_use_xargs
        d%highlight_matches = grep_highlight_matches
        d%quoting_style = grep_quoting_style
    end function current_defaults

    !> Set the settings `grep_compute_defaults' computes to D's values.
    !> Not a port.
    subroutine set_defaults(d)
        type(grep_defaults_type), intent(in) :: d

        if (allocated(d%command)) then
            grep_command = d%command
        else if (allocated(grep_command)) then
            deallocate (grep_command)
        end if

        if (allocated(d%template)) then
            grep_template = d%template
        else if (allocated(grep_template)) then
            deallocate (grep_template)
        end if

        grep_use_null_device = d%use_null_device

        if (allocated(d%find_command)) then
            grep_find_command = d%find_command
        else if (allocated(grep_find_command)) then
            deallocate (grep_find_command)
        end if

        grep_find_command_position = d%find_command_position
        grep_use_null_filename_separator = d%use_null_filename_separator

        if (allocated(d%find_template)) then
            grep_find_template = d%find_template
        else if (allocated(grep_find_template)) then
            deallocate (grep_find_template)
        end if

        grep_find_use_xargs = d%find_use_xargs
        grep_highlight_matches = d%highlight_matches
    end subroutine set_defaults

    ! ------------------------------------------------------------------
    ! Grep mode
    ! ------------------------------------------------------------------

    !> The column (relative to the end of the match) of the first grep
    !> match on a line, from the faces `grep_filter' put there: COL of the
    !> `grep_regexp_alist' rule.  Not a port.
    subroutine first_match_column(number, found)
        integer(int64), intent(out) :: number
        logical, intent(out) :: found
        integer(int64) :: beg, end, mbeg
        type(status_type) :: st

        number = 0
        found = .false.
        if (grep_highlight_matches == GREP_NIL) return
        beg = match_end(0, st)
        end = line_end_at(beg)
        call text_property_any(beg, end, sym("font_lock_face"), sym(grep_match_face), mbeg, found, st)
        if (found) number = mbeg - beg
    end subroutine first_match_column

    !> The end column of the first grep match on a line: END-COL of the
    !> `grep_regexp_alist' rule.  Not a port.
    subroutine first_match_end_column(number, found)
        integer(int64), intent(out) :: number
        logical, intent(out) :: found
        integer(int64) :: beg, end, mbeg, mend
        type(status_type) :: st

        number = 0
        found = .false.
        if (grep_highlight_matches == GREP_NIL) return
        beg = match_end(0, st)
        end = line_end_at(beg)
        call text_property_any(beg, end, sym("font_lock_face"), sym(grep_match_face), mbeg, found, st)
        if (.not. found) return
        call next_single_property_change(mbeg, sym("font_lock_face"), mend, found, st, end)
        if (found) number = mend - beg - 1
    end subroutine first_match_end_column

    !> The end of the line at POS (save-excursion, goto-char,
    !> line-end-position).  Not a port.
    integer(int64) function line_end_at(pos) result(eol)
        integer(int64), intent(in) :: pos
        integer(int64) :: opoint

        opoint = point()
        call goto_char(pos)
        eol = line_end_position()
        call goto_char(opoint)
    end function line_end_at

    !> The value of `grep_regexp_alist'.  Not a port.
    subroutine init_grep_regexp_alist()
        if (allocated(grep_regexp_alist)) deallocate (grep_regexp_alist)
        allocate (grep_regexp_alist(2))
        grep_regexp_alist(1)%regexp = "^\(?:" &
                                      ! Parse using NUL characters when `--null' is used.  Note
                                      ! that we must still assume no newlines in filenames due to
                                      ! "foo: Is a directory." type messages.
                                      //"\(?1:[^"//NUL//NL//"]+\)\(?3:"//NUL//"\)\(?2:[0-9]+\):" &
                                      //"\|" &
                                      ! Fallback if `--null' is not used, use a tight regexp to
                                      ! handle weird file names (with colons in them) as well as
                                      ! possible.  E.g., use [1-9][0-9]* rather than [0-9]+ so as
                                      ! to accept ":034:" in file names.
                                      //"\(?1:" &
                                      //"\(?:[a-zA-Z]:\)?" & ! Allow "C:..." for w32.
                                      //"[^"//NL//":]+?[^"//NL//"/:]\):["//TB//" ]*\(?2:[1-9][0-9]*\)["//TB//" ]*:" &
                                      //"\)"
        grep_regexp_alist(1)%file = 1
        grep_regexp_alist(1)%line = 2
        ! Calculate column positions (col . end-col) of first grep match on
        ! a line.
        grep_regexp_alist(1)%column_function => first_match_column
        grep_regexp_alist(1)%end_column_function => first_match_end_column
        allocate (grep_regexp_alist(1)%highlights(1))
        grep_regexp_alist(1)%highlights(1)%submatch = 3
        grep_regexp_alist(1)%highlights(1)%face = ""
        grep_regexp_alist(1)%highlights(1)%display = ":"
        grep_regexp_alist(2)%regexp = "^Binary file \(.+\) matches"
        grep_regexp_alist(2)%file = 1
        grep_regexp_alist(2)%type = 0
        grep_regexp_alist(2)%hyperlink = 1
        allocate (grep_regexp_alist(2)%highlights(0))
    end subroutine init_grep_regexp_alist

    !> The value of `grep_mode_font_lock_keywords' (the faces of the face
    !> variables are read when the keywords are made).  Not a port.
    subroutine init_grep_mode_font_lock_keywords()
        type(font_lock_keyword_type), allocatable :: k(:)
        character(len=*), parameter :: NO_MESSAGE(3) = [character(len=19) :: "compilation_message", "help_echo", &
                                                                                                                       "mouse_face"]

        allocate (k(7))
        ! Command output lines.
        k(1)%regexp = ": \(.\{,200\}\): \(?:Permission denied\|No such \(?:file or directory\|device or address\)\)$"
        allocate (k(1)%highlights(1))
        k(1)%highlights(1) = font_lock_highlight(1, face=grep_error_face)
        ! Remove match from grep-regexp-alist before fontifying.
        k(2)%regexp = "^Grep[/a-zA-Z]* started.*"
        allocate (k(2)%highlights(1))
        k(2)%highlights(1) = font_lock_highlight(0, override=FONT_LOCK_OVERRIDE_T, nil_properties=NO_MESSAGE)
        k(3)%regexp = "^Grep[/a-zA-Z]* finished with \(?:\(\(?:[0-9]+ \)?match\(?:es\)? found\)\|" &
                      //"\(no matches found\)\).*"
        allocate (k(3)%highlights(3))
        k(3)%highlights(1) = font_lock_highlight(0, override=FONT_LOCK_OVERRIDE_T, nil_properties=NO_MESSAGE)
        k(3)%highlights(2) = font_lock_highlight(1, face=setting_value(compilation_info_face%value), laxmatch=.true.)
        k(3)%highlights(3) = font_lock_highlight(2, face=setting_value(compilation_warning_face%value), laxmatch=.true.)
        k(4)%regexp = "^Grep[/a-zA-Z]* \(exited abnormally\|interrupt\|killed\|terminated\)" &
                      //"\(?:.*with code \([0-9]+\)\)?.*"
        allocate (k(4)%highlights(3))
        k(4)%highlights(1) = font_lock_highlight(0, override=FONT_LOCK_OVERRIDE_T, nil_properties=NO_MESSAGE)
        k(4)%highlights(2) = font_lock_highlight(1, face=grep_error_face)
        k(4)%highlights(3) = font_lock_highlight(2, face=grep_error_face, laxmatch=.true.)
        ! "filename-linenumber-" format is used for context lines in GNU
        ! grep, "filename=linenumber=" for lines with function names in
        ! "git grep -p".
        k(5)%regexp = "^.+?\([-="//NUL//"]\)[0-9]+\([-=]\).*"//NL
        allocate (k(5)%highlights(2))
        k(5)%highlights(1) = font_lock_highlight(0, face=grep_context_face)
        k(5)%highlights(2) = font_lock_highlight(1, properties_function=context_separator_properties)
        ! Hide excessive part of rgrep command.
        k(6)%regexp = "^find \(\(?:-H \)?\. -type d .*\(?:\\)\|"")""\)\)"
        allocate (k(6)%highlights(1))
        k(6)%highlights(1) = font_lock_highlight(1, properties_function=abbreviate_properties)
        ! Hide excessive part of lgrep command.
        k(7)%regexp = "^grep \( *--exclude.*--exclude[^ ]+\)"
        allocate (k(7)%highlights(1))
        k(7)%highlights(1) = font_lock_highlight(1, properties_function=abbreviate_properties)
        call move_alloc(k, grep_mode_font_lock_keywords)
    end subroutine init_grep_mode_font_lock_keywords

    !> S, or "" for nil.  Not a port.
    function setting_value(s) result(v)
        character(len=:), allocatable, intent(in) :: s
        character(len=:), allocatable :: v

        v = ""
        if (allocated(s)) v = s
    end function setting_value

    !> (if (eq (char-after (match-beginning 1)) ?\0) `(face nil display
    !> ,(match-string 2))): a NUL separator of a context line is shown as
    !> the separator after the line number.  Not a port.
    subroutine context_separator_properties(start, end, status)
        integer(int64), intent(in) :: start, end
        type(status_type), intent(out) :: status
        integer :: c
        logical :: found

        status = status_type()
        call char_after(c, found, start)
        if (.not. found .or. c /= 0) return
        call put_text_property(start, end, sym("face"), nil(), status)
        if (status%ok()) call put_text_property(start, end, sym("display"), make_string(match_string(2)), status)
    end subroutine context_separator_properties

    !> (if grep-find-abbreviate grep-find-abbreviate-properties '(face nil
    !> abbreviated-command t)).  Not a port.
    subroutine abbreviate_properties(start, end, status)
        integer(int64), intent(in) :: start, end
        type(status_type), intent(out) :: status
        logical :: changed

        if (grep_find_abbreviate) then
            call add_text_properties(start, end, grep_find_abbreviate_properties(), changed, status)
        else
            call add_text_properties(start, end, list(sym("face"), nil(), sym("abbreviated_command"), t_value()), &
                                     changed, status)
        end if
    end subroutine abbreviate_properties

    !> Properties of button-like ellipsis on part of rgrep command line
    !> (the `keymap' is Deferred).  Port of grep.el
    !> grep-find-abbreviate-properties.
    function grep_find_abbreviate_properties() result(props)
        type(value_type) :: props
        type(value_type) :: parts(10)

        parts(1) = sym("face")
        parts(2) = nil()
        parts(3) = sym("display")
        ! (char-displayable-p ?…): the terminal's coding system is UTF-8.
        parts(4) = make_string("[…]")
        parts(5) = sym("mouse_face")
        parts(6) = sym("highlight")
        parts(7) = sym("help_echo")
        parts(8) = make_string("RET, mouse-2: show unabbreviated command")
        parts(9) = sym("abbreviated_command")
        parts(10) = t_value()
        props = make_list(parts)
    end function grep_find_abbreviate_properties

    !> The mode line of grep buffers after the process state: the number
    !> of matches so far.  Port of grep.el grep-mode-line-matches.
    subroutine grep_mode_line_matches(parts)
        type(mode_line_part_type), allocatable, intent(out) :: parts(:)
        character(len=24) :: buf
        integer :: k

        k = local_entry()
        write (buf, "(i0)") locals(k)%num_matches_found
        allocate (parts(3))
        parts(1)%text = " ["
        parts(1)%face = ""
        parts(2)%text = trim(buf)
        parts(2)%face = grep_hit_face
        parts(3)%text = "]"
        parts(3)%face = ""
    end subroutine grep_mode_line_matches

    !> Sets `grep_last_buffer' and `compilation_window_height'.  Port of
    !> grep.el:grep-mode (`compilation_window_height', the tool bar,
    !> `kill_transform_function' and the outline functions are Deferred).
    recursive subroutine grep_mode(status)
        type(status_type), intent(out) :: status
        type(compilation_directory_matcher_type) :: matcher
        type(compilation_transform_type), allocatable :: transforms(:)
        logical :: saved
        integer :: n, i

        if (.not. associated(major_modes%grep_mode%run)) call syms_of_grep()
        call major_modes%grep_mode%begin(saved, status)
        if (.not. status%ok()) return
        ! The variables define-compilation-mode sets from the mode's name.
        call compilation_scroll_output%set_local(merge(COMPILATION_SCROLL_T, COMPILATION_SCROLL_NIL, grep_scroll_output))
        call compilation_first_column%set_local(grep_first_column)
        call init_grep_mode_font_lock_keywords()
        ! The body.
        grep_last_buffer = current_buffer()
        call compilation_error_face%set_local(grep_hit_face)
        n = size(grep_compilation_transform_finished_rules)

        if (allocated(compilation_transform_file_match_alist)) then
            allocate (transforms(n + size(compilation_transform_file_match_alist)))

            do i = 1, size(compilation_transform_file_match_alist)
                transforms(n + i) = compilation_transform_file_match_alist(i)
            end do
        else
            allocate (transforms(n))
        end if

        do i = 1, n
            transforms(i) = grep_compilation_transform_finished_rules(i)
        end do

        ! compilation-directory-matcher can't be nil, so we set it to a
        ! regexp that can never match.
        matcher%regexp = "\`a\`"
        allocate (matcher%dir(0), matcher%leave(0))
        call compilation_set_mode_locals(error_regexp_alist=grep_regexp_alist, directory_matcher=matcher, &
                                         transform_file_match_alist=transforms, error=grep_error, &
                                         mode_font_lock_keywords=grep_mode_font_lock_keywords, &
                                         mode_line_errors=grep_mode_line_matches, &
                                         process_setup_function=grep_process_setup)
        call compilation_disable_input%set_local(.true.)
        call compilation_error_screen_columns%set_local(grep_error_screen_columns)
        if (grep_use_headings) call compilation_filter_hook%add(grep_heading_filter, depth=80, local=.true.)
        call compilation_filter_hook%add(grep_filter, local=.true.)
        call major_modes%grep_mode%finish(saved, status)
    end subroutine grep_mode

    !> Save the buffers before the grep commands run, as
    !> `grep_save_buffers' says.  Port of grep.el:grep--save-buffers.
    recursive subroutine grep_save_buffers_now(status)
        type(status_type), intent(out) :: status

        status = status_type()

        if (associated(grep_save_buffers_function)) then
            call save_some_buffers(.false., .false., status, save_predicate)
        else if (grep_save_buffers /= GREP_SAVE_BUFFERS_NIL) then
            call save_some_buffers(grep_save_buffers /= GREP_SAVE_BUFFERS_ASK, .false., status)
        end if
    end subroutine grep_save_buffers_now

    ! Whether the current buffer should be saved, as the function in
    ! `grep_save_buffers' says.  Not a port.
    logical function save_predicate()
        save_predicate = grep_save_buffers_function()
    end function save_predicate

    !> Setup compilation variables and buffer for `grep'.
    !> Set up `compilation_exit_message_function' and run
    !> `grep_setup_hook'.  Port of grep.el:grep-process-setup.
    recursive subroutine grep_process_setup(environment, status)
        type(process_arg_type), allocatable, intent(inout) :: environment(:)
        type(status_type), intent(out) :: status
        integer :: k

        status = status_type()

        if (grep_highlight_matches == GREP_AUTO_DETECT) then
            call grep_compute_defaults(status)
            if (.not. status%ok()) return
        end if

        ! Don't output color escapes if they can't be highlighted with
        ! `font-lock-face' by `grep-filter'.
        if (grep_highlight_matches /= GREP_AUTO_DETECT .and. grep_highlight_matches /= GREP_NIL .and. &
            font_lock_mode_enabled()) then
            ! Any TERM except "dumb" allows GNU grep to use `--color=auto'.
            call setenv("TERM=emacs-grep")
            ! GREP_COLOR is used in GNU grep 2.5.1, but deprecated in later
            ! versions.
            call setenv("GREP_COLOR=01;31")
            ! GREP_COLORS is used in GNU grep 2.5.2 and later versions.
            call setenv("GREP_COLORS=mt=01;31:fn=:ln=:bn=:se=:sl=:cx=:ne")
        end if

        k = local_entry()
        locals(k)%num_matches_found = 0
        call compilation_set_mode_locals(exit_message_function=grep_exit_message)
        call grep_setup_hook%run(status)

    contains

        subroutine setenv(entry)
            character(len=*), intent(in) :: entry
            type(process_arg_type), allocatable :: grown(:)
            integer :: n, i

            n = size(environment)
            allocate (grown(n + 1))

            do i = 1, n
                grown(i) = environment(i)
            end do

            grown(n + 1)%s = entry
            call move_alloc(grown, environment)
        end subroutine setenv
    end subroutine grep_process_setup

    !> Return a status message for grep results.  Port of
    !> grep.el:grep-exit-message.
    subroutine grep_exit_message(process_status, exit_status, msg, message, code)
        character(len=*), intent(in) :: process_status, msg
        integer, intent(in) :: exit_status
        character(len=:), allocatable, intent(out) :: message, code
        character(len=24) :: buf
        type(status_type) :: st
        logical :: modified
        integer :: k

        write (buf, "(i0)") exit_status
        message = msg
        code = trim(buf)
        if (process_status /= "exit") return
        ! This relies on the fact that `compilation_start' sets
        ! buffer-modified to false before running the command, so the
        ! buffer is still unmodified if there is no output.
        call buffer_modified_p(current_buffer(), modified, st)

        if (exit_status == 0 .and. modified) then
            k = local_entry()

            if (locals(k)%num_matches_found > 0) then
                write (buf, "(i0)") locals(k)%num_matches_found

                if (locals(k)%num_matches_found == 1) then
                    message = "finished with "//trim(buf)//" match found"//NL
                else
                    message = "finished with "//trim(buf)//" matches found"//NL
                end if
            else
                message = "finished with matches found"//NL
            end if

            code = "matched"
        else if (.not. modified) then
            message = "finished with no matches found"//NL
            code = "no match"
        end if
    end subroutine grep_exit_message

    !> Handle match highlighting escape sequences inserted by the grep
    !> process.  This function is called from `compilation_filter_hook'.
    !> Port of grep.el:grep-filter.
    recursive subroutine grep_filter(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: endm
        type(status_type) :: st
        integer(int64) :: opoint, end, beg, found, mb, me, limit
        character(len=:), allocatable :: text
        integer :: k

        status = status_type()
        opoint = point()
        call forward_line_0()
        end = point()
        call goto_char(compilation_filter_start)
        call forward_line_0()
        beg = point()

        ! Only operate on whole lines so we don't get caught with part of
        ! an escape sequence in one chunk and the rest in another.
        if (point() < end) then
            call copy_marker_at(end, endm, .false., st)
            k = local_entry()

            ! Highlight grep matches and delete marking sequences.
            do
                call marker_position(endm, limit, st)
                call re_search_forward(grep_match_regexp, found, status, bound=limit, noerror=SEARCH_MOVE)
                if (.not. status%ok()) exit
                if (found == 0) exit
                text = match_string(1)
                call replace_match(text, st, fixedcase=.true., literal=.true.)
                mb = match_beginning(0, st)
                me = point()
                call put_text_property(mb, me, sym("face"), nil(), st)
                call put_text_property(mb, me, sym("font_lock_face"), sym(grep_match_face), st)
                k = local_entry()
                locals(k)%num_matches_found = locals(k)%num_matches_found + 1
            end do

            ! Delete all remaining escape sequences.
            call goto_char(beg)

            do while (status%ok())
                call marker_position(endm, limit, st)
                call re_search_forward(ESC//"\[[0-9;]*[mK]", found, status, bound=limit, noerror=SEARCH_MOVE)
                if (.not. status%ok()) exit
                if (found == 0) exit
                call replace_match("", st, fixedcase=.true., literal=.true.)
            end do

            call free_marker(endm, st)
            call compilation_update_mode_line()
        end if

        call goto_char(min(opoint, point_max()))

    contains

        subroutine forward_line_0()
            integer(int64) :: moved

            moved = forward_line(0_int64)
        end subroutine forward_line_0
    end subroutine grep_filter

    !> Filter function to add headings to output of a grep process.
    !> Port of grep.el:grep--heading-filter.
    recursive subroutine grep_heading_filter(status)
        type(status_type), intent(out) :: status
        type(value_type) :: annotation
        type(status_type) :: st
        character(len=:), allocatable :: heading
        integer(int64) :: opoint, limit, bol, found, start, end, here, hb
        logical :: matched
        integer :: k

        status = status_type()
        k = local_entry()

        if (.not. locals(k)%has_heading_state) then
            call copy_marker_at(point_min(), locals(k)%heading_limit, .false., st)
            locals(k)%has_heading_state = .true.
        end if

        opoint = point()
        ! Move point to the old limit and update limit marker.
        bol = line_beginning_position()
        call marker_position(locals(k)%heading_limit, limit, st)
        call goto_char(limit)
        call set_marker(locals(k)%heading_limit, bol, st)

        do
            call re_search_forward(grep_heading_regexp, found, status, bound=bol, noerror=SEARCH_NOERROR)
            if (.not. status%ok()) exit
            if (found == 0) exit
            call get_text_property(point(), sym("compilation_annotation"), annotation, st)
            if (.not. nilp(annotation)) cycle
            heading = match_string(1, matched=matched)
            start = match_beginning(2, st)
            end = match_end(2, st)
            if (start > 0) call put_text_property(start, end, sym("invisible"), t_value(), st)
            if (.not. matched) cycle
            k = local_entry()

            if (allocated(locals(k)%heading)) then
                if (heading == locals(k)%heading .and. len(heading) == len(locals(k)%heading)) cycle
            end if

            here = point()
            call goto_char(line_beginning_position())
            hb = point()
            ! grep--heading-format.
            call insert_before_markers(heading//NL, st)
            call put_text_property(hb, hb + len_chars(heading), sym("font_lock_face"), sym("grep_heading"), st)
            call put_text_property(hb, hb + len_chars(heading), sym("outline_level"), make_fixnum(1_int64), st)
            call put_text_property(hb, point(), sym("compilation_annotation"), t_value(), st)
            call goto_char(here + (point() - hb))
            locals(k)%heading = heading
            ! The limit marker is a marker; the bound of the search moves
            ! with the inserted text.
            bol = bol + (point() - here)
        end do

        call goto_char(opoint)
    end subroutine grep_heading_filter

    !> The number of characters of the UTF-8 string S.  Not a port.
    integer(int64) function len_chars(s) result(n)
        character(len=*), intent(in) :: s
        integer :: i

        n = 0

        do i = 1, len(s)
            if (iand(iachar(s(i:i)), 192) /= 128) n = n + 1
        end do
    end function len_chars

    ! ------------------------------------------------------------------
    ! Defaults
    ! ------------------------------------------------------------------

    !> Make a temporary buffer current (with-temp-buffer); OLD is the
    !> buffer to go back to.  Not a port.
    subroutine temp_begin(old, tmp)
        type(handle_type), intent(out) :: old, tmp
        type(status_type) :: st
        character(len=:), allocatable :: dir

        old = current_buffer()
        dir = ""
        if (allocated(buffers(cur)%directory)) dir = buffers(cur)%directory
        call get_buffer_create(generate_new_buffer_name(" *temp*"), tmp, st)
        call set_buffer(tmp, st)
        buffers(cur)%directory = dir
    end subroutine temp_begin

    !> Kill the temporary buffer TMP and make OLD current again.
    !> Not a port.
    subroutine temp_end(old, tmp)
        type(handle_type), intent(in) :: old, tmp
        type(status_type) :: st

        call kill_buffer(tmp, st)
        call set_buffer(old, st)
    end subroutine temp_end

    !> Run PROGRAM with ARGS (process-file), its output in the current
    !> buffer if INSERT; whether it exits with status RESULT (default 0).
    !> Port of grep.el:grep-probe.
    logical function grep_probe(program, args, insert, result) result(ok)
        character(len=*), intent(in) :: program
        type(minibuffer_string_type), intent(in) :: args(:)
        logical, intent(in) :: insert
        integer, intent(in), optional :: result
        type(value_type), allocatable :: argv(:)
        type(value_type) :: res, dest
        type(status_type) :: st
        integer :: expected, i

        expected = 0
        if (present(result)) expected = result
        allocate (argv(size(args)))

        do i = 1, size(args)
            argv(i) = make_string(args(i)%s)
        end do

        dest = nil()
        if (insert) dest = t_value()
        call call_process(program, res, st, destination=dest, args=make_list(argv))
        ok = .false.
        if (.not. st%ok()) return
        if (fixnump(res)) ok = xfixnum(res) == expected
    end function grep_probe

    !> A file with the text "Copyright" for the probes: a temporary file
    !> (there is no HELLO file in `data_directory').  Port of
    !> grep.el:grep-hello-file.
    subroutine grep_hello_file(file, status)
        character(len=:), allocatable, intent(out) :: file
        type(status_type), intent(out) :: status
        integer :: unit, ios

        call make_temp_file("grep-test-", file, status)
        if (.not. status%ok()) return
        open (newunit=unit, file=file, access="stream", form="unformatted", status="replace", action="write", &
              iostat=ios)

        if (ios /= 0) then
            call set_error(status, FM_GENERIC_ERROR, "Cannot write "//file)
            return
        end if

        write (unit, iostat=ios) "Copyright"//NL
        close (unit)
        if (ios /= 0) call set_error(status, FM_GENERIC_ERROR, "Cannot write "//file)
    end subroutine grep_hello_file

    !> Compute the defaults for the `grep' command.
    !> The value depends on `grep_command', `grep_template',
    !> `grep_use_null_device', `grep_find_command', `grep_find_template',
    !> `grep_use_null_filename_separator', `grep_find_use_xargs',
    !> `grep_highlight_matches', and `grep_quoting_style'.  Port of
    !> grep.el:grep-compute-defaults (the local host only).
    recursive subroutine grep_compute_defaults(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: old, tmp
        type(status_type) :: st
        type(grep_defaults_type) :: d
        character(len=:), allocatable :: hello, grep_options, cmd0, null, gcmd, color
        character(len=*), parameter :: QUOT_BRACES = "\{\}", QUOT_SCOLON = "\;"
        type(value_type) :: res
        logical :: ok

        status = status_type()

        ! Keep default values.
        if (.not. grep_host_defaults_alist%has_defaults) then
            grep_host_defaults_alist%defaults = current_defaults()
            grep_host_defaults_alist%has_defaults = .true.
        end if

        ! There are different defaults on different hosts.  They must be
        ! computed for every host once.
        if (grep_host_defaults_alist%has_localhost) then
            d = grep_host_defaults_alist%localhost
        else
            d = grep_host_defaults_alist%defaults
        end if

        d%quoting_style = grep_quoting_style
        call set_defaults(d)

        if (grep_use_null_device /= GREP_NIL .and. grep_use_null_device /= GREP_T) then
            call temp_begin(old, tmp)
            call grep_hello_file(hello, st)
            ok = .false.

            if (st%ok()) then
                if (allocated(grep_command)) then
                    ! `grep_command' is already set, so use that for testing.
                    ! process-file-shell-command.
                    if (.not. allocated(shell_file_name%value)) call init_callproc()
                    call call_process(shell_file_name%value, res, st, destination=t_value(), &
                                      args=list(make_string(shell_command_switch%value), &
                                                make_string(grep_command//" "//shell_quote_argument("^Copyright")// &
                                                            " "//shell_quote_argument(hello))))
                    if (st%ok() .and. fixnump(res)) ok = xfixnum(res) == 0
                else
                    ! Otherwise use `grep_program'.
                    ok = grep_probe(grep_program, strings([character(len=10) :: "-nH", "^Copyright"], hello), .true.)
                end if

                if (ok) then
                    call goto_char(point_min())
                    ok = looking_at(regexp_quote(hello)//":[0-9]+:Copyright", st)
                end if

                call delete_file(hello, st)
            end if

            grep_use_null_device = merge(GREP_NIL, GREP_T, ok)
            call temp_end(old, tmp)
        end if

        if (grep_use_null_filename_separator == GREP_AUTO_DETECT) then
            call temp_begin(old, tmp)
            call grep_hello_file(hello, st)
            ok = .false.

            if (st%ok()) then
                if (grep_use_null_device /= GREP_NIL) then
                    ok = grep_probe(grep_program, strings([character(len=10) :: "--null", "-ne", "^Copyright"], &
                                                          hello, NULL_DEVICE), .true.)
                else
                    ok = grep_probe(grep_program, strings([character(len=10) :: "-H", "--null", "-ne", "^Copyright"], &
                                                          hello), .true.)
                end if

                if (ok) then
                    call goto_char(point_min())
                    ok = looking_at(regexp_quote(hello)//NUL//"[0-9]+:Copyright", st)
                end if

                call delete_file(hello, st)
            end if

            grep_use_null_filename_separator = merge(GREP_T, GREP_NIL, ok)
            call temp_end(old, tmp)
        end if

        if (grep_highlight_matches == GREP_AUTO_DETECT) then
            call temp_begin(old, tmp)
            ! The "grep --help" exit status varies; pay no attention to it.
            ok = grep_probe(grep_program, strings([character(len=6) :: "--help"]), .true.)
            call goto_char(point_min())
            call re_search_forward_cs("--color[^a-z]", ok)
            grep_highlight_matches = merge(GREP_AUTO, GREP_NIL, ok)
            call temp_end(old, tmp)
        end if

        if (.not. (allocated(grep_command) .and. allocated(grep_find_command) .and. allocated(grep_template) .and. &
                   allocated(grep_find_template))) then
            if (grep_use_null_device /= GREP_NIL) then
                grep_options = "-n"
            else
                grep_options = "-nH"
            end if

            if (grep_use_null_filename_separator /= GREP_NIL) grep_options = grep_options//" --null"

            if (grep_probe(grep_program, strings([character(len=3) :: "-e", "foo"], NULL_DEVICE), .false., 1)) then
                grep_options = grep_options//" -e"
            end if

            if (.not. allocated(grep_command)) then
                color = ""

                if (grep_highlight_matches /= GREP_NIL) then
                    if (grep_probe(grep_program, strings([character(len=7) :: "--color", "x"], NULL_DEVICE), .false., 1)) &
                        then
                        if (grep_highlight_matches == GREP_ALWAYS) then
                            color = "--color=always"
                        else
                            color = "--color=auto"
                        end if
                    end if
                end if

                grep_command = grep_program//" "//color//" "//grep_options//" "
            end if

            if (.not. allocated(grep_template)) grep_template = grep_program//" <X> <C> "//grep_options//" <R> <F>"

            if (grep_find_use_xargs == GREP_FIND_USE_XARGS_NIL) then
                ! For performance, we want:
                ! A. Run grep on batches of files (instead of one grep per
                !    file)
                ! B. If the directory is large and we need multiple
                !    batches, run find in parallel with a running grep.
                ! "find | xargs grep" gives both A and B
                if (grep_probe(find_program, strings([character(len=7) :: NULL_DEVICE, "-print0"]), .false.) .and. &
                    grep_probe(xargs_program, strings([character(len=4) :: "-0", "echo"]), .false.)) then
                    grep_find_use_xargs = GREP_FIND_USE_XARGS_GNU
                    ! "find -exec {} +" gives A but not B
                else if (grep_probe(find_program, strings([character(len=9) :: NULL_DEVICE, "-exec", "echo", "{}", "+"]), &
                                    .false.)) then
                    grep_find_use_xargs = GREP_FIND_USE_XARGS_EXEC_PLUS
                else
                    ! "find -exec {} ;" gives neither A nor B.
                    grep_find_use_xargs = GREP_FIND_USE_XARGS_EXEC
                end if
            end if

            null = ""
            if (grep_use_null_device /= GREP_NIL) null = NULL_DEVICE//" "

            if (.not. allocated(grep_find_command)) then
                grep_find_command_position = 0

                select case (grep_find_use_xargs)
                case (GREP_FIND_USE_XARGS_GNU)
                    grep_find_command = find_program//" . -type f -print0 | "//xargs_program//" -0 "//grep_command
                case (GREP_FIND_USE_XARGS_GNU_SORT)
                    grep_find_command = find_program//" . -type f -print0 | sort -z | "//xargs_program//" -0 "// &
                                        grep_command
                case (GREP_FIND_USE_XARGS_EXEC, GREP_FIND_USE_XARGS_EXEC_PLUS)
                    cmd0 = find_program//" . -type f -exec "//grep_command

                    if (grep_find_use_xargs == GREP_FIND_USE_XARGS_EXEC_PLUS) then
                        grep_find_command = cmd0//" "//null//QUOT_BRACES//" +"
                    else
                        grep_find_command = cmd0//" "//QUOT_BRACES//" "//null//QUOT_SCOLON
                    end if

                    grep_find_command_position = len_chars(cmd0) + 1
                case default
                    grep_find_command = find_program//" . -type f -print | "//xargs_program//" "//grep_command
                end select
            end if

            if (.not. allocated(grep_find_template)) then
                gcmd = grep_program//" <C> "//grep_options//" <R>"

                select case (grep_find_use_xargs)
                case (GREP_FIND_USE_XARGS_GNU)
                    grep_find_template = find_program//" -H <D> <X> -type f <F> -print0 | "//xargs_program//" -0 "//gcmd
                case (GREP_FIND_USE_XARGS_GNU_SORT)
                    grep_find_template = find_program//" -H <D> <X> -type f <F> -print0 | sort -z | "//xargs_program// &
                                         " -0 "//gcmd
                case (GREP_FIND_USE_XARGS_EXEC)
                    grep_find_template = find_program//" -H <D> <X> -type f <F> -exec "//gcmd//" "//QUOT_BRACES//" "// &
                                         null//QUOT_SCOLON
                case (GREP_FIND_USE_XARGS_EXEC_PLUS)
                    grep_find_template = find_program//" -H <D> <X> -type f <F> -exec "//gcmd//" "//null// &
                                         QUOT_BRACES//" +"
                case default
                    grep_find_template = find_program//" -H <D> <X> -type f <F> -print | "//xargs_program//" "//gcmd
                end select
            end if

            ! (setq grep-quoting-style (and remote 'posix)): the local host.
            grep_quoting_style = .false.
        end if

        ! Save defaults for this host.
        grep_host_defaults_alist%localhost = current_defaults()
        grep_host_defaults_alist%has_localhost = .true.

    contains

        !> re-search-forward with case-fold-search false, FOUND whether
        !> it matched.
        subroutine re_search_forward_cs(regexp, found)
            character(len=*), intent(in) :: regexp
            logical, intent(out) :: found
            logical :: saved
            integer(int64) :: pos
            type(status_type) :: s

            saved = case_fold_search%value
            case_fold_search = .false.
            call re_search_forward(regexp, pos, s, noerror=SEARCH_NOERROR)
            case_fold_search = saved
            found = s%ok() .and. pos /= 0
        end subroutine re_search_forward_cs
    end subroutine grep_compute_defaults

    !> The strings WORDS (trimmed), then FILE and EXTRA if given.
    !> Not a port.
    function strings(words, file, extra) result(list)
        character(len=*), intent(in) :: words(:)
        character(len=*), intent(in), optional :: file, extra
        type(minibuffer_string_type), allocatable :: list(:)
        integer :: n, i

        n = size(words)
        if (present(file)) n = n + 1
        if (present(extra)) n = n + 1
        allocate (list(n))

        do i = 1, size(words)
            list(i)%s = trim(words(i))
        end do

        n = size(words)

        if (present(file)) then
            n = n + 1
            list(n)%s = file
        end if

        if (present(extra)) then
            n = n + 1
            list(n)%s = extra
        end if
    end function strings

    !> A regexp matching exactly S (regexp-quote).  Not a port.
    function regexp_quote(s) result(r)
        character(len=*), intent(in) :: s
        character(len=:), allocatable :: r
        integer :: i

        r = ""

        do i = 1, len(s)
            if (index("[*.\?+^$", s(i:i)) > 0) r = r//"\"
            r = r//s(i:i)
        end do
    end function regexp_quote

    !> The default for the grep commands: the region if it is active and
    !> not empty, else the tag at point (`find_tag_default'), else "".
    !> Port of grep.el:grep-tag-default (`find_tag_default_function' is
    !> Deferred).
    function grep_tag_default() result(tag)
        character(len=:), allocatable :: tag
        type(status_type) :: st
        logical :: found
        integer(int64) :: m

        m = mark_position()

        if (transient_mark_mode%value .and. buffers(cur)%mark_active .and. m /= 0) then
            if (point() /= m) then
                call buffer_substring(min(point(), m), max(point(), m), tag, st)
                return
            end if
        end if

        call find_tag_default(tag, found)
        if (.not. found) tag = ""
    end function grep_tag_default

    !> The first element of the history list HISTORY, if any.  Not a port.
    subroutine history_first(history, first, found)
        character(len=*), intent(in) :: history
        character(len=:), allocatable, intent(out) :: first
        logical, intent(out) :: found
        type(value_type) :: v
        type(value_type), allocatable :: elems(:)
        type(status_type) :: st

        found = .false.
        call symbol_value(sym(history), v, st)
        if (.not. st%ok()) return
        if (.not. consp(v)) return
        call list_elements(v, elems)
        if (.not. stringp(elems(1))) return
        first = xstring(elems(1))
        found = .true.
    end subroutine history_first

    !> Compute the default grep command for \[universal_argument] \[grep]
    !> to offer.  Port of grep.el:grep-default-command.
    function grep_default_command() result(command)
        character(len=:), allocatable :: command
        character(len=:), allocatable :: tag_default, sh_arg_re, grep_default, pattern, ext
        integer(int64), allocatable :: md(:)
        type(status_type) :: st
        logical :: found, matched
        integer(int64) :: b2, e2

        tag_default = shell_quote_argument(grep_tag_default())
        ! This a regexp to match single shell arguments.
        sh_arg_re = "\(\(?:""\(?:[^""]\|\\""\)+""\|'[^']+'\|[^""' "//TB//NL//"]\)+\)"
        call history_first("grep_history", grep_default, found)

        if (.not. found) then
            grep_default = ""
            if (allocated(grep_command)) grep_default = grep_command
        end if

        command = grep_default

        ! In the default command, find the arg that specifies the pattern.
        if (string_match("[^ ]+\s +\(?:-[^ ]+\s +\)*"//sh_arg_re//"\(\s +\(\S +\)\)?", grep_default, st) == 0) then
            ! If the string is not yet complete.
            if (string_match("\(\)\'", grep_default, st) == 0) return
        end if

        md = match_data()

        ! Maybe we will replace the pattern with the default tag.  But
        ! first, maybe replace the file name pattern.
        if (allocated(buffers(cur)%filename)) then
            b2 = match_beginning(2, st)
            matched = .false.

            if (b2 > 0) then
                pattern = match_string(3, grep_default)
                matched = string_match(wildcard_to_regexp(file_name_nondirectory(pattern)), &
                                       file_name_nondirectory(buffers(cur)%filename), st) /= 0
                call set_match_data(md)
            end if

            if (.not. matched) then
                call file_name_extension(buffers(cur)%filename, ext, found)
                if (.not. found) ext = ""

                ! (substring grep-default 0 (match-beginning 2)): the whole
                ! string when group 2 did not match (nil).
                if (b2 > 0) then
                    e2 = string_index_to_offset(grep_default, b2)
                    grep_default = grep_default(:e2)//" *."//ext
                else
                    grep_default = grep_default//" *."//ext
                end if
            end if
        end if

        ! Now replace the pattern with the default tag.
        call replace_match(tag_default, st, fixedcase=.true., literal=.true., string=grep_default, subexp=1, &
                           result=command)
        if (.not. st%ok()) command = grep_default
    end function grep_default_command

    ! ------------------------------------------------------------------
    ! The commands
    ! ------------------------------------------------------------------

    !> Run Grep with user-specified COMMAND_ARGS.
    !> The output from the command goes to the "*grep*" buffer.
    !>
    !> While Grep runs asynchronously, you can use \[next_error] (M-x
    !> next_error), or \<grep_mode_map>\[compile_goto_error] in the *grep*
    !> buffer, to go to the lines where Grep found matches.  To kill the
    !> Grep job before it finishes, type \[kill_compilation].
    !>
    !> Noninteractively, COMMAND_ARGS should specify the Grep command-line
    !> arguments.
    !>
    !> For doing a recursive `grep', see the `rgrep' command.  For running
    !> Grep in a specific directory, see `lgrep'.
    !>
    !> This command uses a special history list for its COMMAND_ARGS, so
    !> you can easily repeat a grep command.
    !>
    !> A prefix argument says to default the COMMAND_ARGS based on the
    !> current tag the cursor is over, substituting it into the last Grep
    !> command in the Grep command history (or into `grep_command' if that
    !> history list is empty).  Port of grep.el:grep.
    recursive subroutine grep(command_args, status)
        character(len=*), intent(in) :: command_args
        type(status_type), intent(out) :: status

        call grep_1(command_args, .true., status)
    end subroutine grep

    !> `grep', appending `null_device' when USE_NULL_DEVICE and
    !> `grep_use_null_device' say so.
    !> Port of grep.el:grep, in part (its body).
    recursive subroutine grep_1(command_args, use_null_device, status)
        character(len=*), intent(in) :: command_args
        logical, intent(in) :: use_null_device
        type(status_type), intent(out) :: status

        ! If called non-interactively, also compute the defaults if we
        ! haven't already.
        if (grep_highlight_matches == GREP_AUTO_DETECT) then
            call grep_compute_defaults(status)
            if (.not. status%ok()) return
        end if

        call grep_save_buffers_now(status)
        if (.not. status%ok()) return

        if (use_null_device .and. grep_use_null_device == GREP_T) then
            call compilation_start(command_args//" "//NULL_DEVICE, status, mode=major_modes%grep_mode)
        else
            call compilation_start(command_args, status, mode=major_modes%grep_mode)
        end if
    end subroutine grep_1

    !> The command: read the command, then `grep'.  The interactive form of
    !> grep.el:grep.
    !>
    !> Run grep, with user-specified args, and collect output in a buffer.
    recursive subroutine cmd_grep(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: default, command, initial

        call grep_compute_defaults(status)
        if (.not. status%ok()) return
        default = grep_default_command()

        if (.not. nilp(current_prefix_arg)) then
            call read_shell_command("Run grep (like this): ", command, status, initial=default, history="grep_history")
        else
            initial = ""
            if (allocated(grep_command)) initial = grep_command

            if (grep_command_position /= 0) then
                call read_shell_command("Run grep (like this): ", command, status, initial=initial, &
                                        history="grep_history", default=default, &
                                        position=int(grep_command_position, int64))
            else
                call read_shell_command("Run grep (like this): ", command, status, initial=initial, &
                                        history="grep_history", default=default)
            end if
        end if

        if (.not. status%ok()) return
        call grep(command, status)
    end subroutine cmd_grep

    !> Mark relevant regions read-only, and add relevant occur
    !> text-properties.  Port of grep.el:grep-edit--prepare-buffer.
    recursive subroutine grep_edit_prepare_buffer(status)
        type(status_type), intent(out) :: status
        type(value_type) :: inhibit
        type(status_type) :: st
        type(handle_type) :: target_buffer, old
        type(handle_type) :: m(1), e(1)
        integer(int64) :: pos, beg, end, opoint, location, next
        logical :: found, changed
        integer :: idx

        status = status_type()
        opoint = point()
        call symbol_value(sym("inhibit_read_only"), inhibit, st)
        call set(sym("inhibit_read_only"), t_value(), st)
        pos = point_min()

        do
            call next_region("compilation_annotation", pos, beg, end, found)
            if (.not. found) exit
            call add_text_properties(beg, end, list(sym("read_only"), t_value(), sym("front_sticky"), t_value()), &
                                     changed, st)
            pos = end
        end do

        pos = point_min()

        do
            call next_region("compilation_message", pos, beg, end, found)
            if (.not. found) exit
            ! (Allow insertion of text right after prefix, but not before.)
            call add_text_properties(beg, end, list(sym("read_only"), t_value(), sym("occur_prefix"), t_value(), &
                                                    sym("front_sticky"), t_value(), sym("rear_nonsticky"), t_value()), &
                                     changed, st)
            ! Update the markers if necessary.
            call compilation_message_location(beg, target_buffer, location, found, status)
            if (.not. status%ok()) exit

            if (found) then
                old = current_buffer()
                call set_buffer(target_buffer, st)
                call copy_marker_at(location, m(1), .false., st)
                call copy_marker_at(location, e(1), .false., st)
                call set_buffer(old, st)
                idx = occur_add_target(m, e)
                call next_single_property_change(end, sym("compilation_message"), next, found, st)

                if (.not. found) then
                    call goto_char(end)
                    next = line_end_position() + 1
                end if

                call put_text_property(beg, min(next, point_max()), sym("occur_target"), make_fixnum(int(idx, int64)), st)
            end if

            pos = end
        end do

        call set(sym("inhibit_read_only"), inhibit, st)
        call goto_char(opoint)

    contains

        !> The next region from FROM where PROP is not nil, of one value
        !> (text-property-search-forward).
        subroutine next_region(prop, from, b, e2, ok)
            character(len=*), intent(in) :: prop
            integer(int64), intent(in) :: from
            integer(int64), intent(out) :: b, e2
            logical, intent(out) :: ok
            type(status_type) :: s

            call text_property_not_all(from, point_max(), sym(prop), nil(), b, ok, s)
            if (.not. ok) return
            call next_single_property_change(b, sym(prop), e2, ok, s)
            if (.not. ok) e2 = point_max()
            ok = .true.
        end subroutine next_region
    end subroutine grep_edit_prepare_buffer

    !> Major mode for editing *grep* buffers.
    !> In this mode, changes to the *grep* buffer are applied to the
    !> originating files.
    !> \<grep_edit_mode_map>
    !> Type \[grep_edit_save_changes] to exit Grep-Edit mode, return to Grep
    !> mode.
    !>
    !> The only editable texts in a Grep-Edit buffer are the match results.
    !> Port of grep.el:grep-edit-mode.
    recursive subroutine grep_edit_mode(status)
        type(status_type), intent(out) :: status

        call set_error(status, FM_GENERIC_ERROR, &
                       "This mode can be enabled only by `grep_change_to_grep_edit_mode'")
    end subroutine grep_edit_mode

    !> Switch to `grep_edit_mode' to edit *grep* buffer.  Port of
    !> grep.el:grep-change-to-grep-edit-mode.
    recursive subroutine grep_change_to_grep_edit_mode(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: proc
        integer :: k

        status = status_type()

        if (.not. derived_mode_p(major_modes%grep_mode)) then
            call set_error(status, FM_GENERIC_ERROR, "Not a Grep buffer")
            return
        end if

        proc = get_buffer_process(current_buffer())

        if (proc%index /= 0) then
            call set_error(status, FM_GENERIC_ERROR, "Cannot switch when grep is running")
            return
        end if

        k = local_entry()
        locals(k)%edit_original_map = current_local_map()
        call use_local_map(grep_edit_mode_map, status)
        if (.not. status%ok()) return
        call grep_edit_prepare_buffer(status)
        if (.not. status%ok()) return
        buffers(cur)%read_only = .false.
        buffers(cur)%major_mode = "grep_edit_mode"
        buffers(cur)%mode_name = "Grep-Edit"
        ! buffer-enable-undo.
        buffers(cur)%undo_enabled = .true.
        call restore_buffer_modified_p(.false.)
        if (allocated(buffers(cur)%undo_list)) deallocate (buffers(cur)%undo_list)
        call after_change_functions%add(occur_after_change_function, local=.true.)
        call run_mode_hooks(status, grep_edit_mode_hook)
        if (.not. status%ok()) return
        call message(substitute_command_keys("Editing: Type \[grep_edit_save_changes] to return to Grep mode"))
    end subroutine grep_change_to_grep_edit_mode

    !> Switch back to Grep mode.  Port of grep.el:grep-edit-save-changes.
    recursive subroutine grep_edit_save_changes(status)
        type(status_type), intent(out) :: status
        integer :: k

        status = status_type()

        if (.not. derived_mode_p("grep_edit_mode")) then
            call set_error(status, FM_GENERIC_ERROR, "Not a Grep-Edit buffer")
            return
        end if

        call after_change_functions%remove(occur_after_change_function, local=.true.)
        k = local_entry()
        call use_local_map(locals(k)%edit_original_map, status)
        buffers(cur)%read_only = .true.
        buffers(cur)%major_mode = "grep_mode"
        buffers(cur)%mode_name = "Grep"
        ! buffer-disable-undo.
        buffers(cur)%undo_enabled = .false.
        if (allocated(buffers(cur)%undo_list)) deallocate (buffers(cur)%undo_list)
        call message("Switching to Grep mode")
    end subroutine grep_edit_save_changes

    !> Run grep via find, with user-specified args COMMAND_ARGS.
    !> Collect output in the "*grep*" buffer.
    !> While find runs asynchronously, you can use the \[next_error]
    !> command to find the text that grep hits refer to.
    !>
    !> This command uses a special history list for its arguments, so you
    !> can easily repeat a find command.  Port of grep.el:grep-find.
    recursive subroutine grep_find(command_args, status)
        character(len=*), intent(in) :: command_args
        type(status_type), intent(out) :: status

        status = status_type()
        ! (let ((null-device nil)) (grep command-args)): see grep.
        if (len(command_args) > 0) call grep_1(command_args, .false., status)
    end subroutine grep_find

    !> The command: read the find command, then `grep_find'.  The
    !> interactive form of grep.el:grep-find.
    !>
    !> Run grep via find, with user-specified args.
    recursive subroutine cmd_grep_find(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: command, ignored

        call grep_compute_defaults(status)
        if (.not. status%ok()) return

        if (allocated(grep_find_command)) then
            if (grep_find_command_position /= 0) then
                call read_shell_command("Run find (like this): ", command, status, initial=grep_find_command, &
                                        history="grep_find_history", position=int(grep_find_command_position, int64))
            else
                call read_shell_command("Run find (like this): ", command, status, initial=grep_find_command, &
                                        history="grep_find_history")
            end if

            if (.not. status%ok()) return
            call grep_find(command, status)
        else
            ! No default was set.
            call read_string("compile.el: No `grep_find_command' command available. Press RET.", ignored, status)
        end if
    end subroutine cmd_grep_find

    !> Expand grep COMMAND string replacing <C>, <D>, <F>, <R>, and <X>.
    !> Port of grep.el:grep-expand-template.
    function grep_expand_template(template, regexp, files, dir, excl, more_opts) result(command)
        character(len=*), intent(in) :: template
        character(len=*), intent(in), optional :: regexp, files, dir, excl
        character(len=*), intent(in), optional :: more_opts(:)
        character(len=:), allocatable :: command
        character(len=:), allocatable :: opts, re
        integer :: i

        re = ""
        if (present(regexp)) re = regexp
        ! opts: MORE_OPTS, then "-i" and the color option pushed in front.
        opts = ""

        if (present(more_opts)) then
            do i = 1, size(more_opts)
                opts = opts//trim(more_opts(i))
                if (i < size(more_opts)) opts = opts//" "
            end do
        end if

        if (case_fold_search%value .and. isearch_no_upper_case_p(re, .true.)) call push("-i")

        if (grep_highlight_matches == GREP_ALWAYS) then
            call push("--color=always")
        else if (grep_highlight_matches == GREP_AUTO) then
            call push("--color=auto")
        end if

        command = template
        call substitute("<C>", opts)

        if (present(dir)) then
            call substitute("<D>", dir)
        else
            call substitute("<D>", ".")
        end if

        if (present(files)) then
            call substitute("<F>", files)
        else
            call substitute("<F>", "")
        end if

        call substitute("<N>", NULL_DEVICE)

        if (present(excl)) then
            call substitute("<X>", excl)
        else
            call substitute("<X>", "")
        end if

        call substitute("<R>", shell_quote_argument(re))

    contains

        subroutine push(opt)
            character(len=*), intent(in) :: opt

            if (len(opts) > 0) then
                opts = opt//" "//opts
            else
                opts = opt
            end if
        end subroutine push

        subroutine substitute(keyword, value)
            character(len=*), intent(in) :: keyword, value
            integer :: k

            k = index(command, keyword)
            if (k > 0) command = command(:k - 1)//value//command(k + len(keyword):)
        end subroutine substitute
    end function grep_expand_template

    !> Read regexp arg for interactive grep using `read_regexp'.  Port of
    !> grep.el:grep-read-regexp.
    recursive subroutine grep_read_regexp(regexp, status)
        character(len=:), allocatable, intent(out) :: regexp
        type(status_type), intent(out) :: status
        type(minibuffer_string_type) :: defaults(1)

        defaults(1)%s = grep_tag_default()
        call read_regexp("Search for", regexp, status, defaults, "grep_regexp_history")
    end subroutine grep_read_regexp

    !> The default `grep_read_files_function': the file name at point, the
    !> buffer's file name or the buffer's name.  Port of
    !> grep.el:grep-read-files--default (the functions of
    !> `file_name_at_point_functions' are Deferred).
    subroutine grep_read_files_default(name, found)
        character(len=:), allocatable, intent(out) :: name
        logical, intent(out) :: found
        type(status_type) :: st
        integer(int64) :: idx

        found = .true.

        if (allocated(buffers(cur)%filename)) then
            name = buffers(cur)%filename
            return
        end if

        name = buffers(cur)%name
        idx = string_match("<[0-9]+>\'", name, st)
        if (idx > 0) name = name(:idx - 1)
    end subroutine grep_read_files_default

    !> Read a file-name pattern arg for interactive grep.
    !> The pattern can include shell wildcards.  As SPC can triggers
    !> completion when entering a pattern, including it requires quoting,
    !> e.g. `\[quoted_insert]<space>'.
    !>
    !> REGEXP is used as a string in the prompt.  Port of
    !> grep.el:grep-read-files.
    recursive subroutine grep_read_files(regexp, files, status)
        character(len=*), intent(in) :: regexp
        character(len=:), allocatable, intent(out) :: files
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: bn, fn, default_alias, default_extension, default, ext, alt, head
        type(minibuffer_string_type), allocatable :: defaults(:)
        type(status_type) :: st
        logical :: found, has_fn, has_alias, has_ext
        integer :: i, j
        integer(int64) :: start, e

        if (associated(grep_read_files_function)) then
            call grep_read_files_function(bn, found)
        else
            call grep_read_files_default(bn, found)
        end if

        has_fn = found
        if (has_fn) fn = file_name_nondirectory(bn)
        has_alias = .false.

        if (has_fn) then
            do i = 1, size(grep_files_aliases)
                if (trim(grep_files_aliases(i)%name) == "all") cycle
                ! (mapconcat #'wildcard-to-regexp (split-string ...) "\\|").
                alt = ""
                start = 1

                do
                    call next_word(trim(grep_files_aliases(i)%files), start, j, e)
                    if (j == 0) exit
                    if (len(alt) > 0) alt = alt//"\|"
                    alt = alt//wildcard_to_regexp(grep_files_aliases(i)%files(j:e))
                    start = e + 1
                end do

                if (string_match(alt, fn, st) /= 0) then
                    default_alias = trim(grep_files_aliases(i)%files)
                    has_alias = .true.
                    exit
                end if
            end do
        end if

        has_ext = .false.

        if (has_fn) then
            call file_name_extension(fn, ext, found)

            if (found) then
                default_extension = "*."//ext
                has_ext = .true.
            end if
        end if

        if (has_alias) then
            default = default_alias
        else if (has_ext) then
            default = default_extension
        else
            call history_first("grep_files_history", head, found)

            if (found) then
                default = head
            else
                default = trim(grep_files_aliases(1)%name)
            end if
        end if

        ! (delete-dups (delq nil (append (list default default-alias
        ! default-extension) (mapcar #'car grep-files-aliases)))).
        allocate (defaults(0))
        call add_default(default)
        if (has_alias) call add_default(default_alias)
        if (has_ext) call add_default(default_extension)

        do i = 1, size(grep_files_aliases)
            call add_default(trim(grep_files_aliases(i)%name))
        end do

        call move_alloc(defaults, read_files_defaults)
        call completing_read(format_prompt("Search for """//regexp//""" in files matching wildcard", default), &
                             completion_function_table(merged_files_table), files, status, &
                             history="grep_files_history", defaults=read_files_defaults)
        if (.not. status%ok()) return

        do i = 1, size(grep_files_aliases)
            if (trim(grep_files_aliases(i)%name) == files) then
                files = trim(grep_files_aliases(i)%files)
                return
            end if
        end do

    contains

        subroutine add_default(s)
            character(len=*), intent(in) :: s
            type(minibuffer_string_type), allocatable :: grown(:)
            integer :: k

            do k = 1, size(defaults)
                if (defaults(k)%s == s .and. len(defaults(k)%s) == len(s)) return
            end do

            allocate (grown(size(defaults) + 1))

            do k = 1, size(defaults)
                grown(k) = defaults(k)
            end do

            grown(size(grown))%s = s
            call move_alloc(grown, defaults)
        end subroutine add_default
    end subroutine grep_read_files

    !> The next word of S (split-string on whitespace) from START: B to E,
    !> B 0 for none.  Not a port.
    subroutine next_word(s, start, b, e)
        character(len=*), intent(in) :: s
        integer(int64), intent(in) :: start
        integer, intent(out) :: b
        integer(int64), intent(out) :: e
        integer :: i

        b = 0
        e = 0

        do i = int(start), len(s)
            if (s(i:i) /= " " .and. s(i:i) /= TB .and. s(i:i) /= NL) then
                b = i
                exit
            end if
        end do

        if (b == 0) return
        e = len(s)

        do i = b, len(s)
            if (s(i:i) == " " .or. s(i:i) == TB .or. s(i:i) == NL) then
                e = i - 1
                exit
            end if
        end do
    end subroutine next_word

    !> The completion table of `grep_read_files': (completion-table-merge
    !> defaults #'completion-file-name-table).  Port of
    !> minibuffer.el:completion-table-merge for those two tables.
    recursive subroutine merged_files_table(string, action, result, status, pred, suffix)
        character(len=*), intent(in) :: string
        integer, intent(in) :: action
        type(value_type), intent(out) :: result
        type(status_type), intent(out) :: status
        procedure(completion_predicate), optional :: pred
        character(len=*), intent(in), optional :: suffix
        type(completion_table_type) :: list_table
        type(value_type) :: r1, r2
        type(value_type), allocatable :: e1(:), e2(:), vals(:), all(:)
        integer :: i, n

        status = status_type()
        allocate (vals(size(read_files_defaults)))

        do i = 1, size(read_files_defaults)
            vals(i) = make_string(read_files_defaults(i)%s)
        end do

        list_table = completion_list_table(make_list(vals))

        select case (action)
        case (COMPLETE_TRY)
            call try_completion(string, list_table, r1, status, pred)
            if (.not. status%ok()) return
            call completion_file_name_table(string, COMPLETE_TRY, r2, status, pred)
            if (.not. status%ok()) return

            if (is_string(r1) .or. is_string(r2)) then
                result = make_string(string)
                return
            end if

            ! (try-completion string (mapcar (lambda (value) (if (eq value
            ! t) string value)) (delq nil retvals)) pred).
            allocate (all(0))
            call add_value(r1)
            call add_value(r2)
            call try_completion(string, completion_list_table(make_list(all)), result, status, pred)
        case (COMPLETE_ALL)
            call all_completions(string, list_table, r1, status, pred)
            if (.not. status%ok()) return
            call completion_file_name_table(string, COMPLETE_ALL, r2, status, pred)
            if (.not. status%ok()) return
            call list_elements(r1, e1)
            call list_elements(r2, e2)
            n = size(e1) + size(e2)
            allocate (all(n))
            all(:size(e1)) = e1
            all(size(e1) + 1:) = e2
            result = make_list(all)
        case default
            ! (seq-some (lambda (table) (complete-with-action action table
            ! string pred)) tables).
            result = nil()

            if (action == COMPLETE_TEST) then
                if (test_completion(string, list_table, pred)) result = t_value()
            end if

            if (.not. status%ok()) return
            if (nilp(result)) call completion_file_name_table(string, action, result, status, pred, suffix)
        end select

    contains

        logical function is_string(v)
            type(value_type), intent(in) :: v

            is_string = .false.
            if (stringp(v)) is_string = xstring(v) == string .and. len(xstring(v)) == len(string)
        end function is_string

        subroutine add_value(v)
            type(value_type), intent(in) :: v
            type(value_type), allocatable :: grown(:)

            if (nilp(v)) return
            allocate (grown(size(all) + 1))
            grown(:size(all)) = all

            if (stringp(v)) then
                grown(size(grown)) = v
            else
                grown(size(grown)) = make_string(string)
            end if

            call move_alloc(grown, all)
        end subroutine add_value
    end subroutine merged_files_table

    !> Include elements of LIST which are applicable to DIR.  Port of
    !> grep.el:grep--filter-list-by-dir.
    function filter_list_by_dir(list, dir) result(names)
        type(grep_ignore_type), intent(in) :: list(:)
        character(len=*), intent(in) :: dir
        character(len=64), allocatable :: names(:)
        logical :: keep(size(list))
        integer :: i

        do i = 1, size(list)
            keep(i) = .true.
            if (associated(list(i)%applies)) keep(i) = list(i)%applies(dir)
        end do

        names = pack(list%name, keep)
    end function filter_list_by_dir

    !> Return the list of ignored files applicable to DIR.  Port of
    !> grep.el:grep-find-ignored-files.
    function grep_find_ignored_files(dir) result(names)
        character(len=*), intent(in) :: dir
        character(len=64), allocatable :: names(:)

        names = filter_list_by_dir(grep_find_ignored_files_variable, dir)
    end function grep_find_ignored_files

    !> Return the list of ignored directories applicable to DIR.  Port of
    !> grep.el:rgrep-find-ignored-directories.
    function rgrep_find_ignored_directories(dir) result(names)
        character(len=*), intent(in) :: dir
        character(len=64), allocatable :: names(:)

        names = filter_list_by_dir(grep_find_ignored_directories, dir)
    end function rgrep_find_ignored_directories

    !> Run grep, searching for REGEXP in FILES in directory DIR.
    !> The search is limited to file names matching shell pattern FILES.
    !> FILES may use abbreviations defined in `grep_files_aliases', e.g.
    !> entering `ch' is equivalent to `*.[ch]'.  As whitespace triggers
    !> completion when entering a pattern, including it requires quoting,
    !> e.g. `\[quoted_insert]<space>'.
    !>
    !> With \[universal_argument] prefix, you can edit the constructed
    !> shell command line before it is executed.
    !> With two \[universal_argument] prefixes, directly edit and run
    !> `grep_command'.
    !>
    !> Collect output in the "*grep*" buffer.  While grep runs
    !> asynchronously, you can use \[next_error] (M-x next_error), or
    !> \<grep_mode_map>\[compile_goto_error] in the grep output buffer, to
    !> go to the lines where grep found matches.
    !>
    !> This command shares argument histories with \[rgrep] and \[grep].
    !>
    !> If CONFIRM is true, the user will be given an opportunity to edit
    !> the command before it's run.  Port of grep.el:lgrep.
    recursive subroutine lgrep(regexp, status, files, dir, confirm)
        character(len=*), intent(in) :: regexp
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: files, dir
        logical, intent(in), optional :: confirm
        character(len=:), allocatable :: command, d, edited, excl, saved_dir
        character(len=64), allocatable :: ignores(:)
        character(len=19), allocatable :: more(:)
        logical :: conf, have_command, had_dir
        integer :: i
        type(handle_type) :: old

        status = status_type()
        conf = .false.
        if (present(confirm)) conf = confirm
        if (len(regexp) == 0) return

        if (.not. present(dir)) then
            call set_error(status, FM_USER_ERROR, "Unable to open directory: nil")
            return
        end if

        if (.not. file_accessible_directory_p(dir)) then
            call set_error(status, FM_USER_ERROR, "Unable to open directory: "//dir)
            return
        end if

        command = regexp
        have_command = .true.

        if (.not. present(files)) then
            if (allocated(grep_command)) then
                if (command == grep_command .and. len(command) == len(grep_command)) have_command = .false.
            end if
        else
            call expand_file_name(dir, d, status)
            if (.not. status%ok()) return
            d = file_name_as_directory(d)

            if (grep_use_directories_skip /= GREP_NIL .and. grep_use_directories_skip /= GREP_T) then
                grep_use_directories_skip = merge(GREP_T, GREP_NIL, &
                                                  grep_probe(grep_program, strings([character(len=18) :: &
                                                                                    "--directories=skip", "foo"], NULL_DEVICE), &
                                                             .false., 1))
            end if

            ignores = grep_find_ignored_files(d)
            excl = ""

            if (size(ignores) > 0) then
                excl = " --exclude="

                do i = 1, size(ignores)
                    if (i > 1) excl = excl//" --exclude="
                    excl = excl//shell_quote_argument(trim(ignores(i)))
                end do
            end if

            if (grep_use_directories_skip == GREP_T) then
                more = [character(len=19) :: "--directories=skip"]
            else
                allocate (more(0))
            end if

            command = grep_expand_template(setting(grep_template), regexp, files, excl=excl, more_opts=more)

            if (conf) then
                call read_from_minibuffer("Confirm: ", edited, status, initial=command, history="grep_history")
                if (.not. status%ok()) return
                command = edited
            else
                call add_to_history("grep_history", command)
            end if
        end if

        if (.not. have_command) return
        old = current_buffer()
        had_dir = allocated(buffers(cur)%directory)
        if (had_dir) saved_dir = buffers(cur)%directory
        buffers(cur)%directory = file_name_as_directory(dir)
        if (present(files)) buffers(cur)%directory = d
        ! Setting process-setup-function makes exit-message-function work
        ! even when async processes aren't supported.
        call grep_save_buffers_now(status)

        if (status%ok()) then
            if (grep_use_null_device == GREP_T) then
                call compilation_start(command//" "//NULL_DEVICE, status, mode=major_modes%grep_mode)
            else
                call compilation_start(command, status, mode=major_modes%grep_mode)
            end if
        end if

        call restore_directory(old, had_dir, saved_dir)

        ! Set default-directory if we started lgrep in the *grep* buffer.
        if (status%ok() .and. same_buffer(next_error_last_buffer, current_buffer())) then
            if (present(files)) then
                buffers(cur)%directory = d
            else
                buffers(cur)%directory = file_name_as_directory(dir)
            end if
        end if
    end subroutine lgrep

    !> Whether A and B are the same buffer.  Not a port.
    logical function same_buffer(a, b)
        type(handle_type), intent(in) :: a, b

        same_buffer = a%index == b%index .and. a%generation == b%generation
    end function same_buffer

    !> Give OLD its directory back (the end of a let binding of
    !> `default_directory').  Not a port.
    subroutine restore_directory(old, had_dir, saved_dir)
        type(handle_type), intent(in) :: old
        logical, intent(in) :: had_dir
        character(len=:), allocatable, intent(in) :: saved_dir

        if (.not. buffer_live_p(old)) return

        if (had_dir) then
            buffers(old%index)%directory = saved_dir
        else if (allocated(buffers(old%index)%directory)) then
            deallocate (buffers(old%index)%directory)
        end if
    end subroutine restore_directory

    !> S, or "" for nil.  Not a port.
    function setting(s) result(v)
        character(len=:), allocatable, intent(in) :: s
        character(len=:), allocatable :: v

        v = ""
        if (allocated(s)) v = s
    end function setting

    !> The arguments of `lgrep', `rgrep' and `zrgrep' read interactively:
    !> with C-u C-u the command (RUN, from COMMAND and HISTORY), else the
    !> REGEXP, FILES, DIR (prompted with DIR_PROMPT) and CONFIRM (C-u).
    !> Port of grep.el:lgrep, grep.el:rgrep and grep.el:zrgrep, in part
    !> (their interactive specs).
    recursive subroutine read_grep_args(command, history, dir_prompt, run, regexp, files, dir, confirm, status)
        character(len=:), allocatable, intent(in) :: command
        character(len=*), intent(in) :: history, dir_prompt
        logical, intent(out) :: run
        character(len=:), allocatable, intent(out) :: regexp, files, dir
        logical, intent(out) :: confirm
        type(status_type), intent(out) :: status
        integer :: n

        run = .false.
        confirm = .false.
        n = 0
        if (consp(current_prefix_arg)) n = prefix_numeric_value(current_prefix_arg)

        if (allocated(command) .and. n == 16) then
            run = .true.
            call read_from_minibuffer("Run: ", regexp, status, initial=command, history=history)
            return
        end if

        call grep_read_regexp(regexp, status)
        if (.not. status%ok()) return
        call grep_read_files(regexp, files, status)
        if (.not. status%ok()) return
        call read_directory_name(dir_prompt, dir, status, mustmatch=1)
        if (.not. status%ok()) return

        if (len(dir) == 0) then
            if (allocated(buffers(cur)%directory)) dir = buffers(cur)%directory
        end if

        confirm = n == 4
    end subroutine read_grep_args

    !> The command: read the arguments, then `lgrep'.  The interactive form
    !> of grep.el:lgrep.
    !>
    !> User-friendly grep in a directory.
    recursive subroutine cmd_lgrep(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: regexp, files, dir
        logical :: run, confirm

        call grep_compute_defaults(status)
        if (.not. status%ok()) return

        if (.not. allocated(grep_template) .and. .not. (allocated(grep_command) .and. &
                                                        prefix_numeric_value(current_prefix_arg) == 16)) then
            call set_error(status, FM_GENERIC_ERROR, "grep.el: No `grep_template' available")
            return
        end if

        call read_grep_args(grep_command, "grep_history", "In directory: ", run, regexp, files, dir, confirm, status)
        if (.not. status%ok()) return

        if (run) then
            call lgrep(regexp, status, dir=buffer_dir())
        else
            call lgrep(regexp, status, files, dir, confirm)
        end if
    end subroutine cmd_lgrep

    !> The current buffer's `default_directory'.  Not a port.
    function buffer_dir() result(dir)
        character(len=:), allocatable :: dir

        dir = "./"
        if (allocated(buffers(cur)%directory)) dir = buffers(cur)%directory
    end function buffer_dir

    !> Recursively grep for REGEXP in FILES in directory tree rooted at
    !> DIR.  The search is limited to file names matching shell pattern
    !> FILES.  FILES may use abbreviations defined in
    !> `grep_files_aliases', e.g. entering `ch' is equivalent to `*.[ch]'.
    !> As whitespace triggers completion when entering a pattern, including
    !> it requires quoting, e.g. `\[quoted_insert]<space>'.
    !>
    !> With \[universal_argument] prefix, you can edit the constructed
    !> shell command line before it is executed.
    !> With two \[universal_argument] prefixes, directly edit and run
    !> `grep_find_command'.
    !>
    !> Collect output in the "*grep*" buffer.  While the recursive grep is
    !> running, you can use \[next_error] (M-x next_error), or
    !> \<grep_mode_map>\[compile_goto_error] in the grep output buffer, to
    !> visit the lines where matches were found.  To kill the job before
    !> it finishes, type \[kill_compilation].
    !>
    !> This command shares argument histories with \[lgrep] and
    !> \[grep_find].
    !>
    !> When called programmatically and FILES is absent, REGEXP is expected
    !> to specify a command to run.
    !>
    !> If CONFIRM is true, the user will be given an opportunity to edit
    !> the command before it's run.  Port of grep.el:rgrep.
    recursive subroutine rgrep(regexp, status, files, dir, confirm)
        character(len=*), intent(in) :: regexp
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: files, dir
        logical, intent(in), optional :: confirm
        character(len=:), allocatable :: command, d, edited, saved_dir
        logical :: conf, had_dir
        type(handle_type) :: old

        status = status_type()
        conf = .false.
        if (present(confirm)) conf = confirm

        ! If called non-interactively, also compute the defaults if we
        ! haven't already.
        if (.not. allocated(grep_find_template)) then
            call grep_compute_defaults(status)
            if (.not. status%ok()) return
        end if

        if (len(regexp) == 0) return

        if (.not. present(dir)) then
            call set_error(status, FM_USER_ERROR, "Unable to open directory: nil")
            return
        end if

        if (.not. file_accessible_directory_p(dir)) then
            call set_error(status, FM_USER_ERROR, "Unable to open directory: "//dir)
            return
        end if

        if (.not. present(files)) then
            if (allocated(grep_find_command)) then
                if (regexp == grep_find_command .and. len(regexp) == len(grep_find_command)) return
            end if

            call compilation_start(regexp, status, mode=major_modes%grep_mode)
            return
        end if

        call expand_file_name(dir, d, status)
        if (.not. status%ok()) return
        d = file_name_as_directory(d)
        ! (case-fold-search (read-regexp-case-fold-search regexp)).
        command = rgrep_default_command(regexp, files, d)

        if (conf) then
            call read_from_minibuffer("Confirm: ", edited, status, initial=command, history="grep_find_history")
            if (.not. status%ok()) return
            command = edited
        else
            call add_to_history("grep_find_history", command)
        end if

        call grep_save_buffers_now(status)
        if (.not. status%ok()) return
        old = current_buffer()
        had_dir = allocated(buffers(cur)%directory)
        if (had_dir) saved_dir = buffers(cur)%directory
        buffers(cur)%directory = d
        call compilation_start(command, status, mode=major_modes%grep_mode)
        call restore_directory(old, had_dir, saved_dir)
        ! Set default-directory if we started rgrep in the *grep* buffer.
        if (status%ok() .and. same_buffer(next_error_last_buffer, current_buffer())) buffers(cur)%directory = d
    end subroutine rgrep

    !> The command: read the arguments, then `rgrep'.  The interactive form
    !> of grep.el:rgrep.
    !>
    !> User-friendly recursive grep in directory tree.
    recursive subroutine cmd_rgrep(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: regexp, files, dir
        logical :: run, confirm

        call grep_compute_defaults(status)
        if (.not. status%ok()) return

        if (.not. allocated(grep_find_template) .and. .not. (allocated(grep_find_command) .and. &
                                                             prefix_numeric_value(current_prefix_arg) == 16)) then
            call set_error(status, FM_GENERIC_ERROR, "grep.el: No `grep_find_template' available")
            return
        end if

        call read_grep_args(grep_find_command, "grep_find_history", "Base directory: ", run, regexp, files, dir, &
                            confirm, status)
        if (.not. status%ok()) return

        if (run) then
            call rgrep(regexp, status, dir=buffer_dir())
        else
            call rgrep(regexp, status, files, dir, confirm)
        end if
    end subroutine cmd_rgrep

    !> Compute the command for \[rgrep] to use by default.  Port of
    !> grep.el:rgrep-default-command.
    function rgrep_default_command(regexp, files, dir) result(command)
        character(len=*), intent(in) :: regexp, files
        character(len=*), intent(in), optional :: dir
        character(len=:), allocatable :: command
        character(len=64), allocatable :: ignored_files(:), ignored_dirs(:)
        character(len=:), allocatable :: ignored_files_arg, f, x, d
        integer(int64) :: start, e
        integer :: i, b

        d = ""
        if (present(dir)) d = dir
        ignored_files = grep_find_ignored_files(d)
        ignored_files_arg = ""

        if (size(ignored_files) > 0) then
            ignored_files_arg = shell_quote_argument("(")//" -name "

            do i = 1, size(ignored_files)
                if (i > 1) ignored_files_arg = ignored_files_arg//" -o -name "
                ignored_files_arg = ignored_files_arg//shell_quote_argument(trim(ignored_files(i)))
            end do

            ignored_files_arg = ignored_files_arg//" "//shell_quote_argument(")")
        end if

        f = shell_quote_argument("(")//" "//find_name_arg//" "
        start = 1
        i = 0

        do
            call next_word(files, start, b, e)
            if (b == 0) exit
            if (i > 0) f = f//" -o "//find_name_arg//" "
            f = f//shell_quote_argument(files(b:e))
            i = i + 1
            start = e + 1
        end do

        f = f//" "//shell_quote_argument(")")

        if (rgrep_find_ignores_in_f .and. len(ignored_files_arg) > 0) then
            f = f//" "//shell_quote_argument("!")//" "//ignored_files_arg
        end if

        x = ""
        ignored_dirs = rgrep_find_ignored_directories(d)

        if (size(ignored_dirs) > 0) then
            x = "-type d "//shell_quote_argument("(")//" -path "

            do i = 1, size(ignored_dirs)
                if (i > 1) x = x//" -o -path "
                x = x//shell_quote_argument("*/"//trim(ignored_dirs(i)))
            end do

            x = x//" "//shell_quote_argument(")")//" -prune -o "
        end if

        if (.not. rgrep_find_ignores_in_f .and. len(ignored_files_arg) > 0) then
            x = x//shell_quote_argument("!")//" -type d "//ignored_files_arg//" -prune -o "
        end if

        if (present(dir)) then
            command = grep_expand_template(setting(grep_find_template), regexp, f, dir, x)
        else
            command = grep_expand_template(setting(grep_find_template), regexp, f, excl=x)
        end if
    end function rgrep_default_command

    !> Toggle showing the hidden part of rgrep/lgrep/zrgrep command line.
    !> Port of grep.el:grep-find-toggle-abbreviation.
    recursive subroutine grep_find_toggle_abbreviation(status)
        type(status_type), intent(out) :: status
        type(silent_modifications_type) :: silent
        type(value_type) :: display
        integer(int64) :: beg, end
        logical :: found, changed
        type(status_type) :: st

        status = status_type()
        call silent_modifications_begin(silent)
        call next_single_property_change(point_min(), sym("abbreviated_command"), beg, found, st)
        end = 0
        if (found) call next_single_property_change(beg, sym("abbreviated_command"), end, found, st)

        if (found .and. end > 0) then
            call get_text_property(beg, sym("display"), display, st)

            if (.not. nilp(display)) then
                call remove_list_of_text_properties(beg, end, list(sym("display"), sym("help_echo"), sym("mouse_face"), &
                                                                   sym("help_echo"), sym("keymap")), changed, st)
            else
                call add_text_properties(beg, end, grep_find_abbreviate_properties(), changed, st)
            end if
        else
            call set_error(status, FM_USER_ERROR, "No abbreviated part to hide/show")
        end if

        call silent_modifications_end(silent)
    end subroutine grep_find_toggle_abbreviation

    !> Recursively grep for REGEXP in gzipped FILES in tree rooted at DIR.
    !> Like `rgrep' but uses `zgrep' for `grep_program', sets the default
    !> file name to `*.gz', and sets `grep_highlight_matches' to
    !> GREP_ALWAYS.
    !>
    !> If CONFIRM is true, the user will be given an opportunity to edit
    !> the command before it's run.  Port of grep.el:zrgrep.
    recursive subroutine zrgrep(regexp, status, files, dir, confirm, template)
        character(len=*), intent(in) :: regexp
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: files, dir, template
        logical, intent(in), optional :: confirm
        character(len=:), allocatable :: saved_template
        logical :: had_template
        integer :: saved_highlight

        had_template = allocated(grep_find_template)
        if (had_template) saved_template = grep_find_template
        saved_highlight = grep_highlight_matches

        if (present(template)) then
            grep_find_template = template
        else if (allocated(grep_find_template)) then
            deallocate (grep_find_template)
        end if

        ! Set `grep_highlight_matches' to always since `zgrep' puts filters
        ! in the grep output.
        grep_highlight_matches = GREP_ALWAYS
        call rgrep(regexp, status, files, dir, confirm)
        grep_highlight_matches = saved_highlight

        if (had_template) then
            grep_find_template = saved_template
        else if (allocated(grep_find_template)) then
            deallocate (grep_find_template)
        end if
    end subroutine zrgrep

    !> The command: compute the zgrep defaults, read the arguments, then
    !> `zrgrep'.  The interactive form of grep.el:zrgrep.
    !>
    !> Recursively grep for a regexp in gzipped files.
    recursive subroutine cmd_zrgrep(status)
        type(status_type), intent(out) :: status
        type(grep_defaults_type) :: saved
        type(grep_host_defaults_type) :: saved_alist
        type(grep_files_alias_type), allocatable :: saved_aliases(:)
        character(len=:), allocatable :: saved_program, regexp, files, dir, template
        logical :: run, confirm, have_template

        ! Compute standard default values.
        call grep_compute_defaults(status)
        if (.not. status%ok()) return
        ! Compute the default zrgrep command by running
        ! `grep_compute_defaults' for grep program "zgrep", but not
        ! changing global values.
        saved = current_defaults()
        saved_alist = grep_host_defaults_alist
        saved_program = grep_program
        call move_alloc(grep_files_aliases, saved_aliases)
        grep_program = "zgrep"
        if (allocated(grep_find_template)) deallocate (grep_find_template)
        if (allocated(grep_find_command)) deallocate (grep_find_command)
        grep_host_defaults_alist = grep_host_defaults_type()
        ! `zgrep' doesn't support the `--null' option.
        grep_use_null_filename_separator = GREP_NIL
        ! Use for `grep_read_files'.
        grep_files_aliases = [grep_files_alias_type(name="all", files="* .*"), &
                              grep_files_alias_type(name="gz", files="*.gz")]
        ! Recompute defaults using let-bound values above.
        call grep_compute_defaults(status)
        have_template = allocated(grep_find_template)
        if (have_template) template = grep_find_template

        if (status%ok()) then
            if (.not. have_template .and. .not. (allocated(grep_find_command) .and. &
                                                 prefix_numeric_value(current_prefix_arg) == 16)) then
                call set_error(status, FM_GENERIC_ERROR, "grep.el: No `grep_find_template' available")
            else
                call read_grep_args(grep_find_command, "grep_find_history", "Base directory: ", run, regexp, files, &
                                    dir, confirm, status)
            end if
        end if

        ! The let bindings end.
        grep_program = saved_program
        call move_alloc(saved_aliases, grep_files_aliases)
        grep_host_defaults_alist = saved_alist
        call set_defaults(saved)
        if (.not. status%ok()) return

        if (run) then
            call zrgrep(regexp, status, dir=buffer_dir())
        else if (have_template) then
            call zrgrep(regexp, status, files, dir, confirm, template)
        else
            call zrgrep(regexp, status, files, dir, confirm)
        end if
    end subroutine cmd_zrgrep

    !> Return the name of the file at POINT a `grep_mode' buffer.
    !> The returned file name is relative.  Port of
    !> grep.el:grep-file-at-point.
    subroutine grep_file_at_point(point, file, found)
        integer(int64), intent(in) :: point
        character(len=:), allocatable, intent(out) :: file
        logical, intent(out) :: found
        type(compilation_message_info_type) :: info

        call compilation_message_info(point, info, found)
        if (found) found = info%has_loc .and. allocated(info%file)
        if (found) file = info%file
    end subroutine grep_file_at_point

    ! ------------------------------------------------------------------
    ! Definitions
    ! ------------------------------------------------------------------

    !> Give the variables their defaults, define Grep mode and register
    !> the commands.  Not a port.
    subroutine syms_of_grep()
        type(status_type) :: st
        integer :: i, n

        if (associated(major_modes%grep_mode%run)) return
        if (.not. associated(major_modes%compilation_mode%run)) call syms_of_compile()
        grep_match_regexp = ESC//"\[\(?:0?1;\)?31m\(.*?\)"//ESC//"\[[0-9]*m"
        grep_match_face = "match"
        grep_error = "grep hit"
        ! Reverse the colors because grep hits are not errors (though we
        ! jump there with `next_error'), and unreadable files can't be gone
        ! to.
        grep_hit_face = "compilation_info"
        if (allocated(compilation_info_face%value)) grep_hit_face = compilation_info_face%value
        grep_error_face = "compilation_error"
        grep_context_face = "shadow"
        grep_heading_regexp = "^\(?:\(?2:\(?1:[^"//NUL//NL//"]+\)"//NUL//"\)\|\(?2:\(?1:.+?\)[:=-]\)\)[0-9]+[:=-]"
        if (.not. allocated(grep_program)) grep_program = "grep"
        if (.not. allocated(find_program)) find_program = "find"
        if (.not. allocated(xargs_program)) xargs_program = "xargs"

        if (.not. allocated(find_name_arg)) then
            find_name_arg = "-name"
            if (read_file_name_completion_ignore_case) find_name_arg = "-iname"
        end if

        if (.not. allocated(grep_files_aliases)) then
            grep_files_aliases = [grep_files_alias_type(name="all", files="* .*"), &
                                  grep_files_alias_type(name="el", files="*.el"), &
                                  grep_files_alias_type(name="ch", files="*.[ch]"), &
                                  grep_files_alias_type(name="c", files="*.c"), &
                                  grep_files_alias_type(name="cc", files="*.cc *.cxx *.cpp *.C *.CC *.c++"), &
                                  grep_files_alias_type(name="cchh", files="*.cc *.[ch]xx *.[ch]pp *.[CHh] *.CC *.HH *.[ch]++"), &
                                  grep_files_alias_type(name="hh", files="*.hxx *.hpp *.[Hh] *.HH *.h++"), &
                                  grep_files_alias_type(name="h", files="*.h"), &
                                  grep_files_alias_type(name="l", files="[Cc]hange[Ll]og*"), &
                                  grep_files_alias_type(name="am", files="Makefile.am GNUmakefile *.mk"), &
                                  grep_files_alias_type(name="m", files="[Mm]akefile*"), &
                                  grep_files_alias_type(name="tex", files="*.tex"), &
                                  grep_files_alias_type(name="texi", files="*.texi"), &
                                  grep_files_alias_type(name="asm", files="*.[sS]")]
        end if

        if (.not. allocated(grep_find_ignored_directories)) then
            if (.not. allocated(vc_directory_exclusion_list)) then
                allocate (grep_find_ignored_directories(0))
            else
                allocate (grep_find_ignored_directories(size(vc_directory_exclusion_list)))

                do i = 1, size(vc_directory_exclusion_list)
                    grep_find_ignored_directories(i)%name = trim(vc_directory_exclusion_list(i))
                end do
            end if
        end if

        if (.not. allocated(grep_find_ignored_files_variable)) then
            ! (cons ".#*" (delq nil (mapcar (lambda (s) (unless
            ! (string-match-p "/\\'" s) (concat "*" s)))
            ! completion-ignored-extensions))).
            n = 1

            do i = 1, size(completion_ignored_extensions)
                if (.not. ends_with_slash(completion_ignored_extensions(i))) n = n + 1
            end do

            allocate (grep_find_ignored_files_variable(n))
            grep_find_ignored_files_variable(1)%name = ".#*"
            n = 1

            do i = 1, size(completion_ignored_extensions)
                if (ends_with_slash(completion_ignored_extensions(i))) cycle
                n = n + 1
                grep_find_ignored_files_variable(n)%name = "*"//trim(completion_ignored_extensions(i))
            end do
        end if

        allocate (grep_compilation_transform_finished_rules(2))
        grep_compilation_transform_finished_rules(1)%regexp = "^Grep[/a-zA-Z]* finished with \(?:\(\(?:[0-9]+ \)?" &
                                                              //"match\(?:es\)? found\)\|\(no matches found\)\).*"
        grep_compilation_transform_finished_rules(2)%regexp = "^Grep[/a-zA-Z]* \(exited abnormally\|interrupt\|" &
                                                              //"killed\|terminated\)\(?:.*with code \([0-9]+\)\)?.*"
        call init_grep_regexp_alist()
        call init_grep_mode_font_lock_keywords()
        call grep_heading%define("grep_heading", "Face of headings when `grep_use_headings' is true.")
        call grep_heading%spec_entry(display_when(), face_attrs(inherit=grep_hit_face))
        call major_modes%grep_mode%define("grep_mode", "Grep", grep_mode, grep_mode_hook, &
                                          parent=major_modes%compilation_mode)
        grep_mode_map = major_modes%grep_mode%map
        call set_keymap_parent(grep_mode_map, compilation_minor_mode_map, st)
        call keymap_set(grep_mode_map, "SPC", "scroll_up_command", st)
        call keymap_set(grep_mode_map, "S-SPC", "scroll_down_command", st)
        call keymap_set(grep_mode_map, "DEL", "scroll_down_command", st)
        call keymap_set(grep_mode_map, "RET", "compile_goto_error", st)
        call keymap_set(grep_mode_map, "{", "compilation_previous_file", st)
        call keymap_set(grep_mode_map, "}", "compilation_next_file", st)
        call keymap_set(grep_mode_map, "TAB", "compilation_next_error", st)
        call keymap_set(grep_mode_map, "<backtab>", "compilation_previous_error", st)
        call keymap_set(grep_mode_map, "e", "grep_change_to_grep_edit_mode", st)
        call make_sparse_keymap(grep_edit_mode_map)
        call set_keymap_parent(grep_edit_mode_map, text_mode_map, st)
        call keymap_set(grep_edit_mode_map, "C-c C-c", "grep_edit_save_changes", st)
        call defvar_nil("grep_history")
        call defvar_nil("grep_find_history")
        call defvar_nil("grep_regexp_history")
        call defvar_nil("grep_files_history")
        commands%grep_mode%run => grep_mode
        commands%grep%run => cmd_grep
        commands%grep_find%run => cmd_grep_find
        commands%find_grep%run => cmd_grep_find
        commands%lgrep%run => cmd_lgrep
        commands%rgrep%run => cmd_rgrep
        commands%zrgrep%run => cmd_zrgrep
        commands%rzgrep%run => cmd_zrgrep
        commands%kill_grep%run => kill_compilation
        commands%grep_find_toggle_abbreviation%run => grep_find_toggle_abbreviation
        commands%grep_edit_mode%run => grep_edit_mode
        commands%grep_change_to_grep_edit_mode%run => grep_change_to_grep_edit_mode
        commands%grep_edit_save_changes%run => grep_edit_save_changes
        call add_local_variables_killer(kill_grep_locals)

    contains

        logical function ends_with_slash(s)
            character(len=*), intent(in) :: s
            integer :: l

            l = len_trim(s)
            ends_with_slash = .false.
            if (l > 0) ends_with_slash = s(l:l) == "/"
        end function ends_with_slash
    end subroutine syms_of_grep
end module grep_fm
