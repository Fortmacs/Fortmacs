! xref_mod.f90 -- cross-referencing commands.
! Ported from GNU Emacs 31.1 lisp/progmodes/xref.el.
!
! Copyright (C) 2014-2026 Free Software Foundation, Inc.
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

!> Xref: finding definitions and references of identifiers.  A backend
!> (`xref_backend_type', found through `xref_backend_functions', etags_mod's
!> tags tables by default) finds the items (`xref_item_type': a summary and
!> a location, `xref_location_type'); `find_xref_definitions' (M-.) goes to
!> the definition or lists the candidates in the *xref* buffer,
!> `find_xref_references' (M-?) and `find_xref_apropos' (C-M-.) list what
!> they find there, and `go_back_in_xref_history' (M-,) and `go_forward_in_xref_history' (C-M-,)
!> move through the history of the places jumped from.
!>
!> `xref_matches_in_files' and `xref_matches_in_directory' search files
!> with grep for other modules (project.el's commands), and `show_xrefs'
!> shows what an `xref_fetcher_type' fetches.
!>
!> What is not ported yet is listed in the xref_mod row of PORTING.md's
!> status table.
module xref_mod
    use, intrinsic :: iso_fortran_env, only: int64, int32
    use error_mod, only: status_type, set_error, format_error, FM_USER_ERROR, FM_GENERIC_ERROR
    use handle_mod, only: handle_type, null_handle
    use string_type_mod, only: string_type
    use value_mod, only: value_type, vtag, V_INTEGER, V_NULL, xfixnum, list, make_list
    use data_mod, only: history_setting_type, forward_setting, append_locals_clearer
    use hooks_mod, only: hook_type
    use commands_mod, only: commands
    use callint_mod, only: this_command, current_prefix_arg, PREFIX_NONE, PREFIX_NUMBER, prefix_arg_type
    use buffer_mod, only: buffers, current, current_buffer, set_buffer, get_or_make_buffer, is_buffer_live, close_buffer, &
                          erase_buffer, buffer_file_name, buffer_name, get_file_buffer, is_buffer_modified, &
                          restore_buffer_modified
    use marker_mod, only: copy_marker_at, marker_position, marker_buffer, destroy_marker, set_marker
    use editfns_mod, only: point, point_min, point_max, goto_char, line_beginning_position, line_end_position, &
                           buffer_substring, buffer_substring_no_properties, is_eob, is_bob, is_eol, point_marker, widen, &
                           narrow_to_region
    use cmds_mod, only: forward_line, forward_char
    use insdel_mod, only: insert, before_change_functions, after_change_functions
    use textprop_mod, only: put_text_property, get_text_property, add_text_properties, &
                            next_single_char_property_change, previous_single_char_property_change, get_char_property, &
                            remove_list_of_text_properties, next_single_property_change, text_property_not_all
    use search_mod, only: re_search_forward, string_match, match_beginning, match_end, regexp_quote, case_fold_search, &
                          string_index_to_offset, looking_at, SEARCH_NOERROR
    use subr_mod, only: bounds_of_identifier_at_point, shell_quote_argument, add_to_invisibility_spec, execute_mode_hooks
    use simple_mod, only: push_mark, is_region_active, next_error_last_buffer, set_next_error_function, next_error_found, &
                          widen_automatically, shell_command_switch, back_to_indentation
    use keymap_mod, only: keymap_set, set_keymap_parent, make_sparse_keymap, use_local_map
    use keyboard_mod, only: post_command_hook
    use modes_mod, only: major_modes, minor_modes
    use derived_mod, only: is_derived_mode
    use window_mod, only: windows, pop_to_buffer, display_buffer, quit_window, get_buffer_window, is_window_live, &
                          set_window_point, window_dedication, set_window_dedication, recenter, window_buffer, &
                          switch_to_buffer_other_window, display_action_parameters_type, DISPLAY_ACTION_SAME_WINDOW, &
                          DEDICATED_NONE, DEDICATED_WEAK
    use window_mod, only: select_window
    use frame_mod, only: selected_window
    use files_mod, only: find_file_noselect, revert_buffer_function, set_auto_mode
    use fileio_mod, only: expand_file_name, file_name_nondirectory, file_name_directory, is_file_name_absolute, &
                          file_name_as_directory, directory_file_name, insert_file_contents, &
                          is_visited_file_modtime_current
    use minibuf_mod, only: completion_ignore_case, read_string
    use minibuffer_mod, only: completing_read
    use minibuf_table_mod, only: completion_table_type, completion_list_table
    use replace_mod, only: format_prompt, read_regexp, occur_add_target, occur_after_change_function
    use apropos_mod, only: apropos_parse_pattern, apropos_regexp
    use grep_mod, only: grep_compute_defaults, grep_expand_template, grep_find_template, grep_highlight_matches, &
                        grep_regexps, find_name_arg, GREP_HIGHLIGHT_MATCHES_NO
    use callproc_mod, only: execute_process, execute_process_region, shell_file_name
    use xdisp_mod, only: message, mode_line_part_type, set_mode_line_process
    use faces_mod, only: named_face_type, display_when, face_attrs
    use help_mod, only: substitute_command_keys
    use syntax_propertize_mod, only: syntax_propertize, syntax_ppss_flush_cache
    implicit none(type, external)
    private

    public :: make_xref, make_xref_match, make_xref_file_location, make_xref_buffer_location, &
              make_xref_bogus_location, append_xref_item, find_xref_backend, push_xref_marker_stack, &
              clear_xref_marker_stack, is_xref_marker_stack_empty, is_xref_forward_history_empty, go_back_in_xref_history, &
              go_forward_in_xref_history, pop_to_xref_location, show_xrefs, show_xref_buffer, show_xref_definitions_buffer, &
              show_xref_definitions_completing_read, find_xref_definitions, find_xref_definitions_other_window, &
              find_xref_references, find_xref_apropos, xref_apropos_regexp, xref_references_in_directory, &
              xref_references_in_directory_grep, xref_matches_in_directory, xref_matches_in_files, &
              read_xref_identifier, xref_group_name_for_display, xref_regexp_to_extended, xref_xref_buffer_mode, &
              xref_transient_buffer_mode, xref_edit_mode, change_to_xref_edit_mode, save_xref_edit_changes, &
              next_xref_line, previous_xref_line, next_xref_line_no_show, previous_xref_line_no_show, &
              next_xref_group, previous_xref_group, goto_xref, quit_and_goto_xref, quit_and_pop_xref_marker_stack, &
              show_xref_location_at_point, xref_etags_mode, xref_items_at, init_xref

    !> The name of the buffer to show xrefs.  Port of xref.el
    !> xref-buffer-name.
    character(len=*), parameter, public :: XREF_BUFFER_NAME = "*xref*"

    !> The length of an item that is no match (an xref-item, not an
    !> xref-match-item).  Not a port.
    integer(int64), parameter, public :: XREF_MATCH_LENGTH_NONE = -1

    !> The values of `xref_file_name_display': the full absolute file name
    !> (`abs'), only the nondirectory part (`nondirectory'), or relative to
    !> the current project's root (`project-relative').
    integer, parameter, public :: XREF_FILE_NAME_DISPLAY_ABSOLUTE = 0, XREF_FILE_NAME_DISPLAY_NONDIRECTORY = 1, &
                                  XREF_FILE_NAME_DISPLAY_PROJECT_RELATIVE = 2

    !> The values of `xref_prompt_for_identifier': always prompt, prompt
    !> only when there is no identifier at point, prompt in the commands of
    !> `xref_prompt_for_identifier_commands' (upstream's list of commands),
    !> or in all the commands but those (upstream's list starting with
    !> `not').
    integer, parameter, public :: XREF_PROMPT_FOR_IDENTIFIER_YES = 0, XREF_PROMPT_FOR_IDENTIFIER_NO = 1, &
                                  XREF_PROMPT_FOR_IDENTIFIER_LISTED = 2, XREF_PROMPT_FOR_IDENTIFIER_EXCEPT_LISTED = 3

    !> The values of `xref_auto_jump_to_first_definition' and
    !> `xref_auto_jump_to_first_xref': no auto-jump, jump, show the first
    !> result's location but keep the focus on the Xref buffer's window, or
    !> only move point to the first result.
    integer, parameter, public :: XREF_AUTO_JUMP_NO = 0, XREF_AUTO_JUMP_YES = 1, XREF_AUTO_JUMP_SHOW = 2, &
                                  XREF_AUTO_JUMP_MOVE = 3

    !> Where a location is displayed (the DISPLAY_ACTION of
    !> `xref_show_xrefs_function'): nothing chosen (the selected window), or
    !> the other window.  (Upstream's `frame' is not ported.)
    integer, parameter, public :: XREF_DISPLAY_ACTION_NONE = 0, XREF_DISPLAY_ACTION_WINDOW = 1

    !> The value of `xref_truncation_width' that disables truncation.
    integer, parameter, public :: XREF_TRUNCATION_WIDTH_NONE = 0

    !> The search programs of `xref_search_programs'.
    integer, parameter, public :: XREF_SEARCH_PROGRAM_GREP = 1, XREF_SEARCH_PROGRAM_RIPGREP = 2, &
                                  XREF_SEARCH_PROGRAM_UGREP = 3

    ! What a fetcher of `xref_backend_fetcher_type' asks the backend for.
    integer, parameter :: KIND_DEFINITIONS = 1, KIND_REFERENCES = 2, KIND_APROPOS = 3

    ! How `show_location' selects the window (its SELECT): not at all, select
    ! it, or quit the *xref* window first.
    integer, parameter :: SELECT_NO = 0, SELECT_YES = 1, SELECT_QUIT = 2

    ! The text properties of the *xref* buffer.
    character(len=*), parameter :: P_XREF_ITEM = "xref_item", P_XREF_GROUP = "xref_group", P_FACE = "face", &
                                   P_MOUSE_FACE = "mouse_face", P_HELP_ECHO = "help_echo", P_INVISIBLE = "invisible", &
                                   P_READ_ONLY = "read_only", P_FRONT_STICKY = "front_sticky", &
                                   P_REAR_NONSTICKY = "rear_nonsticky", P_OCCUR_PREFIX = "occur_prefix", &
                                   P_OCCUR_TARGET = "occur_target"
    ! Faces and other names the *xref* buffer's properties take as values.
    character(len=*), parameter :: FACE_XREF_FILE_HEADER = "xref_file_header", FACE_XREF_LINE_NUMBER = "xref_line_number", &
                                   FACE_XREF_MATCH = "xref_match", FACE_SHADOW = "shadow", FACE_HIGHLIGHT = "highlight", &
                                   FACE_ERROR = "error", INVISIBLE_ELLIPSIS = "ellipsis"

    character(len=*), parameter :: LF = achar(10), NUL = achar(0), CR = achar(13)

    !> A location of an xref item: in a file, a buffer, or nowhere (a
    !> message).  Backends extend it (etags_mod's tag locations).  Port of
    !> xref.el's location generics (`xref-location-marker',
    !> `xref-location-group', `xref-location-line').
    type, abstract, public :: xref_location_type
    contains
        !> A marker at the location (the caller destroys it).  Port of
        !> xref.el:xref-location-marker.
        procedure(xref_location_marker_procedure), deferred :: marker
        !> A string used to group a set of locations.  This is typically a
        !> file name, but can also be a package name, or some other label.
        !> When it is a file name, it should be the "expanded" version.
        !> Port of xref.el:xref-location-group.
        procedure(xref_location_group_procedure), deferred :: group
        !> The line number corresponding to the location (0 for none).
        !> Port of xref.el:xref-location-line.
        procedure :: line_number => xref_location_line
    end type xref_location_type

    abstract interface
        !> A marker at location THIS.  Not a port.
        recursive subroutine xref_location_marker_procedure(this, marker, status)
            import :: xref_location_type, handle_type, status_type
            implicit none(type, external)
            class(xref_location_type), intent(in) :: this
            type(handle_type), intent(out) :: marker
            type(status_type), intent(out) :: status
        end subroutine xref_location_marker_procedure

        !> The group of location THIS.  Not a port.
        function xref_location_group_procedure(this) result(group)
            import :: xref_location_type
            implicit none(type, external)
            class(xref_location_type), intent(in) :: this
            character(len=:), allocatable :: group
        end function xref_location_group_procedure
    end interface

    public :: xref_location_marker_procedure, xref_location_group_procedure

    !> A file location is a file/line/column triple.
    !> Line numbers start from 1 and columns from 0.  Port of xref.el
    !> xref-file-location.
    type, extends(xref_location_type), public :: xref_file_location_type
        character(len=:), allocatable :: file
        integer(int64) :: line = 0
        integer(int64) :: column = 0
    contains
        !> Port of xref.el:xref-location-marker (of an xref-file-location).
        procedure :: marker => xref_file_location_marker
        !> Port of xref.el:xref-location-group (of an xref-file-location).
        procedure :: group => xref_file_location_group
        !> Port of xref.el:xref-location-line (of an xref-file-location).
        procedure :: line_number => xref_file_location_line
    end type xref_file_location_type

    !> A position in a buffer.  Port of xref.el xref-buffer-location.
    type, extends(xref_location_type), public :: xref_buffer_location_type
        type(handle_type) :: buffer
        integer(int64) :: position = 0
    contains
        !> Port of xref.el:xref-location-marker (of an xref-buffer-location).
        procedure :: marker => xref_buffer_location_marker
        !> Port of xref.el:xref-location-group (of an xref-buffer-location).
        procedure :: group => xref_buffer_location_group
    end type xref_buffer_location_type

    !> Bogus locations are sometimes useful to indicate errors,
    !> e.g. when we know that a function exists but the actual location
    !> is not known.  Port of xref.el xref-bogus-location.
    type, extends(xref_location_type), public :: xref_bogus_location_type
        character(len=:), allocatable :: message
    contains
        !> Port of xref.el:xref-location-marker (of an xref-bogus-location).
        procedure :: marker => xref_bogus_location_marker
        !> Port of xref.el:xref-location-group (of an xref-bogus-location).
        procedure :: group => xref_bogus_location_group
    end type xref_bogus_location_type

    !> An xref item describes a reference to a location somewhere.
    !>
    !> SUMMARY is a string which describes the location.  When the
    !> location's `line' is not 0, the summary is implied to be the
    !> contents of a file or buffer line containing the location.  When
    !> multiple locations in a row report the same line, in the same group
    !> (corresponding to the case of multiple locations on one line), the
    !> summaries are concatenated in the Xref output buffer.  Consequently,
    !> any code that creates xref values should take care to slice the
    !> summary values when several locations point to the same line.
    !>
    !> A match item (a search result) has a LENGTH, the length of the
    !> match (XREF_MATCH_LENGTH_NONE for other items); the `xref_match'
    !> face covers it in the summary from MATCH_BEGINNING (a 0-based
    !> character offset).  Arrays of items grow with `append_xref_item'.
    !> Port of xref.el xref-item and xref-match-item.
    type, public :: xref_item_type
        character(len=:), allocatable :: summary
        class(xref_location_type), allocatable :: location
        integer(int64) :: length = XREF_MATCH_LENGTH_NONE
        integer(int64) :: match_beginning = 0
    end type xref_item_type

    !> An xref backend: the methods a backend overrides, each with
    !> upstream's default.  A function of `xref_backend_functions' gives
    !> one.  Port of xref.el's backend generics.
    type, public :: xref_backend_type
    contains
        !> Find definitions of IDENTIFIER.
        !>
        !> The result must be an array of xref items.  If IDENTIFIER
        !> contains sufficient information to determine a unique
        !> definition, return only that definition.  If there are multiple
        !> possible definitions, return all of them.  If no definitions
        !> can be found, return an empty array.
        !>
        !> IDENTIFIER can be any string given by `identifier_at_point', or
        !> from the table `identifier_completion_table' gives.
        !>
        !> To create an xref item, call `make_xref'.  Port of
        !> xref.el:xref-backend-definitions.
        procedure :: definitions => xref_backend_definitions
        !> Find references of IDENTIFIER.
        !> The result must be an array of xref items.  If no references
        !> can be found, return an empty array.
        !>
        !> The default implementation uses `xref_references_in_directory'.
        !> Port of xref.el:xref-backend-references.
        procedure :: references => xref_backend_references
        !> Find all identifiers that match PATTERN string.
        !> The second argument has the same meaning as in `apropos'.
        !>
        !> The backend can use `xref_apropos_regexp' to convert the pattern
        !> to regexp.  Port of xref.el:xref-backend-apropos.
        procedure :: apropos => xref_backend_apropos
        !> The relevant identifier at point (FOUND false for none).
        !> Port of xref.el:xref-backend-identifier-at-point.
        procedure :: identifier_at_point => xref_backend_identifier_at_point
        !> The completion table for identifiers (FOUND false for none).
        !> Port of xref.el:xref-backend-identifier-completion-table.
        procedure :: identifier_completion_table => xref_backend_identifier_completion_table
        !> True if case is not significant in identifier completion.
        !> Port of xref.el:xref-backend-identifier-completion-ignore-case.
        procedure :: identifier_completion_ignore_case => xref_backend_identifier_completion_ignore_case
    end type xref_backend_type

    abstract interface
        !> A function of `xref_backend_functions': the backend for the
        !> current context, or BACKEND unallocated if it is not
        !> applicable.  Not a port.
        recursive subroutine xref_backend_function_procedure(backend)
            import :: xref_backend_type
            implicit none(type, external)
            class(xref_backend_type), allocatable, intent(out) :: backend
        end subroutine xref_backend_function_procedure
    end interface

    public :: xref_backend_function_procedure

    ! A function of `xref_backend_functions', or the `t' of a buffer's own
    ! value (the default functions run there), with the depth it was added
    ! with.
    type :: xref_backend_entry_type
        procedure(xref_backend_function_procedure), pointer, nopass :: fn => null()
        logical :: is_default = .false.
        integer :: depth = 0
    end type xref_backend_entry_type

    ! A buffer's own functions.
    type :: xref_backend_local_type
        integer(int64) :: serial = 0
        type(xref_backend_entry_type), allocatable :: entries(:)
    end type xref_backend_local_type

    !> The hook `xref_backend_functions': functions called in turn with no
    !> arguments, until one gives a backend.  `%prepend' and `%append' add a
    !> function as add-hook does (LOCAL for the current buffer's own
    !> value), `%remove' removes one.  Not a port.
    type, public :: xref_backend_functions_type
        type(xref_backend_entry_type), allocatable :: entries(:)
        type(xref_backend_local_type), allocatable :: locals(:)
    contains
        !> Add FN to the hook (subr.el's add-hook; depth 0 by default).
        procedure :: prepend => prepend_backend_function
        !> Add FN to the hook (add-hook with APPEND: depth 90 by default).
        procedure :: append => append_backend_function
        !> Remove FN from the hook (subr.el's remove-hook).
        procedure :: remove => remove_backend_function
        !> Forget the current buffer's own functions.
        procedure :: clear_local => clear_backend_functions_local
        !> Whether the current buffer has its own functions.
        procedure :: is_local => is_backend_functions_local
    end type xref_backend_functions_type

    !> What an xref list is fetched from: `fetch' gives the items.  It must
    !> not depend on the current buffer or selected window.  Port of
    !> xref.el's FETCHER functions.
    type, abstract, public :: xref_fetcher_type
    contains
        !> The items (an error condition if there are none to show).
        procedure(xref_fetch_procedure), deferred :: fetch
    end type xref_fetcher_type

    abstract interface
        !> The items of fetcher THIS.  Not a port.
        recursive subroutine xref_fetch_procedure(this, items, status)
            import :: xref_fetcher_type, xref_item_type, status_type
            implicit none(type, external)
            class(xref_fetcher_type), intent(inout) :: this
            type(xref_item_type), allocatable, intent(out) :: items(:)
            type(status_type), intent(out) :: status
        end subroutine xref_fetch_procedure
    end interface

    public :: xref_fetch_procedure

    !> The fetcher of the find commands: it revisits the saved position
    !> and delegates the finding to the backend's method of KIND, called
    !> with ARGUMENT; INPUT names what was asked for in the error when
    !> nothing is found.  Port of xref.el:xref--create-fetcher's function.
    type, extends(xref_fetcher_type), public :: xref_backend_fetcher_type
        type(handle_type) :: buffer
        integer(int64) :: position = 0
        class(xref_backend_type), allocatable :: backend
        integer :: kind = KIND_DEFINITIONS
        character(len=:), allocatable :: argument, input
    contains
        !> Port of xref.el:xref--create-fetcher (the function it returns).
        procedure :: fetch => fetch_from_backend
    end type xref_backend_fetcher_type

    !> A fetcher of a fixed array of items, which can be fetched once:
    !> refreshing is not supported.  Port of xref.el:xref--show-xrefs (the
    !> old convention of a list of xrefs).
    type, extends(xref_fetcher_type), public :: xref_items_fetcher_type
        type(xref_item_type), allocatable :: items(:)
        logical :: fetched = .false.
    contains
        !> Port of xref.el:xref--show-xrefs (its function).
        procedure :: fetch => fetch_fixed_items
    end type xref_items_fetcher_type

    !> What the show functions get with the fetcher: WINDOW, the window
    !> that was selected before the current command was called;
    !> DISPLAY_ACTION, where the target location should be displayed
    !> (XREF_DISPLAY_ACTION_NONE or XREF_DISPLAY_ACTION_WINDOW); AUTO_JUMP,
    !> an XREF_AUTO_JUMP_* value; and FETCHED, the items already fetched
    !> (when allocated).  Port of the ALIST argument of
    !> `xref-show-xrefs-function'.
    type, public :: xref_show_parameters_type
        type(handle_type) :: window
        integer :: display_action = XREF_DISPLAY_ACTION_NONE
        integer :: auto_jump = XREF_AUTO_JUMP_NO
        type(xref_item_type), allocatable :: fetched(:)
    end type xref_show_parameters_type

    abstract interface
        !> A function of `xref_show_xrefs_function' or
        !> `xref_show_definitions_function': show what FETCHER fetches
        !> according to PARAMETERS; BUFFER is the buffer showing them
        !> (the null handle when there is none).  Not a port.
        recursive subroutine xref_show_procedure(fetcher, parameters, buffer, status)
            import :: xref_fetcher_type, xref_show_parameters_type, handle_type, status_type
            implicit none(type, external)
            class(xref_fetcher_type), intent(inout) :: fetcher
            type(xref_show_parameters_type), intent(in) :: parameters
            type(handle_type), intent(out) :: buffer
            type(status_type), intent(out) :: status
        end subroutine xref_show_procedure

        !> A function of `xref_references_in_directory_function': all
        !> references to IDENTIFIER in files under directory DIRECTORY.  Not a port.
        recursive subroutine xref_references_in_directory_procedure(identifier, directory, items, status)
            import :: xref_item_type, status_type
            implicit none(type, external)
            character(len=*), intent(in) :: identifier, directory
            type(xref_item_type), allocatable, intent(out) :: items(:)
            type(status_type), intent(out) :: status
        end subroutine xref_references_in_directory_procedure
    end interface

    public :: xref_show_procedure, xref_references_in_directory_procedure

    !> An entry of `xref_search_programs': the command TEMPLATE of search
    !> PROGRAM (an XREF_SEARCH_PROGRAM_* value, or another number for a
    !> program of the configuration's own).  The template must be a shell
    !> command (or usually a pipeline) that will search the files based
    !> on the list of file names that is piped from stdin, separated by
    !> null characters.  The template should have the following fields:
    !>
    !>   <C> for extra arguments such as -i and --color
    !>   <R> for the regexp itself (in Extended format)
    type, public :: xref_search_program_type
        integer :: program = 0
        character(len=160) :: template = ""
    end type xref_search_program_type

    !> A group of items in the *xref* buffer: the group's NAME and its
    !> ITEMS.  Port of an element of xref.el's XREF-ALIST.
    type, public :: xref_group_type
        character(len=:), allocatable :: name
        type(xref_item_type), allocatable :: items(:)
    end type xref_group_type

    ! A hit of a search program: the LINE in FILE, and the TEXT of the line.
    type :: xref_hit_type
        integer(int64) :: line = 0
        character(len=:), allocatable :: file, text
    end type xref_hit_type

    ! The backward and forward stacks of markers to visited Xref locations,
    ! the most recent last.
    type :: xref_history_type
        type(handle_type), allocatable :: backward(:), forward(:)
    end type xref_history_type

    ! An *xref* buffer's own state: the items shown (the `xref_item'
    ! property is the index of one), the fetcher that refreshes them, the
    ! window and the window-switching intent before it was made, the
    ! number of matches found, and the keymap its edit mode replaced.
    type :: xref_buffer_entry_type
        integer(int64) :: serial = 0
        type(xref_item_type), allocatable :: items(:)
        class(xref_fetcher_type), allocatable :: fetcher
        type(handle_type) :: original_window
        integer :: original_window_intent = XREF_DISPLAY_ACTION_NONE
        integer(int64) :: found_match_count = 0
    end type xref_buffer_entry_type

    !> Style of file name display in *xref* buffers.
    !>
    !> If the value is XREF_FILE_NAME_DISPLAY_ABSOLUTE, show the file names
    !> in their full absolute form.
    !>
    !> If XREF_FILE_NAME_DISPLAY_NONDIRECTORY, show only the nondirectory
    !> (a.k.a. "base name") part of the file name.
    !>
    !> If XREF_FILE_NAME_DISPLAY_PROJECT_RELATIVE, the default, show only
    !> the file name relative to the current project root.  If there is no
    !> current project, or if the file resides outside of its root, show
    !> that particular file name in its full absolute form.  Port of
    !> xref.el xref-file-name-display.
    integer, public :: xref_file_name_display = XREF_FILE_NAME_DISPLAY_PROJECT_RELATIVE

    !> If not XREF_PROMPT_FOR_IDENTIFIER_NO, prompt for the identifier to
    !> find.
    !>
    !> When XREF_PROMPT_FOR_IDENTIFIER_YES, always prompt for the identifier
    !> name.
    !>
    !> When XREF_PROMPT_FOR_IDENTIFIER_NO, prompt only when there's no
    !> value at point we can use, or when the command has been called with
    !> the prefix argument.
    !>
    !> Otherwise, `xref_prompt_for_identifier_commands' is a list of xref
    !> commands which will always prompt (XREF_PROMPT_FOR_IDENTIFIER_LISTED),
    !> with the identifier at point, if any, used as the default.  If the
    !> value is XREF_PROMPT_FOR_IDENTIFIER_EXCEPT_LISTED, the meaning of the
    !> list is negated: these commands will NOT prompt.  Port of xref.el
    !> xref-prompt-for-identifier.
    integer, public :: xref_prompt_for_identifier = XREF_PROMPT_FOR_IDENTIFIER_EXCEPT_LISTED

    !> The commands `xref_prompt_for_identifier' lists.  Port of the list
    !> of xref.el xref-prompt-for-identifier.
    character(len=:), allocatable, public :: xref_prompt_for_identifier_commands(:)

    !> Functions called after jumping to an xref.
    !> Also see `xref_current_item'.  Port of xref.el xref-after-jump-hook
    !> (the default has only `recenter': `xref_pulse_momentarily' is not
    !> ported).
    type(hook_type), target, public :: xref_after_jump_hook = hook_type(name="xref_after_jump_hook")

    !> Functions called after returning to a pre-jump location.  Port of
    !> xref.el xref-after-return-hook (empty: `xref_pulse_momentarily' is
    !> not ported).
    type(hook_type), target, public :: xref_after_return_hook = hook_type(name="xref_after_return_hook")

    !> Functions called after the xref buffer is updated.  Port of xref.el
    !> xref-after-update-hook.
    type(hook_type), target, public :: xref_after_update_hook = hook_type(name="xref_after_update_hook")

    !> If XREF_AUTO_JUMP_YES, `find_xref_definitions' always jumps to the
    !> first result.  XREF_AUTO_JUMP_SHOW means to show the first result's
    !> location, but keep the focus on the Xref buffer's window.
    !> XREF_AUTO_JUMP_MOVE means to only move point to the first result.
    !> This variable also affects the variants of `find_xref_definitions',
    !> such as `find_xref_definitions_other_window'.  Port of xref.el
    !> xref-auto-jump-to-first-definition.
    integer, public :: xref_auto_jump_to_first_definition = XREF_AUTO_JUMP_NO

    !> If XREF_AUTO_JUMP_YES, `find_xref_references' always jumps to the
    !> first result.  XREF_AUTO_JUMP_SHOW means to show the first result's
    !> location, but keep the focus on the Xref buffer's window.
    !> XREF_AUTO_JUMP_MOVE means to only move point to the first result.
    !> This variable also affects commands similar to
    !> `find_xref_references', such as `find_xref_apropos', and
    !> `project_find_regexp'.  Port of xref.el xref-auto-jump-to-first-xref.
    integer, public :: xref_auto_jump_to_first_xref = XREF_AUTO_JUMP_NO

    !> Special hook to find the xref backend for the current context.
    !> Each function on this hook is called in turn with no arguments,
    !> and should give either no backend to mean that it is not applicable,
    !> or an xref backend, which is an object of a type extending
    !> `xref_backend_type'.  The etags backend is appended to it at startup.
    !> Port of xref.el xref-backend-functions.
    type(xref_backend_functions_type), public :: xref_backend_functions

    !> The item being processed, for `xref_after_jump_hook' (unallocated
    !> location otherwise).  Port of xref.el xref-current-item.
    type(xref_item_type), public :: xref_current_item

    !> Face used to highlight file header in the xref buffer.  Port of
    !> xref.el xref-file-header.
    type(named_face_type), target, public :: xref_file_header_face
    !> Face for displaying line numbers in the xref buffer.  Port of
    !> xref.el xref-line-number.
    type(named_face_type), target, public :: xref_line_number_face
    !> Face used to highlight matches in the xref buffer.  Port of xref.el
    !> xref-match.
    type(named_face_type), target, public :: xref_match_face

    !> Face name to show the number of matches on the mode line.  Port of
    !> xref.el xref-num-matches-face.
    character(len=:), allocatable, public :: xref_match_count_face

    !> The column to visually "truncate" each Xref buffer line to
    !> (XREF_TRUNCATION_WIDTH_NONE disables truncation).  Port of xref.el
    !> xref-truncation-width.
    integer, public :: xref_truncation_width = 400

    !> Function to display a list of search results.
    !>
    !> It should accept the arguments FETCHER and PARAMETERS.
    !>
    !> FETCHER is an `xref_fetcher_type' whose `fetch' gives the items.  It
    !> must not depend on the current buffer or selected window.
    !>
    !> PARAMETERS has the components WINDOW for the window that was
    !> selected before the current command was called, and DISPLAY_ACTION,
    !> which indicates where the target location should be displayed: the
    !> possible values are XREF_DISPLAY_ACTION_NONE, or
    !> XREF_DISPLAY_ACTION_WINDOW meaning the other window.  Port of xref.el
    !> xref-show-xrefs-function.
    procedure(xref_show_procedure), pointer, public :: xref_show_xrefs_function => null()

    !> Function to handle the definition search results.
    !>
    !> Accepts the same arguments as `xref_show_xrefs_function'.
    !>
    !> Generally, it is expected to jump to the definition if there's
    !> only one, and otherwise provide some way to choose among the
    !> definitions.  Port of xref.el xref-show-definitions-function.
    procedure(xref_show_procedure), pointer, public :: xref_show_definitions_function => null()

    !> Function to find all references to a identifier in a directory.
    !> It should take two string arguments: IDENTIFIER and DIRECTORY.
    !> And give an array of xref items representing all code references to
    !> IDENTIFIER in files under DIRECTORY.  Port of xref.el
    !> xref-references-in-directory-function (the default is
    !> `xref_references_in_directory_grep': Semantic is not ported).
    procedure(xref_references_in_directory_procedure), pointer, public :: xref_references_in_directory_function &
        => null()

    !> The command templates of the search programs.  Port of xref.el
    !> xref-search-program-alist.
    type(xref_search_program_type), allocatable, public :: xref_search_programs(:)

    !> The program to use for regexp search inside files.
    !>
    !> This must reference a corresponding entry in `xref_search_programs'.
    !>
    !> This variable is used in `xref_matches_in_files', which is the
    !> utility function used by commands like `project_find_regexp'.
    !> Port of xref.el xref-search-program.
    integer, public :: xref_search_program = XREF_SEARCH_PROGRAM_GREP

    !> The history of identifiers read by `read_xref_identifier'.  Port of
    !> xref.el xref--read-identifier-history.
    type(history_setting_type), target, public :: xref_read_identifier_history = &
                                                  history_setting_type(name="xref_read_identifier_history")
    !> The history of patterns read by `find_xref_apropos'.  Port of xref.el
    !> xref--read-pattern-history.
    type(history_setting_type), target, public :: xref_read_pattern_history = &
                                                  history_setting_type(name="xref_read_pattern_history")

    !> Keymap of `xref_xref_buffer_mode'.  Port of xref.el
    !> xref--xref-buffer-mode-map.
    type(handle_type), public :: xref_xref_buffer_mode_map
    !> Keymap of `xref_transient_buffer_mode'.  Port of xref.el
    !> xref--transient-buffer-mode-map.
    type(handle_type), public :: xref_transient_buffer_mode_map
    !> Keymap for `xref_edit_mode'.  Port of xref.el xref-edit-mode-map.
    type(handle_type), public :: xref_edit_mode_map
    !> Hook run after entering Xref mode.  Port of the hook of xref.el
    !> xref--xref-buffer-mode.
    type(hook_type), target, public :: xref_xref_buffer_mode_hook = hook_type(name="xref_xref_buffer_mode_hook")
    !> Hook run after entering Xref Transient mode.  Port of the hook of
    !> xref.el xref--transient-buffer-mode.
    type(hook_type), target, public :: xref_transient_buffer_mode_hook = &
                                       hook_type(name="xref_transient_buffer_mode_hook")
    !> Hooks run when changing to Xref-Edit mode.  Port of xref.el
    !> xref-edit-mode-hook.
    type(hook_type), target, public :: xref_edit_mode_hook = hook_type(name="xref_edit_mode_hook")

    ! (BACKWARD-STACK . FORWARD-STACK) of markers to visited Xref
    ! locations.  Port of xref.el xref--history (the global history).
    type(xref_history_type) :: xref_history

    ! The *xref* buffers' own state.
    type(xref_buffer_entry_type), allocatable :: xref_buffers(:)

    ! The value of `xref_backend_functions' `xref_etags_mode' saved.  Port of
    ! xref.el xref-etags-mode--saved.
    type(xref_backend_entry_type), allocatable :: xref_etags_mode_saved(:)
    logical :: xref_etags_mode_saved_local = .false.

    ! The file of the last hit `find_file_buffer' looked up and its buffer,
    ! the file the temporary buffer of `collect_matches' was set up for, and
    ! the prefix of the hits' file names.  Port of xref.el
    ! xref--last-file-buffer, xref--temp-buffer-file-name and
    ! xref--hits-file-prefix.
    character(len=:), allocatable :: last_file
    type(handle_type) :: last_file_buffer
    character(len=:), allocatable :: temp_buffer_file_name, hits_file_prefix

contains

    ! ------------------------------------------------------------------
    ! Locations and items
    ! ------------------------------------------------------------------

    !> The line of a location without one: 0.  Port of
    !> xref.el:xref-location-line (the default method).
    integer(int64) function xref_location_line(this) result(line)
        class(xref_location_type), intent(in) :: this

        line = 0
    end function xref_location_line

    !> A new file location: FILE, LINE (from 1) and COLUMN (from 0).
    !> Port of xref.el:xref-make-file-location.
    function make_xref_file_location(file, line, column) result(location)
        character(len=*), intent(in) :: file
        integer(int64), intent(in) :: line, column
        type(xref_file_location_type) :: location

        location%file = file
        location%line = line
        location%column = column
    end function make_xref_file_location

    !> A new buffer location: POSITION in BUFFER.  Port of
    !> xref.el:xref-make-buffer-location.
    function make_xref_buffer_location(buffer, position) result(location)
        type(handle_type), intent(in) :: buffer
        integer(int64), intent(in) :: position
        type(xref_buffer_location_type) :: location

        location%buffer = buffer
        location%position = position
    end function make_xref_buffer_location

    !> A new bogus location with MESSAGE.  Port of
    !> xref.el:xref-make-bogus-location.
    function make_xref_bogus_location(message) result(location)
        character(len=*), intent(in) :: message
        type(xref_bogus_location_type) :: location

        location%message = message
    end function make_xref_bogus_location

    !> A new xref item: SUMMARY and LOCATION.  Port of xref.el:xref-make.
    function make_xref(summary, location) result(item)
        character(len=*), intent(in) :: summary
        class(xref_location_type), intent(in) :: location
        type(xref_item_type) :: item

        item%summary = summary
        allocate (item%location, source=location)
    end function make_xref

    !> A new match item, a search result: SUMMARY, LOCATION and the LENGTH
    !> of the match.  Port of xref.el:xref-make-match.
    function make_xref_match(summary, location, length) result(item)
        character(len=*), intent(in) :: summary
        class(xref_location_type), intent(in) :: location
        integer(int64), intent(in) :: length
        type(xref_item_type) :: item

        item%summary = summary
        allocate (item%location, source=location)
        item%length = length
    end function make_xref_match

    !> Append ITEM to ITEMS (allocated if needed).  An array of items grows
    !> through this subroutine: gfortran 13 leaks the polymorphic location
    !> of an item in an array constructor.  Not a port.
    subroutine append_xref_item(items, item)
        type(xref_item_type), allocatable, intent(inout) :: items(:)
        type(xref_item_type), intent(in) :: item
        type(xref_item_type), allocatable :: grown(:)
        integer :: n

        if (.not. allocated(items)) allocate (items(0))
        n = size(items)
        allocate (grown(n + 1))
        grown(1:n) = items
        grown(n + 1) = item
        call move_alloc(grown, items)
    end subroutine append_xref_item

    ! Append the items MORE to ITEMS.  Not a port.
    subroutine append_xref_items(items, more)
        type(xref_item_type), allocatable, intent(inout) :: items(:)
        type(xref_item_type), intent(in) :: more(:)
        type(xref_item_type), allocatable :: grown(:)
        integer :: n

        if (.not. allocated(items)) allocate (items(0))
        n = size(items)
        allocate (grown(n + size(more)))
        grown(1:n) = items
        grown(n + 1:) = more
        call move_alloc(grown, items)
    end subroutine append_xref_items

    !> The marker for a file location: in the buffer visiting its file,
    !> at its line and column.  The location shouldn't be out of date, but
    !> past the end of the file (or of its line) it is an approximation.
    !> Port of xref.el:xref-location-marker (of an xref-file-location).
    recursive subroutine xref_file_location_marker(this, marker, status)
        class(xref_file_location_type), intent(in) :: this
        type(handle_type), intent(out) :: marker
        type(status_type), intent(out) :: status
        type(handle_type) :: buffer, old
        type(status_type) :: st
        integer(int64) :: opoint, begv, zv, shortage

        old = current_buffer()
        call find_file_noselect(this%file, buffer, status)
        if (.not. status%ok()) return
        call set_buffer(buffer, st)
        opoint = point()
        begv = point_min()
        zv = point_max()
        call widen()
        call goto_char(point_min())
        ! (beginning-of-line line) (forward-char column), ignoring errors.
        shortage = forward_line(this%line - 1)
        call goto_char(min(point() + this%column, point_max()))
        call point_marker(marker)
        call narrow_to_region(begv, zv, st)
        call goto_char(opoint)
        call set_buffer(old, st)
    end subroutine xref_file_location_marker

    !> The group of a file location: its file.  Port of
    !> xref.el:xref-location-group (of an xref-file-location).
    function xref_file_location_group(this) result(group)
        class(xref_file_location_type), intent(in) :: this
        character(len=:), allocatable :: group

        group = this%file
    end function xref_file_location_group

    !> The line of a file location.  Port of xref.el:xref-location-line (of
    !> an xref-file-location).
    integer(int64) function xref_file_location_line(this) result(line)
        class(xref_file_location_type), intent(in) :: this

        line = this%line
    end function xref_file_location_line

    !> A marker at the position of a buffer location.  Port of
    !> xref.el:xref-location-marker (of an xref-buffer-location).
    recursive subroutine xref_buffer_location_marker(this, marker, status)
        class(xref_buffer_location_type), intent(in) :: this
        type(handle_type), intent(out) :: marker
        type(status_type), intent(out) :: status
        type(handle_type) :: old
        type(status_type) :: st

        status = status_type()
        old = current_buffer()
        call set_buffer(this%buffer, status)
        if (.not. status%ok()) return
        call copy_marker_at(this%position, marker, .false., status)
        call set_buffer(old, st)
    end subroutine xref_buffer_location_marker

    !> The group of a buffer location: the buffer's file name, or
    !> "(buffer NAME)".  Port of xref.el:xref-location-group (of an
    !> xref-buffer-location).
    function xref_buffer_location_group(this) result(group)
        class(xref_buffer_location_type), intent(in) :: this
        character(len=:), allocatable :: group
        character(len=:), allocatable :: name
        type(status_type) :: st
        logical :: found

        call buffer_file_name(group, found, st, this%buffer)
        if (found) return
        call buffer_name(this%buffer, name, st)
        group = "(buffer "//name//")"
    end function xref_buffer_location_group

    !> A bogus location has no marker: a `user_error' error condition
    !> occurs with its message.  Port of xref.el:xref-location-marker (of an
    !> xref-bogus-location).
    recursive subroutine xref_bogus_location_marker(this, marker, status)
        class(xref_bogus_location_type), intent(in) :: this
        type(handle_type), intent(out) :: marker
        type(status_type), intent(out) :: status

        marker = null_handle()
        status = status_type()
        call set_error(status, FM_USER_ERROR, this%message)
    end subroutine xref_bogus_location_marker

    !> The group of a bogus location: "(No location)".  Port of
    !> xref.el:xref-location-group (of an xref-bogus-location).
    function xref_bogus_location_group(this) result(group)
        class(xref_bogus_location_type), intent(in) :: this
        character(len=:), allocatable :: group

        group = "(No location)"
    end function xref_bogus_location_group

    ! ------------------------------------------------------------------
    ! Backends
    ! ------------------------------------------------------------------

    !> The error of a backend without the method:
    !> "No Xref backend.  Try `M-x eglot', `M-x visit_tags_table', or `M-x
    !> etags_regen_mode'."  Port of xref.el:xref--no-backend-available.
    subroutine report_no_xref_backend(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call set_error(status, FM_USER_ERROR, "No Xref backend.  Try `M-x eglot', `M-x visit_tags_table', or "// &
                       "`M-x etags_regen_mode'.")
    end subroutine report_no_xref_backend

    !> The default definitions: no backend.  Port of
    !> xref.el:xref-backend-definitions (the default method).
    recursive subroutine xref_backend_definitions(this, identifier, items, status)
        class(xref_backend_type), intent(inout) :: this
        character(len=*), intent(in) :: identifier
        type(xref_item_type), allocatable, intent(out) :: items(:)
        type(status_type), intent(out) :: status

        allocate (items(0))
        call report_no_xref_backend(status)
    end subroutine xref_backend_definitions

    !> The default references: the references of IDENTIFIER in the
    !> directories of the current project, through
    !> `xref_references_in_directory'.  Port of
    !> xref.el:xref-backend-references (the default method; the project's
    !> directories come from `xref_project_directories').
    recursive subroutine xref_backend_references(this, identifier, items, status)
        class(xref_backend_type), intent(inout) :: this
        character(len=*), intent(in) :: identifier
        type(xref_item_type), allocatable, intent(out) :: items(:)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: dirs(:)
        type(xref_item_type), allocatable :: found(:)
        integer :: k

        allocate (items(0))
        allocate (character(len=1) :: dirs(0))
        call xref_project_directories(dirs, status)
        if (.not. status%ok()) return

        do k = 1, size(dirs)
            call message("Searching "//trim(dirs(k))//"...")
            call xref_references_in_directory(identifier, trim(dirs(k)), found, status)
            if (.not. status%ok()) return
            call append_xref_items(items, found)
            call message("Searching "//trim(dirs(k))//"... done")
        end do
    end subroutine xref_backend_references

    !> The directories the default `references' searches: the current
    !> project's root and external roots.  project.el is not ported: an
    !> error condition occurs.  Not a port.
    subroutine xref_project_directories(dirs, status)
        character(len=:), allocatable, intent(out) :: dirs(:)
        type(status_type), intent(out) :: status

        allocate (character(len=1) :: dirs(0))
        status = status_type()
        call set_error(status, FM_GENERIC_ERROR, "Finding references without a backend method needs the current "// &
                       "project (project.el), which is not supported yet")
    end subroutine xref_project_directories

    !> The default apropos: no backend.  Port of
    !> xref.el:xref-backend-apropos (the default method).
    recursive subroutine xref_backend_apropos(this, pattern, items, status)
        class(xref_backend_type), intent(inout) :: this
        character(len=*), intent(in) :: pattern
        type(xref_item_type), allocatable, intent(out) :: items(:)
        type(status_type), intent(out) :: status

        allocate (items(0))
        call report_no_xref_backend(status)
    end subroutine xref_backend_apropos

    !> The default identifier at point: the identifier (word and identifier
    !> constituents) around point.  Port of
    !> xref.el:xref-backend-identifier-at-point (the default method).
    recursive subroutine xref_backend_identifier_at_point(this, identifier, found)
        class(xref_backend_type), intent(inout) :: this
        character(len=:), allocatable, intent(out) :: identifier
        logical, intent(out) :: found
        integer(int64) :: beginning, end
        type(status_type) :: st

        identifier = ""
        call bounds_of_identifier_at_point(beginning, end, found)
        if (found) call buffer_substring_no_properties(beginning, end, identifier, st)
    end subroutine xref_backend_identifier_at_point

    !> The default completion table: none.  Port of
    !> xref.el:xref-backend-identifier-completion-table (the default
    !> method).
    recursive subroutine xref_backend_identifier_completion_table(this, table, found)
        class(xref_backend_type), intent(inout) :: this
        type(completion_table_type), intent(out) :: table
        logical, intent(out) :: found
        type(value_type) :: none(0)

        table = completion_list_table(make_list(none))
        found = .false.
    end subroutine xref_backend_identifier_completion_table

    !> The default: `completion_ignore_case'.  Port of
    !> xref.el:xref-backend-identifier-completion-ignore-case (the default
    !> method).
    logical function xref_backend_identifier_completion_ignore_case(this) result(ignore)
        class(xref_backend_type), intent(inout) :: this

        ignore = completion_ignore_case%value
    end function xref_backend_identifier_completion_ignore_case

    !> The backend for the current context: the first one a function of
    !> `xref_backend_functions' gives (BACKEND unallocated if none does).
    !> Port of xref.el:xref-find-backend.
    recursive subroutine find_xref_backend(backend)
        class(xref_backend_type), allocatable, intent(out) :: backend
        integer :: k, i

        k = backend_local_index(xref_backend_functions)

        if (k == 0) then
            call try_entries(xref_backend_functions%entries)
            return
        end if

        do i = 1, size(xref_backend_functions%locals(k)%entries)
            if (xref_backend_functions%locals(k)%entries(i)%is_default) then
                call try_entries(xref_backend_functions%entries)
            else
                call xref_backend_functions%locals(k)%entries(i)%fn(backend)
            end if

            if (allocated(backend)) return
        end do

    contains

        recursive subroutine try_entries(entries)
            type(xref_backend_entry_type), allocatable, intent(in) :: entries(:)
            integer :: j

            if (.not. allocated(entries)) return

            do j = 1, size(entries)
                if (entries(j)%is_default) cycle
                call entries(j)%fn(backend)
                if (allocated(backend)) return
            end do
        end subroutine try_entries
    end subroutine find_xref_backend

    ! The index of the current buffer's own functions in SELF (0 for
    ! none).  Not a port.
    integer function backend_local_index(self) result(k)
        class(xref_backend_functions_type), intent(in) :: self

        k = 0
        if (current == 0 .or. .not. allocated(self%locals)) return
        k = findloc(self%locals%serial, buffers(current)%serial, dim=1)
    end function backend_local_index

    ! Add FN to ENTRIES at DEPTH: before the first entry of a depth >= DEPTH
    ! (AT_END false, add-hook's push) or after the last of a depth <= DEPTH
    ! (AT_END true, add-hook's append), as subr.el's add-hook sorts them.
    ! A function already there stays where it is.  Not a port.
    subroutine place_backend_function(entries, fn, depth, at_end)
        type(xref_backend_entry_type), allocatable, intent(inout) :: entries(:)
        procedure(xref_backend_function_procedure) :: fn
        integer, intent(in) :: depth
        logical, intent(in) :: at_end
        type(xref_backend_entry_type) :: entry
        integer :: i, at

        if (.not. allocated(entries)) allocate (entries(0))

        do i = 1, size(entries)
            if (entries(i)%is_default) cycle
            if (associated(entries(i)%fn, fn)) return
        end do

        entry%fn => fn
        entry%depth = depth

        if (at_end) then
            at = 1

            do i = size(entries), 1, -1
                if (entries(i)%depth <= depth) then
                    at = i + 1
                    exit
                end if
            end do
        else
            at = size(entries) + 1

            do i = 1, size(entries)
                if (entries(i)%depth >= depth) then
                    at = i
                    exit
                end if
            end do
        end if

        entries = [entries(1:at - 1), entry, entries(at:)]
    end subroutine place_backend_function

    ! The current buffer's own functions, made (with the default functions,
    ! `t', in them) if it has none: their index K.  Not a port.
    subroutine make_backend_local(self, k)
        class(xref_backend_functions_type), intent(inout) :: self
        integer, intent(out) :: k
        type(xref_backend_local_type) :: local

        k = backend_local_index(self)
        if (k /= 0) return
        if (.not. allocated(self%locals)) allocate (self%locals(0))
        local%serial = buffers(current)%serial
        allocate (local%entries(1))
        local%entries(1)%is_default = .true.
        self%locals = [self%locals, local]
        k = size(self%locals)
    end subroutine make_backend_local

    !> Add FN to `xref_backend_functions' (subr.el's add-hook, at the
    !> front among the functions of its DEPTH, 0 by default), to the current
    !> buffer's own functions if LOCAL.  Port of subr.el:add-hook (for
    !> xref-backend-functions).
    subroutine prepend_backend_function(self, fn, depth, local)
        class(xref_backend_functions_type), intent(inout) :: self
        procedure(xref_backend_function_procedure) :: fn
        integer, intent(in), optional :: depth
        logical, intent(in), optional :: local
        integer :: d, k

        d = 0
        if (present(depth)) d = depth

        if (present(local)) then
            if (local .and. current /= 0) then
                call make_backend_local(self, k)
                call place_backend_function(self%locals(k)%entries, fn, d, .false.)
                return
            end if
        end if

        call place_backend_function(self%entries, fn, d, .false.)
    end subroutine prepend_backend_function

    !> Add FN to `xref_backend_functions' (add-hook with APPEND, at the end
    !> among the functions of its DEPTH, 90 by default), to the current
    !> buffer's own functions if LOCAL.  Port of subr.el:add-hook (for
    !> xref-backend-functions, with APPEND).
    subroutine append_backend_function(self, fn, depth, local)
        class(xref_backend_functions_type), intent(inout) :: self
        procedure(xref_backend_function_procedure) :: fn
        integer, intent(in), optional :: depth
        logical, intent(in), optional :: local
        integer :: d, k

        d = 90
        if (present(depth)) d = depth

        if (present(local)) then
            if (local .and. current /= 0) then
                call make_backend_local(self, k)
                call place_backend_function(self%locals(k)%entries, fn, d, .true.)
                return
            end if
        end if

        call place_backend_function(self%entries, fn, d, .true.)
    end subroutine append_backend_function

    !> Remove FN from `xref_backend_functions', from the current buffer's
    !> own functions if LOCAL.  Port of subr.el:remove-hook (for
    !> xref-backend-functions).
    subroutine remove_backend_function(self, fn, local)
        class(xref_backend_functions_type), intent(inout) :: self
        procedure(xref_backend_function_procedure) :: fn
        logical, intent(in), optional :: local
        integer :: k

        if (present(local)) then
            if (local) then
                k = backend_local_index(self)
                if (k /= 0) call drop(self%locals(k)%entries)
                return
            end if
        end if

        call drop(self%entries)

    contains

        subroutine drop(entries)
            type(xref_backend_entry_type), allocatable, intent(inout) :: entries(:)
            integer :: i

            if (.not. allocated(entries)) return

            do i = 1, size(entries)
                if (entries(i)%is_default) cycle

                if (associated(entries(i)%fn, fn)) then
                    entries = [entries(1:i - 1), entries(i + 1:)]
                    return
                end if
            end do
        end subroutine drop
    end subroutine remove_backend_function

    !> Forget the current buffer's own functions.  Port of
    !> data.c:Fkill_local_variable (for xref-backend-functions).
    subroutine clear_backend_functions_local(self)
        class(xref_backend_functions_type), intent(inout) :: self
        integer :: k

        k = backend_local_index(self)
        if (k /= 0) self%locals(k)%serial = 0
    end subroutine clear_backend_functions_local

    !> Whether the current buffer has its own functions.  Port of
    !> data.c:Flocal_variable_p (for xref-backend-functions).
    logical function is_backend_functions_local(self) result(local)
        class(xref_backend_functions_type), intent(in) :: self

        local = backend_local_index(self) /= 0
    end function is_backend_functions_local

    ! Forget the current buffer's own `xref_backend_functions' and *xref*
    ! state (clear_all_local_variables).  Not a port.
    subroutine clear_xref_locals()
        integer :: k

        call xref_backend_functions%clear_local()
        k = xref_buffer_index()
        if (k /= 0) xref_buffers(k)%serial = 0
    end subroutine clear_xref_locals

    ! ------------------------------------------------------------------
    ! The history
    ! ------------------------------------------------------------------

    ! Whether markers A and B are at the same position of the same buffer
    ! (equal of two markers).  Not a port.
    logical function is_same_marker(a, b)
        type(handle_type), intent(in) :: a, b
        type(handle_type) :: ba, bb
        integer(int64) :: pa, pb
        type(status_type) :: st

        call marker_buffer(a, ba, st)
        call marker_buffer(b, bb, st)
        call marker_position(a, pa, st)
        call marker_position(b, pb, st)
        is_same_marker = ba%index == bb%index .and. ba%generation == bb%generation .and. pa == pb
    end function is_same_marker

    ! Push marker M onto STACK, unless it is the same as the top one (then
    ! M is destroyed).  Port of xref.el:xref--push-backward and
    ! xref--push-forward.
    subroutine push_marker(stack, m)
        type(handle_type), allocatable, intent(inout) :: stack(:)
        type(handle_type), intent(in) :: m
        type(status_type) :: st
        integer :: n

        if (.not. allocated(stack)) allocate (stack(0))
        n = size(stack)

        if (n > 0) then
            if (is_same_marker(stack(n), m)) then
                call destroy_marker(m, st)
                return
            end if
        end if

        stack = [stack, m]
    end subroutine push_marker

    ! Pop the top marker of STACK into M.  Not a port.
    subroutine pop_marker(stack, m)
        type(handle_type), allocatable, intent(inout) :: stack(:)
        type(handle_type), intent(out) :: m
        integer :: n

        n = size(stack)
        m = stack(n)
        stack = stack(1:n - 1)
    end subroutine pop_marker

    ! Destroy the markers of STACK and empty it.  Not a port.
    subroutine clear_markers(stack)
        type(handle_type), allocatable, intent(inout) :: stack(:)
        type(status_type) :: st
        integer :: i

        if (.not. allocated(stack)) return

        do i = 1, size(stack)
            call destroy_marker(stack(i), st)
        end do

        deallocate (stack)
        allocate (stack(0))
    end subroutine clear_markers

    !> Add point M (defaults to a marker at point) to the marker stack.
    !> Erase the stack slots following this one.  Port of
    !> xref.el:xref-push-marker-stack.
    subroutine push_xref_marker_stack(m)
        type(handle_type), intent(in), optional :: m
        type(handle_type) :: pm

        if (present(m)) then
            call push_marker(xref_history%backward, m)
        else
            call point_marker(pm)
            call push_marker(xref_history%backward, pm)
        end if

        call clear_markers(xref_history%forward)
    end subroutine push_xref_marker_stack

    !> Discard all markers from the xref history.  Port of
    !> xref.el:xref-clear-marker-stack.
    subroutine clear_xref_marker_stack()
        call clear_markers(xref_history%backward)
        call clear_markers(xref_history%forward)
    end subroutine clear_xref_marker_stack

    !> Whether the xref back-history is empty.  Port of
    !> xref.el:xref-marker-stack-empty-p.
    logical function is_xref_marker_stack_empty()
        is_xref_marker_stack_empty = .true.
        if (allocated(xref_history%backward)) is_xref_marker_stack_empty = size(xref_history%backward) == 0
    end function is_xref_marker_stack_empty

    !> Whether the xref forward-history is empty.  Port of
    !> xref.el:xref-forward-history-empty-p.
    logical function is_xref_forward_history_empty()
        is_xref_forward_history_empty = .true.
        if (allocated(xref_history%forward)) is_xref_forward_history_empty = size(xref_history%forward) == 0
    end function is_xref_forward_history_empty

    ! Display BUFFER in the selected window (pop-to-buffer with
    ! display-buffer-same-window).  Port of xref.el:xref--switch-to-buffer
    ! (the `xref-jump' category is not ported).
    recursive subroutine switch_to_xref_buffer(buffer, status)
        type(handle_type), intent(in) :: buffer
        type(status_type), intent(out) :: status

        call pop_to_buffer(buffer, status, actions=[DISPLAY_ACTION_SAME_WINDOW])
    end subroutine switch_to_xref_buffer

    ! Go to the marker on top of FROM, pushing a marker at point onto TO;
    ! EMPTY is the error when FROM is empty.  Port of the body of
    ! xref.el:xref-go-back and xref-go-forward.
    recursive subroutine go_through_history(from, to, empty, status)
        type(handle_type), allocatable, intent(inout) :: from(:), to(:)
        character(len=*), intent(in) :: empty
        type(status_type), intent(out) :: status
        type(handle_type) :: marker, buffer, pm
        type(status_type) :: st
        integer(int64) :: pos

        status = status_type()
        if (.not. allocated(from)) allocate (from(0))

        if (size(from) == 0) then
            call set_error(status, FM_USER_ERROR, empty)
            return
        end if

        call pop_marker(from, marker)
        call point_marker(pm)
        call push_marker(to, pm)
        call marker_buffer(marker, buffer, st)

        if (buffer%index == 0) then
            call destroy_marker(marker, st)
            call set_error(status, FM_USER_ERROR, "The marked buffer has been deleted")
            return
        end if

        call switch_to_xref_buffer(buffer, status)

        if (.not. status%ok()) then
            call destroy_marker(marker, st)
            return
        end if

        call marker_position(marker, pos, st)
        call goto_char(pos)
        call destroy_marker(marker, st)
        call xref_after_return_hook%execute(status)
    end subroutine go_through_history

    !> Go back to the previous position in xref history.
    !> To undo, use \[go_forward_in_xref_history].  Port of xref.el:xref-go-back.
    recursive subroutine go_back_in_xref_history(status)
        type(status_type), intent(out) :: status

        if (.not. allocated(xref_history%forward)) allocate (xref_history%forward(0))
        call go_through_history(xref_history%backward, xref_history%forward, "At start of xref history", status)
    end subroutine go_back_in_xref_history

    !> Go to the point where a previous \[go_back_in_xref_history] was invoked.
    !> Port of xref.el:xref-go-forward.
    recursive subroutine go_forward_in_xref_history(status)
        type(status_type), intent(out) :: status

        if (.not. allocated(xref_history%backward)) allocate (xref_history%backward(0))
        call go_through_history(xref_history%forward, xref_history%backward, "At end of xref history", status)
    end subroutine go_forward_in_xref_history

    ! Go to POS, widening when it is outside the accessible part of the
    ! buffer and `widen_automatically' is true.  Port of
    ! xref.el:xref--goto-char.
    subroutine goto_xref_char(pos, status)
        integer(int64), intent(in) :: pos
        type(status_type), intent(out) :: status

        status = status_type()

        if (pos < point_min() .or. pos > point_max()) then
            if (.not. widen_automatically) then
                call set_error(status, FM_USER_ERROR, "Position is outside accessible part of buffer")
                return
            end if

            call widen()
        end if

        call goto_char(pos)
    end subroutine goto_xref_char

    ! The buffer and position of MARKER.  Not a port.
    subroutine marker_place(marker, buffer, pos)
        type(handle_type), intent(in) :: marker
        type(handle_type), intent(out) :: buffer
        integer(int64), intent(out) :: pos
        type(status_type) :: st

        call marker_buffer(marker, buffer, st)
        call marker_position(marker, pos, st)
    end subroutine marker_place

    ! Run `xref_after_jump_hook' with ITEM as `xref_current_item'.
    ! Not a port.
    recursive subroutine execute_after_jump_hook(item, status)
        type(xref_item_type), intent(in) :: item
        type(status_type), intent(out) :: status
        type(xref_item_type) :: saved

        saved = xref_current_item
        xref_current_item = item
        call xref_after_jump_hook%execute(status)
        xref_current_item = saved
    end subroutine execute_after_jump_hook

    !> Go to the location of ITEM and display the buffer.
    !> ACTION controls how the buffer is displayed:
    !>   XREF_DISPLAY_ACTION_NONE   -- `switch_to_buffer'
    !>   XREF_DISPLAY_ACTION_WINDOW -- `pop_to_buffer' (other window)
    !> Port of xref.el:xref-pop-to-location (`frame' is not ported).
    recursive subroutine pop_to_xref_location(item, action, status)
        type(xref_item_type), intent(in) :: item
        integer, intent(in) :: action
        type(status_type), intent(out) :: status
        type(handle_type) :: marker, buffer
        type(status_type) :: st
        integer(int64) :: pos

        call item%location%marker(marker, status)
        if (.not. status%ok()) return
        call marker_place(marker, buffer, pos)
        call destroy_marker(marker, st)

        if (action == XREF_DISPLAY_ACTION_WINDOW) then
            call switch_to_buffer_other_window(buffer, status)
        else
            call switch_to_xref_buffer(buffer, status)
        end if

        if (.not. status%ok()) return
        call goto_xref_char(pos, status)
        if (.not. status%ok()) return
        call execute_after_jump_hook(item, status)
    end subroutine pop_to_xref_location

    ! `recenter' with no argument, as a function of `xref_after_jump_hook'.
    ! Not a port.
    recursive subroutine recenter_adapter(status)
        type(status_type), intent(out) :: status

        call recenter(prefix_arg_type(PREFIX_NONE, 0_int64), .false., status)
    end subroutine recenter_adapter

    ! ------------------------------------------------------------------
    ! The *xref* buffer
    ! ------------------------------------------------------------------

    ! The index of the current buffer's *xref* state (0 for none).
    ! Not a port.
    integer function xref_buffer_index() result(k)
        k = 0
        if (current == 0 .or. .not. allocated(xref_buffers)) return
        k = findloc(xref_buffers%serial, buffers(current)%serial, dim=1)
    end function xref_buffer_index

    ! The current buffer's *xref* state, made if it has none: its index K.
    ! Not a port.
    subroutine make_xref_buffer_entry(k)
        integer, intent(out) :: k
        type(xref_buffer_entry_type), allocatable :: grown(:)
        integer :: n

        k = xref_buffer_index()
        if (k /= 0) return
        if (.not. allocated(xref_buffers)) allocate (xref_buffers(0))
        k = findloc(xref_buffers%serial, 0_int64, dim=1)

        if (k == 0) then
            n = size(xref_buffers)
            allocate (grown(n + 1))
            grown(1:n) = xref_buffers
            call move_alloc(grown, xref_buffers)
            k = n + 1
        else
            if (allocated(xref_buffers(k)%items)) deallocate (xref_buffers(k)%items)
            if (allocated(xref_buffers(k)%fetcher)) deallocate (xref_buffers(k)%fetcher)
            xref_buffers(k)%original_window = null_handle()
            xref_buffers(k)%original_window_intent = XREF_DISPLAY_ACTION_NONE
            xref_buffers(k)%found_match_count = 0
        end if

        xref_buffers(k)%serial = buffers(current)%serial
    end subroutine make_xref_buffer_entry

    !> The items the *xref* buffer BUFFER shows, in order (none if it shows
    !> none).  Not a port.
    subroutine xref_items_at(buffer, items)
        type(handle_type), intent(in) :: buffer
        type(xref_item_type), allocatable, intent(out) :: items(:)
        type(handle_type) :: old
        type(status_type) :: st
        integer :: k

        allocate (items(0))
        old = current_buffer()
        call set_buffer(buffer, st)
        if (.not. st%ok()) return
        k = xref_buffer_index()
        if (k /= 0 .and. allocated(xref_buffers(k)%items)) items = xref_buffers(k)%items
        call set_buffer(old, st)
    end subroutine xref_items_at

    ! The value of PROPERTY at POS, as an integer (0 for none) and whether
    ! it has one.  Not a port.
    subroutine property_at(pos, property, value, found)
        integer(int64), intent(in) :: pos
        character(len=*), intent(in) :: property
        integer, intent(out) :: value
        logical, intent(out) :: found
        type(value_type) :: v
        type(status_type) :: st

        value = 0
        found = .false.
        if (pos < point_min() .or. pos >= point_max()) return
        call get_text_property(pos, value_type(name=property), v, st)
        found = allocated(v%a)
        if (found .and. vtag(v) == V_INTEGER) value = int(xfixnum(v))
    end subroutine property_at

    !> Search the next text range where text property PROPERTY is set.
    !> INDEX is its value (an item's index) and FOUND whether there is
    !> one; if BACKWARD, search backward.  Point stays when there is none.
    !> Port of xref.el:xref--search-property.
    subroutine search_xref_property(property, backward, index, found)
        character(len=*), intent(in) :: property
        logical, intent(in) :: backward
        integer, intent(out) :: index
        logical, intent(out) :: found
        type(value_type) :: invisible
        integer(int64) :: start
        logical :: visible

        start = point()
        index = 0
        found = .false.

        do
            if (backward) then
                call goto_char(previous_single_char_property_change(point(), value_type(name=property)))
            else
                call goto_char(next_single_char_property_change(point(), value_type(name=property)))
            end if

            invisible = get_char_property(point(), value_type(name=P_INVISIBLE))
            visible = vtag(invisible) == V_NULL
            if (.not. visible) visible = is_ellipsis(invisible)
            if (visible) call property_at(point(), property, index, found)
            if (found .or. is_eob() .or. is_bob()) exit
        end do

        if (.not. found) call goto_char(start)

    contains

        logical function is_ellipsis(v)
            type(value_type), intent(in) :: v
            type(value_type) :: probe

            probe = value_type(name=INVISIBLE_ELLIPSIS)
            is_ellipsis = .false.
            if (vtag(v) /= vtag(probe)) return
            is_ellipsis = v%a(1)%s == probe%a(1)%s
        end function is_ellipsis
    end subroutine search_xref_property

    ! The index of the item at point (0 for none): at the end of a line,
    ! the one before point.  Port of xref.el:xref--item-at-point.
    integer function xref_item_at_point() result(index)
        integer(int64) :: pos
        logical :: found

        pos = point()
        if (is_eol() .and. pos > point_min()) pos = pos - 1
        call property_at(pos, P_XREF_ITEM, index, found)
        if (index /= 0 .and. xref_buffer_index() == 0) index = 0
    end function xref_item_at_point

    ! The item of index INDEX in the current *xref* buffer.  Not a port.
    function buffer_item(index) result(item)
        integer, intent(in) :: index
        type(xref_item_type) :: item
        integer :: k

        k = xref_buffer_index()
        if (k == 0) return
        if (.not. allocated(xref_buffers(k)%items)) return
        if (index < 1 .or. index > size(xref_buffers(k)%items)) return
        item = xref_buffers(k)%items(index)
    end function buffer_item

    !> Go to POS of TARGET_BUFFER, displayed in a window, honoring the *xref*
    !> buffer's original window intent; run `xref_after_jump_hook' and
    !> give the WINDOW.  Port of xref.el:xref--show-pos-in-buf (the `frame'
    !> intent is not ported).
    recursive subroutine show_xref_position_in_buffer(pos, target_buffer, item, window, status)
        integer(int64), intent(in) :: pos
        type(handle_type), intent(in) :: target_buffer
        type(xref_item_type), intent(in) :: item
        type(handle_type), intent(out) :: window
        type(status_type), intent(out) :: status
        type(display_action_parameters_type) :: table
        type(handle_type) :: original, old_window, xref_window
        type(status_type) :: st
        integer :: k, window_intent, dedication
        logical :: use_original

        window = null_handle()
        k = xref_buffer_index()
        original = null_handle()
        window_intent = XREF_DISPLAY_ACTION_NONE

        if (k /= 0) then
            original = xref_buffers(k)%original_window
            window_intent = xref_buffers(k)%original_window_intent
        end if

        old_window = selected_window()

        if (window_intent == XREF_DISPLAY_ACTION_WINDOW .and. is_window_live(original)) then
            ! xref--display-buffer-in-other-window, in
            ! xref--with-dedicated-window.
            xref_window = get_buffer_window(current_buffer())

            if (xref_window%index /= 0) then
                dedication = window_dedication(xref_window, st)
                call set_window_dedication(xref_window, DEDICATED_WEAK, st)
            end if

            call select_window(original, status, norecord=.true.)

            if (status%ok()) then
                table%inhibit_same_window = .true.
                call display_buffer(target_buffer, window, status, table=table)
            end if

            call select_window(old_window, st, norecord=.true.)
            if (xref_window%index /= 0) call set_window_dedication(xref_window, dedication, st)
        else
            use_original = is_window_live(original)

            if (use_original) then
                dedication = window_dedication(original, st)

                if (dedication /= DEDICATED_NONE) then
                    use_original = is_same_buffer(window_buffer(original, st), target_buffer)
                end if
            end if

            if (use_original) then
                ! xref--display-buffer-in-window.
                call select_window(original, status, norecord=.true.)
                if (status%ok()) call display_buffer(target_buffer, window, status, actions=[DISPLAY_ACTION_SAME_WINDOW])
                call select_window(old_window, st, norecord=.true.)
            else
                call display_buffer(target_buffer, window, status)
            end if
        end if

        if (.not. status%ok()) return
        if (window%index == 0) return
        ! with-selected-window.
        call select_window(window, status, norecord=.true.)
        if (.not. status%ok()) return
        call goto_xref_char(pos, status)
        if (status%ok()) call execute_after_jump_hook(item, status)
        window = selected_window()
        call select_window(old_window, st, norecord=.true.)
    end subroutine show_xref_position_in_buffer

    ! Whether A and B are the same buffer.  Not a port.
    logical function is_same_buffer(a, b)
        type(handle_type), intent(in) :: a, b

        is_same_buffer = a%index == b%index .and. a%generation == b%generation
    end function is_same_buffer

    ! Go to the location of ITEM, selecting its window when SELECT is
    ! SELECT_YES or SELECT_QUIT (quitting the *xref* window first for
    ! SELECT_QUIT).  A `user_error' is shown as a message.  Port of
    ! xref.el:xref--show-location.
    recursive subroutine show_xref_location(item, select, status)
        type(xref_item_type), intent(in) :: item
        integer, intent(in) :: select
        type(status_type), intent(out) :: status
        type(handle_type) :: marker, target_buffer, xref_buffer, window, old_window
        type(status_type) :: st
        integer(int64) :: pos

        status = status_type()
        call item%location%marker(marker, status)

        if (.not. status%ok()) then
            if (status%code == FM_USER_ERROR) then
                call message(format_error(status))
                status = status_type()
            end if

            return
        end if

        call marker_place(marker, target_buffer, pos)
        call destroy_marker(marker, st)
        xref_buffer = current_buffer()

        if (select /= SELECT_NO) then
            if (select == SELECT_QUIT) call quit_window(.false., status)
            if (.not. status%ok()) return
            call set_buffer(xref_buffer, st)
            call show_xref_position_in_buffer(pos, target_buffer, item, window, status)
            if (status%ok() .and. window%index /= 0) call select_window(window, status)
        else
            old_window = selected_window()
            call show_xref_position_in_buffer(pos, target_buffer, item, window, status)
            call select_window(old_window, st, norecord=.true.)
            call set_buffer(xref_buffer, st)
        end if

        if (.not. status%ok()) then
            if (status%code == FM_USER_ERROR) then
                call message(format_error(status))
                status = status_type()
            end if
        end if
    end subroutine show_xref_location

    !> Display the source of xref at point in the appropriate window, if
    !> any.  Port of xref.el:xref-show-location-at-point (the overlay arrow
    !> is not ported).
    recursive subroutine show_xref_location_at_point(status)
        type(status_type), intent(out) :: status
        integer :: index

        status = status_type()
        index = xref_item_at_point()
        if (index == 0) return
        call show_xref_location(buffer_item(index), SELECT_NO, status)
    end subroutine show_xref_location_at_point

    !> Move to the next xref but don't display its source.  Port of
    !> xref.el:xref-next-line-no-show.
    recursive subroutine next_xref_line_no_show(status)
        type(status_type), intent(out) :: status
        integer :: index
        logical :: found

        status = status_type()
        call search_xref_property(P_XREF_ITEM, .false., index, found)
    end subroutine next_xref_line_no_show

    !> Move to the next xref and display its source in the appropriate
    !> window.  Port of xref.el:xref-next-line.
    recursive subroutine next_xref_line(status)
        type(status_type), intent(out) :: status

        call next_xref_line_no_show(status)
        call show_xref_location_at_point(status)
    end subroutine next_xref_line

    !> Move to the previous xref but don't display its source.  Port of
    !> xref.el:xref-prev-line-no-show.
    recursive subroutine previous_xref_line_no_show(status)
        type(status_type), intent(out) :: status
        integer :: index
        logical :: found

        status = status_type()
        call search_xref_property(P_XREF_ITEM, .true., index, found)
    end subroutine previous_xref_line_no_show

    !> Move to the previous xref and display its source in the appropriate
    !> window.  Port of xref.el:xref-prev-line.
    recursive subroutine previous_xref_line(status)
        type(status_type), intent(out) :: status

        call previous_xref_line_no_show(status)
        call show_xref_location_at_point(status)
    end subroutine previous_xref_line

    !> Move to the first item of the next xref group and display its
    !> source.  Port of xref.el:xref-next-group.
    recursive subroutine next_xref_group(status)
        type(status_type), intent(out) :: status
        integer :: index
        logical :: found

        call search_xref_property(P_XREF_GROUP, .false., index, found)
        call search_xref_property(P_XREF_ITEM, .false., index, found)
        call show_xref_location_at_point(status)
    end subroutine next_xref_group

    !> Move to the first item of the previous xref group and display its
    !> source.  Port of xref.el:xref-prev-group.
    recursive subroutine previous_xref_group(status)
        type(status_type), intent(out) :: status
        integer :: index
        logical :: found

        ! Search for the xref group of the current item, provided that the
        ! point is not already in an xref group.
        call property_at(point(), P_XREF_GROUP, index, found)
        if (.not. found) call search_xref_property(P_XREF_GROUP, .true., index, found)
        ! Search for the previous xref group.
        call search_xref_property(P_XREF_GROUP, .true., index, found)
        call search_xref_property(P_XREF_ITEM, .false., index, found)
        call show_xref_location_at_point(status)
    end subroutine previous_xref_group

    !> Jump to the xref on the current line and select its window.
    !> If QUIT is true (interactively, with prefix argument), also
    !> quit the *xref* buffer.  Port of xref.el:xref-goto-xref (the overlay
    !> arrow is not ported).
    recursive subroutine goto_xref(quit, status)
        logical, intent(in) :: quit
        type(status_type), intent(out) :: status
        type(handle_type) :: buffer
        integer :: index

        status = status_type()
        buffer = current_buffer()
        index = xref_item_at_point()

        if (index == 0) then
            call set_error(status, FM_USER_ERROR, "Choose a reference to visit")
            return
        end if

        if (quit) then
            call show_xref_location(buffer_item(index), SELECT_QUIT, status)
        else
            call show_xref_location(buffer_item(index), SELECT_YES, status)
        end if

        if (status%ok()) call next_error_found(status, buffer, current_buffer())
    end subroutine goto_xref

    ! The command: `goto_xref' with QUIT from the prefix argument.
    ! Not a port.
    recursive subroutine cmd_goto_xref(status)
        type(status_type), intent(out) :: status

        call goto_xref(current_prefix_arg%kind /= PREFIX_NONE, status)
    end subroutine cmd_goto_xref

    !> Quit *xref* buffer, then jump to xref on current line.  Port of
    !> xref.el:xref-quit-and-goto-xref.
    recursive subroutine quit_and_goto_xref(status)
        type(status_type), intent(out) :: status

        call goto_xref(.true., status)
    end subroutine quit_and_goto_xref

    !> Quit *xref* buffer, then pop the xref marker stack.  Port of
    !> xref.el:xref-quit-and-pop-marker-stack.
    recursive subroutine quit_and_pop_xref_marker_stack(status)
        type(status_type), intent(out) :: status

        call quit_window(.false., status)
        if (status%ok()) call go_back_in_xref_history(status)
    end subroutine quit_and_pop_xref_marker_stack

    !> Move by N xrefs (back for a negative N) and show the location;
    !> RESET goes to the beginning first.  Port of
    !> xref.el:xref--next-error-function (the overlay arrow is not ported).
    recursive subroutine xref_next_error_function(n, reset, status)
        integer(int64), intent(in) :: n
        logical, intent(in) :: reset
        type(status_type), intent(out) :: status
        type(handle_type) :: win
        type(status_type) :: st
        integer(int64) :: i
        integer :: index
        logical :: backward, found

        status = status_type()
        if (reset) call goto_char(point_min())
        backward = n < 0
        index = 0

        if (n == 0) then
            call property_at(point(), P_XREF_ITEM, index, found)
        else
            do i = 1, abs(n)
                call search_xref_property(P_XREF_ITEM, backward, index, found)
            end do
        end if

        if (index == 0) then
            if (backward) then
                call set_error(status, FM_USER_ERROR, "No previous xref")
            else
                call set_error(status, FM_USER_ERROR, "No next xref")
            end if

            return
        end if

        ! Save the current position (when the buffer is visible, it gets
        ! reset to that window's point from time to time).
        win = get_buffer_window(current_buffer())
        if (win%index /= 0) call set_window_point(windows(win%index), point(), st)
        call show_xref_location(buffer_item(index), SELECT_YES, status)
    end subroutine xref_next_error_function

    !> Make the part of the line at point past `xref_truncation_width'
    !> columns invisible, around point.  Port of
    !> xref.el:xref--apply-truncation.
    recursive subroutine apply_xref_truncation(status)
        type(status_type), intent(out) :: status
        type(value_type) :: v
        type(status_type) :: st
        integer(int64) :: bol, eol, pos, adjusted_bol, width
        logical :: ro, apply, shown_eol, shown_bol, hidden_point

        status = status_type()
        if (xref_truncation_width == XREF_TRUNCATION_WIDTH_NONE) return
        bol = line_beginning_position()
        eol = line_end_position()
        width = xref_truncation_width
        if (eol - bol <= width) return
        call get_text_property(eol - 1, value_type(name=P_INVISIBLE), v, st)
        shown_eol = .not. allocated(v%a)
        call get_text_property(bol, value_type(name=P_INVISIBLE), v, st)
        shown_bol = .not. allocated(v%a)
        call get_text_property(point(), value_type(name=P_INVISIBLE), v, st)
        hidden_point = allocated(v%a)
        ! Either truncation not applied yet, or it hides the current
        ! position: need to refresh.
        apply = (shown_eol .and. shown_bol) .or. hidden_point
        if (.not. apply) return
        ro = buffers(current)%read_only
        buffers(current)%read_only = .false.
        call get_text_property(bol, value_type(name=P_FACE), v, st)
        adjusted_bol = bol

        if (is_name(v, FACE_XREF_LINE_NUMBER)) then
            adjusted_bol = next_single_char_property_change(bol, value_type(name=P_FACE))
        end if

        if (point() - bol < width) then
            pos = bol + width
            call remove_invisible(bol, pos)
            call put_text_property(pos, eol, value_type(name=P_INVISIBLE), value_type(name=INVISIBLE_ELLIPSIS), st)
        else if (eol - point() < width) then
            pos = eol - width
            call remove_invisible(pos, eol)
            call put_text_property(adjusted_bol, pos, value_type(name=P_INVISIBLE), value_type(name=INVISIBLE_ELLIPSIS), &
                                   st)
        else
            pos = point() - width/2
            call put_text_property(adjusted_bol, pos, value_type(name=P_INVISIBLE), value_type(name=INVISIBLE_ELLIPSIS), &
                                   st)
            call remove_invisible(pos, pos + width)
            call put_text_property(pos + width, eol, value_type(name=P_INVISIBLE), value_type(name=INVISIBLE_ELLIPSIS), &
                                   st)
        end if

        buffers(current)%read_only = ro

    contains

        subroutine remove_invisible(b, e)
            integer(int64), intent(in) :: b, e
            logical :: changed
            type(status_type) :: s

            call remove_list_of_text_properties(b, e, list(value_type(name=P_INVISIBLE)), changed, s)
        end subroutine remove_invisible
    end subroutine apply_xref_truncation

    ! Whether V is the name NAME.  Not a port.
    logical function is_name(v, name)
        type(value_type), intent(in) :: v
        character(len=*), intent(in) :: name
        type(value_type) :: probe

        probe = value_type(name=name)
        is_name = .false.
        if (vtag(v) /= vtag(probe)) return
        is_name = v%a(1)%s == probe%a(1)%s
    end function is_name

    ! `apply_xref_truncation' as a function of `post_command_hook' in the
    ! *xref* buffer.  Not a port.
    recursive subroutine apply_truncation_hook(status)
        type(status_type), intent(out) :: status

        status = status_type()
        if (xref_buffer_index() /= 0) call apply_xref_truncation(status)
    end subroutine apply_truncation_hook

    !> Insert GROUPS in the current buffer: a header line for each group,
    !> then a line for each of its items (several matches on one line are
    !> rendered together), with the items' index in the buffer's state as
    !> their `xref_item' property.  Port of xref.el:xref--insert-xrefs.
    recursive subroutine insert_xrefs(groups, status)
        type(xref_group_type), intent(in) :: groups(:)
        type(status_type), intent(out) :: status
        type(status_type) :: st
        character(len=:), allocatable :: prev_group, prefix
        character(len=24) :: digits
        integer(int64) :: line, prev_line, max_line, start, prefix_end, mb
        integer :: g, i, k, width, n
        logical :: has_prev, changed

        status = status_type()
        call make_xref_buffer_entry(k)
        if (allocated(xref_buffers(k)%items)) deallocate (xref_buffers(k)%items)
        allocate (xref_buffers(k)%items(0))
        xref_buffers(k)%found_match_count = 0
        has_prev = .false.
        prev_line = 0
        prev_group = ""

        do g = 1, size(groups)
            max_line = 0

            do i = 1, size(groups(g)%items)
                max_line = max(max_line, groups(g)%items(i)%location%line_number())
            end do

            width = 1
            if (max_line > 0) width = 1 + floor(log10(real(max_line)))
            start = point()
            call insert(groups(g)%name, status)
            if (.not. status%ok()) return
            call put_text_property(start, point(), value_type(name=P_FACE), value_type(name=FACE_XREF_FILE_HEADER), st)
            call put_text_property(start, point(), value_type(name=P_XREF_GROUP), value_type(.true.), st)
            call insert(LF, status)

            do i = 1, size(groups(g)%items)
                call append_xref_item(xref_buffers(k)%items, groups(g)%items(i))
                n = size(xref_buffers(k)%items)
                xref_buffers(k)%found_match_count = xref_buffers(k)%found_match_count + 1
                line = groups(g)%items(i)%location%line_number()

                if (line == 0) then
                    prefix = "  "
                else if (has_prev .and. line == prev_line .and. prev_group == groups(g)%name) then
                    prefix = ""
                else
                    write (digits, "(i0)") line
                    prefix = repeat(" ", max(0, width - len_trim(digits)))//trim(digits)//":"
                end if

                ! Render multiple matches on the same line, together.
                if (has_prev .and. prev_group == groups(g)%name) then
                    if (line == 0 .or. line /= prev_line) call insert(LF, status)
                end if

                start = point()
                call insert(prefix, status)
                prefix_end = point()

                if (line /= 0 .and. len(prefix) > 0) then
                    call put_text_property(start, prefix_end - 1, value_type(name=P_FACE), &
                                           value_type(name=FACE_XREF_LINE_NUMBER), st)
                    call put_text_property(prefix_end - 1, prefix_end, value_type(name=P_FACE), &
                                           value_type(name=FACE_SHADOW), st)
                end if

                call insert(groups(g)%items(i)%summary, status)
                if (.not. status%ok()) return

                if (groups(g)%items(i)%length /= XREF_MATCH_LENGTH_NONE) then
                    mb = prefix_end + groups(g)%items(i)%match_beginning

                    if (groups(g)%items(i)%length > 0) then
                        call put_text_property(mb, min(mb + groups(g)%items(i)%length, point()), &
                                               value_type(name=P_FACE), value_type(name=FACE_XREF_MATCH), st)
                    end if
                end if

                if (point() > start) then
                    call add_text_properties(start, point(), &
                                             list(value_type(name=P_XREF_ITEM), value_type(int(n, int64)), &
                                                  value_type(name=P_MOUSE_FACE), value_type(name=FACE_HIGHLIGHT), &
                                                  value_type(name=P_HELP_ECHO), &
                                                  value_type(text="mouse-2: display in another window, "// &
                                                             "RET or mouse-1: follow reference")), changed, st)
                end if

                prev_line = line
                prev_group = groups(g)%name
                has_prev = .true.
            end do

            call insert(LF, status)
        end do

        call add_to_invisibility_spec(INVISIBLE_ELLIPSIS, .true.)
        start = point()
        call goto_char(point_min())

        do while (forward_line(1_int64) == 0)
            call apply_xref_truncation(st)
        end do

        call goto_char(start)
        call xref_after_update_hook%execute(status)
    end subroutine insert_xrefs

    !> GROUP formatted in the preferred style, given the PROJECT_ROOT ("" for
    !> none).  The style is determined by the value of
    !> `xref_file_name_display'.  If GROUP looks like a file name, its value
    !> is formatted according to that style.  Otherwise it is returned
    !> unchanged.  Port of xref.el:xref--group-name-for-display.
    function xref_group_name_for_display(group, project_root) result(name)
        character(len=*), intent(in) :: group, project_root
        character(len=:), allocatable :: name
        type(status_type) :: st

        select case (xref_file_name_display)
        case (XREF_FILE_NAME_DISPLAY_ABSOLUTE)
            if (is_file_name_absolute(group)) then
                name = group
            else
                call expand_file_name(group, name, st)
            end if
        case (XREF_FILE_NAME_DISPLAY_NONDIRECTORY)
            name = file_name_nondirectory(group)
        case default
            name = group

            if (len(project_root) > 0 .and. index(group, project_root) == 1) then
                name = group(len(project_root) + 1:)
            end if
        end select
    end function xref_group_name_for_display

    !> Find common groups in ITEMS (by their location's group, in order)
    !> and format the group names.  Port of xref.el:xref--analyze (the
    !> project's root, project.el, is not ported: none).
    subroutine analyze_xrefs(items, groups)
        type(xref_item_type), intent(in) :: items(:)
        type(xref_group_type), allocatable, intent(out) :: groups(:)
        type(xref_group_type), allocatable :: grown(:)
        character(len=:), allocatable :: key
        integer :: i, i2, g, n
        logical, allocatable :: done(:)

        allocate (groups(0))
        n = size(items)
        allocate (done(n))
        done = .false.

        do i = 1, n
            if (done(i)) cycle
            key = items(i)%location%group()
            allocate (grown(size(groups) + 1))
            if (size(groups) > 0) grown(1:size(groups)) = groups
            g = size(grown)
            grown(g)%name = xref_group_name_for_display(key, "")
            allocate (grown(g)%items(0))

            do i2 = i, n
                if (done(i2)) cycle

                if (items(i2)%location%group() == key) then
                    call append_xref_item(grown(g)%items, items(i2))
                    done(i2) = .true.
                end if
            end do

            call move_alloc(grown, groups)
        end do
    end subroutine analyze_xrefs

    ! Set up the current *xref* buffer from GROUPS for FETCHER and
    ! PARAMETERS.  Port of xref.el:xref--show-common-initialize.
    recursive subroutine initialize_xref_buffer(groups, fetcher, parameters, status)
        type(xref_group_type), intent(in) :: groups(:)
        class(xref_fetcher_type), intent(in) :: fetcher
        type(xref_show_parameters_type), intent(in) :: parameters
        type(status_type), intent(out) :: status
        logical :: ro, undo
        integer :: k

        ro = buffers(current)%read_only
        undo = buffers(current)%undo_enabled
        buffers(current)%read_only = .false.
        buffers(current)%undo_enabled = .false.
        if (allocated(buffers(current)%undo_entries)) deallocate (buffers(current)%undo_entries)
        call erase_buffer(status)
        if (status%ok()) call insert_xrefs(groups, status)
        call post_command_hook%prepend(apply_truncation_hook, local=.true.)
        call goto_char(point_min())
        call make_xref_buffer_entry(k)
        xref_buffers(k)%original_window = parameters%window
        xref_buffers(k)%original_window_intent = parameters%display_action
        if (allocated(xref_buffers(k)%fetcher)) deallocate (xref_buffers(k)%fetcher)
        allocate (xref_buffers(k)%fetcher, source=fetcher)
        buffers(current)%read_only = ro
        buffers(current)%undo_enabled = undo
        call update_mode_line()
    end subroutine initialize_xref_buffer

    ! The mode line process of the current *xref* buffer: the number of
    ! matches.  Port of xref.el xref-mode-line-matches.
    subroutine update_mode_line()
        type(mode_line_part_type) :: parts(3)
        character(len=24) :: digits
        integer :: k

        k = xref_buffer_index()
        if (k == 0) return
        write (digits, "(i0)") xref_buffers(k)%found_match_count
        parts(1)%text = " ["
        parts(1)%face = ""
        parts(2)%text = trim(digits)
        parts(2)%face = xref_match_count_face
        parts(3)%text = "]"
        parts(3)%face = ""
        call set_mode_line_process(parts)
    end subroutine update_mode_line

    !> Show what FETCHER fetches (or the items PARAMETERS has already
    !> fetched) in the *xref* buffer, in Xref mode, and pop to it.  Port of
    !> xref.el:xref--show-xref-buffer.
    recursive subroutine show_xref_buffer(fetcher, parameters, buffer, status)
        class(xref_fetcher_type), intent(inout) :: fetcher
        type(xref_show_parameters_type), intent(in) :: parameters
        type(handle_type), intent(out) :: buffer
        type(status_type), intent(out) :: status

        call show_xref_buffer_in_mode(fetcher, parameters, .false., buffer, status)
    end subroutine show_xref_buffer

    ! `show_xref_buffer' in Xref Transient mode if TRANSIENT.  Not a port.
    recursive subroutine show_xref_buffer_in_mode(fetcher, parameters, transient, buffer, status)
        class(xref_fetcher_type), intent(inout) :: fetcher
        type(xref_show_parameters_type), intent(in) :: parameters
        logical, intent(in) :: transient
        type(handle_type), intent(out) :: buffer
        type(status_type), intent(out) :: status
        type(xref_item_type), allocatable :: items(:)
        type(xref_group_type), allocatable :: groups(:)
        character(len=:), allocatable :: dd
        type(status_type) :: st

        buffer = null_handle()

        if (allocated(parameters%fetched)) then
            items = parameters%fetched
            status = status_type()
        else
            call fetcher%fetch(items, status)
            if (.not. status%ok()) return
        end if

        call analyze_xrefs(items, groups)
        dd = ""
        if (allocated(buffers(current)%directory)) dd = buffers(current)%directory
        call get_or_make_buffer(XREF_BUFFER_NAME, buffer, status)
        if (.not. status%ok()) return
        call set_buffer(buffer, st)
        if (len(dd) > 0) buffers(current)%directory = dd

        if (transient) then
            call xref_transient_buffer_mode(status)
        else
            call xref_xref_buffer_mode(status)
        end if

        if (.not. status%ok()) return
        call initialize_xref_buffer(groups, fetcher, parameters, status)
        if (.not. status%ok()) return
        call pop_to_buffer(buffer, status)
        if (.not. status%ok()) return
        call jump_to_first_xref(buffer, parameters%auto_jump, status)
    end subroutine show_xref_buffer_in_mode

    !> Refresh the search results in the current buffer.  Port of
    !> xref.el:xref--revert-buffer.
    recursive subroutine revert_xref_buffer(ignore_auto, noconfirm, status)
        logical, intent(in) :: ignore_auto, noconfirm
        type(status_type), intent(out) :: status
        type(xref_item_type), allocatable :: items(:)
        type(xref_group_type), allocatable :: groups(:)
        class(xref_fetcher_type), allocatable :: fetcher
        type(status_type) :: st
        integer(int64) :: opoint, start
        integer :: k
        logical :: ro, undo

        status = status_type()
        k = xref_buffer_index()
        if (k == 0) return
        if (.not. allocated(xref_buffers(k)%fetcher)) return
        allocate (fetcher, source=xref_buffers(k)%fetcher)
        opoint = point()
        ro = buffers(current)%read_only
        undo = buffers(current)%undo_enabled
        buffers(current)%read_only = .false.
        buffers(current)%undo_enabled = .false.
        call fetcher%fetch(items, status)

        if (status%ok()) then
            ! The fetcher keeps its state (a fixed list is fetched once).
            k = xref_buffer_index()
            deallocate (xref_buffers(k)%fetcher)
            allocate (xref_buffers(k)%fetcher, source=fetcher)
            call analyze_xrefs(items, groups)
            call erase_buffer(st)
            call insert_xrefs(groups, status)
        else if (status%code == FM_USER_ERROR) then
            call erase_buffer(st)
            start = point()
            call insert(format_error(status), st)
            call put_text_property(start, point(), value_type(name=P_FACE), value_type(name=FACE_ERROR), st)
            status = status_type()
        end if

        call goto_char(min(opoint, point_max()))
        buffers(current)%read_only = ro
        buffers(current)%undo_enabled = undo
        call update_mode_line()
    end subroutine revert_xref_buffer

    ! Go to the first result in TARGET_BUFFER according to VALUE (an
    ! XREF_AUTO_JUMP_* value).  Port of xref.el:xref--auto-jump-first.
    recursive subroutine jump_to_first_xref(target_buffer, value, status)
        type(handle_type), intent(in) :: target_buffer
        integer, intent(in) :: value
        type(status_type), intent(out) :: status
        type(handle_type) :: window
        integer(int64) :: shortage

        status = status_type()

        if (value /= XREF_AUTO_JUMP_NO) then
            window = get_buffer_window(target_buffer)
            if (window%index /= 0) call select_window(window, status)
            if (.not. status%ok()) return
            call goto_char(point_min())
        end if

        select case (value)
        case (XREF_AUTO_JUMP_YES)
            call next_xref_line_no_show(status)
            if (status%ok()) call goto_xref(.false., status)
        case (XREF_AUTO_JUMP_SHOW)
            call next_xref_line(status)
        case (XREF_AUTO_JUMP_MOVE)
            shortage = forward_line(1_int64)
        case default
        end select
    end subroutine jump_to_first_xref

    !> Show the definitions list in a regular window.
    !>
    !> When only one definition found, jump to it right away instead.
    !> Port of xref.el:xref-show-definitions-buffer.
    recursive subroutine show_xref_definitions_buffer(fetcher, parameters, buffer, status)
        class(xref_fetcher_type), intent(inout) :: fetcher
        type(xref_show_parameters_type), intent(in) :: parameters
        type(handle_type), intent(out) :: buffer
        type(status_type), intent(out) :: status
        type(xref_item_type), allocatable :: items(:)
        type(xref_show_parameters_type) :: with_fetched

        buffer = null_handle()
        call fetcher%fetch(items, status)
        if (.not. status%ok()) return

        if (size(items) <= 1) then
            if (size(items) == 1) call pop_to_xref_location(items(1), parameters%display_action, status)
            return
        end if

        with_fetched = parameters
        with_fetched%fetched = items
        call show_xref_buffer(fetcher, with_fetched, buffer, status)
        if (.not. status%ok()) return
        call jump_to_first_xref(buffer, parameters%auto_jump, status)
    end subroutine show_xref_definitions_buffer

    !> Let the user choose the target definition with completion.
    !>
    !> When there is more than one definition, let the user choose
    !> between them by typing in the minibuffer with completion.  Port of
    !> xref.el:xref-show-definitions-completing-read (the group function
    !> of the completion table's metadata is not ported).
    recursive subroutine show_xref_definitions_completing_read(fetcher, parameters, buffer, status)
        class(xref_fetcher_type), intent(inout) :: fetcher
        type(xref_show_parameters_type), intent(in) :: parameters
        type(handle_type), intent(out) :: buffer
        type(status_type), intent(out) :: status
        type(xref_item_type), allocatable :: items(:), ordered(:)
        type(xref_group_type), allocatable :: groups(:)
        type(value_type), allocatable :: candidates(:)
        character(len=:), allocatable :: answer, prefix, group, line_text
        character(len=24) :: digits
        type(string_type), allocatable :: texts(:)
        type(string_type) :: text
        integer :: g, i, n, prefix_length, chosen
        integer(int64) :: line

        buffer = null_handle()
        call fetcher%fetch(items, status)
        if (.not. status%ok()) return
        call analyze_xrefs(items, groups)
        allocate (ordered(0), texts(0))
        ! FIXME: Groups are not always file names, but they often are.
        prefix = common_prefix(groups)
        prefix_length = 0
        if (len(prefix) > 0) prefix_length = len(file_name_directory(prefix))

        do g = 1, size(groups)
            group = groups(g)%name(prefix_length + 1:)

            do i = 1, size(groups(g)%items)
                line = groups(g)%items(i)%location%line_number()
                line_text = ""

                if (line /= 0) then
                    write (digits, "(i0)") line
                    line_text = trim(digits)//":"
                end if

                text%text = group//":"//line_text//groups(g)%items(i)%summary
                texts = [texts, text]
                call append_xref_item(ordered, groups(g)%items(i))
            end do
        end do

        n = size(texts)
        if (n == 0) return

        if (n == 1) then
            chosen = 1
        else
            allocate (candidates(n))

            do i = 1, n
                candidates(i) = value_type(text=texts(i)%text)
            end do

            call completing_read("Choose definition: ", completion_list_table(make_list(candidates)), answer, status, &
                                 require_match=1, default=texts(1)%text)
            if (.not. status%ok()) return
            chosen = 0

            do i = 1, n
                if (texts(i)%text == answer) then
                    chosen = i
                    exit
                end if
            end do

            if (chosen == 0) return
        end if

        call pop_to_xref_location(ordered(chosen), parameters%display_action, status)

    contains

        ! (try-completion "" xref-alist): the common prefix of the names.
        function common_prefix(gs) result(p)
            type(xref_group_type), intent(in) :: gs(:)
            character(len=:), allocatable :: p
            integer :: j, m

            p = ""
            if (size(gs) == 0) return
            p = gs(1)%name

            do j = 2, size(gs)
                m = 0

                do while (m < min(len(p), len(gs(j)%name)))
                    if (p(m + 1:m + 1) /= gs(j)%name(m + 1:m + 1)) exit
                    m = m + 1
                end do

                p = p(1:m)
            end do
        end function common_prefix
    end subroutine show_xref_definitions_completing_read

    !> Major mode for displaying cross-references.  Port of
    !> xref.el:xref--xref-buffer-mode (Imenu, `add_log_current_subprogram_function',
    !> Outline minor mode and the restoring of point after a revert are
    !> not ported).
    recursive subroutine xref_xref_buffer_mode(status)
        type(status_type), intent(out) :: status
        logical :: saved

        call major_modes%xref_xref_buffer_mode%begin(saved, status)
        if (.not. status%ok()) return
        call xref_buffer_mode_body()
        call major_modes%xref_xref_buffer_mode%end(saved, status)
    end subroutine xref_xref_buffer_mode

    ! The body of `xref_xref_buffer_mode'.  Port of the body of
    ! xref.el:xref--xref-buffer-mode.
    subroutine xref_buffer_mode_body()
        buffers(current)%read_only = .true.
        call set_next_error_function(xref_next_error_function)
        next_error_last_buffer = current_buffer()
        call revert_buffer_function%set_local(revert_xref_buffer)
    end subroutine xref_buffer_mode_body

    !> Major mode derived from `xref_xref_buffer_mode' by
    !> `define_derived_mode', with RET bound to `quit_and_goto_xref'.  Port of
    !> xref.el:xref--transient-buffer-mode.
    recursive subroutine xref_transient_buffer_mode(status)
        type(status_type), intent(out) :: status
        logical :: saved

        call major_modes%xref_transient_buffer_mode%begin(saved, status)
        if (.not. status%ok()) return
        call major_modes%xref_transient_buffer_mode%end(saved, status)
    end subroutine xref_transient_buffer_mode

    ! ------------------------------------------------------------------
    ! Xref-Edit mode
    ! ------------------------------------------------------------------

    ! The next region from FROM where PROP has a value
    ! (text-property-search-forward).  Not a port.
    subroutine next_region(prop, from, b, e, found)
        character(len=*), intent(in) :: prop
        integer(int64), intent(in) :: from
        integer(int64), intent(out) :: b, e
        logical, intent(out) :: found
        type(status_type) :: st

        e = 0
        call text_property_not_all(from, point_max(), value_type(name=prop), value_type(), b, found, st)
        if (.not. found) return
        call next_single_property_change(b, value_type(name=prop), e, found, st)
        if (.not. found) e = point_max()
        found = .true.
    end subroutine next_region

    !> Mark relevant regions read-only, and add relevant occur
    !> text-properties.  Port of xref.el:xref-edit--prepare-buffer.
    recursive subroutine prepare_xref_edit_buffer(status)
        type(status_type), intent(out) :: status
        type(status_type) :: st
        integer(int64) :: pos, b, e, opoint, line_number_end
        logical :: found, changed, ro

        status = status_type()
        opoint = point()
        ro = buffers(current)%read_only
        buffers(current)%read_only = .false.
        pos = point_min()

        do
            call next_region(P_XREF_GROUP, pos, b, e, found)
            if (.not. found) exit
            call add_text_properties(b, e, list(value_type(name=P_READ_ONLY), value_type(.true.), &
                                                value_type(name=P_FRONT_STICKY), value_type(.true.)), changed, st)
            pos = e
        end do

        pos = point_min()

        do
            call next_region(P_XREF_ITEM, pos, b, e, found)
            if (.not. found) exit
            call goto_char(b)
            call goto_char(line_beginning_position())
            line_number_end = 0
            if (looking_at(" *[0-9]+:", st)) line_number_end = match_end(0, st)

            if (line_number_end > 0) then
                ! Allow insertion of text right after prefix, but not
                ! before.
                call add_text_properties(b, line_number_end, &
                                         list(value_type(name=P_READ_ONLY), value_type(.true.), &
                                              value_type(name=P_OCCUR_PREFIX), value_type(.true.), &
                                              value_type(name=P_FRONT_STICKY), value_type(.true.), &
                                              value_type(name=P_REAR_NONSTICKY), value_type(.true.)), changed, st)
            end if

            pos = e
        end do

        buffers(current)%read_only = ro
        call goto_char(opoint)
    end subroutine prepare_xref_edit_buffer

    !> Major mode for editing *xref* buffers.
    !> In this mode, changes to the *xref* buffer are applied to the
    !> originating files.
    !> \<xref_edit_mode_map>
    !> Type \[save_xref_edit_changes] to exit Xref-Edit mode, return to Xref
    !> mode.
    !>
    !> The only editable texts in an Xref-Edit buffer are the match results.
    !> Port of xref.el:xref-edit-mode.
    recursive subroutine xref_edit_mode(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call set_error(status, FM_GENERIC_ERROR, "This mode can be enabled only by `change_to_xref_edit_mode'")
    end subroutine xref_edit_mode

    !> Switch to `xref_edit_mode' to edit *xref* buffer.  Port of
    !> xref.el:xref-change-to-xref-edit-mode.
    recursive subroutine change_to_xref_edit_mode(status)
        type(status_type), intent(out) :: status

        status = status_type()

        if (.not. is_derived_mode(major_modes%xref_xref_buffer_mode)) then
            call set_error(status, FM_GENERIC_ERROR, "Not an Xref buffer")
            return
        end if

        call use_local_map(xref_edit_mode_map, status)
        if (.not. status%ok()) return
        call prepare_xref_edit_buffer(status)
        if (.not. status%ok()) return
        buffers(current)%read_only = .false.
        buffers(current)%major_mode = "xref_edit_mode"
        buffers(current)%mode_name = "Xref-Edit"
        ! buffer-enable-undo.
        buffers(current)%undo_enabled = .true.
        call restore_buffer_modified(.false.)
        if (allocated(buffers(current)%undo_entries)) deallocate (buffers(current)%undo_entries)
        call before_change_functions%prepend(xref_edit_before_change_function, local=.true.)
        call after_change_functions%prepend(occur_after_change_function, local=.true.)
        call execute_mode_hooks(status, xref_edit_mode_hook)
        if (.not. status%ok()) return
        call message(substitute_command_keys("Editing: Type \[save_xref_edit_changes] to return to Xref mode"))
    end subroutine change_to_xref_edit_mode

    !> Switch back to Xref mode.  Port of xref.el:xref-edit-save-changes.
    recursive subroutine save_xref_edit_changes(status)
        type(status_type), intent(out) :: status
        type(status_type) :: st
        logical :: changed

        status = status_type()

        if (.not. is_derived_mode("xref_edit_mode")) then
            call set_error(status, FM_GENERIC_ERROR, "Not a Xref-Edit buffer")
            return
        end if

        call before_change_functions%remove(xref_edit_before_change_function, local=.true.)
        call after_change_functions%remove(occur_after_change_function, local=.true.)
        call use_local_map(xref_xref_buffer_mode_map, status)
        buffers(current)%read_only = .true.
        buffers(current)%major_mode = "xref_xref_buffer_mode"
        buffers(current)%mode_name = "Xref"
        ! buffer-disable-undo.
        buffers(current)%undo_enabled = .false.
        if (allocated(buffers(current)%undo_entries)) deallocate (buffers(current)%undo_entries)
        buffers(current)%read_only = .false.
        call remove_list_of_text_properties(point_min(), point_max(), &
                                            list(value_type(name=P_OCCUR_TARGET), value_type(name=P_OCCUR_PREFIX)), &
                                            changed, st)
        buffers(current)%read_only = .true.
        call message("Switching to Xref mode")
    end subroutine save_xref_edit_changes

    !> Before a change of a match line, give it its `occur_target': the
    !> location of its item.  Port of xref.el:xref-edit--before-change-function.
    recursive subroutine xref_edit_before_change_function(beginning, end, status)
        integer(int64), intent(in) :: beginning, end
        type(status_type), intent(out) :: status
        type(handle_type) :: m(1), e(1), old
        type(value_type) :: v
        type(status_type) :: st
        integer(int64) :: bol, eol, pos
        integer :: index, target
        logical :: found, ro, undo

        status = status_type()
        bol = line_beginning_position()
        eol = line_end_position()
        call get_text_property(bol, value_type(name=P_OCCUR_TARGET), v, st)
        if (allocated(v%a)) return
        call get_text_property(bol, value_type(name=P_OCCUR_PREFIX), v, st)
        if (.not. allocated(v%a)) return
        call property_at(bol, P_XREF_ITEM, index, found)
        if (index == 0) return
        old = current_buffer()

        block
            type(xref_item_type) :: item
            type(handle_type) :: marker, buffer
            item = buffer_item(index)
            if (.not. allocated(item%location)) return
            call item%location%marker(marker, status)
            if (.not. status%ok()) return
            call marker_place(marker, buffer, pos)
            call set_buffer(buffer, st)
            call copy_marker_at(pos, m(1), .false., st)
            call copy_marker_at(pos, e(1), .false., st)
            call destroy_marker(marker, st)
            call set_buffer(old, st)
        end block

        target = occur_add_target(m, e)
        ro = buffers(current)%read_only
        undo = buffers(current)%undo_enabled
        buffers(current)%read_only = .false.
        buffers(current)%undo_enabled = .false.
        call put_text_property(bol, eol, value_type(name=P_OCCUR_TARGET), value_type(int(target, int64)), st)
        buffers(current)%read_only = ro
        buffers(current)%undo_enabled = undo
    end subroutine xref_edit_before_change_function

    ! ------------------------------------------------------------------
    ! Showing and finding
    ! ------------------------------------------------------------------

    !> Display the xref items produced by FETCHER using DISPLAY_ACTION.
    !> The meanings of both arguments are the same as documented in
    !> `xref_show_xrefs_function'.  BUFFER is the buffer showing them.
    !> Port of xref.el:xref-show-xrefs.
    recursive subroutine show_xrefs(fetcher, display_action, buffer, status)
        class(xref_fetcher_type), intent(inout) :: fetcher
        integer, intent(in) :: display_action
        type(handle_type), intent(out) :: buffer
        type(status_type), intent(out) :: status
        type(xref_show_parameters_type) :: parameters
        type(handle_type) :: cb
        integer(int64) :: pt

        cb = current_buffer()
        pt = point()
        parameters%window = selected_window()
        parameters%display_action = display_action
        parameters%auto_jump = xref_auto_jump_to_first_xref
        call xref_show_xrefs_function(fetcher, parameters, buffer, status)
        if (.not. status%ok()) return
        call push_xref_markers(cb, pt, parameters%window)
    end subroutine show_xrefs

    ! Show ITEMS (a fetcher of definitions) with
    ! `xref_show_definitions_function'.  Port of xref.el:xref--show-defs.
    recursive subroutine show_xref_definitions(fetcher, display_action, buffer, status)
        class(xref_fetcher_type), intent(inout) :: fetcher
        integer, intent(in) :: display_action
        type(handle_type), intent(out) :: buffer
        type(status_type), intent(out) :: status
        type(xref_show_parameters_type) :: parameters
        type(handle_type) :: cb
        integer(int64) :: pt

        cb = current_buffer()
        pt = point()
        parameters%window = selected_window()
        parameters%display_action = display_action
        parameters%auto_jump = xref_auto_jump_to_first_definition
        call xref_show_definitions_function(fetcher, parameters, buffer, status)
        if (.not. status%ok()) return
        call push_xref_markers(cb, pt, parameters%window)
    end subroutine show_xref_definitions

    ! Push the mark (unless the region is active) and a marker at PT of TARGET_BUFFER
    ! onto the marker stack, with WIN selected.  Port of
    ! xref.el:xref--push-markers.
    recursive subroutine push_xref_markers(target_buffer, pt, win)
        type(handle_type), intent(in) :: target_buffer, win
        integer(int64), intent(in) :: pt
        type(handle_type) :: old_window, old_buffer
        type(status_type) :: st
        integer(int64) :: opoint

        if (.not. is_buffer_live(target_buffer)) return
        old_window = selected_window()
        old_buffer = current_buffer()
        if (is_window_live(win)) call select_window(win, st, norecord=.true.)
        call set_buffer(target_buffer, st)
        opoint = point()
        call goto_char(pt)
        if (.not. is_region_active()) call push_mark(nomsg=.true.)
        call push_xref_marker_stack()
        call goto_char(opoint)
        call select_window(old_window, st, norecord=.true.)
        call set_buffer(old_buffer, st)
    end subroutine push_xref_markers

    ! Whether COMMAND prompts for the identifier
    ! (`xref_prompt_for_identifier').  Port of xref.el:xref--prompt-p.
    logical function is_xref_prompt(command)
        character(len=*), intent(in) :: command
        logical :: listed

        listed = .false.
        if (allocated(xref_prompt_for_identifier_commands)) listed = any(xref_prompt_for_identifier_commands == command)

        select case (xref_prompt_for_identifier)
        case (XREF_PROMPT_FOR_IDENTIFIER_YES)
            is_xref_prompt = .true.
        case (XREF_PROMPT_FOR_IDENTIFIER_LISTED)
            is_xref_prompt = listed
        case (XREF_PROMPT_FOR_IDENTIFIER_EXCEPT_LISTED)
            is_xref_prompt = .not. listed
        case default
            is_xref_prompt = .false.
        end select
    end function is_xref_prompt

    !> The identifier at point, or read from the minibuffer with PROMPT.
    !> Port of xref.el:xref--read-identifier.
    recursive subroutine read_xref_identifier(prompt, identifier, status)
        character(len=*), intent(in) :: prompt
        character(len=:), allocatable, intent(out) :: identifier
        type(status_type), intent(out) :: status
        class(xref_backend_type), allocatable :: backend
        type(completion_table_type) :: table
        character(len=:), allocatable :: def, command, stem
        logical :: has_def, has_table, saved_ignore, prompting
        integer :: k

        status = status_type()
        identifier = ""
        call find_xref_backend(backend)
        if (.not. allocated(backend)) allocate (backend)
        call backend%identifier_at_point(def, has_def)
        command = ""
        if (associated(this_command)) command = trim(this_command%name)
        prompting = current_prefix_arg%kind /= PREFIX_NONE .or. .not. has_def
        if (.not. prompting) prompting = is_xref_prompt(command)

        if (.not. prompting) then
            identifier = def
            return
        end if

        saved_ignore = completion_ignore_case%value
        completion_ignore_case = backend%identifier_completion_ignore_case()
        call backend%identifier_completion_table(table, has_table)
        ! (substring prompt 0 (string-match "[ :]+\\'" prompt))
        k = len_trim(prompt)

        do while (k > 0)
            if (prompt(k:k) /= " " .and. prompt(k:k) /= ":") exit
            k = k - 1
        end do

        stem = prompt(1:k)

        if (has_def) then
            call completing_read(format_prompt(stem, def), table, identifier, status, &
                                 history="xref_read_identifier_history", default=def)
        else
            call completing_read(format_prompt(stem, ""), table, identifier, status, &
                                 history="xref_read_identifier_history")
        end if

        completion_ignore_case = saved_ignore
        if (.not. status%ok()) return

        if (len(identifier) == 0) then
            if (has_def) then
                identifier = def
            else
                call set_error(status, FM_USER_ERROR, "There is no default identifier")
            end if
        end if
    end subroutine read_xref_identifier

    ! A fetcher of the backend's KIND for ARGUMENT, at point of the current
    ! buffer.  Port of xref.el:xref--create-fetcher.
    recursive subroutine make_xref_fetcher(input, kind, argument, fetcher)
        character(len=*), intent(in) :: input, argument
        integer, intent(in) :: kind
        type(xref_backend_fetcher_type), intent(out) :: fetcher

        fetcher%buffer = current_buffer()
        fetcher%position = point()
        call find_xref_backend(fetcher%backend)
        if (.not. allocated(fetcher%backend)) allocate (fetcher%backend)
        fetcher%kind = kind
        fetcher%argument = argument
        fetcher%input = input
    end subroutine make_xref_fetcher

    !> Revisit the saved position and call the backend's method.  Port of
    !> xref.el:xref--create-fetcher (the function it returns) and
    !> xref--not-found-error.
    recursive subroutine fetch_from_backend(this, items, status)
        class(xref_backend_fetcher_type), intent(inout) :: this
        type(xref_item_type), allocatable, intent(out) :: items(:)
        type(status_type), intent(out) :: status
        type(handle_type) :: old
        type(status_type) :: st
        integer(int64) :: opoint
        character(len=:), allocatable :: kind_name

        old = current_buffer()
        opoint = point()

        ! Xref methods are generally allowed to depend on the text around
        ! point, not just on their explicit arguments.
        if (is_buffer_live(this%buffer)) then
            call set_buffer(this%buffer, st)
            opoint = point()
            call goto_char(max(point_min(), min(this%position, point_max())))
        end if

        select case (this%kind)
        case (KIND_REFERENCES)
            kind_name = "references"
            call this%backend%references(this%argument, items, status)
        case (KIND_APROPOS)
            kind_name = "apropos"
            call this%backend%apropos(this%argument, items, status)
        case default
            kind_name = "definitions"
            call this%backend%definitions(this%argument, items, status)
        end select

        call goto_char(opoint)
        call set_buffer(old, st)
        if (.not. status%ok()) return
        if (.not. allocated(items)) allocate (items(0))
        if (size(items) == 0) call set_error(status, FM_USER_ERROR, "No "//kind_name//" found for: "//this%input)
    end subroutine fetch_from_backend

    !> The items, the first time; after that, "Refresh is not supported".
    !> Port of xref.el:xref--show-xrefs (its function for a list of xrefs).
    recursive subroutine fetch_fixed_items(this, items, status)
        class(xref_items_fetcher_type), intent(inout) :: this
        type(xref_item_type), allocatable, intent(out) :: items(:)
        type(status_type), intent(out) :: status

        status = status_type()

        if (this%fetched) then
            allocate (items(0))
            call set_error(status, FM_USER_ERROR, "Refresh is not supported")
            return
        end if

        if (allocated(this%items)) then
            items = this%items
        else
            allocate (items(0))
        end if

        this%fetched = .true.
    end subroutine fetch_fixed_items

    ! Find the xrefs of KIND for ARGUMENT and show them.  Port of
    ! xref.el:xref--find-xrefs.
    recursive subroutine find_xrefs(input, kind, argument, display_action, buffer, status)
        character(len=*), intent(in) :: input, argument
        integer, intent(in) :: kind, display_action
        type(handle_type), intent(out) :: buffer
        type(status_type), intent(out) :: status
        type(xref_backend_fetcher_type) :: fetcher

        call make_xref_fetcher(input, kind, argument, fetcher)
        call show_xrefs(fetcher, display_action, buffer, status)
    end subroutine find_xrefs

    ! Find the definitions of ID and show them.  Port of
    ! xref.el:xref--find-definitions.
    recursive subroutine find_xref_definitions_internal(id, display_action, buffer, status)
        character(len=*), intent(in) :: id
        integer, intent(in) :: display_action
        type(handle_type), intent(out) :: buffer
        type(status_type), intent(out) :: status
        type(xref_backend_fetcher_type) :: fetcher

        call make_xref_fetcher(id, KIND_DEFINITIONS, id, fetcher)
        call show_xref_definitions(fetcher, display_action, buffer, status)
    end subroutine find_xref_definitions_internal

    !> Find the definition of the identifier at point.
    !> With prefix argument or when there's no identifier at point,
    !> prompt for it.
    !>
    !> If sufficient information is available to determine a unique
    !> definition for IDENTIFIER, display it in the selected window.
    !> Otherwise, display the list of the possible definitions in a
    !> buffer where the user can select from the list.
    !>
    !> Use \[go_back_in_xref_history] to return back to where you invoked this command.
    !> BUFFER is the buffer listing the definitions (the null handle when
    !> there is none).  Port of xref.el:xref-find-definitions.
    recursive subroutine find_xref_definitions(identifier, buffer, status)
        character(len=*), intent(in) :: identifier
        type(handle_type), intent(out) :: buffer
        type(status_type), intent(out) :: status

        call find_xref_definitions_internal(identifier, XREF_DISPLAY_ACTION_NONE, buffer, status)
    end subroutine find_xref_definitions

    ! The command: read the identifier, then `find_xref_definitions'.
    ! Not a port.
    recursive subroutine cmd_find_xref_definitions(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: identifier
        type(handle_type) :: buffer

        call read_xref_identifier("Find definitions of: ", identifier, status)
        if (status%ok()) call find_xref_definitions(identifier, buffer, status)
    end subroutine cmd_find_xref_definitions

    !> Like `find_xref_definitions' but switch to the other window.
    !> If this command needs to split the current window, it by default
    !> obeys the user options `split_height_threshold' and
    !> `split_width_threshold', when it decides whether to split the window
    !> horizontally or vertically.  Port of
    !> xref.el:xref-find-definitions-other-window.
    recursive subroutine find_xref_definitions_other_window(identifier, buffer, status)
        character(len=*), intent(in) :: identifier
        type(handle_type), intent(out) :: buffer
        type(status_type), intent(out) :: status

        call find_xref_definitions_internal(identifier, XREF_DISPLAY_ACTION_WINDOW, buffer, status)
    end subroutine find_xref_definitions_other_window

    ! The command: read the identifier, then
    ! `find_xref_definitions_other_window'.  Not a port.
    recursive subroutine cmd_find_xref_definitions_other_window(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: identifier
        type(handle_type) :: buffer

        call read_xref_identifier("Find definitions of: ", identifier, status)
        if (status%ok()) call find_xref_definitions_other_window(identifier, buffer, status)
    end subroutine cmd_find_xref_definitions_other_window

    !> Find references to the identifier at point.
    !> This command might prompt for the identifier as needed, perhaps
    !> offering the identifier at point as the default.
    !> With prefix argument, or if `xref_prompt_for_identifier' is
    !> XREF_PROMPT_FOR_IDENTIFIER_YES, always prompt for the identifier.  If
    !> `xref_prompt_for_identifier' is XREF_PROMPT_FOR_IDENTIFIER_NO, prompt
    !> only if there's no usable identifier at point.  Port of
    !> xref.el:xref-find-references.
    recursive subroutine find_xref_references(identifier, buffer, status)
        character(len=*), intent(in) :: identifier
        type(handle_type), intent(out) :: buffer
        type(status_type), intent(out) :: status

        call find_xrefs(identifier, KIND_REFERENCES, identifier, XREF_DISPLAY_ACTION_NONE, buffer, status)
    end subroutine find_xref_references

    ! The command: read the identifier, then `find_xref_references'.
    ! Not a port.
    recursive subroutine cmd_find_xref_references(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: identifier
        type(handle_type) :: buffer

        call read_xref_identifier("Find references of: ", identifier, status)
        if (status%ok()) call find_xref_references(identifier, buffer, status)
    end subroutine cmd_find_xref_references

    !> Find all meaningful identifiers that match PATTERN.
    !> The argument has the same meaning as in `apropos'.  Port of
    !> xref.el:xref-find-apropos.
    recursive subroutine find_xref_apropos(pattern, buffer, status)
        character(len=*), intent(in) :: pattern
        type(handle_type), intent(out) :: buffer
        type(status_type), intent(out) :: status

        ! Delegate pattern handling to the backend fully.
        call find_xrefs(pattern, KIND_APROPOS, pattern, XREF_DISPLAY_ACTION_NONE, buffer, status)
    end subroutine find_xref_apropos

    ! The command: read the pattern, then `find_xref_apropos'.  Not a port.
    recursive subroutine cmd_find_xref_apropos(status)
        type(status_type), intent(out) :: status
        class(xref_backend_type), allocatable :: backend
        character(len=:), allocatable :: pattern, def
        type(handle_type) :: buffer
        logical :: has_def

        call find_xref_backend(backend)
        if (.not. allocated(backend)) allocate (backend)
        call backend%identifier_at_point(def, has_def)

        if (has_def) then
            call read_string("Search for pattern (word list or regexp): ", pattern, status, &
                             history="xref_read_pattern_history", default=def)
        else
            call read_string("Search for pattern (word list or regexp): ", pattern, status, &
                             history="xref_read_pattern_history")
        end if

        if (status%ok()) call find_xref_apropos(pattern, buffer, status)
    end subroutine cmd_find_xref_apropos

    !> An Emacs regexp from PATTERN similar to `apropos': a pattern without
    !> regexp special characters is a list of words.  Port of
    !> xref.el:xref-apropos-regexp.
    subroutine xref_apropos_regexp(pattern, regexp, status)
        character(len=*), intent(in) :: pattern
        character(len=:), allocatable, intent(out) :: regexp
        type(status_type), intent(out) :: status
        type(string_type), allocatable :: words(:)
        type(string_type) :: word
        integer :: i, j

        status = status_type()
        regexp = ""

        if (regexp_quote(pattern) == pattern) then
            ! Split into words.
            allocate (words(0))
            i = 1

            do while (i <= len(pattern))
                if (pattern(i:i) == " " .or. pattern(i:i) == achar(9)) then
                    i = i + 1
                    cycle
                end if

                j = i

                do while (j < len(pattern))
                    if (pattern(j + 1:j + 1) == " " .or. pattern(j + 1:j + 1) == achar(9)) exit
                    j = j + 1
                end do

                word%text = pattern(i:j)
                words = [words, word]
                i = j + 1
            end do

            if (size(words) == 0) then
                call set_error(status, FM_USER_ERROR, "No word list given")
                return
            end if

            call apropos_parse_pattern(pattern, words, .false.)
        else
            call apropos_parse_pattern(pattern, words, .false.)
        end if

        regexp = apropos_regexp
    end subroutine xref_apropos_regexp

    !> Minor mode to make xref use etags again.
    !>
    !> Certain major modes install their own mechanisms for listing
    !> identifiers and navigation.  Turn this on to undo those settings
    !> and just use etags.  Port of xref.el:xref-etags-mode (the body).
    recursive subroutine xref_etags_mode_body(enable, status)
        logical, intent(in) :: enable
        type(status_type), intent(out) :: status
        integer :: k

        status = status_type()

        if (enable) then
            k = backend_local_index(xref_backend_functions)
            xref_etags_mode_saved_local = k /= 0

            if (k /= 0) then
                xref_etags_mode_saved = xref_backend_functions%locals(k)%entries
            else
                xref_etags_mode_saved = xref_backend_functions%entries
            end if

            call xref_backend_functions%clear_local()
        else
            call make_backend_local(xref_backend_functions, k)
            if (allocated(xref_etags_mode_saved)) xref_backend_functions%locals(k)%entries = xref_etags_mode_saved
        end if
    end subroutine xref_etags_mode_body

    !> Toggle Xref-Etags mode.  Port of xref.el:xref-etags-mode (the
    !> command).
    recursive subroutine xref_etags_mode(status)
        type(status_type), intent(out) :: status

        call minor_modes%xref_etags_mode%command(status)
    end subroutine xref_etags_mode

    ! ------------------------------------------------------------------
    ! Searching files
    ! ------------------------------------------------------------------

    !> Find all references to IDENTIFIER in directory DIRECTORY.
    !> See `xref_references_in_directory_function' for the implementation.
    !> Gives an array of xref items.  Port of
    !> xref.el:xref-references-in-directory.
    recursive subroutine xref_references_in_directory(identifier, directory, items, status)
        character(len=*), intent(in) :: identifier, directory
        type(xref_item_type), allocatable, intent(out) :: items(:)
        type(status_type), intent(out) :: status

        call xref_references_in_directory_function(identifier, file_name_as_directory(directory), items, status)
    end subroutine xref_references_in_directory

    !> Find all references to IDENTIFIER in directory DIRECTORY using find and grep.
    !> Gives an array of xref items.  Port of
    !> xref.el:xref-references-in-directory-grep (the project's ignores,
    !> project.el, are not ported: none).
    recursive subroutine xref_references_in_directory_grep(identifier, directory, items, status)
        character(len=*), intent(in) :: identifier, directory
        type(xref_item_type), allocatable, intent(out) :: items(:)
        type(status_type), intent(out) :: status
        character(len=1) :: ignores(0)

        call xref_matches_in_directory(regexp_quote(identifier), "*", directory, ignores, items, status, delimited=.true.)
    end subroutine xref_references_in_directory_grep

    !> Find all matches for REGEXP in directory DIRECTORY.
    !> Gives an array of xref items.
    !> Only files matching some of FILES and none of IGNORES are searched.
    !> FILES is a string with glob patterns separated by spaces.
    !> IGNORES is an array of glob patterns for files to ignore.
    !> If DELIMITED is true (upstream's `symbol'), only select matches that
    !> span full identifiers.  Port of xref.el:xref-matches-in-directory.
    recursive subroutine xref_matches_in_directory(regexp, files, directory, ignores, items, status, delimited)
        character(len=*), intent(in) :: regexp, files, directory
        character(len=*), intent(in) :: ignores(:)
        type(xref_item_type), allocatable, intent(out) :: items(:)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: delimited
        type(xref_hit_type), allocatable :: hits(:)
        type(handle_type) :: target_buffer, old
        type(value_type) :: result
        type(status_type) :: st
        character(len=:), allocatable :: template, command, local_directory, hits_regexp, expanded
        integer :: saved_highlight, at
        logical :: delimit

        allocate (items(0))
        ! DIRECTORY can also be a regular file for now; let's not advertise that.
        call grep_compute_defaults(status)
        if (.not. status%ok()) return
        template = grep_find_template
        at = index(template, "<C>")
        if (at > 0) template = template(1:at + 2)//" -E"//template(at + 3:)
        saved_highlight = grep_highlight_matches
        grep_highlight_matches = GREP_HIGHLIGHT_MATCHES_NO
        command = xref_rgrep_command(template, xref_regexp_to_extended(regexp), files, ".", ignores)
        grep_highlight_matches = saved_highlight
        call expand_file_name(directory, expanded, st)
        local_directory = directory_file_name(expanded)
        delimit = .false.
        if (present(delimited)) delimit = delimited
        hits_regexp = regexp
        if (delimit) hits_regexp = "\_<"//regexp//"\_>"
        old = current_buffer()
        call get_or_make_buffer(" *xref-grep*", target_buffer, status)
        if (.not. status%ok()) return
        call set_buffer(target_buffer, st)
        call erase_buffer(st)
        buffers(current)%directory = file_name_as_directory(directory)
        call execute_process(shell_file_name%value, result, status, destination=value_type(.true.), &
                             args=list(value_type(text=shell_command_switch%value), value_type(text=command)))

        if (status%ok()) then
            call parse_xref_hits(grep_regexps(1)%regexp, grep_regexps(1)%line, grep_regexps(1)%file, result, hits, &
                                 status, local_directory)
        end if

        call set_buffer(old, st)
        if (.not. status%ok()) return
        call sort_xref_hits(hits)
        call convert_xref_hits(hits, hits_regexp, items, status)
    end subroutine xref_matches_in_directory

    !> Find all matches for REGEXP in FILES.
    !> Gives an array of xref items.
    !> FILES must be an array of absolute file names.
    !>
    !> See `xref_search_program' and `xref_search_programs' for how
    !> to control which program to use when looking for matches.  Port of
    !> xref.el:xref-matches-in-files (remote files are not ported).
    recursive subroutine xref_matches_in_files(regexp, files, items, status)
        character(len=*), intent(in) :: regexp
        character(len=*), intent(in) :: files(:)
        type(xref_item_type), allocatable, intent(out) :: items(:)
        type(status_type), intent(out) :: status
        type(xref_hit_type), allocatable :: hits(:)
        type(handle_type) :: output, temp, old
        type(value_type) :: result
        type(status_type) :: st
        character(len=:), allocatable :: directory, command, template, joined
        integer :: saved_highlight, k, i

        allocate (items(0))
        status = status_type()
        if (size(files) == 0) return
        old = current_buffer()
        call get_or_make_buffer(" *project grep output*", output, status)
        if (.not. status%ok()) return

        ! Support for remote files (not ported): if the first file is
        ! remote, they all are, and on the same host.
        if (is_file_name_absolute(trim(files(1)))) then
            directory = file_name_directory(trim(files(1)))
        else
            directory = ""
            if (allocated(buffers(current)%directory)) directory = buffers(current)%directory
        end if

        k = 0

        if (allocated(xref_search_programs)) then
            k = findloc(xref_search_programs%program, xref_search_program, dim=1)
        end if

        if (k == 0) then
            call set_error(status, FM_USER_ERROR, "Unknown search program `"//program_name(xref_search_program)//"'")
            return
        end if

        ! The 'auto' default would be fine too, but ripgrep can't handle
        ! the options we pass in that case.
        saved_highlight = grep_highlight_matches
        grep_highlight_matches = GREP_HIGHLIGHT_MATCHES_NO
        template = trim(xref_search_programs(k)%template)
        command = grep_expand_template(template, xref_regexp_to_extended(regexp))
        grep_highlight_matches = saved_highlight
        joined = ""

        do i = 1, size(files)
            if (i > 1) joined = joined//NUL
            joined = joined//trim(files(i))
        end do

        call set_buffer(output, st)
        call erase_buffer(st)
        call get_or_make_buffer(" *xref-temp-input*", temp, status)

        if (status%ok()) then
            call set_buffer(temp, st)
            call erase_buffer(st)
            call insert(joined, st)
            if (len(directory) > 0) buffers(current)%directory = file_name_as_directory(directory)
            call execute_process_region(shell_file_name%value, result, status, start=point_min(), end=point_max(), &
                                        destination=value_type(text=" *project grep output*"), &
                                        args=list(value_type(text=shell_command_switch%value), value_type(text=command)))
            call close_buffer(temp, st)
            call set_buffer(output, st)
            if (status%ok()) call parse_xref_hits(grep_regexps(1)%regexp, grep_regexps(1)%line, grep_regexps(1)%file, &
                                                  result, hits, status)
        end if

        call set_buffer(old, st)
        if (.not. status%ok()) return
        call sort_xref_hits(hits)
        call convert_xref_hits(hits, regexp, items, status)

    contains

        function program_name(p) result(name)
            integer, intent(in) :: p
            character(len=:), allocatable :: name
            character(len=24) :: digits

            select case (p)
            case (XREF_SEARCH_PROGRAM_GREP)
                name = "XREF_SEARCH_PROGRAM_GREP"
            case (XREF_SEARCH_PROGRAM_RIPGREP)
                name = "XREF_SEARCH_PROGRAM_RIPGREP"
            case (XREF_SEARCH_PROGRAM_UGREP)
                name = "XREF_SEARCH_PROGRAM_UGREP"
            case default
                write (digits, "(i0)") p
                name = trim(digits)
            end select
        end function program_name
    end subroutine xref_matches_in_files

    ! The hits of the search output in the current buffer: GREP_REGEXP with
    ! the line and file in groups LINE_GROUP and FILE_GROUP; RESULT is the
    ! exit status.  A file name starting with "." is relative to
    ! PARENT_DIRECTORY when given.  Port of xref.el:xref--parse-hits.
    recursive subroutine parse_xref_hits(grep_regexp, line_group, file_group, result, hits, status, parent_directory)
        character(len=*), intent(in) :: grep_regexp
        integer, intent(in) :: line_group, file_group
        type(value_type), intent(in) :: result
        type(xref_hit_type), allocatable, intent(out) :: hits(:)
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: parent_directory
        type(xref_hit_type) :: hit
        type(status_type) :: st
        character(len=:), allocatable :: first_line, file, digits
        character(len=24) :: code
        integer(int64) :: found
        integer :: ios

        status = status_type()
        allocate (hits(0))
        call goto_char(point_min())

        ! Can't use the exit status: Grep exits with 1 to mean "no matches
        ! found".  Find exits with 1 if any of the invocations exit with
        ! non-zero. "No matches" and "Grep program not found" are all the
        ! same to it.
        if (point_min() /= point_max()) then
            if (.not. looking_at(grep_regexp, st)) then
                ! TODO: Show these matches as well somehow?
                if (.not. looking_at(".*[bB]inary file.* matches", st)) then
                    call buffer_substring(point_min(), line_end_position(), first_line, st)
                    code = "?"
                    if (vtag(result) == V_INTEGER) write (code, "(i0)") xfixnum(result)
                    call set_error(status, FM_USER_ERROR, "Search failed with status "//trim(code)//": "//first_line)
                    return
                end if
            end if
        end if

        do
            call re_search_forward(grep_regexp, found, st, noerror=SEARCH_NOERROR)
            if (found == 0 .or. .not. st%ok()) exit
            digits = substring_of_match(line_group)
            read (digits, *, iostat=ios) hit%line
            if (ios /= 0) hit%line = 0
            file = substring_of_match(file_group)

            if (present(parent_directory)) then
                hit%file = parent_directory//file(2:)
            else
                hit%file = file
            end if

            call buffer_substring_no_properties(point(), line_end_position(), hit%text, st)
            hits = [hits, hit]
        end do

    contains

        function substring_of_match(group) result(text)
            integer, intent(in) :: group
            character(len=:), allocatable :: text
            type(status_type) :: s
            integer(int64) :: b, e

            text = ""
            b = match_beginning(group, s)
            if (.not. s%ok() .or. b == 0) return
            e = match_end(group, s)
            call buffer_substring_no_properties(b, e, text, s)
        end function substring_of_match
    end subroutine parse_xref_hits

    ! Sort HITS by file name, keeping the order of the hits of one file (a
    ! stable sort, as upstream's `sort').  Port of xref.el:xref--sort-hits.
    subroutine sort_xref_hits(hits)
        type(xref_hit_type), intent(inout) :: hits(:)
        type(xref_hit_type) :: h
        integer :: i, j

        do i = 2, size(hits)
            h = hits(i)
            j = i - 1

            do while (j >= 1)
                if (.not. llt(h%file, hits(j)%file)) exit
                hits(j + 1) = hits(j)
                j = j - 1
            end do

            hits(j + 1) = h
        end do
    end subroutine sort_xref_hits

    !> The command of `xref_matches_in_directory': GREP_FIND_TEMPLATE
    !> expanded for REGEXP in the FILES (glob patterns separated by spaces)
    !> of directory DIRECTORY, without IGNORES.  Port of
    !> xref.el:xref--rgrep-command.
    function xref_rgrep_command(grep_find_template, regexp, files, directory, ignores) result(command)
        character(len=*), intent(in) :: grep_find_template, regexp, files, directory
        character(len=*), intent(in) :: ignores(:)
        character(len=:), allocatable :: command, names
        integer :: i, j
        logical :: first

        names = ""
        first = .true.
        i = 1

        do while (i <= len(files))
            if (files(i:i) == " " .or. files(i:i) == achar(9)) then
                i = i + 1
                cycle
            end if

            j = i

            do while (j < len(files))
                if (files(j + 1:j + 1) == " " .or. files(j + 1:j + 1) == achar(9)) exit
                j = j + 1
            end do

            if (.not. first) names = names//" -o "//find_name_arg//" "
            names = names//shell_quote_argument(files(i:j))
            first = .false.
            i = j + 1
        end do

        command = grep_expand_template(grep_find_template, regexp, &
                                       shell_quote_argument("(")//" "//find_name_arg//" "//names//" "// &
                                       shell_quote_argument(")"), &
                                       shell_quote_argument(directory), xref_find_ignores_arguments(ignores, directory))
    end function xref_rgrep_command

    !> Convert IGNORES and DIRECTORY to a string of arguments for `find'.
    !> IGNORES is an array of glob patterns.  DIRECTORY is an absolute
    !> directory, used as the root of the ignore globs.  Port of
    !> xref.el:xref--find-ignores-arguments.
    function xref_find_ignores_arguments(ignores, directory) result(arguments)
        character(len=*), intent(in) :: ignores(:), directory
        character(len=:), allocatable :: arguments, ignore, root
        integer :: i

        arguments = ""
        if (size(ignores) == 0) return
        root = file_name_as_directory(directory)
        arguments = shell_quote_argument("(")//" -path "

        do i = 1, size(ignores)
            ignore = trim(ignores(i))
            if (i > 1) arguments = arguments//" -o -path "

            if (len(ignore) > 0) then
                if (ignore(len(ignore):len(ignore)) == "/") ignore = ignore//"*"
            end if

            if (index(ignore, "./") == 1) then
                ignore = root//ignore(3:)
            else if (index(ignore, "*") /= 1) then
                ignore = "*/"//ignore
            end if

            arguments = arguments//shell_quote_argument(ignore)
        end do

        arguments = arguments//" "//shell_quote_argument(")")//" -prune -o "
    end function xref_find_ignores_arguments

    !> REGEXP, an Emacs regexp, in the extended syntax of grep -E: the
    !> grouping, alternation and repetition operators lose their backslash
    !> and the literal characters gain one.  Port of
    !> xref.el:xref--regexp-to-extended.
    function xref_regexp_to_extended(regexp) result(extended)
        character(len=*), intent(in) :: regexp
        character(len=:), allocatable :: extended, matched, group
        character(len=*), parameter :: PATTERN = "\(?:\\\\\)*\(?:\\[][]\)?\(?:\[.+?]\|\(\\?[(){|}]\)\)"
        type(status_type) :: st
        integer(int64) :: start, found, b, e, g1, g1e
        integer(int64) :: ob, oe, og, oge
        logical :: saved

        extended = ""
        start = 1
        saved = case_fold_search%value
        case_fold_search = .false.

        do
            found = string_match(PATTERN, regexp, st, start)
            if (found == 0 .or. .not. st%ok()) exit
            b = match_beginning(0, st)
            e = match_end(0, st)
            g1 = match_beginning(1, st)
            g1e = match_end(1, st)
            ob = string_index_to_offset(regexp, b)
            oe = string_index_to_offset(regexp, e)
            extended = extended//regexp(string_index_to_offset(regexp, start) + 1:ob)
            matched = regexp(ob + 1:oe)

            if (g1 == 0) then
                extended = extended//matched
            else
                og = string_index_to_offset(regexp, g1)
                oge = string_index_to_offset(regexp, g1e)
                group = regexp(og + 1:oge)

                if (len(group) == 2) then
                    extended = extended//regexp(ob + 1:og)//group(2:2)
                else
                    extended = extended//regexp(ob + 1:og)//"\"//group
                end if
            end if

            if (e == b) then
                ! An empty match: keep the next character as it is.
                if (oe >= len(regexp)) then
                    start = e
                    exit
                end if

                extended = extended//regexp(oe + 1:oe + 1)
                start = e + 1
            else
                start = e
            end if
        end do

    if (string_index_to_offset(regexp, start) < len(regexp)) extended = extended//regexp(string_index_to_offset(regexp, start) + 1:)
        case_fold_search = saved
    end function xref_regexp_to_extended

    ! Whether REGEXP depends on the buffer's syntax, such as the current syntax
    ! table and the applied syntax properties.  Port of
    ! xref.el:xref--regexp-syntax-dependent-p.
    logical function is_xref_regexp_syntax_dependent(regexp) result(dependent)
        character(len=*), intent(in) :: regexp
        type(status_type) :: st
        logical :: saved

        saved = case_fold_search%value
        case_fold_search = .false.
        dependent = string_match("\(?:\`\|[^\\]\)\(?:\\\\\)*\\[bB<>wW_sS]", regexp, st) /= 0
        case_fold_search = saved
    end function is_xref_regexp_syntax_dependent

    ! The match items of HITS for REGEXP.  Port of xref.el:xref--convert-hits.
    recursive subroutine convert_xref_hits(hits, regexp, items, status)
        type(xref_hit_type), intent(in) :: hits(:)
        character(len=*), intent(in) :: regexp
        type(xref_item_type), allocatable, intent(out) :: items(:)
        type(status_type), intent(out) :: status
        type(xref_item_type), allocatable :: matches(:)
        type(handle_type) :: temporary_buffer
        type(status_type) :: st
        character(len=:), allocatable :: name
        logical :: syntax_needed
        integer :: i

        allocate (items(0))
        status = status_type()
        if (allocated(last_file)) deallocate (last_file)
        last_file_buffer = null_handle()
        if (allocated(temp_buffer_file_name)) deallocate (temp_buffer_file_name)
        name = " *xref-temp*"
        call get_or_make_buffer(name, temporary_buffer, status)
        if (.not. status%ok()) return
        hits_file_prefix = ""

        if (size(hits) > 0) then
            if (.not. is_file_name_absolute(hits(1)%file)) then
                call expand_file_name("", hits_file_prefix, st)
                hits_file_prefix = file_name_as_directory(hits_file_prefix)
            end if
        end if

        syntax_needed = is_xref_regexp_syntax_dependent(regexp)

        do i = 1, size(hits)
            call collect_xref_matches(hits(i), regexp, temporary_buffer, syntax_needed, matches, status)
            if (.not. status%ok()) exit
            call append_xref_items(items, matches)
        end do

        call close_buffer(temporary_buffer, st)
    end subroutine convert_xref_hits

    ! The match items of HIT for REGEXP, from the line in the buffer
    ! visiting its file (when it is up to date) or else from the hit's text
    ! in TEMPORARY_BUFFER.  Port of xref.el:xref--collect-matches.
    recursive subroutine collect_xref_matches(hit, regexp, temporary_buffer, syntax_needed, matches, status)
        type(xref_hit_type), intent(in) :: hit
        character(len=*), intent(in) :: regexp
        type(handle_type), intent(in) :: temporary_buffer
        logical, intent(in) :: syntax_needed
        type(xref_item_type), allocatable, intent(out) :: matches(:)
        type(status_type), intent(out) :: status
        type(handle_type) :: target_buffer, old
        type(status_type) :: st
        character(len=:), allocatable :: file
        integer(int64) :: opoint, begv, zv, shortage, inserted
        character(len=:), allocatable :: saved_filename
        logical :: had_filename

        status = status_type()
        file = hits_file_prefix//hit%file
        target_buffer = find_xref_file_buffer(file)
        old = current_buffer()

        if (target_buffer%index /= 0) then
            call set_buffer(target_buffer, st)
            opoint = point()
            begv = point_min()
            zv = point_max()
            call widen()
            call goto_char(point_min())
            shortage = forward_line(hit%line - 1)
            call collect_xref_line_matches(regexp, file, hit%line, line_beginning_position(), line_end_position(), &
                                           syntax_needed, matches)
            call narrow_to_region(begv, zv, st)
            call goto_char(opoint)
        else
            ! Using the temporary buffer is both a performance and a buffer
            ! management optimization.
            call set_buffer(temporary_buffer, st)
            call erase_buffer(st)

            if (syntax_needed) then
                if (.not. allocated(temp_buffer_file_name)) temp_buffer_file_name = ""

                if (temp_buffer_file_name /= file) then
                    call insert_file_contents(file, .false., inserted, st, beginning_byte=0_int64, end_byte=200_int64)
                    had_filename = allocated(buffers(current)%filename)
                    if (had_filename) saved_filename = buffers(current)%filename
                    buffers(current)%filename = file
                    call set_auto_mode(st)

                    if (had_filename) then
                        buffers(current)%filename = saved_filename
                    else
                        deallocate (buffers(current)%filename)
                    end if

                    temp_buffer_file_name = file
                    buffers(current)%read_only = .false.
                    call erase_buffer(st)
                end if
            end if

            call insert(hit%text, st)
            call goto_char(point_min())
            if (syntax_needed) call syntax_ppss_flush_cache(point())
            call collect_xref_line_matches(regexp, file, hit%line, point(), point_max(), syntax_needed, matches)
        end if

        call set_buffer(old, st)
    end subroutine collect_xref_matches

    ! The match items of REGEXP on the line LINE_BEGINNING..LINE_END of the
    ! current buffer, line LINE of FILE: each item's summary runs from its
    ! match (the line's beginning for the first) to the next match (the
    ! line's end for the last).  Port of xref.el:xref--collect-matches-1.
    recursive subroutine collect_xref_line_matches(regexp, file, line, line_beginning, line_end, syntax_needed, matches)
        character(len=*), intent(in) :: regexp, file
        integer(int64), intent(in) :: line, line_beginning, line_end
        logical, intent(in) :: syntax_needed
        type(xref_item_type), allocatable, intent(out) :: matches(:)
        type(xref_item_type) :: item
        type(status_type) :: st
        character(len=:), allocatable :: summary
        integer(int64) :: beginning, end, last_beginning, last_end, summary_end, summary_start, found
        logical :: stop, has_last

        allocate (matches(0))
        if (syntax_needed) call syntax_propertize(line_end, st)
        stop = .false.
        has_last = .false.
        beginning = 0
        end = 0
        last_beginning = 0
        last_end = 0

        do while (.not. stop)
            found = 0

            ! REGEXP might match an empty string.  Or line.
            if (.not. (has_last .and. end == line_beginning)) then
                call re_search_forward(regexp, found, st, bound=line_end, noerror=SEARCH_NOERROR)
                if (.not. st%ok()) found = 0
            end if

            if (found /= 0) then
                ! An empty match where the last one ended would match again
                ! forever: stop there.
                if (has_last .and. match_end(0, st) == end .and. match_beginning(0, st) == end) found = 0
            end if

            if (found /= 0) then
                beginning = match_beginning(0, st)
                end = match_end(0, st)
                summary_end = beginning
            else
                stop = .true.
                summary_end = line_end
            end if

            if (has_last) then
                if (size(matches) > 0) then
                    summary_start = last_beginning
                else
                    summary_start = line_beginning
                end if

                call buffer_substring_no_properties(summary_start, summary_end, summary, st)
                item = make_xref_match(summary, make_xref_file_location(file, line, last_beginning - line_beginning), &
                                       last_end - last_beginning)
                item%match_beginning = last_beginning - summary_start
                call append_xref_item(matches, item)
            end if

            last_beginning = beginning
            last_end = end
            has_last = has_last .or. found /= 0
        end do
    end subroutine collect_xref_line_matches

    ! The buffer visiting FILE when its contents are those of the file
    ! (unmodified and up to date), else the null handle.  Port of
    ! xref.el:xref--find-file-buffer.
    function find_xref_file_buffer(file) result(target_buffer)
        character(len=*), intent(in) :: file
        type(handle_type) :: target_buffer
        type(handle_type) :: old
        type(status_type) :: st
        logical :: stale, modified

        if (allocated(last_file)) then
            if (last_file == file) then
                target_buffer = last_file_buffer
                return
            end if
        end if

        target_buffer = get_file_buffer(file)

        if (target_buffer%index /= 0) then
            ! We can't use buffers whose contents diverge from disk
            ! (bug#54025).
            old = current_buffer()
            call set_buffer(target_buffer, st)
            call is_buffer_modified(target_buffer, modified, st)
            stale = modified .or. .not. is_visited_file_modtime_current()
            call set_buffer(old, st)
            if (stale) target_buffer = null_handle()
        end if

        last_file = file
        last_file_buffer = target_buffer
    end function find_xref_file_buffer

    ! ------------------------------------------------------------------
    ! Setup
    ! ------------------------------------------------------------------

    !> Define the faces, modes, keymaps and commands of xref.el, and bind
    !> the global keys in ESC_MAP and CTL_X_4_MAP.  Port of xref.el's
    !> top-level forms and its key bindings.
    subroutine init_xref(esc_map, ctl_x_4_map)
        type(handle_type), intent(in) :: esc_map, ctl_x_4_map
        type(named_face_type), pointer :: f
        type(status_type) :: st

        if (associated(major_modes%xref_xref_buffer_mode%execute)) return
        xref_show_xrefs_function => show_xref_buffer
        xref_show_definitions_function => show_xref_definitions_buffer
        xref_references_in_directory_function => xref_references_in_directory_grep
        if (.not. allocated(xref_match_count_face)) xref_match_count_face = "compilation_info"

        if (.not. allocated(xref_prompt_for_identifier_commands)) then
            xref_prompt_for_identifier_commands = [character(len=34) :: "find_xref_definitions", &
                                                   "find_xref_definitions_other_window", &
                                                   "find_xref_definitions_other_frame"]
        end if

        if (.not. allocated(xref_search_programs)) then
            allocate (xref_search_programs(3))
            ! '-s' because 'git ls-files' can output broken symlinks.
            xref_search_programs(1) = xref_search_program_type(XREF_SEARCH_PROGRAM_GREP, &
                                                               "xargs -0 grep <C> --null -snHE -e <R>")
            ! '!*/' is there to filter out dirs (e.g. submodules).
            xref_search_programs(2) = xref_search_program_type(XREF_SEARCH_PROGRAM_RIPGREP, &
                                                               "xargs -0 rg <C> --null -nH --no-heading --no-messages "// &
                                                               "-g '!*/' -e <R>")
            xref_search_programs(3) = xref_search_program_type(XREF_SEARCH_PROGRAM_UGREP, &
                                                               "xargs -0 ugrep <C> --null -ns -e <R>")
        end if

        call xref_after_jump_hook%append(recenter_adapter)
        f => xref_file_header_face
        call f%define("xref_file_header", "Face used to highlight file header in the xref buffer.")
        call f%spec_entry(display_when(), face_attrs(inherit="compilation_info"))
        f => xref_line_number_face
        call f%define("xref_line_number", "Face for displaying line numbers in the xref buffer.")
        call f%spec_entry(display_when(), face_attrs(inherit="compilation_line_number"))
        f => xref_match_face
        call f%define("xref_match", "Face used to highlight matches in the xref buffer.")
        call f%spec_entry(display_when(), face_attrs(inherit="match"))
        call major_modes%xref_xref_buffer_mode%define("xref_xref_buffer_mode", "Xref", xref_xref_buffer_mode, &
                                                      xref_xref_buffer_mode_hook, parent=major_modes%special_mode)
        call major_modes%xref_transient_buffer_mode%define("xref_transient_buffer_mode", "Xref Transient", &
                                                           xref_transient_buffer_mode, xref_transient_buffer_mode_hook, &
                                                           parent=major_modes%xref_xref_buffer_mode)
        xref_xref_buffer_mode_map = major_modes%xref_xref_buffer_mode%map
        call keymap_set(xref_xref_buffer_mode_map, "n", "next_xref_line", st)
        call keymap_set(xref_xref_buffer_mode_map, "p", "previous_xref_line", st)
        call keymap_set(xref_xref_buffer_mode_map, "N", "next_xref_group", st)
        call keymap_set(xref_xref_buffer_mode_map, "P", "previous_xref_group", st)
        call keymap_set(xref_xref_buffer_mode_map, "RET", "goto_xref", st)
        call keymap_set(xref_xref_buffer_mode_map, "TAB", "quit_and_goto_xref", st)
        call keymap_set(xref_xref_buffer_mode_map, "C-o", "show_xref_location_at_point", st)
        ! suggested by Johan Claesson "to further reduce finger movement":
        call keymap_set(xref_xref_buffer_mode_map, ".", "next_xref_line", st)
        call keymap_set(xref_xref_buffer_mode_map, ",", "previous_xref_line", st)
        call keymap_set(xref_xref_buffer_mode_map, "M-,", "quit_and_pop_xref_marker_stack", st)
        call keymap_set(xref_xref_buffer_mode_map, "e", "change_to_xref_edit_mode", st)
        xref_transient_buffer_mode_map = major_modes%xref_transient_buffer_mode%map
        call keymap_set(xref_transient_buffer_mode_map, "RET", "quit_and_goto_xref", st)
        call make_sparse_keymap(xref_edit_mode_map)
        call keymap_set(xref_edit_mode_map, "C-c C-c", "save_xref_edit_changes", st)
        call keymap_set(xref_edit_mode_map, "RET", "goto_xref", st)
        call keymap_set(xref_edit_mode_map, "M-,", "quit_and_pop_xref_marker_stack", st)
        call keymap_set(xref_edit_mode_map, "C-o", "show_xref_location_at_point", st)
        call minor_modes%xref_etags_mode%define("xref_etags_mode", body=xref_etags_mode_body)
        call forward_setting(xref_read_identifier_history)
        call forward_setting(xref_read_pattern_history)
        ! The key bindings (xref.el's autoloads).
        call keymap_set(esc_map, ".", "find_xref_definitions", st)
        call keymap_set(esc_map, ",", "go_back_in_xref_history", st)
        call keymap_set(esc_map, "C-,", "go_forward_in_xref_history", st)
        call keymap_set(esc_map, "?", "find_xref_references", st)
        call keymap_set(esc_map, "C-.", "find_xref_apropos", st)
        call keymap_set(ctl_x_4_map, ".", "find_xref_definitions_other_window", st)
        commands%xref_xref_buffer_mode%execute => xref_xref_buffer_mode
        commands%xref_transient_buffer_mode%execute => xref_transient_buffer_mode
        commands%xref_edit_mode%execute => xref_edit_mode
        commands%xref_etags_mode%execute => xref_etags_mode
        commands%find_xref_definitions%execute => cmd_find_xref_definitions
        commands%find_xref_definitions_other_window%execute => cmd_find_xref_definitions_other_window
        commands%find_xref_references%execute => cmd_find_xref_references
        commands%find_xref_apropos%execute => cmd_find_xref_apropos
        commands%go_back_in_xref_history%execute => go_back_in_xref_history
        commands%pop_xref_marker_stack%execute => go_back_in_xref_history
        commands%go_forward_in_xref_history%execute => go_forward_in_xref_history
        commands%next_xref_line%execute => next_xref_line
        commands%next_xref_line_no_show%execute => next_xref_line_no_show
        commands%previous_xref_line%execute => previous_xref_line
        commands%previous_xref_line_no_show%execute => previous_xref_line_no_show
        commands%next_xref_group%execute => next_xref_group
        commands%previous_xref_group%execute => previous_xref_group
        commands%goto_xref%execute => cmd_goto_xref
        commands%quit_and_goto_xref%execute => quit_and_goto_xref
        commands%quit_and_pop_xref_marker_stack%execute => quit_and_pop_xref_marker_stack
        commands%show_xref_location_at_point%execute => show_xref_location_at_point
        commands%change_to_xref_edit_mode%execute => change_to_xref_edit_mode
        commands%save_xref_edit_changes%execute => save_xref_edit_changes
        call append_locals_clearer(clear_xref_locals)
    end subroutine init_xref
end module xref_mod
