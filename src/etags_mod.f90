! etags_mod.f90 -- the tags tables of the etags program.
! Ported from GNU Emacs 31.1 lisp/progmodes/etags.el.
!
! Copyright (C) 1985-1986, 1988-1989, 1992-1996, 1998, 2000-2026 Free
! Software Foundation, Inc.
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

!> Tags tables: the TAGS files the etags program writes (generating them
!> is the etags program's job).  `visit_tags_table' selects one,
!> `tags_table_list' lists several, and the tables are read into buffers
!> in Tags Table mode.  The etags backend of xref_mod
!> (`etags_xref_backend', the default) finds definitions and apropos
!> matches in them; `find_tag', `list_tags', `apropos_tags' and
!> `complete_tag' are the older commands.
!>
!> What is not ported yet is listed in docs/status/etags_mod.md.
module etags_mod
    use, intrinsic :: iso_fortran_env, only: int64, int32
    use error_mod, only: status_type, set_error, FM_USER_ERROR, FM_GENERIC_ERROR, FM_FILE_MISSING
    use handle_mod, only: handle_type, null_handle
    use string_type_mod, only: string_type
    use value_mod, only: value_type, make_list, list, list_elements, vtag, V_STRING, xstring
    use data_mod, only: string_setting_type, history_setting_type, forward_setting, append_locals_clearer, choice_setting_type
    use hooks_mod, only: hook_type, capf_hook_type
    use commands_mod, only: commands
    use callint_mod, only: current_prefix_arg, PREFIX_NONE, prefix_numeric_value
    use buffer_mod, only: buffers, current, current_buffer, set_buffer, get_or_make_buffer, is_buffer_live, close_buffer, &
                          get_file_buffer, buffer_file_name, erase_buffer, set_buffer_read_only
    use marker_mod, only: copy_marker_at, marker_position, marker_buffer, destroy_marker
    use editfns_mod, only: point, point_min, point_max, goto_char, line_beginning_position, line_end_position, &
                           buffer_substring_no_properties, is_eob, char_after, buffer_size, widen, narrow_to_region, &
                           point_marker
    use cmds_mod, only: forward_line, forward_char, beginning_of_line, end_of_line
    use insdel_mod, only: insert
    use textprop_mod, only: put_text_property
    use search_mod, only: re_search_forward, re_search_backward, search_forward, looking_at, string_match, &
                          match_beginning, match_end, regexp_quote, case_fold_search, SEARCH_NOERROR
    use syntax_mod, only: skip_chars_backward, skip_chars_forward
    use subr_mod, only: find_tag_default, read_y_or_n
    use fns_mod, only: read_yes_or_no
    use simple_mod, only: push_mark
    use keymap_mod, only: keymap_set
    use modes_mod, only: major_modes
    use window_mod, only: switch_to_buffer_other_window, pop_to_buffer, bury_buffer, DISPLAY_ACTION_SAME_WINDOW
    use files_mod, only: find_file_noselect, locate_dominating_file, revert_buffer, revert_without_query, &
                         file_relative_name
    use files_mod, only: file_truename
    use fileio_mod, only: expand_file_name, file_name_as_directory, file_name_directory, file_exists, &
                          is_file_directory, is_visited_file_modtime_current
    use minibuf_mod, only: completion_ignore_case, read_string, try_completion, all_completions, is_exact_completion
    use minibuffer_mod, only: completing_read, read_file_name, completion_in_region, completion_at_point_functions
    use minibuf_table_mod, only: completion_table_type, completion_list_table, completion_function_table, &
                                 completion_test_procedure, capf_data_type, COMPLETE_TRY, COMPLETE_ALL, COMPLETE_TEST
    use replace_mod, only: format_prompt
    use apropos_mod, only: apropos_mode
    use help_mod, only: substitute_command_keys, with_output_to_temp_buffer
    use xdisp_mod, only: message
    use fns_mod, only: count_characters
    use coding_mod, only: coding_system_eol_type, buffer_file_coding_system, EOL_DOS
    use xref_mod, only: xref_backend_type, xref_location_type, xref_item_type, make_xref, append_xref_item, &
                        xref_backend_functions, xref_apropos_regexp, push_xref_marker_stack, clear_xref_marker_stack, &
                        go_back_in_xref_history
    implicit none(type, external)
    private

    public :: visit_tags_table, visit_tags_table_buffer, reset_tags_tables, tags_table_mode, tags_table_files, &
              tags_included_tables, tags_completion_table, tags_completion_at_point_function, file_of_tag, &
              find_tag_noselect, find_tag, find_tag_other_window, find_tag_regexp, list_tags, apropos_tags, &
              complete_tag, etags_xref_backend, goto_etags_tag_location, snarf_etags_tag, find_file_of_tag_noselect, &
              init_etags

    !> Values of `tags_case_fold_search' besides .true. (case-insensitive)
    !> and .false. (case-sensitive): the setting of `case_fold_search' (the
    !> default).
    integer, parameter, public :: TAGS_CASE_FOLD_SEARCH_DEFAULT = 2

    !> Values of `tags_add_tables' besides .true. (always add) and .false.
    !> (always start a new list): ask the user (the default).
    integer, parameter, public :: TAGS_ADD_TABLES_ASK = 2

    !> The values of the NEXT argument of `find_tag' and its variants: find
    !> a new tag, another tag that matches the last tagname or regexp used,
    !> or pop back to the previous tag gone to (upstream's `-').
    integer, parameter, public :: FIND_TAG_NEXT_NO = 0, FIND_TAG_NEXT_YES = 1, FIND_TAG_NEXT_POP_BACK = 2

    ! What `visit_tags_table_buffer' visits (its CONT): a first table (nil
    ! or a file name), the next table of the list (t), or the table of
    ! `tags_file_name' (`same').
    integer, parameter :: VISIT_FIRST = 0, VISIT_NEXT = 1, VISIT_SAME = 2

    ! The format of a tags table buffer: not recognized, etags, or empty
    ! (the functions of upstream's tags-table-format-functions).
    integer, parameter :: FORMAT_NONE = 0, FORMAT_ETAGS = 1, FORMAT_EMPTY = 2

    ! Stop searching if we find more than xref-limit matches, as the xref
    ! infrastructure is not designed to handle very long lists.
    ! Switching to some kind of lazy list might be better, but hopefully
    ! we hit the limit rarely.  Port of etags.el etags--xref-limit.
    integer, parameter :: ETAGS_XREF_LIMIT = 1000

    character(len=*), parameter :: FF = achar(12), LF = achar(10), DEL = achar(127), SOH = achar(1), &
                                   TB = achar(9), CR = achar(13)
    ! The property `face' and the faces of the *Tags List* buffer.
    character(len=*), parameter :: P_FACE = "face", FACE_HIGHLIGHT = "highlight"

    abstract interface
        !> A match qualifier of a tag order: whether the tag line at point
        !> qualifies the naive match of TAG just before point.  Not a port.
        logical function tag_match_test_procedure(tag)
            implicit none(type, external)
            character(len=*), intent(in) :: tag
        end function tag_match_test_procedure

        !> `find_tag_default_function': the default tag at point (FOUND
        !> false if there is none).  Not a port.
        subroutine find_tag_default_procedure(tag, found)
            implicit none(type, external)
            character(len=:), allocatable, intent(out) :: tag
            logical, intent(out) :: found
        end subroutine find_tag_default_procedure

        !> `default_tags_table_function': the default tags table file for
        !> the current buffer (FOUND false if there is none).  Not a port.
        subroutine default_tags_table_procedure(file, found)
            implicit none(type, external)
            character(len=:), allocatable, intent(out) :: file
            logical, intent(out) :: found
        end subroutine default_tags_table_procedure
    end interface

    public :: tag_match_test_procedure, find_tag_default_procedure, default_tags_table_procedure

    !> A match qualifier of a tag order: TEST, such as `is_tag_exact_match'.
    type, public :: tag_match_test_type
        procedure(tag_match_test_procedure), pointer, nopass :: test => null()
    end type tag_match_test_type

    !> What a tag line of a tags table says about the tag: TEXT, the
    !> initial part of a line containing the tag, or EXACT when the tag is
    !> a file name (upstream's TEXT t: the tag refers to exactly LINE or
    !> POSITION); LINE, the line number, and POSITION, the 0-based
    !> character position of TEXT within the file (0 for none, then
    !> HAS_POSITION is false).  Port of etags.el's TAG-INFO.
    type, public :: tag_info_type
        character(len=:), allocatable :: text
        logical :: exact = .false.
        integer(int64) :: line = 0
        integer(int64) :: position = 0
        logical :: has_position = .false.
    end type tag_info_type

    !> Location of an etags tag: its TAG_INFO in FILE.  Port of etags.el
    !> xref-etags-location.
    type, extends(xref_location_type), public :: xref_etags_location_type
        type(tag_info_type) :: tag_info
        character(len=:), allocatable :: file
    contains
        !> Port of etags.el:xref-location-marker (of an xref-etags-location).
        procedure :: marker => xref_etags_location_marker
        !> Port of etags.el:xref-location-group (of an xref-etags-location).
        procedure :: group => xref_etags_location_group
        !> Port of etags.el:xref-location-line (of an xref-etags-location).
        procedure :: line_number => xref_etags_location_line
    end type xref_etags_location_type

    !> The etags backend of xref_mod.  Port of etags.el's methods of the
    !> backend `etags'.
    type, extends(xref_backend_type), public :: etags_backend_type
    contains
        !> Port of etags.el:xref-backend-definitions (etags).
        procedure :: definitions => etags_backend_definitions
        !> Port of etags.el:xref-backend-apropos (etags).
        procedure :: apropos => etags_backend_apropos
        !> Port of etags.el:xref-backend-identifier-at-point (etags).
        procedure :: identifier_at_point => etags_backend_identifier_at_point
        !> Port of etags.el:xref-backend-identifier-completion-table (etags).
        procedure :: identifier_completion_table => etags_backend_identifier_completion_table
        !> Port of etags.el:xref-backend-identifier-completion-ignore-case
        !> (etags).
        procedure :: is_identifier_completion_case_ignored => is_etags_backend_identifier_completion_case_ignored
    end type etags_backend_type

    ! An element of `tags_table_computed_list': a table FILE, or a
    ! placeholder (upstream's t) saying the table before it has not been
    ! read into core and might include tables to search.
    type :: computed_entry_type
        character(len=:), allocatable :: file
        logical :: placeholder = .false.
    end type computed_entry_type

    ! A set of tags tables used together (an element of
    ! `tags_table_set_list').
    type :: tags_table_set_type
        type(string_type), allocatable :: files(:)
    end type tags_table_set_type

    ! A buffer's own tags state: the format of a tags table buffer and the
    ! cached files, included tables and completion table (upstream's
    ! buffer-local tags-table-files, tags-included-tables and
    ! tags-completion-table).
    type :: tags_buffer_type
        integer(int64) :: serial = 0
        integer :: format = FORMAT_NONE
        type(string_type), allocatable :: files(:), included(:), completion(:)
        logical :: has_completion_local = .false.
    end type tags_buffer_type

    ! A key of the definitions `find_etags_xref_definitions' found: FILE
    ! and LINE.
    type :: mark_key_type
        character(len=:), allocatable :: file
        integer(int64) :: line = 0
    end type mark_key_type

    !> File name of tags table.
    !> To switch to a new tags table, do not set this variable; instead,
    !> invoke `visit_tags_table', which is the only reliable way of
    !> setting the value of this variable, whether buffer-local or global.
    !> Use the `etags' program to make a tags table file.  Port of etags.el
    !> tags-file-name.
    type(string_setting_type), target, public :: tags_file_name = string_setting_type(name="tags_file_name")

    !> Whether tags operations should be case-sensitive.
    !> .true. means case-insensitive,
    !> .false. means case-sensitive.
    !> TAGS_CASE_FOLD_SEARCH_DEFAULT means use the setting of
    !> `case_fold_search'.  Port of etags.el tags-case-fold-search.
    type(choice_setting_type), public :: tags_case_fold_search = &
                                         choice_setting_type(name="tags_case_fold_search", choice=TAGS_CASE_FOLD_SEARCH_DEFAULT)

    !> List of file names of tags tables to search.
    !> An element that is a directory means the file "TAGS" in that
    !> directory.  To switch to a new list of tags tables, setting this
    !> variable is sufficient.  If you set this variable, do not also set
    !> `tags_file_name'.  Use the `etags' program to make a tags table file.
    !> Port of etags.el tags-table-list.
    character(len=:), allocatable, public :: tags_table_list(:)

    !> Control whether to add a new tags table to the current list.
    !> .true. means do; .false. means don't (always
    !> start a new list).  TAGS_ADD_TABLES_ASK means ask the user whether to
    !> add a new tags table to the current list (as opposed to starting a
    !> new list).  Port of etags.el tags-add-tables.
    type(choice_setting_type), public :: tags_add_tables = choice_setting_type(name="tags_add_tables", choice=TAGS_ADD_TABLES_ASK)

    !> True means reread a TAGS table without querying, if it has changed.
    !> Port of etags.el tags-revert-without-query.
    logical, public :: tags_revert_without_query = .false.

    !> Hook to be run by \[find_tag] after finding a tag.  See
    !> `execute_hooks'.  Port of etags.el find-tag-hook (its value in the
    !> buffer the tag is found in is run).
    type(hook_type), target, public :: find_tag_hook = hook_type(name="find_tag_hook")

    !> A function of no arguments used by \[find_tag] to pick a default
    !> tag.  If null, `find_tag_default' is used.  Port of etags.el
    !> find-tag-default-function (the major mode's
    !> `find_tag_default_function' property is not ported).
    procedure(find_tag_default_procedure), pointer, public :: find_tag_default_function => null()

    !> Face for tags in the output of `apropos_tags'.  Port of etags.el
    !> tags-tag-face.
    character(len=:), allocatable, public :: tags_tag_face

    !> If true, print the name of the tags file in the *Tags List* buffer.
    !> Port of etags.el tags-apropos-verbose.
    logical, public :: tags_apropos_verbose = .false.

    !> If associated, a function to choose a default tags file for a
    !> buffer.  This function receives no arguments and should give the
    !> default tags table file to use for the current buffer.  Port of
    !> etags.el default-tags-table-function.
    procedure(default_tags_table_procedure), pointer, public :: default_tags_table_function => null()

    !> Tag order used in `definitions' of the etags backend to look for
    !> definitions.
    !>
    !> If you want `find_xref_definitions' to find the tagged files by their
    !> file name, add `is_tag_partial_file_name_match' to the array.
    !> Port of etags.el etags-xref-find-definitions-tag-order.
    type(tag_match_test_type), allocatable, public :: etags_find_xref_definitions_tag_order(:)

    !> True means show the matches in the current file first.  Port of
    !> etags.el etags-xref-prefer-current-file.
    logical, public :: etags_xref_prefer_current_file = .false.

    !> Last tag found by \[find_tag].  Port of etags.el last-tag.
    character(len=:), allocatable, public :: last_tag

    !> The history of `find_tag''s tag names.  Port of etags.el
    !> find-tag-history.
    type(history_setting_type), target, public :: find_tag_history = history_setting_type(name="find_tag_history")

    !> Hook run after entering Tags Table mode.  Port of the hook of
    !> etags.el tags-table-mode.
    type(hook_type), target, public :: tags_table_mode_hook = hook_type(name="tags_table_mode_hook")

    ! List of tags tables to search, computed from `tags_table_list'.
    ! This includes tables implicitly included by other tables.  The list
    ! is not always complete: the included tables of a table are not known
    ! until that table is read into core.  A placeholder element says that
    ! the preceding element is a table that has not been read into core
    ! and might contain included tables to search.  Port of etags.el
    ! tags-table-computed-list.
    type(computed_entry_type), allocatable :: tags_table_computed_list(:)
    ! Value of `tags_table_list' that `tags_table_computed_list'
    ! corresponds to.  Port of etags.el tags-table-computed-list-for.
    type(string_type), allocatable :: tags_table_computed_list_for(:)
    ! Index in `tags_table_computed_list' of the current state of
    ! searching (0 for none), and where the current search started.
    ! Port of etags.el tags-table-list-pointer and tags-table-list-started-at.
    integer :: tags_table_list_pointer = 0, tags_table_list_started_at = 0
    ! List of sets of tags table which have been used together in the
    ! past.  Port of etags.el tags-table-set-list.
    type(tags_table_set_type), allocatable :: tags_table_set_list(:)
    ! The global value of the completion table, and whether it is set.
    type(string_type), allocatable :: tags_completion_global(:)
    ! Ring of markers which are locations visited by \[find_tag], the most
    ! recent last.  Port of etags.el tags-location-ring.
    type(handle_type), allocatable :: tags_location_ring(:)
    ! Matches remembered between calls.  Port of etags.el
    ! tag-lines-already-matched.
    type(handle_type), allocatable :: tag_lines_already_matched(:)
    ! The buffers' own tags state.
    type(tags_buffer_type), allocatable :: tags_buffers(:)
    ! The tag orders of an etags table (find-tag-tag-order and
    ! find-tag-regexp-tag-order).
    type(tag_match_test_type), allocatable :: find_tag_tag_order(:), find_tag_regexp_tag_order(:)
    ! The buffer `tags_lazy_completion_table' completes for.
    type(handle_type) :: lazy_buffer

contains

    ! ------------------------------------------------------------------
    ! Buffer state
    ! ------------------------------------------------------------------

    ! The index of the current buffer's tags state (0 for none).
    ! Not a port.
    integer function tags_buffer_index() result(k)
        k = 0
        if (current == 0 .or. .not. allocated(tags_buffers)) return
        k = findloc(tags_buffers%serial, buffers(current)%serial, dim=1)
    end function tags_buffer_index

    ! The current buffer's tags state, made if needed: its index K.
    ! Not a port.
    subroutine make_tags_buffer_entry(k)
        integer, intent(out) :: k
        type(tags_buffer_type) :: entry

        k = tags_buffer_index()
        if (k /= 0) return
        if (.not. allocated(tags_buffers)) allocate (tags_buffers(0))
        entry%serial = buffers(current)%serial
        k = findloc(tags_buffers%serial, 0_int64, dim=1)

        if (k == 0) then
            tags_buffers = [tags_buffers, entry]
            k = size(tags_buffers)
        else
            tags_buffers(k) = entry
        end if
    end subroutine make_tags_buffer_entry

    ! Forget the current buffer's tags state (clear_all_local_variables).
    ! Not a port.
    subroutine clear_tags_locals()
        integer :: k

        k = tags_buffer_index()
        if (k /= 0) tags_buffers(k)%serial = 0
    end subroutine clear_tags_locals

    ! The format of the current buffer's tags table.  Not a port.
    integer function current_format() result(format)
        integer :: k

        format = FORMAT_NONE
        k = tags_buffer_index()
        if (k /= 0) format = tags_buffers(k)%format
    end function current_format

    ! The value of `tags_file_name' in the current buffer ("" for none).
    ! Not a port.
    function current_tags_file_name() result(name)
        character(len=:), allocatable :: name

        name = ""
        if (allocated(tags_file_name%value)) name = tags_file_name%value
    end function current_tags_file_name

    ! Set the default value of `tags_file_name' to NAME (unset for ""),
    ! whether or not the current buffer has its own.  Not a port.
    subroutine set_default_tags_file_name(name)
        character(len=*), intent(in) :: name
        character(len=:), allocatable :: local
        logical :: had_local

        had_local = tags_file_name%local_p()

        if (had_local) then
            local = current_tags_file_name()
            call tags_file_name%clear_local()
        end if

        if (len(name) > 0) then
            tags_file_name = name
        else
            call tags_file_name%clear()
        end if

        if (had_local) call tags_file_name%set_local(local)
    end subroutine set_default_tags_file_name

    ! Set the current buffer's binding of `tags_file_name' to NAME (unset
    ! for "").  Not a port.
    subroutine set_tags_file_name(name)
        character(len=*), intent(in) :: name

        if (len(name) > 0) then
            tags_file_name = name
        else
            call tags_file_name%clear()
        end if
    end subroutine set_tags_file_name

    ! Whether a tags table is in use: `tags_table_list' or
    ! `tags_file_name' is set.  Not a port.
    logical function has_tags_tables()
        has_tags_tables = allocated(tags_file_name%value)
        if (allocated(tags_table_list)) has_tags_tables = has_tags_tables .or. size(tags_table_list) > 0
    end function has_tags_tables

    ! `tags_table_list' as an array of strings.  Not a port.
    subroutine get_tags_table_list(files)
        type(string_type), allocatable, intent(out) :: files(:)
        integer :: i

        allocate (files(0))
        if (.not. allocated(tags_table_list)) return
        deallocate (files)
        allocate (files(size(tags_table_list)))

        do i = 1, size(tags_table_list)
            files(i)%text = trim(tags_table_list(i))
        end do
    end subroutine get_tags_table_list

    ! Set `tags_table_list' to FILES.  Not a port.
    subroutine set_tags_table_list(files)
        type(string_type), intent(in) :: files(:)
        integer :: i, n

        n = 1

        do i = 1, size(files)
            n = max(n, len(files(i)%text))
        end do

        if (allocated(tags_table_list)) deallocate (tags_table_list)
        allocate (character(len=n) :: tags_table_list(size(files)))

        do i = 1, size(files)
            tags_table_list(i) = files(i)%text
        end do
    end subroutine set_tags_table_list

    ! Whether string lists A and B are equal.  Not a port.
    logical function is_same_string_list(a, b)
        type(string_type), intent(in) :: a(:), b(:)
        integer :: i

        is_same_string_list = size(a) == size(b)
        if (.not. is_same_string_list) return

        do i = 1, size(a)
            if (a(i)%text /= b(i)%text .or. len(a(i)%text) /= len(b(i)%text)) then
                is_same_string_list = .false.
                return
            end if
        end do
    end function is_same_string_list

    ! Whether FILES has FILE.  Not a port.
    logical function has_file(files, file)
        type(string_type), intent(in) :: files(:)
        character(len=*), intent(in) :: file
        integer :: i

        has_file = .false.

        do i = 1, size(files)
            if (files(i)%text == file .and. len(files(i)%text) == len(file)) then
                has_file = .true.
                return
            end if
        end do
    end function has_file

    ! ------------------------------------------------------------------
    ! Tags tables
    ! ------------------------------------------------------------------

    !> Initialize the tags table in the current buffer: VALID is true if it
    !> is a valid tags table, and in that case, also give it its own tags
    !> table state, empty.  Port of etags.el:initialize-new-tags-table (the
    !> formats are those of upstream's tags-table-format-functions,
    !> etags and empty tables).
    subroutine initialize_new_tags_table(valid)
        logical, intent(out) :: valid
        integer :: k
        integer(int32) :: c
        logical :: found

        call make_tags_buffer_entry(k)
        if (allocated(tags_buffers(k)%files)) deallocate (tags_buffers(k)%files)
        if (allocated(tags_buffers(k)%included)) deallocate (tags_buffers(k)%included)
        if (allocated(tags_buffers(k)%completion)) deallocate (tags_buffers(k)%completion)
        tags_buffers(k)%has_completion_local = .true.
        tags_buffers(k)%format = FORMAT_NONE
        ! etags-recognize-tags-table.
        call char_after(c, found, point_min())

        if (found .and. c == 12) then
            tags_buffers(k)%format = FORMAT_ETAGS
        else if (buffer_size() == 0) then
            ! tags-recognize-empty-tags-table.
            tags_buffers(k)%format = FORMAT_EMPTY
        end if

        valid = tags_buffers(k)%format /= FORMAT_NONE
    end subroutine initialize_new_tags_table

    !> Major mode for tags table file buffers.  Port of
    !> etags.el:tags-table-mode.
    recursive subroutine tags_table_mode(status)
        type(status_type), intent(out) :: status
        logical :: saved, valid

        call major_modes%tags_table_mode%begin(saved, status)
        if (.not. status%ok()) return
        buffers(current)%undo_enabled = .false.
        if (allocated(buffers(current)%undo_entries)) deallocate (buffers(current)%undo_entries)
        call initialize_new_tags_table(valid)
        call major_modes%tags_table_mode%end(saved, status)
    end subroutine tags_table_mode

    ! Whether the current buffer is a valid tags table of its format
    ! (verify-tags-table-function).  Port of
    ! etags.el:etags-verify-tags-table and the verify function of
    ! tags-recognize-empty-tags-table.
    logical function is_valid_tags_table() result(valid)
        integer(int32) :: c
        logical :: found

        select case (current_format())
        case (FORMAT_ETAGS)
            call char_after(c, found, point_min())
            valid = found .and. c == 12
        case (FORMAT_EMPTY)
            valid = buffer_size() == 0
        case default
            valid = .false.
        end select
    end function is_valid_tags_table

    !> Tell tags commands to use tags table file FILE.
    !> FILE should be the name of a file created with the `etags' program.
    !> A directory name is ok too; it means file TAGS in that directory.
    !>
    !> Normally \[visit_tags_table] sets the global value of
    !> `tags_file_name'.  With a prefix arg, set the buffer-local value
    !> instead.  When called from Fortran, if the optional arg LOCAL is
    !> true, set the local value.  When you find a tag with \[find_tag],
    !> the buffer it finds the tag in is given a local value of this
    !> variable which is the name of the tags file the tag was in.  Port of
    !> etags.el:visit-tags-table.
    recursive subroutine visit_tags_table(file, status, local)
        character(len=*), intent(in) :: file
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: local
        type(handle_type) :: cbuf, target_buffer
        type(status_type) :: st
        character(len=:), allocatable :: saved, expanded
        logical :: had_value, visited, is_local
        integer(int64) :: opoint
        integer :: k

        is_local = .false.
        if (present(local)) is_local = local
        ! Bind tags-file-name so we can control below whether the local or
        ! global value gets set.
        cbuf = current_buffer()
        had_value = allocated(tags_file_name%value)
        saved = current_tags_file_name()
        tags_file_name = file
        opoint = point()
        call visit_tags_table_buffer(visited, status, VISIT_FIRST, file=file)
        target_buffer = current_buffer()
        call set_buffer(cbuf, st)

        if (status%ok() .and. .not. visited) then
            call set_error(status, FM_FILE_MISSING, "Visiting tags table: No such file or directory, "//file)
        end if

        ! Set FILE to the expanded name.  Do that in the buffer we started
        ! from, because visit-tags-table-buffer switches buffers after
        ! updating tags-file-name, so if tags-file-name is local in the
        ! buffer we started, that value is only visible in that buffer.
        expanded = current_tags_file_name()

        if (had_value) then
            tags_file_name = saved
        else
            call tags_file_name%clear()
        end if

        call goto_char(opoint)
        if (.not. status%ok()) return

        if (is_local) then
            ! Force recomputation of tags-completion-table.
            call make_tags_buffer_entry(k)
            tags_buffers(k)%has_completion_local = .true.
            if (allocated(tags_buffers(k)%completion)) deallocate (tags_buffers(k)%completion)
            ! Set the local value of tags-file-name.
            call tags_file_name%set_local(expanded)
        else
            ! Set the global value of tags-file-name.
            call set_default_tags_file_name(expanded)
            call clear_completion_table()
        end if
    end subroutine visit_tags_table

    ! Forget the completion table the current buffer sees (its own, or else
    ! the global one).  Not a port.
    subroutine clear_completion_table()
        integer :: k

        k = tags_buffer_index()

        if (k /= 0) then
            if (tags_buffers(k)%has_completion_local) then
                if (allocated(tags_buffers(k)%completion)) deallocate (tags_buffers(k)%completion)
                return
            end if
        end if

        if (allocated(tags_completion_global)) deallocate (tags_completion_global)
    end subroutine clear_completion_table

    ! The command: read the file, then `visit_tags_table'.  Not a port.
    recursive subroutine cmd_visit_tags_table(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: directory, file, default_tag_directory, default_file
        type(status_type) :: st
        logical :: found

        directory = "./"
        if (allocated(buffers(current)%directory)) directory = buffers(current)%directory
        call locate_dominating_file(directory, "TAGS", default_tag_directory, found)
        if (.not. found) default_tag_directory = directory
        call expand_file_name("TAGS", default_file, st, default_tag_directory)
        ! default to TAGS from default-directory up to root.
        call read_file_name(format_prompt("Visit tags table", "TAGS"), file, status, dir=default_tag_directory, &
                            default_filename=default_file, mustmatch=1)
        if (.not. status%ok()) return
        call visit_tags_table(file, status, current_prefix_arg%kind /= PREFIX_NONE)
    end subroutine cmd_visit_tags_table

    ! Insert the entries ENTRIES into the computed list at index AT,
    ! replacing the placeholder there (REPLACE) or not, and keep the
    ! pointers into the list on their elements.  Not a port.
    subroutine splice_computed(at, entries, replace)
        integer, intent(in) :: at
        type(computed_entry_type), intent(in) :: entries(:)
        logical, intent(in) :: replace
        type(computed_entry_type), allocatable :: grown(:)
        integer :: n, removed, shift

        n = size(tags_table_computed_list)
        removed = 0
        if (replace) removed = 1
        allocate (grown(n - removed + size(entries)))
        grown(1:at - 1) = tags_table_computed_list(1:at - 1)
        grown(at:at + size(entries) - 1) = entries
        grown(at + size(entries):) = tags_table_computed_list(at + removed:)
        call move_alloc(grown, tags_table_computed_list)
        shift = size(entries) - removed
        if (tags_table_list_pointer >= at + removed) tags_table_list_pointer = tags_table_list_pointer + shift
        if (tags_table_list_started_at >= at + removed) tags_table_list_started_at = tags_table_list_started_at + shift
    end subroutine splice_computed

    ! Whether element I of the computed list is a placeholder.
    ! Not a port.
    logical function is_placeholder(i)
        integer, intent(in) :: i

        is_placeholder = .false.
        if (i < 1 .or. i > size(tags_table_computed_list)) return
        is_placeholder = tags_table_computed_list(i)%placeholder
    end function is_placeholder

    ! The computed entries of TABLES (expanded file names): each table,
    ! with the tables it includes after it when it is in core, or a
    ! placeholder after it when it is not.  Port of the loops of
    ! etags.el:tags-table-check-computed-list and
    ! tags-table-extend-computed-list.
    recursive subroutine compute_tables(tables, computed)
        type(string_type), allocatable, intent(inout) :: tables(:)
        type(computed_entry_type), allocatable, intent(out) :: computed(:)
        type(computed_entry_type) :: entry, placeholder
        type(string_type), allocatable :: included(:), rest(:)
        type(handle_type) :: table_buffer, old
        type(status_type) :: st
        character(len=:), allocatable :: name
        logical :: found, valid
        integer :: i, j

        allocate (computed(0))
        placeholder%placeholder = .true.
        i = 1

        do while (i <= size(tables))
            entry%file = tables(i)%text
            computed = [computed, entry]
            table_buffer = get_file_buffer(tables(i)%text)
            valid = .false.
            old = current_buffer()

            if (table_buffer%index /= 0) then
                ! There is a buffer visiting the file.  Now make sure it is
                ! initialized as a tag table buffer.
                call buffer_file_name(name, found, st, table_buffer)
                if (found) call verify_tags_table(name, valid, st)
                call set_buffer(old, st)
            end if

            if (valid) then
                call set_buffer(table_buffer, st)
                call tags_included_tables(included, st)
                call set_buffer(old, st)

                if (size(included) > 0) then
                    ! Insert the included tables into the list we are
                    ! processing.
                    allocate (rest(size(tables) + size(included)))
                    rest(1:i) = tables(1:i)

                    do j = 1, size(included)
                        rest(i + j)%text = expand_tags_table_name(included(j)%text)
                    end do

                    rest(i + size(included) + 1:) = tables(i + 1:)
                    call move_alloc(rest, tables)
                end if
            else
                ! This table is not in core yet.  Insert a placeholder
                ! saying we must read it into core to check for included
                ! tables before searching the next table in the list.
                computed = [computed, placeholder]
            end if

            i = i + 1
        end do
    end subroutine compute_tables

    !> Compute `tags_table_computed_list' from `tags_table_list' if
    !> necessary.  Port of etags.el:tags-table-check-computed-list.
    recursive subroutine check_tags_table_computed_list()
        type(string_type), allocatable :: files(:), tables(:)
        type(computed_entry_type), allocatable :: computed(:)
        integer :: i

        call get_tags_table_list(files)

        do i = 1, size(files)
            files(i)%text = expand_tags_table_name(files(i)%text)
        end do

        if (allocated(tags_table_computed_list_for)) then
            if (is_same_string_list(tags_table_computed_list_for, files)) return
        end if

        ! The list (or default-directory) has changed since last computed.
        tables = files
        call compute_tables(tables, computed)
        ! Record the tags-table-list value (and the context of the current
        ! directory) we computed from.
        tags_table_computed_list_for = files
        call move_alloc(computed, tags_table_computed_list)
    end subroutine check_tags_table_computed_list

    !> Extend `tags_table_computed_list' to remove the first placeholder.
    !>
    !> A placeholder indicates that the preceding element is a table that
    !> has not been read in and might contain included tables to search.
    !> This function reads in the first such table and puts its included
    !> tables into the list.  Port of
    !> etags.el:tags-table-extend-computed-list.
    recursive subroutine extend_tags_table_computed_list()
        type(string_type), allocatable :: tables(:)
        type(computed_entry_type), allocatable :: computed(:)
        type(handle_type) :: old
        type(status_type) :: st
        integer :: j, n
        logical :: valid

        n = size(tags_table_computed_list)
        j = 0

        do j = 1, n - 1
            if (tags_table_computed_list(j + 1)%placeholder) exit
        end do

        if (j >= n) return
        old = current_buffer()
        call verify_tags_table(tags_table_computed_list(j)%file, valid, st)

        if (valid) then
            ! We are now in the buffer visiting (car LIST).  Extract its
            ! list of included tables and insert it into the computed list.
            call tags_included_tables(tables, st)
            call set_buffer(old, st)
            call compute_tables(tables, computed)
            ! COMPUTED now contains the list of included tables (and tables
            ! included by them, etc.).  Now splice this into the current
            ! list.
            call splice_computed(j + 1, computed, .true.)
        else
            call set_buffer(old, st)
            ! It was not a valid table, so just remove the following
            ! placeholder.
            allocate (computed(0))
            call splice_computed(j + 1, computed, .true.)
        end if
    end subroutine extend_tags_table_computed_list

    !> Expand tags table name FILE into a complete file name.  Port of
    !> etags.el:tags-expand-table-name.
    recursive function expand_tags_table_name(file) result(expanded)
        character(len=*), intent(in) :: file
        character(len=:), allocatable :: expanded
        character(len=:), allocatable :: e
        type(status_type) :: st

        call expand_file_name(file, e, st)

        if (is_file_directory(e)) then
            call expand_file_name("TAGS", expanded, st, file_name_as_directory(e))
        else
            expanded = e
        end if
    end function expand_tags_table_name

    ! The index in the computed list of the first table that is FILE after
    ! `expand_tags_table_name' on both sides, from index FROM (0 for none).
    ! Port of etags.el:tags-table-list-member (for the computed list).
    recursive integer function computed_index(file, from) result(index)
        character(len=*), intent(in) :: file
        integer, intent(in) :: from
        character(len=:), allocatable :: expanded
        integer :: i

        index = 0
        expanded = expand_tags_table_name(file)

        do i = max(from, 1), size(tags_table_computed_list)
            if (tags_table_computed_list(i)%placeholder) cycle

            if (expanded == expand_tags_table_name(tags_table_computed_list(i)%file)) then
                index = i
                return
            end if
        end do
    end function computed_index

    ! Whether FILES has FILE after `expand_tags_table_name' on both sides.
    ! Port of etags.el:tags-table-list-member (for a list of names).
    recursive logical function is_tags_table_member(file, files) result(member)
        character(len=*), intent(in) :: file
        type(string_type), intent(in) :: files(:)
        character(len=:), allocatable :: expanded
        integer :: i

        member = .false.
        expanded = expand_tags_table_name(file)

        do i = 1, size(files)
            if (expanded == expand_tags_table_name(files(i)%text)) then
                member = .true.
                return
            end if
        end do
    end function is_tags_table_member

    !> Read FILE into a buffer and verify that it is a valid tags table.
    !> Sets the current buffer to one visiting FILE (if it exists).
    !> VALID is true if it is a valid table.  Port of
    !> etags.el:tags-verify-table.
    recursive subroutine verify_tags_table(file, valid, status)
        character(len=*), intent(in) :: file
        logical, intent(out) :: valid
        type(status_type), intent(out) :: status
        type(handle_type) :: target_buffer
        type(status_type) :: st
        type(string_type), allocatable :: files(:)
        character(len=:), allocatable :: newfile
        logical :: found, revert
        integer :: i

        valid = .false.
        status = status_type()
        target_buffer = get_file_buffer(file)

        if (target_buffer%index /= 0) then
            ! The file is already in a buffer.  Check for the visited file
            ! having changed since we last used it.
            call set_buffer(target_buffer, st)
            if (current_format() == FORMAT_NONE) call tags_table_mode(st)

            ! 'verify-visited-file-modtime' return true if the tags table
            ! file was meanwhile deleted.  Avoid asking the question below
            ! again if so.
            if (.not. is_visited_file_modtime_current() .and. file_exists(file)) then
                ! Decide whether to revert the file.  revert-without-query
                ! can say to revert or the user can say to revert.
                revert = tags_revert_without_query

                if (allocated(revert_without_query) .and. allocated(buffers(current)%filename)) then
                    do i = 1, size(revert_without_query)
                        if (string_match(trim(revert_without_query(i)), buffers(current)%filename, st) /= 0) then
                            revert = .true.
                        end if
                    end do
                end if

                if (.not. revert) then
                    call read_yes_or_no("Tags file "//file//" has changed, read new contents? ", revert, status)
                    if (.not. status%ok()) return
                end if

                if (revert) then
                    call revert_buffer(status, ignore_auto=.true., noconfirm=.true.)
                    if (.not. status%ok()) return
                    call tags_table_mode(status)
                    if (.not. status%ok()) return
                end if
            end if

            valid = is_valid_tags_table()
        else if (file_exists(file)) then
            call find_file_noselect(file, target_buffer, status)
            if (.not. status%ok()) return
            call buffer_file_name(newfile, found, st, target_buffer)

            if (found .and. newfile /= file) then
                ! find-file-noselect has changed the file name.  Propagate
                ! the change to tags-file-name and tags-table-list.
                call get_tags_table_list(files)

                do i = 1, size(files)
                    if (files(i)%text == file) then
                        files(i)%text = newfile
                        call set_tags_table_list(files)
                        exit
                    end if
                end do

                if (current_tags_file_name() == file) call set_tags_file_name(newfile)
            end if

            ! Only change buffer now that we're done using potentially
            ! buffer-local variables.
            call set_buffer(target_buffer, st)
            call tags_table_mode(status)
            if (.not. status%ok()) return
            valid = is_valid_tags_table()
        end if
    end subroutine verify_tags_table

    !> Search current tags tables for tags for THIS_FILE.
    !> Subroutine of `visit_tags_table_buffer'.
    !> Looks for a tags table that has such tags or that includes a table
    !> that has them.  TABLE is the name of the first such table (FOUND
    !> false for none).  CORE_ONLY true means check only tags tables that
    !> are already in buffers.  Port of etags.el:tags-table-including.
    recursive subroutine tags_table_including(this_file, core_only, table, found)
        character(len=*), intent(in) :: this_file
        logical, intent(in) :: core_only
        character(len=:), allocatable, intent(out) :: table
        logical, intent(out) :: found
        type(string_type), allocatable :: files(:), list(:)
        type(handle_type) :: old
        type(status_type) :: st
        character(len=:), allocatable :: expanded
        integer :: tables, hit, elt, i
        logical :: valid

        table = ""
        found = .false.
        if (.not. allocated(tags_table_computed_list)) return
        tables = 1
        hit = 0
        old = current_buffer()

        ! Loop over the list, looking for a table containing tags for
        ! THIS-FILE.
        do while (hit == 0 .and. tables <= size(tags_table_computed_list))
            if (core_only) then
                ! Skip tables not in core.
                do while (is_placeholder(tables + 1))
                    tables = tables + 2
                end do
            else if (is_placeholder(tables + 1)) then
                ! This table has not been read into core yet.  Read it in
                ! now.
                call extend_tags_table_computed_list()
            end if

            if (tables <= size(tags_table_computed_list)) then
                ! Select the tags table buffer and get the file list up to
                ! date.
                call verify_tags_table(expand_tags_table_name(tags_table_computed_list(tables)%file), valid, st)

                if (valid) then
                    call tags_table_files(files, st)

                    do i = 1, size(files)
                        call expand_file_name(files(i)%text, expanded, st)

                        if (expanded == this_file) then
                            ! Found it.
                            hit = tables
                            exit
                        end if
                    end do
                end if

                call set_buffer(old, st)
            end if

            tables = tables + 1
        end do

        if (hit == 0) return
        ! Now determine if the table we found was one included by another
        ! table, not explicitly listed.  We do this by checking each
        ! element of the computed list to see if it appears in the user's
        ! explicit list; the last element we will check is FOUND itself.
        ! Then we return the last one which did in fact appear in
        ! tags-table-list.
        call get_tags_table_list(list)
        elt = 1

        do while (elt <= hit)
            if (is_tags_table_member(tags_table_computed_list(elt)%file, list)) then
                ! This table appears in the user's list, so it could be the
                ! one which includes the table we found.
                table = tags_table_computed_list(elt)%file
                found = .true.
            end if

            elt = elt + 1
            if (is_placeholder(elt)) elt = elt + 1
        end do
    end subroutine tags_table_including

    !> Move `tags_table_list_pointer' along and set `tags_file_name'.
    !> Subroutine of `visit_tags_table_buffer'.  MORE is false when out of
    !> tables.  Port of etags.el:tags-next-table.
    recursive subroutine next_tags_table(more)
        logical, intent(out) :: more

        ! If there is a placeholder element next, compute the list to
        ! replace it.
        do while (is_placeholder(tags_table_list_pointer + 1))
            call extend_tags_table_computed_list()
        end do

        ! Go to the next table in the list.
        tags_table_list_pointer = tags_table_list_pointer + 1

        ! Wrap around.
        if (tags_table_list_pointer > size(tags_table_computed_list)) then
            tags_table_list_pointer = 0
            if (size(tags_table_computed_list) > 0) tags_table_list_pointer = 1
        end if

        if (tags_table_list_pointer == tags_table_list_started_at) then
            ! We have come full circle.  No more tables.
            tags_table_list_pointer = 0
            more = .false.
        else
            ! Set tags-file-name to the name from the list.  It is already
            ! expanded.
            call set_tags_file_name(tags_table_computed_list(tags_table_list_pointer)%file)
            more = .true.
        end if
    end subroutine next_tags_table

    !> Select the buffer containing the current tags table.
    !> CONT specifies which tags table to visit: the file FILE when it is
    !> given; with VISIT_NEXT, the next table in `tags_table_list'; with
    !> VISIT_SAME, don't look for a new table, just select the buffer
    !> visiting `tags_file_name'; with VISIT_FIRST, choose a first buffer
    !> from information in `tags_file_name', `tags_table_list',
    !> `tags_table_list_pointer'.  CBUF, if present, specifies the initial
    !> buffer, which is important if that buffer has a local value of
    !> `tags_file_name'.  VISITED is true if it visits a tags table, false
    !> if there are no more in the list.  Port of
    !> etags.el:visit-tags-table-buffer.
    recursive subroutine visit_tags_table_buffer(visited, status, cont, file, cbuf)
        logical, intent(out) :: visited
        type(status_type), intent(out) :: status
        integer, intent(in), optional :: cont
        character(len=*), intent(in), optional :: file
        type(handle_type), intent(in), optional :: cbuf
        type(handle_type) :: curbuf, buffer
        type(status_type) :: st
        type(string_type), allocatable :: list(:), set(:)
        type(tags_table_set_type) :: entry
        character(len=:), allocatable :: name, local_tags_file_name, bfn
        integer :: mode, found, s, i
        logical :: more, has, valid, keep, is_new

        visited = .false.
        status = status_type()
        mode = VISIT_FIRST
        if (present(cont)) mode = cont
        is_new = mode == VISIT_FIRST
        if (present(cbuf)) call set_buffer(cbuf, st)
        if (.not. allocated(tags_table_computed_list)) allocate (tags_table_computed_list(0))

        ! Set tags-file-name to the tags table file we want to visit.
        select case (mode)
        case (VISIT_SAME)
            ! Use the ambient value of tags-file-name.
            if (len(current_tags_file_name()) == 0) then
                call set_error(status, FM_USER_ERROR, substitute_command_keys("No tags table in use; "// &
                                                                              "use \[visit_tags_table] to select one"))
                return
            end if
        case (VISIT_NEXT)
            ! Find the next table.
            call next_tags_table(more)

            ! Skip over nonexistent files.
            do while (more)
                name = current_tags_file_name()
                buffer = get_file_buffer(name)
                if (buffer%index /= 0 .or. file_exists(name)) exit
                call next_tags_table(more)
            end do
        case default
            ! Pick a table out of our hat.
            call check_tags_table_computed_list()
            has = .false.

            ! If passed a string, use that.
            if (present(file)) then
                name = file
                has = .true.
            end if

            ! First, try a local variable.
            if (.not. has .and. tags_file_name%local_p()) then
                name = current_tags_file_name()
                has = len(name) > 0
            end if

            ! Second, try a user-specified function to guess.
            if (.not. has .and. associated(default_tags_table_function)) then
                call default_tags_table_function(name, has)
            end if

            ! Third, look for a tags table that contains tags for the
            ! current buffer's file.  If one is found, the lists will be
            ! frobnicated, and CONT will be set so we don't do it below.
            if (.not. has .and. allocated(buffers(current)%filename)) then
                bfn = buffers(current)%filename
                curbuf = current_buffer()
                ! First check only tables already in buffers.
                call tags_table_including(bfn, .true., name, has)
                ! Since that didn't find any, now do the expensive version:
                ! reading new files.
                if (.not. has) call tags_table_including(bfn, .false., name, has)
                call set_buffer(curbuf, st)
                if (has) is_new = .false.
            end if

            ! Fourth, use the user variable tags-file-name, if it is not
            ! already in the current list.
            if (.not. has) then
                name = current_tags_file_name()

                if (len(name) > 0) then
                    has = computed_index(name, 1) == 0
                end if
            end if

            ! Fifth, use the user variable giving the table list.  Find the
            ! first element of the list that actually exists.
            if (.not. has) then
                call get_tags_table_list(list)

                do i = 1, size(list)
                    name = expand_tags_table_name(list(i)%text)
                    buffer = get_file_buffer(name)

                    if (buffer%index /= 0 .or. file_exists(name)) then
                        name = list(i)%text
                        has = .true.
                        exit
                    end if
                end do
            end if

            ! Finally, prompt the user for a file name.
            if (.not. has) then
                call read_file_name(format_prompt("Visit tags table", "TAGS"), name, status, &
                                    default_filename="TAGS", mustmatch=1)
                if (.not. status%ok()) return
                call expand_file_name(name, bfn, st)
                name = bfn
            end if

            call set_tags_file_name(name)
        end select

        ! Expand the table name into a full file name.
        if (len(current_tags_file_name()) > 0) call set_tags_file_name(expand_tags_table_name(current_tags_file_name()))
        if (mode == VISIT_NEXT .and. tags_table_list_pointer == 0) return
        ! Verify that tags-file-name names a valid tags table.  Bind another
        ! variable with the value of tags-file-name before we switch
        ! buffers, in case tags-file-name is buffer-local.
        curbuf = current_buffer()
        local_tags_file_name = current_tags_file_name()
        call verify_tags_table(local_tags_file_name, valid, status)
        if (.not. status%ok()) return

        if (.not. valid) then
            ! The buffer was not valid.  Don't use it again.
            call set_buffer(curbuf, st)
            call tags_file_name%clear_local()
            if (current_tags_file_name() == local_tags_file_name) call tags_file_name%clear()

            if (file_exists(local_tags_file_name)) then
                call set_error(status, FM_USER_ERROR, "File "//local_tags_file_name//" is not a valid tags table")
            else
                call set_error(status, FM_USER_ERROR, "File "//local_tags_file_name//" does not exist")
            end if

            return
        end if

        ! Bury the tags table buffer so it doesn't get in the user's way.
        call bury_buffer(st, current_buffer())

        ! If this was a new table selection (CONT is nil), make sure
        ! tags-table-list includes the chosen table, and update the list
        ! pointer variables.
        if (is_new) then
            ! Look in the list for the table we chose.
            found = computed_index(local_tags_file_name, 1)

            if (found /= 0) then
                ! There it is.  Just switch to it.
                tags_table_list_pointer = found
                tags_table_list_started_at = found
            else
                ! The table is not in the current set.  Try to find it in
                ! another previously used set.
                if (.not. allocated(tags_table_set_list)) allocate (tags_table_set_list(0))
                s = 0

                do i = 1, size(tags_table_set_list)
                    if (is_tags_table_member(local_tags_file_name, tags_table_set_list(i)%files)) then
                        s = i
                        exit
                    end if
                end do

                call get_tags_table_list(list)

                if (s /= 0) then
                    ! Found in some other set.  Switch to that set.
                    ! Save the current list.
                    if (.not. is_saved_set(list)) call save_set(list)
                    set = tags_table_set_list(s)%files
                    call set_tags_table_list(set)
                else
                    ! Not found in any existing set.
                    keep = .false.

                    if (size(list) > 0) then
                        if (tags_add_tables%value) then
                            keep = .true.
                        else if (tags_add_tables%choice == TAGS_ADD_TABLES_ASK) then
                            call read_y_or_n("Keep current list of tags tables also? ", keep, status)
                            if (.not. status%ok()) return
                        end if
                    end if

                    if (keep) then
                        ! Add it to the current list.
                        allocate (set(size(list) + 1))
                        set(1)%text = local_tags_file_name
                        set(2:) = list
                        call set_tags_table_list(set)
                    else
                        ! Make a fresh list, and store the old one.
                        call message("Starting a new list of tags tables")
                        if (size(list) > 0 .and. .not. is_saved_set(list)) call save_set(list)

                        ! Clear out buffers holding old tables.
                        do i = 1, size(list)
                            buffer = get_file_buffer(expand_tags_table_name(list(i)%text))
                            if (buffer%index /= 0) call close_buffer(buffer, st)
                        end do

                        allocate (set(1))
                        set(1)%text = local_tags_file_name
                        call set_tags_table_list(set)
                    end if
                end if

                ! Recompute tags-table-computed-list.
                call check_tags_table_computed_list()
                ! Set the tags table list state variables to start over
                ! from tags-table-computed-list.
                tags_table_list_started_at = 1
                tags_table_list_pointer = 1
            end if
        end if

        ! Return of t says the tags table is valid.
        visited = .true.

    contains

        logical function is_saved_set(files)
            type(string_type), intent(in) :: files(:)
            integer :: j

            is_saved_set = .false.

            do j = 1, size(tags_table_set_list)
                if (is_same_string_list(tags_table_set_list(j)%files, files)) then
                    is_saved_set = .true.
                    return
                end if
            end do
        end function is_saved_set

        subroutine save_set(files)
            type(string_type), intent(in) :: files(:)

            entry%files = files
            tags_table_set_list = [entry, tags_table_set_list]
        end subroutine save_set
    end subroutine visit_tags_table_buffer

    !> Reset tags state to cancel effect of any previous
    !> \[visit_tags_table] or \[find_tag].  Port of
    !> etags.el:tags-reset-tags-tables.
    recursive subroutine reset_tags_tables(status)
        type(status_type), intent(out) :: status
        type(status_type) :: st
        integer :: i

        status = status_type()

        ! Clear out the markers we are throwing away.
        if (allocated(tags_location_ring)) then
            do i = 1, size(tags_location_ring)
                call destroy_marker(tags_location_ring(i), st)
            end do

            deallocate (tags_location_ring)
        end if

        call clear_xref_marker_stack()
        call set_default_tags_file_name("")
        if (allocated(tags_table_list)) deallocate (tags_table_list)
        if (allocated(tags_table_computed_list)) deallocate (tags_table_computed_list)
        if (allocated(tags_table_computed_list_for)) deallocate (tags_table_computed_list_for)
        tags_table_list_pointer = 0
        tags_table_list_started_at = 0
        if (allocated(tags_table_set_list)) deallocate (tags_table_set_list)
    end subroutine reset_tags_tables

    !> The file name of the file whose tags point is within.
    !> Assumes the tags table is the current buffer.
    !> If RELATIVE is true, file name returned is relative to tags
    !> table file's directory.  If RELATIVE is false, file name returned
    !> is complete.  Port of etags.el:file-of-tag (etags-file-of-tag for an
    !> etags table).
    recursive subroutine file_of_tag(relative, file, status)
        logical, intent(in) :: relative
        character(len=:), allocatable, intent(out) :: file
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: section_file, truename, directory
        type(status_type) :: st
        integer(int64) :: opoint, found

        file = ""
        status = status_type()
        if (current_format() /= FORMAT_ETAGS) return
        opoint = point()
        call re_search_backward(FF//LF//"\([^"//LF//"]+\),[0-9]*"//LF, found, status)

        if (.not. status%ok()) then
            call goto_char(opoint)
            return
        end if

        call buffer_substring_no_properties(match_beginning(1, st), match_end(1, st), section_file, st)
        call goto_char(opoint)

        if (relative) then
            file = section_file
        else
            directory = "./"
            if (allocated(buffers(current)%directory)) directory = buffers(current)%directory
            call file_truename(directory, truename, st)
            if (.not. st%ok()) truename = directory
            call expand_file_name(section_file, file, st, file_name_as_directory(truename))
        end if
    end subroutine file_of_tag

    !> The files in the current tags table.
    !> Assumes the tags table is the current buffer.  The file names are
    !> given as they appeared in the `etags' command that created the
    !> table, usually without directory names.  Port of
    !> etags.el:tags-table-files (etags-tags-table-files for an etags
    !> table).
    recursive subroutine tags_table_files(files, status)
        type(string_type), allocatable, intent(out) :: files(:)
        type(status_type), intent(out) :: status
        integer :: k

        status = status_type()
        k = tags_buffer_index()

        if (k /= 0) then
            if (allocated(tags_buffers(k)%files)) then
                if (size(tags_buffers(k)%files) > 0) then
                    files = tags_buffers(k)%files
                    return
                end if
            end if
        end if

        call scan_table_files(.false., files)
        call make_tags_buffer_entry(k)
        tags_buffers(k)%files = files
    end subroutine tags_table_files

    !> The tags tables included by the current table.
    !> Assumes the tags table is the current buffer.  Port of
    !> etags.el:tags-included-tables (etags-tags-included-tables for an
    !> etags table).
    recursive subroutine tags_included_tables(tables, status)
        type(string_type), allocatable, intent(out) :: tables(:)
        type(status_type), intent(out) :: status
        type(status_type) :: st
        character(len=:), allocatable :: expanded
        integer :: k, i

        status = status_type()
        k = tags_buffer_index()

        if (k /= 0) then
            if (allocated(tags_buffers(k)%included)) then
                if (size(tags_buffers(k)%included) > 0) then
                    tables = tags_buffers(k)%included
                    return
                end if
            end if
        end if

        call scan_table_files(.true., tables)

        ! Expand in the default-directory of the tags table buffer.
        do i = 1, size(tables)
            call expand_file_name(tables(i)%text, expanded, st)
            tables(i)%text = expanded
        end do

        call make_tags_buffer_entry(k)
        tags_buffers(k)%included = tables
    end subroutine tags_included_tables

    ! The file names of the sections of the current etags table: the
    ! included tables when INCLUDED, else the other files.  Port of
    ! etags.el:etags-tags-table-files and etags-tags-included-tables.
    subroutine scan_table_files(included, files)
        logical, intent(in) :: included
        type(string_type), allocatable, intent(out) :: files(:)
        type(string_type) :: entry
        type(status_type) :: st
        integer(int64) :: beginning, found, opoint, moved
        logical :: is_include

        allocate (files(0))
        if (current_format() /= FORMAT_ETAGS) return
        opoint = point()
        call goto_char(point_min())

        do
            call search_forward(FF//LF, found, st, noerror=SEARCH_NOERROR)
            if (found == 0 .or. .not. st%ok()) exit
            beginning = point()
            call end_of_line()
            moved = skip_chars_backward("^,", st, beginning)
            is_include = looking_at("include$", st)

            if (is_include .eqv. included) then
                call buffer_substring_no_properties(beginning, point() - 1, entry%text, st)
                files = [files, entry]
            end if
        end do

        call goto_char(opoint)
    end subroutine scan_table_files

    !> Build the completion table on demand for a buffer's tags tables:
    !> the names of the tags in the current tags table for TARGET_BUFFER and its
    !> (recursively) included tags tables.  Port of
    !> etags.el:tags-completion-table.
    recursive subroutine tags_completion_table(target_buffer, names, status)
        type(handle_type), intent(in) :: target_buffer
        type(string_type), allocatable, intent(out) :: names(:)
        type(status_type), intent(out) :: status
        type(string_type), allocatable :: table(:), all(:)
        type(handle_type) :: old
        type(status_type) :: st
        character(len=:), allocatable :: file
        logical :: visited, found
        integer :: k, cont

        status = status_type()
        old = current_buffer()
        call set_buffer(target_buffer, st)
        k = tags_buffer_index()

        if (k /= 0) then
            if (tags_buffers(k)%has_completion_local .and. allocated(tags_buffers(k)%completion)) then
                names = tags_buffers(k)%completion
                call set_buffer(old, st)
                return
            end if
        end if

        if (k == 0 .or. .not. has_local_completion()) then
            if (allocated(tags_completion_global)) then
                names = tags_completion_global
                call set_buffer(old, st)
                return
            end if
        end if

        ! No cached value for this buffer.
        ! (A buffer with no file is named by its buffer name, not
        ! upstream's printed nil: R6-42.)
        call buffer_file_name(file, found, st)
        if (.not. found) file = buffers(current)%name
        call message("Making tags completion table for "//file//"...")
        allocate (all(0))
        cont = VISIT_FIRST

        ! Iterate over the current list of tags tables.
        do
            call visit_tags_table_buffer(visited, status, cont, cbuf=target_buffer)
            if (.not. status%ok() .or. .not. visited) exit
            ! Find possible completions in this table.
            call etags_tags_completion_table(table)
            all = [all, table]
            cont = VISIT_NEXT
        end do

        call set_buffer(target_buffer, st)

        if (.not. status%ok()) then
            call set_buffer(old, st)
            return
        end if

        call message("Making tags completion table for "//file//"...done")
        call delete_dups(all, names)
        ! Cache the result in a variable.
        k = tags_buffer_index()

        if (has_local_completion()) then
            tags_buffers(k)%completion = names
        else
            tags_completion_global = names
        end if

        call set_buffer(old, st)

    contains

        logical function has_local_completion()
            integer :: j

            has_local_completion = .false.
            j = tags_buffer_index()
            if (j /= 0) has_local_completion = tags_buffers(j)%has_completion_local
        end function has_local_completion
    end subroutine tags_completion_table

    ! NAMES without duplicates, the last occurrence of each kept, in order.
    ! Port of the (nreverse (delete-dups ...)) of
    ! etags.el:tags-completion-table.
    subroutine delete_dups(all, names)
        type(string_type), intent(in) :: all(:)
        type(string_type), allocatable, intent(out) :: names(:)
        logical, allocatable :: keep(:)
        integer :: i, j

        allocate (keep(size(all)))
        keep = .true.

        do i = 1, size(all)
            do j = i + 1, size(all)
                if (all(j)%text == all(i)%text .and. len(all(j)%text) == len(all(i)%text)) then
                    keep(i) = .false.
                    exit
                end if
            end do
        end do

        allocate (names(count(keep)))
        j = 0

        do i = 1, size(all)
            if (.not. keep(i)) cycle
            j = j + 1
            names(j) = all(i)
        end do
    end subroutine delete_dups

    ! The tag names of the current etags table.  Port of
    ! etags.el:etags-tags-completion-table (the progress reporter is not
    ! ported).
    subroutine etags_tags_completion_table(table)
        type(string_type), allocatable, intent(out) :: table(:)
        type(string_type) :: entry
        type(status_type) :: st
        integer(int64) :: opoint, found, b0, e0, moved

        allocate (table(0))
        if (current_format() /= FORMAT_ETAGS) return
        opoint = point()
        call goto_char(point_min())

        ! This regexp matches an explicit tag name or the place where it
        ! would start.
        do
            call re_search_forward("["//FF//TB//LF//CR//"()=,; ]?"//DEL//"\(?:\([^"//LF//SOH//"]+\)"//SOH//"\)?", &
                                   found, st, noerror=SEARCH_NOERROR)
            if (found == 0 .or. .not. st%ok()) exit
            b0 = match_beginning(0, st)
            e0 = match_end(0, st)

            if (match_beginning(1, st) /= 0) then
                ! There is an explicit tag name.
                call buffer_substring_no_properties(match_beginning(1, st), match_end(1, st), entry%text, st)
            else
                ! No explicit tag name.  Backtrack a little, and look for
                ! the implicit one.
                call goto_char(b0)
                moved = skip_chars_backward("^"//FF//TB//LF//CR//"()=,; ", st)
                call buffer_substring_no_properties(point(), b0, entry%text, st)
                call goto_char(e0)
            end if

            table = [table, entry]
        end do

        call goto_char(opoint)
    end subroutine etags_tags_completion_table

    ! The lazy completion table: complete over the tags completion table of
    ! `lazy_buffer'.  Port of etags.el:tags-lazy-completion-table (its
    ! function).
    recursive subroutine lazy_completion_table(string, action, result, status, test, suffix)
        character(len=*), intent(in) :: string
        integer, intent(in) :: action
        type(value_type), intent(out) :: result
        type(status_type), intent(out) :: status
        procedure(completion_test_procedure), optional :: test
        character(len=*), intent(in), optional :: suffix
        type(string_type), allocatable :: names(:)
        type(value_type), allocatable :: values(:)
        type(completion_table_type) :: table
        type(handle_type) :: old
        type(status_type) :: st
        integer(int64) :: opoint
        logical :: visited
        integer :: i

        result = value_type()
        old = current_buffer()
        call set_buffer(lazy_buffer, status)
        if (.not. status%ok()) return
        opoint = point()
        ! If we need to ask for the tag table, allow that.
        call visit_tags_table_buffer(visited, status)
        call set_buffer(lazy_buffer, st)
        call goto_char(opoint)
        if (status%ok()) call tags_completion_table(lazy_buffer, names, status)
        call set_buffer(old, st)
        if (.not. status%ok()) return
        allocate (values(size(names)))

        do i = 1, size(names)
            values(i) = value_type(text=names(i)%text)
        end do

        table = completion_list_table(make_list(values))

        select case (action)
        case (COMPLETE_TRY)
            call try_completion(string, table, result, status, test)
        case (COMPLETE_ALL)
            call all_completions(string, table, result, status, test)
        case (COMPLETE_TEST)
            if (is_exact_completion(string, table, test)) result = value_type(.true.)
        case default
        end select
    end subroutine lazy_completion_table

    !> A completion table over the tags of the current buffer's tags
    !> tables, computed when it is used.  Port of
    !> etags.el:tags-lazy-completion-table.
    function tags_lazy_completion_table() result(table)
        type(completion_table_type) :: table

        lazy_buffer = current_buffer()
        table = completion_function_table(lazy_completion_table)
    end function tags_lazy_completion_table

    !> Using tags, the completion data for the text around point.
    !> If no tags table is loaded, do nothing.  Port of
    !> etags.el:tags-completion-at-point-function.
    recursive subroutine tags_completion_at_point_function(data, status)
        type(capf_data_type), intent(out) :: data
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: pattern
        integer(int64) :: opoint, found, beginning
        logical :: has, saved_ignore

        status = status_type()
        if (.not. has_tags_tables()) return
        saved_ignore = completion_ignore_case%value
        completion_ignore_case = is_find_tag_completion_ignore_case()
        call find_default_tag(pattern, has)
        completion_ignore_case = saved_ignore
        if (.not. has) return
        opoint = point()
        ! Avoid end-of-buffer error.
        call goto_char(min(point_max(), point() + len(pattern) - 1))
        ! The find-tag function might be overly optimistic.
        call search_backward_noerror(pattern, found)

        if (found /= 0) then
            beginning = point()
            data%found = .true.
            data%start = beginning
            data%end = beginning + len(pattern)
            data%table = tags_lazy_completion_table()
            data%exclusive = .false.
        end if

        call goto_char(opoint)
    end subroutine tags_completion_at_point_function

    ! search-backward STRING with NOERROR: FOUND is the position (0 for
    ! none).  Not a port.
    subroutine search_backward_noerror(string, found)
        use search_mod, only: search_backward
        character(len=*), intent(in) :: string
        integer(int64), intent(out) :: found
        type(status_type) :: st

        call search_backward(string, found, st, noerror=SEARCH_NOERROR)
        if (.not. st%ok()) found = 0
    end subroutine search_backward_noerror

    !> Read a tag name, with defaulting and completion.  Port of
    !> etags.el:find-tag-tag.
    recursive subroutine read_tag(prompt, tag, status)
        character(len=*), intent(in) :: prompt
        character(len=:), allocatable, intent(out) :: tag
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: default
        logical :: has, saved_ignore

        saved_ignore = completion_ignore_case%value
        completion_ignore_case = is_find_tag_completion_ignore_case()
        call find_default_tag(default, has)

        if (has) then
            call completing_read(format_prompt(prompt, default), tags_lazy_completion_table(), tag, status, &
                                 default=default)
        else
            call completing_read(format_prompt(prompt, ""), tags_lazy_completion_table(), tag, status)
        end if

        completion_ignore_case = saved_ignore
        if (.not. status%ok()) return

        if (len(tag) == 0) then
            if (has) then
                tag = default
            else
                call set_error(status, FM_USER_ERROR, "There is no default tag")
            end if
        end if
    end subroutine read_tag

    ! Whether case is ignored in completing tags: `tags_case_fold_search'
    ! or else `case_fold_search'.  Port of
    ! etags.el:find-tag--completion-ignore-case.
    logical function is_find_tag_completion_ignore_case() result(ignore)
        if (tags_case_fold_search%choice == TAGS_CASE_FOLD_SEARCH_DEFAULT) then
            ignore = case_fold_search%value
        else
            ignore = tags_case_fold_search%value
        end if
    end function is_find_tag_completion_ignore_case

    ! The default tag: `find_tag_default_function' or `find_tag_default'.
    ! Port of etags.el:find-tag--default.
    subroutine find_default_tag(tag, found)
        character(len=:), allocatable, intent(out) :: tag
        logical, intent(out) :: found

        if (associated(find_tag_default_function)) then
            call find_tag_default_function(tag, found)
        else
            call find_tag_default(tag, found)
        end if
    end subroutine find_default_tag

    ! Read the arguments of the tag commands: TAGNAME read with
    ! PROMPT (with `read_string' if NO_DEFAULT) and NEXT from the prefix
    ! argument.  Port of etags.el:find-tag-interactive.
    recursive subroutine read_find_tag_arguments(prompt, no_default, tagname, next, status)
        character(len=*), intent(in) :: prompt
        logical, intent(in) :: no_default
        character(len=:), allocatable, intent(out) :: tagname
        integer, intent(out) :: next
        type(status_type), intent(out) :: status

        status = status_type()
        next = FIND_TAG_NEXT_NO
        tagname = ""

        if (current_prefix_arg%kind /= PREFIX_NONE .and. allocated(last_tag)) then
            next = FIND_TAG_NEXT_YES
            if (prefix_numeric_value(current_prefix_arg) < 0) next = FIND_TAG_NEXT_POP_BACK
        else if (no_default) then
            call read_string(prompt, tagname, status)
        else
            call read_tag(prompt, tagname, status)
        end if
    end subroutine read_find_tag_arguments

    !> Find tag (in current tags table) whose name contains TAGNAME.
    !> BUFFER is the buffer containing the tag's definition, whose point
    !> is moved there, but the buffer is not selected.
    !> The default for TAGNAME is the expression in the buffer near point.
    !>
    !> If NEXT is FIND_TAG_NEXT_YES (interactively, with prefix arg),
    !> search for another tag that matches the last tagname or regexp used.
    !> When there are multiple matches for a tag, more exact matches are
    !> found first.  If NEXT is FIND_TAG_NEXT_POP_BACK (interactively, with
    !> prefix arg that is a negative number or just \[negative_argument]),
    !> pop back to the previous tag gone to.
    !>
    !> If REGEXP is true, treat TAGNAME as a regexp.
    !>
    !> A marker representing the point when this command is invoked is
    !> pushed onto a ring and may be popped back to with \[pop_tag_mark].
    !> Contrast this with the ring of marks gone to by the command.
    !>
    !> See documentation of variable `tags_file_name'.  Port of
    !> etags.el:find-tag-noselect.
    recursive subroutine find_tag_noselect(tagname, buffer, status, next, regexp)
        character(len=*), intent(in) :: tagname
        type(handle_type), intent(out) :: buffer
        type(status_type), intent(out) :: status
        integer, intent(in), optional :: next
        logical, intent(in), optional :: regexp
        type(handle_type) :: marker, mbuf, found_buffer
        type(status_type) :: st
        character(len=:), allocatable :: pattern
        integer(int64) :: pos
        integer :: n
        logical :: is_regexp, visited, has_last, continuing

        n = FIND_TAG_NEXT_NO
        if (present(next)) n = next
        is_regexp = .false.
        if (present(regexp)) is_regexp = regexp
        buffer = null_handle()
        status = status_type()
        call find_tag_history%append(tagname, keep_all=.true.)

        if (n == FIND_TAG_NEXT_POP_BACK) then
            ! Pop back to a previous location.
            if (.not. allocated(tags_location_ring)) allocate (tags_location_ring(0))

            if (size(tags_location_ring) == 0) then
                call set_error(status, FM_USER_ERROR, "No previous tag locations")
                return
            end if

            marker = tags_location_ring(size(tags_location_ring))
            tags_location_ring = tags_location_ring(1:size(tags_location_ring) - 1)
            call marker_buffer(marker, mbuf, st)

            if (mbuf%index == 0) then
                call destroy_marker(marker, st)
                call set_error(status, FM_GENERIC_ERROR, "The marked buffer has been deleted")
                return
            end if

            ! Move to the saved location.
            call set_buffer(mbuf, st)
            call marker_position(marker, pos, st)
            call goto_char(pos)
            ! Kill that marker so it doesn't slow down editing.
            call destroy_marker(marker, st)
            ! Run the user's hook.  Do we really want to do this for pop?
            call find_tag_hook%execute(status)
            buffer = current_buffer()
            return
        end if

        ! Record whence we came.
        call push_xref_marker_stack()
        has_last = allocated(last_tag)
        continuing = n /= FIND_TAG_NEXT_NO .and. has_last

        if (continuing) then
            ! Find the same table we last used.
            call visit_tags_table_buffer(visited, status, VISIT_SAME)
            pattern = last_tag
        else
            ! Pick a table to use.
            call visit_tags_table_buffer(visited, status)
            ! Record TAGNAME for a future call with NEXT-P non-nil.
            last_tag = tagname
            pattern = tagname
        end if

        if (.not. status%ok()) return

        ! find-tag-in-order does the real work.
        if (is_regexp) then
            call find_tag_in_order(pattern, .true., find_tag_regexp_tag_order, .true., "matching",.not. continuing, &
                                   found_buffer, status)
        else
            call find_tag_in_order(pattern, .false., find_tag_tag_order, .false., "containing",.not. continuing, &
                                   found_buffer, status)
        end if

        if (.not. status%ok()) return
        ! Record the location so we can pop back to it later.
        call set_buffer(found_buffer, st)
        call point_marker(marker)
        call find_tag_hook%execute(status)
        if (.not. allocated(tags_location_ring)) allocate (tags_location_ring(0))

        if (size(tags_location_ring) >= 16) then
            call destroy_marker(tags_location_ring(1), st)
            tags_location_ring = tags_location_ring(2:)
        end if

        tags_location_ring = [tags_location_ring, marker]
        buffer = current_buffer()
    end subroutine find_tag_noselect

    ! The command of `find_tag_noselect'.  Not a port.
    recursive subroutine cmd_find_tag_noselect(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: tagname
        type(handle_type) :: buffer
        integer :: next

        call read_find_tag_arguments("Find tag", .false., tagname, next, status)
        if (status%ok()) call find_tag_noselect(tagname, buffer, status, next)
    end subroutine cmd_find_tag_noselect

    !> Find tag (in current tags table) whose name contains TAGNAME.
    !> Select the buffer containing the tag's definition, and move point
    !> there.  The default for TAGNAME is the expression in the buffer
    !> around or before point.
    !>
    !> If NEXT is FIND_TAG_NEXT_YES (interactively, with prefix arg),
    !> search for another tag that matches the last tagname or regexp used.
    !> When there are multiple matches for a tag, more exact matches are
    !> found first.  If NEXT is FIND_TAG_NEXT_POP_BACK (interactively, with
    !> prefix arg that is a negative number or just \[negative_argument]),
    !> pop back to the previous tag gone to.
    !>
    !> If REGEXP is true, treat TAGNAME as a regexp.
    !>
    !> A marker representing the point when this command is invoked is
    !> pushed onto a ring and may be popped back to with \[pop_tag_mark].
    !> Contrast this with the ring of marks gone to by the command.
    !>
    !> See documentation of variable `tags_file_name'.  Port of
    !> etags.el:find-tag.
    recursive subroutine find_tag(tagname, status, next, regexp)
        character(len=*), intent(in) :: tagname
        type(status_type), intent(out) :: status
        integer, intent(in), optional :: next
        logical, intent(in), optional :: regexp
        type(handle_type) :: target_buffer
        type(status_type) :: st
        integer(int64) :: pos

        call find_tag_noselect(tagname, target_buffer, status, next, regexp)
        if (.not. status%ok()) return
        call set_buffer(target_buffer, st)
        pos = point()
        call pop_to_buffer(target_buffer, status, actions=[DISPLAY_ACTION_SAME_WINDOW])
        if (.not. status%ok()) return
        call goto_char(pos)
    end subroutine find_tag

    ! The command of `find_tag'.  Not a port.
    recursive subroutine cmd_find_tag(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: tagname
        integer :: next

        call read_find_tag_arguments("Find tag", .false., tagname, next, status)
        if (status%ok()) call find_tag(tagname, status, next)
    end subroutine cmd_find_tag

    !> Find tag (in current tags table) whose name contains TAGNAME.
    !> Select the buffer containing the tag's definition in another window,
    !> and move point there.  The default for TAGNAME is the expression in
    !> the buffer around or before point.
    !>
    !> If NEXT is FIND_TAG_NEXT_YES (interactively, with prefix arg),
    !> search for another tag that matches the last tagname or regexp used.
    !> When there are multiple matches for a tag, more exact matches are
    !> found first.  If NEXT is FIND_TAG_NEXT_POP_BACK (interactively, with
    !> prefix arg that is a negative number or just \[negative_argument]),
    !> pop back to the previous tag gone to.
    !>
    !> If REGEXP is true, treat TAGNAME as a regexp.
    !>
    !> A marker representing the point when this command is invoked is
    !> pushed onto a ring and may be popped back to with \[pop_tag_mark].
    !> Contrast this with the ring of marks gone to by the command.
    !>
    !> See documentation of variable `tags_file_name'.  Port of
    !> etags.el:find-tag-other-window.
    recursive subroutine find_tag_other_window(tagname, status, next, regexp)
        character(len=*), intent(in) :: tagname
        type(status_type), intent(out) :: status
        integer, intent(in), optional :: next
        logical, intent(in), optional :: regexp
        type(handle_type) :: tagbuf
        type(status_type) :: st
        integer(int64) :: tagpoint

        call find_tag_noselect(tagname, tagbuf, status, next, regexp)
        if (.not. status%ok()) return
        call set_buffer(tagbuf, st)
        tagpoint = point()
        call switch_to_buffer_other_window(tagbuf, status)
        if (.not. status%ok()) return
        ! We have to set this new window's point; it might already have been
        ! displaying a different portion of tagbuf.
        call goto_char(tagpoint)
    end subroutine find_tag_other_window

    ! The command of `find_tag_other_window'.  Not a port.
    recursive subroutine cmd_find_tag_other_window(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: tagname
        integer :: next

        call read_find_tag_arguments("Find tag other window", .false., tagname, next, status)
        if (status%ok()) call find_tag_other_window(tagname, status, next)
    end subroutine cmd_find_tag_other_window

    !> Find tag (in current tags table) whose name matches REGEXP.
    !> Select the buffer containing the tag's definition and move point
    !> there.
    !>
    !> If NEXT is FIND_TAG_NEXT_YES (interactively, with prefix arg),
    !> search for another tag that matches the last tagname or regexp used.
    !> When there are multiple matches for a tag, more exact matches are
    !> found first.  If NEXT is FIND_TAG_NEXT_POP_BACK (interactively, with
    !> prefix arg that is a negative number or just \[negative_argument]),
    !> pop back to the previous tag gone to.
    !>
    !> If OTHER_WINDOW is true, select the buffer in another window.
    !>
    !> A marker representing the point when this command is invoked is
    !> pushed onto a ring and may be popped back to with \[pop_tag_mark].
    !> Contrast this with the ring of marks gone to by the command.
    !>
    !> See documentation of variable `tags_file_name'.  Port of
    !> etags.el:find-tag-regexp.
    recursive subroutine find_tag_regexp(regexp, status, next, other_window)
        character(len=*), intent(in) :: regexp
        type(status_type), intent(out) :: status
        integer, intent(in), optional :: next
        logical, intent(in), optional :: other_window
        logical :: other

        other = .false.
        if (present(other_window)) other = other_window

        ! We go through find-tag-other-window to do all the display hair
        ! there.
        if (other) then
            call find_tag_other_window(regexp, status, next, .true.)
        else
            call find_tag(regexp, status, next, .true.)
        end if
    end subroutine find_tag_regexp

    ! The command of `find_tag_regexp'.  Not a port.
    recursive subroutine cmd_find_tag_regexp(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: regexp
        integer :: next

        call read_find_tag_arguments("Find tag regexp", .true., regexp, next, status)
        if (status%ok()) call find_tag_regexp(regexp, status, next)
    end subroutine cmd_find_tag_regexp

    ! Whether TEST is one of the file name qualifiers.  Not a port.
    logical function is_file_name_test(test)
        type(tag_match_test_type), intent(in) :: test

        is_file_name_test = associated(test%test, is_tag_exact_file_name_match) .or. &
                            associated(test%test, is_tag_file_name_match) .or. &
                            associated(test%test, is_tag_partial_file_name_match)
    end function is_file_name_test

    ! Search forward from point for PATTERN, a regexp if REGEXP: FOUND is
    ! the position (0 for none).  Not a port.
    subroutine search_tag(pattern, regexp, found)
        character(len=*), intent(in) :: pattern
        logical, intent(in) :: regexp
        integer(int64), intent(out) :: found
        type(status_type) :: st

        found = 0
        ! The empty format searches nothing (upstream's `ignore').
        if (current_format() /= FORMAT_ETAGS) return

        if (regexp) then
            call re_search_forward(pattern, found, st, noerror=SEARCH_NOERROR)
        else
            call search_forward(pattern, found, st, noerror=SEARCH_NOERROR)
        end if

        if (.not. st%ok()) found = 0
    end subroutine search_tag

    ! Whether a marker in MARKERS is at POS of the current buffer.
    ! Not a port.
    logical function has_marker_at(markers, pos)
        type(handle_type), intent(in) :: markers(:)
        integer(int64), intent(in) :: pos
        type(handle_type) :: b, cb
        type(status_type) :: st
        integer(int64) :: p
        integer :: i

        has_marker_at = .false.
        cb = current_buffer()

        do i = 1, size(markers)
            call marker_buffer(markers(i), b, st)
            call marker_position(markers(i), p, st)

            if (b%index == cb%index .and. b%generation == cb%generation .and. p == pos) then
                has_marker_at = .true.
                return
            end if
        end do
    end function has_marker_at

    !> Internal tag-finding function.
    !> PATTERN is a string to search for (a regexp if REGEXP), and to give
    !> to any member of the qualifier array ORDER.  NEXT_LINE_AFTER_FAILURE
    !> is true if after a failed match, point should be moved to the next
    !> line.  MATCHING is a string, an English `-ing' word, to be used in
    !> an error message.  FIRST_SEARCH starts the search for a fresh tag;
    !> otherwise the saved state continues a previous search.  BUFFER is
    !> the buffer where the tag was found.  Port of
    !> etags.el:find-tag-in-order.
    recursive subroutine find_tag_in_order(pattern, regexp, order, next_line_after_failure, matching, first_search, &
                                           buffer, status)
        character(len=*), intent(in) :: pattern, matching
        logical, intent(in) :: regexp, next_line_after_failure, first_search
        type(tag_match_test_type), intent(in) :: order(:)
        type(handle_type), intent(out) :: buffer
        type(status_type), intent(out) :: status
        type(tag_info_type) :: tag_info
        type(handle_type) :: cbuf, tags_buffer, marker
        type(status_type) :: st
        character(len=:), allocatable :: file, relative
        integer(int64) :: found, tags_point, shortage
        integer :: k, hit
        logical :: first_table, visited, saved_fold, qualified

        buffer = null_handle()
        status = status_type()
        cbuf = current_buffer()
        saved_fold = case_fold_search%value
        case_fold_search = is_find_tag_completion_ignore_case()

        if (first_search) then
            ! This is the start of a search for a fresh tag.  Clear the list
            ! of tags matched by the previous search.  find-tag-noselect has
            ! already put us in the first tags table buffer before we got
            ! called.
            call clear_matched()
        else
            ! Continuing to search for the tag specified last time.
            call visit_tags_table_buffer(visited, status, VISIT_SAME)

            if (.not. status%ok()) then
                case_fold_search = saved_fold
                return
            end if
        end if

        tags_buffer = current_buffer()
        tags_point = point()
        first_table = .true.
        hit = 0

        ! Iterate over the list of tags tables.
        tables: do
            if (.not. first_table) then
                call visit_tags_table_buffer(visited, status, VISIT_NEXT, cbuf=cbuf)
                if (.not. status%ok() .or. .not. visited) exit tables
            end if

            ! Start at beginning of tags file.
            if (first_search .and. first_table) call goto_char(point_min())
            first_table = .false.

            ! Iterate over the list of ordering tests.
            do k = 1, size(order)
                do
                    call search_tag(pattern, regexp, found)
                    if (found == 0) exit
                    ! Naive match found.  Qualify the match.
                    qualified = order(k)%test(pattern)

                    ! Make sure it is not a previous qualified match.
                    if (qualified) then
                        if (allocated(tag_lines_already_matched)) then
                            qualified = .not. has_marker_at(tag_lines_already_matched, line_beginning_position())
                        end if
                    end if

                    if (qualified) then
                        hit = k
                        exit tables
                    end if

                    if (next_line_after_failure) shortage = forward_line(1_int64)
                end do

                ! Try the next flavor of match.
                call goto_char(point_min())
            end do
        end do tables

        if (.not. status%ok()) then
            case_fold_search = saved_fold
            return
        end if

        if (hit == 0) then
            ! We leave the loop on a match, so only get here if there were no
            ! matches.  Clear out the markers we use to avoid duplicate
            ! matches so they don't slow down editing.
            call clear_matched()
            case_fold_search = saved_fold
            call set_buffer(tags_buffer, st)
            call goto_char(tags_point)

            if (first_search) then
                call set_error(status, FM_USER_ERROR, "No tags "//matching//" "//pattern)
            else
                call set_error(status, FM_USER_ERROR, "No more tags "//matching//" "//pattern)
            end if

            return
        end if

        ! Found a tag; extract location info.
        call beginning_of_line()
        call copy_marker_at(point(), marker, .false., st)
        if (.not. allocated(tag_lines_already_matched)) allocate (tag_lines_already_matched(0))
        tag_lines_already_matched = [marker, tag_lines_already_matched]

        ! Expand the filename, using the tags table buffer's
        ! default-directory.
        if (is_file_name_test(order(hit))) then
            found = point()
            shortage = forward_line(1_int64)
            call file_of_tag(.false., relative, status)
            call goto_char(found)
        else
            call file_of_tag(.false., relative, status)
        end if

        if (status%ok()) call expand_file_name(relative, file, st)
        if (status%ok()) call snarf_etags_tag(.false., tag_info)
        case_fold_search = saved_fold
        if (.not. status%ok()) return
        ! Get the local value in the tags table buffer before switching
        ! buffers.
        call find_file_of_tag_noselect(file, status)
        if (.not. status%ok()) return
        call widen()
        call push_mark()
        call goto_etags_tag_location(tag_info, status)
        if (.not. status%ok()) return
        ! Return the buffer where the tag was found.
        buffer = current_buffer()

    contains

        subroutine clear_matched()
            integer :: j

            if (.not. allocated(tag_lines_already_matched)) return

            do j = 1, size(tag_lines_already_matched)
                call destroy_marker(tag_lines_already_matched(j), st)
            end do

            deallocate (tag_lines_already_matched)
        end subroutine clear_matched
    end subroutine find_tag_in_order

    !> Find the right line in the specified FILE: make the buffer visiting
    !> it current.  Port of etags.el:tag-find-file-of-tag-noselect
    !> (`auto_compression_mode' and `tags_compression_info_list' are not
    !> ported).
    recursive subroutine find_file_of_tag_noselect(file, status)
        character(len=*), intent(in) :: file
        type(status_type), intent(out) :: status
        type(handle_type) :: the_buffer, old
        type(status_type) :: st
        character(len=:), allocatable :: name
        logical :: found, current_modtime

        status = status_type()
        ! Search a buffer visiting the file.
        the_buffer = get_file_buffer(file)

        ! If found a buffer but file modified, ensure we re-read!
        if (the_buffer%index /= 0) then
            old = current_buffer()
            call set_buffer(the_buffer, st)
            current_modtime = is_visited_file_modtime_current()
            call set_buffer(old, st)

            if (.not. current_modtime) then
                call buffer_file_name(name, found, st, the_buffer)
                if (found) call find_file_noselect(name, the_buffer, status)
                if (.not. status%ok()) return
            end if
        else if (file_exists(file)) then
            ! If no buffer found, search for the file on disk.
            call find_file_noselect(file, the_buffer, status)
            if (.not. status%ok()) return
        end if

        if (the_buffer%index == 0) then
            call set_error(status, FM_GENERIC_ERROR, "File "//file//" not found")
            return
        end if

        call set_buffer(the_buffer, st)
    end subroutine find_file_of_tag_noselect

    ! ------------------------------------------------------------------
    ! The etags format
    ! ------------------------------------------------------------------

    !> The tag info of the tag line at point of the current etags table
    !> (an exact file tag on a file name line), with point left on the next
    !> line.  USE_EXPLICIT gives the explicit tag name as the text, when
    !> there is one.  Port of etags.el:etags-snarf-tag.
    recursive subroutine snarf_etags_tag(use_explicit, tag_info)
        logical, intent(in) :: use_explicit
        type(tag_info_type), intent(out) :: tag_info
        type(status_type) :: st
        integer(int64) :: opoint, explicit_start, found, beginning, shortage, moved
        character(len=:), allocatable :: digits
        logical :: file_line
        integer :: ios

        opoint = point()
        shortage = forward_line(-1_int64)
        file_line = looking_at(FF//LF, st)
        call goto_char(opoint)

        if (file_line) then
            ! The match was for a source file name, not any tag within a
            ! file.  Give text of t, meaning to go exactly to the location
            ! we specify, the beginning of the file.
            tag_info%exact = .true.
            tag_info%text = ""
            tag_info%position = 0
            tag_info%has_position = .true.
        else
            ! Find the end of the tag and record the whole tag text.
            call search_forward(DEL, found, st, noerror=SEARCH_NOERROR)
            call buffer_substring_no_properties(line_beginning_position(), point() - 1, tag_info%text, st)
            ! If use-explicit is true and explicit tag is present, use it
            ! as part of return value.  Else just skip it.
            explicit_start = point()
            call search_forward(SOH, found, st, bound=line_beginning_position(2_int64), noerror=SEARCH_NOERROR)

            if (found /= 0 .and. use_explicit) then
                call buffer_substring_no_properties(explicit_start, point() - 1, tag_info%text, st)
            end if

            if (looking_at("[0-9]", st)) then
                beginning = point()
                moved = skip_chars_forward("0-9", st)
                call buffer_substring_no_properties(beginning, point(), digits, st)
                read (digits, *, iostat=ios) tag_info%line
                if (ios /= 0) tag_info%line = 0
            end if

            call search_forward(",", found, st, noerror=SEARCH_NOERROR)

            if (looking_at("[0-9]", st)) then
                beginning = point()
                moved = skip_chars_forward("0-9", st)
                call buffer_substring_no_properties(beginning, point(), digits, st)
                read (digits, *, iostat=ios) tag_info%position
                tag_info%has_position = ios == 0
            end if
        end if

        ! Leave point on the next line of the tags file.
        shortage = forward_line(1_int64)
    end subroutine snarf_etags_tag

    !> Go to location of tag specified by TAG_INFO.
    !> Its TEXT is the initial part of a line containing the tag, LINE the
    !> line number, POSITION the (zero-based) char position of TEXT within
    !> the file.
    !>
    !> If EXACT, the tag refers to exactly LINE or POSITION, whichever is
    !> present, LINE having preference, no searching.
    !>
    !> If the tag isn't exactly at the given position, then look near that
    !> position using a search window that expands progressively until it
    !> hits the start of file.  Port of etags.el:etags-goto-tag-location
    !> (`selective_display' is not ported).
    recursive subroutine goto_etags_tag_location(tag_info, status)
        type(tag_info_type), intent(in) :: tag_info
        type(status_type), intent(out) :: status
        type(status_type) :: st
        character(len=:), allocatable :: pat, file
        integer(int64) :: startpos, offset, found, shortage
        logical :: is_found, has_start

        status = status_type()

        if (tag_info%exact) then
            ! Direct file tag.
            if (tag_info%line /= 0) then
                call goto_char(point_min())
                shortage = forward_line(tag_info%line - 1)
            else if (tag_info%has_position) then
                call goto_char(max(point_min(), min(tag_info%position, point_max())))
            else
                call set_error(status, FM_GENERIC_ERROR, "etags.el: BUG: bogus direct file tag")
                return
            end if
        else
            ! This constant is 1/2 the initial search window.
            ! There is no sense in making it too small,
            ! since just going around the loop once probably
            ! costs about as much as searching 2000 chars.
            offset = 1000
            pat = "^"//regexp_quote(tag_info%text)
            has_start = tag_info%has_position

            ! The character position in the tags table is 0-origin and
            ! counts CRs.  Convert it to a 1-origin Emacs character
            ! position.
            if (has_start) then
                startpos = tag_info%position + 1

                if (tag_info%line /= 0 .and. allocated(buffer_file_coding_system%value)) then
                    if (coding_system_eol_type(buffer_file_coding_system%value) == EOL_DOS) then
                        ! Act as if CRs were elided from all preceding lines.
                        ! Although this doesn't always give exactly the
                        ! correct position, it does typically improve the
                        ! guess.
                        startpos = startpos - (tag_info%line - 1)
                    end if
                end if
            else if (tag_info%line /= 0) then
                ! If no char pos was given, try the given line number.
                call goto_char(point_min())
                shortage = forward_line(tag_info%line - 1)
                startpos = point()
            else
                startpos = point_min()
            end if

            ! First see if the tag is right at the specified location.
            call goto_char(max(point_min(), min(startpos, point_max())))
            is_found = looking_at(pat, st)

            do while (.not. is_found)
                call goto_char(max(point_min(), startpos - offset))
                if (point() == point_min()) exit
                call re_search_forward(pat, found, st, bound=min(point_max(), startpos + offset), noerror=SEARCH_NOERROR)
                is_found = found /= 0
                ! expand search window
                offset = 3*offset
            end do

            if (.not. is_found) then
                call re_search_forward(pat, found, st, noerror=SEARCH_NOERROR)
                is_found = found /= 0
            end if

            if (.not. is_found) then
                file = ""
                if (allocated(buffers(current)%filename)) file = buffers(current)%filename
                call set_error(status, FM_USER_ERROR, "Rerun etags: `"//pat//"' not found in "//file)
                return
            end if
        end if

        call beginning_of_line()
    end subroutine goto_etags_tag_location

    ! List the tags of FILE in the current etags table into TEXT (and the
    ! ranges of the tag names into the face ranges): FOUND is false when
    ! the table has no such file.  Port of etags.el:etags-list-tags (the
    ! buttons, button.el, are not ported).
    recursive subroutine list_etags_tags(file, text, ranges, found)
        character(len=*), intent(in) :: file
        character(len=:), allocatable, intent(inout) :: text
        integer(int64), allocatable, intent(inout) :: ranges(:)
        logical, intent(out) :: found
        type(tag_info_type) :: tag_info
        type(status_type) :: st
        integer(int64) :: at, shortage, opoint

        found = .false.
        if (current_format() /= FORMAT_ETAGS) return
        call goto_char(point_min())
        call re_search_forward(FF//LF//"\("//file//"\),", at, st, noerror=SEARCH_NOERROR)
        if (at == 0 .or. .not. st%ok()) return
        shortage = forward_line(1_int64)
        ! Exuberant ctags add a line starting with the DEL character; skip
        ! past it.
        if (looking_at(DEL, st)) shortage = forward_line(1_int64)

        do while (.not. is_eob())
            if (looking_at(FF, st)) exit
            ! We used to use explicit tags when available, but the current
            ! goto-func can only handle implicit tags.
            opoint = point()
            call snarf_etags_tag(.false., tag_info)
            call goto_char(opoint)
            ranges = [ranges, count_characters(text) + 1]
            text = text//tag_info%text
            ranges = [ranges, count_characters(text) + 1]

            if (len(tag_info%text) > 0) then
                if (tag_info%text(1:1) == "(") text = text//" ...)"
            end if

            text = text//LF
            shortage = forward_line(1_int64)
        end do

        found = .true.
    end subroutine list_etags_tags

    ! The tags of the current etags table that STRING matches, into TEXT
    ! (with the ranges of the tag names and of the highlighted file names).
    ! Port of etags.el:etags-tags-apropos (the buttons and the progress
    ! reporter are not ported).
    recursive subroutine etags_apropos_tags(string, text, ranges, highlights)
        character(len=*), intent(in) :: string
        character(len=:), allocatable, intent(inout) :: text
        integer(int64), allocatable, intent(inout) :: ranges(:), highlights(:)
        type(tag_info_type) :: tag_info
        type(status_type) :: st
        character(len=:), allocatable :: file_label, file
        integer(int64) :: found, opoint, shortage

        if (tags_apropos_verbose) then
            text = text//substitute_command_keys("Tags in file `")
            highlights = [highlights, count_characters(text) + 1]
            file = ""
            if (allocated(buffers(current)%filename)) file = buffers(current)%filename
            text = text//file
            highlights = [highlights, count_characters(text) + 1]
            text = text//substitute_command_keys("':"//LF//LF)
        end if

        call goto_char(point_min())

        do
            call re_search_forward(string, found, st, noerror=SEARCH_NOERROR)
            if (found == 0 .or. .not. st%ok()) exit
            call beginning_of_line()
            opoint = point()
            call snarf_etags_tag(.false., tag_info)
            call goto_char(opoint)

            if (.not. tag_info%exact) then
                call file_of_tag(.true., file_label, st)
                text = text//"["//file_label//"]: "
                ranges = [ranges, count_characters(text) + 1]
                text = text//tag_info%text

                if (len(tag_info%text) > 0) then
                    if (tag_info%text(1:1) == "(") text = text//" ...)"
                end if

                ranges = [ranges, count_characters(text) + 1]
            else
                shortage = forward_line(1_int64)
                call file_of_tag(.true., file_label, st)
                call goto_char(opoint)
                ranges = [ranges, count_characters(text) + 1]
                text = text//"- "//file_label
                ranges = [ranges, count_characters(text) + 1]
            end if

            text = text//LF
            shortage = forward_line(1_int64)
        end do

        call message("")
        if (tags_apropos_verbose) text = text//LF
    end subroutine etags_apropos_tags

    ! ------------------------------------------------------------------
    ! Match qualifier functions for tagnames.
    ! These functions assume the etags file format defined in
    ! etc/ETAGS.EBNF.
    ! ------------------------------------------------------------------

    ! Whether REGEXP matches at POS (false outside the buffer), point kept.
    ! Not a port.
    logical function is_looking_at_position(pos, regexp) result(matched)
        integer(int64), intent(in) :: pos
        character(len=*), intent(in) :: regexp
        type(status_type) :: st
        integer(int64) :: opoint

        matched = .false.
        if (pos < point_min() .or. pos > point_max()) return
        opoint = point()
        call goto_char(pos)
        matched = looking_at(regexp, st)
        call goto_char(opoint)
    end function is_looking_at_position

    !> Whether TAG matches complete file name.
    !> Any directory part of the file name is also matched.  Port of
    !> etags.el:tag-exact-file-name-match-p.
    logical function is_tag_exact_file_name_match(tag) result(match)
        character(len=*), intent(in) :: tag
        type(status_type) :: st

        match = looking_at(",[0-9"//LF//"]", st)
        if (match) match = is_looking_at_position(point() - (len(tag) + 2), FF//LF)
    end function is_tag_exact_file_name_match

    !> Whether TAG matches file name, excluding directory part.  Port of
    !> etags.el:tag-file-name-match-p.
    logical function is_tag_file_name_match(tag) result(match)
        character(len=*), intent(in) :: tag
        type(status_type) :: st

        match = looking_at(",[0-9"//LF//"]", st)
        if (match) match = is_looking_at_position(point() - (len(tag) + 1), "/")
    end function is_tag_file_name_match

    !> Whether current tag line matches TAG exactly.
    !> Point should be just after a string that matches TAG.  Port of
    !> etags.el:tag-exact-match-p.
    logical function is_tag_exact_match(tag) result(match)
        character(len=*), intent(in) :: tag
        type(status_type) :: st
        integer(int32) :: c
        logical :: found

        ! The match is really exact if there is an explicit tag name.
        match = .false.
        call char_after(c, found, point())

        if (found .and. c == 1) then
            call char_after(c, found, point() - len(tag) - 1)
            match = found .and. c == 127
        end if

        if (match) return
        ! We are not on the explicit tag name, but perhaps it follows.
        ! The optional "/x" part is for Ada tags.
        match = looking_at("[^"//DEL//LF//"]*"//DEL//regexp_quote(tag)//"\(/[fpsbtk]\)?"//SOH, st)
    end function is_tag_exact_match

    !> Whether current tag line has an implicit name.
    !> Point should be just after a string that matches TAG.  Port of
    !> etags.el:tag-implicit-name-match-p.
    logical function is_tag_implicit_name_match(tag) result(match)
        character(len=*), intent(in) :: tag
        type(status_type) :: st

        ! Look at the comment of the make_tag function in lib-src/etags.c
        ! for a textual description of the four rules.
        match = string_match("^[^ "//TB//"()=,;]+$", tag, st) /= 0
        ! Rules #2 and #4, and a check that there's no explicit name.
        if (match) match = looking_at("[ "//TB//"()=,;]?"//DEL//"[0-9]*,[0-9]*$", st)
        ! rule #3
        if (match) match = is_looking_at_position(point() - (len(tag) + 1), "["//LF//" "//TB//"()=,;]")
    end function is_tag_implicit_name_match

    !> Whether current tag line matches TAG as an identifier.
    !> Point should be just after a string that matches TAG.  Port of
    !> etags.el:tag-symbol-match-p.
    logical function is_tag_identifier_match(tag) result(match)
        character(len=*), intent(in) :: tag
        type(status_type) :: st

        match = looking_at("\Sw.*"//DEL, st)
        if (match) match = looking_at("\S_.*"//DEL, st)
        if (match) match = is_looking_at_position(point() - (len(tag) + 1), "\Sw")
        if (match) match = is_looking_at_position(point() - (len(tag) + 1), "\S_")
    end function is_tag_identifier_match

    !> Whether current tag line matches TAG as a word.
    !> Point should be just after a string that matches TAG.  Port of
    !> etags.el:tag-word-match-p.
    logical function is_tag_word_match(tag) result(match)
        character(len=*), intent(in) :: tag
        type(status_type) :: st

        match = looking_at("\b.*"//DEL, st)
        if (match) match = is_looking_at_position(point() - len(tag), "\b")
    end function is_tag_word_match

    !> Whether current tag matches file name.
    !> This is a substring match, and it can include directory separators.
    !> Point should be just after a string that matches TAG.  Port of
    !> etags.el:tag-partial-file-name-match-p.
    logical function is_tag_partial_file_name_match(tag) result(match)
        character(len=*), intent(in) :: tag
        type(status_type) :: st

        match = looking_at(".*,[0-9"//LF//"]", st)
        if (match) match = is_looking_at_position(line_beginning_position() - 2, FF//LF)
    end function is_tag_partial_file_name_match

    !> Whether current tag line contains TAG as a substring.  Port of
    !> etags.el:tag-any-match-p.
    logical function is_tag_any_match(tag) result(match)
        character(len=*), intent(in) :: tag
        type(status_type) :: st

        match = looking_at(".*"//DEL, st)
    end function is_tag_any_match

    !> Whether current tag line matches REGEXP.  Port of
    !> etags.el:tag-re-match-p.
    logical function is_tag_regexp_match(regexp) result(match)
        character(len=*), intent(in) :: regexp
        type(status_type) :: st
        integer(int64) :: opoint, bol, found

        opoint = point()
        call beginning_of_line()
        bol = point()
        call search_forward(DEL, found, st, bound=line_end_position(), noerror=SEARCH_NOERROR)
        match = found /= 0
        if (match) call re_search_backward(regexp, found, st, bound=bol, noerror=SEARCH_NOERROR)
        if (match) match = found /= 0
        call goto_char(opoint)
    end function is_tag_regexp_match

    ! ------------------------------------------------------------------
    ! Listing and completing
    ! ------------------------------------------------------------------

    ! The file name of the current buffer relative to the directory of the
    ! current tags table ("" when it visits no file).  Port of
    ! etags.el:tags--get-current-buffer-name-in-tags-file.
    recursive function get_current_buffer_name_in_tags_file() result(name)
        character(len=:), allocatable :: name
        character(len=:), allocatable :: filename, tag_directory
        type(handle_type) :: old
        type(status_type) :: st
        logical :: visited

        name = ""
        if (.not. allocated(buffers(current)%filename)) return
        filename = buffers(current)%filename
        old = current_buffer()
        call visit_tags_table_buffer(visited, st)
        tag_directory = ""
        if (allocated(buffers(current)%directory)) tag_directory = buffers(current)%directory
        call set_buffer(old, st)
        if (.not. st%ok()) return
        name = file_relative_name(filename, tag_directory)
    end function get_current_buffer_name_in_tags_file

    ! Complete STRING from file names in the current tags table.  Port of
    ! etags.el:tags-complete-tags-table-file.
    recursive subroutine complete_tags_table_file(string, action, result, status, test, suffix)
        character(len=*), intent(in) :: string
        integer, intent(in) :: action
        type(value_type), intent(out) :: result
        type(status_type), intent(out) :: status
        procedure(completion_test_procedure), optional :: test
        character(len=*), intent(in), optional :: suffix
        type(string_type), allocatable :: files(:)
        type(value_type), allocatable :: values(:)
        type(completion_table_type) :: table
        type(handle_type) :: old
        type(status_type) :: st
        logical :: visited
        integer :: i

        result = value_type()
        old = current_buffer()
        ! If we need to ask for the tag table, allow that.
        call visit_tags_table_buffer(visited, status)
        if (status%ok()) call tags_table_files(files, status)
        call set_buffer(old, st)
        if (.not. status%ok()) return
        allocate (values(size(files)))

        do i = 1, size(files)
            values(i) = value_type(text=files(i)%text)
        end do

        table = completion_list_table(make_list(values))

        select case (action)
        case (COMPLETE_ALL)
            call all_completions(string, table, result, status, test)
        case (COMPLETE_TRY)
            call try_completion(string, table, result, status, test)
        case default
            ! (any other value means `try-completions' operation)
            if (action == COMPLETE_TEST) call try_completion(string, table, result, status, test)
        end select
    end subroutine complete_tags_table_file

    ! Show TEXT in the *Tags List* buffer in Apropos mode, with the face
    ! `tags_tag_face' on the RANGES and `highlight' on the HIGHLIGHTS.
    ! Not a port.
    recursive subroutine show_tags_list(text, ranges, highlights, status)
        character(len=*), intent(in) :: text
        integer(int64), intent(in) :: ranges(:), highlights(:)
        type(status_type), intent(out) :: status
        type(handle_type) :: buffer, old
        type(status_type) :: st
        integer :: i

        call with_output_to_temp_buffer("*Tags List*", text, status)
        if (.not. status%ok()) return
        old = current_buffer()
        call get_or_make_buffer("*Tags List*", buffer, st)
        call set_buffer(buffer, st)
        call apropos_mode(status)
        call set_buffer_read_only(buffer, .false., st)

        do i = 1, size(ranges) - 1, 2
            if (ranges(i + 1) > ranges(i)) then
                call put_text_property(ranges(i), ranges(i + 1), value_type(name=P_FACE), value_type(name=tags_tag_face), st)
            end if
        end do

        do i = 1, size(highlights) - 1, 2
            call put_text_property(highlights(i), highlights(i + 1), value_type(name=P_FACE), &
                                   value_type(name=FACE_HIGHLIGHT), st)
        end do

        call set_buffer_read_only(buffer, .true., st)
        call set_buffer(old, st)
    end subroutine show_tags_list

    !> Display list of tags in file FILE.
    !> Interactively, prompt for FILE, with completion, offering the
    !> current buffer's file name as the default.
    !> This command searches only the first table in the list of tags
    !> tables, and does not search included tables.
    !> FILE should be as it was submitted to the `etags' command, which
    !> usually means relative to the directory of the tags table file.
    !> Port of etags.el:list-tags (the tags are not buttons: button.el is
    !> not ported).
    recursive subroutine list_tags(file, status)
        character(len=*), intent(in) :: file
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: text
        integer(int64), allocatable :: ranges(:), highlights(:)
        type(handle_type) :: cbuf
        type(status_type) :: st
        integer(int64) :: opoint
        logical :: visited, gotany, found
        integer :: cont

        status = status_type()

        if (len(file) == 0) then
            call set_error(status, FM_USER_ERROR, "You must specify a file name")
            return
        end if

        allocate (ranges(0))
        text = substitute_command_keys("Tags in file `")
        highlights = [count_characters(text) + 1]
        text = text//file
        highlights = [highlights, count_characters(text) + 1]
        text = text//substitute_command_keys("':"//LF//LF)
        cbuf = current_buffer()
        opoint = point()
        gotany = .false.
        cont = VISIT_FIRST

        do
            call visit_tags_table_buffer(visited, status, cont, cbuf=cbuf)
            if (.not. status%ok() .or. .not. visited) exit
            cont = VISIT_NEXT
            call list_etags_tags(file, text, ranges, found)
            if (found) gotany = .true.
        end do

        call set_buffer(cbuf, st)
        call goto_char(opoint)
        if (.not. status%ok()) return

        if (.not. gotany) then
            call set_error(status, FM_USER_ERROR, "File "//file//" not in current tags tables")
            return
        end if

        call show_tags_list(text, ranges, highlights, status)
    end subroutine list_tags

    ! The command: read the file, then `list_tags'.  Not a port.
    recursive subroutine cmd_list_tags(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: file, default

        ! Default FILE to the current buffer's file.
        default = get_current_buffer_name_in_tags_file()

        if (len(default) > 0) then
            call completing_read("List tags in file: ", completion_function_table(complete_tags_table_file), file, &
                                 status, require_match=1, initial_input=default)
        else
            call completing_read("List tags in file: ", completion_function_table(complete_tags_table_file), file, &
                                 status, require_match=1)
        end if

        if (status%ok()) call list_tags(file, status)
    end subroutine cmd_list_tags

    !> Display list of all tags in tags table REGEXP matches.  Port of
    !> etags.el:tags-apropos (the tags are not buttons: button.el is not
    !> ported; `tags_apropos_additional_actions' is not ported).
    recursive subroutine apropos_tags(regexp, status)
        character(len=*), intent(in) :: regexp
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: text
        integer(int64), allocatable :: ranges(:), highlights(:)
        type(handle_type) :: cbuf
        type(status_type) :: st
        integer(int64) :: opoint
        logical :: visited
        integer :: cont

        status = status_type()
        allocate (ranges(0))
        text = substitute_command_keys("Click mouse-2 to follow tags."//LF//LF//"Tags matching regexp `")
        highlights = [count_characters(text) + 1]
        text = text//regexp
        highlights = [highlights, count_characters(text) + 1]
        text = text//substitute_command_keys("':"//LF//LF)
        cbuf = current_buffer()
        opoint = point()
        cont = VISIT_FIRST

        do
            call visit_tags_table_buffer(visited, status, cont, cbuf=cbuf)
            if (.not. status%ok() .or. .not. visited) exit
            cont = VISIT_NEXT
            call etags_apropos_tags(regexp, text, ranges, highlights)
        end do

        call set_buffer(cbuf, st)
        call goto_char(opoint)
        if (.not. status%ok()) return
        call show_tags_list(text, ranges, highlights, status)
    end subroutine apropos_tags

    ! The command: read the regexp, then `apropos_tags'.  Not a port.
    recursive subroutine cmd_apropos_tags(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: regexp

        call read_string("Tags apropos (regexp): ", regexp, status)
        if (status%ok()) call apropos_tags(regexp, status)
    end subroutine cmd_apropos_tags

    !> Perform tags completion on the text around point.
    !> Completes to the set of names listed in the current tags table.
    !> The string to complete is chosen in the same way as the default
    !> for \[find_tag] (which see).  Port of etags.el:complete-tag.
    recursive subroutine complete_tag(status)
        type(status_type), intent(out) :: status
        type(capf_data_type) :: data
        logical :: saved_ignore, done

        status = status_type()

        if (.not. has_tags_tables()) then
            call set_error(status, FM_USER_ERROR, substitute_command_keys("No tags table loaded; "// &
                                                                          "try \[visit_tags_table]"))
            return
        end if

        call tags_completion_at_point_function(data, status)
        if (.not. status%ok()) return

        if (.not. data%found) then
            call set_error(status, FM_USER_ERROR, "Nothing to complete")
            return
        end if

        saved_ignore = completion_ignore_case%value
        completion_ignore_case = is_find_tag_completion_ignore_case()
        done = completion_in_region(data%start, data%end, data%table)
        completion_ignore_case = saved_ignore
    end subroutine complete_tag

    ! ------------------------------------------------------------------
    ! The xref backend
    ! ------------------------------------------------------------------

    !> The etags backend when a tags table is in use (`tags_table_list'
    !> or `tags_file_name' is set), as a function of
    !> `xref_backend_functions'.  Port of etags.el:etags--xref-backend.
    recursive subroutine etags_xref_backend(backend)
        class(xref_backend_type), allocatable, intent(out) :: backend

        if (has_tags_tables()) allocate (etags_backend_type :: backend)
    end subroutine etags_xref_backend

    !> The identifier at point: the default tag.  Port of
    !> etags.el:xref-backend-identifier-at-point (etags).
    recursive subroutine etags_backend_identifier_at_point(this, identifier, found)
        class(etags_backend_type), intent(inout) :: this
        character(len=:), allocatable, intent(out) :: identifier
        logical, intent(out) :: found

        call find_default_tag(identifier, found)
    end subroutine etags_backend_identifier_at_point

    !> The tags completion table.  Port of
    !> etags.el:xref-backend-identifier-completion-table (etags).
    recursive subroutine etags_backend_identifier_completion_table(this, table, found)
        class(etags_backend_type), intent(inout) :: this
        type(completion_table_type), intent(out) :: table
        logical, intent(out) :: found

        table = tags_lazy_completion_table()
        found = .true.
    end subroutine etags_backend_identifier_completion_table

    !> Whether case is ignored in tags completion.  Port of
    !> etags.el:xref-backend-identifier-completion-ignore-case (etags).
    logical function is_etags_backend_identifier_completion_case_ignored(this) result(ignore)
        class(etags_backend_type), intent(inout) :: this

        ignore = is_find_tag_completion_ignore_case()
    end function is_etags_backend_identifier_completion_case_ignored

    !> The definitions of IDENTIFIER in the tags tables, those in the current
    !> file first if `etags_xref_prefer_current_file'.  Port of
    !> etags.el:xref-backend-definitions (etags).
    recursive subroutine etags_backend_definitions(this, identifier, items, status)
        class(etags_backend_type), intent(inout) :: this
        character(len=*), intent(in) :: identifier
        type(xref_item_type), allocatable, intent(out) :: items(:)
        type(status_type), intent(out) :: status
        type(xref_item_type), allocatable :: definitions(:), same_file(:), others(:)
        type(status_type) :: st
        character(len=:), allocatable :: file
        integer :: i

        call find_etags_xref_definitions(identifier, .false., definitions, status)
        if (.not. status%ok()) return

        if (etags_xref_prefer_current_file .and. allocated(buffers(current)%filename)) then
            call expand_file_name(buffers(current)%filename, file, st)
            allocate (same_file(0), others(0))

            do i = 1, size(definitions)
                if (definitions(i)%location%group() == file) then
                    call append_xref_item(same_file, definitions(i))
                else
                    call append_xref_item(others, definitions(i))
                end if
            end do

            items = same_file

            do i = 1, size(others)
                call append_xref_item(items, others(i))
            end do
        else
            call move_alloc(definitions, items)
        end if
    end subroutine etags_backend_definitions

    !> The tags that PATTERN (as in `apropos') matches.  Port of
    !> etags.el:xref-backend-apropos (etags; `tags_apropos_additional_actions'
    !> is not ported).
    recursive subroutine etags_backend_apropos(this, pattern, items, status)
        class(etags_backend_type), intent(inout) :: this
        character(len=*), intent(in) :: pattern
        type(xref_item_type), allocatable, intent(out) :: items(:)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: regexp

        call xref_apropos_regexp(pattern, regexp, status)

        if (.not. status%ok()) then
            allocate (items(0))
            return
        end if

        call find_etags_xref_definitions(regexp, .true., items, status)
    end subroutine etags_backend_apropos

    ! The existing file that FILE (an expanded file name) names: a
    ! `file_missing' error condition occurs if there is none.  Port of
    ! etags.el:etags--ensure-file (`tags_compression_info_list' is not
    ! ported).
    subroutine ensure_etags_file(file, status)
        character(len=*), intent(in) :: file
        type(status_type), intent(out) :: status

        status = status_type()
        if (.not. file_exists(file)) call set_error(status, FM_FILE_MISSING, "Cannot locate file in TAGS, "//file)
    end subroutine ensure_etags_file

    ! The definitions of PATTERN (a regexp if REGEXP) in all the tags
    ! tables: the emulation of `find_tag_in_order' that gives all matches.
    ! Port of etags.el:etags--xref-find-definitions.
    recursive subroutine find_etags_xref_definitions(pattern, regexp, items, status)
        character(len=*), intent(in) :: pattern
        logical, intent(in) :: regexp
        type(xref_item_type), allocatable, intent(out) :: items(:)
        type(status_type), intent(out) :: status
        type(tag_match_test_type), allocatable :: order(:)
        type(mark_key_type), allocatable :: marks(:)
        type(mark_key_type) :: key
        type(tag_info_type) :: tag_info
        type(xref_etags_location_type) :: location
        type(xref_item_type) :: item
        type(handle_type) :: cbuf
        type(status_type) :: st
        character(len=:), allocatable :: file, relative
        integer(int64) :: found, opoint
        logical :: visited, saved_fold, seen
        integer :: k, i, cont

        allocate (items(0), marks(0))
        status = status_type()
        cbuf = current_buffer()
        opoint = point()
        saved_fold = case_fold_search%value
        case_fold_search = is_find_tag_completion_ignore_case()
        cont = VISIT_FIRST

        if (regexp) then
            order = find_tag_regexp_tag_order
        else
            order = etags_find_xref_definitions_tag_order
        end if

        tables: do
            call visit_tags_table_buffer(visited, status, cont, cbuf=cbuf)
            if (.not. status%ok() .or. .not. visited) exit tables
            cont = VISIT_NEXT

            do k = 1, size(order)
                call goto_char(point_min())

                do
                    call search_tag(pattern, regexp, found)
                    if (found == 0 .or. size(marks) >= ETAGS_XREF_LIMIT) exit
                    if (.not. order(k)%test(pattern)) cycle
                    call beginning_of_line()
                    call snarf_etags_tag(.false., tag_info)
                    call file_of_tag(.false., relative, status)
                    if (.not. status%ok()) exit tables
                    call expand_file_name(relative, file, st)
                    call ensure_etags_file(file, status)
                    if (.not. status%ok()) exit tables
                    key%file = file
                    key%line = tag_info%line
                    seen = .false.

                    do i = 1, size(marks)
                        if (marks(i)%line == key%line .and. marks(i)%file == key%file) then
                            seen = .true.
                            exit
                        end if
                    end do

                    if (seen) cycle
                    location%tag_info = tag_info
                    location%file = file

                    if (tag_info%exact) then
                        item = make_xref("(filename match)", location)
                    else
                        item = make_xref(tag_info%text, location)
                    end if

                    call append_xref_item(items, item)
                    marks = [marks, key]
                end do
            end do
        end do tables

        case_fold_search = saved_fold
        call set_buffer(cbuf, st)
        call goto_char(opoint)
    end subroutine find_etags_xref_definitions

    !> A marker at the tag, in the buffer visiting its file.  Port of
    !> etags.el:xref-location-marker (of an xref-etags-location).
    recursive subroutine xref_etags_location_marker(this, marker, status)
        class(xref_etags_location_type), intent(in) :: this
        type(handle_type), intent(out) :: marker
        type(status_type), intent(out) :: status
        type(handle_type) :: buffer, old
        type(status_type) :: st
        integer(int64) :: opoint, begv, zv

        marker = null_handle()
        old = current_buffer()
        call find_file_noselect(this%file, buffer, status)
        if (.not. status%ok()) return
        call set_buffer(buffer, st)
        opoint = point()
        begv = point_min()
        zv = point_max()
        call widen()
        call goto_etags_tag_location(this%tag_info, status)
        if (status%ok()) call point_marker(marker)
        call narrow_to_region(begv, zv, st)
        call goto_char(opoint)
        call set_buffer(old, st)
    end subroutine xref_etags_location_marker

    !> The group of a tag location: its file.  Port of
    !> etags.el:xref-location-group (of an xref-etags-location).
    function xref_etags_location_group(this) result(group)
        class(xref_etags_location_type), intent(in) :: this
        character(len=:), allocatable :: group

        group = this%file
    end function xref_etags_location_group

    !> The line of a tag location: its tag info's line.  Port of
    !> etags.el:xref-location-line (of an xref-etags-location).
    integer(int64) function xref_etags_location_line(this) result(line)
        class(xref_etags_location_type), intent(in) :: this

        line = this%tag_info%line
    end function xref_etags_location_line

    ! ------------------------------------------------------------------
    ! Setup
    ! ------------------------------------------------------------------

    !> Define the mode, the variables and the commands of etags.el, and
    !> add the etags backend to `xref_backend_functions' and the tags
    !> completion to `completion_at_point_functions'.  Port of etags.el's
    !> top-level forms and of xref.el's (add-hook 'xref-backend-functions
    !> #'etags--xref-backend t).
    subroutine init_etags()
        if (associated(major_modes%tags_table_mode%execute)) return
        if (.not. allocated(tags_tag_face)) tags_tag_face = "default"
        allocate (find_tag_tag_order(8), find_tag_regexp_tag_order(1))
        find_tag_tag_order(1)%test => is_tag_exact_file_name_match
        find_tag_tag_order(2)%test => is_tag_file_name_match
        find_tag_tag_order(3)%test => is_tag_exact_match
        find_tag_tag_order(4)%test => is_tag_implicit_name_match
        find_tag_tag_order(5)%test => is_tag_identifier_match
        find_tag_tag_order(6)%test => is_tag_word_match
        find_tag_tag_order(7)%test => is_tag_partial_file_name_match
        find_tag_tag_order(8)%test => is_tag_any_match
        find_tag_regexp_tag_order(1)%test => is_tag_regexp_match

        if (.not. allocated(etags_find_xref_definitions_tag_order)) then
            allocate (etags_find_xref_definitions_tag_order(2))
            etags_find_xref_definitions_tag_order(1)%test => is_tag_exact_match
            etags_find_xref_definitions_tag_order(2)%test => is_tag_implicit_name_match
        end if

        call major_modes%tags_table_mode%define("tags_table_mode", "Tags Table", tags_table_mode, tags_table_mode_hook, &
                                                parent=major_modes%special_mode)
        call forward_setting(tags_file_name)
        call forward_setting(find_tag_history)
        ! We make the etags backend the default for now, until something
        ! better comes along.  Use APPEND so that any `add-hook' calls made
        ! before this package is loaded put new items before this one.
        call xref_backend_functions%append(etags_xref_backend)
        call completion_at_point_functions%append(tags_completion_at_point_function)
        commands%tags_table_mode%execute => tags_table_mode
        commands%visit_tags_table%execute => cmd_visit_tags_table
        commands%reset_tags_tables%execute => reset_tags_tables
        commands%find_tag_noselect%execute => cmd_find_tag_noselect
        commands%find_tag%execute => cmd_find_tag
        commands%find_tag_other_window%execute => cmd_find_tag_other_window
        commands%find_tag_regexp%execute => cmd_find_tag_regexp
        commands%pop_tag_mark%execute => go_back_in_xref_history
        commands%list_tags%execute => cmd_list_tags
        commands%apropos_tags%execute => cmd_apropos_tags
        commands%complete_tag%execute => complete_tag
        call append_locals_clearer(clear_tags_locals)
    end subroutine init_etags
end module etags_mod
