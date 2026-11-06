! diary_library_mod.f90 -- diary functions.
! Ported from GNU Emacs 31.1 lisp/calendar/diary-lib.el (in part).
!
! Copyright (C) 1989-2026 Free Software Foundation, Inc.
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

!> The diary: M-x diary, and the calendar's d (`view_diary_entries'), D,
!> s (`show_all_diary_entries'), m (`mark_diary_entries') and the i keys
!> that insert entries.  The diary file (`diary_file') holds dated
!> entries, whose date forms `diary_date_forms' describes, and expression
!> entries (`%%(diary-anniversary 10 31 1948) Arthur's birthday'), which
!> a fixed reader evaluates: the entry forms diary-date, diary-block,
!> diary-float, diary-anniversary, diary-cyclic, diary-offset,
!> diary-remind and diary-day-of-year, with integer arguments, t (all
!> values) and its empty opposite, quoted lists of integers, strings, quoted face names and
!> (for diary-offset and diary-remind) a quoted entry form; any other
!> form is reported as a bad entry and does not apply.  The entries for
!> a range of days are shown by `diary_display_function': the fancy
!> display in *Fancy Diary Entries* (`display_fancy_diary', the default)
!> or the simple display of the diary file's buffer with the other
!> entries hidden (`display_simple_diary').  What is not ported yet is
!> listed in docs/status/diary_library_mod.md.
module diary_library_mod
    use, intrinsic :: iso_fortran_env, only: int64
    use error_mod, only: status_type, set_error, format_error, GENERIC_ERROR_CODE, USER_ERROR_CODE, WRONG_TYPE_ARGUMENT_CODE
    use handle_mod, only: handle_type, null_handle
    use value_mod, only: value_type, command_procedure, xfixnum
    use string_type_mod, only: string_type, append_string
    use buffer_mod, only: buffers, current, current_buffer, set_buffer, get_buffer, get_or_make_buffer, is_buffer_live, &
                          close_buffer, erase_buffer, restore_buffer_modified, get_file_buffer, generate_new_buffer_name
    use editing_functions_mod, only: point, point_minimum, point_maximum, goto_character, buffer_substring, insert, widen, &
                                     is_beginning_of_line, &
                                     is_end_of_buffer, line_end_position, save_excursion_begin, save_excursion_end, &
                                     save_restriction_begin, save_restriction_end, excursion_type, restriction_type, &
                                     format_message, preceding_character
    use editing_commands_mod, only: forward_line
    use indent_mod, only: current_column
    use simple_mod, only: count_lines, special_mode, fundamental_mode
    use search_mod, only: looking_at, regular_expression_search_forward, regular_expression_search_backward, search_backward, &
                          regular_expression_quote, string_match, &
                          match_beginning, match_end, case_fold_search, SEARCH_NOERROR
    use basic_subprograms_mod, only: match_string_no_properties, remove_overlays, add_to_invisibility_specification
    use syntax_mod, only: copy_syntax_table, standard_syntax_table, syntax_table, set_syntax_table, modify_syntax_entry
    use expression_motion_mod, only: forward_expression
    use interval_tree_mod, only: make_overlay, overlay_put
    use text_properties_mod, only: put_text_property
    use marker_mod, only: make_marker, set_marker, destroy_marker, marker_buffer, marker_position
    use regular_expression_optimization_mod, only: regular_expression_optimization
    use utility_functions_mod, only: is_string_less
    use file_input_output_mod, only: expand_file_name, file_exists, is_file_readable, insert_file_contents, &
                                     is_visited_file_modtime_current
    use files_mod, only: find_file_noselect, find_file_other_window, after_save_hook, after_revert_hook, revert_buffer
    use hooks_mod, only: hook_type
    use derived_mod, only: is_derived_mode
    use modes_mod, only: major_modes, minor_modes
    use commands_mod, only: commands
    use execute_interactively_mod, only: current_prefix_argument, prefix_numeric_value, PREFIX_NONE
    use keymap_mod, only: keymap_set
    use help_mod, only: register_keymap_name
    use minibuffer_mod, only: read_file_name, REQUIRE_MATCH_YES
    use display_engine_mod, only: message, current_message
    use warnings_mod, only: display_warning, WARNING_LEVEL_ERROR
    use frame_mod, only: selected_window, selected_frame, frame_columns
    use window_mod, only: windows, display_buffer, pop_to_buffer, select_window, is_window_live, set_window_point, &
                          set_window_start, get_buffer_window, window_body_height, fit_window_to_buffer, window_buffer
    use newcomment_mod, only: comment_start, comment_end
    use button_mod, only: button_type, button_kind_type, insert_button, initialize_button
    use faces_mod, only: named_face_type, face_named, display_when, face_attributes, FACE_CLASS_COLOR
    use calendar_mod, only: calendar_date_type, operator(==), calendar_current_date, calendar_cursor_to_date, &
                            calendar_date_string, calendar_day_name, calendar_month_name, calendar_day_of_week, &
                            calendar_absolute_from_gregorian, calendar_gregorian_from_absolute, &
                            calendar_last_day_of_month, is_calendar_leap_year, calendar_nth_named_absday, &
                            calendar_nth_named_day, calendar_increment_month, calendar_get_month_range, &
                            calendar_get_date_range, calendar_mark_visible_date, calendar_get_buffer, calendar_redraw, &
                            calendar_day_of_year_string, calendar_set_mode_line, calendar_in_read_only_buffer, &
                            calendar_read_expression, calendar_marked_date, calendar_display_type, &
                            set_calendar_display, restore_calendar_display, ensure_diary_date_forms, &
                            calendar_total_months_local, calendar_mode_map, calendar_day_names, calendar_day_abbreviations, &
                            calendar_month_names, calendar_month_abbreviations, calendar_date_style, &
                            CALENDAR_DATE_STYLE_ISO, CALENDAR_DATE_STYLE_EUROPEAN, diary_file, diary_nonmarking_symbol, &
                            diary_date_forms, diary_show_holidays_flag, diary_fancy_buffer, holiday_buffer, &
                            calendar_mark_diary_entries, calendar_mark_diary_entries_hook, &
                            calendar_view_diary_entries_hook, diary_face => diary, diary_file_buffer, calendar_buffer
    use holidays_mod, only: holiday_type, calendar_holiday_list, calendar_check_holidays
    implicit none(type, external)
    private

    public :: diary_entry_type, operator(<)
    public :: diary, view_diary_entries, view_other_diary_entries, list_diary_entries, show_all_diary_entries, &
              mark_diary_entries, check_diary_file, unhide_diary_everything, include_other_diary_files, &
              mark_included_diary_files, display_simple_diary, display_fancy_diary, sort_diary_entries, &
              diary_entry_time, diary_name_pattern, mark_calendar_days_named, mark_calendar_month, &
              mark_calendar_date_pattern, list_diary_expression_entries, process_diary_expression_entry, &
              make_diary_date, diary_ordinal_suffix, insert_diary_entry_string, insert_diary_entry, &
              insert_weekly_diary_entry, insert_monthly_diary_entry, insert_yearly_diary_entry, &
              insert_anniversary_diary_entry, insert_block_diary_entry, insert_cyclic_diary_entry, diary_mode, &
              diary_fancy_display_mode, redraw_diary_calendar, goto_diary_entry, initialize_diary_library

    ! ------------------------------------------------------------------
    ! User options
    ! ------------------------------------------------------------------

    !> The string indicating inclusion of another file of diary entries.
    !> See the documentation for the function `include_other_diary_files'.
    !> Port of diary-lib.el diary-include-string.
    character(len=:), allocatable, public :: diary_include_string

    !> If .false., do not include days with no diary entry in the list of diary entries.
    !> Such days will then not be shown in the fancy diary buffer, even if they
    !> are holidays.  Port of diary-lib.el diary-list-include-blanks.
    logical, public :: diary_list_include_blanks = .false.

    !> True means prefix each diary entry with the name of the file defining it.
    !> Port of diary-lib.el diary-file-name-prefix.
    logical, public :: diary_file_name_prefix = .false.

    !> The string used to indicate an expression diary entry in `diary_file'.
    !> See the documentation for the function `list_diary_expression_entries'.
    !> Port of diary-lib.el diary-sexp-entry-symbol.
    character(len=:), allocatable, public :: diary_expression_entry_symbol

    !> String marking the start of a comment in the diary, or "".
    !> "" means there are no comments.  The diary does not display
    !> parts of entries that are inside comments.  You can use comments
    !> for whatever you like, e.g. for meta-data that packages such as
    !> appt.el can use.  Comments may not span multiple lines, and there
    !> can be only one comment on any line.
    !> See also `diary_comment_end'.  Port of diary-lib.el
    !> diary-comment-start.
    character(len=:), allocatable, public :: diary_comment_start

    !> String marking the end of a comment in the diary.
    !> The empty string means comments finish at the end of a line.
    !> See also `diary_comment_start'.  Port of diary-lib.el
    !> diary-comment-end.
    character(len=:), allocatable, public :: diary_comment_end

    !> Hook run after displaying the diary.
    !> The variables `diary_number_of_days' and `diary_original_date' hold
    !> the arguments of `list_diary_entries' during the call.  Port of
    !> diary-lib.el diary-hook.
    type(hook_type), target, public :: diary_hook = hook_type(name="diary_hook")

    !> Function used to display the diary.
    !> The two standard options are `display_fancy_diary' and
    !> `display_simple_diary'; null() displays nothing.
    !>
    !> When this function is called, the variable `diary_entries'
    !> is an array, in order by date, of all relevant diary entries
    !> (`diary_entry_type': its DATE and its STRING, the diary
    !> entry for the given date).  This can be used, for example, to
    !> produce a different buffer for display (perhaps combined with
    !> holidays), or hard copy output.  Port of diary-lib.el
    !> diary-display-function.
    procedure(command_procedure), pointer, public :: diary_display_function => display_fancy_diary

    !> Hook run after diary file is culled for relevant entries.
    !>
    !> If you add `include_other_diary_files' to this hook, you
    !> will probably also want to add `mark_included_diary_files'
    !> to `diary_mark_entries_hook'.  For example, to cause the fancy
    !> diary buffer to be displayed with diary entries from various
    !> included files, each day's entries sorted into lexicographic
    !> order, add the following to your init file:
    !>
    !>      diary_display_function => display_fancy_diary
    !>      call diary_list_entries_hook%prepend(include_other_diary_files)
    !>      call diary_list_entries_hook%append(sort_diary_entries)
    !>
    !> Note how the sort function is placed last, so that it can sort
    !> the entries included from other files.
    !>
    !> This hook runs after `diary_nongregorian_listing_hook'.  These two hooks
    !> differ only if you are using included diary files.  In that case,
    !> `diary_nongregorian_listing_hook' runs for each file, whereas
    !> `diary_list_entries_hook' only runs once, for the main diary file.
    !> So for example, to sort the complete list of diary entries you would
    !> use the list-entries hook, whereas to process e.g. Islamic entries in
    !> the main file and all included files, you would use the nongregorian hook.
    !> Port of diary-lib.el diary-list-entries-hook.
    type(hook_type), target, public :: diary_list_entries_hook = hook_type(name="diary_list_entries_hook")

    !> List of functions called after marking diary entries in the calendar.
    !> You might wish to add `mark_included_diary_files', in which case
    !> you will probably also want to add `include_other_diary_files' to
    !> `diary_list_entries_hook'.
    !>
    !> This hook runs after `diary_nongregorian_marking_hook'.  These two hooks
    !> differ only if you are using included diary files.  In that case,
    !> `diary_nongregorian_marking_hook' runs for each file, whereas
    !> `diary_mark_entries_hook' only runs once, for the main diary file.
    !> Port of diary-lib.el diary-mark-entries-hook.
    type(hook_type), target, public :: diary_mark_entries_hook = hook_type(name="diary_mark_entries_hook")

    !> List of functions called for listing diary file and included files.
    !> As the files are processed for diary entries, these functions are used
    !> to cull relevant entries.
    !>
    !> You can use this hook for other functions as well, if you want them to
    !> be run on the main diary file and any included diary files.  Otherwise,
    !> use `diary_list_entries_hook', which runs only for the main diary file.
    !> Port of diary-lib.el diary-nongregorian-listing-hook (the entries of
    !> the other calendars are Deferred).
    type(hook_type), target, public :: diary_nongregorian_listing_hook = &
                                       hook_type(name="diary_nongregorian_listing_hook")

    !> List of functions called for marking diary file and included files.
    !> As the files are processed for diary entries, these functions are used
    !> to cull relevant entries.
    !>
    !> You can use this hook for other functions as well, if you want them to
    !> be run on the main diary file and any included diary files.  Otherwise,
    !> use `diary_mark_entries_hook', which runs only for the main diary file.
    !> Port of diary-lib.el diary-nongregorian-marking-hook (the entries of
    !> the other calendars are Deferred).
    type(hook_type), target, public :: diary_nongregorian_marking_hook = &
                                       hook_type(name="diary_nongregorian_marking_hook")

    !> Value returned by `diary_entry_time' when no time is found.
    !> The default value -9999 causes entries with no recognizable time
    !> to be placed before those with times; 9999 would place entries
    !> with no recognizable time after those with times.  Port of
    !> diary-lib.el diary-unknown-time.
    integer, public :: diary_unknown_time = -9999

    !> Interpret a two-digit year DD in a diary entry as either 19DD or 20DD.
    !> When the current century is added to a two-digit year, if the result
    !> is more than 50 years in the future, the previous century is assumed.
    !> If the result is more than 50 years in the past, the next century is assumed.
    !> If this variable is .false., years must be written in full.  Port of
    !> diary-lib.el diary-abbreviated-year-flag.
    logical, public :: diary_abbreviated_year_flag = .true.

    !> Specifies how many days of diary entries are to be displayed initially.
    !> This variable affects the diary display when the command \[diary] is
    !> used, or if the value of the variable `calendar_view_diary_initially_flag'
    !> is .true.  For example, if the default value 1 is used, then only the
    !> current day's diary entries will be displayed.  If the value 2 is used,
    !> then both the current day's and the next day's entries will be displayed.
    !>
    !> This variable does not affect the diary display with the `d' command
    !> from the calendar; in that case, the prefix argument controls the number
    !> of days of diary entries displayed.  Port of diary-lib.el
    !> diary-number-of-entries (a number of days for each day of the week
    !> is Deferred).
    integer, public :: diary_number_of_entries = 1

    ! ------------------------------------------------------------------
    ! Faces
    ! ------------------------------------------------------------------

    !> Face used for anniversaries in the fancy diary display.  Port of
    !> diary-lib.el diary-anniversary.
    type(named_face_type), target, public :: diary_anniversary_face

    !> Face used for times of day in the fancy diary display.  Port of
    !> diary-lib.el diary-time.
    type(named_face_type), target, public :: diary_time

    !> Face used for buttons in the fancy diary display.  Port of
    !> diary-lib.el diary-button.
    type(named_face_type), target, public :: diary_button

    ! ------------------------------------------------------------------
    ! Variables
    ! ------------------------------------------------------------------

    !> A diary entry for a date: the DATE it applies to, its STRING as it
    !> appears in the diary display, its SPECIFIER (the date part as it
    !> appears in the diary file), and where it is: a MARKER at its text
    !> (a null handle for none), the FILE holding it and its LITERAL text
    !> in the file (before expansion; "" when it is STRING).  Upstream's
    !> list (DATE STRING SPECIFIER (MARKER FILENAME LITERAL) GLOBCOLOR).
    type :: diary_entry_type
        type(calendar_date_type) :: date
        character(len=:), allocatable :: string
        character(len=:), allocatable :: specifier
        type(handle_type) :: marker
        character(len=:), allocatable :: file
        character(len=:), allocatable :: literal
    end type diary_entry_type

    !> E1 is earlier than E2: by date, then by time of day, then by text.
    interface operator(<)
        module procedure diary_entry_compare
    end interface operator(<)

    !> The diary entries of the last `list_diary_entries', in order by
    !> date.  Port of diary-lib.el diary-entries-list.
    type(diary_entry_type), allocatable, public :: diary_entries(:)

    !> The date the last `list_diary_entries' started with, for the hooks
    !> (upstream's original-date, which diary-list-entries binds).
    type(calendar_date_type), public :: diary_original_date

    !> The number of days the last `list_diary_entries' covered, for the
    !> hooks (upstream's number, which diary-list-entries binds).
    integer, public :: diary_number_of_days = 0

    !> List of any diary files included in the last call to `list_diary_entries'.
    !> Or to `mark_diary_entries'.  Port of diary-lib.el diary-included-files.
    type(string_type), allocatable, public :: diary_included_files(:)

    !> True during the marking of diary entries, .false. otherwise.
    !> Port of diary-lib.el diary-marking-entries-flag.
    logical, public :: diary_marking_entries_flag = .false.

    !> True during the marking of diary entries, if current entry is marking.
    !> Port of diary-lib.el diary-marking-entry-flag.
    logical, public :: diary_marking_entry_flag = .false.

    !> Keymap for `diary_mode'.  Port of diary-lib.el diary-mode-map.
    type(handle_type), public :: diary_mode_map

    !> Hook run when entering `diary_mode'.
    type(hook_type), target, public :: diary_mode_hook = hook_type(name="diary_mode_hook")

    !> Hook run when entering `diary_fancy_display_mode'.
    type(hook_type), target, public :: diary_fancy_display_mode_hook = hook_type(name="diary_fancy_display_mode_hook")

    ! Whether the entries of an included file are being listed or marked
    ! (upstream's diary-including).
    logical :: including = .false.

    ! Point in the diary file's buffer when the entries were listed
    ! (upstream's diary-saved-point), for the simple display.
    integer(int64) :: saved_point = 1

    ! The string of the first date of the entries (diary--date-string).
    character(len=:), allocatable :: date_string

    ! The syntax table used when parsing dates in the diary file: the
    ! standard syntax table, with `*' and `:' word constituents (upstream's
    ! diary-syntax-table).
    type(handle_type) :: diary_syntax_table

    ! The button type of the fancy display's entries (diary-entry).
    type(button_kind_type), target :: diary_entry_button

    ! Where an entry of the fancy display is: its MARKER, FILE, SPECIFIER
    ! and LITERAL text (upstream's button property `locator').
    type :: diary_locator_type
        type(handle_type) :: marker
        character(len=:), allocatable :: file
        character(len=:), allocatable :: specifier
        character(len=:), allocatable :: literal
    end type diary_locator_type

    ! The locators of the fancy display's buttons, by their button data.
    type(diary_locator_type), allocatable :: locators(:)

    ! The name of the invisibility of hidden diary text, and the text
    ! property and overlay names used here.
    character(len=*), parameter :: DIARY_INVISIBLE = "diary", INVISIBLE_PROPERTY = "invisible", &
                                   EVAPORATE_PROPERTY = "evaporate", FONT_LOCK_FACE_PROPERTY = "font_lock_face"

    ! The kinds of a value of an expression entry: an integer, t, nil, a
    ! list of integers, a string, a quoted name, a quoted entry form.
    integer, parameter :: VALUE_INTEGER = 1, VALUE_ALL = 2, VALUE_NONE = 3, VALUE_INTEGERS = 4, VALUE_STRING = 5, &
                          VALUE_NAME = 6, VALUE_EXPRESSION = 7

    ! A value of an expression entry: its KIND, an integer VALUE, the
    ! INTEGERS of a list, or the TEXT of a string, a name or a form.
    type :: expression_value_type
        integer :: kind = VALUE_NONE
        integer :: value = 0
        integer, allocatable :: integers(:)
        character(len=:), allocatable :: text
    end type expression_value_type

    ! What an entry form gives for a date: whether it APPLIES, the TEXT
    ! of the entry (HAS_TEXT; else the entry applies with its own text),
    ! and the MARK of its date in the calendar (a value of kind
    ! VALUE_NONE for none).
    type :: expression_result_type
        logical :: applies = .false.
        logical :: has_text = .false.
        character(len=:), allocatable :: text
        type(expression_value_type) :: mark
    end type expression_result_type

    abstract interface
        ! What the entry function of an expression entry gives for DATE:
        ! its ARGUMENTS as the file writes them, the entry's text ENTRY, and
        ! RESULT.  Not a port.
        recursive subroutine entry_function_procedure(arguments, date, entry, result, status)
            import :: expression_value_type, calendar_date_type, expression_result_type, status_type
            implicit none(type, external)
            type(expression_value_type), intent(in) :: arguments(:)
            type(calendar_date_type), intent(in) :: date
            character(len=*), intent(in) :: entry
            type(expression_result_type), intent(out) :: result
            type(status_type), intent(out) :: status
        end subroutine entry_function_procedure
    end interface

    ! An entry function the diary file can name: its NAME as the file
    ! writes it (upstream's), its LEAST and MOST numbers of arguments, and
    ! the procedure that COMPUTEs it.
    type :: entry_function_type
        character(len=24) :: name = ""
        integer :: least = 0
        integer :: most = 0
        procedure(entry_function_procedure), pointer, nopass :: compute => null()
    end type entry_function_type

    ! The entry functions of expression entries (made in
    ! `initialize_diary_library').
    type(entry_function_type) :: entry_functions(8)

    ! The kinds of entries `insert_diary_entry_1' inserts.
    integer, parameter :: ENTRY_MONTHLY = 1, ENTRY_YEARLY = 2

contains

    ! ------------------------------------------------------------------
    ! The diary commands
    ! ------------------------------------------------------------------

    !> Check that the file specified by `diary_file' exists and is readable.
    !> If so, FILE is the file name, otherwise an error condition occurs.
    !> Port of diary-lib.el:diary-check-diary-file.
    recursive subroutine check_diary_file(file, status)
        character(len=:), allocatable, intent(out) :: file
        type(status_type), intent(out) :: status

        status = status_type()
        file = diary_file

        if (.not. file_exists(diary_file)) then
            call set_error(status, GENERIC_ERROR_CODE, format_message("Diary file `"//diary_file//"' does not exist"))
        else if (.not. is_file_readable(diary_file)) then
            call set_error(status, GENERIC_ERROR_CODE, format_message("Diary file `"//diary_file//"' is not readable"))
        end if
    end subroutine check_diary_file

    !> Generate the diary window for ARGUMENT days starting with the current date.
    !> If no argument is provided, the number of days of diary entries is governed
    !> by the variable `diary_number_of_entries'.  A value of ARGUMENT less than 1
    !> does nothing.  This function is suitable for execution in an init file.
    !> Port of diary-lib.el:diary.
    recursive subroutine diary(status, argument)
        type(status_type), intent(out) :: status
        integer, intent(in), optional :: argument
        character(len=:), allocatable :: file

        call check_diary_file(file, status)
        if (.not. status%ok()) return
        call list_diary_entries(calendar_current_date(), status, argument)
    end subroutine diary

    ! The command `diary': ARGUMENT is the prefix argument.  Not a port.
    recursive subroutine execute_diary(status)
        type(status_type), intent(out) :: status

        if (current_prefix_argument%kind == PREFIX_NONE) then
            call diary(status)
        else
            call diary(status, int(prefix_numeric_value(current_prefix_argument)))
        end if
    end subroutine execute_diary

    !> Prepare and display a buffer with diary entries.
    !> Searches the file named in `diary_file' for entries that match
    !> ARGUMENT days starting with the date indicated by the cursor position
    !> in the displayed three-month calendar.  Port of
    !> diary-lib.el:diary-view-entries.
    recursive subroutine view_diary_entries(argument, status)
        integer, intent(in) :: argument
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: file
        type(calendar_date_type) :: date

        call check_diary_file(file, status)
        if (.not. status%ok()) return
        call calendar_cursor_to_date(date, status, .true.)
        if (.not. status%ok()) return
        call list_diary_entries(date, status, argument)
    end subroutine view_diary_entries

    ! The command `view_diary_entries': ARGUMENT is the numeric prefix
    ! argument.  Not a port.
    recursive subroutine execute_view_diary_entries(status)
        type(status_type), intent(out) :: status

        call view_diary_entries(int(prefix_numeric_value(current_prefix_argument)), status)
    end subroutine execute_view_diary_entries

    ! The diary for the date at the cursor in the calendar, as
    ! `calendar_basic_setup' shows it (upstream's (diary-view-entries)).
    ! Not a port.
    recursive subroutine view_diary_entries_initially(status)
        type(status_type), intent(out) :: status

        call view_diary_entries(1, status)
    end subroutine view_diary_entries_initially

    !> Prepare and display buffer of diary entries from an alternative diary file.
    !> Searches for entries that match ARGUMENT days, starting with the date indicated
    !> by the cursor position in the displayed three-month calendar.
    !> DFILE specifies the file to use as the diary file.  Port of
    !> diary-lib.el:diary-view-other-diary-entries.
    recursive subroutine view_other_diary_entries(argument, dfile, status)
        integer, intent(in) :: argument
        character(len=*), intent(in) :: dfile
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: saved

        saved = diary_file
        diary_file = dfile
        call view_diary_entries(argument, status)
        diary_file = saved
    end subroutine view_other_diary_entries

    ! The command `view_other_diary_entries': reads the file.  Not a port.
    recursive subroutine execute_view_other_diary_entries(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: file
        integer :: argument

        argument = int(prefix_numeric_value(current_prefix_argument))
        call read_file_name("Enter diary file name: ", file, status, directory=buffers(current)%directory, &
                            mustmatch=REQUIRE_MATCH_YES)
        if (.not. status%ok()) return
        call view_other_diary_entries(argument, file, status)
    end subroutine execute_view_other_diary_entries

    !> Show all of the diary entries in the diary file.
    !> This function gets rid of the selective display of the diary file so that
    !> all entries, not just some, are visible.  If there is no diary buffer, one
    !> is created.  Port of diary-lib.el:diary-show-all-entries.
    recursive subroutine show_all_diary_entries(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: file
        type(handle_type) :: calendar_window, buffer, old, window
        integer :: height
        logical :: in_calendar
        type(status_type) :: st

        call check_diary_file(file, status)
        if (.not. status%ok()) return
        calendar_window = selected_window()
        height = window_body_height(calendar_window, st)
        old = current_buffer()
        buffer = diary_file_buffer()

        if (.not. is_buffer_live(buffer)) then
            call find_file_noselect(file, buffer, status)
            if (.not. status%ok()) return
        end if

        call set_buffer(buffer, status)
        if (.not. status%ok()) return
        call ensure_diary_mode(status)
        if (status%ok()) call unhide_diary_everything(status)
        if (status%ok()) call display_buffer(buffer, window, status)

        if (status%ok() .and. is_window_live(calendar_window)) then
            if (height /= window_body_height(calendar_window, st)) then
                call set_buffer(window_buffer(calendar_window, st), st)
                in_calendar = is_derived_mode(major_modes%calendar_mode)
                if (in_calendar) call fit_window_to_buffer(calendar_window)
            end if
        end if

        if (is_buffer_live(old)) call set_buffer(old, st)
    end subroutine show_all_diary_entries

    ! Put the current buffer in Diary mode if it is in the default major
    ! mode (upstream's (when (eq major-mode (default-value 'major-mode))
    ! (diary-mode))).  Not a port.
    recursive subroutine ensure_diary_mode(status)
        type(status_type), intent(out) :: status

        status = status_type()
        if (.not. allocated(buffers(current)%major_mode)) return
        if (buffers(current)%major_mode == trim(major_modes%fundamental_mode%name)) call diary_mode(status)
    end subroutine ensure_diary_mode

    ! ------------------------------------------------------------------
    ! Listing the entries
    ! ------------------------------------------------------------------

    !> Create and display a buffer containing the relevant lines in `diary_file'.
    !> Selects entries for NUMBER days starting with date DATE.  Hides any
    !> other entries using overlays.  If NUMBER is less than 1, this function
    !> does nothing.
    !>
    !> ENTRIES are all relevant diary entries found (`diary_entry_type':
    !> the date of the entry, the entry text, and the applicability).  If
    !> the variable `diary_list_include_blanks' is .true., they include a
    !> dummy diary entry consisting of the empty string for a date with
    !> no diary entries.
    !>
    !> If producing entries for multiple dates (i.e., NUMBER > 1), then
    !> this function normally returns the entries from any given diary
    !> file in date order.  The entries for any given day are in the
    !> order in which they were found in the file, not necessarily in
    !> time-of-day order.  Note that any functions present on the
    !> hooks (see below) may add entries, or change the order.  For
    !> example, `include_other_diary_files' adds entries from any
    !> include files that it finds to the end of the original list.  The
    !> entries from each file will be in date order, but the overall
    !> list will not be.  If you want the entire list to be in time
    !> order, add `sort_diary_entries' to the end of `diary_list_entries_hook'.
    !>
    !> After preparing the initial list, hooks run in this order:
    !>
    !>   `diary_nongregorian_listing_hook' runs for the main diary file,
    !>       and each included file.  For example, this is the appropriate hook
    !>       to process Islamic entries in all diary files.
    !>
    !>   `diary_list_entries_hook' runs once only, for the main diary file.
    !>       For example, this is appropriate for sorting all the entries.
    !>       If not using include files, there is no difference from the previous
    !>       hook.
    !>
    !>   `diary_hook' runs last, after the diary is displayed.
    !>       This is used e.g. by appt-check.
    !>
    !> Functions called by these hooks may use the variables `diary_original_date'
    !> and `diary_number_of_days', which are the arguments with which this
    !> function was called.
    !>
    !> This function displays the list using `diary_display_function', unless
    !> LIST_ONLY is true, in which case it just returns the list.  Port of
    !> diary-lib.el:diary-list-entries.
    recursive subroutine list_diary_entries(date, status, number, list_only, entries)
        type(calendar_date_type), intent(in) :: date
        type(status_type), intent(out) :: status
        integer, intent(in), optional :: number
        logical, intent(in), optional :: list_only
        type(diary_entry_type), allocatable, intent(out), optional :: entries(:)
        type(diary_entry_type), allocatable :: outer(:)
        type(calendar_date_type) :: day, saved_original
        type(handle_type) :: diary_buffer, temporary_buffer, old, saved_table, ov
        type(excursion_type) :: excursion
        type(restriction_type) :: restriction
        character(len=:), allocatable :: text, saved_date_string
        integer :: n, k, saved_number
        integer(int64) :: inserted
        logical :: only, incp, expression_found, entry_found, saved_fold
        type(status_type) :: st

        status = status_type()
        only = .false.
        if (present(list_only)) only = list_only

        if (present(number)) then
            n = number
        else
            n = diary_number_of_entries
        end if

        if (present(entries)) allocate (entries(0))
        if (n <= 0) return
        call ensure_diary_date_forms()
        ! The variables the hooks see (upstream binds them dynamically).
        saved_original = diary_original_date
        saved_number = diary_number_of_days
        if (allocated(date_string)) saved_date_string = date_string
        diary_original_date = date
        diary_number_of_days = n
        date_string = calendar_date_string(date)
        diary_buffer = diary_file_buffer()
        incp = including
        ! diary-entries-list is bound here: the entries of the includer wait.
        call move_alloc(diary_entries, outer)
        allocate (diary_entries(0))

        if (.not. incp) then
            if (allocated(diary_included_files)) deallocate (diary_included_files)
            allocate (diary_included_files(0))
            call destroy_entry_markers(outer)
            call message("Preparing diary...")
        end if

        old = current_buffer()
        temporary_buffer = null_handle()

        prepare: block
            if (is_buffer_live(diary_buffer)) then
                call set_buffer(diary_buffer, status)
                if (.not. status%ok()) exit prepare

                if (.not. is_visited_file_modtime_current()) then
                    call revert_buffer(status, ignore_auto=.true., noconfirm=.true.)
                    if (.not. status%ok()) exit prepare
                end if
            else if (only) then
                call get_or_make_buffer(generate_new_buffer_name(" *diary-temp*"), temporary_buffer, status)
                if (.not. status%ok()) exit prepare
                call set_buffer(temporary_buffer, status)
                if (.not. status%ok()) exit prepare
                ! If including, caller has already verified it is readable.
                call insert_file_contents(diary_file, .false., inserted, status)
                if (.not. status%ok()) exit prepare
            else
                call find_file_noselect(diary_file, diary_buffer, status)
                if (.not. status%ok()) exit prepare
                call set_buffer(diary_buffer, status)
                if (.not. status%ok()) exit prepare
            end if

            ! Setup things like the invisibility-spec (the header line is
            ! Deferred).
            if (.not. is_buffer_live(temporary_buffer)) then
                call ensure_diary_mode(status)
                if (.not. status%ok()) exit prepare
            end if

            ! d-s-p is passed to the diary display function.
            saved_point = point()
            call save_excursion_begin(excursion)
            call save_restriction_begin(restriction)
            ! bug#5093
            call widen()
            saved_table = syntax_table()
            call set_syntax_table(diary_syntax_table, st)
            call goto_character(point_minimum())

            if (.not. only) then
                call make_overlay(ov, point_minimum(), point_maximum(), status, rear_advance=.false., front_advance=.true.)
                if (status%ok()) call overlay_put(ov, INVISIBLE_PROPERTY, value_type(name=DIARY_INVISIBLE), status)
                if (status%ok()) call overlay_put(ov, EVAPORATE_PROPERTY, value_type(.true.), status)
            end if

            day = date

            do k = 1, n
                if (.not. status%ok()) exit
                call list_diary_expression_entries(day, expression_found, status)
                if (.not. status%ok()) exit
                call list_diary_entries_2(day, diary_nonmarking_symbol, only, entry_found, status)
                if (.not. status%ok()) exit

                if (diary_list_include_blanks .and. .not. (expression_found .or. entry_found)) then
                    call append_diary_entry(day, "", "", 0_int64, "")
                end if

                day = calendar_gregorian_from_absolute(1 + calendar_absolute_from_gregorian(day))
            end do

            call set_syntax_table(saved_table, st)

            if (status%ok()) then
                call goto_character(point_minimum())
                ! Although it looks like list-entries-hook runs
                ! every time, include_other_diary_files
                ! clears it when it runs in included files.
                saved_fold = case_fold_search%value
                call diary_nongregorian_listing_hook%execute(status)
                if (status%ok()) call diary_list_entries_hook%execute(status)
                case_fold_search%value = saved_fold

                if (status%ok() .and. .not. only) then
                    if (associated(diary_display_function)) call diary_display_function(status)
                end if

                if (status%ok()) call diary_hook%execute(status)
            end if

            call save_restriction_end(restriction)
            call save_excursion_end(excursion)
        end block prepare

        if (is_buffer_live(temporary_buffer)) call close_buffer(temporary_buffer, st)
        if (is_buffer_live(old)) call set_buffer(old, st)

        if (status%ok() .and. .not. incp) then
            ! Don't clobber messages displayed while preparing the diary.
            call current_message(text)

            if (allocated(text)) then
                if (text == "Preparing diary...") call message("Preparing diary...done")
            end if
        end if

        if (present(entries)) entries = diary_entries

        if (incp) then
            ! The includer's list comes back (diary-entries-list is bound
            ! here); `include_diary_files' appends these entries to it.
            call move_alloc(outer, diary_entries)
        end if

        diary_original_date = saved_original
        diary_number_of_days = saved_number
        if (allocated(saved_date_string)) date_string = saved_date_string
    end subroutine list_diary_entries

    ! Destroy the markers of ENTRIES (upstream's markers are collected
    ! as garbage).  Not a port.
    subroutine destroy_entry_markers(entries)
        type(diary_entry_type), allocatable, intent(inout) :: entries(:)
        integer :: k
        type(status_type) :: st

        if (.not. allocated(entries)) return

        do k = 1, size(entries)
            if (entries(k)%marker%index /= 0) call destroy_marker(entries(k)%marker, st)
        end do

        deallocate (entries)
    end subroutine destroy_entry_markers

    !> Add an entry to `diary_entries'.
    !> Do nothing if STRING is unallocated.  DATE is the date for which
    !> the entry applies; STRING is the text of the
    !> entry as it will appear in the diary (i.e. with any format
    !> strings such as "%d" expanded); SPECIFIER is the date part of
    !> the entry as it appears in the diary file; LITERAL is the entry
    !> as it appears in the diary file (i.e. before expansion).
    !> If LITERAL is "", it is taken to be the same as STRING.  MARKER is
    !> the position of the entry's text (0 for none).
    !>
    !> Also removes the region between `diary_comment_start' and
    !> `diary_comment_end', if the former is not "".  Port of
    !> diary-lib.el:diary-add-to-list (the face attributes of GLOBCOLOR
    !> and `diary_modify_entry_list_string_function' are Deferred).
    recursive subroutine append_diary_entry(date, string, specifier, marker, literal)
        type(calendar_date_type), intent(in) :: date
        character(len=*), intent(in) :: string, specifier
        integer(int64), intent(in) :: marker
        character(len=*), intent(in) :: literal
        type(diary_entry_type) :: entry
        character(len=:), allocatable :: text, dfile, cstart, removed
        integer(int64) :: found, last
        type(status_type) :: st

        ! b-f-n is unallocated if we are visiting an include file in a temp-buffer.
        if (allocated(buffers(current)%filename)) then
            dfile = buffers(current)%filename
        else
            dfile = diary_file
        end if

        text = string
        entry%literal = literal
        ! (funcall diary-file-name-prefix-function dfile): the file name.
        if (diary_file_name_prefix .and. len(dfile) > 0) text = "["//dfile//"] "//text

        if (len(diary_comment_start) > 0) then
            cstart = regular_expression_quote(diary_comment_start)

            if (string_match(cstart, text, st) > 0) then
                ! Preserve the value with the comments.
                if (len(entry%literal) == 0) entry%literal = text
                ! Handles multiple comments per entry, so long as each is on
                ! a single line, and each line has no more than one comment.
                removed = ""
                last = 1
                found = string_match(cstart//".*"//regular_expression_quote(diary_comment_end), text, st, start=last)

                do while (found > 0 .and. st%ok())
                    removed = removed//substring(text, last, found)
                    last = match_end(0, st)
                    if (last <= found) last = found + 1
                    found = string_match(cstart//".*"//regular_expression_quote(diary_comment_end), text, st, start=last)
                end do

                text = removed//substring(text, last, huge(1_int64))
            end if
        end if

        entry%date = date
        entry%string = text
        entry%specifier = specifier
        entry%file = dfile
        entry%marker = null_handle()

        if (marker > 0) then
            call make_marker(entry%marker)
            call set_marker(entry%marker, marker, st)
        end if

        if (.not. allocated(diary_entries)) allocate (diary_entries(0))
        diary_entries = [diary_entries, entry]
    end subroutine append_diary_entry

    ! The characters of TEXT from the character index FIRST up to (not
    ! including) LAST, as a string's match data gives them.  Not a port.
    function substring(text, first, last) result(part)
        character(len=*), intent(in) :: text
        integer(int64), intent(in) :: first, last
        character(len=:), allocatable :: part
        integer(int64) :: n, b, e, k, chars

        ! Character indices to byte offsets (UTF-8).
        n = len(text, kind=int64)
        b = n + 1
        e = n + 1
        chars = 0

        do k = 1, n
            if (iand(iachar(text(k:k)), 192) /= 128) then
                chars = chars + 1
                if (chars == first) b = k
                if (chars == last) e = k
            end if
        end do

        if (b > n) then
            part = ""
        else
            part = text(b:e - 1)
        end if
    end function substring

    !> Find diary entries applying to DATE, by searching from point_minimum for
    !> each element of `diary_date_forms'.  MARK indicates an entry is non-marking.
    !> If LIST_ONLY is
    !> true, don't change the buffer, only return a list of entries.
    !> FOUND is true if any entries were found.  Port of
    !> diary-lib.el:diary-list-entries-2 (the other calendars' MONTHS,
    !> SYMBOL and GDATE are Deferred, and so is the face markup of the
    !> entries, GLOBATTR).
    recursive subroutine list_diary_entries_2(date, mark, list_only, found, status)
        type(calendar_date_type), intent(in) :: date
        character(len=*), intent(in) :: mark
        logical, intent(in) :: list_only
        logical, intent(out) :: found
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: dayname, monthname, month, day, year, regular_expression, text, specifier
        character(len=32) :: digits
        integer(int64) :: hit, back, date_start, entry_start, shortage
        integer :: k, j
        logical :: saved_fold
        type(status_type) :: st

        status = status_type()
        found = .false.
        saved_fold = case_fold_search%value
        case_fold_search%value = .true.
        dayname = calendar_day_name(date)//"\|"//calendar_day_name(date, .true.)//"\.?"
        monthname = "\*\|"//calendar_month_name(date%month)//"\|"//calendar_month_name(date%month, .true.)//"\.?"
        write (digits, "(i0)") date%month
        month = "\*\|0*"//trim(digits)
        write (digits, "(i0)") date%day
        day = "\*\|0*"//trim(digits)
        write (digits, "(i0)") date%year
        year = "\*\|0*"//trim(digits)

        if (diary_abbreviated_year_flag) then
            ! (format "\\|%02d" (% year 100))
            if (mod(date%year, 100) >= 0) then
                write (digits, "(i2.2)") mod(date%year, 100)
            else
                write (digits, "(i0)") mod(date%year, 100)
            end if

            year = year//"\|"//trim(digits)
        end if

        do k = 1, size(diary_date_forms%entries)
            associate (form => diary_date_forms%entries(k))
                ! date-form uses day etc as set above.
                regular_expression = "^"//regular_expression_quote(mark)//"?\("

                do j = 1, form%count
                    if (j > 1) regular_expression = regular_expression//"\)\(?:"
                    regular_expression = regular_expression//part_pattern(form%parts(j))
                end do

                regular_expression = regular_expression//"\)"
                call goto_character(point_minimum())

                do
                    call regular_expression_search_forward(regular_expression, hit, st, noerror=SEARCH_NOERROR)

                    if (.not. st%ok()) then
                        status = st
                        exit
                    end if

                    if (hit == 0) exit
                    if (form%backup) call regular_expression_search_backward("\<", back, st, noerror=SEARCH_NOERROR)

                    ! regexp moves us past the end of date, onto the next line.
                    ! Trailing whitespace after date not allowed (see diary-file).
                    if (is_beginning_of_line() .and. .not. looking_at("[ \t]", st)) then
                        ! Diary entry that consists only of date.
                        call goto_character(point() - 1)
                    else
                        ! Found a nonempty diary entry--make it
                        ! visible and add it to the list.
                        date_start = line_end_position(0_int64)
                        ! Actual entry starts on the next-line?
                        if (looking_at("[ \t]*\n[ \t]", st)) shortage = forward_line(1_int64)
                        found = .true.
                        entry_start = point()
                        shortage = forward_line(1_int64)

                        ! continued entry
                        do while (looking_at("[ \t]", st))
                            shortage = forward_line(1_int64)
                        end do

                        if (.not. (is_end_of_buffer() .and. .not. is_beginning_of_line())) call goto_character(point() - 1)

                        if (.not. list_only) then
                            call remove_overlays(date_start, point(), INVISIBLE_PROPERTY, value_type(name=DIARY_INVISIBLE), &
                                                 st)
                        end if

                        call buffer_substring(entry_start, point(), text, st)
                        call buffer_substring(date_start + 1, entry_start - 1, specifier, st)
                        call append_diary_entry(date, text, specifier, entry_start, "")
                    end if
                end do
            end associate

            if (.not. status%ok()) exit
        end do

        case_fold_search%value = saved_fold

    contains

        ! The regular expression of PART: a keyword's pattern for DATE,
        ! else PART itself.
        function part_pattern(part) result(pattern)
            character(len=*), intent(in) :: part
            character(len=:), allocatable :: pattern

            select case (trim(part))
            case ("dayname")
                pattern = dayname
            case ("monthname")
                pattern = monthname
            case ("month")
                pattern = month
            case ("day")
                pattern = day
            case ("year")
                pattern = year
            case default
                pattern = trim(part)
            end select
        end function part_pattern
    end subroutine list_diary_entries_2

    !> Show all invisible text in the diary.  Port of
    !> diary-lib.el:diary-unhide-everything.
    recursive subroutine unhide_diary_everything(status)
        type(status_type), intent(out) :: status
        type(restriction_type) :: restriction

        ! bug#5477
        call save_restriction_begin(restriction)
        call widen()
        call remove_overlays(point_minimum(), point_maximum(), INVISIBLE_PROPERTY, value_type(name=DIARY_INVISIBLE), status)
        call save_restriction_end(restriction)
        ! (kill-local-variable 'mode-line-format)
        call buffers(current)%mode_line_format%clear()
    end subroutine unhide_diary_everything

    !> Process diary entries from included diary files.
    !> By default, lists included entries, but if optional argument MARK is true
    !> marks entries instead.
    !> For example, this enables you to share common diary files.
    !> Specify include files using lines matching `diary_include_string', e.g.
    !>     #include "filename"
    !> This is recursive; that is, included files may include other files.
    !> Port of diary-lib.el:diary-include-files.
    recursive subroutine include_diary_files(mark, status)
        logical, intent(in) :: mark
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: file, efile, saved_file
        type(diary_entry_type), allocatable :: included(:)
        type(hook_type) :: saved_mark_hook, saved_list_hook, saved_hook
        integer(int64) :: hit, after
        logical :: saved_including, saved_blanks
        integer :: k
        type(status_type) :: st

        status = status_type()
        call goto_character(point_minimum())

        do
            call regular_expression_search_forward("^"//regular_expression_quote(diary_include_string)//" ""\([^""]*\)""", &
                                                   hit, st, &
                                                   noerror=SEARCH_NOERROR)
            if (hit == 0) exit
            after = point()
            file = match_string_no_properties(1)

            if (.not. file_exists(file)) then
                call display_warning("diary", format_message("Can't find included diary file ")//file//achar(10), &
                                     WARNING_LEVEL_ERROR, status=status)
                if (.not. status%ok()) return
                call goto_character(after)
                cycle
            end if

            if (.not. is_file_readable(file)) then
                call display_warning("diary", format_message("Can't read included diary file ")//file//achar(10), &
                                     WARNING_LEVEL_ERROR, status=status)
                if (.not. status%ok()) return
                call goto_character(after)
                cycle
            end if

            call expand_file_name(file, efile, st)

            do k = 1, size(diary_included_files)
                if (diary_included_files(k)%text == efile) then
                    call set_error(status, GENERIC_ERROR_CODE, "Recursive diary include for "//file)
                    return
                end if
            end do

            call append_string(diary_included_files, efile)
            ! (let ((diary-file ...) (diary-mark-entries-hook ...)
            ! (diary-list-entries-hook ...) (diary-including t) diary-hook
            ! diary-list-include-blanks) ...)
            saved_file = diary_file
            saved_mark_hook = diary_mark_entries_hook
            saved_list_hook = diary_list_entries_hook
            saved_hook = diary_hook
            saved_including = including
            saved_blanks = diary_list_include_blanks
            diary_file = file
            call diary_mark_entries_hook%clear()
            call diary_mark_entries_hook%prepend(mark_included_diary_files)
            call diary_list_entries_hook%clear()
            call diary_list_entries_hook%prepend(include_other_diary_files)
            call diary_hook%clear()
            including = .true.
            diary_list_include_blanks = .false.

            if (mark) then
                call mark_diary_entries(.false., status)
            else
                call list_diary_entries(diary_original_date, status, diary_number_of_days, .true., included)
                if (status%ok()) diary_entries = [diary_entries, included]
            end if

            diary_file = saved_file
            diary_mark_entries_hook = saved_mark_hook
            diary_list_entries_hook = saved_list_hook
            diary_hook = saved_hook
            including = saved_including
            diary_list_include_blanks = saved_blanks
            if (.not. status%ok()) return
            call goto_character(after)
        end do

        call goto_character(point_minimum())
    end subroutine include_diary_files

    !> Add diary entries from included diary files to `diary_entries'.
    !> To use, add this function to `diary_list_entries_hook'.
    !> For details, see `include_diary_files'.
    !> See also `mark_included_diary_files'.  Port of
    !> diary-lib.el:diary-include-other-diary-files.
    recursive subroutine include_other_diary_files(status)
        type(status_type), intent(out) :: status

        call include_diary_files(.false., status)
    end subroutine include_other_diary_files

    !> Mark diary entries from included diary files.
    !> To use, add this function to `diary_mark_entries_hook'.
    !> For details, see `include_diary_files'.
    !> See also `include_other_diary_files'.  Port of
    !> diary-lib.el:diary-mark-included-diary-files.
    recursive subroutine mark_included_diary_files(status)
        type(status_type), intent(out) :: status

        call include_diary_files(.true., status)
    end subroutine mark_included_diary_files

    ! ------------------------------------------------------------------
    ! Displaying the entries
    ! ------------------------------------------------------------------

    !> Common subroutine of `display_simple_diary' and `display_fancy_diary'.
    !> Handles the case where there are no diary entries.
    !> NO_ENTRIES is true if there are none, HOLIDAY_STRING the date
    !> with its holidays.  Port of diary-lib.el:diary-display-no-entries.
    recursive subroutine display_no_diary_entries(no_entries, holiday_string, status)
        logical, intent(out) :: no_entries
        character(len=:), allocatable, intent(out) :: holiday_string
        type(status_type), intent(out) :: status
        type(holiday_type), allocatable :: holiday_list(:)
        character(len=:), allocatable :: no_entries_message, lines
        type(handle_type) :: old
        integer :: k
        type(status_type) :: st

        status = status_type()

        if (diary_show_holidays_flag) then
            call calendar_check_holidays(diary_original_date, holiday_list)
        else
            allocate (holiday_list(0))
        end if

        holiday_string = date_string
        if (size(holiday_list) > 0) holiday_string = holiday_string//": "
        lines = ""

        do k = 1, size(holiday_list)
            if (k > 1) then
                holiday_string = holiday_string//"; "
                lines = lines//achar(10)
            end if

            holiday_string = holiday_string//holiday_list(k)%string
            lines = lines//holiday_list(k)%string
        end do

        no_entries_message = "No diary entries for "//holiday_string
        ! Empty list, or single item with no text.
        ! FIXME multiple items with no text?
        no_entries = size(diary_entries) == 0

        if (size(diary_entries) == 1) then
            if (len(diary_entries(1)%string) == 0) no_entries = .true.
        end if

        ! Inconsistency: whether or not the holidays are displayed in a
        ! separate buffer depends on if there are diary entries.
        if (no_entries) then
            if (len(no_entries_message) < frame_columns(selected_frame) .or. size(holiday_list) == 0) then
                call message(no_entries_message)
            else
                ! holiday-list which is too wide for a message gets a buffer.
                old = current_buffer()
                call calendar_in_read_only_buffer(holiday_buffer, "Holidays for "//date_string, lines, status)
                if (is_buffer_live(old)) call set_buffer(old, st)
                if (.not. status%ok()) return
                call message("No diary entries for "//date_string)
            end if
        end if
    end subroutine display_no_diary_entries

    !> Display the diary buffer if there are any relevant entries or holidays.
    !> Entries that do not apply are made invisible.  Holidays are shown
    !> in the mode line.  This is an option for `diary_display_function'.
    !> Port of diary-lib.el:diary-simple-display.
    recursive subroutine display_simple_diary(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: dbuff, old, window
        character(len=:), allocatable :: holiday_string
        logical :: no_entries
        type(status_type) :: st

        dbuff = diary_file_buffer()
        call display_no_diary_entries(no_entries, holiday_string, status)
        if (.not. status%ok()) return
        old = current_buffer()
        call set_buffer(dbuff, status)
        if (.not. status%ok()) return
        ! This may be too wide, but when simple diary is used there is
        ! nowhere else for the holidays to go.
        call calendar_set_mode_line("Diary for "//holiday_string)

        if (.not. no_entries) then
            call display_buffer(dbuff, window, status)

            if (status%ok() .and. is_window_live(window)) then
                ! d-s-p is passed from list_diary_entries.
                call set_window_point(windows(window%index), saved_point, st)
                call set_window_start(windows(window%index), point_minimum(), st)
            end if
        end if

        if (is_buffer_live(old)) call set_buffer(old, st)
    end subroutine display_simple_diary

    !> Prepare a diary buffer with relevant entries in a fancy, noneditable form.
    !> Holidays are shown unless `diary_show_holidays_flag' is .false.
    !> Days with no diary entries are not shown (even if that day is a
    !> holiday), unless `diary_list_include_blanks' is .true.
    !>
    !> This is an option for `diary_display_function'.  Port of
    !> diary-lib.el:diary-fancy-display (the face markup of the entries
    !> is Deferred).
    recursive subroutine display_fancy_diary(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: dbuff, old, fancy, window, old_window
        character(len=:), allocatable :: holiday_string
        logical :: no_entries
        type(status_type) :: st

        status = status_type()
        old = current_buffer()
        ! Turn off selective-display in the diary file's buffer.
        dbuff = diary_file_buffer()

        if (is_buffer_live(dbuff)) then
            call set_buffer(dbuff, status)
            if (status%ok()) call unhide_diary_everything(status)
            call set_buffer(old, st)
            if (.not. status%ok()) return
        end if

        call display_no_diary_entries(no_entries, holiday_string, status)
        if (.not. status%ok() .or. no_entries) return
        ! Prepare the fancy diary buffer (calendar-in-read-only-buffer).
        call get_or_make_buffer(diary_fancy_buffer, fancy, status)
        if (.not. status%ok()) return
        call set_buffer(fancy, status)
        if (.not. status%ok()) return

        fill: block
            if (.not. is_derived_mode(major_modes%special_mode)) then
                call special_mode(status)
                if (.not. status%ok()) exit fill
            end if

            buffers(current)%read_only = .false.
            buffers(current)%undo_enabled = .false.
            call erase_buffer(status)
            if (.not. status%ok()) exit fill
            call display_buffer(fancy, window, status)
            if (.not. status%ok()) exit fill

            if (is_window_live(window)) then
                old_window = selected_window()
                call select_window(window, status, norecord=.true.)

                if (status%ok()) then
                    call calendar_set_mode_line("Diary Entries")
                    call insert_fancy_entries(status)
                    call goto_character(point_minimum())
                end if

                if (is_window_live(old_window)) call select_window(old_window, st, norecord=.true.)
                call set_buffer(fancy, st)
                if (.not. status%ok()) exit fill
            end if

            call restore_buffer_modified(.false.)
            buffers(current)%read_only = .true.

            ! FIXME can't remember what this check was for.
            ! To prevent something looping, or a minor optimization?
            if (buffers(current)%major_mode == trim(major_modes%diary_fancy_display_mode%name)) then
                call diary_fancy_display_mode_hook%execute(status)
            else
                call diary_fancy_display_mode(status)
            end if

            if (.not. status%ok()) exit fill
            call calendar_set_mode_line(date_string)
        end block fill

        if (is_buffer_live(old)) call set_buffer(old, st)
    end subroutine display_fancy_diary

    ! The body of `display_fancy_diary': each date with its holidays,
    ! then its entries, as buttons.  Not a port (part of
    ! diary-lib.el:diary-fancy-display).
    recursive subroutine insert_fancy_entries(status)
        type(status_type), intent(out) :: status
        type(holiday_type), allocatable :: holiday_list(:)
        type(calendar_date_type) :: date
        type(calendar_display_type) :: saved
        type(button_type) :: button
        type(diary_locator_type) :: locator
        integer :: last_month, last_year, k, j, longest, cc
        integer(int64) :: start
        logical :: first, newer
        type(status_type) :: st

        status = status_type()
        last_month = 1
        last_year = 1
        date = calendar_date_type()
        allocate (holiday_list(0))
        if (allocated(locators)) call destroy_locators()
        allocate (locators(0))

        do k = 1, size(diary_entries)
            if (.not. (date == diary_entries(k)%date)) then
                date = diary_entries(k)%date

                if (diary_show_holidays_flag) then
                    newer = calendar_absolute_from_gregorian(calendar_date_type(last_month, &
                                                                    calendar_last_day_of_month(last_month, last_year), last_year)) &
                            < calendar_absolute_from_gregorian(date)

                    if (newer) then
                        ! We need to get the holidays for the next 3 months.
                        last_month = date%month
                        last_year = date%year
                        call calendar_increment_month(last_month, last_year, 1)
                        call set_calendar_display(last_month, last_year, 3, saved)
                        call calendar_holiday_list(holiday_list)
                        call restore_calendar_display(saved)
                        call calendar_increment_month(last_month, last_year, 1)
                    end if
                end if

                longest = 0
                first = point() == point_minimum()
                if (.not. first) call insert(achar(10), status)
                if (.not. status%ok()) return
                start = point()
                call insert(calendar_date_string(date), status)
                if (.not. status%ok()) return
                call put_text_property(start, point(), value_type(name=FONT_LOCK_FACE_PROPERTY), &
                                       value_type(name=trim(diary_face%name)), st)
                newer = .false.

                ! Make a list of all holidays for date.
                do j = 1, size(holiday_list)
                    if (holiday_list(j)%date == date) then
                        newer = .true.
                        exit
                    end if
                end do

                if (newer) call insert(":  ", status)
                cc = int(current_column())
                newer = .false.

                do j = 1, size(holiday_list)
                    if (.not. (holiday_list(j)%date == date)) cycle
                    if (newer) call insert(achar(10)//repeat(" ", cc), status)
                    call insert(holiday_list(j)%string, status)
                    longest = max(longest, len(holiday_list(j)%string))
                    newer = .true.
                end do

                call insert(achar(10), status)
                start = point()
                call insert(repeat("=", cc + longest), status)
                call put_text_property(start, point(), value_type(name=FONT_LOCK_FACE_PROPERTY), &
                                       value_type(name=trim(diary_face%name)), st)
                call insert(achar(10), status)
                if (.not. status%ok()) return
            end if

            associate (entry => diary_entries(k))
                if (len(entry%string) > 0) then
                    ! (MARKER FILENAME SPECIFIER LITERAL)
                    locator%marker = entry%marker
                    locator%file = entry%file
                    locator%specifier = entry%specifier
                    locator%literal = entry%literal
                    if (len(locator%literal) == 0) locator%literal = entry%string
                    locators = [locators, locator]
                    call insert_button(entry%string, button, status, kind=diary_entry_button, &
                                       button_data=value_type(int(size(locators), int64)))
                    if (.not. status%ok()) return
                    call insert(achar(10), status)
                    if (.not. status%ok()) return
                end if
            end associate
        end do
    end subroutine insert_fancy_entries

    ! Forget the locators of the fancy display's buttons (the markers
    ! belong to `diary_entries').  Not a port.
    subroutine destroy_locators()
        deallocate (locators)
    end subroutine destroy_locators

    !> Jump to the diary entry for the BUTTON at point (DATA: its
    !> locator).  Port of diary-lib.el:diary-goto-entry.
    recursive subroutine goto_diary_entry(button, data, status)
        type(button_type), intent(in) :: button
        type(value_type), intent(in) :: data
        type(status_type), intent(out) :: status
        type(diary_locator_type) :: locator
        type(handle_type) :: markbuf
        integer(int64) :: k, position, found
        logical :: located
        type(status_type) :: st

        status = status_type()
        if (button%position < 0) return
        k = xfixnum(data)
        if (.not. allocated(locators)) return
        if (k < 1 .or. k > size(locators)) return
        locator = locators(k)
        ! If marker pointing to diary location is valid, use that.
        markbuf = null_handle()
        if (locator%marker%index /= 0) call marker_buffer(locator%marker, markbuf, st)

        if (is_buffer_live(markbuf) .and. st%ok()) then
            call marker_position(locator%marker, position, st)
            call pop_to_buffer(markbuf, status)
            if (.not. status%ok()) return
            call goto_character(position)
            return
        end if

        ! Marker is invalid (eg buffer has been killed).
        located = .false.

        if (len(locator%file) > 0) then
            if (file_exists(locator%file)) then
                call find_file_other_window(locator%file, status)
                if (.not. status%ok()) return
                call ensure_diary_mode(status)
                if (.not. status%ok()) return
                call goto_character(point_minimum())
                located = .true.
                call regular_expression_search_forward(regular_expression_quote(locator%specifier)//".*\(" &
                                                       //regular_expression_quote(locator%literal)//"\)", &
                                                       found, st, noerror=SEARCH_NOERROR)
                if (found > 0) call goto_character(match_beginning(1, st))
            end if
        end if

        if (.not. located) call message("Unable to locate this diary entry")
    end subroutine goto_diary_entry

    ! ------------------------------------------------------------------
    ! Marking the entries
    ! ------------------------------------------------------------------

    !> Return a regular expression matching the strings in the array NAMES.
    !> If the optional argument ABBREVIATIONS is present, the regular
    !> expression also matches the supplied abbreviations, with or without
    !> final `.' characters.  Port of diary-lib.el:diary-name-pattern (no
    !> PARENTHESIS).
    function diary_name_pattern(names, abbreviations) result(pattern)
        character(len=*), intent(in) :: names(:)
        character(len=*), intent(in), optional :: abbreviations(:)
        character(len=:), allocatable :: pattern
        character(len=len(names) + 1), allocatable :: strings(:)
        integer :: k, n

        n = size(names)
        if (present(abbreviations)) n = n + 2*size(abbreviations)
        allocate (strings(n))
        strings(1:size(names)) = names

        if (present(abbreviations)) then
            do k = 1, size(abbreviations)
                strings(size(names) + k) = abbreviations(k)
                strings(size(names) + size(abbreviations) + k) = trim(abbreviations(k))//"."
            end do
        end if

        pattern = regular_expression_optimization(strings)
    end function diary_name_pattern

    !> Mark days in the calendar window that have diary entries.
    !> Marks each entry in the diary that is visible in the calendar window.
    !>
    !> After marking the entries, runs `diary_nongregorian_marking_hook'
    !> for the main diary file, and each included file.  For example,
    !> this is the appropriate hook to process Islamic entries in all
    !> diary files.  Next `diary_mark_entries_hook' runs, for the main diary
    !> file only.  If not using include files, there is no difference between
    !> these two hooks.
    !>
    !> If the optional argument REDRAW is true (which is the case
    !> interactively, for example) then this first removes any existing diary
    !> marks.  This is intended to deal with deleted diary entries.
    !> Port of diary-lib.el:diary-mark-entries (the face markup of the
    !> entries is Deferred).
    recursive subroutine mark_diary_entries(redraw, status)
        logical, intent(in) :: redraw
        type(status_type), intent(out) :: status
        type(handle_type) :: diary_buffer, temporary_buffer, old, saved_table
        type(excursion_type) :: excursion
        type(restriction_type) :: restriction
        character(len=:), allocatable :: file
        character(len=len(calendar_buffer)) :: saved_calendar_buffer
        integer(int64) :: inserted
        logical :: incp, saved_flag
        type(status_type) :: st

        status = status_type()
        call ensure_diary_date_forms()

        ! To remove any deleted diary entries. Do not redraw when:
        ! i) processing #include diary files (else only get the marks from
        ! the last #include file processed).
        ! ii) called via calendar_redraw (since calendar has already been
        ! erased).
        ! Use of REDRAW handles both of these cases.
        if (redraw .and. calendar_mark_diary_entries) then
            calendar_mark_diary_entries = .false.
            call calendar_redraw(status)
            if (.not. status%ok()) return
        end if

        saved_flag = diary_marking_entries_flag
        diary_marking_entries_flag = .true.
        diary_buffer = diary_file_buffer()
        ! Record current calendar buffer in case this function is
        ! called in a calendar-mode buffer not named `calendar_buffer'.
        saved_calendar_buffer = calendar_buffer
        calendar_buffer = calendar_get_buffer_name()
        incp = including

        if (.not. incp) then
            if (allocated(diary_included_files)) deallocate (diary_included_files)
            allocate (diary_included_files(0))
            call message("Marking diary entries...")
        end if

        old = current_buffer()
        temporary_buffer = null_handle()

        prepare: block
            if (is_buffer_live(diary_buffer)) then
                call set_buffer(diary_buffer, status)
                if (.not. status%ok()) exit prepare
            else if (incp) then
                call get_or_make_buffer(generate_new_buffer_name(" *diary-temp*"), temporary_buffer, status)
                if (.not. status%ok()) exit prepare
                call set_buffer(temporary_buffer, status)
                if (.not. status%ok()) exit prepare
                ! If including, caller has already verified it is readable.
                call insert_file_contents(diary_file, .false., inserted, status)
                if (.not. status%ok()) exit prepare
            else
                call check_diary_file(file, status)
                if (.not. status%ok()) exit prepare
                call find_file_noselect(file, diary_buffer, status)
                if (.not. status%ok()) exit prepare
                call set_buffer(diary_buffer, status)
                if (.not. status%ok()) exit prepare
            end if

            if (.not. is_buffer_live(temporary_buffer)) then
                call ensure_diary_mode(status)
                if (.not. status%ok()) exit prepare
            end if

            calendar_mark_diary_entries = .true.
            saved_table = syntax_table()
            call set_syntax_table(diary_syntax_table, st)
            call save_excursion_begin(excursion)
            call save_restriction_begin(restriction)
            ! bug#33423
            call widen()
            call mark_diary_entries_1(status)
            if (status%ok()) call mark_diary_expression_entries(status)
            ! Although it looks like mark-entries-hook runs every time,
            ! mark_included_diary_files clears it when it runs in
            ! included files.
            if (status%ok()) call diary_nongregorian_marking_hook%execute(status)
            if (status%ok()) call diary_mark_entries_hook%execute(status)
            call save_restriction_end(restriction)
            call save_excursion_end(excursion)
            call set_syntax_table(saved_table, st)
        end block prepare

        if (is_buffer_live(temporary_buffer)) call close_buffer(temporary_buffer, st)
        if (is_buffer_live(old)) call set_buffer(old, st)
        diary_marking_entries_flag = saved_flag
        calendar_buffer = saved_calendar_buffer
        if (status%ok() .and. .not. incp) call message("Marking diary entries...done")
    end subroutine mark_diary_entries

    ! The name of the calendar buffer (calendar-get-buffer's).  Not a port.
    function calendar_get_buffer_name() result(name)
        character(len=:), allocatable :: name
        type(handle_type) :: buffer

        buffer = calendar_get_buffer()
        name = trim(calendar_buffer)
        if (is_buffer_live(buffer)) name = trim(buffers(buffer%index)%name)
    end function calendar_get_buffer_name

    ! The command `mark_diary_entries': REDRAW is true, as upstream's
    ! (interactive "p").  Not a port.
    recursive subroutine execute_mark_diary_entries(status)
        type(status_type), intent(out) :: status

        call mark_diary_entries(.true., status)
    end subroutine execute_mark_diary_entries

    ! `mark_diary_entries' as the calendar calls it when it is drawn.
    ! Not a port.
    recursive subroutine mark_diary_entries_quietly(status)
        type(status_type), intent(out) :: status

        call mark_diary_entries(.false., status)
    end subroutine mark_diary_entries_quietly

    !> Mark diary entries of a certain type: the Gregorian entries of the
    !> date forms, by `mark_calendar_date_pattern'.  Port of
    !> diary-lib.el:diary-mark-entries-1 (the other calendars' MARKFUNC,
    !> MONTHS, SYMBOL and ABSFUNC are Deferred).
    recursive subroutine mark_diary_entries_1(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: dayname, monthname, regular_expression, dd_name, mm_name, y_string
        integer :: k, j, d_name_position, m_name_position, d_position, m_position, y_position, mm, dd, yy, current_year, offset
        integer(int64) :: hit
        type(calendar_date_type) :: today
        logical :: saved_fold
        type(status_type) :: st

        status = status_type()
        saved_fold = case_fold_search%value
        case_fold_search%value = .true.
        dayname = diary_name_pattern(calendar_day_names, calendar_day_abbreviations)
        monthname = diary_name_pattern(calendar_month_names, calendar_month_abbreviations)//"\|\*"

        do k = 1, size(diary_date_forms%entries)
            associate (form => diary_date_forms%entries(k))
                ! The positions of the keywords (the 'backup directive is
                ! not one of the parts).
                d_name_position = 0
                m_name_position = 0
                d_position = 0
                m_position = 0
                y_position = 0
                regular_expression = "^\("

                do j = 1, form%count
                    if (j > 1) regular_expression = regular_expression//"\)\("

                    select case (trim(form%parts(j)))
                    case ("dayname")
                        if (d_name_position == 0) d_name_position = j
                        regular_expression = regular_expression//dayname
                    case ("monthname")
                        if (m_name_position == 0) m_name_position = j
                        regular_expression = regular_expression//monthname
                    case ("month")
                        if (m_position == 0) m_position = j
                        regular_expression = regular_expression//"[0-9]+\|\*"
                    case ("day")
                        if (d_position == 0) d_position = j
                        regular_expression = regular_expression//"[0-9]+\|\*"
                    case ("year")
                        if (y_position == 0) y_position = j
                        regular_expression = regular_expression//"[0-9]+\|\*"
                    case default
                        regular_expression = regular_expression//trim(form%parts(j))
                    end select
                end do

                regular_expression = regular_expression//"\)"
                call goto_character(point_minimum())

                do
                    call regular_expression_search_forward(regular_expression, hit, st, noerror=SEARCH_NOERROR)

                    if (.not. st%ok()) then
                        status = st
                        exit
                    end if

                    if (hit == 0) exit
                    dd_name = ""
                    mm_name = ""
                    if (d_name_position > 0) dd_name = match_string_no_properties(d_name_position)
                    if (m_name_position > 0) mm_name = match_string_no_properties(m_name_position)
                    mm = 0
                    dd = 0
                    if (m_position > 0) mm = leading_number(match_string_no_properties(m_position))
                    if (d_position > 0) dd = leading_number(match_string_no_properties(d_position))
                    yy = 0

                    if (y_position > 0) then
                        y_string = match_string_no_properties(y_position)

                        if (len(y_string) == 2 .and. diary_abbreviated_year_flag) then
                            today = calendar_current_date()
                            current_year = today%year
                            yy = leading_number(y_string) + 100*(current_year/100)
                            offset = yy - current_year

                            ! Add 2-digit year to current century.
                            ! If more than 50 years in the future,
                            ! assume last century. If more than 50
                            ! years in the past, assume next century.
                            if (offset > 50) then
                                yy = yy - 100
                            else if (offset < -50) then
                                yy = yy + 100
                            end if
                        else
                            yy = leading_number(y_string)
                        end if
                    end if

                    ! Only mark all days of a given name if the pattern
                    ! contains no more specific elements.
                    if (d_name_position > 0 .and. .not. (d_position > 0 .or. m_position > 0 .or. y_position > 0)) then
                        call mark_calendar_days_named(day_number(dd_name), status)
                    else
                        if (m_name_position > 0) then
                            if (mm_name == "*") then
                                mm = 0
                            else
                                mm = month_number(mm_name)
                            end if
                        end if

                        call mark_calendar_date_pattern(mm, dd, yy, status)
                    end if

                    if (.not. status%ok()) exit
                end do
            end associate

            if (.not. status%ok()) exit
        end do

        case_fold_search%value = saved_fold
    end subroutine mark_diary_entries_1

    ! The number at the beginning of TEXT, 0 if there is none
    ! (string-to-number on the digits a date form matched).  Not a port.
    integer function leading_number(text) result(n)
        character(len=*), intent(in) :: text
        integer :: k

        n = 0

        do k = 1, len(text)
            if (scan(text(k:k), "0123456789") == 0) exit
            if (k > 9) exit
            n = 10*n + iachar(text(k:k)) - iachar("0")
        end do
    end function leading_number

    ! The day of the week (0 for Sunday) named NAME, its abbreviation or
    ! its abbreviation and a period, ignoring case (upstream's assoc-string
    ! in (calendar-make-alist calendar-day-name-array 0 ...)).  Not a port.
    integer function day_number(name) result(day)
        character(len=*), intent(in) :: name
        integer :: k

        day = 0

        do k = 0, 6
            if (is_same_name(name, calendar_day_names(k)) .or. is_same_name(name, calendar_day_abbreviations(k)) &
                .or. is_same_name(name, trim(calendar_day_abbreviations(k))//".")) then
                day = k
                return
            end if
        end do
    end function day_number

    ! The month (1 for January) named NAME, its abbreviation or its
    ! abbreviation and a period, ignoring case (upstream's assoc-string in
    ! its months-alist).  Not a port.
    integer function month_number(name) result(month)
        character(len=*), intent(in) :: name
        integer :: k

        month = 0

        do k = 1, 12
            if (is_same_name(name, calendar_month_names(k)) .or. is_same_name(name, calendar_month_abbreviations(k)) &
                .or. is_same_name(name, trim(calendar_month_abbreviations(k))//".")) then
                month = k
                return
            end if
        end do
    end function month_number

    ! Whether A and B are the same name, ignoring case and trailing
    ! blanks.  Not a port.
    logical function is_same_name(a, b) result(same)
        character(len=*), intent(in) :: a, b
        integer :: k

        same = len_trim(a) == len_trim(b)
        if (.not. same) return

        do k = 1, len_trim(a)
            if (lower(a(k:k)) /= lower(b(k:k))) then
                same = .false.
                return
            end if
        end do

    contains

        character function lower(c)
            character, intent(in) :: c

            lower = c
            if (c >= "A" .and. c <= "Z") lower = achar(iachar(c) + 32)
        end function lower
    end function is_same_name

    !> Mark days in the calendar window that have expression diary entries.
    !> Each entry in the diary file (or included files) visible in the calendar window
    !> is marked.  See the documentation for the function `list_diary_expression_entries'.
    !> Port of diary-lib.el:diary-mark-sexp-entries (the face markup of
    !> the entries is Deferred).
    recursive subroutine mark_diary_expression_entries(status)
        type(status_type), intent(out) :: status
        type(calendar_date_type) :: first, last, gdate
        type(expression_result_type) :: mark
        type(handle_type) :: old
        character(len=:), allocatable :: s_entry, expression, entry
        integer(int64) :: hit, expression_start, entry_start, found, shortage
        integer :: first_date, last_date, date
        type(status_type) :: st

        status = status_type()
        s_entry = "^\("//regular_expression_quote(diary_expression_entry_symbol)//"(\)\|\(" &
                  //regular_expression_quote(diary_nonmarking_symbol) &
                  //regular_expression_quote(diary_expression_entry_symbol)//"(diary-remind\)"
        old = current_buffer()
        call set_buffer(calendar_get_buffer(), st)
        call calendar_get_date_range(first, last, .true.)
        call set_buffer(old, st)
        first_date = calendar_absolute_from_gregorian(first)
        last_date = calendar_absolute_from_gregorian(last)
        call goto_character(point_minimum())

        do
            call regular_expression_search_forward(s_entry, hit, st, noerror=SEARCH_NOERROR)
            if (hit == 0) exit
            diary_marking_entry_flag = preceding_character() == iachar("(")
            call regular_expression_search_backward("(", found, st, noerror=SEARCH_NOERROR)
            expression_start = point()
            call forward_expression(1_int64, status)
            if (.not. status%ok()) return
            call buffer_substring(expression_start, point(), expression, st)
            call goto_character(min(point() + 1, point_maximum()))

            if (is_beginning_of_line() .and. .not. looking_at("[ \t]", st)) then
                ! Diary entry consists only of the expression.
                call goto_character(point() - 1)
                entry = ""
            else
                entry_start = point()
                ! Find end of entry.
                shortage = forward_line(1_int64)

                do while (looking_at("[ \t]", st))
                    shortage = forward_line(1_int64)
                end do

                if (is_beginning_of_line()) call goto_character(point() - 1)
                call buffer_substring(entry_start, point(), entry, st)
            end if

            ! FIXME this loops over all visible dates.
            ! Could be optimized in many cases. Depends on whether t or * present.
            do date = first_date, last_date
                gdate = calendar_gregorian_from_absolute(date)
                call process_diary_expression_entry(expression, entry, gdate, mark)
                if (.not. mark%applies) cycle
                call mark_date(gdate, mark%mark, status)
                if (.not. status%ok()) return
            end do
        end do
    end subroutine mark_diary_expression_entries

    ! Mark DATE in the calendar with MARK (a single-character string, a
    ! face's name, or none for `diary''s face).  Not a port.
    recursive subroutine mark_date(date, mark, status)
        type(calendar_date_type), intent(in) :: date
        type(expression_value_type), intent(in) :: mark
        type(status_type), intent(out) :: status
        type(named_face_type), pointer :: face

        select case (mark%kind)
        case (VALUE_STRING)
            call calendar_mark_visible_date(date, status, mark=mark%text)
        case (VALUE_NAME)
            face => face_named(face_name(mark%text))

            if (associated(face)) then
                call calendar_mark_visible_date(date, status, face=face)
            else
                call calendar_mark_visible_date(date, status)
            end if
        case default
            call calendar_mark_visible_date(date, status)
        end select
    end subroutine mark_date

    ! The Fortmacs name of the face that a diary file names NAME (with
    ! hyphens where Fortmacs's names have underscores).  Not a port.
    function face_name(name) result(file_name)
        character(len=*), intent(in) :: name
        character(len=:), allocatable :: file_name
        integer :: k

        file_name = name

        do k = 1, len(file_name)
            if (file_name(k:k) == "-") file_name(k:k) = "_"
        end do
    end function face_name

    !> Mark all dates in the calendar window that are day DAYNAME of the week.
    !> 0 means all Sundays, 1 means all Mondays, and so on.
    !> Optional argument FACE or MARK is passed to `calendar_mark_visible_date'.
    !> Port of diary-lib.el:calendar-mark-days-named.
    recursive subroutine mark_calendar_days_named(dayname, status, face, mark)
        integer, intent(in) :: dayname
        type(status_type), intent(out) :: status
        type(named_face_type), intent(in), optional :: face
        character(len=*), intent(in), optional :: mark
        type(handle_type) :: old
        integer :: m1, y1, m2, y2, day, last_day
        type(status_type) :: st

        status = status_type()
        old = current_buffer()
        call set_buffer(calendar_get_buffer(), status)
        if (.not. status%ok()) return
        call calendar_get_month_range(m1, y1, m2, y2)
        day = calendar_absolute_from_gregorian(calendar_nth_named_day(1, dayname, m1, y1))
        last_day = calendar_absolute_from_gregorian(calendar_nth_named_day(-1, dayname, m2, y2))

        do while (day <= last_day)
            call calendar_mark_visible_date(calendar_gregorian_from_absolute(day), status, face, mark)
            if (.not. status%ok()) exit
            day = day + 7
        end do

        call set_buffer(old, st)
    end subroutine mark_calendar_days_named

    !> Mark dates in the MONTH/YEAR that conform to pattern P_MONTH/P_DAY/P_YEAR.
    !> A value of 0 in any position of the pattern is a wildcard.
    !> Optional argument FACE or MARK is passed to `calendar_mark_visible_date'.
    !> Port of diary-lib.el:calendar-mark-month.
    recursive subroutine mark_calendar_month(month, year, p_month, p_day, p_year, status, face, mark)
        integer, intent(in) :: month, year, p_month, p_day, p_year
        type(status_type), intent(out) :: status
        type(named_face_type), intent(in), optional :: face
        character(len=*), intent(in), optional :: mark
        integer :: i

        status = status_type()

        if ((month == p_month .and. (p_year == 0 .or. year == p_year)) .or. &
            (p_month == 0 .and. (p_year == 0 .or. year == p_year))) then
            if (p_day == 0) then
                do i = 1, calendar_last_day_of_month(month, year)
                    call calendar_mark_visible_date(calendar_date_type(month, i, year), status, face, mark)
                    if (.not. status%ok()) return
                end do
            else
                call calendar_mark_visible_date(calendar_date_type(month, p_day, year), status, face, mark)
            end if
        end if
    end subroutine mark_calendar_month

    !> Mark all dates in the calendar window that conform to MONTH/DAY/YEAR.
    !> A value of 0 in any position is a wildcard.  Optional argument FACE
    !> or MARK is passed to `calendar_mark_visible_date'.  Port of
    !> diary-lib.el:calendar-mark-date-pattern.
    recursive subroutine mark_calendar_date_pattern(month, day, year, status, face, mark)
        integer, intent(in) :: month, day, year
        type(status_type), intent(out) :: status
        type(named_face_type), intent(in), optional :: face
        character(len=*), intent(in), optional :: mark
        type(handle_type) :: old
        integer :: m, y, m2, y2, k
        type(status_type) :: st

        status = status_type()
        old = current_buffer()
        call set_buffer(calendar_get_buffer(), status)
        if (.not. status%ok()) return
        call calendar_get_month_range(m, y, m2, y2)

        do k = 1, calendar_total_months_local()
            call mark_calendar_month(m, y, month, day, year, status, face, mark)
            if (.not. status%ok()) exit
            call calendar_increment_month(m, y, 1)
        end do

        call set_buffer(old, st)
    end subroutine mark_calendar_date_pattern

    ! ------------------------------------------------------------------
    ! Sorting
    ! ------------------------------------------------------------------

    !> Return time at the beginning of the string S as a military-style integer.
    !> For example, returns 1325 for 1:25pm.
    !>
    !> Returns `diary_unknown_time' (default value -9999) if no time is recognized.
    !> The recognized forms are XXXX, X:XX, or XX:XX (military time), and XXam,
    !> XXAM, XXpm, XXPM, XX:XXam, XX:XXAM, XX:XXpm, or XX:XXPM.  A period (.) can
    !> be used instead of a colon (:) to separate the hour and minute parts.
    !> Port of diary-lib.el:diary-entry-time.
    integer function diary_entry_time(s) result(time)
        character(len=*), intent(in) :: s
        logical :: saved_fold
        character(len=:), allocatable :: ampm
        type(status_type) :: st

        saved_fold = case_fold_search%value
        case_fold_search%value = .false.

        if (string_match("\`[ \t\n]*\([0-9]?[0-9]\)[:.]?\([0-9][0-9]\)\(\>\|[^ap]\)", s, st) > 0) then
            ! military time
            time = 100*leading_number(match_string_no_properties(1, s)) + leading_number(match_string_no_properties(2, s))
        else if (string_match("\`[ \t\n]*\([0-9]?[0-9]\)\([ap]\)m\>", s, st) > 0) then
            ! hour only (XXam or XXpm)
            time = 100*mod(leading_number(match_string_no_properties(1, s)), 12)
            ampm = match_string_no_properties(2, s)
            if (ampm /= "a") time = time + 1200
        else if (string_match("\`[ \t\n]*\([0-9]?[0-9]\)[:.]\([0-9][0-9]\)\([ap]\)m\>", s, st) > 0) then
            ! hour and minute (XX:XXam or XX:XXpm)
            time = 100*mod(leading_number(match_string_no_properties(1, s)), 12) &
                   + leading_number(match_string_no_properties(2, s))
            ampm = match_string_no_properties(3, s)
            if (ampm /= "a") time = time + 1200
        else
            ! unrecognizable
            time = diary_unknown_time
        end if

        case_fold_search%value = saved_fold
    end function diary_entry_time

    ! Return true if E1 is earlier than E2.  Port of
    ! diary-lib.el:diary-entry-compare (the operator `<' of
    ! `diary_entry_type').
    logical function diary_entry_compare(e1, e2) result(less)
        type(diary_entry_type), intent(in) :: e1, e2
        integer :: a1, a2, t1, t2

        a1 = calendar_absolute_from_gregorian(e1%date)
        a2 = calendar_absolute_from_gregorian(e2%date)
        less = a1 < a2
        if (less .or. a1 /= a2) return
        t1 = diary_entry_time(e1%string)
        t2 = diary_entry_time(e2%string)
        less = t1 < t2
        if (t1 == t2) less = is_string_less(e1%string, e2%string)
    end function diary_entry_compare

    !> Sort the list of diary entries by time of day.
    !> If you add this function to `diary_list_entries_hook', it should
    !> be the last item in the hook, in case earlier items add diary
    !> entries, or change the order.  Port of diary-lib.el:diary-sort-entries.
    recursive subroutine sort_diary_entries(status)
        type(status_type), intent(out) :: status
        type(diary_entry_type) :: item
        integer :: i, j

        status = status_type()
        if (.not. allocated(diary_entries)) return

        ! A stable insertion sort, as upstream's sort.
        do i = 2, size(diary_entries)
            item = diary_entries(i)
            j = i - 1

            do while (j >= 1)
                if (.not. (item < diary_entries(j))) exit
                diary_entries(j + 1) = diary_entries(j)
                j = j - 1
            end do

            diary_entries(j + 1) = item
        end do
    end subroutine sort_diary_entries

    ! ------------------------------------------------------------------
    ! Expression entries
    ! ------------------------------------------------------------------

    !> Add expression entries for DATE from the diary file to `diary_entries'.
    !> Also, make them visible in the diary.  FOUND is true if any entries are found.
    !>
    !> Expression diary entries must be prefaced by a `diary_expression_entry_symbol'
    !> (normally `%%').  The form of an expression diary entry is
    !>
    !>                   %%(FORM) ENTRY
    !>
    !> If the FORM gives nothing for the date, the diary entry does not
    !> apply.  If it gives something, ENTRY will be taken to apply to the
    !> date; if it gives a string, that string will be the diary entry in
    !> the fancy diary display.
    !>
    !> Fortmacs reads these forms, upstream's Lisp calls of the diary entry
    !> functions, with a fixed reader: an argument is an integer, t (all
    !> values) or its empty opposite, a quoted list of integers ('(1 2 3)), a string or a quoted face name
    !> ('diary); any other form is reported as a bad entry.  In the following,
    !> the optional parameter MARK specifies a face or single-character
    !> string to use when highlighting the day in the calendar.  For those
    !> functions that take MONTH, DAY, and YEAR as arguments, the order of
    !> the input parameters changes according to `calendar_date_style'
    !> (e.g. to DAY MONTH YEAR in the European style).
    !>
    !>   %%(diary-date MONTH DAY YEAR &optional MARK) text
    !>     Entry applies if date is MONTH, DAY, YEAR.  DAY, MONTH, and YEAR can
    !>     be a list of integers, t (meaning all values), or an integer.
    !>
    !>   %%(diary-float MONTH DAYNAME N &optional DAY MARK) text
    !>     Entry will appear on the Nth DAYNAME after/before MONTH DAY.
    !>     DAYNAME=0 means Sunday, DAYNAME=1 means Monday, and so on.
    !>     If N>0, use the Nth DAYNAME after MONTH DAY.
    !>     If N<0, use the Nth DAYNAME before MONTH DAY.
    !>     DAY defaults to 1 if N>0, and MONTH's last day otherwise.
    !>     MONTH can be a list of months, a single month, or t to
    !>     specify all months.
    !>
    !>   %%(diary-block M1 D1 Y1 M2 D2 Y2 &optional MARK) text
    !>     Entry will appear on dates between M1/D1/Y1 and M2/D2/Y2,
    !>     inclusive.
    !>
    !>   %%(diary-anniversary MONTH DAY YEAR &optional MARK) text
    !>     Entry will appear on anniversary dates of MONTH DAY, YEAR.
    !>     Text can contain `%d' or `%d%s'; `%d' will be replaced by the
    !>     number of years since the MONTH DAY, YEAR, and `%s' by the
    !>     ordinal ending of that number (i.e. `st', `nd', `rd' or `th',
    !>     as appropriate).  The anniversary of February 29 is
    !>     considered to be March 1 in a non-leap year.
    !>
    !>   %%(diary-cyclic N MONTH DAY YEAR &optional MARK) text
    !>     Entry will appear every N days, starting MONTH DAY, YEAR.
    !>     Text can contain `%d' or `%d%s'; `%d' will be replaced by the
    !>     number of repetitions since the MONTH DAY, YEAR and `%s' by
    !>     the ordinal ending of that number (i.e. `st', `nd', `rd' or
    !>     `th', as appropriate).
    !>
    !>   %%(diary-remind 'FORM DAYS &optional MARKING) text
    !>     Entry is a reminder for diary entry form FORM.  DAYS is either a
    !>     single number or a list of numbers indicating the number(s)
    !>     of days before the event that the warning(s) should occur.
    !>     A negative number -DAYS has the same meaning as a list (1 2 ... DAYS).
    !>     If the current date is (one of) DAYS before the event indicated
    !>     by FORM, then a suitable message appears.  In addition to the
    !>     reminders beforehand, the diary entry also appears on the
    !>     date itself.  If optional MARKING is true then the
    !>     *reminders* are marked on the calendar.  Marking of reminders
    !>     is independent of whether the entry *itself* is a marking or
    !>     non-marking one.
    !>
    !>   %%(diary-offset 'FORM DAYS) text
    !>     Entry applies if the date is DAYS days after the date of the
    !>     diary entry form FORM.
    !>
    !> `diary-day-of-year' does not accept any text, and so only makes
    !> sense with `display_fancy_diary': it gives the day of year and
    !> number of days remaining, every day.
    !>
    !> Marking these entries is *extremely* time consuming, so it is
    !> best if they are non-marking.  Port of
    !> diary-lib.el:diary-list-sexp-entries (the entry functions of the
    !> other calendars, lunar.el and solar.el are Deferred, and so is the
    !> face markup of the entries).
    recursive subroutine list_diary_expression_entries(date, found, status)
        type(calendar_date_type), intent(in) :: date
        logical, intent(out) :: found
        type(status_type), intent(out) :: status
        type(expression_result_type) :: result
        character(len=:), allocatable :: s_entry, expression, entry, specifier, literal
        integer(int64) :: hit, expression_start, line_start, entry_start, shortage
        type(status_type) :: st

        status = status_type()
        found = .false.
        s_entry = "^"//regular_expression_quote(diary_nonmarking_symbol)//"?" &
                  //regular_expression_quote(diary_expression_entry_symbol)//"("
        call goto_character(point_minimum())

        do
            call regular_expression_search_forward(s_entry, hit, st, noerror=SEARCH_NOERROR)
            if (hit == 0) exit
            call goto_character(point() - 1)
            expression_start = point()
            call forward_expression(1_int64, status)
            if (.not. status%ok()) return
            call buffer_substring(expression_start, point(), expression, st)
            line_start = line_end_position(0_int64)
            call buffer_substring(line_start + 1, point(), specifier, st)
            entry_start = line_start + 1
            call goto_character(min(point() + 1, point_maximum()))

            if (is_beginning_of_line() .and. .not. looking_at("[ \t]", st)) then
                ! Diary entry consists only of the expression.
                call goto_character(point() - 1)
                entry = ""
            else
                entry_start = point()
                shortage = forward_line(1_int64)

                do while (looking_at("[ \t]", st))
                    shortage = forward_line(1_int64)
                end do

                if (is_beginning_of_line()) call goto_character(point() - 1)
                call buffer_substring(entry_start, point(), entry, st)
            end if

            call process_diary_expression_entry(expression, entry, date, result)
            ! before evaluation
            literal = entry

            if (result%applies) then
                if (result%has_text) entry = result%text
                call remove_overlays(line_start, point(), INVISIBLE_PROPERTY, value_type(name=DIARY_INVISIBLE), st)
                call append_diary_entry(date, entry, specifier, entry_start, literal)
                found = .true.
            end if
        end do
    end subroutine list_diary_expression_entries

    !> Process an expression diary ENTRY for DATE: EXPRESSION is the entry's
    !> form, upstream's Lisp call of a diary entry function.  RESULT says
    !> whether the entry applies, with its text and its mark.  A form that
    !> cannot be read or evaluated is reported as a warning ("Bad diary
    !> expression") and does not apply.  Port of
    !> diary-lib.el:diary-sexp-entry (a fixed reader of the forms
    !> `list_diary_expression_entries' describes; `calendar_debug_expression'
    !> is Deferred).
    recursive subroutine process_diary_expression_entry(expression, entry, date, result)
        character(len=*), intent(in) :: expression, entry
        type(calendar_date_type), intent(in) :: date
        type(expression_result_type), intent(out) :: result
        character(len=:), allocatable :: text, error_text
        type(status_type) :: status

        call evaluate_entry_expression(expression, entry, date, result, status)

        if (.not. status%ok()) then
            error_text = format_error(status)
            allocate (character(len=len(diary_file) + len(expression) + len(error_text) + 64) :: text)
            write (text, "(a, i0, 8a)") "Bad diary expression at line ", count_lines(point_minimum(), point()), " in ", &
                diary_file, ":", achar(10), expression, achar(10), "Error: ", error_text
            call display_warning("diary", trim(text)//achar(10), WARNING_LEVEL_ERROR)
            result = expression_result_type()
        end if
    end subroutine process_diary_expression_entry

    ! Read the entry form EXPRESSION and evaluate it for DATE and ENTRY into
    ! RESULT; an error condition occurs for a form the reader does not
    ! know.  Not a port (the reader of upstream's eval in
    ! diary-lib.el:diary-sexp-entry).
    recursive subroutine evaluate_entry_expression(expression, entry, date, result, status)
        character(len=*), intent(in) :: expression, entry
        type(calendar_date_type), intent(in) :: date
        type(expression_result_type), intent(out) :: result
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: form_name
        type(expression_value_type), allocatable :: arguments(:)
        integer :: k

        result = expression_result_type()
        call read_entry_expression(expression, form_name, arguments, status)
        if (.not. status%ok()) return
        ! The form names a function of the diary file, text read at run time.
        k = findloc(entry_functions%name == form_name, .true., dim=1)

        if (k == 0) then
            call set_error(status, GENERIC_ERROR_CODE, "Unknown diary entry form "//form_name)
        else if (size(arguments) < entry_functions(k)%least .or. size(arguments) > entry_functions(k)%most) then
            call set_error(status, WRONG_TYPE_ARGUMENT_CODE, "Wrong number of arguments: "//form_name)
        else
            call entry_functions(k)%compute(arguments, date, entry, result, status)
        end if

        ! (cons mark entry) gives the entry; a string gives itself.
        if (status%ok() .and. result%applies .and. .not. result%has_text) then
            result%has_text = .true.
            result%text = entry
        end if
    end subroutine evaluate_entry_expression

    ! Read the entry form TEXT, "(NAME ARGUMENT...)", into NAME and ARGUMENTS.
    ! An argument is an integer, t, nil, a string, a quoted list of
    ! integers, a quoted name or a quoted form.  Not a port.
    recursive subroutine read_entry_expression(text, name, arguments, status)
        character(len=*), intent(in) :: text
        character(len=:), allocatable, intent(out) :: name
        type(expression_value_type), allocatable, intent(out) :: arguments(:)
        type(status_type), intent(out) :: status
        type(expression_value_type) :: argument
        integer :: k

        status = status_type()
        allocate (arguments(0))
        name = ""
        k = 1
        call skip_blanks(text, k)

        if (k > len(text)) then
            call set_error(status, GENERIC_ERROR_CODE, "End of file during parsing")
            return
        end if

        if (text(k:k) /= "(") then
            call set_error(status, GENERIC_ERROR_CODE, "Invalid diary entry form")
            return
        end if

        k = k + 1
        call skip_blanks(text, k)
        name = read_entry_word(text, k)

        if (len(name) == 0) then
            call set_error(status, GENERIC_ERROR_CODE, "Invalid diary entry form")
            return
        end if

        do
            call skip_blanks(text, k)

            if (k > len(text)) then
                call set_error(status, GENERIC_ERROR_CODE, "End of file during parsing")
                return
            end if

            if (text(k:k) == ")") exit
            call read_argument(text, k, argument, status)
            if (.not. status%ok()) return
            arguments = [arguments, argument]
        end do
    end subroutine read_entry_expression

    ! Read the argument of TEXT at K into ARGUMENT, leaving K after it.
    ! Not a port.
    recursive subroutine read_argument(text, k, argument, status)
        character(len=*), intent(in) :: text
        integer, intent(inout) :: k
        type(expression_value_type), intent(out) :: argument
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: word
        integer :: start, depth
        logical :: in_string

        status = status_type()

        select case (text(k:k))
        case ('"')
            ! A string, with backslash escapes.
            argument%kind = VALUE_STRING
            argument%text = ""
            k = k + 1

            do
                if (k > len(text)) then
                    call set_error(status, GENERIC_ERROR_CODE, "End of file during parsing")
                    return
                end if

                if (text(k:k) == '"') exit

                if (text(k:k) == "\") then
                    k = k + 1
                    if (k > len(text)) cycle
                end if

                argument%text = argument%text//text(k:k)
                k = k + 1
            end do

            k = k + 1
        case ("'")
            k = k + 1
            call skip_blanks(text, k)

            if (k > len(text)) then
                call set_error(status, GENERIC_ERROR_CODE, "End of file during parsing")
                return
            end if

            if (text(k:k) == "(") then
                ! A quoted list: integers, or a form.
                start = k
                depth = 0
                in_string = .false.

                do while (k <= len(text))
                    if (in_string) then
                        if (text(k:k) == "\") then
                            k = k + 1
                        else if (text(k:k) == '"') then
                            in_string = .false.
                        end if
                    else if (text(k:k) == '"') then
                        in_string = .true.
                    else if (text(k:k) == "(") then
                        depth = depth + 1
                    else if (text(k:k) == ")") then
                        depth = depth - 1

                        if (depth == 0) then
                            k = k + 1
                            exit
                        end if
                    end if

                    k = k + 1
                end do

                if (depth /= 0) then
                    call set_error(status, GENERIC_ERROR_CODE, "End of file during parsing")
                    return
                end if

                call read_quoted_list(text(start:k - 1), argument, status)
            else
                word = read_entry_word(text, k)

                select case (word)
                case ("nil")
                    argument%kind = VALUE_NONE
                case ("t")
                    argument%kind = VALUE_ALL
                case ("")
                    call set_error(status, GENERIC_ERROR_CODE, "Invalid read syntax")
                case default
                    if (is_integer_text(word)) then
                        argument%kind = VALUE_INTEGER
                        read (word, *) argument%value
                    else
                        argument%kind = VALUE_NAME
                        argument%text = word
                    end if
                end select
            end if
        case ("(")
            call set_error(status, GENERIC_ERROR_CODE, "Unknown diary entry form "//text(k:))
        case default
            word = read_entry_word(text, k)

            select case (word)
            case ("nil")
                argument%kind = VALUE_NONE
            case ("t")
                argument%kind = VALUE_ALL
            case ("")
                call set_error(status, GENERIC_ERROR_CODE, "Invalid read syntax")
            case default
                if (is_integer_text(word)) then
                    argument%kind = VALUE_INTEGER
                    read (word, *) argument%value
                else
                    call set_error(status, GENERIC_ERROR_CODE, "Unknown diary entry variable "//word)
                end if
            end select
        end select
    end subroutine read_argument

    ! Read the quoted list TEXT, "(...)": integers, nil for none, or a
    ! form whose first element is a name.  Not a port.
    recursive subroutine read_quoted_list(text, argument, status)
        character(len=*), intent(in) :: text
        type(expression_value_type), intent(out) :: argument
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: word
        integer :: k, value

        status = status_type()
        k = 2
        call skip_blanks(text, k)

        if (text(k:k) == ")") then
            ! '() is nil.
            argument%kind = VALUE_NONE
            return
        end if

        word = read_entry_word(text, k)

        if (.not. is_integer_text(word)) then
            argument%kind = VALUE_EXPRESSION
            argument%text = text
            return
        end if

        argument%kind = VALUE_INTEGERS
        allocate (argument%integers(0))

        do
            if (.not. is_integer_text(word)) then
                call set_error(status, WRONG_TYPE_ARGUMENT_CODE, "Wrong type argument: expected integer, got "//word)
                return
            end if

            read (word, *) value
            argument%integers = [argument%integers, value]
            call skip_blanks(text, k)
            if (k > len(text)) exit
            if (text(k:k) == ")") exit
            word = read_entry_word(text, k)
        end do
    end subroutine read_quoted_list

    ! Move K past blanks and newlines in TEXT.  Not a port.
    subroutine skip_blanks(text, k)
        character(len=*), intent(in) :: text
        integer, intent(inout) :: k

        do while (k <= len(text))
            if (scan(text(k:k), " "//achar(9)//achar(10)//achar(13)) == 0) exit
            k = k + 1
        end do
    end subroutine skip_blanks

    ! The word (a name or a number) of TEXT at K, moving K past it.
    ! Not a port.
    function read_entry_word(text, k) result(word)
        character(len=*), intent(in) :: text
        integer, intent(inout) :: k
        character(len=:), allocatable :: word
        integer :: start

        start = k

        do while (k <= len(text))
            if (scan(text(k:k), " ()'"""//achar(9)//achar(10)//achar(13)) /= 0) exit
            k = k + 1
        end do

        word = text(start:k - 1)
    end function read_entry_word

    ! Whether TEXT is an integer: digits, with an optional sign and a
    ! final period.  Not a port.
    logical function is_integer_text(text) result(is_integer)
        character(len=*), intent(in) :: text
        integer :: first, last

        is_integer = .false.
        if (len(text) == 0) return
        first = 1
        last = len(text)
        if (scan(text(1:1), "+-") /= 0) first = 2
        if (text(last:last) == ".") last = last - 1
        if (last < first) return
        if (last - first > 8) return
        is_integer = verify(text(first:last), "0123456789") == 0
    end function is_integer_text

    ! The integer of ARGUMENT; an error condition occurs if it is not one.
    ! Not a port.
    integer function integer_argument(argument, status) result(value)
        type(expression_value_type), intent(in) :: argument
        type(status_type), intent(inout) :: status

        value = 0
        if (.not. status%ok()) return

        if (argument%kind /= VALUE_INTEGER) then
            call set_error(status, WRONG_TYPE_ARGUMENT_CODE, "Wrong type argument: expected integer")
            return
        end if

        value = argument%value
    end function integer_argument

    ! Whether the integer N matches ARGUMENT: an integer, a list of integers or
    ! t (upstream's (or (and (listp dd) (memq d dd)) (equal d dd) (eq dd
    ! t))).  Not a port.
    logical function matches(argument, n)
        type(expression_value_type), intent(in) :: argument
        integer, intent(in) :: n

        select case (argument%kind)
        case (VALUE_INTEGER)
            matches = argument%value == n
        case (VALUE_INTEGERS)
            matches = any(argument%integers == n)
        case (VALUE_ALL)
            matches = .true.
        case default
            matches = .false.
        end select
    end function matches

    !> Convert A B C into the internal calendar date form, MONTH, DAY,
    !> YEAR.  The expected order of the inputs depends on
    !> `calendar_date_style', e.g. in the European case, A = day, B =
    !> month, C = year.  Port of diary-lib.el:diary-make-date (for
    !> integers; the entry forms reorder their arguments the same way).
    pure function make_diary_date(a, b, c) result(date)
        integer, intent(in) :: a, b, c
        type(calendar_date_type) :: date

        date = calendar_date_type(a, b, c)

        select case (calendar_date_style)
        case (CALENDAR_DATE_STYLE_ISO)
            ! YMD
            date = calendar_date_type(b, c, a)
        case (CALENDAR_DATE_STYLE_EUROPEAN)
            ! DMY
            date = calendar_date_type(b, a, c)
        case default
            continue
        end select
    end function make_diary_date

    ! The arguments A B C of an entry form as MONTH, DAY, YEAR, in the
    ! order of `calendar_date_style' (diary-make-date on any values).
    ! Not a port.
    subroutine order_date_arguments(a, b, c, month, day, year)
        type(expression_value_type), intent(in) :: a, b, c
        type(expression_value_type), intent(out) :: month, day, year

        select case (calendar_date_style)
        case (CALENDAR_DATE_STYLE_ISO)
            month = b
            day = c
            year = a
        case (CALENDAR_DATE_STYLE_EUROPEAN)
            month = b
            day = a
            year = c
        case default
            month = a
            day = b
            year = c
        end select
    end subroutine order_date_arguments

    ! ARGUMENTS(K), or nil if there is no such argument.  Not a port.
    function optional_argument(arguments, k) result(argument)
        type(expression_value_type), intent(in) :: arguments(:)
        integer, intent(in) :: k
        type(expression_value_type) :: argument

        argument = expression_value_type()
        if (k <= size(arguments)) argument = arguments(k)
    end function optional_argument

    ! A date of three integer arguments in the order of
    ! `calendar_date_style'.  Not a port.
    function date_argument(a, b, c, status) result(date)
        type(expression_value_type), intent(in) :: a, b, c
        type(status_type), intent(inout) :: status
        type(calendar_date_type) :: date
        type(expression_value_type) :: month, day, year

        call order_date_arguments(a, b, c, month, day, year)
        date%month = integer_argument(month, status)
        date%day = integer_argument(day, status)
        date%year = integer_argument(year, status)
    end function date_argument

    ! Specific date(s) diary entry.
    ! Entry applies if date is MONTH, DAY, YEAR.  Each parameter can be a
    ! list of integers, t (meaning all values), or an integer.  The order
    ! of the input parameters changes according to `calendar_date_style'
    ! (e.g. to DAY MONTH YEAR in the European style).
    !
    ! An optional parameter MARK specifies a face or single-character string
    ! to use when highlighting the day in the calendar.  Port of
    ! diary-lib.el:diary-date.
    recursive subroutine diary_date(arguments, date, entry, result, status)
        type(expression_value_type), intent(in) :: arguments(:)
        type(calendar_date_type), intent(in) :: date
        character(len=*), intent(in) :: entry
        type(expression_result_type), intent(out) :: result
        type(status_type), intent(out) :: status
        type(expression_value_type) :: mm, dd, yy

        status = status_type()
        call order_date_arguments(arguments(1), arguments(2), arguments(3), mm, dd, yy)
        result%applies = matches(dd, date%day) .and. matches(mm, date%month) .and. matches(yy, date%year)
        result%mark = optional_argument(arguments, 4)
    end subroutine diary_date

    ! Block diary entry.
    ! Entry applies if date is between, or on one of, two dates.  The order
    ! of the input parameters changes according to `calendar_date_style'
    ! (e.g. to D1, M1, Y1, D2, M2, Y2 in the European style).
    !
    ! An optional parameter MARK specifies a face or single-character string
    ! to use when highlighting the day in the calendar.  Port of
    ! diary-lib.el:diary-block.
    recursive subroutine diary_block(arguments, date, entry, result, status)
        type(expression_value_type), intent(in) :: arguments(:)
        type(calendar_date_type), intent(in) :: date
        character(len=*), intent(in) :: entry
        type(expression_result_type), intent(out) :: result
        type(status_type), intent(out) :: status
        integer :: date1, date2, d

        status = status_type()
        date1 = calendar_absolute_from_gregorian(date_argument(arguments(1), arguments(2), arguments(3), status))
        date2 = calendar_absolute_from_gregorian(date_argument(arguments(4), arguments(5), arguments(6), status))
        if (.not. status%ok()) return
        d = calendar_absolute_from_gregorian(date)
        result%applies = date1 <= d .and. d <= date2
        result%mark = optional_argument(arguments, 7)
    end subroutine diary_block

    ! Diary entry for the Nth DAYNAME after/before MONTH DAY.
    ! DAYNAME=0 means Sunday, DAYNAME=1 means Monday, and so on.
    ! If N>0, use the Nth DAYNAME after MONTH DAY.
    ! If N<0, use the Nth DAYNAME before MONTH DAY.
    ! DAY defaults to 1 if N>0, and MONTH's last day otherwise.
    ! MONTH can be a list of months, an integer, or t (meaning all months).
    ! Optional MARK specifies a face or single-character string to use when
    ! highlighting the day in the calendar.  Port of
    ! diary-lib.el:diary-float.
    recursive subroutine diary_float(arguments, date, entry, result, status)
        type(expression_value_type), intent(in) :: arguments(:)
        type(calendar_date_type), intent(in) :: date
        character(len=*), intent(in) :: entry
        type(expression_result_type), intent(out) :: result
        type(status_type), intent(out) :: status
        type(expression_value_type) :: month, day_argument
        type(calendar_date_type) :: first, last
        integer :: dayname, n, limit, last_absolute, first_absolute, day
        logical :: has_day

        status = status_type()
        month = arguments(1)
        dayname = integer_argument(arguments(2), status)
        n = integer_argument(arguments(3), status)
        day_argument = optional_argument(arguments, 4)
        has_day = day_argument%kind /= VALUE_NONE
        day = 0
        if (has_day) day = integer_argument(day_argument, status)
        if (.not. status%ok()) return
        result%mark = optional_argument(arguments, 5)
        ! This is messy because the diary entry may apply, but the date on which it
        ! is based can be in a different month/year.  For example, asking for the
        ! first Monday after December 30.  For large values of |n| the problem is
        ! more grotesque.
        if (dayname /= calendar_day_of_week(date)) return
        ! Last (n>0) or first (n<0) possible base date for entry.
        limit = calendar_nth_named_absday(-n, dayname, date%month, date%year, date%day)

        if (n > 0) then
            last_absolute = limit
            first_absolute = limit - 6
        else
            last_absolute = limit + 6
            first_absolute = limit
        end if

        last = calendar_gregorian_from_absolute(last_absolute)
        first = calendar_gregorian_from_absolute(first_absolute)

        if (first%month == last%month) then
            ! only possible base dates in one month
            result%applies = matches(month, first%month) .and. first%day <= base_day(first) &
                             .and. base_day(first) <= last%day
        else if (first%year < last%year .or. (first%year == last%year .and. first%month < last%month)) then
            ! Only possible base dates straddle two months.
            result%applies = (matches(month, first%month) .and. first%day <= base_day(first)) &
                             .or. (matches(month, last%month) .and. base_day(last) <= last%day)
        end if

    contains

        ! (or day (if (> n 0) 1 (calendar-last-day-of-month m y)))
        integer function base_day(d)
            type(calendar_date_type), intent(in) :: d

            if (has_day) then
                base_day = day
            else if (n > 0) then
                base_day = 1
            else
                base_day = calendar_last_day_of_month(d%month, d%year)
            end if
        end function base_day
    end subroutine diary_float

    !> Ordinal suffix for N.  (That is, `st', `nd', `rd', or `th', as
    !> appropriate.)  Port of diary-lib.el:diary-ordinal-suffix.
    pure function diary_ordinal_suffix(n) result(suffix)
        integer, intent(in) :: n
        character(len=2) :: suffix
        character(len=2), parameter :: suffixes(0:3) = ["th", "st", "nd", "rd"]

        if (any(mod(n, 100) == [11, 12, 13]) .or. 3 < mod(n, 10)) then
            suffix = "th"
        else if (mod(n, 10) < 0) then
            ! (aref ["th" "st" "nd" "rd"] (% n 10)) of a negative N is an error.
            suffix = "th"
        else
            suffix = suffixes(mod(n, 10))
        end if
    end function diary_ordinal_suffix

    ! ENTRY formatted with the number N and its ordinal suffix: `%d' is N,
    ! `%s' the next value as text, `%%' a percent sign (upstream's (format
    ! entry n (diary-ordinal-suffix n))).  An error condition occurs for
    ! another directive or a missing value.  Not a port.
    recursive subroutine format_entry(entry, n, text, status)
        character(len=*), intent(in) :: entry
        integer, intent(in) :: n
        character(len=:), allocatable, intent(out) :: text
        type(status_type), intent(out) :: status
        character(len=32) :: digits
        integer :: k, used

        status = status_type()
        text = ""
        used = 0
        k = 1

        do while (k <= len(entry))
            if (entry(k:k) /= "%") then
                text = text//entry(k:k)
                k = k + 1
                cycle
            end if

            if (k == len(entry)) then
                call set_error(status, GENERIC_ERROR_CODE, "Format string ends in middle of format specifier")
                return
            end if

            select case (entry(k + 1:k + 1))
            case ("%")
                text = text//"%"
            case ("d")
                used = used + 1

                if (used == 1) then
                    write (digits, "(i0)") n
                    text = text//trim(digits)
                else if (used == 2) then
                    call set_error(status, GENERIC_ERROR_CODE, "Format specifier doesn't match argument type")
                    return
                else
                    call set_error(status, GENERIC_ERROR_CODE, "Not enough arguments for format string")
                    return
                end if
            case ("s")
                used = used + 1

                if (used == 1) then
                    write (digits, "(i0)") n
                    text = text//trim(digits)
                else if (used == 2) then
                    text = text//trim(diary_ordinal_suffix(n))
                else
                    call set_error(status, GENERIC_ERROR_CODE, "Not enough arguments for format string")
                    return
                end if
            case default
                call set_error(status, GENERIC_ERROR_CODE, "Invalid format operation %"//entry(k + 1:k + 1))
                return
            end select

            k = k + 2
        end do
    end subroutine format_entry

    ! Anniversary diary entry.
    ! Entry applies if date is the anniversary of MONTH, DAY, YEAR.
    ! The order of the input parameters changes according to
    ! `calendar_date_style' (e.g. to DAY MONTH YEAR in the European style).
    !
    ! The diary entry can contain `%d' or `%d%s'; the %d will be replaced
    ! by the number of years since the MONTH, DAY, YEAR, and the %s will
    ! be replaced by the ordinal ending of that number (that is, `st',
    ! `nd', `rd' or `th', as appropriate).  The anniversary of February 29
    ! is considered to be March 1 in non-leap years.
    !
    ! An optional parameter MARK specifies a face or single-character
    ! string to use when highlighting the day in the calendar.  Port of
    ! diary-lib.el:diary-anniversary.
    recursive subroutine diary_anniversary(arguments, date, entry, result, status)
        type(expression_value_type), intent(in) :: arguments(:)
        type(calendar_date_type), intent(in) :: date
        character(len=*), intent(in) :: entry
        type(expression_result_type), intent(out) :: result
        type(status_type), intent(out) :: status
        type(expression_value_type) :: mm_argument, dd_argument, yy_argument
        integer :: mm, dd, y, diff

        status = status_type()
        call order_date_arguments(arguments(1), arguments(2), optional_argument(arguments, 3), mm_argument, dd_argument, &
                                  yy_argument)
        dd = integer_argument(dd_argument, status)
        mm = integer_argument(mm_argument, status)
        y = date%year

        if (yy_argument%kind == VALUE_NONE) then
            diff = 100
        else
            diff = y - integer_argument(yy_argument, status)
        end if

        if (.not. status%ok()) return

        if (mm == 2 .and. dd == 29 .and. .not. is_calendar_leap_year(y)) then
            mm = 3
            dd = 1
        end if

        if (diff > 0 .and. calendar_date_type(mm, dd, y) == date) then
            result%applies = .true.
            result%has_text = .true.
            call format_entry(entry, diff, result%text, status)
            result%mark = optional_argument(arguments, 4)
        end if
    end subroutine diary_anniversary

    ! Cycle diary entry--entry applies every N days starting at MONTH, DAY, YEAR.
    ! The order of the input parameters changes according to
    ! `calendar_date_style' (e.g. to N DAY MONTH YEAR in the European
    ! style).  The entry can contain `%d' or `%d%s'; the %d will be
    ! replaced by the number of repetitions since the MONTH DAY YEAR,
    ! and %s by the ordinal ending of that number (that is, `st', `nd',
    ! `rd' or `th', as appropriate).
    !
    ! An optional parameter MARK specifies a face or single-character
    ! string to use when highlighting the day in the calendar.  Port of
    ! diary-lib.el:diary-cyclic.
    recursive subroutine diary_cyclic(arguments, date, entry, result, status)
        type(expression_value_type), intent(in) :: arguments(:)
        type(calendar_date_type), intent(in) :: date
        character(len=*), intent(in) :: entry
        type(expression_result_type), intent(out) :: result
        type(status_type), intent(out) :: status
        integer :: n, diff, cycle_number

        status = status_type()
        n = integer_argument(arguments(1), status)
        if (.not. status%ok()) return

        if (n <= 0) then
            call set_error(status, USER_ERROR_CODE, "Day count must be positive")
            return
        end if

        diff = calendar_absolute_from_gregorian(date) &
               - calendar_absolute_from_gregorian(date_argument(arguments(2), arguments(3), arguments(4), status))
        if (.not. status%ok()) return
        cycle_number = diff/n

        if (diff >= 0 .and. mod(diff, n) == 0) then
            result%applies = .true.
            result%has_text = .true.
            call format_entry(entry, cycle_number, result%text, status)
            result%mark = optional_argument(arguments, 5)
        end if
    end subroutine diary_cyclic

    ! Offsetted diary entry.  Offsets the entry form FORM by DAYS days.
    ! Entry applies if the date is DAYS days after another diary entry
    ! form FORM.  Port of diary-lib.el:diary-offset.
    recursive subroutine diary_offset(arguments, date, entry, result, status)
        type(expression_value_type), intent(in) :: arguments(:)
        type(calendar_date_type), intent(in) :: date
        character(len=*), intent(in) :: entry
        type(expression_result_type), intent(out) :: result
        type(status_type), intent(out) :: status
        integer :: days

        status = status_type()

        if (arguments(2)%kind /= VALUE_INTEGER) then
            call set_error(status, USER_ERROR_CODE, "Days must be an integer")
            return
        end if

        days = arguments(2)%value

        if (arguments(1)%kind /= VALUE_EXPRESSION) then
            call set_error(status, WRONG_TYPE_ARGUMENT_CODE, "Wrong type argument: expected a diary entry form")
            return
        end if

        call evaluate_entry_expression(arguments(1)%text, entry, &
                                       calendar_gregorian_from_absolute(calendar_absolute_from_gregorian(date) - days), result, &
                                       status)
    end subroutine diary_offset

    ! Day of year and number of days remaining in the year of date diary
    ! entry.  Port of diary-lib.el:diary-day-of-year.
    recursive subroutine diary_day_of_year(arguments, date, entry, result, status)
        type(expression_value_type), intent(in) :: arguments(:)
        type(calendar_date_type), intent(in) :: date
        character(len=*), intent(in) :: entry
        type(expression_result_type), intent(out) :: result
        type(status_type), intent(out) :: status

        status = status_type()
        result%applies = .true.
        result%has_text = .true.
        result%text = calendar_day_of_year_string(date)
    end subroutine diary_day_of_year

    ! Provide a reminder of a diary entry.
    ! FORM is a diary entry form.  DAYS is either a single number or a list
    ! of numbers indicating the number(s) of days before the event that
    ! the warning(s) should occur on.  A negative number -DAYS has the
    ! same meaning as a list (1 2 ... DAYS).  If the current date
    ! is (one of) DAYS before the event indicated by FORM, then this function
    ! returns a suitable message.
    !
    ! In addition to the reminders beforehand, the diary entry also
    ! appears on the date itself.
    !
    ! A `diary_nonmarking_symbol' at the beginning of the line of the
    ! diary-remind entry specifies that the diary entry (not the
    ! reminder) is non-marking.  Marking of reminders is independent of
    ! whether the entry itself is a marking or nonmarking; if optional
    ! parameter MARKING is true then the reminders are marked on the
    ! calendar.  Port of diary-lib.el:diary-remind (with the default
    ! `diary_remind_message'; customizing it is Deferred).
    recursive subroutine diary_remind(arguments, date, entry, result, status)
        type(expression_value_type), intent(in) :: arguments(:)
        type(calendar_date_type), intent(in) :: date
        character(len=*), intent(in) :: entry
        type(expression_result_type), intent(out) :: result
        type(status_type), intent(out) :: status
        type(expression_value_type) :: marking
        integer, allocatable :: days(:)
        integer :: k

        status = status_type()

        if (arguments(1)%kind /= VALUE_EXPRESSION) then
            call set_error(status, WRONG_TYPE_ARGUMENT_CODE, "Wrong type argument: expected a diary entry form")
            return
        end if

        marking = optional_argument(arguments, 3)

        ! Convert a negative number to a list of days.
        select case (arguments(2)%kind)
        case (VALUE_INTEGER)
            if (arguments(2)%value < 0) then
                days = [(k, k=1, -arguments(2)%value)]
                call remind_list(days, result, status)
            else
                call remind_one(arguments(2)%value, .true., result, status)
            end if
        case (VALUE_INTEGERS)
            call remind_list(arguments(2)%integers, result, status)
        case default
            ! The entry applies on the date only.
            call remind_one(0, .false., result, status)
        end select

    contains

        ! (diary-remind expression DAYS marking) for an integer DAYS (HAS_DAYS)
        ! or for none.
        recursive subroutine remind_one(days, has_days, result, status)
            integer, intent(in) :: days
            logical, intent(in) :: has_days
            type(expression_result_type), intent(out) :: result
            type(status_type), intent(out) :: status
            type(expression_result_type) :: diary_entry
            character(len=32) :: digits
            character(len=:), allocatable :: text

            result = expression_result_type()
            call evaluate_entry_expression(arguments(1)%text, entry, date, diary_entry, status)
            if (.not. status%ok()) return

            if (diary_entry%applies .and. (.not. diary_marking_entries_flag .or. diary_marking_entry_flag)) then
                ! Diary entry applies on date.
                result = diary_entry
            else if (has_days .and. .not. diary_entry%applies &
                     .and. (.not. diary_marking_entries_flag .or. marking%kind /= VALUE_NONE)) then
                ! Diary entry may apply to `days' before date.
                ! Adjust date, and re-evaluate.
                call evaluate_entry_expression(arguments(1)%text, entry, &
                                               calendar_gregorian_from_absolute(calendar_absolute_from_gregorian(date) + days), &
                                               diary_entry, status)
                if (.not. status%ok()) return
                if (.not. diary_entry%applies) return

                ! Discard any mark portion from diary-anniversary, etc.
                if (mod(days, 7) == 0) then
                    write (digits, "(i0)") days/7
                    text = trim(digits)//" week"
                    if (days /= 7) text = text//"s"
                else
                    write (digits, "(i0)") days
                    text = trim(digits)//" day"
                    if (days /= 1) text = text//"s"
                end if

                result%applies = .true.
                result%has_text = .true.
                result%text = "Reminder: Only "//text//" until "//diary_entry%text
            end if
        end subroutine remind_one

        ! (or (diary-remind expression (car days) marking) (diary-remind expression (cdr
        ! days) marking)) over the list DAYS.
        recursive subroutine remind_list(days, result, status)
            integer, intent(in) :: days(:)
            type(expression_result_type), intent(out) :: result
            type(status_type), intent(out) :: status
            integer :: j

            result = expression_result_type()
            status = status_type()

            do j = 1, size(days)
                call remind_one(days(j), .true., result, status)
                if (.not. status%ok()) return
                if (result%applies) return
            end do
        end subroutine remind_list
    end subroutine diary_remind

    ! ------------------------------------------------------------------
    ! Inserting entries
    ! ------------------------------------------------------------------

    !> Insert a diary entry STRING which may be NONMARKING in FILE.
    !> If omitted, NONMARKING defaults to .false. and FILE defaults to
    !> `diary_file'.  If OMIT_TRAILING_SPACE is true, then do not add
    !> a trailing space to the entry.  If DO_NOT_SHOW is true, do not
    !> show the diary buffer.  Port of diary-lib.el:diary-make-entry.
    recursive subroutine insert_diary_entry_string(string, status, nonmarking, file, omit_trailing_space, do_not_show)
        character(len=*), intent(in) :: string
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: nonmarking, omit_trailing_space, do_not_show
        character(len=*), intent(in), optional :: file
        character(len=:), allocatable :: name, text
        type(handle_type) :: buffer
        integer(int64) :: found, shortage
        logical :: saved_fold, hidden
        type(status_type) :: st

        if (present(file)) then
            name = file
        else
            name = diary_file
        end if

        hidden = .false.
        if (present(do_not_show)) hidden = do_not_show

        if (hidden) then
            call find_file_noselect(name, buffer, status)
            if (.not. status%ok()) return
            call set_buffer(buffer, status)
        else
            call find_file_other_window(name, status)
        end if

        if (.not. status%ok()) return
        call ensure_diary_mode(status)
        if (.not. status%ok()) return
        call widen()
        call unhide_diary_everything(status)
        if (.not. status%ok()) return
        call goto_character(point_maximum())
        saved_fold = case_fold_search%value
        case_fold_search%value = .true.
        call search_backward("Local Variables:", found, st, bound=max(point_maximum() - 3000, point_minimum()), &
                             noerror=SEARCH_NOERROR)
        case_fold_search%value = saved_fold

        if (found > 0) then
            shortage = forward_line(0_int64)
            call insert(achar(10), status)
            if (.not. status%ok()) return
            shortage = forward_line(-1_int64)
        end if

        text = ""
        if (.not. is_beginning_of_line()) text = achar(10)

        if (present(nonmarking)) then
            if (nonmarking) text = text//diary_nonmarking_symbol
        end if

        text = text//string

        if (present(omit_trailing_space)) then
            if (.not. omit_trailing_space) text = text//" "
        else
            text = text//" "
        end if

        call insert(text, status)
    end subroutine insert_diary_entry_string

    ! The string of DATE for a new diary entry: `diary_date_insertion_form'
    ! of `calendar_date_style', or the form `diary_date_display_form' gives
    ! for KIND (ENTRY_MONTHLY, ENTRY_YEARLY, or 0 for the form of an
    ! anniversary, block or cyclic entry: for those, NUMBERS).
    ! Not a port (upstream's calendar-date-string under these forms).
    function diary_date_insertion_string(date, kind, numbers) result(s)
        type(calendar_date_type), intent(in) :: date
        integer, intent(in) :: kind
        logical, intent(in) :: numbers
        character(len=:), allocatable :: s
        character(len=64) :: text

        select case (kind)
        case (ENTRY_MONTHLY)
            select case (calendar_date_style)
            case (CALENDAR_DATE_STYLE_ISO)
                write (text, '("*-*-", i2.2)') date%day
            case (CALENDAR_DATE_STYLE_EUROPEAN)
                write (text, "(i0)") date%day
                text = trim(text)//" * "
            case default
                write (text, '("* ", i0)') date%day
            end select

            s = trim(text)
            if (calendar_date_style == CALENDAR_DATE_STYLE_EUROPEAN) s = s//" "
        case (ENTRY_YEARLY)
            select case (calendar_date_style)
            case (CALENDAR_DATE_STYLE_ISO)
                write (text, '("*-", i2.2, "-", i2.2)') date%month, date%day
                s = trim(text)
            case (CALENDAR_DATE_STYLE_EUROPEAN)
                write (text, "(i0)") date%day
                s = trim(text)//" "//calendar_month_name(date%month, .true.)
            case default
                write (text, "(i0)") date%day
                s = calendar_month_name(date%month, .true.)//" "//trim(text)
            end select
        case default
            if (numbers) then
                ! Iso cannot contain "-", because this form used eg by
                ! insert_anniversary_diary_entry.
                select case (calendar_date_style)
                case (CALENDAR_DATE_STYLE_ISO)
                    write (text, '(i0, " ", i2.2, " ", i2.2)') date%year, date%month, date%day
                case (CALENDAR_DATE_STYLE_EUROPEAN)
                    write (text, '(i0, " ", i0, " ", i0)') date%day, date%month, date%year
                case default
                    write (text, '(i0, " ", i0, " ", i0)') date%month, date%day, date%year
                end select
            else
                select case (calendar_date_style)
                case (CALENDAR_DATE_STYLE_ISO)
                    write (text, '(i0, "-", i2.2, "-", i2.2)') date%year, date%month, date%day
                case (CALENDAR_DATE_STYLE_EUROPEAN)
                    write (text, '(i0, "/", i0, "/", i0)') date%day, date%month, date%year
                case default
                    write (text, '(i0, "/", i0, "/", i0)') date%month, date%day, date%year
                end select
            end if

            s = trim(text)
        end select
    end function diary_date_insertion_string

    !> Insert a diary entry for the date indicated by point.
    !> Prefix argument ARGUMENT makes the entry nonmarking.  Port of
    !> diary-lib.el:diary-insert-entry (no EVENT: the mouse is Deferred).
    recursive subroutine insert_diary_entry(argument, status)
        logical, intent(in) :: argument
        type(status_type), intent(out) :: status
        type(calendar_date_type) :: date

        call calendar_cursor_to_date(date, status, .true.)
        if (.not. status%ok()) return
        call insert_diary_entry_string(diary_date_insertion_string(date, 0, .false.), status, argument)
    end subroutine insert_diary_entry

    ! The command `insert_diary_entry': ARGUMENT is the prefix argument.
    ! Not a port.
    recursive subroutine execute_insert_diary_entry(status)
        type(status_type), intent(out) :: status

        call insert_diary_entry(current_prefix_argument%kind /= PREFIX_NONE, status)
    end subroutine execute_insert_diary_entry

    !> Insert a weekly diary entry for the day of the week indicated by point.
    !> Prefix argument ARGUMENT makes the entry nonmarking.  Port of
    !> diary-lib.el:diary-insert-weekly-entry.
    recursive subroutine insert_weekly_diary_entry(argument, status)
        logical, intent(in) :: argument
        type(status_type), intent(out) :: status
        type(calendar_date_type) :: date

        call calendar_cursor_to_date(date, status, .true.)
        if (.not. status%ok()) return
        call insert_diary_entry_string(calendar_day_name(date), status, argument)
    end subroutine insert_weekly_diary_entry

    ! The command `insert_weekly_diary_entry'.  Not a port.
    recursive subroutine execute_insert_weekly_diary_entry(status)
        type(status_type), intent(out) :: status

        call insert_weekly_diary_entry(current_prefix_argument%kind /= PREFIX_NONE, status)
    end subroutine execute_insert_weekly_diary_entry

    ! Subroutine to insert a diary entry related to the date at point.
    ! KIND is the type of entry (ENTRY_MONTHLY or ENTRY_YEARLY).  NOMARK
    ! true means make the entry non-marking.  Port of
    ! diary-lib.el:diary-insert-entry-1 (the other calendars' MONTHS,
    ! SYMBOL and ABSFUNC are Deferred).
    recursive subroutine insert_diary_entry_1(kind, nomark, status)
        integer, intent(in) :: kind
        logical, intent(in) :: nomark
        type(status_type), intent(out) :: status
        type(calendar_date_type) :: date

        call calendar_cursor_to_date(date, status, .true.)
        if (.not. status%ok()) return
        call insert_diary_entry_string(diary_date_insertion_string(date, kind, .false.), status, nomark)
    end subroutine insert_diary_entry_1

    !> Insert a monthly diary entry for the day of the month indicated by point.
    !> Prefix argument ARGUMENT makes the entry nonmarking.  Port of
    !> diary-lib.el:diary-insert-monthly-entry.
    recursive subroutine insert_monthly_diary_entry(argument, status)
        logical, intent(in) :: argument
        type(status_type), intent(out) :: status

        call insert_diary_entry_1(ENTRY_MONTHLY, argument, status)
    end subroutine insert_monthly_diary_entry

    ! The command `insert_monthly_diary_entry'.  Not a port.
    recursive subroutine execute_insert_monthly_diary_entry(status)
        type(status_type), intent(out) :: status

        call insert_monthly_diary_entry(current_prefix_argument%kind /= PREFIX_NONE, status)
    end subroutine execute_insert_monthly_diary_entry

    !> Insert an annual diary entry for the day of the year indicated by point.
    !> Prefix argument ARGUMENT makes the entry nonmarking.  Port of
    !> diary-lib.el:diary-insert-yearly-entry.
    recursive subroutine insert_yearly_diary_entry(argument, status)
        logical, intent(in) :: argument
        type(status_type), intent(out) :: status

        call insert_diary_entry_1(ENTRY_YEARLY, argument, status)
    end subroutine insert_yearly_diary_entry

    ! The command `insert_yearly_diary_entry'.  Not a port.
    recursive subroutine execute_insert_yearly_diary_entry(status)
        type(status_type), intent(out) :: status

        call insert_yearly_diary_entry(current_prefix_argument%kind /= PREFIX_NONE, status)
    end subroutine execute_insert_yearly_diary_entry

    !> Insert an anniversary diary entry for the date given by point.
    !> Prefix argument ARGUMENT makes the entry nonmarking.  Port of
    !> diary-lib.el:diary-insert-anniversary-entry.
    recursive subroutine insert_anniversary_diary_entry(argument, status)
        logical, intent(in) :: argument
        type(status_type), intent(out) :: status
        type(calendar_date_type) :: date

        call calendar_cursor_to_date(date, status, .true.)
        if (.not. status%ok()) return
        call insert_diary_entry_string(diary_expression_entry_symbol//"(diary-anniversary " &
                                       //diary_date_insertion_string(date, 0, .true.)//")", status, argument)
    end subroutine insert_anniversary_diary_entry

    ! The command `insert_anniversary_diary_entry'.  Not a port.
    recursive subroutine execute_insert_anniversary_diary_entry(status)
        type(status_type), intent(out) :: status

        call insert_anniversary_diary_entry(current_prefix_argument%kind /= PREFIX_NONE, status)
    end subroutine execute_insert_anniversary_diary_entry

    !> Insert a block diary entry for the days between the point and marked date.
    !> Prefix argument ARGUMENT makes the entry nonmarking.  Port of
    !> diary-lib.el:diary-insert-block-entry.
    recursive subroutine insert_block_diary_entry(argument, status)
        logical, intent(in) :: argument
        type(status_type), intent(out) :: status
        type(calendar_date_type) :: cursor, mark, start, end

        call calendar_cursor_to_date(cursor, status, .true.)
        if (.not. status%ok()) return
        mark = calendar_marked_date()

        if (mark%month == 0) then
            call set_error(status, GENERIC_ERROR_CODE, "No mark set in this buffer")
            return
        end if

        if (calendar_absolute_from_gregorian(mark) < calendar_absolute_from_gregorian(cursor)) then
            start = mark
            end = cursor
        else
            start = cursor
            end = mark
        end if

        call insert_diary_entry_string(diary_expression_entry_symbol//"(diary-block " &
                                       //diary_date_insertion_string(start, 0, .true.)//" " &
                                       //diary_date_insertion_string(end, 0, .true.)//")", status, argument)
    end subroutine insert_block_diary_entry

    ! The command `insert_block_diary_entry'.  Not a port.
    recursive subroutine execute_insert_block_diary_entry(status)
        type(status_type), intent(out) :: status

        call insert_block_diary_entry(current_prefix_argument%kind /= PREFIX_NONE, status)
    end subroutine execute_insert_block_diary_entry

    !> Insert a cyclic diary entry starting at the date given by point.
    !> Prefix argument ARGUMENT makes the entry nonmarking.  Port of
    !> diary-lib.el:diary-insert-cyclic-entry.
    recursive subroutine insert_cyclic_diary_entry(argument, status)
        logical, intent(in) :: argument
        type(status_type), intent(out) :: status
        type(calendar_date_type) :: date
        character(len=32) :: digits
        integer :: n

        call calendar_read_expression("Repeat every how many days", is_positive, n, status, 1)
        if (.not. status%ok()) return
        call calendar_cursor_to_date(date, status, .true.)
        if (.not. status%ok()) return
        write (digits, "(i0)") n
        call insert_diary_entry_string(diary_expression_entry_symbol//"(diary-cyclic "//trim(digits)//" " &
                                       //diary_date_insertion_string(date, 0, .true.)//")", status, argument)

    contains

        logical function is_positive(x)
            integer, intent(in) :: x

            is_positive = x > 0
        end function is_positive
    end subroutine insert_cyclic_diary_entry

    ! The command `insert_cyclic_diary_entry'.  Not a port.
    recursive subroutine execute_insert_cyclic_diary_entry(status)
        type(status_type), intent(out) :: status

        call insert_cyclic_diary_entry(current_prefix_argument%kind /= PREFIX_NONE, status)
    end subroutine execute_insert_cyclic_diary_entry

    ! ------------------------------------------------------------------
    ! The modes
    ! ------------------------------------------------------------------

    !> If `calendar_buffer' is live and diary entries are marked, redraw
    !> it.  Port of diary-lib.el:diary-redraw-calendar.
    recursive subroutine redraw_diary_calendar(status)
        type(status_type), intent(out) :: status
        type(excursion_type) :: excursion

        status = status_type()

        if (calendar_mark_diary_entries) then
            call save_excursion_begin(excursion)
            call calendar_redraw(status)
            call save_excursion_end(excursion)
        end if
    end subroutine redraw_diary_calendar

    !> Major mode for editing the diary file.
    !>
    !> \<diary_mode_map>\{diary_mode_map}  Port of diary-lib.el:diary-mode
    !> (its font-lock keywords and its header line are Deferred).
    recursive subroutine diary_mode(status)
        type(status_type), intent(out) :: status
        logical :: saved

        call major_modes%diary_mode%begin(saved, status)
        if (.not. status%ok()) return
        ! (setq-local comment-start diary-comment-start): "" is the
        ! default, no comments; and so for comment-end.
        if (len(diary_comment_start) > 0) call comment_start%set_local(diary_comment_start)
        if (len(diary_comment_end) > 0) call comment_end%set_local(diary_comment_end)
        call add_to_invisibility_specification(DIARY_INVISIBLE, .false.)
        call after_save_hook%prepend(redraw_diary_calendar, local=.true.)
        ! In case the file was modified externally, refresh the calendar
        ! after refreshing the diary buffer.
        call after_revert_hook%prepend(redraw_diary_calendar, local=.true.)
        call major_modes%diary_mode%end(saved, status)
    end subroutine diary_mode

    !> Major mode used while displaying diary entries using Fancy Display.
    !> Port of diary-lib.el:diary-fancy-display-mode (its font-lock
    !> keywords and `diary_fancy_overriding_map' are Deferred).
    recursive subroutine diary_fancy_display_mode(status)
        type(status_type), intent(out) :: status
        logical :: saved

        call major_modes%diary_fancy_display_mode%begin(saved, status)
        if (.not. status%ok()) return
        call minor_modes%view_mode%enable(status)
        if (.not. status%ok()) return
        call major_modes%diary_fancy_display_mode%end(saved, status)
    end subroutine diary_fancy_display_mode

    ! ------------------------------------------------------------------
    ! Definitions
    ! ------------------------------------------------------------------

    !> The diary's options, faces, modes, commands and calendar keys.
    !> Not a port.
    subroutine initialize_diary_library()
        type(named_face_type), pointer :: f
        type(status_type) :: st

        if (.not. allocated(diary_include_string)) diary_include_string = "#include"
        if (.not. allocated(diary_expression_entry_symbol)) diary_expression_entry_symbol = "%%"
        if (.not. allocated(diary_comment_start)) diary_comment_start = ""
        if (.not. allocated(diary_comment_end)) diary_comment_end = ""
        if (.not. allocated(diary_entries)) allocate (diary_entries(0))
        if (.not. allocated(diary_included_files)) allocate (diary_included_files(0))
        entry_functions(1) = entry_function_type("diary-date", 3, 4, diary_date)
        entry_functions(2) = entry_function_type("diary-block", 6, 7, diary_block)
        entry_functions(3) = entry_function_type("diary-float", 3, 5, diary_float)
        entry_functions(4) = entry_function_type("diary-anniversary", 2, 4, diary_anniversary)
        entry_functions(5) = entry_function_type("diary-cyclic", 4, 5, diary_cyclic)
        entry_functions(6) = entry_function_type("diary-offset", 2, 2, diary_offset)
        entry_functions(7) = entry_function_type("diary-remind", 2, 3, diary_remind)
        entry_functions(8) = entry_function_type("diary-day-of-year", 0, 0, diary_day_of_year)
        call copy_syntax_table(diary_syntax_table, st, standard_syntax_table())
        call modify_syntax_entry(iachar("*"), iachar("*"), "w", st, diary_syntax_table)
        call modify_syntax_entry(iachar(":"), iachar(":"), "w", st, diary_syntax_table)
        f => diary_anniversary_face
        call f%define("diary_anniversary", "Face used for anniversaries in the fancy diary display.")
        call f%specification_entry(display_when(), face_attributes(inherit="font_lock_keyword_face"))
        f => diary_time
        call f%define("diary_time", "Face used for times of day in the fancy diary display.")
        call f%specification_entry(display_when(), face_attributes(inherit="font_lock_variable_name_face"))
        f => diary_button
        call f%define("diary_button", "Face used for buttons in the fancy diary display.")
        call initialize_button()
        diary_entry_button%face => diary_button
        diary_entry_button%help_echo = "Find this diary entry"
        diary_entry_button%follow_link = .true.
        diary_entry_button%action => goto_diary_entry
        call diary_entry_button%define("diary_entry")
        call major_modes%diary_mode%define("diary_mode", "Diary", diary_mode, diary_mode_hook)
        diary_mode_map = major_modes%diary_mode%map
        call keymap_set(diary_mode_map, "C-c C-s", "show_all_diary_entries", st)
        call keymap_set(diary_mode_map, "C-c C-q", "quit_window", st)
        call register_keymap_name("diary_mode_map", diary_mode_map)
        call major_modes%diary_fancy_display_mode%define("diary_fancy_display_mode", "Diary", diary_fancy_display_mode, &
                                                         diary_fancy_display_mode_hook, parent=major_modes%special_mode)
        call keymap_set(calendar_mode_map, "m", "mark_diary_entries", st)
        call keymap_set(calendar_mode_map, "d", "view_diary_entries", st)
        call keymap_set(calendar_mode_map, "D", "view_other_diary_entries", st)
        call keymap_set(calendar_mode_map, "s", "show_all_diary_entries", st)
        call keymap_set(calendar_mode_map, "i d", "insert_diary_entry", st)
        call keymap_set(calendar_mode_map, "i w", "insert_weekly_diary_entry", st)
        call keymap_set(calendar_mode_map, "i m", "insert_monthly_diary_entry", st)
        call keymap_set(calendar_mode_map, "i y", "insert_yearly_diary_entry", st)
        call keymap_set(calendar_mode_map, "i a", "insert_anniversary_diary_entry", st)
        call keymap_set(calendar_mode_map, "i b", "insert_block_diary_entry", st)
        call keymap_set(calendar_mode_map, "i c", "insert_cyclic_diary_entry", st)
        calendar_mark_diary_entries_hook => mark_diary_entries_quietly
        calendar_view_diary_entries_hook => view_diary_entries_initially
        commands%diary%execute => execute_diary
        commands%view_diary_entries%execute => execute_view_diary_entries
        commands%view_other_diary_entries%execute => execute_view_other_diary_entries
        commands%show_all_diary_entries%execute => show_all_diary_entries
        commands%mark_diary_entries%execute => execute_mark_diary_entries
        commands%insert_diary_entry%execute => execute_insert_diary_entry
        commands%insert_weekly_diary_entry%execute => execute_insert_weekly_diary_entry
        commands%insert_monthly_diary_entry%execute => execute_insert_monthly_diary_entry
        commands%insert_yearly_diary_entry%execute => execute_insert_yearly_diary_entry
        commands%insert_anniversary_diary_entry%execute => execute_insert_anniversary_diary_entry
        commands%insert_block_diary_entry%execute => execute_insert_block_diary_entry
        commands%insert_cyclic_diary_entry%execute => execute_insert_cyclic_diary_entry
        commands%diary_mode%execute => diary_mode
        commands%diary_fancy_display_mode%execute => diary_fancy_display_mode
    end subroutine initialize_diary_library
end module diary_library_mod
