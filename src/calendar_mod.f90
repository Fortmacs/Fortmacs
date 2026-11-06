! calendar_mod.f90 -- calendar functions.
! Ported from GNU Emacs 31.1 lisp/calendar/calendar.el (in part).
!
! Copyright (C) 1988-1995, 1997, 2000-2026 Free Software Foundation, Inc.
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

!> The calendar: M-x calendar shows three months of the Gregorian
!> calendar side by side in *Calendar*, in Calendar mode, whose keys move
!> the cursor by days, weeks, months and years and scroll the months
!> (calendar_movement_mod, a submodule).  A date is a `calendar_date_type'
!> (upstream's list (MONTH DAY YEAR)); the absolute date counts the days
!> since the imaginary Gregorian date Sunday, December 31, 1 BC.
!>
!> The holidays (holidays.el: a, h, x) are holidays_mod, which sets
!> `calendar_mark_holidays_hook' and `calendar_list_holidays_hook'; the
!> diary (diary-lib.el: d, D, s, m and the i keys) is diary_library_mod,
!> which sets `calendar_mark_diary_entries_hook' and
!> `calendar_view_diary_entries_hook'.  calendar.el's diary options are
!> here: `diary_file', `diary_nonmarking_symbol', `diary_date_forms',
!> `calendar_mark_diary_entries_flag' and the others.
!>
!> Deferred to the next parts of roadmap step 46: `calendar_holiday_marker',
!> `diary_entry_marker', `diary_date_insertion_form' (the date style
!> chooses the insertion form), the other calendars (cal-iso.el,
!> cal-julian.el, cal-hebrew.el, cal-islam.el, cal-bahai.el, cal-china.el,
!> cal-coptic.el, cal-persia.el, cal-french.el, cal-mayan.el: the g and
!> p keys but g d, g D and p d, `calendar_other_dates' and
!> `calendar_print_other_dates' p o), lunar.el and solar.el (M, S),
!> appt.el (A a, A d), cal-html.el and cal-tex.el (H, t), cal-x.el
!> (`calendar_setup' and its frames, `calendar_remove_frame_by_deleting'),
!> cal-menu.el (the menus and the mouse: `calendar_event_buffer', the
!> EVENT arguments, the mode line's mouse maps), and the code upstream
!> runs to compute the date display, month headers, intermonth text,
!> help echo and mode line (`calendar_date_display_form' and its three
!> styles, `calendar_month_header' and its three styles, the computed
!> values of `calendar_intermonth_text', `calendar_intermonth_header'
!> and `calendar_date_echo_text', `calendar_mode_line_format': questions
!> for the owner; the date style chooses what upstream's defaults
!> compute).  Also Deferred: the :set functions of the options (a
!> configuration assigns them before the calendar is first used, as
!> upstream's calendar.el loads after the init file), the string and
!> attribute-list forms of `calendar_today_marker' (a question; today is
!> marked with the `calendar_today' face), `calendar_make_temporary_face',
!> `calendar_get_date_range', the years of `get_calendar_month_visible_year'
!> for N months, the obsolete calendar-nongregorian-visible-p,
!> `calendar_increment_month_cons', `calendar_make_alist',
!> `calendar_abbreviation_construct', `calendar_day_header_construct' and
!> `calendar_customized_p' (used only by the deferred parts), the
!> wide characters of the intermonth text (their `(space :align-to)'
!> display), `scroll_margin' (not ported) and the obsolete
!> `calendar_read'.  Not applicable: `calendar_load_hook' (obsolete; no
!> files are loaded), `calendar_dlet' and `calendar_sum' (macros).
module calendar_mod
    use, intrinsic :: iso_fortran_env, only: int64
    use error_mod, only: status_type, set_error, GENERIC_ERROR_CODE, USER_ERROR_CODE, WRONG_TYPE_ARGUMENT_CODE
    use handle_mod, only: handle_type, null_handle
    use value_mod, only: value_type, make_list, make_keymap_reference, command_procedure
    use buffer_mod, only: buffers, current, current_buffer, get_buffer, set_buffer, get_or_make_buffer, is_buffer_live, &
                          close_buffer, restore_buffer_modified, erase_buffer, buffer_serial_live, force_mode_line_update, &
                          header_line_format_type
    use editing_functions_mod, only: point, point_minimum, point_maximum, goto_character, buffer_substring
    use insert_delete_mod, only: insert
    use editing_commands_mod, only: forward_line
    use indent_mod, only: move_to_column, indent_to, current_column, indent_tabs_mode
    use simple_mod, only: count_lines, mark_ring_maximum, special_mode
    use search_mod, only: looking_at, regular_expression_search_backward, regular_expression_search_forward
    use text_properties_mod, only: put_text_property, get_text_property
    use interval_tree_mod, only: make_overlay, overlay_put, overlay_get, delete_overlay, overlays_in, overlays_at
    use fill_mod, only: string_width
    use multilingual_environment_utilities_mod, only: truncate_string_to_width
    use time_functions_mod, only: decode_time, decoded_time_type
    use hooks_mod, only: hook_type
    use derived_mod, only: is_derived_mode
    use modes_mod, only: major_modes
    use commands_mod, only: commands
    use execute_interactively_mod, only: current_prefix_argument, prefix_numeric_value, PREFIX_NONE
    use keymap_mod, only: keymap_set, lookup_key, current_global_map
    use keyboard_mod, only: this_command_key_events
    use print_mod, only: readable_text
    use help_mod, only: substitute_command_keys, register_keymap_name
    use minibuffer_reading_mod, only: read_from_minibuffer, completion_ignore_case
    use minibuffer_reading_tables_mod, only: completion_table_type, completion_list_table
    use minibuffer_mod, only: completing_read, REQUIRE_MATCH_YES
    use display_engine_mod, only: message
    use info_mod, only: info
    use frame_mod, only: selected_window
    use window_mod, only: windows, window_buffer, pop_to_buffer, switch_to_buffer, select_window, split_window_and_copy, &
                          fit_window_to_buffer, quit_windows_on, get_buffer_window, set_window_point, window_body_width, &
                          window_dedication, is_window_live, is_window_splittable, is_window_combined, &
                          is_window_full_height, DEDICATED_NONE, split_height_threshold, split_width_threshold, &
                          pop_up_windows, display_buffer, window_total_width
    use data_mod, only: append_locals_clearer
    use faces_mod, only: named_face_type, display_when, face_attributes, highlight, FACE_CLASS_COLOR, FACE_BACKGROUND_LIGHT, &
                         FACE_BACKGROUND_DARK
    use configuration_mod, only: data_directory
    use file_input_output_mod, only: expand_file_name
    use buffer_mod, only: get_file_buffer, is_buffer_modified
    use utility_functions_mod, only: read_yes_or_no
    implicit none(type, external)
    private

    public :: calendar_date_type, operator(==)
    public :: calendar, calendar_basic_setup, calendar_generate_window, calendar_generate, calendar_generate_month, &
              calendar_redraw, calendar_mode, calendar_update_mode_line, calendar_exit, calendar_current_date, &
              calendar_column_to_segment, calendar_cursor_to_date, calendar_gregorian_from_absolute, &
              calendar_other_month, calendar_set_mark, calendar_exchange_point_and_mark, calendar_count_days_region, &
              calendar_not_implemented, calendar_read_expression, calendar_read_date, calendar_interval, &
              calendar_day_name, calendar_month_name, calendar_day_of_week, calendar_week_end_day, calendar_unmark, &
              is_calendar_date_visible, calendar_get_month_range, is_calendar_date_valid, &
              calendar_mark_visible_date, calendar_star_date, calendar_mark_today, calendar_date_string, &
              calendar_dayname_on_or_before, calendar_nth_named_absday, calendar_nth_named_day, &
              calendar_day_of_year_string, calendar_print_day_of_year, calendar_set_date_style, &
              calendar_goto_info_node, calendar_recompute_layout_variables, calendar_month_edges, &
              is_calendar_leap_year, calendar_last_day_of_month, calendar_day_number, calendar_absolute_from_gregorian, &
              calendar_date_from_day_of_year, calendar_increment_month, calendar_get_buffer, calendar_buffer_list, &
              calendar_displayed_month, calendar_displayed_year, calendar_total_months_local, calendar_mode_line_text, &
              initialize_calendar, calendar_local_index, has_calendar_date_property, set_calendar_display, &
              restore_calendar_display, get_calendar_month_visible_year, get_calendar_nongregorian_visible_dates, &
              calendar_set_mode_line, calendar_in_read_only_buffer, calendar_get_date_range, calendar_marked_date, &
              ensure_diary_date_forms, diary_file_buffer
    public :: calendar_cursor_to_nearest_date, calendar_cursor_to_visible_date, calendar_goto_today, &
              calendar_forward_month, calendar_forward_year, calendar_backward_month, calendar_backward_year, &
              calendar_scroll_left, calendar_scroll_right, calendar_scroll_calendar_left, &
              calendar_scroll_calendar_right, calendar_recenter, calendar_forward_day, calendar_backward_day, &
              calendar_forward_week, calendar_backward_week, calendar_beginning_of_week, calendar_end_of_week, &
              calendar_beginning_of_month, calendar_end_of_month, calendar_beginning_of_year, calendar_end_of_year, &
              calendar_goto_date, calendar_goto_day_of_year, calendar_show_more_months, calendar_show_fewer_months

    !> A Gregorian date: MONTH (1 to 12), DAY and YEAR (a negative year is
    !> BC, -1 being 1 BC).  `calendar_date_type()', all zero, stands for no
    !> date.  Upstream's list (MONTH DAY YEAR), whose
    !> parts calendar.el:calendar-extract-month, calendar-extract-day and
    !> calendar-extract-year read.
    type :: calendar_date_type
        integer :: month = 0
        integer :: day = 0
        integer :: year = 0
    end type calendar_date_type

    !> Two dates are equal if their month, day and year are.
    interface operator(==)
        module procedure is_calendar_date_equal
    end interface operator(==)

    !> The displayed month and year and the number of months of a
    !> buffer's calendar, as `set_calendar_display' saves them.
    type, public :: calendar_display_type
        private
        integer :: entry = 0
        integer(int64) :: serial = 0
        integer :: month = 0
        integer :: year = 0
        integer :: total_months = 3
    end type calendar_display_type

    !> The longest day or month name the name arrays hold.
    integer, parameter, public :: CALENDAR_NAME_MAXIMUM = 32

    !> Values of `calendar_date_style'.
    integer, parameter, public :: CALENDAR_DATE_STYLE_AMERICAN = 0, CALENDAR_DATE_STYLE_EUROPEAN = 1, &
                                  CALENDAR_DATE_STYLE_ISO = 2

    !> `calendar_split_width_threshold''s value for no value: frames are not
    !> split side by side.
    integer, parameter, public :: CALENDAR_SPLIT_WIDTH_THRESHOLD_NONE = -1

    ! ------------------------------------------------------------------
    ! User options
    ! ------------------------------------------------------------------

    !> The offset of the principal month from the center of the calendar window.
    !> 0 means the principal month is in the center (default), -1 means on the left,
    !> +1 means on the right.  Larger (or smaller) values push the principal month off
    !> the screen.  Port of calendar.el calendar-offset.
    integer, public :: calendar_offset = 0

    !> Minimum height `calendar_generate_window' should use for calendar window.
    !> Port of calendar.el calendar-minimum-window-height.
    integer, public :: calendar_minimum_window_height = 8

    !> Value to use for `split_width_threshold' when creating a calendar.
    !> This only affects frames wider than the default value of
    !> `split_width_threshold'.  CALENDAR_SPLIT_WIDTH_THRESHOLD_NONE (the
    !> default) means no value: the calendar does not split a window side
    !> by side.  Port of calendar.el calendar-split-width-threshold.
    integer, public :: calendar_split_width_threshold = CALENDAR_SPLIT_WIDTH_THRESHOLD_NONE

    !> The day of the week on which a week in the calendar begins.
    !> 0 means Sunday (default), 1 means Monday, and so on.
    !>
    !> If you change this variable directly (without using customize)
    !> after starting `calendar', you should call `calendar_redraw' to
    !> update the calendar display to reflect the change, otherwise
    !> movement commands will not work correctly.  Port of calendar.el
    !> calendar-week-start-day.
    integer, public :: calendar_week_start_day = 0

    !> Days of the week considered weekend days.
    !> 0 means Sunday, 1 means Monday, and so on.
    !>
    !> Determines which day headers are fontified with
    !> `calendar_weekend_header'.  Port of calendar.el calendar-weekend-days.
    integer, allocatable, public :: calendar_weekend_days(:)

    !> List of functions to be called when the calendar window is created.
    !> Quitting the calendar and re-entering it will cause these functions
    !> to be called again.  Port of calendar.el calendar-initial-window-hook.
    type(hook_type), target, public :: calendar_initial_window_hook = hook_type(name="calendar_initial_window_hook")

    !> List of functions called whenever the current date is visible.
    !> To mark today's date, add the function `calendar_mark_today'.
    !> To replace the date with asterisks, add the function `calendar_star_date'.
    !> See also `calendar_today_invisible_hook'.
    !>
    !> In general, be careful about changing characters in the calendar buffer,
    !> since it may cause the movement commands to fail.  Port of
    !> calendar.el calendar-today-visible-hook.
    type(hook_type), target, public :: calendar_today_visible_hook = hook_type(name="calendar_today_visible_hook")

    !> List of functions called whenever the current date is not visible.
    !> See also `calendar_today_visible_hook'.  Port of calendar.el
    !> calendar-today-invisible-hook.
    type(hook_type), target, public :: calendar_today_invisible_hook = hook_type(name="calendar_today_invisible_hook")

    !> List of functions called whenever the cursor moves in the calendar.
    !> For example,
    !>
    !>   call calendar_move_hook%prepend(calendar_update_mode_line)
    !>
    !> updates the mode line for whatever date the cursor is moved to.
    !> Port of calendar.el calendar-move-hook.
    type(hook_type), target, public :: calendar_move_hook = hook_type(name="calendar_move_hook")

    !> Hook run when entering `calendar_mode'.  Port of calendar.el
    !> calendar-mode-hook.
    type(hook_type), target, public :: calendar_mode_hook = hook_type(name="calendar_mode_hook")

    !> True means display holidays for current three month period on entry.
    !> The holidays are displayed in another window when the calendar is first
    !> displayed.  Port of calendar.el calendar-view-holidays-initially-flag.
    logical, public :: calendar_view_holidays_initially_flag = .false.

    !> True means mark dates of holidays in the calendar window.
    !> The dates are marked with the `holiday' face.  Port of calendar.el
    !> calendar-mark-holidays-flag (`calendar_holiday_marker', a face or a
    !> single-character string, is Deferred with `calendar_today_marker').
    logical, public :: calendar_mark_holidays_flag = .false.

    !> If .false., show only major holidays from the Christian calendar.
    !> This means only those Christian holidays that appear on secular calendars.
    !> Otherwise, show all the holidays that would appear in a complete Christian
    !> calendar.  Port of calendar.el calendar-christian-all-holidays-flag.
    logical, public :: calendar_christian_all_holidays_flag = .false.

    !> If .false., show only major holidays from the Islamic calendar.
    !> This means only those Islamic holidays that appear on secular calendars.
    !> Otherwise, show all the holidays that would appear in a complete Islamic
    !> calendar.  Port of calendar.el calendar-islamic-all-holidays-flag.
    logical, public :: calendar_islamic_all_holidays_flag = .false.

    !> True means display current date's diary entries on entry to calendar.
    !> The diary is displayed in another window when the calendar is first displayed,
    !> if the current date is visible.  The number of days of diary entries displayed
    !> is governed by the variable `diary_number_of_entries'.  Port of
    !> calendar.el calendar-view-diary-initially-flag (`calendar_setup' is
    !> Deferred).
    logical, public :: calendar_view_diary_initially_flag = .false.

    !> True means mark dates with diary entries, in the calendar window.
    !> The dates are marked with the `diary' face.  Port of calendar.el
    !> calendar-mark-diary-entries-flag (`diary_entry_marker', a face or a
    !> single-character string, is Deferred).
    logical, public :: calendar_mark_diary_entries_flag = .false.

    !> True means include holidays in the diary display.
    !> The holidays appear in the mode line of the diary buffer, or in the
    !> fancy diary buffer next to the date.  This slows down the diary functions
    !> somewhat; setting it to .false. makes the diary display faster.  Port
    !> of calendar.el diary-show-holidays-flag.
    logical, public :: diary_show_holidays_flag = .true.

    !> String displayed when the cursor is over a date in the calendar.
    !>
    !> Changing this variable without using customize has no effect on
    !> pre-existing calendar windows.  Port of calendar.el
    !> calendar-date-echo-text (a fixed string; a computed text is Deferred).
    character(len=:), allocatable, public :: calendar_date_echo_text

    !> Empty space to the left of the first month in the calendar.  Port of
    !> calendar.el calendar-left-margin.
    integer, public :: calendar_left_margin = 5

    !> Space between months in the calendar.  Minimum value is 1.  Port of
    !> calendar.el calendar-intermonth-spacing.
    integer, public :: calendar_intermonth_spacing = 4

    !> Width of each day column in the calendar.  Minimum value is 3.
    !> Port of calendar.el calendar-column-width.
    integer, public :: calendar_column_width = 3

    !> Width of the day column headers in the calendar.
    !> Must be at least one less than `calendar_column_width'.  Port of
    !> calendar.el calendar-day-header-width.
    integer, public :: calendar_day_header_width = 2

    !> Width of the day digits in the calendar.  Minimum value is 2.
    !> Port of calendar.el calendar-day-digit-width.
    integer, public :: calendar_day_digit_width = 2

    !> Header text to display in the space to the left of each calendar month
    !> ("" for nothing).  See `calendar_intermonth_text'.  Port of
    !> calendar.el calendar-intermonth-header (a fixed string; a computed text
    !> is Deferred).
    character(len=:), allocatable, public :: calendar_intermonth_header

    !> Text to display in the space to the left of each calendar month
    !> ("" for nothing).
    !> Will be truncated to the smaller of `calendar_left_margin' and
    !> `calendar_intermonth_spacing'.  The last character is forced to be a space.
    !>
    !> See also `calendar_intermonth_header'.  Port of calendar.el
    !> calendar-intermonth-text (a fixed string; a computed text is Deferred).
    character(len=:), allocatable, public :: calendar_intermonth_text

    !> Your preferred style for writing dates.
    !> The options are:
    !> CALENDAR_DATE_STYLE_AMERICAN - month/day/year
    !> CALENDAR_DATE_STYLE_EUROPEAN - day/month/year
    !> CALENDAR_DATE_STYLE_ISO      - year/month/day
    !> This affects how dates written in your diary are interpreted.
    !> It also affects date display, as well as those calendar and diary
    !> functions that take a date as an argument, e.g. the diary-date
    !> entries, by changing the order in which the arguments are
    !> interpreted.
    !>
    !> Setting this variable directly after the calendar or the diary
    !> was first used does not change `diary_date_forms'; use
    !> `calendar_set_date_style'.  Port of calendar.el calendar-date-style.
    integer, public :: calendar_date_style = CALENDAR_DATE_STYLE_AMERICAN

    !> Array of capitalized strings giving, in order from Sunday, the day names.
    !> If you change this without using customize after the calendar has loaded,
    !> then you may also want to change `calendar_day_abbreviations'
    !> and `calendar_day_headers'.  Port of calendar.el
    !> calendar-day-name-array.
    character(len=CALENDAR_NAME_MAXIMUM), public :: calendar_day_names(0:6) = [character(len=CALENDAR_NAME_MAXIMUM) :: &
                                                                               "Sunday", "Monday", "Tuesday", "Wednesday", &
                                                                               "Thursday", "Friday", "Saturday"]

    !> Default length of abbreviations to use for day and month names.
    !> If you change this without using customize after the calendar has loaded,
    !> then you may also want to change `calendar_day_abbreviations' and
    !> `calendar_month_abbreviations'.  Port of calendar.el calendar-abbrev-length.
    integer, public :: calendar_abbreviation_length = 3

    !> Array of capitalized strings giving the abbreviated day names.
    !> The order should be the same as that of the full names specified
    !> in `calendar_day_names'.  These abbreviations may be used
    !> instead of the full names in the diary file.  Do not include a
    !> trailing `.' in the strings specified in this variable, though
    !> you may use such in the diary file.  By default, each string is
    !> the first `calendar_abbreviation_length' characters of the corresponding
    !> full name.  Port of calendar.el calendar-day-abbrev-array.
    character(len=CALENDAR_NAME_MAXIMUM), public :: calendar_day_abbreviations(0:6) = [character(len=CALENDAR_NAME_MAXIMUM) :: &
                                                                                       "Sun", "Mon", "Tue", "Wed", "Thu", "Fri", &
                                                                                       "Sat"]

    !> Array of strings to use for the headers of the calendar's day columns.
    !> The order should be the same as in `calendar_day_names'.
    !> In use, the calendar truncates elements to no more than
    !> `calendar_day_header_width' columns wide.
    !> Emacs constructs the default from either `calendar_day_names'
    !> (if `calendar_day_header_width' is more than `calendar_abbreviation_length'),
    !> or from `calendar_day_abbreviations' (assuming that the abbreviated
    !> name are more likely to be unique when truncated).  Port of
    !> calendar.el calendar-day-header-array.
    character(len=CALENDAR_NAME_MAXIMUM), public :: calendar_day_headers(0:6) = [character(len=CALENDAR_NAME_MAXIMUM) :: &
                                                                                 "Su", "Mo", "Tu", "We", "Th", "Fr", "Sa"]

    !> Array of capitalized strings giving, in order, the month names.
    !> If you change this without using customize after the calendar has loaded,
    !> then you may also want to change `calendar_month_abbreviations'.  Port of
    !> calendar.el calendar-month-name-array.
    character(len=CALENDAR_NAME_MAXIMUM), public :: calendar_month_names(12) = [character(len=CALENDAR_NAME_MAXIMUM) :: &
                                                                                "January", "February", "March", "April", "May", &
                                                                                "June", "July", "August", "September", &
                                                                                "October", "November", "December"]

    !> Array of capitalized strings giving the abbreviated month names.
    !> The order should be the same as that of the full names specified
    !> in `calendar_month_names'.  These abbreviations are used in
    !> the calendar menu entries, and can also be used in the diary
    !> file.  Do not include a trailing `.' in the strings specified in
    !> this variable, though you may use such in the diary file.  By
    !> default, each string is the first `calendar_abbreviation_length'
    !> characters of the corresponding full name.  Port of calendar.el
    !> calendar-month-abbrev-array.
    character(len=CALENDAR_NAME_MAXIMUM), public :: calendar_month_abbreviations(12) = [character(len=CALENDAR_NAME_MAXIMUM) :: &
                                                                                        "Jan", "Feb", "Mar", "Apr", "May", "Jun", &
                                                                                        "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]

    ! ------------------------------------------------------------------
    ! Faces
    ! ------------------------------------------------------------------

    !> Face for indicating today's date in the calendar.  Port of
    !> calendar.el calendar-today.
    type(named_face_type), target, public :: calendar_today

    !> Face for highlighting diary entries.  Port of calendar.el diary.
    type(named_face_type), target, public :: diary

    !> Face for indicating in the calendar dates that have holidays.
    !> Port of calendar.el holiday.
    type(named_face_type), target, public :: holiday

    !> Face used for weekday column headers in the calendar.  Port of
    !> calendar.el calendar-weekday-header.
    type(named_face_type), target, public :: calendar_weekday_header

    !> Face used for weekend column headers in the calendar.  Port of
    !> calendar.el calendar-weekend-header.
    type(named_face_type), target, public :: calendar_weekend_header

    !> Face used for month headers in the calendar.  Port of calendar.el
    !> calendar-month-header.
    type(named_face_type), target, public :: calendar_month_header

    ! ------------------------------------------------------------------
    ! Variables
    ! ------------------------------------------------------------------

    !> Number of months displayed in the calendar.
    !> It is made buffer-local automatically in calendar-mode buffers.
    !> Port of calendar.el calendar-total-months.
    integer, public :: calendar_total_months = 3

    !> Width of the region with numbers in each month in the calendar.
    !> Port of calendar.el calendar-month-digit-width.
    integer, public :: calendar_month_digit_width = 0

    !> Full width of each month in the calendar.  Port of calendar.el
    !> calendar-month-width.
    integer, public :: calendar_month_width = 0

    !> First row in the calendar with actual dates.  Port of calendar.el
    !> calendar-first-date-row.
    integer, parameter, public :: calendar_first_date_row = 3

    !> Name of the buffer used for the calendar.  Port of calendar.el
    !> calendar-buffer.
    character(len=64), public :: calendar_buffer = "*Calendar*"

    !> Name of the buffer used for the displaying the holidays.  Port of
    !> calendar.el holiday-buffer.
    character(len=*), parameter, public :: holiday_buffer = "*Holidays*"

    !> Variable version of the user option `calendar_mark_holidays_flag'.
    !> Port of calendar.el calendar-mark-holidays (renamed: the function
    !> `calendar_mark_holidays' has its name).
    logical, public :: calendar_mark_holidays_variable = .false.

    !> `calendar_mark_holidays', which holidays_mod sets (calendar_mod
    !> cannot use it).  Not a port.
    procedure(command_procedure), pointer, public :: calendar_mark_holidays_hook => null()

    !> `calendar_list_holidays', which holidays_mod sets (calendar_mod
    !> cannot use it).  Not a port.
    procedure(command_procedure), pointer, public :: calendar_list_holidays_hook => null()

    !> Variable version of the user option `calendar_mark_diary_entries_flag'.
    !> Port of calendar.el calendar-mark-diary-entries.
    logical, public :: calendar_mark_diary_entries = .false.

    !> `mark_diary_entries' (with REDRAW .false.), which diary_library_mod
    !> sets (calendar_mod cannot use it).  Not a port.
    procedure(command_procedure), pointer, public :: calendar_mark_diary_entries_hook => null()

    !> `view_diary_entries' for one day, which diary_library_mod sets
    !> (calendar_mod cannot use it).  Not a port.
    procedure(command_procedure), pointer, public :: calendar_view_diary_entries_hook => null()

    !> The longest part of a pseudo-pattern of `diary_date_forms'.
    integer, parameter, public :: DIARY_DATE_FORM_PART_MAXIMUM = 80

    !> The most parts of a pseudo-pattern of `diary_date_forms'.
    integer, parameter, public :: DIARY_DATE_FORM_PARTS_MAXIMUM = 8

    !> A pseudo-pattern of `diary_date_forms': its COUNT PARTS in order, each
    !> a regular expression or one of the keywords month, day, year,
    !> monthname and dayname, and whether the date recognizer backs up to
    !> the beginning of the current word (BACKUP, upstream's leading
    !> `backup').
    type, public :: diary_date_form_type
        logical :: backup = .false.
        integer :: count = 0
        character(len=DIARY_DATE_FORM_PART_MAXIMUM) :: parts(DIARY_DATE_FORM_PARTS_MAXIMUM) = ""
    end type diary_date_form_type

    !> A list of pseudo-patterns describing the forms of date used in the
    !> diary: its ENTRIES in order.
    type, public :: diary_date_forms_type
        type(diary_date_form_type), allocatable :: entries(:)
    contains
        !> Add a pseudo-pattern at the end: its parts (a regular expression
        !> or a keyword each), then BACKUP:
        !> `call diary_date_forms%append("month", "/", "day", "[^/0-9]")'.
        procedure :: append => append_diary_date_form
        !> Remove all the pseudo-patterns.
        procedure :: clear => clear_diary_date_forms
    end type diary_date_forms_type

    !> Name of the file in which one's personal diary of dates is kept.
    !>
    !> The file's entries are lines beginning with any of the forms
    !> specified by the variable `diary_date_forms', which by default
    !> uses the forms of `diary_american_date_forms':
    !>
    !>             MONTH/DAY
    !>             MONTH/DAY/YEAR
    !>             MONTHNAME DAY
    !>             MONTHNAME DAY, YEAR
    !>             DAYNAME
    !>
    !> with the remainder of the line being the diary entry string for
    !> that date.  MONTH and DAY are one or two digit numbers, YEAR is a
    !> number and may be written in full or abbreviated to the final two
    !> digits (if `diary_abbreviated_year_flag' is .true.).  MONTHNAME
    !> and DAYNAME can be spelled in full (as specified by the variables
    !> `calendar_month_names' and `calendar_day_names'), or
    !> abbreviated (as specified by `calendar_month_abbreviations' and
    !> `calendar_day_abbreviations') with or without a period.  Case is
    !> ignored.  Any of DAY, MONTH, or MONTHNAME, YEAR can be `*' which
    !> matches any day, month, or year, respectively.  If the date does
    !> not contain a year, it is generic and applies to any year.  A
    !> DAYNAME entry applies to the appropriate day of the week in every week.
    !>
    !> You can customize `diary_date_forms' to your preferred format.
    !> Three default styles are provided: `diary_american_date_forms',
    !> `diary_european_date_forms', and `diary_iso_date_forms'.
    !> You can choose between these by setting `calendar_date_style' in your
    !> init file, or by using `calendar_set_date_style' when in the calendar.
    !>
    !> A diary entry can be preceded by the character `diary_nonmarking_symbol'
    !> (ordinarily `&') to make that entry nonmarking--that is, it will not be
    !> marked on dates in the calendar window but will appear in a diary window.
    !>
    !> Multiline diary entries are made by indenting lines after the first with
    !> either a TAB or one or more spaces.
    !>
    !> Lines not in one the above formats are ignored.  Here are some sample diary
    !> entries (in the default American style):
    !>
    !>      12/22/1988 Twentieth wedding anniversary!!
    !>      &1/1. Happy New Year!
    !>      10/22 Ruth's birthday.
    !>      21: Payday
    !>      Tuesday--weekly meeting with grad students at 10am
    !>               Supowit, Shen, Bitner, and Kapoor to attend.
    !>      1/13/89 Friday the thirteenth!!
    !>      &thu 4pm squash game with Lloyd.
    !>      mar 16 Dad's birthday
    !>      April 15, 1989 Income tax due.
    !>      &* 15 time cards due.
    !>
    !> If the first line of a diary entry consists only of the date or day name with
    !> no trailing blanks or punctuation, then that line is not displayed in the
    !> diary window; only the continuation lines is shown.  For example, the
    !> single diary entry
    !>
    !>      02/11/1989
    !>       Bill Blattner visits Princeton today
    !>       2pm Cognitive Studies Committee meeting
    !>       2:30-5:30 Lizzie at Lawrenceville for `Group Initiative'
    !>       4:00pm Jamie Tappenden
    !>       7:30pm Dinner at George and Ed's for Alan Ryan
    !>       7:30-10:00pm dance at Stewart Country Day School
    !>
    !> will appear in the diary window without the date line at the beginning.  This
    !> facility allows the diary window to look neater, but can cause confusion if
    !> used with more than one day's entries displayed.
    !>
    !> Diary entries can be based on expressions.  For example, the diary entry
    !>
    !>       %%(diary-block 11 1 1990 11 10 1990) Vacation
    !>
    !> causes the diary entry "Vacation" to appear from November 1 through
    !> November 10, 1990.  See the documentation for the function
    !> `list_diary_expression_entries' for more details.
    !>
    !> Diary files can contain directives to include the contents of other files; for
    !> details, see the documentation for the variable `diary_list_entries_hook'.
    !> Port of calendar.el diary-file (in the data directory,
    !> `~/.local/share/fortmacs/diary'; the entries of the other calendars,
    !> with `diary_nongregorian_listing_hook', are Deferred).
    character(len=:), allocatable, public :: diary_file

    !> Symbol indicating that a diary entry is not to be marked in the calendar.
    !> Port of calendar.el diary-nonmarking-symbol.
    character(len=:), allocatable, public :: diary_nonmarking_symbol

    !> List of pseudo-patterns describing the ISO style of dates.
    !> The defaults are: MONTH[-/]DAY; YEAR[-/]MONTH[-/]DAY; MONTHNAME DAY;
    !> YEAR MONTHNAME DAY; DAYNAME.  Normally you should not customize this,
    !> but `diary_date_forms' (which see).  Port of calendar.el
    !> diary-iso-date-forms.
    type(diary_date_forms_type), public :: diary_iso_date_forms

    !> List of pseudo-patterns describing the American style of dates.
    !> The defaults are: MONTH/DAY; MONTH/DAY/YEAR; MONTHNAME DAY;
    !> MONTHNAME DAY, YEAR; DAYNAME.  Normally you should not customize this,
    !> but `diary_date_forms' (which see).  Port of calendar.el
    !> diary-american-date-forms.
    type(diary_date_forms_type), public :: diary_american_date_forms

    !> List of pseudo-patterns describing the European style of dates.
    !> The defaults are: DAY/MONTH; DAY/MONTH/YEAR; DAY MONTHNAME;
    !> DAY MONTHNAME YEAR; DAYNAME.  Normally you should not customize this, but
    !> `diary_date_forms' (which see).  Port of calendar.el
    !> diary-european-date-forms.
    type(diary_date_forms_type), public :: diary_european_date_forms

    !> List of pseudo-patterns describing the forms of date used in the diary.
    !> The patterns on the list must be MUTUALLY EXCLUSIVE and should not match
    !> any portion of the diary entry itself, just the date component.
    !>
    !> A pseudo-pattern is a list of regular expressions and the keywords month,
    !> day, year, monthname, and dayname (a part that is exactly one of these
    !> words).  The keyword monthname will
    !> match the name of the month (see `calendar_month_names'), capitalized
    !> or not, or its user-specified abbreviation (see `calendar_month_abbreviations'),
    !> followed by a period or not; it will also match `*'.  Similarly, dayname
    !> will match the name of the day (see `calendar_day_names'), capitalized or
    !> not, or its user-specified abbreviation (see `calendar_day_abbreviations'),
    !> followed by a period or not.  The keywords month, day, and year will
    !> match those numerical values, preceded by arbitrarily many zeros; they will
    !> also match `*'.
    !>
    !> The matching of the diary entries with the date forms is done with the
    !> standard syntax table from Fundamental mode, but with the `*' changed so
    !> that it is a word constituent.
    !>
    !> If, to be mutually exclusive, a pseudo-pattern must match a portion of the
    !> diary entry itself, its BACKUP must be .true.  This
    !> directive causes the date recognizer to back up to the beginning of the
    !> current word of the diary entry, so in no case can the pattern match more than
    !> a portion of the first word of the diary entry.
    !>
    !> For examples of three common styles, see `diary_american_date_forms',
    !> `diary_european_date_forms', and `diary_iso_date_forms'.  For example,
    !>
    !>     call diary_date_forms%clear()
    !>     call diary_date_forms%append("month", "/", "day", "[^/0-9]")
    !>     call diary_date_forms%append("dayname", "\W")
    !>
    !> Port of calendar.el diary-date-forms (the forms of
    !> `calendar_date_style' when first used, unless the init file set it,
    !> as upstream's calendar.el is loaded after the init file).
    type(diary_date_forms_type), public :: diary_date_forms

    !> Name of the buffer used for the optional fancy display of the diary.
    !> Port of calendar.el diary-fancy-buffer.
    character(len=*), parameter, public :: diary_fancy_buffer = "*Fancy Diary Entries*"

    !> Name of the buffer used for the display of date on other calendars.
    !> Port of calendar.el calendar-other-calendars-buffer.
    character(len=*), parameter, public :: calendar_other_calendars_buffer = "*Other Calendars*"

    !> Name of the buffer used for the lunar phases.  Port of calendar.el
    !> lunar-phases-buffer.
    character(len=*), parameter, public :: lunar_phases_buffer = "*Phases of Moon*"

    !> Name of buffer used for sunrise/sunset times.  Port of calendar.el
    !> solar-sunrises-buffer.
    character(len=*), parameter, public :: solar_sunrises_buffer = "*Sunrise/Sunset Times*"

    !> Name of the buffer used by `calendar_hebrew_list_yahrzeits'.  Port of
    !> calendar.el calendar-hebrew-yahrzeit-buffer.
    character(len=*), parameter, public :: calendar_hebrew_yahrzeit_buffer = "*Yahrzeits*"

    !> Keymap for `calendar_mode'.  Port of calendar.el calendar-mode-map.
    type(handle_type), public :: calendar_mode_map

    !> Values of `calendar_recenter_last_op'.
    integer, parameter, public :: CALENDAR_RECENTER_LAST_OP_NONE = 0, CALENDAR_RECENTER_LAST_OP_CENTER = 1, &
                                  CALENDAR_RECENTER_LAST_OP_FIRST = 2, CALENDAR_RECENTER_LAST_OP_LAST = 3

    !> Last calendar recenter operation performed.  Port of cal-move.el
    !> calendar-recenter-last-op.
    integer, public :: calendar_recenter_last_op = CALENDAR_RECENTER_LAST_OP_NONE

    ! The month edge columns of month segment SEGMENT: LEFT is the
    ! leftmost column associated with it, FIRST and LAST are the first and
    ! last columns with day digits in, RIGHT is the rightmost column (an
    ! element of upstream's calendar-month-edges).
    type :: month_edge_type
        integer :: segment = 0
        integer :: left = 0, first = 0, last = 0, right = 0
    end type month_edge_type

    ! The buffer-local variables of a calendar-mode buffer (SERIAL is the
    ! buffer's serial number; the entry of serial 0 holds the default
    ! values): calendar-total-months, calendar-right-margin,
    ! calendar-month-edges, calendar-mark-ring, displayed-month and
    ! displayed-year.
    type :: local_type
        integer(int64) :: serial = 0
        integer :: total_months = 3
        integer :: right_margin = 0
        type(month_edge_type), allocatable :: month_edges(:)
        type(calendar_date_type), allocatable :: mark_ring(:)
        integer :: displayed_month = 0
        integer :: displayed_year = 0
    end type local_type

    type(local_type), allocatable :: locals(:)

    ! A string of an array of strings of different lengths.
    type :: string_type
        character(len=:), allocatable :: text
    end type string_type

    ! The key descriptions of the mode line's entries, made when the
    ! calendar is first used (upstream makes them when calendar.el loads).
    character(len=:), allocatable :: mode_line_keys

    ! Whether calendar.el's top-level forms ran: the layout variables are
    ! computed and the mode line's keys described when the calendar is
    ! first used, as upstream's calendar.el is loaded then.
    logical :: loaded = .false.

    ! Text property names.
    character(len=*), parameter :: DATE_PROPERTY = "date", FONT_LOCK_FACE_PROPERTY = "font_lock_face", &
                                   MOUSE_FACE_PROPERTY = "mouse_face", HELP_ECHO_PROPERTY = "help_echo"

    interface
        !> Move the cursor to the closest date.
        !> The position of the cursor is unchanged if it is already on a date.
        !> Returns the list (month day year) giving the cursor position.
        !> Port of cal-move.el:calendar-cursor-to-nearest-date.
        recursive module subroutine calendar_cursor_to_nearest_date(date, status)
            implicit none(type, external)
            type(calendar_date_type), intent(out) :: date
            type(status_type), intent(out) :: status
        end subroutine calendar_cursor_to_nearest_date

        !> Move the cursor to DATE that is on the screen.  Port of
        !> cal-move.el:calendar-cursor-to-visible-date.
        recursive module subroutine calendar_cursor_to_visible_date(date)
            implicit none(type, external)
            type(calendar_date_type), intent(in) :: date
        end subroutine calendar_cursor_to_visible_date

        !> Reposition the calendar window so the current date is visible.
        !> Port of cal-move.el:calendar-goto-today.
        recursive module subroutine calendar_goto_today(status)
            implicit none(type, external)
            type(status_type), intent(out) :: status
        end subroutine calendar_goto_today

        !> Move the cursor forward ARGUMENT months.
        !> Movement is backward if ARGUMENT is negative.  Port of
        !> cal-move.el:calendar-forward-month.
        recursive module subroutine calendar_forward_month(argument, status)
            implicit none(type, external)
            integer, intent(in) :: argument
            type(status_type), intent(out) :: status
        end subroutine calendar_forward_month

        !> Move the cursor forward by ARGUMENT years.
        !> Movement is backward if ARGUMENT is negative.  Port of
        !> cal-move.el:calendar-forward-year.
        recursive module subroutine calendar_forward_year(argument, status)
            implicit none(type, external)
            integer, intent(in) :: argument
            type(status_type), intent(out) :: status
        end subroutine calendar_forward_year

        !> Move the cursor backward by ARGUMENT months.
        !> Movement is forward if ARGUMENT is negative.  Port of
        !> cal-move.el:calendar-backward-month.
        recursive module subroutine calendar_backward_month(argument, status)
            implicit none(type, external)
            integer, intent(in) :: argument
            type(status_type), intent(out) :: status
        end subroutine calendar_backward_month

        !> Move the cursor backward ARGUMENT years.
        !> Movement is forward is ARGUMENT is negative.  Port of
        !> cal-move.el:calendar-backward-year.
        recursive module subroutine calendar_backward_year(argument, status)
            implicit none(type, external)
            integer, intent(in) :: argument
            type(status_type), intent(out) :: status
        end subroutine calendar_backward_year

        !> Scroll the displayed calendar left by ARGUMENT months.
        !> If ARGUMENT is negative the calendar is scrolled right.  Maintains the relative
        !> position of the cursor with respect to the calendar as well as possible.
        !> Port of cal-move.el:calendar-scroll-left (no EVENT).
        recursive module subroutine calendar_scroll_left(argument, status)
            implicit none(type, external)
            integer, intent(in) :: argument
            type(status_type), intent(out) :: status
        end subroutine calendar_scroll_left

        !> Scroll the displayed calendar window right by ARGUMENT months.
        !> If ARGUMENT is negative the calendar is scrolled left.  Maintains the relative
        !> position of the cursor with respect to the calendar as well as possible.
        !> Port of cal-move.el:calendar-scroll-right (no EVENT).
        recursive module subroutine calendar_scroll_right(argument, status)
            implicit none(type, external)
            integer, intent(in) :: argument
            type(status_type), intent(out) :: status
        end subroutine calendar_scroll_right

        !> Scroll the displayed calendar window left ARGUMENT times.
        !> If ARGUMENT is negative the calendar is scrolled right.  Maintains the relative
        !> position of the cursor with respect to the calendar as well as possible.
        !> Port of cal-move.el:calendar-scroll-calendar-left (no EVENT).
        recursive module subroutine calendar_scroll_calendar_left(argument, status)
            implicit none(type, external)
            integer, intent(in) :: argument
            type(status_type), intent(out) :: status
        end subroutine calendar_scroll_calendar_left

        !> Scroll the displayed calendar window right ARGUMENT times.
        !> If ARGUMENT is negative the calendar is scrolled left.  Maintains the relative
        !> position of the cursor with respect to the calendar as well as possible.
        !> Port of cal-move.el:calendar-scroll-calendar-right (no EVENT).
        recursive module subroutine calendar_scroll_calendar_right(argument, status)
            implicit none(type, external)
            integer, intent(in) :: argument
            type(status_type), intent(out) :: status
        end subroutine calendar_scroll_calendar_right

        !> Scroll the calendar so that the month of the date at point is centered.
        !> Next invocation puts this month on the leftmost position, and another
        !> invocation puts this month on the rightmost position.  Subsequent
        !> invocations reuse the same order in a cyclical manner.  Port of
        !> cal-move.el:calendar-recenter.
        recursive module subroutine calendar_recenter(status)
            implicit none(type, external)
            type(status_type), intent(out) :: status
        end subroutine calendar_recenter

        !> Move the cursor forward ARGUMENT days.
        !> Moves backward if ARGUMENT is negative.  Port of
        !> cal-move.el:calendar-forward-day.
        recursive module subroutine calendar_forward_day(argument, status)
            implicit none(type, external)
            integer, intent(in) :: argument
            type(status_type), intent(out) :: status
        end subroutine calendar_forward_day

        !> Move the cursor back ARGUMENT days.
        !> Moves forward if ARGUMENT is negative.  Port of
        !> cal-move.el:calendar-backward-day.
        recursive module subroutine calendar_backward_day(argument, status)
            implicit none(type, external)
            integer, intent(in) :: argument
            type(status_type), intent(out) :: status
        end subroutine calendar_backward_day

        !> Move the cursor forward ARGUMENT weeks.
        !> Moves backward if ARGUMENT is negative.  Port of
        !> cal-move.el:calendar-forward-week.
        recursive module subroutine calendar_forward_week(argument, status)
            implicit none(type, external)
            integer, intent(in) :: argument
            type(status_type), intent(out) :: status
        end subroutine calendar_forward_week

        !> Move the cursor back ARGUMENT weeks.
        !> Moves forward if ARGUMENT is negative.  Port of
        !> cal-move.el:calendar-backward-week.
        recursive module subroutine calendar_backward_week(argument, status)
            implicit none(type, external)
            integer, intent(in) :: argument
            type(status_type), intent(out) :: status
        end subroutine calendar_backward_week

        !> Move the cursor back ARGUMENT `calendar_week_start_day''s.  Port of
        !> cal-move.el:calendar-beginning-of-week.
        recursive module subroutine calendar_beginning_of_week(argument, status)
            implicit none(type, external)
            integer, intent(in) :: argument
            type(status_type), intent(out) :: status
        end subroutine calendar_beginning_of_week

        !> Move the cursor forward ARGUMENT `calendar_week_start_day'+6's.
        !> Port of cal-move.el:calendar-end-of-week.
        recursive module subroutine calendar_end_of_week(argument, status)
            implicit none(type, external)
            integer, intent(in) :: argument
            type(status_type), intent(out) :: status
        end subroutine calendar_end_of_week

        !> Move the cursor backward ARGUMENT month beginnings.  Port of
        !> cal-move.el:calendar-beginning-of-month.
        recursive module subroutine calendar_beginning_of_month(argument, status)
            implicit none(type, external)
            integer, intent(in) :: argument
            type(status_type), intent(out) :: status
        end subroutine calendar_beginning_of_month

        !> Move the cursor forward ARGUMENT month ends.  Port of
        !> cal-move.el:calendar-end-of-month.
        recursive module subroutine calendar_end_of_month(argument, status)
            implicit none(type, external)
            integer, intent(in) :: argument
            type(status_type), intent(out) :: status
        end subroutine calendar_end_of_month

        !> Move the cursor backward ARGUMENT year beginnings.  Port of
        !> cal-move.el:calendar-beginning-of-year.
        recursive module subroutine calendar_beginning_of_year(argument, status)
            implicit none(type, external)
            integer, intent(in) :: argument
            type(status_type), intent(out) :: status
        end subroutine calendar_beginning_of_year

        !> Move the cursor forward ARGUMENT year beginnings.  Port of
        !> cal-move.el:calendar-end-of-year.
        recursive module subroutine calendar_end_of_year(argument, status)
            implicit none(type, external)
            integer, intent(in) :: argument
            type(status_type), intent(out) :: status
        end subroutine calendar_end_of_year

        !> Move cursor to DATE.  Port of cal-move.el:calendar-goto-date.
        recursive module subroutine calendar_goto_date(date, status)
            implicit none(type, external)
            type(calendar_date_type), intent(in) :: date
            type(status_type), intent(out) :: status
        end subroutine calendar_goto_date

        !> Move cursor to YEAR, DAY number; echo DAY/YEAR unless NOECHO is true.
        !> Negative DAY counts backward from end of year.
        !> Interactively, prompt for YEAR and DAY number.  Port of
        !> cal-move.el:calendar-goto-day-of-year.
        recursive module subroutine calendar_goto_day_of_year(year, day, status, noecho)
            implicit none(type, external)
            integer, intent(in) :: year, day
            type(status_type), intent(out) :: status
            logical, intent(in), optional :: noecho
        end subroutine calendar_goto_day_of_year

        !> Show ARGUMENT more months on the right in the calendar.
        !> The calendar shows at most 12 months and at least 3 months.
        !> Port of cal-move.el:calendar-show-more-months.
        recursive module subroutine calendar_show_more_months(argument, status)
            implicit none(type, external)
            integer, intent(in) :: argument
            type(status_type), intent(out) :: status
        end subroutine calendar_show_more_months

        !> Show ARGUMENT fewer months on the right in the calendar.
        !> If the date corresponding to current cursor position is beyond the new
        !> calendar range, scroll the calendar left so that the date remains in the
        !> view.  Port of cal-move.el:calendar-show-fewer-months.
        recursive module subroutine calendar_show_fewer_months(argument, status)
            implicit none(type, external)
            integer, intent(in) :: argument
            type(status_type), intent(out) :: status
        end subroutine calendar_show_fewer_months

        !> Register the commands of cal-move.el.  Not a port.
        module subroutine initialize_calendar_movement()
            implicit none(type, external)
        end subroutine initialize_calendar_movement
    end interface

contains

    ! ------------------------------------------------------------------
    ! Buffer-local variables
    ! ------------------------------------------------------------------

    !> The index of the current buffer's entry in the calendar's
    !> buffer-local values, or 1 (the default values) if it has none.
    !> Public only for calendar_movement_mod (gfortran does not emit a private
    !> procedure only a submodule calls); not part of the config API.
    !> Not a port.
    integer function calendar_local_index() result(k)
        call ensure_locals()
        k = 0
        if (current /= 0) k = findloc(locals%serial, buffers(current)%serial, dim=1)
        if (k == 0) k = 1
    end function calendar_local_index

    ! Allocate `locals' with the entry of the default values.  Not a port.
    subroutine ensure_locals()
        type(local_type) :: defaults

        if (allocated(locals)) return
        defaults%serial = 0
        allocate (locals(0))
        locals = [locals, defaults]
    end subroutine ensure_locals

    ! Make the current buffer's entry in `locals' from the default values,
    ! as make-local-variable does, reusing the entry of a closed buffer;
    ! K is its index.  Not a port.
    subroutine make_local_entry(k)
        integer, intent(out) :: k
        type(local_type) :: entry

        k = calendar_local_index()
        if (k > 1 .or. current == 0) return

        do k = 2, size(locals)
            if (.not. buffer_serial_live(locals(k)%serial)) exit
        end do

        entry = locals(1)
        entry%serial = buffers(current)%serial
        ! The default value of calendar-total-months.
        entry%total_months = calendar_total_months
        if (k > size(locals)) locals = [locals, entry]
        locals(k) = entry
    end subroutine make_local_entry

    ! kill-all-local-variables: forget the current buffer's values.
    ! Not a port.
    subroutine clear_calendar_locals()
        integer :: k

        k = calendar_local_index()
        if (k > 1) locals(k)%serial = -1
    end subroutine clear_calendar_locals

    !> The month in the middle of the calendar of the current buffer.
    !> Port of calendar.el displayed-month.
    integer function calendar_displayed_month() result(month)
        month = locals(calendar_local_index())%displayed_month
    end function calendar_displayed_month

    !> The year in the middle of the calendar of the current buffer.
    !> Port of calendar.el displayed-year.
    integer function calendar_displayed_year() result(year)
        year = locals(calendar_local_index())%displayed_year
    end function calendar_displayed_year

    !> The number of months the calendar of the current buffer shows (its
    !> `calendar_total_months').  Not a port.
    integer function calendar_total_months_local() result(n)
        n = locals(calendar_local_index())%total_months
    end function calendar_total_months_local

    ! ------------------------------------------------------------------
    ! Layout
    ! ------------------------------------------------------------------

    !> Compute the month edge columns for month SEGMENT.
    !> Returns LEFT, the
    !> leftmost column associated with a month, FIRST and LAST, the
    !> first and last columns with day digits in, and RIGHT, the
    !> rightmost column.  Port of calendar.el:calendar-month-edges.
    subroutine calendar_month_edges(segment, left, first, last, right)
        integer, intent(in) :: segment
        integer, intent(out) :: left, first, last, right

        ! The leftmost column with a digit in it in this month segment.
        first = calendar_left_margin + segment*calendar_month_width
        ! The rightmost column with a digit in it in this month segment.
        last = first + calendar_month_digit_width - 1

        if (segment == 0) then
            left = 0
        else
            left = calendar_left_margin + segment*calendar_month_width - calendar_intermonth_spacing/2
        end if

        ! The rightmost edge of this month segment, dividing the
        ! space between months in two.
        right = calendar_left_margin + (segment + 1)*calendar_month_width - calendar_intermonth_spacing/2
    end subroutine calendar_month_edges

    !> Recompute some layout-related calendar "constants".  Port of
    !> calendar.el:calendar-recompute-layout-variables.
    subroutine calendar_recompute_layout_variables()
        call recompute_layout(calendar_local_index())
    end subroutine calendar_recompute_layout_variables

    ! calendar-recompute-layout-variables for the entry K of `locals'.
    ! Not a port.
    subroutine recompute_layout(k)
        integer, intent(in) :: k
        integer :: i

        calendar_month_digit_width = 6*calendar_column_width + calendar_day_digit_width
        calendar_month_width = 7*calendar_column_width + calendar_intermonth_spacing

        associate (s => locals(k))
            s%right_margin = calendar_left_margin + s%total_months*(7*calendar_column_width) &
                             + (s%total_months - 1)*calendar_intermonth_spacing
            if (allocated(s%month_edges)) deallocate (s%month_edges)
            allocate (s%month_edges(s%total_months))

            do i = 0, s%total_months - 1
                s%month_edges(i + 1)%segment = i
                call calendar_month_edges(i, s%month_edges(i + 1)%left, s%month_edges(i + 1)%first, &
                                          s%month_edges(i + 1)%last, s%month_edges(i + 1)%right)
            end do
        end associate
    end subroutine recompute_layout

    ! What loading calendar.el does once: compute the layout variables and
    ! the keys of the mode line.  Not a port.
    subroutine ensure_loaded()
        if (loaded) return
        loaded = .true.
        call ensure_locals()
        locals(1)%total_months = calendar_total_months
        call recompute_layout(1)
        mode_line_keys = substitute_command_keys("\<calendar_mode_map>\[calendar_goto_info_node] info") &
                         //" / "//substitute_command_keys("\<calendar_mode_map>\[calendar_other_month] other") &
                         //" / "//substitute_command_keys("\<calendar_mode_map>\[calendar_goto_today] today")
    end subroutine ensure_loaded

    ! ------------------------------------------------------------------
    ! Date arithmetic
    ! ------------------------------------------------------------------

    !> Increment the variables MONTH and YEAR by N months.
    !> Forward if N is positive or backward if N is negative.
    !> A negative YEAR is interpreted as BC; -1 being 1 BC, and so on.
    !> Optional NMONTHS is the number of months per year (default 12).
    !> Port of calendar.el:calendar-increment-month.
    pure subroutine calendar_increment_month(month, year, n, nmonths)
        integer, intent(inout) :: month, year
        integer, intent(in) :: n
        integer, intent(in), optional :: nmonths
        integer :: nm, total

        nm = 12
        if (present(nmonths)) nm = nmonths
        ! -1 BC -> 0 AD, etc
        if (year < 0) year = year + 1
        total = year*nm + month - 1 + n
        month = 1 + modulo(total, nm)
        year = total/nm
        if (total < 0 .and. month > 1) year = year - 1
        ! 0 AD -> -1 BC, etc
        if (year < 1) year = year - 1
    end subroutine calendar_increment_month

    !> Return true if YEAR is a Gregorian leap year.
    !> A negative year is interpreted as BC; -1 being 1 BC, and so on.
    !> Port of calendar.el:calendar-leap-year-p.
    elemental logical function is_calendar_leap_year(year) result(leap)
        integer, intent(in) :: year
        integer :: y

        ! 1 BC = 0 AD, 2 BC acts like 1 AD, etc.
        y = year
        if (y < 0) y = abs(y) - 1
        leap = mod(y, 4) == 0 .and. (mod(y, 100) /= 0 .or. mod(y, 400) == 0)
    end function is_calendar_leap_year

    !> The last day in MONTH during YEAR.  Port of
    !> calendar.el:calendar-last-day-of-month.
    elemental integer function calendar_last_day_of_month(month, year) result(last)
        integer, intent(in) :: month, year
        integer, parameter :: LAST_DAYS(12) = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

        if (month == 2 .and. is_calendar_leap_year(year)) then
            last = 29
        else
            last = LAST_DAYS(month)
        end if
    end function calendar_last_day_of_month

    !> Return the day number within the year of the date DATE.
    !> For example, calendar_day_number(calendar_date_type(1, 1, 1987))
    !> returns the value 1, while calendar_day_number(calendar_date_type(12,
    !> 31, 1980)) returns 366.  Port of calendar.el:calendar-day-number.
    elemental integer function calendar_day_number(date) result(day_of_year)
        type(calendar_date_type), intent(in) :: date

        day_of_year = date%day + 31*(date%month - 1)

        if (date%month > 2) then
            day_of_year = day_of_year - (23 + 4*date%month)/10
            if (is_calendar_leap_year(date%year)) day_of_year = day_of_year + 1
        end if
    end function calendar_day_number

    !> The number of days elapsed between the Gregorian date 12/31/1 BC and DATE.
    !> The Gregorian date Sunday, December 31, 1 BC is imaginary.
    !> A negative year is interpreted as BC; -1 being 1 BC, and so
    !> on.  Dates before 12/31/1 BC return negative results.  Port of
    !> calendar.el:calendar-absolute-from-gregorian (a year zero, which
    !> upstream rejects with "There was no year zero", is taken as 1 BC;
    !> `calendar_read_date' reads years above zero only).
    elemental integer function calendar_absolute_from_gregorian(date) result(absolute)
        type(calendar_date_type), intent(in) :: date
        integer :: offset_years

        if (date%year > 0) then
            offset_years = date%year - 1
            ! days this year + days in prior years + Julian leap years -
            ! century years + Gregorian leap years
            absolute = calendar_day_number(date) + 365*offset_years + offset_years/4 - offset_years/100 &
                       + offset_years/400
        else
            ! Years between date and 1 BC, excluding 1 BC (1 for 2 BC, etc).
            offset_years = abs(date%year + 1)
            ! days in year 1 BC
            absolute = calendar_day_number(date) - 365*offset_years - offset_years/4 + offset_years/100 &
                       - offset_years/400 - calendar_day_number(calendar_date_type(12, 31, -1))
        end if
    end function calendar_absolute_from_gregorian

    !> Compute the date (month day year) corresponding to the absolute DATE.
    !> The absolute date is the number of days elapsed since the (imaginary)
    !> Gregorian date Sunday, December 31, 1 BC.  This function does not
    !> handle dates in years BC.  Port of
    !> calendar.el:calendar-gregorian-from-absolute.
    elemental function calendar_gregorian_from_absolute(date) result(gregorian)
        integer, intent(in) :: date
        type(calendar_date_type) :: gregorian
        integer :: d0, n400, d1, n100, d2, n4, d3, n1, day, year, month, mdays

        ! For an explanation, see the footnote on page 384 of "Calendrical
        ! Calculations, Part II: Three Historical Calendars" by
        ! E. M. Reingold, N. Dershowitz, and S. M. Clamen,
        ! Software--Practice and Experience, Volume 23, Number 4 (April,
        ! 1993), pages 383-404 <https://doi.org/10.1002/spe.4380230404>.
        d0 = date - 1
        n400 = d0/146097
        d1 = mod(d0, 146097)
        n100 = d1/36524
        d2 = mod(d1, 36524)
        n4 = d2/1461
        d3 = mod(d2, 1461)
        n1 = d3/365
        day = 1 + mod(d3, 365)
        year = 400*n400 + 100*n100 + n4*4 + n1
        month = 1

        if (n100 == 4 .or. n1 == 4) then
            gregorian = calendar_date_type(12, 31, year)
        else
            year = year + 1

            do
                mdays = calendar_last_day_of_month(month, year)
                if (mdays >= day) exit
                day = day - mdays
                month = month + 1
            end do

            gregorian = calendar_date_type(month, day, year)
        end if
    end function calendar_gregorian_from_absolute

    !> Return the date of the DAYNO-th day in YEAR.
    !> DAYNO must be an integer between -366 and 366.  Port of
    !> calendar.el:calendar-date-from-day-of-year.
    elemental function calendar_date_from_day_of_year(year, dayno) result(date)
        integer, intent(in) :: year, dayno
        type(calendar_date_type) :: date
        integer :: n

        if (dayno < 0) then
            n = 365
            if (is_calendar_leap_year(year)) n = 366
            n = 1 + dayno + n
        else
            n = dayno
        end if

        date = calendar_gregorian_from_absolute(n + calendar_absolute_from_gregorian(calendar_date_type(12, 31, year - 1)))
    end function calendar_date_from_day_of_year

    !> The number of months difference between MON1, YR1 and MON2, YR2.
    !> The result is positive if the second date is later than the first.
    !> Negative years are interpreted as years BC; -1 being 1 BC, and so on.
    !> Port of calendar.el:calendar-interval.
    elemental integer function calendar_interval(mon1, yr1, mon2, yr2) result(n)
        integer, intent(in) :: mon1, yr1, mon2, yr2
        integer :: y1, y2

        ! -1 BC -> 0 AD, etc
        y1 = yr1
        y2 = yr2
        if (y1 < 0) y1 = y1 + 1
        if (y2 < 0) y2 = y2 + 1
        n = 12*(y2 - y1) + mon2 - mon1
    end function calendar_interval

    !> Return the day-of-the-week index of DATE, 0 for Sunday, 1 for Monday, etc.
    !> A negative year is
    !> interpreted as BC; -1 being 1 BC, and so on.  Port of
    !> calendar.el:calendar-day-of-week.
    elemental integer function calendar_day_of_week(date) result(day)
        type(calendar_date_type), intent(in) :: date

        day = modulo(calendar_absolute_from_gregorian(date), 7)
    end function calendar_day_of_week

    !> Return the index (0 for Sunday, etc.) of the last day of the week.
    !> Port of calendar.el:calendar-week-end-day.
    integer function calendar_week_end_day() result(day)
        day = modulo(calendar_week_start_day + 6, 7)
    end function calendar_week_end_day

    !> Return true if DATE is a valid date.  Port of
    !> calendar.el:calendar-date-is-valid-p.
    elemental logical function is_calendar_date_valid(date) result(valid)
        type(calendar_date_type), intent(in) :: date

        valid = .false.
        if (date%month < 1 .or. date%month > 12) return
        ! BC dates left as non-valid, to suppress errors from
        ! complex holiday algorithms not suitable for years BC.
        ! Note there are side effects on calendar navigation.
        if (date%year < 1) return
        valid = date%day >= 1 .and. date%day <= calendar_last_day_of_month(date%month, date%year)
    end function is_calendar_date_valid

    !> Return true if the DATE1 and DATE2 are the same.  Port of
    !> calendar.el:calendar-date-equal.
    elemental logical function is_calendar_date_equal(date1, date2) result(equal)
        type(calendar_date_type), intent(in) :: date1, date2

        equal = date1%month == date2%month .and. date1%day == date2%day .and. date1%year == date2%year
    end function is_calendar_date_equal

    !> Return the absolute date of the DAYNAME on or before absolute DATE.
    !> DAYNAME=0 means Sunday, DAYNAME=1 means Monday, and so on.
    !>
    !> Note: Applying this function to d+6 gives us the DAYNAME on or after
    !> an absolute day d.  Similarly, applying it to d+3 gives the DAYNAME
    !> nearest to absolute date d, applying it to d-1 gives the DAYNAME
    !> previous to absolute date d, and applying it to d+7 gives the DAYNAME
    !> following absolute date d.  Port of
    !> calendar.el:calendar-dayname-on-or-before.
    elemental integer function calendar_dayname_on_or_before(dayname, date) result(absolute)
        integer, intent(in) :: dayname, date

        absolute = date - mod(date - dayname, 7)
    end function calendar_dayname_on_or_before

    !> Absolute date of the Nth DAYNAME after/before MONTH YEAR DAY.
    !> A DAYNAME of 0 means Sunday, 1 means Monday, and so on.
    !> If N>0, return the Nth DAYNAME after MONTH DAY, YEAR (inclusive).
    !> If N<0, return the Nth DAYNAME before MONTH DAY, YEAR (inclusive).
    !> DAY defaults to 1 if N>0, and MONTH's last day otherwise.  Port of
    !> calendar.el:calendar-nth-named-absday.
    integer function calendar_nth_named_absday(n, dayname, month, year, day) result(absolute)
        integer, intent(in) :: n, dayname, month, year
        integer, intent(in), optional :: day
        integer :: d

        if (n > 0) then
            d = 1
            if (present(day)) d = day
            absolute = 7*(n - 1) + calendar_dayname_on_or_before(dayname, &
                                                                 6 + calendar_absolute_from_gregorian( &
                                                                 calendar_date_type(month, d, year)))
        else
            d = calendar_last_day_of_month(month, year)
            if (present(day)) d = day
            absolute = 7*(n + 1) + calendar_dayname_on_or_before(dayname, &
                                                                 calendar_absolute_from_gregorian( &
                                                                 calendar_date_type(month, d, year)))
        end if
    end function calendar_nth_named_absday

    !> Date of the Nth DAYNAME after/before MONTH YEAR DAY.
    !> Like `calendar_nth_named_absday', but returns a Gregorian date.
    !> Port of calendar.el:calendar-nth-named-day.
    function calendar_nth_named_day(n, dayname, month, year, day) result(date)
        integer, intent(in) :: n, dayname, month, year
        integer, intent(in), optional :: day
        type(calendar_date_type) :: date

        date = calendar_gregorian_from_absolute(calendar_nth_named_absday(n, dayname, month, year, day))
    end function calendar_nth_named_day

    !> Return the current date (month day year).
    !> Optional integer OFFSET is a number of days from the current date.
    !> Port of calendar.el:calendar-current-date.
    function calendar_current_date(offset) result(date)
        integer, intent(in), optional :: offset
        type(calendar_date_type) :: date
        type(decoded_time_type) :: now

        now = decode_time()
        date = calendar_date_type(now%month, now%day, int(now%year))

        if (present(offset)) then
            if (offset /= 0) date = calendar_gregorian_from_absolute(offset + calendar_absolute_from_gregorian(date))
        end if
    end function calendar_current_date

    ! ------------------------------------------------------------------
    ! Names and strings
    ! ------------------------------------------------------------------

    !> Return a string with the name of the day of the week of DATE.
    !> Day names are taken from the variable `calendar_day_names',
    !> unless ABBREVIATION is true, in which case `calendar_day_abbreviations' is
    !> used.  Port of calendar.el:calendar-day-name (the day of the week
    !> as an index, upstream's ABSOLUTE, is an element of the arrays).
    function calendar_day_name(date, abbreviation) result(name)
        type(calendar_date_type), intent(in) :: date
        logical, intent(in), optional :: abbreviation
        character(len=:), allocatable :: name

        name = trim(calendar_day_names(calendar_day_of_week(date)))

        if (present(abbreviation)) then
            if (abbreviation) name = trim(calendar_day_abbreviations(calendar_day_of_week(date)))
        end if
    end function calendar_day_name

    !> Return a string with the name of month number MONTH.
    !> Months are numbered from one.  Month names are taken from the
    !> variable `calendar_month_names', unless the optional
    !> argument ABBREVIATION is true, in which case
    !> `calendar_month_abbreviations' is used.  Port of
    !> calendar.el:calendar-month-name.
    function calendar_month_name(month, abbreviation) result(name)
        integer, intent(in) :: month
        logical, intent(in), optional :: abbreviation
        character(len=:), allocatable :: name

        name = trim(calendar_month_names(month))

        if (present(abbreviation)) then
            if (abbreviation) name = trim(calendar_month_abbreviations(month))
        end if
    end function calendar_month_name

    !> A string of DATE, driven by the variable `calendar_date_style'.
    !> An optional parameter ABBREVIATE, when true, causes the month
    !> and day names to be abbreviated as specified by
    !> `calendar_month_abbreviations' and `calendar_day_abbreviations',
    !> respectively.  An optional parameter NODAYNAME, when true, omits the
    !> name of the day of the week.  Port of calendar.el:calendar-date-string
    !> (with what calendar.el's three date display styles give).
    function calendar_date_string(date, abbreviate, nodayname) result(s)
        type(calendar_date_type), intent(in) :: date
        logical, intent(in), optional :: abbreviate, nodayname
        character(len=:), allocatable :: s, day_part, monthname
        logical :: abbreviation
        character(len=24) :: text

        abbreviation = .false.
        if (present(abbreviate)) abbreviation = abbreviate
        ! (if dayname (concat dayname ", "))
        day_part = calendar_day_name(date, abbreviation)//", "

        if (present(nodayname)) then
            if (nodayname) day_part = ""
        end if

        monthname = calendar_month_name(date%month, abbreviation)

        select case (calendar_date_style)
        case (CALENDAR_DATE_STYLE_ISO)
            ! (format "%s-%.2d-%.2d" year month day)
            write (text, "(i0,'-',i2.2,'-',i2.2)") date%year, date%month, date%day
            s = trim(text)
        case (CALENDAR_DATE_STYLE_EUROPEAN)
            write (text, "(i0)") date%day
            s = day_part//trim(text)//" "//monthname
            write (text, '(" ", i0)') date%year
            s = s//trim(text)
        case default
            write (text, '(" ", i0, ", ", i0)') date%day, date%year
            s = day_part//monthname//trim(text)
        end select
    end function calendar_date_string

    !> String of day number of year of Gregorian DATE.
    !> Defaults to today's date if DATE is not given.  Port of
    !> calendar.el:calendar-day-of-year-string.
    function calendar_day_of_year_string(date) result(s)
        type(calendar_date_type), intent(in), optional :: date
        character(len=:), allocatable :: s
        type(calendar_date_type) :: d
        integer :: day, days_remaining
        character(len=64) :: text

        if (present(date)) then
            d = date
        else
            d = calendar_current_date()
        end if

        day = calendar_day_number(d)
        days_remaining = calendar_day_number(calendar_date_type(12, 31, d%year)) - day
        write (text, '("Day ", i0, " of ", i0, "; ", i0, " day")') day, d%year, days_remaining
        s = trim(text)
        if (days_remaining /= 1) s = s//"s"
        s = s//" remaining in the year"
    end function calendar_day_of_year_string

    ! The month header of MONTH, YEAR: upstream's
    ! calendar-american-month-header, calendar-european-month-header or
    ! calendar-iso-month-header, by `calendar_date_style'.  Not a port.
    function month_header(month, year) result(s)
        integer, intent(in) :: month, year
        character(len=:), allocatable :: s
        character(len=16) :: text

        if (calendar_date_style == CALENDAR_DATE_STYLE_ISO) then
            ! (format "%d %s" year (calendar-month-name month))
            write (text, "(i0)") year
            s = trim(text)//" "//calendar_month_name(month)
        else
            ! (format "%s %d" (calendar-month-name month) year)
            write (text, '(" ", i0)') year
            s = calendar_month_name(month)//trim(text)
        end if
    end function month_header

    !> Concatenate list of STRINGS separated with copies of CHAR to fill LENGTH.
    !> The effect is like joining them with a separator but the separating
    !> pieces are as balanced as possible.  If LENGTH is too short, the STRINGS are just
    !> concatenated and the result truncated.  Port of
    !> calendar.el:calendar-string-spread (the strings are plain text;
    !> CHAR is one column wide).
    function calendar_string_spread(strings, char, length) result(s)
        type(string_type), intent(in) :: strings(:)
        character(len=1), intent(in) :: char
        integer, intent(in) :: length
        character(len=:), allocatable :: s
        type(string_type), allocatable :: items(:)
        integer :: n, m, i, total

        ! The algorithm is based on equation (3.25) on page 85 of Concrete
        ! Mathematics by Ronald L. Graham, Donald E. Knuth, and Oren Patashnik,
        ! Addison-Wesley, Reading, MA, 1989.
        if (size(strings) < 2) then
            allocate (items(size(strings) + 2))
            items(1)%text = ""
            items(2:size(strings) + 1) = strings
            items(size(items))%text = ""
        else
            items = strings
        end if

        total = 0

        do i = 1, size(items)
            total = total + int(string_width(items(i)%text))
        end do

        n = length - total
        m = size(items) - 1
        s = items(1)%text

        do i = 0, size(items) - 2
            s = s//repeat(char, max(0, (n + i)/m))//items(i + 2)%text
        end do

        s = truncate_string_to_width(s, length)
    end function calendar_string_spread

    ! The format "PROMPT (default DEFAULT): " of minibuffer.el's
    ! format-prompt.  Not a port.
    function format_prompt(prompt, default) result(full)
        character(len=*), intent(in) :: prompt, default
        character(len=:), allocatable :: full

        full = prompt//" (default "//default//"): "
    end function format_prompt

    ! ------------------------------------------------------------------
    ! Generating the calendar
    ! ------------------------------------------------------------------

    !> Return current usable calendar-mode buffer.  Port of
    !> calendar.el:calendar-get-buffer.
    function calendar_get_buffer() result(h)
        type(handle_type) :: h

        if (is_derived_mode(major_modes%calendar_mode)) then
            h = current_buffer()
        else
            h = get_buffer(trim(calendar_buffer))
        end if
    end function calendar_get_buffer

    !> Like `move_to_column', but indents if the line is too short.
    !> Port of calendar.el:calendar-move-to-column.
    subroutine calendar_move_to_column(indent)
        integer, intent(in) :: indent
        integer(int64) :: reached, column
        type(status_type) :: st

        call move_to_column(int(indent, int64), reached)
        if (reached < indent) column = indent_to(int(indent, int64), st)
    end subroutine calendar_move_to_column

    !> Move to the next line, adding a newline if necessary.  Port of
    !> calendar.el:calendar-ensure-newline.
    subroutine calendar_ensure_newline(status)
        type(status_type), intent(out) :: status

        status = status_type()
        if (forward_line(1_int64) /= 0) call insert(achar(10), status)
    end subroutine calendar_ensure_newline

    !> Move to column INDENT, adding spaces as needed.
    !> Inserts STRING so that it ends at INDENT.  Truncates
    !> STRING to length TRUNCATE, and ensures a trailing space.  Port of
    !> calendar.el:calendar-insert-at-column (STRING a literal string, ""
    !> for nothing).
    subroutine calendar_insert_at_column(indent, string, truncate, status)
        integer, intent(in) :: indent, truncate
        character(len=*), intent(in) :: string
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: s
        integer :: k

        status = status_type()

        if (len(string) == 0) then
            call calendar_move_to_column(indent)
            return
        end if

        s = string
        if (string_width(s) > truncate) s = truncate_string_to_width(s, truncate)

        if (s(len(s):len(s)) /= " ") then
            if (string_width(s) == truncate) then
                ! Drop the last character.
                k = len(s)

                do while (k > 1)
                    if (iand(iachar(s(k:k)), 192) /= 128) exit
                    k = k - 1
                end do

                s = s(1:k - 1)
            end if

            s = s//" "
        end if

        call calendar_move_to_column(indent - int(string_width(s)))
        call insert(s, status)
    end subroutine calendar_insert_at_column

    ! Insert TEXT with the text property PROPERTY set to the face FACE.
    ! Not a port.
    subroutine insert_with_face(text, property, face, status)
        character(len=*), intent(in) :: text, property
        type(named_face_type), intent(in) :: face
        type(status_type), intent(out) :: status
        integer(int64) :: start

        start = point()
        call insert(text, status)
        if (.not. status%ok()) return
        call put_text_property(start, point(), value_type(name=property), value_type(name=trim(face%name)), status)
    end subroutine insert_with_face

    !> Produce a calendar for MONTH, YEAR on the Gregorian calendar.
    !> The calendar is inserted at the top of the buffer in which point is
    !> currently located, but indented INDENT spaces.  The indentation is
    !> done from the first character on the line and does not disturb the
    !> first INDENT characters on the line.  Port of
    !> calendar.el:calendar-generate-month.
    subroutine calendar_generate_month(month, year, indent, status)
        integer, intent(in) :: month, year, indent
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: header, name, spread
        character(len=8) :: digits
        integer :: blank_days, last, trunc, day, i, j, n, pad
        integer(int64) :: start
        type(status_type) :: st

        ! at start of month
        blank_days = modulo(calendar_day_of_week(calendar_date_type(month, 1, year)) - calendar_week_start_day, 7)
        last = calendar_last_day_of_month(month, year)
        trunc = min(calendar_intermonth_spacing, calendar_left_margin - 1)
        call goto_character(point_minimum())
        call calendar_move_to_column(indent)
        ! The month header, in the calendar_month_header face, spread to
        ! the digits' width.
        header = month_header(month, year)
        n = calendar_month_digit_width - int(string_width(header))
        pad = max(0, n/2)
        spread = truncate_string_to_width(repeat(" ", pad)//header//repeat(" ", max(0, (n + 1)/2)), &
                                          calendar_month_digit_width)
        start = point()
        call insert(spread, status)
        if (.not. status%ok()) return

        if (pad < len(spread)) then
            call put_text_property(start + pad, min(point(), start + pad + character_count(header)), &
                                   value_type(name=FONT_LOCK_FACE_PROPERTY), &
                                   value_type(name=trim(calendar_month_header%name)), st)
        end if

        call calendar_ensure_newline(status)
        if (.not. status%ok()) return
        call calendar_insert_at_column(indent, calendar_intermonth_header, trunc, status)
        if (.not. status%ok()) return

        ! Use the first N characters of each day to head the columns.
        do i = 0, 6
            j = modulo(calendar_week_start_day + i, 7)
            name = truncate_string_to_width(trim(calendar_day_headers(j)), calendar_day_header_width)

            if (any(calendar_weekend_days == j)) then
                call insert_with_face(name, FONT_LOCK_FACE_PROPERTY, calendar_weekend_header, status)
            else
                call insert_with_face(name, FONT_LOCK_FACE_PROPERTY, calendar_weekday_header, status)
            end if

            if (.not. status%ok()) return
            call insert(repeat(" ", calendar_day_header_width - int(string_width(name))) &
                        //repeat(" ", calendar_column_width - calendar_day_header_width), status)
            if (.not. status%ok()) return
        end do

        call calendar_ensure_newline(status)
        if (.not. status%ok()) return
        call calendar_insert_at_column(indent, calendar_intermonth_text, trunc, status)
        if (.not. status%ok()) return
        ! Add blank days before the first of the month.
        call insert(repeat(" ", blank_days*calendar_column_width), status)
        if (.not. status%ok()) return

        ! Put in the days of the month.
        do i = 0, last - 1
            day = i + 1
            write (digits, "(i8)") day
            start = point()
            call insert(digits(len(digits) - max(calendar_day_digit_width, len_trim(adjustl(digits))) + 1:), status)
            if (.not. status%ok()) return
            call put_text_property(start, point(), value_type(name=MOUSE_FACE_PROPERTY), &
                                   value_type(name=trim(highlight%name)), st)
            call put_text_property(start, point(), value_type(name=HELP_ECHO_PROPERTY), &
                                   value_type(text=calendar_date_echo_text), st)
            ! 'date property prevents intermonth text confusing re-searches.
            call put_text_property(start, point(), value_type(name=DATE_PROPERTY), value_type(.true.), st)
            call insert(repeat(" ", calendar_column_width - calendar_day_digit_width), status)
            if (.not. status%ok()) return

            if (mod(day + blank_days, 7) == 0 .and. day /= last) then
                call calendar_ensure_newline(status)
                if (.not. status%ok()) return
                call calendar_insert_at_column(indent, calendar_intermonth_text, trunc, status)
                if (.not. status%ok()) return
            end if
        end do
    end subroutine calendar_generate_month

    ! The number of characters of the UTF-8 text S.  Not a port.
    integer function character_count(s) result(n)
        character(len=*), intent(in) :: s
        integer :: k

        n = 0

        do k = 1, len(s)
            if (iand(iachar(s(k:k)), 192) /= 128) n = n + 1
        end do
    end function character_count

    !> Generate a three-month Gregorian calendar centered around MONTH, YEAR.
    !> Port of calendar.el:calendar-generate.
    subroutine calendar_generate(month, year, status)
        integer, intent(in) :: month, year
        type(status_type), intent(out) :: status
        integer :: m, y, i, k

        ! A negative YEAR is interpreted as BC; -1 being 1 BC, and so on.
        ! Note that while calendars for years BC could be displayed as it
        ! stands, almost all other calendar functions (eg holidays) would
        ! at best have unpredictable results for such dates.
        status = status_type()

        if (month + 12*(year - 1) < 2) then
            call set_error(status, USER_ERROR_CODE, "Months before January, 1 AD cannot be displayed")
            return
        end if

        k = calendar_local_index()
        locals(k)%displayed_month = month
        locals(k)%displayed_year = year
        call erase_buffer(status)
        if (.not. status%ok()) return
        m = month
        y = year
        call calendar_increment_month(m, y, -1)

        do i = 0, locals(k)%total_months - 1
            call calendar_generate_month(m, y, calendar_left_margin + calendar_month_width*i, status)
            if (.not. status%ok()) return
            call calendar_increment_month(m, y, 1)
        end do
    end subroutine calendar_generate

    !> Generate the calendar window for the current date.
    !> Optional integers MON and YR are used instead of today's date.
    !> Port of calendar.el:calendar-generate-window.
    recursive subroutine calendar_generate_window(status, mon, yr)
        type(status_type), intent(out) :: status
        integer, intent(in), optional :: mon, yr
        type(calendar_date_type) :: today
        type(handle_type) :: old, sw
        type(status_type) :: st
        integer :: month, year
        logical :: in_calendar_window, saved_read_only

        ! (inhibit-read-only t)
        saved_read_only = buffers(current)%read_only
        buffers(current)%read_only = .false.
        today = calendar_current_date()
        month = today%month
        year = today%year
        if (present(mon)) month = mon
        if (present(yr)) year = yr
        ! (with-current-buffer (window-buffer) (derived-mode-p 'calendar-mode))
        sw = selected_window()
        old = current_buffer()
        call set_buffer(window_buffer(sw, st), st)
        in_calendar_window = is_derived_mode(major_modes%calendar_mode)
        call set_buffer(old, st)
        call calendar_generate(month, year, status)

        if (status%ok()) then
            call restore_buffer_modified(.false.)

            ! Don't do any window-related stuff if we weren't called from a
            ! window displaying the calendar.
            if (in_calendar_window) then
                ! Adjust the window to exactly fit the displayed calendar.
                ! (For a full height window or a window that is
                ! horizontally combined, upstream's set-window-vscroll
                ! has nothing to do on a terminal.)
                if (is_window_combined(sw, .false.)) then
                    call fit_window_to_buffer(sw, minimum_height=calendar_minimum_window_height)
                end if
            end if

            if (calendar_mark_holidays_variable .and. associated(calendar_mark_holidays_hook)) then
                call calendar_mark_holidays_hook(status)
            end if
        end if

        if (status%ok()) then
            ! (unwind-protect (if calendar-mark-diary-entries (diary-mark-entries)) ...)
            if (calendar_mark_diary_entries .and. associated(calendar_mark_diary_entries_hook)) then
                call calendar_mark_diary_entries_hook(status)
            end if

            if (.not. is_calendar_date_visible(today)) then
                call calendar_today_invisible_hook%execute(status)
            else
                ! Functions in calendar-today-visible-hook may rely on the cursor
                ! being on today's date.
                call calendar_cursor_to_visible_date(today)
                call calendar_today_visible_hook%execute(status)
            end if
        end if

        buffers(current)%read_only = saved_read_only
    end subroutine calendar_generate_window

    !> Redraw the calendar display, if `calendar_buffer' is live.  Port of
    !> calendar.el:calendar-redraw.
    recursive subroutine calendar_redraw(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: buffer, old, window
        type(calendar_date_type) :: cursor_date
        type(status_type) :: st

        status = status_type()
        buffer = calendar_get_buffer()
        if (.not. is_buffer_live(buffer)) return
        old = current_buffer()
        call set_buffer(buffer, status)
        if (.not. status%ok()) return
        call calendar_cursor_to_nearest_date(cursor_date, status)

        if (status%ok()) then
            call calendar_generate_window(status, calendar_displayed_month(), calendar_displayed_year())

            if (status%ok()) then
                call cursor_to_date_or_error(cursor_date, status)

                if (status%ok()) then
                    call calendar_update_mode_line()
                    window = get_buffer_window(buffer)
                    if (is_window_live(window)) call set_window_point(windows(window%index), point(), st)
                end if
            end if
        end if

        call set_buffer(old, st)
    end subroutine calendar_redraw

    ! calendar-cursor-to-visible-date on DATE, a wrong-type-argument error
    ! for no date.  Not a port.
    recursive subroutine cursor_to_date_or_error(date, status)
        type(calendar_date_type), intent(in) :: date
        type(status_type), intent(out) :: status

        status = status_type()

        if (date%month == 0) then
            call set_error(status, WRONG_TYPE_ARGUMENT_CODE, "expected integer")
            return
        end if

        call calendar_cursor_to_visible_date(date)
    end subroutine cursor_to_date_or_error

    ! ------------------------------------------------------------------
    ! The mode
    ! ------------------------------------------------------------------

    !> A major mode for the calendar window.
    !> For a complete description, see the info node `Calendar/Diary'.
    !>
    !> \<calendar_mode_map>\{calendar_mode_map}  Port of
    !> calendar.el:calendar-mode.
    recursive subroutine calendar_mode(status)
        type(status_type), intent(out) :: status
        logical :: saved
        integer :: k

        call ensure_loaded()
        call major_modes%calendar_mode%begin(saved, status)
        if (.not. status%ok()) return
        buffers(current)%read_only = .true.
        ! (buffer-undo-list t)
        buffers(current)%undo_enabled = .false.
        call indent_tabs_mode%set_local(.false.)
        call make_local_entry(k)
        call calendar_update_mode_line()
        ! Init with user options.
        calendar_mark_holidays_variable = calendar_mark_holidays_flag
        calendar_mark_diary_entries = calendar_mark_diary_entries_flag
        ! Most functions only work if displayed-month and displayed-year are set,
        ! so let's make sure they're always set.  Most likely, this will be reset
        ! soon in calendar-generate, but better safe than sorry.
        k = calendar_local_index()
        if (locals(k)%displayed_month == 0) locals(k)%displayed_month = 1
        if (locals(k)%displayed_year == 0) locals(k)%displayed_year = 2001
        call major_modes%calendar_mode%end(saved, status)
    end subroutine calendar_mode

    !> The text of the calendar's mode line: `calendar_mode_line_format''s
    !> entries spread over the calendar's width, after as many spaces as
    !> the left margin less two.  Not a port (the text
    !> calendar.el:calendar-update-mode-line makes, with the default
    !> calendar.el calendar-mode-line-format).
    function calendar_mode_line_text() result(text)
        character(len=:), allocatable :: text
        type(string_type) :: items(5)
        integer :: start, k

        call ensure_loaded()
        k = calendar_local_index()
        start = calendar_left_margin - 2
        items(1)%text = "<"
        items(2)%text = trim(buffers(current)%name)

        ! (string-trim (buffer-name) "\\*" "\\*")
        if (len(items(2)%text) > 0) then
            if (items(2)%text(1:1) == "*") items(2)%text = items(2)%text(2:)
        end if

        if (len(items(2)%text) > 0) then
            if (items(2)%text(len(items(2)%text):) == "*") items(2)%text = items(2)%text(:len(items(2)%text) - 1)
        end if

        items(3)%text = mode_line_keys
        items(4)%text = calendar_date_string(calendar_current_date(), .true.)
        items(5)%text = ">"
        text = repeat(" ", max(0, start))//calendar_string_spread(items, " ", locals(k)%right_margin - (start - 1))
    end function calendar_mode_line_text

    !> Update the calendar mode line with the current date and date style.
    !> Port of calendar.el:calendar-update-mode-line (a terminal window
    !> has no display margins).
    recursive subroutine calendar_update_mode_line()
        type(handle_type) :: buffer, old
        type(header_line_format_type) :: format
        type(calendar_date_type) :: date
        type(status_type) :: st

        buffer = calendar_get_buffer()
        if (.not. is_buffer_live(buffer)) return
        old = current_buffer()
        call set_buffer(buffer, st)
        ! The date nearest the cursor (the default format does not use it,
        ! but the cursor moves there).
        call calendar_cursor_to_nearest_date(date, st)
        call format%append(calendar_mode_line_text())
        buffers(current)%mode_line_format = format
        call force_mode_line_update(.false.)
        call set_buffer(old, st)
    end subroutine calendar_update_mode_line

    !> Go to the info node for the calendar.  Port of
    !> calendar.el:calendar-goto-info-node.
    recursive subroutine calendar_goto_info_node(status)
        type(status_type), intent(out) :: status

        call info(status, "(emacs)Calendar/Diary")
        if (.not. status%ok()) return
        call fit_window_to_buffer(selected_window())
    end subroutine calendar_goto_info_node

    ! ------------------------------------------------------------------
    ! Displaying the calendar
    ! ------------------------------------------------------------------

    !> Display a three-month Gregorian calendar.
    !> The three months appear side by side, with the current month in
    !> the middle surrounded by the previous and next months.  The
    !> cursor is put on today's date.  If optional prefix argument ARGUMENT
    !> is true, prompts for the central month and year.
    !>
    !> Once in the calendar window, future or past months can be moved
    !> into view.  Arbitrary months can be displayed, or the calendar
    !> can be scrolled forward or backward.  The cursor can be moved
    !> forward or backward by one day, one week, one month, or one year.
    !> All of these commands take prefix arguments which, when negative,
    !> cause movement in the opposite direction.  For convenience, the
    !> digit keys and the minus sign are automatically prefixes.  Use
    !> \[describe_mode] for details of the key bindings in the calendar
    !> window.
    !>
    !> Runs the following hooks:
    !>
    !> `calendar_today_visible_hook', `calendar_today_invisible_hook' - after
    !>    generating a calendar, if today's date is visible or not, respectively
    !> `calendar_initial_window_hook' - after first creating a calendar
    !>
    !> This function is suitable for execution in an init file.  Port of
    !> calendar.el:calendar (the frames of `calendar_setup' are
    !> Deferred).
    recursive subroutine calendar(status, argument)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: argument

        call calendar_basic_setup(status, argument)
    end subroutine calendar

    ! The command `calendar': ARGUMENT is the prefix argument.  Not a port.
    recursive subroutine execute_calendar(status)
        type(status_type), intent(out) :: status

        call calendar(status, current_prefix_argument%kind /= PREFIX_NONE)
    end subroutine execute_calendar

    !> Create a three-month calendar.
    !> If optional prefix argument ARGUMENT is true, prompts for the month
    !> and year, else uses the current date.  If NODISPLAY is true, don't
    !> display the generated calendar.  Port of
    !> calendar.el:calendar-basic-setup.
    recursive subroutine calendar_basic_setup(status, argument, nodisplay)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: argument, nodisplay
        type(handle_type) :: buffer_handle, buffer, window, sw, diary_buffer
        type(calendar_date_type) :: today, date, first
        type(status_type) :: st
        integer :: month, year, saved_height, saved_width
        logical :: saved_pop_up, prompt, display

        prompt = .false.
        if (present(argument)) prompt = argument
        display = .true.
        if (present(nodisplay)) display = .not. nodisplay
        buffer_handle = current_buffer()
        call get_or_make_buffer(trim(calendar_buffer), buffer, status)
        if (.not. status%ok()) return
        call set_buffer(buffer, status)
        if (.not. status%ok()) return
        call calendar_mode(status)
        if (.not. status%ok()) return
        saved_pop_up = pop_up_windows
        saved_height = split_height_threshold
        saved_width = split_width_threshold
        pop_up_windows = .true.
        ! Not really needed now, but means we use exactly the same
        ! behavior as before in the non-wide case (see below).
        split_height_threshold = 1000

        if (calendar_split_width_threshold == CALENDAR_SPLIT_WIDTH_THRESHOLD_NONE) then
            split_width_threshold = huge(0)
        else
            split_width_threshold = calendar_split_width_threshold
        end if

        setup: block
            today = calendar_current_date()

            if (prompt) then
                call calendar_read_date(date, status, noday=.true.)
                if (.not. status%ok()) exit setup
            else
                date = today
            end if

            month = date%month
            year = date%year
            call calendar_increment_month(month, year, -calendar_offset)

            ! Display the buffer before calling calendar-generate-window so that it
            ! can get a chance to adjust the window sizes to the frame size.
            if (display) then
                ! Is this a wide frame?  If so, split it horizontally.
                sw = selected_window()

                if (is_window_splittable(sw, .true.)) then
                    call split_window_and_copy(sw, .true., window, status)
                    if (.not. status%ok()) exit setup
                end if

                call pop_to_buffer(buffer, status)
                if (.not. status%ok()) exit setup
                ! Has the window already been split vertically?
                sw = selected_window()

                if (window_dedication(sw, st) == DEDICATED_NONE .and. is_window_splittable(sw, .false.) &
                    .and. is_window_full_height(sw)) then
                    call split_window_and_copy(sw, .false., window, status)
                    if (.not. status%ok()) exit setup
                    ! In the upper window, show whatever was visible before.
                    ! This looks better than using other-buffer.
                    if (is_buffer_live(buffer_handle)) call switch_to_buffer(trim(buffers(buffer_handle%index)%name), status)
                    if (.not. status%ok()) exit setup
                    ! Switch to the lower window with the calendar buffer.
                    call select_window(window, status)
                    if (.not. status%ok()) exit setup
                end if
            end if

            call calendar_generate_window(status, month, year)
            if (.not. status%ok()) exit setup

            if (is_calendar_date_visible(today)) then
                call calendar_cursor_to_visible_date(today)
            else
                first = calendar_date_type(month, 1, year)
                call calendar_cursor_to_visible_date(first)
            end if

            call calendar_update_mode_line()

            if (calendar_view_diary_initially_flag .and. is_calendar_date_visible(date) &
                .and. associated(calendar_view_diary_entries_hook)) then
                ! Do not clobber the calendar with the diary, if the diary
                ! has previously been shown in the window that now shows the
                ! calendar (bug#18381): the selected window shows the
                ! calendar here, and the diary goes to another one.
                call calendar_view_diary_entries_hook(status)
                if (.not. status%ok()) exit setup
            end if
        end block setup

        pop_up_windows = saved_pop_up
        split_height_threshold = saved_height
        split_width_threshold = saved_width
        if (.not. status%ok()) return

        if (calendar_view_holidays_initially_flag .and. associated(calendar_list_holidays_hook)) then
            ! (diary-live-p): the fancy diary buffer or the buffer visiting
            ! the diary file.
            diary_buffer = get_buffer(diary_fancy_buffer)
            if (.not. is_buffer_live(diary_buffer)) diary_buffer = diary_file_buffer()
            split_height_threshold = 1000

            if (is_buffer_live(diary_buffer)) then
                if (is_window_live(get_buffer_window(diary_buffer))) split_height_threshold = 2
            end if

            ! FIXME display buffer?
            call calendar_list_holidays_hook(status)
            split_height_threshold = saved_height
            if (.not. status%ok()) return
        end if

        call calendar_initial_window_hook%execute(status)
    end subroutine calendar_basic_setup

    ! The command `calendar_basic_setup': ARGUMENT is the prefix argument.
    ! Not a port.
    recursive subroutine execute_calendar_basic_setup(status)
        type(status_type), intent(out) :: status

        call calendar_basic_setup(status, current_prefix_argument%kind /= PREFIX_NONE)
    end subroutine execute_calendar_basic_setup

    !> List of all calendar-related buffers (as buffers, not strings).
    !> Port of calendar.el:calendar-buffer-list.
    function calendar_buffer_list() result(buffers)
        type(handle_type), allocatable :: buffers(:)
        character(len=64) :: names(7)
        type(handle_type) :: b
        integer :: k

        names = [character(len=64) :: calendar_hebrew_yahrzeit_buffer, lunar_phases_buffer, holiday_buffer, &
                 diary_fancy_buffer, solar_sunrises_buffer, calendar_buffer, calendar_other_calendars_buffer]
        allocate (buffers(0))

        do k = 1, size(names)
            b = get_buffer(trim(names(k)))
            if (is_buffer_live(b)) buffers = [b, buffers]

            ! (get-file-buffer diary-file) comes after the fancy diary.
            if (k == 5) then
                b = diary_file_buffer()
                if (is_buffer_live(b)) buffers = [b, buffers]
            end if
        end do
    end function calendar_buffer_list

    !> The buffer visiting `diary_file', or the null handle.  Not a port
    !> (calendar.el's (get-file-buffer diary-file)).
    function diary_file_buffer() result(buffer)
        type(handle_type) :: buffer
        character(len=:), allocatable :: expanded
        type(status_type) :: st

        buffer = null_handle()
        call ensure_diary_file()
        call expand_file_name(diary_file, expanded, st)
        if (st%ok()) buffer = get_file_buffer(expanded)
    end function diary_file_buffer

    !> Get out of the calendar window and hide it and related buffers.
    !> If KILL (interactively, the prefix), close the buffers instead of
    !> hiding them.  Port of calendar.el:calendar-exit (the frames of
    !> `calendar_setup' are Deferred).
    recursive subroutine calendar_exit(status, kill)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: kill
        type(handle_type), allocatable :: calendar_buffers(:)
        type(handle_type) :: diary_buffer
        logical :: close_buffers, modified, answer
        integer :: k
        type(status_type) :: st

        close_buffers = .false.
        if (present(kill)) close_buffers = kill
        status = status_type()

        ! FIXME: We need to record the associations between each
        ! calendar-mode buffer and its auxiliary buffers explicitly.  For the
        ! moment, don't handle buffers from `calendar-buffer-list' when
        ! exiting a calendar-mode buffer not named `calendar-buffer'.
        if (is_derived_mode(major_modes%calendar_mode)) then
            if (trim(buffers(current)%name) /= trim(calendar_buffer)) then
                call quit_windows_on(current_buffer(), close_buffers, status)
                return
            end if
        end if

        diary_buffer = diary_file_buffer()
        calendar_buffers = calendar_buffer_list()

        if (is_buffer_live(diary_buffer)) then
            call is_buffer_modified(diary_buffer, modified, st)

            if (modified) then
                call read_yes_or_no("Diary modified; do you really want to exit the calendar? ", answer, status)
                if (.not. status%ok()) return
                if (.not. answer) return
            end if
        end if

        do k = 1, size(calendar_buffers)
            call quit_windows_on(calendar_buffers(k), close_buffers, st)
        end do

        ! Finally, close non-displayed buffers (if requested).
        if (close_buffers) then
            do k = 1, size(calendar_buffers)
                if (is_buffer_live(calendar_buffers(k))) call close_buffer(calendar_buffers(k), st)
            end do
        end if
    end subroutine calendar_exit

    ! The command `calendar_exit': KILL is the prefix argument.  Not a port.
    recursive subroutine execute_calendar_exit(status)
        type(status_type), intent(out) :: status

        call calendar_exit(status, current_prefix_argument%kind /= PREFIX_NONE)
    end subroutine execute_calendar_exit

    ! ------------------------------------------------------------------
    ! The cursor's date
    ! ------------------------------------------------------------------

    !> Convert current column to calendar month "segment".
    !> The left-most month returns 0, the next right 1, and so on.  Port of
    !> calendar.el:calendar-column-to-segment.
    integer function calendar_column_to_segment() result(segment)
        integer :: column

        column = max(0, int(current_column()) + calendar_intermonth_spacing/2 - calendar_left_margin)
        segment = column/(7*calendar_column_width + calendar_intermonth_spacing)
    end function calendar_column_to_segment

    !> Whether the text at POSITION has the `date' property.  Public only for
    !> calendar_movement_mod, as `calendar_local_index'.  Not a port.
    logical function has_calendar_date_property(position)
        integer(int64), intent(in) :: position
        type(value_type) :: v
        type(status_type) :: st

        has_calendar_date_property = .false.
        if (position < point_minimum() .or. position >= point_maximum()) return
        call get_text_property(position, value_type(name=DATE_PROPERTY), v, st)
        has_calendar_date_property = allocated(v%a)
    end function has_calendar_date_property

    !> Return the date (month day year) of current cursor position.
    !> If cursor is not on a specific date, a `user_error' error
    !> condition occurs if optional parameter ERROR is true, otherwise
    !> DATE is no date.  Port of calendar.el:calendar-cursor-to-date (no
    !> EVENT).
    recursive subroutine calendar_cursor_to_date(date, status, error)
        type(calendar_date_type), intent(out) :: date
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: error
        integer :: segment, month_index, month, k
        integer(int64) :: original_point, found
        character(len=:), allocatable :: digits
        logical :: on_date
        type(status_type) :: st

        status = status_type()
        date = calendar_date_type()
        original_point = point()
        k = calendar_local_index()
        segment = calendar_column_to_segment()
        month_index = locals(k)%displayed_month + segment - 1
        month = mod(month_index, 12)
        ! Call with point on either of the two digits in a 2-digit date,
        ! or on or before the digit of a 1-digit date.
        on_date = looking_at("[ 0-9]?[0-9][^0-9]", st)
        if (on_date) on_date = has_calendar_date_property(point())

        if (.not. on_date) then
            if (present(error)) then
                if (error) call set_error(status, USER_ERROR_CODE, "Not on a date!")
            end if

            return
        end if

        ! Convert segment to real month and year.
        if (month == 0) month = 12

        ! Go back to before the first date digit.
        if (.not. looking_at(" ", st)) then
            call regular_expression_search_backward("[^0-9]", found, status)

            if (.not. status%ok()) then
                call goto_character(original_point)
                return
            end if
        end if

        call buffer_substring(point() + 1, point() + 1 + calendar_day_digit_width, digits, st)
        date%month = month
        date%day = leading_integer(digits)

        if (month_index < 1) then
            date%year = locals(k)%displayed_year - 1
        else if (month_index > 12) then
            date%year = locals(k)%displayed_year + 1
        else
            date%year = locals(k)%displayed_year
        end if

        call goto_character(original_point)
    end subroutine calendar_cursor_to_date

    ! The integer at the beginning of TEXT after blanks, 0 if there is
    ! none (string-to-number on a date's digits).  Not a port.
    integer function leading_integer(text) result(n)
        character(len=*), intent(in) :: text
        integer :: k

        n = 0
        k = verify(text, " ")
        if (k == 0) return

        do while (k <= len(text))
            if (scan(text(k:k), "0123456789") == 0) exit
            n = 10*n + iachar(text(k:k)) - iachar("0")
            k = k + 1
        end do
    end function leading_integer

    !> Return true if DATE is valid and is visible in the calendar window.
    !> Port of calendar.el:calendar-date-is-visible-p.
    logical function is_calendar_date_visible(date) result(visible)
        type(calendar_date_type), intent(in) :: date
        integer :: k, n

        visible = .false.
        if (.not. is_calendar_date_valid(date)) return
        k = calendar_local_index()
        n = calendar_interval(locals(k)%displayed_month, locals(k)%displayed_year, date%month, date%year)
        visible = -2 < n .and. n < locals(k)%total_months - 1
    end function is_calendar_date_visible

    !> Return the range M1, Y1 to M2, Y2 of the current calendar.  Port of
    !> calendar.el:calendar-get-month-range.
    subroutine calendar_get_month_range(m1, y1, m2, y2)
        integer, intent(out) :: m1, y1, m2, y2
        integer :: k

        k = calendar_local_index()
        y1 = locals(k)%displayed_year
        y2 = locals(k)%displayed_year
        m1 = locals(k)%displayed_month - 1
        m2 = mod(m1 - 1 + locals(k)%total_months, 12)
        if (m2 == 0) m2 = 12

        if (m1 > 1) then
            if (m2 < m1) y2 = y1 + 1
        else if (m1 == 0) then
            m1 = 12
            y1 = y1 - 1
        end if
    end subroutine calendar_get_month_range

    !> Return the range FIRST to LAST of dates of the current calendar.
    !> The day in the date is 1 by default.  If optional argument FULL is
    !> true, the day in the end date is the last day.  Port of
    !> calendar.el:calendar-get-date-range.
    subroutine calendar_get_date_range(first, last, full)
        type(calendar_date_type), intent(out) :: first, last
        logical, intent(in), optional :: full
        integer :: m1, y1, m2, y2

        call calendar_get_month_range(m1, y1, m2, y2)
        first = calendar_date_type(m1, 1, y1)
        last = calendar_date_type(m2, 1, y2)

        if (present(full)) then
            if (full) last%day = calendar_last_day_of_month(m2, y2)
        end if
    end subroutine calendar_get_date_range

    !> The marked date of the current calendar buffer (the first element
    !> of `calendar_mark_ring'), `calendar_date_type()' for none.
    !> Not a port.
    function calendar_marked_date() result(date)
        type(calendar_date_type) :: date
        integer :: k

        date = calendar_date_type()
        k = calendar_local_index()
        if (.not. allocated(locals(k)%mark_ring)) return
        if (size(locals(k)%mark_ring) > 0) date = locals(k)%mark_ring(1)
    end function calendar_marked_date

    !> Save the current buffer's displayed month and year and its number
    !> of months in SAVED and set them to MONTH, YEAR and TOTAL_MONTHS,
    !> until `restore_calendar_display' (as upstream binds displayed-month,
    !> displayed-year and calendar-total-months).  Not a port.
    subroutine set_calendar_display(month, year, total_months, saved)
        integer, intent(in) :: month, year, total_months
        type(calendar_display_type), intent(out) :: saved
        integer :: k

        k = calendar_local_index()
        saved%entry = k
        saved%serial = locals(k)%serial
        saved%month = locals(k)%displayed_month
        saved%year = locals(k)%displayed_year
        saved%total_months = locals(k)%total_months
        locals(k)%displayed_month = month
        locals(k)%displayed_year = year
        locals(k)%total_months = total_months
    end subroutine set_calendar_display

    !> Restore the displayed month, year and number of months that
    !> `set_calendar_display' saved in SAVED, in the buffer they were saved
    !> from.  Not a port.
    subroutine restore_calendar_display(saved)
        type(calendar_display_type), intent(in) :: saved

        if (saved%entry < 1 .or. saved%entry > size(locals)) return
        if (locals(saved%entry)%serial /= saved%serial) return
        locals(saved%entry)%displayed_month = saved%month
        locals(saved%entry)%displayed_year = saved%year
        locals(saved%entry)%total_months = saved%total_months
    end subroutine restore_calendar_display

    !> Return true if [M1, M2] intersects [MON, MON+N].  Port of
    !> calendar.el:calendar--month-overlap-p.
    pure logical function is_calendar_month_overlap(m1, m2, mon, n) result(overlap)
        integer, intent(in) :: m1, m2, mon, n
        integer :: last

        if (n == 0) then
            overlap = m1 <= mon .and. mon <= m2
            return
        end if

        last = mon + n

        if (last <= 12) then
            overlap = max(m1, mon) <= min(m2, last)
        else
            overlap = max(m1, 1) <= min(m2, last - 12) .or. max(m1, mon) <= min(m2, 12)
        end if
    end function is_calendar_month_overlap

    !> Set VISIBLE to true if MONTH is visible in the calendar.
    !> Since the calendar shows at most 12 months, each month, if visible, is
    !> associated with only one year: YEAR is the year associated with
    !> MONTH (0 if MONTH is not visible).  Port of
    !> calendar.el:calendar-month-visible-p (N omitted; the list of years
    !> for MONTH and the next N months is Deferred, see calendar_mod's
    !> status file).
    subroutine get_calendar_month_visible_year(month, year, visible)
        integer, intent(in) :: month
        integer, intent(out) :: year
        logical, intent(out) :: visible
        integer :: m1, y1, m2, y2

        call calendar_get_month_range(m1, y1, m2, y2)
        year = 0

        if (y1 == y2) then
            if (is_calendar_month_overlap(m1, m2, month, 0)) year = y1
        else
            ! (push y2 r) then (push y1 r): the first year wins.
            if (is_calendar_month_overlap(1, m2, month, 0)) year = y2
            if (is_calendar_month_overlap(m1, 12, month, 0)) year = y1
        end if

        visible = year /= 0
    end subroutine get_calendar_month_visible_year

    !> DATES are the Gregorian dates of MONTH, DAY that are visible in the
    !> calendar window (empty if it is not visible).
    !> MONTH and DAY are in some non-Gregorian calendar system.  The functions
    !> TO_ABSOLUTE and FROM_ABSOLUTE convert that system to and from absolute,
    !> respectively.  The optional argument N is an integer indicating the
    !> first local month in the non-Gregorian local year and defaults to 1.
    !> Port of
    !> calendar.el:calendar-nongregorian-date-visible-p.
    recursive subroutine get_calendar_nongregorian_visible_dates(month, day, to_absolute, from_absolute, dates, n)
        integer, intent(in) :: month, day
        interface
            integer function to_absolute(date)
                import :: calendar_date_type
                implicit none(type, external)
                type(calendar_date_type), intent(in) :: date
            end function to_absolute

            function from_absolute(date) result(local)
                import :: calendar_date_type
                implicit none(type, external)
                integer, intent(in) :: date
                type(calendar_date_type) :: local
            end function from_absolute
        end interface
        type(calendar_date_type), allocatable, intent(out) :: dates(:)
        integer, intent(in), optional :: n
        type(calendar_date_type), allocatable :: local_dates(:)
        type(calendar_date_type) :: local_start, local_end, date
        integer :: m1, y1, m2, y2, first, i, local_y1, local_y2, local_m1, local_m2

        call calendar_get_month_range(m1, y1, m2, y2)
        ! Local date of first/last date in calendar window.
        local_start = from_absolute(calendar_absolute_from_gregorian(calendar_date_type(m1, 1, y1)))
        local_end = from_absolute(calendar_absolute_from_gregorian( &
                                  calendar_date_type(m2, calendar_last_day_of_month(m2, y2), y2)))
        ! Local year/month of first/last dates.
        local_y1 = local_start%year
        local_y2 = local_end%year
        local_m1 = local_start%month
        local_m2 = local_end%month
        first = 1
        if (present(n)) first = n
        allocate (local_dates(0))

        ! The local calendar:
        !    Year    : y1 ............. y1 y2 ............. y2
        ! Month (N=1): 1 ... ..... ... END 1 ... ..... ... END
        ! Month (N>1): N ... END 1 ... N-1 N ... END 1 ... N-1
        if (local_y1 == local_y2) then
            ! N > 1
            if ((local_m1 <= month .and. month <= local_m2) &
                .or. (local_m1 > local_m2 .and. (month >= local_m1 .or. month <= local_m2))) then
                local_dates = [calendar_date_type(month, day, local_y1), local_dates]
            end if
        else if (local_y1 < local_y2) then
            ! In a large calendar range (e.g. 12 months), the same local
            ! month may appear twice and there may be three local years.
            ! Cf holiday-chinese.
            do i = 0, local_y2 - local_y1 - 2
                local_dates = [calendar_date_type(month, day, local_y1 + i + 1), local_dates]
            end do

            if (local_m1 >= first) then
                if (month >= local_m1 .or. month < first) then
                    local_dates = [calendar_date_type(month, day, local_y1), local_dates]
                end if

                if (month <= local_m2) local_dates = [calendar_date_type(month, day, local_y2), local_dates]
            else
                ! m1 < N
                if (local_m1 <= month .and. month < first) then
                    local_dates = [calendar_date_type(month, day, local_y1), local_dates]
                end if

                if (month >= first .or. month <= local_m2) then
                    local_dates = [calendar_date_type(month, day, local_y2), local_dates]
                end if
            end if
        end if

        allocate (dates(0))

        do i = 1, size(local_dates)
            date = calendar_gregorian_from_absolute(to_absolute(local_dates(i)))
            if (is_calendar_date_visible(date)) dates = [dates, date]
        end do
    end subroutine get_calendar_nongregorian_visible_dates

    !> Set mode line to TEXT, centered, surrounded by dashes.  Port of
    !> calendar.el:calendar-set-mode-line (in a buffer visiting no file;
    !> the mode line shows `%-' as dashes to its end and `%%' as `%', as
    !> format-mode-line does; the layout for a buffer visiting a file is
    !> Deferred, and the width under --daemon is Not applicable).
    recursive subroutine calendar_set_mode_line(text)
        character(len=*), intent(in) :: text
        type(string_type) :: items(1)
        type(header_line_format_type) :: format
        character(len=:), allocatable :: spread, shown
        integer :: width, k
        type(status_type) :: st

        ! As per doc of window-width, total visible mode-line length.
        width = window_total_width(status=st)
        if (.not. st%ok()) return
        items(1)%text = text
        spread = calendar_string_spread(items, "-", width)
        shown = ""
        k = 1

        do while (k <= len(spread))
            if (spread(k:k) == "%" .and. k < len(spread)) then
                if (spread(k + 1:k + 1) == "%") then
                    shown = shown//"%"
                    k = k + 2
                    cycle
                else if (spread(k + 1:k + 1) == "-") then
                    shown = shown//repeat("-", max(0, width - int(string_width(shown))))
                    exit
                end if
            end if

            shown = shown//spread(k:k)
            k = k + 1
        end do

        call format%append(shown)
        buffers(current)%mode_line_format = format
        call force_mode_line_update(.false.)
    end subroutine calendar_set_mode_line

    !> Switch to BUFFER and execute the forms in BODY.
    !> First creates or erases BUFFER as needed.  Leaves BUFFER read-only,
    !> with disabled undo.  Leaves point at `point_minimum', displays BUFFER.
    !> Port of calendar.el:calendar-in-read-only-buffer (its BODY
    !> in every use: `calendar_set_mode_line' with MODE_LINE, then TEXT
    !> inserted).
    recursive subroutine calendar_in_read_only_buffer(buffer, mode_line, text, status)
        character(len=*), intent(in) :: buffer, mode_line, text
        type(status_type), intent(out) :: status
        type(handle_type) :: h, window, old_window, old_buffer
        type(status_type) :: st

        call get_or_make_buffer(buffer, h, status)
        if (.not. status%ok()) return
        call set_buffer(h, status)
        if (.not. status%ok()) return

        if (.not. is_derived_mode(major_modes%special_mode)) then
            call special_mode(status)
            if (.not. status%ok()) return
        end if

        buffers(current)%read_only = .false.
        buffers(current)%undo_enabled = .false.
        call erase_buffer(status)
        if (.not. status%ok()) return
        call display_buffer(h, window, status)
        if (.not. status%ok()) return

        if (is_window_live(window)) then
            ! with-selected-window
            old_window = selected_window()
            old_buffer = current_buffer()
            call select_window(window, status, norecord=.true.)

            if (status%ok()) then
                call calendar_set_mode_line(mode_line)
                call insert(text, status)
                if (status%ok()) call goto_character(point_minimum())
            end if

            if (is_window_live(old_window)) call select_window(old_window, st, norecord=.true.)
            if (is_buffer_live(old_buffer)) call set_buffer(old_buffer, st)
            if (.not. status%ok()) return
        end if

        call restore_buffer_modified(.false.)
        buffers(current)%read_only = .true.
    end subroutine calendar_in_read_only_buffer

    ! ------------------------------------------------------------------
    ! Reading dates
    ! ------------------------------------------------------------------

    !> Return an integer VALUE read from the minibuffer.
    !> PROMPT and DEFAULT build
    !> the actual prompt as format-prompt does.  TEST is called with a
    !> single value (the integer
    !> the user entered) and it should return true if that value is a valid choice;
    !> reading is repeated until it does.
    !> DEFAULT is the default value to use (absent: none).  Text that is
    !> not an integer is a `wrong_type_argument' error.  Port of
    !> calendar.el:calendar-read-sexp (integers only).
    recursive subroutine calendar_read_expression(prompt, test, value, status, default)
        character(len=*), intent(in) :: prompt
        interface
            logical function test(x)
                implicit none(type, external)
                integer, intent(in) :: x
            end function test
        end interface
        integer, intent(out) :: value
        type(status_type), intent(out) :: status
        integer, intent(in), optional :: default
        character(len=:), allocatable :: input, default_text
        character(len=11) :: digits
        integer :: ios

        value = 0

        if (present(default)) then
            write (digits, "(i0)") default
            default_text = trim(digits)
        else
            default_text = "nil"
        end if

        do
            call read_from_minibuffer(format_prompt(prompt, default_text), input, status, history="minibuffer_history", &
                                      default=default_text)
            if (.not. status%ok()) return
            if (len_trim(input) == 0) input = default_text
            read (input, *, iostat=ios) value

            if (ios /= 0 .or. verify(trim(adjustl(input)), "+-0123456789") /= 0) then
                call set_error(status, WRONG_TYPE_ARGUMENT_CODE, "expected integer, got "//trim(adjustl(input)))
                return
            end if

            if (test(value)) return
        end do
    end subroutine calendar_read_expression

    !> Prompt for Gregorian date.  Return a date (month day year).
    !> If optional NODAY is true, does not ask for day, but just returns
    !> (month 1 year).  DEFAULT_DATE defaults to today's date.  Port of
    !> calendar.el:calendar-read-date (upstream's NODAY other than t,
    !> which returns (month year), is the same date's month and year).
    recursive subroutine calendar_read_date(date, status, noday, default_date)
        type(calendar_date_type), intent(out) :: date
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: noday
        type(calendar_date_type), intent(in), optional :: default_date
        type(calendar_date_type) :: default
        type(completion_table_type) :: table
        type(value_type) :: names(12)
        character(len=:), allocatable :: defmon, answer
        character(len=16) :: prompt
        integer :: year, month, defday, last, k
        logical :: saved_ignore_case

        date = calendar_date_type()

        if (present(default_date)) then
            default = default_date
        else
            default = calendar_current_date()
        end if

        call calendar_read_expression("Year (>0)", is_positive, year, status, default%year)
        if (.not. status%ok()) return
        defmon = trim(calendar_month_names(default%month))

        do k = 1, 12
            names(k) = value_type(text=trim(calendar_month_names(k)))
        end do

        ! (completion-table-case-fold ...), with the metadata
        ! ((category . calendar-month) (display-sort-function . identity)).
        table = completion_list_table(make_list(names), "calendar_month")
        table%unsorted = .true.
        saved_ignore_case = completion_ignore_case%value
        completion_ignore_case%value = .true.
        call completing_read(format_prompt("Month name", defmon), table, answer, status, &
                             require_match=REQUIRE_MATCH_YES, default=defmon)
        completion_ignore_case%value = saved_ignore_case
        if (.not. status%ok()) return
        month = 0

        do k = 1, 12
            if (lower(trim(calendar_month_names(k))) == lower(answer)) then
                month = k
                exit
            end if
        end do

        if (month == 0) month = default%month
        defday = default%day
        last = calendar_last_day_of_month(month, year)

        if (present(noday)) then
            if (noday) then
                date = calendar_date_type(month, 1, year)
                return
            end if
        end if

        ! Don't offer today's day as default if it's not valid for the
        ! chosen month/year.
        write (prompt, '("Day (1-", i0, ")")') last

        if (defday <= last) then
            call calendar_read_expression(trim(prompt), is_day, date%day, status, defday)
        else
            call calendar_read_expression(trim(prompt), is_day, date%day, status)
        end if

        if (.not. status%ok()) return
        date%month = month
        date%year = year

    contains

        logical function is_positive(x)
            integer, intent(in) :: x

            is_positive = x > 0
        end function is_positive

        logical function is_day(x)
            integer, intent(in) :: x

            is_day = 0 < x .and. x <= last
        end function is_day
    end subroutine calendar_read_date

    ! S in lower case (ASCII).  Not a port.
    pure function lower(s) result(l)
        character(len=*), intent(in) :: s
        character(len=len(s)) :: l
        integer :: k

        l = s

        do k = 1, len(s)
            if (s(k:k) >= "A" .and. s(k:k) <= "Z") l(k:k) = achar(iachar(s(k:k)) + 32)
        end do
    end function lower

    ! ------------------------------------------------------------------
    ! Commands
    ! ------------------------------------------------------------------

    !> Display a three-month calendar centered around MONTH and YEAR.
    !> Port of calendar.el:calendar-other-month (no EVENT).
    recursive subroutine calendar_other_month(month, year, status)
        integer, intent(in) :: month, year
        type(status_type), intent(out) :: status
        type(calendar_date_type) :: old_date, today, first

        status = status_type()
        if (month == calendar_displayed_month() .and. year == calendar_displayed_year()) return
        call calendar_cursor_to_date(old_date, status)
        if (.not. status%ok()) return
        today = calendar_current_date()
        call calendar_generate_window(status, month, year)
        if (.not. status%ok()) return

        if (is_calendar_date_visible(old_date)) then
            call calendar_cursor_to_visible_date(old_date)
        else if (is_calendar_date_visible(today)) then
            call calendar_cursor_to_visible_date(today)
        else
            first = calendar_date_type(month, 1, year)
            call calendar_cursor_to_visible_date(first)
        end if

        call calendar_update_mode_line()
    end subroutine calendar_other_month

    ! The command `calendar_other_month': reads the month and year.
    ! Not a port.
    recursive subroutine execute_calendar_other_month(status)
        type(status_type), intent(out) :: status
        type(calendar_date_type) :: date

        call calendar_read_date(date, status, noday=.true.)
        if (.not. status%ok()) return
        call calendar_other_month(date%month, date%year, status)
    end subroutine execute_calendar_other_month

    !> Mark the date under the cursor, or jump to marked date.
    !> With no prefix argument, push current date onto marked date ring.
    !> With argument ARGUMENT, jump to mark, pop it, and put point at end of ring.
    !> Port of calendar.el:calendar-set-mark (no EVENT).
    recursive subroutine calendar_set_mark(argument, status)
        logical, intent(in) :: argument
        type(status_type), intent(out) :: status
        type(calendar_date_type) :: date, mark
        integer :: k

        call calendar_cursor_to_date(date, status, .true.)
        if (.not. status%ok()) return
        k = calendar_local_index()

        if (argument) then
            if (.not. allocated(locals(k)%mark_ring)) allocate (locals(k)%mark_ring(0))

            if (size(locals(k)%mark_ring) == 0) then
                call set_error(status, GENERIC_ERROR_CODE, "No mark set in this buffer")
                return
            end if

            mark = locals(k)%mark_ring(1)
            call calendar_goto_date(mark, status)
            if (.not. status%ok()) return
            k = calendar_local_index()
            locals(k)%mark_ring = [locals(k)%mark_ring(2:), date]
        else
            if (.not. allocated(locals(k)%mark_ring)) allocate (locals(k)%mark_ring(0))
            locals(k)%mark_ring = [date, locals(k)%mark_ring]

            ! Since the top of the mark ring is the marked date in the
            ! calendar, the mark ring in the calendar is one longer than
            ! in other buffers to get the same effect.
            if (size(locals(k)%mark_ring) > mark_ring_maximum + 1) then
                locals(k)%mark_ring = locals(k)%mark_ring(1:mark_ring_maximum + 1)
            end if

            call message("Mark set")
        end if
    end subroutine calendar_set_mark

    ! The command `calendar_set_mark': ARGUMENT is the prefix argument.
    ! Not a port.
    recursive subroutine execute_calendar_set_mark(status)
        type(status_type), intent(out) :: status

        call calendar_set_mark(current_prefix_argument%kind /= PREFIX_NONE, status)
    end subroutine execute_calendar_set_mark

    !> Exchange the current cursor position with the marked date.  Port of
    !> calendar.el:calendar-exchange-point-and-mark.
    recursive subroutine calendar_exchange_point_and_mark(status)
        type(status_type), intent(out) :: status
        type(calendar_date_type) :: date, mark
        integer :: k

        k = calendar_local_index()
        mark = calendar_date_type()

        if (allocated(locals(k)%mark_ring)) then
            if (size(locals(k)%mark_ring) > 0) mark = locals(k)%mark_ring(1)
        end if

        call calendar_cursor_to_date(date, status, .true.)
        if (.not. status%ok()) return

        if (mark%month == 0) then
            call set_error(status, GENERIC_ERROR_CODE, "No mark set in this buffer")
            return
        end if

        locals(k)%mark_ring(1) = date
        call calendar_goto_date(mark, status)
    end subroutine calendar_exchange_point_and_mark

    !> Count the number of days (inclusive) between point and the mark.
    !> Port of calendar.el:calendar-count-days-region.
    recursive subroutine calendar_count_days_region(status)
        type(status_type), intent(out) :: status
        type(calendar_date_type) :: date
        integer :: days, k
        character(len=40) :: text

        call calendar_cursor_to_date(date, status, .true.)
        if (.not. status%ok()) return
        k = calendar_local_index()
        days = -1

        if (allocated(locals(k)%mark_ring)) then
            if (size(locals(k)%mark_ring) > 0) then
                days = calendar_absolute_from_gregorian(date) - calendar_absolute_from_gregorian(locals(k)%mark_ring(1))
                days = 1 + abs(days)
            end if
        end if

        if (days < 0) then
            call set_error(status, GENERIC_ERROR_CODE, "No mark set in this buffer")
            return
        end if

        if (days > 1) then
            write (text, '("Region has ", i0, " days (inclusive)")') days
        else
            write (text, '("Region has ", i0, " day (inclusive)")') days
        end if

        call message(trim(text))
    end subroutine calendar_count_days_region

    !> Not implemented.  Port of calendar.el:calendar-not-implemented.
    recursive subroutine calendar_not_implemented(status)
        type(status_type), intent(out) :: status
        type(value_type) :: binding
        type(status_type) :: st

        status = status_type()
        call lookup_key(make_keymap_reference(current_global_map()), this_command_key_events(), binding, st)
        call set_error(status, GENERIC_ERROR_CODE, readable_text(binding)//" not available in the calendar")
    end subroutine calendar_not_implemented

    !> Delete all diary/holiday marks/highlighting from the calendar.
    !> Port of calendar.el:calendar-unmark.
    recursive subroutine calendar_unmark(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: buffer, old
        type(handle_type), allocatable :: ovs(:)
        integer :: k

        calendar_mark_holidays_variable = .false.
        calendar_mark_diary_entries = .false.
        status = status_type()
        buffer = calendar_get_buffer()
        old = current_buffer()
        call set_buffer(buffer, status)
        if (.not. status%ok()) return
        call overlays_in(point_minimum(), point_maximum(), ovs)

        do k = 1, size(ovs)
            call delete_overlay(ovs(k))
        end do

        call set_buffer(old, status)
    end subroutine calendar_unmark

    !> Mark DATE in the calendar window with FACE, or with MARK, a
    !> single-character string, after the date.  The mark defaults to
    !> the `diary' face.  Port of calendar.el:calendar-mark-visible-date
    !> (attribute lists are Deferred with `calendar_make_temporary_face').
    recursive subroutine calendar_mark_visible_date(date, status, face, mark)
        type(calendar_date_type), intent(in) :: date
        type(status_type), intent(out) :: status
        type(named_face_type), intent(in), optional :: face
        character(len=*), intent(in), optional :: mark
        type(handle_type) :: buffer, old, ov
        integer(int64) :: original_point
        logical :: single
        type(status_type) :: st

        status = status_type()
        if (.not. is_calendar_date_valid(date)) return
        buffer = calendar_get_buffer()
        old = current_buffer()
        call set_buffer(buffer, status)
        if (.not. status%ok()) return
        original_point = point()
        call calendar_cursor_to_visible_date(date)
        single = .false.

        if (present(mark)) then
            single = character_count(mark) == 1
        end if

        if (present(face)) then
            ! Face.
            call make_overlay(ov, point() - 1, point() + 1, status)
            if (status%ok()) call overlay_put(ov, "face", value_type(name=trim(face%name)), status)
        else if (single) then
            ! Single-character mark, goes after the date.
            call make_overlay(ov, point() + 1, point() + 2, status)
            if (status%ok()) call overlay_put(ov, "display", value_type(text=mark), status)
        else
            call make_overlay(ov, point() - 1, point() + 1, status)
            if (status%ok()) call overlay_put(ov, "face", value_type(name=trim(diary%name)), status)
        end if

        call goto_character(original_point)
        call set_buffer(old, st)
    end subroutine calendar_mark_visible_date

    !> Replace the date under the cursor in the calendar window with asterisks.
    !> You might want to add this function to `calendar_today_visible_hook'.
    !> Port of calendar.el:calendar-star-date.
    recursive subroutine calendar_star_date(status)
        type(status_type), intent(out) :: status
        type(handle_type), allocatable :: ovs(:)
        type(handle_type) :: ov
        type(value_type) :: v
        integer :: k

        status = status_type()
        call overlays_at(point(), ovs)

        do k = 1, size(ovs)
            v = overlay_get(ovs(k), "calendar_star")
            if (allocated(v%a)) return
        end do

        call make_overlay(ov, point() - 1, point(), status)
        if (.not. status%ok()) return
        call overlay_put(ov, "display", value_type(text="*"), status)
        call overlay_put(ov, "calendar_star", value_type(.true.), status)
        ! Use two overlays so as not to mess up
        ! calendar-cursor-to-nearest-date (and calendar-forward-day).
        call make_overlay(ov, point(), point() + 1, status)
        if (.not. status%ok()) return
        call overlay_put(ov, "display", value_type(text="*"), status)
        call overlay_put(ov, "calendar_star", value_type(.true.), status)
    end subroutine calendar_star_date

    !> Mark the date under the cursor in the calendar window.
    !> The date is marked with the `calendar_today' face.  You might want to add
    !> this function to `calendar_today_visible_hook'.  Port of
    !> calendar.el:calendar-mark-today (`calendar_today_marker' is
    !> Deferred: today is marked with its default, the face).
    recursive subroutine calendar_mark_today(status)
        type(status_type), intent(out) :: status
        type(calendar_date_type) :: date

        call calendar_cursor_to_date(date, status)
        if (.not. status%ok()) return
        call calendar_mark_visible_date(date, status, face=calendar_today)
    end subroutine calendar_mark_today

    !> Show day number in year/days remaining in year for date under the
    !> cursor.  Port of calendar.el:calendar-print-day-of-year.
    recursive subroutine calendar_print_day_of_year(status)
        type(status_type), intent(out) :: status
        type(calendar_date_type) :: date

        call calendar_cursor_to_date(date, status, .true.)
        if (.not. status%ok()) return
        call message(calendar_day_of_year_string(date))
    end subroutine calendar_print_day_of_year

    !> Set the style of calendar and diary dates to STYLE.
    !> The valid styles are described in the documentation of `calendar_date_style'.
    !> Port of calendar.el:calendar-set-date-style (`diary_date_insertion_form'
    !> follows the style).
    recursive subroutine calendar_set_date_style(style, status)
        integer, intent(in) :: style
        type(status_type), intent(out) :: status

        select case (style)
        case (CALENDAR_DATE_STYLE_AMERICAN, CALENDAR_DATE_STYLE_EUROPEAN, CALENDAR_DATE_STYLE_ISO)
            calendar_date_style = style
        case default
            calendar_date_style = CALENDAR_DATE_STYLE_AMERICAN
        end select

        call set_style_diary_date_forms()
        call calendar_redraw(status)
    end subroutine calendar_set_date_style

    ! The command `calendar_set_date_style': reads the style.  Not a port.
    recursive subroutine execute_calendar_set_date_style(status)
        type(status_type), intent(out) :: status
        type(value_type) :: styles(3)
        character(len=:), allocatable :: answer

        styles(1) = value_type(text="american")
        styles(2) = value_type(text="european")
        styles(3) = value_type(text="iso")
        call completing_read(format_prompt("Date style", "american"), completion_list_table(make_list(styles)), &
                             answer, status, require_match=REQUIRE_MATCH_YES, default="american")
        if (.not. status%ok()) return

        select case (answer)
        case ("european")
            call calendar_set_date_style(CALENDAR_DATE_STYLE_EUROPEAN, status)
        case ("iso")
            call calendar_set_date_style(CALENDAR_DATE_STYLE_ISO, status)
        case default
            call calendar_set_date_style(CALENDAR_DATE_STYLE_AMERICAN, status)
        end select
    end subroutine execute_calendar_set_date_style

    ! ------------------------------------------------------------------
    ! The diary's options
    ! ------------------------------------------------------------------

    !> Add the pseudo-pattern of PART1 to PART8 (each a regular expression
    !> or one of the keywords month, day, year, monthname and dayname) at
    !> the end of SELF; BACKUP makes the date recognizer back up to the
    !> beginning of the current word.  Not a port.
    subroutine append_diary_date_form(self, part1, part2, part3, part4, part5, part6, part7, part8, backup)
        class(diary_date_forms_type), intent(inout) :: self
        character(len=*), intent(in) :: part1
        character(len=*), intent(in), optional :: part2, part3, part4, part5, part6, part7, part8
        logical, intent(in), optional :: backup
        type(diary_date_form_type) :: form

        if (present(backup)) form%backup = backup
        call add_part(part1)
        if (present(part2)) call add_part(part2)
        if (present(part3)) call add_part(part3)
        if (present(part4)) call add_part(part4)
        if (present(part5)) call add_part(part5)
        if (present(part6)) call add_part(part6)
        if (present(part7)) call add_part(part7)
        if (present(part8)) call add_part(part8)
        if (.not. allocated(self%entries)) allocate (self%entries(0))
        self%entries = [self%entries, form]

    contains

        subroutine add_part(part)
            character(len=*), intent(in) :: part

            form%count = form%count + 1
            form%parts(form%count) = part
        end subroutine add_part
    end subroutine append_diary_date_form

    !> Remove all the pseudo-patterns of SELF.  Not a port.
    subroutine clear_diary_date_forms(self)
        class(diary_date_forms_type), intent(inout) :: self

        if (allocated(self%entries)) deallocate (self%entries)
        allocate (self%entries(0))
    end subroutine clear_diary_date_forms

    ! The three styles' pseudo-patterns of calendar.el.  Not a port.
    subroutine set_default_diary_date_forms()
        call diary_iso_date_forms%clear()
        call diary_iso_date_forms%append("month", "[-/]", "day", "[^-/0-9]")
        call diary_iso_date_forms%append("year", "[-/]", "month", "[-/]", "day", "[^0-9]")
        ! Cannot allow [-/] as separators here, since it would also match
        ! the first element (bug#7377).
        call diary_iso_date_forms%append("monthname", " *", "day", "[^-0-9]")
        call diary_iso_date_forms%append("year", " *", "monthname", " *", "day", "[^0-9]")
        call diary_iso_date_forms%append("dayname", "\W")
        call diary_american_date_forms%clear()
        call diary_american_date_forms%append("month", "/", "day", "[^/0-9]")
        call diary_american_date_forms%append("month", "/", "day", "/", "year", "[^0-9]")
        call diary_american_date_forms%append("monthname", " *", "day", "[^,0-9]")
        call diary_american_date_forms%append("monthname", " *", "day", ", *", "year", "[^0-9]")
        call diary_american_date_forms%append("dayname", "\W")
        call diary_european_date_forms%clear()
        call diary_european_date_forms%append("day", "/", "month", "[^/0-9]")
        call diary_european_date_forms%append("day", "/", "month", "/", "year", "[^0-9]")
        call diary_european_date_forms%append("day", " *", "monthname", "\W+\<\([^*0-9]\|\([0-9]+[:.aApP]\)\)", &
                                              backup=.true.)
        call diary_european_date_forms%append("day", " *", "monthname", " *", "year", "[^0-9:.aApP]")
        call diary_european_date_forms%append("dayname", "\W")
    end subroutine set_default_diary_date_forms

    ! Set `diary_date_forms' to the pseudo-patterns of `calendar_date_style'
    ! (calendar-set-date-style's setq of diary-date-forms).  Not a port.
    subroutine set_style_diary_date_forms()
        select case (calendar_date_style)
        case (CALENDAR_DATE_STYLE_ISO)
            diary_date_forms = diary_iso_date_forms
        case (CALENDAR_DATE_STYLE_EUROPEAN)
            diary_date_forms = diary_european_date_forms
        case default
            diary_date_forms = diary_american_date_forms
        end select
    end subroutine set_style_diary_date_forms

    !> Give `diary_date_forms' the pseudo-patterns of `calendar_date_style'
    !> unless it has its own (the default value of calendar.el's
    !> diary-date-forms, computed when the diary is first used).
    !> Not a port.
    subroutine ensure_diary_date_forms()
        if (.not. allocated(diary_date_forms%entries)) call set_style_diary_date_forms()
    end subroutine ensure_diary_date_forms

    ! Give `diary_file' and `diary_nonmarking_symbol' their default values
    ! unless they have their own.  Not a port.
    subroutine ensure_diary_file()
        if (.not. allocated(diary_file)) diary_file = data_directory()//"diary"
        if (.not. allocated(diary_nonmarking_symbol)) diary_nonmarking_symbol = "&"
    end subroutine ensure_diary_file

    ! ------------------------------------------------------------------
    ! Definitions
    ! ------------------------------------------------------------------

    !> The faces of calendar.el.  Not a port.
    subroutine define_faces()
        type(named_face_type), pointer :: f
        integer, parameter :: C = FACE_CLASS_COLOR, L = FACE_BACKGROUND_LIGHT, D = FACE_BACKGROUND_DARK

        f => calendar_today
        call f%define("calendar_today", "Face for indicating today's date in the calendar.")
        call f%specification_entry(display_when(), face_attributes(underline=.true.))
        f => diary
        call f%define("diary", "Face for highlighting diary entries."//achar(10)// &
                      "Used to mark diary entries in the calendar (see `diary_entry_marker'),"//achar(10)// &
                      "and to highlight the date header in the fancy diary.")
        call f%specification_entry(display_when(classes=[C], minimum_colors=88, backgrounds=[L]), &
                                   face_attributes(foreground="red1"))
        call f%specification_entry(display_when(classes=[C], backgrounds=[L]), face_attributes(foreground="red"))
        call f%specification_entry(display_when(classes=[C], minimum_colors=88, backgrounds=[D]), &
                                   face_attributes(foreground="yellow1"))
        call f%specification_entry(display_when(classes=[C], backgrounds=[D]), face_attributes(foreground="yellow"))
        call f%specification_entry(display_when(), face_attributes(weight="bold"))
        f => holiday
        call f%define("holiday", "Face for indicating in the calendar dates that have holidays."//achar(10)// &
                      "See `calendar_holiday_marker'.")
        call f%specification_entry(display_when(classes=[C], backgrounds=[L]), face_attributes(background="pink"))
        call f%specification_entry(display_when(classes=[C], backgrounds=[D]), face_attributes(background="chocolate4"))
        call f%specification_entry(display_when(), face_attributes(inverse_video=.true.))
        f => calendar_weekday_header
        call f%define("calendar_weekday_header", "Face used for weekday column headers in the calendar."//achar(10)// &
                      "See also the face `calendar_weekend_header'.")
        call f%specification_entry(display_when(), face_attributes(inherit="font_lock_constant_face"))
        f => calendar_weekend_header
        call f%define("calendar_weekend_header", "Face used for weekend column headers in the calendar."//achar(10)// &
                      "See also the face `calendar_weekday_header'.")
        call f%specification_entry(display_when(), face_attributes(inherit="font_lock_comment_face"))
        f => calendar_month_header
        call f%define("calendar_month_header", "Face used for month headers in the calendar.")
        call f%specification_entry(display_when(), face_attributes(inherit="font_lock_function_name_face"))
    end subroutine define_faces

    !> Define Calendar mode, its keymap, faces and commands, and the
    !> options' defaults.  Not a port.
    subroutine initialize_calendar()
        type(status_type) :: st
        integer :: d
        character(len=1) :: digit
        type(handle_type) :: map

        if (.not. allocated(calendar_weekend_days)) calendar_weekend_days = [0, 6]

        if (.not. allocated(calendar_date_echo_text)) then
            calendar_date_echo_text = "mouse-2: general menu"//achar(10)//"mouse-3: menu for this date"
        end if

        if (.not. allocated(calendar_intermonth_header)) calendar_intermonth_header = ""
        if (.not. allocated(calendar_intermonth_text)) calendar_intermonth_text = ""
        call ensure_diary_file()
        call set_default_diary_date_forms()
        call ensure_locals()
        call append_locals_clearer(clear_calendar_locals)
        call define_faces()
        call major_modes%calendar_mode%define("calendar_mode", "Calendar", calendar_mode, calendar_mode_hook)
        ! Calendar mode is suitable only for specially formatted data.
        major_modes%calendar_mode%special = .true.
        calendar_mode_map = major_modes%calendar_mode%map
        map = calendar_mode_map
        ! (suppress-keymap map)
        call keymap_set(map, "<remap> <self_insert_command>", "undefined", st)
        call keymap_set(map, "-", "negative_argument", st)

        do d = 0, 9
            write (digit, "(i1)") d
            call keymap_set(map, digit, "digit_argument", st)
        end do

        call keymap_set(map, "<remap> <narrow_to_region>", "calendar_not_implemented", st)
        call keymap_set(map, "<remap> <mark_word>", "calendar_not_implemented", st)
        call keymap_set(map, "<remap> <mark_expression>", "calendar_not_implemented", st)
        call keymap_set(map, "<remap> <mark_paragraph>", "calendar_not_implemented", st)
        call keymap_set(map, "<remap> <mark_subprogram>", "calendar_not_implemented", st)
        call keymap_set(map, "<remap> <mark_whole_buffer>", "calendar_not_implemented", st)
        call keymap_set(map, "<remap> <mark_page>", "calendar_not_implemented", st)
        call keymap_set(map, "<remap> <downcase_region>", "calendar_not_implemented", st)
        call keymap_set(map, "<remap> <upcase_region>", "calendar_not_implemented", st)
        call keymap_set(map, "<remap> <cut_region>", "calendar_not_implemented", st)
        call keymap_set(map, "<remap> <copy_region_quietly>", "calendar_not_implemented", st)
        call keymap_set(map, "<remap> <capitalize_region>", "calendar_not_implemented", st)
        call keymap_set(map, "<remap> <write_region>", "calendar_not_implemented", st)
        call keymap_set(map, "<", "calendar_scroll_right", st)
        call keymap_set(map, "C-x <", "calendar_scroll_right", st)
        call keymap_set(map, "<prior>", "calendar_scroll_calendar_right", st)
        call keymap_set(map, "M-v", "calendar_scroll_calendar_right", st)
        call keymap_set(map, ">", "calendar_scroll_left", st)
        call keymap_set(map, "C-x >", "calendar_scroll_left", st)
        call keymap_set(map, "<next>", "calendar_scroll_calendar_left", st)
        call keymap_set(map, "C-v", "calendar_scroll_calendar_left", st)
        call keymap_set(map, "C-l", "calendar_recenter", st)
        call keymap_set(map, "C-b", "calendar_backward_day", st)
        call keymap_set(map, "C-p", "calendar_backward_week", st)
        call keymap_set(map, "M-{", "calendar_backward_month", st)
        call keymap_set(map, "{", "calendar_backward_month", st)
        call keymap_set(map, "C-x [", "calendar_backward_year", st)
        call keymap_set(map, "[", "calendar_backward_year", st)
        call keymap_set(map, "C-f", "calendar_forward_day", st)
        call keymap_set(map, "C-n", "calendar_forward_week", st)
        call keymap_set(map, "<left>", "calendar_backward_day", st)
        call keymap_set(map, "<up>", "calendar_backward_week", st)
        call keymap_set(map, "<right>", "calendar_forward_day", st)
        call keymap_set(map, "<down>", "calendar_forward_week", st)
        call keymap_set(map, "M-}", "calendar_forward_month", st)
        call keymap_set(map, "}", "calendar_forward_month", st)
        call keymap_set(map, "C-x ]", "calendar_forward_year", st)
        call keymap_set(map, "]", "calendar_forward_year", st)
        call keymap_set(map, "C-a", "calendar_beginning_of_week", st)
        call keymap_set(map, "C-e", "calendar_end_of_week", st)
        call keymap_set(map, "M-a", "calendar_beginning_of_month", st)
        call keymap_set(map, "M-e", "calendar_end_of_month", st)
        call keymap_set(map, "M-<", "calendar_beginning_of_year", st)
        call keymap_set(map, "M->", "calendar_end_of_year", st)
        call keymap_set(map, "C-@", "calendar_set_mark", st)
        ! Many people are used to typing C-SPC and getting C-@.
        call keymap_set(map, "C-SPC", "calendar_set_mark", st)
        call keymap_set(map, "C-x C-x", "calendar_exchange_point_and_mark", st)
        call keymap_set(map, "M-=", "calendar_count_days_region", st)
        call keymap_set(map, "g d", "calendar_goto_date", st)
        call keymap_set(map, "g D", "calendar_goto_day_of_year", st)
        call keymap_set(map, "SPC", "scroll_other_window", st)
        call keymap_set(map, "S-SPC", "scroll_other_window_down", st)
        call keymap_set(map, "DEL", "scroll_other_window_down", st)
        call keymap_set(map, "C-c C-l", "calendar_redraw", st)
        call keymap_set(map, ".", "calendar_goto_today", st)
        call keymap_set(map, "o", "calendar_other_month", st)
        call keymap_set(map, "q", "calendar_exit", st)
        call keymap_set(map, "u", "calendar_unmark", st)
        call keymap_set(map, "p d", "calendar_print_day_of_year", st)
        call keymap_set(map, "?", "calendar_goto_info_node", st)
        call register_keymap_name("calendar_mode_map", calendar_mode_map)
        commands%calendar%execute => execute_calendar
        commands%calendar_basic_setup%execute => execute_calendar_basic_setup
        commands%calendar_mode%execute => calendar_mode
        commands%calendar_redraw%execute => calendar_redraw
        commands%calendar_exit%execute => execute_calendar_exit
        commands%calendar_other_month%execute => execute_calendar_other_month
        commands%calendar_set_mark%execute => execute_calendar_set_mark
        commands%calendar_exchange_point_and_mark%execute => calendar_exchange_point_and_mark
        commands%calendar_count_days_region%execute => calendar_count_days_region
        commands%calendar_not_implemented%execute => calendar_not_implemented
        commands%calendar_unmark%execute => calendar_unmark
        commands%calendar_print_day_of_year%execute => calendar_print_day_of_year
        commands%calendar_set_date_style%execute => execute_calendar_set_date_style
        commands%calendar_goto_info_node%execute => calendar_goto_info_node
        call initialize_calendar_movement()
    end subroutine initialize_calendar
end module calendar_mod
