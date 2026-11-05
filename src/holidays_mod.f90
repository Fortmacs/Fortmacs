! holidays_mod.f90 -- holiday functions for the calendar package.
! Ported from GNU Emacs 31.1 lisp/calendar/holidays.el.
!
! Copyright (C) 1989-1990, 1992-1994, 1997, 2001-2026 Free Software
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

!> The holidays of the calendar: M-x holidays and M-x list_holidays, and
!> the calendar's h (`calendar_cursor_holidays'), x
!> (`calendar_mark_holidays') and a (`calendar_list_holidays').  A holiday
!> list is a `holidays_type', whose entries are `holiday_entry_type'
!> records (upstream's Lisp forms): a fixed or floating Gregorian date,
!> a date relative to Easter or Advent, a Julian or Islamic date, a
!> procedure computing the date in a year (upstream's sexp holiday), or
!> a procedure computing a list of holidays.  An entry gives the
!> `holiday_type' values (a date and its string) visible in the calendar
!> window.  The Julian and Islamic calendars' conversions are in the
!> submodules cal_julian_mod and cal_islam_mod.  What is not ported yet is
!> listed in docs/status/holidays_mod.md.
module holidays_mod
    use error_mod, only: status_type, set_error, FM_WRONG_TYPE_ARGUMENT
    use handle_mod, only: handle_type
    use buffer_mod, only: current_buffer, set_buffer, is_buffer_live
    use calendar_mod, only: calendar_date_type, operator(==), calendar_display_type, set_calendar_display, &
                            restore_calendar_display, calendar_get_month_range, calendar_month_name, &
                            calendar_date_string, calendar_current_date, calendar_read_date, calendar_read_expression, &
                            calendar_cursor_to_date, calendar_mark_visible_date, calendar_in_read_only_buffer, &
                            calendar_absolute_from_gregorian, calendar_gregorian_from_absolute, &
                            calendar_dayname_on_or_before, calendar_nth_named_absday, calendar_nth_named_day, &
                            calendar_last_day_of_month, calendar_increment_month, calendar_interval, &
                            is_calendar_date_visible, is_calendar_month_visible, is_calendar_nongregorian_date_visible, &
                            calendar_mode_map, holiday_buffer, holiday, calendar_mark_holidays_variable, &
                            calendar_mark_holidays_hook, calendar_list_holidays_hook, &
                            calendar_christian_all_holidays_flag, calendar_islamic_all_holidays_flag, &
                            calendar_day_number, calendar_displayed_month, calendar_displayed_year, &
                            calendar_total_months_local
    use xdisp_mod, only: message
    use commands_mod, only: commands
    use callint_mod, only: current_prefix_arg, PREFIX_NONE
    use keymap_mod, only: keymap_set
    use value_mod, only: value_type, make_list
    use minibuf_mod, only: completion_ignore_case
    use minibuf_table_mod, only: completion_list_table
    use minibuffer_mod, only: completing_read, REQUIRE_MATCH_YES
    use casefiddle_mod, only: capitalize_string
    use simple_mod, only: read_variable
    use frame_mod, only: selected_frame, frame_cols
    use search_mod, only: string_chars
    implicit none(type, external)
    private

    public :: holiday_type, holiday_entry_type, holidays_type, holiday_expression_procedure, holidays_procedure
    public :: calendar_holiday_list, calendar_list_holidays, holidays, list_holidays, calendar_check_holidays, &
              holiday_in_range, calendar_cursor_holidays, calendar_mark_holidays, holiday_fixed, holiday_float, &
              holiday_filter_visible_calendar, holiday_expression, holiday_after, holiday_advent, holiday_easter_etc, &
              holiday_easter_etc_absolute, holiday_greek_orthodox_easter, holiday_greek_orthodox_easter_absolute, &
              init_holidays, append_holiday

    !> The longest string a `holiday_entry_type' holds.
    integer, parameter, public :: HOLIDAY_STRING_MAX = 80

    !> The kinds of `holiday_entry_type': a fixed Gregorian date
    !> (upstream's holiday-fixed), the Nth DAYNAME after/before a Gregorian
    !> date (holiday-float), a date a procedure computes (holiday-sexp), a
    !> date relative to Easter (holiday-easter-etc), to Advent
    !> (holiday-advent) or to Greek Orthodox Easter
    !> (holiday-greek-orthodox-easter), a fixed Julian date
    !> (holiday-julian), a fixed Islamic date (holiday-islamic), and a list
    !> of holidays a procedure computes (any other entry, such as
    !> (holiday-islamic-new-year) or a conditional).
    integer, parameter, public :: HOLIDAY_ENTRY_FIXED = 1, HOLIDAY_ENTRY_FLOAT = 2, HOLIDAY_ENTRY_EXPRESSION = 3, &
                                  HOLIDAY_ENTRY_EASTER_ETC = 4, HOLIDAY_ENTRY_ADVENT = 5, &
                                  HOLIDAY_ENTRY_GREEK_ORTHODOX_EASTER = 6, HOLIDAY_ENTRY_JULIAN = 7, &
                                  HOLIDAY_ENTRY_ISLAMIC = 8, HOLIDAY_ENTRY_PROCEDURE = 9

    !> A holiday: its DATE (Gregorian) and its STRING (an item of
    !> upstream's holiday lists).
    type :: holiday_type
        type(calendar_date_type) :: date
        character(len=:), allocatable :: string
    end type holiday_type

    abstract interface
        !> The date of an expression holiday in YEAR (`calendar_date_type()'
        !> for none) and its STRING, which starts as the entry's string.
        !> Upstream's SEXP, an expression in the variable `year', and STRING,
        !> an expression in the variable `date'.  Not a port.
        recursive subroutine holiday_expression_procedure(year, date, string, status)
            import :: calendar_date_type, status_type
            implicit none(type, external)
            integer, intent(in) :: year
            type(calendar_date_type), intent(out) :: date
            character(len=:), allocatable, intent(inout) :: string
            type(status_type), intent(out) :: status
        end subroutine holiday_expression_procedure

        !> The HOLIDAYS of a computed entry that are visible in the calendar
        !> window (upstream's entry that returns a list of dates with their
        !> strings).  Not a port.
        recursive subroutine holidays_procedure(holidays, status)
            import :: holiday_type, status_type
            implicit none(type, external)
            type(holiday_type), allocatable, intent(out) :: holidays(:)
            type(status_type), intent(out) :: status
        end subroutine holidays_procedure
    end interface

    !> An entry of a holiday list (an element of upstream's `calendar_holidays'),
    !> written with a keyword constructor.  KIND is one of the
    !> HOLIDAY_ENTRY_* kinds, which use these components:
    !>
    !>     HOLIDAY_ENTRY_FIXED: MONTH, DAY, STRING
    !>     HOLIDAY_ENTRY_FLOAT: MONTH, DAYNAME, N, STRING and DAY (0 for
    !>         upstream's default)
    !>     HOLIDAY_ENTRY_EXPRESSION: EXPRESSION, STRING
    !>     HOLIDAY_ENTRY_EASTER_ETC, HOLIDAY_ENTRY_ADVENT,
    !>     HOLIDAY_ENTRY_GREEK_ORTHODOX_EASTER: N, STRING (a blank STRING
    !>         is upstream's call without arguments)
    !>     HOLIDAY_ENTRY_JULIAN, HOLIDAY_ENTRY_ISLAMIC: MONTH, DAY, STRING
    !>     HOLIDAY_ENTRY_PROCEDURE: PROCEDURE
    type :: holiday_entry_type
        integer :: kind = HOLIDAY_ENTRY_FIXED
        integer :: month = 0
        integer :: day = 0
        integer :: dayname = 0
        integer :: n = 0
        character(len=HOLIDAY_STRING_MAX) :: string = ""
        procedure(holiday_expression_procedure), pointer, nopass :: expression => null()
        procedure(holidays_procedure), pointer, nopass :: procedure => null()
    end type holiday_entry_type

    !> A holiday list: its ENTRIES in order.
    type :: holidays_type
        type(holiday_entry_type), allocatable :: entries(:)
    contains
        !> Add an entry, or the entries of another list, at the end.
        generic :: append => append_holiday_entry, append_holidays
        !> Remove all the entries.
        procedure :: clear => clear_holidays
        procedure, private :: append_holiday_entry, append_holidays
    end type holidays_type

    ! ------------------------------------------------------------------
    ! User options
    ! ------------------------------------------------------------------

    !> General holidays.  Default value is for the United States.
    !> See the documentation for `calendar_holidays' for details.  Port of
    !> holidays.el holiday-general-holidays.
    type(holidays_type), target, public :: holiday_general_holidays

    !> Local holidays.
    !> See the documentation for `calendar_holidays' for details.  Port of
    !> holidays.el holiday-local-holidays.
    type(holidays_type), target, public :: holiday_local_holidays

    !> User defined holidays.
    !> See the documentation for `calendar_holidays' for details.  Port of
    !> holidays.el holiday-other-holidays.
    type(holidays_type), target, public :: holiday_other_holidays

    !> Christian holidays.
    !> See the documentation for `calendar_holidays' for details.  Port of
    !> holidays.el holiday-christian-holidays.
    type(holidays_type), target, public :: holiday_christian_holidays

    !> Islamic holidays.
    !> See the documentation for `calendar_holidays' for details.  Port of
    !> holidays.el holiday-islamic-holidays.
    type(holidays_type), target, public :: holiday_islamic_holidays

    !> List of notable days for the command \[holidays].
    !>
    !> Additional holidays are easy to add to the list, just put them in the
    !> list `holiday_other_holidays' in your init file.  Similarly, by clearing
    !> any of `holiday_general_holidays', `holiday_local_holidays',
    !> `holiday_christian_holidays' or `holiday_islamic_holidays' in your
    !> init file, you can eliminate unwanted categories of holidays.
    !>
    !> The aforementioned variables control the holiday choices offered
    !> by the function `holiday_list' when it is called interactively.
    !>
    !> They also initialize the default value of `calendar_holidays',
    !> which is the default list of holidays used by the function
    !> `holiday_list' when it is called from a program.
    !>
    !> Entries on the list are `holiday_entry_type' records that give
    !> (possibly empty) lists of holidays (`holiday_type': a date and a
    !> string) in the three-month period centered around the displayed month
    !> of the displayed year.  The kinds of entries are:
    !>
    !>     HOLIDAY_ENTRY_FIXED (MONTH, DAY, STRING) is a fixed date on the
    !>                                Gregorian calendar
    !>     HOLIDAY_ENTRY_FLOAT (MONTH, DAYNAME, N, STRING, optional DAY) is
    !>                                the Nth DAYNAME (0 for Sunday, etc.)
    !>                                after/before Gregorian MONTH DAY.  N<0
    !>                                means count back from the end of the
    !>                                month.  Optional DAY defaults to 1 if
    !>                                N>0, and MONTH's last day otherwise.
    !>     HOLIDAY_ENTRY_ISLAMIC (MONTH, DAY, STRING) a fixed date on the
    !>                                Islamic calendar
    !>     HOLIDAY_ENTRY_JULIAN (MONTH, DAY, STRING) a fixed date on the
    !>                                Julian calendar
    !>     HOLIDAY_ENTRY_EXPRESSION (EXPRESSION, STRING) EXPRESSION is a
    !>                                procedure giving a Gregorian date in
    !>                                a year; if it gives a visible date,
    !>                                that's the holiday; if it gives no
    !>                                date, there's no holiday.  It may
    !>                                change STRING.
    !>
    !> For example, to add Bastille Day, celebrated in France on July 14, add
    !>
    !>     holiday_entry_type(kind=HOLIDAY_ENTRY_FIXED, month=7, day=14, string="Bastille Day")
    !>
    !> to the list.  To add Hurricane Supplication Day, celebrated in the Virgin
    !> Islands on the fourth Monday in July, add
    !>
    !>     holiday_entry_type(kind=HOLIDAY_ENTRY_FLOAT, month=7, dayname=1, n=4, &
    !>                        string="Hurricane Supplication Day")
    !>
    !> to the list (the last Monday would be specified with `-1' instead of `4').
    !> To add the Islamic feast celebrating Mohammed's birthday, use
    !>
    !>     holiday_entry_type(kind=HOLIDAY_ENTRY_ISLAMIC, month=3, day=12, string="Mohammed's Birthday")
    !>
    !> since the Islamic months are numbered from 1 starting with Muharram.
    !> To add Thomas Jefferson's birthday, April 2, 1743 (Julian), use
    !>
    !>     holiday_entry_type(kind=HOLIDAY_ENTRY_JULIAN, month=4, day=2, string="Jefferson's Birthday")
    !>
    !> To include a holiday conditionally, use the expression kind or a
    !> procedure.  For example, to include American presidential elections,
    !> which occur on the first Tuesday after the first Monday in November
    !> of years divisible by 4, add
    !>
    !>     holiday_entry_type(kind=HOLIDAY_ENTRY_EXPRESSION, expression=election, &
    !>                        string="US Presidential Election")
    !>
    !> with the procedure
    !>
    !>     subroutine election(year, date, string, status)
    !>         ...
    !>         if (mod(year, 4) == 0) date = calendar_gregorian_from_absolute(1 + &
    !>             calendar_dayname_on_or_before(1, 6 + calendar_absolute_from_gregorian( &
    !>             calendar_date_type(11, 1, year))))
    !>
    !> To include the phases of the moon, add
    !>
    !>     holiday_entry_type(kind=HOLIDAY_ENTRY_PROCEDURE, procedure=lunar_phases)
    !>
    !> to the holiday list, where `lunar_phases' is a procedure that you've
    !> written to return a (possibly empty) list of the relevant VISIBLE
    !> dates with descriptive strings.  Port of holidays.el calendar-holidays
    !> (made from the lists above when first used, unless the init file set
    !> it, as upstream's holidays.el is loaded after the init file).
    type(holidays_type), target, public :: calendar_holidays

    ! A holiday list a user can name at the "Ask" choice of
    ! `list_holidays': its NAME and the list.
    type :: named_holidays_type
        character(len=32) :: name = ""
        type(holidays_type), pointer :: holidays => null()
    end type named_holidays_type

    interface
        !> The number of days elapsed between the Gregorian date 12/31/1 BC and DATE.
        !> The Gregorian date Sunday, December 31, 1 BC is imaginary.  Port of
        !> cal-julian.el:calendar-julian-to-absolute.  (In cal_julian_mod.)
        pure module integer function calendar_julian_to_absolute(date) result(absolute)
            implicit none(type, external)
            type(calendar_date_type), intent(in) :: date
        end function calendar_julian_to_absolute

        !> Compute the Julian (month day year) corresponding to the absolute DATE.
        !> The absolute date is the number of days elapsed since the (imaginary)
        !> Gregorian date Sunday, December 31, 1 BC.  Port of
        !> cal-julian.el:calendar-julian-from-absolute.  (In cal_julian_mod.)
        pure module function calendar_julian_from_absolute(date) result(julian)
            implicit none(type, external)
            integer, intent(in) :: date
            type(calendar_date_type) :: julian
        end function calendar_julian_from_absolute

        !> Holiday on MONTH, DAY (Julian) called STRING.
        !> If MONTH, DAY (Julian) is visible, HOLIDAYS holds the corresponding
        !> Gregorian date with STRING.  It is empty if it is not visible in
        !> the current calendar window.  Port of
        !> cal-julian.el:holiday-julian.  (In cal_julian_mod.)
        recursive module subroutine holiday_julian(month, day, string, holidays)
            implicit none(type, external)
            integer, intent(in) :: month, day
            character(len=*), intent(in) :: string
            type(holiday_type), allocatable, intent(out) :: holidays(:)
        end subroutine holiday_julian

        !> Return true if YEAR is a leap year on the Islamic calendar.  Port of
        !> cal-islam.el:calendar-islamic-leap-year-p.  (In cal_islam_mod.)
        pure module logical function is_calendar_islamic_leap_year(year) result(leap)
            implicit none(type, external)
            integer, intent(in) :: year
        end function is_calendar_islamic_leap_year

        !> The last day in MONTH during YEAR on the Islamic calendar.  Port of
        !> cal-islam.el:calendar-islamic-last-day-of-month.  (In cal_islam_mod.)
        pure module integer function calendar_islamic_last_day_of_month(month, year) result(last)
            implicit none(type, external)
            integer, intent(in) :: month, year
        end function calendar_islamic_last_day_of_month

        !> Return the day number within the year of the Islamic date DATE.
        !> Port of cal-islam.el:calendar-islamic-day-number.  (In
        !> cal_islam_mod.)
        pure module integer function calendar_islamic_day_number(date) result(day_of_year)
            implicit none(type, external)
            type(calendar_date_type), intent(in) :: date
        end function calendar_islamic_day_number

        !> Absolute date of Islamic DATE.
        !> The absolute date is the number of days elapsed since the (imaginary)
        !> Gregorian date Sunday, December 31, 1 BC.  Port of
        !> cal-islam.el:calendar-islamic-to-absolute.  (In cal_islam_mod.)
        pure module integer function calendar_islamic_to_absolute(date) result(absolute)
            implicit none(type, external)
            type(calendar_date_type), intent(in) :: date
        end function calendar_islamic_to_absolute

        !> Compute the Islamic date (month day year) corresponding to absolute DATE.
        !> The absolute date is the number of days elapsed since the (imaginary)
        !> Gregorian date Sunday, December 31, 1 BC.  A pre-Islamic date gives
        !> (0 0 0).  Port of cal-islam.el:calendar-islamic-from-absolute.  (In
        !> cal_islam_mod.)
        pure module function calendar_islamic_from_absolute(date) result(islamic)
            implicit none(type, external)
            integer, intent(in) :: date
            type(calendar_date_type) :: islamic
        end function calendar_islamic_from_absolute

        !> Holiday on MONTH, DAY (Islamic) called STRING.
        !> If MONTH, DAY (Islamic) is visible, HOLIDAYS holds the corresponding
        !> Gregorian date with STRING.
        !> It is empty if it is not visible in the current calendar window.
        !> Port of cal-islam.el:holiday-islamic.  (In cal_islam_mod.)
        recursive module subroutine holiday_islamic(month, day, string, holidays)
            implicit none(type, external)
            integer, intent(in) :: month, day
            character(len=*), intent(in) :: string
            type(holiday_type), allocatable, intent(out) :: holidays(:)
        end subroutine holiday_islamic

        !> Holiday entry for the Islamic New Year, if visible in the calendar
        !> window.  Port of cal-islam.el:holiday-islamic-new-year.  (In
        !> cal_islam_mod.)
        recursive module subroutine holiday_islamic_new_year(holidays, status)
            implicit none(type, external)
            type(holiday_type), allocatable, intent(out) :: holidays(:)
            type(status_type), intent(out) :: status
        end subroutine holiday_islamic_new_year
    end interface

    public :: calendar_julian_to_absolute, calendar_julian_from_absolute, holiday_julian, is_calendar_islamic_leap_year, &
              calendar_islamic_last_day_of_month, calendar_islamic_day_number, calendar_islamic_to_absolute, &
              calendar_islamic_from_absolute, holiday_islamic, holiday_islamic_new_year

contains

    ! ------------------------------------------------------------------
    ! Holiday lists
    ! ------------------------------------------------------------------

    !> Add ENTRY at the end of the list.  Not a port.
    subroutine append_holiday_entry(self, entry)
        class(holidays_type), intent(inout) :: self
        type(holiday_entry_type), intent(in) :: entry

        if (.not. allocated(self%entries)) allocate (self%entries(0))
        self%entries = [self%entries, entry]
    end subroutine append_holiday_entry

    !> Add the entries of the list OTHER at the end of the list (upstream's
    !> append of two holiday lists).  Not a port.
    subroutine append_holidays(self, other)
        class(holidays_type), intent(inout) :: self
        type(holidays_type), intent(in) :: other

        if (.not. allocated(self%entries)) allocate (self%entries(0))
        if (allocated(other%entries)) self%entries = [self%entries, other%entries]
    end subroutine append_holidays

    !> Remove all the entries of the list.  Not a port.
    subroutine clear_holidays(self)
        class(holidays_type), intent(inout) :: self

        if (allocated(self%entries)) deallocate (self%entries)
        allocate (self%entries(0))
    end subroutine clear_holidays

    ! Whether the list LIST has entries (upstream's non-nil list).
    ! Not a port.
    logical function has_entries(list)
        type(holidays_type), intent(in) :: list

        has_entries = .false.
        if (allocated(list%entries)) has_entries = size(list%entries) > 0
    end function has_entries

    !> Add the holiday DATE, STRING at the end of HOLIDAYS.  Public only
    !> for cal_julian_mod and cal_islam_mod (gfortran does not emit a
    !> private procedure only a submodule calls); not part of the config
    !> API.  Not a port.
    subroutine append_holiday(holidays, date, string)
        type(holiday_type), allocatable, intent(inout) :: holidays(:)
        type(calendar_date_type), intent(in) :: date
        character(len=*), intent(in) :: string
        type(holiday_type), allocatable :: grown(:)
        integer :: n

        if (.not. allocated(holidays)) allocate (holidays(0))
        n = size(holidays)
        allocate (grown(n + 1))
        grown(1:n) = holidays
        grown(n + 1)%date = date
        grown(n + 1)%string = string
        call move_alloc(grown, holidays)
    end subroutine append_holiday

    ! Add the holidays MORE at the end of HOLIDAYS.  Not a port.
    subroutine append_holiday_list(holidays, more)
        type(holiday_type), allocatable, intent(inout) :: holidays(:)
        type(holiday_type), intent(in) :: more(:)
        type(holiday_type), allocatable :: grown(:)
        integer :: n

        if (.not. allocated(holidays)) allocate (holidays(0))
        n = size(holidays)
        allocate (grown(n + size(more)))
        grown(1:n) = holidays
        grown(n + 1:) = more
        call move_alloc(grown, holidays)
    end subroutine append_holiday_list

    ! The default value of `calendar_holidays', made the first time it is
    ! used unless the init file set it.  Not a port.
    subroutine ensure_calendar_holidays()
        if (allocated(calendar_holidays%entries)) return
        call calendar_holidays%clear()
        call calendar_holidays%append(holiday_general_holidays)
        call calendar_holidays%append(holiday_local_holidays)
        call calendar_holidays%append(holiday_other_holidays)
        call calendar_holidays%append(holiday_christian_holidays)
        call calendar_holidays%append(holiday_islamic_holidays)
    end subroutine ensure_calendar_holidays

    ! ------------------------------------------------------------------
    ! Lists of holidays
    ! ------------------------------------------------------------------

    !> Form the list of holidays that occur on dates in the calendar window.
    !> The holidays are those in the list `calendar_holidays'.  Port of
    !> holidays.el:calendar-holiday-list (an entry whose procedure fails
    !> gives no holidays; the warning upstream displays, and
    !> `calendar_debug_expression', are Deferred).
    recursive subroutine calendar_holiday_list(holidays)
        type(holiday_type), allocatable, intent(out) :: holidays(:)
        type(holiday_type), allocatable :: h(:), res(:)
        type(status_type) :: st
        integer :: k

        call ensure_calendar_holidays()
        allocate (res(0))

        do k = 1, size(calendar_holidays%entries)
            call compute_entry_holidays(calendar_holidays%entries(k), h, st)
            if (.not. st%ok()) cycle

            ! The holidays of an entry go before those of the entries before it.
            if (size(h) > 0) then
                call append_holiday_list(h, res)
                call move_alloc(h, res)
            end if
        end do

        call sort_holidays(res)
        call move_alloc(res, holidays)
    end subroutine calendar_holiday_list

    ! Sort HOLIDAYS by date, keeping the order of holidays on the same
    ! date: upstream's (sort ... 'calendar-date-compare), a stable sort.
    ! Not a port.
    subroutine sort_holidays(holidays)
        type(holiday_type), intent(inout) :: holidays(:)
        type(holiday_type) :: item
        integer :: i, j, key

        do i = 2, size(holidays)
            item = holidays(i)
            key = calendar_absolute_from_gregorian(item%date)
            j = i - 1

            do while (j >= 1)
                if (calendar_absolute_from_gregorian(holidays(j)%date) <= key) exit
                holidays(j + 1) = holidays(j)
                j = j - 1
            end do

            holidays(j + 1) = item
        end do
    end subroutine sort_holidays

    ! The holidays of ENTRY visible in the calendar window, what
    ! calendar-holiday-list gets for each element.  Not a port.
    recursive subroutine compute_entry_holidays(entry, holidays, status)
        type(holiday_entry_type), intent(in) :: entry
        type(holiday_type), allocatable, intent(out) :: holidays(:)
        type(status_type), intent(out) :: status

        status = status_type()

        select case (entry%kind)
        case (HOLIDAY_ENTRY_FIXED)
            call holiday_fixed(entry%month, entry%day, trim(entry%string), holidays)
        case (HOLIDAY_ENTRY_FLOAT)
            if (entry%day == 0) then
                call holiday_float(entry%month, entry%dayname, entry%n, trim(entry%string), holidays)
            else
                call holiday_float(entry%month, entry%dayname, entry%n, trim(entry%string), holidays, entry%day)
            end if
        case (HOLIDAY_ENTRY_EXPRESSION)
            if (.not. associated(entry%expression)) then
                call set_error(status, FM_WRONG_TYPE_ARGUMENT, "expected a procedure, got no expression")
                return
            end if

            call holiday_expression(entry%expression, trim(entry%string), holidays, status)
        case (HOLIDAY_ENTRY_EASTER_ETC)
            if (len_trim(entry%string) == 0) then
                call holiday_easter_etc(holidays)
            else
                call holiday_easter_etc(holidays, entry%n, trim(entry%string))
            end if
        case (HOLIDAY_ENTRY_ADVENT)
            if (len_trim(entry%string) == 0) then
                call holiday_advent(holidays)
            else
                call holiday_advent(holidays, entry%n, trim(entry%string))
            end if
        case (HOLIDAY_ENTRY_GREEK_ORTHODOX_EASTER)
            if (len_trim(entry%string) == 0) then
                call holiday_greek_orthodox_easter(holidays)
            else
                call holiday_greek_orthodox_easter(holidays, entry%n, trim(entry%string))
            end if
        case (HOLIDAY_ENTRY_JULIAN)
            call holiday_julian(entry%month, entry%day, trim(entry%string), holidays)
        case (HOLIDAY_ENTRY_ISLAMIC)
            call holiday_islamic(entry%month, entry%day, trim(entry%string), holidays)
        case (HOLIDAY_ENTRY_PROCEDURE)
            if (.not. associated(entry%procedure)) then
                call set_error(status, FM_WRONG_TYPE_ARGUMENT, "expected a procedure, got no procedure")
                return
            end if

            call entry%procedure(holidays, status)
        case default
            call set_error(status, FM_WRONG_TYPE_ARGUMENT, "expected a holiday entry kind")
        end select

        if (.not. allocated(holidays)) allocate (holidays(0))
    end subroutine compute_entry_holidays

    ! The text of HOLIDAYS, one line each: their date strings, ": " and
    ! their strings.  Not a port.
    function holiday_lines(holidays) result(text)
        type(holiday_type), intent(in) :: holidays(:)
        character(len=:), allocatable :: text
        integer :: k

        text = ""

        do k = 1, size(holidays)
            if (k > 1) text = text//achar(10)
            text = text//calendar_date_string(holidays(k)%date)//": "//holidays(k)%string
        end do
    end function holiday_lines

    !> Create a buffer containing the holidays for the current calendar window.
    !> The holidays are those in the list `calendar_holidays'.
    !> FOUND is true if any holidays are found.  Port of
    !> holidays.el:calendar-list-holidays (no EVENT: the mouse and the
    !> menus are Deferred).
    recursive subroutine calendar_list_holidays(status, found)
        type(status_type), intent(out) :: status
        logical, intent(out), optional :: found
        type(holiday_type), allocatable :: holiday_list(:)
        type(handle_type) :: old
        character(len=:), allocatable :: mode_line
        character(len=16) :: digits
        integer :: m1, y1, m2, y2
        type(status_type) :: st

        status = status_type()
        if (present(found)) found = .false.
        old = current_buffer()
        call message("Looking up holidays...")
        call calendar_holiday_list(holiday_list)
        call calendar_get_month_range(m1, y1, m2, y2)

        if (size(holiday_list) == 0) then
            call message("Looking up holidays...none found")
        else
            if (y1 == y2) then
                write (digits, "(i0)") y2
                mode_line = "Notable Dates from "//calendar_month_name(m1)//" to "//calendar_month_name(m2)//", " &
                            //trim(digits)//"%-"
            else
                write (digits, "(i0)") y1
                mode_line = "Notable Dates from "//calendar_month_name(m1)//", "//trim(digits)//" to " &
                            //calendar_month_name(m2)
                write (digits, "(i0)") y2
                mode_line = mode_line//", "//trim(digits)//"%-"
            end if

            call calendar_in_read_only_buffer(holiday_buffer, mode_line, holiday_lines(holiday_list), status)
            if (is_buffer_live(old)) call set_buffer(old, st)
            if (.not. status%ok()) return
            call message("Looking up holidays...done")
        end if

        if (present(found)) found = size(holiday_list) > 0
    end subroutine calendar_list_holidays

    ! The command `calendar_list_holidays'.  Not a port.
    recursive subroutine cmd_calendar_list_holidays(status)
        type(status_type), intent(out) :: status

        call calendar_list_holidays(status)
    end subroutine cmd_calendar_list_holidays

    !> Display the holidays for last month, this month, and next month.
    !> If called with an optional prefix argument ARG, prompts for month and year.
    !> This function is suitable for execution in an init file.  Port of
    !> holidays.el:holidays.
    recursive subroutine holidays(status, arg)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: arg
        type(calendar_date_type) :: date
        type(calendar_display_type) :: saved
        logical :: prompt

        prompt = .false.
        if (present(arg)) prompt = arg

        if (prompt) then
            ! (completion-ignore-case t): calendar_read_date binds it.
            call calendar_read_date(date, status, noday=.true.)
            if (.not. status%ok()) return
        else
            date = calendar_current_date()
        end if

        call set_calendar_display(date%month, date%year, 3, saved)
        call calendar_list_holidays(status)
        call restore_calendar_display(saved)
    end subroutine holidays

    ! The command `holidays': ARG is the prefix argument.  Not a port.
    recursive subroutine cmd_holidays(status)
        type(status_type), intent(out) :: status

        call holidays(status, current_prefix_arg%kind /= PREFIX_NONE)
    end subroutine cmd_holidays

    !> Display holidays for years Y1 to Y2 (inclusive).
    !> Y2 defaults to Y1.  The optional list of holidays L defaults to
    !> `calendar_holidays'.  If you want to control what holidays are
    !> displayed, use a different list.  For example,
    !>
    !>   l = holiday_general_holidays
    !>   call l%append(holiday_local_holidays)
    !>   call list_holidays(2006, status, 2006, l)
    !>
    !> will display holidays for the year 2006 defined in the two
    !> mentioned lists, and nothing else.
    !>
    !> When called interactively, this command offers a choice of
    !> holidays, based on the variables `holiday_general_holidays' etc.  See the
    !> documentation of `calendar_holidays' for a list of the variables
    !> that control the choices, as well as a description of the format
    !> of a holiday list.
    !>
    !> The optional LABEL is used to label the buffer created.  Port of
    !> holidays.el:list-holidays (the lists of the choices are those of
    !> holidays.el:holiday-available-holiday-lists).
    recursive subroutine list_holidays(y1, status, y2, l, label)
        integer, intent(in) :: y1
        type(status_type), intent(out) :: status
        integer, intent(in), optional :: y2
        type(holidays_type), intent(in), optional :: l
        character(len=*), intent(in), optional :: label
        type(holidays_type) :: saved_holidays
        type(holiday_type), allocatable :: holiday_list(:), more(:)
        type(calendar_display_type) :: saved
        type(handle_type) :: old
        character(len=:), allocatable :: title, mode_line
        character(len=16) :: digits
        integer :: last, year
        logical :: replace
        type(status_type) :: st

        last = y1
        if (present(y2)) last = y2
        call message("Computing holidays...")
        call ensure_calendar_holidays()
        ! (calendar-holidays (or l calendar-holidays))
        replace = .false.
        if (present(l)) replace = has_entries(l)
        saved_holidays = calendar_holidays
        if (replace) calendar_holidays = l
        title = "Holidays"
        if (present(label)) title = label
        call set_calendar_display(2, y1, 12, saved)
        allocate (holiday_list(0))
        year = y1

        do while (year <= last)
            call restore_calendar_display(saved)
            call set_calendar_display(2, year, 12, saved)
            call calendar_holiday_list(more)
            call append_holiday_list(holiday_list, more)
            year = year + 1
        end do

        old = current_buffer()
        write (digits, "(i0)") y1

        if (y1 == last) then
            mode_line = title//" for "//trim(digits)
        else
            mode_line = title//" for "//trim(digits)
            write (digits, "(i0)") last
            mode_line = mode_line//"-"//trim(digits)
        end if

        call calendar_in_read_only_buffer(holiday_buffer, mode_line, holiday_lines(holiday_list), status)
        if (status%ok()) call message("Computing holidays...done")
        if (is_buffer_live(old)) call set_buffer(old, st)
        call restore_calendar_display(saved)
        calendar_holidays = saved_holidays
    end subroutine list_holidays

    ! The command `list_holidays': reads the years and the list as
    ! upstream's command does.  Not a port.
    recursive subroutine cmd_list_holidays(status)
        type(status_type), intent(out) :: status
        type(named_holidays_type), allocatable :: lists(:)
        type(value_type), allocatable :: names(:)
        type(holidays_type) :: which
        character(len=:), allocatable :: answer, choice, name, variable
        character(len=64) :: prompt
        type(calendar_date_type) :: today
        integer :: start_year, end_year, k
        logical :: saved_ignore_case

        today = calendar_current_date()
        call calendar_read_expression("Starting year of holidays (>0)", is_positive, start_year, status, today%year)
        if (.not. status%ok()) return
        write (prompt, '("Ending year (inclusive) of holidays (>=", i0, ")")') start_year
        call calendar_read_expression(trim(prompt), is_at_least_start, end_year, status, start_year)
        if (.not. status%ok()) return
        call holiday_available_holiday_lists(lists)
        allocate (names(size(lists)))

        do k = 1, size(lists)
            names(k) = value_type(text=trim(lists(k)%name))
        end do

        saved_ignore_case = completion_ignore_case%value
        completion_ignore_case%value = .true.
        call completing_read("List (TAB for choices): ", completion_list_table(make_list(names)), answer, status, &
                             require_match=REQUIRE_MATCH_YES)
        completion_ignore_case%value = saved_ignore_case
        if (.not. status%ok()) return
        choice = capitalize_string(answer)
        call which%clear()

        if (choice == "Ask") then
            call read_variable("Enter list name: ", variable, status)
            if (.not. status%ok()) return
            call holiday_list_named(variable, which, status)
            if (.not. status%ok()) return
        else
            k = findloc(lists%name == choice, .true., dim=1)

            if (k > 0 .and. len(choice) > 0) then
                if (associated(lists(k)%holidays)) which = lists(k)%holidays
            end if
        end if

        if (choice == "Ask" .or. len(choice) == 0) then
            name = "Holidays"
        else
            name = choice//" Holidays"
        end if

        call list_holidays(start_year, status, end_year, which, name)

    contains

        logical function is_positive(x)
            integer, intent(in) :: x

            is_positive = x > 0
        end function is_positive

        logical function is_at_least_start(x)
            integer, intent(in) :: x

            is_at_least_start = x >= start_year
        end function is_at_least_start
    end subroutine cmd_list_holidays

    !> Return a list of all holiday lists.
    !> This is used by `list_holidays'.  Port of
    !> holidays.el:holiday-available-holiday-lists (the choices
    !> "Equinoxes/Solstices", "Hebrew", "Bahá’í", "Oriental" and "Solar" are
    !> Deferred with their lists; customizing the value with add-function
    !> is Not applicable).
    subroutine holiday_available_holiday_lists(lists)
        type(named_holidays_type), allocatable, intent(out) :: lists(:)

        call ensure_calendar_holidays()
        allocate (lists(0))
        call append_named(lists, "All", calendar_holidays)
        if (has_entries(holiday_general_holidays)) call append_named(lists, "General", holiday_general_holidays)
        if (has_entries(holiday_local_holidays)) call append_named(lists, "Local", holiday_local_holidays)
        if (has_entries(holiday_other_holidays)) call append_named(lists, "Other", holiday_other_holidays)
        if (has_entries(holiday_christian_holidays)) call append_named(lists, "Christian", holiday_christian_holidays)
        if (has_entries(holiday_islamic_holidays)) call append_named(lists, "Islamic", holiday_islamic_holidays)
        call append_named(lists, "Ask")
    end subroutine holiday_available_holiday_lists

    ! Add the choice NAME of the list HOLIDAYS (none for "Ask") to LISTS.
    ! Not a port.
    subroutine append_named(lists, name, holidays)
        type(named_holidays_type), allocatable, intent(inout) :: lists(:)
        character(len=*), intent(in) :: name
        type(holidays_type), target, intent(in), optional :: holidays
        type(named_holidays_type) :: item

        item%name = name
        if (present(holidays)) item%holidays => holidays
        lists = [lists, item]
    end subroutine append_named

    ! The holiday list of the user option NAME, typed at the "Ask" choice
    ! of `list_holidays' (upstream reads its value): a `wrong_type_argument'
    ! error for an option that is not a holiday list.  Not a port.
    subroutine holiday_list_named(name, holidays, status)
        character(len=*), intent(in) :: name
        type(holidays_type), intent(out) :: holidays
        type(status_type), intent(out) :: status
        type(named_holidays_type) :: options(6)
        integer :: k

        status = status_type()
        call ensure_calendar_holidays()
        options(1)%name = "calendar_holidays"
        options(1)%holidays => calendar_holidays
        options(2)%name = "holiday_general_holidays"
        options(2)%holidays => holiday_general_holidays
        options(3)%name = "holiday_local_holidays"
        options(3)%holidays => holiday_local_holidays
        options(4)%name = "holiday_other_holidays"
        options(4)%holidays => holiday_other_holidays
        options(5)%name = "holiday_christian_holidays"
        options(5)%holidays => holiday_christian_holidays
        options(6)%name = "holiday_islamic_holidays"
        options(6)%holidays => holiday_islamic_holidays
        k = findloc(options%name == name, .true., dim=1)

        if (k == 0) then
            call set_error(status, FM_WRONG_TYPE_ARGUMENT, "expected type(holidays_type), got "//name)
            return
        end if

        holidays = options(k)%holidays
        if (.not. allocated(holidays%entries)) call holidays%clear()
    end subroutine holiday_list_named

    !> Check the list of holidays for any that occur on DATE.
    !> This function considers the
    !> holidays from the list `calendar_holidays', and returns in STRINGS
    !> the holidays that apply on DATE (none if none do).  Port of
    !> holidays.el:calendar-check-holidays.
    recursive subroutine calendar_check_holidays(date, strings)
        type(calendar_date_type), intent(in) :: date
        type(holiday_type), allocatable, intent(out) :: strings(:)
        type(holiday_type), allocatable :: holiday_list(:)
        type(calendar_display_type) :: saved
        integer :: month, year, k

        month = date%month
        year = date%year
        call calendar_increment_month(month, year, 1)
        call set_calendar_display(month, year, 1, saved)
        call calendar_holiday_list(holiday_list)
        call restore_calendar_display(saved)
        allocate (strings(0))

        do k = 1, size(holiday_list)
            if (holiday_list(k)%date == date) call append_holiday(strings, date, holiday_list(k)%string)
        end do
    end subroutine calendar_check_holidays

    !> Generate a list of all holidays in range from absolute date D1 to D2.
    !> Port of holidays.el:holiday-in-range.
    recursive subroutine holiday_in_range(d1, d2, in_range)
        integer, intent(in) :: d1, d2
        type(holiday_type), allocatable, intent(out) :: in_range(:)
        type(holiday_type), allocatable :: found(:), more(:)
        type(calendar_date_type) :: start, end_date
        type(calendar_display_type) :: saved
        integer :: month, year, number_of_intervals, k, a

        start = calendar_gregorian_from_absolute(d1)
        month = start%month
        year = start%year
        end_date = calendar_gregorian_from_absolute(d2)
        number_of_intervals = 1 + calendar_interval(month, year, end_date%month, end_date%year)/3
        call calendar_increment_month(month, year, 1)
        call set_calendar_display(month, year, 3, saved)
        allocate (found(0))

        do k = 1, number_of_intervals
            call calendar_holiday_list(more)
            call append_holiday_list(found, more)
            call calendar_increment_month(month, year, 3)
            call restore_calendar_display(saved)
            call set_calendar_display(month, year, 3, saved)
        end do

        call restore_calendar_display(saved)
        allocate (in_range(0))

        ! Each holiday in range goes before those found before it.
        do k = 1, size(found)
            a = calendar_absolute_from_gregorian(found(k)%date)
            if (d1 <= a .and. a <= d2) call prepend_holiday(in_range, found(k)%date, found(k)%string)
        end do
    end subroutine holiday_in_range

    !> Find holidays for the date specified by the cursor in the calendar window.
    !> Optional DATE is a date to use instead of the
    !> cursor position.  Port of holidays.el:calendar-cursor-holidays (the
    !> EVENT and its menu are Deferred with the mouse).
    recursive subroutine calendar_cursor_holidays(status, date)
        type(status_type), intent(out) :: status
        type(calendar_date_type), intent(in), optional :: date
        type(calendar_date_type) :: day
        type(holiday_type), allocatable :: holiday_list(:)
        character(len=:), allocatable :: date_string, msg, text
        integer :: k

        call message("Checking holidays...")

        if (present(date)) then
            day = date
            status = status_type()
        else
            call calendar_cursor_to_date(day, status, .true.)
            if (.not. status%ok()) return
        end if

        date_string = calendar_date_string(day)
        call calendar_check_holidays(day, holiday_list)

        if (size(holiday_list) == 0) then
            call message("No holidays known for "//date_string)
            return
        end if

        msg = date_string//":  "
        text = ""

        do k = 1, size(holiday_list)
            if (k > 1) then
                msg = msg//";  "
                text = text//achar(10)
            end if

            msg = msg//holiday_list(k)%string
            text = text//holiday_list(k)%string
        end do

        if (string_chars(msg) <= frame_cols(selected_frame)) then
            call message(msg)
        else
            call calendar_in_read_only_buffer(holiday_buffer, date_string, text, status)
            if (.not. status%ok()) return
            call message("Checking holidays...done")
        end if
    end subroutine calendar_cursor_holidays

    ! The command `calendar_cursor_holidays'.  Not a port.
    recursive subroutine cmd_calendar_cursor_holidays(status)
        type(status_type), intent(out) :: status

        call calendar_cursor_holidays(status)
    end subroutine cmd_calendar_cursor_holidays

    !> Mark notable days in the calendar window.  Port of
    !> holidays.el:calendar-mark-holidays (no EVENT, with the mouse;
    !> `calendar_holiday_marker' is Deferred: dates are marked with its
    !> default, the `holiday' face).
    recursive subroutine calendar_mark_holidays(status)
        type(status_type), intent(out) :: status
        type(holiday_type), allocatable :: holiday_list(:)
        integer :: k

        status = status_type()
        calendar_mark_holidays_variable = .true.
        call message("Marking holidays...")
        call calendar_holiday_list(holiday_list)

        do k = 1, size(holiday_list)
            call calendar_mark_visible_date(holiday_list(k)%date, status, face=holiday)
            if (.not. status%ok()) return
        end do

        call message("Marking holidays...done")
    end subroutine calendar_mark_holidays

    ! ------------------------------------------------------------------
    ! The dates of holidays
    ! ------------------------------------------------------------------

    !> Holiday on MONTH, DAY (Gregorian) called STRING.
    !> If MONTH, DAY is visible, HOLIDAYS holds MONTH DAY of the visible
    !> year with STRING.  It is empty if it is not visible in the current
    !> calendar window.  Port of holidays.el:holiday-fixed.
    recursive subroutine holiday_fixed(month, day, string, holidays)
        integer, intent(in) :: month, day
        character(len=*), intent(in) :: string
        type(holiday_type), allocatable, intent(out) :: holidays(:)
        integer :: y

        allocate (holidays(0))
        if (is_calendar_month_visible(month, y)) call append_holiday(holidays, calendar_date_type(month, day, y), string)
    end subroutine holiday_fixed

    !> Holiday called STRING on the Nth DAYNAME after/before MONTH DAY.
    !> DAYNAME = 0 means Sunday, DAYNAME = 1 means Monday, and so on.  DAY
    !> defaults to 1 if N > 0, and MONTH's last day otherwise.
    !>
    !> If N > 0, use the Nth DAYNAME after MONTH DAY.
    !> If N < 0, use the Nth DAYNAME before MONTH DAY.
    !>
    !> When MONTH DAY falls on DAYNAME, the holiday will be |N|-1 weeks before
    !> or after MONTH DAY.  For example, with N = +1 (-1) the holiday falls on
    !> MONTH DAY, and with N = +2 (-2) the holiday falls 1 week after (before)
    !> MONTH DAY.
    !>
    !> If the holiday is visible in the calendar window, HOLIDAYS holds its
    !> date with STRING.  Otherwise it is empty.  Port of
    !> holidays.el:holiday-float.
    recursive subroutine holiday_float(month, dayname, n, string, holidays, day)
        integer, intent(in) :: month, dayname, n
        character(len=*), intent(in) :: string
        type(holiday_type), allocatable, intent(out) :: holidays(:)
        integer, intent(in), optional :: day
        integer :: m1, y1, m2, y2, d1, d2, date, d, k
        integer, allocatable :: years(:)
        type(calendar_date_type) :: first, last

        ! This is messy because the holiday may be visible, while the date
        ! on which it is based is not.  For example, the first Monday after
        ! December 30 may be visible when January is not.  For large values
        ! of |n| the problem is more grotesque.
        call calendar_get_month_range(m1, y1, m2, y2)

        ! first possible base date for holiday
        if (n > 0) then
            d1 = calendar_nth_named_absday(1, dayname, m1, y1) - 7*n + 1
        else
            d1 = calendar_nth_named_absday(1, dayname, m1, y1) - 7*n - 7
        end if

        ! last possible base date for holiday
        if (n > 0) then
            d2 = calendar_nth_named_absday(-1, dayname, m2, y2) - 7*n + 7
        else
            d2 = calendar_nth_named_absday(-1, dayname, m2, y2) - 7*n - 1
        end if

        first = calendar_gregorian_from_absolute(d1)
        last = calendar_gregorian_from_absolute(d2)
        ! possible years of base date
        allocate (years(0))
        if (first%year == last%year .or. month >= first%month) years = [first%year, years]
        if (first%year /= last%year .and. month <= last%month) years = [last%year, years]
        allocate (holidays(0))

        do k = 1, size(years)
            ! day of base date
            if (present(day)) then
                d = day
            else if (n > 0) then
                d = 1
            else
                d = calendar_last_day_of_month(month, years(k))
            end if

            ! base date for holiday
            date = calendar_absolute_from_gregorian(calendar_date_type(month, d, years(k)))

            if (d1 <= date .and. date <= d2) then
                call prepend_holiday(holidays, calendar_nth_named_day(n, dayname, month, years(k), d), string)
            end if
        end do
    end subroutine holiday_float

    ! Add the holiday DATE, STRING at the front of HOLIDAYS (upstream's
    ! push).  Not a port.
    subroutine prepend_holiday(holidays, date, string)
        type(holiday_type), allocatable, intent(inout) :: holidays(:)
        type(calendar_date_type), intent(in) :: date
        character(len=*), intent(in) :: string
        type(holiday_type), allocatable :: grown(:)

        allocate (grown(1))
        grown(1)%date = date
        grown(1)%string = string
        if (allocated(holidays)) call append_holiday_list(grown, holidays)
        call move_alloc(grown, holidays)
    end subroutine prepend_holiday

    !> Filter list of holidays HOLIDAYS, and return only the visible ones.
    !> Port of holidays.el:holiday-filter-visible-calendar (a holiday with
    !> no date is `calendar_date_type()', which is not valid).
    subroutine holiday_filter_visible_calendar(holidays)
        type(holiday_type), allocatable, intent(inout) :: holidays(:)
        type(holiday_type), allocatable :: visible(:)
        integer :: k

        allocate (visible(0))

        do k = 1, size(holidays)
            if (is_calendar_date_visible(holidays(k)%date)) call append_holiday(visible, holidays(k)%date, &
                                                                                holidays(k)%string)
        end do

        call move_alloc(visible, holidays)
    end subroutine holiday_filter_visible_calendar

    !> Expression holiday for dates in the calendar window.
    !> EXPRESSION is a procedure that gives `date' in a year.  STRING is
    !> the holiday description of `date', which EXPRESSION may change.  If
    !> `date' is visible in the calendar window, the holiday STRING is on
    !> that date.  If there is no date, or if the date is not visible,
    !> there is no holiday.  Port of holidays.el:holiday-sexp.
    recursive subroutine holiday_expression(expression, string, holidays, status)
        procedure(holiday_expression_procedure) :: expression
        character(len=*), intent(in) :: string
        type(holiday_type), allocatable, intent(out) :: holidays(:)
        type(status_type), intent(out) :: status
        type(calendar_date_type) :: date
        character(len=:), allocatable :: text
        integer :: m1, y1, m2, y2

        call calendar_get_month_range(m1, y1, m2, y2)
        allocate (holidays(0))
        text = string
        call expression(y1, date, text, status)
        if (.not. status%ok()) return
        call append_holiday(holidays, date, text)

        if (y1 /= y2) then
            text = string
            call expression(y2, date, text, status)
            if (.not. status%ok()) return
            call append_holiday(holidays, date, text)
        end if

        call holiday_filter_visible_calendar(holidays)
    end subroutine holiday_expression

    !> Holiday called STRING on the Nth day after/before REF in the calendar.
    !> REF is a function that accepts the Year as the argument and returns the
    !> absolute date of the reference day.  Negative values of N are
    !> interpreted as days before the reference day.  Port of
    !> holidays.el:holiday-after.
    recursive subroutine holiday_after(ref, n, string, holidays)
        interface
            integer function ref(year)
                implicit none(type, external)
                integer, intent(in) :: year
            end function ref
        end interface
        integer, intent(in) :: n
        character(len=*), intent(in) :: string
        type(holiday_type), allocatable, intent(out) :: holidays(:)
        integer :: m1, y1, m2, y2

        call calendar_get_month_range(m1, y1, m2, y2)
        allocate (holidays(0))
        call append_holiday(holidays, calendar_gregorian_from_absolute(n + ref(y1)), string)
        if (y1 /= y2) call append_holiday(holidays, calendar_gregorian_from_absolute(n + ref(y2)), string)
        call holiday_filter_visible_calendar(holidays)
    end subroutine holiday_after

    !> Date of Nth day after advent (named STRING), if visible in calendar window.
    !> Negative values of N are interpreted as days before advent.
    !> STRING is used purely for display purposes.  HOLIDAYS holds
    !> the date with STRING, where the date is that of the
    !> Nth day before or after advent.
    !>
    !> For backwards compatibility, if this function is called with no
    !> arguments, then it returns the value appropriate for advent itself.
    !> Port of holidays.el:holiday-advent.
    recursive subroutine holiday_advent(holidays, n, string)
        type(holiday_type), allocatable, intent(out) :: holidays(:)
        integer, intent(in), optional :: n
        character(len=*), intent(in), optional :: string

        ! Backwards compatibility layer.
        if (.not. present(n)) then
            call holiday_advent(holidays, 0, "Advent")
            return
        end if

        if (present(string)) then
            call holiday_after(advent_absolute, n, string, holidays)
        else
            call holiday_after(advent_absolute, n, "", holidays)
        end if
    end subroutine holiday_advent

    ! The absolute date of Advent in YEAR.  Not a port (the procedure of
    ! holidays.el:holiday-advent).
    integer function advent_absolute(year)
        integer, intent(in) :: year

        advent_absolute = calendar_dayname_on_or_before(0, calendar_absolute_from_gregorian(calendar_date_type(12, 3, year)))
    end function advent_absolute

    !> Date of Nth day after Easter (named STRING), if visible in calendar window.
    !> Negative values of N are interpreted as days before Easter.
    !> STRING is used purely for display purposes.  HOLIDAYS holds
    !> the date with STRING, where the date is that of the
    !> Nth day before or after Easter.
    !>
    !> For backwards compatibility, if this function is called with no
    !> arguments, then it returns a list of "standard" Easter-related
    !> holidays (with more entries if `calendar_christian_all_holidays_flag'
    !> is .true.).  Port of holidays.el:holiday-easter-etc.
    recursive subroutine holiday_easter_etc(holidays, n, string)
        type(holiday_type), allocatable, intent(out) :: holidays(:)
        integer, intent(in), optional :: n
        character(len=*), intent(in), optional :: string
        type(holiday_type), allocatable :: more(:)

        ! Backwards compatibility layer.
        if (.not. present(n)) then
            allocate (holidays(0))

            ! The combined list is not in order.
            if (calendar_christian_all_holidays_flag) then
                call add(-63, "Septuagesima Sunday")
                call add(-56, "Sexagesima Sunday")
                call add(-49, "Shrove Sunday")
                call add(-48, "Shrove Monday")
                call add(-47, "Shrove Tuesday")
                call add(-14, "Passion Sunday")
                call add(-7, "Palm Sunday")
                call add(-3, "Maundy Thursday")
                call add(35, "Rogation Sunday")
                call add(39, "Ascension Day")
                call add(49, "Pentecost (Whitsunday)")
                call add(50, "Whitmonday")
                call add(56, "Trinity Sunday")
                call add(60, "Corpus Christi")
            end if

            call add(-46, "Ash Wednesday")
            call add(-2, "Good Friday")
            call add(0, "Easter Sunday")
            return
        end if

        if (present(string)) then
            call holiday_after(holiday_easter_etc_absolute, n, string, holidays)
        else
            call holiday_after(holiday_easter_etc_absolute, n, "", holidays)
        end if

    contains

        ! One Easter holiday, appended.
        recursive subroutine add(days, name)
            integer, intent(in) :: days
            character(len=*), intent(in) :: name

            call holiday_after(holiday_easter_etc_absolute, days, name, more)
            call append_holiday_list(holidays, more)
        end subroutine add
    end subroutine holiday_easter_etc

    !> Return the absolute date of Easter in Year.  Port of
    !> holidays.el:holiday-easter-etc-abs.
    integer function holiday_easter_etc_absolute(y) result(easter)
        integer, intent(in) :: y
        integer :: century, shifted_epact, adjusted_epact, paschal_moon

        century = 1 + y/100
        ! age of moon for April 5...
        ! ...by Nicaean rule
        ! ...corrected for the Gregorian century rule
        ! ...corrected for Metonic cycle inaccuracy
        ! keeps value positive
        shifted_epact = mod(14 + 11*mod(y, 19) - (3*century)/4 + (5 + 8*century)/25 + 30*century, 30)
        ! adjust for 29.5 day month
        adjusted_epact = shifted_epact

        if (shifted_epact == 0) then
            adjusted_epact = shifted_epact + 1
        else if (shifted_epact == 1 .and. 10 < mod(y, 19)) then
            adjusted_epact = shifted_epact + 1
        end if

        ! day after the full moon on or after March 21
        paschal_moon = calendar_absolute_from_gregorian(calendar_date_type(4, 19, y)) - adjusted_epact
        easter = calendar_dayname_on_or_before(0, paschal_moon + 7)
    end function holiday_easter_etc_absolute

    !> Date of Nth day after Easter (named STRING), if visible in calendar window.
    !> It is calculated according to the rule of the Council of Nicaea.
    !> Negative values of N are interpreted as days before Easter.
    !> STRING is used purely for display purposes.  HOLIDAYS holds
    !> the date with STRING, where the date is that of the
    !> Nth day before or after Easter.
    !>
    !> For backwards compatibility, if this function is called with no
    !> arguments, it returns the date of Pascha (Greek Orthodox Easter).
    !> Port of holidays.el:holiday-greek-orthodox-easter.
    recursive subroutine holiday_greek_orthodox_easter(holidays, n, string)
        type(holiday_type), allocatable, intent(out) :: holidays(:)
        integer, intent(in), optional :: n
        character(len=*), intent(in), optional :: string
        integer :: days

        days = 0
        if (present(n)) days = n

        if (present(string)) then
            call holiday_after(holiday_greek_orthodox_easter_absolute, days, string, holidays)
        else
            call holiday_after(holiday_greek_orthodox_easter_absolute, days, "Pascha (Greek Orthodox Easter)", holidays)
        end if
    end subroutine holiday_greek_orthodox_easter

    !> Return the absolute date of Easter in Year.  Port of
    !> holidays.el:holiday-greek-orthodox-easter-abs.
    integer function holiday_greek_orthodox_easter_absolute(y) result(easter)
        integer, intent(in) :: y
        integer :: julian_year, shifted_epact, paschal_moon
        type(calendar_date_type) :: julian

        julian = calendar_julian_from_absolute(calendar_absolute_from_gregorian(calendar_date_type(3, 31, y)))
        julian_year = julian%year
        ! age of moon for April 5
        shifted_epact = mod(14 + 11*mod(julian_year, 19), 30)
        ! day after full moon on or after March 21
        paschal_moon = calendar_julian_to_absolute(calendar_date_type(4, 19, julian_year)) - shifted_epact
        easter = calendar_dayname_on_or_before(0, paschal_moon + 7)
    end function holiday_greek_orthodox_easter_absolute

    ! ------------------------------------------------------------------
    ! The default lists
    ! ------------------------------------------------------------------

    ! The entries of upstream's (if calendar-christian-all-holidays-flag
    ! (append ...)) in `holiday_christian_holidays'.  Not a port.
    recursive subroutine christian_all_holidays(holidays, status)
        type(holiday_type), allocatable, intent(out) :: holidays(:)
        type(status_type), intent(out) :: status
        type(holiday_type), allocatable :: more(:)

        status = status_type()
        allocate (holidays(0))
        if (.not. calendar_christian_all_holidays_flag) return
        call holiday_fixed(1, 6, "Epiphany", more)
        call append_holiday_list(holidays, more)
        call holiday_julian(12, 25, "Christmas (Julian calendar)", more)
        call append_holiday_list(holidays, more)
        call holiday_greek_orthodox_easter(more)
        call append_holiday_list(holidays, more)
        call holiday_fixed(8, 15, "Assumption", more)
        call append_holiday_list(holidays, more)
        call holiday_advent(more, 0, "Advent")
        call append_holiday_list(holidays, more)
    end subroutine christian_all_holidays

    ! The entries of upstream's (if calendar-islamic-all-holidays-flag
    ! (append ...)) in `holiday_islamic_holidays'.  Not a port.
    recursive subroutine islamic_all_holidays(holidays, status)
        type(holiday_type), allocatable, intent(out) :: holidays(:)
        type(status_type), intent(out) :: status
        type(holiday_type), allocatable :: more(:)

        status = status_type()
        allocate (holidays(0))
        if (.not. calendar_islamic_all_holidays_flag) return
        call holiday_islamic(1, 10, "Ashura", more)
        call append_holiday_list(holidays, more)
        call holiday_islamic(3, 12, "Mulad-al-Nabi", more)
        call append_holiday_list(holidays, more)
        call holiday_islamic(7, 26, "Shab-e-Mi'raj", more)
        call append_holiday_list(holidays, more)
        call holiday_islamic(8, 15, "Shab-e-Bara't", more)
        call append_holiday_list(holidays, more)
        call holiday_islamic(9, 27, "Shab-e Qadr", more)
        call append_holiday_list(holidays, more)
        call holiday_islamic(10, 1, "Id-al-Fitr", more)
        call append_holiday_list(holidays, more)
        call holiday_islamic(12, 10, "Id-al-Adha", more)
        call append_holiday_list(holidays, more)
    end subroutine islamic_all_holidays

    ! The default values of the holiday lists.  Not a port.
    subroutine set_default_holidays()
        integer, parameter :: F = HOLIDAY_ENTRY_FIXED, L = HOLIDAY_ENTRY_FLOAT

        holiday_general_holidays%entries = [ &
                                           holiday_entry_type(kind=F, month=1, day=1, string="New Year's Day"), &
                                           holiday_entry_type(kind=L, month=1, dayname=1, n=3, &
                                                              string="Martin Luther King Day"), &
                                           holiday_entry_type(kind=F, month=2, day=2, string="Groundhog Day"), &
                                           holiday_entry_type(kind=F, month=2, day=14, string="Valentine's Day"), &
                                           holiday_entry_type(kind=L, month=2, dayname=1, n=3, string="President's Day"), &
                                           holiday_entry_type(kind=F, month=3, day=17, string="St. Patrick's Day"), &
                                           holiday_entry_type(kind=F, month=4, day=1, string="April Fools' Day"), &
                                           holiday_entry_type(kind=L, month=5, dayname=0, n=2, string="Mother's Day"), &
                                           holiday_entry_type(kind=L, month=5, dayname=1, n=-1, string="Memorial Day"), &
                                           holiday_entry_type(kind=F, month=6, day=14, string="Flag Day"), &
                                           holiday_entry_type(kind=L, month=6, dayname=0, n=3, string="Father's Day"), &
                                           holiday_entry_type(kind=F, month=7, day=4, string="Independence Day"), &
                                           holiday_entry_type(kind=L, month=9, dayname=1, n=1, string="Labor Day"), &
                                           holiday_entry_type(kind=L, month=10, dayname=1, n=2, string="Columbus Day"), &
                                           holiday_entry_type(kind=F, month=10, day=31, string="Halloween"), &
                                           holiday_entry_type(kind=F, month=11, day=11, string="Veteran's Day"), &
                                           holiday_entry_type(kind=L, month=11, dayname=4, n=4, string="Thanksgiving")]
        call holiday_local_holidays%clear()
        call holiday_other_holidays%clear()
        ! (holiday-easter-etc) respects calendar-christian-all-holidays-flag.
        holiday_christian_holidays%entries = [ &
                                             holiday_entry_type(kind=HOLIDAY_ENTRY_EASTER_ETC), &
                                             holiday_entry_type(kind=F, month=12, day=25, string="Christmas"), &
                                             holiday_entry_type(kind=HOLIDAY_ENTRY_PROCEDURE, &
                                                                procedure=christian_all_holidays)]
        holiday_islamic_holidays%entries = [ &
                                           holiday_entry_type(kind=HOLIDAY_ENTRY_PROCEDURE, &
                                                              procedure=holiday_islamic_new_year), &
                                           holiday_entry_type(kind=HOLIDAY_ENTRY_ISLAMIC, month=9, day=1, &
                                                              string="Ramadan Begins"), &
                                           holiday_entry_type(kind=HOLIDAY_ENTRY_PROCEDURE, &
                                                              procedure=islamic_all_holidays)]
        ! Made from the lists when first used (`ensure_calendar_holidays').
        if (allocated(calendar_holidays%entries)) deallocate (calendar_holidays%entries)
    end subroutine set_default_holidays

    ! ------------------------------------------------------------------
    ! Definitions
    ! ------------------------------------------------------------------

    !> Define the holiday lists' defaults, the holiday commands and their
    !> keys in the calendar.  Not a port.
    subroutine init_holidays()
        type(status_type) :: st

        call set_default_holidays()
        call keymap_set(calendar_mode_map, "a", "calendar_list_holidays", st)
        call keymap_set(calendar_mode_map, "h", "calendar_cursor_holidays", st)
        call keymap_set(calendar_mode_map, "x", "calendar_mark_holidays", st)
        calendar_mark_holidays_hook => calendar_mark_holidays
        calendar_list_holidays_hook => cmd_calendar_list_holidays
        commands%holidays%execute => cmd_holidays
        commands%list_holidays%execute => cmd_list_holidays
        ! (defalias 'holiday-list 'list-holidays)
        commands%holiday_list%execute => cmd_list_holidays
        commands%calendar_list_holidays%execute => cmd_calendar_list_holidays
        commands%calendar_cursor_holidays%execute => cmd_calendar_cursor_holidays
        commands%calendar_mark_holidays%execute => calendar_mark_holidays
    end subroutine init_holidays
end module holidays_mod
