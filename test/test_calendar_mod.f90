! test_calendar_mod.f90 -- tests for calendar_mod and cal_move_mod.
! Ports GNU Emacs 31.1 test/lisp/calendar/calendar-tests.el.
!
! Copyright (C) 2024-2026 Free Software Foundation, Inc.
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

!> Ports calendar-test-validity-predicate and (with the default mode
!> line, as the forms of `calendar_mode_line_format' are Deferred)
!> calendar-test-date-in-calendar-mode-line; own tests of the date
!> arithmetic, the date strings, the three months as calendar.el draws
!> them for fixed dates, and the movement commands of cal-move.el.
module test_calendar_mod
    use, intrinsic :: iso_fortran_env, only: int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_mod, only: status_type, format_error
    use handle_mod, only: handle_type
    use buffer_mod, only: buffers, current, current_buffer, set_buffer, get_or_make_buffer, close_buffer, get_buffer, &
                          is_buffer_live
    use editfns_mod, only: buffer_string
    use bindings_mod, only: init_bindings
    use calendar_mod, only: calendar_date_type, operator(==), calendar, calendar_mode, calendar_generate, &
                            calendar_exit, calendar_current_date, calendar_cursor_to_date, &
                            calendar_cursor_to_visible_date, calendar_gregorian_from_absolute, &
                            calendar_absolute_from_gregorian, calendar_day_number, is_calendar_leap_year, &
                            calendar_last_day_of_month, is_calendar_date_valid, calendar_day_of_week, &
                            calendar_date_string, calendar_day_name, calendar_month_name, calendar_increment_month, &
                            calendar_interval, calendar_date_from_day_of_year, calendar_nth_named_day, &
                            calendar_dayname_on_or_before, calendar_day_of_year_string, calendar_other_month, &
                            calendar_redraw, calendar_displayed_month, calendar_displayed_year, &
                            calendar_mode_line_text, calendar_set_mark, calendar_exchange_point_and_mark, &
                            calendar_forward_day, calendar_backward_day, calendar_forward_week, calendar_forward_month, &
                            calendar_backward_month, calendar_forward_year, calendar_beginning_of_week, &
                            calendar_end_of_week, calendar_beginning_of_month, calendar_end_of_month, &
                            calendar_beginning_of_year, calendar_end_of_year, calendar_goto_date, calendar_goto_today, &
                            calendar_scroll_left, calendar_scroll_right, calendar_scroll_calendar_left, &
                            calendar_show_more_months, calendar_show_fewer_months, calendar_total_months_local, &
                            calendar_goto_day_of_year, calendar_week_start_day, calendar_date_style, &
                            CALENDAR_DATE_STYLE_AMERICAN, CALENDAR_DATE_STYLE_EUROPEAN, CALENDAR_DATE_STYLE_ISO
    implicit none(type, external)
    private

    public :: collect_calendar_mod

    character(len=*), parameter :: NL = achar(10)

contains

    !> Collect all exported unit tests.
    subroutine collect_calendar_mod(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("calendar-test-validity-predicate", test_validity_test), &
                    new_unittest("calendar-test-date-in-calendar-mode-line", test_date_in_mode_line), &
                    new_unittest("calendar-absolute-dates", test_absolute_dates), &
                    new_unittest("calendar-date-strings", test_date_strings), &
                    new_unittest("calendar-display", test_display), &
                    new_unittest("calendar-display-week-start", test_display_week_start), &
                    new_unittest("calendar-movement", test_movement), &
                    new_unittest("calendar-movement-scrolls", test_movement_scrolls), &
                    new_unittest("calendar-mark", test_mark), &
                    new_unittest("calendar-more-months", test_more_months) &
                    ]
    end subroutine collect_calendar_mod

    ! Make a buffer NAME in Calendar mode showing the three months around
    ! MONTH, YEAR, and make it current.
    subroutine calendar_buffer(name, month, year, h)
        character(len=*), intent(in) :: name
        integer, intent(in) :: month, year
        type(handle_type), intent(out) :: h
        type(status_type) :: st

        call init_bindings()
        call get_or_make_buffer(name, h, st)
        call set_buffer(h, st)
        call calendar_mode(st)
        call generate(month, year, st)
    end subroutine calendar_buffer

    ! calendar_generate in the current (read-only) calendar buffer, with
    ! inhibit-read-only bound as calendar-generate-window binds it.
    subroutine generate(month, year, status)
        integer, intent(in) :: month, year
        type(status_type), intent(out) :: status

        buffers(current)%read_only = .false.
        call calendar_generate(month, year, status)
        buffers(current)%read_only = .true.
    end subroutine generate

    ! The date under the cursor.
    function cursor_date() result(date)
        type(calendar_date_type) :: date
        type(status_type) :: st

        call calendar_cursor_to_date(date, st)
    end function cursor_date

    ! Check that the cursor is on MONTH/DAY/YEAR.
    subroutine check_cursor(error, month, day, year, what)
        type(error_type), allocatable, intent(out) :: error
        integer, intent(in) :: month, day, year
        character(len=*), intent(in) :: what
        type(calendar_date_type) :: date

        date = cursor_date()

        if (date%month == 0) then
            call check(error, .false., what//": the cursor is not on a date")
            return
        end if

        call check(error, date == calendar_date_type(month, day, year), &
                   what//": cursor on "//calendar_date_string(date, nodayname=.true.))
    end subroutine check_cursor

    subroutine test_validity_test(error)
        type(error_type), allocatable, intent(out) :: error

        ! nil
        call check(error,.not. is_calendar_date_valid(calendar_date_type()))
        if (allocated(error)) return
        ! (list 1 2): no year
        call check(error,.not. is_calendar_date_valid(calendar_date_type(1, 2, 0)))
        if (allocated(error)) return
        call check(error, is_calendar_date_valid(calendar_date_type(5, 1, 2025)))
        if (allocated(error)) return
        call check(error,.not. is_calendar_date_valid(calendar_date_type(2, 29, 2025)))
        if (allocated(error)) return
        call check(error, is_calendar_date_valid(calendar_date_type(2, 29, 2024)))
    end subroutine test_validity_test

    !> Test whether the calendar follows the date the cursor moves to.
    subroutine test_date_in_mode_line(error)
        type(error_type), allocatable, intent(out) :: error
        type(calendar_date_type) :: today, next
        type(handle_type) :: old, buffer
        type(status_type) :: st
        character(len=:), allocatable :: line

        call init_bindings()
        old = current_buffer()
        today = calendar_current_date()
        next = calendar_gregorian_from_absolute(1 + calendar_absolute_from_gregorian(today))
        call calendar(st)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        buffer = get_buffer("*Calendar*")
        call check(error, is_buffer_live(buffer), "no *Calendar*")
        if (allocated(error)) return
        call set_buffer(buffer, st)
        line = calendar_mode_line_text()
        call check(error, index(line, calendar_date_string(today, .true.)) > 0, line)
        if (allocated(error)) return
        call check(error, index(line, "   < ") == 1 .and. line(len(line):) == ">", line)
        if (allocated(error)) return
        call check(error, index(line, " Calendar ") > 0 .and. index(line, "? info / o other / . today") > 0, line)
        if (allocated(error)) return
        call check(error, cursor_date() == today, "today")
        if (allocated(error)) return
        call calendar_forward_day(1, st)
        call check(error, cursor_date() == next, "forward day")
        if (allocated(error)) return
        call calendar_goto_today(st)
        call check(error, cursor_date() == today, "goto today")
        if (allocated(error)) return
        call calendar_cursor_to_visible_date(next)
        call calendar_redraw(st)
        call check(error, st%ok() .and. cursor_date() == next, "redraw")
        if (allocated(error)) return
        call calendar_cursor_to_visible_date(next)
        call calendar_scroll_left(1, st)
        call calendar_other_month(today%month, today%year, st)
        call check(error, st%ok() .and. cursor_date() == next, "other month")
        if (allocated(error)) return
        call calendar_exit(st, .true.)
        call check(error,.not. is_buffer_live(get_buffer("*Calendar*")), "q with a prefix closes *Calendar*")
        call set_buffer(old, st)
    end subroutine test_date_in_mode_line

    subroutine test_absolute_dates(error)
        type(error_type), allocatable, intent(out) :: error
        type(calendar_date_type) :: date
        integer :: month, year, a

        call check(error, calendar_absolute_from_gregorian(calendar_date_type(1, 1, 1)) == 1)
        if (allocated(error)) return
        call check(error, calendar_absolute_from_gregorian(calendar_date_type(12, 31, -1)) == 0)
        if (allocated(error)) return
        call check(error, calendar_absolute_from_gregorian(calendar_date_type(11, 3, 2026)) == 739923)
        if (allocated(error)) return
        call check(error, calendar_gregorian_from_absolute(739923) == calendar_date_type(11, 3, 2026))
        if (allocated(error)) return

        do a = 730000, 731000
            date = calendar_gregorian_from_absolute(a)
            call check(error, calendar_absolute_from_gregorian(date) == a)
            if (allocated(error)) return
        end do

        ! The examples of calendar-day-number's docstring.
        call check(error, calendar_day_number(calendar_date_type(1, 1, 1987)) == 1)
        if (allocated(error)) return
        call check(error, calendar_day_number(calendar_date_type(12, 31, 1980)) == 366)
        if (allocated(error)) return
        call check(error, is_calendar_leap_year(2000) .and. .not. is_calendar_leap_year(1900) &
                   .and. is_calendar_leap_year(-1) .and. .not. is_calendar_leap_year(-2))
        if (allocated(error)) return
        call check(error, calendar_last_day_of_month(2, 2028) == 29 .and. calendar_last_day_of_month(2, 2027) == 28)
        if (allocated(error)) return
        ! Tuesday.
        call check(error, calendar_day_of_week(calendar_date_type(11, 3, 2026)) == 2)
        if (allocated(error)) return
        month = 12
        year = 2026
        call calendar_increment_month(month, year, 1)
        call check(error, month == 1 .and. year == 2027)
        if (allocated(error)) return
        month = 1
        year = 1
        call calendar_increment_month(month, year, -1)
        call check(error, month == 12 .and. year == -1, "1 AD to 1 BC")
        if (allocated(error)) return
        call check(error, calendar_interval(12, -1, 1, 1) == 1)
        if (allocated(error)) return
        call check(error, calendar_date_from_day_of_year(2026, -1) == calendar_date_type(12, 31, 2026))
        if (allocated(error)) return
        call check(error, calendar_date_from_day_of_year(2024, 60) == calendar_date_type(2, 29, 2024))
        if (allocated(error)) return
        ! Thanksgiving 2026: the fourth Thursday of November.
        call check(error, calendar_nth_named_day(4, 4, 11, 2026) == calendar_date_type(11, 26, 2026))
        if (allocated(error)) return
        ! The last Monday of May 2026.
        call check(error, calendar_nth_named_day(-1, 1, 5, 2026) == calendar_date_type(5, 25, 2026))
        if (allocated(error)) return
        call check(error, calendar_dayname_on_or_before(0, 739923) == 739921)
    end subroutine test_absolute_dates

    subroutine test_date_strings(error)
        type(error_type), allocatable, intent(out) :: error
        type(calendar_date_type), parameter :: DATE = calendar_date_type(11, 3, 2026)
        integer :: saved

        saved = calendar_date_style
        call check(error, calendar_day_name(DATE), "Tuesday")
        if (allocated(error)) return
        call check(error, calendar_month_name(11, .true.), "Nov")
        if (allocated(error)) return
        calendar_date_style = CALENDAR_DATE_STYLE_AMERICAN
        call check(error, calendar_date_string(DATE), "Tuesday, November 3, 2026")
        if (allocated(error)) return
        call check(error, calendar_date_string(DATE, .true.), "Tue, Nov 3, 2026")
        if (allocated(error)) return
        call check(error, calendar_date_string(DATE, nodayname=.true.), "November 3, 2026")
        if (allocated(error)) return
        calendar_date_style = CALENDAR_DATE_STYLE_EUROPEAN
        call check(error, calendar_date_string(DATE), "Tuesday, 3 November 2026")
        if (allocated(error)) return
        calendar_date_style = CALENDAR_DATE_STYLE_ISO
        call check(error, calendar_date_string(DATE), "2026-11-03")
        calendar_date_style = saved
        if (allocated(error)) return
        call check(error, calendar_day_of_year_string(DATE), "Day 307 of 2026; 58 days remaining in the year")
        if (allocated(error)) return
        call check(error, calendar_day_of_year_string(calendar_date_type(12, 30, 2026)), &
                   "Day 364 of 2026; 1 day remaining in the year")
    end subroutine test_date_strings

    !> The three months around November 2026 and January 2027, as
    !> calendar.el:calendar-generate-month draws them with the default
    !> layout (a left margin of 5, months 25 columns apart, day columns 3
    !> wide, trailing spaces kept, no final newline).
    subroutine test_display(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, old
        type(status_type) :: st
        character(len=:), allocatable :: expected

        old = current_buffer()
        call calendar_buffer("*cal-test-display*", 11, 2026, h)
        expected = &
            "         October 2026            November 2026            December 2026    "//NL// &
            "     Su Mo Tu We Th Fr Sa     Su Mo Tu We Th Fr Sa     Su Mo Tu We Th Fr Sa "//NL// &
            "                  1  2  3      1  2  3  4  5  6  7            1  2  3  4  5 "//NL// &
            "      4  5  6  7  8  9 10      8  9 10 11 12 13 14      6  7  8  9 10 11 12 "//NL// &
            "     11 12 13 14 15 16 17     15 16 17 18 19 20 21     13 14 15 16 17 18 19 "//NL// &
            "     18 19 20 21 22 23 24     22 23 24 25 26 27 28     20 21 22 23 24 25 26 "//NL// &
            "     25 26 27 28 29 30 31     29 30                    27 28 29 30 31 "
        call check(error, buffer_string(), expected)
        if (allocated(error)) return
        call generate(1, 2027, st)
        expected = &
            "        December 2026             January 2027            February 2027    "//NL// &
            "     Su Mo Tu We Th Fr Sa     Su Mo Tu We Th Fr Sa     Su Mo Tu We Th Fr Sa "//NL// &
            "            1  2  3  4  5                     1  2         1  2  3  4  5  6 "//NL// &
            "      6  7  8  9 10 11 12      3  4  5  6  7  8  9      7  8  9 10 11 12 13 "//NL// &
            "     13 14 15 16 17 18 19     10 11 12 13 14 15 16     14 15 16 17 18 19 20 "//NL// &
            "     20 21 22 23 24 25 26     17 18 19 20 21 22 23     21 22 23 24 25 26 27 "//NL// &
            "     27 28 29 30 31           24 25 26 27 28 29 30     28 "//NL// &
            "                              31 "
        call check(error, buffer_string(), expected)
        if (allocated(error)) return
        call check(error, calendar_displayed_month() == 1 .and. calendar_displayed_year() == 2027)
        if (allocated(error)) return
        call generate(1, 1, st)
        call check(error,.not. st%ok(), "January 1 AD is not displayed")
        call set_buffer(old, st)
        call close_buffer(h, st)
    end subroutine test_display

    !> Weeks starting on Monday (`calendar_week_start_day' 1).
    subroutine test_display_week_start(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, old
        type(status_type) :: st
        character(len=:), allocatable :: expected

        old = current_buffer()
        calendar_week_start_day = 1
        call calendar_buffer("*cal-test-monday*", 11, 2026, h)
        calendar_week_start_day = 0
        expected = &
            "         October 2026            November 2026            December 2026    "//NL// &
            "     Mo Tu We Th Fr Sa Su     Mo Tu We Th Fr Sa Su     Mo Tu We Th Fr Sa Su "//NL// &
            "               1  2  3  4                        1         1  2  3  4  5  6 "//NL// &
            "      5  6  7  8  9 10 11      2  3  4  5  6  7  8      7  8  9 10 11 12 13 "//NL// &
            "     12 13 14 15 16 17 18      9 10 11 12 13 14 15     14 15 16 17 18 19 20 "//NL// &
            "     19 20 21 22 23 24 25     16 17 18 19 20 21 22     21 22 23 24 25 26 27 "//NL// &
            "     26 27 28 29 30 31        23 24 25 26 27 28 29     28 29 30 31 "//NL// &
            "                              30 "
        call check(error, buffer_string(), expected)
        call set_buffer(old, st)
        call close_buffer(h, st)
    end subroutine test_display_week_start

    subroutine test_movement(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, old
        type(status_type) :: st

        old = current_buffer()
        call calendar_buffer("*cal-test-move*", 11, 2026, h)
        call calendar_cursor_to_visible_date(calendar_date_type(11, 3, 2026))
        call check_cursor(error, 11, 3, 2026, "visible date")
        if (allocated(error)) return
        call calendar_forward_day(1, st)
        call check_cursor(error, 11, 4, 2026, "C-f")
        if (allocated(error)) return
        call calendar_backward_day(5, st)
        call check_cursor(error, 10, 30, 2026, "C-b 5")
        if (allocated(error)) return
        call calendar_forward_week(1, st)
        call check_cursor(error, 11, 6, 2026, "C-n")
        if (allocated(error)) return
        call calendar_beginning_of_week(1, st)
        call check_cursor(error, 11, 1, 2026, "C-a")
        if (allocated(error)) return
        call calendar_beginning_of_week(1, st)
        call check_cursor(error, 10, 25, 2026, "C-a on Sunday")
        if (allocated(error)) return
        call calendar_end_of_week(1, st)
        call check_cursor(error, 10, 31, 2026, "C-e")
        if (allocated(error)) return
        call calendar_beginning_of_month(1, st)
        call check_cursor(error, 10, 1, 2026, "M-a")
        if (allocated(error)) return
        call calendar_end_of_month(1, st)
        call check_cursor(error, 10, 31, 2026, "M-e")
        if (allocated(error)) return
        call calendar_forward_month(1, st)
        call check_cursor(error, 11, 30, 2026, "M-} keeps the day within the month")
        if (allocated(error)) return
        call calendar_backward_month(1, st)
        call check_cursor(error, 10, 30, 2026, "M-{")
        if (allocated(error)) return
        call calendar_goto_date(calendar_date_type(12, 25, 2026), st)
        call check_cursor(error, 12, 25, 2026, "g d")
        if (allocated(error)) return
        call check(error, calendar_displayed_month() == 11, "g d on a visible date does not scroll")
        if (allocated(error)) return
        call calendar_goto_day_of_year(2026, 307, st, noecho=.true.)
        call check_cursor(error, 11, 3, 2026, "g D")
        if (allocated(error)) return
        ! On the blank before the first date row, C-f goes to the nearest date.
        call calendar_cursor_to_visible_date(calendar_date_type(11, 1, 2026))
        call check(error, cursor_date() == calendar_date_type(11, 1, 2026))
        call set_buffer(old, st)
        call close_buffer(h, st)
    end subroutine test_movement

    subroutine test_movement_scrolls(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, old
        type(status_type) :: st

        old = current_buffer()
        call calendar_buffer("*cal-test-scroll*", 11, 2026, h)
        call calendar_cursor_to_visible_date(calendar_date_type(12, 30, 2026))
        call calendar_forward_day(3, st)
        call check_cursor(error, 1, 2, 2027, "C-f off the right edge")
        if (allocated(error)) return
        ! The new month appears at the right edge.
        call check(error, calendar_displayed_month() == 12 .and. calendar_displayed_year() == 2026, "edge")
        if (allocated(error)) return
        call calendar_forward_year(1, st)
        call check_cursor(error, 1, 2, 2028, "C-x ]")
        if (allocated(error)) return
        call check(error, calendar_displayed_month() == 12 .and. calendar_displayed_year() == 2027)
        if (allocated(error)) return
        call calendar_beginning_of_year(1, st)
        call check_cursor(error, 1, 1, 2028, "M-<")
        if (allocated(error)) return
        call calendar_end_of_year(1, st)
        call check_cursor(error, 12, 31, 2028, "M->")
        if (allocated(error)) return
        ! December at the right edge.
        call check(error, calendar_displayed_month() == 11 .and. calendar_displayed_year() == 2028, "M-> edge")
        if (allocated(error)) return
        call calendar_scroll_left(1, st)
        call check(error, calendar_displayed_month() == 12, "> scrolls one month")
        if (allocated(error)) return
        call check_cursor(error, 12, 31, 2028, "> keeps the visible date")
        if (allocated(error)) return
        call calendar_scroll_right(1, st)
        call calendar_scroll_calendar_left(1, st)
        call check(error, calendar_displayed_month() == 2 .and. calendar_displayed_year() == 2029, "C-v")
        if (allocated(error)) return
        ! The date left the calendar: the cursor goes to the first of
        ! the month in the same relative position.
        call check_cursor(error, 3, 1, 2029, "C-v moves the cursor")
        if (allocated(error)) return
        call calendar_goto_date(calendar_date_type(6, 15, 2020), st)
        call check_cursor(error, 6, 15, 2020, "g d far away")
        if (allocated(error)) return
        call check(error, calendar_displayed_month() == 6 .and. calendar_displayed_year() == 2020, "g d centers")
        call set_buffer(old, st)
        call close_buffer(h, st)
    end subroutine test_movement_scrolls

    subroutine test_mark(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, old
        type(status_type) :: st

        old = current_buffer()
        call calendar_buffer("*cal-test-mark*", 11, 2026, h)
        call calendar_exchange_point_and_mark(st)
        call check(error,.not. st%ok(), "No mark set in this buffer")
        if (allocated(error)) return
        call calendar_cursor_to_visible_date(calendar_date_type(11, 3, 2026))
        call calendar_set_mark(.false., st)
        call calendar_forward_week(2, st)
        call calendar_exchange_point_and_mark(st)
        call check_cursor(error, 11, 3, 2026, "C-x C-x")
        if (allocated(error)) return
        call calendar_exchange_point_and_mark(st)
        call check_cursor(error, 11, 17, 2026, "C-x C-x again")
        if (allocated(error)) return
        call calendar_set_mark(.true., st)
        call check_cursor(error, 11, 3, 2026, "C-u C-SPC")
        call set_buffer(old, st)
        call close_buffer(h, st)
    end subroutine test_mark

    subroutine test_more_months(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, old
        type(status_type) :: st

        old = current_buffer()
        call calendar_buffer("*cal-test-months*", 11, 2026, h)
        call calendar_cursor_to_visible_date(calendar_date_type(11, 3, 2026))
        call calendar_show_fewer_months(1, st)
        call check(error,.not. st%ok(), "at least 3 months")
        if (allocated(error)) return
        call check(error, calendar_total_months_local() == 3)
        if (allocated(error)) return
        call calendar_show_more_months(0, st)
        call check(error, st%ok() .and. calendar_total_months_local() == 3)
        call set_buffer(old, st)
        call close_buffer(h, st)
    end subroutine test_more_months
end module test_calendar_mod
