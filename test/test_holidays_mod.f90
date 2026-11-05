! test_holidays_mod.f90 -- tests for holidays_mod.
! Ports GNU Emacs 31.1 test/lisp/calendar/holidays-tests.el.
!
! Copyright (C) 2026 Free Software Foundation, Inc.
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

!> Ports holidays-test-holiday-easter-etc and holidays-test-more-months;
!> own tests of the Easter dates, the Julian and Islamic conversions, the
!> holidays of a calendar window, M-x list_holidays (whose expected text
!> was made by GNU Emacs from upstream's holidays.el, calendar.el,
!> cal-julian.el and cal-islam.el), the check of a date, h, x and u.
module test_holidays_mod
    use, intrinsic :: iso_fortran_env, only: int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_mod, only: status_type
    use handle_mod, only: handle_type
    use buffer_mod, only: buffers, current, current_buffer, set_buffer, get_or_make_buffer, close_buffer, get_buffer, &
                          is_buffer_live
    use editfns_mod, only: buffer_string, point_min, point_max
    use itree_mod, only: overlays_in
    use xdisp_mod, only: current_message
    use bindings_mod, only: init_bindings
    use handle_mod, only: null_handle
    use frame_mod, only: selected_frame, ensure_frame_windows
    use window_mod, only: delete_window_tree, resize_window_tree
    use calendar_mod, only: calendar_date_type, operator(==), calendar_mode, calendar_generate, calendar_current_date, &
                            calendar_increment_month, calendar_absolute_from_gregorian, calendar_gregorian_from_absolute, &
                            calendar_dayname_on_or_before, calendar_display_type, set_calendar_display, &
                            restore_calendar_display, calendar_unmark, calendar_mark_holidays_variable, &
                            calendar_christian_all_holidays_flag, calendar_islamic_all_holidays_flag
    use holidays_mod, only: holiday_type, holiday_entry_type, holidays_type, HOLIDAY_ENTRY_EXPRESSION, &
                            holiday_easter_etc, holiday_easter_etc_absolute, holiday_greek_orthodox_easter_absolute, &
                            holiday_float, holiday_julian, holiday_advent, holiday_islamic, holiday_islamic_new_year, &
                            holiday_greek_orthodox_easter, holiday_in_range, calendar_julian_from_absolute, &
                            calendar_julian_to_absolute, calendar_islamic_from_absolute, calendar_islamic_to_absolute, &
                            calendar_list_holidays, list_holidays, calendar_check_holidays, calendar_cursor_holidays, &
                            calendar_mark_holidays
    implicit none(type, external)
    private

    public :: collect_holidays_mod

    character(len=*), parameter :: NL = achar(10)

    ! The text of M-x list_holidays for 2026 with the default lists, and
    ! for 2024 with `calendar_christian_all_holidays_flag' and
    ! `calendar_islamic_all_holidays_flag' true.
    character(len=*), parameter :: Y2026_LINES(*) = [character(len=64) :: &
                                                     "Thursday, January 1, 2026: New Year's Day", &
                                                     "Monday, January 19, 2026: Martin Luther King Day", &
                                                     "Monday, February 2, 2026: Groundhog Day", &
                                                     "Saturday, February 14, 2026: Valentine's Day", &
                                                     "Monday, February 16, 2026: President's Day", &
                                                     "Wednesday, February 18, 2026: Ramadan Begins", &
                                                     "Wednesday, February 18, 2026: Ash Wednesday", &
                                                     "Tuesday, March 17, 2026: St. Patrick's Day", &
                                                     "Wednesday, April 1, 2026: April Fools' Day", &
                                                     "Friday, April 3, 2026: Good Friday", &
                                                     "Sunday, April 5, 2026: Easter Sunday", &
                                                     "Sunday, May 10, 2026: Mother's Day", &
                                                     "Monday, May 25, 2026: Memorial Day", &
                                                     "Sunday, June 14, 2026: Flag Day", &
                                                     "Wednesday, June 17, 2026: Islamic New Year 1448", &
                                                     "Sunday, June 21, 2026: Father's Day", &
                                                     "Saturday, July 4, 2026: Independence Day", &
                                                     "Monday, September 7, 2026: Labor Day", &
                                                     "Monday, October 12, 2026: Columbus Day", &
                                                     "Saturday, October 31, 2026: Halloween", &
                                                     "Wednesday, November 11, 2026: Veteran's Day", &
                                                     "Thursday, November 26, 2026: Thanksgiving", &
                                                     "Friday, December 25, 2026: Christmas"]
    character(len=*), parameter :: Y2024_ALL_LINES(*) = [character(len=64) :: &
                                                         "Monday, January 1, 2024: New Year's Day", &
                                                         "Saturday, January 6, 2024: Epiphany", &
                                                         "Sunday, January 7, 2024: Christmas (Julian calendar)", &
                                                         "Monday, January 15, 2024: Martin Luther King Day", &
                                                         "Sunday, January 28, 2024: Septuagesima Sunday", &
                                                         "Friday, February 2, 2024: Groundhog Day", &
                                                         "Sunday, February 4, 2024: Sexagesima Sunday", &
                                                         "Tuesday, February 6, 2024: Shab-e-Mi'raj", &
                                                         "Sunday, February 11, 2024: Shrove Sunday", &
                                                         "Monday, February 12, 2024: Shrove Monday", &
                                                         "Tuesday, February 13, 2024: Shrove Tuesday", &
                                                         "Wednesday, February 14, 2024: Ash Wednesday", &
                                                         "Wednesday, February 14, 2024: Valentine's Day", &
                                                         "Monday, February 19, 2024: President's Day", &
                                                         "Sunday, February 25, 2024: Shab-e-Bara't", &
                                                         "Monday, March 11, 2024: Ramadan Begins", &
                                                         "Sunday, March 17, 2024: Passion Sunday", &
                                                         "Sunday, March 17, 2024: St. Patrick's Day", &
                                                         "Sunday, March 24, 2024: Palm Sunday", &
                                                         "Thursday, March 28, 2024: Maundy Thursday", &
                                                         "Friday, March 29, 2024: Good Friday", &
                                                         "Sunday, March 31, 2024: Easter Sunday", &
                                                         "Monday, April 1, 2024: April Fools' Day", &
                                                         "Saturday, April 6, 2024: Shab-e Qadr", &
                                                         "Wednesday, April 10, 2024: Id-al-Fitr", &
                                                         "Sunday, May 5, 2024: Pascha (Greek Orthodox Easter)", &
                                                         "Sunday, May 5, 2024: Rogation Sunday", &
                                                         "Thursday, May 9, 2024: Ascension Day", &
                                                         "Sunday, May 12, 2024: Mother's Day", &
                                                         "Sunday, May 19, 2024: Pentecost (Whitsunday)", &
                                                         "Monday, May 20, 2024: Whitmonday", &
                                                         "Sunday, May 26, 2024: Trinity Sunday", &
                                                         "Monday, May 27, 2024: Memorial Day", &
                                                         "Thursday, May 30, 2024: Corpus Christi", &
                                                         "Friday, June 14, 2024: Flag Day", &
                                                         "Sunday, June 16, 2024: Father's Day", &
                                                         "Monday, June 17, 2024: Id-al-Adha", &
                                                         "Thursday, July 4, 2024: Independence Day", &
                                                         "Monday, July 8, 2024: Islamic New Year 1446", &
                                                         "Wednesday, July 17, 2024: Ashura", &
                                                         "Thursday, August 15, 2024: Assumption", &
                                                         "Monday, September 2, 2024: Labor Day", &
                                                         "Monday, September 16, 2024: Mulad-al-Nabi", &
                                                         "Monday, October 14, 2024: Columbus Day", &
                                                         "Thursday, October 31, 2024: Halloween", &
                                                         "Monday, November 11, 2024: Veteran's Day", &
                                                         "Thursday, November 28, 2024: Thanksgiving", &
                                                         "Sunday, December 1, 2024: Advent", &
                                                         "Wednesday, December 25, 2024: Christmas"]

contains

    !> Collect all exported unit tests.
    subroutine collect_holidays_mod(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("holidays-test-holiday-easter-etc", test_holiday_easter_etc), &
                    new_unittest("holidays-test-more-months", test_more_months), &
                    new_unittest("holidays-easter-dates", test_easter_dates), &
                    new_unittest("holidays-conversions", test_conversions), &
                    new_unittest("holidays-window", test_window), &
                    new_unittest("holidays-list-holidays", test_list_holidays), &
                    new_unittest("holidays-list-holidays-all", test_list_holidays_all), &
                    new_unittest("holidays-expression", test_expression), &
                    new_unittest("holidays-calendar-list-holidays", test_calendar_list_holidays), &
                    new_unittest("holidays-check-holidays", test_check_holidays), &
                    new_unittest("holidays-mark", test_mark) &
                    ]
    end subroutine collect_holidays_mod

    ! The bindings, and one window of 24 lines by 80 columns on an
    ! 80-column frame, as the frame of `emacs --batch' that made the
    ! expected text.
    subroutine setup()
        call init_bindings()
        selected_frame%minibuffer_selected = .false.
        call delete_window_tree(selected_frame%root_window)
        selected_frame%root_window = null_handle()
        call ensure_frame_windows()
        call resize_window_tree(selected_frame%root_window, 0, 0, 24, 80, 80)
        selected_frame%tty%wcm%cm_cols = 80
    end subroutine setup

    ! LINES joined with newlines.
    function joined(lines) result(text)
        character(len=*), intent(in) :: lines(:)
        character(len=:), allocatable :: text
        integer :: k

        text = ""

        do k = 1, size(lines)
            if (k > 1) text = text//NL
            text = text//trim(lines(k))
        end do
    end function joined

    ! The text and the mode line of the "*Holidays*" buffer.
    subroutine holiday_buffer_text(text, mode_line)
        character(len=:), allocatable, intent(out) :: text, mode_line
        type(handle_type) :: h, old
        type(status_type) :: st

        text = ""
        mode_line = ""
        h = get_buffer("*Holidays*")
        if (.not. is_buffer_live(h)) return
        old = current_buffer()
        call set_buffer(h, st)
        text = buffer_string()

        if (allocated(buffers(current)%mode_line_format%segments)) then
            mode_line = buffers(current)%mode_line_format%segments(1)%text
        end if

        call set_buffer(old, st)
    end subroutine holiday_buffer_text

    ! Whether HOLIDAYS is the one holiday MONTH/DAY/YEAR called STRING.
    logical function is_one_holiday(holidays, month, day, year, string)
        type(holiday_type), intent(in) :: holidays(:)
        integer, intent(in) :: month, day, year
        character(len=*), intent(in) :: string

        is_one_holiday = .false.
        if (size(holidays) /= 1) return
        is_one_holiday = holidays(1)%date == calendar_date_type(month, day, year) .and. holidays(1)%string == string
    end function is_one_holiday

    subroutine test_holiday_easter_etc(error)
        type(error_type), allocatable, intent(out) :: error
        type(holiday_type), allocatable :: holidays(:)
        type(calendar_display_type) :: saved

        call setup()
        call set_calendar_display(12, 2026, 3, saved)
        call holiday_easter_etc(holidays, -63, "Septuagesima Sunday")
        call check(error, is_one_holiday(holidays, 1, 24, 2027, "Septuagesima Sunday"))

        if (.not. allocated(error)) then
            call holiday_easter_etc(holidays, -56, "Sexagesima Sunday")
            call check(error, is_one_holiday(holidays, 1, 31, 2027, "Sexagesima Sunday"))
        end if

        call restore_calendar_display(saved)
    end subroutine test_holiday_easter_etc

    ! Return holidays starting from YR, MON in a span of YEARS.
    function get_holidays(mon, yr, years, months) result(res)
        integer, intent(in) :: mon, yr, years, months
        character(len=:), allocatable :: res, text, mode_line
        type(calendar_display_type) :: saved
        type(status_type) :: st
        integer :: month, year, k

        month = mon
        year = yr
        res = ""

        do k = 1, (years*12)/months
            call set_calendar_display(month, year, months, saved)
            call calendar_list_holidays(st)
            call restore_calendar_display(saved)
            call holiday_buffer_text(text, mode_line)

            if (len(res) > 0) then
                res = res//NL//text
            else
                res = text
            end if

            call calendar_increment_month(month, year, months)
        end do
    end function get_holidays

    subroutine test_more_months(error)
        type(error_type), allocatable, intent(out) :: error
        type(calendar_date_type) :: today
        character(len=:), allocatable :: holiday_list

        call setup()
        today = calendar_current_date()
        holiday_list = get_holidays(today%month, today%year, 1, 3)
        call check(error, holiday_list == get_holidays(today%month, today%year, 1, 12), holiday_list)
    end subroutine test_more_months

    subroutine test_easter_dates(error)
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: YEARS(7) = [2000, 2019, 2024, 2025, 2026, 2027, 2100]
        type(calendar_date_type), parameter :: EASTER(7) = [ &
                                               calendar_date_type(4, 23, 2000), calendar_date_type(4, 21, 2019), &
                                               calendar_date_type(3, 31, 2024), calendar_date_type(4, 20, 2025), &
                                               calendar_date_type(4, 5, 2026), calendar_date_type(3, 28, 2027), &
                                               calendar_date_type(3, 28, 2100)]
        type(calendar_date_type), parameter :: PASCHA(7) = [ &
                                               calendar_date_type(4, 30, 2000), calendar_date_type(4, 28, 2019), &
                                               calendar_date_type(5, 5, 2024), calendar_date_type(4, 20, 2025), &
                                               calendar_date_type(4, 12, 2026), calendar_date_type(5, 2, 2027), &
                                               calendar_date_type(5, 2, 2100)]
        integer :: k

        do k = 1, size(YEARS)
            call check(error, calendar_gregorian_from_absolute(holiday_easter_etc_absolute(YEARS(k))) == EASTER(k), &
                       "Easter")
            if (allocated(error)) return
            call check(error, calendar_gregorian_from_absolute(holiday_greek_orthodox_easter_absolute(YEARS(k))) &
                       == PASCHA(k), "Pascha")
            if (allocated(error)) return
        end do
    end subroutine test_easter_dates

    subroutine test_conversions(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, calendar_julian_from_absolute(calendar_absolute_from_gregorian(calendar_date_type(1, 7, 2027))) &
                   == calendar_date_type(12, 25, 2026))
        if (allocated(error)) return
        call check(error, calendar_julian_to_absolute(calendar_date_type(12, 25, 2026)) == 739988)
        if (allocated(error)) return
        call check(error, calendar_islamic_from_absolute(calendar_absolute_from_gregorian(calendar_date_type(6, 17, 2026))) &
                   == calendar_date_type(1, 1, 1448))
        if (allocated(error)) return
        call check(error, calendar_islamic_to_absolute(calendar_date_type(1, 1, 1448)) == 739784)
        if (allocated(error)) return
        ! Before the epoch (July 16, 622 AD, Julian, absolute 227015).
        call check(error, calendar_islamic_from_absolute(227014) == calendar_date_type(0, 0, 0))
    end subroutine test_conversions

    subroutine test_window(error)
        type(error_type), allocatable, intent(out) :: error
        type(holiday_type), allocatable :: holidays(:)
        type(calendar_display_type) :: saved
        type(status_type) :: st

        call setup()
        call set_calendar_display(12, 2026, 3, saved)

        checks: block
            call holiday_float(1, 1, 3, "MLK", holidays)
            call check(error, is_one_holiday(holidays, 1, 18, 2027, "MLK"), "holiday_float")
            if (allocated(error)) exit checks
            call holiday_julian(12, 25, "Jul", holidays)
            call check(error, is_one_holiday(holidays, 1, 7, 2027, "Jul"), "holiday_julian")
            if (allocated(error)) exit checks
            call holiday_advent(holidays, 0, "Advent")
            call check(error, is_one_holiday(holidays, 11, 29, 2026, "Advent"), "holiday_advent")
            if (allocated(error)) exit checks
            call holiday_islamic(9, 1, "Ramadan", holidays)
            call check(error, size(holidays) == 0, "holiday_islamic, not visible")
            if (allocated(error)) exit checks
            call holiday_in_range(calendar_absolute_from_gregorian(calendar_date_type(12, 20, 2026)), &
                                  calendar_absolute_from_gregorian(calendar_date_type(1, 20, 2027)), holidays)
            call check(error, size(holidays) == 3, "holiday_in_range")
            if (allocated(error)) exit checks
            call check(error, holidays(1)%date == calendar_date_type(1, 18, 2027) .and. &
                       holidays(3)%string == "Christmas", "holiday_in_range order")
            if (allocated(error)) exit checks
            call restore_calendar_display(saved)
            call set_calendar_display(3, 2027, 3, saved)
            call holiday_islamic(9, 1, "Ramadan", holidays)
            call check(error, is_one_holiday(holidays, 2, 8, 2027, "Ramadan"), "holiday_islamic")
            if (allocated(error)) exit checks
            call holiday_islamic_new_year(holidays, st)
            call check(error, size(holidays) == 0, "holiday_islamic_new_year")
            if (allocated(error)) exit checks
            call holiday_greek_orthodox_easter(holidays)
            call check(error, size(holidays) == 0, "holiday_greek_orthodox_easter")
        end block checks

        call restore_calendar_display(saved)
    end subroutine test_window

    subroutine test_list_holidays(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: text, mode_line
        type(status_type) :: st

        call setup()
        call list_holidays(2026, st)
        call check(error, st%ok(), "list_holidays")
        if (allocated(error)) return
        call holiday_buffer_text(text, mode_line)
        call check(error, text == joined(Y2026_LINES), text)
        if (allocated(error)) return
        call check(error, mode_line == repeat("-", 31)//"Holidays for 2026"//repeat("-", 32), mode_line)
    end subroutine test_list_holidays

    subroutine test_list_holidays_all(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: text, mode_line
        type(status_type) :: st

        call setup()
        calendar_christian_all_holidays_flag = .true.
        calendar_islamic_all_holidays_flag = .true.
        call list_holidays(2024, st)
        calendar_christian_all_holidays_flag = .false.
        calendar_islamic_all_holidays_flag = .false.
        call holiday_buffer_text(text, mode_line)
        call check(error, text == joined(Y2024_ALL_LINES), text)
        if (allocated(error)) return
        call check(error, mode_line == repeat("-", 31)//"Holidays for 2024"//repeat("-", 32), mode_line)
    end subroutine test_list_holidays_all

    ! US presidential elections: the first Tuesday after the first Monday
    ! in November of years divisible by 4 (the example of
    ! `calendar_holidays').
    subroutine election(year, date, string, status)
        integer, intent(in) :: year
        type(calendar_date_type), intent(out) :: date
        character(len=:), allocatable, intent(inout) :: string
        type(status_type), intent(out) :: status

        status = status_type()
        if (len(string) == 0) string = "Election"
        if (mod(year, 4) == 0) date = calendar_gregorian_from_absolute(1 + calendar_dayname_on_or_before(1, &
                                                                                             6 + calendar_absolute_from_gregorian( &
                                                                                                  calendar_date_type(11, 1, year))))
    end subroutine election

    subroutine test_expression(error)
        type(error_type), allocatable, intent(out) :: error
        type(holidays_type) :: l
        character(len=:), allocatable :: text, mode_line
        type(status_type) :: st

        call setup()
        call l%append(holiday_entry_type(kind=HOLIDAY_ENTRY_EXPRESSION, expression=election, &
                                         string="US Presidential Election"))
        call list_holidays(2027, st, 2028, l, "Election Holidays")
        call holiday_buffer_text(text, mode_line)
        call check(error, text == "Tuesday, November 7, 2028: US Presidential Election", text)
        if (allocated(error)) return
        call check(error, index(mode_line, "-Election Holidays for 2027-2028-") > 0, mode_line)
    end subroutine test_expression

    subroutine test_calendar_list_holidays(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: text, mode_line
        type(calendar_display_type) :: saved
        type(status_type) :: st
        logical :: found

        call setup()
        call set_calendar_display(12, 2026, 3, saved)
        call calendar_list_holidays(st, found)
        call restore_calendar_display(saved)
        call check(error, st%ok() .and. found, "calendar_list_holidays")
        if (allocated(error)) return
        call holiday_buffer_text(text, mode_line)
        call check(error, text == "Wednesday, November 11, 2026: Veteran's Day"//NL// &
                   "Thursday, November 26, 2026: Thanksgiving"//NL// &
                   "Friday, December 25, 2026: Christmas"//NL// &
                   "Friday, January 1, 2027: New Year's Day"//NL// &
                   "Monday, January 18, 2027: Martin Luther King Day", text)
        if (allocated(error)) return
        ! "%-" shows as dashes to the end of the mode line.
        call check(error, mode_line == repeat("-", 14)//"Notable Dates from November, 2026 to January, 2027" &
                   //repeat("-", 16), mode_line)
    end subroutine test_calendar_list_holidays

    subroutine test_check_holidays(error)
        type(error_type), allocatable, intent(out) :: error
        type(holiday_type), allocatable :: strings(:)
        character(len=:), allocatable :: text
        type(status_type) :: st

        call setup()
        call calendar_check_holidays(calendar_date_type(2, 18, 2026), strings)
        call check(error, size(strings) == 2, "two holidays")
        if (allocated(error)) return
        call check(error, strings(1)%string == "Ramadan Begins" .and. strings(2)%string == "Ash Wednesday")
        if (allocated(error)) return
        call calendar_cursor_holidays(st, calendar_date_type(2, 18, 2026))
        call current_message(text)
        call check(error, text == "Wednesday, February 18, 2026:  Ramadan Begins;  Ash Wednesday", text)
        if (allocated(error)) return
        call calendar_cursor_holidays(st, calendar_date_type(2, 19, 2026))
        call current_message(text)
        call check(error, text == "No holidays known for Thursday, February 19, 2026", text)
    end subroutine test_check_holidays

    subroutine test_mark(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, old
        type(handle_type), allocatable :: ovs(:)
        type(status_type) :: st

        call setup()
        old = current_buffer()
        call get_or_make_buffer("*hol-test-mark*", h, st)
        call set_buffer(h, st)
        call calendar_mode(st)
        buffers(current)%read_only = .false.
        call calendar_generate(12, 2026, st)
        buffers(current)%read_only = .true.
        call calendar_mark_holidays(st)
        call overlays_in(point_min(), point_max(), ovs)
        call check(error, st%ok() .and. calendar_mark_holidays_variable .and. size(ovs) == 5, "x")

        if (.not. allocated(error)) then
            call calendar_unmark(st)
            call overlays_in(point_min(), point_max(), ovs)
            call check(error,.not. calendar_mark_holidays_variable .and. size(ovs) == 0, "u")
        end if

        call set_buffer(old, st)
        call close_buffer(h, st)
    end subroutine test_mark
end module test_holidays_mod
