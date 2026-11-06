! test_diary_library_mod.f90 -- tests of the diary.
! Own tests; upstream has no diary-lib-tests.el.
!
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

!> Own tests of the diary: the times of entries, the date forms and the
!> expression entries a range of days lists, the fancy display, the
!> marks in the calendar, a bad expression entry, the entries the
!> calendar's i keys insert and the European date forms.  The expected
!> entries, fancy text and marks were made by GNU Emacs 29 running
!> upstream's 31.1 calendar.el, holidays.el and diary-lib.el on the same
!> diary file.
module test_diary_library_mod
    use, intrinsic :: iso_fortran_env, only: int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_mod, only: status_type
    use handle_mod, only: handle_type, null_handle
    use buffer_mod, only: buffers, current, current_buffer, set_buffer, get_or_make_buffer, close_buffer, get_buffer, &
                          is_buffer_live, restore_buffer_modified
    use editing_functions_mod, only: buffer_string, point_minimum, point_maximum
    use interval_tree_mod, only: overlays_in, overlay_get
    use value_mod, only: value_type
    use display_engine_mod, only: current_message
    use bindings_mod, only: initialize_bindings
    use frame_mod, only: selected_frame, ensure_frame_windows
    use window_mod, only: delete_window_tree, resize_window_tree
    use file_input_output_mod, only: expand_file_name, fortmacs_wd, initialize_fileio, delete_file_internal, &
                                     create_directory_internal
    use calendar_mod, only: calendar_date_type, operator(==), calendar_mode, calendar_generate, calendar_unmark, &
                            calendar_goto_date, calendar_set_mark, calendar_date_style, diary_file, &
                            diary_date_forms, calendar_mark_diary_entries, CALENDAR_DATE_STYLE_AMERICAN, &
                            CALENDAR_DATE_STYLE_EUROPEAN, CALENDAR_DATE_STYLE_ISO, diary_european_date_forms, &
                            diary_american_date_forms, diary_fancy_buffer, diary_file_buffer
    use diary_library_mod, only: diary_entry_type, operator(<), diary_entry_time, diary_ordinal_suffix, make_diary_date, &
                                 list_diary_entries, mark_diary_entries, insert_diary_entry, insert_weekly_diary_entry, &
                                 insert_monthly_diary_entry, insert_yearly_diary_entry, insert_anniversary_diary_entry, &
                                 insert_block_diary_entry, diary_entries, sort_diary_entries
    implicit none(type, external)
    private

    public :: collect_diary_library_mod

    character(len=*), parameter :: NEWLINE = achar(10)

    ! The diary file of the tests: calendar.el's sample entries, a
    ! date-only entry and expression entries.
    character(len=*), parameter :: DIARY_LINES(*) = [character(len=64) :: &
                                                     "12/22/1988 Twentieth wedding anniversary!!", &
                                                     "&1/1. Happy New Year!", &
                                                     "10/22 Ruth's birthday.", &
                                                     "21: Payday", &
                                                     "Tuesday--weekly meeting with grad students at 10am", &
                                                     "         Supowit, Shen, Bitner, and Kapoor to attend.", &
                                                     "1/13/89 Friday the thirteenth!!", &
                                                     "&thu 4pm squash game with Lloyd.", &
                                                     "mar 16 Dad's birthday", &
                                                     "April 15, 1989 Income tax due.", &
                                                     "&* 15 time cards due.", &
                                                     "11/11 Lunch", &
                                                     "11/5/2026", &
                                                     " 2pm Committee meeting", &
                                                     " 4:00pm Jamie", &
                                                     "%%(diary-block 11 1 2026 11 10 2026) Vacation", &
                                                     "%%(diary-anniversary 10 31 1948) Arthur's %d%s birthday", &
                                                     "&%%(diary-cyclic 50 3 1 2026) Renew medication (%d)", &
                                                     "%%(diary-float t 4 2) 2nd Thursday", &
                                                     "%%(diary-remind '(diary-date 11 20 2026) 3) Dentist", &
                                                     "%%(diary-date t 5 t ""*"") Fifth"]

    ! The fancy display of November 10 and 11, 2026, and of November 4
    ! and 5, 2026.
    character(len=*), parameter :: FANCY_10(*) = [character(len=64) :: &
                                                  "Tuesday, November 10, 2026", &
                                                  "==========================", &
                                                  "Vacation", &
                                                  "-weekly meeting with grad students at 10am", &
                                                  "         Supowit, Shen, Bitner, and Kapoor to attend.", &
                                                  "", &
                                                  "Wednesday, November 11, 2026:  Veteran's Day", &
                                                  "============================================", &
                                                  "Lunch", &
                                                  ""]
    character(len=*), parameter :: FANCY_4(*) = [character(len=64) :: &
                                                 "Wednesday, November 4, 2026", &
                                                 "===========================", &
                                                 "Vacation", &
                                                 "", &
                                                 "Thursday, November 5, 2026", &
                                                 "==========================", &
                                                 "Vacation", &
                                                 "Fifth", &
                                                 " 2pm Committee meeting", &
                                                 " 4:00pm Jamie", &
                                                 "4pm squash game with Lloyd.", &
                                                 ""]

contains

    !> Collect all exported unit tests.
    subroutine collect_diary_library_mod(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("diary-entry-time", test_entry_time), &
                    new_unittest("diary-ordinal-suffix", test_ordinal_suffix), &
                    new_unittest("diary-make-date", test_make_date), &
                    new_unittest("diary-list-entries", test_list_entries), &
                    new_unittest("diary-expression-entries", test_expression_entries), &
                    new_unittest("diary-sort-entries", test_sort_entries), &
                    new_unittest("diary-fancy-display", test_fancy_display), &
                    new_unittest("diary-mark-entries", test_mark_entries), &
                    new_unittest("diary-bad-expression", test_bad_expression), &
                    new_unittest("diary-insert-entries", test_insert_entries), &
                    new_unittest("diary-european-date-forms", test_european_date_forms) &
                    ]
    end subroutine collect_diary_library_mod

    ! The bindings, and one window of 24 lines by 80 columns on an
    ! 80-column frame, as the frame of `emacs --batch' that made the
    ! expected text; the American date style.
    subroutine setup()
        call initialize_bindings()
        selected_frame%minibuffer_selected = .false.
        call delete_window_tree(selected_frame%root_window)
        selected_frame%root_window = null_handle()
        call ensure_frame_windows()
        call resize_window_tree(selected_frame%root_window, 0, 0, 24, 80, 80)
        selected_frame%terminal%wcm%cursor_motion_columns = 80
        calendar_date_style = CALENDAR_DATE_STYLE_AMERICAN
        diary_date_forms = diary_american_date_forms
    end subroutine setup

    ! LINES joined with newlines.
    function joined(lines) result(text)
        character(len=*), intent(in) :: lines(:)
        character(len=:), allocatable :: text
        integer :: k

        text = ""

        do k = 1, size(lines)
            if (k > 1) text = text//NEWLINE
            text = text//trim(lines(k))
        end do
    end function joined

    ! Write TEXT to the diary file NAME under build/test-diary, closing a
    ! buffer that visits it, and make it `diary_file'.
    subroutine write_diary(name, text)
        character(len=*), intent(in) :: name, text
        character(len=:), allocatable :: path
        type(handle_type) :: h
        type(status_type) :: st
        integer :: u

        call initialize_fileio()
        call expand_file_name("build/test-diary", path, st, fortmacs_wd)
        call create_directory_internal(path, st)
        path = path//"/"//name
        diary_file = path
        h = diary_file_buffer()

        if (is_buffer_live(h)) then
            call set_buffer(h, st)
            call restore_buffer_modified(.false.)
            call close_buffer(h, st)
        end if

        call delete_file_internal(path, st)
        open (newunit=u, file=path, access="stream", form="unformatted", status="replace", action="write")
        write (u) text//NEWLINE
        close (u)
    end subroutine write_diary

    ! The entries of ENTRIES as "MONTH/DAY/YEAR STRING | SPECIFIER" lines.
    function entry_lines(entries) result(text)
        type(diary_entry_type), intent(in) :: entries(:)
        character(len=:), allocatable :: text
        character(len=16) :: date
        integer :: k

        text = ""

        do k = 1, size(entries)
            write (date, '(i0, "/", i0, "/", i0)') entries(k)%date%month, entries(k)%date%day, entries(k)%date%year
            text = text//trim(date)//" "//entries(k)%string//" | "//entries(k)%specifier//NEWLINE
        end do
    end function entry_lines

    subroutine test_entry_time(error)
        type(error_type), allocatable, intent(out) :: error

        call setup()
        call check(error, diary_entry_time("1325 lunch") == 1325, "1325")
        if (allocated(error)) return
        call check(error, diary_entry_time(" 9:05 x") == 905, "9:05")
        if (allocated(error)) return
        call check(error, diary_entry_time("10.30 x") == 1030, "10.30")
        if (allocated(error)) return
        call check(error, diary_entry_time("4pm squash") == 1600, "4pm")
        if (allocated(error)) return
        call check(error, diary_entry_time("12am x") == 0, "12am")
        if (allocated(error)) return
        call check(error, diary_entry_time("4:30pm x") == 1630, "4:30pm")
        if (allocated(error)) return
        call check(error, diary_entry_time("4:30PM x") == 430, "PM is not pm")
        if (allocated(error)) return
        call check(error, diary_entry_time("Lunch") == -9999, "none")
    end subroutine test_entry_time

    subroutine test_ordinal_suffix(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, diary_ordinal_suffix(1) == "st" .and. diary_ordinal_suffix(2) == "nd" .and. &
                   diary_ordinal_suffix(3) == "rd" .and. diary_ordinal_suffix(4) == "th" .and. &
                   diary_ordinal_suffix(11) == "th" .and. diary_ordinal_suffix(12) == "th" .and. &
                   diary_ordinal_suffix(13) == "th" .and. diary_ordinal_suffix(21) == "st" .and. &
                   diary_ordinal_suffix(102) == "nd" .and. diary_ordinal_suffix(111) == "th")
    end subroutine test_ordinal_suffix

    subroutine test_make_date(error)
        type(error_type), allocatable, intent(out) :: error

        call setup()
        call check(error, make_diary_date(11, 5, 2026) == calendar_date_type(11, 5, 2026), "american")
        if (allocated(error)) return
        calendar_date_style = CALENDAR_DATE_STYLE_EUROPEAN
        call check(error, make_diary_date(5, 11, 2026) == calendar_date_type(11, 5, 2026), "european")
        if (allocated(error)) return
        calendar_date_style = CALENDAR_DATE_STYLE_ISO
        call check(error, make_diary_date(2026, 11, 5) == calendar_date_type(11, 5, 2026), "iso")
        calendar_date_style = CALENDAR_DATE_STYLE_AMERICAN
    end subroutine test_make_date

    subroutine test_list_entries(error)
        type(error_type), allocatable, intent(out) :: error
        type(diary_entry_type), allocatable :: entries(:)
        character(len=:), allocatable :: expected
        type(status_type) :: st

        call setup()
        call write_diary("diary-list", joined(DIARY_LINES))
        call list_diary_entries(calendar_date_type(11, 5, 2026), st, 1, .true., entries)
        expected = "11/5/2026 Vacation | %%(diary-block 11 1 2026 11 10 2026)"//NEWLINE// &
                   "11/5/2026 Fifth | %%(diary-date t 5 t ""*"")"//NEWLINE// &
                   "11/5/2026  2pm Committee meeting"//NEWLINE//" 4:00pm Jamie | "//NEWLINE//NEWLINE// &
                   "11/5/2026 4pm squash game with Lloyd. | &thu"//NEWLINE
        call check(error, st%ok() .and. entry_lines(entries) == expected, "November 5: "//entry_lines(entries))
        if (allocated(error)) return
        call list_diary_entries(calendar_date_type(11, 10, 2026), st, 2, .true., entries)
        expected = "11/10/2026 Vacation | %%(diary-block 11 1 2026 11 10 2026)"//NEWLINE// &
                   "11/10/2026 -weekly meeting with grad students at 10am"//NEWLINE// &
                   "         Supowit, Shen, Bitner, and Kapoor to attend. | Tuesday"//NEWLINE// &
                   "11/11/2026 Lunch | 11/11"//NEWLINE
        call check(error, st%ok() .and. entry_lines(entries) == expected, "November 10: "//entry_lines(entries))
    end subroutine test_list_entries

    subroutine test_expression_entries(error)
        type(error_type), allocatable, intent(out) :: error
        type(diary_entry_type), allocatable :: entries(:)
        character(len=:), allocatable :: expected
        type(status_type) :: st

        call setup()
        call write_diary("diary-expressions", joined(DIARY_LINES))
        call list_diary_entries(calendar_date_type(11, 17, 2026), st, 3, .true., entries)
        expected = "11/17/2026 Reminder: Only 3 days until Dentist | %%(diary-remind '(diary-date 11 20 2026) 3)"//NEWLINE// &
                   "11/17/2026 -weekly meeting with grad students at 10am"//NEWLINE// &
                   "         Supowit, Shen, Bitner, and Kapoor to attend. | Tuesday"//NEWLINE// &
                   "11/19/2026 4pm squash game with Lloyd. | &thu"//NEWLINE
        call check(error, st%ok() .and. entry_lines(entries) == expected, "remind: "//entry_lines(entries))
        if (allocated(error)) return
        call list_diary_entries(calendar_date_type(10, 31, 2026), st, 1, .true., entries)
        expected = "10/31/2026 Arthur's 78th birthday | %%(diary-anniversary 10 31 1948)"//NEWLINE
        call check(error, st%ok() .and. entry_lines(entries) == expected, "anniversary: "//entry_lines(entries))
        if (allocated(error)) return
        call list_diary_entries(calendar_date_type(11, 6, 2026), st, 1, .true., entries)
        expected = "11/6/2026 Vacation | %%(diary-block 11 1 2026 11 10 2026)"//NEWLINE// &
                   "11/6/2026 Renew medication (5) | &%%(diary-cyclic 50 3 1 2026)"//NEWLINE
        call check(error, st%ok() .and. entry_lines(entries) == expected, "cyclic: "//entry_lines(entries))
        if (allocated(error)) return
        call list_diary_entries(calendar_date_type(11, 12, 2026), st, 1, .true., entries)
        call check(error, st%ok() .and. index(entry_lines(entries), "11/12/2026 2nd Thursday | %%(diary-float t 4 2)") == 1, &
                   "float: "//entry_lines(entries))
    end subroutine test_expression_entries

    subroutine test_sort_entries(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st

        call setup()
        call write_diary("diary-sort", "11/5/2026 4pm tea"//NEWLINE//"11/5/2026 9:30 coffee"//NEWLINE// &
                         "11/5/2026 lunch"//NEWLINE// &
                         "11/5/2026 9:30 breakfast")
        call list_diary_entries(calendar_date_type(11, 5, 2026), st, 1, .true.)
        call sort_diary_entries(st)
        call check(error, st%ok() .and. size(diary_entries) == 4, "four entries")
        if (allocated(error)) return
        call check(error, diary_entries(1)%string == "lunch" .and. diary_entries(2)%string == "9:30 breakfast" .and. &
                   diary_entries(3)%string == "9:30 coffee" .and. diary_entries(4)%string == "4pm tea", "sorted")
    end subroutine test_sort_entries

    ! The text of the fancy diary buffer.
    function fancy_text() result(text)
        character(len=:), allocatable :: text
        type(handle_type) :: h, old
        type(status_type) :: st

        text = ""
        h = get_buffer(diary_fancy_buffer)
        if (.not. is_buffer_live(h)) return
        old = current_buffer()
        call set_buffer(h, st)
        text = buffer_string()
        call set_buffer(old, st)
    end function fancy_text

    subroutine test_fancy_display(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st

        call setup()
        call write_diary("diary-fancy", joined(DIARY_LINES))
        call list_diary_entries(calendar_date_type(11, 10, 2026), st, 2)
        call check(error, st%ok() .and. fancy_text() == joined(FANCY_10), "November 10: "//fancy_text())
        if (allocated(error)) return
        call list_diary_entries(calendar_date_type(11, 4, 2026), st, 2)
        call check(error, st%ok() .and. fancy_text() == joined(FANCY_4), "November 4: "//fancy_text())
    end subroutine test_fancy_display

    subroutine test_mark_entries(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, old
        type(handle_type), allocatable :: ovs(:)
        type(value_type) :: display
        integer :: k, displays
        character(len=40) :: counts
        type(status_type) :: st

        call setup()
        call write_diary("diary-mark", joined(DIARY_LINES))
        old = current_buffer()
        call get_or_make_buffer("*diary-test-mark*", h, st)
        call set_buffer(h, st)
        call calendar_mode(st)
        buffers(current)%read_only = .false.
        call calendar_generate(11, 2026, st)
        buffers(current)%read_only = .true.
        call mark_diary_entries(.false., st)
        call set_buffer(h, st)
        call overlays_in(point_minimum(), point_maximum(), ovs)
        displays = 0

        do k = 1, size(ovs)
            display = overlay_get(ovs(k), "display")
            if (allocated(display%a)) displays = displays + 1
        end do

        write (counts, "(l1, 1x, l1, 2(1x, i0))") st%ok(), calendar_mark_diary_entries, size(ovs), displays
        call check(error, st%ok() .and. calendar_mark_diary_entries .and. size(ovs) == 34 .and. displays == 3, &
                   "m: "//trim(counts))

        if (.not. allocated(error)) then
            call calendar_unmark(st)
            call overlays_in(point_minimum(), point_maximum(), ovs)
            call check(error,.not. calendar_mark_diary_entries .and. size(ovs) == 0, "u")
        end if

        call set_buffer(old, st)
        call close_buffer(h, st)
    end subroutine test_mark_entries

    subroutine test_bad_expression(error)
        type(error_type), allocatable, intent(out) :: error
        type(diary_entry_type), allocatable :: entries(:)
        character(len=:), allocatable :: text
        type(status_type) :: st

        call setup()
        call write_diary("diary-bad", "11/5/2026 Good"//NEWLINE//"%%(diary-moon 1) Bad"//NEWLINE// &
                         "%%(diary-cyclic 0 1 1 2026) Zero")
        call list_diary_entries(calendar_date_type(11, 5, 2026), st, 1, .true., entries)
        call check(error, st%ok() .and. entry_lines(entries) == "11/5/2026 Good | 1/5/2026"//NEWLINE, entry_lines(entries))
        if (allocated(error)) return
        call current_message(text)
        call check(error, allocated(text), "a message")
        if (allocated(error)) return
        call check(error, index(text, "Bad diary expression at line 3 in ") == 1 .and. &
                   index(text, ": (diary-cyclic 0 1 1 2026): Day count must be positive") > 0, text)
    end subroutine test_bad_expression

    ! The text of the buffer visiting the diary file.
    function diary_text() result(text)
        character(len=:), allocatable :: text
        type(handle_type) :: h, old
        type(status_type) :: st

        text = ""
        h = diary_file_buffer()
        if (.not. is_buffer_live(h)) return
        old = current_buffer()
        call set_buffer(h, st)
        text = buffer_string()
        call set_buffer(old, st)
    end function diary_text

    subroutine test_insert_entries(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, old
        character(len=:), allocatable :: expected
        type(status_type) :: st

        call setup()
        call write_diary("diary-insert", "1/1 New year")
        old = current_buffer()
        call get_or_make_buffer("*diary-test-insert*", h, st)
        call set_buffer(h, st)
        call calendar_mode(st)
        buffers(current)%read_only = .false.
        call calendar_generate(11, 2026, st)
        buffers(current)%read_only = .true.
        call calendar_goto_date(calendar_date_type(11, 2, 2026), st)
        call calendar_set_mark(.false., st)
        call calendar_goto_date(calendar_date_type(11, 5, 2026), st)
        call insert_diary_entry(.false., st)
        call set_buffer(h, st)
        call insert_weekly_diary_entry(.true., st)
        call set_buffer(h, st)
        call insert_monthly_diary_entry(.false., st)
        call set_buffer(h, st)
        call insert_yearly_diary_entry(.false., st)
        call set_buffer(h, st)
        call insert_anniversary_diary_entry(.false., st)
        call set_buffer(h, st)
        call insert_block_diary_entry(.false., st)
        expected = "1/1 New year"//NEWLINE//"11/5/2026 "//NEWLINE//"&Thursday "//NEWLINE//"* 5 "//NEWLINE//"Nov 5 "//NEWLINE// &
                   "%%(diary-anniversary 11 5 2026) "//NEWLINE//"%%(diary-block 11 2 2026 11 5 2026) "
        call check(error, st%ok() .and. diary_text() == expected, diary_text())
        call set_buffer(diary_file_buffer(), st)
        call restore_buffer_modified(.false.)
        if (is_buffer_live(old)) call set_buffer(old, st)
        call close_buffer(h, st)
    end subroutine test_insert_entries

    subroutine test_european_date_forms(error)
        type(error_type), allocatable, intent(out) :: error
        type(diary_entry_type), allocatable :: entries(:)
        character(len=:), allocatable :: expected
        type(status_type) :: st

        call setup()
        calendar_date_style = CALENDAR_DATE_STYLE_EUROPEAN
        diary_date_forms = diary_european_date_forms
        call write_diary("diary-european", "5/11/2026 Slash"//NEWLINE//"5 Nov 2026 Name year"//NEWLINE// &
                         "5 november Name"//NEWLINE// &
                         "5 Nov 10:00 Time"//NEWLINE//"%%(diary-block 1 11 2026 10 11 2026) Block")
        call list_diary_entries(calendar_date_type(11, 5, 2026), st, 1, .true., entries)
        expected = "11/5/2026 Block | %%(diary-block 1 11 2026 10 11 2026)"//NEWLINE// &
                   "11/5/2026 Slash | /11/2026"//NEWLINE// &
                   "11/5/2026 Name | 5 november"//NEWLINE// &
                   "11/5/2026 10:00 Time | 5 Nov"//NEWLINE// &
                   "11/5/2026 Name year | 5 Nov 2026"//NEWLINE
        call check(error, st%ok() .and. entry_lines(entries) == expected, entry_lines(entries))
        calendar_date_style = CALENDAR_DATE_STYLE_AMERICAN
        diary_date_forms = diary_american_date_forms
    end subroutine test_european_date_forms
end module test_diary_library_mod
