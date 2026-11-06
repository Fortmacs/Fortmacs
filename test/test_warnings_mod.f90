! test_warnings_mod.f90 -- tests for warnings (warnings_mod).
! Ports GNU Emacs 31.1 test/lisp/emacs-lisp/warnings-tests.el in part;
! the *Messages* checks of test-display-warning need batch mode, which
! Fortmacs does not have.
!
! Copyright (C) 2023-2026 Free Software Foundation, Inc.
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

!> The ported warnings-tests (warning-suppress-p, display-warning and the
!> minimum levels, with messages built by an internal write) and own
!> tests of the *Warnings* buffer, `warn', a level's Fortran format, the
!> suppression lists, `warning_series' and the delayed warnings.
module test_warnings_mod
    use, intrinsic :: iso_fortran_env, only: int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use test_support_mod, only: show_alone
    use error_mod, only: status_type, format_message
    use handle_mod, only: handle_type
    use string_type_mod, only: string_type, append_string
    use buffer_mod, only: buffers, get_buffer, get_or_make_buffer, set_buffer, close_buffer, current_buffer
    use editing_functions_mod, only: buffer_string, point
    use window_mod, only: get_buffer_window
    use time_functions_mod, only: current_time
    use startup_mod, only: after_initialization_time
    use keyboard_mod, only: delayed_warnings
    use bindings_mod, only: initialize_bindings
    use warnings_mod, only: display_warning, warn, warning_levels, is_warning_suppressed, warning_types_type, &
                            WARNING_LEVEL_EMERGENCY, WARNING_LEVEL_ERROR, WARNING_LEVEL_WARNING, WARNING_LEVEL_DEBUG, &
                            WARNING_MINIMUM_EMERGENCY, WARNING_MINIMUM_ERROR, WARNING_MINIMUM_WARNING, WARNING_MINIMUM_DEBUG, &
                            warning_minimum_level, &
                            warning_minimum_log_level, warning_suppress_types, warning_suppress_log_types, warning_series, &
                            warning_display_at_bottom, &
                            display_delayed_warnings, collapse_delayed_warnings
    implicit none(type, external)
    private

    public :: collect_warnings_mod

contains

    !> Collect all exported unit tests.
    subroutine collect_warnings_mod(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("test-warning-suppress-p", test_is_warning_suppressed), &
                    new_unittest("test-display-warning", test_display_warning), &
                    new_unittest("test-display-warning/warning-minimum-level", test_minimum_level), &
                    new_unittest("test-display-warning/warning-minimum-log-level", test_minimum_log_level), &
                    new_unittest("warnings-buffer", test_warnings_buffer), &
                    new_unittest("warn-and-level-formats", test_warn), &
                    new_unittest("warning-suppress-types", test_suppress_types), &
                    new_unittest("warning-series", test_series), &
                    new_unittest("delayed-warnings", test_delayed) &
                    ]
    end subroutine collect_warnings_mod

    ! A fresh editor, after initialization, with no *Warnings* buffer.
    subroutine enter()
        type(handle_type) :: h
        type(status_type) :: st

        call initialize_bindings()
        if (allocated(delayed_warnings)) deallocate (delayed_warnings)
        call close_warnings_buffer()
        call get_or_make_buffer("warnings-test", h, st)
        call set_buffer(h, st)
        call show_alone()
        after_initialization_time = current_time()
    end subroutine enter

    ! Undo `enter' and the settings the tests change.
    subroutine leave()
        call close_warnings_buffer()
        call show_alone()
        if (allocated(after_initialization_time)) deallocate (after_initialization_time)
        if (allocated(delayed_warnings)) deallocate (delayed_warnings)
        warning_minimum_level = WARNING_MINIMUM_WARNING
        warning_minimum_log_level = WARNING_MINIMUM_WARNING
        call warning_suppress_types%clear()
        call warning_suppress_log_types%clear()
        warning_series = .false.
        warning_display_at_bottom = .true.
    end subroutine leave

    subroutine close_warnings_buffer()
        type(handle_type) :: h
        type(status_type) :: st

        h = get_buffer("*Warnings*")
        if (h%index /= 0) call close_buffer(h, st)
    end subroutine close_warnings_buffer

    ! The text of the buffer NAME ("" if there is none).
    function text_of(name) result(text)
        character(len=*), intent(in) :: name
        character(len=:), allocatable :: text
        type(handle_type) :: h, old
        type(status_type) :: st

        text = ""
        h = get_buffer(name)
        if (h%index == 0) return
        old = current_buffer()
        call set_buffer(h, st)
        text = buffer_string()
        call set_buffer(old, st)
    end function text_of

    ! Whether there is a buffer NAME.
    logical function has_buffer(name)
        character(len=*), intent(in) :: name
        type(handle_type) :: h

        h = get_buffer(name)
        has_buffer = h%index /= 0
    end function has_buffer

    ! A list of the names A and, if present, B and C.
    function type_list(a, b, c) result(types)
        character(len=*), intent(in) :: a
        character(len=*), intent(in), optional :: b, c
        type(string_type), allocatable :: types(:)

        call append_string(types, a, b, c)
    end function type_list

    !> Port of warnings-tests.el:test-warning-suppress-p.
    subroutine test_is_warning_suppressed(error)
        type(error_type), allocatable, intent(out) :: error
        type(warning_types_type) :: foo, foo_bar, foo_bax, none

        call foo%append("foo")
        call foo_bar%append("foo", "bar")
        call foo_bax%append("foo", "bax")
        call check(error, is_warning_suppressed("foo", foo), "foo in ((foo))")
        if (allocated(error)) return
        call check(error, is_warning_suppressed(type_list("foo", "bar"), foo_bar), "(foo bar) in ((foo bar))")
        if (allocated(error)) return
        call check(error, is_warning_suppressed(type_list("foo", "bar", "baz"), foo_bar), "(foo bar baz) in ((foo bar))")
        if (allocated(error)) return
        call check(error,.not. is_warning_suppressed(type_list("foo", "bar", "baz"), foo_bax), "(foo bar baz) not in ((foo bax))")
        if (allocated(error)) return
        call check(error,.not. is_warning_suppressed("foobar", none), "foobar not in nil")
        if (allocated(error)) return
        ! Own: one name matches only the one-name element.
        call check(error,.not. is_warning_suppressed("foo", foo_bar), "foo not in ((foo bar))")
    end subroutine test_is_warning_suppressed

    !> Port of warnings-tests.el:test-display-warning (the warning in the
    !> buffer; *Messages* gets it only in batch mode).
    subroutine test_display_warning(error)
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: levels(3) = [WARNING_LEVEL_EMERGENCY, WARNING_LEVEL_ERROR, WARNING_LEVEL_WARNING]
        type(handle_type) :: h
        type(status_type) :: st
        character(len=:), allocatable :: text
        character(len=8) :: message
        integer :: k

        call enter()
        write (message, "(a, i0)") "Hello", 123

        do k = 1, size(levels)
            call get_or_make_buffer("warnings-test-temp", h, st)
            call set_buffer(h, st)
            call display_warning(type_list("foo"), trim(message), levels(k), "warnings-test-temp", st)
            call check(error, st%ok(), "display_warning")
            if (allocated(error)) exit
            text = buffer_string()
            call check(error, index(text, "foo") > 0 .and. index(text, "Hello123") > 0, "foo and Hello123: "//text)
            if (allocated(error)) exit
            call close_buffer(h, st)
        end do

        call leave()
    end subroutine test_display_warning

    !> Port of warnings-tests.el:test-display-warning/warning-minimum-level
    !> (interactively, as Fortmacs runs: the warning is logged but its
    !> buffer is not displayed).
    subroutine test_minimum_level(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        type(handle_type) :: w

        call enter()
        warning_minimum_level = WARNING_MINIMUM_EMERGENCY
        call display_warning(type_list("foo"), "baz", WARNING_LEVEL_WARNING, status=st)
        call check(error, index(text_of("*Warnings*"), "baz") > 0, "logged")

        if (.not. allocated(error)) then
            w = get_buffer_window(get_buffer("*Warnings*"))
            call check(error, w%index == 0, "not displayed")
        end if

        call leave()
    end subroutine test_minimum_level

    !> Port of warnings-tests.el:test-display-warning/warning-minimum-log-level.
    subroutine test_minimum_log_level(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call enter()
        warning_minimum_log_level = WARNING_MINIMUM_ERROR
        call get_or_make_buffer("warnings-test-temp", h, st)
        call set_buffer(h, st)
        call display_warning(type_list("foo"), "hello", WARNING_LEVEL_WARNING, "warnings-test-temp", st)
        call check(error, index(buffer_string(), "hello") == 0, "not logged")
        call close_buffer(h, st)
        call leave()
    end subroutine test_minimum_log_level

    !> The *Warnings* buffer: made in Special mode, read-only, one line
    !> per warning with the level's description and the type, shown in a
    !> window; WARNING_LEVEL_DEBUG is not logged by default.
    subroutine test_warnings_buffer(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        type(handle_type) :: h, w

        call enter()
        call display_warning("holidays", "Bad holiday list item", WARNING_LEVEL_ERROR, status=st)
        call check(error, st%ok(), "display_warning")

        if (.not. allocated(error)) then
            call display_warning("foo", "second", status=st)
            call display_warning("foo", "ignored", WARNING_LEVEL_DEBUG, status=st)
            call check(error, text_of("*Warnings*") == "Error (holidays): Bad holiday list item"//achar(10)// &
                       "Warning (foo): second"//achar(10), "text: "//text_of("*Warnings*"))
        end if

        if (.not. allocated(error)) then
            h = get_buffer("*Warnings*")
            call check(error, buffers(h%index)%read_only .and. .not. buffers(h%index)%undo_enabled, "read-only, no undo")
        end if

        if (.not. allocated(error)) then
            w = get_buffer_window(h)
            call check(error, w%index /= 0, "displayed")
        end if

        call leave()
    end subroutine test_warnings_buffer

    !> `warn' (finished text, type fortmacs; its literal quotes curved with
    !> `format_message', as upstream's format-message does), a message built
    !> with an internal write, and a level whose Fortran format leaves the
    !> type out.
    subroutine test_warn(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        character(len=40) :: message

        call enter()
        call warn(format_message("Can't find `x'"), status=st)
        write (message, "(a, i0)") "Bad file at line ", 3
        call display_warning("diary", trim(message), WARNING_LEVEL_ERROR, status=st)
        warning_levels(4)%string = "(""Debug: "")"
        warning_minimum_log_level = WARNING_MINIMUM_DEBUG
        call display_warning("diary", "no type", WARNING_LEVEL_DEBUG, status=st)
        call check(error, text_of("*Warnings*") == "Warning (fortmacs): Can"//char(226)//char(128)//char(153)//"t find "// &
                   char(226)//char(128)//char(152)//"x"//char(226)//char(128)//char(153)//achar(10)// &
                   "Error (diary): Bad file at line 3"//achar(10)//"Debug: no type"//achar(10), "text: "//text_of("*Warnings*"))
        call leave()
    end subroutine test_warn

    !> `warning_suppress_types' keeps the buffer from being shown;
    !> `warning_suppress_log_types' keeps the warning out of it.
    subroutine test_suppress_types(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        type(handle_type) :: w

        call enter()
        call warning_suppress_log_types%append("quiet")
        call warning_suppress_types%append("hidden")
        call display_warning("quiet", "never", status=st)
        call check(error,.not. has_buffer("*Warnings*"), "not logged")

        if (.not. allocated(error)) then
            call display_warning(type_list("hidden", "sub"), "logged", status=st)
            call check(error, text_of("*Warnings*") == "Warning (hidden): logged"//achar(10), "logged")
        end if

        if (.not. allocated(error)) then
            w = get_buffer_window(get_buffer("*Warnings*"))
            call check(error, w%index == 0, "not displayed")
        end if

        call leave()
    end subroutine test_suppress_types

    !> `warning_series': the first warning stores a marker at its start
    !> and later ones leave point there (with `warning_display_at_bottom'
    !> false, which would move point to the last warning).
    subroutine test_series(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        type(handle_type) :: h, old

        call enter()
        warning_display_at_bottom = .false.
        call display_warning("foo", "before", status=st)
        warning_series = .true.
        call display_warning("foo", "first", status=st)
        call check(error, warning_series%marker%index /= 0, "a marker")

        if (.not. allocated(error)) then
            call display_warning("foo", "second", status=st)
            h = get_buffer("*Warnings*")
            old = current_buffer()
            call set_buffer(h, st)
            call check(error, point() == int(len("Warning (foo): before") + 2, int64), "point at the series start")
            call set_buffer(old, st)
        end if

        call leave()
    end subroutine test_series

    !> Before initialization, warnings are delayed; the hook collapses
    !> duplicates and displays them.
    subroutine test_delayed(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st

        call enter()
        deallocate (after_initialization_time)
        call display_warning("foo", "early", status=st)
        call display_warning("foo", "early", status=st)
        call display_warning("bar", "late", WARNING_LEVEL_ERROR, status=st)
        call check(error, size(delayed_warnings) == 3 .and. .not. has_buffer("*Warnings*"), "delayed")

        if (.not. allocated(error)) then
            call collapse_delayed_warnings(st)
            call check(error, size(delayed_warnings) == 2, "collapsed")
        end if

        if (.not. allocated(error)) then
            after_initialization_time = current_time()
            call display_delayed_warnings(st)
            call check(error, text_of("*Warnings*") == "Warning (foo): early [2 times]"//achar(10)// &
                       "Error (bar): late"//achar(10), "text: "//text_of("*Warnings*"))
        end if

        if (.not. allocated(error)) call check(error, size(delayed_warnings) == 0, "emptied")
        call leave()
    end subroutine test_delayed
end module test_warnings_mod
