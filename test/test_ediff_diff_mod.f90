! test_ediff_diff_mod.f90 -- tests for ediff_diff_mod.
! Ports GNU Emacs 31.1 test/lisp/vc/ediff-diff-tests.el.
!
! Copyright (C) 2017-2026 Free Software Foundation, Inc.
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

!> Ports of ediff-diff-tests.el: the arguments `execute_ediff_process'
!> passes (upstream records them by replacing call-process; here `echo'
!> prints them), and own tests of the difference regions diff and diff3
!> give for two and three buffers.
module test_ediff_diff_mod
    use, intrinsic :: iso_fortran_env, only: int64
    use testdrive, only: new_unittest, unittest_type, error_type, check, skip_test
    use test_support_mod, only: show_alone
    use error_mod, only: status_type, format_error
    use handle_mod, only: handle_type
    use buffer_mod, only: buffers, current, get_or_make_buffer, set_buffer, close_buffer, erase_buffer, current_buffer, &
                          get_buffer, is_buffer_live
    use insdel_mod, only: insert
    use editfns_mod, only: buffer_string
    use keyboard_mod, only: discard_input
    use bindings_mod, only: init_bindings
    use ediff_init_mod, only: ediff_sessions, ediff_session_index, EDIFF_BUFFER_TYPE_A, EDIFF_BUFFER_TYPE_B, &
                              EDIFF_BUFFER_TYPE_C
    use ediff_util_mod, only: get_ediff_diff_position, quit_ediff_session
    use ediff_diff_mod, only: execute_ediff_process
    use ediff_mod, only: ediff_buffers, ediff_buffers3
    implicit none(type, external)
    private

    public :: collect_ediff_diff_mod

    character, parameter :: LF = achar(10)

contains

    !> Collect all exported unit tests.
    subroutine collect_ediff_diff_mod(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("ediff-diff-tests--ediff-exec-process--quoted-file", test_exec_process_quoted_file), &
                    new_unittest("ediff-diff-tests--ediff-exec-process--nil", test_exec_process_nil), &
                    new_unittest("ediff-diff-regions", test_diff_regions), &
                    new_unittest("ediff-diff3-regions", test_diff3_regions) &
                    ]
    end subroutine collect_ediff_diff_mod

    !> Whether PROGRAM is an executable file in a directory of PATH.
    logical function on_path(program) result(found)
        character(len=*), intent(in) :: program
        character(len=:), allocatable :: path, dir
        integer :: n, st, k

        found = .false.
        call get_environment_variable("PATH", length=n, status=st)
        if (st /= 0) return
        allocate (character(len=n) :: path)
        call get_environment_variable("PATH", path)

        do while (len(path) > 0)
            k = index(path, ":")
            if (k == 0) k = len(path) + 1
            dir = path(1:k - 1)

            if (k > len(path)) then
                path = ""
            else
                path = path(k + 1:)
            end if

            if (len(dir) == 0) cycle
            inquire (file=dir//"/"//program, exist=found)
            if (found) return
        end do
    end function on_path

    !> Whether PROGRAM can be run, skipping the test if not.
    logical function has_program(program, error)
        character(len=*), intent(in) :: program
        type(error_type), allocatable, intent(out) :: error

        has_program = on_path(program)
        if (.not. has_program) call skip_test(error, "no "//program)
    end function has_program

    !> A fresh buffer NAME holding TEXT.
    subroutine temp_buffer(name, text, h)
        character(len=*), intent(in) :: name, text
        type(handle_type), intent(out) :: h
        type(handle_type) :: old
        type(status_type) :: st

        old = current_buffer()
        call get_or_make_buffer(name, h, st)
        call set_buffer(h, st)
        call erase_buffer(st)
        call insert(text, st)
        call set_buffer(old, st)
    end subroutine temp_buffer

    !> The text of buffer H.
    function text_of(h) result(s)
        type(handle_type), intent(in) :: h
        character(len=:), allocatable :: s
        type(handle_type) :: old
        type(status_type) :: st

        old = current_buffer()
        call set_buffer(h, st)
        s = buffer_string()
        call set_buffer(old, st)
    end function text_of

    !> *scratch* current, shown alone.
    subroutine setup()
        type(handle_type) :: buffer
        type(status_type) :: st

        call init_bindings()
        call discard_input()
        call get_or_make_buffer("*scratch*", buffer, st)
        call set_buffer(buffer, st)
        call show_alone()
    end subroutine setup

    !> Check that Bug#25950 is fixed: the "/:" quoting is removed.
    subroutine test_exec_process_quoted_file(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        if (.not. has_program("echo", error)) return
        call setup()
        call temp_buffer(" *ediff-exec-test*", "", h)
        call execute_ediff_process("echo", h, "", st, file1="/:/a", file2="/:/b")
        call check(error, st%ok(), format_error(st))
        if (.not. allocated(error)) call check(error, text_of(h), "/a /b"//LF)
        call close_buffer(h, st)
    end subroutine test_exec_process_quoted_file

    !> Check that Bug#26378 is fixed: absent and "" files are ignored.
    subroutine test_exec_process_nil(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        if (.not. has_program("echo", error)) return
        call setup()
        call temp_buffer(" *ediff-exec-test*", "", h)
        call execute_ediff_process("echo", h, "", st, file1="foo", file3="")
        call check(error, st%ok(), format_error(st))
        if (.not. allocated(error)) call check(error, text_of(h), "foo"//LF)
        call close_buffer(h, st)
    end subroutine test_exec_process_nil

    !> The regions of a changed line and an added line, in buffers A and B.
    subroutine test_diff_regions(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: a, b, control
        type(status_type) :: st
        integer(int64) :: pos
        integer :: k

        if (.not. has_program("diff", error)) return
        call setup()
        call temp_buffer("ediff-a", "one"//LF//"two"//LF//"three"//LF, a)
        call temp_buffer("ediff-b", "one"//LF//"TWO"//LF//"three"//LF//"four"//LF, b)
        call ediff_buffers(a, b, st)

        body: block
            call check(error, st%ok(), format_error(st))
            if (allocated(error)) exit body
            control = get_buffer("*Ediff Control Panel*")
            call check(error, is_buffer_live(control), "the control buffer exists")
            if (allocated(error)) exit body
            k = ediff_session_index(control)
            call check(error, ediff_sessions(k)%number_of_differences, 2)
            if (allocated(error)) exit body
            call check(error, buffers(control%index)%major_mode, "ediff_mode")
            if (allocated(error)) exit body
            ! "two" in A (5 to 9), "TWO" in B.
            call get_ediff_diff_position(k, EDIFF_BUFFER_TYPE_A, .false., pos, st, 0)
            call check(error, pos, 5_int64)
            if (allocated(error)) exit body
            call get_ediff_diff_position(k, EDIFF_BUFFER_TYPE_A, .true., pos, st, 0)
            call check(error, pos, 9_int64)
            if (allocated(error)) exit body
            call get_ediff_diff_position(k, EDIFF_BUFFER_TYPE_B, .false., pos, st, 0)
            call check(error, pos, 5_int64)
            if (allocated(error)) exit body
            ! "four" added at the end of B: empty in A.
            call get_ediff_diff_position(k, EDIFF_BUFFER_TYPE_A, .false., pos, st, 1)
            call check(error, pos, 15_int64)
            if (allocated(error)) exit body
            call get_ediff_diff_position(k, EDIFF_BUFFER_TYPE_A, .true., pos, st, 1)
            call check(error, pos, 15_int64)
            if (allocated(error)) exit body
            call get_ediff_diff_position(k, EDIFF_BUFFER_TYPE_B, .false., pos, st, 1)
            call check(error, pos, 15_int64)
            if (allocated(error)) exit body
            call get_ediff_diff_position(k, EDIFF_BUFFER_TYPE_B, .true., pos, st, 1)
            call check(error, pos, 20_int64)
            if (allocated(error)) exit body
            call get_ediff_diff_position(k, EDIFF_BUFFER_TYPE_A, .false., pos, st, 2)
            call check(error,.not. st%ok(), "difference 3 is invalid")
        end block body

        call quit_session()
        call close_buffer(a, st)
        call close_buffer(b, st)
    end subroutine test_diff_regions

    !> Three buffers compared with diff3: the region where C differs.
    subroutine test_diff3_regions(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: a, b, c, control
        type(status_type) :: st
        integer(int64) :: pos
        integer :: k

        if (.not. has_program("diff3", error)) return
        call setup()
        call temp_buffer("ediff-a", "one"//LF//"two"//LF//"three"//LF, a)
        call temp_buffer("ediff-b", "one"//LF//"two"//LF//"three"//LF, b)
        call temp_buffer("ediff-c", "one"//LF//"2"//LF//"three"//LF, c)
        call ediff_buffers3(a, b, c, st)

        body: block
            call check(error, st%ok(), format_error(st))
            if (allocated(error)) exit body
            control = get_buffer("*Ediff Control Panel*")
            k = ediff_session_index(control)
            call check(error, k > 0, "a session")
            if (allocated(error)) exit body
            call check(error, ediff_sessions(k)%number_of_differences, 1)
            if (allocated(error)) exit body
            call get_ediff_diff_position(k, EDIFF_BUFFER_TYPE_C, .false., pos, st, 0)
            call check(error, pos, 5_int64)
            if (allocated(error)) exit body
            call get_ediff_diff_position(k, EDIFF_BUFFER_TYPE_C, .true., pos, st, 0)
            call check(error, pos, 7_int64)
            if (allocated(error)) exit body
            ! A and B agree: C is the one that differs.
            call check(error, trim(ediff_sessions(k)%differences(EDIFF_BUFFER_TYPE_A, 1)%state_of_diff), "=diff(B)")
        end block body

        call quit_session()
        call close_buffer(a, st)
        call close_buffer(b, st)
        call close_buffer(c, st)
    end subroutine test_diff3_regions

    !> Quit the session of *Ediff Control Panel*, if any.
    subroutine quit_session()
        type(handle_type) :: control
        type(status_type) :: st
        integer :: k

        control = get_buffer("*Ediff Control Panel*")
        if (.not. is_buffer_live(control)) return
        call set_buffer(control, st)
        k = ediff_session_index(control)
        if (k > 0) call quit_ediff_session(k, .false., st)
    end subroutine quit_session
end module test_ediff_diff_mod
