! test_which_function_mod.f90 -- tests for Which Function mode (which_function_mod).
! Ports GNU Emacs 31.1 test/lisp/progmodes/which-func-tests.el.
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

!> Ports which-func-tests.el (its one test, with a Shell-script buffer
!> standing for the Emacs Lisp file and without the header line, which is
!> not ported) and own tests of `which_function', its hooks and the mode
!> line element.  Every test closes the buffers it makes and leaves
!> Which Function mode off.
module test_which_function_mod
    use, intrinsic :: iso_fortran_env, only: int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_mod, only: status_type, format_error
    use handle_mod, only: handle_type
    use buffer_mod, only: get_or_make_buffer, set_buffer, close_buffer, erase_buffer
    use insert_delete_mod, only: insert
    use editing_functions_mod, only: point_minimum, goto_character
    use search_mod, only: search_forward
    use bindings_mod, only: initialize_bindings
    use modes_mod, only: minor_modes
    use test_support_mod, only: show_alone
    use frame_mod, only: selected_window
    use display_engine_mod, only: mode_line_miscellaneous_info_faces, face_span_type
    use index_menu_mod, only: has_imenu_index
    use which_function_mod, only: which_function, which_function_mode_variable, which_function_functions, &
                                  which_function_cleanup_function, which_function_modes, which_function_non_auto_modes, &
                                  update_which_function_window
    use shell_script_mod, only: sh_mode
    use configuration_mode_mod, only: configuration_unix_mode
    use python_mod, only: python_mode
    implicit none(type, external)
    private

    public :: collect_which_function_mod

    character(len=*), parameter :: LF = achar(10)

contains

    !> Collect all exported unit tests.
    subroutine collect_which_function_mod(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("which-func-tests-toggle", test_toggle), &
                    new_unittest("which-function-add-log", test_add_log), &
                    new_unittest("which-function-imenu", test_imenu), &
                    new_unittest("which-function-functions-and-cleanup", test_functions), &
                    new_unittest("which-func-modes", test_modes), &
                    new_unittest("which-func-mode-line", test_mode_line) &
                    ]
    end subroutine collect_which_function_mod

    !> A buffer NAME holding TEXT, current.
    subroutine make_buffer(name, text, h)
        character(len=*), intent(in) :: name, text
        type(handle_type), intent(out) :: h
        type(status_type) :: st

        call get_or_make_buffer(name, h, st)
        call set_buffer(h, st)
        call erase_buffer(st)
        call insert(text, st)
        call goto_character(point_minimum())
    end subroutine make_buffer

    !> Whether `which_function_mode_variable' is on in buffer H.
    logical function is_on(h)
        type(handle_type), intent(in) :: h
        type(status_type) :: st

        call set_buffer(h, st)
        is_on = which_function_mode_variable%value
    end function is_on

    !> which-func-tests-toggle: Which Function mode turns `which_function_mode_variable'
    !> on in buffers whose mode supports it, and leaves it on when turned
    !> off; buffers made while it is on get it from their mode.
    subroutine test_toggle(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: code, other
        type(status_type) :: st

        call initialize_bindings()
        call make_buffer("which-func-tests.sh", "f()"//LF//"{"//LF//"}"//LF, code)
        call sh_mode(st)
        call make_buffer("fundamental", "", other)

        body: block
            call check(error,.not. is_on(code) .and. .not. is_on(other), "off at first")
            if (allocated(error)) exit body
            call minor_modes%which_function_mode%enable(st)
            call check(error, is_on(code), "on in the code buffer")
            if (allocated(error)) exit body
            call check(error,.not. is_on(other), "off in the fundamental buffer")
            if (allocated(error)) exit body
            call minor_modes%which_function_mode%disable(st)
            ! which-func-mode stays set even when which-function-mode is off.
            call check(error, is_on(code), "still on in the code buffer")
            if (allocated(error)) exit body
            call check(error,.not. is_on(other), "still off in the fundamental buffer")
            if (allocated(error)) exit body
            call close_buffer(code, st)
            call close_buffer(other, st)
            call minor_modes%which_function_mode%enable(st)
            call make_buffer("which-func-tests.sh", "f()"//LF//"{"//LF//"}"//LF, code)
            call sh_mode(st)
            call make_buffer("fundamental", "", other)
            call check(error, is_on(code), "on in a new code buffer")
            if (allocated(error)) exit body
            call check(error,.not. is_on(other), "off in a new fundamental buffer")
        end block body

        call minor_modes%which_function_mode%disable(st)
        call close_buffer(code, st)
        call close_buffer(other, st)
    end subroutine test_toggle

    !> `which_function' asks the mode's `add_log_current_subprogram_function'.
    subroutine test_add_log(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        character(len=:), allocatable :: name
        integer(int64) :: found

        call initialize_bindings()
        call make_buffer(" *which-func*", "f()"//LF//"{"//LF//"}"//LF//"g()"//LF//"{"//LF//"  x=1"//LF//"}"//LF, h)
        call sh_mode(st)
        call search_forward("x=", found, st)
        call which_function(name, st)
        call check(error, allocated(name), "a name")
        if (.not. allocated(error)) call check(error, name, "g")
        call close_buffer(h, st)
    end subroutine test_add_log

    !> Without an add-log function, `which_function' takes the closest
    !> entry of the Imenu index at or before point.
    subroutine test_imenu(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        character(len=:), allocatable :: name
        integer(int64) :: found

        call initialize_bindings()
        ! (No line has the ":" or "=" of the add-log heuristic.)
        call make_buffer(" *which-func*", "[one]"//LF//"x"//LF//"[two]"//LF//"y"//LF, h)
        call configuration_unix_mode(st)
        call search_forward("y", found, st)
        call which_function(name, st)

        body: block
            call check(error, allocated(name), "a name")
            if (allocated(error)) exit body
            call check(error, name, "two")
            if (allocated(error)) exit body
            ! An entry at point is the closest.
            call goto_character(point_minimum())
            call which_function(name, st)
            call check(error, allocated(name), "a name at the first entry")
            if (allocated(error)) exit body
            call check(error, name, "one")
        end block body

        call close_buffer(h, st)
    end subroutine test_imenu

    !> `which_function_functions' come first; `which_function_cleanup_function'
    !> filters the name; a joiner gets the entry and its submenus.
    subroutine test_functions(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        character(len=:), allocatable :: name
        integer(int64) :: found

        call initialize_bindings()
        call make_buffer(" *which-func*", "class C:"//LF//"    def m(self):"//LF//"        pass"//LF, h)
        call python_mode(st)
        call search_forward("pass", found, st)
        call which_function(name, st)

        body: block
            call check(error, allocated(name), "a name")
            if (allocated(error)) exit body
            ! python-info-current-defun, from which-func-functions.
            call check(error, name, "C.m")
            if (allocated(error)) exit body
            which_function_cleanup_function => upcase_name
            call which_function(name, st)
            which_function_cleanup_function => null()
            call check(error, name, "C.M", "cleaned up")
            if (allocated(error)) exit body
            call which_function_functions%prepend(fixed_name, local=.true.)
            call which_function(name, st)
            call check(error, name, "fixed", "the first function")
        end block body

        call close_buffer(h, st)
    end subroutine test_functions

    ! A `which_function_cleanup_function' for test_functions.
    subroutine upcase_name(name)
        character(len=:), allocatable, intent(inout) :: name
        integer :: i

        do i = 1, len(name)
            if (name(i:i) >= "a" .and. name(i:i) <= "z") name(i:i) = achar(iachar(name(i:i)) - 32)
        end do
    end subroutine upcase_name

    ! A function of `which_function_functions' for test_functions.
    subroutine fixed_name(string, status)
        character(len=:), allocatable, intent(out) :: string
        type(status_type), intent(out) :: status

        status = status_type()
        string = "fixed"
    end subroutine fixed_name

    !> `which_function_modes' limits the modes; in `which_function_non_auto_modes'
    !> the index is not made until Imenu is used.
    subroutine test_modes(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call initialize_bindings()
        call minor_modes%which_function_mode%enable(st)
        which_function_modes%all = .false.
        which_function_modes%modes = [character(len=64) :: "python_mode"]
        call make_buffer(" *which-func*", "f()"//LF, h)
        call sh_mode(st)

        body: block
            call check(error,.not. which_function_mode_variable%value, "not in sh_mode")
            if (allocated(error)) exit body
            call python_mode(st)
            call check(error, which_function_mode_variable%value, "in python_mode")
            if (allocated(error)) exit body
            which_function_modes%all = .true.
            deallocate (which_function_non_auto_modes)
            allocate (which_function_non_auto_modes(1))
            which_function_non_auto_modes(1) = "sh_mode"
            call sh_mode(st)
            call check(error, which_function_mode_variable%value .and. .not. has_imenu_index(), "no index in a non-auto mode")
        end block body

        deallocate (which_function_non_auto_modes)
        allocate (which_function_non_auto_modes(0))
        which_function_modes%all = .true.
        deallocate (which_function_modes%modes)
        allocate (which_function_modes%modes(0))
        call minor_modes%which_function_mode%disable(st)
        call close_buffer(h, st)
    end subroutine test_modes

    !> The element of the mode line: "[NAME] " after the update of the
    !> selected window, NAME in the face which_function_face; "n/a" before.
    subroutine test_mode_line(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        character(len=:), allocatable :: miscellaneous
        type(face_span_type), allocatable :: spans(:)
        character(len=64), allocatable :: faces(:)
        integer(int64) :: found

        call initialize_bindings()
        call make_buffer(" *which-func*", "f()"//LF//"{"//LF//"  x=1"//LF//"}"//LF, h)
        call sh_mode(st)
        call show_alone()
        call minor_modes%which_function_mode%enable(st)

        body: block
            call mode_line_miscellaneous_info_faces(h%index, miscellaneous, spans, faces)
            call check(error, miscellaneous, "[n/a] ", "unknown")
            if (allocated(error)) exit body
            call search_forward("x=", found, st)
            call update_which_function_window(selected_window(), st)
            call check(error, st%ok(), format_error(st))
            if (allocated(error)) exit body
            call mode_line_miscellaneous_info_faces(h%index, miscellaneous, spans, faces)
            call check(error, miscellaneous, "[f] ")
            if (allocated(error)) exit body
            call check(error, size(spans) == 1, "one face")
            if (allocated(error)) exit body
            call check(error, spans(1)%first == 2 .and. spans(1)%last == 2 .and. trim(faces(1)) == "which_function_face", &
                       "the name in which_function_face")
            if (allocated(error)) exit body
            call minor_modes%which_function_mode%disable(st)
            call mode_line_miscellaneous_info_faces(h%index, miscellaneous, spans, faces)
            call check(error, miscellaneous, "", "off")
        end block body

        call minor_modes%which_function_mode%disable(st)
        call close_buffer(h, st)
    end subroutine test_mode_line
end module test_which_function_mod
