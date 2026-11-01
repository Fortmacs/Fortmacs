! test_rmc_fm.f90 -- tests for read-multiple-choice (rmc_fm).
! Ports GNU Emacs 31.1 test/lisp/emacs-lisp/rmc-tests.el in part; only
! the non-graphical cases.
!
! Copyright (C) 2021-2026 Free Software Foundation, Inc.
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

!> Ports test/lisp/emacs-lisp/rmc-tests.el: the non-graphical cases of
!> rmc--add-key-description, test-read-multiple-choice (keys fed to
!> `read-key`) and the layout of test-read-multiple-choice-help.
module test_rmc_fm
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_fm, only: status_type, FM_QUIT
    use lisp_fm, only: value_type
    use keymap_fm, only: key_parse
    use keyboard_fm, only: unread_command_events, discard_input, last_message
    use bindings_fm, only: init_bindings
    use buffer_fm, only: get_or_make_buffer, set_buffer
    use handle_fm, only: handle_type
    use rmc_fm, only: rmc_choice_type, rmc_add_key_description, read_multiple_choice, generated_help, &
                      read_char_choice_use_read_key
    implicit none(type, external)
    private

    public :: collect_rmc_fm

contains

    !> Collect all exported unit tests.
    subroutine collect_rmc_fm(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("test-rmc--add-key-description/non-graphical-display", test_add_key_description), &
                    new_unittest("test-read-multiple-choice", test_read), &
                    new_unittest("test-read-multiple-choice-help", test_help), &
                    new_unittest("read-multiple-choice-read-key", test_read_key), &
                    new_unittest("read-multiple-choice-long-form", test_long_form) &
                    ]
    end subroutine collect_rmc_fm

    function choice(key, name, description) result(c)
        character, intent(in) :: key
        character(len=*), intent(in) :: name
        character(len=*), intent(in), optional :: description
        type(rmc_choice_type) :: c

        c%key = iachar(key)
        c%name = name
        if (present(description)) c%description = description
    end function choice

    !> Bindings and a current buffer for reading in the minibuffer.
    subroutine setup()
        type(handle_type) :: h
        type(status_type) :: st

        call init_bindings()
        call discard_input()
        call get_or_make_buffer("*scratch*", h, st)
        call set_buffer(h, st)
    end subroutine setup

    subroutine test_add_key_description(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, rmc_add_key_description(choice("y", "yes")), "[Y]es")
        if (allocated(error)) return
        call check(error, rmc_add_key_description(choice("n", "foo")), "n foo")
        if (allocated(error)) return
        call check(error, rmc_add_key_description(choice(" ", "foo bar")), "SPC foo bar")
    end subroutine test_add_key_description

    subroutine test_read(error)
        type(error_type), allocatable, intent(out) :: error
        type(rmc_choice_type) :: choices(2)
        type(value_type), allocatable :: evs(:)
        type(status_type) :: st
        integer :: answer

        choices(1) = choice("y", "yes")
        choices(2) = choice("n", "no")
        call setup()
        call key_parse("n", evs, st)
        call unread_command_events(evs)
        call read_multiple_choice("Do it? ", choices, answer, st)
        call check(error, st%ok() .and. answer == 2)
        if (allocated(error)) return
        ! Invalid choices ask again; C-g quits.
        call key_parse("x y", evs, st)
        call unread_command_events(evs)
        call read_multiple_choice("Do it? ", choices, answer, st)
        call check(error, st%ok() .and. answer == 1)
        if (allocated(error)) return
        call key_parse("C-g", evs, st)
        call unread_command_events(evs)
        call read_multiple_choice("Do it? ", choices, answer, st)
        call check(error, st%code == FM_QUIT)
        call discard_input()
    end subroutine test_read

    subroutine test_help(error)
        type(error_type), allocatable, intent(out) :: error
        type(rmc_choice_type) :: choices(3)
        character(len=*), parameter :: LF = achar(10)

        choices(1) = choice("a", "aaa")
        choices(2) = choice("b", "bbb")
        choices(3) = choice("c", "ccc", "a really long description of ccc")
        call check(error, "Choose:"//LF//LF//generated_help(choices, 3), &
                   "Choose:"//LF//LF// &
                   "a: [A]aa                 b: [B]bb                 c: [C]cc"//LF// &
                   repeat(" ", 50)//"a really long"//LF// &
                   repeat(" ", 50)//"description of ccc"//LF// &
                   repeat(" ", 50)//LF)
    end subroutine test_help

    !> read-char-choice-use-read-key: the prompt in the echo area, with
    !> "[?]" for help.
    subroutine test_read_key(error)
        type(error_type), allocatable, intent(out) :: error
        type(rmc_choice_type) :: choices(2)
        type(value_type), allocatable :: evs(:)
        type(status_type) :: st
        integer :: answer

        choices(1) = choice("y", "yes")
        choices(2) = choice("n", "no")
        call discard_input()
        read_char_choice_use_read_key = .true.
        call key_parse("n", evs, st)
        call unread_command_events(evs)
        call read_multiple_choice("Do it?", choices, answer, st)
        read_char_choice_use_read_key = .false.
        call check(error, st%ok() .and. answer == 2, "n")
        if (allocated(error)) return
        call check(error, last_message == "Do it? ([Y]es, [N]o, [?]): ", last_message)
    end subroutine test_read_key

    !> LONG-FORM reads a name with completion.
    subroutine test_long_form(error)
        type(error_type), allocatable, intent(out) :: error
        type(rmc_choice_type) :: choices(2)
        type(value_type), allocatable :: evs(:)
        type(status_type) :: st
        integer :: answer

        choices(1) = choice("y", "yes")
        choices(2) = choice("n", "no")
        call setup()
        call key_parse("n o RET", evs, st)
        call unread_command_events(evs)
        call read_multiple_choice("Do it?", choices, answer, st, long_form=.true.)
        call check(error, st%ok() .and. answer == 2, "no")
    end subroutine test_long_form
end module test_rmc_fm
