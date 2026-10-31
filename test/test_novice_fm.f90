! test_novice_fm.f90 -- tests for disabled commands (novice_fm).
! Own tests; upstream has no novice tests.
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

!> Own tests (upstream has no novice tests): C-x C-u, disabled by
!> default, answered n, SPC, y and !, and `enable-command` and
!> `disable-command`.
module test_novice_fm
    use, intrinsic :: iso_fortran_env, only: int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use test_support, only: show_alone
    use error_fm, only: status_type
    use handle_fm, only: handle_type
    use lisp_fm, only: value_type
    use lread_fm, only: sym
    use callint_fm, only: find_command, command_type
    use commands_fm, only: commands
    use buffer_fm, only: get_buffer_create, set_buffer, kill_buffer
    use insdel_fm, only: insert
    use editfns_fm, only: goto_char, buffer_string, point_max
    use simple_fm, only: push_mark
    use keymap_fm, only: key_parse
    use keyboard_fm, only: unread_command_events, discard_input, command_loop
    use callint_fm, only: prefix_arg
    use bindings_fm, only: init_bindings
    use novice_fm, only: enable_command, disable_command, disabled_command_function_variable
    use lread_fm, only: user_init_file
    use fileio_fm, only: expand_file_name, make_directory_internal, delete_file_internal, emacs_wd, init_fileio
    use files_fm, only: find_file_noselect
    implicit none(type, external)
    private

    public :: collect_novice_fm

contains

    !> Collect all exported unit tests.
    subroutine collect_novice_fm(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("disabled-command-answers", test_answers), &
                    new_unittest("enable-and-disable-command", test_enable_disable), &
                    new_unittest("disabled-message", test_disabled_message) &
                    ]
    end subroutine collect_novice_fm

    subroutine enter(h)
        type(handle_type), intent(out) :: h
        type(status_type) :: st

        call init_bindings()
        call discard_input()
        prefix_arg = value_type()
        disabled_command_function_variable = .true.
        commands%upcase_region%disabled = .true.
        call get_buffer_create("novice-test", h, st)
        call set_buffer(h, st)
        call insert("abc", st)
        call show_alone()
    end subroutine enter

    !> Select the whole text and type `keys`.
    subroutine run(keys)
        character(len=*), intent(in) :: keys
        type(value_type), allocatable :: evs(:)
        type(status_type) :: st

        call push_mark(1_int64, .true., .true.)
        call goto_char(point_max())
        call key_parse(keys, evs, st)
        call unread_command_events(evs)
        call command_loop(st)
    end subroutine run

    logical function disabled(name)
        character(len=*), intent(in) :: name
        type(command_type), pointer :: p

        p => find_command(name)
        disabled = p%disabled
    end function disabled

    subroutine test_answers(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call enter(h)
        ! n: cancel.
        call run("C-x C-u n")
        call check(error, buffer_string() == "abc" .and. disabled("upcase_region"), buffer_string())
        if (allocated(error)) return
        ! SPC: run it once.
        call run("C-x C-u SPC")
        call check(error, buffer_string() == "ABC" .and. disabled("upcase_region"), buffer_string())
        if (allocated(error)) return
        ! Other keys ask again.
        call run("C-x C-l x n")
        call check(error, buffer_string() == "ABC", buffer_string())
        if (allocated(error)) return
        ! y: enable it.
        call run("C-x C-l y")
        call check(error, buffer_string() == "abc" .and. .not. disabled("downcase_region"), buffer_string())
        if (allocated(error)) return
        call run("C-x C-l")
        call check(error, buffer_string() == "abc")
        if (allocated(error)) return
        ! !: enable all disabled commands.
        call run("C-x C-u !")
        call check(error, buffer_string() == "ABC" .and. .not. disabled_command_function_variable, buffer_string())
        if (allocated(error)) return
        call run("C-x C-l")
        call check(error, buffer_string() == "abc", buffer_string())
        disabled_command_function_variable = .true.
        call disable_command("downcase_region", st)
        call discard_input()
        call kill_buffer(h, st)
    end subroutine test_answers

    subroutine test_enable_disable(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, init_buf
        type(status_type) :: st
        character(len=:), allocatable :: text, path
        character(len=*), parameter :: LF = achar(10)

        call init_fileio()
        call expand_file_name("build/test-novice", path, st, emacs_wd)
        call make_directory_internal(path, st)
        path = path//"/init.f90"
        user_init_file = path
        call write_text(user_init_file, "module user_config"//LF//"    use fortmacs"//LF//"contains"//LF// &
                        "    subroutine configure(st)"//LF//"        type(status_type), intent(out) :: st"//LF// &
                        "        commands%upcase_region%disabled = .true."//LF// &
                        "    end subroutine configure"//LF//"end module user_config"//LF)
        call enter(h)
        call enable_command("upcase_region", st)
        call check(error, st%ok(), "enable_command")
        if (allocated(error)) return
        call run("C-x C-u")
        call check(error, buffer_string() == "ABC", buffer_string())
        if (allocated(error)) return
        ! novice.el: the old line is replaced by one at the end.
        text = read_text(user_init_file)
        call check(error, text == "module user_config"//LF//"    use fortmacs"//LF//"contains"//LF// &
                   "    subroutine configure(st)"//LF//"        type(status_type), intent(out) :: st"//LF// &
                   "        commands%upcase_region%disabled = .false."//LF// &
                   "    end subroutine configure"//LF//"end module user_config"//LF, text)
        if (allocated(error)) return
        call disable_command("upcase_region", st)
        call check(error, disabled("upcase_region"))
        if (allocated(error)) return
        text = read_text(user_init_file)
        call check(error, index(text, "commands%upcase_region%disabled = .true.") > 0 .and. &
                   index(text, ".false.") == 0, text)
        if (allocated(error)) return
        call enable_command("no-such-command", st)
        call check(error,.not. st%ok())
        call discard_input()
        call kill_buffer(h, st)
        call find_file_noselect(user_init_file, init_buf, st)
        call kill_buffer(init_buf, st)
        call delete_file_internal(user_init_file, st)
        call delete_file_internal(user_init_file//"~", st)
        deallocate (user_init_file)
    end subroutine test_enable_disable

    subroutine write_text(file, text)
        character(len=*), intent(in) :: file, text
        integer :: u

        open (newunit=u, file=file, access="stream", form="unformatted", status="replace", action="write")
        write (u) text
        close (u)
    end subroutine write_text

    function read_text(file) result(text)
        character(len=*), intent(in) :: file
        character(len=:), allocatable :: text
        integer :: u, n

        open (newunit=u, file=file, access="stream", form="unformatted", status="old", action="read")
        inquire (unit=u, size=n)
        allocate (character(len=n) :: text)
        read (u) text
        close (u)
    end function read_text

    !> A string `disabled' property disables the command too; y clears it.
    subroutine test_disabled_message(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        character(len=:), allocatable :: saved

        call enter(h)
        commands%upcase_region%disabled = .false.
        commands%upcase_region%disabled_message = "Not for you."
        call run("C-x C-u n")
        call check(error, buffer_string() == "abc", buffer_string())
        if (allocated(error)) return
        if (allocated(user_init_file)) saved = user_init_file
        ! (No init file: y enables it for the session.)
        user_init_file = ""
        call run("C-x C-u y")

        if (allocated(saved)) then
            user_init_file = saved
        else
            deallocate (user_init_file)
        end if

        call check(error, buffer_string() == "ABC" .and. .not. allocated(commands%upcase_region%disabled_message), &
                   buffer_string())
        commands%upcase_region%disabled = .true.
        call discard_input()
        call kill_buffer(h, st)
    end subroutine test_disabled_message
end module test_novice_fm
