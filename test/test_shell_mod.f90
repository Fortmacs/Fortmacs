! test_shell_mod.f90 -- tests for shell_mod.
! Ports GNU Emacs 31.1 test/lisp/shell-tests.el in part; the completion
! tests wait for shell completion.
!
! Copyright (C) 2010-2026 Free Software Foundation, Inc.
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

!> Ports of shell-tests.el; own tests of command and environment variable
!> completion.
module test_shell_mod
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use test_support_mod, only: show_alone
    use insert_delete_mod, only: insert
    use editing_functions_mod, only: point, goto_character, buffer_string
    use minibuffer_mod, only: completion_at_point
    use environment_mod, only: getenv, setenv
    use files_mod, only: create_temporary_file
    use file_input_output_mod, only: initialize_fileio, write_region, create_directory_internal, delete_file_internal, &
                                     delete_directory_internal, set_file_modes
    use error_mod, only: status_type, format_error
    use handle_mod, only: handle_type
    use buffer_mod, only: buffers, current, get_or_make_buffer, set_buffer, close_buffer, generate_new_buffer_name
    use bindings_mod, only: initialize_bindings
    use modes_mod, only: minor_modes
    use shell_mod, only: shell_mode, split_string_shell_command, shell_unquote_argument_internal, shell_cd, &
                         shell_parse_programmable_completion_arguments, &
                         shell_directory_tracker, list_buffers_directory, shell_input_autoexpand, &
                         SHELL_INPUT_AUTOEXPAND_INPUT, SHELL_INPUT_AUTOEXPAND_HISTORY
    use command_interpreter_mod, only: command_interpreter_input_autoexpand, COMMAND_INTERPRETER_INPUT_AUTOEXPAND_INPUT, &
                                       COMMAND_INTERPRETER_INPUT_AUTOEXPAND_HISTORY
    use string_type_mod, only: string_type
    implicit none(type, external)
    private

    public :: collect_shell_mod

contains

    !> Collect all exported unit tests.
    subroutine collect_shell_mod(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("shell-tests-unquote-1", test_unquote_1), &
                    new_unittest("shell-tests-completion-before-semi", test_completion_before_semi), &
                    new_unittest("shell-tests-completion-after-semi", test_completion_after_semi), &
                    new_unittest("shell-environment-variable-completion", test_variable_completion), &
                    new_unittest("shell-command-completion", test_command_completion), &
                    new_unittest("shell-tests-split-string", test_split_string), &
                    new_unittest("shell-dirtrack-on-by-default", test_dirtrack_on), &
                    new_unittest("shell-dirtrack-should-not-be-on-in-unrelated-modes", test_dirtrack_unrelated), &
                    new_unittest("shell-dirtrack-sets-list-buffers-directory", test_sets_list_buffers_directory), &
                    new_unittest("shell-directory-tracker-cd", test_tracker_cd), &
                    new_unittest("shell-input-autoexpand", test_input_autoexpand) &
                    ]
    end subroutine collect_shell_mod

    !> A temporary buffer in DIRECTORY (with-temp-buffer).
    subroutine temporary_buffer(h, directory)
        type(handle_type), intent(out) :: h
        character(len=*), intent(in) :: directory
        type(status_type) :: st

        call get_or_make_buffer(generate_new_buffer_name(" *temp*"), h, st)
        call set_buffer(h, st)
        buffers(current)%directory = directory
    end subroutine temporary_buffer

    subroutine test_unquote_1(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: unquoted

        call initialize_bindings()
        call shell_unquote_argument_internal("te'st", unquoted)
        call check(error, unquoted, "test")
    end subroutine test_unquote_1

    !> SPLIT gives the arguments EXPECTED (separated by `|').
    subroutine split(error, string, expected)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), intent(in) :: string, expected
        type(string_type), allocatable :: arguments(:)
        character(len=:), allocatable :: got
        integer :: k

        call split_string_shell_command(string, arguments)
        got = ""

        do k = 1, size(arguments)
            if (k > 1) got = got//"|"
            got = got//arguments(k)%text
        end do

        call check(error, got, expected, string)
    end subroutine split

    subroutine test_split_string(error)
        type(error_type), allocatable, intent(out) :: error

        call initialize_bindings()
        call split(error, "ls /tmp", "ls|/tmp")
        if (.not. allocated(error)) call split(error, "ls '/tmp/foo bar'", "ls|/tmp/foo bar")
        if (.not. allocated(error)) call split(error, "ls ""/tmp/foo bar""", "ls|/tmp/foo bar")
        if (.not. allocated(error)) call split(error, "ls /tmp/'foo bar'", "ls|/tmp/foo bar")
        if (.not. allocated(error)) call split(error, "ls /tmp/'foo""bar'", "ls|/tmp/foo""bar")
        if (.not. allocated(error)) call split(error, "ls /tmp/""foo''bar""", "ls|/tmp/foo''bar")
        if (.not. allocated(error)) call split(error, "ls /tmp/'foo\ bar'", "ls|/tmp/foo\ bar")
        if (.not. allocated(error)) call split(error, "ls /tmp/foo\ bar", "ls|/tmp/foo bar")
    end subroutine test_split_string

    subroutine test_dirtrack_on(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call initialize_bindings()
        call temporary_buffer(h, "/tmp/")
        call shell_mode(st)
        call check(error, st%ok(), format_error(st))
        if (.not. allocated(error)) call check(error, minor_modes%shell_dirtrack_mode%enabled(), "dirtrack")
        call close_buffer(h, st)
    end subroutine test_dirtrack_on

    !> Shell mode gives `command_interpreter_input_autoexpand' the state of
    !> `shell_input_autoexpand', each in its own named constants.  Not a
    !> port.
    subroutine test_input_autoexpand(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call initialize_bindings()
        call temporary_buffer(h, "/tmp/")
        call shell_mode(st)
        call check(error, command_interpreter_input_autoexpand%choice == COMMAND_INTERPRETER_INPUT_AUTOEXPAND_HISTORY, "history")

        if (.not. allocated(error)) then
            shell_input_autoexpand = SHELL_INPUT_AUTOEXPAND_INPUT
            call shell_mode(st)
            call check(error, command_interpreter_input_autoexpand%choice == COMMAND_INTERPRETER_INPUT_AUTOEXPAND_INPUT, "input")
        end if

        if (.not. allocated(error)) then
            shell_input_autoexpand = .false.
            call shell_mode(st)
            call check(error, command_interpreter_input_autoexpand%is_off(), "off")
        end if

        shell_input_autoexpand = SHELL_INPUT_AUTOEXPAND_HISTORY
        call close_buffer(h, st)
    end subroutine test_input_autoexpand

    subroutine test_dirtrack_unrelated(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call initialize_bindings()
        call temporary_buffer(h, "/tmp/")
        call check(error,.not. minor_modes%shell_dirtrack_mode%enabled(), "dirtrack")
        call close_buffer(h, st)
    end subroutine test_dirtrack_unrelated

    subroutine test_sets_list_buffers_directory(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        character(len=*), parameter :: START_DIRECTORY = "/usr/bin/"

        call initialize_bindings()
        call temporary_buffer(h, START_DIRECTORY)
        call check(error,.not. allocated(list_buffers_directory%value), "should-not")
        if (allocated(error)) return
        call shell_mode(st)
        call shell_cd("..", st)
        call check(error, st%ok(), format_error(st))
        if (.not. allocated(error)) call check(error, allocated(list_buffers_directory%value), "set")
        if (.not. allocated(error)) call check(error, list_buffers_directory%value /= START_DIRECTORY, "changed")

        if (.not. allocated(error)) then
            call check(error, index(START_DIRECTORY, list_buffers_directory%value) == 1, list_buffers_directory%value)
        end if

        call close_buffer(h, st)
    end subroutine test_sets_list_buffers_directory

    subroutine test_tracker_cd(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        character(len=*), parameter :: START_DIRECTORY = "/usr/bin/"

        call initialize_bindings()
        call temporary_buffer(h, START_DIRECTORY)
        call check(error,.not. allocated(list_buffers_directory%value), "should-not")
        if (allocated(error)) return
        call shell_mode(st)
        call shell_directory_tracker("cd ..", st)
        call check(error, st%ok(), format_error(st))
        if (.not. allocated(error)) call check(error, allocated(list_buffers_directory%value), "set")
        if (.not. allocated(error)) call check(error, list_buffers_directory%value /= START_DIRECTORY, "changed")

        if (.not. allocated(error)) then
            call check(error, index(START_DIRECTORY, list_buffers_directory%value) == 1, list_buffers_directory%value)
        end if

        call close_buffer(h, st)
    end subroutine test_tracker_cd

    ! The arguments ARGUMENTS and BEGINS as "cd|ba 1 4".
    function show_parse(arguments, begins) result(text)
        type(string_type), intent(in) :: arguments(:)
        integer(int64), intent(in) :: begins(:)
        character(len=:), allocatable :: text
        character(len=20) :: n
        integer :: k

        text = ""

        do k = 1, size(arguments)
            if (k > 1) text = text//"|"
            text = text//arguments(k)%text
        end do

        do k = 1, size(begins)
            write (n, "(i0)") begins(k)
            text = text//" "//trim(n)
        end do
    end function show_parse

    subroutine test_completion_before_semi(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        type(string_type), allocatable :: arguments(:)
        integer(int64), allocatable :: begins(:)

        call initialize_bindings()
        call temporary_buffer(h, "/tmp/")
        call shell_mode(st)
        call insert("cd ba;", st)
        call goto_character(point() - 1)
        call shell_parse_programmable_completion_arguments(arguments, begins)
        call check(error, show_parse(arguments, begins), "cd|ba 1 4")
        call close_buffer(h, st)
    end subroutine test_completion_before_semi

    subroutine test_completion_after_semi(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        type(string_type), allocatable :: arguments(:)
        integer(int64), allocatable :: begins(:)

        call initialize_bindings()
        call temporary_buffer(h, "/tmp/")
        call shell_mode(st)
        call insert("cd ba;", st)
        call shell_parse_programmable_completion_arguments(arguments, begins)
        call check(error, show_parse(arguments, begins), "cd|ba| 1 4 7")
        call close_buffer(h, st)
    end subroutine test_completion_after_semi

    !> An environment variable naming a directory completes with a slash
    !> after it.
    subroutine test_variable_completion(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call initialize_bindings()
        call setenv("FORTMACS_TEST_DIRECTORY", "/tmp")
        call temporary_buffer(h, "/tmp/")
        call show_alone()
        call shell_mode(st)
        call insert("echo $FORTMACS_TEST_DIRECT", st)
        call completion_at_point(st)
        call check(error, st%ok(), format_error(st))
        if (.not. allocated(error)) call check(error, buffer_string(), "echo $FORTMACS_TEST_DIRECTORY/")
        call close_buffer(h, st)
        call setenv("FORTMACS_TEST_DIRECTORY")
    end subroutine test_variable_completion

    !> A command name completes from the executables in PATH, with a space
    !> after it.
    subroutine test_command_completion(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, temporary
        type(status_type) :: st
        character(len=:), allocatable :: directory, path
        logical :: found

        call initialize_bindings()
        call initialize_fileio()
        call create_temporary_file("shell-command", directory, st)
        call delete_file_internal(directory, st)
        call create_directory_internal(directory, st)
        call get_or_make_buffer(" shell-command-tmp", temporary, st)
        call set_buffer(temporary, st)
        call write_region(directory//"/fortmacs-test-command", .false., st)
        call set_file_modes(directory//"/fortmacs-test-command", int(o"755", int32), st)
        call close_buffer(temporary, st)
        call getenv("PATH", path, found)
        call setenv("PATH", directory)
        call temporary_buffer(h, "/tmp/")
        call show_alone()
        call shell_mode(st)
        call insert("fortmacs-test-co", st)
        call completion_at_point(st)
        call check(error, st%ok(), format_error(st))
        if (.not. allocated(error)) call check(error, buffer_string(), "fortmacs-test-command ")
        call close_buffer(h, st)
        call setenv("PATH", path)
        call delete_file_internal(directory//"/fortmacs-test-command", st)
        call delete_directory_internal(directory, st)
    end subroutine test_command_completion
end module test_shell_mod
