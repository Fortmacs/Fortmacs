! test_shell_fm.f90 -- tests for shell_fm.
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

!> Ports of shell-tests.el; own tests of command and environment variable
!> completion.
module test_shell_fm
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use test_support, only: show_alone
    use insdel_fm, only: insert
    use editfns_fm, only: point, goto_char, buffer_string
    use minibuffer_fm, only: completion_at_point
    use env_fm, only: getenv
    use sysdep_fm, only: sys_setenv, sys_unsetenv
    use files_fm, only: make_temp_file
    use fileio_fm, only: init_fileio, write_region, make_directory_internal, delete_file_internal, &
                         delete_directory_internal, set_file_modes
    use error_fm, only: status_type, format_error
    use handle_fm, only: handle_type
    use buffer_fm, only: buffers, cur, get_buffer_create, set_buffer, kill_buffer, generate_new_buffer_name
    use bindings_fm, only: init_bindings
    use modes_fm, only: minor_modes
    use process_fm, only: process_arg_type
    use shell_fm, only: shell_mode, split_string_shell_command, shell__unquote_argument, shell_cd, &
                        shell_parse_pcomplete_arguments, &
                        shell_directory_tracker, list_buffers_directory
    implicit none(type, external)
    private

    public :: collect_shell_fm

contains

    subroutine collect_shell_fm(testsuite)
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
                    new_unittest("shell-directory-tracker-cd", test_tracker_cd) &
                    ]
    end subroutine collect_shell_fm

    !> A temporary buffer in DIR (with-temp-buffer).
    subroutine temp_buffer(h, dir)
        type(handle_type), intent(out) :: h
        character(len=*), intent(in) :: dir
        type(status_type) :: st
        call get_buffer_create(generate_new_buffer_name(" *temp*"), h, st)
        call set_buffer(h, st)
        buffers(cur)%directory = dir
    end subroutine temp_buffer

    subroutine test_unquote_1(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: unquoted
        call init_bindings()
        call shell__unquote_argument("te'st", unquoted)
        call check(error, unquoted, "test")
    end subroutine test_unquote_1

    !> SPLIT gives the arguments EXPECTED (separated by `|').
    subroutine split(error, string, expected)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), intent(in) :: string, expected
        type(process_arg_type), allocatable :: args(:)
        character(len=:), allocatable :: got
        integer :: k
        call split_string_shell_command(string, args)
        got = ""
        do k = 1, size(args)
            if (k > 1) got = got//"|"
            got = got//args(k)%s
        end do
        call check(error, got, expected, string)
    end subroutine split

    subroutine test_split_string(error)
        type(error_type), allocatable, intent(out) :: error
        call init_bindings()
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
        call init_bindings()
        call temp_buffer(h, "/tmp/")
        call shell_mode(st)
        call check(error, st%ok(), format_error(st))
        if (.not. allocated(error)) call check(error, minor_modes%shell_dirtrack_mode%enabled(), "dirtrack")
        call kill_buffer(h, st)
    end subroutine test_dirtrack_on

    subroutine test_dirtrack_unrelated(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        call init_bindings()
        call temp_buffer(h, "/tmp/")
        call check(error,.not. minor_modes%shell_dirtrack_mode%enabled(), "dirtrack")
        call kill_buffer(h, st)
    end subroutine test_dirtrack_unrelated

    subroutine test_sets_list_buffers_directory(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        character(len=*), parameter :: START_DIR = "/usr/bin/"
        call init_bindings()
        call temp_buffer(h, START_DIR)
        call check(error,.not. allocated(list_buffers_directory%value), "should-not")
        if (allocated(error)) return
        call shell_mode(st)
        call shell_cd("..", st)
        call check(error, st%ok(), format_error(st))
        if (.not. allocated(error)) call check(error, allocated(list_buffers_directory%value), "set")
        if (.not. allocated(error)) call check(error, list_buffers_directory%value /= START_DIR, "changed")
        if (.not. allocated(error)) then
            call check(error, index(START_DIR, list_buffers_directory%value) == 1, list_buffers_directory%value)
        end if
        call kill_buffer(h, st)
    end subroutine test_sets_list_buffers_directory

    subroutine test_tracker_cd(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        character(len=*), parameter :: START_DIR = "/usr/bin/"
        call init_bindings()
        call temp_buffer(h, START_DIR)
        call check(error,.not. allocated(list_buffers_directory%value), "should-not")
        if (allocated(error)) return
        call shell_mode(st)
        call shell_directory_tracker("cd ..", st)
        call check(error, st%ok(), format_error(st))
        if (.not. allocated(error)) call check(error, allocated(list_buffers_directory%value), "set")
        if (.not. allocated(error)) call check(error, list_buffers_directory%value /= START_DIR, "changed")
        if (.not. allocated(error)) then
            call check(error, index(START_DIR, list_buffers_directory%value) == 1, list_buffers_directory%value)
        end if
        call kill_buffer(h, st)
    end subroutine test_tracker_cd

    ! The arguments ARGS and BEGINS as "cd|ba 1 4".
    function show_parse(args, begins) result(text)
        type(process_arg_type), intent(in) :: args(:)
        integer(int64), intent(in) :: begins(:)
        character(len=:), allocatable :: text
        character(len=20) :: n
        integer :: k

        text = ""

        do k = 1, size(args)
            if (k > 1) text = text//"|"
            text = text//args(k)%s
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
        type(process_arg_type), allocatable :: args(:)
        integer(int64), allocatable :: begins(:)

        call init_bindings()
        call temp_buffer(h, "/tmp/")
        call shell_mode(st)
        call insert("cd ba;", st)
        call goto_char(point() - 1)
        call shell_parse_pcomplete_arguments(args, begins)
        call check(error, show_parse(args, begins), "cd|ba 1 4")
        call kill_buffer(h, st)
    end subroutine test_completion_before_semi

    subroutine test_completion_after_semi(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        type(process_arg_type), allocatable :: args(:)
        integer(int64), allocatable :: begins(:)

        call init_bindings()
        call temp_buffer(h, "/tmp/")
        call shell_mode(st)
        call insert("cd ba;", st)
        call shell_parse_pcomplete_arguments(args, begins)
        call check(error, show_parse(args, begins), "cd|ba| 1 4 7")
        call kill_buffer(h, st)
    end subroutine test_completion_after_semi

    !> An environment variable naming a directory completes with a slash
    !> after it.
    subroutine test_variable_completion(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        integer :: err

        call init_bindings()
        err = sys_setenv("FORTMACS_TEST_DIRECTORY", "/tmp")
        call temp_buffer(h, "/tmp/")
        call show_alone()
        call shell_mode(st)
        call insert("echo $FORTMACS_TEST_DIRECT", st)
        call completion_at_point(st)
        call check(error, st%ok(), format_error(st))
        if (.not. allocated(error)) call check(error, buffer_string(), "echo $FORTMACS_TEST_DIRECTORY/")
        call kill_buffer(h, st)
        err = sys_unsetenv("FORTMACS_TEST_DIRECTORY")
    end subroutine test_variable_completion

    !> A command name completes from the executables in PATH, with a space
    !> after it.
    subroutine test_command_completion(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, tmp
        type(status_type) :: st
        character(len=:), allocatable :: dir, path
        logical :: found
        integer :: err

        call init_bindings()
        call init_fileio()
        call make_temp_file("shell-command", dir, st)
        call delete_file_internal(dir, st)
        call make_directory_internal(dir, st)
        call get_buffer_create(" shell-command-tmp", tmp, st)
        call set_buffer(tmp, st)
        call write_region(dir//"/fortmacs-test-command", .false., st)
        call set_file_modes(dir//"/fortmacs-test-command", int(o"755", int32), st)
        call kill_buffer(tmp, st)
        call getenv("PATH", path, found)
        err = sys_setenv("PATH", dir)
        call temp_buffer(h, "/tmp/")
        call show_alone()
        call shell_mode(st)
        call insert("fortmacs-test-co", st)
        call completion_at_point(st)
        call check(error, st%ok(), format_error(st))
        if (.not. allocated(error)) call check(error, buffer_string(), "fortmacs-test-command ")
        call kill_buffer(h, st)
        err = sys_setenv("PATH", path)
        call delete_file_internal(dir//"/fortmacs-test-command", st)
        call delete_directory_internal(dir, st)
    end subroutine test_command_completion
end module test_shell_fm
