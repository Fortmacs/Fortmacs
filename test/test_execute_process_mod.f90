! test_execute_process_mod.f90 -- tests for execute_process_mod.
! Ports GNU Emacs 31.1 test/src/callproc-tests.el in part;
! initial-environment-preserved and call-process-w32-debug-spawn-error
! are for MS-Windows.
!
! Copyright (C) 2016-2026 Free Software Foundation, Inc.
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

module test_execute_process_mod
    use, intrinsic :: iso_fortran_env, only: int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_mod, only: status_type, format_error, FM_FILE_MISSING
    use handle_mod, only: handle_type
    use value_mod, only: value_type, list, xfixnum, xstring, vtag, V_INTEGER, V_STRING
    use buffer_mod, only: buffers, current, get_or_make_buffer, set_buffer, close_buffer, erase_buffer, &
                          current_buffer
    use insert_delete_mod, only: insert
    use editing_functions_mod, only: buffer_string, buffer_size, point
    use file_input_output_mod, only: write_region, delete_file_internal
    use files_mod, only: temporary_file_directory
    use execute_process_mod, only: execute_process, execute_process_region, make_environment_block, getenv_internal, &
                                   GETENV_SET, GETENV_NOT_SET, GETENV_REMOVED
    use environment_mod, only: getenv, setenv
    use string_type_mod, only: string_type, append_string
    implicit none(type, external)
    private

    public :: collect_execute_process_mod

contains

    !> Collect all exported unit tests.
    subroutine collect_execute_process_mod(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("call-process-region-entire-buffer-with-delete", test_region_delete), &
                    new_unittest("call-process-output", test_output), &
                    new_unittest("call-process-exit-status", test_exit_status), &
                    new_unittest("call-process-signal", test_signal), &
                    new_unittest("call-process-missing-program", test_missing_program), &
                    new_unittest("call-process-infile", test_infile), &
                    new_unittest("call-process-destination-buffer-name", test_buffer_name), &
                    new_unittest("call-process-default-directory", test_directory), &
                    new_unittest("call-process-region-filter", test_region_filter), &
                    new_unittest("process-environment-inherited", test_environment_inherited), &
                    new_unittest("process-environment-overrides", test_environment_overrides), &
                    new_unittest("make-environment-block", test_environment_block), &
                    new_unittest("getenv-internal-status", test_getenv_internal_status), &
                    new_unittest("getenv-internal-initial", test_getenv_internal_initial) &
                    ]
    end subroutine collect_execute_process_mod

    !> A fresh, current buffer `name` holding `text`.
    subroutine fresh_buffer(name, text, h)
        character(len=*), intent(in) :: name, text
        type(handle_type), intent(out) :: h
        type(status_type) :: st

        call get_or_make_buffer(name, h, st)
        call set_buffer(h, st)
        call erase_buffer(st)
        if (len(text) > 0) call insert(text, st)
    end subroutine fresh_buffer

    subroutine done(h)
        type(handle_type), intent(in) :: h
        type(status_type) :: st

        call close_buffer(h, st)
    end subroutine done

    !> Check that Bug#40576 is fixed.  (Upstream runs `emacs --version`;
    !> any program that ignores its input will do.)
    subroutine test_region_delete(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(value_type) :: r
        type(status_type) :: st

        call fresh_buffer("*callproc-test*", "Buffer contents"//new_line("a"), h)
        call execute_process_region("true", r, st, delete=.true.)
        call check(error, st%ok(), format_error(st))
        if (.not. allocated(error)) call check(error, vtag(r) == V_INTEGER .and. xfixnum(r) == 0, "status 0")
        if (.not. allocated(error)) call check(error, buffer_size(), 0_int64)
        call done(h)
    end subroutine test_region_delete

    !> Output goes into the current buffer before point; text is UTF-8.
    subroutine test_output(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(value_type) :: r
        type(status_type) :: st

        call fresh_buffer("*callproc-test*", "", h)
        call execute_process("echo", r, st, destination=value_type(.true.), &
                             args=list(value_type(text="hello"), value_type(text="w"//char(195)//char(182)//"rld")))
        call check(error, st%ok(), format_error(st))
        if (.not. allocated(error)) call check(error, xfixnum(r) == 0, "status 0")

        if (.not. allocated(error)) then
            call check(error, buffer_string(), "hello w"//char(195)//char(182)//"rld"//new_line("a"))
        end if

        if (.not. allocated(error)) call check(error, buffer_size(), 12_int64)
        if (.not. allocated(error)) call check(error, point(), 13_int64)

        ! Standard error is mixed with the output; discarded with no
        ! destination.
        if (.not. allocated(error)) then
            call erase_buffer(st)
            call execute_process("sh", r, st, destination=value_type(.true.), &
                                 args=list(value_type(text="-c"), value_type(text="echo out; echo err >&2")))
            call check(error, buffer_string(), "out"//new_line("a")//"err"//new_line("a"))
        end if

        if (.not. allocated(error)) then
            call erase_buffer(st)
            call execute_process("echo", r, st, args=list(value_type(text="discarded")))
            call check(error, st%ok() .and. buffer_size() == 0, "discarded")
        end if

        call done(h)
    end subroutine test_output

    subroutine test_exit_status(error)
        type(error_type), allocatable, intent(out) :: error
        type(value_type) :: r
        type(status_type) :: st

        call execute_process("false", r, st)
        call check(error, st%ok() .and. vtag(r) == V_INTEGER .and. xfixnum(r) == 1, "false")
        if (allocated(error)) return
        call execute_process("sh", r, st, args=list(value_type(text="-c"), value_type(text="exit 42")))
        call check(error, xfixnum(r), 42_int64)
    end subroutine test_exit_status

    !> A program killed by a signal gives the signal's description.
    subroutine test_signal(error)
        type(error_type), allocatable, intent(out) :: error
        type(value_type) :: r
        type(status_type) :: st

        call execute_process("sh", r, st, args=list(value_type(text="-c"), value_type(text="kill -TERM $$")))
        call check(error, st%ok() .and. vtag(r) == V_STRING, "a string")
        if (.not. allocated(error)) call check(error, xstring(r), "Terminated")
    end subroutine test_signal

    subroutine test_missing_program(error)
        type(error_type), allocatable, intent(out) :: error
        type(value_type) :: r
        type(status_type) :: st

        call execute_process("fortmacs-no-such-program", r, st)
        call check(error, st%code == FM_FILE_MISSING, "file_missing")

        if (.not. allocated(error)) then
            call check(error, format_error(st), &
                       "Searching for program: No such file or directory, fortmacs-no-such-program")
        end if
    end subroutine test_missing_program

    subroutine test_infile(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(value_type) :: r
        type(status_type) :: st
        character(len=:), allocatable :: file

        file = temporary_file_directory()//"fortmacs-callproc-infile"
        call fresh_buffer("*callproc-test*", "from a file", h)
        call write_region(file, .false., st)
        call erase_buffer(st)
        call execute_process("cat", r, st, infile=file, destination=value_type(.true.))
        call check(error, st%ok(), format_error(st))
        if (.not. allocated(error)) call check(error, buffer_string(), "from a file")
        call delete_file_internal(file, st)

        if (.not. allocated(error)) then
            call execute_process("cat", r, st, infile=file)
            call check(error, format_error(st), &
                       "Opening process input file: No such file or directory, "//file)
        end if

        call done(h)
    end subroutine test_infile

    subroutine test_buffer_name(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, out, now
        type(value_type) :: r
        type(status_type) :: st

        call fresh_buffer("*callproc-test*", "", h)
        call execute_process("echo", r, st, destination=value_type(text="*callproc-out*"), &
                             args=list(value_type(text="elsewhere")))
        call check(error, st%ok() .and. buffer_size() == 0, "not in the current buffer")
        if (allocated(error)) return
        now = current_buffer()
        call check(error, now%index == h%index, "current buffer kept")
        if (allocated(error)) return
        call get_or_make_buffer("*callproc-out*", out, st)
        call set_buffer(out, st)
        call check(error, buffer_string(), "elsewhere"//new_line("a"))
        call done(out)
        call done(h)
    end subroutine test_buffer_name

    !> The program runs in the current buffer's default-directory (expanded).
    subroutine test_directory(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(value_type) :: r
        type(status_type) :: st
        character(len=:), allocatable :: home
        integer :: length, stat

        call fresh_buffer("*callproc-test*", "", h)
        buffers(current)%directory = "/"
        call execute_process("pwd", r, st, destination=value_type(.true.))
        call check(error, buffer_string(), "/"//new_line("a"))

        if (.not. allocated(error)) then
            buffers(current)%directory = "/fortmacs-no-such-directory/"
            call execute_process("pwd", r, st)
            call check(error, format_error(st), &
                       "Setting current directory: No such file or directory, /fortmacs-no-such-directory/")
        end if

        if (.not. allocated(error)) then
            ! A default-directory under `~' is expanded first.
            call get_environment_variable("HOME", length=length, status=stat)
            allocate (character(len=max(length, 0)) :: home)
            if (stat == 0 .and. length > 0) call get_environment_variable("HOME", home)
            call setenv("HOME", "/")
            call erase_buffer(st)
            buffers(current)%directory = "~/"
            call execute_process("pwd", r, st, destination=value_type(.true.))

            if (stat == 0) then
                call setenv("HOME", home)
            else
                call setenv("HOME")
            end if

            call check(error, buffer_string(), "/"//new_line("a"))
        end if

        call done(h)
    end subroutine test_directory

    !> Part of the buffer through a filter, the output after it.
    subroutine test_region_filter(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(value_type) :: r
        type(status_type) :: st

        call fresh_buffer("*callproc-test*", "hello world", h)
        call execute_process_region("tr", r, st, 1_int64, 6_int64, destination=value_type(.true.), &
                                    args=list(value_type(text="a-z"), value_type(text="A-Z")))
        call check(error, st%ok(), format_error(st))
        if (.not. allocated(error)) call check(error, buffer_string(), "hello worldHELLO")

        if (.not. allocated(error)) then
            call execute_process_region("tr", r, st, 1_int64, 6_int64, delete=.true., destination=value_type(.true.), &
                                        args=list(value_type(text="a-z"), value_type(text="A-Z")))
            call check(error, buffer_string(), " worldHELLOHELLO")
        end if

        call done(h)
    end subroutine test_region_filter

    !> A variable `setenv' sets or removes reaches the program, through
    !> `process_environment'.
    subroutine test_environment_inherited(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(value_type) :: r
        type(status_type) :: st

        call fresh_buffer("*callproc-test*", "", h)
        call setenv("FORTMACS_CALLPROC_TEST", "hello")
        call execute_process("sh", r, st, destination=value_type(.true.), args=print_test_variable())
        call check(error, buffer_string(), "hello")

        if (.not. allocated(error)) then
            call erase_buffer(st)
            call setenv("FORTMACS_CALLPROC_TEST")
            call execute_process("sh", r, st, destination=value_type(.true.), args=print_test_variable())
            call check(error, buffer_string(), "unset")
        end if

        call done(h)
    end subroutine test_environment_inherited

    !> ENVIRONMENT's entries come before `process_environment', as when it
    !> is bound to a list starting with them; a lone name removes the
    !> variable.
    subroutine test_environment_overrides(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(value_type) :: r
        type(status_type) :: st

        call fresh_buffer("*callproc-test*", "", h)
        call setenv("FORTMACS_CALLPROC_TEST", "hello")
        call execute_process("sh", r, st, destination=value_type(.true.), args=print_test_variable(), &
                             environment=list(value_type(text="FORTMACS_CALLPROC_TEST=over")))
        call check(error, buffer_string(), "over")

        if (.not. allocated(error)) then
            call erase_buffer(st)
            call execute_process("sh", r, st, destination=value_type(.true.), args=print_test_variable(), &
                                 environment=list(value_type(text="FORTMACS_CALLPROC_TEST")))
            call check(error, buffer_string(), "unset")
        end if

        call setenv("FORTMACS_CALLPROC_TEST")
        call done(h)
    end subroutine test_environment_overrides

    !> ARGS for sh printing FORTMACS_CALLPROC_TEST, or "unset".
    function print_test_variable() result(args)
        type(value_type) :: args

        args = list(value_type(text="-c"), value_type(text='printf %s "${FORTMACS_CALLPROC_TEST-unset}"'))
    end function print_test_variable

    !> make_environment_block: PWD set to the directory without trailing
    !> slashes (but "/" and "//" kept), the first definition of a variable
    !> wins, lone names removed.
    subroutine test_environment_block(error)
        type(error_type), allocatable, intent(out) :: error
        type(string_type), allocatable :: block(:)
        type(string_type), allocatable :: overrides(:)
        character(len=:), allocatable :: saved
        logical :: found
        integer :: k

        call getenv("PWD", saved, found)
        call setenv("PWD", "/somewhere")
        call append_string(overrides, "FORTMACS_A=1", "FORTMACS_A=2", "FORTMACS_B", "FORTMACS_B=3")
        call make_environment_block("/tmp///", block, overrides)
        call check(error, block(1)%text, "PWD=/tmp")
        if (.not. allocated(error)) call check(error, count([(block(k)%text == "FORTMACS_A=1", k=1, size(block))]), 1)
        if (.not. allocated(error)) call check(error,.not. any([(block(k)%text == "FORTMACS_A=2", k=1, size(block))]))

        if (.not. allocated(error)) then
            call check(error,.not. any([(index(block(k)%text, "FORTMACS_B") == 1, k=1, size(block))]))
        end if

        if (.not. allocated(error)) then
            call check(error, count([(index(block(k)%text, "PWD=") == 1, k=1, size(block))]), 1)
        end if

        if (.not. allocated(error)) then
            call make_environment_block("/", block)
            call check(error, block(1)%text, "PWD=/")
        end if

        if (.not. allocated(error)) then
            call make_environment_block("//", block)
            call check(error, block(1)%text, "PWD=//")
        end if

        if (found) then
            call setenv("PWD", saved)
        else
            call setenv("PWD")
        end if
    end subroutine test_environment_block

    !> getenv_internal's status in `process_environment': a variable that
    !> is set, one that is not set, and one that `setenv' removed (a lone
    !> name).
    subroutine test_getenv_internal_status(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: value
        integer :: stat

        call setenv("FORTMACS_GETENV_TEST", "hello")
        call getenv_internal("FORTMACS_GETENV_TEST", value, stat)
        call check(error, stat == GETENV_SET .and. value == "hello", "set")

        if (.not. allocated(error)) then
            call getenv_internal("FORTMACS_GETENV_NEVER_SET", value, stat)
            call check(error, stat == GETENV_NOT_SET .and. value == "", "not set")
        end if

        if (.not. allocated(error)) then
            call setenv("FORTMACS_GETENV_TEST")
            call getenv_internal("FORTMACS_GETENV_TEST", value, stat)
            call check(error, stat == GETENV_REMOVED .and. value == "", "removed")
        end if
    end subroutine test_getenv_internal_status

    !> getenv_internal with INITIAL searches `initial_environment', which
    !> `setenv' leaves alone (upstream's (getenv-internal VARIABLE
    !> initial-environment)).
    subroutine test_getenv_internal_initial(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: value, path
        integer :: stat, length, path_status

        call setenv("FORTMACS_GETENV_INITIAL_TEST", "hello")
        call getenv_internal("FORTMACS_GETENV_INITIAL_TEST", value, stat, initial=.true.)
        call check(error, stat == GETENV_NOT_SET .and. value == "", "not in the initial environment")
        call setenv("FORTMACS_GETENV_INITIAL_TEST")
        if (allocated(error)) return
        call get_environment_variable("PATH", length=length, status=path_status)
        if (path_status /= 0) return
        allocate (character(len=length) :: path)
        call get_environment_variable("PATH", path)
        call getenv_internal("PATH", value, stat, initial=.true.)
        call check(error, stat == GETENV_SET .and. value == path, "PATH")
        if (allocated(error)) return
        call getenv_internal("PATH", value, stat, initial=.false.)
        call check(error, stat == GETENV_SET .and. value == path, "PATH, initial false")
    end subroutine test_getenv_internal_initial
end module test_execute_process_mod
