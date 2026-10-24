! test_compile_fm.f90 -- tests for compile_fm.
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

!> Own tests: upstream's compile-tests.el tests error parsing, which is
!> not ported.  The waits are bounded by a generous wall-clock limit, not
!> by fixed delays.
module test_compile_fm
    use, intrinsic :: iso_fortran_env, only: int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use test_support, only: show_alone
    use error_fm, only: status_type, format_error
    use handle_fm, only: handle_type
    use buffer_fm, only: buffers, cur, get_buffer_create, set_buffer, kill_buffer, erase_buffer, get_buffer, &
                         buffer_live_p, set_buffer_read_only
    use insdel_fm, only: insert
    use editfns_fm, only: buffer_string, point_max
    use keyboard_fm, only: last_message
    use fileio_fm, only: file_name_as_directory
    use files_fm, only: temporary_file_directory, abbreviate_file_name
    use keymap_fm, only: key_binding, key_parse
    use lisp_fm, only: value_type, symbolp, xsymbol, symbols
    use bindings_fm, only: init_bindings
    use process_fm, only: accept_process_output, get_buffer_process, num_processors
    use compile_fm, only: compile, recompile, kill_compilation, compile_command, comint_carriage_motion, &
                          compilation_always_kill
    implicit none(type, external)
    private

    public :: collect_compile_fm

    integer, parameter :: TIMEOUT = 600

contains

    subroutine collect_compile_fm(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)
        testsuite = [ &
                    new_unittest("compile-command-default", test_default_command), &
                    new_unittest("compile-finished", test_finished), &
                    new_unittest("compile-exited-abnormally", test_abnormal), &
                    new_unittest("compile-cd", test_cd), &
                    new_unittest("recompile", test_recompile), &
                    new_unittest("kill-compilation", test_kill), &
                    new_unittest("comint-carriage-motion", test_carriage_motion), &
                    new_unittest("compilation-mode", test_mode) &
                    ]
    end subroutine collect_compile_fm

    !> A fresh source buffer in `dir', shown alone.
    subroutine source_buffer(h, dir)
        type(handle_type), intent(out) :: h
        character(len=*), intent(in) :: dir
        type(status_type) :: st
        call get_buffer_create("*compile-test*", h, st)
        call set_buffer(h, st)
        call erase_buffer(st)
        buffers(cur)%directory = dir
        call show_alone()
        ! (Deleting the other windows makes the selected window's buffer
        ! current.)
        call set_buffer(h, st)
        call show_alone()
    end subroutine source_buffer

    !> Wait until the compilation in *compilation* has finished.
    subroutine wait_compilation()
        type(handle_type) :: buf, proc
        type(status_type) :: st
        integer(int64) :: start, now, rate
        logical :: got
        buf = get_buffer("*compilation*")
        call system_clock(start, rate)
        do
            proc = get_buffer_process(buf)
            if (proc%index == 0) exit
            call system_clock(now)
            if ((now - start)/max(rate, 1_int64) > TIMEOUT) exit
            call accept_process_output(got, st, proc, 1.0)
        end do
    end subroutine wait_compilation

    !> The text of *compilation*.
    function compilation_text() result(text)
        character(len=:), allocatable :: text
        type(handle_type) :: old, buf
        type(status_type) :: st
        text = ""
        buf = get_buffer("*compilation*")
        if (.not. buffer_live_p(buf)) return
        old = buffers_current()
        call set_buffer(buf, st)
        text = buffer_string()
        call set_buffer(old, st)
    end function compilation_text

    function buffers_current() result(h)
        use buffer_fm, only: current_buffer
        type(handle_type) :: h
        h = current_buffer()
    end function buffers_current

    !> Line `n' of `text'.
    function line(text, n) result(l)
        character(len=*), intent(in) :: text
        integer, intent(in) :: n
        character(len=:), allocatable :: l
        integer :: k, b, e
        b = 1
        do k = 1, n - 1
            e = index(text(b:), new_line("a"))
            if (e == 0) then
                l = ""
                return
            end if
            b = b + e
        end do
        e = index(text(b:), new_line("a"))
        if (e == 0) then
            l = text(b:)
        else
            l = text(b:b + e - 2)
        end if
    end function line

    subroutine test_default_command(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=16) :: n
        character(len=:), allocatable :: expected
        if (allocated(compile_command%value)) deallocate (compile_command%value)
        call init_bindings()
        write (n, "(i0)") (2*num_processors() + 2)/3
        expected = "make -k -j"//trim(n)//" "
        call check(error, compile_command%value, expected)
    end subroutine test_default_command

    !> The header, the output and the end line.
    subroutine test_finished(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        character(len=:), allocatable :: text, dir
        call init_bindings()
        dir = file_name_as_directory(temporary_file_directory())
        call source_buffer(h, dir)
        last_message = ""
        call compile("echo built", st)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        call wait_compilation()
        text = compilation_text()
        call check(error, line(text, 1), "-*- mode: compilation; default-directory: """//abbreviate_file_name(dir)//""" -*-")
        if (.not. allocated(error)) call check(error, index(line(text, 2), "Compilation started at ") == 1, line(text, 2))
        if (.not. allocated(error)) call check(error, len(line(text, 2)), len("Compilation started at Fri Oct 24 10:00:00"))
        if (.not. allocated(error)) call check(error, line(text, 3), "")
        if (.not. allocated(error)) call check(error, line(text, 4), "echo built")
        if (.not. allocated(error)) call check(error, line(text, 5), "built")
        if (.not. allocated(error)) call check(error, line(text, 6), "")
        if (.not. allocated(error)) then
            call check(error, index(line(text, 7), "Compilation finished at ") == 1, line(text, 7))
        end if
        if (.not. allocated(error)) call check(error, index(line(text, 7), ", duration ") > 0, line(text, 7))
        if (.not. allocated(error)) call check(error, last_message, "Compilation finished")
        call kill_buffer(h, st)
    end subroutine test_finished

    subroutine test_abnormal(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        character(len=:), allocatable :: text
        call source_buffer(h, file_name_as_directory(temporary_file_directory()))
        call compile("exit 2", st)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        call wait_compilation()
        text = compilation_text()
        call check(error, index(line(text, 6), "Compilation exited abnormally with code 2 at ") == 1, line(text, 6))
        if (.not. allocated(error)) call check(error, last_message, "Compilation exited abnormally with code 2")
        call kill_buffer(h, st)
    end subroutine test_abnormal

    !> A leading "cd DIR;" sets the buffer's directory (the shell does the
    !> cd).
    subroutine test_cd(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, buf
        type(status_type) :: st
        call source_buffer(h, "/")
        call compile("cd /tmp; pwd", st)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        call wait_compilation()
        call check(error, line(compilation_text(), 5), "/tmp")
        if (allocated(error)) return
        buf = get_buffer("*compilation*")
        call check(error, buffers(buf%index)%directory, "/tmp/")
        call kill_buffer(h, st)
    end subroutine test_cd

    !> recompile in *compilation* runs the same command again.
    subroutine test_recompile(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, buf
        type(status_type) :: st
        call source_buffer(h, file_name_as_directory(temporary_file_directory()))
        call compile("echo again", st)
        call wait_compilation()
        buf = get_buffer("*compilation*")
        call set_buffer(buf, st)
        compile_command = "echo other"
        call recompile(st)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        call wait_compilation()
        call check(error, line(compilation_text(), 4), "echo again")
        if (.not. allocated(error)) call check(error, line(compilation_text(), 5), "again")
        call kill_buffer(h, st)
    end subroutine test_recompile

    !> kill-compilation interrupts it.
    subroutine test_kill(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        call source_buffer(h, file_name_as_directory(temporary_file_directory()))
        compilation_always_kill = .true.
        call compile("sleep 100", st)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        call kill_compilation(st)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        call wait_compilation()
        call check(error, index(line(compilation_text(), 6), "Compilation interrupt at ") == 1, line(compilation_text(), 6))
        compilation_always_kill = .false.
        call kill_buffer(h, st)
    end subroutine test_kill

    !> CR and BS in output.
    subroutine test_carriage_motion(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        call source_buffer(h, "/")
        call insert("abc"//achar(13)//"xy"//new_line("a")//"ab"//achar(8)//"c"//new_line("a")//"end"//achar(13), st)
        call comint_carriage_motion(1_int64, point_max())
        call check(error, buffer_string(), "xy"//new_line("a")//"ac"//new_line("a")//"end")
        call kill_buffer(h, st)
    end subroutine test_carriage_motion

    !> The mode of *compilation*: read-only, its keys.
    subroutine test_mode(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, buf
        type(status_type) :: st
        type(value_type), allocatable :: key(:)
        type(value_type) :: b
        call init_bindings()
        call source_buffer(h, file_name_as_directory(temporary_file_directory()))
        call compile("true", st)
        call wait_compilation()
        buf = get_buffer("*compilation*")
        call set_buffer(buf, st)
        call check(error, buffers(cur)%mode_name, "Compilation")
        if (.not. allocated(error)) call check(error, buffers(cur)%read_only, "read-only")
        if (.not. allocated(error)) then
            call key_parse("g", key, st)
            call key_binding(key, b, st)
            call check(error, symbolp(b), "bound")
            if (.not. allocated(error)) call check(error, trim(symbols(xsymbol(b))%name), "recompile")
        end if
        call set_buffer_read_only(buf, .false., st)
        call kill_buffer(buf, st)
        call kill_buffer(h, st)
    end subroutine test_mode
end module test_compile_fm
