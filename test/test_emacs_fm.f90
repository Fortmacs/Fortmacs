! test_emacs_fm.f90 -- tests for emacs_fm's startup: the command-line
! options and the init file's `configure`, run through the `fortmacs`
! API on a pty.  Own tests: upstream has no startup tests for these.
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

module test_emacs_fm
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use buffer_fm, only: erase_buffer
    use editfns_fm, only: buffer_string
    use sysdep_fm, only: open_pty, close_fd, set_tty_size, init_sys_modes, &
                         reset_sys_modes, emacs_write, emacs_read, input_available, &
                         emacs_tty_type
    use keyboard_fm, only: last_message
    use lread_fm, only: user_init_file
    use emacs_fm, only: emacs_main, command_line, arg_type, init_file_had_error, load_init_file, sort_args
    use error_fm, only: format_error, format_message
    use fortmacs, only: status_type, set_error, FM_GENERIC_ERROR, command_type, &
                        keymap_global_set, insert, get_buffer_create, set_buffer, &
                        handle_type, tab_width
    implicit none(type, external)
    private

    public :: collect_emacs_fm

    type(command_type), target :: cmd_insert_config_stamp

contains

    subroutine collect_emacs_fm(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)
        testsuite = [ &
                    new_unittest("command-line-options", test_command_line), &
                    new_unittest("sort-args", test_sort_args), &
                    new_unittest("init-file-configure", test_configure), &
                    new_unittest("init-file-skipped-with-q", test_no_init_file), &
                    new_unittest("init-file-error", test_init_error) &
                    ]
    end subroutine collect_emacs_fm

    !> An init file's `configure`: a setting, a command and a key.
    subroutine sample_configure(status)
        type(status_type), intent(out) :: status
        tab_width = 4
        cmd_insert_config_stamp = command_type("insert-config-stamp", insert_config_stamp)
        call cmd_insert_config_stamp%define()
        call keymap_global_set("C-c d", "insert-config-stamp", status)
    end subroutine sample_configure

    !> The command bound by `sample_configure`.
    subroutine insert_config_stamp(status)
        type(status_type), intent(out) :: status
        call insert("configured", status)
    end subroutine insert_config_stamp

    !> An init file whose `configure` fails.
    subroutine failing_configure(status)
        type(status_type), intent(out) :: status
        call set_error(status, FM_GENERIC_ERROR, "Something is wrong")
    end subroutine failing_configure

    !> Run the editor on a pty with `keys` typed (they must end with
    !> C-x C-c), *scratch* emptied first.
    subroutine run_on_pty(keys, st, args, configure)
        character(len=*), intent(in) :: keys
        type(status_type), intent(out) :: st
        type(arg_type), intent(in), optional :: args(:)
        procedure(sample_configure), optional :: configure
        type(emacs_tty_type) :: old
        type(handle_type) :: scratch
        type(status_type) :: ignore
        integer(int32) :: master, slave
        character(len=4096) :: buf
        integer(int64) :: n
        call get_buffer_create("*scratch*", scratch, st)
        call set_buffer(scratch, st)
        call erase_buffer(st)
        call open_pty(master, slave, st)
        if (.not. st%ok()) return
        call set_tty_size(slave, 40_int32, 8_int32, ignore)
        ! Raw mode first, so C-c is not taken as an interrupt.
        call init_sys_modes(slave, old, ignore)
        call emacs_write(master, keys, ignore)
        call emacs_main("xterm-256color", slave, st, args, configure)
        do while (input_available(master, 100_int32))
            call emacs_read(master, buf, n, ignore)
            if (n <= 0) exit
        end do
        call reset_sys_modes(slave, old, ignore)
        call close_fd(slave)
        call close_fd(master)
    end subroutine run_on_pty

    !> Arguments from strings.
    function argv(strings) result(args)
        character(len=*), intent(in) :: strings(:)
        type(arg_type), allocatable :: args(:)
        integer :: i
        allocate (args(size(strings)))
        do i = 1, size(strings)
            args(i)%s = trim(strings(i))
        end do
    end function argv

    !> The arguments joined by spaces.
    function joined(args) result(s)
        type(arg_type), intent(in) :: args(:)
        character(len=:), allocatable :: s
        integer :: i
        s = ""
        do i = 1, size(args)
            if (i > 1) s = s//" "
            s = s//args(i)%s
        end do
    end function joined

    subroutine test_command_line(error)
        type(error_type), allocatable, intent(out) :: error
        type(arg_type), allocatable :: rest(:)
        type(status_type) :: st
        logical :: init
        call command_line(argv([character(len=4) :: "-q", "+3", "file"]), rest, init, st)
        call check(error, st%ok() .and. .not. init .and. joined(rest) == "+3 file", "-q")
        if (allocated(error)) return
        call command_line(argv([character(len=14) :: "--no-init-file", "+3", "file"]), rest, init, st)
        call check(error, st%ok() .and. .not. init .and. joined(rest) == "+3 file", "--no-init-file")
        if (allocated(error)) return
        call command_line(argv([character(len=2) :: "-Q"]), rest, init, st)
        call check(error,.not. init, "-Q")
        if (allocated(error)) return
        ! Long options may be abbreviated.
        call command_line(argv([character(len=6) :: "--no-i", "file"]), rest, init, st)
        call check(error, st%ok() .and. .not. init .and. joined(rest) == "file", "--no-i")
        if (allocated(error)) return
        call command_line(argv([character(len=5) :: "--no-", "file"]), rest, init, st)
        call check(error, format_error(st) == format_message("Option `--no-' is ambiguous"), format_error(st))
        if (allocated(error)) return
        call command_line(argv([character(len=16) :: "--no-init-file=x"]), rest, init, st)
        call check(error, format_error(st) == format_message("Option `-no-init-file' doesn't allow an argument"), &
                   format_error(st))
        if (allocated(error)) return
        ! The options end at the first other argument.
        call command_line(argv([character(len=4) :: "file", "-q"]), rest, init, st)
        call check(error, init .and. joined(rest) == "file -q", "after a file")
        if (allocated(error)) return
        call command_line(argv([character(len=1) ::]), rest, init, st)
        call check(error, init .and. size(rest) == 0, "no arguments")
    end subroutine test_command_line

    !> emacs.c:sort_args puts known options first, by priority.
    subroutine test_sort_args(error)
        type(error_type), allocatable, intent(out) :: error
        type(arg_type), allocatable :: args(:)
        character(len=:), allocatable :: why
        logical :: ok
        args = argv([character(len=4) :: "file", "-q", "+3", "-Q"])
        call sort_args(args, ok, why)
        call check(error, ok .and. joined(args) == "-Q -q file +3", joined(args))
        if (allocated(error)) return
        args = argv([character(len=4) :: "-q", "a", "-q"])
        call sort_args(args, ok, why)
        call check(error, joined(args) == "-q a", "duplicates")
        if (allocated(error)) return
        args = argv([character(len=4) :: "a", "--", "-q"])
        call sort_args(args, ok, why)
        call check(error, joined(args) == "a -- -q", "after --")
        if (allocated(error)) return
        args = argv([character(len=8) :: "x", "-l", "lib", "--no-ini"])
        call sort_args(args, ok, why)
        call check(error, joined(args) == "--no-ini x -l lib", joined(args))
        if (allocated(error)) return
        args = argv([character(len=4) :: "x", "-l"])
        call sort_args(args, ok, why)
        call check(error,.not. ok .and. why == "Option '-l' requires an argument", why)
    end subroutine test_sort_args

    subroutine test_configure(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        call run_on_pty(achar(3)//"d"//achar(24)//achar(3), st, configure=sample_configure)
        call check(error, st%ok(), "emacs_main")
        if (.not. allocated(error)) call check(error, buffer_string(), "configured")
        if (.not. allocated(error)) call check(error, tab_width%value == 4)
        if (.not. allocated(error)) call check(error,.not. init_file_had_error, "no error")
        tab_width = 8
    end subroutine test_configure

    subroutine test_no_init_file(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        type(arg_type) :: args(1)
        args(1)%s = "-q"
        ! C-c is undefined without the init file, so "d" self-inserts.
        call run_on_pty(achar(3)//"d"//achar(24)//achar(3), st, args, sample_configure)
        call check(error, st%ok(), "emacs_main")
        if (.not. allocated(error)) call check(error, buffer_string(), "d")
        if (.not. allocated(error)) call check(error, tab_width%value == 8)
    end subroutine test_no_init_file

    subroutine test_init_error(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        ! The editor still starts.
        call run_on_pty("hi"//achar(24)//achar(3), st, configure=failing_configure)
        call check(error, st%ok(), "emacs_main")
        if (.not. allocated(error)) call check(error, buffer_string(), "hi")
        if (.not. allocated(error)) call check(error, init_file_had_error, "init-file-had-error")
        if (allocated(error)) return
        ! The message (typing clears it, so it is checked here).
        call load_init_file(failing_configure)
        call check(error, init_file_had_error .and. last_message == &
                   "An error occurred while loading the init file: Something is wrong", "the message")
        if (allocated(error)) return
        call load_init_file(sample_configure)
        call check(error,.not. init_file_had_error, "reset")
        tab_width = 8
        ! Leave no init file name behind for other suites.
        if (allocated(user_init_file)) deallocate (user_init_file)
    end subroutine test_init_error
end module test_emacs_fm
