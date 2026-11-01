! test_emacs_mod.f90 -- tests for emacs_mod's startup: the command-line
! options and the init file's `configure`, run through the `fortmacs`
! API on a pty.
! Own tests; upstream's emacs-tests.el tests seccomp and bwrap, which
! are not ported.
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

module test_emacs_mod
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use buffer_mod, only: erase_buffer, buffers, current
    use editfns_mod, only: buffer_string
    use sysdep_mod, only: open_pty, close_fd, set_tty_size, init_sys_modes, &
                          reset_sys_modes, emacs_write, emacs_read, input_available, &
                          emacs_tty_type
    use keyboard_mod, only: last_message
    use value_mod, only: value_type, xstring
    use data_mod, only: symbol_value, value_eq
    use names_mod, only: user_init_file
    use emacs_mod, only: emacs_main, command_line, arg_type, init_file_had_error, load_init_file, sort_args
    use error_mod, only: format_error, format_message
    use frame_mod, only: selected_frame
    use test_support_mod, only: show_alone
    use window_mod, only: windows, window_leaves
    use fileio_mod, only: emacs_wd, file_name_as_directory, write_region, create_directory_internal, init_fileio
    use buffer_mod, only: close_buffer, get_file_buffer
    use fortmacs, only: status_type, set_error, FM_GENERIC_ERROR, command_type, &
                        keymap_global_set, insert, get_or_make_buffer, set_buffer, &
                        handle_type, tab_width
    implicit none(type, external)
    private

    public :: collect_emacs_mod

    type(command_type), target :: cmd_insert_config_stamp

contains

    !> Collect all exported unit tests.
    subroutine collect_emacs_mod(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("command-line-options", test_command_line), &
                    new_unittest("sort-args", test_sort_args), &
                    new_unittest("init-file-configure", test_configure), &
                    new_unittest("init-file-skipped-with-q", test_no_init_file), &
                    new_unittest("init-file-error", test_init_error), &
                    new_unittest("scratch-fundamental-mode", test_scratch), &
                    new_unittest("initial-window-shows-scratch", test_initial_window), &
                    new_unittest("command-line-displays-files", test_display_files) &
                    ]
    end subroutine collect_emacs_mod

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

    !> Execute the editor on a pty with `keys` typed (they must end with
    !> C-x C-c), *scratch* emptied first; the terminal has `lines` lines
    !> (8 if absent).
    subroutine execute_on_pty(keys, st, args, configure, lines)
        character(len=*), intent(in) :: keys
        type(status_type), intent(out) :: st
        type(arg_type), intent(in), optional :: args(:)
        procedure(sample_configure), optional :: configure
        integer(int32), intent(in), optional :: lines
        type(emacs_tty_type) :: old
        type(handle_type) :: scratch
        type(status_type) :: ignore
        integer(int32) :: master, slave
        character(len=4096) :: buffer
        integer(int64) :: n

        call get_or_make_buffer("*scratch*", scratch, st)
        call set_buffer(scratch, st)
        call erase_buffer(st)
        call open_pty(master, slave, st)
        if (.not. st%ok()) return

        if (present(lines)) then
            call set_tty_size(slave, 40_int32, lines, ignore)
        else
            call set_tty_size(slave, 40_int32, 8_int32, ignore)
        end if

        ! Raw mode first, so C-c is not taken as an interrupt.
        call init_sys_modes(slave, old, ignore)
        call emacs_write(master, keys, ignore)
        call emacs_main("xterm-256color", slave, st, args, configure)

        do while (input_available(master, 100_int32))
            call emacs_read(master, buffer, n, ignore)
            if (n <= 0) exit
        end do

        call reset_sys_modes(slave, old, ignore)
        call close_fd(slave)
        call close_fd(master)
    end subroutine execute_on_pty

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

        call execute_on_pty(achar(3)//"d"//achar(24)//achar(3), st, configure=sample_configure)
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
        call execute_on_pty(achar(3)//"d"//achar(24)//achar(3), st, args, sample_configure)
        call check(error, st%ok(), "emacs_main")
        if (.not. allocated(error)) call check(error, buffer_string(), "d")
        if (.not. allocated(error)) call check(error, tab_width%value == 8)
    end subroutine test_no_init_file

    subroutine test_init_error(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st

        ! The editor still starts.
        call execute_on_pty("hi"//achar(24)//achar(3), st, configure=failing_configure)
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

    !> *scratch* starts empty (no initial-scratch-message: there is no
    !> Lisp to evaluate) and in Fundamental mode (initial-major-mode is
    !> not lisp-interaction-mode), as the project's design decisions say.
    subroutine test_scratch(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        type(value_type) :: mode, name
        type(status_type) :: vst

        call execute_on_pty(achar(24)//achar(3), st)
        call check(error, st%ok(), "emacs_main")
        if (allocated(error)) return
        call check(error, buffers(current)%name == "*scratch*", buffers(current)%name)
        if (allocated(error)) return
        call check(error, buffer_string() == "", "no initial message: "//buffer_string())
        if (allocated(error)) return
        ! The mode as `major_mode' and `mode_name' read (unallocated slots
        ! mean Fundamental mode, as in a fresh buffer).
        call symbol_value(value_type(name="major_mode"), mode, vst)
        call symbol_value(value_type(name="mode_name"), name, vst)
        call check(error, value_eq(mode, value_type(name="fundamental_mode")), "major_mode")
        if (allocated(error)) return
        call check(error, xstring(name) == "Fundamental", xstring(name))
    end subroutine test_scratch

    !> The frame's window shows *scratch*, and what is typed goes there
    !> (frame.c:make_frame gives the root window the current buffer).
    subroutine test_initial_window(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st

        call execute_on_pty("abc"//achar(24)//achar(3), st)
        call check(error, st%ok(), "emacs_main")
        if (allocated(error)) return
        call check(error, windows(selected_frame%root_window%index)%buffer%index == current, "root window shows *scratch*")
        if (allocated(error)) return
        call check(error, buffers(current)%name == "*scratch*", buffers(current)%name)
        if (allocated(error)) return
        call check(error, buffer_string(), "abc")
    end subroutine test_initial_window

    !> Two files on the command line: the second is shown in the selected
    !> window, the first in the other one (startup.el:command-line-1).
    subroutine test_display_files(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        type(handle_type), allocatable :: leaves(:)
        type(handle_type) :: a, b, tmp
        character(len=:), allocatable :: dir
        character(len=256) :: names(2)

        call init_fileio()
        dir = file_name_as_directory(file_name_as_directory(emacs_wd)//"build/test-emacs-files")
        call create_directory_internal(dir, st)
        call get_or_make_buffer(" *emacs-test-file*", tmp, st)
        call set_buffer(tmp, st)
        call insert("A", st)
        call write_region(dir//"a.txt", .false., st)
        call write_region(dir//"b.txt", .false., st)
        call close_buffer(tmp, st)
        names(1) = dir//"a.txt"
        names(2) = dir//"b.txt"
        call execute_on_pty(achar(24)//achar(3), st, argv(names), lines=20_int32)
        call check(error, st%ok(), "emacs_main: "//format_error(st))
        if (allocated(error)) return
        a = get_file_buffer(dir//"a.txt")
        b = get_file_buffer(dir//"b.txt")
        call window_leaves(selected_frame%root_window, leaves)
        call check(error, size(leaves) == 2, "two windows")
        if (allocated(error)) return
        call check(error, windows(selected_frame%selected_window%index)%buffer%index == b%index, &
                   "b.txt in the selected window")
        if (allocated(error)) return
        call check(error, windows(leaves(1)%index)%buffer%index == b%index .and. &
                   windows(leaves(2)%index)%buffer%index == a%index, "b.txt above a.txt")
        call close_buffer(a, st)
        call close_buffer(b, st)
        call show_alone()
    end subroutine test_display_files
end module test_emacs_mod
