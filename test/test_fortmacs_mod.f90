! test_fortmacs_mod.f90 -- tests for fortmacs_mod's startup: the command-line
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

module test_fortmacs_mod
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use buffer_mod, only: erase_buffer, buffers, current
    use editing_functions_mod, only: buffer_string
    use system_dependent_mod, only: open_pty, close_fd, set_terminal_size, initialize_system_modes, &
                                    reset_system_modes, fortmacs_write, fortmacs_read, input_available, &
                                    fortmacs_terminal_type
    use keyboard_mod, only: last_message
    use value_mod, only: value_type, xstring
    use data_mod, only: symbol_value, value_eq
    use names_mod, only: user_initialization_file
    use fortmacs_mod, only: fortmacs_main, command_line, initialization_file_had_error, load_initialization_file, sort_arguments
    use error_mod, only: format_error, format_message
    use frame_mod, only: selected_frame
    use test_support_mod, only: show_alone
    use window_mod, only: windows, window_leaves
    use file_input_output_mod, only: fortmacs_wd, file_name_as_directory, write_region, create_directory_internal, initialize_fileio
    use buffer_mod, only: close_buffer, get_file_buffer
    use fortmacs, only: status_type, set_error, GENERIC_ERROR_CODE, command_type, &
                        keymap_global_set, insert, get_or_make_buffer, set_buffer, &
                        handle_type, tab_width, message_log_maximum, show_parenthesis_style, SHOW_PARENTHESIS_STYLE_PARENTHESIS, &
                        shell_indent_for_fi, shell_indent_after_if, SHELL_MINUS, SHELL_PLUS
    use string_type_mod, only: string_type
    implicit none(type, external)
    private

    public :: collect_fortmacs_mod

    type(command_type), target :: command_insert_configuration_stamp

contains

    !> Collect all exported unit tests.
    subroutine collect_fortmacs_mod(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("command-line-options", test_command_line), &
                    new_unittest("sort-args", test_sort_arguments), &
                    new_unittest("init-file-configure", test_configure), &
                    new_unittest("init-file-skipped-with-q", test_no_initialization_file), &
                    new_unittest("init-file-error", test_initialize_error), &
                    new_unittest("init-file-rejected-choice", test_rejected_choice), &
                    new_unittest("scratch-fundamental-mode", test_scratch), &
                    new_unittest("initial-window-shows-scratch", test_initial_window), &
                    new_unittest("command-line-displays-files", test_display_files) &
                    ]
    end subroutine collect_fortmacs_mod

    !> An init file's `configure`: a setting, a command and a key.
    subroutine sample_configure(status)
        type(status_type), intent(out) :: status

        tab_width = 4
        command_insert_configuration_stamp = command_type("insert-config-stamp", insert_configuration_stamp)
        call command_insert_configuration_stamp%define()
        call keymap_global_set("C-c d", "insert-config-stamp", status)
    end subroutine sample_configure

    !> The command bound by `sample_configure`.
    subroutine insert_configuration_stamp(status)
        type(status_type), intent(out) :: status

        call insert("configured", status)
    end subroutine insert_configuration_stamp

    !> An init file whose `configure` fails.
    subroutine failing_configure(status)
        type(status_type), intent(out) :: status

        call set_error(status, GENERIC_ERROR_CODE, "Something is wrong")
    end subroutine failing_configure

    !> An init file giving choice settings values they do not accept.
    subroutine rejecting_configure(status)
        type(status_type), intent(out) :: status

        status = status_type()
        show_parenthesis_style = .true.
        shell_indent_for_fi = 0
        shell_indent_after_if = SHELL_MINUS
        message_log_maximum = 500
    end subroutine rejecting_configure

    !> Execute the editor on a pty with `keys` typed (they must end with
    !> C-x C-c), *scratch* emptied first; the terminal has `lines` lines
    !> (8 if absent).
    subroutine execute_on_pty(keys, st, arguments, configure, lines)
        character(len=*), intent(in) :: keys
        type(status_type), intent(out) :: st
        type(string_type), intent(in), optional :: arguments(:)
        procedure(sample_configure), optional :: configure
        integer(int32), intent(in), optional :: lines
        type(fortmacs_terminal_type) :: old
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
            call set_terminal_size(slave, 40_int32, lines, ignore)
        else
            call set_terminal_size(slave, 40_int32, 8_int32, ignore)
        end if

        ! Raw mode first, so C-c is not taken as an interrupt.
        call initialize_system_modes(slave, old, ignore)
        call fortmacs_write(master, keys, ignore)
        call fortmacs_main("xterm-256color", slave, st, arguments, configure)

        do while (input_available(master, 100_int32))
            call fortmacs_read(master, buffer, n, ignore)
            if (n <= 0) exit
        end do

        call reset_system_modes(slave, old, ignore)
        call close_fd(slave)
        call close_fd(master)
    end subroutine execute_on_pty

    !> Arguments from strings.
    function argv(strings) result(arguments)
        character(len=*), intent(in) :: strings(:)
        type(string_type), allocatable :: arguments(:)
        integer :: i

        allocate (arguments(size(strings)))

        do i = 1, size(strings)
            arguments(i)%text = trim(strings(i))
        end do
    end function argv

    !> The arguments joined by spaces.
    function joined(arguments) result(s)
        type(string_type), intent(in) :: arguments(:)
        character(len=:), allocatable :: s
        integer :: i

        s = ""

        do i = 1, size(arguments)
            if (i > 1) s = s//" "
            s = s//arguments(i)%text
        end do
    end function joined

    subroutine test_command_line(error)
        type(error_type), allocatable, intent(out) :: error
        type(string_type), allocatable :: rest(:)
        type(status_type) :: st
        logical :: initialize

        call command_line(argv([character(len=4) :: "-q", "+3", "file"]), rest, initialize, st)
        call check(error, st%ok() .and. .not. initialize .and. joined(rest) == "+3 file", "-q")
        if (allocated(error)) return
        call command_line(argv([character(len=14) :: "--no-init-file", "+3", "file"]), rest, initialize, st)
        call check(error, st%ok() .and. .not. initialize .and. joined(rest) == "+3 file", "--no-init-file")
        if (allocated(error)) return
        call command_line(argv([character(len=2) :: "-Q"]), rest, initialize, st)
        call check(error,.not. initialize, "-Q")
        if (allocated(error)) return
        ! Long options may be abbreviated.
        call command_line(argv([character(len=6) :: "--no-i", "file"]), rest, initialize, st)
        call check(error, st%ok() .and. .not. initialize .and. joined(rest) == "file", "--no-i")
        if (allocated(error)) return
        call command_line(argv([character(len=5) :: "--no-", "file"]), rest, initialize, st)
        call check(error, format_error(st) == format_message("Option `--no-' is ambiguous"), format_error(st))
        if (allocated(error)) return
        call command_line(argv([character(len=16) :: "--no-init-file=x"]), rest, initialize, st)
        call check(error, format_error(st) == format_message("Option `-no-init-file' doesn't allow an argument"), &
                   format_error(st))
        if (allocated(error)) return
        ! The options end at the first other argument.
        call command_line(argv([character(len=4) :: "file", "-q"]), rest, initialize, st)
        call check(error, initialize .and. joined(rest) == "file -q", "after a file")
        if (allocated(error)) return
        call command_line(argv([character(len=1) ::]), rest, initialize, st)
        call check(error, initialize .and. size(rest) == 0, "no arguments")
    end subroutine test_command_line

    !> emacs.c:sort_args puts known options first, by priority.
    subroutine test_sort_arguments(error)
        type(error_type), allocatable, intent(out) :: error
        type(string_type), allocatable :: arguments(:)
        character(len=:), allocatable :: why
        logical :: ok

        arguments = argv([character(len=4) :: "file", "-q", "+3", "-Q"])
        call sort_arguments(arguments, ok, why)
        call check(error, ok .and. joined(arguments) == "-Q -q file +3", joined(arguments))
        if (allocated(error)) return
        arguments = argv([character(len=4) :: "-q", "a", "-q"])
        call sort_arguments(arguments, ok, why)
        call check(error, joined(arguments) == "-q a", "duplicates")
        if (allocated(error)) return
        arguments = argv([character(len=4) :: "a", "--", "-q"])
        call sort_arguments(arguments, ok, why)
        call check(error, joined(arguments) == "a -- -q", "after --")
        if (allocated(error)) return
        arguments = argv([character(len=8) :: "x", "-l", "lib", "--no-ini"])
        call sort_arguments(arguments, ok, why)
        call check(error, joined(arguments) == "--no-ini x -l lib", joined(arguments))
        if (allocated(error)) return
        arguments = argv([character(len=4) :: "x", "-l"])
        call sort_arguments(arguments, ok, why)
        call check(error,.not. ok .and. why == "Option '-l' requires an argument", why)
    end subroutine test_sort_arguments

    subroutine test_configure(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st

        call execute_on_pty(achar(3)//"d"//achar(24)//achar(3), st, configure=sample_configure)
        call check(error, st%ok(), "fortmacs_main")
        if (.not. allocated(error)) call check(error, buffer_string(), "configured")
        if (.not. allocated(error)) call check(error, tab_width%value == 4)
        if (.not. allocated(error)) call check(error,.not. initialization_file_had_error, "no error")
        tab_width = 8
    end subroutine test_configure

    subroutine test_no_initialization_file(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        type(string_type) :: arguments(1)

        arguments(1)%text = "-q"
        ! Without the init file C-c d is an undefined key (C-c is a prefix key, as
        ! in Emacs), so it inserts nothing; the second "d" self-inserts.
        call execute_on_pty(achar(3)//"dd"//achar(24)//achar(3), st, arguments, sample_configure)
        call check(error, st%ok(), "fortmacs_main")
        if (.not. allocated(error)) call check(error, buffer_string(), "d")
        if (.not. allocated(error)) call check(error, tab_width%value == 8)
    end subroutine test_no_initialization_file

    subroutine test_initialize_error(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st

        ! The editor still starts.
        call execute_on_pty("hi"//achar(24)//achar(3), st, configure=failing_configure)
        call check(error, st%ok(), "fortmacs_main")
        if (.not. allocated(error)) call check(error, buffer_string(), "hi")
        if (.not. allocated(error)) call check(error, initialization_file_had_error, "init-file-had-error")
        if (allocated(error)) return
        ! The message (typing clears it, so it is checked here).
        call load_initialization_file(failing_configure)
        call check(error, initialization_file_had_error .and. last_message == &
                   "An error occurred while loading the init file: Something is wrong", "the message")
        if (allocated(error)) return
        call load_initialization_file(sample_configure)
        call check(error,.not. initialization_file_had_error, "reset")
        tab_width = 8
        ! Leave no init file name behind for other suites.
        if (allocated(user_initialization_file)) deallocate (user_initialization_file)
    end subroutine test_initialize_error

    !> Values a choice setting does not accept are errors of the init file
    !> (panel 2026-11-04); the settings keep their defaults, the accepted
    !> assignments take effect.
    subroutine test_rejected_choice(error)
        type(error_type), allocatable, intent(out) :: error

        call load_initialization_file(rejecting_configure)
        call check(error, initialization_file_had_error, "init-file-had-error")
        if (allocated(error)) return
        call check(error, last_message, "An error occurred while loading the init file: message_log_maximum: 500 is not "// &
                   "a value of this option; a number of lines is `message_log_maximum%lines = 500'")
        if (allocated(error)) return
        call check(error, show_parenthesis_style%choice == SHOW_PARENTHESIS_STYLE_PARENTHESIS, "show_parenthesis_style kept")
        if (allocated(error)) return
        call check(error, shell_indent_for_fi%other_value(), "%columns = 0")
        if (allocated(error)) return
        call check(error, message_log_maximum%lines == 1000, "message_log_maximum kept")
        if (allocated(error)) return
        call check(error, shell_indent_after_if%choice == SHELL_MINUS, "accepted value")
        shell_indent_after_if = SHELL_PLUS
        if (allocated(user_initialization_file)) deallocate (user_initialization_file)
    end subroutine test_rejected_choice

    !> *scratch* starts empty (no initial-scratch-message: there is no
    !> Lisp to evaluate) and in Fundamental mode (initial-major-mode is
    !> not lisp-interaction-mode), as the project's design decisions say.
    subroutine test_scratch(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        type(value_type) :: mode, name
        type(status_type) :: vst

        call execute_on_pty(achar(24)//achar(3), st)
        call check(error, st%ok(), "fortmacs_main")
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
        call check(error, st%ok(), "fortmacs_main")
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
        type(handle_type) :: a, b, temporary
        character(len=:), allocatable :: directory
        character(len=256) :: names(2)

        call initialize_fileio()
        directory = file_name_as_directory(file_name_as_directory(fortmacs_wd)//"build/test-emacs-files")
        call create_directory_internal(directory, st)
        call get_or_make_buffer(" *emacs-test-file*", temporary, st)
        call set_buffer(temporary, st)
        call insert("A", st)
        call write_region(directory//"a.txt", .false., st)
        call write_region(directory//"b.txt", .false., st)
        call close_buffer(temporary, st)
        names(1) = directory//"a.txt"
        names(2) = directory//"b.txt"
        call execute_on_pty(achar(24)//achar(3), st, argv(names), lines=20_int32)
        call check(error, st%ok(), "fortmacs_main: "//format_error(st))
        if (allocated(error)) return
        a = get_file_buffer(directory//"a.txt")
        b = get_file_buffer(directory//"b.txt")
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
end module test_fortmacs_mod
