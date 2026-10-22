! emacs_fm.f90 -- starting and running the editor.
! Ported from GNU Emacs 31.1 src/emacs.c (main) with the terminal setup,
! options, init file and file arguments of lisp/startup.el and
! lisp/faces.el (tty-run-terminal-initialization).
!
! Copyright (C) 1985-1987, 1993-1995, 1997-1999, 2001-2026 Free Software
! Foundation, Inc.
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

!> `emacs_main` sets up the *scratch* buffer, the key bindings and the
!> terminal, runs the init file's `configure` (unless -q), visits the
!> files named on the command line, then runs the command loop until
!> C-x C-c, redisplaying whenever it waits for input.  `run` is the
!> program around it.
module emacs_fm
    use, intrinsic :: iso_fortran_env, only: int32, int64, output_unit, error_unit
    use error_fm, only: status_type, set_error, format_error, FM_GENERIC_ERROR
    use version_fm, only: version_string
    use handle_fm, only: handle_type
    use buffer_fm, only: buffers, cur, get_buffer_create, set_buffer
    use editfns_fm, only: goto_char, point_min
    use cmds_fm, only: forward_line
    use fileio_fm, only: init_fileio, emacs_wd, file_name_as_directory, file_name_absolute_p, &
                         file_exists_p, file_executable_p, file_newer_than_file_p
    use files_fm, only: find_file_noselect
    use frame_fm, only: selected_frame
    use window_fm, only: free_window
    use sysdep_fm, only: install_interrupt_handler, emacs_isatty, sys_string_type, emacs_execv, emacs_strerror
    use term_fm, only: init_tty, delete_tty, tty_set_terminal_modes
    use termchar_fm, only: tty_flush
    use dispnew_fm, only: adjust_frame_glyphs
    use xdisp_fm, only: redisplay, init_frame_windows, realize_basic_faces, message
    use keyboard_fm, only: set_input_fd, command_loop, redisplay_hook
    use bindings_fm, only: init_bindings
    use term_xterm_fm, only: terminal_init_xterm
    use config_fm, only: install_prefix, running_executable, recompile, init_file_name, user_build_name
    implicit none(type, external)
    private

    public :: emacs_main, command_line, command_line_1, arg_type, init_proc, run
    public :: load_init_file

    !> One command-line argument.
    type :: arg_type
        character(len=:), allocatable :: s
    end type arg_type

    abstract interface
        !> The user's configuration: `configure` in init.f90, run once at
        !> startup the way upstream loads the init file.
        subroutine init_proc(status)
            import :: status_type
            implicit none(type, external)
            type(status_type), intent(out) :: status
        end subroutine init_proc
    end interface

    !> startup.el:init-file-had-error: whether the init file's
    !> `configure` failed at startup.
    public :: init_file_had_error
    logical, protected :: init_file_had_error = .false.

    !> A message for the echo area once the editor is up ("" for none).
    character(len=:), allocatable :: startup_message

contains

    !> keyboard.c's redisplay before waiting for input.
    subroutine do_redisplay()
        type(status_type) :: status

        call redisplay(selected_frame, status)
    end subroutine do_redisplay

    !> Whether terminal type `term` gets term/xterm.el's setup, as
    !> faces.el:tty-run-terminal-initialization finds term/xterm.el for
    !> "xterm*" (and term/screen.el and term/tmux.el, which use it).
    logical function xterm_like(term)
        character(len=*), intent(in) :: term

        xterm_like = index(term, "xterm") == 1 .or. index(term, "screen") == 1 .or. &
                     index(term, "tmux") == 1
    end function xterm_like

    !> Whether `s` is "+" followed by digits, then optionally ":" and
    !> digits; `line` gets the first number.
    logical function line_arg_p(s, line)
        character(len=*), intent(in) :: s
        integer(int64), intent(out) :: line
        integer :: colon, stat

        line_arg_p = .false.
        line = 0
        if (len(s) < 2) return
        if (s(1:1) /= "+") return
        colon = index(s, ":")
        if (colon == 0) colon = len(s) + 1
        if (.not. digits(s(2:colon - 1))) return
        if (colon <= len(s)) then
            if (.not. digits(s(colon + 1:))) return
        end if
        read (s(2:colon - 1), *, iostat=stat) line
        line_arg_p = stat == 0
    contains
        pure logical function digits(t)
            character(len=*), intent(in) :: t

            digits = len(t) > 0 .and. verify(t, "0123456789") == 0
        end function digits
    end function line_arg_p

    !> Visit the files named in `args`, leaving the last one current:
    !> "+LINE" puts point on that line of the next file, "--" makes the
    !> rest file names, and "-file", "-find-file" or "-visit" (or with
    !> "--") name a file.  Port of the file part of
    !> startup.el:command-line-1 ("+LINE:COLUMN" ignores the column for
    !> now; other options are unknown).
    subroutine command_line_1(args, status)
        type(arg_type), intent(in) :: args(:)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: argi, dir
        integer(int64) :: line, n
        integer :: k
        logical :: just_files

        line = 0
        just_files = .false.
        k = 0
        do while (k < size(args))
            k = k + 1
            argi = args(k)%s
            if (.not. just_files .and. line_arg_p(argi, n)) then
                line = n
                cycle
            end if
            if (.not. just_files .and. len(argi) > 0) then
                if (argi == "--") then
                    just_files = .true.
                    cycle
                end if
                if (argi(1:1) == "-") then
                    ! --foo is the same as -foo.
                    if (len(argi) > 2) then
                        if (argi(1:2) == "--") argi = argi(2:)
                    end if
                    select case (argi)
                    case ("-file", "-find-file", "-visit")
                        if (k == size(args)) then
                            call set_error(status, FM_GENERIC_ERROR, "File name omitted from `"//argi//"' option")
                            return
                        end if
                        k = k + 1
                        argi = args(k)%s
                    case default
                        call set_error(status, FM_GENERIC_ERROR, "Unknown option `"//args(k)%s//"'")
                        return
                    end select
                end if
            end if
            ! process-file-arg
            dir = file_name_as_directory(emacs_wd)
            if (len(emacs_wd) == 0 .and. .not. file_name_absolute_p(argi)) then
                call message("Ignoring relative file name ("//argi//") due to nil default-directory")
                cycle
            end if
            call visit(argi, dir, line, status)
            if (.not. status%ok()) return
            line = 0
        end do
    end subroutine command_line_1

    !> Visit `name` (relative to `dir`), make its buffer current and go to
    !> line `line` if positive.
    subroutine visit(name, dir, line, status)
        character(len=*), intent(in) :: name, dir
        integer(int64), intent(in) :: line
        type(status_type), intent(out) :: status
        type(handle_type) :: buf
        integer(int64) :: shortage

        if (file_name_absolute_p(name)) then
            call find_file_noselect(name, buf, status)
        else
            call find_file_noselect(dir//name, buf, status)
        end if
        if (.not. status%ok()) return
        call set_buffer(buf, status)
        if (.not. status%ok()) return
        if (line /= 0) then
            call goto_char(point_min())
            shortage = forward_line(line - 1)
        end if
    end subroutine visit

    !> Run the editor on terminal type `terminal_type` at file descriptor
    !> `fd` until the user leaves, visiting the files in `args` first.
    !> Port of the terminal path of emacs.c:main (init_display, then the
    !> file part of startup.el:command-line, then Frecursive_edit).
    subroutine emacs_main(terminal_type, fd, status, args, configure)
        character(len=*), intent(in) :: terminal_type
        integer(int32), intent(in) :: fd
        type(status_type), intent(out) :: status
        type(arg_type), intent(in), optional :: args(:)
        procedure(init_proc), optional :: configure
        type(handle_type) :: scratch
        type(status_type) :: st
        type(arg_type), allocatable :: rest(:)
        logical :: init

        call init_fileio()
        call init_bindings()
        call get_buffer_create("*scratch*", scratch, status)
        if (.not. status%ok()) return
        call set_buffer(scratch, status)
        if (.not. status%ok()) return
        ! buffer.c:init_buffer: *scratch* starts in the working directory.
        if (len(emacs_wd) > 0) buffers(cur)%directory = file_name_as_directory(emacs_wd)

        ! A fresh frame (emacs_main may run more than once in tests).
        call free_window(selected_frame%root_window)
        selected_frame%garbaged = .true.
        call init_tty(selected_frame%tty, terminal_type, fd, status)
        if (.not. status%ok()) return
        if (xterm_like(terminal_type)) call terminal_init_xterm()
        call set_input_fd(fd)
        call install_interrupt_handler()
        call adjust_frame_glyphs(selected_frame)
        call init_frame_windows(selected_frame)
        call realize_basic_faces(selected_frame)
        call tty_set_terminal_modes(selected_frame%tty)

        ! startup.el:command-line: the options, then the init file, then
        ! command-line-1 for the rest.  Errors are shown once the editor
        ! is up.
        if (present(args)) then
            call command_line(args, rest, init)
        else
            allocate (rest(0))
            init = .true.
        end if
        ! A reminder from `run`; errors from what follows replace it.
        if (allocated(startup_message)) then
            if (len(startup_message) > 0) call message(startup_message)
            startup_message = ""
        end if
        init_file_had_error = .false.
        if (init .and. present(configure)) call load_init_file(configure)
        call command_line_1(rest, st)
        if (.not. st%ok()) call message(format_error(st))

        redisplay_hook => do_redisplay
        call command_loop(st)
        redisplay_hook => null()

        call delete_tty(selected_frame%tty, status)
        call tty_flush(selected_frame%tty, st)
        call set_input_fd(-1_int32)
    end subroutine emacs_main

    !> Run the init file's `configure`.  An error is shown in the echo area
    !> and sets `init_file_had_error`; startup goes on.  Port of
    !> startup.el:startup--load-user-init-file (the init file is compiled
    !> in, so loading it is calling it).
    subroutine load_init_file(configure)
        procedure(init_proc) :: configure
        type(status_type) :: status

        init_file_had_error = .false.
        call configure(status)
        if (.not. status%ok()) then
            init_file_had_error = .true.
            call message("An error occurred while loading the init file: "//format_error(status))
        end if
    end subroutine load_init_file

    !> Handle the options that come before the files: -q (--no-init-file)
    !> and -Q (--quick) turn off the init file (`init` false); `rest` gets
    !> the arguments from the first other one on, for `command_line_1`.
    !> Port of the option loop of startup.el:command-line.
    subroutine command_line(args, rest, init)
        type(arg_type), intent(in) :: args(:)
        type(arg_type), allocatable, intent(out) :: rest(:)
        logical, intent(out) :: init
        character(len=:), allocatable :: argi
        integer :: k

        init = .true.
        k = 0
        options: do while (k < size(args))
            argi = args(k + 1)%s
            ! --foo is the same as -foo.
            if (len(argi) > 2) then
                if (argi(1:2) == "--") argi = argi(2:)
            end if
            select case (argi)
            case ("-q", "-no-init-file", "-Q", "-quick")
                init = .false.
            case default
                exit options
            end select
            k = k + 1
        end do options
        rest = args(k + 1:)
    end subroutine command_line

    !> The program: handle --version, --help and --recompile, check for a
    !> terminal, then run the editor with the user's `configure`.
    !> `exit_code` is the process's exit status (the main program stops
    !> with it).  An installed fortmacs runs the user's build instead, if
    !> there is an up-to-date one; a user build passes `prefix`, the
    !> installation it was built against.  Port of the startup part of
    !> emacs.c:main.
    subroutine run(configure, exit_code, prefix)
        procedure(init_proc) :: configure
        integer, intent(out) :: exit_code
        character(len=*), intent(in), optional :: prefix
        character(len=*), parameter :: usage = &
                                       "Usage: fortmacs [--version | --help | --recompile] [-q] [+LINE] [FILE]..."
        character(len=:), allocatable :: term, install, reminder
        type(arg_type), allocatable :: args(:)
        type(status_type) :: status
        integer :: nargs, arglen, stat, k
        logical :: installed

        exit_code = 0
        nargs = command_argument_count()
        allocate (args(nargs))
        do k = 1, nargs
            call get_command_argument(k, length=arglen)
            allocate (character(len=arglen) :: args(k)%s)
            call get_command_argument(k, value=args(k)%s)
        end do

        ! Which installation this is: a user build knows; an installed
        ! fortmacs finds out from where it is.
        if (present(prefix)) then
            install = prefix
            installed = .true.
        else
            call install_prefix(running_executable(), install, installed)
        end if

        if (nargs > 0) then
            if (args(1)%s == "--recompile" .or. args(1)%s == "-recompile") then
                call do_recompile(installed, install, exit_code)
                return
            end if
        end if

        reminder = ""
        if (.not. present(prefix) .and. installed) call run_user_build(args, reminder)
        if (present(prefix)) then
            if (file_newer_than_file_p(init_file_name(), running_executable())) then
                reminder = init_file_name()//" has changed; run fortmacs --recompile to use it"
            end if
        end if

        if (nargs > 0) then
            select case (args(1)%s)
            case ("--version", "-version")
                write (output_unit, "(a)") version_string()
                if (present(prefix)) write (output_unit, "(a)") "With the configuration in "//init_file_name()
                return
            case ("--help", "-help")
                write (output_unit, "(a)") usage
                write (output_unit, "(a)") "Edit the FILEs (or *scratch*) in the terminal; C-x C-s saves, C-x C-c quits."
                write (output_unit, "(a)") "+LINE puts point on line LINE of the next FILE."
                write (output_unit, "(a)") "-q (--no-init-file) skips the configure of your init.f90."
                write (output_unit, "(a)") "--recompile builds "//init_file_name()//" into your own fortmacs."
                return
            case default
                ! Everything else is for startup.el:command-line.
            end select
        end if

        ! emacs.c:main: a terminal is needed on standard input.
        if (.not. emacs_isatty(0_int32)) then
            write (error_unit, "(a)") "fortmacs: standard input is not a tty"
            exit_code = 1
            return
        end if
        call get_environment_variable("TERM", length=arglen, status=stat)
        if (stat /= 0 .or. arglen == 0) then
            write (error_unit, "(a)") "fortmacs: please set the environment variable TERM"
            exit_code = 1
            return
        end if
        allocate (character(len=arglen) :: term)
        call get_environment_variable("TERM", term)
        startup_message = reminder
        call emacs_main(term, 0_int32, status, args, configure)
        if (.not. status%ok()) then
            write (error_unit, "(a)") "fortmacs: "//format_error(status)
            exit_code = 1
        end if
    end subroutine run

    !> `fortmacs --recompile`: build the init file into the user's own
    !> fortmacs, reporting on standard output and error.
    subroutine do_recompile(installed, install, exit_code)
        logical, intent(in) :: installed
        character(len=*), intent(in) :: install
        integer, intent(out) :: exit_code
        character(len=:), allocatable :: log
        type(status_type) :: status

        exit_code = 1
        if (.not. installed) then
            write (error_unit, "(a)") "fortmacs: --recompile needs an installed Fortmacs "// &
                "(fpm install puts PREFIX/bin/fortmacs next to PREFIX/lib/libfortmacs.a)"
            return
        end if
        call init_fileio()
        call recompile(install, log, status)
        if (len(log) > 0) write (error_unit, "(a)", advance="no") log
        if (.not. status%ok()) then
            write (error_unit, "(a)") "fortmacs: "//format_error(status)
            return
        end if
        write (output_unit, "(a)") "fortmacs: built "//user_build_name()
        exit_code = 0
    end subroutine do_recompile

    !> Replace this process with the user's build, if there is one and it
    !> is newer than this fortmacs; otherwise `reminder` says what to do.
    !> Returns only if the build is not run.
    subroutine run_user_build(args, reminder)
        type(arg_type), intent(in) :: args(:)
        character(len=:), allocatable, intent(inout) :: reminder
        type(sys_string_type), allocatable :: argv(:)
        character(len=:), allocatable :: build, init
        integer(int32) :: err
        integer :: k

        build = user_build_name()
        init = init_file_name()
        if (.not. file_executable_p(build)) then
            if (file_exists_p(init)) reminder = init//" is not built yet; run fortmacs --recompile to use it"
            return
        end if
        if (file_newer_than_file_p(running_executable(), build)) then
            reminder = "Fortmacs was updated since "//init//" was built; run fortmacs --recompile"
            return
        end if
        allocate (argv(size(args) + 1))
        argv(1)%s = build
        do k = 1, size(args)
            argv(k + 1)%s = args(k)%s
        end do
        err = emacs_execv(build, argv)
        reminder = "Can't run "//build//": "//emacs_strerror(err)
    end subroutine run_user_build

end module emacs_fm
