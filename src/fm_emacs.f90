! fm_emacs.f90 -- starting and running the editor.
! Ported from GNU Emacs 31.1 src/emacs.c (main) with the terminal setup and
! file arguments of lisp/startup.el and lisp/faces.el
! (tty-run-terminal-initialization).
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
!> terminal, visits the files named on the command line, then runs the
!> command loop until C-x C-c, redisplaying whenever it waits for input.
module fm_emacs
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use fm_error, only: status_type, set_error, format_error, FM_GENERIC_ERROR
    use fm_handle, only: handle_type
    use fm_buffer, only: buffers, cur, get_buffer_create, set_buffer
    use fm_editfns, only: goto_char, point_min
    use fm_cmds, only: forward_line
    use fm_fileio, only: init_fileio, emacs_wd, file_name_as_directory, file_name_absolute_p
    use fm_files, only: find_file_noselect
    use fm_frame, only: frame_type
    use fm_window, only: free_window
    use fm_term, only: init_tty, delete_tty, tty_set_terminal_modes
    use fm_termchar, only: tty_flush
    use fm_dispnew, only: adjust_frame_glyphs
    use fm_xdisp, only: redisplay, init_frame_windows, realize_basic_faces, message
    use fm_keyboard, only: set_input_fd, command_loop, redisplay_hook
    use fm_bindings, only: init_bindings
    use fm_term_xterm, only: terminal_init_xterm
    implicit none(type, external)
    private

    public :: emacs_main, command_line_1, arg_type

    !> One command-line argument.
    type :: arg_type
        character(len=:), allocatable :: s
    end type arg_type

    !> The terminal frame (frame.c:selected_frame).
    type(frame_type), target :: selected_frame

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
    subroutine emacs_main(terminal_type, fd, status, args)
        character(len=*), intent(in) :: terminal_type
        integer(int32), intent(in) :: fd
        type(status_type), intent(out) :: status
        type(arg_type), intent(in), optional :: args(:)
        type(handle_type) :: scratch
        type(status_type) :: st

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
        call adjust_frame_glyphs(selected_frame)
        call init_frame_windows(selected_frame)
        call realize_basic_faces(selected_frame)
        call tty_set_terminal_modes(selected_frame%tty)

        ! An error in the arguments is shown once the editor is up.
        if (present(args)) then
            call command_line_1(args, st)
            if (.not. st%ok()) call message(format_error(st))
        end if

        redisplay_hook => do_redisplay
        call command_loop(st)
        redisplay_hook => null()

        call delete_tty(selected_frame%tty, status)
        call tty_flush(selected_frame%tty, st)
        call set_input_fd(-1_int32)
    end subroutine emacs_main

end module fm_emacs
