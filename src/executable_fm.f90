! executable_fm.f90 -- base functionality for executable interpreter scripts.
! Ported from GNU Emacs 31.1 lisp/progmodes/executable.el.
!
! Copyright (C) 1994-2026 Free Software Foundation, Inc.
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

!> Setting the magic number (#!) of interpreter scripts and making
!> them executable when they are saved.  It also has files.el's
!> `executable_find' (files_fm cannot use callproc_fm, which uses it).
!>
!> Deferred: `executable_interpret' (C-c C-x; it needs rule specifications
!> in `compilation_error_regexp_names', so `executable_error_regexp_table'
!> is deferred with it) and `executable_command_find_posix_p' (needs
!> `data_directory').
module executable_fm
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use error_fm, only: status_type, set_error, format_message, format_error, FM_GENERIC_ERROR
    use buffer_fm, only: buffers, cur, current_buffer
    use editfns_fm, only: point, point_min, point_max, goto_char, buffer_substring, widen, narrow_to_region
    use search_fm, only: looking_at, string_match, match_beginning, match_end, replace_match, match_data, set_match_data
    use insdel_fm, only: insert
    use fileio_fm, only: is_file_name_absolute, file_name_nondirectory, is_file_executable, set_file_modes, &
                         file_modes, default_file_modes
    use callproc_fm, only: locate_program
    use files_fm, only: after_save_hook
    use subr_fm, only: read_y_or_n
    use minibuf_fm, only: read_string
    use xdisp_fm, only: message
    use font_core_fm, only: font_lock_keyword_type, font_lock_highlight_type, FONT_LOCK_OVERRIDE_YES
    use font_lock_fm, only: font_lock_keyword, font_lock_highlight
    use commands_fm, only: commands
    use keyboard_fm, only: this_command
    use lread_fm, only: sym
    use data_fm, only: value_eq
    implicit none(type, external)
    private

    public :: executable_find, executable_chmod_function, executable_set_magic, executable_self_display_command, &
              executable_make_buffer_file_executable_if_script_p, syms_of_executable

    !> Values of `executable_query'.
    integer, parameter, public :: EXECUTABLE_QUERY_NO = 0, EXECUTABLE_QUERY_FUNCTION = 1, EXECUTABLE_QUERY_ALWAYS = 2

    !> The value of `executable_chmod' that is nil.
    integer, parameter, public :: EXECUTABLE_CHMOD_NONE = -huge(0)

    !> True means offer to add a magic number to a file.
    !> This takes effect when you switch to certain major modes,
    !> including Shell-script mode (`sh_mode').
    !> When you type \[executable_set_magic], it always offers to add or
    !> update the magic number.  Port of executable.el executable-insert.
    logical, public :: executable_insert = .true.

    !> If true (not `EXECUTABLE_QUERY_NO'), ask user before changing an
    !> existing magic number.  When this is `EXECUTABLE_QUERY_FUNCTION',
    !> only ask when called non-interactively.  Port of executable.el
    !> executable-query.
    integer, public :: executable_query = EXECUTABLE_QUERY_FUNCTION

    !> On files with this kind of name no magic is inserted or changed.
    !> Port of executable.el executable-magicless-file-regexp.
    character(len=:), allocatable, public :: executable_magicless_file_regexp

    !> Interpreter magic number prefix inserted when there was no magic number.
    !> Use of `executable_prefix_env' is preferable to this option.  Port of
    !> executable.el executable-prefix.
    character(len=:), allocatable, public :: executable_prefix

    !> If true, use "/usr/bin/env" in interpreter magic number.
    !> If this variable is true, the interpreter magic number inserted
    !> by `executable_set_magic' will be "#!/usr/bin/env INTERPRETER",
    !> otherwise it will be "#!/path/to/INTERPRETER".  Port of
    !> executable.el executable-prefix-env.
    logical, public :: executable_prefix_env = .false.

    !> After saving, if the file is not executable, set this mode.
    !> This mode passed to `set_file_modes' is taken absolutely when negative, or
    !> relative to the files existing modes.  Do nothing if this is
    !> `EXECUTABLE_CHMOD_NONE'.
    !> Typical values are 73 (+x) or -493 (rwxr-xr-x).  Port of
    !> executable.el executable-chmod.
    integer, public :: executable_chmod = 73

    !> Command you use with argument `-n+2' to make text files self-display.
    !> Note that the like of `more' doesn't work too well under Emacs \[shell].
    !> Port of executable.el executable-self-display (obsolete since 25.1).
    character(len=:), allocatable, public :: executable_self_display

    !> Rules for highlighting executable scripts' magic number.
    !> This can be included in `font_lock_keywords' by modes that call `executable'.
    !> Port of executable.el executable-font-lock-keywords.
    type(font_lock_keyword_type), allocatable, public :: executable_font_lock_keywords(:)

contains

    !> Search for COMMAND in `exec_path' and return the absolute file name.
    !> Return "" if COMMAND is not found anywhere in `exec_path'.  Port of
    !> files.el:executable-find (without REMOTE; `exec_path' is PATH).
    function executable_find(command) result(path)
        character(len=*), intent(in) :: command
        character(len=:), allocatable :: path
        character(len=:), allocatable :: dir

        dir = "/"

        if (cur /= 0) then
            if (allocated(buffers(cur)%directory)) dir = buffers(cur)%directory
        end if

        call locate_program(command, dir, path)
    end function executable_find

    !> This gets called after saving a file to assure that it be executable.
    !> You can set the absolute or relative mode in variable `executable_chmod' for
    !> non-executable files.  Port of executable.el:executable-chmod.
    subroutine executable_chmod_function(status)
        type(status_type), intent(out) :: status

        status = status_type()
        if (executable_chmod == EXECUTABLE_CHMOD_NONE) return
        if (cur == 0) return
        if (.not. allocated(buffers(cur)%filename)) return
        if (is_file_executable(buffers(cur)%filename)) return

        if (executable_chmod < 0) then
            call set_file_modes(buffers(cur)%filename, int(-executable_chmod, int32), status)
        else
            call set_file_modes(buffers(cur)%filename, ior(int(executable_chmod, int32), &
                                                           file_modes(buffers(cur)%filename)), status)
        end if
    end subroutine executable_chmod_function

    !> Set this buffer's interpreter to INTERPRETER with optional ARGUMENT.
    !> The variables `executable_magicless_file_regexp', `executable_prefix_env',
    !> `executable_insert', `executable_query' and `executable_chmod' control
    !> when and how magic numbers are inserted or replaced and scripts made
    !> executable.  The interpreter's file name is returned in RESULT.
    !> Port of executable.el:executable-set-magic.
    recursive subroutine executable_set_magic(interpreter_in, result, status, argument_in, no_query_flag, insert_flag)
        character(len=*), intent(in) :: interpreter_in
        character(len=:), allocatable, intent(out) :: result
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: argument_in
        logical, intent(in), optional :: no_query_flag, insert_flag
        character(len=:), allocatable :: interpreter, argument, current
        integer(int64) :: here, idx, mb1, me1
        integer(int64), allocatable :: saved(:)
        logical :: noquery, insert_it, skip, ok, matched

        noquery = .false.
        if (present(no_query_flag)) noquery = no_query_flag
        insert_it = .false.
        if (present(insert_flag)) insert_it = insert_flag
        status = status_type()

        if (is_file_name_absolute(interpreter_in)) then
            interpreter = interpreter_in
        else
            interpreter = executable_find(interpreter_in)

            if (len(interpreter) == 0) then
                call set_error(status, FM_GENERIC_ERROR, "Interpreter "//interpreter_in//" not recognized")
                return
            end if
        end if

        result = interpreter
        idx = string_match("\`/:", interpreter, status)

        if (idx > 0) then
            argument = interpreter(3:)
        else
            argument = interpreter
        end if

        if (present(argument_in)) then
            if (len(argument_in) > 0) argument = argument//" "//argument_in
        end if

        ! For backward compatibility, allow `executable_prefix_env' to be
        ! overridden by custom `executable_prefix'.
        idx = string_match("#!\([ "//achar(9)//"]*/usr/bin/env[ "//achar(9)//"]*\)?$", executable_prefix, status)

        if (idx > 0) then
            if (executable_prefix_env) argument = "/usr/bin/env "//file_name_nondirectory(argument)
        else
            argument = executable_prefix(3:)//argument
        end if

        skip = buffers(cur)%read_only

        if (.not. skip .and. allocated(buffers(cur)%filename)) then
            skip = string_match(executable_magicless_file_regexp, buffers(cur)%filename, status) > 0
        end if

        if (.not. skip) skip = .not. (insert_it .or. executable_insert)
        if (.not. skip) skip = point_min() > 1
        if (skip) return
        here = point()
        call goto_char(point_min())
        call after_save_hook%add(executable_chmod_function, local=.true.)
        matched = looking_at("#![ "//achar(9)//"]*\(.*\)$", status)

        if (matched) then
            mb1 = match_beginning(1, status)
            me1 = match_end(1, status)
            call goto_char(mb1)
            ! If the line ends in a space,
            ! don't offer to change it.
            call buffer_substring(me1 - 1, me1, current, status)
            ok = current /= " "

            if (ok) then
                call buffer_substring(point(), me1, current, status)
                ok = argument /= current .or. len(argument) /= len(current)
            end if

            if (ok) then
                if (executable_query /= EXECUTABLE_QUERY_NO .and. .not. noquery) then
                    ! (save-match-data (save-window-excursion ...)): make
                    ! buffer visible before question.
                    saved = match_data()
                    call read_y_or_n(format_message("Replace magic number by `#!")//argument//format_message("'? "), &
                                     ok, status)
                    call set_match_data(saved)

                    if (.not. status%ok()) then
                        call goto_char(here)
                        return
                    end if
                end if

                if (ok) then
                    call replace_match(argument, status, fixedcase=.true., literal=.true., subexp=1)
                    call message(format_message("Magic number changed to `#!")//argument//format_message("'"))
                end if
            end if
        else
            call insert("#!"//argument//achar(10), status)
            call message(format_message("Magic number changed to `#!")//argument//format_message("'"))
        end if

        call goto_char(here)
    end subroutine executable_set_magic

    ! `executable_set_magic' as a command.
    recursive subroutine cmd_executable_set_magic(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: name, arg, result

        call read_string("Name or file name of interpreter: ", name, status)
        if (.not. status%ok()) return
        call read_string("Argument for "//name//": ", arg, status)
        if (.not. status%ok()) return
        call executable_set_magic(name, result, status, arg, executable_query == EXECUTABLE_QUERY_FUNCTION, .true.)
    end subroutine cmd_executable_set_magic

    !> Turn a text file into a self-displaying Un*x command.
    !> The magic number of such a command displays all lines but itself.
    !> Port of executable.el:executable-self-display (obsolete since 25.1).
    recursive subroutine executable_self_display_command(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: result

        if (value_eq(this_command, sym("executable_self_display"))) this_command = sym("executable_set_magic")
        call executable_set_magic(executable_self_display, result, status, "-n+2")
    end subroutine executable_self_display_command

    !> Make file executable according to umask if not already executable.
    !> If file already has any execute bits set at all, do not change existing
    !> file modes.  Port of
    !> executable.el:executable-make-buffer-file-executable-if-script-p.
    subroutine executable_make_buffer_file_executable_if_script_p(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: head
        integer(int64) :: begv, zv
        integer(int32) :: current_mode, add_mode
        type(status_type) :: st

        status = status_type()
        if (cur == 0) return
        if (buffers(cur)%text%z - 1 < 2) return
        begv = point_min()
        zv = point_max()
        call widen()
        call buffer_substring(point_min(), point_min() + 2, head, st)
        call narrow_to_region(begv, zv, st)
        if (.not. allocated(head)) return
        if (head /= "#!") return

        if (.not. allocated(buffers(cur)%filename)) then
            call message("Unable to make file executable: Wrong type argument: stringp, nil")
            return
        end if

        current_mode = file_modes(buffers(cur)%filename)

        if (current_mode < 0) then
            call message("Unable to make file executable: Wrong type argument: integerp, nil")
            return
        end if

        add_mode = iand(int(o'111', int32), default_file_modes())
        if (iand(int(o'111', int32), current_mode) /= 0 .or. add_mode == 0) return
        call set_file_modes(buffers(cur)%filename, ior(current_mode, add_mode), st)
        if (.not. st%ok()) call message("Unable to make file executable: "//format_error(st))
    end subroutine executable_make_buffer_file_executable_if_script_p

    !> Give the variables their defaults and register the commands.
    !> Not a port.
    subroutine syms_of_executable()
        type(font_lock_highlight_type) :: h(1)

        if (.not. allocated(executable_magicless_file_regexp)) executable_magicless_file_regexp = &
            "/[Mm]akefile$\|/\.\(z?profile\|bash_profile\|z?login\|bash_login\|z?logout\|bash_logout\|.+shrc\|" &
            //"esrc\|rcrc\|[kz]shenv\)$"
        if (.not. allocated(executable_prefix)) executable_prefix = "#!"
        if (.not. allocated(executable_self_display)) executable_self_display = "tail"

        if (.not. allocated(executable_font_lock_keywords)) then
            allocate (executable_font_lock_keywords(1))
            h(1) = font_lock_highlight(1, "font_lock_keyword_face", override=FONT_LOCK_OVERRIDE_YES)
            executable_font_lock_keywords(1) = font_lock_keyword("\`#!.*/\([^ "//achar(9)//achar(10)//"]+\)", &
                                                                 highlights=h)
        end if

        commands%executable_set_magic%run => cmd_executable_set_magic
        commands%executable_self_display%run => executable_self_display_command
    end subroutine syms_of_executable
end module executable_fm
