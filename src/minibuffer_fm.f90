! minibuffer_fm.f90 -- minibuffer commands and reading file names.
! Ported from GNU Emacs 31.1 lisp/minibuffer.el.
!
! Copyright (C) 2008-2026 Free Software Foundation, Inc.
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

!> Leaving the minibuffer, its basic keys, and `read-file-name`.
!> Completion (TAB, *Completions*) is not ported yet.
module minibuffer_fm
    use, intrinsic :: iso_fortran_env, only: int64
    use error_fm, only: status_type, set_error, FM_GENERIC_ERROR
    use buffer_fm, only: buffers, cur
    use insdel_fm, only: del_range
    use keymap_fm, only: define_key, key_parse
    use lisp_fm, only: value_type
    use lread_fm, only: sym
    use callint_fm, only: defcommand
    use keyboard_fm, only: throw_exit, command_loop_level
    use minibuf_fm, only: minibufferp, minibuf_level, minibuffer_prompt_end, abort_minibuffers, &
                          minibuffer_local_map, read_from_minibuffer
    use fileio_fm, only: file_name_absolute_p, expand_file_name, substitute_in_file_name
    use files_fm, only: abbreviate_file_name
    implicit none(type, external)
    private

    public :: exit_minibuffer, delete_minibuffer_contents, read_file_name, syms_of_minibuffer

    !> Non-nil means when reading a filename start with default dir in minibuffer.
    !>
    !> When the initial minibuffer contents show a name of a file or a directory,
    !> typing RETURN without editing the initial contents is equivalent to typing
    !> the default file name.
    !>
    !> If this variable is non-nil, the minibuffer contents are always
    !> initially non-empty, and typing RETURN without editing will fetch the
    !> default name, if one is provided.
    !>
    !> If this variable is nil, the minibuffer often starts out empty.
    !> Port of minibuffer.el insert-default-directory.
    logical, public :: insert_default_directory = .true.

contains

    !> Finish the minibuffer input (RET).  Port of
    !> minibuffer.el:exit-minibuffer.
    subroutine exit_minibuffer(status)
        type(status_type), intent(out) :: status

        if (minibufferp()) then
            if (command_loop_level /= minibuf_level) then
                call set_error(status, FM_GENERIC_ERROR, "Not in most nested command loop")
                return
            end if
        end if
        call throw_exit(.false.)
    end subroutine exit_minibuffer

    !> Delete the text after the prompt.  Port of
    !> minibuffer.el:delete-minibuffer-contents.
    subroutine delete_minibuffer_contents(status)
        type(status_type), intent(out) :: status

        call del_range(minibuffer_prompt_end(), buffers(cur)%zv, status)
    end subroutine delete_minibuffer_contents

    !> Read a file name in the minibuffer, starting with directory `dir`
    !> (default: default-directory) abbreviated as the input to edit.
    !> Unchanged input gives `default_filename` (default: the buffer's
    !> file); the result goes through substitute-in-file-name, so typing
    !> "~/x" or "/x" after the directory means just that.  Port of
    !> minibuffer.el:read-file-name-default without completion (TAB),
    !> MUSTMATCH or PREDICATE.
    recursive subroutine read_file_name(prompt, result, status, dir, default_filename, initial)
        character(len=*), intent(in) :: prompt
        character(len=:), allocatable, intent(out) :: result
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: dir, default_filename, initial
        character(len=:), allocatable :: d, default, insdef, val
        logical :: have_default

        if (present(dir)) then
            d = dir
        else if (allocated(buffers(cur)%directory)) then
            d = buffers(cur)%directory
        else
            d = "~/"
        end if
        if (.not. file_name_absolute_p(d)) then
            call expand_file_name(d, val, status)
            if (.not. status%ok()) return
            d = val
        end if
        have_default = .true.
        if (present(default_filename)) then
            default = default_filename
        else if (.not. present(initial)) then
            have_default = allocated(buffers(cur)%filename)
            if (have_default) default = buffers(cur)%filename
        else if (len(initial) == 0) then
            default = d
        else
            call expand_file_name(initial, default, status, d)
            if (.not. status%ok()) return
        end if
        d = abbreviate_file_name(d)
        if (have_default) default = abbreviate_file_name(default)

        insdef = ""
        if (insert_default_directory) insdef = d
        if (present(initial)) insdef = insdef//initial
        if (have_default) then
            call read_from_minibuffer(prompt, val, status, initial=insdef, history="file-name-history", &
                                      default=default)
        else
            call read_from_minibuffer(prompt, val, status, initial=insdef, history="file-name-history")
        end if
        if (.not. status%ok()) return
        ! completing-read: empty input gives the default.
        if (len(val) == 0 .and. have_default) val = default
        ! Unchanged input means the default file.
        if (have_default .and. val == insdef .and. len(val) == len(insdef)) val = default
        result = substitute_in_file_name(val)
    end subroutine read_file_name

    subroutine cmd_exit_minibuffer(status)
        type(status_type), intent(out) :: status

        call exit_minibuffer(status)
    end subroutine cmd_exit_minibuffer

    subroutine cmd_abort_minibuffers(status)
        type(status_type), intent(out) :: status

        call abort_minibuffers(status)
    end subroutine cmd_abort_minibuffers

    !> Register the commands and minibuffer.el's minibuffer-local-map keys.
    subroutine syms_of_minibuffer()
        call defcommand("exit-minibuffer", cmd_exit_minibuffer)
        call defcommand("abort-minibuffers", cmd_abort_minibuffers)
        call bind("C-g", "abort-minibuffers")
        call bind("C-j", "exit-minibuffer")
        call bind("RET", "exit-minibuffer")
    contains
        subroutine bind(keys, command)
            character(len=*), intent(in) :: keys, command
            type(value_type), allocatable :: seq(:)
            type(status_type) :: status

            call key_parse(keys, seq, status)
            if (status%ok()) call define_key(minibuffer_local_map, seq, sym(command), status)
        end subroutine bind
    end subroutine syms_of_minibuffer

end module minibuffer_fm
