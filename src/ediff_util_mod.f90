! ediff_util_mod.f90 -- the Ediff session: setup, motion, copying, quitting.
! Ported from GNU Emacs 31.1 lisp/vc/ediff-util.el (in part: comparing
! two and three buffers on a terminal frame).
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

!> An Ediff session: `set_up_ediff' makes the control buffer
!> *Ediff Control Panel* in Ediff mode, runs diff (ediff_diff_mod), puts
!> an overlay on each difference region and arranges the windows
!> (ediff_wind_mod); the control buffer's keys then move between the
!> differences (n, p, j, ga, gb), copy them between the buffers (a, b
!> or ab, ba, ..., restored with ra, rb, ...), recenter (C-l), toggle
!> the split (|), the highlighting (h) and the help (?), recompute the
!> differences (!) and quit (q).
!>
!> The procedures that need the windows and diff modules are in the
!> submodule ediff_util_session_mod (ediff_wind_mod and ediff_diff_mod use
!> this module).
module ediff_util_mod
    use, intrinsic :: iso_fortran_env, only: int64
    use error_mod, only: status_type
    use value_mod, only: command_type
    use handle_mod, only: handle_type
    use buffer_mod, only: get_buffer, is_buffer_live, set_buffer, current_buffer
    use hooks_mod, only: hook_type
    use callint_mod, only: prefix_arg_type
    use editfns_mod, only: point_min, point_max
    use itree_mod, only: make_overlay
    implicit none(type, external)
    private

    public :: ediff_unique_buffer_name, make_ediff_bullet_proof_overlay

    !> Select the control window after window setup.
    !> .true. for compatibility.  Custom `ediff_window_setup_function'
    !> implementations may want to set it to .false. to fully control
    !> window setup.  Port of ediff-util.el
    !> ediff-select-control-window-on-setup.
    logical, public :: ediff_select_control_window_on_setup = .true.

    interface
        !> Set up an Ediff session for BUFFER_A, BUFFER_B and (with a live
        !> BUFFER_C) BUFFER_C, whose contents diff reads from FILE_A, FILE_B
        !> and FILE_C: make the control buffer in Ediff mode, compute the
        !> difference regions, set up the windows and run STARTUP_HOOKS and
        !> `ediff_startup_hook'.  JOB_NAME (a command, as commands%ediff_files) is
        !> the kind of job.  CONTROL_BUFFER is the control buffer.  Port of
        !> ediff-util.el:ediff-setup (the merge jobs and MERGE-BUFFER-FILE
        !> are deferred; SETUP-PARAMETERS is JOB_NAME).
        recursive module subroutine set_up_ediff(buffer_a, file_a, buffer_b, file_b, buffer_c, file_c, job_name, &
                                                 control_buffer, status, startup_hooks)
            implicit none(type, external)
            type(handle_type), intent(in) :: buffer_a, buffer_b, buffer_c
            character(len=*), intent(in) :: file_a, file_b, file_c
            type(command_type), pointer, intent(in) :: job_name
            type(handle_type), intent(out) :: control_buffer
            type(status_type), intent(out) :: status
            type(hook_type), intent(inout), optional :: startup_hooks
        end subroutine set_up_ediff

        !> Set up the selected window for control buffer CONTROL_BUFFER of
        !> session K: show the help message, fit the window, mark the help
        !> text and remember the window configuration.  Port of
        !> ediff-util.el:ediff-setup-control-buffer.
        recursive module subroutine set_up_ediff_control_buffer(k, control_buffer, status)
            implicit none(type, external)
            integer, intent(in) :: k
            type(handle_type), intent(in) :: control_buffer
            type(status_type), intent(out) :: status
        end subroutine set_up_ediff_control_buffer

        !> Ediff mode controls all operations in a single Ediff session.
        !> This mode is entered through one of the following commands:
        !> `ediff', `ediff_files', `ediff_buffers', `ebuffers', `ediff3',
        !> `ediff_files3', `ediff_buffers3', `ebuffers3' and
        !> `ediff_current_file'.
        !>
        !> Commands:
        !> \{ediff_mode_map}
        !> Port of ediff-util.el:ediff-mode.
        recursive module subroutine ediff_mode(status)
            implicit none(type, external)
            type(status_type), intent(out) :: status
        end subroutine ediff_mode

        !> Recompute difference regions in buffers A, B, and C.
        !> Buffers are not synchronized with their respective files, so
        !> changes done to these buffers are not saved at this point---the
        !> user can do this later, if necessary.  Port of
        !> ediff-util.el:ediff-update-diffs (word mode and the ancestor are
        !> deferred with those jobs).
        recursive module subroutine update_ediff_diffs(status)
            implicit none(type, external)
            type(status_type), intent(out) :: status
        end subroutine update_ediff_diffs

        !> Bring the highlighted region of all buffers being compared into
        !> view.
        !> Reestablish the default window display.
        !>
        !> If optional NO_REHIGHLIGHT is true, do not rehighlight buffers.
        !> Port of ediff-util.el:ediff-recenter (no frames to raise).
        recursive module subroutine recenter_ediff(status, no_rehighlight)
            implicit none(type, external)
            type(status_type), intent(out) :: status
            logical, intent(in), optional :: no_rehighlight
        end subroutine recenter_ediff

        !> Toggle vertical/horizontal window split.
        !> Does nothing if file-A and file-B are in different frames.
        !> Port of ediff-util.el:ediff-toggle-split.
        recursive module subroutine toggle_ediff_split(status)
            implicit none(type, external)
            type(status_type), intent(out) :: status
        end subroutine toggle_ediff_split

        !> Switch between highlighting using ASCII flags and highlighting
        !> using faces.
        !> On a dumb terminal, switches between ASCII highlighting and no
        !> highlighting.  Port of ediff-util.el:ediff-toggle-hilit.
        recursive module subroutine toggle_ediff_highlighting(status)
            implicit none(type, external)
            type(status_type), intent(out) :: status
        end subroutine toggle_ediff_highlighting

        !> Toggle short/long help message.  Port of
        !> ediff-util.el:ediff-toggle-help.
        recursive module subroutine toggle_ediff_help(status)
            implicit none(type, external)
            type(status_type), intent(out) :: status
        end subroutine toggle_ediff_help

        !> Advance to the next difference.
        !> With a prefix argument ARG, go forward that many differences.
        !> Port of ediff-util.el:ediff-next-difference (the skipping of
        !> regions waits for the regexp filters and fine differences).
        recursive module subroutine next_ediff_difference(status, arg)
            implicit none(type, external)
            type(status_type), intent(out) :: status
            integer(int64), intent(in), optional :: arg
        end subroutine next_ediff_difference

        !> Go to the previous difference.
        !> With a prefix argument ARG, go back that many differences.
        !> Port of ediff-util.el:ediff-previous-difference.
        recursive module subroutine previous_ediff_difference(status, arg)
            implicit none(type, external)
            type(status_type), intent(out) :: status
            integer(int64), intent(in), optional :: arg
        end subroutine previous_ediff_difference

        !> Go to the difference specified as a prefix argument
        !> DIFFERENCE_NUMBER.
        !> If the prefix argument is negative, count differences from the
        !> end.  Port of ediff-util.el:ediff-jump-to-difference.
        recursive module subroutine jump_to_ediff_difference(difference_number, status)
            implicit none(type, external)
            integer(int64), intent(in) :: difference_number
            type(status_type), intent(out) :: status
        end subroutine jump_to_ediff_difference

        !> Go to difference closest to the point in buffer A, B, or C.
        !> The buffer depends on last command character (a, b, or c) that
        !> invoked this command; BUFFER_TYPE is that buffer.  For instance,
        !> if the command was `ga' then the point value in buffer A is used.
        !>
        !> With a prefix argument ARG, synchronize all files around the
        !> current point position in the specified buffer.  Port of
        !> ediff-util.el:ediff-jump-to-difference-at-point.
        recursive module subroutine jump_to_ediff_difference_at_point(buffer_type, arg, status)
            implicit none(type, external)
            integer, intent(in) :: buffer_type
            logical, intent(in) :: arg
            type(status_type), intent(out) :: status
        end subroutine jump_to_ediff_difference_at_point

        !> The difference of session K closest to POS (point if absent) in
        !> BUFFER_TYPE, as the user counts them (from 1).  WHICH_DIFF 1
        !> finds the difference after POS, -1 the one before; 0 the nearest.
        !> Port of ediff-util.el:ediff-diff-at-point (WHICH-DIFF `after',
        !> `before' or absent).
        recursive module subroutine ediff_diff_at_point(k, buffer_type, diff_no, status, pos, which_diff)
            implicit none(type, external)
            integer, intent(in) :: k, buffer_type
            integer, intent(out) :: diff_no
            type(status_type), intent(out) :: status
            integer(int64), intent(in), optional :: pos
            integer, intent(in), optional :: which_diff
        end subroutine ediff_diff_at_point

        !> Copy buffer-X'th difference region to buffer Y (X,Y are A, B, or
        !> C).
        !> With numerical prefix argument ARG, copy the difference specified
        !> in the arg.  With prefix `\[universal_argument]', copy all
        !> differences.  Otherwise, copy the difference given by
        !> `ediff_current_difference'.  KEYS, a 2-character string such as
        !> "ab", determines the source and the target buffers (the command's
        !> keys when it is bound to `ab', `ba', `ac', ...).  Port of
        !> ediff-util.el:ediff-diff-to-diff.
        recursive module subroutine copy_ediff_diff_to_diff(arg, keys, status)
            implicit none(type, external)
            type(prefix_arg_type), intent(in) :: arg
            character(len=*), intent(in) :: keys
            type(status_type), intent(out) :: status
        end subroutine copy_ediff_diff_to_diff

        !> Copy difference N of session K from FROM_TYPE to TO_TYPE, saving
        !> the old region unless BATCH_INVOCATION.  Port of
        !> ediff-util.el:ediff-copy-diff (REG-TO-COPY is deferred with
        !> merging).
        recursive module subroutine copy_ediff_diff(k, n, from_type, to_type, status, batch_invocation)
            implicit none(type, external)
            integer, intent(in) :: k, n, from_type, to_type
            type(status_type), intent(out) :: status
            logical, intent(in), optional :: batch_invocation
        end subroutine copy_ediff_diff

        !> Restore ARGth diff from `ediff_killed_diffs_alist'.
        !> ARG is a prefix argument.  If ARG is absent, restore the
        !> current-difference.  KEY, a character, determines the target
        !> buffer (the last key of the command: `ra', `rb').  Port of
        !> ediff-util.el:ediff-restore-diff.
        recursive module subroutine restore_ediff_diff(arg, key, status)
            implicit none(type, external)
            type(prefix_arg_type), intent(in) :: arg
            character, intent(in) :: key
            type(status_type), intent(out) :: status
        end subroutine restore_ediff_diff

        !> Finish an Ediff session and exit Ediff.
        !> Unselects the selected difference, if any, restores the read-only
        !> and modified flags of the compared file buffers, kills Ediff
        !> buffers for this session (but not buffers A, B, C).
        !>
        !> If `ediff_keep_variants' is .false., the user will be asked
        !> whether the buffers containing the variants should be removed (if
        !> they haven't been modified).  If it is .true., they will be
        !> preserved unconditionally.
        !>
        !> With prefix argument REVERSE_DEFAULT_KEEP_VARIANTS, temporarily
        !> reverse the meaning of this variable.  Port of
        !> ediff-util.el:ediff-quit.
        recursive module subroutine quit_ediff(reverse_default_keep_variants, status)
            implicit none(type, external)
            logical, intent(in) :: reverse_default_keep_variants
            type(status_type), intent(out) :: status
        end subroutine quit_ediff

        !> Perform the quit operations of session K.  Port of
        !> ediff-util.el:ediff-really-quit.
        recursive module subroutine quit_ediff_session(k, reverse_default_keep_variants, status)
            implicit none(type, external)
            integer, intent(in) :: k
            logical, intent(in) :: reverse_default_keep_variants
            type(status_type), intent(out) :: status
        end subroutine quit_ediff_session

        !> Unselect the current difference of session K and select N, or
        !> with FLAG 1 only select N, with FLAG -1 only unselect; recenter
        !> unless NO_RECENTER.  Port of
        !> ediff-util.el:ediff-unselect-and-select-difference (FLAG
        !> `select-only' or `unselect-only').
        recursive module subroutine unselect_and_select_ediff_difference(k, n, status, flag, no_recenter)
            implicit none(type, external)
            integer, intent(in) :: k, n
            type(status_type), intent(out) :: status
            integer, intent(in), optional :: flag
            logical, intent(in), optional :: no_recenter
        end subroutine unselect_and_select_ediff_difference

        !> The position where difference N (the current one if absent) of
        !> session K begins in BUFFER_TYPE, or with AT_END ends.  An error
        !> condition occurs for an invalid N or a killed buffer.  Port of
        !> ediff-util.el:ediff-get-diff-posn (AT_END for POS `end').
        recursive module subroutine get_ediff_diff_position(k, buffer_type, at_end, position, status, n)
            implicit none(type, external)
            integer, intent(in) :: k, buffer_type
            logical, intent(in) :: at_end
            integer(int64), intent(out) :: position
            type(status_type), intent(out) :: status
            integer, intent(in), optional :: n
        end subroutine get_ediff_diff_position

        !> The text of difference N of session K in BUFFER_TYPE (from START
        !> to END when given).  Port of ediff-util.el:ediff-get-region-contents.
        recursive module subroutine get_ediff_region_contents(k, n, buffer_type, text, status, start, end)
            implicit none(type, external)
            integer, intent(in) :: k, n, buffer_type
            character(len=:), allocatable, intent(out) :: text
            type(status_type), intent(out) :: status
            integer(int64), intent(in), optional :: start, end
        end subroutine get_ediff_region_contents

        !> Read a file name with PROMPT (no trailing ": "), in DEFAULT_DIR
        !> (or that of DEFAULT_FILE, or `default_directory'), DEFAULT_FILE
        !> the default.  A directory read means DEFAULT_FILE in it; with
        !> NO_DIRS a directory is a `user_error'.  Port of
        !> ediff-util.el:ediff-read-file-name (`dired_dwim_target_defaults'
        !> is deferred).
        recursive module subroutine read_ediff_file_name(prompt, default_dir, default_file, result, status, no_dirs)
            implicit none(type, external)
            character(len=*), intent(in) :: prompt, default_dir, default_file
            character(len=:), allocatable, intent(out) :: result
            type(status_type), intent(out) :: status
            logical, intent(in), optional :: no_dirs
        end subroutine read_ediff_file_name

        !> Write the text of BUFFER to a new temporary file FILE whose name
        !> starts with PREFIX ("ediff" if absent), with mode
        !> `ediff_temp_file_mode' and coding system
        !> `ediff_coding_system_for_write'.  Port of
        !> ediff-util.el:ediff-make-temp-file (GIVEN-FILE, START and END are
        !> deferred with the jobs that use them).
        recursive module subroutine create_ediff_temp_file(buffer, file, status, prefix)
            implicit none(type, external)
            type(handle_type), intent(in) :: buffer
            character(len=:), allocatable, intent(out) :: file
            type(status_type), intent(out) :: status
            character(len=*), intent(in), optional :: prefix
        end subroutine create_ediff_temp_file

        !> Make sure the current buffer (for a file) has the same contents
        !> as the file on disk, offering to save a modified buffer or to
        !> revert an obsolete one; a `user_error' if the user declines.
        !> Port of ediff-util.el:ediff-verify-file-buffer (FILE-MAGIC is
        !> deferred with remote and compressed files).
        recursive module subroutine verify_ediff_file_buffer(status)
            implicit none(type, external)
            type(status_type), intent(out) :: status
        end subroutine verify_ediff_file_buffer

        !> Like `other_buffer', but prefers visible buffers and ignores
        !> temporary or other insignificant buffers (those beginning with
        !> "^[ *]"); EXCLUDED holds the names of buffers not to return.
        !> NAME is the buffer's name.  Port of ediff-util.el:ediff-other-buffer
        !> (the buffers marked in a Buffer Menu and the mail reader modes are
        !> deferred).
        recursive module subroutine ediff_other_buffer(excluded, name)
            implicit none(type, external)
            character(len=*), intent(in) :: excluded(:)
            character(len=:), allocatable, intent(out) :: name
        end subroutine ediff_other_buffer

        !> Register the commands of ediff-util.el and add
        !> `clean_up_ediff_mess' to `ediff_quit_hook'.  Not a port.
        module subroutine init_ediff_util()
            implicit none(type, external)
        end subroutine init_ediff_util
    end interface

    public :: set_up_ediff, set_up_ediff_control_buffer, ediff_mode, update_ediff_diffs, recenter_ediff, &
              toggle_ediff_split, toggle_ediff_highlighting, toggle_ediff_help, next_ediff_difference, &
              previous_ediff_difference, jump_to_ediff_difference, jump_to_ediff_difference_at_point, ediff_diff_at_point, &
              copy_ediff_diff_to_diff, copy_ediff_diff, restore_ediff_diff, quit_ediff, quit_ediff_session, &
              unselect_and_select_ediff_difference, get_ediff_diff_position, get_ediff_region_contents, &
              read_ediff_file_name, create_ediff_temp_file, verify_ediff_file_buffer, ediff_other_buffer, init_ediff_util

contains

    !> A buffer name no buffer has: PREFIX followed by SUFFIX, or else
    !> PREFIX<2>SUFFIX, PREFIX<3>SUFFIX, ...  Port of
    !> ediff-util.el:ediff-unique-buffer-name.
    function ediff_unique_buffer_name(prefix, suffix) result(name)
        character(len=*), intent(in) :: prefix, suffix
        character(len=:), allocatable :: name
        integer :: n
        character(len=20) :: digits

        name = prefix//suffix
        if (.not. is_buffer_live(get_buffer(name))) return
        n = 2

        do
            write (digits, "(i0)") n
            name = prefix//"<"//trim(digits)//">"//suffix
            if (.not. is_buffer_live(get_buffer(name))) return
            n = n + 1
        end do
    end function ediff_unique_buffer_name

    !> An overlay in BUFFER from BEGINNING to END (the whole accessible
    !> part with WHOLE), advancing at its end and never detached, or the
    !> null handle if BUFFER is not live.  Port of
    !> ediff-util.el:ediff-make-bullet-proof-overlay (the bounds given
    !> as positions, or as (point-min) and (point-max) with WHOLE).
    recursive subroutine make_ediff_bullet_proof_overlay(beginning, end, buffer, overlay, status, whole)
        integer(int64), intent(in) :: beginning, end
        type(handle_type), intent(in) :: buffer
        type(handle_type), intent(out) :: overlay
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: whole
        type(handle_type) :: old
        type(status_type) :: st
        integer(int64) :: b, e

        status = status_type()
        if (.not. is_buffer_live(buffer)) return
        old = current_buffer()
        call set_buffer(buffer, status)
        if (.not. status%ok()) return
        b = beginning
        e = end

        if (present(whole)) then
            if (whole) then
                b = point_min()
                e = point_max()
            end if
        end if

        ! Advance front and rear of the overlay.
        call make_overlay(overlay, b, e, status, buffer=buffer, rear_advance=.true.)
        ! Make the old buffer current again, keeping the first error.
        call set_buffer(old, st)
        if (status%ok()) status = st
    end subroutine make_ediff_bullet_proof_overlay
end module ediff_util_mod
