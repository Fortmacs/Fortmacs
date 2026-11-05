! simple_merge_mode_mod.f90 -- minor mode to resolve diff3 conflicts.
! Ported from GNU Emacs 31.1 lisp/vc/smerge-mode.el.
!
! Copyright (C) 1999-2026 Free Software Foundation, Inc.
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

!> Smerge mode: a lightweight alternative to emerge/ediff for the
!> conflict markers diff3 (and Git, Mercurial, ...) leave in a file:
!> moving between conflicts (`next_smerge_conflict', `previous_smerge_conflict'), keeping one
!> or all versions, removing one, combining, swapping and extending
!> conflicts, resolving them automatically (`resolve_smerge', with diff
!> and patch), comparing two versions in a Diff buffer, the faces and
!> font-lock keywords, the C-c ^ prefix map, `smerge_repeat_map' and
!> `start_smerge_session' (which VC's find-file hooks call).
!>
!> Deferred: see docs/status/simple_merge_mode_mod.md.
module simple_merge_mode_mod
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use error_mod, only: status_type, set_error, format_error, FM_GENERIC_ERROR, FM_USER_ERROR, FM_WRONG_TYPE_ARGUMENT
    use handle_mod, only: handle_type, null_handle, has_handle
    use value_mod, only: value_type, make_keymap_ref, make_buffer_ref, make_list, vtag, xfixnum, V_INTEGER
    use execute_interactively_mod, only: current_prefix_arg, prefix_numeric_value
    use commands_mod, only: commands
    use data_mod, only: append_locals_clearer, string_to_number
    use hooks_mod, only: hook_type
    use buffer_mod, only: buffers, current, current_buffer, set_buffer, is_buffer_live, buffer_serial_live, get_buffer, &
                          get_or_make_buffer, generate_new_buffer_name, close_buffer, erase_buffer, buffer_modified_tick
    use editing_functions_mod, only: point, point_min, point_max, goto_char, is_eob, char_after, line_end_position, &
                                     line_beginning_position, delete_region, delete_and_extract_region, insert_buffer_substring, &
                                     buffer_substring_no_properties, narrow_to_region, widen, is_buffer_narrowed, mark_marker
    use insert_delete_mod, only: insert
    use editing_commands_mod, only: forward_line
    use marker_mod, only: copy_marker_at, marker_position, destroy_marker
    use search_mod, only: looking_at, re_search_forward, re_search_backward, match_beginning, match_end, match_data, &
                          set_match_data, SEARCH_NOERROR
    use basic_subprograms_mod, only: match_string
    use basic_subprograms_mod, only: change_group_type, prepare_change_group, activate_change_group, accept_change_group, &
                                     cancel_change_group
    use syntax_mod, only: parse_state_type, syntax_ppss, forward_comment
    use newcomment_mod, only: is_comment_only, comment_search_forward, comment_forward, comment_enter_backward
    use simple_mod, only: count_lines, is_use_region
    use file_input_output_mod, only: write_region, insert_file_contents, file_name_nondirectory
    use files_mod, only: create_temp_file, delete_file, file_relative_name
    use execute_process_mod, only: execute_process, execute_process_region
    use coding_mod, only: coding_system_for_read, buffer_file_coding_system
    use interval_tree_mod, only: make_overlay, move_overlay, destroy_overlay, overlay_start, overlay_end, overlay_buffer, &
                                 overlay_put, overlay_advance, overlays_at, overlays_in
    use just_in_time_lock_mod, only: jit_lock_refontify
    use keymap_mod, only: make_sparse_keymap, keymap_set, set_repeat_map, current_global_map
    use help_mod, only: register_keymap_name
    use window_mod, only: display_buffer, display_action_parameters_type
    use display_engine_mod, only: message
    use display_update_mod, only: ding
    use paragraphs_mod, only: paragraph_separate
    use faces_mod, only: named_face_type, display_when, face_attrs, FACE_CLASS_COLOR, FACE_BACKGROUND_LIGHT, &
                         FACE_BACKGROUND_DARK
    use font_core_mod, only: font_lock_keyword_type, font_lock_highlight_type, font_lock_mode_enabled, &
                             FONT_LOCK_OVERRIDE_REPLACE, FONT_LOCK_OVERRIDE_KEEP, FONT_LOCK_OVERRIDE_PREPEND
    use font_lock_mod, only: font_lock_keyword, font_lock_highlight, font_lock_add_keywords, font_lock_fontify_region, &
                             FONT_LOCK_ADD_END
    use easy_minor_mode_mod, only: easy_mmode_next, easy_mmode_prev
    use modes_mod, only: minor_modes
    use diff_mod, only: diff_switches, diff_command, diff_check_labels, diff_use_labels
    use diff_mode_mod, only: diff_mode
    implicit none(type, external)
    private

    public :: init_smerge_mode, smerge_mode, start_smerge_session, match_smerge_conflict, find_smerge_conflict, &
              check_smerge, next_smerge_conflict, previous_smerge_conflict, keep_smerge_all, keep_smerge_base, keep_smerge_lower, &
              keep_smerge_upper, keep_smerge_current, delete_smerge_current, combine_smerge_with_next, &
              auto_combine_smerge, resolve_smerge, resolve_smerge_all, swap_smerge, extend_smerge, &
              diff_smerge_base_upper, diff_smerge_base_lower, diff_smerge_upper_lower, auto_leave_smerge, &
              keep_smerge_n, get_smerge_current, smerge_resolve_procedure, smerge_resolve_function_set_local, &
              smerge_conflict_style

    abstract interface
        !> A `smerge_resolve_function': resolve the conflict the match data
        !> describe (as `match_smerge_conflict' sets it), or fail with an
        !> error condition.  SAFE true means to apply only safe heuristics.
        !> Not a port.
        recursive subroutine smerge_resolve_procedure(status, safe)
            import :: status_type
            implicit none(type, external)
            type(status_type), intent(out) :: status
            logical, intent(in), optional :: safe
        end subroutine smerge_resolve_procedure
    end interface

    !> Buffer name to use for displaying diffs.  Port of smerge-mode.el
    !> smerge-diff-buffer-name.
    character(len=:), allocatable, public :: smerge_diff_buffer_name

    !> An array of strings specifying switches to be passed to diff.
    !> Used in `diff_smerge_base_upper' and related functions.  Port of
    !> smerge-mode.el smerge-diff-switches.
    character(len=64), allocatable, public :: smerge_diff_switches(:)

    !> True means to leave `smerge_mode' when the last conflict is resolved.
    !> Port of smerge-mode.el smerge-auto-leave.
    logical, public :: smerge_auto_leave = .true.

    !> Prefix for `smerge_mode' commands, as a key description ("ESC",
    !> "C-c ^", or "" for none).  It takes effect when Smerge mode is first
    !> turned on, as upstream's does when smerge-mode.el is loaded.
    !> Port of smerge-mode.el smerge-command-prefix.
    character(len=:), allocatable, public :: smerge_command_prefix

    !> Max number of lines between conflicts that should be combined.
    !> Port of smerge-mode.el smerge-auto-combine-max-separation.
    integer, public :: smerge_auto_combine_max_separation = 2

    !> Mode-specific merge function.
    !> The function is called with or without its SAFE argument (true if
    !> the resolution function should only apply safe heuristics) and with
    !> the match data set according to `match_smerge_conflict'.  A buffer
    !> gives it its own value with `smerge_resolve_function_set_local'.
    !> Port of smerge-mode.el smerge-resolve-function.
    procedure(smerge_resolve_procedure), pointer, public :: smerge_resolve_function => null()

    !> Run after `smerge_mode' is turned on or off.  Port of smerge-mode.el
    !> smerge-mode-hook.
    type(hook_type), target, public :: smerge_mode_hook = hook_type(name="smerge_mode_hook")

    !> The keymap of the Smerge commands, under `smerge_command_prefix' in
    !> `smerge_mode_map'.  Port of smerge-mode.el smerge-basic-map.
    type(handle_type), public :: smerge_basic_map

    !> Keymap to repeat SMerge commands.
    !> Used in `repeat_mode'.  Port of smerge-mode.el smerge-repeat-map.
    type(handle_type), public :: smerge_repeat_map

    !> Keymap for `smerge_mode'.  Port of smerge-mode.el smerge-mode-map.
    type(handle_type), public :: smerge_mode_map

    !> Face for the `upper' version of a conflict.
    type(named_face_type), target, public :: smerge_upper
    !> Face for the `lower' version of a conflict.
    type(named_face_type), target, public :: smerge_lower
    !> Face for the base code.
    type(named_face_type), target, public :: smerge_base
    !> Face for the conflict markers.
    type(named_face_type), target, public :: smerge_markers
    !> Face used for char-based changes shown by `smerge_refine'.
    type(named_face_type), target, public :: smerge_refined_changed
    !> Face used for removed characters shown by `smerge_refine'.
    type(named_face_type), target, public :: smerge_refined_removed
    !> Face used for added characters shown by `smerge_refine'.
    type(named_face_type), target, public :: smerge_refined_added

    !> The face of the `upper' version in the font-lock keywords.  Port of
    !> smerge-mode.el smerge-upper-face.
    character(len=64), public :: smerge_upper_face = "smerge_upper"
    !> The face of the `lower' version in the font-lock keywords.  Port of
    !> smerge-mode.el smerge-lower-face.
    character(len=64), public :: smerge_lower_face = "smerge_lower"
    !> The face of the base version in the font-lock keywords.  Port of
    !> smerge-mode.el smerge-base-face.
    character(len=64), public :: smerge_base_face = "smerge_base"
    !> The face of the conflict markers in the font-lock keywords.  Port of
    !> smerge-mode.el smerge-markers-face.
    character(len=64), public :: smerge_markers_face = "smerge_markers"

    !> Values of `smerge_conflict_style': undecided, or the style diff3
    !> writes with -E or with -A.
    integer, parameter, public :: SMERGE_CONFLICT_STYLE_NONE = 0, SMERGE_CONFLICT_STYLE_DIFF3_E = 1, &
                                  SMERGE_CONFLICT_STYLE_DIFF3_A = 2

    character(len=*), parameter :: LF = achar(10)

    !> The line that begins a conflict.  Port of smerge-mode.el
    !> smerge-begin-re.
    character(len=*), parameter, public :: smerge_begin_regexp = "^<<<<<<< \(.*\)"//LF
    !> The line that ends a conflict.  Port of smerge-mode.el smerge-end-re.
    character(len=*), parameter, public :: smerge_end_regexp = "^>>>>>>> \(.*\)"//LF
    !> The line that begins the base version.  Port of smerge-mode.el
    !> smerge-base-re.
    character(len=*), parameter, public :: smerge_base_regexp = "^||||||| \(.*\)"//LF
    !> The line that begins the lower version.  Port of smerge-mode.el
    !> smerge-lower-re.
    character(len=*), parameter, public :: smerge_lower_regexp = "^======="//LF
    !> The conflict marker lines, as alternatives that `smerge_mode' puts
    !> in front of `paragraph_separate'.  Port of smerge-mode.el
    !> smerge-parsep-re.
    character(len=*), parameter, public :: smerge_parsep_regexp = smerge_begin_regexp//"\|"//smerge_end_regexp//"\|"// &
                                           smerge_base_regexp//"\|"//smerge_lower_regexp//"\|"
    !> The names of the parts of a conflict, by subexpression of the match
    !> data (0 to 3).  Port of smerge-mode.el smerge-match-names.
    character(len=8), parameter, public :: smerge_match_names(0:3) = [character(len=8) :: "conflict", "upper", &
                                                                                                                    "base", "lower"]

    !> A diff hunk header of the normal format (as diff writes without
    !> options): smerge-apply-resolution-patch's regexp.
    character(len=*), parameter :: normal_hunk_header_regexp = &
                                   "\([0-9]+\)\(?:,\([0-9]+\)\)?\([acd]\)\([0-9]+\)\(?:,\([0-9]+\)\)?$"

    !> A buffer's Smerge variables (buffer-local upstream), by buffer
    !> serial number: `smerge_conflict_style', smerge-check-cache (the
    !> match data of the conflict at `check_point' while the buffer's
    !> modification count was `check_tick') and the buffer's own
    !> `smerge_resolve_function'.
    type :: smerge_local_type
        integer(int64) :: serial = 0
        logical :: set = .false.
        integer :: conflict_style = SMERGE_CONFLICT_STYLE_NONE
        logical :: has_check_cache = .false.
        integer(int64) :: check_point = 0, check_tick = 0
        integer(int64), allocatable :: check_data(:)
        procedure(smerge_resolve_procedure), pointer, nopass :: resolve_function => null()
    end type smerge_local_type

    type(smerge_local_type), allocatable :: locals(:)

    !> The overlays `find_smerge_conflict' puts on conflicts (upstream's
    !> overlays with the property `smerge' set to `conflict'), in all
    !> buffers.
    type(handle_type), allocatable :: conflict_overlays(:)

    !> Whether `smerge_mode_map' has its `smerge_command_prefix' binding.
    logical :: mode_map_ready = .false.

contains

    ! ------------------------------------------------------------------
    ! Buffer-local variables
    ! ------------------------------------------------------------------

    !> The index of the current buffer's entry in `locals' (0 if none).
    !> Not a port.
    integer function local_index() result(k)
        k = 0
        if (current == 0) return
        if (.not. allocated(locals)) return
        k = findloc(locals%serial, buffers(current)%serial, mask=locals%set, dim=1)
    end function local_index

    !> Find or make the current buffer's entry in `locals', reusing the
    !> entry of a closed buffer; K is its index (0 without a current
    !> buffer).  Not a port.
    subroutine make_local_entry(k)
        integer, intent(out) :: k
        type(smerge_local_type) :: entry

        k = local_index()
        if (k > 0 .or. current == 0) return
        if (.not. allocated(locals)) allocate (locals(0))

        ! Reuse the entry of a dead buffer.
        do k = 1, size(locals)
            if (.not. locals(k)%set) exit
            if (.not. buffer_serial_live(locals(k)%serial)) exit
        end do

        if (k > size(locals)) locals = [locals, entry]
        locals(k) = smerge_local_type()
        locals(k)%set = .true.
        locals(k)%serial = buffers(current)%serial
    end subroutine make_local_entry

    !> kill-all-local-variables: Smerge's buffer-local variables.
    !> Not a port.
    subroutine clear_smerge_locals()
        integer :: k

        k = local_index()
        if (k > 0) locals(k) = smerge_local_type()
    end subroutine clear_smerge_locals

    !> Keep track of which style of conflict is in use in the current
    !> buffer: SMERGE_CONFLICT_STYLE_NONE if the style is undecided, or
    !> else SMERGE_CONFLICT_STYLE_DIFF3_E or SMERGE_CONFLICT_STYLE_DIFF3_A.
    !> Port of smerge-mode.el smerge-conflict-style.
    integer function smerge_conflict_style() result(style)
        integer :: k

        style = SMERGE_CONFLICT_STYLE_NONE
        k = local_index()
        if (k > 0) style = locals(k)%conflict_style
    end function smerge_conflict_style

    !> Give the current buffer FN as its own `smerge_resolve_function'
    !> (its buffer-local value).  Not a port.
    subroutine smerge_resolve_function_set_local(fn)
        procedure(smerge_resolve_procedure) :: fn
        integer :: k

        call make_local_entry(k)
        if (k > 0) locals(k)%resolve_function => fn
    end subroutine smerge_resolve_function_set_local

    !> The default `smerge_resolve_function'.  Port of smerge-mode.el
    !> smerge-resolve-function (its default value).
    recursive subroutine resolve_unknown(status, safe)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: safe

        if (present(safe)) continue
        call set_error(status, FM_USER_ERROR, "Don't know how to resolve")
    end subroutine resolve_unknown

    !> Whether strings A and B are equal, trailing blanks included (Lisp's
    !> `equal' on strings; Fortran's == pads the shorter one with blanks).
    !> Not a port.
    pure logical function is_same(a, b)
        character(len=*), intent(in) :: a, b

        is_same = len(a) == len(b)
        if (is_same) is_same = a == b
    end function is_same

    ! ------------------------------------------------------------------
    ! Matching conflicts
    ! ------------------------------------------------------------------

    !> Get info about the conflict.  Puts the info in the match data.
    !> The submatches contain:
    !>  0:  the whole conflict.
    !>  1:  upper version of the code.
    !>  2:  base version of the code.
    !>  3:  lower version of the code.
    !> A `user_error' error condition occurs if not inside a conflict.
    !> Port of smerge-mode.el:smerge-match-conflict.
    recursive subroutine match_smerge_conflict(status)
        type(status_type), intent(out) :: status
        integer(int64) :: orig, start, upper_start, upper_end, lower_start, lower_end, end, base_start, base_end
        integer(int64) :: found, shortage, here
        character(len=:), allocatable :: filename, visited
        type(status_type) :: st
        integer :: k

        status = status_type()
        orig = point()

        search: block
            shortage = forward_line(1_int64)
            call re_search_backward(smerge_begin_regexp, found, status, noerror=SEARCH_NOERROR)
            if (.not. status%ok()) exit search

            if (found == 0) then
                call not_in_conflict()
                exit search
            end if

            start = match_beginning(0, st)
            upper_start = match_end(0, st)
            filename = match_string(1)
            call re_search_forward(smerge_end_regexp, found, status, noerror=SEARCH_NOERROR)
            if (.not. status%ok()) exit search

            if (found == 0) then
                call not_in_conflict()
                exit search
            end if

            ! Point is not within the conflict we found, so this conflict
            ! is not ours.
            if (match_end(0, st) < orig) then
                call not_in_conflict()
                exit search
            end if

            lower_end = match_beginning(0, st)
            end = match_end(0, st)
            call re_search_backward(smerge_lower_regexp, found, status, bound=start, noerror=SEARCH_NOERROR)
            if (.not. status%ok()) exit search

            if (found == 0) then
                call not_in_conflict()
                exit search
            end if

            upper_end = match_beginning(0, st)
            lower_start = match_end(0, st)
            base_start = 0
            base_end = 0
            ! Handle the various conflict styles.
            here = point()
            call goto_char(upper_start)
            call re_search_forward(smerge_begin_regexp, found, status, bound=end, noerror=SEARCH_NOERROR)
            call goto_char(here)
            if (.not. status%ok()) exit search

            if (found /= 0) then
                ! There's a nested conflict and we're after the beginning
                ! of the outer one but before the beginning of the inner one.
                ! Of course, maybe this is not a nested conflict but in that
                ! case it can only be something nastier that we don't know how
                ! to handle, so may as well arbitrarily decide to treat it as
                ! a nested conflict.  --Stef
                call set_error(status, FM_GENERIC_ERROR, "There is a nested conflict")
                exit search
            end if

            call re_search_backward(smerge_base_regexp, found, status, bound=start, noerror=SEARCH_NOERROR)
            if (.not. status%ok()) exit search
            visited = ""
            if (allocated(buffers(current)%filename)) visited = buffers(current)%filename

            if (found /= 0) then
                ! A 3-parts conflict.
                call make_local_entry(k)
                if (k > 0) locals(k)%conflict_style = SMERGE_CONFLICT_STYLE_DIFF3_A
                base_end = upper_end
                upper_end = match_beginning(0, st)
                base_start = match_end(0, st)
            else if (is_same(filename, file_name_nondirectory(visited))) then
                ! A 2-parts conflict.
                call make_local_entry(k)
                if (k > 0) locals(k)%conflict_style = SMERGE_CONFLICT_STYLE_DIFF3_E
            else if (base_start == 0 .and. (smerge_conflict_style() == SMERGE_CONFLICT_STYLE_DIFF3_A .or. &
                                            is_same(filename, "ANCESTOR") .or. is_revision_number(filename))) then
                ! A same-diff conflict.
                base_start = upper_start
                base_end = upper_end
                upper_start = lower_start
                upper_end = lower_end
            end if

            if (base_start /= 0) then
                call set_match_data([start, end, upper_start, upper_end, base_start, base_end, lower_start, lower_end, &
                                     base_start - 1, base_start, lower_start - 1, lower_start])
            else
                call set_match_data([start, end, upper_start, upper_end, 0_int64, 0_int64, lower_start, lower_end, &
                                     0_int64, 0_int64, lower_start - 1, lower_start])
            end if
        end block search

        call goto_char(orig)

    contains

        ! The search-failed handler: not inside a conflict.
        subroutine not_in_conflict()
            call set_error(status, FM_USER_ERROR, "Point not in conflict region")
        end subroutine not_in_conflict

        ! (string-match "\\`[.0-9]+\\'" S)
        logical function is_revision_number(s)
            character(len=*), intent(in) :: s

            is_revision_number = len(s) > 0
            if (is_revision_number) is_revision_number = verify(s, ".0123456789") == 0
        end function is_revision_number
    end subroutine match_smerge_conflict

    !> POS gets the beginning of part N of the conflict at point (0 for the
    !> whole conflict, as for `match_smerge_conflict'), or 0 if there is no
    !> conflict at point or it has no part N.  The match data are those of
    !> the conflict, cached while point and the buffer stay the same.
    !> Port of smerge-mode.el:smerge-check.
    recursive subroutine check_smerge(n, pos)
        integer, intent(in) :: n
        integer(int64), intent(out) :: pos
        type(status_type) :: st
        integer(int64) :: tick
        integer :: k

        pos = 0
        call buffer_modified_tick(current_buffer(), tick, st)
        call make_local_entry(k)
        if (k == 0) return

        if (.not. (locals(k)%has_check_cache .and. locals(k)%check_point == point() .and. locals(k)%check_tick == tick)) then
            call match_smerge_conflict(st)
            if (.not. st%ok()) return
            k = local_index()
            locals(k)%check_data = match_data()
            locals(k)%has_check_cache = .true.
            locals(k)%check_point = point()
            locals(k)%check_tick = tick
        end if

        if (size(locals(k)%check_data) > 2*n) pos = locals(k)%check_data(2*n + 1)
    end subroutine check_smerge

    !> An error unless part N of the conflict (see `match_smerge_conflict')
    !> matched.  Port of smerge-mode.el:smerge-ensure-match.
    subroutine ensure_smerge_match(n, status)
        integer, intent(in) :: n
        type(status_type), intent(out) :: status
        type(status_type) :: st

        status = status_type()
        if (match_end(n, st) == 0) call set_error(status, FM_GENERIC_ERROR, "No `"//trim(smerge_match_names(n))//"'")
    end subroutine ensure_smerge_match

    !> Turn `smerge_mode' off when `smerge_auto_leave' is true and there
    !> are no conflict markers left.  Port of smerge-mode.el:smerge-auto-leave
    !> (the undo entry that turns the mode on again is deferred).
    recursive subroutine auto_leave_smerge(status)
        type(status_type), intent(out) :: status
        integer(int64) :: here, found

        status = status_type()
        if (.not. smerge_auto_leave) return
        here = point()
        call goto_char(point_min())
        call re_search_forward(smerge_begin_regexp, found, status, noerror=SEARCH_NOERROR)
        call goto_char(here)
        if (.not. status%ok()) return
        if (found /= 0) return
        call minor_modes%smerge_mode%disable(status)
    end subroutine auto_leave_smerge

    !> The conflict overlay at POS, if any (else the null handle).  Port of
    !> smerge-mode.el:smerge-conflict-overlay.
    function get_smerge_conflict_overlay(pos) result(conflict)
        integer(int64), intent(in) :: pos
        type(handle_type) :: conflict
        type(handle_type), allocatable :: ols(:)
        integer :: k

        conflict = null_handle()
        call overlays_at(pos, ols)

        do k = 1, size(ols)
            if (.not. has_handle(conflict_overlays, ols(k))) cycle
            if (overlay_end(ols(k)) > pos) conflict = ols(k)
        end do
    end function get_smerge_conflict_overlay

    !> Forget the conflict overlays that were deleted (evaporated, or their
    !> buffer closed), releasing their handles.  Not a port: upstream
    !> relies on GC.
    subroutine prune_conflict_overlays()
        logical, allocatable :: live(:)
        integer :: k

        if (.not. allocated(conflict_overlays)) allocate (conflict_overlays(0))
        allocate (live(size(conflict_overlays)))

        do k = 1, size(conflict_overlays)
            live(k) = is_buffer_live(overlay_buffer(conflict_overlays(k)))
            if (.not. live(k)) call destroy_overlay(conflict_overlays(k))
        end do

        conflict_overlays = pack(conflict_overlays, live)
    end subroutine prune_conflict_overlays

    !> Find and match a conflict region.  Intended as a font-lock MATCHER.
    !> The submatches are the same as in `match_smerge_conflict'.
    !> FOUND is true if a match is found between point and LIMIT (0 for
    !> no limit).  Point is moved to the end of the conflict.  Port of
    !> smerge-mode.el:smerge-find-conflict.
    recursive subroutine find_smerge_conflict(limit, found, status)
        integer(int64), intent(in) :: limit
        logical, intent(out) :: found
        type(status_type), intent(out) :: status
        type(handle_type) :: conflict
        type(status_type) :: st
        integer(int64) :: pos, f, ostart, oend

        status = status_type()
        found = .false.
        pos = point()
        call prune_conflict_overlays()

        ! First check to see if point is already inside a conflict, using
        ! the conflict overlays.
        do while (.not. found)
            conflict = get_smerge_conflict_overlay(pos)
            if (conflict%index == 0) exit
            ! Check the overlay's validity and kill it if it's out of date.
            ostart = overlay_start(conflict)
            oend = overlay_end(conflict)
            call goto_char(ostart)
            call match_smerge_conflict(st)

            if (st%ok()) then
                call goto_char(match_end(0, st))
                ! (Matching backward is an error.)
                found = point() > pos
            end if

            if (.not. found) then
                call remove_smerge_properties(ostart, oend)
                call goto_char(pos)
            end if
        end do

        ! If we're not already inside a conflict, look for the next conflict
        ! and add/update its overlay.
        do while (.not. found)
            if (limit > 0) then
                call re_search_forward(smerge_begin_regexp, f, status, bound=limit, noerror=SEARCH_NOERROR)
            else
                call re_search_forward(smerge_begin_regexp, f, status, noerror=SEARCH_NOERROR)
            end if

            if (.not. status%ok()) return
            if (f == 0) exit
            call match_smerge_conflict(st)
            if (.not. st%ok()) cycle
            call goto_char(match_end(0, st))
            conflict = get_smerge_conflict_overlay(point() - 1)

            if (conflict%index /= 0) then
                ! Update its location, just in case it got messed up.
                call move_overlay(conflict, match_beginning(0, st), match_end(0, st), st)
            else
                call make_overlay(conflict, match_beginning(0, st), match_end(0, st), st, front_advance=.true., &
                                  rear_advance=.false.)

                if (st%ok()) then
                    call overlay_put(conflict, "evaporate", value_type(.true.), st)
                    conflict_overlays = [conflict_overlays, conflict]
                end if
            end if

            found = .true.
        end do
    end subroutine find_smerge_conflict

    !> Remove Smerge's overlays from BEGINNING to END (moving or splitting
    !> those that extend past it, as remove-overlays does) and mark the
    !> text for fontification again.  Port of
    !> smerge-mode.el:smerge-remove-props, with subr.el:remove-overlays for
    !> the conflict overlays (the refinement overlays are deferred).
    recursive subroutine remove_smerge_properties(beginning, end)
        integer(int64), intent(in) :: beginning, end
        type(handle_type), allocatable :: ols(:)
        type(handle_type) :: copy
        type(status_type) :: st
        integer(int64) :: b, e, ostart, oend
        logical :: front_advance, rear_advance
        integer :: k

        b = min(beginning, end)
        e = max(beginning, end)
        call prune_conflict_overlays()
        call overlays_in(b, e, ols)

        do k = 1, size(ols)
            if (.not. has_handle(conflict_overlays, ols(k))) cycle
            ! Either push this overlay outside beginning...end or split it to
            ! exclude beginning...end or delete it entirely (if it is contained
            ! in beginning...end).
            ostart = overlay_start(ols(k))
            oend = overlay_end(ols(k))

            if (ostart < b) then
                if (oend > e) then
                    ! (copy-overlay O): the conflict overlays have only the
                    ! `evaporate' property.
                    call overlay_advance(ols(k), front_advance, rear_advance)
                    call make_overlay(copy, ostart, b, st, front_advance=front_advance, rear_advance=rear_advance)

                    if (st%ok()) then
                        call overlay_put(copy, "evaporate", value_type(.true.), st)
                        conflict_overlays = [conflict_overlays, copy]
                    end if

                    call move_overlay(ols(k), e, oend, st)
                else
                    call move_overlay(ols(k), ostart, b, st)
                end if
            else if (oend > e) then
                call move_overlay(ols(k), e, oend, st)
            else
                call destroy_overlay(ols(k))
            end if
        end do

        call prune_conflict_overlays()
        ! Now that we use overlays rather than text-properties, this function
        ! does not cause refontification any more.  (with-silent-modifications
        ! (remove-text-properties beginning end '(fontified nil))) marks the text
        ! for fontification, as `jit_lock_refontify' does.
        call jit_lock_refontify(st, b, e)
    end subroutine remove_smerge_properties

    ! ------------------------------------------------------------------
    ! Keeping and removing versions
    ! ------------------------------------------------------------------

    !> Keep part N of the conflict the match data describe and remove the
    !> rest of it.  Port of smerge-mode.el:smerge-keep-n.
    recursive subroutine keep_smerge_n(n, status)
        integer, intent(in) :: n
        type(status_type), intent(out) :: status
        integer(int64) :: mb0, me0, mbn, men
        type(status_type) :: st

        mb0 = match_beginning(0, st)
        me0 = match_end(0, st)
        mbn = match_beginning(n, st)
        men = match_end(n, st)
        call remove_smerge_properties(mb0, me0)
        ! We used to use replace-match, but that did not preserve markers so well.
        call delete_region(men, me0, status)
        if (.not. status%ok()) return
        call delete_region(mb0, mbn, status)
    end subroutine keep_smerge_n

    !> Concatenate all versions.  Port of smerge-mode.el:smerge-keep-all.
    recursive subroutine keep_smerge_all(status)
        type(status_type), intent(out) :: status
        integer(int64) :: mb0, me0, mb1, me1, mb2, me2, mb3, me3
        type(status_type) :: st

        call match_smerge_conflict(status)
        if (.not. status%ok()) return
        mb0 = match_beginning(0, st)
        me0 = match_end(0, st)
        mb1 = match_beginning(1, st)
        me1 = match_end(1, st)
        mb3 = match_beginning(3, st)
        me3 = match_end(3, st)
        mb2 = match_beginning(2, st)
        if (mb2 == 0) mb2 = point_max()
        me2 = match_end(2, st)
        if (me2 == 0) me2 = point_min()
        call delete_region(me3, me0, status)
        if (.not. status%ok()) return
        call delete_region(max(me2, me1), mb3, status)
        if (.not. status%ok()) return

        if (match_end(2, st) /= 0 .and. me1 /= me3) then
            call delete_region(me1, mb2, status)
            if (.not. status%ok()) return
        end if

        call delete_region(mb0, min(mb1, mb2), status)
        if (.not. status%ok()) return
        call auto_leave_smerge(status)
    end subroutine keep_smerge_all

    !> Revert to the base version.  Port of smerge-mode.el:smerge-keep-base.
    recursive subroutine keep_smerge_base(status)
        type(status_type), intent(out) :: status

        call match_smerge_conflict(status)
        if (.not. status%ok()) return
        call ensure_smerge_match(2, status)
        if (.not. status%ok()) return
        call keep_smerge_n(2, status)
        if (.not. status%ok()) return
        call auto_leave_smerge(status)
    end subroutine keep_smerge_base

    !> Keep the "lower" version of a merge conflict.
    !> In a conflict that looks like:
    !>   <<<<<<<
    !>   UUU
    !>   =======
    !>   LLL
    !>   >>>>>>>
    !> this keeps "LLL".  Port of smerge-mode.el:smerge-keep-lower.
    recursive subroutine keep_smerge_lower(status)
        type(status_type), intent(out) :: status

        call match_smerge_conflict(status)
        if (.not. status%ok()) return
        call keep_smerge_n(3, status)
        if (.not. status%ok()) return
        call auto_leave_smerge(status)
    end subroutine keep_smerge_lower

    !> Keep the "upper" version of a merge conflict.
    !> In a conflict that looks like:
    !>   <<<<<<<
    !>   UUU
    !>   =======
    !>   LLL
    !>   >>>>>>>
    !> this keeps "UUU".  Port of smerge-mode.el:smerge-keep-upper.
    recursive subroutine keep_smerge_upper(status)
        type(status_type), intent(out) :: status

        call match_smerge_conflict(status)
        if (.not. status%ok()) return
        call keep_smerge_n(1, status)
        if (.not. status%ok()) return
        call auto_leave_smerge(status)
    end subroutine keep_smerge_upper

    !> The part of the conflict the match data describe that point is in:
    !> 3 (lower), 2 (base) or 1 (upper), else 0.  Port of
    !> smerge-mode.el:smerge-get-current.
    integer function get_smerge_current() result(i)
        type(status_type) :: st
        integer(int64) :: pt

        pt = point()
        i = 3

        do while (i > 0)
            if (match_end(i, st) /= 0) then
                if (pt >= match_beginning(i, st) .and. pt <= match_end(i, st)) exit
            end if

            i = i - 1
        end do
    end function get_smerge_current

    !> Use the current (under the cursor) version.  Port of
    !> smerge-mode.el:smerge-keep-current.
    recursive subroutine keep_smerge_current(status)
        type(status_type), intent(out) :: status
        integer :: i

        call match_smerge_conflict(status)
        if (.not. status%ok()) return
        i = get_smerge_current()

        if (i <= 0) then
            call set_error(status, FM_GENERIC_ERROR, "Not inside a version")
            return
        end if

        call keep_smerge_n(i, status)
        if (.not. status%ok()) return
        call auto_leave_smerge(status)
    end subroutine keep_smerge_current

    !> Remove the current (under the cursor) version.  Port of
    !> smerge-mode.el:smerge-kill-current.
    recursive subroutine delete_smerge_current(status)
        type(status_type), intent(out) :: status
        integer :: i, n, left(3), count
        type(status_type) :: st

        call match_smerge_conflict(status)
        if (.not. status%ok()) return
        i = get_smerge_current()

        if (i <= 0) then
            call set_error(status, FM_GENERIC_ERROR, "Not inside a version")
            return
        end if

        ! The other versions, in order.
        count = 0

        do n = 1, 3
            if (match_end(n, st) == 0) cycle
            if (match_end(n, st) == match_end(i, st)) cycle
            count = count + 1
            left(count) = n
        end do

        if (count == 0) then
            call set_error(status, FM_WRONG_TYPE_ARGUMENT, "Wrong type argument: expected integer")
            return
        end if

        if (count > 1) then
            if (match_end(left(1), st) /= match_end(left(2), st)) then
                ! We don't know how to do that.
                call ding()
                return
            end if
        end if

        call keep_smerge_n(left(1), status)
        if (.not. status%ok()) return
        call auto_leave_smerge(status)
    end subroutine delete_smerge_current

    ! ------------------------------------------------------------------
    ! Combining, swapping and extending conflicts
    ! ------------------------------------------------------------------

    !> Combine the current conflict with the next one.  Port of
    !> smerge-mode.el:smerge-combine-with-next.
    recursive subroutine combine_smerge_with_next(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: ends(0:3), md(12)
        integer(int64), allocatable :: data(:)
        integer(int64) :: pos, found, b, e
        type(status_type) :: st
        integer :: i

        ! `auto_combine_smerge' relies on the finish position (at the
        ! beginning of the closing marker).
        call match_smerge_conflict(status)
        if (.not. status%ok()) return
        ends = null_handle()
        md = null_handle()

        do i = 0, 3
            if (match_end(i, st) /= 0) call copy_marker_at(match_end(i, st), ends(i), .true., st)
        end do

        combine: block
            call goto_char(at(ends(0)))
            call re_search_forward(smerge_begin_regexp, found, status, noerror=SEARCH_NOERROR)
            if (.not. status%ok()) exit combine

            if (found == 0) then
                call set_error(status, FM_GENERIC_ERROR, "No next conflict")
                exit combine
            end if

            call match_smerge_conflict(status)
            if (.not. status%ok()) exit combine
            data = match_data()

            do i = 1, min(size(data), 12)
                if (data(i) /= 0) call copy_marker_at(data(i), md(i), .false., st)
            end do

            ! First copy the in-between text in each alternative.
            do i = 1, 3
                if (ends(i)%index == 0) cycle
                call goto_char(at(ends(i)))
                call insert_buffer_substring(current_buffer(), status, at(ends(0)), at(md(1)))
                if (.not. status%ok()) exit combine
            end do

            call delete_region(at(ends(0)), at(md(1)), status)
            if (.not. status%ok()) exit combine

            ! Then move the second conflict's alternatives into the first.
            do i = 1, 3
                call restore_match_data()
                if (ends(i)%index == 0) cycle
                if (match_end(i, st) == 0) cycle
                b = match_beginning(i, st)
                e = match_end(i, st)
                call goto_char(at(ends(i)))
                call insert_buffer_substring(current_buffer(), status, b, e)
                if (.not. status%ok()) exit combine
            end do

            call delete_region(at(md(1)), at(md(2)), status)
        end block combine

        ! Free the markers.
        do i = 1, 12
            if (md(i)%index /= 0) call destroy_marker(md(i), st)
        end do

        do i = 0, 3
            if (ends(i)%index /= 0) call destroy_marker(ends(i), st)
        end do

    contains

        ! The position of marker M.
        integer(int64) function at(m)
            type(handle_type), intent(in) :: m
            type(status_type) :: mst

            call marker_position(m, pos, mst)
            at = pos
        end function at

        ! (set-match-data match-data): the second conflict's match data
        ! from its markers.
        subroutine restore_match_data()
            integer(int64) :: positions(12)
            integer :: j

            positions = 0

            do j = 1, 12
                if (md(j)%index /= 0) positions(j) = at(md(j))
            end do

            call set_match_data(positions(1:min(size(data), 12)))
        end subroutine restore_match_data
    end subroutine combine_smerge_with_next

    !> Automatically combine conflicts that are near each other.  Port of
    !> smerge-mode.el:smerge-auto-combine.
    recursive subroutine auto_combine_smerge(status)
        type(status_type), intent(out) :: status
        integer(int64) :: here, inner, shortage, limit
        logical :: found, next

        status = status_type()
        here = point()
        call goto_char(point_min())

        outer: do
            call find_smerge_conflict(0_int64, found, status)
            if (.not. status%ok()) exit outer
            if (.not. found) exit outer

            do
                ! 2 is 1 (default) + 1 (the begin markers).
                inner = point()
                limit = line_beginning_position(int(2 + smerge_auto_combine_max_separation, int64))
                call find_smerge_conflict(limit, next, status)
                call goto_char(inner)
                if (.not. status%ok()) exit outer
                if (.not. next) exit
                ! Go back inside the conflict.
                shortage = forward_line(-1_int64)
                call combine_smerge_with_next(status)
                if (.not. status%ok()) exit outer
                ! Move past the end of the conflict.
                shortage = forward_line(1_int64)
            end do
        end do outer

        call goto_char(here)
    end subroutine auto_combine_smerge

    !> Swap the "Upper" and the "Lower" chunks.
    !> Can be used before things like `keep_smerge_all' or `resolve_smerge'
    !> where the ordering can have some subtle influence on the result,
    !> such as preferring the spacing of the "Lower" chunk.  Port of
    !> smerge-mode.el:smerge-swap.
    recursive subroutine swap_smerge(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: txt3, txt1
        integer(int64) :: mb1, me1, mb3, me3
        type(status_type) :: st

        call match_smerge_conflict(status)
        if (.not. status%ok()) return
        mb1 = match_beginning(1, st)
        me1 = match_end(1, st)
        mb3 = match_beginning(3, st)
        me3 = match_end(3, st)
        call goto_char(mb3)
        call delete_and_extract_region(point(), me3, txt3, status)
        if (.not. status%ok()) return
        call delete_and_extract_region(mb1, me1, txt1, status)
        if (.not. status%ok()) return
        call insert(txt1, status)
        if (.not. status%ok()) return
        call goto_char(mb1)
        call insert(txt3, status)
    end subroutine swap_smerge

    !> Extend current conflict with some of the surrounding text.
    !> Point should be inside a conflict and OTHERPOS should be either a
    !> marker indicating the position until which to extend the conflict
    !> (either before or after the current conflict), or LINES, the number
    !> of lines over which to extend the conflict.  If positive, it extends
    !> over the lines following the conflict and other, it extends over the
    !> lines preceding the conflict.
    !> When used interactively, you can specify OTHERPOS either using an
    !> active region, or with a numeric prefix.  By default it uses a
    !> numeric prefix of 1.  Port of smerge-mode.el:smerge-extend.
    recursive subroutine extend_smerge(status, lines, otherpos)
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: lines
        type(handle_type), intent(in), optional :: otherpos
        type(handle_type) :: other, end_marker
        type(status_type) :: st
        character(len=:), allocatable :: text
        integer(int64) :: beginning, end, pos, me, here
        logical :: own_other, after
        integer :: i

        own_other = .false.
        other = null_handle()
        if (present(otherpos)) other = otherpos
        call match_smerge_conflict(status)

        if (.not. status%ok()) then
            if (.not. present(otherpos)) return
            ! Point is at the other end: go to OTHERPOS and use point as
            ! the other end instead.
            here = point()
            call goto_char(at(otherpos))
            call copy_marker_at(here, other, .false., st)
            own_other = .true.
            call match_smerge_conflict(status)
            if (.not. status%ok()) return
        end if

        beginning = match_beginning(0, st)
        call copy_marker_at(match_end(0, st), end_marker, .false., st)

        extend: block
            if (.not. present(otherpos)) then
                pos = 1
                if (present(lines)) pos = lines

                if (pos >= 0) then
                    call goto_char(at(end_marker))
                else
                    call goto_char(beginning)
                end if

                call copy_marker_at(line_beginning_position(pos + 1), other, .false., st)
                own_other = .true.
            end if

            end = at(end_marker)

            if (end <= at(other)) then
                call buffer_substring_no_properties(end, at(other), text, status)
            else if (at(other) <= beginning) then
                call buffer_substring_no_properties(at(other), beginning, text, status)
            else
                call set_error(status, FM_USER_ERROR, "The other end should be outside the conflict")
                exit extend
            end if

            if (.not. status%ok()) exit extend
            after = end <= at(other)

            do i = 0, 2
                if (after) then
                    me = match_end(3 - i, st)
                else
                    me = match_beginning(3 - i, st)
                end if

                if (me == 0) cycle
                call goto_char(me)
                call insert(text, status)
                if (.not. status%ok()) exit extend
            end do

            if (after) then
                call delete_region(at(end_marker), at(other), status)
            else
                call delete_region(beginning, at(other), status)
            end if
        end block extend

        call destroy_marker(end_marker, st)
        if (own_other) call destroy_marker(other, st)

    contains

        ! The position of marker M.
        integer(int64) function at(m)
            type(handle_type), intent(in) :: m
            type(status_type) :: mst

            call marker_position(m, at, mst)
        end function at
    end subroutine extend_smerge

    ! ------------------------------------------------------------------
    ! Resolving conflicts
    ! ------------------------------------------------------------------

    !> Replace the conflict with a bunch of subconflicts.
    !> BUFFER contains a plain diff between match-1 and match-3.  M2B is
    !> 0 when the conflict has no base.  Port of
    !> smerge-mode.el:smerge-apply-resolution-patch.
    recursive subroutine apply_smerge_resolution_patch(buffer, m0b, m0e, m3b_in, m3e_in, m2b, status)
        type(handle_type), intent(in) :: buffer
        integer(int64), intent(in) :: m0b, m0e, m3b_in, m3e_in, m2b
        type(status_type), intent(out) :: status
        type(handle_type) :: textbuf
        character(len=:), allocatable :: name1, name2, name3, othertext, header
        integer(int64) :: line, m3b, startline, endline, nlines, otherlines, other_start, pos, shortage, n
        integer(int32) :: op
        logical :: found
        type(status_type) :: st

        line = 1
        textbuf = current_buffer()
        call goto_char(m0b)
        call buffer_substring_no_properties(point() + 8, line_end_position(), name1, status)
        if (.not. status%ok()) return

        if (m2b /= 0) then
            call goto_char(m2b)
            shortage = forward_line(-1_int64)
            call buffer_substring_no_properties(point() + 8, line_end_position(), name2, status)
            if (.not. status%ok()) return
        end if

        call goto_char(m0e)
        shortage = forward_line(-1_int64)
        call buffer_substring_no_properties(point() + 8, line_end_position(), name3, status)
        if (.not. status%ok()) return
        call remove_smerge_properties(m0b, m0e)
        call delete_region(m3e_in, m0e, status)
        if (.not. status%ok()) return
        call delete_region(m0b, m3b_in, status)
        if (.not. status%ok()) return
        ! (Upstream then sets m3e to m3e - (m3b - m0b), which it does not use.)
        m3b = m0b
        call goto_char(m3b)
        call set_buffer(buffer, status)
        if (.not. status%ok()) return
        call goto_char(point_min())

        hunks: do while (.not. is_eob())
            if (.not. looking_at(normal_hunk_header_regexp, status)) then
                if (.not. status%ok()) exit hunks
                call buffer_substring_no_properties(point(), line_end_position(), header, st)
                call set_error(status, FM_GENERIC_ERROR, "Unexpected patch hunk header: "//header)
                exit hunks
            end if

            call char_after(op, found, match_beginning(3, st))
            call string_to_number(match_string(1), startline)
            ! No clue why this is the way it is, but line numbers seem to be
            ! off-by-one for `a' ops.
            if (op == ichar("a")) startline = startline + 1

            if (op == ichar("a")) then
                endline = startline
            else if (match_end(2, st) /= 0) then
                call string_to_number(match_string(2), endline)
                endline = endline + 1
            else
                endline = startline + 1
            end if

            nlines = endline - startline

            if (op == ichar("d")) then
                otherlines = -1
            else if (match_end(5, st) == 0) then
                otherlines = 1
            else
                call string_to_number(match_string(5), otherlines)
                call string_to_number(match_string(4), other_start)
                otherlines = otherlines - other_start + 1
            end if

            ! Skip header.
            shortage = forward_line(1_int64)
            ! Skip deleted text.
            shortage = forward_line(nlines)
            ! Skip separator.
            if (op == ichar("c")) shortage = forward_line(1_int64)

            if (otherlines < 0) then
                othertext = ""
            else
                pos = point()

                do n = 1, otherlines
                    call delete_region(point(), point() + 2, status)
                    if (.not. status%ok()) exit hunks
                    shortage = forward_line(1_int64)
                end do

                call buffer_substring_no_properties(pos, point(), othertext, status)
                if (.not. status%ok()) exit hunks
            end if

            call set_buffer(textbuf, status)
            if (.not. status%ok()) return
            shortage = forward_line(startline - line)

            if (m2b /= 0) then
                call insert("<<<<<<< "//name1//LF//othertext//"||||||| "//name2//LF//"======="//LF, status)
            else
                call insert("<<<<<<< "//name1//LF//othertext//"======="//LF, status)
            end if

            if (.not. status%ok()) return
            shortage = forward_line(nlines)
            call insert(">>>>>>> "//name3//LF, status)
            if (.not. status%ok()) return
            line = endline
            call set_buffer(buffer, status)
            if (.not. status%ok()) return
        end do hunks

        if (is_buffer_live(textbuf)) call set_buffer(textbuf, st)
    end subroutine apply_smerge_resolution_patch

    !> TEXT with every run of spaces, tabs and newlines made one space:
    !> what replacing the matches of smerge-mode.el's
    !> smerge-resolve--normalize-re ("[\n\t][ \t\n]*\\| [ \t\n]+") with " "
    !> gives.  Not a port.
    function collapse_whitespace(text) result(s)
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: s
        character(len=*), parameter :: blanks = " "//achar(9)//LF
        integer :: i, j

        s = ""
        i = 1

        do while (i <= len(text))
            if (scan(text(i:i), blanks) == 0) then
                s = s//text(i:i)
                i = i + 1
            else
                j = verify(text(i:), blanks)

                if (j == 0) then
                    i = len(text) + 1
                else
                    i = i + j - 1
                end if

                s = s//" "
            end if
        end do
    end function collapse_whitespace

    !> Extract the text within the comments that span BEGINNING..END.
    !> Port of smerge-mode.el:smerge-resolve--extract-comment.
    recursive function extract_resolve_smerge_comment(beginning, end) result(text)
        integer(int64), intent(in) :: beginning, end
        character(len=:), allocatable :: text
        character(len=:), allocatable :: comment
        integer(int64) :: here, combeg, inner, after
        type(status_type) :: st
        logical :: moved

        here = point()
        text = " "
        call goto_char(beginning)

        do while (point() < end)
            combeg = comment_search_forward(end, .true., st)
            if (combeg == 0) exit
            inner = point()
            call goto_char(combeg)
            moved = comment_forward(1_int64)
            after = point()
            call comment_enter_backward()
            call buffer_substring_no_properties(inner, point(), comment, st)
            text = text//comment//" "
            call goto_char(after)
        end do

        call goto_char(here)
        text = collapse_whitespace(text)
    end function extract_resolve_smerge_comment

    !> The text from BEGINNING to END with its whitespace normalized.
    !> Port of smerge-mode.el:smerge-resolve--normalize.
    function normalize_resolve_smerge(beginning, end) result(text)
        integer(int64), intent(in) :: beginning, end
        character(len=:), allocatable :: text
        type(status_type) :: st

        call buffer_substring_no_properties(beginning, end, text, st)
        text = collapse_whitespace(" "//text//" ")
    end function normalize_resolve_smerge

    !> Resolve the conflict at point intelligently.
    !> This relies on mode-specific knowledge and thus only works in some
    !> major modes.  Uses `smerge_resolve_function' to do the actual work.
    !> SAFE true means to try only safe heuristics.  Port of
    !> smerge-mode.el:smerge-resolve.
    recursive subroutine resolve_smerge(status, safe)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: safe
        integer(int64), allocatable :: md(:)
        integer(int64) :: m0b, m0e, m1b, m1e, m2b, m2e, m3b, m3e, nlines, inserted
        character(len=:), allocatable :: m, b, o, t1, t2, t3
        type(handle_type) :: buffer, old
        type(status_type) :: st
        logical :: is_safe, done
        integer :: choice, code

        is_safe = .false.
        if (present(safe)) is_safe = safe
        call match_smerge_conflict(status)
        if (.not. status%ok()) return
        ! FIXME: This ends up removing the refinement-highlighting when no
        ! resolution is performed.
        call remove_smerge_properties(match_beginning(0, st), match_end(0, st))
        md = match_data()
        m0b = match_beginning(0, st)
        m1b = match_beginning(1, st)
        m2b = match_beginning(2, st)
        m3b = match_beginning(3, st)
        m0e = match_end(0, st)
        m1e = match_end(1, st)
        m2e = match_end(2, st)
        m3e = match_end(3, st)
        old = current_buffer()
        call get_or_make_buffer(generate_new_buffer_name(" *smerge*"), buffer, status)
        if (.not. status%ok()) return

        resolve: block
            ! Trivial diff3 -A non-conflicts.
            if (match_end(1, st) == match_end(3, st) .and. match_beginning(1, st) == match_beginning(3, st)) then
                call keep_smerge_n(3, status)
                exit resolve
            end if

            ! Mode-specific conflict resolution.
            call resolve_with_smerge_resolve_function(done)
            if (done) exit resolve

            ! Non-conflict.
            if (m1e == m3e .and. m1b == m3b) then
                call set_match_data(md)
                call keep_smerge_n(3, status)
                exit resolve
            end if

            ! Refine a 2-way conflict using "diff -b".
            ! In case of a 3-way conflict with an empty base
            ! (i.e. 2 conflicting additions), we do the same, presuming
            ! that the 2 additions should be somehow merged rather
            ! than concatenated.
            nlines = count_lines(m3b, m3e)
            call create_temp_file("smm", m, status)
            if (.not. status%ok()) exit resolve
            call write_part(m1b, m1e, m)
            if (.not. status%ok()) exit resolve
            call create_temp_file("smo", o, status)
            if (.not. status%ok()) exit resolve
            call write_part(m3b, m3e, o)
            if (.not. status%ok()) exit resolve
            call test_refinable(done)

            if (done) then
                if (.not. status%ok()) exit resolve
                call apply_smerge_resolution_patch(buffer, m0b, m0e, m3b, m3e, m2b, status)
                exit resolve
            end if

            if (.not. status%ok()) exit resolve

            ! "Mere whitespace changes" conflicts.
            if (m2e /= 0) then
                call create_temp_file("smb", b, status)
                if (.not. status%ok()) exit resolve
                call write_part(m2b, m2e, b)
                if (.not. status%ok()) exit resolve
                call in_buffer_erase()
                ! Only minor whitespace changes made locally.
                ! BEWARE: pass "-c" 'cause the output is reused in the next test.
                call diff_into_buffer("-bc", b, m, code)
                if (.not. status%ok()) exit resolve

                if (code == 0) then
                    call set_match_data(md)
                    call keep_smerge_n(3, status)
                    exit resolve
                end if
            end if

            ! Try "diff -b BASE UPPER | patch LOWER".
            ! If the BASE is empty, this would just concatenate the two,
            ! which is rarely right.
            if (.not. is_safe .and. m2e /= 0 .and. allocated(b) .and. m2b /= m2e) then
                ! BEWARE: we're using here the patch of the previous test.
                call patch_from_buffer(o, code)
                if (.not. status%ok()) exit resolve

                if (code == 0) then
                    call replace_conflict(o)
                    exit resolve
                end if
            end if

            ! Try "diff -b BASE LOWER | patch UPPER".
            if (.not. is_safe .and. m2e /= 0 .and. allocated(b) .and. m2b /= m2e) then
                call write_part(m3b, m3e, o)
                if (.not. status%ok()) exit resolve
                call diff_into_buffer("-bc", b, o, code)
                if (.not. status%ok()) exit resolve
                call patch_from_buffer(m, code)
                if (.not. status%ok()) exit resolve

                if (code == 0) then
                    call replace_conflict(m)
                    exit resolve
                end if
            end if

            ! If the conflict is only made of comments, and one of the two
            ! changes is only rearranging spaces (e.g. reflowing text) while
            ! the other is a real change, drop the space-rearrangement.
            choice = 0

            if (m2e /= 0) then
                if (is_comment_only(m1b, m1e)) then
                    if (is_comment_only(m2b, m2e)) then
                        if (is_comment_only(m3b, m3e)) then
                            t1 = extract_resolve_smerge_comment(m1b, m1e)
                            t2 = extract_resolve_smerge_comment(m2b, m2e)
                            t3 = extract_resolve_smerge_comment(m3b, m3e)
                            choice = choose(t1, t2, t3)
                        end if
                    end if
                end if
            end if

            if (choice /= 0) then
                call set_match_data(md)
                call keep_smerge_n(choice, status)
                exit resolve
            end if

            ! Idem, when the conflict is contained within a single comment.
            if (m2e /= 0) call test_in_one_comment(choice)

            if (choice /= 0) then
                call set_match_data(md)
                call keep_smerge_n(choice, status)
                exit resolve
            end if

            call set_error(status, FM_USER_ERROR, "Don't know how to resolve")
        end block resolve

        if (is_buffer_live(buffer)) call close_buffer(buffer, st)
        if (is_buffer_live(old)) call set_buffer(old, st)
        if (allocated(m)) call delete_file(m, st)
        if (allocated(b)) call delete_file(b, st)
        if (allocated(o)) call delete_file(o, st)
        if (.not. status%ok()) return
        call auto_leave_smerge(status)

    contains

        ! The mode-specific function, in an atomic change group, its errors
        ! ignored: RESOLVED says whether it resolved the conflict.
        subroutine resolve_with_smerge_resolve_function(resolved)
            logical, intent(out) :: resolved
            type(change_group_type) :: group
            type(status_type) :: fst, cst
            procedure(smerge_resolve_procedure), pointer :: fn
            integer :: k

            fn => smerge_resolve_function
            k = local_index()

            if (k > 0) then
                if (associated(locals(k)%resolve_function)) fn => locals(k)%resolve_function
            end if

            if (.not. associated(fn)) fn => resolve_unknown
            call prepare_change_group(group)
            call activate_change_group(group)

            if (is_safe) then
                call fn(fst, is_safe)
            else
                call fn(fst)
            end if

            if (fst%ok()) then
                call accept_change_group(group)
            else
                call cancel_change_group(group, cst)
            end if

            resolved = fst%ok()
        end subroutine resolve_with_smerge_resolve_function

        ! Write the text from START to FINISH to FILE, silently.
        subroutine write_part(start, finish, file)
            integer(int64), intent(in) :: start, finish
            character(len=*), intent(in) :: file
            character(len=:), allocatable :: msg

            call write_region(file, .false., status, start, finish, msg)
        end subroutine write_part

        ! The refinement test: neither upper nor lower is empty, diff -b
        ! of them succeeds or the base is absent or empty, and the diff
        ! changes more than the whole lower part: REFINABLE.
        subroutine test_refinable(refinable)
            logical, intent(out) :: refinable
            character(len=32) :: change_text
            integer :: rc

            refinable = .false.
            if (m1b == m1e .or. m3b == m3e) return
            call diff_into_buffer("-b", o, m, rc)
            if (.not. status%ok()) return
            ! TODO: We don't know how to do the refinement if there's a
            ! non-empty ancestor and m1 and m3 aren't just plain equal.
            if (rc /= 0 .and. m2b /= 0 .and. m2b /= m2e) return
            ! Make sure there's some refinement.
            call set_buffer(buffer, status)
            if (.not. status%ok()) return
            call goto_char(point_min())
            write (change_text, '("1,", i0, "c")') nlines
            refinable = .not. looking_at(trim(change_text), status)
            call set_buffer(old, st)
        end subroutine test_refinable

        ! Erase the diff buffer.
        subroutine in_buffer_erase()
            call set_buffer(buffer, status)
            if (.not. status%ok()) return
            call erase_buffer(status)
            call set_buffer(old, st)
        end subroutine in_buffer_erase

        ! (call-process diff-command nil buffer nil SWITCHES FILE1 FILE2): the
        ! exit status, -1 if the program was cut by a signal.
        subroutine diff_into_buffer(switches, file1, file2, rc)
            character(len=*), intent(in) :: switches, file1, file2
            integer, intent(out) :: rc
            type(value_type) :: result
            type(value_type) :: args(3)

            rc = -1
            args(1) = value_type(text=switches)
            args(2) = value_type(text=file1)
            args(3) = value_type(text=file2)
            call execute_process(diff_program(), result, status, destination=make_buffer_ref(buffer), args=make_list(args))
            if (.not. status%ok()) return
            if (vtag(result) == V_INTEGER) rc = int(xfixnum(result))
        end subroutine diff_into_buffer

        ! (call-process-region (point-min) (point-max) "patch" t nil nil "-r"
        ! null-device "--no-backup-if-mismatch" "-fl" FILE) in the diff
        ! buffer: the exit status.
        subroutine patch_from_buffer(file, rc)
            character(len=*), intent(in) :: file
            integer, intent(out) :: rc
            type(value_type) :: result
            type(value_type) :: args(5)

            rc = -1
            args(1) = value_type(text="-r")
            args(2) = value_type(text="/dev/null")
            args(3) = value_type(text="--no-backup-if-mismatch")
            args(4) = value_type(text="-fl")
            args(5) = value_type(text=file)
            call set_buffer(buffer, status)
            if (.not. status%ok()) return
            call execute_process_region("patch", result, status, point_min(), point_max(), delete=.true., &
                                        args=make_list(args))
            call set_buffer(old, st)
            if (.not. status%ok()) return
            if (vtag(result) == V_INTEGER) rc = int(xfixnum(result))
        end subroutine patch_from_buffer

        ! Replace the conflict with the contents of FILE.
        subroutine replace_conflict(file)
            character(len=*), intent(in) :: file
            type(handle_type) :: begv, zv
            integer(int64) :: b, e
            logical :: narrowed

            ! (save-restriction (narrow-to-region m0b m0e) ...)
            narrowed = is_buffer_narrowed()

            if (narrowed) then
                call copy_marker_at(buffers(current)%begv, begv, .false., st)
                call copy_marker_at(buffers(current)%zv, zv, .true., st)
            end if

            call narrow_to_region(m0b, m0e, status)

            if (status%ok()) then
                call remove_smerge_properties(m0b, m0e)
                call insert_file_contents(file, .false., inserted, status, replace=.true.)
            end if

            call widen()

            if (narrowed) then
                call marker_position(begv, b, st)
                call marker_position(zv, e, st)
                call narrow_to_region(b, e, st)
                call destroy_marker(begv, st)
                call destroy_marker(zv, st)
            end if
        end subroutine replace_conflict

        ! 3 when T1 and T2 are equal and T3 differs, 1 when T2 and T3 are
        ! equal and T1 differs, else 0.
        integer function choose(t1, t2, t3)
            character(len=*), intent(in) :: t1, t2, t3

            choose = 0

            if (is_same(t1, t2) .and. .not. is_same(t2, t3)) then
                choose = 3
            else if (.not. is_same(t1, t2) .and. is_same(t2, t3)) then
                choose = 1
            end if
        end function choose

        ! The comment test: the conflict is within a single comment (and
        ! no conflict comes before point); then `choose' of the normalized
        ! texts.
        subroutine test_in_one_comment(c)
            integer, intent(out) :: c
            type(parse_state_type) :: ps
            integer(int64) :: here, found
            logical :: moved

            c = 0
            here = point()

            test: block
                call syntax_ppss(ps, m0b)
                if (ps%incomment == 0) exit test
                ! If there's a conflict earlier in the file,
                ! syntax-ppss is not reliable.
                call re_search_backward(smerge_begin_regexp, found, st, noerror=SEARCH_NOERROR)
                if (found /= 0) exit test
                call syntax_ppss(ps, m0b)
                call goto_char(ps%comstr_start)
                moved = forward_comment(1_int64)
                if (point() <= m0e) exit test
                c = choose(normalize_resolve_smerge(m1b, m1e), normalize_resolve_smerge(m2b, m2e), &
                           normalize_resolve_smerge(m3b, m3e))
            end block test

            call goto_char(here)
        end subroutine test_in_one_comment
    end subroutine resolve_smerge

    !> Perform automatic resolution on all conflicts.  Port of
    !> smerge-mode.el:smerge-resolve-all.
    recursive subroutine resolve_smerge_all(status)
        type(status_type), intent(out) :: status
        type(status_type) :: st
        integer(int64) :: here, found

        status = status_type()
        here = point()
        call goto_char(point_min())

        do
            call re_search_forward(smerge_begin_regexp, found, status, noerror=SEARCH_NOERROR)
            if (.not. status%ok()) exit
            if (found == 0) exit
            ! (with-demoted-errors "%S" ...)
            call match_smerge_conflict(st)
            if (st%ok()) call resolve_smerge(st, .true.)
            if (.not. st%ok()) call message(format_error(st))
        end do

        call goto_char(here)
    end subroutine resolve_smerge_all

    ! ------------------------------------------------------------------
    ! Diffs between versions
    ! ------------------------------------------------------------------

    !> Append the argument TEXT to ARGS.  Not a port.
    subroutine append_arg(args, text)
        type(value_type), allocatable, intent(inout) :: args(:)
        character(len=*), intent(in) :: text
        type(value_type) :: arg

        arg = value_type(text=text)
        args = [args, arg]
    end subroutine append_arg

    !> The program `diff_command' names.  Not a port.
    function diff_program() result(program)
        character(len=:), allocatable :: program

        program = "diff"
        if (allocated(diff_command)) program = diff_command
    end function diff_program

    !> Show the differences between parts N1 and N2 of the conflict at
    !> point in `smerge_diff_buffer_name', in Diff mode.  Port of
    !> smerge-mode.el:smerge-diff.
    recursive subroutine diff_smerge(n1, n2, status)
        integer, intent(in) :: n1, n2
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: name1, name2, file1, file2, dir, file, msg, saved_coding
        type(value_type), allocatable :: args(:)
        type(value_type) :: result
        type(handle_type) :: buffer, old, window
        type(status_type) :: st
        integer(int64) :: beg1, end1, beg2, end2
        logical :: read_only, had_coding
        integer :: k

        call match_smerge_conflict(status)
        if (.not. status%ok()) return
        call ensure_smerge_match(n1, status)
        if (.not. status%ok()) return
        call ensure_smerge_match(n2, status)
        if (.not. status%ok()) return
        name1 = trim(smerge_match_names(n1))
        name2 = trim(smerge_match_names(n2))
        ! Read them before the match-data gets clobbered.
        beg1 = match_beginning(n1, st)
        end1 = match_end(n1, st)
        beg2 = match_beginning(n2, st)
        end2 = match_end(n2, st)
        call create_temp_file("smerge1", file1, status)
        if (.not. status%ok()) return
        call create_temp_file("smerge2", file2, status)

        if (.not. status%ok()) then
            call delete_file(file1, st)
            return
        end if

        dir = "./"
        if (allocated(buffers(current)%directory)) dir = buffers(current)%directory
        file = ""
        if (allocated(buffers(current)%filename)) file = file_relative_name(buffers(current)%filename)
        ! We would want to use `emacs-mule-unix' for read&write, but we
        ! bump into problems with the coding-system used by diff to write
        ! the file names and the time stamps in the header.
        ! `buffer_file_coding_system' is not always correct either, but if
        ! the OS/user uses only one coding-system, then it works.
        had_coding = allocated(coding_system_for_read%value)
        if (had_coding) saved_coding = coding_system_for_read%value
        if (allocated(buffer_file_coding_system%value)) coding_system_for_read%value = buffer_file_coding_system%value
        call write_region(file1, .false., status, beg1, end1, msg)
        if (status%ok()) call write_region(file2, .false., status, beg2, end2, msg)
        old = current_buffer()

        show: block
            if (.not. status%ok()) exit show
            buffer = get_buffer(smerge_diff_buffer_name)

            if (buffer%index /= 0) then
                call set_buffer(buffer, status)
                if (.not. status%ok()) exit show
            else
                call get_or_make_buffer(smerge_diff_buffer_name, buffer, status)
                if (.not. status%ok()) exit show
                call set_buffer(buffer, status)
                if (.not. status%ok()) exit show
                buffers(current)%read_only = .true.
            end if

            buffers(current)%directory = dir
            ! Upstream binds inhibit-read-only to t here; the buffer's own text has
            ! no read-only properties, so clearing its read-only flag for the
            ! time does the same.
            read_only = buffers(current)%read_only
            buffers(current)%read_only = .false.
            call erase_buffer(status)

            if (status%ok()) then
                allocate (args(0))

                do k = 1, size(smerge_diff_switches)
                    call append_arg(args, trim(smerge_diff_switches(k)))
                end do

                call diff_check_labels()

                if (diff_use_labels%value) then
                    call append_arg(args, "--label")
                    call append_arg(args, name1//"/"//file)
                    call append_arg(args, "--label")
                    call append_arg(args, name2//"/"//file)
                end if

                call append_arg(args, file1)
                call append_arg(args, file2)
                call execute_process(diff_program(), result, status, destination=value_type(.true.), args=make_list(args))

                if (status%ok()) then
                    if (vtag(result) == V_INTEGER) then
                        if (xfixnum(result) == 0) call insert("No differences found."//LF, status)
                    end if
                end if
            end if

            buffers(current)%read_only = read_only
            if (.not. status%ok()) exit show
            call goto_char(point_min())
            call diff_mode(status)
            if (.not. status%ok()) exit show
            call display_buffer(current_buffer(), window, status, &
                                table=display_action_parameters_type(inhibit_same_window=.true.))
        end block show

        if (is_buffer_live(old)) call set_buffer(old, st)

        if (had_coding) then
            coding_system_for_read%value = saved_coding
        else if (allocated(coding_system_for_read%value)) then
            deallocate (coding_system_for_read%value)
        end if

        call delete_file(file1, st)
        call delete_file(file2, st)
    end subroutine diff_smerge

    !> Diff `base' and `upper' version in current conflict region.  Port of
    !> smerge-mode.el:smerge-diff-base-upper.
    recursive subroutine diff_smerge_base_upper(status)
        type(status_type), intent(out) :: status

        call diff_smerge(2, 1, status)
    end subroutine diff_smerge_base_upper

    !> Diff `base' and `lower' version in current conflict region.  Port of
    !> smerge-mode.el:smerge-diff-base-lower.
    recursive subroutine diff_smerge_base_lower(status)
        type(status_type), intent(out) :: status

        call diff_smerge(2, 3, status)
    end subroutine diff_smerge_base_lower

    !> Diff `upper' and `lower' version in current conflict region.  Port of
    !> smerge-mode.el:smerge-diff-upper-lower.
    recursive subroutine diff_smerge_upper_lower(status)
        type(status_type), intent(out) :: status

        call diff_smerge(1, 3, status)
    end subroutine diff_smerge_upper_lower

    ! ------------------------------------------------------------------
    ! Navigation
    ! ------------------------------------------------------------------

    !> Go to the next COUNT'th conflict.
    !> Interactively, COUNT is the prefix numeric argument, and defaults to 1.
    !> Port of smerge-mode.el:smerge-next (easy-mmode-define-navigation's
    !> command; refining the conflict afterwards, as `diff_refine' asks, is
    !> deferred with `smerge_refine').
    recursive subroutine next_smerge_conflict(status, count, interactive)
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: count
        logical, intent(in), optional :: interactive
        integer(int64) :: n

        n = 1
        if (present(count)) n = count
        call easy_mmode_next(smerge_begin_regexp, "conflict", n, status, interactive=interactive)
    end subroutine next_smerge_conflict

    !> Go to the previous COUNT'th conflict.
    !> Interactively, COUNT is the prefix numeric argument, and defaults to 1.
    !> Port of smerge-mode.el:smerge-prev (easy-mmode-define-navigation's
    !> command).
    recursive subroutine previous_smerge_conflict(status, count)
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: count
        integer(int64) :: n

        n = 1
        if (present(count)) n = count
        call easy_mmode_prev(smerge_begin_regexp, "conflict", n, status)
    end subroutine previous_smerge_conflict

    ! ------------------------------------------------------------------
    ! The minor mode
    ! ------------------------------------------------------------------

    !> Minor mode to simplify editing output from the diff3 program.
    !>
    !> \{smerge_mode_map}  Port of smerge-mode.el:smerge-mode.
    recursive subroutine smerge_mode(status)
        type(status_type), intent(out) :: status

        call minor_modes%smerge_mode%command(status)
    end subroutine smerge_mode

    !> The body of `smerge_mode': the font-lock keywords and the
    !> paragraph separators of conflict markers while the mode is on.
    !> Port of smerge-mode.el:smerge-mode, in part (its body; removing the
    !> font-lock keywords is deferred with font-lock-remove-keywords).
    recursive subroutine smerge_mode_body(enable, status)
        logical, intent(in) :: enable
        type(status_type), intent(out) :: status
        type(font_lock_keyword_type) :: keywords(1)
        type(status_type) :: st
        integer(int64) :: here, mb0, me0, inner
        logical :: found
        integer :: k

        status = status_type()
        if (enable) call ready_mode_map()

        if (font_lock_mode_enabled()) then
            here = point()

            if (enable) then
                keywords(1) = smerge_font_lock_keyword()
                call font_lock_add_keywords("", keywords, status, FONT_LOCK_ADD_END)
                if (.not. status%ok()) return
            end if

            call goto_char(point_min())

            do
                call find_smerge_conflict(0_int64, found, status)
                if (.not. status%ok()) exit
                if (.not. found) exit
                inner = point()
                mb0 = match_beginning(0, st)
                me0 = match_end(0, st)
                ! (with-demoted-errors "%S" ...): those things do happen,
                ! occasionally.
                call font_lock_fontify_region(mb0, me0, st, loudly=.false.)
                if (.not. st%ok()) call message(format_error(st))
                call goto_char(inner)
            end do

            call goto_char(here)
            if (.not. status%ok()) return
        end if

        if (.not. allocated(paragraph_separate%value)) paragraph_separate%value = ""
        k = index(paragraph_separate%value, smerge_parsep_regexp)

        if (k > 0) then
            if (.not. enable) then
                call paragraph_separate%set_local(paragraph_separate%value(:k - 1)// &
                                                  paragraph_separate%value(k + len(smerge_parsep_regexp):))
            end if
        else if (enable) then
            call paragraph_separate%set_local(smerge_parsep_regexp//paragraph_separate%value)
        end if

        if (.not. enable) call remove_smerge_properties(point_min(), point_max())
    end subroutine smerge_mode_body

    !> The font-lock keyword of Smerge mode: each conflict that
    !> `find_smerge_conflict' finds, its versions and markers in their
    !> faces.  Port of smerge-mode.el smerge-font-lock-keywords.
    function smerge_font_lock_keyword() result(keyword)
        type(font_lock_keyword_type) :: keyword
        type(font_lock_highlight_type) :: highlights(6)

        highlights(1) = font_lock_highlight(1, trim(smerge_upper_face), FONT_LOCK_OVERRIDE_PREPEND, .true.)
        highlights(2) = font_lock_highlight(2, trim(smerge_base_face), FONT_LOCK_OVERRIDE_PREPEND, .true.)
        highlights(3) = font_lock_highlight(3, trim(smerge_lower_face), FONT_LOCK_OVERRIDE_PREPEND, .true.)
        ! FIXME: `keep' doesn't work right with syntactic fontification.
        highlights(4) = font_lock_highlight(0, trim(smerge_markers_face), FONT_LOCK_OVERRIDE_KEEP)
        highlights(5) = font_lock_highlight(4, override=FONT_LOCK_OVERRIDE_REPLACE, laxmatch=.true.)
        highlights(6) = font_lock_highlight(5, override=FONT_LOCK_OVERRIDE_REPLACE, laxmatch=.true.)
        keyword = font_lock_keyword(highlights=highlights, matcher=find_smerge_conflict)
    end function smerge_font_lock_keyword

    !> Turn on `smerge_mode' and move point to first conflict marker.
    !> If no conflict marker is found, turn off `smerge_mode'.
    !> INTERACTIVELY true (as the command) turns the mode on again even
    !> when it is on.  Port of smerge-mode.el:smerge-start-session.
    recursive subroutine start_smerge_session(status, interactively)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: interactively
        type(status_type) :: st
        logical :: again

        status = status_type()
        again = .false.
        if (present(interactively)) again = interactively
        if (minor_modes%smerge_mode%enabled() .and. .not. again) return
        call minor_modes%smerge_mode%enable(status)
        if (.not. status%ok()) return

        if (.not. looking_at(smerge_begin_regexp, st)) then
            if (st%ok()) call next_smerge_conflict(st)
        end if

        if (.not. st%ok()) call auto_leave_smerge(status)
    end subroutine start_smerge_session

    !> Give `smerge_mode_map' its `smerge_command_prefix' binding to
    !> `smerge_basic_map', once.  Not a port.
    subroutine ready_mode_map()
        type(status_type) :: st

        if (mode_map_ready) return
        mode_map_ready = .true.
        if (len(smerge_command_prefix) > 0) call keymap_set(smerge_mode_map, smerge_command_prefix, &
                                                            make_keymap_ref(smerge_basic_map), st)
    end subroutine ready_mode_map

    ! ------------------------------------------------------------------
    ! Commands
    ! ------------------------------------------------------------------

    !> The numeric prefix argument.  Not a port.
    integer(int64) function p_arg()
        p_arg = prefix_numeric_value(current_prefix_arg)
    end function p_arg

    recursive subroutine cmd_next_smerge_conflict(status)
        type(status_type), intent(out) :: status

        call next_smerge_conflict(status, p_arg(), interactive=.true.)
    end subroutine cmd_next_smerge_conflict

    recursive subroutine cmd_previous_smerge_conflict(status)
        type(status_type), intent(out) :: status

        call previous_smerge_conflict(status, p_arg())
    end subroutine cmd_previous_smerge_conflict

    recursive subroutine cmd_resolve_smerge(status)
        type(status_type), intent(out) :: status

        call resolve_smerge(status)
    end subroutine cmd_resolve_smerge

    recursive subroutine cmd_start_smerge_session(status)
        type(status_type), intent(out) :: status

        call start_smerge_session(status, .true.)
    end subroutine cmd_start_smerge_session

    recursive subroutine cmd_extend_smerge(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: m

        if (is_use_region()) then
            call mark_marker(m)
            call extend_smerge(status, otherpos=m)
        else
            call extend_smerge(status, p_arg())
        end if
    end subroutine cmd_extend_smerge

    ! ------------------------------------------------------------------
    ! Definitions
    ! ------------------------------------------------------------------

    !> The faces of smerge-mode.el.  Not a port.
    subroutine define_faces()
        type(named_face_type), pointer :: f
        integer, parameter :: C = FACE_CLASS_COLOR, L = FACE_BACKGROUND_LIGHT, D = FACE_BACKGROUND_DARK

        f => smerge_upper
        call f%define("smerge_upper", "Face for the `upper' version of a conflict.")
        call f%spec_entry(display_when(classes=[C], min_colors=88, backgrounds=[L]), &
                          face_attrs(background="#ffdddd", extend=.true.))
        call f%spec_entry(display_when(classes=[C], min_colors=88, backgrounds=[D]), &
                          face_attrs(background="#553333", extend=.true.))
        call f%spec_entry(display_when(classes=[C]), face_attrs(foreground="red", extend=.true.))
        f => smerge_lower
        call f%define("smerge_lower", "Face for the `lower' version of a conflict.")
        call f%spec_entry(display_when(classes=[C], min_colors=88, backgrounds=[L]), &
                          face_attrs(background="#ddffdd", extend=.true.))
        call f%spec_entry(display_when(classes=[C], min_colors=88, backgrounds=[D]), &
                          face_attrs(background="#335533", extend=.true.))
        call f%spec_entry(display_when(classes=[C]), face_attrs(foreground="green", extend=.true.))
        f => smerge_base
        call f%define("smerge_base", "Face for the base code.")
        call f%spec_entry(display_when(classes=[C], min_colors=88, backgrounds=[L]), &
                          face_attrs(background="#ffffaa", extend=.true.))
        call f%spec_entry(display_when(classes=[C], min_colors=88, backgrounds=[D]), &
                          face_attrs(background="#888833", extend=.true.))
        call f%spec_entry(display_when(classes=[C]), face_attrs(foreground="yellow", extend=.true.))
        f => smerge_markers
        call f%define("smerge_markers", "Face for the conflict markers.")
        call f%spec_entry(display_when(backgrounds=[L]), face_attrs(background="grey85", extend=.true.))
        call f%spec_entry(display_when(backgrounds=[D]), face_attrs(background="grey30", extend=.true.))
        f => smerge_refined_changed
        call f%define("smerge_refined_changed", "Face used for char-based changes shown by `smerge_refine'.")
        call f%spec_entry(display_when(), face_attrs())
        f => smerge_refined_removed
        call f%define("smerge_refined_removed", "Face used for removed characters shown by `smerge_refine'.")
        call f%spec_entry(display_when(is_default=.true.), face_attrs(inherit="smerge_refined_changed"))
        call f%spec_entry(display_when(classes=[C], min_colors=88, backgrounds=[L]), face_attrs(background="#ffbbbb"))
        call f%spec_entry(display_when(classes=[C], min_colors=88, backgrounds=[D]), face_attrs(background="#882222"))
        call f%spec_entry(display_when(), face_attrs(inverse_video=.true.))
        f => smerge_refined_added
        call f%define("smerge_refined_added", "Face used for added characters shown by `smerge_refine'.")
        call f%spec_entry(display_when(is_default=.true.), face_attrs(inherit="smerge_refined_changed"))
        call f%spec_entry(display_when(classes=[C], min_colors=88, backgrounds=[L]), face_attrs(background="#aaffaa"))
        call f%spec_entry(display_when(classes=[C], min_colors=88, backgrounds=[D]), face_attrs(background="#228822"))
        call f%spec_entry(display_when(), face_attrs(inverse_video=.true.))
    end subroutine define_faces

    !> Define Smerge mode, its keymaps and faces, and register the
    !> commands.  Not a port.
    subroutine init_smerge_mode()
        type(status_type) :: st
        type(handle_type) :: diff_map, empty

        if (.not. allocated(smerge_diff_buffer_name)) smerge_diff_buffer_name = "*vc-diff*"
        if (.not. allocated(smerge_command_prefix)) smerge_command_prefix = "C-c ^"

        if (.not. allocated(smerge_diff_switches)) then
            if (allocated(diff_switches)) then
                smerge_diff_switches = [character(len=64) :: "-d", "-b", diff_switches]
            else
                smerge_diff_switches = [character(len=64) :: "-d", "-b"]
            end if
        end if

        if (.not. associated(smerge_resolve_function)) smerge_resolve_function => resolve_unknown
        if (.not. allocated(conflict_overlays)) allocate (conflict_overlays(0))
        call define_faces()
        call minor_modes%smerge_mode%define("smerge_mode", " SMerge", hook=smerge_mode_hook, keymap=.true., &
                                            body=smerge_mode_body)
        smerge_mode_map = minor_modes%smerge_mode%map
        call register_keymap_name("smerge_mode_map", smerge_mode_map)
        call append_locals_clearer(clear_smerge_locals)
        commands%smerge_mode%execute => smerge_mode
        commands%next_smerge_conflict%execute => cmd_next_smerge_conflict
        commands%previous_smerge_conflict%execute => cmd_previous_smerge_conflict
        commands%keep_smerge_all%execute => keep_smerge_all
        commands%keep_smerge_base%execute => keep_smerge_base
        commands%keep_smerge_lower%execute => keep_smerge_lower
        commands%keep_smerge_upper%execute => keep_smerge_upper
        commands%keep_smerge_current%execute => keep_smerge_current
        commands%delete_smerge_current%execute => delete_smerge_current
        commands%combine_smerge_with_next%execute => combine_smerge_with_next
        commands%auto_combine_smerge%execute => auto_combine_smerge
        commands%resolve_smerge%execute => cmd_resolve_smerge
        commands%resolve_smerge_all%execute => resolve_smerge_all
        commands%swap_smerge%execute => swap_smerge
        commands%extend_smerge%execute => cmd_extend_smerge
        commands%diff_smerge_base_upper%execute => diff_smerge_base_upper
        commands%diff_smerge_base_lower%execute => diff_smerge_base_lower
        commands%diff_smerge_upper_lower%execute => diff_smerge_upper_lower
        commands%start_smerge_session%execute => cmd_start_smerge_session
        ! smerge-basic-map.
        call make_sparse_keymap(smerge_basic_map)
        call keymap_set(smerge_basic_map, "n", "next_smerge_conflict", st)
        call keymap_set(smerge_basic_map, "p", "previous_smerge_conflict", st)
        call keymap_set(smerge_basic_map, "r", "resolve_smerge", st)
        call keymap_set(smerge_basic_map, "a", "keep_smerge_all", st)
        call keymap_set(smerge_basic_map, "b", "keep_smerge_base", st)
        ! For the obsolete keep-other.
        call keymap_set(smerge_basic_map, "o", "keep_smerge_lower", st)
        call keymap_set(smerge_basic_map, "l", "keep_smerge_lower", st)
        ! For the obsolete keep-mine.
        call keymap_set(smerge_basic_map, "m", "keep_smerge_upper", st)
        call keymap_set(smerge_basic_map, "u", "keep_smerge_upper", st)
        call keymap_set(smerge_basic_map, "C", "combine_smerge_with_next", st)
        call keymap_set(smerge_basic_map, "C-m", "keep_smerge_current", st)
        call make_sparse_keymap(diff_map, value_type(text="Diff"))
        call keymap_set(diff_map, "<", "diff_smerge_base_upper", st)
        call keymap_set(diff_map, ">", "diff_smerge_base_lower", st)
        call keymap_set(diff_map, "=", "diff_smerge_upper_lower", st)
        call keymap_set(smerge_basic_map, "=", make_keymap_ref(diff_map), st)
        call register_keymap_name("smerge_basic_map", smerge_basic_map)
        ! smerge-repeat-map.
        call make_sparse_keymap(smerge_repeat_map)
        call keymap_set(smerge_repeat_map, "n", "next_smerge_conflict", st)
        call keymap_set(smerge_repeat_map, "p", "previous_smerge_conflict", st)
        call keymap_set(smerge_repeat_map, "r", "resolve_smerge", st)
        call keymap_set(smerge_repeat_map, "a", "keep_smerge_all", st)
        call keymap_set(smerge_repeat_map, "b", "keep_smerge_base", st)
        call keymap_set(smerge_repeat_map, "o", "keep_smerge_lower", st)
        call keymap_set(smerge_repeat_map, "l", "keep_smerge_lower", st)
        call keymap_set(smerge_repeat_map, "m", "keep_smerge_upper", st)
        call keymap_set(smerge_repeat_map, "u", "keep_smerge_upper", st)
        call keymap_set(smerge_repeat_map, "C", "combine_smerge_with_next", st)
        call keymap_set(smerge_repeat_map, "C-m", "keep_smerge_current", st)
        call keymap_set(smerge_repeat_map, "<", "diff_smerge_base_upper", st)
        call keymap_set(smerge_repeat_map, ">", "diff_smerge_base_lower", st)
        call keymap_set(smerge_repeat_map, "=", "diff_smerge_upper_lower", st)
        call set_repeat_map(smerge_repeat_map)
        call register_keymap_name("smerge_repeat_map", smerge_repeat_map)
        ! Make it so `C-c ^ n' doesn't insert `n' but just signals an error
        ! when SMerge mode is not enabled (bug#73544).
        call make_sparse_keymap(empty)
        call keymap_set(current_global_map(), "C-c ^", make_keymap_ref(empty), st)
    end subroutine init_smerge_mode
end module simple_merge_mode_mod
