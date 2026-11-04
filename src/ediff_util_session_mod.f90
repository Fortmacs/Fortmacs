! ediff_util_session_mod.f90 -- the Ediff session's commands.
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

! The procedures of ediff_util_mod that use ediff_wind_mod and
! ediff_diff_mod (which use ediff_util_mod), with the commands.
submodule(ediff_util_mod) ediff_util_session_mod
    use error_mod, only: set_error, format_error, format_message, FM_GENERIC_ERROR, FM_USER_ERROR, FM_QUIT
    use handle_mod, only: null_handle
    use value_mod, only: value_type, vtag, xfixnum, V_INTEGER
    use buffer_mod, only: buffers, current, get_or_make_buffer, close_buffer, buffer_list, is_buffer_modified, &
                          header_line_format_type, restore_buffer_modified
    use syntax_mod, only: skip_chars_forward
    use data_mod, only: clear_all_local_variables
    use editfns_mod, only: point, goto_char, is_bol, buffer_substring, delete_region, narrow_to_region
    use insdel_mod, only: insert
    use cmds_mod, only: backward_char
    use simple_mod, only: count_lines, deactivate_mark
    use keymap_mod, only: make_sparse_keymap, keymap_set, use_local_map, destroy_keymap
    use keyboard_mod, only: last_command_event
    use callint_mod, only: this_command, current_prefix_arg, prefix_numeric_value, PREFIX_NONE, PREFIX_NUMBER, &
                           PREFIX_RAW, PREFIX_MINUS
    use commands_mod, only: commands
    use itree_mod, only: overlay_start, overlay_end, overlay_buffer, overlay_put, destroy_overlay, is_overlay
    use window_mod, only: windows, select_window, delete_window, is_window_live, set_window_start, set_window_dedication, &
                          is_pos_visible_in_window, move_to_window_line, recenter, window_total_height, frame_root_window, &
                          fit_window_to_buffer, switch_to_buffer, window_parent, split_window_below, &
                          split_window_right, DEDICATED_WEAK
    use frame_mod, only: selected_frame
    use fileio_mod, only: expand_file_name, file_name_directory, file_name_nondirectory, file_name_as_directory, &
                          is_file_directory, write_region, set_file_modes, is_visited_file_modtime_current
    use files_mod, only: create_temp_file, abbreviate_file_name, delete_file, save_buffer, revert_buffer
    use minibuffer_mod, only: read_file_name, REQUIRE_MATCH_YES
    use subr_mod, only: read_y_or_n
    use fns_mod, only: read_yes_or_no
    use coding_mod, only: coding_system_for_write
    use dispnew_mod, only: ding
    use xdisp_mod, only: message
    use ediff_init_mod, only: ediff_sessions, ediff_session_type, ediff_replaced_region_type, ediff_session_index, &
                              make_ediff_session_entry, drop_ediff_session, has_ediff_face_support, is_ediff_buffer_job, &
                              ediff_char_to_buffer_type, ediff_buffer_type_name, set_ediff_overlay_face, move_ediff_overlay, &
                              ediff_background_face, paint_ediff_background_regions, check_ediff_control_buffer, &
                              close_ediff_buffer_carefully, ediff_use_faces, ediff_highlight_all_diffs, ediff_keep_variants, &
                              ediff_make_buffers_readonly_at_startup, ediff_fortmacs_help_in_control_buffer, &
                              ediff_before_flag_bol, ediff_before_flag_mol, ediff_after_flag_eol, ediff_after_flag_mol, &
                              ediff_temp_file_mode, ediff_coding_system_for_write, ediff_before_setup_hook, &
                              ediff_startup_hook, ediff_select_hook, ediff_unselect_hook, ediff_prepare_buffer_hook, &
                              ediff_mode_hook, ediff_keymap_setup_hook, ediff_quit_hook, ediff_cleanup_hook, &
                              EDIFF_CURRENT_DIFF_FACES, EDIFF_KILLED_VITAL_BUFFER, EDIFF_NO_DIFFERENCES, &
                              EDIFF_BUFFER_TYPE_A, EDIFF_BUFFER_TYPE_B, EDIFF_BUFFER_TYPE_C, EDIFF_BUFFER_TYPE_ANCESTOR, &
                              EDIFF_HIGHLIGHTING_STYLE_FACE, &
                              EDIFF_HIGHLIGHTING_STYLE_ASCII, EDIFF_HIGHLIGHTING_STYLE_NONE
    use ediff_help_mod, only: ediff_use_long_help_message, set_ediff_help_message, indent_ediff_help_message, &
                              set_ediff_help_overlays
    use ediff_wind_mod, only: ediff_split_window_function, set_up_ediff_windows, refresh_ediff_mode_lines, &
                              get_ediff_visible_buffer_window, ediff_window_config
    use ediff_diff_mod, only: set_up_ediff_diff_regions, set_up_ediff_diff_regions3, set_ediff_actual_diff_options, &
                              ediff_actual_diff_options
    implicit none(type, external)

    character, parameter :: LF = achar(10)

    !> Flags of `unselect_and_select_ediff_difference'.
    integer, parameter :: SELECT_ONLY = 1, UNSELECT_ONLY = -1

contains

    ! ------------------------------------------------------------------
    ! Helpers
    ! ------------------------------------------------------------------

    ! Make BUFFER current, or return with the error condition of a killed
    ! Ediff buffer (none while quitting).  Port of
    ! ediff-init.el:ediff-with-current-buffer (its entry).
    recursive subroutine enter_ediff_buffer(buffer, status)
        type(handle_type), intent(in) :: buffer
        type(status_type), intent(out) :: status

        status = status_type()

        if (is_buffer_live(buffer)) then
            call set_buffer(buffer, status)
        else if (.not. associated(this_command, commands%quit_ediff)) then
            call set_error(status, FM_GENERIC_ERROR, EDIFF_KILLED_VITAL_BUFFER)
        end if
    end subroutine enter_ediff_buffer

    ! Whether N (the current difference if absent) is a difference of
    ! session K.  Port of ediff-init.el:ediff-valid-difference-p.
    logical function is_valid_difference(k, n)
        integer, intent(in) :: k
        integer, intent(in), optional :: n
        integer :: m

        m = ediff_sessions(k)%current_difference
        if (present(n)) m = n
        is_valid_difference = m >= 0 .and. m < ediff_sessions(k)%number_of_differences
    end function is_valid_difference

    ! Select WINDOW (if live) for a while, saving the selected window and
    ! the current buffer in OLD_WINDOW and OLD_BUFFER.  Not a port: the
    ! entry of with-selected-window.
    recursive subroutine select_window_begin(window, old_window, old_buffer, status)
        type(handle_type), intent(in) :: window
        type(handle_type), intent(out) :: old_window, old_buffer
        type(status_type), intent(out) :: status

        old_window = selected_frame%selected_window
        old_buffer = current_buffer()
        call select_window(window, status, norecord=.true.)
    end subroutine select_window_begin

    ! Select OLD_WINDOW again and make OLD_BUFFER current.  Not a port:
    ! the exit of with-selected-window.
    recursive subroutine select_window_end(old_window, old_buffer, status)
        type(handle_type), intent(in) :: old_window, old_buffer
        type(status_type), intent(out) :: status

        status = status_type()
        if (is_window_live(old_window)) call select_window(old_window, status, norecord=.true.)
        if (.not. status%ok()) return
        if (is_buffer_live(old_buffer)) call set_buffer(old_buffer, status)
    end subroutine select_window_end

    ! The message of an invalid difference number N (counted from 0) of
    ! session K.  Port of ediff-init.el ediff-BAD-DIFF-NUMBER.
    function bad_diff_number(k, n) result(text)
        integer, intent(in) :: k, n
        character(len=:), allocatable :: text
        character(len=:), allocatable :: name
        character(len=80) :: numbers

        name = "null()"
        if (associated(this_command)) name = trim(this_command%name)
        write (numbers, '(": Bad diff region number, ", i0, ".  Valid numbers are 1 to ", i0)') n + 1, &
            ediff_sessions(k)%number_of_differences
        text = name//trim(numbers)
    end function bad_diff_number

    ! ------------------------------------------------------------------
    ! The mode and its keymap
    ! ------------------------------------------------------------------

    recursive module subroutine ediff_mode(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call clear_all_local_variables()
        buffers(current)%major_mode = "ediff_mode"
        buffers(current)%mode_name = "Ediff"
        ! We use run-hooks instead of run-mode-hooks for one reason.  The
        ! ediff control buffer is read-only and it is not supposed to be
        ! modified by minor modes and such.
        call ediff_mode_hook%execute(status)
    end subroutine ediff_mode

    ! Set up the keymap used in the control buffer of session K
    ! (`ediff_mode_map') and use it.  Port of
    ! ediff-util.el:ediff-setup-keymap with ediff-set-keys (the keys of
    ! the deferred commands are not bound).
    recursive subroutine set_up_ediff_keymap(k, status)
        integer, intent(in) :: k
        type(status_type), intent(out) :: status
        type(handle_type) :: map
        character(len=1) :: digit
        integer :: d

        status = status_type()
        call make_sparse_keymap(map)
        ediff_sessions(k)%keymap = map
        ! (suppress-keymap ediff-mode-map)
        call bind("<remap> <self_insert_command>", "undefined")
        call bind("-", "negative_argument")

        do d = 0, 9
            write (digit, "(i1)") d
            call bind(digit, "digit_argument")
        end do

        call bind("p", "previous_ediff_difference")
        call bind("DEL", "previous_ediff_difference")
        call bind("<delete>", "previous_ediff_difference")
        if (.not. ediff_fortmacs_help_in_control_buffer) call bind("C-h", "previous_ediff_difference")
        call bind("<backspace>", "previous_ediff_difference")
        call bind("S-SPC", "previous_ediff_difference")
        call bind("n", "next_ediff_difference")
        call bind("SPC", "next_ediff_difference")
        call bind("j", "jump_to_ediff_difference")
        call bind("g a", "jump_to_ediff_difference_at_point")
        call bind("g b", "jump_to_ediff_difference_at_point")
        call bind("q", "quit_ediff")
        call bind("C-l", "recenter_ediff")
        call bind("|", "toggle_ediff_split")
        call bind("h", "toggle_ediff_highlighting")
        call bind("?", "toggle_ediff_help")
        call bind("!", "update_ediff_diffs")

        if (ediff_sessions(k)%three_way_comparison_job) then
            call bind("a b", "copy_ediff_a_to_b")
            call bind("b a", "copy_ediff_b_to_a")
            call bind("a c", "copy_ediff_a_to_c")
            call bind("b c", "copy_ediff_b_to_c")
            call bind("c a", "copy_ediff_c_to_a")
            call bind("c b", "copy_ediff_c_to_b")
            call bind("r a", "restore_ediff_diff")
            call bind("r b", "restore_ediff_diff")
            call bind("r c", "restore_ediff_diff")
        else
            ! 2-way comparison.
            call bind("a", "copy_ediff_a_to_b")
            call bind("b", "copy_ediff_b_to_a")
            call bind("r a", "restore_ediff_diff")
            call bind("r b", "restore_ediff_diff")
        end if

        call bind("# c", "toggle_ediff_ignore_case")
        if (ediff_sessions(k)%three_way_job) call bind("g c", "jump_to_ediff_difference_at_point")
        if (.not. status%ok()) return
        call use_local_map(map, status)
        if (.not. status%ok()) return
        call ediff_keymap_setup_hook%execute(status)

    contains

        ! Bind KEY to COMMAND in the map unless an earlier binding failed.
        subroutine bind(key, command)
            character(len=*), intent(in) :: key, command

            if (.not. status%ok()) return
            call keymap_set(map, key, command, status)
        end subroutine bind
    end subroutine set_up_ediff_keymap

    ! ------------------------------------------------------------------
    ! Setup
    ! ------------------------------------------------------------------

    recursive module subroutine set_up_ediff(buffer_a, file_a, buffer_b, file_b, buffer_c, file_c, job_name, &
                                             control_buffer, status, startup_hooks)
        type(handle_type), intent(in) :: buffer_a, buffer_b, buffer_c
        character(len=*), intent(in) :: file_a, file_b, file_c
        type(command_type), pointer, intent(in) :: job_name
        type(handle_type), intent(out) :: control_buffer
        type(status_type), intent(out) :: status
        type(hook_type), intent(inout), optional :: startup_hooks
        character(len=:), allocatable :: name, fa, fb, fc
        type(handle_type) :: old, old_window, old_buffer
        type(status_type) :: st
        integer(int64) :: shift
        integer :: k, t, lt, gt, digits

        call ediff_before_setup_hook%execute(status)
        if (.not. status%ok()) return
        call expand_file_name(file_a, fa, status)
        if (.not. status%ok()) return
        call expand_file_name(file_b, fb, status)
        if (.not. status%ok()) return
        fc = ""

        if (len(file_c) > 0) then
            call expand_file_name(file_c, fc, status)
            if (.not. status%ok()) return
        end if

        name = ediff_unique_buffer_name("*Ediff Control Panel", "*")
        old = current_buffer()
        call set_buffer(buffer_a, status)
        if (.not. status%ok()) return
        call get_or_make_buffer(name, control_buffer, status)
        if (.not. status%ok()) return
        call set_buffer(control_buffer, status)
        if (.not. status%ok()) return
        call ediff_mode(status)
        if (.not. status%ok()) return
        call make_ediff_session_entry(control_buffer, k)

        associate (s => ediff_sessions(k))
            s%use_long_help_message = ediff_use_long_help_message
            s%split_window_function => ediff_split_window_function
            s%keep_variants = ediff_keep_variants
            s%use_faces = ediff_use_faces
            s%highlight_all_diffs = ediff_highlight_all_diffs

            if (has_ediff_face_support() .and. s%use_faces) then
                s%highlighting_style = EDIFF_HIGHLIGHTING_STYLE_FACE
            else
                s%highlighting_style = EDIFF_HIGHLIGHTING_STYLE_ASCII
            end if

            ! Unwrap set up parameters passed as argument.
            s%job_name => job_name
            ! Set variables classifying the current ediff job.  The merge,
            ! window and region jobs are Deferred (docs/status/ediff_mod.md):
            ! their classes stay .false.
            s%three_way_comparison_job = associated(job_name, commands%ediff_files3) .or. &
                                         associated(job_name, commands%ediff_buffers3)
            s%three_way_job = s%three_way_comparison_job .or. s%merge_job
            s%diff3_job = s%three_way_comparison_job .or. s%merge_with_ancestor_job
            ! Don't delete variants in case of ediff-buffer-* jobs without
            ! asking.  This is because one may lose work---dangerous.
            if (is_ediff_buffer_job(job_name)) s%keep_variants = .true.
            buffers(current)%read_only = .false.
            s%buffers(EDIFF_BUFFER_TYPE_A) = buffer_a
            s%buffers(EDIFF_BUFFER_TYPE_B) = buffer_b
            s%buffers(EDIFF_BUFFER_TYPE_C) = buffer_c
            s%control_buffer = control_buffer
            ! The suffix of the control buffer's name, "<2>", and its
            ! number less one.
            s%control_buffer_suffix = ""
            s%control_buffer_number = 0
            lt = index(name, "<")
            gt = index(name, ">")

            if (lt > 0 .and. gt > lt) then
                s%control_buffer_suffix = name(lt:gt)
                digits = 0
                if (gt - lt > 1) read (name(lt + 1:gt - 1), *, iostat=t) digits
                s%control_buffer_number = max(0, digits - 1)
            end if
        end associate

        call get_or_make_buffer(ediff_unique_buffer_name("*ediff-errors", "*"), ediff_sessions(k)%error_buffer, status)
        if (.not. status%ok()) return
        buffers(ediff_sessions(k)%error_buffer%index)%undo_enabled = .false.
        if (.not. allocated(ediff_actual_diff_options)) call set_ediff_actual_diff_options(.false.)

        do t = EDIFF_BUFFER_TYPE_A, EDIFF_BUFFER_TYPE_C
            call make_ediff_bullet_proof_overlay(0_int64, 0_int64, ediff_sessions(k)%buffers(t), &
                                                 ediff_sessions(k)%wide_bounds(t), status, whole=.true.)
            if (.not. status%ok()) return
        end do

        ! This has effect only on ediff-windows/regions.
        ediff_sessions(k)%visible_bounds = ediff_sessions(k)%wide_bounds
        call set_up_ediff_keymap(k, status)
        if (.not. status%ok()) return
        ediff_sessions(k)%narrow_bounds = ediff_sessions(k)%wide_bounds

        ! These won't run if there are errors in diff.
        do t = EDIFF_BUFFER_TYPE_A, EDIFF_BUFFER_TYPE_C
            if (t == EDIFF_BUFFER_TYPE_C .and. .not. ediff_sessions(k)%three_way_job) exit
            call set_buffer(ediff_sessions(k)%buffers(t), status)
            if (.not. status%ok()) return
            call ediff_prepare_buffer_hook%execute(status)
            if (.not. status%ok()) return
            if (ediff_sessions(k)%merge_job .and. t /= EDIFF_BUFFER_TYPE_C) buffers(current)%read_only = .true.

            if (ediff_make_buffers_readonly_at_startup) then
                buffers(current)%read_only = .true.
            else if (t == EDIFF_BUFFER_TYPE_C) then
                buffers(current)%read_only = .false.
            end if
        end do

        call set_buffer(control_buffer, status)
        if (.not. status%ok()) return

        ! The following must be after setting up ediff-narrow-bounds.
        if (ediff_sessions(k)%diff3_job) then
            call set_up_ediff_diff_regions3(k, fa, fb, fc, status)
        else
            call set_up_ediff_diff_regions(k, fa, fb, status)
        end if

        if (.not. status%ok()) return
        call set_buffer(control_buffer, status)
        if (.not. status%ok()) return
        ediff_sessions(k)%number_of_differences = size(ediff_sessions(k)%differences, 2)
        ediff_sessions(k)%current_difference = -1

        do t = EDIFF_BUFFER_TYPE_A, EDIFF_BUFFER_TYPE_C
            if (t == EDIFF_BUFFER_TYPE_C .and. .not. ediff_sessions(k)%three_way_job) exit
            call make_ediff_current_diff_overlay(k, t, status)
            if (.not. status%ok()) return
        end do

        call set_up_ediff_windows(buffer_a, buffer_b, buffer_c, control_buffer, status)
        if (.not. status%ok()) return

        ! Position point in buffers A, B (and C).
        do t = EDIFF_BUFFER_TYPE_A, EDIFF_BUFFER_TYPE_C
            if (t == EDIFF_BUFFER_TYPE_C .and. .not. ediff_sessions(k)%three_way_job) exit
            if (.not. is_window_live(ediff_sessions(k)%windows(t))) cycle
            shift = overlay_start(ediff_sessions(k)%narrow_bounds(t))
            call select_window_begin(ediff_sessions(k)%windows(t), old_window, old_buffer, status)
            if (status%ok()) call goto_char(shift)
            ! Select the old window again, keeping the first error.
            call select_window_end(old_window, old_buffer, st)
            if (status%ok()) status = st
            if (.not. status%ok()) return
        end do

        if (ediff_select_control_window_on_setup .and. is_window_live(ediff_sessions(k)%control_window)) then
            call select_window(ediff_sessions(k)%control_window, status)
            if (.not. status%ok()) return
        end if

        call set_buffer(control_buffer, status)
        if (.not. status%ok()) return
        call make_ediff_region_visible(k, status)
        if (.not. status%ok()) return

        if (present(startup_hooks)) then
            call startup_hooks%execute(status)
            if (.not. status%ok()) return
        end if

        call set_buffer(control_buffer, status)
        if (.not. status%ok()) return
        call refresh_ediff_mode_lines(k)
        buffers(current)%read_only = .true.
        call ediff_startup_hook%execute(status)

        ! Make the old buffer current again, keeping the first error.
        if (is_buffer_live(old)) then
            call set_buffer(old, st)
            if (status%ok()) status = st
        end if
    end subroutine set_up_ediff

    recursive module subroutine set_up_ediff_control_buffer(k, control_buffer, status)
        integer, intent(in) :: k
        type(handle_type), intent(in) :: control_buffer
        type(status_type), intent(out) :: status
        type(handle_type) :: window, parent, root
        integer :: root_height
        integer(int64) :: moved

        window = selected_frame%selected_window
        call set_buffer(control_buffer, status)
        if (.not. status%ok()) return

        if (windows(window%index)%dedicated == 0) then
            call switch_to_buffer(buffers(current)%name, status)
            if (.not. status%ok()) return
            call set_buffer(control_buffer, status)
            if (.not. status%ok()) return
        end if

        call erase_control_buffer()
        if (.not. status%ok()) return
        call set_ediff_help_message(k, status)
        if (.not. status%ok()) return
        call set_buffer(control_buffer, status)
        if (.not. status%ok()) return
        call insert(ediff_sessions(k)%help_message, status)
        if (.not. status%ok()) return
        parent = window_parent(window, status)
        if (.not. status%ok()) return

        ! Emulate the behavior of Emacs 27: fit the window, but to at most
        ! half the frame (the lower window of a split gets the residue
        ! line).
        if (parent%index /= 0) then
            if (is_pos_visible_in_window(point_min(), window)) then
                root = frame_root_window(window, status)
                if (.not. status%ok()) return
                root_height = window_total_height(root, status)
                if (.not. status%ok()) return
                call fit_window_to_buffer(window, (root_height + 1)/2)
            end if
        end if

        call indent_ediff_help_message(k, status)
        if (.not. status%ok()) return
        call set_ediff_help_overlays(status)
        if (.not. status%ok()) return
        call restore_buffer_modified(.false.)
        call refresh_ediff_mode_lines(k)
        ediff_sessions(k)%control_window = selected_frame%selected_window
        ediff_sessions(k)%window_config_saved = ediff_window_config(k)
        call set_window_dedication(selected_frame%selected_window, DEDICATED_WEAK, status)
        if (.not. status%ok()) return
        call goto_char(point_min())
        ! (skip-chars-forward ediff-whitespace)
        moved = skip_chars_forward(" "//LF//achar(9)//achar(12)//achar(13)//char(194)//char(160), status)

    contains

        ! (erase-buffer) in the read-only control buffer.
        subroutine erase_control_buffer()
            use buffer_mod, only: erase_buffer
            logical :: saved

            saved = buffers(current)%read_only
            buffers(current)%read_only = .false.
            call erase_buffer(status)
            buffers(current)%read_only = saved
        end subroutine erase_control_buffer
    end subroutine set_up_ediff_control_buffer

    ! ------------------------------------------------------------------
    ! Commands for working with Ediff
    ! ------------------------------------------------------------------

    recursive module subroutine update_ediff_diffs(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: old
        type(status_type) :: st
        character(len=:), allocatable :: file_a, file_b, file_c
        integer(int64) :: point_a
        integer :: k, diff_no

        call check_ediff_control_buffer(k, status)
        if (.not. status%ok()) return
        old = current_buffer()
        call enter_ediff_buffer(ediff_sessions(k)%buffers(EDIFF_BUFFER_TYPE_A), status)
        if (.not. status%ok()) return
        point_a = point()
        call set_buffer(old, status)
        if (.not. status%ok()) return
        call unselect_and_select_ediff_difference(k, -1, status)
        if (.not. status%ok()) return
        call make_temp(EDIFF_BUFFER_TYPE_A, file_a)
        if (.not. status%ok()) return
        call make_temp(EDIFF_BUFFER_TYPE_B, file_b)
        if (.not. status%ok()) return
        file_c = ""

        if (ediff_sessions(k)%three_way_job) then
            call make_temp(EDIFF_BUFFER_TYPE_C, file_c)
            if (.not. status%ok()) return
        end if

        call clear_ediff_differences(k)
        ! Invalidate the replaced regions.
        if (allocated(ediff_sessions(k)%replaced_regions)) deallocate (ediff_sessions(k)%replaced_regions)
        call set_buffer(ediff_sessions(k)%control_buffer, status)

        if (status%ok()) then
            if (ediff_sessions(k)%diff3_job) then
                call set_up_ediff_diff_regions3(k, file_a, file_b, file_c, status)
            else
                call set_up_ediff_diff_regions(k, file_a, file_b, status)
            end if
        end if

        ! Delete the temporary files, keeping the first error.
        call delete_file(file_a, st)
        if (status%ok()) status = st
        call delete_file(file_b, st)
        if (status%ok()) status = st

        if (len(file_c) > 0) then
            call delete_file(file_c, st)
            if (status%ok()) status = st
        end if

        if (.not. status%ok()) return
        ediff_sessions(k)%number_of_differences = size(ediff_sessions(k)%differences, 2)
        call set_buffer(ediff_sessions(k)%control_buffer, status)
        if (.not. status%ok()) return
        call ediff_diff_at_point(k, EDIFF_BUFFER_TYPE_A, diff_no, status, pos=point_a)
        if (.not. status%ok()) return
        call jump_to_ediff_difference(int(diff_no, int64), status)
        if (.not. status%ok()) return
        call message("")

    contains

        ! A temporary file with the text of buffer BUFFER_TYPE, named after
        ! its file.
        subroutine make_temp(buffer_type, temp)
            integer, intent(in) :: buffer_type
            character(len=:), allocatable, intent(out) :: temp
            integer(int64) :: bi

            bi = ediff_sessions(k)%buffers(buffer_type)%index

            if (is_buffer_live(ediff_sessions(k)%buffers(buffer_type)) .and. allocated(buffers(bi)%filename)) then
                call create_ediff_temp_file(ediff_sessions(k)%buffers(buffer_type), temp, status, &
                                            file_name_nondirectory(buffers(bi)%filename))
            else
                call create_ediff_temp_file(ediff_sessions(k)%buffers(buffer_type), temp, status)
            end if
        end subroutine make_temp
    end subroutine update_ediff_diffs

    recursive module subroutine recenter_ediff(status, no_rehighlight)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: no_rehighlight
        integer :: k
        logical :: flag

        call check_ediff_control_buffer(k, status)
        if (.not. status%ok()) return
        flag = .false.
        if (present(no_rehighlight)) flag = no_rehighlight
        call recenter_ediff_session(k, flag, status)
    end subroutine recenter_ediff

    ! The body of `recenter_ediff' for session K, current in its control
    ! buffer.  Port of ediff-util.el:ediff-recenter.
    recursive subroutine recenter_ediff_session(k, no_rehighlight, status)
        integer, intent(in) :: k
        logical, intent(in) :: no_rehighlight
        type(status_type), intent(out) :: status
        type(handle_type) :: control
        type(status_type) :: st
        logical :: saved, all_live
        integer :: t

        status = status_type()
        control = ediff_sessions(k)%control_buffer
        all_live = is_buffer_live(ediff_sessions(k)%buffers(EDIFF_BUFFER_TYPE_A)) .and. &
                   is_buffer_live(ediff_sessions(k)%buffers(EDIFF_BUFFER_TYPE_B))
        if (ediff_sessions(k)%three_way_job) all_live = all_live .and. &
                                                        is_buffer_live(ediff_sessions(k)%buffers(EDIFF_BUFFER_TYPE_C))
        ! The control buffer is writable while the windows are set up.
        saved = buffers(current)%read_only
        buffers(current)%read_only = .false.

        if (all_live) then
            associate (s => ediff_sessions(k))
                call set_up_ediff_windows(s%buffers(EDIFF_BUFFER_TYPE_A), s%buffers(EDIFF_BUFFER_TYPE_B), &
                                          s%buffers(EDIFF_BUFFER_TYPE_C), control, status)
            end associate
        else if (.not. associated(this_command, commands%quit_ediff)) then
            call ding()
            call message(EDIFF_KILLED_VITAL_BUFFER)
        end if

        ! Make the control buffer current again, keeping the first error.
        if (is_buffer_live(control)) then
            call set_buffer(control, st)
            if (st%ok()) buffers(current)%read_only = saved
            if (status%ok()) status = st
        end if

        if (.not. status%ok()) return
        ! Set visibility range appropriate to this invocation of Ediff.
        call make_ediff_region_visible(k, status)
        if (.not. status%ok()) return

        ! Redisplay whatever buffers are showing, if there is a selected
        ! difference.
        if (all_live) then
            if (.not. no_rehighlight) then
                call select_ediff_difference(k, ediff_sessions(k)%current_difference, status)
                if (.not. status%ok()) return
            end if

            do t = EDIFF_BUFFER_TYPE_A, EDIFF_BUFFER_TYPE_C
                if (t == EDIFF_BUFFER_TYPE_C .and. .not. ediff_sessions(k)%three_way_job) exit
                call recenter_ediff_one_window(k, t, status)
                if (.not. status%ok()) return
            end do
        end if

        if (.not. no_rehighlight) then
            call restore_ediff_highlighting(k, status)
            if (.not. status%ok()) return
        end if

        if (is_buffer_live(control)) then
            call set_buffer(control, status)
            if (.not. status%ok()) return
        end if

        call refresh_ediff_mode_lines(k)
    end subroutine recenter_ediff_session

    ! Show the current difference of session K in the window of
    ! BUFFER_TYPE, without changing the selected window.  Port of
    ! ediff-util.el:ediff-recenter-one-window.
    recursive subroutine recenter_ediff_one_window(k, buffer_type, status)
        integer, intent(in) :: k, buffer_type
        type(status_type), intent(out) :: status
        type(handle_type) :: window, old_window, old_buffer, control
        type(status_type) :: st
        integer(int64) :: beginning, end

        status = status_type()
        if (.not. is_valid_difference(k)) return
        ! Context must be saved before switching to windows A/B/C.
        control = ediff_sessions(k)%control_buffer
        window = ediff_sessions(k)%windows(buffer_type)
        if (.not. is_window_live(window)) return
        call get_ediff_diff_position(k, buffer_type, .false., beginning, status)
        if (.not. status%ok()) return
        call get_ediff_diff_position(k, buffer_type, .true., end, status)
        if (.not. status%ok()) return
        call select_window_begin(window, old_window, old_buffer, status)

        if (status%ok()) then
            call deactivate_mark()
            call position_ediff_region(beginning, end, beginning, status)
        end if

        ! Select the old window again, keeping the first error.
        call select_window_end(old_window, old_buffer, st)
        if (status%ok()) status = st
        if (.not. status%ok()) return
        call set_buffer(control, status)
    end subroutine recenter_ediff_one_window

    ! Show BEGINNING to END in the selected window, centered when it
    ! fits, and put point at POS.  Port of ediff-util.el:ediff-position-region
    ! (windows jobs are deferred).
    recursive subroutine position_ediff_region(beginning, end_in, pos, status)
        integer(int64), intent(in) :: beginning, end_in, pos
        type(status_type), intent(out) :: status
        integer(int64) :: end, lines, prev_point, line
        integer :: height

        end = min(end_in, point_max())
        call set_window_start(windows(selected_frame%selected_window%index), beginning, status)
        if (.not. status%ok()) return

        if (is_pos_visible_in_window(end, selected_frame%selected_window)) then
            ! Determine the number of lines that the region occupies.
            lines = 0
            prev_point = 0

            do
                call move_to_window_line(prefix_arg_type(PREFIX_NUMBER, lines), line, status)
                if (.not. status%ok()) return
                if (.not. end > point()) exit
                ! `end' may be beyond the window bottom, so check that we
                ! are making progress.
                if (.not. prev_point < point()) exit
                prev_point = point()
                lines = lines + 1
            end do

            ! And position the beginning on the right line.
            call goto_char(beginning)
            height = window_total_height(selected_frame%selected_window, status)
            if (.not. status%ok()) return
            call recenter(prefix_arg_type(PREFIX_NUMBER, (1 + max(height - 1 - lines, 1_int64))/2), .false., status)
            if (.not. status%ok()) return
        end if

        call goto_char(pos)
    end subroutine position_ediff_region

    recursive module subroutine toggle_ediff_split(status)
        type(status_type), intent(out) :: status
        integer :: k

        call check_ediff_control_buffer(k, status)
        if (.not. status%ok()) return

        ! The windows of buffers A, B and C are on one frame.
        if (associated(ediff_sessions(k)%split_window_function, split_window_below)) then
            ediff_sessions(k)%split_window_function => split_window_right
        else
            ediff_sessions(k)%split_window_function => split_window_below
        end if

        call recenter_ediff_session(k, .true., status)
    end subroutine toggle_ediff_split

    recursive module subroutine toggle_ediff_highlighting(status)
        type(status_type), intent(out) :: status
        integer :: k
        logical :: faces

        call check_ediff_control_buffer(k, status)
        if (.not. status%ok()) return
        call unselect_and_select_ediff_difference(k, ediff_sessions(k)%current_difference, status, UNSELECT_ONLY)
        if (.not. status%ok()) return
        faces = has_ediff_face_support()

        ! Cycle through highlighting.
        associate (s => ediff_sessions(k))
            if (s%use_faces .and. faces .and. s%highlight_all_diffs) then
                call message("Unhighlighting unselected difference regions")
                s%highlight_all_diffs = .false.
                s%highlighting_style = EDIFF_HIGHLIGHTING_STYLE_FACE
            else if ((s%use_faces .and. faces .and. s%highlighting_style == EDIFF_HIGHLIGHTING_STYLE_FACE) .or. &
                     (.not. faces .and. s%highlighting_style == EDIFF_HIGHLIGHTING_STYLE_NONE)) then
                call message("Highlighting with ASCII flags")
                s%highlighting_style = EDIFF_HIGHLIGHTING_STYLE_ASCII
                s%highlight_all_diffs = .false.
                s%use_faces = .false.
            else if (s%highlighting_style == EDIFF_HIGHLIGHTING_STYLE_ASCII) then
                call message("ASCII highlighting flags removed")
                s%highlighting_style = EDIFF_HIGHLIGHTING_STYLE_NONE
                s%highlight_all_diffs = .false.
            else if (faces) then
                ! The other cases with face support.
                call message("Re-highlighting all difference regions")
                s%use_faces = .true.
                s%highlighting_style = EDIFF_HIGHLIGHTING_STYLE_FACE
                s%highlight_all_diffs = .true.
            end if

            call paint_ediff_background_regions(k,.not. (s%use_faces .and. s%highlight_all_diffs), status)
        end associate

        if (.not. status%ok()) return
        call unselect_and_select_ediff_difference(k, ediff_sessions(k)%current_difference, status, SELECT_ONLY)
    end subroutine toggle_ediff_highlighting

    recursive module subroutine toggle_ediff_help(status)
        type(status_type), intent(out) :: status
        integer :: k

        call check_ediff_control_buffer(k, status)
        if (.not. status%ok()) return
        ediff_sessions(k)%use_long_help_message = .not. ediff_sessions(k)%use_long_help_message
        call set_ediff_help_message(k, status)
        if (.not. status%ok()) return
        ! Force redisplay.
        ediff_sessions(k)%window_config_saved = ""
        call recenter_ediff_session(k, .true., status)
    end subroutine toggle_ediff_help

    ! ------------------------------------------------------------------
    ! Moving between differences
    ! ------------------------------------------------------------------

    recursive module subroutine next_ediff_difference(status, arg)
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: arg
        integer(int64) :: count
        integer :: k, n

        call check_ediff_control_buffer(k, status)
        if (.not. status%ok()) return
        count = 1
        if (present(arg)) count = arg

        if (ediff_sessions(k)%current_difference < ediff_sessions(k)%number_of_differences) then
            n = int(min(int(ediff_sessions(k)%number_of_differences, int64), &
                        ediff_sessions(k)%current_difference + count))
            call make_ediff_region_visible(k, status)
            if (.not. status%ok()) return
            call message("")
            call unselect_and_select_ediff_difference(k, n, status)
        else
            call make_ediff_region_visible(k, status)
            if (status%ok()) call set_error(status, FM_USER_ERROR, "At end of the difference list")
        end if
    end subroutine next_ediff_difference

    recursive module subroutine previous_ediff_difference(status, arg)
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: arg
        integer(int64) :: count
        integer :: k, n

        call check_ediff_control_buffer(k, status)
        if (.not. status%ok()) return
        count = 1
        if (present(arg)) count = arg

        if (ediff_sessions(k)%current_difference > -1) then
            n = int(max(-1_int64, ediff_sessions(k)%current_difference - count))
            call make_ediff_region_visible(k, status)
            if (.not. status%ok()) return
            call message("")
            call unselect_and_select_ediff_difference(k, n, status)
        else
            call make_ediff_region_visible(k, status)
            if (status%ok()) call set_error(status, FM_USER_ERROR, "At beginning of the difference list")
        end if
    end subroutine previous_ediff_difference

    recursive module subroutine jump_to_ediff_difference(difference_number, status)
        integer(int64), intent(in) :: difference_number
        type(status_type), intent(out) :: status
        integer :: k, n

        call check_ediff_control_buffer(k, status)
        if (.not. status%ok()) return

        if (difference_number < 0) then
            n = int(ediff_sessions(k)%number_of_differences + difference_number)
        else if (difference_number > 0) then
            n = int(difference_number - 1)
        else
            n = -1
        end if

        ! -1 is allowed by ediff-unselect-and-select-difference --- it is
        ! the position before the first one.
        if (n >= -1 .and. n <= ediff_sessions(k)%number_of_differences) then
            call unselect_and_select_ediff_difference(k, n, status)
        else
            call set_error(status, FM_GENERIC_ERROR, bad_diff_number(k, n))
        end if
    end subroutine jump_to_ediff_difference

    recursive module subroutine jump_to_ediff_difference_at_point(buffer_type, arg, status)
        integer, intent(in) :: buffer_type
        logical, intent(in) :: arg
        type(status_type), intent(out) :: status
        type(handle_type) :: buffer, old, wind(3), old_window, old_buffer
        type(status_type) :: st
        integer(int64) :: pt, beginning, shift
        integer :: k, diff_no, t
        logical :: past_last_diff

        call check_ediff_control_buffer(k, status)
        if (.not. status%ok()) return
        buffer = ediff_sessions(k)%buffers(buffer_type)
        old = current_buffer()
        call enter_ediff_buffer(buffer, status)
        if (.not. status%ok()) return
        pt = point()
        call set_buffer(old, status)
        if (.not. status%ok()) return

        if (arg) then
            call ediff_diff_at_point(k, buffer_type, diff_no, status, which_diff=1)
        else
            call ediff_diff_at_point(k, buffer_type, diff_no, status)
        end if

        if (.not. status%ok()) return
        past_last_diff = ediff_sessions(k)%number_of_differences < diff_no

        if (past_last_diff) then
            call set_buffer(buffer, status)
            if (.not. status%ok()) return
            beginning = point_max()
            call set_buffer(old, status)
            if (.not. status%ok()) return
            call jump_to_ediff_difference(-1_int64, status)
        else
            call get_ediff_diff_position(k, buffer_type, .false., beginning, status, diff_no - 1)
            if (.not. status%ok()) return
            call jump_to_ediff_difference(int(diff_no, int64), status)
        end if

        if (.not. status%ok()) return
        wind = ediff_sessions(k)%windows(EDIFF_BUFFER_TYPE_A:EDIFF_BUFFER_TYPE_C)
        if (.not. arg) return
        shift = beginning - pt

        do t = 1, 3
            if (.not. is_window_live(wind(t))) cycle
            call select_window_begin(wind(t), old_window, old_buffer, status)

            if (status%ok()) then
                if (past_last_diff) call goto_char(point_max())
                ! Its error is ignored, as upstream's ignore-errors does.
                call backward_char(st, shift)
                call recenter(prefix_arg_type(PREFIX_NONE, 0_int64), .false., status)
            end if

            ! Select the old window again, keeping the first error.
            call select_window_end(old_window, old_buffer, st)
            if (status%ok()) status = st
            if (.not. status%ok()) return
        end do
    end subroutine jump_to_ediff_difference_at_point

    recursive module subroutine ediff_diff_at_point(k, buffer_type, diff_no, status, pos, which_diff)
        integer, intent(in) :: k, buffer_type
        integer, intent(out) :: diff_no
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: pos
        integer, intent(in), optional :: which_diff
        type(handle_type) :: old
        type(status_type) :: st
        integer(int64) :: p, prev_beginning, prev_end, beginning, end
        integer :: max_dif_num, which

        old = current_buffer()
        max_dif_num = ediff_sessions(k)%number_of_differences - 1
        diff_no = -1
        prev_beginning = 0
        prev_end = 0
        beginning = 0
        end = 0
        which = 0
        if (present(which_diff)) which = which_diff
        call enter_ediff_buffer(ediff_sessions(k)%buffers(buffer_type), status)
        if (.not. status%ok()) return
        p = point()
        if (present(pos)) p = pos

        do while ((p < prev_beginning .or. p > beginning) .and. diff_no < max_dif_num)
            diff_no = diff_no + 1
            prev_beginning = beginning
            prev_end = end
            call get_ediff_diff_position(k, buffer_type, .false., beginning, status, diff_no)
            if (status%ok()) call get_ediff_diff_position(k, buffer_type, .true., end, status, diff_no)

            ! Make the old buffer current again, keeping the first error.
            if (.not. status%ok()) then
                call set_buffer(old, st)
                return
            end if
        end do

        ! Boost diff-no by 1, if past the last diff region.
        if (which /= 0 .and. p > beginning .and. diff_no == max_dif_num) diff_no = diff_no + 1

        if (which == 1) then
            diff_no = diff_no + 1
        else if (which == -1) then
            continue
        else if (abs(count_lines(p, max(point_min(), prev_end))) < abs(count_lines(p, max(point_min(), beginning)))) then
            ! Choose prev difference.
            continue
        else
            ! Choose next difference.
            diff_no = diff_no + 1
        end if

        call set_buffer(old, status)
    end subroutine ediff_diff_at_point

    ! ------------------------------------------------------------------
    ! Copying diffs
    ! ------------------------------------------------------------------

    recursive module subroutine copy_ediff_diff_to_diff(arg, keys, status)
        type(prefix_arg_type), intent(in) :: arg
        character(len=*), intent(in) :: keys
        type(status_type), intent(out) :: status
        integer :: k, n

        call check_ediff_control_buffer(k, status)
        if (.not. status%ok()) return

        if (arg%kind == PREFIX_RAW .and. arg%value == 4) then
            ! Copy all differences with `C-u' prefix.
            n = 0

            do while (is_valid_difference(k, n))
                call copy_ediff_diff_to_diff(prefix_arg_type(PREFIX_NUMBER, int(n + 1, int64)), keys, status)
                if (.not. status%ok()) return
                n = n + 1
            end do

            return
        end if

        ! Translate neg arg to -1.
        if (arg%kind == PREFIX_MINUS) then
            call jump_to_ediff_difference(-1_int64, status)
        else if (arg%kind == PREFIX_NUMBER) then
            call jump_to_ediff_difference(arg%value, status)
        end if

        if (.not. status%ok()) return
        if (len(keys) < 2) return
        call copy_ediff_diff(k, ediff_sessions(k)%current_difference, ediff_char_to_buffer_type(keys(1:1)), &
                             ediff_char_to_buffer_type(keys(2:2)), status)
        if (.not. status%ok()) return
        ! Recenter with rehighlighting, but no messages.
        call recenter_ediff_session(k, .false., status)
    end subroutine copy_ediff_diff_to_diff

    recursive module subroutine copy_ediff_diff(k, n, from_type, to_type, status, batch_invocation)
        integer, intent(in) :: k, n, from_type, to_type
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: batch_invocation
        type(handle_type) :: to_buffer, old
        type(status_type) :: st, restore_status
        character(len=:), allocatable :: reg_to_copy, reg_to_delete, messg
        integer(int64) :: reg_to_delete_beginning, reg_to_delete_end
        logical :: batch, saved

        batch = .false.
        if (present(batch_invocation)) batch = batch_invocation
        to_buffer = ediff_sessions(k)%buffers(to_type)
        saved = .true.
        messg = ""
        call get_ediff_diff_position(k, to_type, .false., reg_to_delete_beginning, status, n)
        if (.not. status%ok()) return
        call get_ediff_diff_position(k, to_type, .true., reg_to_delete_end, status, n)
        if (.not. status%ok()) return
        call get_ediff_region_contents(k, n, from_type, reg_to_copy, status)
        if (.not. status%ok()) return
        call get_ediff_region_contents(k, n, to_type, reg_to_delete, status, reg_to_delete_beginning, reg_to_delete_end)
        if (.not. status%ok()) return

        if (reg_to_delete == reg_to_copy) then
            ! Don't copy identical buffers.
            saved = .false.
        else
            ! Seems ok to copy.
            if (.not. batch) then
                call test_ediff_save_region(k, n, to_type, status)
                if (.not. status%ok()) return
            end if

            ! An error here is shown, as upstream's condition-case does.
            old = current_buffer()
            call set_buffer(to_buffer, st)

            if (st%ok()) then
                call goto_char(reg_to_delete_end)
                call insert(reg_to_copy, st)
            end if

            if (st%ok() .and. reg_to_delete_end > reg_to_delete_beginning) then
                call delete_region(reg_to_delete_beginning, reg_to_delete_end, st)
            end if

            ! Make the old buffer current again, keeping the first error.
            call set_buffer(old, restore_status)
            if (st%ok()) st = restore_status

            if (st%ok()) then
                if (.not. batch) call save_ediff_diff_region(k, n, to_type, reg_to_delete, messg)
            else
                call message("copy_ediff_diff: "//format_error(st))
                call ding()
                ! Let the user see the error msg.
                saved = .false.
            end if
        end if

        if (.not. batch) then
            call refresh_ediff_mode_lines(k)
            ! Make sure that the message about saving and how to restore is
            ! seen by the user.
            call message(messg)
        end if
    end subroutine copy_ediff_diff

    ! Save TEXT, the old difference N of BUFFER_TYPE in session K (in the
    ! session's `replaced_regions'), and give in MESSG what was
    ! saved.  Port of ediff-util.el:ediff-save-diff-region.
    subroutine save_ediff_diff_region(k, n, buffer_type, text, messg)
        integer, intent(in) :: k, n, buffer_type
        character(len=*), intent(in) :: text
        character(len=:), allocatable, intent(out) :: messg
        type(ediff_replaced_region_type) :: entry
        integer :: j
        character(len=:), allocatable :: recover
        character(len=20) :: digits

        j = replaced_region_index(k, n, buffer_type)

        if (j > 0) then
            ediff_sessions(k)%replaced_regions(j)%text = text
        else
            entry%n = n
            entry%buffer_type = buffer_type
            entry%text = text
            if (.not. allocated(ediff_sessions(k)%replaced_regions)) allocate (ediff_sessions(k)%replaced_regions(0))
            ediff_sessions(k)%replaced_regions = [ediff_sessions(k)%replaced_regions, entry]
        end if

        recover = ""
        if (.not. ediff_sessions(k)%merge_job) recover = char(ichar(ediff_buffer_type_name(buffer_type)) + 32)
        write (digits, "(i0)") n + 1
        messg = "Saving old diff region #"//trim(digits)//" of buffer "// &
                ediff_buffer_type_name(buffer_type)//format_message(".  To recover, type `r")//recover//format_message("'")
        call message(messg)
    end subroutine save_ediff_diff_region

    ! The index of the replaced region of difference N of BUFFER_TYPE in
    ! session K, 0 if none.  Not a port: the lookup in
    ! ediff-killed-diffs-alist that ediff-util.el does inline.
    integer function replaced_region_index(k, n, buffer_type) result(j)
        integer, intent(in) :: k, n, buffer_type

        j = 0
        if (.not. allocated(ediff_sessions(k)%replaced_regions)) return
        j = findloc(ediff_sessions(k)%replaced_regions%n == n .and. &
                    ediff_sessions(k)%replaced_regions%buffer_type == buffer_type, .true., dim=1)
    end function replaced_region_index

    ! Whether difference N of BUFFER_TYPE may be saved: if it was saved
    ! before, ask; "Quit" if the user declines.  Port of
    ! ediff-util.el:ediff-test-save-region.
    recursive subroutine test_ediff_save_region(k, n, buffer_type, status)
        integer, intent(in) :: k, n, buffer_type
        type(status_type), intent(out) :: status
        logical :: answer
        character(len=20) :: digits

        status = status_type()
        if (replaced_region_index(k, n, buffer_type) == 0) return
        write (digits, "(i0)") n + 1
        call read_yes_or_no("You've previously copied diff region "//trim(digits)//" to buffer "// &
                            ediff_buffer_type_name(buffer_type)//".  Confirm? ", answer, status)
        if (.not. status%ok()) return
        if (.not. answer) call set_error(status, FM_QUIT)
    end subroutine test_ediff_save_region

    ! Pop last killed Nth diff region from buffer BUFFER_TYPE.  Port of
    ! ediff-util.el:ediff-pop-diff.
    recursive subroutine pop_ediff_diff(k, n, buffer_type, status)
        integer, intent(in) :: k, n, buffer_type
        type(status_type), intent(out) :: status
        type(handle_type) :: old
        type(status_type) :: st, restore_status
        character(len=:), allocatable :: saved_diff
        type(ediff_replaced_region_type), allocatable :: rest(:)
        integer(int64) :: reg_beginning, reg_end
        integer :: i, j
        logical :: recovered
        character(len=20) :: digits

        write (digits, "(i0)") n + 1
        j = replaced_region_index(k, n, buffer_type)

        if (j > 0) then
            saved_diff = ediff_sessions(k)%replaced_regions(j)%text
        else if (ediff_sessions(k)%number_of_differences > 0) then
            call set_error(status, FM_USER_ERROR, "Nothing saved for diff "//trim(digits)//" in buffer "// &
                           ediff_buffer_type_name(buffer_type))
            return
        else
            call set_error(status, FM_GENERIC_ERROR, EDIFF_NO_DIFFERENCES)
            return
        end if

        call get_ediff_diff_position(k, buffer_type, .false., reg_beginning, status, n)
        if (.not. status%ok()) return
        call get_ediff_diff_position(k, buffer_type, .true., reg_end, status, n)
        if (.not. status%ok()) return
        ! An error here is shown, as upstream's condition-case does.
        old = current_buffer()
        call enter_ediff_buffer(ediff_sessions(k)%buffers(buffer_type), st)

        if (st%ok()) then
            call goto_char(reg_end)
            call insert(saved_diff, st)
            if (st%ok() .and. reg_end > reg_beginning) call delete_region(reg_beginning, reg_end, st)
        end if

        ! Make the old buffer current again, keeping the first error.
        call set_buffer(old, restore_status)
        if (st%ok()) st = restore_status
        recovered = st%ok()

        if (.not. recovered) then
            call message("pop_ediff_diff: "//format_error(st))
            call ding()
        end if

        call refresh_ediff_mode_lines(k)

        if (recovered) then
            rest = pack(ediff_sessions(k)%replaced_regions, [(i /= j, i=1, size(ediff_sessions(k)%replaced_regions))])
            call move_alloc(rest, ediff_sessions(k)%replaced_regions)
            call message("Diff region "//trim(digits)//" in buffer "//ediff_buffer_type_name(buffer_type)// &
                         " restored")
        end if
    end subroutine pop_ediff_diff

    recursive module subroutine restore_ediff_diff(arg, key, status)
        type(prefix_arg_type), intent(in) :: arg
        character, intent(in) :: key
        type(status_type), intent(out) :: status
        integer :: k

        call check_ediff_control_buffer(k, status)
        if (.not. status%ok()) return

        if (arg%kind == PREFIX_NUMBER) then
            call jump_to_ediff_difference(arg%value, status)
            if (.not. status%ok()) return
        end if

        call pop_ediff_diff(k, ediff_sessions(k)%current_difference, ediff_char_to_buffer_type(key), status)
        if (.not. status%ok()) return
        ! Recenter with rehighlighting, but no messages.
        call recenter_ediff_session(k, .false., status)
    end subroutine restore_ediff_diff

    ! ------------------------------------------------------------------
    ! Quitting
    ! ------------------------------------------------------------------

    recursive module subroutine quit_ediff(reverse_default_keep_variants, status)
        type(status_type), intent(out) :: status
        logical, intent(in) :: reverse_default_keep_variants
        type(handle_type) :: control
        character(len=:), allocatable :: group
        logical :: answer
        integer :: k

        call check_ediff_control_buffer(k, status)
        if (.not. status%ok()) return
        control = current_buffer()
        group = ""
        if (is_buffer_live(ediff_sessions(k)%meta_buffer)) group = " & show containing session group"
        call read_y_or_n("Quit this Ediff session"//group//"? ", answer, status)
        if (.not. status%ok()) return
        call message("")
        if (.not. answer) return
        this_command => commands%quit_ediff
        call set_buffer(control, status)
        if (.not. status%ok()) return
        call quit_ediff_session(k, reverse_default_keep_variants, status)
    end subroutine quit_ediff

    recursive module subroutine quit_ediff_session(k, reverse_default_keep_variants, status)
        integer, intent(in) :: k
        logical, intent(in) :: reverse_default_keep_variants
        type(status_type), intent(out) :: status
        logical :: keep_variants
        integer :: t

        call unhighlight_ediff_diffs_totally(k, status)
        if (.not. status%ok()) return
        call clear_ediff_differences(k)
        ! Restore the visibility range.  This affects only
        ! ediff-*-regions/windows.
        ediff_sessions(k)%visible_bounds = ediff_sessions(k)%wide_bounds
        ! Apply selective display to narrow or widen.
        call make_ediff_region_visible(k, status)
        if (.not. status%ok()) return

        do t = EDIFF_BUFFER_TYPE_A, EDIFF_BUFFER_TYPE_C
            if (is_overlay(ediff_sessions(k)%wide_bounds(t))) call destroy_overlay(ediff_sessions(k)%wide_bounds(t))
            ediff_sessions(k)%wide_bounds(t) = null_handle()
            ediff_sessions(k)%narrow_bounds(t) = null_handle()
            ediff_sessions(k)%visible_bounds(t) = null_handle()
        end do

        call ediff_cleanup_hook%execute(status)
        if (.not. status%ok()) return
        ! reverse-default-keep-variants is t if the user quits with a
        ! prefix arg.
        keep_variants = ediff_sessions(k)%keep_variants
        if (reverse_default_keep_variants) keep_variants = .not. keep_variants
        call dispose_of_ediff_variants(k, .true., keep_variants, status)
        if (.not. status%ok()) return
        ! One hook here is clean_up_ediff_mess, which kills the control
        ! buffer and other auxiliary buffers.
        call ediff_quit_hook%execute(status)
    end subroutine quit_ediff_session

    ! Kill control buffer, other auxiliary Ediff buffers.
    ! Leave one of the frames split between buffers A/B/C.  Port of
    ! ediff-util.el:ediff-cleanup-mess (`balance-windows' is not ported).
    recursive subroutine clean_up_ediff_mess(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: buff_a, buff_b, buff_c, control_buffer, control_window, new
        type(status_type) :: st
        logical :: three_way_job
        integer :: k

        status = status_type()
        k = ediff_session_index()
        if (k == 0) return
        buff_a = ediff_sessions(k)%buffers(EDIFF_BUFFER_TYPE_A)
        buff_b = ediff_sessions(k)%buffers(EDIFF_BUFFER_TYPE_B)
        buff_c = ediff_sessions(k)%buffers(EDIFF_BUFFER_TYPE_C)
        control_buffer = ediff_sessions(k)%control_buffer
        control_window = get_ediff_visible_buffer_window(control_buffer)
        three_way_job = ediff_sessions(k)%three_way_job
        call close_ediff_buffer_carefully(ediff_sessions(k)%diff_buffer, status)
        if (.not. status%ok()) return
        call close_ediff_buffer_carefully(get_buffer(" *ediff-tmp*"), status)
        if (.not. status%ok()) return
        call close_ediff_buffer_carefully(ediff_sessions(k)%error_buffer, status)
        if (.not. status%ok()) return
        call close_ediff_buffer_carefully(get_buffer(" *ediff-message*"), status)
        if (.not. status%ok()) return
        call close_ediff_buffer_carefully(get_buffer("*ediff-debug*"), status)
        if (.not. status%ok()) return

        ! Delete control window.
        if (is_window_live(control_window)) then
            call delete_window(control_window, status)
            if (.not. status%ok()) return
        end if

        call destroy_keymap(ediff_sessions(k)%keymap, status)
        if (.not. status%ok()) return
        call drop_ediff_session(k)
        call close_ediff_buffer_carefully(control_buffer, status)
        if (.not. status%ok()) return

        ! Display only if not visible.  The errors of showing the buffers
        ! are ignored, as upstream's condition-case does.
        if (.not. is_window_live(get_ediff_visible_buffer_window(buff_b)) .and. is_buffer_live(buff_b)) then
            call switch_to_buffer(buffers(buff_b%index)%name, st)
        end if

        if (.not. is_window_live(get_ediff_visible_buffer_window(buff_a)) .and. is_buffer_live(buff_a)) then
            st = status_type()

            if (is_window_live(get_ediff_visible_buffer_window(buff_b)) .and. associated(ediff_split_window_function)) then
                call ediff_split_window_function(new, st)
            end if

            if (st%ok()) call switch_to_buffer(buffers(buff_a%index)%name, st)
        end if

        if (three_way_job .and. is_buffer_live(buff_c)) then
            if (.not. is_window_live(get_ediff_visible_buffer_window(buff_c))) then
                st = status_type()

                if (is_window_live(get_ediff_visible_buffer_window(buff_a)) .or. &
                    is_window_live(get_ediff_visible_buffer_window(buff_b))) then
                    if (associated(ediff_split_window_function)) call ediff_split_window_function(new, st)
                end if

                if (st%ok()) call switch_to_buffer(buffers(buff_c%index)%name, st)
            end if
        end if

        call message("")
    end subroutine clean_up_ediff_mess

    ! Kill buffers A, B, and, possibly, C, if these buffers aren't
    ! modified; ASK asks the user about each unmodified buffer, unless
    ! KEEP_VARIANTS, in which case buffers are never killed.  Port of
    ! ediff-util.el:ediff-janitor.
    recursive subroutine dispose_of_ediff_variants(k, ask, keep_variants, status)
        integer, intent(in) :: k
        logical, intent(in) :: ask, keep_variants
        type(status_type), intent(out) :: status
        type(handle_type) :: buffers_of(4)

        buffers_of = ediff_sessions(k)%buffers
        call dispose_of_ediff_variant(buffers_of(EDIFF_BUFFER_TYPE_A), EDIFF_BUFFER_TYPE_A, ask, keep_variants, status)
        if (.not. status%ok()) return
        call dispose_of_ediff_variant(buffers_of(EDIFF_BUFFER_TYPE_B), EDIFF_BUFFER_TYPE_B, ask, keep_variants, status)
        if (.not. status%ok()) return

        ! When merging, the ancestor buffer is disposed of instead of buffer C.
        if (ediff_sessions(k)%merge_job) then
            call dispose_of_ediff_variant(buffers_of(EDIFF_BUFFER_TYPE_ANCESTOR), EDIFF_BUFFER_TYPE_ANCESTOR, ask, &
                                          keep_variants, status)
        else
            call dispose_of_ediff_variant(buffers_of(EDIFF_BUFFER_TYPE_C), EDIFF_BUFFER_TYPE_C, ask, keep_variants, status)
        end if
    end subroutine dispose_of_ediff_variants

    ! Kill the variant buffer BUFFER of BUFFER_TYPE, according to user
    ! directives (ask, kill unconditionally, keep).  Port of
    ! ediff-util.el:ediff-dispose-of-variant-according-to-user (indirect
    ! buffers are deferred with region jobs).
    recursive subroutine dispose_of_ediff_variant(buffer, buffer_type, ask, keep_variants, status)
        type(handle_type), intent(in) :: buffer
        integer, intent(in) :: buffer_type
        logical, intent(in) :: ask, keep_variants
        type(status_type), intent(out) :: status
        logical :: modified, answer

        status = status_type()
        if (.not. is_buffer_live(buffer) .or. keep_variants) return
        call is_buffer_modified(buffer, modified, status)
        if (.not. status%ok() .or. modified) return

        if (ask) then
            call read_y_or_n("Kill buffer "//ediff_buffer_type_name(buffer_type)//" ["//buffers(buffer%index)%name// &
                             "]? ", answer, status)
            if (.not. status%ok() .or. .not. answer) return
        end if

        call close_ediff_buffer_carefully(buffer, status)
    end subroutine dispose_of_ediff_variant

    ! ------------------------------------------------------------------
    ! Selecting and highlighting differences
    ! ------------------------------------------------------------------

    ! Select difference N of session K: highlight it and run
    ! `ediff_select_hook'.  Port of ediff-util.el:ediff-select-difference
    ! (fine differences are deferred).
    recursive subroutine select_ediff_difference(k, n, status)
        integer, intent(in) :: k, n
        type(status_type), intent(out) :: status
        integer :: t

        status = status_type()
        if (.not. is_buffer_live(ediff_sessions(k)%buffers(EDIFF_BUFFER_TYPE_A))) return
        if (.not. is_buffer_live(ediff_sessions(k)%buffers(EDIFF_BUFFER_TYPE_B))) return
        if (.not. is_valid_difference(k, n)) return

        if (has_ediff_face_support() .and. ediff_sessions(k)%use_faces) then
            do t = EDIFF_BUFFER_TYPE_A, EDIFF_BUFFER_TYPE_ANCESTOR
                call highlight_ediff_diff_in_one_buffer(k, n, t, status)
                if (.not. status%ok()) return
            end do
        else if (ediff_sessions(k)%highlighting_style == EDIFF_HIGHLIGHTING_STYLE_ASCII) then
            do t = EDIFF_BUFFER_TYPE_A, EDIFF_BUFFER_TYPE_C
                if (t == EDIFF_BUFFER_TYPE_C .and. .not. ediff_sessions(k)%three_way_job) exit
                call place_ediff_flags_in_buffer(k, t, n, status)
                if (.not. status%ok()) return
            end do
        end if

        ! Set current difference here so the hook will be able to refer to
        ! it.
        ediff_sessions(k)%current_difference = n
        call ediff_select_hook%execute(status)
    end subroutine select_ediff_difference

    ! Unselect difference N of session K by removing the ASCII flags or
    ! the highlighting.  Port of ediff-util.el:ediff-unselect-difference.
    recursive subroutine unselect_ediff_difference(k, n, status)
        integer, intent(in) :: k, n
        type(status_type), intent(out) :: status
        integer :: t

        status = status_type()
        if (.not. is_valid_difference(k, n)) return

        if (has_ediff_face_support() .and. ediff_sessions(k)%use_faces) then
            do t = EDIFF_BUFFER_TYPE_A, EDIFF_BUFFER_TYPE_ANCESTOR
                call unhighlight_ediff_diff_in_one_buffer(k, t, status)
                if (.not. status%ok()) return
            end do
        else if (ediff_sessions(k)%highlighting_style == EDIFF_HIGHLIGHTING_STYLE_ASCII) then
            do t = EDIFF_BUFFER_TYPE_A, EDIFF_BUFFER_TYPE_C
                if (t == EDIFF_BUFFER_TYPE_C .and. .not. ediff_sessions(k)%three_way_job) exit
                call remove_ediff_flags(ediff_sessions(k)%differences(t, n + 1)%overlay, status)
                if (.not. status%ok()) return
            end do
        end if

        call ediff_unselect_hook%execute(status)
    end subroutine unselect_ediff_difference

    recursive module subroutine unselect_and_select_ediff_difference(k, n, status, flag, no_recenter)
        integer, intent(in) :: k, n
        type(status_type), intent(out) :: status
        integer, intent(in), optional :: flag
        logical, intent(in), optional :: no_recenter
        integer :: saved, how
        logical :: recentering

        how = 0
        if (present(flag)) how = flag
        recentering = .true.
        if (present(no_recenter)) recentering = .not. no_recenter
        status = status_type()

        if (recentering) then
            ! Recenter on difference N without highlighting it again.
            saved = ediff_sessions(k)%current_difference
            ediff_sessions(k)%current_difference = n
            call recenter_ediff_session(k, .true., status)
            ediff_sessions(k)%current_difference = saved
            if (.not. status%ok()) return
        end if

        if (how /= SELECT_ONLY) then
            call unselect_ediff_difference(k, ediff_sessions(k)%current_difference, status)
            if (.not. status%ok()) return
        end if

        if (how /= UNSELECT_ONLY) then
            call select_ediff_difference(k, n, status)
            if (.not. status%ok()) return
        end if

        ! Need to set current diff here even though it is also set in
        ! select_ediff_difference because it might not be called if
        ! unselect-only is specified.
        ediff_sessions(k)%current_difference = n

        if (is_buffer_live(ediff_sessions(k)%control_buffer)) then
            call set_buffer(ediff_sessions(k)%control_buffer, status)
            if (.not. status%ok()) return
        end if

        call refresh_ediff_mode_lines(k)
    end subroutine unselect_and_select_ediff_difference

    ! Highlight difference N of session K in BUFFER_TYPE with the current
    ! difference overlay.  Port of
    ! ediff-util.el:ediff-highlight-diff-in-one-buffer.
    recursive subroutine highlight_ediff_diff_in_one_buffer(k, n, buffer_type, status)
        integer, intent(in) :: k, n, buffer_type
        type(status_type), intent(out) :: status
        type(handle_type) :: buffer, old
        integer(int64) :: last, beginning, end, end_hilit

        status = status_type()
        buffer = ediff_sessions(k)%buffers(buffer_type)
        if (.not. is_buffer_live(buffer)) return
        if (.not. allocated(ediff_sessions(k)%differences)) return
        old = current_buffer()
        call set_buffer(buffer, status)
        if (.not. status%ok()) return
        last = point_max()
        call set_buffer(old, status)
        if (.not. status%ok()) return
        call get_ediff_diff_position(k, buffer_type, .false., beginning, status, n)
        if (.not. status%ok()) return
        call get_ediff_diff_position(k, buffer_type, .true., end, status, n)
        if (.not. status%ok()) return
        end_hilit = end
        if (beginning == end) end_hilit = end + 1
        end_hilit = min(last, end_hilit)
        call move_ediff_overlay(ediff_sessions(k)%current_diff_overlays(buffer_type), beginning, end_hilit, status)
        if (.not. status%ok()) return
        ! Unhighlight the background overlay for diff n so it won't
        ! interfere with the current diff overlay.
        call set_ediff_overlay_face(ediff_sessions(k)%differences(buffer_type, n + 1)%overlay, "", status)
    end subroutine highlight_ediff_diff_in_one_buffer

    ! Remove the current difference overlay of session K in BUFFER_TYPE
    ! and give the difference its background face again.  Port of
    ! ediff-util.el:ediff-unhighlight-diff-in-one-buffer.
    recursive subroutine unhighlight_ediff_diff_in_one_buffer(k, buffer_type, status)
        integer, intent(in) :: k, buffer_type
        type(status_type), intent(out) :: status
        integer :: n

        status = status_type()
        if (.not. is_buffer_live(ediff_sessions(k)%buffers(buffer_type))) return
        call move_ediff_overlay(ediff_sessions(k)%current_diff_overlays(buffer_type), 1_int64, 1_int64, status)
        if (.not. status%ok()) return
        n = ediff_sessions(k)%current_difference
        if (.not. is_valid_difference(k, n)) return

        ! Rehighlight the overlay in the background of the current
        ! difference region.
        if (has_ediff_face_support() .and. ediff_sessions(k)%use_faces .and. ediff_sessions(k)%highlight_all_diffs) then
            call set_ediff_overlay_face(ediff_sessions(k)%differences(buffer_type, n + 1)%overlay, &
                                        ediff_background_face(buffer_type, n), status)
        else
            call set_ediff_overlay_face(ediff_sessions(k)%differences(buffer_type, n + 1)%overlay, "", status)
        end if
    end subroutine unhighlight_ediff_diff_in_one_buffer

    ! Delete the highlighting overlays of session K and restore the faces
    ! to their original form.  Port of
    ! ediff-util.el:ediff-unhighlight-diffs-totally with
    ! ediff-unhighlight-diffs-totally-in-one-buffer.
    recursive subroutine unhighlight_ediff_diffs_totally(k, status)
        integer, intent(in) :: k
        type(status_type), intent(out) :: status
        integer :: t

        do t = EDIFF_BUFFER_TYPE_A, EDIFF_BUFFER_TYPE_ANCESTOR
            call unselect_and_select_ediff_difference(k, -1, status)
            if (.not. status%ok()) return

            if (has_ediff_face_support() .and. ediff_sessions(k)%use_faces) then
                call paint_ediff_background_regions(k, .true., status)
                if (.not. status%ok()) return

                if (is_overlay(ediff_sessions(k)%current_diff_overlays(t))) then
                    call destroy_overlay(ediff_sessions(k)%current_diff_overlays(t))
                end if

                ediff_sessions(k)%current_diff_overlays(t) = null_handle()
            end if
        end do
    end subroutine unhighlight_ediff_diffs_totally

    ! Put the ASCII flags around difference N of session K in BUFFER_TYPE.
    ! Port of ediff-util.el:ediff-place-flags-in-buffer with
    ! ediff-place-flags-in-buffer1.
    recursive subroutine place_ediff_flags_in_buffer(k, buffer_type, n, status)
        integer, intent(in) :: k, buffer_type, n
        type(status_type), intent(out) :: status
        type(handle_type) :: old, overlay
        type(status_type) :: st
        integer(int64) :: before, after
        logical :: ascii

        status = status_type()
        if (.not. is_buffer_live(ediff_sessions(k)%buffers(buffer_type))) return
        overlay = ediff_sessions(k)%differences(buffer_type, n + 1)%overlay
        ascii = ediff_sessions(k)%highlighting_style == EDIFF_HIGHLIGHTING_STYLE_ASCII
        call get_ediff_diff_position(k, buffer_type, .false., before, status, n)
        if (.not. status%ok()) return
        old = current_buffer()
        call set_buffer(ediff_sessions(k)%buffers(buffer_type), status)
        if (.not. status%ok()) return
        ! Insert flag before the difference.
        call goto_char(before)

        if (.not. ascii) then
            call overlay_put(overlay, "before_string", value_type(), status)
        else if (is_bol()) then
            call overlay_put(overlay, "before_string", value_type(text=ediff_before_flag_bol), status)
        else
            call overlay_put(overlay, "before_string", value_type(text=ediff_before_flag_mol), status)
        end if

        ! Insert the flag after the difference.
        if (status%ok()) call get_ediff_diff_position(k, buffer_type, .true., after, status, n)

        if (status%ok()) then
            call goto_char(after)

            if (.not. ascii) then
                call overlay_put(overlay, "after_string", value_type(), status)
            else if (is_bol()) then
                call overlay_put(overlay, "after_string", value_type(text=ediff_after_flag_eol), status)
            else
                call overlay_put(overlay, "after_string", value_type(text=ediff_after_flag_mol), status)
            end if
        end if

        ! Make the old buffer current again, keeping the first error.
        call set_buffer(old, st)
        if (status%ok()) status = st
    end subroutine place_ediff_flags_in_buffer

    ! Remove the ASCII flags of OVERLAY.  Port of
    ! ediff-util.el:ediff-remove-flags-from-buffer.
    subroutine remove_ediff_flags(overlay, status)
        type(handle_type), intent(in) :: overlay
        type(status_type), intent(out) :: status

        call overlay_put(overlay, "before_string", value_type(), status)
        if (.not. status%ok()) return
        call overlay_put(overlay, "after_string", value_type(), status)
    end subroutine remove_ediff_flags

    recursive module subroutine get_ediff_diff_position(k, buffer_type, at_end, position, status, n)
        integer, intent(in) :: k, buffer_type
        logical, intent(in) :: at_end
        integer(int64), intent(out) :: position
        type(status_type), intent(out) :: status
        integer, intent(in), optional :: n
        type(handle_type) :: overlay
        integer :: m

        status = status_type()
        position = 0
        m = ediff_sessions(k)%current_difference
        if (present(n)) m = n

        if (m < 0 .or. m >= ediff_sessions(k)%number_of_differences) then
            if (ediff_sessions(k)%number_of_differences > 0) then
                call set_error(status, FM_GENERIC_ERROR, bad_diff_number(k, m))
            else
                call set_error(status, FM_GENERIC_ERROR, EDIFF_NO_DIFFERENCES)
            end if

            return
        end if

        overlay = ediff_sessions(k)%differences(buffer_type, m + 1)%overlay

        if (.not. is_buffer_live(overlay_buffer(overlay))) then
            call set_error(status, FM_GENERIC_ERROR, EDIFF_KILLED_VITAL_BUFFER)
            return
        end if

        if (at_end) then
            position = overlay_end(overlay)
        else
            position = overlay_start(overlay)
        end if
    end subroutine get_ediff_diff_position

    recursive module subroutine get_ediff_region_contents(k, n, buffer_type, text, status, start, end)
        integer, intent(in) :: k, n, buffer_type
        character(len=:), allocatable, intent(out) :: text
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: start, end
        type(handle_type) :: old
        type(status_type) :: st
        integer(int64) :: s, e

        text = ""

        if (present(start)) then
            s = start
        else
            call get_ediff_diff_position(k, buffer_type, .false., s, status, n)
            if (.not. status%ok()) return
        end if

        if (present(end)) then
            e = end
        else
            call get_ediff_diff_position(k, buffer_type, .true., e, status, n)
            if (.not. status%ok()) return
        end if

        old = current_buffer()
        call enter_ediff_buffer(ediff_sessions(k)%buffers(buffer_type), status)
        if (.not. status%ok()) return
        call buffer_substring(s, e, text, status)
        ! Make the old buffer current again, keeping the first error.
        call set_buffer(old, st)
        if (status%ok()) status = st
    end subroutine get_ediff_region_contents

    ! Restore highlighting to what it should be according to
    ! `ediff_use_faces', the highlighting style and
    ! `ediff_highlight_all_diffs'.  Port of
    ! ediff-util.el:ediff-restore-highlighting.
    recursive subroutine restore_ediff_highlighting(k, status)
        integer, intent(in) :: k
        type(status_type), intent(out) :: status

        status = status_type()

        if (has_ediff_face_support() .and. ediff_sessions(k)%use_faces .and. ediff_sessions(k)%highlight_all_diffs) then
            call paint_ediff_background_regions(k, .false., status)
            if (.not. status%ok()) return
        end if

        call select_ediff_difference(k, ediff_sessions(k)%current_difference, status)
    end subroutine restore_ediff_highlighting

    ! Delete the difference overlays of session K so they won't slow down
    ! future editing operations.  Port of
    ! ediff-util.el:ediff-clear-diff-vector (for the vectors of all the
    ! buffers; fine differences are deferred).
    subroutine clear_ediff_differences(k)
        integer, intent(in) :: k
        integer :: t, n

        if (.not. allocated(ediff_sessions(k)%differences)) return

        do n = 1, size(ediff_sessions(k)%differences, 2)
            do t = EDIFF_BUFFER_TYPE_A, EDIFF_BUFFER_TYPE_ANCESTOR
                if (is_overlay(ediff_sessions(k)%differences(t, n)%overlay)) then
                    call destroy_overlay(ediff_sessions(k)%differences(t, n)%overlay)
                end if
            end do
        end do

        deallocate (ediff_sessions(k)%differences)
        allocate (ediff_sessions(k)%differences(4, 0))
    end subroutine clear_ediff_differences

    ! Make the current difference overlay of session K in BUFFER_TYPE,
    ! when the display has faces.  Port of
    ! ediff-util.el:ediff-make-current-diff-overlay.
    recursive subroutine make_ediff_current_diff_overlay(k, buffer_type, status)
        integer, intent(in) :: k, buffer_type
        type(status_type), intent(out) :: status
        integer(int64) :: pos

        status = status_type()
        if (.not. has_ediff_face_support()) return
        ! (point-max) of the control buffer.
        pos = point_max()
        call make_ediff_bullet_proof_overlay(pos, pos, ediff_sessions(k)%buffers(buffer_type), &
                                             ediff_sessions(k)%current_diff_overlays(buffer_type), status)
        if (.not. status%ok()) return
        call set_ediff_overlay_face(ediff_sessions(k)%current_diff_overlays(buffer_type), &
                                    trim(EDIFF_CURRENT_DIFF_FACES(buffer_type)), status)
    end subroutine make_ediff_current_diff_overlay

    ! Narrow buffers A, B (and C) of session K to the visible bounds.
    ! Port of ediff-util.el:ediff-visible-region.
    recursive subroutine make_ediff_region_visible(k, status)
        integer, intent(in) :: k
        type(status_type), intent(out) :: status
        type(handle_type) :: old
        type(status_type) :: st
        integer :: t

        status = status_type()

        associate (b => ediff_sessions(k)%buffers)
            if (same_handle(b(EDIFF_BUFFER_TYPE_A), b(EDIFF_BUFFER_TYPE_B))) return
            if (same_handle(b(EDIFF_BUFFER_TYPE_A), b(EDIFF_BUFFER_TYPE_C))) return
            if (same_handle(b(EDIFF_BUFFER_TYPE_C), b(EDIFF_BUFFER_TYPE_B))) return
        end associate

        ! If ediff-*-regions/windows, ediff-visible-bounds is already set.
        ! Otherwise, always use full range.
        if (.not. ediff_sessions(k)%narrow_job) ediff_sessions(k)%visible_bounds = ediff_sessions(k)%wide_bounds
        old = current_buffer()

        do t = EDIFF_BUFFER_TYPE_A, EDIFF_BUFFER_TYPE_C
            if (t == EDIFF_BUFFER_TYPE_C .and. .not. ediff_sessions(k)%three_way_job) exit

            associate (overlay => ediff_sessions(k)%visible_bounds(t))
                if (.not. is_buffer_live(overlay_buffer(overlay))) cycle
                call enter_ediff_buffer(ediff_sessions(k)%buffers(t), status)
                if (status%ok()) call narrow_to_region(overlay_start(overlay), overlay_end(overlay), status)
            end associate

            if (.not. status%ok()) exit
        end do

        ! Make the old buffer current again, keeping the first error.
        call set_buffer(old, st)
        if (status%ok()) status = st

    contains

        ! Whether A and B are the same live buffer.
        logical function same_handle(a, b)
            type(handle_type), intent(in) :: a, b

            same_handle = a%index /= 0 .and. a%index == b%index .and. a%generation == b%generation
        end function same_handle
    end subroutine make_ediff_region_visible

    ! ------------------------------------------------------------------
    ! Files and buffers
    ! ------------------------------------------------------------------

    recursive module subroutine read_ediff_file_name(prompt, default_dir, default_file, result, status, no_dirs)
        character(len=*), intent(in) :: prompt, default_dir, default_file
        character(len=:), allocatable, intent(out) :: result
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: no_dirs
        character(len=:), allocatable :: dir, file, expanded, f, default
        logical :: dirs_not_allowed

        dirs_not_allowed = .false.
        if (present(no_dirs)) dirs_not_allowed = no_dirs

        ! Hack default-dir if it is not set.
        if (len(default_dir) > 0) then
            dir = default_dir
        else if (len(default_file) > 0) then
            dir = file_name_directory(default_file)
            if (len(dir) == 0) dir = buffers(current)%directory
        else
            dir = buffers(current)%directory
        end if

        call expand_file_name(dir, expanded, status)
        if (.not. status%ok()) return
        dir = file_name_as_directory(abbreviate_file_name(expanded))
        ! Strip the directory from default-file.
        file = file_name_nondirectory(default_file)
        default = dir
        if (len(file) > 0) default = file
        call read_file_name(prompt//" (default "//default//"): ", f, status, dir=dir, default_filename=default, &
                            mustmatch=REQUIRE_MATCH_YES)
        if (.not. status%ok()) return
        call expand_file_name(f, result, status, dir)
        if (.not. status%ok()) return

        ! If user entered a directory name, expand the default file in that
        ! directory.
        if (is_file_directory(result) .and. len(file) > 0) then
            f = result
            call expand_file_name(file, result, status, f)
            if (.not. status%ok()) return
        end if

        if (dirs_not_allowed .and. is_file_directory(result)) then
            call set_error(status, FM_USER_ERROR, "File "//result//" is a directory")
        end if
    end subroutine read_ediff_file_name

    recursive module subroutine create_ediff_temp_file(buffer, file, status, prefix)
        type(handle_type), intent(in) :: buffer
        character(len=:), allocatable, intent(out) :: file
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: prefix
        character(len=:), allocatable :: p, msg, saved_coding, f
        type(handle_type) :: old
        type(status_type) :: st
        logical :: had_coding

        p = "ediff"
        if (present(prefix)) p = prefix
        call create_temp_file(p, f, status)
        if (.not. status%ok()) return
        old = current_buffer()
        call enter_ediff_buffer(buffer, status)
        if (.not. status%ok()) return
        had_coding = allocated(coding_system_for_write%value)
        if (had_coding) saved_coding = coding_system_for_write%value
        coding_system_for_write = ediff_coding_system_for_write
        ! Create the file.
        call write_region(f, .false., status, point_min(), point_max(), message=msg)

        if (had_coding) then
            coding_system_for_write = saved_coding
        else
            call coding_system_for_write%clear()
        end if

        ! Make the old buffer current again, keeping the first error.
        call set_buffer(old, st)
        if (status%ok()) status = st
        if (.not. status%ok()) return
        call set_file_modes(f, int(ediff_temp_file_mode, kind(0)), status)
        if (.not. status%ok()) return
        call expand_file_name(f, file, status)
    end subroutine create_ediff_temp_file

    recursive module subroutine verify_ediff_file_buffer(status)
        type(status_type), intent(out) :: status
        type(status_type) :: st
        logical :: answer, modified

        status = status_type()

        ! First check if the file has been modified since the buffer
        ! visited it.
        if (is_visited_file_modtime_current()) then
            call is_buffer_modified(current_buffer(), modified, status)
            if (.not. status%ok()) return
            if (.not. modified) return
            ! If buffer is not obsolete and is modified, offer to save.
            call read_yes_or_no("Buffer "//buffers(current)%name//" has been modified.  Save it in file "// &
                                buffers(current)%filename//"?", answer, status)
            if (.not. status%ok()) return

            if (answer) then
                ! An error is shown, as upstream's condition-case does.
                call save_buffer(1_int64, .false., st)

                if (.not. st%ok()) then
                    call ding()
                    call message("Couldn't save "//buffers(current)%filename)
                end if
            else
                call set_error(status, FM_USER_ERROR, "Buffer is out of sync for file "//buffers(current)%filename)
            end if
        else
            ! If buffer is obsolete, offer to revert.
            call read_yes_or_no("File "//buffers(current)%filename//" was modified since visited by buffer "// &
                                buffers(current)%name//".  REVERT file "//buffers(current)%filename//"? ", answer, status)
            if (.not. status%ok()) return

            if (answer) then
                call revert_buffer(status, ignore_auto=.true., noconfirm=.true.)
            else
                call set_error(status, FM_USER_ERROR, "Buffer out of sync for file "//buffers(current)%filename)
            end if
        end if
    end subroutine verify_ediff_file_buffer

    recursive module subroutine ediff_other_buffer(excluded, name)
        character(len=*), intent(in) :: excluded(:)
        character(len=:), allocatable, intent(out) :: name
        type(handle_type), allocatable :: all_buffers(:)
        type(handle_type) :: preferred_buffer, first
        integer :: j, use_dired_major_mode, dired_count, first_dired
        character(len=:), allocatable :: bname
        logical :: is_dired

        all_buffers = buffer_list()
        preferred_buffer = null_handle()
        if (size(all_buffers) > 0) preferred_buffer = all_buffers(1)
        ! If at least one buffer on the exclusion list is dired, then force
        ! all others to be dired.
        use_dired_major_mode = -1

        if (size(excluded) > 0) then
            first = get_buffer(trim(excluded(1)))

            if (is_buffer_live(first)) then
                use_dired_major_mode = 0
                if (buffers(first%index)%major_mode == "dired_mode") use_dired_major_mode = 1
            end if
        end if

        dired_count = 0
        first_dired = 0

        ! Significant buffers must be visible and not belong to the
        ! exclusion list.
        do j = 1, size(all_buffers)
            bname = buffers(all_buffers(j)%index)%name
            is_dired = buffers(all_buffers(j)%index)%major_mode == "dired_mode"
            if (any(excluded == bname)) cycle
            if (.not. is_window_live(get_ediff_visible_buffer_window(all_buffers(j)))) cycle

            if (all_buffers(j)%index == preferred_buffer%index) then
                name = bname
                return
            end if

            if (use_dired_major_mode == 1) then
                if (is_dired) then
                    name = bname
                    return
                end if

                cycle
            end if

            if (is_dired) then
                ! Remember this buffer in case dired buffs are the only
                ! ones visible.
                if (use_dired_major_mode == -1) then
                    dired_count = dired_count + 1
                    first_dired = j
                end if

                cycle
            end if

            if (index(bname, " ") == 1 .or. index(bname, "*") == 1) cycle
            if (bname == "*scratch*") cycle
            name = bname
            return
        end do

        if (dired_count > 0) then
            name = buffers(all_buffers(first_dired)%index)%name
            return
        end if

        ! Try also buffers that are not displayed in windows.
        do j = 1, size(all_buffers)
            bname = buffers(all_buffers(j)%index)%name
            is_dired = buffers(all_buffers(j)%index)%major_mode == "dired_mode"
            if (any(excluded == bname)) cycle

            if (use_dired_major_mode == 1) then
                if (.not. is_dired) cycle
            else if (is_dired) then
                cycle
            end if

            if (index(bname, " ") == 1 .or. index(bname, "*") == 1) cycle
            if (bname == "*scratch*") cycle
            name = bname
            return
        end do

        name = "*scratch*"
    end subroutine ediff_other_buffer

    ! ------------------------------------------------------------------
    ! Commands
    ! ------------------------------------------------------------------

    ! The last key of the command, as a character ("" if none).
    ! Not a port: what the commands do with last-command-event.
    function last_command_char() result(c)
        character(len=:), allocatable :: c

        c = ""
        if (vtag(last_command_event) /= V_INTEGER) return
        if (xfixnum(last_command_event) < 0 .or. xfixnum(last_command_event) > 127) return
        c = achar(int(xfixnum(last_command_event)))
    end function last_command_char

    recursive subroutine cmd_next_ediff_difference(status)
        type(status_type), intent(out) :: status

        call next_ediff_difference(status, prefix_numeric_value(current_prefix_arg))
    end subroutine cmd_next_ediff_difference

    recursive subroutine cmd_previous_ediff_difference(status)
        type(status_type), intent(out) :: status

        call previous_ediff_difference(status, prefix_numeric_value(current_prefix_arg))
    end subroutine cmd_previous_ediff_difference

    recursive subroutine cmd_jump_to_ediff_difference(status)
        type(status_type), intent(out) :: status

        call jump_to_ediff_difference(prefix_numeric_value(current_prefix_arg), status)
    end subroutine cmd_jump_to_ediff_difference

    recursive subroutine cmd_jump_to_ediff_difference_at_point(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: c

        c = last_command_char()
        if (len(c) == 0) c = "a"
        call jump_to_ediff_difference_at_point(ediff_char_to_buffer_type(c), current_prefix_arg%kind /= PREFIX_NONE, &
                                               status)
    end subroutine cmd_jump_to_ediff_difference_at_point

    recursive subroutine cmd_recenter_ediff(status)
        type(status_type), intent(out) :: status

        call recenter_ediff(status)
    end subroutine cmd_recenter_ediff

    recursive subroutine cmd_quit_ediff(status)
        type(status_type), intent(out) :: status

        call quit_ediff(current_prefix_arg%kind /= PREFIX_NONE, status)
    end subroutine cmd_quit_ediff

    recursive subroutine cmd_restore_ediff_diff(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: c

        c = last_command_char()
        if (len(c) == 0) c = " "
        call restore_ediff_diff(current_prefix_arg, c(1:1), status)
    end subroutine cmd_restore_ediff_diff

    !> Copy ARGth difference region from buffer A to B.
    !> ARG is a prefix argument.  If absent, copy the current difference
    !> region.  Port of ediff-util.el:ediff-copy-A-to-B.
    recursive subroutine copy_ediff_a_to_b(status)
        type(status_type), intent(out) :: status

        call copy_ediff_diff_to_diff(current_prefix_arg, "ab", status)
    end subroutine copy_ediff_a_to_b

    !> Copy ARGth difference region from buffer B to A.
    !> ARG is a prefix argument.  If absent, copy the current difference
    !> region.  Port of ediff-util.el:ediff-copy-B-to-A.
    recursive subroutine copy_ediff_b_to_a(status)
        type(status_type), intent(out) :: status

        call copy_ediff_diff_to_diff(current_prefix_arg, "ba", status)
    end subroutine copy_ediff_b_to_a

    !> Copy ARGth difference region from buffer A to buffer C.
    !> ARG is a prefix argument.  If absent, copy the current difference
    !> region.  Port of ediff-util.el:ediff-copy-A-to-C.
    recursive subroutine copy_ediff_a_to_c(status)
        type(status_type), intent(out) :: status

        call copy_ediff_diff_to_diff(current_prefix_arg, "ac", status)
    end subroutine copy_ediff_a_to_c

    !> Copy ARGth difference region from buffer B to buffer C.
    !> ARG is a prefix argument.  If absent, copy the current difference
    !> region.  Port of ediff-util.el:ediff-copy-B-to-C.
    recursive subroutine copy_ediff_b_to_c(status)
        type(status_type), intent(out) :: status

        call copy_ediff_diff_to_diff(current_prefix_arg, "bc", status)
    end subroutine copy_ediff_b_to_c

    !> Copy ARGth difference region from buffer C to B.
    !> ARG is a prefix argument.  If absent, copy the current difference
    !> region.  Port of ediff-util.el:ediff-copy-C-to-B.
    recursive subroutine copy_ediff_c_to_b(status)
        type(status_type), intent(out) :: status

        call copy_ediff_diff_to_diff(current_prefix_arg, "cb", status)
    end subroutine copy_ediff_c_to_b

    !> Copy ARGth difference region from buffer C to A.
    !> ARG is a prefix argument.  If absent, copy the current difference
    !> region.  Port of ediff-util.el:ediff-copy-C-to-A.
    recursive subroutine copy_ediff_c_to_a(status)
        type(status_type), intent(out) :: status

        call copy_ediff_diff_to_diff(current_prefix_arg, "ca", status)
    end subroutine copy_ediff_c_to_a

    module subroutine init_ediff_util()
        if (associated(commands%quit_ediff%execute)) return
        commands%next_ediff_difference%execute => cmd_next_ediff_difference
        commands%previous_ediff_difference%execute => cmd_previous_ediff_difference
        commands%jump_to_ediff_difference%execute => cmd_jump_to_ediff_difference
        commands%jump_to_ediff_difference_at_point%execute => cmd_jump_to_ediff_difference_at_point
        commands%quit_ediff%execute => cmd_quit_ediff
        commands%recenter_ediff%execute => cmd_recenter_ediff
        commands%toggle_ediff_split%execute => toggle_ediff_split
        commands%toggle_ediff_highlighting%execute => toggle_ediff_highlighting
        commands%toggle_ediff_help%execute => toggle_ediff_help
        commands%update_ediff_diffs%execute => update_ediff_diffs
        commands%copy_ediff_a_to_b%execute => copy_ediff_a_to_b
        commands%copy_ediff_b_to_a%execute => copy_ediff_b_to_a
        commands%copy_ediff_a_to_c%execute => copy_ediff_a_to_c
        commands%copy_ediff_b_to_c%execute => copy_ediff_b_to_c
        commands%copy_ediff_c_to_b%execute => copy_ediff_c_to_b
        commands%copy_ediff_c_to_a%execute => copy_ediff_c_to_a
        commands%restore_ediff_diff%execute => cmd_restore_ediff_diff
        call ediff_quit_hook%prepend(clean_up_ediff_mess)
    end subroutine init_ediff_util
end submodule ediff_util_session_mod
