! fortmacs_diff_utilities_session_mod.f90 -- the Ediff session's commands.
! Ported from GNU Emacs 31.1 lisp/vc/ediff-util.el (in part: comparing
! two and three buffers and merging on a terminal frame).
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

! The procedures of fortmacs_diff_utilities_mod that use fortmacs_diff_window_mod and
! fortmacs_diff_diff_mod (which use fortmacs_diff_utilities_mod), with the commands.
submodule(fortmacs_diff_utilities_mod) fortmacs_diff_utilities_session_mod
    use error_mod, only: set_error, substitute_quotes, GENERIC_ERROR_CODE, USER_ERROR_CODE
    use handle_mod, only: null_handle
    use value_mod, only: vtag, xfixnum, V_INTEGER
    use buffer_mod, only: buffers, current, get_or_make_buffer, buffer_list, is_buffer_modified
    use data_mod, only: clear_all_local_variables, choice_state
    use editing_functions_mod, only: point, goto_character, widen, insert_buffer_substring
    use derived_mod, only: major_mode_type, major_mode_named
    use editing_commands_mod, only: backward_character
    use keymap_mod, only: make_sparse_keymap, keymap_set, use_local_map, destroy_keymap
    use keyboard_mod, only: last_command_event, sit_for
    use execute_interactively_mod, only: this_command, current_prefix_argument, prefix_numeric_value, PREFIX_NONE, PREFIX_NUMBER, &
                                         PREFIX_RAW, PREFIX_MINUS
    use commands_mod, only: commands
    use interval_tree_mod, only: overlay_start
    use window_mod, only: windows, select_window, delete_window, is_window_live, recenter, switch_to_buffer, &
                          split_window_below, split_window_right, window_minimum_height
    use frame_mod, only: selected_frame
    use file_input_output_mod, only: expand_file_name, file_name_directory, file_name_nondirectory, file_name_as_directory, &
                                     is_file_directory, write_region, set_file_modes, is_visited_file_modtime_current, file_exists
    use files_mod, only: create_temporary_file, abbreviate_file_name, delete_file, save_buffer, revert_buffer, &
                         write_file_functions
    use minibuffer_mod, only: read_file_name, REQUIRE_MATCH_YES
    use basic_subprograms_mod, only: read_y_or_n
    use utility_functions_mod, only: read_yes_or_no
    use coding_mod, only: coding_system_for_write
    use display_update_mod, only: ding
    use display_engine_mod, only: message
    use fortmacs_diff_settings_mod, only: fortmacs_diff_sessions, fortmacs_diff_session_index, FORTMACS_DIFF_SELECT_ONLY, &
                                          FORTMACS_DIFF_UNSELECT_ONLY, make_fortmacs_diff_session_entry, &
                                          drop_fortmacs_diff_session, has_fortmacs_diff_face_support, &
                                          is_fortmacs_diff_buffer_job, fortmacs_diff_character_to_buffer_type, &
                                          check_fortmacs_diff_control_buffer, close_fortmacs_diff_buffer_carefully, &
                                          fortmacs_diff_use_faces, fortmacs_diff_highlight_all_diffs, &
                                          fortmacs_diff_keep_variants, fortmacs_diff_make_buffers_readonly_at_startup, &
                                          fortmacs_diff_fortmacs_help_in_control_buffer, fortmacs_diff_temporary_file_mode, &
                                          fortmacs_diff_coding_system_for_write, fortmacs_diff_before_setup_hook, &
                                          fortmacs_diff_startup_hook, fortmacs_diff_prepare_buffer_hook, &
                                          fortmacs_diff_mode_hook, fortmacs_diff_keymap_setup_hook, fortmacs_diff_quit_hook, &
                                          FORTMACS_DIFF_KILLED_VITAL_BUFFER, FORTMACS_DIFF_BUFFER_TYPE_A, &
                                          FORTMACS_DIFF_BUFFER_TYPE_B, FORTMACS_DIFF_BUFFER_TYPE_C, &
                                          FORTMACS_DIFF_BUFFER_TYPE_ANCESTOR, FORTMACS_DIFF_HIGHLIGHTING_STYLE_FACE, &
                                          FORTMACS_DIFF_HIGHLIGHTING_STYLE_ASCII, FORTMACS_DIFF_HIGHLIGHTING_STYLE_NONE, &
                                          fortmacs_diff_show_ancestor, fortmacs_diff_autostore_merges, &
                                          fortmacs_diff_split_window_procedure
    use fortmacs_diff_help_mod, only: fortmacs_diff_use_long_help_message
    use fortmacs_diff_window_mod, only: fortmacs_diff_split_window_function, fortmacs_diff_merge_split_window_function, &
                                        set_up_fortmacs_diff_windows, get_fortmacs_diff_visible_buffer_window
    use fortmacs_diff_diff_mod, only: set_fortmacs_diff_actual_diff_options, fortmacs_diff_actual_diff_options
    use fortmacs_diff_multiple_mod, only: fortmacs_diff_session_registry, update_fortmacs_diff_registry, &
                                          update_fortmacs_diff_meta_buffer
    use fortmacs_diff_merging_mod, only: fortmacs_diff_default_variant, fortmacs_diff_show_clashes_only, &
                                         fortmacs_diff_skip_merge_regions_that_differ_from_default, &
                                         fortmacs_diff_quit_merge_hook, set_fortmacs_diff_merge_mode, &
                                         FORTMACS_DIFF_DEFAULT_VARIANT_DEFAULT_B
    implicit none(type, external)

contains

    ! ------------------------------------------------------------------
    ! Helpers
    ! ------------------------------------------------------------------

    recursive module subroutine enter_fortmacs_diff_buffer(buffer, status)
        type(handle_type), intent(in) :: buffer
        type(status_type), intent(out) :: status

        status = status_type()

        if (is_buffer_live(buffer)) then
            call set_buffer(buffer, status)
        else if (.not. associated(this_command, commands%quit_fortmacs_diff)) then
            call set_error(status, GENERIC_ERROR_CODE, FORTMACS_DIFF_KILLED_VITAL_BUFFER)
        end if
    end subroutine enter_fortmacs_diff_buffer

    recursive module subroutine select_window_begin(window, old_window, old_buffer, status)
        type(handle_type), intent(in) :: window
        type(handle_type), intent(out) :: old_window, old_buffer
        type(status_type), intent(out) :: status

        old_window = selected_frame%selected_window
        old_buffer = current_buffer()
        call select_window(window, status, norecord=.true.)
    end subroutine select_window_begin

    recursive module subroutine select_window_end(old_window, old_buffer, status)
        type(handle_type), intent(in) :: old_window, old_buffer
        type(status_type), intent(out) :: status

        status = status_type()
        if (is_window_live(old_window)) call select_window(old_window, status, norecord=.true.)
        if (.not. status%ok()) return
        if (is_buffer_live(old_buffer)) call set_buffer(old_buffer, status)
    end subroutine select_window_end

    ! ------------------------------------------------------------------
    ! The mode and its keymap
    ! ------------------------------------------------------------------

    recursive module subroutine fortmacs_diff_mode(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call clear_all_local_variables()
        buffers(current)%major_mode = "fortmacs_diff_mode"
        buffers(current)%mode_name = "Ediff"
        ! We use run-hooks instead of run-mode-hooks for one reason.  The
        ! ediff control buffer is read-only and it is not supposed to be
        ! modified by minor modes and such.
        call fortmacs_diff_mode_hook%execute(status)
    end subroutine fortmacs_diff_mode

    ! Set up the keymap used in the control buffer of session SESSION_INDEX
    ! (`fortmacs_diff_mode_map') and use it.  Port of
    ! ediff-util.el:ediff-setup-keymap with ediff-set-keys (the keys of
    ! the deferred commands are not bound).
    recursive subroutine set_up_fortmacs_diff_keymap(session_index, status)
        integer, intent(in) :: session_index
        type(status_type), intent(out) :: status
        type(handle_type) :: map
        character(len=1) :: digit
        integer :: d

        status = status_type()
        call make_sparse_keymap(map)
        fortmacs_diff_sessions(session_index)%keymap = map
        ! (suppress-keymap ediff-mode-map)
        call bind("<remap> <self_insert_command>", "undefined")
        call bind("-", "negative_argument")

        do d = 0, 9
            write (digit, "(i1)") d
            call bind(digit, "digit_argument")
        end do

        call bind("p", "previous_fortmacs_diff_difference")
        call bind("DEL", "previous_fortmacs_diff_difference")
        call bind("<delete>", "previous_fortmacs_diff_difference")
        if (.not. fortmacs_diff_fortmacs_help_in_control_buffer) call bind("C-h", "previous_fortmacs_diff_difference")
        call bind("<backspace>", "previous_fortmacs_diff_difference")
        call bind("S-SPC", "previous_fortmacs_diff_difference")
        call bind("n", "next_fortmacs_diff_difference")
        call bind("SPC", "next_fortmacs_diff_difference")
        call bind("j", "jump_to_fortmacs_diff_difference")
        call bind("g a", "jump_to_fortmacs_diff_difference_at_point")
        call bind("g b", "jump_to_fortmacs_diff_difference_at_point")
        call bind("q", "quit_fortmacs_diff")
        call bind("C-l", "recenter_fortmacs_diff")
        call bind("|", "toggle_fortmacs_diff_split")
        call bind("h", "toggle_fortmacs_diff_highlighting")
        call bind("?", "toggle_fortmacs_diff_help")
        call bind("!", "update_fortmacs_diff_diffs")
        call bind("M", "show_fortmacs_diff_current_session_meta_buffer")
        call bind("R", "show_fortmacs_diff_registry")

        if (fortmacs_diff_sessions(session_index)%merge_job) then
            ! Will barf if no ancestor.
            call bind("/", "toggle_fortmacs_diff_show_ancestor")
            ! In merging, we allow only A->C and B->C copying.
            call bind("a", "copy_fortmacs_diff_a_to_c")
            call bind("b", "copy_fortmacs_diff_b_to_c")
            call bind("r", "restore_fortmacs_diff_diff_in_merge_buffer")
            call bind("s", "shrink_fortmacs_diff_window_c")
            call bind("+", "combine_fortmacs_diff_diffs")
            call bind("$ $", "toggle_fortmacs_diff_show_clashes_only")
            call bind("$ *", "toggle_fortmacs_diff_skip_changed_regions")
            call bind("&", "regular_expression_merge_fortmacs_diff")
        else if (fortmacs_diff_sessions(session_index)%three_way_comparison_job) then
            call bind("a b", "copy_fortmacs_diff_a_to_b")
            call bind("b a", "copy_fortmacs_diff_b_to_a")
            call bind("a c", "copy_fortmacs_diff_a_to_c")
            call bind("b c", "copy_fortmacs_diff_b_to_c")
            call bind("c a", "copy_fortmacs_diff_c_to_a")
            call bind("c b", "copy_fortmacs_diff_c_to_b")
            call bind("r a", "restore_fortmacs_diff_diff")
            call bind("r b", "restore_fortmacs_diff_diff")
            call bind("r c", "restore_fortmacs_diff_diff")
        else
            ! 2-way comparison.
            call bind("a", "copy_fortmacs_diff_a_to_b")
            call bind("b", "copy_fortmacs_diff_b_to_a")
            call bind("r a", "restore_fortmacs_diff_diff")
            call bind("r b", "restore_fortmacs_diff_diff")
        end if

        call bind("# c", "toggle_fortmacs_diff_ignore_case")
        call bind("w a", "save_fortmacs_diff_buffer")
        call bind("w b", "save_fortmacs_diff_buffer")
        call bind("w d", "save_fortmacs_diff_buffer")

        if (fortmacs_diff_sessions(session_index)%three_way_job) then
            call bind("w c", "save_fortmacs_diff_buffer")
            call bind("g c", "jump_to_fortmacs_diff_difference_at_point")
        end if

        if (.not. status%ok()) return
        call use_local_map(map, status)
        if (.not. status%ok()) return
        call fortmacs_diff_keymap_setup_hook%execute(status)

    contains

        ! Bind KEY to COMMAND in the map unless an earlier binding failed.
        subroutine bind(key, command)
            character(len=*), intent(in) :: key, command

            if (.not. status%ok()) return
            call keymap_set(map, key, command, status)
        end subroutine bind
    end subroutine set_up_fortmacs_diff_keymap

    ! ------------------------------------------------------------------
    ! Setup
    ! ------------------------------------------------------------------

    recursive module subroutine set_up_fortmacs_diff(buffer_a, file_a, buffer_b, file_b, buffer_c, file_c, job_name, &
                                                     control_buffer, status, startup_hooks, merge_buffer_file)
        type(handle_type), intent(in) :: buffer_a, buffer_b, buffer_c
        character(len=*), intent(in) :: file_a, file_b, file_c
        type(command_type), pointer, intent(in) :: job_name
        type(handle_type), intent(out) :: control_buffer
        type(status_type), intent(out) :: status
        type(hook_type), intent(inout), optional :: startup_hooks
        character(len=*), intent(in), optional :: merge_buffer_file
        character(len=:), allocatable :: name, fa, fb, fc, merge_file
        type(handle_type) :: old, old_window, old_buffer, buffer_c_of_job
        type(status_type) :: st
        integer(int64) :: shift
        integer :: session_index, t, lt, gt, digits

        call fortmacs_diff_before_setup_hook%execute(status)
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

        if (present(merge_buffer_file)) then
            call expand_file_name(merge_buffer_file, merge_file, status)
            if (.not. status%ok()) return

            ! Check the directory exists.
            if (.not. file_exists(file_name_directory(merge_file))) then
                call set_error(status, GENERIC_ERROR_CODE, "Directory "// &
                               abbreviate_file_name(file_name_directory(merge_file))// &
                               " given as place to save the merge doesn't exist")
                return
            end if

            if (file_exists(merge_file) .and. is_file_directory(merge_file)) then
                call set_error(status, GENERIC_ERROR_CODE, "The merge buffer file "//abbreviate_file_name(merge_file)// &
                               " must not be a directory")
                return
            end if
        end if

        name = fortmacs_diff_unique_buffer_name("*Ediff Control Panel", "*")
        old = current_buffer()
        call set_buffer(buffer_a, status)
        if (.not. status%ok()) return
        call get_or_make_buffer(name, control_buffer, status)
        if (.not. status%ok()) return
        call set_buffer(control_buffer, status)
        if (.not. status%ok()) return
        call fortmacs_diff_mode(status)
        if (.not. status%ok()) return
        call make_fortmacs_diff_session_entry(control_buffer, session_index)

        associate (s => fortmacs_diff_sessions(session_index))
            s%use_long_help_message = fortmacs_diff_use_long_help_message
            s%split_window_function => fortmacs_diff_split_window_function
            s%default_variant = choice_state(fortmacs_diff_default_variant)
            s%keep_variants = fortmacs_diff_keep_variants
            s%use_faces = fortmacs_diff_use_faces
            s%highlight_all_diffs = fortmacs_diff_highlight_all_diffs
            ! The buffer-local user options of ediff-merg.el and
            ! ediff-init.el.
            s%autostore_merges = choice_state(fortmacs_diff_autostore_merges)
            s%show_clashes_only = fortmacs_diff_show_clashes_only
            s%skip_merge_regions_that_differ_from_default = fortmacs_diff_skip_merge_regions_that_differ_from_default

            if (has_fortmacs_diff_face_support() .and. s%use_faces) then
                s%highlighting_style = FORTMACS_DIFF_HIGHLIGHTING_STYLE_FACE
            else
                s%highlighting_style = FORTMACS_DIFF_HIGHLIGHTING_STYLE_ASCII
            end if

            ! Unwrap set up parameters passed as argument.
            s%job_name => job_name
            ! Set variables classifying the current ediff job.  The window
            ! and region jobs are Deferred (docs/status/fortmacs_diff_mod.md): their
            ! classes stay .false.
            s%three_way_comparison_job = associated(job_name, commands%fortmacs_diff_files3) .or. &
                                         associated(job_name, commands%fortmacs_diff_buffers3)
            s%merge_with_ancestor_job = associated(job_name, commands%fortmacs_diff_merge_files_with_ancestor) .or. &
                                        associated(job_name, commands%fortmacs_diff_merge_buffers_with_ancestor)
            s%merge_job = s%merge_with_ancestor_job .or. associated(job_name, commands%fortmacs_diff_merge_files) .or. &
                          associated(job_name, commands%fortmacs_diff_merge_buffers)
            s%three_way_job = s%three_way_comparison_job .or. s%merge_job
            s%diff3_job = s%three_way_comparison_job .or. s%merge_with_ancestor_job
            ! Don't delete variants in case of ediff-buffer-* jobs without
            ! asking.  This is because one may lose work---dangerous.
            if (is_fortmacs_diff_buffer_job(job_name)) s%keep_variants = .true.
        end associate

        buffer_c_of_job = buffer_c

        ! Adjust for merge jobs.
        if (fortmacs_diff_sessions(session_index)%merge_job) then
            fortmacs_diff_sessions(session_index)%split_window_function => fortmacs_diff_merge_split_window_function
            ! Remember the ancestor buffer, if any.
            fortmacs_diff_sessions(session_index)%buffers(FORTMACS_DIFF_BUFFER_TYPE_ANCESTOR) = buffer_c
            call make_fortmacs_diff_merge_buffer(buffer_c_of_job, status)
            if (.not. status%ok()) return
        end if

        buffers(current)%read_only = .false.

        associate (s => fortmacs_diff_sessions(session_index))
            s%buffers(FORTMACS_DIFF_BUFFER_TYPE_A) = buffer_a
            s%buffers(FORTMACS_DIFF_BUFFER_TYPE_B) = buffer_b
            s%buffers(FORTMACS_DIFF_BUFFER_TYPE_C) = buffer_c_of_job
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

        call get_or_make_buffer(fortmacs_diff_unique_buffer_name("*ediff-errors", "*"), &
                                fortmacs_diff_sessions(session_index)%error_buffer, &
                                status)
        if (.not. status%ok()) return
        buffers(fortmacs_diff_sessions(session_index)%error_buffer%index)%undo_enabled = .false.
        if (.not. allocated(fortmacs_diff_actual_diff_options)) call set_fortmacs_diff_actual_diff_options(.false.)

        do t = FORTMACS_DIFF_BUFFER_TYPE_A, FORTMACS_DIFF_BUFFER_TYPE_C
            call make_fortmacs_diff_bullet_proof_overlay(0_int64, 0_int64, fortmacs_diff_sessions(session_index)%buffers(t), &
                                                         fortmacs_diff_sessions(session_index)%wide_bounds(t), status, whole=.true.)
            if (.not. status%ok()) return
        end do

        ! This has effect only on ediff-windows/regions.
        fortmacs_diff_sessions(session_index)%visible_bounds = fortmacs_diff_sessions(session_index)%wide_bounds
        call set_up_fortmacs_diff_keymap(session_index, status)
        if (.not. status%ok()) return
        fortmacs_diff_sessions(session_index)%narrow_bounds = fortmacs_diff_sessions(session_index)%wide_bounds

        ! These won't run if there are errors in diff.
        do t = FORTMACS_DIFF_BUFFER_TYPE_A, FORTMACS_DIFF_BUFFER_TYPE_C
            if (t == FORTMACS_DIFF_BUFFER_TYPE_C .and. .not. fortmacs_diff_sessions(session_index)%three_way_job) exit
            call set_buffer(fortmacs_diff_sessions(session_index)%buffers(t), status)
            if (.not. status%ok()) return
            ! The merge buffer should never be narrowed.
            if (t == FORTMACS_DIFF_BUFFER_TYPE_C .and. fortmacs_diff_sessions(session_index)%merge_job) call widen()
            call fortmacs_diff_prepare_buffer_hook%execute(status)
            if (.not. status%ok()) return

            if (fortmacs_diff_sessions(session_index)%merge_job .and. t /= FORTMACS_DIFF_BUFFER_TYPE_C) then
                buffers(current)%read_only = .true.
            end if

            if (fortmacs_diff_make_buffers_readonly_at_startup) then
                buffers(current)%read_only = .true.
            else if (t == FORTMACS_DIFF_BUFFER_TYPE_C) then
                buffers(current)%read_only = .false.
            end if
        end do

        if (is_buffer_live(fortmacs_diff_sessions(session_index)%buffers(FORTMACS_DIFF_BUFFER_TYPE_ANCESTOR))) then
            call set_buffer(fortmacs_diff_sessions(session_index)%buffers(FORTMACS_DIFF_BUFFER_TYPE_ANCESTOR), status)
            if (.not. status%ok()) return
            buffers(current)%read_only = .true.
            call fortmacs_diff_prepare_buffer_hook%execute(status)
            if (.not. status%ok()) return
        end if

        call set_buffer(control_buffer, status)
        if (.not. status%ok()) return

        ! The following must be after setting up ediff-narrow-bounds.
        if (fortmacs_diff_sessions(session_index)%diff3_job) then
            call fortmacs_diff_sessions(session_index)%set_up_difference_regions3(fa, fb, fc, status)
        else
            call fortmacs_diff_sessions(session_index)%set_up_difference_regions(fa, fb, status)
        end if

        if (.not. status%ok()) return
        call set_buffer(control_buffer, status)
        if (.not. status%ok()) return
        fortmacs_diff_sessions(session_index)%number_of_differences = size(fortmacs_diff_sessions(session_index)%differences, 2)
        fortmacs_diff_sessions(session_index)%current_difference = -1

        do t = FORTMACS_DIFF_BUFFER_TYPE_A, FORTMACS_DIFF_BUFFER_TYPE_ANCESTOR
            if (t == FORTMACS_DIFF_BUFFER_TYPE_C .and. .not. fortmacs_diff_sessions(session_index)%three_way_job) cycle
            if (t == FORTMACS_DIFF_BUFFER_TYPE_ANCESTOR &
                .and. .not. fortmacs_diff_sessions(session_index)%merge_with_ancestor_job) cycle
            call fortmacs_diff_sessions(session_index)%make_current_difference_overlay(t, status)
            if (.not. status%ok()) return
        end do

        call set_up_fortmacs_diff_windows(buffer_a, buffer_b, buffer_c_of_job, control_buffer, status)
        if (.not. status%ok()) return

        ! Position point in buffers A, B (and C).
        do t = FORTMACS_DIFF_BUFFER_TYPE_A, FORTMACS_DIFF_BUFFER_TYPE_C
            if (t == FORTMACS_DIFF_BUFFER_TYPE_C .and. .not. fortmacs_diff_sessions(session_index)%three_way_job) exit
            if (.not. is_window_live(fortmacs_diff_sessions(session_index)%windows(t))) cycle
            shift = overlay_start(fortmacs_diff_sessions(session_index)%narrow_bounds(t))
            call select_window_begin(fortmacs_diff_sessions(session_index)%windows(t), old_window, old_buffer, status)
            if (status%ok()) call goto_character(shift)
            ! Select the old window again, keeping the first error.
            call select_window_end(old_window, old_buffer, st)
            if (status%ok()) status = st
            if (.not. status%ok()) return
        end do

        if (fortmacs_diff_select_control_window_on_setup &
            .and. is_window_live(fortmacs_diff_sessions(session_index)%control_window)) then
            call select_window(fortmacs_diff_sessions(session_index)%control_window, status)
            if (.not. status%ok()) return
        end if

        call set_buffer(control_buffer, status)
        if (.not. status%ok()) return
        call fortmacs_diff_sessions(session_index)%make_region_visible(status)
        if (.not. status%ok()) return

        if (present(startup_hooks)) then
            call startup_hooks%execute(status)
            if (.not. status%ok()) return
        end if

        call set_buffer(control_buffer, status)
        if (.not. status%ok()) return

        if (allocated(merge_file)) then
            call fortmacs_diff_sessions(session_index)%arrange_autosave_in_merge_jobs(status, merge_file)
        else
            call fortmacs_diff_sessions(session_index)%arrange_autosave_in_merge_jobs(status)
        end if

        if (.not. status%ok()) return
        call set_buffer(control_buffer, status)
        if (.not. status%ok()) return
        call fortmacs_diff_sessions(session_index)%refresh_mode_lines()
        buffers(current)%read_only = .true.
        if (.not. allocated(fortmacs_diff_session_registry)) allocate (fortmacs_diff_session_registry(0))
        fortmacs_diff_session_registry = [control_buffer, fortmacs_diff_session_registry]
        call update_fortmacs_diff_registry(status)
        if (.not. status%ok()) return

        if (is_buffer_live(fortmacs_diff_sessions(session_index)%meta_buffer)) then
            call update_fortmacs_diff_meta_buffer(fortmacs_diff_sessions(session_index)%meta_buffer, status, &
                                                  session_number=fortmacs_diff_sessions(session_index)%meta_session_number)
            if (.not. status%ok()) return
        end if

        call set_buffer(control_buffer, status)
        if (.not. status%ok()) return
        call fortmacs_diff_startup_hook%execute(status)

        ! Make the old buffer current again, keeping the first error.
        if (is_buffer_live(old)) then
            call set_buffer(old, st)
            if (status%ok()) status = st
        end if

    contains

        ! Make the merge buffer *ediff-merge* in BUFFER, a copy of buffer A
        ! (or of buffer B with the default variant default-B) in its major
        ! mode, whose own `write_file_functions' give it its mode when it
        ! is saved.  The control buffer is current again afterwards.
        recursive subroutine make_fortmacs_diff_merge_buffer(buffer, status)
            type(handle_type), intent(out) :: buffer
            type(status_type), intent(out) :: status
            type(handle_type) :: variant
            type(major_mode_type), pointer :: mode

            ! If default variant is `combined', the right stuff is inserted
            ! by merge_differences.
            variant = buffer_a

            if (fortmacs_diff_sessions(session_index)%default_variant%choice == FORTMACS_DIFF_DEFAULT_VARIANT_DEFAULT_B) then
                variant = buffer_b
            end if

            call get_or_make_buffer(fortmacs_diff_unique_buffer_name("*ediff-merge", "*"), buffer, status)
            if (.not. status%ok()) return
            call set_buffer(buffer, status)
            if (.not. status%ok()) return
            call insert_buffer_substring(variant, status)
            if (.not. status%ok()) return
            call goto_character(point_minimum())
            mode => null()
            if (allocated(buffers(variant%index)%major_mode)) mode => major_mode_named(buffers(variant%index)%major_mode)

            if (associated(mode)) then
                if (associated(mode%execute)) call mode%execute(status)
                if (.not. status%ok()) return
            end if

            ! Merge buffer is always widened.
            call widen()
            call write_file_functions%prepend(set_fortmacs_diff_merge_mode, local=.true.)
            call set_buffer(control_buffer, status)
        end subroutine make_fortmacs_diff_merge_buffer
    end subroutine set_up_fortmacs_diff

    ! ------------------------------------------------------------------
    ! Commands for working with Ediff
    ! ------------------------------------------------------------------

    recursive module subroutine update_fortmacs_diff_diffs(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: old
        type(status_type) :: st
        character(len=:), allocatable :: file_a, file_b, file_c, file_ancestor
        integer(int64) :: point_a
        integer :: session_index, diff_no

        call check_fortmacs_diff_control_buffer(session_index, status)
        if (.not. status%ok()) return
        old = current_buffer()
        call enter_fortmacs_diff_buffer(fortmacs_diff_sessions(session_index)%buffers(FORTMACS_DIFF_BUFFER_TYPE_A), status)
        if (.not. status%ok()) return
        point_a = point()
        call set_buffer(old, status)
        if (.not. status%ok()) return
        call fortmacs_diff_sessions(session_index)%unselect_and_select_difference(-1, status)
        if (.not. status%ok()) return
        call make_temporary(FORTMACS_DIFF_BUFFER_TYPE_A, file_a)
        if (.not. status%ok()) return
        call make_temporary(FORTMACS_DIFF_BUFFER_TYPE_B, file_b)
        if (.not. status%ok()) return
        file_c = ""

        if (fortmacs_diff_sessions(session_index)%three_way_job) then
            call make_temporary(FORTMACS_DIFF_BUFFER_TYPE_C, file_c)
            if (.not. status%ok()) return
        end if

        file_ancestor = ""

        if (fortmacs_diff_sessions(session_index)%merge_with_ancestor_job) then
            call make_temporary(FORTMACS_DIFF_BUFFER_TYPE_ANCESTOR, file_ancestor)
            if (.not. status%ok()) return
        end if

        call fortmacs_diff_sessions(session_index)%clear_differences()

        ! Invalidate the replaced regions.
        if (allocated(fortmacs_diff_sessions(session_index)%replaced_regions)) then
            deallocate (fortmacs_diff_sessions(session_index)%replaced_regions)
        end if

        call set_buffer(fortmacs_diff_sessions(session_index)%control_buffer, status)

        if (status%ok()) then
            if (fortmacs_diff_sessions(session_index)%merge_with_ancestor_job) then
                call fortmacs_diff_sessions(session_index)%set_up_difference_regions3(file_a, file_b, file_ancestor, status)
            else if (fortmacs_diff_sessions(session_index)%diff3_job) then
                call fortmacs_diff_sessions(session_index)%set_up_difference_regions3(file_a, file_b, file_c, status)
            else
                call fortmacs_diff_sessions(session_index)%set_up_difference_regions(file_a, file_b, status)
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

        if (len(file_ancestor) > 0) then
            call delete_file(file_ancestor, st)
            if (status%ok()) status = st
        end if

        if (.not. status%ok()) return
        fortmacs_diff_sessions(session_index)%number_of_differences = size(fortmacs_diff_sessions(session_index)%differences, 2)

        if (fortmacs_diff_sessions(session_index)%three_way_job) then
            call fortmacs_diff_sessions(session_index)%set_all_difference_states(status)
            if (.not. status%ok()) return
        end if

        call set_buffer(fortmacs_diff_sessions(session_index)%control_buffer, status)
        if (.not. status%ok()) return
        call fortmacs_diff_sessions(session_index)%get_difference_at_point(FORTMACS_DIFF_BUFFER_TYPE_A, diff_no, status, &
                                                                           position=point_a)
        if (.not. status%ok()) return
        call jump_to_fortmacs_diff_difference(int(diff_no, int64), status)
        if (.not. status%ok()) return
        call message("")

    contains

        ! A temporary file with the text of buffer BUFFER_TYPE, named after
        ! its file.
        subroutine make_temporary(buffer_type, temporary)
            integer, intent(in) :: buffer_type
            character(len=:), allocatable, intent(out) :: temporary
            integer(int64) :: bi

            bi = fortmacs_diff_sessions(session_index)%buffers(buffer_type)%index

            if (is_buffer_live(fortmacs_diff_sessions(session_index)%buffers(buffer_type)) &
                .and. allocated(buffers(bi)%filename)) then
                call create_fortmacs_diff_temporary_file(fortmacs_diff_sessions(session_index)%buffers(buffer_type), &
                                                         temporary, status, &
                                                         file_name_nondirectory(buffers(bi)%filename))
            else
                call create_fortmacs_diff_temporary_file(fortmacs_diff_sessions(session_index)%buffers(buffer_type), &
                                                         temporary, status)
            end if
        end subroutine make_temporary
    end subroutine update_fortmacs_diff_diffs

    recursive module subroutine recenter_fortmacs_diff(status, no_rehighlight)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: no_rehighlight
        integer :: session_index
        logical :: flag

        call check_fortmacs_diff_control_buffer(session_index, status)
        if (.not. status%ok()) return
        flag = .false.
        if (present(no_rehighlight)) flag = no_rehighlight
        call fortmacs_diff_sessions(session_index)%recenter(flag, status)
    end subroutine recenter_fortmacs_diff

    recursive module subroutine toggle_fortmacs_diff_split(status)
        type(status_type), intent(out) :: status
        integer :: session_index

        call check_fortmacs_diff_control_buffer(session_index, status)
        if (.not. status%ok()) return

        ! The windows of buffers A, B and C are on one frame.
        if (associated(fortmacs_diff_sessions(session_index)%split_window_function, split_window_below)) then
            fortmacs_diff_sessions(session_index)%split_window_function => split_window_right
        else
            fortmacs_diff_sessions(session_index)%split_window_function => split_window_below
        end if

        call fortmacs_diff_sessions(session_index)%recenter(.true., status)
    end subroutine toggle_fortmacs_diff_split

    recursive module subroutine toggle_fortmacs_diff_highlighting(status)
        type(status_type), intent(out) :: status
        integer :: session_index
        logical :: faces

        call check_fortmacs_diff_control_buffer(session_index, status)
        if (.not. status%ok()) return
        call fortmacs_diff_sessions(session_index)%unselect_and_select_difference( &
            fortmacs_diff_sessions(session_index)%current_difference, status, FORTMACS_DIFF_UNSELECT_ONLY)
        if (.not. status%ok()) return
        faces = has_fortmacs_diff_face_support()

        ! Cycle through highlighting.
        associate (s => fortmacs_diff_sessions(session_index))
            if (s%use_faces .and. faces .and. s%highlight_all_diffs) then
                call message("Unhighlighting unselected difference regions")
                s%highlight_all_diffs = .false.
                s%highlighting_style = FORTMACS_DIFF_HIGHLIGHTING_STYLE_FACE
            else if ((s%use_faces .and. faces .and. s%highlighting_style == FORTMACS_DIFF_HIGHLIGHTING_STYLE_FACE) .or. &
                     (.not. faces .and. s%highlighting_style == FORTMACS_DIFF_HIGHLIGHTING_STYLE_NONE)) then
                call message("Highlighting with ASCII flags")
                s%highlighting_style = FORTMACS_DIFF_HIGHLIGHTING_STYLE_ASCII
                s%highlight_all_diffs = .false.
                s%use_faces = .false.
            else if (s%highlighting_style == FORTMACS_DIFF_HIGHLIGHTING_STYLE_ASCII) then
                call message("ASCII highlighting flags removed")
                s%highlighting_style = FORTMACS_DIFF_HIGHLIGHTING_STYLE_NONE
                s%highlight_all_diffs = .false.
            else if (faces) then
                ! The other cases with face support.
                call message("Re-highlighting all difference regions")
                s%use_faces = .true.
                s%highlighting_style = FORTMACS_DIFF_HIGHLIGHTING_STYLE_FACE
                s%highlight_all_diffs = .true.
            end if

            call fortmacs_diff_sessions(session_index)%paint_background_regions(.not. (s%use_faces &
                                                                                       .and. s%highlight_all_diffs), status)
        end associate

        if (.not. status%ok()) return
        call fortmacs_diff_sessions(session_index)%unselect_and_select_difference( &
            fortmacs_diff_sessions(session_index)%current_difference, status, FORTMACS_DIFF_SELECT_ONLY)
    end subroutine toggle_fortmacs_diff_highlighting

    recursive module subroutine toggle_fortmacs_diff_help(status)
        type(status_type), intent(out) :: status
        integer :: session_index

        call check_fortmacs_diff_control_buffer(session_index, status)
        if (.not. status%ok()) return
        fortmacs_diff_sessions(session_index)%use_long_help_message = &
            .not. fortmacs_diff_sessions(session_index)%use_long_help_message
        call fortmacs_diff_sessions(session_index)%set_help_message(status)
        if (.not. status%ok()) return
        ! Force redisplay.
        fortmacs_diff_sessions(session_index)%window_configuration_saved = ""
        call fortmacs_diff_sessions(session_index)%recenter(.true., status)
    end subroutine toggle_fortmacs_diff_help

    ! ------------------------------------------------------------------
    ! Moving between differences
    ! ------------------------------------------------------------------

    recursive module subroutine next_fortmacs_diff_difference(status, argument)
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: argument
        integer(int64) :: count
        integer :: session_index, difference
        logical :: skip
        character(len=64) :: progress

        call check_fortmacs_diff_control_buffer(session_index, status)
        if (.not. status%ok()) return
        count = 1
        if (present(argument)) count = argument

        if (fortmacs_diff_sessions(session_index)%current_difference < fortmacs_diff_sessions( &
            session_index)%number_of_differences) then
            difference = int(min(int(fortmacs_diff_sessions(session_index)%number_of_differences, int64), &
                                 fortmacs_diff_sessions(session_index)%current_difference + count))
            call fortmacs_diff_sessions(session_index)%make_region_visible(status)
            if (.not. status%ok()) return
            skip = .false.

            if (difference < fortmacs_diff_sessions(session_index)%number_of_differences) then
                call fortmacs_diff_sessions(session_index)%check_region_skip(difference, skip, status)
                if (.not. status%ok()) return
            end if

            ! Skip loop.
            do while (difference < fortmacs_diff_sessions(session_index)%number_of_differences .and. skip)
                difference = difference + 1

                if (modulo(difference, 20) == 0) then
                    write (progress, '("Skipped over region ", i0, " and counting ...")') difference
                    call message(trim(progress))
                end if

                if (difference < fortmacs_diff_sessions(session_index)%number_of_differences) then
                    call fortmacs_diff_sessions(session_index)%check_region_skip(difference, skip, status)
                    if (.not. status%ok()) return
                end if
            end do

            call message("")
            call fortmacs_diff_sessions(session_index)%unselect_and_select_difference(difference, status)
        else
            call fortmacs_diff_sessions(session_index)%make_region_visible(status)
            if (status%ok()) call set_error(status, USER_ERROR_CODE, "At end of the difference list")
        end if
    end subroutine next_fortmacs_diff_difference

    recursive module subroutine previous_fortmacs_diff_difference(status, argument)
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: argument
        integer(int64) :: count
        integer :: session_index, difference
        logical :: skip
        character(len=64) :: progress

        call check_fortmacs_diff_control_buffer(session_index, status)
        if (.not. status%ok()) return
        count = 1
        if (present(argument)) count = argument

        if (fortmacs_diff_sessions(session_index)%current_difference > -1) then
            difference = int(max(-1_int64, fortmacs_diff_sessions(session_index)%current_difference - count))
            call fortmacs_diff_sessions(session_index)%make_region_visible(status)
            if (.not. status%ok()) return
            skip = .false.

            if (difference >= 0) then
                call fortmacs_diff_sessions(session_index)%check_region_skip(difference, skip, status)
                if (.not. status%ok()) return
            end if

            do while (difference > -1 .and. skip)
                if (modulo(difference + 1, 20) == 0) then
                    write (progress, '("Skipped over region ", i0, " and counting ...")') difference + 1
                    call message(trim(progress))
                end if

                difference = difference - 1

                if (difference >= 0) then
                    call fortmacs_diff_sessions(session_index)%check_region_skip(difference, skip, status)
                    if (.not. status%ok()) return
                end if
            end do

            call message("")
            call fortmacs_diff_sessions(session_index)%unselect_and_select_difference(difference, status)
        else
            call fortmacs_diff_sessions(session_index)%make_region_visible(status)
            if (status%ok()) call set_error(status, USER_ERROR_CODE, "At beginning of the difference list")
        end if
    end subroutine previous_fortmacs_diff_difference

    recursive module subroutine jump_to_fortmacs_diff_difference(difference_number, status)
        integer(int64), intent(in) :: difference_number
        type(status_type), intent(out) :: status
        integer :: session_index, n

        call check_fortmacs_diff_control_buffer(session_index, status)
        if (.not. status%ok()) return

        if (difference_number < 0) then
            n = int(fortmacs_diff_sessions(session_index)%number_of_differences + difference_number)
        else if (difference_number > 0) then
            n = int(difference_number - 1)
        else
            n = -1
        end if

        ! -1 is allowed by ediff-unselect-and-select-difference --- it is
        ! the position before the first one.
        if (n >= -1 .and. n <= fortmacs_diff_sessions(session_index)%number_of_differences) then
            call fortmacs_diff_sessions(session_index)%unselect_and_select_difference(n, status)
        else
            call set_error(status, GENERIC_ERROR_CODE, fortmacs_diff_sessions(session_index)%get_bad_difference_message(n))
        end if
    end subroutine jump_to_fortmacs_diff_difference

    recursive module subroutine jump_to_fortmacs_diff_difference_at_point(buffer_type, argument, status)
        integer, intent(in) :: buffer_type
        logical, intent(in) :: argument
        type(status_type), intent(out) :: status
        type(handle_type) :: buffer, old, wind(3), old_window, old_buffer
        type(status_type) :: st
        integer(int64) :: point_value, beginning, shift
        integer :: session_index, diff_no, t
        logical :: past_last_diff

        call check_fortmacs_diff_control_buffer(session_index, status)
        if (.not. status%ok()) return
        buffer = fortmacs_diff_sessions(session_index)%buffers(buffer_type)
        old = current_buffer()
        call enter_fortmacs_diff_buffer(buffer, status)
        if (.not. status%ok()) return
        point_value = point()
        call set_buffer(old, status)
        if (.not. status%ok()) return

        if (argument) then
            call fortmacs_diff_sessions(session_index)%get_difference_at_point(buffer_type, diff_no, status, which_diff=1)
        else
            call fortmacs_diff_sessions(session_index)%get_difference_at_point(buffer_type, diff_no, status)
        end if

        if (.not. status%ok()) return
        past_last_diff = fortmacs_diff_sessions(session_index)%number_of_differences < diff_no

        if (past_last_diff) then
            call set_buffer(buffer, status)
            if (.not. status%ok()) return
            beginning = point_maximum()
            call set_buffer(old, status)
            if (.not. status%ok()) return
            call jump_to_fortmacs_diff_difference(-1_int64, status)
        else
            call fortmacs_diff_sessions(session_index)%get_difference_position(buffer_type, .false., beginning, status, diff_no - 1)
            if (.not. status%ok()) return
            call jump_to_fortmacs_diff_difference(int(diff_no, int64), status)
        end if

        if (.not. status%ok()) return
        wind = fortmacs_diff_sessions(session_index)%windows(FORTMACS_DIFF_BUFFER_TYPE_A:FORTMACS_DIFF_BUFFER_TYPE_C)
        if (.not. argument) return
        shift = beginning - point_value

        do t = 1, 3
            if (.not. is_window_live(wind(t))) cycle
            call select_window_begin(wind(t), old_window, old_buffer, status)

            if (status%ok()) then
                if (past_last_diff) call goto_character(point_maximum())
                ! Its error is ignored, as upstream's ignore-errors does.
                call backward_character(st, shift)
                call recenter(prefix_argument_type(PREFIX_NONE, 0_int64), .false., status)
            end if

            ! Select the old window again, keeping the first error.
            call select_window_end(old_window, old_buffer, st)
            if (status%ok()) status = st
            if (.not. status%ok()) return
        end do
    end subroutine jump_to_fortmacs_diff_difference_at_point

    ! ------------------------------------------------------------------
    ! Copying diffs
    ! ------------------------------------------------------------------

    recursive module subroutine copy_fortmacs_diff_diff_to_diff(argument, keys, status)
        type(prefix_argument_type), intent(in) :: argument
        character(len=*), intent(in) :: keys
        type(status_type), intent(out) :: status
        integer :: session_index, n

        call check_fortmacs_diff_control_buffer(session_index, status)
        if (.not. status%ok()) return

        if (argument%kind == PREFIX_RAW .and. argument%value == 4) then
            ! Copy all differences with `C-u' prefix.
            n = 0

            do while (fortmacs_diff_sessions(session_index)%is_valid_difference(n))
                call copy_fortmacs_diff_diff_to_diff(prefix_argument_type(PREFIX_NUMBER, int(n + 1, int64)), keys, status)
                if (.not. status%ok()) return
                n = n + 1
            end do

            return
        end if

        ! Translate neg arg to -1.
        if (argument%kind == PREFIX_MINUS) then
            call jump_to_fortmacs_diff_difference(-1_int64, status)
        else if (argument%kind == PREFIX_NUMBER) then
            call jump_to_fortmacs_diff_difference(argument%value, status)
        end if

        if (.not. status%ok()) return
        if (len(keys) < 2) return
        call fortmacs_diff_sessions(session_index)%copy_difference(fortmacs_diff_sessions(session_index)%current_difference, &
                                                                   fortmacs_diff_character_to_buffer_type(keys(1:1)), &
                                                                   fortmacs_diff_character_to_buffer_type(keys(2:2)), status)
        if (.not. status%ok()) return
        ! Recenter with rehighlighting, but no messages.
        call fortmacs_diff_sessions(session_index)%recenter(.false., status)
    end subroutine copy_fortmacs_diff_diff_to_diff

    recursive module subroutine restore_fortmacs_diff_diff(argument, key, status)
        type(prefix_argument_type), intent(in) :: argument
        character, intent(in) :: key
        type(status_type), intent(out) :: status
        integer :: session_index

        call check_fortmacs_diff_control_buffer(session_index, status)
        if (.not. status%ok()) return

        if (argument%kind == PREFIX_NUMBER) then
            call jump_to_fortmacs_diff_difference(argument%value, status)
            if (.not. status%ok()) return
        end if

        call fortmacs_diff_sessions(session_index)%pop_difference(fortmacs_diff_sessions(session_index)%current_difference, &
                                                                  fortmacs_diff_character_to_buffer_type(key), &
                                                                  status)
        if (.not. status%ok()) return
        ! Recenter with rehighlighting, but no messages.
        call fortmacs_diff_sessions(session_index)%recenter(.false., status)
    end subroutine restore_fortmacs_diff_diff

    ! ------------------------------------------------------------------
    ! Quitting
    ! ------------------------------------------------------------------

    recursive module subroutine quit_fortmacs_diff(reverse_default_keep_variants, status)
        type(status_type), intent(out) :: status
        logical, intent(in) :: reverse_default_keep_variants
        type(handle_type) :: control
        character(len=:), allocatable :: group
        logical :: answer
        integer :: session_index

        call check_fortmacs_diff_control_buffer(session_index, status)
        if (.not. status%ok()) return
        control = current_buffer()
        group = ""
        if (is_buffer_live(fortmacs_diff_sessions(session_index)%meta_buffer)) group = " & show containing session group"
        call read_y_or_n("Quit this Ediff session"//group//"? ", answer, status)
        if (.not. status%ok()) return
        call message("")
        if (.not. answer) return
        this_command => commands%quit_fortmacs_diff
        call set_buffer(control, status)
        if (.not. status%ok()) return
        call fortmacs_diff_sessions(session_index)%quit(reverse_default_keep_variants, status)
    end subroutine quit_fortmacs_diff

    ! Kill control buffer, other auxiliary Ediff buffers.
    ! Leave one of the frames split between buffers A/B/C.  Port of
    ! ediff-util.el:ediff-cleanup-mess (`balance-windows' is not ported).
    recursive subroutine clean_up_fortmacs_diff_mess(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: buffer_a, buffer_b, buffer_c, control_buffer, control_window, new
        type(status_type) :: st
        procedure(fortmacs_diff_split_window_procedure), pointer :: split_window_function
        logical :: three_way_job
        integer :: session_index

        status = status_type()
        session_index = fortmacs_diff_session_index()
        if (session_index == 0) return
        buffer_a = fortmacs_diff_sessions(session_index)%buffers(FORTMACS_DIFF_BUFFER_TYPE_A)
        buffer_b = fortmacs_diff_sessions(session_index)%buffers(FORTMACS_DIFF_BUFFER_TYPE_B)
        buffer_c = fortmacs_diff_sessions(session_index)%buffers(FORTMACS_DIFF_BUFFER_TYPE_C)
        control_buffer = fortmacs_diff_sessions(session_index)%control_buffer
        control_window = get_fortmacs_diff_visible_buffer_window(control_buffer)
        three_way_job = fortmacs_diff_sessions(session_index)%three_way_job
        split_window_function => fortmacs_diff_sessions(session_index)%split_window_function
        call close_fortmacs_diff_buffer_carefully(fortmacs_diff_sessions(session_index)%diff_buffer, status)
        if (.not. status%ok()) return
        call close_fortmacs_diff_buffer_carefully(fortmacs_diff_sessions(session_index)%custom_diff_buffer, status)
        if (.not. status%ok()) return
        call close_fortmacs_diff_buffer_carefully(get_buffer(" *ediff-tmp*"), status)
        if (.not. status%ok()) return
        call close_fortmacs_diff_buffer_carefully(fortmacs_diff_sessions(session_index)%error_buffer, status)
        if (.not. status%ok()) return
        call close_fortmacs_diff_buffer_carefully(get_buffer(" *ediff-message*"), status)
        if (.not. status%ok()) return
        call close_fortmacs_diff_buffer_carefully(get_buffer("*ediff-debug*"), status)
        if (.not. status%ok()) return

        ! Delete control window.
        if (is_window_live(control_window)) then
            call delete_window(control_window, status)
            if (.not. status%ok()) return
        end if

        call destroy_keymap(fortmacs_diff_sessions(session_index)%keymap, status)
        if (.not. status%ok()) return
        call drop_fortmacs_diff_session(session_index)
        call close_fortmacs_diff_buffer_carefully(control_buffer, status)
        if (.not. status%ok()) return

        ! Display only if not visible.  The errors of showing the buffers
        ! are ignored, as upstream's condition-case does.
        if (.not. is_window_live(get_fortmacs_diff_visible_buffer_window(buffer_b)) .and. is_buffer_live(buffer_b)) then
            call switch_to_buffer(buffers(buffer_b%index)%name, st)
        end if

        if (.not. is_window_live(get_fortmacs_diff_visible_buffer_window(buffer_a)) .and. is_buffer_live(buffer_a)) then
            st = status_type()

            if (is_window_live(get_fortmacs_diff_visible_buffer_window(buffer_b)) .and. associated(split_window_function)) then
                call split_window_function(new, st)
            end if

            if (st%ok()) call switch_to_buffer(buffers(buffer_a%index)%name, st)
        end if

        if (three_way_job .and. is_buffer_live(buffer_c)) then
            if (.not. is_window_live(get_fortmacs_diff_visible_buffer_window(buffer_c))) then
                st = status_type()

                if (is_window_live(get_fortmacs_diff_visible_buffer_window(buffer_a)) .or. &
                    is_window_live(get_fortmacs_diff_visible_buffer_window(buffer_b))) then
                    if (associated(split_window_function)) call split_window_function(new, st)
                end if

                if (st%ok()) call switch_to_buffer(buffers(buffer_c%index)%name, st)
            end if
        end if

        call message("")
    end subroutine clean_up_fortmacs_diff_mess

    ! ------------------------------------------------------------------
    ! Selecting and highlighting differences
    ! ------------------------------------------------------------------

    ! ------------------------------------------------------------------
    ! Files and buffers
    ! ------------------------------------------------------------------

    recursive module subroutine read_fortmacs_diff_file_name(prompt, default_directory, default_file, result, status, &
                                                             no_directories)
        character(len=*), intent(in) :: prompt, default_directory, default_file
        character(len=:), allocatable, intent(out) :: result
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: no_directories
        character(len=:), allocatable :: directory, file, expanded, f, default
        logical :: directories_not_allowed

        directories_not_allowed = .false.
        if (present(no_directories)) directories_not_allowed = no_directories

        ! Hack default-dir if it is not set.
        if (len(default_directory) > 0) then
            directory = default_directory
        else if (len(default_file) > 0) then
            directory = file_name_directory(default_file)
            if (len(directory) == 0) directory = buffers(current)%directory
        else
            directory = buffers(current)%directory
        end if

        call expand_file_name(directory, expanded, status)
        if (.not. status%ok()) return
        directory = file_name_as_directory(abbreviate_file_name(expanded))
        ! Strip the directory from default-file.
        file = file_name_nondirectory(default_file)
        default = directory
        if (len(file) > 0) default = file
        call read_file_name(prompt//" (default "//default//"): ", f, status, directory=directory, default_filename=default, &
                            mustmatch=REQUIRE_MATCH_YES)
        if (.not. status%ok()) return
        call expand_file_name(f, result, status, directory)
        if (.not. status%ok()) return

        ! If user entered a directory name, expand the default file in that
        ! directory.
        if (is_file_directory(result) .and. len(file) > 0) then
            f = result
            call expand_file_name(file, result, status, f)
            if (.not. status%ok()) return
        end if

        if (directories_not_allowed .and. is_file_directory(result)) then
            call set_error(status, USER_ERROR_CODE, "File "//result//" is a directory")
        end if
    end subroutine read_fortmacs_diff_file_name

    recursive module subroutine create_fortmacs_diff_temporary_file(buffer, file, status, prefix)
        type(handle_type), intent(in) :: buffer
        character(len=:), allocatable, intent(out) :: file
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: prefix
        character(len=:), allocatable :: p, message_text, saved_coding, f
        type(handle_type) :: old
        type(status_type) :: st
        logical :: had_coding

        p = "ediff"
        if (present(prefix)) p = prefix
        call create_temporary_file(p, f, status)
        if (.not. status%ok()) return
        old = current_buffer()
        call enter_fortmacs_diff_buffer(buffer, status)
        if (.not. status%ok()) return
        had_coding = allocated(coding_system_for_write%value)
        if (had_coding) saved_coding = coding_system_for_write%value
        coding_system_for_write = fortmacs_diff_coding_system_for_write
        ! Create the file.
        call write_region(f, .false., status, point_minimum(), point_maximum(), message=message_text)

        if (had_coding) then
            coding_system_for_write = saved_coding
        else
            call coding_system_for_write%clear()
        end if

        ! Make the old buffer current again, keeping the first error.
        call set_buffer(old, st)
        if (status%ok()) status = st
        if (.not. status%ok()) return
        call set_file_modes(f, int(fortmacs_diff_temporary_file_mode, kind(0)), status)
        if (.not. status%ok()) return
        call expand_file_name(f, file, status)
    end subroutine create_fortmacs_diff_temporary_file

    recursive module subroutine verify_fortmacs_diff_file_buffer(status)
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
                call set_error(status, USER_ERROR_CODE, "Buffer is out of sync for file "//buffers(current)%filename)
            end if
        else
            ! If buffer is obsolete, offer to revert.
            call read_yes_or_no("File "//buffers(current)%filename//" was modified since visited by buffer "// &
                                buffers(current)%name//".  REVERT file "//buffers(current)%filename//"? ", answer, status)
            if (.not. status%ok()) return

            if (answer) then
                call revert_buffer(status, ignore_auto=.true., noconfirm=.true.)
            else
                call set_error(status, USER_ERROR_CODE, "Buffer out of sync for file "//buffers(current)%filename)
            end if
        end if
    end subroutine verify_fortmacs_diff_file_buffer

    recursive module subroutine fortmacs_diff_other_buffer(excluded, name)
        character(len=*), intent(in) :: excluded(:)
        character(len=:), allocatable, intent(out) :: name
        type(handle_type), allocatable :: all_buffers(:)
        type(handle_type) :: preferred_buffer, first
        integer :: j, use_directory_editor_major_mode, directory_editor_count, first_directory_editor
        character(len=:), allocatable :: bname
        logical :: is_directory_editor

        all_buffers = buffer_list()
        preferred_buffer = null_handle()
        if (size(all_buffers) > 0) preferred_buffer = all_buffers(1)
        ! If at least one buffer on the exclusion list is dired, then force
        ! all others to be dired.
        use_directory_editor_major_mode = -1

        if (size(excluded) > 0) then
            first = get_buffer(trim(excluded(1)))

            if (is_buffer_live(first)) then
                use_directory_editor_major_mode = 0
                if (buffers(first%index)%major_mode == "directory_editor_mode") use_directory_editor_major_mode = 1
            end if
        end if

        directory_editor_count = 0
        first_directory_editor = 0

        ! Significant buffers must be visible and not belong to the
        ! exclusion list.
        do j = 1, size(all_buffers)
            bname = buffers(all_buffers(j)%index)%name
            is_directory_editor = buffers(all_buffers(j)%index)%major_mode == "directory_editor_mode"
            if (any(excluded == bname)) cycle
            if (.not. is_window_live(get_fortmacs_diff_visible_buffer_window(all_buffers(j)))) cycle

            if (all_buffers(j)%index == preferred_buffer%index) then
                name = bname
                return
            end if

            if (use_directory_editor_major_mode == 1) then
                if (is_directory_editor) then
                    name = bname
                    return
                end if

                cycle
            end if

            if (is_directory_editor) then
                ! Remember this buffer in case dired buffs are the only
                ! ones visible.
                if (use_directory_editor_major_mode == -1) then
                    directory_editor_count = directory_editor_count + 1
                    first_directory_editor = j
                end if

                cycle
            end if

            if (index(bname, " ") == 1 .or. index(bname, "*") == 1) cycle
            if (bname == "*scratch*") cycle
            name = bname
            return
        end do

        if (directory_editor_count > 0) then
            name = buffers(all_buffers(first_directory_editor)%index)%name
            return
        end if

        ! Try also buffers that are not displayed in windows.
        do j = 1, size(all_buffers)
            bname = buffers(all_buffers(j)%index)%name
            is_directory_editor = buffers(all_buffers(j)%index)%major_mode == "directory_editor_mode"
            if (any(excluded == bname)) cycle

            if (use_directory_editor_major_mode == 1) then
                if (.not. is_directory_editor) cycle
            else if (is_directory_editor) then
                cycle
            end if

            if (index(bname, " ") == 1 .or. index(bname, "*") == 1) cycle
            if (bname == "*scratch*") cycle
            name = bname
            return
        end do

        name = "*scratch*"
    end subroutine fortmacs_diff_other_buffer

    ! ------------------------------------------------------------------
    ! Merging
    ! ------------------------------------------------------------------

    ! Restore `fortmacs_diff_show_ancestor' on exit: a function of
    ! `fortmacs_diff_quit_hook', run in the control buffer of the session that
    ! quits.  Port of ediff-util.el:ediff--restore-options-on-exit.
    recursive subroutine restore_fortmacs_diff_options_on_exit(status)
        type(status_type), intent(out) :: status
        integer :: session_index

        status = status_type()
        session_index = fortmacs_diff_session_index()
        if (session_index == 0) return

        if (fortmacs_diff_sessions(session_index)%show_ancestor_original) then
            call message("Restoring fortmacs_diff_show_ancestor to .true....")
        else
            call message("Restoring fortmacs_diff_show_ancestor to .false....")
        end if

        fortmacs_diff_show_ancestor = fortmacs_diff_sessions(session_index)%show_ancestor_original
        fortmacs_diff_sessions(session_index)%show_ancestor_original = .false.
        call fortmacs_diff_quit_hook%remove(restore_fortmacs_diff_options_on_exit)
    end subroutine restore_fortmacs_diff_options_on_exit

    recursive module subroutine toggle_fortmacs_diff_show_ancestor(status)
        type(status_type), intent(out) :: status
        integer :: session_index

        call check_fortmacs_diff_control_buffer(session_index, status)
        if (.not. status%ok()) return
        call fortmacs_diff_sessions(session_index)%check_ancestor_exists(status)
        if (.not. status%ok()) return

        ! Save original value if not yet, and add hook to restore it on
        ! exit.
        if (.not. fortmacs_diff_sessions(session_index)%show_ancestor_original) then
            fortmacs_diff_sessions(session_index)%show_ancestor_original = fortmacs_diff_show_ancestor
            call fortmacs_diff_quit_hook%prepend(restore_fortmacs_diff_options_on_exit)
        end if

        fortmacs_diff_show_ancestor = .not. fortmacs_diff_show_ancestor

        ! If equal than orig, then nothing to restore on exit.
        if (fortmacs_diff_show_ancestor .eqv. fortmacs_diff_sessions(session_index)%show_ancestor_original) then
            fortmacs_diff_sessions(session_index)%show_ancestor_original = .false.
            call fortmacs_diff_quit_hook%remove(restore_fortmacs_diff_options_on_exit)
        end if

        if (.not. fortmacs_diff_show_ancestor) then
            call delete_window(fortmacs_diff_sessions(session_index)%windows(FORTMACS_DIFF_BUFFER_TYPE_ANCESTOR), status)
            if (.not. status%ok()) return
            call fortmacs_diff_sessions(session_index)%recenter(.false., status)
            if (.not. status%ok()) return
            call message("Ancestor buffer is hidden")
        else
            ! The ancestor's frame is the selected one (control frames are
            ! Deferred).
            call fortmacs_diff_sessions(session_index)%recenter(.false., status)
            if (.not. status%ok()) return
            call message("Showing ancestor buffer")
        end if
    end subroutine toggle_fortmacs_diff_show_ancestor

    recursive module subroutine toggle_fortmacs_diff_show_clashes_only(status)
        type(status_type), intent(out) :: status
        integer :: session_index

        call check_fortmacs_diff_control_buffer(session_index, status)
        if (.not. status%ok()) return

        if (.not. fortmacs_diff_sessions(session_index)%merge_with_ancestor_job) then
            call set_error(status, USER_ERROR_CODE, "This command makes sense only when merging with an ancestor")
            return
        end if

        fortmacs_diff_sessions(session_index)%show_clashes_only = .not. fortmacs_diff_sessions(session_index)%show_clashes_only

        if (fortmacs_diff_sessions(session_index)%show_clashes_only) then
            call message("Focus on regions where both buffers differ from the ancestor")
        else
            call message("Canceling focus on regions where changes clash")
        end if
    end subroutine toggle_fortmacs_diff_show_clashes_only

    recursive module subroutine toggle_fortmacs_diff_skip_changed_regions(status)
        type(status_type), intent(out) :: status
        integer :: session_index

        call check_fortmacs_diff_control_buffer(session_index, status)
        if (.not. status%ok()) return
        fortmacs_diff_sessions(session_index)%skip_merge_regions_that_differ_from_default = &
            .not. fortmacs_diff_sessions(session_index)%skip_merge_regions_that_differ_from_default

        if (fortmacs_diff_sessions(session_index)%skip_merge_regions_that_differ_from_default) then
            call message("Skipping regions that differ from default setting")
        else
            call message("Showing regions that differ from default setting")
        end if
    end subroutine toggle_fortmacs_diff_skip_changed_regions

    recursive module subroutine maybe_save_and_delete_fortmacs_diff_merge(status)
        type(status_type), intent(out) :: status
        integer :: session_index

        status = status_type()
        session_index = fortmacs_diff_session_index()
        if (session_index == 0) return
        call fortmacs_diff_sessions(session_index)%save_and_delete_merge(.false., status)
    end subroutine maybe_save_and_delete_fortmacs_diff_merge

    ! ------------------------------------------------------------------
    ! Saving
    ! ------------------------------------------------------------------

    recursive module subroutine save_fortmacs_diff_buffer(argument, key, status)
        logical, intent(in) :: argument
        character, intent(in) :: key
        type(status_type), intent(out) :: status
        type(handle_type) :: buffer, old
        type(status_type) :: st
        integer :: session_index, saved_window_minimum_height

        call check_fortmacs_diff_control_buffer(session_index, status)
        if (.not. status%ok()) return
        call fortmacs_diff_sessions(session_index)%compute_custom_diffs(status)
        if (.not. status%ok()) return

        select case (key)
        case ("a", "b", "c")
            buffer = fortmacs_diff_sessions(session_index)%buffers(fortmacs_diff_character_to_buffer_type(key))
        case ("d")
            call message("Saving diff output ...")
            ! Give the user time to see the message.
            call sit_for(1000)

            if (argument .and. is_buffer_live(fortmacs_diff_sessions(session_index)%diff_buffer)) then
                buffer = fortmacs_diff_sessions(session_index)%diff_buffer
            else if (is_buffer_live(fortmacs_diff_sessions(session_index)%custom_diff_buffer)) then
                buffer = fortmacs_diff_sessions(session_index)%custom_diff_buffer
            else if (is_buffer_live(fortmacs_diff_sessions(session_index)%diff_buffer)) then
                buffer = fortmacs_diff_sessions(session_index)%diff_buffer
            else
                call set_error(status, USER_ERROR_CODE, substitute_quotes("Output from `diff' not found"))
                return
            end if
        case default
            buffer = fortmacs_diff_sessions(session_index)%control_buffer
        end select

        old = current_buffer()
        call enter_fortmacs_diff_buffer(buffer, status)
        if (.not. status%ok()) return
        ! window_minimum_height is 2 while the buffer is saved.
        saved_window_minimum_height = window_minimum_height
        window_minimum_height = 2
        call save_buffer(1_int64, .false., status)
        window_minimum_height = saved_window_minimum_height

        ! Make the old buffer current again, keeping the first error.
        if (is_buffer_live(old)) then
            call set_buffer(old, st)
            if (status%ok()) status = st
        end if
    end subroutine save_fortmacs_diff_buffer

    ! ------------------------------------------------------------------
    ! Commands
    ! ------------------------------------------------------------------

    ! The last key of the command, as a character ("" if none).
    ! Not a port: what the commands do with last-command-event.
    function last_command_character() result(c)
        character(len=:), allocatable :: c

        c = ""
        if (vtag(last_command_event) /= V_INTEGER) return
        if (xfixnum(last_command_event) < 0 .or. xfixnum(last_command_event) > 127) return
        c = achar(int(xfixnum(last_command_event)))
    end function last_command_character

    recursive subroutine execute_next_fortmacs_diff_difference(status)
        type(status_type), intent(out) :: status

        call next_fortmacs_diff_difference(status, prefix_numeric_value(current_prefix_argument))
    end subroutine execute_next_fortmacs_diff_difference

    recursive subroutine execute_previous_fortmacs_diff_difference(status)
        type(status_type), intent(out) :: status

        call previous_fortmacs_diff_difference(status, prefix_numeric_value(current_prefix_argument))
    end subroutine execute_previous_fortmacs_diff_difference

    recursive subroutine execute_jump_to_fortmacs_diff_difference(status)
        type(status_type), intent(out) :: status

        call jump_to_fortmacs_diff_difference(prefix_numeric_value(current_prefix_argument), status)
    end subroutine execute_jump_to_fortmacs_diff_difference

    recursive subroutine execute_jump_to_fortmacs_diff_difference_at_point(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: c

        c = last_command_character()
        if (len(c) == 0) c = "a"
        call jump_to_fortmacs_diff_difference_at_point(fortmacs_diff_character_to_buffer_type(c), &
                                                       current_prefix_argument%kind /= PREFIX_NONE, &
                                                       status)
    end subroutine execute_jump_to_fortmacs_diff_difference_at_point

    recursive subroutine execute_recenter_fortmacs_diff(status)
        type(status_type), intent(out) :: status

        call recenter_fortmacs_diff(status)
    end subroutine execute_recenter_fortmacs_diff

    recursive subroutine execute_quit_fortmacs_diff(status)
        type(status_type), intent(out) :: status

        call quit_fortmacs_diff(current_prefix_argument%kind /= PREFIX_NONE, status)
    end subroutine execute_quit_fortmacs_diff

    recursive subroutine execute_restore_fortmacs_diff_diff(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: c

        c = last_command_character()
        if (len(c) == 0) c = " "
        call restore_fortmacs_diff_diff(current_prefix_argument, c(1:1), status)
    end subroutine execute_restore_fortmacs_diff_diff

    !> Copy ARGth difference region from buffer A to B.
    !> ARGUMENT is a prefix argument.  If absent, copy the current difference
    !> region.  Port of ediff-util.el:ediff-copy-A-to-B.
    recursive subroutine copy_fortmacs_diff_a_to_b(status)
        type(status_type), intent(out) :: status

        call copy_fortmacs_diff_diff_to_diff(current_prefix_argument, "ab", status)
    end subroutine copy_fortmacs_diff_a_to_b

    !> Copy ARGth difference region from buffer B to A.
    !> ARGUMENT is a prefix argument.  If absent, copy the current difference
    !> region.  Port of ediff-util.el:ediff-copy-B-to-A.
    recursive subroutine copy_fortmacs_diff_b_to_a(status)
        type(status_type), intent(out) :: status

        call copy_fortmacs_diff_diff_to_diff(current_prefix_argument, "ba", status)
    end subroutine copy_fortmacs_diff_b_to_a

    !> Copy ARGth difference region from buffer A to buffer C.
    !> ARGUMENT is a prefix argument.  If absent, copy the current difference
    !> region.  Port of ediff-util.el:ediff-copy-A-to-C.
    recursive subroutine copy_fortmacs_diff_a_to_c(status)
        type(status_type), intent(out) :: status

        call copy_fortmacs_diff_diff_to_diff(current_prefix_argument, "ac", status)
    end subroutine copy_fortmacs_diff_a_to_c

    !> Copy ARGth difference region from buffer B to buffer C.
    !> ARGUMENT is a prefix argument.  If absent, copy the current difference
    !> region.  Port of ediff-util.el:ediff-copy-B-to-C.
    recursive subroutine copy_fortmacs_diff_b_to_c(status)
        type(status_type), intent(out) :: status

        call copy_fortmacs_diff_diff_to_diff(current_prefix_argument, "bc", status)
    end subroutine copy_fortmacs_diff_b_to_c

    !> Copy ARGth difference region from buffer C to B.
    !> ARGUMENT is a prefix argument.  If absent, copy the current difference
    !> region.  Port of ediff-util.el:ediff-copy-C-to-B.
    recursive subroutine copy_fortmacs_diff_c_to_b(status)
        type(status_type), intent(out) :: status

        call copy_fortmacs_diff_diff_to_diff(current_prefix_argument, "cb", status)
    end subroutine copy_fortmacs_diff_c_to_b

    !> Copy ARGth difference region from buffer C to A.
    !> ARGUMENT is a prefix argument.  If absent, copy the current difference
    !> region.  Port of ediff-util.el:ediff-copy-C-to-A.
    recursive subroutine copy_fortmacs_diff_c_to_a(status)
        type(status_type), intent(out) :: status

        call copy_fortmacs_diff_diff_to_diff(current_prefix_argument, "ca", status)
    end subroutine copy_fortmacs_diff_c_to_a

    !> Restore ARGth diff in the merge buffer.
    !> ARGUMENT is a prefix argument.  If absent, restore the current diff.
    !> Port of ediff-util.el:ediff-restore-diff-in-merge-buffer.
    recursive subroutine restore_fortmacs_diff_diff_in_merge_buffer(status)
        type(status_type), intent(out) :: status

        call restore_fortmacs_diff_diff(current_prefix_argument, "c", status)
    end subroutine restore_fortmacs_diff_diff_in_merge_buffer

    recursive subroutine execute_save_fortmacs_diff_buffer(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: c

        c = last_command_character()
        if (len(c) == 0) c = " "
        call save_fortmacs_diff_buffer(current_prefix_argument%kind /= PREFIX_NONE, c(1:1), status)
    end subroutine execute_save_fortmacs_diff_buffer

    module subroutine initialize_fortmacs_diff_utility()
        if (associated(commands%quit_fortmacs_diff%execute)) return
        commands%next_fortmacs_diff_difference%execute => execute_next_fortmacs_diff_difference
        commands%previous_fortmacs_diff_difference%execute => execute_previous_fortmacs_diff_difference
        commands%jump_to_fortmacs_diff_difference%execute => execute_jump_to_fortmacs_diff_difference
        commands%jump_to_fortmacs_diff_difference_at_point%execute => execute_jump_to_fortmacs_diff_difference_at_point
        commands%quit_fortmacs_diff%execute => execute_quit_fortmacs_diff
        commands%recenter_fortmacs_diff%execute => execute_recenter_fortmacs_diff
        commands%toggle_fortmacs_diff_split%execute => toggle_fortmacs_diff_split
        commands%toggle_fortmacs_diff_highlighting%execute => toggle_fortmacs_diff_highlighting
        commands%toggle_fortmacs_diff_help%execute => toggle_fortmacs_diff_help
        commands%update_fortmacs_diff_diffs%execute => update_fortmacs_diff_diffs
        commands%copy_fortmacs_diff_a_to_b%execute => copy_fortmacs_diff_a_to_b
        commands%copy_fortmacs_diff_b_to_a%execute => copy_fortmacs_diff_b_to_a
        commands%copy_fortmacs_diff_a_to_c%execute => copy_fortmacs_diff_a_to_c
        commands%copy_fortmacs_diff_b_to_c%execute => copy_fortmacs_diff_b_to_c
        commands%copy_fortmacs_diff_c_to_b%execute => copy_fortmacs_diff_c_to_b
        commands%copy_fortmacs_diff_c_to_a%execute => copy_fortmacs_diff_c_to_a
        commands%restore_fortmacs_diff_diff%execute => execute_restore_fortmacs_diff_diff
        commands%restore_fortmacs_diff_diff_in_merge_buffer%execute => restore_fortmacs_diff_diff_in_merge_buffer
        commands%toggle_fortmacs_diff_show_ancestor%execute => toggle_fortmacs_diff_show_ancestor
        commands%toggle_fortmacs_diff_show_clashes_only%execute => toggle_fortmacs_diff_show_clashes_only
        commands%toggle_fortmacs_diff_skip_changed_regions%execute => toggle_fortmacs_diff_skip_changed_regions
        commands%save_fortmacs_diff_buffer%execute => execute_save_fortmacs_diff_buffer
        call fortmacs_diff_quit_hook%prepend(clean_up_fortmacs_diff_mess)
        call fortmacs_diff_quit_merge_hook%prepend(maybe_save_and_delete_fortmacs_diff_merge)
    end subroutine initialize_fortmacs_diff_utility
end submodule fortmacs_diff_utilities_session_mod
