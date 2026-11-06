! fortmacs_diff_settings_utilities_mod.f90 -- the selecting, copying and quitting of an Ediff session,
! a submodule of fortmacs_diff_settings_mod.
! Ported from GNU Emacs 31.1 lisp/vc/ediff-util.el (in part: see
! docs/status/fortmacs_diff_utilities_mod.md).
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

!> The type-bound procedures of `fortmacs_diff_session_type' that port
!> ediff-util.el (selecting, highlighting, copying and restoring the
!> differences, recentering and quitting): a submodule of the type's module,
!> fortmacs_diff_settings_mod, because they use fortmacs_diff_utilities_mod
!> and the modules that use it.
submodule(fortmacs_diff_settings_mod) fortmacs_diff_settings_utilities_mod
    use, intrinsic :: iso_fortran_env, only: int64
    use error_mod, only: status_type, set_error, format_error, substitute_quotes, GENERIC_ERROR_CODE, USER_ERROR_CODE, QUIT_CODE
    use handle_mod, only: handle_type, null_handle, same_handle
    use buffer_mod, only: is_buffer_live, set_buffer, current_buffer, buffers, current, get_or_make_buffer, close_buffer, &
                          is_buffer_modified, restore_buffer_modified, erase_buffer, get_file_buffer
    use execute_interactively_mod, only: prefix_argument_type, this_command, PREFIX_NUMBER
    use editing_functions_mod, only: point_minimum, point_maximum, point, goto_character, is_beginning_of_line, &
                                     buffer_substring, delete_region, narrow_to_region
    use value_mod, only: value_type, choice_type
    use syntax_mod, only: skip_characters_forward
    use diff_mode_mod, only: diff_mode
    use insert_delete_mod, only: insert
    use simple_mod, only: count_lines, deactivate_mark
    use keyboard_mod, only: sit_for
    use help_mod, only: with_output_to_temporary_buffer
    use commands_mod, only: commands
    use interval_tree_mod, only: overlay_start, overlay_end, overlay_buffer, overlay_put, destroy_overlay, is_overlay
    use window_mod, only: windows, is_window_live, set_window_start, set_window_dedication, is_position_visible_in_window, &
                          move_to_window_line, recenter, window_total_height, frame_root_window, fit_window_to_buffer, &
                          switch_to_buffer, window_parent, DEDICATED_WEAK
    use frame_mod, only: selected_frame
    use file_input_output_mod, only: expand_file_name, file_exists
    use files_mod, only: delete_file, save_buffer, set_visited_file_name
    use minibuffer_mod, only: read_file_name
    use basic_subprograms_mod, only: read_y_or_n
    use utility_functions_mod, only: read_yes_or_no
    use display_update_mod, only: ding
    use display_engine_mod, only: message
    use fortmacs_diff_help_mod, only: set_fortmacs_diff_help_overlays
    use fortmacs_diff_window_mod, only: set_up_fortmacs_diff_windows
    use fortmacs_diff_diff_mod, only: execute_fortmacs_diff_process, fortmacs_diff_custom_diff_program, &
                                      fortmacs_diff_custom_diff_options
    use fortmacs_diff_multiple_mod, only: fortmacs_diff_session_registry, update_fortmacs_diff_registry, &
                                          update_fortmacs_diff_meta_buffer, show_fortmacs_diff_meta_buffer
    use fortmacs_diff_merging_mod, only: fortmacs_diff_quit_merge_hook
    use fortmacs_diff_utilities_mod, only: create_fortmacs_diff_temporary_file, fortmacs_diff_unique_buffer_name, &
                                           make_fortmacs_diff_bullet_proof_overlay, quit_fortmacs_diff, &
                                           enter_fortmacs_diff_buffer, select_window_begin, select_window_end
    implicit none(type, external)

    character, parameter :: LF = achar(10)

contains

    module logical function is_valid_difference(self, difference)
        class(fortmacs_diff_session_type), intent(in) :: self
        integer, intent(in), optional :: difference
        integer :: m

        m = self%current_difference
        if (present(difference)) m = difference
        is_valid_difference = m >= 0 .and. m < self%number_of_differences
    end function is_valid_difference

    module function get_bad_difference_message(self, difference) result(text)
        class(fortmacs_diff_session_type), intent(in) :: self
        integer, intent(in) :: difference
        character(len=:), allocatable :: text
        character(len=:), allocatable :: name
        character(len=80) :: numbers

        name = "null()"
        if (associated(this_command)) name = trim(this_command%name)
        write (numbers, '(": Bad diff region number, ", i0, ".  Valid numbers are 1 to ", i0)') difference + 1, &
            self%number_of_differences
        text = name//trim(numbers)
    end function get_bad_difference_message

    recursive module subroutine arrange_autosave_in_merge_jobs(self, status, merge_buffer_file)
        class(fortmacs_diff_session_type), intent(inout), target :: self
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: merge_buffer_file
        type(handle_type) :: old
        type(status_type) :: st
        integer :: session_index

        call self%get_index(session_index, status)
        if (.not. status%ok()) return
        if (.not. fortmacs_diff_sessions(session_index)%merge_job) return

        if (present(merge_buffer_file)) then
            fortmacs_diff_sessions(session_index)%autostore_merges = choice_type(value=.true.)
            fortmacs_diff_sessions(session_index)%merge_store_file = merge_buffer_file
        end if

        if (.not. allocated(fortmacs_diff_sessions(session_index)%merge_store_file)) return
        ! Save before leaving ctl buffer.
        call verify_fortmacs_diff_file_merge_buffer(fortmacs_diff_sessions(session_index)%merge_store_file, status)
        if (.not. status%ok()) return
        old = current_buffer()
        call set_buffer(fortmacs_diff_sessions(session_index)%buffers(FORTMACS_DIFF_BUFFER_TYPE_C), status)
        if (status%ok()) call set_visited_file_name(fortmacs_diff_sessions(session_index)%merge_store_file, status)
        ! Make the old buffer current again, keeping the first error.
        call set_buffer(old, st)
        if (status%ok()) status = st
    end subroutine arrange_autosave_in_merge_jobs

    recursive module subroutine set_up_control_buffer(self, control_buffer, status)
        class(fortmacs_diff_session_type), intent(inout), target :: self
        type(handle_type), intent(in) :: control_buffer
        type(status_type), intent(out) :: status
        type(handle_type) :: window, parent, root
        integer :: root_height
        integer(int64) :: moved
        integer :: session_index

        call self%get_index(session_index, status)
        if (.not. status%ok()) return
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
        call fortmacs_diff_sessions(session_index)%set_help_message(status)
        if (.not. status%ok()) return
        call set_buffer(control_buffer, status)
        if (.not. status%ok()) return
        call insert(fortmacs_diff_sessions(session_index)%help_message, status)
        if (.not. status%ok()) return
        parent = window_parent(window, status)
        if (.not. status%ok()) return

        ! Emulate the behavior of Emacs 27: fit the window, but to at most
        ! half the frame (the lower window of a split gets the residue
        ! line).
        if (parent%index /= 0) then
            if (is_position_visible_in_window(point_minimum(), window)) then
                root = frame_root_window(window, status)
                if (.not. status%ok()) return
                root_height = window_total_height(root, status)
                if (.not. status%ok()) return
                call fit_window_to_buffer(window, (root_height + 1)/2)
            end if
        end if

        call fortmacs_diff_sessions(session_index)%indent_help_message(status)
        if (.not. status%ok()) return
        call set_fortmacs_diff_help_overlays(status)
        if (.not. status%ok()) return
        call restore_buffer_modified(.false.)
        call fortmacs_diff_sessions(session_index)%refresh_mode_lines()
        fortmacs_diff_sessions(session_index)%control_window = selected_frame%selected_window
        fortmacs_diff_sessions(session_index)%window_configuration_saved = &
            fortmacs_diff_sessions(session_index)%get_window_configuration()
        call set_window_dedication(selected_frame%selected_window, DEDICATED_WEAK, status)
        if (.not. status%ok()) return
        call goto_character(point_minimum())
        ! (skip-chars-forward ediff-whitespace)
        moved = skip_characters_forward(" "//LF//achar(9)//achar(12)//achar(13)//char(194)//char(160), status)

    contains

        ! (erase-buffer) in the read-only control buffer.
        subroutine erase_control_buffer()
            logical :: saved

            saved = buffers(current)%read_only
            buffers(current)%read_only = .false.
            call erase_buffer(status)
            buffers(current)%read_only = saved
        end subroutine erase_control_buffer
    end subroutine set_up_control_buffer

    recursive module subroutine recenter_session(self, no_rehighlight, status)
        class(fortmacs_diff_session_type), intent(inout), target :: self
        logical, intent(in) :: no_rehighlight
        type(status_type), intent(out) :: status
        type(handle_type) :: control
        type(status_type) :: st
        logical :: saved, all_live
        integer :: t
        integer :: session_index

        call self%get_index(session_index, status)
        if (.not. status%ok()) return
        control = fortmacs_diff_sessions(session_index)%control_buffer
        all_live = is_buffer_live(fortmacs_diff_sessions(session_index)%buffers(FORTMACS_DIFF_BUFFER_TYPE_A)) .and. &
                   is_buffer_live(fortmacs_diff_sessions(session_index)%buffers(FORTMACS_DIFF_BUFFER_TYPE_B))

        if (fortmacs_diff_sessions(session_index)%three_way_job) then
            all_live = all_live .and. is_buffer_live(fortmacs_diff_sessions(session_index)%buffers(FORTMACS_DIFF_BUFFER_TYPE_C))
        end if

        ! The control buffer is writable while the windows are set up.
        saved = buffers(current)%read_only
        buffers(current)%read_only = .false.

        if (all_live) then
            associate (s => fortmacs_diff_sessions(session_index))
                call set_up_fortmacs_diff_windows(s%buffers(FORTMACS_DIFF_BUFFER_TYPE_A), s%buffers(FORTMACS_DIFF_BUFFER_TYPE_B), &
                                                  s%buffers(FORTMACS_DIFF_BUFFER_TYPE_C), control, status)
            end associate
        else if (.not. associated(this_command, commands%quit_fortmacs_diff)) then
            call ding()
            call message(FORTMACS_DIFF_KILLED_VITAL_BUFFER)
        end if

        ! Make the control buffer current again, keeping the first error.
        if (is_buffer_live(control)) then
            call set_buffer(control, st)
            if (st%ok()) buffers(current)%read_only = saved
            if (status%ok()) status = st
        end if

        if (.not. status%ok()) return
        ! Set visibility range appropriate to this invocation of Ediff.
        call fortmacs_diff_sessions(session_index)%make_region_visible(status)
        if (.not. status%ok()) return

        ! Redisplay whatever buffers are showing, if there is a selected
        ! difference.
        if (all_live) then
            if (.not. no_rehighlight) then
                call fortmacs_diff_sessions(session_index)%select_difference(fortmacs_diff_sessions( &
                                                                             session_index)%current_difference, status)
                if (.not. status%ok()) return
            end if

            do t = FORTMACS_DIFF_BUFFER_TYPE_A, FORTMACS_DIFF_BUFFER_TYPE_C
                if (t == FORTMACS_DIFF_BUFFER_TYPE_C .and. .not. fortmacs_diff_sessions(session_index)%three_way_job) exit
                call fortmacs_diff_sessions(session_index)%recenter_one_window(t, status)
                if (.not. status%ok()) return
            end do
        end if

        if (.not. no_rehighlight) then
            call fortmacs_diff_sessions(session_index)%restore_highlighting(status)
            if (.not. status%ok()) return
        end if

        if (is_buffer_live(control)) then
            call set_buffer(control, status)
            if (.not. status%ok()) return
        end if

        call fortmacs_diff_sessions(session_index)%refresh_mode_lines()
    end subroutine recenter_session

    recursive module subroutine recenter_one_window(self, buffer_type, status)
        class(fortmacs_diff_session_type), intent(inout), target :: self
        integer, intent(in) :: buffer_type
        type(status_type), intent(out) :: status
        type(handle_type) :: window, old_window, old_buffer, control
        type(status_type) :: st
        integer(int64) :: beginning, end
        integer :: session_index

        call self%get_index(session_index, status)
        if (.not. status%ok()) return
        if (.not. fortmacs_diff_sessions(session_index)%is_valid_difference()) return
        ! Context must be saved before switching to windows A/B/C.
        control = fortmacs_diff_sessions(session_index)%control_buffer
        window = fortmacs_diff_sessions(session_index)%windows(buffer_type)
        if (.not. is_window_live(window)) return
        call fortmacs_diff_sessions(session_index)%get_difference_position(buffer_type, .false., beginning, status)
        if (.not. status%ok()) return
        call fortmacs_diff_sessions(session_index)%get_difference_position(buffer_type, .true., end, status)
        if (.not. status%ok()) return
        call select_window_begin(window, old_window, old_buffer, status)

        if (status%ok()) then
            call deactivate_mark()
            call position_fortmacs_diff_region(beginning, end, beginning, status)
        end if

        ! Select the old window again, keeping the first error.
        call select_window_end(old_window, old_buffer, st)
        if (status%ok()) status = st
        if (.not. status%ok()) return
        call set_buffer(control, status)
    end subroutine recenter_one_window

    ! Show BEGINNING to END in the selected window, centered when it
    ! fits, and put point at POSITION.  Port of ediff-util.el:ediff-position-region
    ! (windows jobs are deferred).
    recursive subroutine position_fortmacs_diff_region(beginning, end_in, position, status)
        integer(int64), intent(in) :: beginning, end_in, position
        type(status_type), intent(out) :: status
        integer(int64) :: end, lines, previous_point, line
        integer :: height

        end = min(end_in, point_maximum())
        call set_window_start(windows(selected_frame%selected_window%index), beginning, status)
        if (.not. status%ok()) return

        if (is_position_visible_in_window(end, selected_frame%selected_window)) then
            ! Determine the number of lines that the region occupies.
            lines = 0
            previous_point = 0

            do
                call move_to_window_line(prefix_argument_type(PREFIX_NUMBER, lines), line, status)
                if (.not. status%ok()) return
                if (.not. end > point()) exit
                ! `end' may be beyond the window bottom, so check that we
                ! are making progress.
                if (.not. previous_point < point()) exit
                previous_point = point()
                lines = lines + 1
            end do

            ! And position the beginning on the right line.
            call goto_character(beginning)
            height = window_total_height(selected_frame%selected_window, status)
            if (.not. status%ok()) return
            call recenter(prefix_argument_type(PREFIX_NUMBER, (1 + max(height - 1 - lines, 1_int64))/2), .false., status)
            if (.not. status%ok()) return
        end if

        call goto_character(position)
    end subroutine position_fortmacs_diff_region

    recursive module subroutine get_difference_at_point(self, buffer_type, diff_no, status, position, which_diff)
        class(fortmacs_diff_session_type), intent(inout), target :: self
        integer, intent(in) :: buffer_type
        integer, intent(out) :: diff_no
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: position
        integer, intent(in), optional :: which_diff
        type(handle_type) :: old
        type(status_type) :: st
        integer(int64) :: p, previous_beginning, previous_end, beginning, end
        integer :: maximum_dif_number, which
        integer :: session_index

        call self%get_index(session_index, status)
        if (.not. status%ok()) return
        old = current_buffer()
        maximum_dif_number = fortmacs_diff_sessions(session_index)%number_of_differences - 1
        diff_no = -1
        previous_beginning = 0
        previous_end = 0
        beginning = 0
        end = 0
        which = 0
        if (present(which_diff)) which = which_diff
        call enter_fortmacs_diff_buffer(fortmacs_diff_sessions(session_index)%buffers(buffer_type), status)
        if (.not. status%ok()) return
        p = point()
        if (present(position)) p = position

        do while ((p < previous_beginning .or. p > beginning) .and. diff_no < maximum_dif_number)
            diff_no = diff_no + 1
            previous_beginning = beginning
            previous_end = end
            call fortmacs_diff_sessions(session_index)%get_difference_position(buffer_type, .false., beginning, status, diff_no)
            if (status%ok()) call fortmacs_diff_sessions(session_index)%get_difference_position(buffer_type, .true., end, &
                                                                                                status, diff_no)

            ! Make the old buffer current again, keeping the first error.
            if (.not. status%ok()) then
                call set_buffer(old, st)
                return
            end if
        end do

        ! Boost diff-no by 1, if past the last diff region.
        if (which /= 0 .and. p > beginning .and. diff_no == maximum_dif_number) diff_no = diff_no + 1

        if (which == 1) then
            diff_no = diff_no + 1
        else if (which == -1) then
            continue
        else if (abs(count_lines(p, max(point_minimum(), previous_end))) < abs(count_lines(p, max(point_minimum(), &
                                                                                                  beginning)))) then
            ! Choose prev difference.
            continue
        else
            ! Choose next difference.
            diff_no = diff_no + 1
        end if

        call set_buffer(old, status)
    end subroutine get_difference_at_point

    recursive module subroutine copy_difference(self, difference, from_type, to_type, status, batch_invocation, region_to_copy)
        class(fortmacs_diff_session_type), intent(inout), target :: self
        integer, intent(in) :: difference, from_type, to_type
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: batch_invocation
        character(len=*), intent(in), optional :: region_to_copy
        type(handle_type) :: to_buffer, old
        type(status_type) :: st, restore_status
        character(len=:), allocatable :: reg_to_copy, reg_to_delete, messg
        integer(int64) :: reg_to_delete_beginning, reg_to_delete_end
        logical :: batch, saved
        integer :: session_index

        call self%get_index(session_index, status)
        if (.not. status%ok()) return
        batch = .false.
        if (present(batch_invocation)) batch = batch_invocation
        to_buffer = fortmacs_diff_sessions(session_index)%buffers(to_type)
        saved = .true.
        messg = ""
        call fortmacs_diff_sessions(session_index)%get_difference_position(to_type, .false., reg_to_delete_beginning, status, &
                                                                           difference)
        if (.not. status%ok()) return
        call fortmacs_diff_sessions(session_index)%get_difference_position(to_type, .true., reg_to_delete_end, status, difference)
        if (.not. status%ok()) return

        if (present(region_to_copy)) then
            reg_to_copy = region_to_copy
        else
            call fortmacs_diff_sessions(session_index)%get_region_contents(difference, from_type, reg_to_copy, status)
            if (.not. status%ok()) return
        end if

        call fortmacs_diff_sessions(session_index)%get_region_contents(difference, to_type, reg_to_delete, status, &
                                                                       reg_to_delete_beginning, reg_to_delete_end)
        if (.not. status%ok()) return

        if (reg_to_delete == reg_to_copy) then
            ! Don't copy identical buffers.
            saved = .false.
        else
            ! Seems ok to copy.
            if (.not. batch) then
                call fortmacs_diff_sessions(session_index)%test_save_region(difference, to_type, status)
                if (.not. status%ok()) return
            end if

            ! An error here is shown, as upstream's condition-case does.
            old = current_buffer()
            call set_buffer(to_buffer, st)

            if (st%ok()) then
                call goto_character(reg_to_delete_end)
                call insert(reg_to_copy, st)
            end if

            if (st%ok() .and. reg_to_delete_end > reg_to_delete_beginning) then
                call delete_region(reg_to_delete_beginning, reg_to_delete_end, st)
            end if

            ! Make the old buffer current again, keeping the first error.
            call set_buffer(old, restore_status)
            if (st%ok()) st = restore_status

            if (st%ok()) then
                if (.not. batch) call fortmacs_diff_sessions(session_index)%save_difference_region(difference, to_type, &
                                                                                                   reg_to_delete, messg)
            else
                call message("copy_difference: "//format_error(st))
                call ding()
                ! Let the user see the error msg.
                saved = .false.
            end if
        end if

        ! Adjust state of difference in case 3-way and diff was copied ok.
        if (saved .and. fortmacs_diff_sessions(session_index)%three_way_job) then
            call fortmacs_diff_sessions(session_index)%set_difference_state(difference, status)
            if (.not. status%ok()) return
        end if

        if (.not. batch) then
            call fortmacs_diff_sessions(session_index)%refresh_mode_lines()
            ! Make sure that the message about saving and how to restore is
            ! seen by the user.
            call message(messg)
        end if
    end subroutine copy_difference

    module subroutine save_difference_region(self, difference, buffer_type, text, messg)
        class(fortmacs_diff_session_type), intent(inout), target :: self
        integer, intent(in) :: difference, buffer_type
        character(len=*), intent(in) :: text
        character(len=:), allocatable, intent(out) :: messg
        type(fortmacs_diff_replaced_region_type) :: entry
        integer :: j
        character(len=:), allocatable :: recover
        character(len=20) :: digits

        j = self%get_replaced_region_index(difference, buffer_type)

        if (j > 0) then
            self%replaced_regions(j)%text = text
        else
            entry%n = difference
            entry%buffer_type = buffer_type
            entry%text = text

            if (.not. allocated(self%replaced_regions)) then
                allocate (self%replaced_regions(0))
            end if

            self%replaced_regions = [self%replaced_regions, entry]
        end if

        recover = ""

        if (.not. self%merge_job) then
            recover = char(ichar(fortmacs_diff_buffer_type_name(buffer_type)) + 32)
        end if

        write (digits, "(i0)") difference + 1
        messg = "Saving old diff region #"//trim(digits)//" of buffer "// &
                fortmacs_diff_buffer_type_name(buffer_type)//substitute_quotes(".  To recover, type `r")//recover// &
                substitute_quotes("'")
        call message(messg)
    end subroutine save_difference_region

    module integer function get_replaced_region_index(self, difference, buffer_type) result(j)
        class(fortmacs_diff_session_type), intent(in) :: self
        integer, intent(in) :: difference, buffer_type

        j = 0
        if (.not. allocated(self%replaced_regions)) return
        j = findloc(self%replaced_regions%n == difference .and. &
                    self%replaced_regions%buffer_type == buffer_type, .true., dim=1)
    end function get_replaced_region_index

    recursive module subroutine test_save_region(self, difference, buffer_type, status)
        class(fortmacs_diff_session_type), intent(inout), target :: self
        integer, intent(in) :: difference, buffer_type
        type(status_type), intent(out) :: status
        logical :: answer
        character(len=20) :: digits
        integer :: session_index

        call self%get_index(session_index, status)
        if (.not. status%ok()) return
        if (fortmacs_diff_sessions(session_index)%get_replaced_region_index(difference, buffer_type) == 0) return
        write (digits, "(i0)") difference + 1
        call read_yes_or_no("You've previously copied diff region "//trim(digits)//" to buffer "// &
                            fortmacs_diff_buffer_type_name(buffer_type)//".  Confirm? ", answer, status)
        if (.not. status%ok()) return
        if (.not. answer) call set_error(status, QUIT_CODE)
    end subroutine test_save_region

    recursive module subroutine pop_difference(self, difference, buffer_type, status)
        class(fortmacs_diff_session_type), intent(inout), target :: self
        integer, intent(in) :: difference, buffer_type
        type(status_type), intent(out) :: status
        type(handle_type) :: old
        type(status_type) :: st, restore_status
        character(len=:), allocatable :: saved_diff
        type(fortmacs_diff_replaced_region_type), allocatable :: rest(:)
        integer(int64) :: reg_beginning, reg_end
        integer :: i, j
        logical :: recovered
        character(len=20) :: digits
        integer :: session_index

        call self%get_index(session_index, status)
        if (.not. status%ok()) return
        write (digits, "(i0)") difference + 1
        j = fortmacs_diff_sessions(session_index)%get_replaced_region_index(difference, buffer_type)

        if (j > 0) then
            saved_diff = fortmacs_diff_sessions(session_index)%replaced_regions(j)%text
        else if (fortmacs_diff_sessions(session_index)%number_of_differences > 0) then
            call set_error(status, USER_ERROR_CODE, "Nothing saved for diff "//trim(digits)//" in buffer "// &
                           fortmacs_diff_buffer_type_name(buffer_type))
            return
        else
            call set_error(status, GENERIC_ERROR_CODE, FORTMACS_DIFF_NO_DIFFERENCES)
            return
        end if

        call fortmacs_diff_sessions(session_index)%get_difference_position(buffer_type, .false., reg_beginning, status, difference)
        if (.not. status%ok()) return
        call fortmacs_diff_sessions(session_index)%get_difference_position(buffer_type, .true., reg_end, status, difference)
        if (.not. status%ok()) return
        ! An error here is shown, as upstream's condition-case does.
        old = current_buffer()
        call enter_fortmacs_diff_buffer(fortmacs_diff_sessions(session_index)%buffers(buffer_type), st)

        if (st%ok()) then
            call goto_character(reg_end)
            call insert(saved_diff, st)
            if (st%ok() .and. reg_end > reg_beginning) call delete_region(reg_beginning, reg_end, st)
        end if

        ! Make the old buffer current again, keeping the first error.
        call set_buffer(old, restore_status)
        if (st%ok()) st = restore_status
        recovered = st%ok()

        if (.not. recovered) then
            call message("pop_difference: "//format_error(st))
            call ding()
        end if

        ! Adjust state of difference.
        if (fortmacs_diff_sessions(session_index)%three_way_job .and. recovered) then
            call fortmacs_diff_sessions(session_index)%set_difference_state(difference, status)
            if (.not. status%ok()) return
        end if

        call fortmacs_diff_sessions(session_index)%refresh_mode_lines()

        if (recovered) then
            rest = pack(fortmacs_diff_sessions(session_index)%replaced_regions, [(i /= j, i=1, &
                                                                     size(fortmacs_diff_sessions(session_index)%replaced_regions))])
            call move_alloc(rest, fortmacs_diff_sessions(session_index)%replaced_regions)
            call message("Diff region "//trim(digits)//" in buffer "//fortmacs_diff_buffer_type_name(buffer_type)// &
                         " restored")
        end if
    end subroutine pop_difference

    recursive module subroutine quit(self, reverse_default_keep_variants, status)
        class(fortmacs_diff_session_type), intent(inout), target :: self
        logical, intent(in) :: reverse_default_keep_variants
        type(status_type), intent(out) :: status
        type(handle_type) :: control_buffer, meta_buffer
        logical :: keep_variants
        integer :: t, session_number
        integer :: session_index

        call self%get_index(session_index, status)
        if (.not. status%ok()) return
        control_buffer = fortmacs_diff_sessions(session_index)%control_buffer
        meta_buffer = fortmacs_diff_sessions(session_index)%meta_buffer
        session_number = fortmacs_diff_sessions(session_index)%meta_session_number
        call fortmacs_diff_sessions(session_index)%unhighlight_differences_totally(status)
        if (.not. status%ok()) return
        call fortmacs_diff_sessions(session_index)%clear_differences()
        ! Restore the visibility range.  This affects only
        ! ediff-*-regions/windows.
        fortmacs_diff_sessions(session_index)%visible_bounds = fortmacs_diff_sessions(session_index)%wide_bounds
        ! Apply selective display to narrow or widen.
        call fortmacs_diff_sessions(session_index)%make_region_visible(status)
        if (.not. status%ok()) return

        do t = FORTMACS_DIFF_BUFFER_TYPE_A, FORTMACS_DIFF_BUFFER_TYPE_C
            if (is_overlay(fortmacs_diff_sessions(session_index)%wide_bounds(t))) then
                call destroy_overlay(fortmacs_diff_sessions(session_index)%wide_bounds(t))
            end if

            fortmacs_diff_sessions(session_index)%wide_bounds(t) = null_handle()
            fortmacs_diff_sessions(session_index)%narrow_bounds(t) = null_handle()
            fortmacs_diff_sessions(session_index)%visible_bounds(t) = null_handle()
        end do

        if (allocated(fortmacs_diff_session_registry)) then
            fortmacs_diff_session_registry = pack(fortmacs_diff_session_registry,.not. same_handle(fortmacs_diff_session_registry, &
                                                                                                   control_buffer))
        end if

        call update_fortmacs_diff_registry(status)
        if (.not. status%ok()) return
        call set_buffer(control_buffer, status)
        if (.not. status%ok()) return

        ! If the user interrupts (canceling saving the merge buffer),
        ! continue normally.
        if (fortmacs_diff_sessions(session_index)%merge_job) then
            call fortmacs_diff_quit_merge_hook%execute(status)
            if (status%code == QUIT_CODE) status = status_type()
            if (.not. status%ok()) return
        end if

        call fortmacs_diff_cleanup_hook%execute(status)
        if (.not. status%ok()) return
        ! reverse-default-keep-variants is t if the user quits with a
        ! prefix arg.
        keep_variants = fortmacs_diff_sessions(session_index)%keep_variants
        if (reverse_default_keep_variants) keep_variants = .not. keep_variants
        call fortmacs_diff_sessions(session_index)%dispose_of_variants(.true., keep_variants, status)
        if (.not. status%ok()) return
        ! One hook here is clean_up_fortmacs_diff_mess, which kills the control
        ! buffer and other auxiliary buffers.
        call fortmacs_diff_quit_hook%execute(status)
        if (.not. status%ok()) return
        call update_fortmacs_diff_meta_buffer(meta_buffer, status, session_number=session_number)
        if (.not. status%ok()) return
        ! What ediff-mult.el:ediff-filegroup-action adds to
        ! `ediff-after-quit-hook-internal': back to the session group.
        if (is_buffer_live(meta_buffer)) call show_fortmacs_diff_meta_buffer(status, meta_buffer, session_number=session_number)
    end subroutine quit

    recursive module subroutine dispose_of_variants(self, ask, keep_variants, status)
        class(fortmacs_diff_session_type), intent(inout), target :: self
        logical, intent(in) :: ask, keep_variants
        type(status_type), intent(out) :: status
        type(handle_type) :: buffers_of(4)
        integer :: session_index

        call self%get_index(session_index, status)
        if (.not. status%ok()) return
        buffers_of = fortmacs_diff_sessions(session_index)%buffers
        call dispose_of_fortmacs_diff_variant(buffers_of(FORTMACS_DIFF_BUFFER_TYPE_A), FORTMACS_DIFF_BUFFER_TYPE_A, ask, &
                                              keep_variants, status)
        if (.not. status%ok()) return
        call dispose_of_fortmacs_diff_variant(buffers_of(FORTMACS_DIFF_BUFFER_TYPE_B), FORTMACS_DIFF_BUFFER_TYPE_B, ask, &
                                              keep_variants, status)
        if (.not. status%ok()) return

        ! When merging, the ancestor buffer is disposed of instead of buffer C.
        if (fortmacs_diff_sessions(session_index)%merge_job) then
            call dispose_of_fortmacs_diff_variant(buffers_of(FORTMACS_DIFF_BUFFER_TYPE_ANCESTOR), &
                                                  FORTMACS_DIFF_BUFFER_TYPE_ANCESTOR, ask, &
                                                  keep_variants, status)
        else
            call dispose_of_fortmacs_diff_variant(buffers_of(FORTMACS_DIFF_BUFFER_TYPE_C), FORTMACS_DIFF_BUFFER_TYPE_C, ask, &
                                                  keep_variants, status)
        end if
    end subroutine dispose_of_variants

    ! Kill the variant buffer BUFFER of BUFFER_TYPE, according to user
    ! directives (ask, kill unconditionally, keep).  Port of
    ! ediff-util.el:ediff-dispose-of-variant-according-to-user (indirect
    ! buffers are deferred with region jobs).
    recursive subroutine dispose_of_fortmacs_diff_variant(buffer, buffer_type, ask, keep_variants, status)
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
            call read_y_or_n("Kill buffer "//fortmacs_diff_buffer_type_name(buffer_type)//" ["//buffers(buffer%index)%name// &
                             "]? ", answer, status)
            if (.not. status%ok() .or. .not. answer) return
        end if

        call close_fortmacs_diff_buffer_carefully(buffer, status)
    end subroutine dispose_of_fortmacs_diff_variant

    recursive module subroutine select_difference(self, difference, status)
        class(fortmacs_diff_session_type), intent(inout), target :: self
        integer, intent(in) :: difference
        type(status_type), intent(out) :: status
        integer :: t
        integer :: session_index

        call self%get_index(session_index, status)
        if (.not. status%ok()) return
        if (.not. is_buffer_live(fortmacs_diff_sessions(session_index)%buffers(FORTMACS_DIFF_BUFFER_TYPE_A))) return
        if (.not. is_buffer_live(fortmacs_diff_sessions(session_index)%buffers(FORTMACS_DIFF_BUFFER_TYPE_B))) return
        if (.not. fortmacs_diff_sessions(session_index)%is_valid_difference(difference)) return

        if (has_fortmacs_diff_face_support() .and. fortmacs_diff_sessions(session_index)%use_faces) then
            do t = FORTMACS_DIFF_BUFFER_TYPE_A, FORTMACS_DIFF_BUFFER_TYPE_ANCESTOR
                call fortmacs_diff_sessions(session_index)%highlight_difference_in_one_buffer(difference, t, status)
                if (.not. status%ok()) return
            end do
        else if (fortmacs_diff_sessions(session_index)%highlighting_style == FORTMACS_DIFF_HIGHLIGHTING_STYLE_ASCII) then
            do t = FORTMACS_DIFF_BUFFER_TYPE_A, FORTMACS_DIFF_BUFFER_TYPE_C
                if (t == FORTMACS_DIFF_BUFFER_TYPE_C .and. .not. fortmacs_diff_sessions(session_index)%three_way_job) exit
                call fortmacs_diff_sessions(session_index)%place_flags_in_buffer(t, difference, status)
                if (.not. status%ok()) return
            end do
        end if

        ! Set current difference here so the hook will be able to refer to
        ! it.
        fortmacs_diff_sessions(session_index)%current_difference = difference
        call fortmacs_diff_select_hook%execute(status)
    end subroutine select_difference

    recursive module subroutine unselect_difference(self, difference, status)
        class(fortmacs_diff_session_type), intent(inout), target :: self
        integer, intent(in) :: difference
        type(status_type), intent(out) :: status
        integer :: t
        integer :: session_index

        call self%get_index(session_index, status)
        if (.not. status%ok()) return
        if (.not. fortmacs_diff_sessions(session_index)%is_valid_difference(difference)) return

        if (has_fortmacs_diff_face_support() .and. fortmacs_diff_sessions(session_index)%use_faces) then
            do t = FORTMACS_DIFF_BUFFER_TYPE_A, FORTMACS_DIFF_BUFFER_TYPE_ANCESTOR
                call fortmacs_diff_sessions(session_index)%unhighlight_difference_in_one_buffer(t, status)
                if (.not. status%ok()) return
            end do
        else if (fortmacs_diff_sessions(session_index)%highlighting_style == FORTMACS_DIFF_HIGHLIGHTING_STYLE_ASCII) then
            do t = FORTMACS_DIFF_BUFFER_TYPE_A, FORTMACS_DIFF_BUFFER_TYPE_C
                if (t == FORTMACS_DIFF_BUFFER_TYPE_C .and. .not. fortmacs_diff_sessions(session_index)%three_way_job) exit
                call remove_fortmacs_diff_flags(fortmacs_diff_sessions(session_index)%differences(t, difference + 1)%overlay, &
                                                status)
                if (.not. status%ok()) return
            end do
        end if

        call fortmacs_diff_unselect_hook%execute(status)
    end subroutine unselect_difference

    recursive module subroutine unselect_and_select_difference(self, difference, status, flag, no_recenter)
        class(fortmacs_diff_session_type), intent(inout), target :: self
        integer, intent(in) :: difference
        type(status_type), intent(out) :: status
        integer, intent(in), optional :: flag
        logical, intent(in), optional :: no_recenter
        integer :: saved, how
        logical :: recentering
        integer :: session_index

        call self%get_index(session_index, status)
        if (.not. status%ok()) return
        how = 0
        if (present(flag)) how = flag
        recentering = .true.
        if (present(no_recenter)) recentering = .not. no_recenter
        status = status_type()

        if (recentering) then
            ! Recenter on difference N without highlighting it again.
            saved = fortmacs_diff_sessions(session_index)%current_difference
            fortmacs_diff_sessions(session_index)%current_difference = difference
            call fortmacs_diff_sessions(session_index)%recenter(.true., status)
            fortmacs_diff_sessions(session_index)%current_difference = saved
            if (.not. status%ok()) return
        end if

        if (how /= FORTMACS_DIFF_SELECT_ONLY) then
            call fortmacs_diff_sessions(session_index)%unselect_difference(fortmacs_diff_sessions( &
                                                                           session_index)%current_difference, status)
            if (.not. status%ok()) return
        end if

        if (how /= FORTMACS_DIFF_UNSELECT_ONLY) then
            call fortmacs_diff_sessions(session_index)%select_difference(difference, status)
            if (.not. status%ok()) return
        end if

        ! Need to set current diff here even though it is also set in
        ! select_difference because it might not be called if
        ! unselect-only is specified.
        fortmacs_diff_sessions(session_index)%current_difference = difference

        if (is_buffer_live(fortmacs_diff_sessions(session_index)%control_buffer)) then
            call set_buffer(fortmacs_diff_sessions(session_index)%control_buffer, status)
            if (.not. status%ok()) return
        end if

        call fortmacs_diff_sessions(session_index)%refresh_mode_lines()
    end subroutine unselect_and_select_difference

    recursive module subroutine highlight_difference_in_one_buffer(self, difference, buffer_type, status)
        class(fortmacs_diff_session_type), intent(inout), target :: self
        integer, intent(in) :: difference, buffer_type
        type(status_type), intent(out) :: status
        type(handle_type) :: buffer, old
        integer(int64) :: last, beginning, end, end_hilit
        integer :: session_index

        call self%get_index(session_index, status)
        if (.not. status%ok()) return
        buffer = fortmacs_diff_sessions(session_index)%buffers(buffer_type)
        if (.not. is_buffer_live(buffer)) return
        if (.not. allocated(fortmacs_diff_sessions(session_index)%differences)) return
        old = current_buffer()
        call set_buffer(buffer, status)
        if (.not. status%ok()) return
        last = point_maximum()
        call set_buffer(old, status)
        if (.not. status%ok()) return
        call fortmacs_diff_sessions(session_index)%get_difference_position(buffer_type, .false., beginning, status, difference)
        if (.not. status%ok()) return
        call fortmacs_diff_sessions(session_index)%get_difference_position(buffer_type, .true., end, status, difference)
        if (.not. status%ok()) return
        end_hilit = end
        if (beginning == end) end_hilit = end + 1
        end_hilit = min(last, end_hilit)
        call move_fortmacs_diff_overlay(fortmacs_diff_sessions(session_index)%current_diff_overlays(buffer_type), beginning, &
                                        end_hilit, status)
        if (.not. status%ok()) return
        ! Unhighlight the background overlay for diff n so it won't
        ! interfere with the current diff overlay.
        call set_fortmacs_diff_overlay_face(fortmacs_diff_sessions(session_index)%differences(buffer_type, &
                                                                                              difference + 1)%overlay, "", status)
    end subroutine highlight_difference_in_one_buffer

    recursive module subroutine unhighlight_difference_in_one_buffer(self, buffer_type, status)
        class(fortmacs_diff_session_type), intent(inout), target :: self
        integer, intent(in) :: buffer_type
        type(status_type), intent(out) :: status
        integer :: n
        integer :: session_index

        call self%get_index(session_index, status)
        if (.not. status%ok()) return
        if (.not. is_buffer_live(fortmacs_diff_sessions(session_index)%buffers(buffer_type))) return
        call move_fortmacs_diff_overlay(fortmacs_diff_sessions(session_index)%current_diff_overlays(buffer_type), 1_int64, &
                                        1_int64, status)
        if (.not. status%ok()) return
        n = fortmacs_diff_sessions(session_index)%current_difference
        if (.not. fortmacs_diff_sessions(session_index)%is_valid_difference(n)) return

        ! Rehighlight the overlay in the background of the current
        ! difference region.
        if (has_fortmacs_diff_face_support() .and. fortmacs_diff_sessions(session_index)%use_faces &
            .and. fortmacs_diff_sessions(session_index)%highlight_all_diffs) then
            call set_fortmacs_diff_overlay_face(fortmacs_diff_sessions(session_index)%differences(buffer_type, n + 1)%overlay, &
                                                fortmacs_diff_background_face(buffer_type, n), status)
        else
            call set_fortmacs_diff_overlay_face(fortmacs_diff_sessions(session_index)%differences(buffer_type, &
                                                                                                  n + 1)%overlay, "", status)
        end if
    end subroutine unhighlight_difference_in_one_buffer

    recursive module subroutine unhighlight_differences_totally(self, status)
        class(fortmacs_diff_session_type), intent(inout), target :: self
        type(status_type), intent(out) :: status
        integer :: t
        integer :: session_index

        call self%get_index(session_index, status)
        if (.not. status%ok()) return

        do t = FORTMACS_DIFF_BUFFER_TYPE_A, FORTMACS_DIFF_BUFFER_TYPE_ANCESTOR
            call fortmacs_diff_sessions(session_index)%unselect_and_select_difference(-1, status)
            if (.not. status%ok()) return

            if (has_fortmacs_diff_face_support() .and. fortmacs_diff_sessions(session_index)%use_faces) then
                call fortmacs_diff_sessions(session_index)%paint_background_regions(.true., status)
                if (.not. status%ok()) return

                if (is_overlay(fortmacs_diff_sessions(session_index)%current_diff_overlays(t))) then
                    call destroy_overlay(fortmacs_diff_sessions(session_index)%current_diff_overlays(t))
                end if

                fortmacs_diff_sessions(session_index)%current_diff_overlays(t) = null_handle()
            end if
        end do
    end subroutine unhighlight_differences_totally

    recursive module subroutine place_flags_in_buffer(self, buffer_type, difference, status)
        class(fortmacs_diff_session_type), intent(inout), target :: self
        integer, intent(in) :: buffer_type, difference
        type(status_type), intent(out) :: status
        type(handle_type) :: old, overlay
        type(status_type) :: st
        integer(int64) :: before, after
        logical :: ascii
        integer :: session_index

        call self%get_index(session_index, status)
        if (.not. status%ok()) return
        if (.not. is_buffer_live(fortmacs_diff_sessions(session_index)%buffers(buffer_type))) return
        overlay = fortmacs_diff_sessions(session_index)%differences(buffer_type, difference + 1)%overlay
        ascii = fortmacs_diff_sessions(session_index)%highlighting_style == FORTMACS_DIFF_HIGHLIGHTING_STYLE_ASCII
        call fortmacs_diff_sessions(session_index)%get_difference_position(buffer_type, .false., before, status, difference)
        if (.not. status%ok()) return
        old = current_buffer()
        call set_buffer(fortmacs_diff_sessions(session_index)%buffers(buffer_type), status)
        if (.not. status%ok()) return
        ! Insert flag before the difference.
        call goto_character(before)

        if (.not. ascii) then
            call overlay_put(overlay, "before_string", value_type(), status)
        else if (is_beginning_of_line()) then
            call overlay_put(overlay, "before_string", value_type(text=fortmacs_diff_before_flag_beginning_of_line), status)
        else
            call overlay_put(overlay, "before_string", value_type(text=fortmacs_diff_before_flag_mol), status)
        end if

        ! Insert the flag after the difference.
        if (status%ok()) call fortmacs_diff_sessions(session_index)%get_difference_position(buffer_type, .true., after, &
                                                                                            status, difference)

        if (status%ok()) then
            call goto_character(after)

            if (.not. ascii) then
                call overlay_put(overlay, "after_string", value_type(), status)
            else if (is_beginning_of_line()) then
                call overlay_put(overlay, "after_string", value_type(text=fortmacs_diff_after_flag_end_of_line), status)
            else
                call overlay_put(overlay, "after_string", value_type(text=fortmacs_diff_after_flag_mol), status)
            end if
        end if

        ! Make the old buffer current again, keeping the first error.
        call set_buffer(old, st)
        if (status%ok()) status = st
    end subroutine place_flags_in_buffer

    ! Remove the ASCII flags of OVERLAY.  Port of
    ! ediff-util.el:ediff-remove-flags-from-buffer.
    subroutine remove_fortmacs_diff_flags(overlay, status)
        type(handle_type), intent(in) :: overlay
        type(status_type), intent(out) :: status

        call overlay_put(overlay, "before_string", value_type(), status)
        if (.not. status%ok()) return
        call overlay_put(overlay, "after_string", value_type(), status)
    end subroutine remove_fortmacs_diff_flags

    recursive module subroutine get_difference_position(self, buffer_type, at_end, position, status, difference)
        class(fortmacs_diff_session_type), intent(inout), target :: self
        integer, intent(in) :: buffer_type
        logical, intent(in) :: at_end
        integer(int64), intent(out) :: position
        type(status_type), intent(out) :: status
        integer, intent(in), optional :: difference
        type(handle_type) :: overlay
        integer :: m
        integer :: session_index

        call self%get_index(session_index, status)
        if (.not. status%ok()) return
        position = 0
        m = fortmacs_diff_sessions(session_index)%current_difference
        if (present(difference)) m = difference

        if (m < 0 .or. m >= fortmacs_diff_sessions(session_index)%number_of_differences) then
            if (fortmacs_diff_sessions(session_index)%number_of_differences > 0) then
                call set_error(status, GENERIC_ERROR_CODE, fortmacs_diff_sessions(session_index)%get_bad_difference_message(m))
            else
                call set_error(status, GENERIC_ERROR_CODE, FORTMACS_DIFF_NO_DIFFERENCES)
            end if

            return
        end if

        overlay = fortmacs_diff_sessions(session_index)%differences(buffer_type, m + 1)%overlay

        if (.not. is_buffer_live(overlay_buffer(overlay))) then
            call set_error(status, GENERIC_ERROR_CODE, FORTMACS_DIFF_KILLED_VITAL_BUFFER)
            return
        end if

        if (at_end) then
            position = overlay_end(overlay)
        else
            position = overlay_start(overlay)
        end if
    end subroutine get_difference_position

    recursive module subroutine get_region_contents(self, difference, buffer_type, text, status, start, end)
        class(fortmacs_diff_session_type), intent(inout), target :: self
        integer, intent(in) :: difference, buffer_type
        character(len=:), allocatable, intent(out) :: text
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: start, end
        type(handle_type) :: old
        type(status_type) :: st
        integer(int64) :: s, e
        integer :: session_index

        call self%get_index(session_index, status)
        if (.not. status%ok()) return
        text = ""

        if (present(start)) then
            s = start
        else
            call fortmacs_diff_sessions(session_index)%get_difference_position(buffer_type, .false., s, status, difference)
            if (.not. status%ok()) return
        end if

        if (present(end)) then
            e = end
        else
            call fortmacs_diff_sessions(session_index)%get_difference_position(buffer_type, .true., e, status, difference)
            if (.not. status%ok()) return
        end if

        old = current_buffer()
        call enter_fortmacs_diff_buffer(fortmacs_diff_sessions(session_index)%buffers(buffer_type), status)
        if (.not. status%ok()) return
        call buffer_substring(s, e, text, status)
        ! Make the old buffer current again, keeping the first error.
        call set_buffer(old, st)
        if (status%ok()) status = st
    end subroutine get_region_contents

    recursive module subroutine restore_highlighting(self, status)
        class(fortmacs_diff_session_type), intent(inout), target :: self
        type(status_type), intent(out) :: status
        integer :: session_index

        call self%get_index(session_index, status)
        if (.not. status%ok()) return

        if (has_fortmacs_diff_face_support() .and. fortmacs_diff_sessions(session_index)%use_faces &
            .and. fortmacs_diff_sessions(session_index)%highlight_all_diffs) then
            call fortmacs_diff_sessions(session_index)%paint_background_regions(.false., status)
            if (.not. status%ok()) return
        end if

        call fortmacs_diff_sessions(session_index)%select_difference(fortmacs_diff_sessions(session_index)%current_difference, &
                                                                     status)
    end subroutine restore_highlighting

    module subroutine clear_differences(self)
        class(fortmacs_diff_session_type), intent(inout), target :: self
        integer :: t, n

        if (.not. allocated(self%differences)) return

        do n = 1, size(self%differences, 2)
            do t = FORTMACS_DIFF_BUFFER_TYPE_A, FORTMACS_DIFF_BUFFER_TYPE_ANCESTOR
                if (is_overlay(self%differences(t, n)%overlay)) then
                    call destroy_overlay(self%differences(t, n)%overlay)
                end if
            end do
        end do

        deallocate (self%differences)
        allocate (self%differences(4, 0))
    end subroutine clear_differences

    recursive module subroutine make_current_difference_overlay(self, buffer_type, status)
        class(fortmacs_diff_session_type), intent(inout), target :: self
        integer, intent(in) :: buffer_type
        type(status_type), intent(out) :: status
        integer(int64) :: position
        integer :: session_index

        call self%get_index(session_index, status)
        if (.not. status%ok()) return
        if (.not. has_fortmacs_diff_face_support()) return
        ! (point-max) of the control buffer.
        position = point_maximum()
        call make_fortmacs_diff_bullet_proof_overlay(position, position, &
                                                     fortmacs_diff_sessions(session_index)%buffers(buffer_type), &
                                                   fortmacs_diff_sessions(session_index)%current_diff_overlays(buffer_type), status)
        if (.not. status%ok()) return
        call set_fortmacs_diff_overlay_face(fortmacs_diff_sessions(session_index)%current_diff_overlays(buffer_type), &
                                            trim(FORTMACS_DIFF_CURRENT_DIFF_FACES(buffer_type)), status)
    end subroutine make_current_difference_overlay

    recursive module subroutine make_region_visible(self, status)
        class(fortmacs_diff_session_type), intent(inout), target :: self
        type(status_type), intent(out) :: status
        type(handle_type) :: old
        type(status_type) :: st
        integer :: t
        integer :: session_index

        call self%get_index(session_index, status)
        if (.not. status%ok()) return

        associate (b => fortmacs_diff_sessions(session_index)%buffers)
            if (same_handle(b(FORTMACS_DIFF_BUFFER_TYPE_A), b(FORTMACS_DIFF_BUFFER_TYPE_B))) return
            if (same_handle(b(FORTMACS_DIFF_BUFFER_TYPE_A), b(FORTMACS_DIFF_BUFFER_TYPE_C))) return
            if (same_handle(b(FORTMACS_DIFF_BUFFER_TYPE_C), b(FORTMACS_DIFF_BUFFER_TYPE_B))) return
        end associate

        ! If ediff-*-regions/windows, ediff-visible-bounds is already set.
        ! Otherwise, always use full range.
        if (.not. fortmacs_diff_sessions(session_index)%narrow_job) then
            fortmacs_diff_sessions(session_index)%visible_bounds = fortmacs_diff_sessions(session_index)%wide_bounds
        end if

        old = current_buffer()

        do t = FORTMACS_DIFF_BUFFER_TYPE_A, FORTMACS_DIFF_BUFFER_TYPE_C
            if (t == FORTMACS_DIFF_BUFFER_TYPE_C .and. .not. fortmacs_diff_sessions(session_index)%three_way_job) exit

            associate (overlay => fortmacs_diff_sessions(session_index)%visible_bounds(t))
                if (.not. is_buffer_live(overlay_buffer(overlay))) cycle
                call enter_fortmacs_diff_buffer(fortmacs_diff_sessions(session_index)%buffers(t), status)
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
    end subroutine make_region_visible

    module subroutine check_ancestor_exists(self, status)
        class(fortmacs_diff_session_type), intent(inout), target :: self
        type(status_type), intent(out) :: status
        integer :: session_index

        call self%get_index(session_index, status)
        if (.not. status%ok()) return
        if (is_buffer_live(fortmacs_diff_sessions(session_index)%buffers(FORTMACS_DIFF_BUFFER_TYPE_ANCESTOR))) return

        if (fortmacs_diff_sessions(session_index)%merge_with_ancestor_job) then
            call set_error(status, USER_ERROR_CODE, "Lost connection to ancestor buffer.  This shouldn't happen.  "// &
                           "Please report this bug to bug-gnu-emacs@gnu.org")
        else
            call set_error(status, USER_ERROR_CODE, "Not merging with ancestor")
        end if
    end subroutine check_ancestor_exists

    recursive module subroutine save_and_delete_merge(self, save_and_continue, status)
        class(fortmacs_diff_session_type), intent(inout), target :: self
        logical, intent(in) :: save_and_continue
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: merge_store_file
        logical :: always
        integer :: session_index

        call self%get_index(session_index, status)
        if (.not. status%ok()) return
        ! Fake ediff-autostore-merges, if necessary.
        always = save_and_continue .or. fortmacs_diff_sessions(session_index)%autostore_merges%value
        if (.not. always .and. fortmacs_diff_sessions(session_index)%autostore_merges%choice &
            /= FORTMACS_DIFF_AUTOSTORE_MERGES_GROUP_JOBS_ONLY) return

        if (allocated(fortmacs_diff_sessions(session_index)%merge_store_file)) then
            ! Store, ask to delete.
            call write_fortmacs_diff_merge_buffer_and_maybe_close(fortmacs_diff_sessions(session_index)%buffers( &
                                     FORTMACS_DIFF_BUFFER_TYPE_C), fortmacs_diff_sessions(session_index)%merge_store_file, .true., &
                                                                  save_and_continue, status)
        else if (always) then
            ! Ask for file name.
            call read_file_name("Save the result of the merge in file: ", merge_store_file, status)
            if (.not. status%ok()) return
            call write_fortmacs_diff_merge_buffer_and_maybe_close(fortmacs_diff_sessions(session_index)%buffers( &
                                                 FORTMACS_DIFF_BUFFER_TYPE_C), merge_store_file, .false., save_and_continue, status)
        end if
        ! Otherwise the parent session group's job passed no file.
    end subroutine save_and_delete_merge

    ! Write merge buffer BUFFER into FILE (asking before overwriting an
    ! existing file); with SHOW_FILE, say where.  Unless
    ! SAVE_AND_CONTINUE, offer to close the buffer.  Port of
    ! ediff-util.el:ediff-write-merge-buffer-and-maybe-kill.
    recursive subroutine write_fortmacs_diff_merge_buffer_and_maybe_close(buffer, file, show_file, save_and_continue, status)
        type(handle_type), intent(in) :: buffer
        character(len=*), intent(in) :: file
        logical, intent(in) :: show_file, save_and_continue
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: expanded, warn_message
        type(handle_type) :: visiting, old
        type(status_type) :: st
        logical :: answer

        call expand_file_name(file, expanded, status)
        if (.not. status%ok()) return
        visiting = get_file_buffer(expanded)

        if (.not. same_handle(visiting, buffer)) then
            warn_message = "Another buffer is visiting file "//file//". Too dangerous to save the merge buffer"
            call ding()
            call message(warn_message)
            call with_output_to_temporary_buffer(" *ediff-message*", LF//LF//warn_message//LF//LF, status)
            if (.not. status%ok()) return
            call sit_for(2000)
            return
        end if

        old = current_buffer()
        call enter_fortmacs_diff_buffer(buffer, status)
        if (.not. status%ok()) return
        answer = .true.
        if (file_exists(file)) call read_y_or_n("File "//file//" exists, overwrite? ", answer, status)

        if (status%ok() .and. answer) then
            call set_visited_file_name(file, status)
            if (status%ok()) call save_buffer(1_int64, .false., status)

            if (status%ok() .and. show_file) then
                call message("Merge buffer saved in: "//file)
                call restore_buffer_modified(.false.)
                call sit_for(3000)
            end if

            if (status%ok() .and. .not. save_and_continue) then
                call read_y_or_n("Merge buffer saved.  Now kill the buffer? ", answer, status)
                if (status%ok() .and. answer) call close_fortmacs_diff_buffer_carefully(buffer, status)
            end if
        end if

        ! Make the old buffer current again, keeping the first error.
        if (is_buffer_live(old)) then
            call set_buffer(old, st)
            if (status%ok()) status = st
        end if

    contains

        ! Whether A and B are the same buffer.
        logical function same_handle(a, b)
            type(handle_type), intent(in) :: a, b

            same_handle = a%index == b%index .and. a%generation == b%generation
        end function same_handle
    end subroutine write_fortmacs_diff_merge_buffer_and_maybe_close

    ! If a buffer visits FILE, where the merge is to be saved, offer to
    ! save and close it; an error condition occurs if the user declines.
    ! Port of ediff-util.el:ediff-verify-file-merge-buffer.
    recursive subroutine verify_fortmacs_diff_file_merge_buffer(file, status)
        character(len=*), intent(in) :: file
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: expanded, warn_message
        type(handle_type) :: buffer, old
        type(status_type) :: st
        logical :: answer

        call expand_file_name(file, expanded, status)
        if (.not. status%ok()) return
        buffer = get_file_buffer(expanded)
        if (.not. is_buffer_live(buffer)) return
        warn_message = "Buffer "//buffers(buffer%index)%name//" is visiting "//file//". Save and kill the buffer? "
        call with_output_to_temporary_buffer(" *ediff-message*", LF//LF//warn_message//LF//LF, status)
        if (.not. status%ok()) return
        call message(warn_message)
        call read_y_or_n(warn_message, answer, status)
        if (.not. status%ok()) return

        if (.not. answer) then
            call set_error(status, GENERIC_ERROR_CODE, "Too dangerous to merge versions of a file visited by another buffer")
            return
        end if

        old = current_buffer()
        call set_buffer(buffer, status)
        if (.not. status%ok()) return
        call save_buffer(1_int64, .false., status)
        if (status%ok()) call close_buffer(buffer, status)

        ! Make the old buffer current again, keeping the first error.
        if (is_buffer_live(old)) then
            call set_buffer(old, st)
            if (status%ok()) status = st
        end if
    end subroutine verify_fortmacs_diff_file_merge_buffer

    recursive module subroutine compute_custom_diffs(self, status)
        class(fortmacs_diff_session_type), intent(inout), target :: self
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: file_a, file_b
        type(handle_type) :: old
        type(status_type) :: st
        integer :: session_index

        call self%get_index(session_index, status)
        if (.not. status%ok()) return
        call temporary_unless_visited(FORTMACS_DIFF_BUFFER_TYPE_A, file_a, status)
        if (.not. status%ok()) return
        call temporary_unless_visited(FORTMACS_DIFF_BUFFER_TYPE_B, file_b, status)
        if (.not. status%ok()) return

        if (.not. is_buffer_live(fortmacs_diff_sessions(session_index)%custom_diff_buffer)) then
            call get_or_make_buffer(fortmacs_diff_unique_buffer_name("*ediff-custom-diff", "*"), &
                                    fortmacs_diff_sessions(session_index)%custom_diff_buffer, status)
            if (.not. status%ok()) return
        end if

        old = current_buffer()
        call set_buffer(fortmacs_diff_sessions(session_index)%custom_diff_buffer, status)
        if (.not. status%ok()) return
        buffers(current)%read_only = .false.
        call erase_buffer(status)
        if (status%ok()) call set_buffer(old, status)
        if (.not. status%ok()) return
        call execute_fortmacs_diff_process(fortmacs_diff_custom_diff_program, &
                                           fortmacs_diff_sessions(session_index)%custom_diff_buffer, &
                                           fortmacs_diff_custom_diff_options, &
                                           status, file1=file_a, file2=file_b)

        ! Put the diff file in diff-mode.
        if (status%ok()) then
            call set_buffer(fortmacs_diff_sessions(session_index)%custom_diff_buffer, status)
            if (status%ok()) call diff_mode(status)
            call set_buffer(old, st)
            if (status%ok()) status = st
        end if

        ! Delete the temporary files, keeping the first error.
        call delete_temporary(FORTMACS_DIFF_BUFFER_TYPE_A, file_a)
        call delete_temporary(FORTMACS_DIFF_BUFFER_TYPE_B, file_b)

    contains

        ! The file of buffer BUFFER_TYPE in FILE: the one it visits if that
        ! exists, else a temporary copy.
        recursive subroutine temporary_unless_visited(buffer_type, file, status)
            integer, intent(in) :: buffer_type
            character(len=:), allocatable, intent(out) :: file
            type(status_type), intent(out) :: status
            type(handle_type) :: buffer

            status = status_type()
            buffer = fortmacs_diff_sessions(session_index)%buffers(buffer_type)

            if (is_buffer_live(buffer)) then
                if (allocated(buffers(buffer%index)%filename)) then
                    if (file_exists(buffers(buffer%index)%filename)) then
                        file = buffers(buffer%index)%filename
                        return
                    end if
                end if
            end if

            call create_fortmacs_diff_temporary_file(buffer, file, status)
        end subroutine temporary_unless_visited

        ! Delete FILE if it is a temporary copy of buffer BUFFER_TYPE.
        subroutine delete_temporary(buffer_type, file)
            integer, intent(in) :: buffer_type
            character(len=*), intent(in) :: file
            type(handle_type) :: buffer

            buffer = fortmacs_diff_sessions(session_index)%buffers(buffer_type)

            if (is_buffer_live(buffer)) then
                if (allocated(buffers(buffer%index)%filename)) then
                    if (buffers(buffer%index)%filename == file) return
                end if
            end if

            if (.not. file_exists(file)) return
            call delete_file(file, st)
            if (status%ok()) status = st
        end subroutine delete_temporary
    end subroutine compute_custom_diffs
end submodule fortmacs_diff_settings_utilities_mod
