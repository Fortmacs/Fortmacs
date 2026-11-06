! fortmacs_diff_settings_diff_mod.f90 -- the diff procedures of an Ediff session,
! a submodule of fortmacs_diff_settings_mod.
! Ported from GNU Emacs 31.1 lisp/vc/ediff-diff.el (in part: see
! docs/status/fortmacs_diff_diff_mod.md).
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
!> ediff-diff.el (running diff and diff3 and making the difference regions):
!> a submodule of the type's module, fortmacs_diff_settings_mod, because they
!> use fortmacs_diff_diff_mod, which uses it.
submodule(fortmacs_diff_settings_mod) fortmacs_diff_settings_diff_mod
    use, intrinsic :: iso_fortran_env, only: int64
    use error_mod, only: status_type, set_error, substitute_quotes, GENERIC_ERROR_CODE, USER_ERROR_CODE
    use handle_mod, only: handle_type
    use buffer_mod, only: buffers, current, current_buffer, set_buffer, get_or_make_buffer, is_buffer_live, erase_buffer
    use editing_functions_mod, only: point, goto_character, point_minimum, buffer_string, buffer_size
    use insert_delete_mod, only: insert
    use editing_commands_mod, only: forward_line, beginning_of_line
    use search_mod, only: regular_expression_search_forward, match_beginning, SEARCH_NOERROR
    use basic_subprograms_mod, only: string_matches, match_string
    use replace_mod, only: flush_lines
    use file_input_output_mod, only: file_exists
    use window_mod, only: switch_to_buffer
    use display_engine_mod, only: message
    use fortmacs_diff_utilities_mod, only: fortmacs_diff_unique_buffer_name, make_fortmacs_diff_bullet_proof_overlay
    use fortmacs_diff_merging_mod, only: fortmacs_diff_variant_state_of_merge, FORTMACS_DIFF_DEFAULT_VARIANT_DEFAULT_B
    use fortmacs_diff_diff_mod, only: execute_fortmacs_diff_process, fortmacs_diff_actual_diff3_options, &
                                      fortmacs_diff_actual_diff_options, fortmacs_diff_diff3_ok_lines_regular_expression, &
                                      fortmacs_diff_diff3_options, fortmacs_diff_diff3_program, fortmacs_diff_diff_options, &
                                      fortmacs_diff_diff_program, FORTMACS_DIFF_DIFF_OK_LINES_REGULAR_EXPRESSION, &
                                      FORTMACS_DIFF_MATCH_DIFF3_LINE, FORTMACS_DIFF_MATCH_DIFF_LINE
    implicit none(type, external)

    character, parameter :: CR = achar(13)

contains

    recursive module subroutine set_up_difference_regions(self, file_a, file_b, status)
        class(fortmacs_diff_session_type), intent(inout), target :: self
        character(len=*), intent(in) :: file_a, file_b
        type(status_type), intent(out) :: status
        type(fortmacs_diff_diff_record_type), allocatable :: records(:)
        integer :: code
        logical :: matched
        integer :: session_index

        ! Looking for '-c', '-i', '-u', or 'c', 'i', 'u' among clustered
        ! non-long options.
        call self%get_index(session_index, status)
        if (.not. status%ok()) return
        matched = string_matches("^-[ciu]\| -[ciu]\|\(^\| \)-[^- ]+[ciu]", fortmacs_diff_diff_options, status)
        if (.not. status%ok()) return

        if (matched) then
            call set_error(status, GENERIC_ERROR_CODE, &
                           substitute_quotes("Options `-c', `-u', and `-i' are not allowed in `fortmacs_diff_diff_options'"))
            return
        end if

        ! Create, if it doesn't exist.
        if (.not. is_buffer_live(fortmacs_diff_sessions(session_index)%diff_buffer)) then
            call get_or_make_buffer(fortmacs_diff_unique_buffer_name("*ediff-diff", "*"), &
                                    fortmacs_diff_sessions(session_index)%diff_buffer, &
                                    status)
            if (.not. status%ok()) return
        end if

        call fortmacs_diff_sessions(session_index)%make_diff2_buffer(fortmacs_diff_sessions(session_index)%diff_buffer, &
                                                                     file_a, file_b, code, status)
        if (.not. status%ok()) return
        call fortmacs_diff_sessions(session_index)%prepare_error_list(FORTMACS_DIFF_DIFF_OK_LINES_REGULAR_EXPRESSION, &
                                                                      fortmacs_diff_sessions(session_index)%diff_buffer, status)
        if (.not. status%ok()) return
        call fortmacs_diff_sessions(session_index)%extract_differences(fortmacs_diff_sessions(session_index)%diff_buffer, &
                                                                       records, status)
        if (.not. status%ok()) return
        call fortmacs_diff_sessions(session_index)%make_difference_overlays(records, status)
    end subroutine set_up_difference_regions

    recursive module subroutine make_diff2_buffer(self, diff_buffer, file1, file2, code, status)
        class(fortmacs_diff_session_type), intent(inout), target :: self
        type(handle_type), intent(in) :: diff_buffer
        character(len=*), intent(in) :: file1, file2
        integer, intent(out) :: code
        type(status_type), intent(out) :: status
        type(handle_type) :: old
        integer :: session_index

        call self%get_index(session_index, status)
        if (.not. status%ok()) return
        code = 1

        if (.not. file_exists(file1)) then
            call message("Can't find file: "//file1)
            return
        else if (.not. file_exists(file2)) then
            call message("Can't find file: "//file2)
            return
        end if

        ! This erases the diff buffer automatically.
        call execute_fortmacs_diff_process(fortmacs_diff_diff_program, diff_buffer, fortmacs_diff_actual_diff_options, status, &
                                           fortmacs_diff_sessions(session_index)%job_name, file1, file2)
        if (.not. status%ok()) return
        call message("")
        old = current_buffer()
        call set_buffer(diff_buffer, status)
        if (.not. status%ok()) return
        code = int(buffer_size())
        call set_buffer(old, status)
    end subroutine make_diff2_buffer

    recursive module subroutine prepare_error_list(self, ok_regular_expression, diff_buffer, status)
        class(fortmacs_diff_session_type), intent(inout), target :: self
        character(len=*), intent(in) :: ok_regular_expression
        type(handle_type), intent(in) :: diff_buffer
        type(status_type), intent(out) :: status
        type(handle_type) :: old, control
        type(status_type) :: st
        character(len=:), allocatable :: text, error_name, diff_name
        integer(int64) :: deleted, size
        integer :: session_index

        call self%get_index(session_index, status)
        if (.not. status%ok()) return
        old = current_buffer()

        if (.not. is_buffer_live(fortmacs_diff_sessions(session_index)%error_buffer)) then
            call get_or_make_buffer(fortmacs_diff_unique_buffer_name("*ediff-errors", "*"), &
                                    fortmacs_diff_sessions(session_index)%error_buffer, status)
            if (.not. status%ok()) return
        end if

        call set_buffer(diff_buffer, status)
        if (.not. status%ok()) return
        text = buffer_string()
        diff_name = buffers(current)%name
        call set_buffer(fortmacs_diff_sessions(session_index)%error_buffer, status)

        if (status%ok()) then
            buffers(current)%undo_enabled = .false.
            call erase_buffer(status)
        end if

        if (status%ok()) call insert(text, status)

        if (status%ok()) then
            call goto_character(point_minimum())
            call flush_lines(ok_regular_expression, deleted, status)
        end if

        size = buffer_size()
        error_name = buffers(current)%name
        ! Make the old buffer current again, keeping the first error.
        call set_buffer(old, st)
        if (status%ok()) status = st
        if (.not. status%ok()) return

        ! If diff reports errors, show them then quit.
        if (size /= 0) then
            control = fortmacs_diff_sessions(session_index)%control_buffer
            call switch_to_buffer(error_name, status)
            if (.not. status%ok()) return
            call close_fortmacs_diff_buffer_carefully(control, status)
            if (.not. status%ok()) return
            call set_error(status, USER_ERROR_CODE, "Errors in diff output.  Diff output is in #<buffer "//diff_name//">")
        end if
    end subroutine prepare_error_list

    !> Give in POSITION the point in buffer BUFFER LINES lines after FROM
    !> (point-min if FROM is 0).  Not a port: (goto-char FROM)
    !> (forward-line LINES) in ediff-extract-diffs.
    recursive subroutine get_line_position(buffer, from, lines, position, status)
        type(handle_type), intent(in) :: buffer
        integer(int64), intent(in) :: from, lines
        integer(int64), intent(out) :: position
        type(status_type), intent(out) :: status
        type(handle_type) :: old
        integer(int64) :: shortage

        position = 0
        old = current_buffer()
        call set_buffer(buffer, status)
        if (.not. status%ok()) return

        if (from == 0) then
            call goto_character(point_minimum())
        else
            call goto_character(from)
        end if

        shortage = forward_line(lines)
        position = point()
        call set_buffer(old, status)
    end subroutine get_line_position

    !> The integer group N of the last match in the current buffer
    !> matched, or DEFAULT if it did not match.  Not a port:
    !> (string-to-number (buffer-substring (match-beginning N) (match-end N))).
    recursive integer(int64) function matched_number(n, default) result(value)
        integer, intent(in) :: n
        integer(int64), intent(in) :: default
        character(len=:), allocatable :: text
        logical :: matched
        integer :: ios

        value = default
        text = match_string(n, matched=matched)
        if (.not. matched) return
        read (text, *, iostat=ios) value
        if (ios /= 0) value = 0
    end function matched_number

    recursive module subroutine extract_differences(self, diff_buffer, records, status)
        class(fortmacs_diff_session_type), intent(inout), target :: self
        type(handle_type), intent(in) :: diff_buffer
        type(fortmacs_diff_diff_record_type), allocatable, intent(out) :: records(:)
        type(status_type), intent(out) :: status
        type(fortmacs_diff_diff_record_type) :: record
        type(handle_type) :: old, buffer_a, buffer_b, buffer_c
        type(status_type) :: st
        integer(int64) :: a_previous, b_previous, c_previous, a_previous_point, b_previous_point, c_previous_point, found, &
                          a_begin, a_end, b_begin, &
                          b_end, c_begin, c_end
        character(len=:), allocatable :: diff_type
        integer :: session_index

        call self%get_index(session_index, status)
        if (.not. status%ok()) return
        allocate (records(0))
        buffer_a = fortmacs_diff_sessions(session_index)%buffers(FORTMACS_DIFF_BUFFER_TYPE_A)
        buffer_b = fortmacs_diff_sessions(session_index)%buffers(FORTMACS_DIFF_BUFFER_TYPE_B)
        buffer_c = fortmacs_diff_sessions(session_index)%buffers(FORTMACS_DIFF_BUFFER_TYPE_C)
        ! This is needed to set the first diff line correctly.
        a_previous = 1
        b_previous = 1
        c_previous = 1
        ! The narrow bounds: the beginnings of buffers A and B.
        a_previous_point = fortmacs_diff_sessions(session_index)%get_narrow_start(FORTMACS_DIFF_BUFFER_TYPE_A)
        b_previous_point = fortmacs_diff_sessions(session_index)%get_narrow_start(FORTMACS_DIFF_BUFFER_TYPE_B)
        c_previous_point = 0
        old = current_buffer()
        call set_buffer(diff_buffer, status)
        if (.not. status%ok()) return
        call goto_character(point_minimum())

        do
            call regular_expression_search_forward(FORTMACS_DIFF_MATCH_DIFF_LINE, found, status, noerror=SEARCH_NOERROR)
            if (.not. status%ok()) exit
            if (found == 0) exit
            a_begin = matched_number(1, 0_int64)
            a_end = matched_number(3, a_begin)
            diff_type = match_string(4)
            b_begin = matched_number(5, 0_int64)
            b_end = matched_number(7, b_begin)

            ! Fix the beginning and end numbers, because diff is somewhat
            ! strange about how it numbers lines.
            if (diff_type == "a") then
                b_end = b_end + 1
                a_begin = a_begin + 1
                a_end = a_begin
            else if (diff_type == "d") then
                a_end = a_end + 1
                b_begin = b_begin + 1
                b_end = b_begin
            else
                a_end = a_end + 1
                b_end = b_end + 1
            end if

            if (fortmacs_diff_sessions(session_index)%default_variant%choice == FORTMACS_DIFF_DEFAULT_VARIANT_DEFAULT_B) then
                c_begin = b_begin
                c_end = b_end
            else
                c_begin = a_begin
                c_end = a_end
            end if

            ! Else convert lines to points.
            record = fortmacs_diff_diff_record_type()
            call get_line_position(buffer_a, a_previous_point, a_begin - a_previous, &
                                   record%beginning(FORTMACS_DIFF_BUFFER_TYPE_A), status)
            if (.not. status%ok()) exit
            call get_line_position(buffer_a, record%beginning(FORTMACS_DIFF_BUFFER_TYPE_A), a_end - a_begin, &
                                   record%end(FORTMACS_DIFF_BUFFER_TYPE_A), status)
            if (.not. status%ok()) exit
            a_previous = a_end
            a_previous_point = record%end(FORTMACS_DIFF_BUFFER_TYPE_A)
            call get_line_position(buffer_b, b_previous_point, b_begin - b_previous, &
                                   record%beginning(FORTMACS_DIFF_BUFFER_TYPE_B), status)
            if (.not. status%ok()) exit
            call get_line_position(buffer_b, record%beginning(FORTMACS_DIFF_BUFFER_TYPE_B), b_end - b_begin, &
                                   record%end(FORTMACS_DIFF_BUFFER_TYPE_B), status)
            if (.not. status%ok()) exit
            b_previous = b_end
            b_previous_point = record%end(FORTMACS_DIFF_BUFFER_TYPE_B)

            if (is_buffer_live(buffer_c)) then
                call get_line_position(buffer_c, c_previous_point, c_begin - c_previous, &
                                       record%beginning(FORTMACS_DIFF_BUFFER_TYPE_C), &
                                       status)
                if (.not. status%ok()) exit
                call get_line_position(buffer_c, record%beginning(FORTMACS_DIFF_BUFFER_TYPE_C), c_end - c_begin, &
                                       record%end(FORTMACS_DIFF_BUFFER_TYPE_C), status)
                if (.not. status%ok()) exit
                c_previous = c_end
                c_previous_point = record%end(FORTMACS_DIFF_BUFFER_TYPE_C)

                ! State of diff: shows which buff is different from the
                ! other two.
                if (fortmacs_diff_sessions(session_index)%default_variant%choice == FORTMACS_DIFF_DEFAULT_VARIANT_DEFAULT_B) then
                    record%state_of_diff = FORTMACS_DIFF_BUFFER_TYPE_A
                else
                    record%state_of_diff = FORTMACS_DIFF_BUFFER_TYPE_B
                end if

                record%state_of_merge = &
                    fortmacs_diff_variant_state_of_merge(fortmacs_diff_sessions(session_index)%default_variant%choice)
            end if

            records = [records, record]
            call set_buffer(diff_buffer, status)
            if (.not. status%ok()) exit
        end do

        ! Make the old buffer current again, keeping the first error.
        call set_buffer(old, st)
        if (status%ok()) status = st
    end subroutine extract_differences

    module integer(int64) function get_narrow_start(self, buffer_type) result(position)
        use interval_tree_mod, only: overlay_start
        class(fortmacs_diff_session_type), intent(in) :: self
        integer, intent(in) :: buffer_type

        position = overlay_start(self%narrow_bounds(buffer_type))
    end function get_narrow_start

    recursive module subroutine make_difference_overlays(self, records, status)
        class(fortmacs_diff_session_type), intent(inout), target :: self
        type(fortmacs_diff_diff_record_type), intent(in) :: records(:)
        type(status_type), intent(out) :: status
        integer :: session_index

        call self%get_index(session_index, status)
        if (.not. status%ok()) return

        if (allocated(fortmacs_diff_sessions(session_index)%differences)) then
            deallocate (fortmacs_diff_sessions(session_index)%differences)
        end if

        allocate (fortmacs_diff_sessions(session_index)%differences(4, size(records)))
        call fortmacs_diff_sessions(session_index)%set_difference_overlays_in_one_buffer(FORTMACS_DIFF_BUFFER_TYPE_A, &
                                                                                         records, status)
        if (.not. status%ok()) return
        call fortmacs_diff_sessions(session_index)%set_difference_overlays_in_one_buffer(FORTMACS_DIFF_BUFFER_TYPE_B, &
                                                                                         records, status)
        if (.not. status%ok()) return

        if (fortmacs_diff_sessions(session_index)%three_way_job) then
            call fortmacs_diff_sessions(session_index)%set_difference_overlays_in_one_buffer(FORTMACS_DIFF_BUFFER_TYPE_C, &
                                                                                             records, status)
            if (.not. status%ok()) return
        end if

        if (fortmacs_diff_sessions(session_index)%merge_with_ancestor_job) then
            call fortmacs_diff_sessions(session_index)%set_difference_overlays_in_one_buffer( &
                FORTMACS_DIFF_BUFFER_TYPE_ANCESTOR, records, status)
            if (.not. status%ok()) return
        end if

        ! Set up the array showing the status of merge regions.
        if (fortmacs_diff_sessions(session_index)%merge_job) then
            if (allocated(fortmacs_diff_sessions(session_index)%merge_states)) then
                deallocate (fortmacs_diff_sessions(session_index)%merge_states)
            end if

            allocate (fortmacs_diff_sessions(session_index)%merge_states(size(records)))
            fortmacs_diff_sessions(session_index)%merge_states%state_of_merge = records%state_of_merge
            fortmacs_diff_sessions(session_index)%merge_states%state_of_ancestor = records%state_of_ancestor
        end if

        call message("Processing difference regions ... done")
    end subroutine make_difference_overlays

    recursive module subroutine set_difference_overlays_in_one_buffer(self, buffer_type, records, status)
        class(fortmacs_diff_session_type), intent(inout), target :: self
        integer, intent(in) :: buffer_type
        type(fortmacs_diff_diff_record_type), intent(in) :: records(:)
        type(status_type), intent(out) :: status
        type(handle_type) :: buffer, overlay
        integer :: current_diff, state, others(3), j
        character(len=:), allocatable :: state_of_diff
        character(len=96) :: progress
        integer :: session_index

        call self%get_index(session_index, status)
        if (.not. status%ok()) return
        buffer = fortmacs_diff_sessions(session_index)%buffers(buffer_type)

        do current_diff = 0, size(records) - 1
            associate (record => records(current_diff + 1))
                state = record%state_of_diff
                state_of_diff = ""

                if (state /= buffer_type .and. buffer_type /= FORTMACS_DIFF_BUFFER_TYPE_ANCESTOR .and. &
                    any(state == [FORTMACS_DIFF_BUFFER_TYPE_A, FORTMACS_DIFF_BUFFER_TYPE_B, FORTMACS_DIFF_BUFFER_TYPE_C])) then
                    others = [FORTMACS_DIFF_BUFFER_TYPE_A, FORTMACS_DIFF_BUFFER_TYPE_B, FORTMACS_DIFF_BUFFER_TYPE_C]
                    j = findloc(others /= buffer_type .and. others /= state, .true., dim=1)
                    state_of_diff = "=diff("//fortmacs_diff_buffer_type_name(others(j))//")"
                end if

                ! Put overlays at appropriate places in buffer.
                call make_fortmacs_diff_bullet_proof_overlay(record%beginning(buffer_type), record%end(buffer_type), buffer, &
                                                             overlay, status)
            end associate

            if (.not. status%ok()) return

            if (has_fortmacs_diff_face_support() .and. fortmacs_diff_sessions(session_index)%use_faces .and. &
                fortmacs_diff_sessions(session_index)%highlight_all_diffs) then
                call set_fortmacs_diff_overlay_face(overlay, fortmacs_diff_background_face(buffer_type, current_diff), status)
                if (.not. status%ok()) return
            end if

            if (modulo(current_diff, 10) == 0) then
                write (progress, '("Buffer ", a, ": Processing difference region ", i0, " of ", i0)') &
                    fortmacs_diff_buffer_type_name(buffer_type), current_diff, size(records)
                call message(trim(progress))
            end if

            ! Record all overlays for this difference.
            fortmacs_diff_sessions(session_index)%differences(buffer_type, current_diff + 1) = &
                fortmacs_diff_difference_type(overlay=overlay, state_of_diff=state_of_diff)
        end do
    end subroutine set_difference_overlays_in_one_buffer

    !> The line range of the group of FILE ("1", "2" or "3") of the diff3
    !> difference after point in the current buffer: BEGINNING and END (a
    !> line after it).  Port of ediff-diff.el:ediff-get-diff3-group.
    recursive subroutine get_fortmacs_diff_diff3_group(file, beginning, end, status)
        character(len=*), intent(in) :: file
        integer(int64), intent(out) :: beginning, end
        type(status_type), intent(out) :: status
        integer(int64) :: original_point, found, x, second
        character(len=:), allocatable :: kind

        ! This save-excursion allows ediff-get-diff3-group to be called
        ! for the various groups of lines (1, 2, 3) in any order, and for
        ! the lines to appear in any order.  The reason this is necessary
        ! is that GNU diff3 can produce the groups in the order 1, 2, 3 or
        ! 1, 3, 2.
        original_point = point()
        beginning = 0
        end = 0
        call regular_expression_search_forward("^"//file//":\([0-9]+\)\(,\([0-9]+\)\)?\([ac]\)"//CR//"?$", found, status)

        if (.not. status%ok()) then
            call goto_character(original_point)
            return
        end if

        second = match_beginning(2, status)

        if (.not. status%ok()) then
            call goto_character(original_point)
            return
        end if

        call beginning_of_line(2_int64)
        kind = match_string(4)
        x = matched_number(1, 0_int64)

        ! Treatment depends on whether it is an "a" group or a "c" group.
        if (kind == "c") then
            beginning = x

            if (second > 0) then
                ! It has two numbers.
                end = matched_number(3, 0_int64) + 1
            else
                ! It has one number.
                end = x + 1
            end if
        else
            ! It is an "a" group.
            beginning = x + 1
            end = x + 1
        end if

        call goto_character(original_point)
    end subroutine get_fortmacs_diff_diff3_group

    recursive module subroutine extract_differences3(self, diff_buffer, three_way_comp, records, status)
        class(fortmacs_diff_session_type), intent(inout), target :: self
        type(handle_type), intent(in) :: diff_buffer
        logical, intent(in) :: three_way_comp
        type(fortmacs_diff_diff_record_type), allocatable, intent(out) :: records(:)
        type(status_type), intent(out) :: status
        type(fortmacs_diff_diff_record_type) :: record
        type(handle_type) :: old
        type(status_type) :: st
        integer(int64) :: previous(4), previous_point(4), found, beginning(4), end(4)
        character(len=1) :: group(3)
        character(len=:), allocatable :: agreement
        integer :: t, last
        integer :: session_index

        call self%get_index(session_index, status)
        if (.not. status%ok()) return
        allocate (records(0))
        previous = 1
        ! The ancestor's lines are counted from its beginning.
        previous_point = [fortmacs_diff_sessions(session_index)%get_narrow_start(FORTMACS_DIFF_BUFFER_TYPE_A), &
                          fortmacs_diff_sessions(session_index)%get_narrow_start(FORTMACS_DIFF_BUFFER_TYPE_B), 0_int64, &
                          0_int64]

        if (three_way_comp) then
            previous_point(FORTMACS_DIFF_BUFFER_TYPE_C) = &
                fortmacs_diff_sessions(session_index)%get_narrow_start(FORTMACS_DIFF_BUFFER_TYPE_C)
        end if

        ! The groups of buffers A, B and C (or the ancestor) in diff3's
        ! output.
        group = ["1", "3", "2"]
        if (three_way_comp) group = ["1", "2", "3"]
        last = FORTMACS_DIFF_BUFFER_TYPE_C

        if (.not. three_way_comp &
            .and. is_buffer_live(fortmacs_diff_sessions(session_index)%buffers(FORTMACS_DIFF_BUFFER_TYPE_ANCESTOR))) then
            last = FORTMACS_DIFF_BUFFER_TYPE_ANCESTOR
        end if

        old = current_buffer()
        call set_buffer(diff_buffer, status)
        if (.not. status%ok()) return
        call goto_character(point_minimum())

        do
            call regular_expression_search_forward(FORTMACS_DIFF_MATCH_DIFF3_LINE, found, status, noerror=SEARCH_NOERROR)
            if (.not. status%ok()) exit
            if (found == 0) exit
            agreement = match_string(1)
            ! Leave point after matched line.
            call beginning_of_line(2_int64)
            ! If the files A and B are the same and not 3way-comparison,
            ! ignore the difference.
            if (.not. three_way_comp .and. agreement == group(3)) cycle

            do t = 1, 3
                call get_fortmacs_diff_diff3_group(group(t), beginning(t), end(t), status)
                if (.not. status%ok()) exit
            end do

            if (.not. status%ok()) exit
            record = fortmacs_diff_diff_record_type()
            record%state_of_ancestor = beginning(3) == end(3)

            if (.not. three_way_comp) then
                ! The third group is the ancestor's; buffer C's lines are
                ! those of the default variant.
                beginning(FORTMACS_DIFF_BUFFER_TYPE_ANCESTOR) = beginning(3)
                end(FORTMACS_DIFF_BUFFER_TYPE_ANCESTOR) = end(3)

                if (fortmacs_diff_sessions(session_index)%default_variant%choice == FORTMACS_DIFF_DEFAULT_VARIANT_DEFAULT_B) then
                    beginning(FORTMACS_DIFF_BUFFER_TYPE_C) = beginning(FORTMACS_DIFF_BUFFER_TYPE_B)
                    end(FORTMACS_DIFF_BUFFER_TYPE_C) = end(FORTMACS_DIFF_BUFFER_TYPE_B)
                else
                    beginning(FORTMACS_DIFF_BUFFER_TYPE_C) = beginning(FORTMACS_DIFF_BUFFER_TYPE_A)
                    end(FORTMACS_DIFF_BUFFER_TYPE_C) = end(FORTMACS_DIFF_BUFFER_TYPE_A)
                end if
            end if

            ! Compute the regions: convert lines to points.
            do t = FORTMACS_DIFF_BUFFER_TYPE_A, last
                call get_line_position(fortmacs_diff_sessions(session_index)%buffers(t), previous_point(t), &
                                       beginning(t) - previous(t), &
                                       record%beginning(t), status)
                if (.not. status%ok()) exit
                call get_line_position(fortmacs_diff_sessions(session_index)%buffers(t), record%beginning(t), &
                                       end(t) - beginning(t), &
                                       record%end(t), status)
                if (.not. status%ok()) exit
                previous(t) = end(t)
                previous_point(t) = record%end(t)
            end do

            if (.not. status%ok()) exit

            if (three_way_comp) then
                if (agreement == group(1)) then
                    record%state_of_diff = FORTMACS_DIFF_BUFFER_TYPE_A
                else if (agreement == group(2)) then
                    record%state_of_diff = FORTMACS_DIFF_BUFFER_TYPE_B
                else if (agreement == group(3)) then
                    record%state_of_diff = FORTMACS_DIFF_BUFFER_TYPE_C
                end if
            else
                if (agreement == group(1)) then
                    record%state_of_merge = FORTMACS_DIFF_STATE_OF_MERGE_PREFER_A
                else if (agreement == group(2)) then
                    record%state_of_merge = FORTMACS_DIFF_STATE_OF_MERGE_PREFER_B
                else
                    record%state_of_merge = &
                        fortmacs_diff_variant_state_of_merge(fortmacs_diff_sessions(session_index)%default_variant%choice)
                end if

                if (any(record%state_of_merge == [FORTMACS_DIFF_STATE_OF_MERGE_DEFAULT_A, &
                                                  FORTMACS_DIFF_STATE_OF_MERGE_PREFER_A])) then
                    record%state_of_diff = FORTMACS_DIFF_BUFFER_TYPE_B
                else
                    record%state_of_diff = FORTMACS_DIFF_BUFFER_TYPE_A
                end if
            end if

            records = [records, record]
            call set_buffer(diff_buffer, status)
            if (.not. status%ok()) exit
        end do

        ! Make the old buffer current again, keeping the first error.
        call set_buffer(old, st)
        if (status%ok()) status = st
    end subroutine extract_differences3

    recursive module subroutine set_up_difference_regions3(self, file_a, file_b, file_c, status)
        class(fortmacs_diff_session_type), intent(inout), target :: self
        character(len=*), intent(in) :: file_a, file_b, file_c
        type(status_type), intent(out) :: status
        type(fortmacs_diff_diff_record_type), allocatable :: records(:)
        logical :: matched
        integer :: session_index

        ! Looking for '-i' or a 'i' among clustered non-long options.
        call self%get_index(session_index, status)
        if (.not. status%ok()) return
        matched = string_matches("^-i\| -i\|\(^\| \)-[^- ]+i", fortmacs_diff_diff_options, status)
        if (.not. status%ok()) return

        if (matched) then
            call set_error(status, GENERIC_ERROR_CODE, &
                           substitute_quotes("Option `-i' is not allowed in `fortmacs_diff_diff3_options'"))
            return
        end if

        if (.not. is_buffer_live(fortmacs_diff_sessions(session_index)%diff_buffer)) then
            call get_or_make_buffer(fortmacs_diff_unique_buffer_name("*ediff-diff", "*"), &
                                    fortmacs_diff_sessions(session_index)%diff_buffer, &
                                    status)
            if (.not. status%ok()) return
        end if

        call message("Computing differences ...")

        if (fortmacs_diff_sessions(session_index)%merge_with_ancestor_job) then
            ! Ancestor must be the middle file.
            call execute_fortmacs_diff_process(fortmacs_diff_diff3_program, fortmacs_diff_sessions(session_index)%diff_buffer, &
                                               fortmacs_diff_actual_diff3_options, &
                                               status, fortmacs_diff_sessions(session_index)%job_name, file_a, file_c, file_b)
        else
            call execute_fortmacs_diff_process(fortmacs_diff_diff3_program, fortmacs_diff_sessions(session_index)%diff_buffer, &
                                               fortmacs_diff_actual_diff3_options, &
                                               status, fortmacs_diff_sessions(session_index)%job_name, file_a, file_b, file_c)
        end if

        if (.not. status%ok()) return
        call fortmacs_diff_sessions(session_index)%prepare_error_list(fortmacs_diff_diff3_ok_lines_regular_expression, &
                                                                      fortmacs_diff_sessions(session_index)%diff_buffer, status)
        if (.not. status%ok()) return
        call fortmacs_diff_sessions(session_index)%extract_differences3(fortmacs_diff_sessions(session_index)%diff_buffer, &
                                                                   fortmacs_diff_sessions(session_index)%three_way_comparison_job, &
                                                                        records, status)
        if (.not. status%ok()) return
        call fortmacs_diff_sessions(session_index)%make_difference_overlays(records, status)
    end subroutine set_up_difference_regions3
end submodule fortmacs_diff_settings_diff_mod
