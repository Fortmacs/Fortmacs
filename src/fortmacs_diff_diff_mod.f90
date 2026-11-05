! fortmacs_diff_diff_mod.f90 -- running diff and diff3 for Ediff.
! Ported from GNU Emacs 31.1 lisp/vc/ediff-diff.el (in part: the
! difference regions of two and three buffers; fine differences and
! words are deferred).
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

!> The difference regions of an Ediff session: `diff' (or `diff3' for
!> three buffers) runs on the files, its normal output is parsed into
!> line ranges, the ranges become positions in buffers A, B and C, and
!> each region gets an overlay (`convert_ediff_diffs_to_overlays').  Lines
!> diff prints that are no normal output are errors, shown in a buffer of
!> their own.
module fortmacs_diff_diff_mod
    use, intrinsic :: iso_fortran_env, only: int64
    use error_mod, only: status_type, set_error, format_message, FM_GENERIC_ERROR, FM_USER_ERROR
    use handle_mod, only: handle_type, null_handle
    use value_mod, only: value_type, list, make_list, command_type
    use buffer_mod, only: buffers, current, current_buffer, set_buffer, get_or_make_buffer, is_buffer_live, erase_buffer
    use editing_functions_mod, only: point, goto_char, point_min, point_max, buffer_substring, buffer_string, buffer_size
    use insert_delete_mod, only: insert
    use editing_commands_mod, only: forward_line, beginning_of_line
    use search_mod, only: re_search_forward, match_beginning, match_end, match_data, set_match_data, SEARCH_NOERROR
    use basic_subprograms_mod, only: string_matches, match_string
    use replace_mod, only: flush_lines
    use execute_process_mod, only: execute_process
    use coding_mod, only: coding_system_for_read
    use file_input_output_mod, only: file_exists
    use files_mod, only: file_name_unquote
    use window_mod, only: switch_to_buffer
    use display_engine_mod, only: message
    use fortmacs_diff_settings_mod, only: ediff_sessions, ediff_difference_type, has_ediff_face_support, set_ediff_overlay_face, &
                                          ediff_background_face, ediff_buffer_type_name, is_ediff_buffer_job, &
                                          check_ediff_control_buffer, close_ediff_buffer_carefully, ediff_coding_system_for_read, &
                                     ediff_coding_system_for_write, EDIFF_BUFFER_TYPE_A, EDIFF_BUFFER_TYPE_B, EDIFF_BUFFER_TYPE_C, &
                                          EDIFF_BUFFER_TYPE_ANCESTOR, EDIFF_STATE_OF_MERGE_NONE, EDIFF_STATE_OF_MERGE_DEFAULT_A, &
                                          EDIFF_STATE_OF_MERGE_PREFER_A, EDIFF_STATE_OF_MERGE_PREFER_B
    use fortmacs_diff_utilities_mod, only: ediff_unique_buffer_name, make_ediff_bullet_proof_overlay, update_ediff_diffs
    use fortmacs_diff_merging_mod, only: ediff_variant_state_of_merge, EDIFF_DEFAULT_VARIANT_DEFAULT_B
    use commands_mod, only: commands
    implicit none(type, external)
    private

    public :: set_up_ediff_diff_regions, set_up_ediff_diff_regions3, make_ediff_diff2_buffer, prepare_ediff_error_list, &
              extract_ediff_diffs, extract_ediff_diffs3, convert_ediff_diffs_to_overlays, get_ediff_diff3_group, &
              execute_ediff_process, set_ediff_actual_diff_options, toggle_ediff_ignore_case, init_ediff_diff

    character, parameter :: CR = achar(13), LF = achar(10)

    !> A difference region as diff reports it, in all the buffers of a
    !> job: upstream's vector [a-beg a-end b-beg b-end c-beg c-end anc-beg
    !> anc-end state-of-diff state-of-merge state-of-ancestor], positions by
    !> buffer type (0 for a buffer the job does not have).
    !> `state_of_diff' is the buffer type that differs from the other two
    !> (3-way jobs), 0 if none; `state_of_merge' how the region of buffer C
    !> is made in merge jobs (EDIFF_STATE_OF_MERGE_*); `state_of_ancestor'
    !> whether the ancestor's region is empty.
    type, public :: ediff_diff_record_type
        integer(int64) :: beginning(4) = 0, end(4) = 0
        integer :: state_of_diff = 0
        integer :: state_of_merge = EDIFF_STATE_OF_MERGE_NONE
        logical :: state_of_ancestor = .false.
    end type ediff_diff_record_type

    !> Program to use for generating the differential of the two files.
    !> Port of ediff-diff.el ediff-diff-program.
    character(len=:), allocatable, public :: ediff_diff_program

    !> Program to be used for three-way comparison.
    !> Must produce output compatible with Unix's diff3 program.  Port of
    !> ediff-diff.el ediff-diff3-program.
    character(len=:), allocatable, public :: ediff_diff3_program

    !> Program to use for generating custom diff output for saving it in a
    !> file.
    !> This output is not used by Ediff internally.  Port of ediff-diff.el
    !> ediff-custom-diff-program.
    character(len=:), allocatable, public :: ediff_custom_diff_program

    !> Options to pass to `ediff_custom_diff_program'.  Port of
    !> ediff-diff.el ediff-custom-diff-options.
    character(len=:), allocatable, public :: ediff_custom_diff_options

    !> Options to pass to `ediff_diff_program'.
    !> If Unix diff is used as `ediff_diff_program',
    !> then a useful option is `-w', to ignore space.
    !> Options `-c', `-u', and `-i' are not allowed.  Case sensitivity
    !> can be toggled interactively using \[toggle_ediff_ignore_case].
    !>
    !> Do not remove the default options.  If you need to change this
    !> variable, add new options after the default ones.  Port of
    !> ediff-diff.el ediff-diff-options (the options a session uses are
    !> computed when the first one starts, as upstream computes them when
    !> it loads Ediff).
    character(len=:), allocatable, public :: ediff_diff_options

    !> Option that causes the diff program to ignore case of letters.
    !> Port of ediff-diff.el ediff-ignore-case-option.
    character(len=:), allocatable, public :: ediff_ignore_case_option

    !> Option that causes the diff3 program to ignore case of letters.
    !> GNU diff3 doesn't have such an option.  Port of ediff-diff.el
    !> ediff-ignore-case-option3.
    character(len=:), allocatable, public :: ediff_ignore_case_option3

    !> Options to pass to `ediff_diff3_program'.  Port of ediff-diff.el
    !> ediff-diff3-options.
    character(len=:), allocatable, public :: ediff_diff3_options

    !> Regexp that matches normal output lines from `ediff_diff3_program'.
    !> Lines that do not match are assumed to be error messages.  Port of
    !> ediff-diff.el ediff-diff3-ok-lines-regexp.
    character(len=:), allocatable, public :: ediff_diff3_ok_lines_regexp

    !> The actual options used in comparison.  Port of ediff-diff.el
    !> ediff-actual-diff-options.
    character(len=:), allocatable, public :: ediff_actual_diff_options

    !> The actual options used in comparison.  Port of ediff-diff.el
    !> ediff-actual-diff3-options.
    character(len=:), allocatable, public :: ediff_actual_diff3_options

    !> Pattern to match lines produced by diff3 that describe differences.
    !> Port of ediff-diff.el ediff-match-diff3-line.
    character(len=*), parameter, public :: EDIFF_MATCH_DIFF3_LINE = "^====\(.?\)"//CR//"?$"

    !> Regexp that matches normal output lines from `ediff_diff_program'.
    !> This is mostly lifted from Emerge, except that Ediff also considers
    !> warnings and `Missing newline'-type messages to be normal output.
    !> Lines that do not match are assumed to be error messages.  Port of
    !> ediff-diff.el ediff-diff-ok-lines-regexp.
    character(len=*), parameter, public :: EDIFF_DIFF_OK_LINES_REGEXP = &
                                           "^\([0-9,]+[acd][0-9,]+"//CR//"?$\|[<>] \|---\|.*Warning *:\|.*No +newline"// &
                                           "\|.*missing +newline\|^"//CR//"?$\)"

    !> Pattern to match lines produced by diff that describe differences.
    !> Port of ediff-diff.el ediff-match-diff-line.
    character(len=*), parameter, public :: EDIFF_MATCH_DIFF_LINE = &
                                           "^\([0-9]+\)\(\|,\([0-9]+\)\)\([acd]\)\([0-9]+\)\(\|,\([0-9]+\)\)"//CR//"?$"

contains

    !> Generate the difference regions and overlays of session K for the
    !> two files FILE_A and FILE_B.  Port of
    !> ediff-diff.el:ediff-setup-diff-regions.
    recursive subroutine set_up_ediff_diff_regions(k, file_a, file_b, status)
        integer, intent(in) :: k
        character(len=*), intent(in) :: file_a, file_b
        type(status_type), intent(out) :: status
        type(ediff_diff_record_type), allocatable :: records(:)
        integer :: code
        logical :: matched

        ! Looking for '-c', '-i', '-u', or 'c', 'i', 'u' among clustered
        ! non-long options.
        matched = string_matches("^-[ciu]\| -[ciu]\|\(^\| \)-[^- ]+[ciu]", ediff_diff_options, status)
        if (.not. status%ok()) return

        if (matched) then
            call set_error(status, FM_GENERIC_ERROR, &
                           format_message("Options `-c', `-u', and `-i' are not allowed in `ediff_diff_options'"))
            return
        end if

        ! Create, if it doesn't exist.
        if (.not. is_buffer_live(ediff_sessions(k)%diff_buffer)) then
            call get_or_make_buffer(ediff_unique_buffer_name("*ediff-diff", "*"), ediff_sessions(k)%diff_buffer, status)
            if (.not. status%ok()) return
        end if

        call make_ediff_diff2_buffer(k, ediff_sessions(k)%diff_buffer, file_a, file_b, code, status)
        if (.not. status%ok()) return
        call prepare_ediff_error_list(k, EDIFF_DIFF_OK_LINES_REGEXP, ediff_sessions(k)%diff_buffer, status)
        if (.not. status%ok()) return
        call extract_ediff_diffs(k, ediff_sessions(k)%diff_buffer, records, status)
        if (.not. status%ok()) return
        call convert_ediff_diffs_to_overlays(k, records, status)
    end subroutine set_up_ediff_diff_regions

    !> Run the diff program on FILE1 and FILE2 and put the output in
    !> DIFF_BUFFER; CODE is the size of DIFF_BUFFER, or 1 (an error exit
    !> code) when a file is missing.  Port of
    !> ediff-diff.el:ediff-make-diff2-buffer.
    recursive subroutine make_ediff_diff2_buffer(k, diff_buffer, file1, file2, code, status)
        integer, intent(in) :: k
        type(handle_type), intent(in) :: diff_buffer
        character(len=*), intent(in) :: file1, file2
        integer, intent(out) :: code
        type(status_type), intent(out) :: status
        type(handle_type) :: old

        status = status_type()
        code = 1

        if (.not. file_exists(file1)) then
            call message("Can't find file: "//file1)
            return
        else if (.not. file_exists(file2)) then
            call message("Can't find file: "//file2)
            return
        end if

        ! This erases the diff buffer automatically.
        call execute_ediff_process(ediff_diff_program, diff_buffer, ediff_actual_diff_options, status, &
                                   ediff_sessions(k)%job_name, file1, file2)
        if (.not. status%ok()) return
        call message("")
        old = current_buffer()
        call set_buffer(diff_buffer, status)
        if (.not. status%ok()) return
        code = int(buffer_size())
        call set_buffer(old, status)
    end subroutine make_ediff_diff2_buffer

    !> Copy the output in DIFF_BUFFER that does not match OK_REGEXP into
    !> session K's error buffer; if there is any, show it, kill the
    !> control buffer and return with a `user_error'.  Port of
    !> ediff-diff.el:ediff-prepare-error-list (the output is shown
    !> undecoded).
    recursive subroutine prepare_ediff_error_list(k, ok_regexp, diff_buffer, status)
        integer, intent(in) :: k
        character(len=*), intent(in) :: ok_regexp
        type(handle_type), intent(in) :: diff_buffer
        type(status_type), intent(out) :: status
        type(handle_type) :: old, control
        type(status_type) :: st
        character(len=:), allocatable :: text, error_name, diff_name
        integer(int64) :: deleted, size

        status = status_type()
        old = current_buffer()

        if (.not. is_buffer_live(ediff_sessions(k)%error_buffer)) then
            call get_or_make_buffer(ediff_unique_buffer_name("*ediff-errors", "*"), ediff_sessions(k)%error_buffer, status)
            if (.not. status%ok()) return
        end if

        call set_buffer(diff_buffer, status)
        if (.not. status%ok()) return
        text = buffer_string()
        diff_name = buffers(current)%name
        call set_buffer(ediff_sessions(k)%error_buffer, status)

        if (status%ok()) then
            buffers(current)%undo_enabled = .false.
            call erase_buffer(status)
        end if

        if (status%ok()) call insert(text, status)

        if (status%ok()) then
            call goto_char(point_min())
            call flush_lines(ok_regexp, deleted, status)
        end if

        size = buffer_size()
        error_name = buffers(current)%name
        ! Make the old buffer current again, keeping the first error.
        call set_buffer(old, st)
        if (status%ok()) status = st
        if (.not. status%ok()) return

        ! If diff reports errors, show them then quit.
        if (size /= 0) then
            control = ediff_sessions(k)%control_buffer
            call switch_to_buffer(error_name, status)
            if (.not. status%ok()) return
            call close_ediff_buffer_carefully(control, status)
            if (.not. status%ok()) return
            call set_error(status, FM_USER_ERROR, "Errors in diff output.  Diff output is in #<buffer "//diff_name//">")
        end if
    end subroutine prepare_ediff_error_list

    !> Give in POS the point in buffer BUFFER LINES lines after FROM
    !> (point-min if FROM is 0).  Not a port: (goto-char FROM)
    !> (forward-line LINES) in ediff-extract-diffs.
    recursive subroutine get_line_position(buffer, from, lines, pos, status)
        type(handle_type), intent(in) :: buffer
        integer(int64), intent(in) :: from, lines
        integer(int64), intent(out) :: pos
        type(status_type), intent(out) :: status
        type(handle_type) :: old
        integer(int64) :: shortage

        pos = 0
        old = current_buffer()
        call set_buffer(buffer, status)
        if (.not. status%ok()) return

        if (from == 0) then
            call goto_char(point_min())
        else
            call goto_char(from)
        end if

        shortage = forward_line(lines)
        pos = point()
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

    !> Parse the normal diff output in DIFF_BUFFER into RECORDS, positions
    !> in buffers A and B (and C when live) of session K.  Port of
    !> ediff-diff.el:ediff-extract-diffs (WORD-MODE and BOUNDS other than
    !> the narrow bounds are deferred with those jobs).
    recursive subroutine extract_ediff_diffs(k, diff_buffer, records, status)
        integer, intent(in) :: k
        type(handle_type), intent(in) :: diff_buffer
        type(ediff_diff_record_type), allocatable, intent(out) :: records(:)
        type(status_type), intent(out) :: status
        type(ediff_diff_record_type) :: record
        type(handle_type) :: old, buffer_a, buffer_b, buffer_c
        type(status_type) :: st
        integer(int64) :: a_prev, b_prev, c_prev, a_prev_pt, b_prev_pt, c_prev_pt, found, a_begin, a_end, b_begin, &
                          b_end, c_begin, c_end
        character(len=:), allocatable :: diff_type

        status = status_type()
        allocate (records(0))
        buffer_a = ediff_sessions(k)%buffers(EDIFF_BUFFER_TYPE_A)
        buffer_b = ediff_sessions(k)%buffers(EDIFF_BUFFER_TYPE_B)
        buffer_c = ediff_sessions(k)%buffers(EDIFF_BUFFER_TYPE_C)
        ! This is needed to set the first diff line correctly.
        a_prev = 1
        b_prev = 1
        c_prev = 1
        ! The narrow bounds: the beginnings of buffers A and B.
        a_prev_pt = narrow_start(k, EDIFF_BUFFER_TYPE_A)
        b_prev_pt = narrow_start(k, EDIFF_BUFFER_TYPE_B)
        c_prev_pt = 0
        old = current_buffer()
        call set_buffer(diff_buffer, status)
        if (.not. status%ok()) return
        call goto_char(point_min())

        do
            call re_search_forward(EDIFF_MATCH_DIFF_LINE, found, status, noerror=SEARCH_NOERROR)
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

            if (ediff_sessions(k)%default_variant%choice == EDIFF_DEFAULT_VARIANT_DEFAULT_B) then
                c_begin = b_begin
                c_end = b_end
            else
                c_begin = a_begin
                c_end = a_end
            end if

            ! Else convert lines to points.
            record = ediff_diff_record_type()
            call get_line_position(buffer_a, a_prev_pt, a_begin - a_prev, record%beginning(EDIFF_BUFFER_TYPE_A), status)
            if (.not. status%ok()) exit
            call get_line_position(buffer_a, record%beginning(EDIFF_BUFFER_TYPE_A), a_end - a_begin, &
                                   record%end(EDIFF_BUFFER_TYPE_A), status)
            if (.not. status%ok()) exit
            a_prev = a_end
            a_prev_pt = record%end(EDIFF_BUFFER_TYPE_A)
            call get_line_position(buffer_b, b_prev_pt, b_begin - b_prev, record%beginning(EDIFF_BUFFER_TYPE_B), status)
            if (.not. status%ok()) exit
            call get_line_position(buffer_b, record%beginning(EDIFF_BUFFER_TYPE_B), b_end - b_begin, &
                                   record%end(EDIFF_BUFFER_TYPE_B), status)
            if (.not. status%ok()) exit
            b_prev = b_end
            b_prev_pt = record%end(EDIFF_BUFFER_TYPE_B)

            if (is_buffer_live(buffer_c)) then
                call get_line_position(buffer_c, c_prev_pt, c_begin - c_prev, record%beginning(EDIFF_BUFFER_TYPE_C), &
                                       status)
                if (.not. status%ok()) exit
                call get_line_position(buffer_c, record%beginning(EDIFF_BUFFER_TYPE_C), c_end - c_begin, &
                                       record%end(EDIFF_BUFFER_TYPE_C), status)
                if (.not. status%ok()) exit
                c_prev = c_end
                c_prev_pt = record%end(EDIFF_BUFFER_TYPE_C)

                ! State of diff: shows which buff is different from the
                ! other two.
                if (ediff_sessions(k)%default_variant%choice == EDIFF_DEFAULT_VARIANT_DEFAULT_B) then
                    record%state_of_diff = EDIFF_BUFFER_TYPE_A
                else
                    record%state_of_diff = EDIFF_BUFFER_TYPE_B
                end if

                record%state_of_merge = ediff_variant_state_of_merge(ediff_sessions(k)%default_variant%choice)
            end if

            records = [records, record]
            call set_buffer(diff_buffer, status)
            if (.not. status%ok()) exit
        end do

        ! Make the old buffer current again, keeping the first error.
        call set_buffer(old, st)
        if (status%ok()) status = st
    end subroutine extract_ediff_diffs

    !> Where the narrow bounds of session K begin in BUFFER_TYPE.
    !> Not a port: (ediff-overlay-start
    !> (ediff-get-value-according-to-buffer-type BUFFER-TYPE
    !> ediff-narrow-bounds)).
    integer(int64) function narrow_start(k, buffer_type) result(pos)
        use interval_tree_mod, only: overlay_start
        integer, intent(in) :: k, buffer_type

        pos = overlay_start(ediff_sessions(k)%narrow_bounds(buffer_type))
    end function narrow_start

    !> Make an overlay for each difference region of RECORDS in each buffer
    !> of session K, and record them in its difference vectors.  Port of
    !> ediff-diff.el:ediff-convert-diffs-to-overlays.
    recursive subroutine convert_ediff_diffs_to_overlays(k, records, status)
        integer, intent(in) :: k
        type(ediff_diff_record_type), intent(in) :: records(:)
        type(status_type), intent(out) :: status

        if (allocated(ediff_sessions(k)%differences)) deallocate (ediff_sessions(k)%differences)
        allocate (ediff_sessions(k)%differences(4, size(records)))
        call set_ediff_diff_overlays_in_one_buffer(k, EDIFF_BUFFER_TYPE_A, records, status)
        if (.not. status%ok()) return
        call set_ediff_diff_overlays_in_one_buffer(k, EDIFF_BUFFER_TYPE_B, records, status)
        if (.not. status%ok()) return

        if (ediff_sessions(k)%three_way_job) then
            call set_ediff_diff_overlays_in_one_buffer(k, EDIFF_BUFFER_TYPE_C, records, status)
            if (.not. status%ok()) return
        end if

        if (ediff_sessions(k)%merge_with_ancestor_job) then
            call set_ediff_diff_overlays_in_one_buffer(k, EDIFF_BUFFER_TYPE_ANCESTOR, records, status)
            if (.not. status%ok()) return
        end if

        ! Set up the array showing the status of merge regions.
        if (ediff_sessions(k)%merge_job) then
            if (allocated(ediff_sessions(k)%merge_states)) deallocate (ediff_sessions(k)%merge_states)
            allocate (ediff_sessions(k)%merge_states(size(records)))
            ediff_sessions(k)%merge_states%state_of_merge = records%state_of_merge
            ediff_sessions(k)%merge_states%state_of_ancestor = records%state_of_ancestor
        end if

        call message("Processing difference regions ... done")
    end subroutine convert_ediff_diffs_to_overlays

    !> Make the overlays of RECORDS in the BUFFER_TYPE buffer of session K.
    !> Port of ediff-diff.el:ediff-set-diff-overlays-in-one-buffer (word
    !> numbers are deferred).
    recursive subroutine set_ediff_diff_overlays_in_one_buffer(k, buffer_type, records, status)
        integer, intent(in) :: k, buffer_type
        type(ediff_diff_record_type), intent(in) :: records(:)
        type(status_type), intent(out) :: status
        type(handle_type) :: buffer, overlay
        integer :: current_diff, state, others(3), j
        character(len=:), allocatable :: state_of_diff
        character(len=96) :: progress

        status = status_type()
        buffer = ediff_sessions(k)%buffers(buffer_type)

        do current_diff = 0, size(records) - 1
            associate (record => records(current_diff + 1))
                state = record%state_of_diff
                state_of_diff = ""

                if (state /= buffer_type .and. buffer_type /= EDIFF_BUFFER_TYPE_ANCESTOR .and. &
                    any(state == [EDIFF_BUFFER_TYPE_A, EDIFF_BUFFER_TYPE_B, EDIFF_BUFFER_TYPE_C])) then
                    others = [EDIFF_BUFFER_TYPE_A, EDIFF_BUFFER_TYPE_B, EDIFF_BUFFER_TYPE_C]
                    j = findloc(others /= buffer_type .and. others /= state, .true., dim=1)
                    state_of_diff = "=diff("//ediff_buffer_type_name(others(j))//")"
                end if

                ! Put overlays at appropriate places in buffer.
                call make_ediff_bullet_proof_overlay(record%beginning(buffer_type), record%end(buffer_type), buffer, &
                                                     overlay, status)
            end associate

            if (.not. status%ok()) return

            if (has_ediff_face_support() .and. ediff_sessions(k)%use_faces .and. &
                ediff_sessions(k)%highlight_all_diffs) then
                call set_ediff_overlay_face(overlay, ediff_background_face(buffer_type, current_diff), status)
                if (.not. status%ok()) return
            end if

            if (modulo(current_diff, 10) == 0) then
                write (progress, '("Buffer ", a, ": Processing difference region ", i0, " of ", i0)') &
                    ediff_buffer_type_name(buffer_type), current_diff, size(records)
                call message(trim(progress))
            end if

            ! Record all overlays for this difference.
            ediff_sessions(k)%differences(buffer_type, current_diff + 1) = &
                ediff_difference_type(overlay=overlay, state_of_diff=state_of_diff)
        end do
    end subroutine set_ediff_diff_overlays_in_one_buffer

    !> The line range of the group of FILE ("1", "2" or "3") of the diff3
    !> difference after point in the current buffer: BEGINNING and END (a
    !> line after it).  Port of ediff-diff.el:ediff-get-diff3-group.
    recursive subroutine get_ediff_diff3_group(file, beginning, end, status)
        character(len=*), intent(in) :: file
        integer(int64), intent(out) :: beginning, end
        type(status_type), intent(out) :: status
        integer(int64) :: opoint, found, x, second
        character(len=:), allocatable :: kind

        ! This save-excursion allows ediff-get-diff3-group to be called
        ! for the various groups of lines (1, 2, 3) in any order, and for
        ! the lines to appear in any order.  The reason this is necessary
        ! is that GNU diff3 can produce the groups in the order 1, 2, 3 or
        ! 1, 3, 2.
        opoint = point()
        beginning = 0
        end = 0
        call re_search_forward("^"//file//":\([0-9]+\)\(,\([0-9]+\)\)?\([ac]\)"//CR//"?$", found, status)

        if (.not. status%ok()) then
            call goto_char(opoint)
            return
        end if

        second = match_beginning(2, status)

        if (.not. status%ok()) then
            call goto_char(opoint)
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

        call goto_char(opoint)
    end subroutine get_ediff_diff3_group

    !> Parse the diff3 output in DIFF_BUFFER into RECORDS for session K.
    !> THREE_WAY_COMP: it is a 3-way comparison; else it is merging with an
    !> ancestor, in which case buffer C's contents is identical to buffer
    !> A's or B's, depending on `ediff_default_variant', and the second
    !> file of diff3 is the ancestor.  Port of
    !> ediff-diff.el:ediff-extract-diffs3 (word mode is Deferred with its
    !> jobs, see docs/status/fortmacs_diff_diff_mod.md).
    recursive subroutine extract_ediff_diffs3(k, diff_buffer, three_way_comp, records, status)
        integer, intent(in) :: k
        type(handle_type), intent(in) :: diff_buffer
        logical, intent(in) :: three_way_comp
        type(ediff_diff_record_type), allocatable, intent(out) :: records(:)
        type(status_type), intent(out) :: status
        type(ediff_diff_record_type) :: record
        type(handle_type) :: old
        type(status_type) :: st
        integer(int64) :: prev(4), prev_pt(4), found, beginning(4), end(4)
        character(len=1) :: group(3)
        character(len=:), allocatable :: agreement
        integer :: t, last

        status = status_type()
        allocate (records(0))
        prev = 1
        ! The ancestor's lines are counted from its beginning.
        prev_pt = [narrow_start(k, EDIFF_BUFFER_TYPE_A), narrow_start(k, EDIFF_BUFFER_TYPE_B), 0_int64, 0_int64]
        if (three_way_comp) prev_pt(EDIFF_BUFFER_TYPE_C) = narrow_start(k, EDIFF_BUFFER_TYPE_C)
        ! The groups of buffers A, B and C (or the ancestor) in diff3's
        ! output.
        group = ["1", "3", "2"]
        if (three_way_comp) group = ["1", "2", "3"]
        last = EDIFF_BUFFER_TYPE_C

        if (.not. three_way_comp .and. is_buffer_live(ediff_sessions(k)%buffers(EDIFF_BUFFER_TYPE_ANCESTOR))) then
            last = EDIFF_BUFFER_TYPE_ANCESTOR
        end if

        old = current_buffer()
        call set_buffer(diff_buffer, status)
        if (.not. status%ok()) return
        call goto_char(point_min())

        do
            call re_search_forward(EDIFF_MATCH_DIFF3_LINE, found, status, noerror=SEARCH_NOERROR)
            if (.not. status%ok()) exit
            if (found == 0) exit
            agreement = match_string(1)
            ! Leave point after matched line.
            call beginning_of_line(2_int64)
            ! If the files A and B are the same and not 3way-comparison,
            ! ignore the difference.
            if (.not. three_way_comp .and. agreement == group(3)) cycle

            do t = 1, 3
                call get_ediff_diff3_group(group(t), beginning(t), end(t), status)
                if (.not. status%ok()) exit
            end do

            if (.not. status%ok()) exit
            record = ediff_diff_record_type()
            record%state_of_ancestor = beginning(3) == end(3)

            if (.not. three_way_comp) then
                ! The third group is the ancestor's; buffer C's lines are
                ! those of the default variant.
                beginning(EDIFF_BUFFER_TYPE_ANCESTOR) = beginning(3)
                end(EDIFF_BUFFER_TYPE_ANCESTOR) = end(3)

                if (ediff_sessions(k)%default_variant%choice == EDIFF_DEFAULT_VARIANT_DEFAULT_B) then
                    beginning(EDIFF_BUFFER_TYPE_C) = beginning(EDIFF_BUFFER_TYPE_B)
                    end(EDIFF_BUFFER_TYPE_C) = end(EDIFF_BUFFER_TYPE_B)
                else
                    beginning(EDIFF_BUFFER_TYPE_C) = beginning(EDIFF_BUFFER_TYPE_A)
                    end(EDIFF_BUFFER_TYPE_C) = end(EDIFF_BUFFER_TYPE_A)
                end if
            end if

            ! Compute the regions: convert lines to points.
            do t = EDIFF_BUFFER_TYPE_A, last
                call get_line_position(ediff_sessions(k)%buffers(t), prev_pt(t), beginning(t) - prev(t), &
                                       record%beginning(t), status)
                if (.not. status%ok()) exit
                call get_line_position(ediff_sessions(k)%buffers(t), record%beginning(t), end(t) - beginning(t), &
                                       record%end(t), status)
                if (.not. status%ok()) exit
                prev(t) = end(t)
                prev_pt(t) = record%end(t)
            end do

            if (.not. status%ok()) exit

            if (three_way_comp) then
                if (agreement == group(1)) then
                    record%state_of_diff = EDIFF_BUFFER_TYPE_A
                else if (agreement == group(2)) then
                    record%state_of_diff = EDIFF_BUFFER_TYPE_B
                else if (agreement == group(3)) then
                    record%state_of_diff = EDIFF_BUFFER_TYPE_C
                end if
            else
                if (agreement == group(1)) then
                    record%state_of_merge = EDIFF_STATE_OF_MERGE_PREFER_A
                else if (agreement == group(2)) then
                    record%state_of_merge = EDIFF_STATE_OF_MERGE_PREFER_B
                else
                    record%state_of_merge = ediff_variant_state_of_merge(ediff_sessions(k)%default_variant%choice)
                end if

                if (any(record%state_of_merge == [EDIFF_STATE_OF_MERGE_DEFAULT_A, EDIFF_STATE_OF_MERGE_PREFER_A])) then
                    record%state_of_diff = EDIFF_BUFFER_TYPE_B
                else
                    record%state_of_diff = EDIFF_BUFFER_TYPE_A
                end if
            end if

            records = [records, record]
            call set_buffer(diff_buffer, status)
            if (.not. status%ok()) exit
        end do

        ! Make the old buffer current again, keeping the first error.
        call set_buffer(old, st)
        if (status%ok()) status = st
    end subroutine extract_ediff_diffs3

    !> Generate the difference regions and overlays of session K for three
    !> files: FILE_C is the third file to compare (the ancestor's file in
    !> merges).  Port of ediff-diff.el:ediff-setup-diff-regions3.
    recursive subroutine set_up_ediff_diff_regions3(k, file_a, file_b, file_c, status)
        integer, intent(in) :: k
        character(len=*), intent(in) :: file_a, file_b, file_c
        type(status_type), intent(out) :: status
        type(ediff_diff_record_type), allocatable :: records(:)
        logical :: matched

        ! Looking for '-i' or a 'i' among clustered non-long options.
        matched = string_matches("^-i\| -i\|\(^\| \)-[^- ]+i", ediff_diff_options, status)
        if (.not. status%ok()) return

        if (matched) then
            call set_error(status, FM_GENERIC_ERROR, format_message("Option `-i' is not allowed in `ediff_diff3_options'"))
            return
        end if

        if (.not. is_buffer_live(ediff_sessions(k)%diff_buffer)) then
            call get_or_make_buffer(ediff_unique_buffer_name("*ediff-diff", "*"), ediff_sessions(k)%diff_buffer, status)
            if (.not. status%ok()) return
        end if

        call message("Computing differences ...")

        if (ediff_sessions(k)%merge_with_ancestor_job) then
            ! Ancestor must be the middle file.
            call execute_ediff_process(ediff_diff3_program, ediff_sessions(k)%diff_buffer, ediff_actual_diff3_options, &
                                       status, ediff_sessions(k)%job_name, file_a, file_c, file_b)
        else
            call execute_ediff_process(ediff_diff3_program, ediff_sessions(k)%diff_buffer, ediff_actual_diff3_options, &
                                       status, ediff_sessions(k)%job_name, file_a, file_b, file_c)
        end if

        if (.not. status%ok()) return
        call prepare_ediff_error_list(k, ediff_diff3_ok_lines_regexp, ediff_sessions(k)%diff_buffer, status)
        if (.not. status%ok()) return
        call extract_ediff_diffs3(k, ediff_sessions(k)%diff_buffer, ediff_sessions(k)%three_way_comparison_job, &
                                  records, status)
        if (.not. status%ok()) return
        call convert_ediff_diffs_to_overlays(k, records, status)
    end subroutine set_up_ediff_diff_regions3

    !> Execute the diff PROGRAM.
    !>
    !> The PROGRAM output is sent to BUFFER, which must be a live buffer
    !> object.
    !>
    !> OPTIONS is a string of space-separated options to pass to PROGRAM.
    !> It may be a blank string.
    !>
    !> FILE1, FILE2 and FILE3 are the filenames to pass to PROGRAM; absent
    !> and "" ones are ignored.  JOB_NAME is the kind of job
    !> (a command, as commands%ediff_buffers; none if absent): for a buffer job the output is
    !> read with `ediff_coding_system_for_write', and only then is
    !> "--binary" passed.  Port of ediff-diff.el:ediff-exec-process (always
    !> synchronous: SYNCH is only false for the custom diff of `D', which is
    !> deferred).
    recursive subroutine execute_ediff_process(program, buffer, options, status, job_name, file1, file2, file3)
        character(len=*), intent(in) :: program, options
        type(handle_type), intent(in) :: buffer
        type(status_type), intent(out) :: status
        type(command_type), pointer, intent(in), optional :: job_name
        character(len=*), intent(in), optional :: file1, file2, file3
        type(value_type) :: result, item
        type(value_type), allocatable :: argv(:)
        type(handle_type) :: old
        type(status_type) :: st
        integer(int64), allocatable :: data(:)
        character(len=:), allocatable :: saved_coding, olddir
        logical :: buffer_job, had_coding
        integer :: i, j

        data = match_data()
        buffer_job = .false.
        if (present(job_name)) buffer_job = is_ediff_buffer_job(job_name)
        allocate (argv(0))
        i = 1

        ! The options, split at white space.
        do
            j = verify(options(i:), " "//achar(9)//LF)
            if (j == 0) exit
            i = i + j - 1
            j = scan(options(i:), " "//achar(9)//LF)

            if (j == 0) then
                j = len(options) + 1
            else
                j = i + j - 1
            end if

            ! The --binary option, if present, should be used only for
            ! buffer jobs or for refining the differences.
            if (options(i:j - 1) /= "--binary" .or. buffer_job) then
                item = value_type(text=options(i:j - 1))
                argv = [argv, item]
            end if

            i = j
        end do

        if (present(file1)) call append_file(file1)
        if (present(file2)) call append_file(file2)
        if (present(file3)) call append_file(file3)
        old = current_buffer()
        call set_buffer(buffer, status)
        if (.not. status%ok()) return
        call erase_buffer(status)

        if (.not. status%ok()) then
            call set_buffer(old, st)
            return
        end if

        ! default-directory may be on some remote machine or a
        ! non-existing dir.
        olddir = "/"
        if (allocated(buffers(current)%directory)) olddir = buffers(current)%directory
        buffers(current)%directory = "/"
        had_coding = allocated(coding_system_for_read%value)
        if (had_coding) saved_coding = coding_system_for_read%value

        ! If this is a buffer job, we are diffing temporary files produced
        ! with ediff_coding_system_for_write, so use the same encoding to
        ! read the results.
        if (buffer_job) then
            coding_system_for_read = ediff_coding_system_for_write
        else
            coding_system_for_read = ediff_coding_system_for_read
        end if

        ! Avoid localization of messages so we can parse the output.
        call execute_process(program, result, status, destination=value_type(.true.), args=make_list(argv), &
                             environment=list(value_type(text="LC_MESSAGES=C")))

        if (had_coding) then
            coding_system_for_read = saved_coding
        else
            call coding_system_for_read%clear()
        end if

        buffers(current)%directory = olddir
        ! Make the old buffer current again, keeping the first error.
        call set_buffer(old, st)
        if (status%ok()) status = st
        call set_match_data(data)

    contains

        ! Append FILE, unquoted, to ARGS unless it is "".
        subroutine append_file(file)
            character(len=*), intent(in) :: file

            if (len(file) == 0) return
            item = value_type(text=file_name_unquote(file))
            argv = [argv, item]
        end subroutine append_file
    end subroutine execute_ediff_process

    !> Compute the options diff and diff3 run with: `ediff_diff_options'
    !> and `ediff_diff3_options', and with IGNORE_CASE the options that
    !> ignore case.  Port of ediff-diff.el:ediff-set-actual-diff-options
    !> (`ediff_ignore_case' is the session's, given as IGNORE_CASE).
    subroutine set_ediff_actual_diff_options(ignore_case)
        logical, intent(in) :: ignore_case

        if (ignore_case) then
            ediff_actual_diff_options = ediff_diff_options//" "//ediff_ignore_case_option
            ediff_actual_diff3_options = ediff_diff3_options//" "//ediff_ignore_case_option3
        else
            ediff_actual_diff_options = ediff_diff_options
            ediff_actual_diff3_options = ediff_diff3_options
        end if
    end subroutine set_ediff_actual_diff_options

    !> Toggle whether difference regions that differ only in letter case
    !> are skipped (`ediff_ignore_case'), and recompute the differences.
    !> Port of ediff-diff.el:ediff-toggle-ignore-case.
    recursive subroutine toggle_ediff_ignore_case(status)
        type(status_type), intent(out) :: status
        integer :: k

        call check_ediff_control_buffer(k, status)
        if (.not. status%ok()) return
        ediff_sessions(k)%ignore_case = .not. ediff_sessions(k)%ignore_case
        call set_ediff_actual_diff_options(ediff_sessions(k)%ignore_case)

        if (ediff_sessions(k)%ignore_case) then
            call message("Ignoring regions that differ only in case")
        else
            call message("Ignoring case differences turned OFF")
        end if

        if (ediff_sessions(k)%merge_job) then
            call message("Ignoring letter case is too dangerous in merge jobs")
        else if (ediff_sessions(k)%diff3_job .and. len(ediff_ignore_case_option3) == 0) then
            call message("Ignoring letter case is not supported by this diff3 program")
        else if (.not. ediff_sessions(k)%three_way_job .and. len(ediff_ignore_case_option) == 0) then
            call message("Ignoring letter case is not supported by this diff program")
        else
            call update_ediff_diffs(status)
        end if
    end subroutine toggle_ediff_ignore_case

    !> Register the command of ediff-diff.el and give the options their
    !> defaults.  Not a port.
    subroutine init_ediff_diff()
        commands%toggle_ediff_ignore_case%execute => toggle_ediff_ignore_case
        if (.not. allocated(ediff_diff_program)) ediff_diff_program = "diff"
        if (.not. allocated(ediff_diff3_program)) ediff_diff3_program = "diff3"
        if (.not. allocated(ediff_custom_diff_program)) ediff_custom_diff_program = ediff_diff_program
        if (.not. allocated(ediff_custom_diff_options)) ediff_custom_diff_options = "-c"
        if (.not. allocated(ediff_diff_options)) ediff_diff_options = ""
        if (.not. allocated(ediff_ignore_case_option)) ediff_ignore_case_option = "-i"
        if (.not. allocated(ediff_ignore_case_option3)) ediff_ignore_case_option3 = ""
        if (.not. allocated(ediff_diff3_options)) ediff_diff3_options = ""

        if (.not. allocated(ediff_diff3_ok_lines_regexp)) then
            ediff_diff3_ok_lines_regexp = "^\([1-3]:\|====\|  \|.*Warning *:\|.*No newline\|.*missing newline\|^"// &
                                          CR//"$\)"
        end if
    end subroutine init_ediff_diff
end module fortmacs_diff_diff_mod
