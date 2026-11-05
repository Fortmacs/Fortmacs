! diff_mode_apply_mod.f90 -- hunk texts and applying hunks, a submodule
! of diff_mode_mod.
! Ported from GNU Emacs 31.1 lisp/vc/diff-mode.el.
!
! Copyright (C) 1998-2026 Free Software Foundation, Inc.
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

!> The source text of a hunk, finding it in its file, and applying,
!> testing, reverting and visiting hunks; and diff_mode_mod's helpers.
submodule(diff_mode_mod) diff_mode_apply_mod
    use value_mod, only: value_type
    use buffer_mod, only: get_or_make_buffer, close_buffer, generate_new_buffer_name, is_buffer_live
    use editing_functions_mod, only: buffer_string
    use search_mod, only: search_forward, search_backward, regular_expression_quote
    use utility_functions_mod, only: count_characters
    use display_engine_mod, only: message
    use string_type_mod, only: string_type
    implicit none(type, external)

contains

    ! ------------------------------------------------------------------
    ! Helpers of diff_mode_mod (separate module procedures, so that this
    ! submodule can call them too)
    ! ------------------------------------------------------------------

    module procedure make_current_entry
    type(diff_locals_type) :: entry

    if (.not. allocated(diff_buffers)) allocate (diff_buffers(0))
    k = findloc(diff_buffers%serial, buffers(current)%serial, dim=1)
    if (k > 0) return
    entry%serial = buffers(current)%serial
    diff_buffers = [diff_buffers, entry]
    k = size(diff_buffers)
    end procedure make_current_entry

    module procedure bind_inhibit_read_only
    type(status_type) :: st

    call symbol_value(value_type(name="inhibit_read_only"), saved, st)
    if (.not. st%ok()) saved = value_type()
    call set(value_type(name="inhibit_read_only"), value_type(.true.), st)
    end procedure bind_inhibit_read_only

    module procedure restore_inhibit_read_only
    type(status_type) :: st

    call set(value_type(name="inhibit_read_only"), saved, st)
    end procedure restore_inhibit_read_only

    module procedure at
    type(status_type) :: st

    at = looking_at(regular_expression, st)
    end procedure at

    module procedure search_fwd
    type(status_type) :: st
    integer :: noerror

    noerror = SEARCH_NOERROR

    if (present(move)) then
        if (move) noerror = SEARCH_MOVE
    end if

    call regular_expression_search_forward(regular_expression, found, st, bound=bound, noerror=noerror, count=count)
    if (.not. st%ok()) found = 0
    end procedure search_fwd

    module procedure search_bwd
    type(status_type) :: st

    call regular_expression_search_backward(regular_expression, found, st, bound=bound, noerror=SEARCH_NOERROR)
    if (.not. st%ok()) found = 0
    end procedure search_bwd

    module procedure mb
    type(status_type) :: st

    mb = match_beginning(n, st)
    end procedure mb

    module procedure me
    type(status_type) :: st

    me = match_end(n, st)
    end procedure me

    module procedure character_at_point
    logical :: found

    call character_after(c, found)
    if (.not. found) c = -1
    end procedure character_at_point

    !> Check the first or second half of the context hunk at point: it
    !> must have LINES lines.  Port of
    !> diff-mode.el:diff-sanity-check-context-hunk-half.
    recursive subroutine diff_sanity_check_context_hunk_half(lines, status)
        use basic_subprograms_mod, only: read_y_or_n
        integer(int64), intent(in) :: lines
        type(status_type), intent(out) :: status
        integer(int64) :: count, shortage
        integer(int32) :: c, c1
        logical :: yes, more
        logical :: found

        status = status_type()
        count = lines

        do
            more = .false.
            c = character_at_point()
            call character_after(c1, found, point() + 1)
            if (.not. found) c1 = -1

            if ((c == 32 .or. c == 33 .or. c == 43 .or. c == 45) .and. (c1 == 32 .or. c1 == 9)) then
                count = count - 1
                more = .true.
            else if (count == 0 .or. count == lines) then
                more = .false.
            else if (c == 33 .or. c == 43 .or. c == 45) then
                yes = .false.

                if (c1 == 10) then
                    call read_y_or_n("Try to auto-fix whitespace loss damage? ", yes, status)
                    if (.not. status%ok()) return
                end if

                if (.not. yes) then
                    call set_error(status, GENERIC_ERROR_CODE, "End of hunk ambiguously marked")
                    return
                end if

                call goto_character(point() + 1)
                call insert(" ", status)
                shortage = forward_line(-1_int64)
                more = .true.
            else if (lines < 0) then
                call set_error(status, GENERIC_ERROR_CODE, "End of hunk ambiguously marked")
                return
            else
                call read_y_or_n("Try to auto-fix whitespace loss and word-wrap damage? ", yes, status)
                if (.not. status%ok()) return

                if (.not. yes) then
                    call set_error(status, GENERIC_ERROR_CODE, "Abort!")
                    return
                end if

                if (c == 10 .or. c < 0) then
                    call insert("  ", status)
                    shortage = forward_line(-1_int64)
                else
                    call insert(" ", status)
                    call delete_region(point() - 2, point() - 1, status)
                end if

                more = .true.
            end if

            if (.not. more) exit
            shortage = forward_line(1_int64)
        end do
    end subroutine diff_sanity_check_context_hunk_half

    module procedure diff_sanity_check_hunk
    use basic_subprograms_mod, only: read_y_or_n
    integer(int64) :: saved, before, after, shortage
    integer(int32) :: c
    logical :: yes, more
    type(value_type) :: saved_inhibit

    status = status_type()
    ! Every modification is protected by a y-or-n-p, so it's probably OK
    ! to override a read-only setting.
    call bind_inhibit_read_only(saved_inhibit)
    saved = point()

    if (.not. at(diff_hunk_header_regular_expression)) then
        call set_error(status, GENERIC_ERROR_CODE, "Not recognizable hunk header")
    else if (character_at_point() == iachar("*")) then
        if (.not. at("\*\{15\}\(?: .*\)?"//LF//"\*\*\* \([0-9]+\)\(?:,\([0-9]+\)\)? \*\*\*\*")) then
            call set_error(status, GENERIC_ERROR_CODE, "Unrecognized context diff first hunk header format")
        else
            shortage = forward_line(2_int64)
            call diff_sanity_check_context_hunk_half(half_lines(), status)

            if (status%ok()) then
                if (.not. at(diff_context_middle_hunk_header_regular_expression)) then
                    call set_error(status, GENERIC_ERROR_CODE, "Unrecognized context diff second hunk header format")
                else
                    shortage = forward_line(1_int64)
                    call diff_sanity_check_context_hunk_half(half_lines(), status)
                end if
            end if
        end if
    else if (character_at_point() == iachar("@")) then
        if (.not. at(diff_hunk_header_regular_expression_unified)) then
            call set_error(status, GENERIC_ERROR_CODE, "Unrecognized unified diff hunk header format")
        else
            before = 1
            after = 1
            if (me(2) > 0) call string_to_number(match_string(2), before)
            if (me(4) > 0) call string_to_number(match_string(4), after)
            shortage = forward_line(1_int64)

            do
                more = .false.
                c = character_at_point()

                select case (c)
                case (32)
                    before = before - 1
                    after = after - 1
                    more = .true.
                case (45)
                    if (at(diff_separator_regular_expression) .and. before == 0 .and. after == 0) then
                        more = .false.
                    else if (at(diff_file_header_regular_expression) .and. before == 0 .and. after == 0) then
                        call insert(LF, status)
                        call goto_character(point() - 1)
                        more = .false.
                    else
                        before = before - 1
                        more = .true.
                    end if
                case (43)
                    after = after - 1
                    more = .true.
                case default
                    if (diff_valid_unified_empty_line .and. c == 10 .and. before > 0 .and. after > 0) then
                        before = before - 1
                        after = after - 1
                        more = .true.
                    else if (at("\\ ")) then
                        ! See `(diffutils)Incomplete Lines'.
                        more = .true.
                    else if (before == 0 .and. after == 0) then
                        more = .false.
                    else if (before < 0 .or. after < 0) then
                        if (before == 0 .or. after == 0) then
                            call set_error(status, GENERIC_ERROR_CODE, "End of hunk ambiguously marked")
                        else
                            call set_error(status, GENERIC_ERROR_CODE, "Hunk seriously messed up")
                        end if
                    else
                        if (c == 10 .or. c < 0) then
                            call read_y_or_n("Try to auto-fix whitespace loss? ", yes, status)
                        else
                            call read_y_or_n("Try to auto-fix word-wrap damage? ", yes, status)
                        end if

                        if (status%ok() .and. .not. yes) call set_error(status, GENERIC_ERROR_CODE, "Abort!")

                        if (status%ok()) then
                            if (c == 10 .or. c < 0) then
                                call insert(" ", status)
                                shortage = forward_line(-1_int64)
                            else
                                call insert(" ", status)
                                if (status%ok()) call delete_region(point() - 2, point() - 1, status)
                            end if

                            more = .true.
                        end if
                    end if
                end select

                if (.not. status%ok() .or. .not. more) exit
                shortage = forward_line(1_int64)
            end do
        end if
    end if

    call restore_inhibit_read_only(saved_inhibit)
    call goto_character(saved)

contains

    !> The number of lines a context half header from the match says.
    integer(int64) function half_lines()
        integer(int64) :: first, last

        half_lines = 1

        if (me(2) > 0) then
            call string_to_number(match_string(1), first)
            call string_to_number(match_string(2), last)
            half_lines = 1 + last - first
        end if
    end function half_lines
    end procedure diff_sanity_check_hunk

    module procedure diff_hunk_text
    type(handle_type) :: old, temporary
    type(status_type) :: st
    integer(int64) :: source_position, destination_position, divider_position, prefix_character_count, keep, x, shortage, start
    integer(int32) :: current_character, last_character
    logical :: kill

    status = status_type()
    res%text = ""
    res%offset = 0
    old = current_buffer()
    call get_or_make_buffer(generate_new_buffer_name(" *temp*"), temporary, status)
    if (.not. status%ok()) return
    call set_buffer(temporary, st)
    call insert(hunk, st)
    call goto_character(point_minimum())
    source_position = 0
    destination_position = 0
    divider_position = 0
    prefix_character_count = 2

    if (at("^@@")) then
        prefix_character_count = 1
        shortage = forward_line(1_int64)
        source_position = point()
        destination_position = point()
    else if (at("^\*\*")) then
        shortage = forward_line(2_int64)
        source_position = point()
        x = search_fwd(diff_context_middle_hunk_header_regular_expression)
        call beginning_of_line()
        divider_position = point()
        shortage = forward_line(1_int64)
        destination_position = point()
    else if (at("^[0-9]+a[0-9,]+$")) then
        shortage = forward_line(1_int64)
        destination_position = point()
    else if (at("^[0-9,]+d[0-9]+$")) then
        shortage = forward_line(1_int64)
        source_position = point()
    else if (at("^[0-9,]+c[0-9,]+$")) then
        shortage = forward_line(1_int64)
        source_position = point()
        x = search_fwd("^---$")
        call beginning_of_line()
        divider_position = point()
        shortage = forward_line(1_int64)
        destination_position = point()
    else
        call set_error(status, GENERIC_ERROR_CODE, "Unknown diff hunk type")
        call done()
        return
    end if

    if ((destp .and. destination_position == 0) .or. (.not. destp .and. source_position == 0)) then
        call done()
        return
    end if

    if (source_position == divider_position) then
        source_position = destination_position
    else if (destination_position == point_maximum()) then
        destination_position = source_position
    end if

    if (character_offset >= 0) call goto_character(point_minimum() + character_offset)
    start = point()
    ! (save-excursion: a marker at point.)
    keep = merge(destination_position, source_position, destp)
    if (divider_position > 0 .and. divider_position > keep) call delete_region(divider_position, point_maximum(), st)
    call delete_region(point_minimum(), keep, st)

    ! Point moves with the text before it.
    if (start >= keep) then
        start = start - (keep - 1)
    else
        start = 1
    end if

    last_character = -1
    call goto_character(point_minimum())

    do while (.not. is_end_of_buffer())
        current_character = character_at_point()

        if (current_character == merge(iachar("-"), iachar("+"), destp) .or. current_character == iachar("\")) then
            x = point()
            kill = .false.

            if (current_character == iachar("\") .and. last_character /= merge(iachar("-"), iachar("+"), destp)) then
                shortage = forward_line(1_int64)
                kill = is_end_of_buffer() .or. (last_character == iachar("-") .and. character_at_point() == iachar("+"))
                call goto_character(x)
            end if

            if (kill) x = max(point() - 1, point_minimum())
            shortage = forward_line(1_int64)
            call adjust_start(x, point())
            call delete_region(x, point(), st)
            call goto_character(x)
        else
            call adjust_start(point(), point() + prefix_character_count)
            call delete_region(point(), min(point() + prefix_character_count, point_maximum()), st)
            shortage = forward_line(1_int64)
        end if

        last_character = current_character
    end do

    res%text = buffer_string()
    if (character_offset >= 0) res%offset = start - point_minimum()
    call done()

contains

    !> The text from B to E is deleted: move START (a marker) with it.
    subroutine adjust_start(b, e)
        integer(int64), intent(in) :: b, e

        if (start > e) then
            start = start - (e - b)
        else if (start > b) then
            start = b
        end if
    end subroutine adjust_start

    subroutine done()
        type(status_type) :: st2

        call set_buffer(old, st2)
        call close_buffer(temporary, st2)
    end subroutine done
    end procedure diff_hunk_text

    !> Return the buffer positions BEGINNING and END of the nearest occurrence
    !> of TEXT (FOUND false if TEXT isn't found).  Port of
    !> diff-mode.el:diff-find-text.
    subroutine diff_find_text(text, found, beginning, end)
        character(len=*), intent(in) :: text
        logical, intent(out) :: found
        integer(int64), intent(out) :: beginning, end
        integer(int64) :: original, f, fb, fe, bb, be
        logical :: forw, back
        type(status_type) :: st

        original = point()
        call search_forward(text, f, st, noerror=SEARCH_NOERROR)
        forw = f /= 0 .and. st%ok()

        if (forw) then
            fb = mb(0)
            fe = me(0)
        end if

        call goto_character(original + count_characters(text))
        call search_backward(text, f, st, noerror=SEARCH_NOERROR)
        back = f /= 0 .and. st%ok()

        if (back) then
            bb = mb(0)
            be = me(0)
        end if

        call choose(forw, fb, fe, back, bb, be, original, found, beginning, end)
    end subroutine diff_find_text

    !> The nearer to ORIGINAL of the forward and backward matches.  Not a port.
    subroutine choose(forw, fb, fe, back, bb, be, original, found, beginning, end)
        logical, intent(in) :: forw, back
        integer(int64), intent(in) :: fb, fe, bb, be, original
        logical, intent(out) :: found
        integer(int64), intent(out) :: beginning, end

        found = forw .or. back
        beginning = 0
        end = 0

        if (forw .and. back) then
            if (fb - original > original - bb) then
                beginning = bb
                end = be
            else
                beginning = fb
                end = fe
            end if
        else if (back) then
            beginning = bb
            end = be
        else if (forw) then
            beginning = fb
            end = fe
        end if
    end subroutine choose

    !> Return the buffer positions BEGINNING and END of the nearest occurrence
    !> of TEXT.  Whitespace differences are ignored.  Port of
    !> diff-mode.el:diff-find-approx-text.
    subroutine diff_find_approx_text(text, found, beginning, end)
        character(len=*), intent(in) :: text
        logical, intent(out) :: found
        integer(int64), intent(out) :: beginning, end
        character(len=:), allocatable :: regular_expression
        integer(int64) :: original, f, fb, fe, bb, be
        logical :: forw, back
        type(status_type) :: st

        original = point()
        regular_expression = "^[ "//TAB//LF//"]*"//words_regular_expression(text)//"[ "//TAB//LF//"]*"//LF
        call regular_expression_search_forward(regular_expression, f, st, noerror=SEARCH_NOERROR)
        forw = f /= 0 .and. st%ok()

        if (forw) then
            fb = mb(0)
            fe = me(0)
        end if

        call goto_character(original + count_characters(text))
        call regular_expression_search_backward(regular_expression, f, st, noerror=SEARCH_NOERROR)
        back = f /= 0 .and. st%ok()

        if (back) then
            bb = mb(0)
            be = me(0)
        end if

        call choose(forw, fb, fe, back, bb, be, original, found, beginning, end)

    contains

        !> (mapconcat #'regexp-quote (split-string text) "[ \t\n]+")
        function words_regular_expression(t) result(r)
            character(len=*), intent(in) :: t
            character(len=:), allocatable :: r
            character(len=*), parameter :: WHITESPACE = " "//achar(12)//achar(9)//achar(10)//achar(13)//achar(11)
            integer :: i, j

            r = ""
            i = 1

            do while (i <= len(t))
                if (index(WHITESPACE, t(i:i)) > 0) then
                    i = i + 1
                    cycle
                end if

                j = i

                do while (j <= len(t))
                    if (index(WHITESPACE, t(j:j)) > 0) exit
                    j = j + 1
                end do

                if (len(r) > 0) r = r//"[ "//TAB//LF//"]+"
                r = r//regular_expression_quote(t(i:j - 1))
                i = j
            end do
        end function words_regular_expression
    end subroutine diff_find_approx_text

    module procedure diff_find_source_location
    use files_mod, only: find_file_noselect
    character(len=:), allocatable :: hunk, line, file
    type(diff_text_type) :: old, new
    integer(int64) :: saved, character_offset, x, original_position, shortage, pb, pe, start, line_number
    type(handle_type) :: output_buffer
    logical :: other, reversed, quiet, has_file, maybe_old, maybe_new, found_position, switched
    integer(int64) :: ob, oe, nb, ne
    type(status_type) :: st

    status = status_type()
    other = .false.
    if (present(other_file)) other = other_file
    other = other .neqv. diff_jump_to_old_file
    reversed = .false.
    if (present(reverse)) reversed = reverse
    quiet = .false.
    if (present(noprompt)) quiet = noprompt
    saved = point()
    start = diff_beginning_of_hunk(status, .true.)

    if (.not. status%ok()) then
        call goto_character(saved)
        return
    end if

    character_offset = saved - start

    if (.not. quiet) then
        call diff_sanity_check_hunk(status)

        if (.not. status%ok()) then
            call goto_character(saved)
            return
        end if
    end if

    x = point()
    call buffer_substring(x, end_of_hunk_position(), hunk, status)
    call diff_hunk_text(hunk, reversed, character_offset, old, status)
    if (status%ok()) call diff_hunk_text(hunk,.not. reversed, character_offset, new, status)

    if (.not. status%ok()) then
        call goto_character(saved)
        return
    end if

    if (.not. at("\(?:\*\{15\}.*"//LF//"\)?[-@* ]*\([0-9,]+\)\([ acd+]+\([0-9,]+\)\)?")) then
        call set_error(status, GENERIC_ERROR_CODE, "Can't find the hunk header")
        call goto_character(saved)
        return
    end if

    if (other) then
        line = match_string(1)
    else if (me(3) > 0) then
        line = match_string(3)
    else
        if (search_fwd(diff_context_middle_hunk_header_regular_expression) == 0) then
            call set_error(status, GENERIC_ERROR_CODE, "Can't find the hunk separator")
            call goto_character(saved)
            return
        end if

        line = match_string(1)
    end if

    call diff_find_file_name(file, has_file, status, other, quiet)

    if (.not. status%ok()) then
        call goto_character(saved)
        return
    end if

    if (.not. has_file) then
        call set_error(status, GENERIC_ERROR_CODE, "Can't find the file")
        call goto_character(saved)
        return
    end if

    call find_file_noselect(file, location%buffer, status)

    if (.not. status%ok()) then
        call goto_character(saved)
        return
    end if

    if (present(other_argument)) then
        if (other_argument > 8) diff_jump_to_old_file = other
    end if

    call goto_character(saved)
    output_buffer = current_buffer()
    call set_buffer(location%buffer, st)
    call goto_character(point_minimum())
    call string_to_number(line, line_number)
    shortage = forward_line(line_number - 1)
    original_position = point()
    switched = .false.
    call diff_find_text(old%text, maybe_old, ob, oe)
    call goto_character(original_position)
    call diff_find_text(new%text, maybe_new, nb, ne)
    call goto_character(original_position)
    found_position = .true.

    if (maybe_new .and. maybe_old .and. .not. reversed) then
        switched = .true.
        pb = nb
        pe = ne
    else if (maybe_old) then
        pb = ob
        pe = oe
    else if (maybe_new) then
        switched = .true.
        pb = nb
        pe = ne
    else
        switched = .false.
        call diff_find_approx_text(old%text, found_position, pb, pe)
        call goto_character(original_position)

        if (.not. found_position) then
            switched = .true.
            call diff_find_approx_text(new%text, found_position, pb, pe)
            call goto_character(original_position)
            if (.not. found_position) switched = .false.
        end if
    end if

    if (found_position) then
        location%found = .true.
        location%line_offset = count_lines_between(original_position, pb)
        location%beginning = pb
        location%end = pe
    else
        location%found = .false.
        location%beginning = original_position
        location%end = original_position + count_characters(old%text)
    end if

    if (switched) then
        location%source = new
        location%destination = old
        location%switched = .true.
    else
        location%source = old
        location%destination = new
    end if

    call set_buffer(output_buffer, st)
    call goto_character(saved)

contains

    integer(int64) function end_of_hunk_position() result(position)
        integer(int64) :: s

        s = point()
        position = diff_end_of_hunk()
        call goto_character(s)
    end function end_of_hunk_position

    !> (count-lines A B), signed as it is used here: B before A counts
    !> the lines between them.
    integer(int64) function count_lines_between(a, b) result(n)
        use simple_mod, only: count_lines
        integer(int64), intent(in) :: a, b

        n = count_lines(a, b)
    end function count_lines_between
    end procedure diff_find_source_location

    !> Report what happened to a hunk.  Port of
    !> diff-mode.el:diff-hunk-status-msg.
    subroutine diff_hunk_status_message(found, line_offset, reversed, dry_run)
        logical, intent(in) :: found, reversed, dry_run
        integer(int64), intent(in) :: line_offset
        character(len=:), allocatable :: message_text
        character(len=40) :: offset_text

        if (dry_run) then
            message_text = merge("already applied", "not yet applied", reversed)
            message_text = trim(message_text)
        else
            message_text = merge("undone ", "applied", reversed)
            message_text = trim(message_text)
        end if

        if (.not. found) then
            call message("Hunk text not found")
        else if (line_offset == 0) then
            call message("Hunk "//message_text)
        else if (line_offset == 1) then
            write (offset_text, '(" at offset ", i0, " line")') line_offset
            call message("Hunk "//message_text//trim(offset_text))
        else
            write (offset_text, '(" at offset ", i0, " lines")') line_offset
            call message("Hunk "//message_text//trim(offset_text))
        end if
    end subroutine diff_hunk_status_message

    module procedure diff_apply_hunk
    use files_mod, only: is_backup_file_name, delete_file
    use utility_functions_mod, only: read_yes_or_no
    use basic_subprograms_mod, only: read_y_or_n
    use window_mod, only: display_buffer, display_action_parameters_type, set_window_point, windows, pop_to_buffer
    use frame_mod, only: selected_frame
    type(string_type), allocatable :: names(:)
    type(diff_source_location_type) :: location
    type(display_action_parameters_type) :: table
    type(handle_type) :: output_buffer, window, saved_window
    type(status_type) :: st
    integer(int64) :: x
    integer :: failures, k
    logical :: reversed, deletion, yes, ask_backup

    status = status_type()
    reversed = .false.
    if (present(reverse)) reversed = reverse

    if (present(beginning) .neqv. present(end)) then
        call set_error(status, GENERIC_ERROR_CODE, "Invalid call to `diff_apply_hunk'")
        return
    end if

    if (present(beginning)) then
        call diff_apply_buffer(failures, status, beginning, end, reversed, no_save=.true.)
        return
    end if

    x = diff_beginning_of_hunk(status, .true.)
    if (.not. status%ok()) return
    call diff_hunk_file_names(names, reversed)
    deletion = .false.
    if (size(names) > 0) deletion = names(1)%text == "/dev/null"
    call diff_find_source_location(location, status, deletion .neqv. reversed, reversed)
    if (.not. status%ok()) return

    if (.not. location%found) then
        call set_error(status, USER_ERROR_CODE, "Can't find the text to patch")
        return
    end if

    output_buffer = current_buffer()
    call set_buffer(location%buffer, st)
    ask_backup = .false.

    if (allocated(buffers(current)%filename)) then
        call make_current_entry(k)
        ask_backup = is_backup_file_name(buffers(current)%filename) .and. .not. diff_buffers(k)%apply_to_backup

        if (ask_backup) then
            call read_yes_or_no("Really apply this hunk to "//file_name_nondirectory(buffers(current)%filename)//"? ", &
                                yes, status)

            if (.not. status%ok()) then
                call set_buffer(output_buffer, st)
                return
            end if

            diff_buffers(k)%apply_to_backup = yes
            ask_backup = .not. yes
        end if
    end if

    call set_buffer(output_buffer, st)

    if (ask_backup) then
        if (reversed) then
            call set_error(status, USER_ERROR_CODE, "Use M-x diff_apply_hunk to apply it to the other file")
        else
            call set_error(status, USER_ERROR_CODE, "Use C-u M-x diff_apply_hunk to apply it to the other file")
        end if

        return
    end if

    if (location%switched) then
        ! (save-window-excursion (pop-to-buffer buffer) ...)
        saved_window = selected_frame%selected_window
        call pop_to_buffer(location%buffer, status)
        if (.not. status%ok()) return
        call goto_character(location%beginning + location%source%offset)

        if (reversed) then
            call read_y_or_n("Hunk hasn't been applied yet; apply it now? ", yes, status)
        else
            call read_y_or_n("Hunk has already been applied; undo it? ", yes, status)
        end if

        call set_buffer(output_buffer, st)
        if (.not. status%ok()) return

        if (.not. yes) then
            call message("(Nothing done)")
            return
        end if
    end if

    if (deletion .and. .not. location%switched) then
        call set_buffer(location%buffer, st)
        call read_y_or_n("Delete file "//char(226)//char(128)//char(152)//buffers(current)%filename// &
                         char(226)//char(128)//char(153)//"?", yes, status)

        if (status%ok() .and. yes) then
            call delete_file(buffers(current)%filename, status)
            if (status%ok()) call close_buffer(location%buffer, status)
        end if

        call set_buffer(output_buffer, st)
        return
    end if

    call set_buffer(location%buffer, st)
    call goto_character(location%beginning)
    call delete_region(location%beginning, location%end, status)
    if (status%ok()) call insert(location%destination%text, status)
    call set_buffer(output_buffer, st)
    if (.not. status%ok()) return
    table%inhibit_same_window = .true.
    call display_buffer(location%buffer, window, st, table=table)
    if (window%index /= 0) call set_window_point(windows(window%index), location%beginning + location%destination%offset, st)
    call diff_hunk_status_message(.true., location%line_offset, location%switched .neqv. reversed, .false.)
    if (diff_advance_after_apply_hunk) call diff_hunk_next(st)
    end procedure diff_apply_hunk

    module procedure diff_test_hunk
    use window_mod, only: display_buffer, set_window_point, windows
    type(diff_source_location_type) :: location
    type(handle_type) :: window
    type(status_type) :: st
    logical :: reversed

    reversed = .false.
    if (present(reverse)) reversed = reverse
    call diff_find_source_location(location, status, .false., reversed)
    if (.not. status%ok()) return
    call display_buffer(location%buffer, window, st)
    if (window%index /= 0) call set_window_point(windows(window%index), location%beginning + location%source%offset, st)
    call diff_hunk_status_message(location%found, location%line_offset, reversed .neqv. location%switched, .true.)
    end procedure diff_test_hunk

    module procedure diff_apply_buffer
    use files_mod, only: save_buffer
    !> The edits of one buffer, newest first.
    type :: edit_type
        type(handle_type) :: buffer
        integer(int64), allocatable :: pbeg(:), pend(:)
        type(string_type), allocatable :: destination(:)
    end type edit_type
    type(edit_type), allocatable :: edits(:)
    type(diff_source_location_type) :: location
    type(handle_type) :: output_buffer
    type(status_type) :: st
    integer(int64) :: saved, x, before
    integer :: k, j, n
    logical :: reversed, testing, nosave, more
    type(value_type) :: saved_inhibit
    character(len=48) :: text

    status = status_type()
    reversed = .false.
    if (present(reverse)) reversed = reverse
    testing = .false.
    if (present(test)) testing = test
    nosave = .false.
    if (present(no_save)) nosave = no_save
    allocate (edits(0))
    failures = 0
    saved = point()

    if (present(beginning)) then
        call goto_character(beginning)
    else
        call goto_character(point_minimum())
    end if

    x = diff_beginning_of_hunk(status, .true.)

    if (.not. status%ok()) then
        call goto_character(saved)
        return
    end if

    do
        call diff_find_source_location(location, status, .false., reversed, testing)

        if (.not. status%ok()) then
            call goto_character(saved)
            return
        end if

        if (location%found .and. .not. location%switched) then
            call add_edit(location)
        else
            failures = failures + 1
        end if

        before = point()
        call diff_hunk_next(st)
        more = point() /= before
        if (more .and. present(end)) more = point() < end
        if (more) more = at(diff_hunk_header_regular_expression)
        if (.not. more) exit
    end do

    call goto_character(saved)

    if (failures == 0) then
        if (.not. testing) then
            output_buffer = current_buffer()

            do k = size(edits), 1, -1
                call set_buffer(edits(k)%buffer, st)
                call bind_inhibit_read_only(saved_inhibit)

                do j = 1, size(edits(k)%pbeg)
                    call goto_character(edits(k)%pbeg(j))
                    call delete_region(edits(k)%pbeg(j), edits(k)%pend(j), status)
                    if (status%ok()) call insert(edits(k)%destination(j)%text, status)
                end do

                call restore_inhibit_read_only(saved_inhibit)
                if (.not. nosave) call save_buffer(1_int64, .false., status)
            end do

            call set_buffer(output_buffer, st)
            n = size(edits)

            if (nosave) then
                call message(plural("Edited", n))
            else
                call message(plural("Saved", n))
            end if
        end if
    else if (.not. testing) then
        if (failures == 1) then
            call message("1 hunk failed; no buffers changed")
        else
            write (text, '(i0, " hunks failed; no buffers changed")') failures
            call message(trim(text))
        end if
    end if

contains

    !> "VERB N buffer(s)".
    function plural(verb, count) result(s)
        character(len=*), intent(in) :: verb
        integer, intent(in) :: count
        character(len=:), allocatable :: s
        character(len=24) :: count_text

        write (count_text, '(" ", i0, " buffer")') count
        s = verb//trim(count_text)
        if (count /= 1) s = s//"s"
    end function plural

    !> Add the edit L (a position and its destination) to the edits of its buffer.  Not a port.
    subroutine add_edit(l)
        type(diff_source_location_type), intent(in) :: l
        type(edit_type) :: edit
        type(string_type) :: destination
        integer :: i

        i = findloc(edits%buffer%index == l%buffer%index .and. edits%buffer%generation == l%buffer%generation, .true., dim=1)

        if (i == 0) then
            ! Newest buffer first, as `push' onto the table.
            edit%buffer = l%buffer
            allocate (edit%pbeg(0), edit%pend(0), edit%destination(0))
            edits = [edit, edits]
            i = 1
        end if

        destination%text = l%destination%text
        edits(i)%pbeg = [l%beginning, edits(i)%pbeg]
        edits(i)%pend = [l%end, edits(i)%pend]
        edits(i)%destination = [destination, edits(i)%destination]
    end subroutine add_edit
    end procedure diff_apply_buffer

    module procedure diff_goto_source
    use window_mod, only: pop_to_buffer
    type(diff_source_location_type) :: location
    integer(int64) :: saved
    logical :: reversed

    saved = point()
    call beginning_of_line()
    reversed = .not. at("[-<]")
    call goto_character(saved)
    call diff_find_source_location(location, status, other_file, reversed, other_argument=other_argument)
    if (.not. status%ok()) return
    call pop_to_buffer(location%buffer, status)
    if (.not. status%ok()) return
    call goto_character(location%beginning + location%source%offset)
    end procedure diff_goto_source
end submodule diff_mode_apply_mod
