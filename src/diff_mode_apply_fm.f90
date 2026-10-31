! diff_mode_apply_fm.f90 -- hunk texts and applying hunks, a submodule
! of diff_mode_fm.
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
!> testing, reverting and visiting hunks; and diff_mode_fm's helpers.
submodule(diff_mode_fm) diff_mode_apply_fm
    use buffer_fm, only: get_buffer_create, kill_buffer, generate_new_buffer_name, buffer_live_p
    use editfns_fm, only: buffer_string
    use search_fm, only: search_forward, search_backward, regexp_quote
    use fns_fm, only: schars
    use xdisp_fm, only: message
    implicit none(type, external)

contains

    ! ------------------------------------------------------------------
    ! Helpers of diff_mode_fm (separate module procedures, so that this
    ! submodule can call them too)
    ! ------------------------------------------------------------------

    module procedure entry_index
    type(diff_locals_type), allocatable :: grown(:)
    integer :: n

    if (.not. allocated(diff_buffers)) allocate (diff_buffers(0))

    do k = 1, size(diff_buffers)
        if (diff_buffers(k)%serial == buffers(cur)%serial) return
    end do

    n = size(diff_buffers)
    allocate (grown(n + 1))
    grown(1:n) = diff_buffers
    grown(n + 1)%serial = buffers(cur)%serial
    call move_alloc(grown, diff_buffers)
    k = n + 1
    end procedure entry_index

    module procedure bind_inhibit_read_only
    type(status_type) :: st

    call symbol_value(sym("inhibit_read_only"), saved, st)
    if (.not. st%ok()) saved = nil()
    call set(sym("inhibit_read_only"), t_value(), st)
    end procedure bind_inhibit_read_only

    module procedure restore_inhibit_read_only
    type(status_type) :: st

    call set(sym("inhibit_read_only"), saved, st)
    end procedure restore_inhibit_read_only

    module procedure at
    type(status_type) :: st

    at = looking_at(re, st)
    end procedure at

    module procedure search_fwd
    type(status_type) :: st
    integer :: noerror

    noerror = SEARCH_NOERROR

    if (present(move)) then
        if (move) noerror = SEARCH_MOVE
    end if

    call re_search_forward(re, found, st, bound=bound, noerror=noerror, count=count)
    if (.not. st%ok()) found = 0
    end procedure search_fwd

    module procedure search_bwd
    type(status_type) :: st

    call re_search_backward(re, found, st, bound=bound, noerror=SEARCH_NOERROR)
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

    module procedure char_at_point
    logical :: found

    call char_after(c, found)
    if (.not. found) c = -1
    end procedure char_at_point

    module procedure string_to_number
    integer :: k

    n = 0

    do k = 1, len(s)
        if (s(k:k) < "0" .or. s(k:k) > "9") exit
        n = 10*n + (iachar(s(k:k)) - iachar("0"))
    end do
    end procedure string_to_number

    module procedure number_to_string
    character(len=24) :: buf

    write (buf, "(i0)") n
    s = trim(buf)
    end procedure number_to_string

    !> Check the first or second half of the context hunk at point: it
    !> must have LINES lines.  Port of
    !> diff-mode.el:diff-sanity-check-context-hunk-half.
    recursive subroutine diff_sanity_check_context_hunk_half(lines, status)
        use subr_fm, only: y_or_n_p
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
            c = char_at_point()
            call char_after(c1, found, point() + 1)
            if (.not. found) c1 = -1

            if ((c == 32 .or. c == 33 .or. c == 43 .or. c == 45) .and. (c1 == 32 .or. c1 == 9)) then
                count = count - 1
                more = .true.
            else if (count == 0 .or. count == lines) then
                more = .false.
            else if (c == 33 .or. c == 43 .or. c == 45) then
                yes = .false.

                if (c1 == 10) then
                    call y_or_n_p("Try to auto-fix whitespace loss damage? ", yes, status)
                    if (.not. status%ok()) return
                end if

                if (.not. yes) then
                    call set_error(status, FM_GENERIC_ERROR, "End of hunk ambiguously marked")
                    return
                end if

                call goto_char(point() + 1)
                call insert(" ", status)
                shortage = forward_line(-1_int64)
                more = .true.
            else if (lines < 0) then
                call set_error(status, FM_GENERIC_ERROR, "End of hunk ambiguously marked")
                return
            else
                call y_or_n_p("Try to auto-fix whitespace loss and word-wrap damage? ", yes, status)
                if (.not. status%ok()) return

                if (.not. yes) then
                    call set_error(status, FM_GENERIC_ERROR, "Abort!")
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
    use subr_fm, only: y_or_n_p
    integer(int64) :: saved, before, after, shortage
    integer(int32) :: c
    logical :: yes, more
    type(value_type) :: saved_inhibit

    status = status_type()
    ! Every modification is protected by a y-or-n-p, so it's probably OK
    ! to override a read-only setting.
    call bind_inhibit_read_only(saved_inhibit)
    saved = point()

    if (.not. at(diff_hunk_header_re)) then
        call set_error(status, FM_GENERIC_ERROR, "Not recognizable hunk header")
    else if (char_at_point() == iachar("*")) then
        if (.not. at("\*\{15\}\(?: .*\)?"//LF//"\*\*\* \([0-9]+\)\(?:,\([0-9]+\)\)? \*\*\*\*")) then
            call set_error(status, FM_GENERIC_ERROR, "Unrecognized context diff first hunk header format")
        else
            shortage = forward_line(2_int64)
            call diff_sanity_check_context_hunk_half(half_lines(), status)

            if (status%ok()) then
                if (.not. at(diff_context_mid_hunk_header_re)) then
                    call set_error(status, FM_GENERIC_ERROR, "Unrecognized context diff second hunk header format")
                else
                    shortage = forward_line(1_int64)
                    call diff_sanity_check_context_hunk_half(half_lines(), status)
                end if
            end if
        end if
    else if (char_at_point() == iachar("@")) then
        if (.not. at(diff_hunk_header_re_unified)) then
            call set_error(status, FM_GENERIC_ERROR, "Unrecognized unified diff hunk header format")
        else
            before = 1
            after = 1
            if (me(2) > 0) before = string_to_number(match_string(2))
            if (me(4) > 0) after = string_to_number(match_string(4))
            shortage = forward_line(1_int64)

            do
                more = .false.
                c = char_at_point()

                select case (c)
                case (32)
                    before = before - 1
                    after = after - 1
                    more = .true.
                case (45)
                    if (at(diff_separator_re) .and. before == 0 .and. after == 0) then
                        more = .false.
                    else if (at(diff_file_header_re) .and. before == 0 .and. after == 0) then
                        call insert(LF, status)
                        call goto_char(point() - 1)
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
                            call set_error(status, FM_GENERIC_ERROR, "End of hunk ambiguously marked")
                        else
                            call set_error(status, FM_GENERIC_ERROR, "Hunk seriously messed up")
                        end if
                    else
                        if (c == 10 .or. c < 0) then
                            call y_or_n_p("Try to auto-fix whitespace loss? ", yes, status)
                        else
                            call y_or_n_p("Try to auto-fix word-wrap damage? ", yes, status)
                        end if

                        if (status%ok() .and. .not. yes) call set_error(status, FM_GENERIC_ERROR, "Abort!")

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
    call goto_char(saved)

contains

    !> The number of lines a context half header from the match says.
    integer(int64) function half_lines()
        half_lines = 1
        if (me(2) > 0) half_lines = 1 + string_to_number(match_string(2)) - string_to_number(match_string(1))
    end function half_lines
    end procedure diff_sanity_check_hunk

    module procedure diff_hunk_text
    type(handle_type) :: old, tmp
    type(status_type) :: st
    integer(int64) :: src_pos, dst_pos, divider_pos, num_pfx_chars, keep, x, shortage, start
    integer(int32) :: curr_char, last_char
    logical :: kill

    status = status_type()
    res%text = ""
    res%offset = 0
    old = current_buffer()
    call get_buffer_create(generate_new_buffer_name(" *temp*"), tmp, status)
    if (.not. status%ok()) return
    call set_buffer(tmp, st)
    call insert(hunk, st)
    call goto_char(point_min())
    src_pos = 0
    dst_pos = 0
    divider_pos = 0
    num_pfx_chars = 2

    if (at("^@@")) then
        num_pfx_chars = 1
        shortage = forward_line(1_int64)
        src_pos = point()
        dst_pos = point()
    else if (at("^\*\*")) then
        shortage = forward_line(2_int64)
        src_pos = point()
        x = search_fwd(diff_context_mid_hunk_header_re)
        call beginning_of_line()
        divider_pos = point()
        shortage = forward_line(1_int64)
        dst_pos = point()
    else if (at("^[0-9]+a[0-9,]+$")) then
        shortage = forward_line(1_int64)
        dst_pos = point()
    else if (at("^[0-9,]+d[0-9]+$")) then
        shortage = forward_line(1_int64)
        src_pos = point()
    else if (at("^[0-9,]+c[0-9,]+$")) then
        shortage = forward_line(1_int64)
        src_pos = point()
        x = search_fwd("^---$")
        call beginning_of_line()
        divider_pos = point()
        shortage = forward_line(1_int64)
        dst_pos = point()
    else
        call set_error(status, FM_GENERIC_ERROR, "Unknown diff hunk type")
        call done()
        return
    end if

    if ((destp .and. dst_pos == 0) .or. (.not. destp .and. src_pos == 0)) then
        call done()
        return
    end if

    if (src_pos == divider_pos) then
        src_pos = dst_pos
    else if (dst_pos == point_max()) then
        dst_pos = src_pos
    end if

    if (char_offset >= 0) call goto_char(point_min() + char_offset)
    start = point()
    ! (save-excursion: a marker at point.)
    keep = merge(dst_pos, src_pos, destp)
    if (divider_pos > 0 .and. divider_pos > keep) call delete_region(divider_pos, point_max(), st)
    call delete_region(point_min(), keep, st)

    ! Point moves with the text before it.
    if (start >= keep) then
        start = start - (keep - 1)
    else
        start = 1
    end if

    last_char = -1
    call goto_char(point_min())

    do while (.not. eobp())
        curr_char = char_at_point()

        if (curr_char == merge(iachar("-"), iachar("+"), destp) .or. curr_char == iachar("\")) then
            x = point()
            kill = .false.

            if (curr_char == iachar("\") .and. last_char /= merge(iachar("-"), iachar("+"), destp)) then
                shortage = forward_line(1_int64)
                kill = eobp() .or. (last_char == iachar("-") .and. char_at_point() == iachar("+"))
                call goto_char(x)
            end if

            if (kill) x = max(point() - 1, point_min())
            shortage = forward_line(1_int64)
            call adjust_start(x, point())
            call delete_region(x, point(), st)
            call goto_char(x)
        else
            call adjust_start(point(), point() + num_pfx_chars)
            call delete_region(point(), min(point() + num_pfx_chars, point_max()), st)
            shortage = forward_line(1_int64)
        end if

        last_char = curr_char
    end do

    res%text = buffer_string()
    if (char_offset >= 0) res%offset = start - point_min()
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
        call kill_buffer(tmp, st2)
    end subroutine done
    end procedure diff_hunk_text

    !> Return the buffer position (BEG . END) of the nearest occurrence of
    !> TEXT (FOUND false if TEXT isn't found).  Port of
    !> diff-mode.el:diff-find-text.
    subroutine diff_find_text(text, found, beg, end)
        character(len=*), intent(in) :: text
        logical, intent(out) :: found
        integer(int64), intent(out) :: beg, end
        integer(int64) :: orig, f, fb, fe, bb, be
        logical :: forw, back
        type(status_type) :: st

        orig = point()
        call search_forward(text, f, st, noerror=SEARCH_NOERROR)
        forw = f /= 0 .and. st%ok()

        if (forw) then
            fb = mb(0)
            fe = me(0)
        end if

        call goto_char(orig + schars(text))
        call search_backward(text, f, st, noerror=SEARCH_NOERROR)
        back = f /= 0 .and. st%ok()

        if (back) then
            bb = mb(0)
            be = me(0)
        end if

        call choose(forw, fb, fe, back, bb, be, orig, found, beg, end)
    end subroutine diff_find_text

    !> The nearer to ORIG of the forward and backward matches.  Not a port.
    subroutine choose(forw, fb, fe, back, bb, be, orig, found, beg, end)
        logical, intent(in) :: forw, back
        integer(int64), intent(in) :: fb, fe, bb, be, orig
        logical, intent(out) :: found
        integer(int64), intent(out) :: beg, end

        found = forw .or. back
        beg = 0
        end = 0

        if (forw .and. back) then
            if (fb - orig > orig - bb) then
                beg = bb
                end = be
            else
                beg = fb
                end = fe
            end if
        else if (back) then
            beg = bb
            end = be
        else if (forw) then
            beg = fb
            end = fe
        end if
    end subroutine choose

    !> Return the buffer position (BEG . END) of the nearest occurrence of
    !> TEXT.  Whitespace differences are ignored.  Port of
    !> diff-mode.el:diff-find-approx-text.
    subroutine diff_find_approx_text(text, found, beg, end)
        character(len=*), intent(in) :: text
        logical, intent(out) :: found
        integer(int64), intent(out) :: beg, end
        character(len=:), allocatable :: re
        integer(int64) :: orig, f, fb, fe, bb, be
        logical :: forw, back
        type(status_type) :: st

        orig = point()
        re = "^[ "//TAB//LF//"]*"//words_re(text)//"[ "//TAB//LF//"]*"//LF
        call re_search_forward(re, f, st, noerror=SEARCH_NOERROR)
        forw = f /= 0 .and. st%ok()

        if (forw) then
            fb = mb(0)
            fe = me(0)
        end if

        call goto_char(orig + schars(text))
        call re_search_backward(re, f, st, noerror=SEARCH_NOERROR)
        back = f /= 0 .and. st%ok()

        if (back) then
            bb = mb(0)
            be = me(0)
        end if

        call choose(forw, fb, fe, back, bb, be, orig, found, beg, end)

    contains

        !> (mapconcat #'regexp-quote (split-string text) "[ \t\n]+")
        function words_re(t) result(r)
            character(len=*), intent(in) :: t
            character(len=:), allocatable :: r
            character(len=*), parameter :: WS = " "//achar(12)//achar(9)//achar(10)//achar(13)//achar(11)
            integer :: i, j

            r = ""
            i = 1

            do while (i <= len(t))
                if (index(WS, t(i:i)) > 0) then
                    i = i + 1
                    cycle
                end if

                j = i

                do while (j <= len(t))
                    if (index(WS, t(j:j)) > 0) exit
                    j = j + 1
                end do

                if (len(r) > 0) r = r//"[ "//TAB//LF//"]+"
                r = r//regexp_quote(t(i:j - 1))
                i = j
            end do
        end function words_re
    end subroutine diff_find_approx_text

    module procedure diff_find_source_location
    use files_fm, only: find_file_noselect
    character(len=:), allocatable :: hunk, line, file
    type(diff_text_type) :: old, new
    integer(int64) :: saved, char_offset, x, orig_pos, shortage, pb, pe, start
    type(handle_type) :: obuf
    logical :: other, rev, quiet, have_file, maybe_old, maybe_new, found_pos, switched
    integer(int64) :: ob, oe, nb, ne
    type(status_type) :: st

    status = status_type()
    other = .false.
    if (present(other_file)) other = other_file
    other = other .neqv. diff_jump_to_old_file
    rev = .false.
    if (present(reverse)) rev = reverse
    quiet = .false.
    if (present(noprompt)) quiet = noprompt
    saved = point()
    start = diff_beginning_of_hunk(status, .true.)

    if (.not. status%ok()) then
        call goto_char(saved)
        return
    end if

    char_offset = saved - start

    if (.not. quiet) then
        call diff_sanity_check_hunk(status)

        if (.not. status%ok()) then
            call goto_char(saved)
            return
        end if
    end if

    x = point()
    call buffer_substring(x, end_of_hunk_pos(), hunk, status)
    call diff_hunk_text(hunk, rev, char_offset, old, status)
    if (status%ok()) call diff_hunk_text(hunk,.not. rev, char_offset, new, status)

    if (.not. status%ok()) then
        call goto_char(saved)
        return
    end if

    if (.not. at("\(?:\*\{15\}.*"//LF//"\)?[-@* ]*\([0-9,]+\)\([ acd+]+\([0-9,]+\)\)?")) then
        call set_error(status, FM_GENERIC_ERROR, "Can't find the hunk header")
        call goto_char(saved)
        return
    end if

    if (other) then
        line = match_string(1)
    else if (me(3) > 0) then
        line = match_string(3)
    else
        if (search_fwd(diff_context_mid_hunk_header_re) == 0) then
            call set_error(status, FM_GENERIC_ERROR, "Can't find the hunk separator")
            call goto_char(saved)
            return
        end if

        line = match_string(1)
    end if

    call diff_find_file_name(file, have_file, status, other, quiet)

    if (.not. status%ok()) then
        call goto_char(saved)
        return
    end if

    if (.not. have_file) then
        call set_error(status, FM_GENERIC_ERROR, "Can't find the file")
        call goto_char(saved)
        return
    end if

    call find_file_noselect(file, loc%buf, status)

    if (.not. status%ok()) then
        call goto_char(saved)
        return
    end if

    if (present(other_arg)) then
        if (other_arg > 8) diff_jump_to_old_file = other
    end if

    call goto_char(saved)
    obuf = current_buffer()
    call set_buffer(loc%buf, st)
    call goto_char(point_min())
    shortage = forward_line(string_to_number(line) - 1)
    orig_pos = point()
    switched = .false.
    call diff_find_text(old%text, maybe_old, ob, oe)
    call goto_char(orig_pos)
    call diff_find_text(new%text, maybe_new, nb, ne)
    call goto_char(orig_pos)
    found_pos = .true.

    if (maybe_new .and. maybe_old .and. .not. rev) then
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
        call diff_find_approx_text(old%text, found_pos, pb, pe)
        call goto_char(orig_pos)

        if (.not. found_pos) then
            switched = .true.
            call diff_find_approx_text(new%text, found_pos, pb, pe)
            call goto_char(orig_pos)
            if (.not. found_pos) switched = .false.
        end if
    end if

    if (found_pos) then
        loc%found = .true.
        loc%line_offset = count_lines_between(orig_pos, pb)
        loc%beg = pb
        loc%end = pe
    else
        loc%found = .false.
        loc%beg = orig_pos
        loc%end = orig_pos + schars(old%text)
    end if

    if (switched) then
        loc%src = new
        loc%dst = old
        loc%switched = .true.
    else
        loc%src = old
        loc%dst = new
    end if

    call set_buffer(obuf, st)
    call goto_char(saved)

contains

    integer(int64) function end_of_hunk_pos() result(pos)
        integer(int64) :: s

        s = point()
        pos = diff_end_of_hunk()
        call goto_char(s)
    end function end_of_hunk_pos

    !> (count-lines A B), signed as it is used here: B before A counts
    !> the lines between them.
    integer(int64) function count_lines_between(a, b) result(n)
        use simple_fm, only: count_lines
        integer(int64), intent(in) :: a, b

        n = count_lines(a, b)
    end function count_lines_between
    end procedure diff_find_source_location

    !> Report what happened to a hunk.  Port of
    !> diff-mode.el:diff-hunk-status-msg.
    subroutine diff_hunk_status_msg(found, line_offset, reversed, dry_run)
        logical, intent(in) :: found, reversed, dry_run
        integer(int64), intent(in) :: line_offset
        character(len=:), allocatable :: msg

        if (dry_run) then
            msg = merge("already applied", "not yet applied", reversed)
            msg = trim(msg)
        else
            msg = merge("undone ", "applied", reversed)
            msg = trim(msg)
        end if

        if (.not. found) then
            call message("Hunk text not found")
        else if (line_offset == 0) then
            call message("Hunk "//msg)
        else if (line_offset == 1) then
            call message("Hunk "//msg//" at offset "//number_to_string(line_offset)//" line")
        else
            call message("Hunk "//msg//" at offset "//number_to_string(line_offset)//" lines")
        end if
    end subroutine diff_hunk_status_msg

    module procedure diff_apply_hunk
    use files_fm, only: backup_file_name_p, delete_file
    use fns_fm, only: yes_or_no_p
    use subr_fm, only: y_or_n_p
    use window_fm, only: display_buffer, display_alist_type, set_window_point, windows, pop_to_buffer
    use frame_fm, only: selected_frame
    type(diff_name_type), allocatable :: names(:)
    type(diff_source_location_type) :: loc
    type(display_alist_type) :: table
    type(handle_type) :: obuf, window, saved_window
    type(status_type) :: st
    integer(int64) :: x
    integer :: failures, k
    logical :: rev, deletion, yes, ask_backup

    status = status_type()
    rev = .false.
    if (present(reverse)) rev = reverse

    if (present(beg) .neqv. present(end)) then
        call set_error(status, FM_GENERIC_ERROR, "Invalid call to `diff_apply_hunk'")
        return
    end if

    if (present(beg)) then
        call diff_apply_buffer(failures, status, beg, end, rev, no_save=.true.)
        return
    end if

    x = diff_beginning_of_hunk(status, .true.)
    if (.not. status%ok()) return
    call diff_hunk_file_names(names, rev)
    deletion = .false.
    if (size(names) > 0) deletion = names(1)%s == "/dev/null"
    call diff_find_source_location(loc, status, deletion .neqv. rev, rev)
    if (.not. status%ok()) return

    if (.not. loc%found) then
        call set_error(status, FM_USER_ERROR, "Can't find the text to patch")
        return
    end if

    obuf = current_buffer()
    call set_buffer(loc%buf, st)
    ask_backup = .false.

    if (allocated(buffers(cur)%filename)) then
        k = entry_index()
        ask_backup = backup_file_name_p(buffers(cur)%filename) .and. .not. diff_buffers(k)%apply_to_backup

        if (ask_backup) then
            call yes_or_no_p("Really apply this hunk to "//file_name_nondirectory(buffers(cur)%filename)//"? ", &
                             yes, status)

            if (.not. status%ok()) then
                call set_buffer(obuf, st)
                return
            end if

            diff_buffers(k)%apply_to_backup = yes
            ask_backup = .not. yes
        end if
    end if

    call set_buffer(obuf, st)

    if (ask_backup) then
        if (rev) then
            call set_error(status, FM_USER_ERROR, "Use M-x diff_apply_hunk to apply it to the other file")
        else
            call set_error(status, FM_USER_ERROR, "Use C-u M-x diff_apply_hunk to apply it to the other file")
        end if

        return
    end if

    if (loc%switched) then
        ! (save-window-excursion (pop-to-buffer buf) ...)
        saved_window = selected_frame%selected_window
        call pop_to_buffer(loc%buf, status)
        if (.not. status%ok()) return
        call goto_char(loc%beg + loc%src%offset)

        if (rev) then
            call y_or_n_p("Hunk hasn't been applied yet; apply it now? ", yes, status)
        else
            call y_or_n_p("Hunk has already been applied; undo it? ", yes, status)
        end if

        call set_buffer(obuf, st)
        if (.not. status%ok()) return

        if (.not. yes) then
            call message("(Nothing done)")
            return
        end if
    end if

    if (deletion .and. .not. loc%switched) then
        call set_buffer(loc%buf, st)
        call y_or_n_p("Delete file "//char(226)//char(128)//char(152)//buffers(cur)%filename// &
                      char(226)//char(128)//char(153)//"?", yes, status)

        if (status%ok() .and. yes) then
            call delete_file(buffers(cur)%filename, status)
            if (status%ok()) call kill_buffer(loc%buf, status)
        end if

        call set_buffer(obuf, st)
        return
    end if

    call set_buffer(loc%buf, st)
    call goto_char(loc%beg)
    call delete_region(loc%beg, loc%end, status)
    if (status%ok()) call insert(loc%dst%text, status)
    call set_buffer(obuf, st)
    if (.not. status%ok()) return
    table%inhibit_same_window = .true.
    call display_buffer(loc%buf, window, st, table=table)
    if (window%index /= 0) call set_window_point(windows(window%index), loc%beg + loc%dst%offset, st)
    call diff_hunk_status_msg(.true., loc%line_offset, loc%switched .neqv. rev, .false.)
    if (diff_advance_after_apply_hunk) call diff_hunk_next(st)
    end procedure diff_apply_hunk

    module procedure diff_test_hunk
    use window_fm, only: display_buffer, set_window_point, windows
    type(diff_source_location_type) :: loc
    type(handle_type) :: window
    type(status_type) :: st
    logical :: rev

    rev = .false.
    if (present(reverse)) rev = reverse
    call diff_find_source_location(loc, status, .false., rev)
    if (.not. status%ok()) return
    call display_buffer(loc%buf, window, st)
    if (window%index /= 0) call set_window_point(windows(window%index), loc%beg + loc%src%offset, st)
    call diff_hunk_status_msg(loc%found, loc%line_offset, rev .neqv. loc%switched, .true.)
    end procedure diff_test_hunk

    module procedure diff_apply_buffer
    use files_fm, only: save_buffer
    !> The edits of one buffer, newest first.
    type :: edit_type
        type(handle_type) :: buf
        integer(int64), allocatable :: pbeg(:), pend(:)
        type(diff_name_type), allocatable :: dst(:)
    end type edit_type
    type(edit_type), allocatable :: edits(:)
    type(diff_source_location_type) :: loc
    type(handle_type) :: obuf
    type(status_type) :: st
    integer(int64) :: saved, x, before
    integer :: k, j, n
    logical :: rev, testing, nosave, more
    type(value_type) :: saved_inhibit

    status = status_type()
    rev = .false.
    if (present(reverse)) rev = reverse
    testing = .false.
    if (present(test)) testing = test
    nosave = .false.
    if (present(no_save)) nosave = no_save
    allocate (edits(0))
    failures = 0
    saved = point()

    if (present(beg)) then
        call goto_char(beg)
    else
        call goto_char(point_min())
    end if

    x = diff_beginning_of_hunk(status, .true.)

    if (.not. status%ok()) then
        call goto_char(saved)
        return
    end if

    do
        call diff_find_source_location(loc, status, .false., rev, testing)

        if (.not. status%ok()) then
            call goto_char(saved)
            return
        end if

        if (loc%found .and. .not. loc%switched) then
            call add_edit(loc)
        else
            failures = failures + 1
        end if

        before = point()
        call diff_hunk_next(st)
        more = point() /= before
        if (more .and. present(end)) more = point() < end
        if (more) more = at(diff_hunk_header_re)
        if (.not. more) exit
    end do

    call goto_char(saved)

    if (failures == 0) then
        if (.not. testing) then
            obuf = current_buffer()

            do k = size(edits), 1, -1
                call set_buffer(edits(k)%buf, st)
                call bind_inhibit_read_only(saved_inhibit)

                do j = 1, size(edits(k)%pbeg)
                    call goto_char(edits(k)%pbeg(j))
                    call delete_region(edits(k)%pbeg(j), edits(k)%pend(j), status)
                    if (status%ok()) call insert(edits(k)%dst(j)%s, status)
                end do

                call restore_inhibit_read_only(saved_inhibit)
                if (.not. nosave) call save_buffer(1_int64, .false., status)
            end do

            call set_buffer(obuf, st)
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
            call message(number_to_string(int(failures, int64))//" hunks failed; no buffers changed")
        end if
    end if

contains

    !> "VERB N buffer(s)".
    function plural(verb, count) result(s)
        character(len=*), intent(in) :: verb
        integer, intent(in) :: count
        character(len=:), allocatable :: s

        s = verb//" "//number_to_string(int(count, int64))//" buffer"
        if (count /= 1) s = s//"s"
    end function plural

    !> (push (cons pos dst) (alist-get buf buffer-edits)).
    subroutine add_edit(l)
        type(diff_source_location_type), intent(in) :: l
        type(edit_type), allocatable :: grown(:)
        integer(int64), allocatable :: gb(:), ge(:)
        type(diff_name_type), allocatable :: gd(:)
        integer :: i, m

        do i = 1, size(edits)
            if (edits(i)%buf%index == l%buf%index .and. edits(i)%buf%generation == l%buf%generation) exit
        end do

        if (i > size(edits)) then
            ! Newest buffer first, as `push' onto the table.
            m = size(edits)
            allocate (grown(m + 1))
            grown(2:m + 1) = edits
            grown(1)%buf = l%buf
            allocate (grown(1)%pbeg(0), grown(1)%pend(0), grown(1)%dst(0))
            call move_alloc(grown, edits)
            i = 1
        end if

        m = size(edits(i)%pbeg)
        allocate (gb(m + 1), ge(m + 1), gd(m + 1))
        gb(1) = l%beg
        ge(1) = l%end
        gd(1)%s = l%dst%text
        gb(2:) = edits(i)%pbeg
        ge(2:) = edits(i)%pend
        gd(2:) = edits(i)%dst
        call move_alloc(gb, edits(i)%pbeg)
        call move_alloc(ge, edits(i)%pend)
        call move_alloc(gd, edits(i)%dst)
    end subroutine add_edit
    end procedure diff_apply_buffer

    module procedure diff_goto_source
    use window_fm, only: pop_to_buffer
    type(diff_source_location_type) :: loc
    integer(int64) :: saved
    logical :: rev

    saved = point()
    call beginning_of_line()
    rev = .not. at("[-<]")
    call goto_char(saved)
    call diff_find_source_location(loc, status, other_file, rev, other_arg=other_arg)
    if (.not. status%ok()) return
    call pop_to_buffer(loc%buf, status)
    if (.not. status%ok()) return
    call goto_char(loc%beg + loc%src%offset)
    end procedure diff_goto_source
end submodule diff_mode_apply_fm
