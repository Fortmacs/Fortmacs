! search_fm.f90 -- newline scanning (the start of the search.c port).
! Ported from GNU Emacs 31.1 src/search.c.
!
! Copyright (C) 1985-1987, 1993-1994, 1997-1999, 2001-2026 Free Software
! Foundation, Inc.
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

!> Finding newlines in the current buffer.
!>
!> Upstream's find_newline consults a per-buffer newline cache
!> (region-cache.c) and scans contiguous runs with memchr/memrchr.  Here
!> each run on either side of the gap is scanned with INDEX; the cache is
!> deferred until profiling shows a need.  Byte positions are always
!> known, so the "-1 = unknown" convention is replaced by optional
!> arguments.
!>
!> Literal string search (`search-forward`, `search-backward`) with the
!> case table and the match data; regexp search comes later.
module search_fm
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use error_fm, only: status_type, set_error, FM_GENERIC_ERROR, FM_SEARCH_FAILED, FM_ARGS_OUT_OF_RANGE
    use handle_fm, only: handle_type
    use character_fm, only: string_char_and_length, char_bytes
    use casetab_fm, only: case_canon
    use buffer_fm, only: buffers, cur, buf_prev_char_len, temp_set_point_both, buf_fetch_char, &
                         buf_next_char_len, current_buffer
    use marker_fm, only: char_to_byte, byte_to_char
    implicit none(type, external)
    private

    public :: find_newline, scan_newline, scan_newline_from_point, &
              find_newline_no_quit, find_before_next_newline
    public :: search_forward, search_backward, search_buffer, match_beginning, match_end, &
              match_buffer

    !> Non-nil if searches and matches should ignore case.  Port of
    !> buffer.c case-fold-search (one global value; not buffer-local yet).
    logical, public :: case_fold_search = .true.

    !> Values of NOERROR for `search_forward` and `search_backward`: nil
    !> (signal `search-failed`), t (return nil), and anything else (move to
    !> the limit of the search, return nil).
    integer, parameter, public :: SEARCH_SIGNAL = 0, SEARCH_NOERROR = 1, SEARCH_MOVE = 2

    !> The match data of the last search (search.c:search_regs and
    !> last_thing_searched): group 0 only, as there is no regexp search
    !> yet; character positions, -1 when unset.
    integer(int64) :: regs_start = -1, regs_end = -1
    type(handle_type) :: last_thing_searched

    character(len=*), parameter :: newline = achar(10)

contains

    !> Byte position of the first newline in [from_byte, to_byte) of the
    !> current buffer, or 0.
    integer(int64) function next_newline_byte(from_byte, to_byte) result(found)
        integer(int64), intent(in) :: from_byte, to_byte
        integer(int64) :: a, b, k
        found = 0
        associate (t => buffers(cur)%text)
            ! Before the gap: string index = byte position.
            a = from_byte
            b = min(to_byte, t%gpt_byte) - 1
            if (a <= b) then
                k = index(t%beg(a:b), newline, kind=int64)
                if (k > 0) then
                    found = a + k - 1
                    return
                end if
            end if
            ! After the gap: string index = byte position + gap_size.
            a = max(from_byte, t%gpt_byte)
            b = to_byte - 1
            if (a <= b) then
                k = index(t%beg(a + t%gap_size:b + t%gap_size), newline, kind=int64)
                if (k > 0) found = a + k - 1
            end if
        end associate
    end function next_newline_byte

    !> Byte position of the last newline in [from_byte, to_byte), or 0.
    integer(int64) function prev_newline_byte(from_byte, to_byte) result(found)
        integer(int64), intent(in) :: from_byte, to_byte
        integer(int64) :: a, b, k
        found = 0
        associate (t => buffers(cur)%text)
            a = max(from_byte, t%gpt_byte)
            b = to_byte - 1
            if (a <= b) then
                k = index(t%beg(a + t%gap_size:b + t%gap_size), newline, back=.true., kind=int64)
                if (k > 0) then
                    found = a + k - 1
                    return
                end if
            end if
            a = from_byte
            b = min(to_byte, t%gpt_byte) - 1
            if (a <= b) then
                k = index(t%beg(a:b), newline, back=.true., kind=int64)
                if (k > 0) found = a + k - 1
            end if
        end associate
    end function prev_newline_byte

    !> Search for `count` newlines from `start` toward `end` (forward if
    !> count > 0, backward if count < 0; `end` defaults to ZV or BEGV).
    !> Returns the position just after the count'th newline found -- also
    !> when searching backward -- or `end` if there are fewer.  `counted`
    !> receives the number found (negated when backward); `bytepos` the
    !> byte position of the result.  Port of search.c:find_newline.
    integer(int64) function find_newline(start, end, count, counted, bytepos, start_byte, end_byte) &
        result(pos)
        integer(int64), intent(in) :: start, count
        integer(int64), intent(in), optional :: end, start_byte, end_byte
        integer(int64), intent(out), optional :: counted, bytepos
        integer(int64) :: lim, lim_byte, pos_byte, nl, left
        associate (b => buffers(cur))
            if (present(end)) then
                lim = end
                if (present(end_byte)) then
                    lim_byte = end_byte
                else
                    lim_byte = char_to_byte(end)
                end if
            else if (count > 0) then
                lim = b%zv
                lim_byte = b%zv_byte
            else
                lim = b%begv
                lim_byte = b%begv_byte
            end if
        end associate
        if (present(start_byte)) then
            pos_byte = start_byte
        else
            pos_byte = char_to_byte(start)
        end if
        left = count
        if (count > 0) then
            do while (pos_byte < lim_byte)
                nl = next_newline_byte(pos_byte, lim_byte)
                if (nl == 0) exit
                pos_byte = nl + 1
                left = left - 1
                if (left == 0) then
                    call finish(byte_to_char(pos_byte), pos_byte)
                    return
                end if
            end do
        else if (count < 0) then
            do while (pos_byte > lim_byte)
                nl = prev_newline_byte(lim_byte, pos_byte)
                if (nl == 0) exit
                left = left + 1
                if (left >= 0) then
                    call finish(byte_to_char(nl + 1), nl + 1)
                    return
                end if
                pos_byte = nl
            end do
        else
            ! count == 0: "do anything you please"; stay put.
            lim = start
            lim_byte = pos_byte
        end if
        ! Fewer than COUNT newlines: stop at the limit.
        call finish(lim, lim_byte)
    contains

        subroutine finish(p, pbyte)
            integer(int64), intent(in) :: p, pbyte
            pos = p
            if (present(bytepos)) bytepos = pbyte
            if (present(counted)) counted = count - left
        end subroutine finish
    end function find_newline

    !> Move point past `count` line boundaries from `start`, or to `limit`
    !> if there are fewer.  Port of search.c:scan_newline.
    subroutine scan_newline(start, start_byte, limit, limit_byte, count)
        integer(int64), intent(in) :: start, start_byte, limit, limit_byte, count
        integer(int64) :: charpos, bytepos, counted
        charpos = find_newline(start, limit, count, counted, bytepos, start_byte, limit_byte)
        if (counted /= count) then
            call temp_set_point_both(buffers(cur), limit, limit_byte)
        else
            call temp_set_point_both(buffers(cur), charpos, bytepos)
        end if
    end subroutine scan_newline

    !> Scan `count` lines from point (count <= 0: to the start of the line
    !> |count| lines back) without moving point.  Returns how many newlines
    !> were found.  Port of search.c:scan_newline_from_point.
    integer(int64) function scan_newline_from_point(count, charpos, bytepos) result(counted)
        integer(int64), intent(in) :: count
        integer(int64), intent(out) :: charpos, bytepos
        associate (b => buffers(cur))
            if (count <= 0) then
                charpos = find_newline(b%pt, b%begv, count - 1, counted, bytepos, b%pt_byte, b%begv_byte)
            else
                charpos = find_newline(b%pt, b%zv, count, counted, bytepos, b%pt_byte, b%zv_byte)
            end if
        end associate
    end function scan_newline_from_point

    !> Port of search.c:find_newline_no_quit.
    integer(int64) function find_newline_no_quit(from, frombyte, cnt, bytepos)
        integer(int64), intent(in) :: from, frombyte, cnt
        integer(int64), intent(out), optional :: bytepos
        find_newline_no_quit = find_newline(from, count=cnt, bytepos=bytepos, start_byte=frombyte)
    end function find_newline_no_quit

    !> Like `find_newline`, but returns the position before the newline,
    !> searching no further than `to` (default: ZV/BEGV).
    !> Port of search.c:find_before_next_newline.
    integer(int64) function find_before_next_newline(from, cnt, bytepos, to) result(pos)
        integer(int64), intent(in) :: from, cnt
        integer(int64), intent(out), optional :: bytepos
        integer(int64), intent(in), optional :: to
        integer(int64) :: counted, pbyte
        pos = find_newline(from, to, cnt, counted, pbyte)
        if (counted == cnt) then
            pos = pos - 1
            pbyte = pbyte - buf_prev_char_len(buffers(cur), pbyte)
        end if
        if (present(bytepos)) bytepos = pbyte
    end function find_before_next_newline

    ! ------------------------------------------------------------------
    ! String search
    ! ------------------------------------------------------------------

    !> Search forward from point for STRING.
    !> Set point to the end of the occurrence found, and return point
    !> in `found` (0 for nil).
    !> The optional argument BOUND is a buffer position that bounds the
    !> search.  The match found must not end after that position.  Absent
    !> means search to the end of the accessible portion of the buffer.
    !> The optional argument NOERROR indicates how errors are handled
    !> when the search fails: if it is SEARCH_SIGNAL or absent, emit an
    !> error; if it is SEARCH_NOERROR, simply return nil and do nothing;
    !> if it is SEARCH_MOVE, move to the limit of search and return nil.
    !> The optional argument COUNT is a number that indicates the search
    !> direction and the number of occurrences to search for.  If it is
    !> positive, search forward for COUNT successive occurrences; if it is
    !> negative, search backward, instead of forward, for -COUNT
    !> occurrences.  Absent means the same as 1.
    !> With COUNT positive/negative, the match found is the
    !> COUNTth/-COUNTth one in the buffer located entirely after/before
    !> the origin of the search.
    !>
    !> Search case-sensitivity is determined by the value of the variable
    !> `case-fold-search', which see.
    !>
    !> See also the functions `match-beginning' and `match-end'.  Port of
    !> search.c:Fsearch_forward.
    subroutine search_forward(string, found, status, bound, noerror, count)
        character(len=*), intent(in) :: string
        integer(int64), intent(out) :: found
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: bound, count
        integer, intent(in), optional :: noerror
        call search_command(string, 1_int64, found, status, bound, noerror, count)
    end subroutine search_forward

    !> Search backward from point for STRING.
    !> This function is almost identical to `search-forward', except that
    !> by default it searches backward instead of forward, and the sign of
    !> COUNT also indicates exactly the opposite searching direction.
    !> See `search-forward' for details.  Port of search.c:Fsearch_backward.
    subroutine search_backward(string, found, status, bound, noerror, count)
        character(len=*), intent(in) :: string
        integer(int64), intent(out) :: found
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: bound, count
        integer, intent(in), optional :: noerror
        call search_command(string, -1_int64, found, status, bound, noerror, count)
    end subroutine search_backward

    !> Port of search.c:search_command (literal strings only).
    subroutine search_command(string, direction, found, status, bound, noerror, count)
        character(len=*), intent(in) :: string
        integer(int64), intent(in) :: direction
        integer(int64), intent(out) :: found
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: bound, count
        integer, intent(in), optional :: noerror
        integer(int64) :: n, lim, lim_byte, np
        integer :: mode
        status = status_type()
        found = 0
        mode = SEARCH_SIGNAL
        if (present(noerror)) mode = noerror
        n = direction
        if (present(count)) n = n*count
        associate (b => buffers(cur))
            if (.not. present(bound)) then
                if (n > 0) then
                    lim = b%zv
                    lim_byte = b%zv_byte
                else
                    lim = b%begv
                    lim_byte = b%begv_byte
                end if
            else
                lim = bound
                if (merge(lim < b%pt, lim > b%pt, n > 0)) then
                    call set_error(status, FM_GENERIC_ERROR, "Invalid search bound (wrong side of point)")
                    return
                end if
                if (lim > b%zv) then
                    lim = b%zv
                    lim_byte = b%zv_byte
                else if (lim < b%begv) then
                    lim = b%begv
                    lim_byte = b%begv_byte
                else
                    lim_byte = char_to_byte(lim)
                end if
            end if
            np = search_buffer(string, b%pt, b%pt_byte, lim, lim_byte, n, case_fold_search)
            if (np <= 0) then
                if (mode == SEARCH_SIGNAL) then
                    call set_error(status, FM_SEARCH_FAILED, '"'//string//'"')
                else if (mode == SEARCH_MOVE) then
                    call temp_set_point_both(b, lim, lim_byte)
                end if
                return
            end if
            call temp_set_point_both(b, np, char_to_byte(np))
        end associate
        found = np
    end subroutine search_command

    !> Search for the `n`th occurrence of STRING in the current buffer,
    !> starting at position `pos` and stopping at position `lim`, treating
    !> STRING as a literal string; with `fold`, through the case canon
    !> table.  `n` negative searches backward.  The value is the position
    !> after the match (forward) or of its start (backward), or -M when M
    !> matches remained to be found.  Port of search.c:search_buffer and
    !> search_buffer_non_re (no Boyer-Moore: simple_search, which works
    !> for any pattern).
    integer(int64) function search_buffer(string, pos, pos_byte, lim, lim_byte, n, fold) result(np)
        character(len=*), intent(in) :: string
        integer(int64), intent(in) :: pos, pos_byte, lim, lim_byte, n
        logical, intent(in) :: fold
        integer(int32), allocatable :: pat(:)
        integer(int64) :: i, k, len_byte
        integer(int32) :: c, clen
        ! Searching 0 times means don't move.  Null string is found at
        ! starting position.
        if (n == 0 .or. len(string) == 0) then
            call set_search_regs(pos_byte, 0_int64)
            np = pos
            return
        end if
        ! The pattern as characters, translated as the buffer text will be.
        allocate (pat(len(string)))
        i = 1
        k = 0
        len_byte = 0
        do while (i <= len(string))
            call string_char_and_length(string, i, c, clen)
            if (fold) c = case_canon(c)
            k = k + 1
            pat(k) = c
            len_byte = len_byte + char_bytes(c)
            i = i + clen
        end do
        np = simple_search(n, pat(1:k), len_byte, fold, pos, pos_byte, lim, lim_byte)
    end function search_buffer

    !> Do a simple string search N times for the characters PAT, whose
    !> length in bytes is `len_byte`, from buffer position POS/POS_BYTE
    !> until LIM/LIM_BYTE.  With `fold`, buffer characters are
    !> translated through the case canon table.
    !>
    !> Return the character position where the match is found.
    !> Otherwise, if M matches remained to be found, return -M.
    !> Port of search.c:simple_search (multibyte buffers).
    integer(int64) function simple_search(count, pat, len_byte, fold, start, start_byte, lim, lim_byte) &
        result(np)
        integer(int64), intent(in) :: count, len_byte, start, start_byte, lim, lim_byte
        integer(int32), intent(in) :: pat(:)
        logical, intent(in) :: fold
        integer(int64) :: n, pos, pos_byte, this_pos, this_pos_byte, this_len, match_byte, nchars
        integer(int32) :: buf_ch
        n = count
        pos = start
        pos_byte = start_byte
        nchars = size(pat)
        match_byte = -1
        associate (b => buffers(cur))
            if (lim > pos) then
                forward: do while (n > 0)
                    do
                        ! Try matching at position POS.
                        this_pos_byte = pos_byte
                        this_len = 0
                        if (pos + nchars > lim .or. pos_byte + len_byte > lim_byte) exit forward
                        do while (this_len < nchars)
                            buf_ch = buf_fetch_char(b, this_pos_byte)
                            if (fold) buf_ch = case_canon(buf_ch)
                            if (buf_ch /= pat(this_len + 1)) exit
                            this_len = this_len + 1
                            this_pos_byte = this_pos_byte + buf_next_char_len(b, this_pos_byte)
                        end do
                        if (this_len == nchars) then
                            match_byte = this_pos_byte - pos_byte
                            pos = pos + nchars
                            pos_byte = pos_byte + match_byte
                            exit
                        end if
                        pos_byte = pos_byte + buf_next_char_len(b, pos_byte)
                        pos = pos + 1
                    end do
                    n = n - 1
                end do forward
            else if (lim < pos) then
                ! Backwards search.
                backward: do while (n < 0)
                    do
                        ! Try matching at position POS.
                        this_pos = pos
                        this_pos_byte = pos_byte
                        this_len = 0
                        if (this_pos - nchars < lim .or. pos_byte - len_byte < lim_byte) exit backward
                        do while (this_len < nchars)
                            this_pos = this_pos - 1
                            this_pos_byte = this_pos_byte - buf_prev_char_len(b, this_pos_byte)
                            buf_ch = buf_fetch_char(b, this_pos_byte)
                            if (fold) buf_ch = case_canon(buf_ch)
                            if (buf_ch /= pat(nchars - this_len)) exit
                            this_len = this_len + 1
                        end do
                        if (this_len == nchars) then
                            match_byte = pos_byte - this_pos_byte
                            pos = this_pos
                            pos_byte = this_pos_byte
                            exit
                        end if
                        pos = pos - 1
                        pos_byte = pos_byte - buf_prev_char_len(b, pos_byte)
                    end do
                    n = n + 1
                end do backward
            end if
        end associate
        if (n == 0) then
            if (count > 0) then
                call set_search_regs(pos_byte - match_byte, match_byte)
            else
                call set_search_regs(pos_byte, match_byte)
            end if
            np = pos
        else
            np = -abs(n)
        end if
    end function simple_search

    !> Record a match of `nbytes` bytes at `beg_byte` in the current
    !> buffer as the match data.  Port of search.c:set_search_regs.
    subroutine set_search_regs(beg_byte, nbytes)
        integer(int64), intent(in) :: beg_byte, nbytes
        regs_start = byte_to_char(beg_byte)
        regs_end = byte_to_char(beg_byte + nbytes)
        last_thing_searched = current_buffer()
    end subroutine set_search_regs

    !> Return position of start of text matched by last search.
    !> SUBEXP, a number, specifies the parenthesized subexpression in the
    !> last regexp for which to return the start position.
    !> Value is nil (0) if SUBEXPth subexpression didn't match, or there
    !> were fewer than SUBEXP subexpressions.
    !> SUBEXP zero means the entire text matched by the whole regexp or
    !> whole string.
    !>
    !> Return value is undefined if the last search failed.  Port of
    !> search.c:Fmatch_beginning.
    integer(int64) function match_beginning(subexp, status) result(pos)
        integer, intent(in) :: subexp
        type(status_type), intent(out) :: status
        call match_limit(subexp, .true., pos, status)
    end function match_beginning

    !> Return position of end of text matched by last search.
    !> SUBEXP, a number, specifies the parenthesized subexpression in the
    !> last regexp for which to return the end position.
    !> Value is nil (0) if SUBEXPth subexpression didn't match, or there
    !> were fewer than SUBEXP subexpressions.
    !> SUBEXP zero means the entire text matched by the whole regexp or
    !> whole string.
    !>
    !> Return value is undefined if the last search failed.  Port of
    !> search.c:Fmatch_end.
    integer(int64) function match_end(subexp, status) result(pos)
        integer, intent(in) :: subexp
        type(status_type), intent(out) :: status
        call match_limit(subexp, .false., pos, status)
    end function match_end

    !> Port of search.c:match_limit.
    subroutine match_limit(n, beginningp, pos, status)
        integer, intent(in) :: n
        logical, intent(in) :: beginningp
        integer(int64), intent(out) :: pos
        type(status_type), intent(out) :: status
        status = status_type()
        pos = 0
        if (n < 0) then
            call set_error(status, FM_ARGS_OUT_OF_RANGE)
            return
        end if
        if (regs_start < 0) then
            call set_error(status, FM_GENERIC_ERROR, "No match data, because no search succeeded")
            return
        end if
        if (n > 0) return
        if (beginningp) then
            pos = regs_start
        else
            pos = regs_end
        end if
    end subroutine match_limit

    !> The buffer the match data refer to (last_thing_searched).
    type(handle_type) function match_buffer()
        match_buffer = last_thing_searched
    end function match_buffer
end module search_fm
