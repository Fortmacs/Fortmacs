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
!> String and regexp search (`search_forward', `re_search_forward',
!> `looking_at', `string_match' and their relatives) with the case table,
!> a cache of compiled patterns and the match data.  regex_emacs_fm does
!> the matching.
!>
!> Positions in strings are 1-based character indices, as Fortran's
!> `index' (Emacs counts from 0), so that 0 can stand for nil.
!>
!> Deferred: `match_data' markers, `inhibit_changing_match_data',
!> Boyer-Moore search, the newline cache.
module search_fm
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use error_fm, only: status_type, set_error, FM_GENERIC_ERROR, FM_SEARCH_FAILED, FM_ARGS_OUT_OF_RANGE
    use handle_fm, only: handle_type
    use print_fm, only: prin1_string
    use data_fm, only: logical_setting
    use character_fm, only: string_char_and_length, char_bytes, char_head_p
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
    public :: re_search_forward, re_search_backward, posix_search_forward, posix_search_backward, &
              looking_at, posix_looking_at, string_match, posix_string_match, match_data, set_match_data, &
              regexp_quote, match_in_string_p, syms_of_search, replace_match, match_data_translate
    ! For search_regexp_fm (gfortran makes private procedures invisible to
    ! submodules).
    public :: string_chars, string_index_to_offset, string_offset_to_index, clear_search_regs, ensure_search_regs, &
              matcher_overflow, &
              search_command

    !> Non-nil if searches and matches should ignore case.  Port of
    !> buffer.c case-fold-search (buffer-local when set).
    type(logical_setting), public :: case_fold_search = logical_setting(name="case_fold_search", value=.true.)

    !> Values of NOERROR for `search_forward' and `search_backward': nil
    !> (signal `search_failed'), t (return nil), and anything else (move to
    !> the limit of the search, return nil).
    integer, parameter, public :: SEARCH_SIGNAL = 0, SEARCH_NOERROR = 1, SEARCH_MOVE = 2

    !> If a string, the regexp that replaces spaces in a regexp being
    !> compiled; unallocated is nil.  Port of search.c
    !> search-spaces-regexp.
    character(len=:), allocatable, public :: search_spaces_regexp

    !> The match data of the last search (search.c:search_regs), indexed
    !> by group: buffer positions or string indices, -1 when unset.  The
    !> size is search_regs.num_regs: unallocated is 0.
    integer(int64), allocatable :: search_regs_start(:), search_regs_end(:)
    !> Registers a regexp match first allocates (regex-emacs.c RE_NREGS).
    integer, parameter :: RE_NREGS = 30
    !> The buffer of the last search (last_thing_searched), unless it was
    !> on a string.
    type(handle_type) :: last_thing_searched
    logical :: last_search_in_string = .false.

    character(len=*), parameter :: newline = achar(10)

    interface
        !> Port of search.c:looking_at_1.  (In search_regexp_fm.)
        module function looking_at_1(regexp, posix, modify_data, status) result(matched)
            implicit none(type, external)
            character(len=*), intent(in) :: regexp
            logical, intent(in) :: posix, modify_data
            type(status_type), intent(out) :: status
            logical :: matched
        end function looking_at_1

        !> Port of search.c:string_match_1.  (In search_regexp_fm.)
        module function string_match_1(regexp, string, start, posix, modify_data, status) result(idx)
            implicit none(type, external)
            character(len=*), intent(in) :: regexp, string
            integer(int64), intent(in), optional :: start
            logical, intent(in) :: posix, modify_data
            type(status_type), intent(out) :: status
            integer(int64) :: idx
        end function string_match_1

        !> Regexp search for `search_buffer'.  Port of
        !> search.c:search_buffer_re.  (In search_regexp_fm.)
        module function search_buffer_re(string, pos_in, pos_byte_in, lim, lim_byte, n_in, fold, posix, status) &
            result(np)
            implicit none(type, external)
            character(len=*), intent(in) :: string
            integer(int64), intent(in) :: pos_in, pos_byte_in, lim, lim_byte, n_in
            logical, intent(in) :: fold, posix
            type(status_type), intent(inout) :: status
            integer(int64) :: np
        end function search_buffer_re

        !> Replace text matched by last search with NEWTEXT.
        !> Leave point at the end of the replacement text.
        !>
        !> If optional second arg FIXEDCASE is non-nil, do not alter the
        !> case of the replacement text.  Otherwise, maybe capitalize the
        !> whole text, or maybe just word initials, based on the replaced
        !> text.  If the replaced text has only capital letters and has at
        !> least one multiletter word, convert NEWTEXT to all caps.
        !> Otherwise if all words are capitalized in the replaced text,
        !> capitalize each word in NEWTEXT.  Note that what exactly is a
        !> word is determined by the syntax tables in effect in the current
        !> buffer, and the variable `case_symbols_as_words'.
        !>
        !> If optional third arg LITERAL is non-nil, insert NEWTEXT
        !> literally.  Otherwise treat `\' as special:
        !>   `\&' in NEWTEXT means substitute original matched text.
        !>   `\N' means substitute what matched the Nth `\(...\)'.
        !>        If Nth parens didn't match, substitute nothing.
        !>   `\\' means insert one `\'.
        !>   `\?' is treated literally
        !>        (for compatibility with `query_replace_regexp').
        !>   Any other character following `\' signals an error.
        !> Case conversion does not apply to these substitutions.
        !>
        !> If optional fourth argument STRING is non-nil, it should be a
        !> string to act on; this should be the string on which the
        !> previous match was done via `string_match'.  In this case,
        !> `replace_match' creates and returns a new string (in RESULT),
        !> made by copying STRING and replacing the part of STRING that was
        !> matched (the original STRING itself is not altered).
        !>
        !> The optional fifth argument SUBEXP specifies a subexpression;
        !> it says to replace just that subexpression with NEWTEXT, rather
        !> than replacing the entire matched text.  Port of
        !> search.c:Freplace_match.  (In search_regexp_fm.)
        module subroutine replace_match(newtext, status, fixedcase, literal, string, subexp, result)
            implicit none(type, external)
            character(len=*), intent(in) :: newtext
            type(status_type), intent(out) :: status
            logical, intent(in), optional :: fixedcase, literal
            character(len=*), intent(in), optional :: string
            integer, intent(in), optional :: subexp
            character(len=:), allocatable, intent(out), optional :: result
        end subroutine replace_match

        !> Register the search commands.  (In search_regexp_fm.)
        module subroutine syms_of_search()
            implicit none(type, external)
        end subroutine syms_of_search
    end interface

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

    !> Search for `count' newlines from `start' toward `end' (forward if
    !> count > 0, backward if count < 0; `end' defaults to ZV or BEGV).
    !> Returns the position just after the count'th newline found -- also
    !> when searching backward -- or `end' if there are fewer.  `counted'
    !> receives the number found (negated when backward); `bytepos' the
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

    !> Move point past `count' line boundaries from `start', or to `limit'
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

    !> Scan `count' lines from point (count <= 0: to the start of the line
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

    !> Like `find_newline', but returns the position before the newline,
    !> searching no further than `to' (default: ZV/BEGV).
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
    ! Compiled patterns
    ! ------------------------------------------------------------------

    !> The 1-based character index in STR of the byte offset `off'.
    pure integer(int64) function string_offset_to_index(str, off) result(idx)
        character(len=*), intent(in) :: str
        integer(int64), intent(in) :: off
        integer(int64) :: i
        ! Each character before OFF has one head byte.
        idx = 1
        do i = 1, off
            if (char_head_p(ichar(str(i:i), int32))) idx = idx + 1
        end do
    end function string_offset_to_index

    !> The byte offset in STR of the 1-based character index `idx'.
    pure integer(int64) function string_index_to_offset(str, idx) result(off)
        character(len=*), intent(in) :: str
        integer(int64), intent(in) :: idx
        integer(int64) :: n
        integer(int32) :: c, clen
        off = 0
        n = 1
        do while (n < idx .and. off < len(str, kind=int64))
            call string_char_and_length(str, off + 1, c, clen)
            off = off + clen
            n = n + 1
        end do
    end function string_index_to_offset

    !> Number of characters in STR.
    pure integer(int64) function string_chars(str) result(n)
        character(len=*), intent(in) :: str
        integer(int64) :: off
        integer(int32) :: c, clen
        n = 0
        off = 0
        do while (off < len(str, kind=int64))
            call string_char_and_length(str, off + 1, c, clen)
            off = off + clen
            n = n + 1
        end do
    end function string_chars

    !> Make the match data `n' empty registers.
    subroutine clear_search_regs(n)
        integer, intent(in) :: n
        if (allocated(search_regs_start)) then
            if (size(search_regs_start) /= n) deallocate (search_regs_start, search_regs_end)
        end if
        if (.not. allocated(search_regs_start)) allocate (search_regs_start(0:n - 1), search_regs_end(0:n - 1))
        search_regs_start = -1
        search_regs_end = -1
    end subroutine clear_search_regs

    !> Make room in the match data for the `n' registers of a regexp match
    !> and unset them all, as re_match_2_internal does with
    !> REGS_UNALLOCATED (max (RE_NREGS, n) registers the first time) and
    !> REGS_REALLOCATE (grow by half when too few, never shrink).  Port of
    !> the register allocation in regex-emacs.c:re_match_2_internal.
    subroutine ensure_search_regs(n)
        integer, intent(in) :: n
        integer :: m
        if (.not. allocated(search_regs_start)) then
            m = max(RE_NREGS, n)
        else
            m = size(search_regs_start)
            if (m < n) m = max(m + m/2, n)
        end if
        call clear_search_regs(m)
    end subroutine ensure_search_regs

    ! ------------------------------------------------------------------
    ! Matching at point and in strings
    ! ------------------------------------------------------------------

    !> Return t if text after point matches regular expression REGEXP.
    !> By default, this function modifies the match data that
    !> `match_beginning', `match_end' and `match_data' access.  If
    !> INHIBIT-MODIFY is non-nil, don't modify the match data.  Port of
    !> search.c:Flooking_at.
    logical function looking_at(regexp, status, inhibit_modify) result(matched)
        character(len=*), intent(in) :: regexp
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: inhibit_modify
        matched = looking_at_1(regexp, .false.,.not. opt(inhibit_modify), status)
    end function looking_at

    !> Return t if text after point matches REGEXP according to Posix rules.
    !> Find the longest match, in accordance with Posix regular expression
    !> rules.
    !>
    !> By default, this function modifies the match data that
    !> `match_beginning', `match_end' and `match_data' access.  If
    !> INHIBIT-MODIFY is non-nil, don't modify the match data.  Port of
    !> search.c:Fposix_looking_at.
    logical function posix_looking_at(regexp, status, inhibit_modify) result(matched)
        character(len=*), intent(in) :: regexp
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: inhibit_modify
        matched = looking_at_1(regexp, .true.,.not. opt(inhibit_modify), status)
    end function posix_looking_at

    !> The value of an optional logical, false if absent.
    pure logical function opt(flag)
        logical, intent(in), optional :: flag
        opt = .false.
        if (present(flag)) opt = flag
    end function opt

    !> Return index of start of first match for REGEXP in STRING, or nil
    !> (0).  Matching ignores case if `case_fold_search' is non-nil.  If
    !> START is present, start search at that index in STRING.
    !>
    !> If INHIBIT-MODIFY is non-nil, match data is not changed.
    !>
    !> If INHIBIT-MODIFY is nil or missing, match data is changed, and
    !> `match_end' and `match_beginning' give indices of substrings matched
    !> by parenthesis constructs in the pattern.  You can use the function
    !> `match_string' to extract the substrings matched by the parenthesis
    !> constructions in REGEXP.  For index of first char beyond the match,
    !> do (match-end 0).
    !>
    !> Indices are 1-based character indices, as Fortran's `index' (Emacs
    !> counts from 0); a negative START counts from the end.  Port of
    !> search.c:Fstring_match.
    integer(int64) function string_match(regexp, string, status, start, inhibit_modify) result(idx)
        character(len=*), intent(in) :: regexp, string
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: start
        logical, intent(in), optional :: inhibit_modify
        idx = string_match_1(regexp, string, start, .false.,.not. opt(inhibit_modify), status)
    end function string_match

    !> Return index of start of first match for Posix REGEXP in STRING, or
    !> nil (0).  Find the longest match, in accord with Posix regular
    !> expression rules.  Case is ignored if `case_fold_search' is non-nil
    !> in the current buffer.
    !>
    !> If INHIBIT-MODIFY is non-nil, match data is not changed.
    !>
    !> If INHIBIT-MODIFY is nil or missing, match data is changed, and
    !> `match_end' and `match_beginning' give indices of substrings matched
    !> by parenthesis constructs in the pattern.  You can use the function
    !> `match_string' to extract the substrings matched by the parenthesis
    !> constructions in REGEXP.  For index of first char beyond the match,
    !> do (match-end 0).  Port of search.c:Fposix_string_match.
    integer(int64) function posix_string_match(regexp, string, status, start, inhibit_modify) result(idx)
        character(len=*), intent(in) :: regexp, string
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: start
        logical, intent(in), optional :: inhibit_modify
        idx = string_match_1(regexp, string, start, .true.,.not. opt(inhibit_modify), status)
    end function posix_string_match

    !> Signal that the matcher's stack overflowed (matcher_overflow).
    subroutine matcher_overflow(status)
        type(status_type), intent(inout) :: status
        call set_error(status, FM_GENERIC_ERROR, "Stack overflow in regexp matcher")
    end subroutine matcher_overflow

    ! ------------------------------------------------------------------
    ! String search
    ! ------------------------------------------------------------------

    !> Search forward from point for STRING.
    !> Set point to the end of the occurrence found, and return point
    !> in `found' (0 for nil).
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
    !> `case_fold_search', which see.
    !>
    !> See also the functions `match_beginning', `match_end', `match_string',
    !> and `replace_match'.  Port of search.c:Fsearch_forward.
    subroutine search_forward(string, found, status, bound, noerror, count)
        character(len=*), intent(in) :: string
        integer(int64), intent(out) :: found
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: bound, count
        integer, intent(in), optional :: noerror
        call search_command(string, 1_int64, .false., .false., found, status, bound, noerror, count)
    end subroutine search_forward

    !> Search backward from point for STRING.
    !> This function is almost identical to `search_forward', except that
    !> by default it searches backward instead of forward, and the sign of
    !> COUNT also indicates exactly the opposite searching direction.
    !> See `search_forward' for details.  Port of search.c:Fsearch_backward.
    subroutine search_backward(string, found, status, bound, noerror, count)
        character(len=*), intent(in) :: string
        integer(int64), intent(out) :: found
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: bound, count
        integer, intent(in), optional :: noerror
        call search_command(string, -1_int64, .false., .false., found, status, bound, noerror, count)
    end subroutine search_backward

    !> Search forward from point for regular expression REGEXP.
    !> Set point to the end of the occurrence found, and return point
    !> in `found' (0 for nil).
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
    !> `case_fold_search', which see.
    !>
    !> See also the functions `match_beginning', `match_end', `match_string',
    !> and `replace_match'.  Port of search.c:Fre_search_forward.
    subroutine re_search_forward(regexp, found, status, bound, noerror, count)
        character(len=*), intent(in) :: regexp
        integer(int64), intent(out) :: found
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: bound, count
        integer, intent(in), optional :: noerror
        call search_command(regexp, 1_int64, .true., .false., found, status, bound, noerror, count)
    end subroutine re_search_forward

    !> Search backward from point for regular expression REGEXP.
    !> This function is almost identical to `re_search_forward', except that
    !> by default it searches backward instead of forward, and the sign of
    !> COUNT also indicates exactly the opposite searching direction.
    !> See `re_search_forward' for details.
    !>
    !> Note that searching backwards may give a shorter match than expected,
    !> because REGEXP is still matched in the forward direction.  Port of
    !> search.c:Fre_search_backward.
    subroutine re_search_backward(regexp, found, status, bound, noerror, count)
        character(len=*), intent(in) :: regexp
        integer(int64), intent(out) :: found
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: bound, count
        integer, intent(in), optional :: noerror
        call search_command(regexp, -1_int64, .true., .false., found, status, bound, noerror, count)
    end subroutine re_search_backward

    !> Search forward from point for REGEXP according to Posix rules.
    !> Find the longest match in accord with Posix regular expression rules.
    !> Set point to the end of the occurrence found, and return point.
    !> BOUND, NOERROR and COUNT are as for `re_search_forward'.  Port of
    !> search.c:Fposix_search_forward.
    subroutine posix_search_forward(regexp, found, status, bound, noerror, count)
        character(len=*), intent(in) :: regexp
        integer(int64), intent(out) :: found
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: bound, count
        integer, intent(in), optional :: noerror
        call search_command(regexp, 1_int64, .true., .true., found, status, bound, noerror, count)
    end subroutine posix_search_forward

    !> Search backward from point for match for REGEXP according to Posix
    !> rules.  Find the longest match in accord with Posix regular
    !> expression rules.  This function is almost identical to
    !> `posix_search_forward', except that by default it searches backward
    !> instead of forward, and the sign of COUNT also indicates exactly the
    !> opposite searching direction.  Port of
    !> search.c:Fposix_search_backward.
    subroutine posix_search_backward(regexp, found, status, bound, noerror, count)
        character(len=*), intent(in) :: regexp
        integer(int64), intent(out) :: found
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: bound, count
        integer, intent(in), optional :: noerror
        call search_command(regexp, -1_int64, .true., .true., found, status, bound, noerror, count)
    end subroutine posix_search_backward

    !> Port of search.c:search_command.
    subroutine search_command(string, direction, re, posix, found, status, bound, noerror, count)
        character(len=*), intent(in) :: string
        integer(int64), intent(in) :: direction
        logical, intent(in) :: re, posix
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
        end associate
        np = search_buffer(string, buffers(cur)%pt, buffers(cur)%pt_byte, lim, lim_byte, n, case_fold_search%value, &
                           status, re, posix)
        if (.not. status%ok()) return
        if (np <= 0) then
            if (mode == SEARCH_SIGNAL) then
                call set_error(status, FM_SEARCH_FAILED, prin1_string(string))
            else if (mode == SEARCH_MOVE) then
                call temp_set_point_both(buffers(cur), lim, lim_byte)
            end if
            return
        end if
        call temp_set_point_both(buffers(cur), np, char_to_byte(np))
        found = np
    end subroutine search_command

    !> Return true if REGEXP matches just one constant string.  Port of
    !> search.c:trivial_regexp_p.
    pure logical function trivial_regexp_p(regexp)
        character(len=*), intent(in) :: regexp
        integer :: i
        trivial_regexp_p = .false.
        i = 1
        do while (i <= len(regexp))
            select case (regexp(i:i))
            case (".", "*", "+", "?", "[", "^", "$")
                return
            case ("\")
                if (i == len(regexp)) return
                i = i + 1
                if (index("|()`'bB<>wWsS={}_cC123456789", regexp(i:i)) > 0) return
            case default
                continue
            end select
            i = i + 1
        end do
        trivial_regexp_p = .true.
    end function trivial_regexp_p

    !> Search for the `n'th occurrence of STRING in the current buffer,
    !> starting at position `pos' and stopping at position `lim'; with
    !> `re', STRING is a regexp (a Posix one with `posix'); with `fold',
    !> case is folded through the case canon table.  `n' negative searches
    !> backward.  The value is the position after the match (forward) or
    !> of its start (backward), or -M when M matches remained to be found.
    !> Port of search.c:search_buffer and search_buffer_non_re (no
    !> Boyer-Moore: simple_search, which works for any pattern).
    integer(int64) function search_buffer(string, pos, pos_byte, lim, lim_byte, n, fold, status, re, posix) &
        result(np)
        character(len=*), intent(in) :: string
        integer(int64), intent(in) :: pos, pos_byte, lim, lim_byte, n
        logical, intent(in) :: fold
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: re, posix
        integer(int32), allocatable :: pat(:)
        integer(int64) :: i, k, len_byte
        integer(int32) :: c, clen
        logical :: is_re
        status = status_type()
        is_re = opt(re)
        ! Searching 0 times means don't move.  Null string is found at
        ! starting position.
        if (n == 0 .or. len(string) == 0) then
            call set_search_regs(pos_byte, 0_int64)
            np = pos
            return
        end if
        if (is_re .and. .not. (trivial_regexp_p(string) .and. .not. allocated(search_spaces_regexp))) then
            np = search_buffer_re(string, pos, pos_byte, lim, lim_byte, n, fold, opt(posix), status)
            return
        end if
        ! The pattern as characters, translated as the buffer text will be.
        ! In a trivial regexp, a backslash just quotes the next character.
        allocate (pat(len(string)))
        i = 1
        k = 0
        len_byte = 0
        do while (i <= len(string))
            if (is_re .and. string(i:i) == "\") i = i + 1
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
    !> length in bytes is `len_byte', from buffer position POS/POS_BYTE
    !> until LIM/LIM_BYTE.  With `fold', buffer characters are
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

    !> Record a match of `nbytes' bytes at `beg_byte' in the current
    !> buffer as the match data.  Port of search.c:set_search_regs.
    subroutine set_search_regs(beg_byte, nbytes)
        integer(int64), intent(in) :: beg_byte, nbytes
        integer :: n
        ! Make sure we have registers in which to store the match position,
        ! and clear out the other registers.
        n = 2
        if (allocated(search_regs_start)) n = max(n, size(search_regs_start))
        call clear_search_regs(n)
        search_regs_start(0) = byte_to_char(beg_byte)
        search_regs_end(0) = byte_to_char(beg_byte + nbytes)
        last_thing_searched = current_buffer()
        last_search_in_string = .false.
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
        character(len=11) :: buf
        status = status_type()
        pos = 0
        if (n < 0) then
            ! args_out_of_range (num, 0).
            write (buf, "(i0)") n
            call set_error(status, FM_ARGS_OUT_OF_RANGE, trim(buf)//", 0")
            return
        end if
        if (.not. allocated(search_regs_start)) then
            call set_error(status, FM_GENERIC_ERROR, "No match data, because no search succeeded")
            return
        end if
        if (n >= size(search_regs_start)) return
        if (search_regs_start(n) < 0) return
        if (beginningp) then
            pos = search_regs_start(n)
        else
            pos = search_regs_end(n)
        end if
    end subroutine match_limit

    !> Return a list of positions that record text matched by the last
    !> search.  Element 2N of the returned list is the position of the
    !> beginning of the match of the Nth subexpression; it corresponds to
    !> `(match-beginning N)'; element 2N + 1 is the position of the end of
    !> the match of the Nth subexpression; it corresponds to `(match-end
    !> N)'.  See `match_beginning' and `match_end'.  Positions are
    !> integers, 0 (nil) when the Nth pair didn't match; trailing nils are
    !> left out.
    !>
    !> Use `set_match_data' to reinstate the match data from the elements
    !> of this list.  Port of search.c:Fmatch_data (INTEGERS t, without the
    !> buffer).
    function match_data() result(data)
        integer(int64), allocatable :: data(:)
        integer :: i, n
        allocate (data(0))
        if (.not. allocated(search_regs_start)) return
        n = 0
        do i = 0, size(search_regs_start) - 1
            if (search_regs_start(i) >= 0) n = i + 1
        end do
        deallocate (data)
        allocate (data(2*n))
        do i = 0, n - 1
            if (search_regs_start(i) >= 0) then
                data(2*i + 1) = search_regs_start(i)
                data(2*i + 2) = search_regs_end(i)
            else
                data(2*i + 1:2*i + 2) = 0
            end if
        end do
    end function match_data

    !> Set internal data on last search match from elements of LIST.
    !> LIST should have been created by calling `match_data' previously;
    !> nil (0) elements mark groups that didn't match.  The positions are
    !> taken to be in the current buffer.  Port of search.c:Fset_match_data.
    subroutine set_match_data(data)
        integer(int64), intent(in) :: data(:)
        integer :: i, length, num_regs
        ! Allocate registers if they don't already exist (growing as
        ! xpalloc does: by half, or to 16 when tiny, or by what is needed).
        length = size(data)/2
        num_regs = 0
        if (allocated(search_regs_start)) num_regs = size(search_regs_start)
        if (length > num_regs) then
            i = num_regs + num_regs/2
            if (8*i < 128) i = 16
            if (i - num_regs < length - num_regs) i = length
            num_regs = i
        end if
        if (num_regs > 0) call clear_search_regs(num_regs)
        do i = 0, length - 1
            if (data(2*i + 1) > 0 .and. data(2*i + 2) > 0) then
                search_regs_start(i) = data(2*i + 1)
                search_regs_end(i) = data(2*i + 2)
            end if
        end do
        last_thing_searched = current_buffer()
        last_search_in_string = .false.
    end subroutine set_match_data

    !> Add N to all positions of the match data.  Internal.  Port of
    !> search.c:Fmatch_data__translate (string indices here are 1-based,
    !> so a match moved to the start of a string is translated by
    !> 1 - (match-beginning 0)).
    subroutine match_data_translate(n)
        integer(int64), intent(in) :: n
        integer :: i
        if (.not. allocated(search_regs_start)) return
        do i = 0, size(search_regs_start) - 1
            if (search_regs_start(i) >= 0) then
                search_regs_start(i) = max(0_int64, search_regs_start(i) + n)
                search_regs_end(i) = max(0_int64, search_regs_end(i) + n)
            end if
        end do
    end subroutine match_data_translate

    !> The buffer the match data refer to (last_thing_searched).
    type(handle_type) function match_buffer()
        match_buffer = last_thing_searched
    end function match_buffer

    !> Whether the last search was on a string.
    logical function match_in_string_p()
        match_in_string_p = last_search_in_string
    end function match_in_string_p

    !> Return a regexp string which matches exactly STRING and nothing
    !> else.  Port of search.c:Fregexp_quote.
    pure function regexp_quote(string) result(quoted)
        character(len=*), intent(in) :: string
        character(len=:), allocatable :: quoted
        integer :: i
        quoted = ""
        do i = 1, len(string)
            if (index("[*.\?+^$", string(i:i)) > 0) quoted = quoted//"\"
            quoted = quoted//string(i:i)
        end do
    end function regexp_quote
end module search_fm
