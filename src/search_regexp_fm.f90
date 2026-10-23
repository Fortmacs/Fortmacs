! search_regexp_fm.f90 -- regexp search (the part of search.c that uses
! the regexp engine).
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

!> The cache of compiled patterns, `looking_at', `string_match' and regexp
!> search in buffers.  A submodule of search_fm, because the regexp engine
!> depends (through the syntax table) on modules that use search_fm.
submodule(search_fm) search_regexp_fm
    use commands_fm, only: commands
    use regex_emacs_fm, only: re_pattern_buffer_type, re_registers_type, re_subject_type, re_compile_pattern, &
                              re_search, re_match, subject_from_string, subject_from_buffer
    use syntax_fm, only: syntax_table, syntax_modiff
    use minibuf_fm, only: read_string
    use casefiddle_fm, only: upcase_string, upcase_initials_string, upcase_region, upcase_initials_region, &
                             case_symbols_as_words
    use casetab_fm, only: lowercasep, uppercasep
    use syntax_fm, only: char_syntax_class, SWORD, SSYMBOL
    use insdel_fm, only: replace_range
    use editfns_fm, only: buffer_substring, goto_char
    use buffer_fm, only: temp_set_point_both
    use error_fm, only: format_message, FM_ARGS_OUT_OF_RANGE
    implicit none(type, external)

    !> Number of compiled patterns kept (REGEXP_CACHE_SIZE).
    integer, parameter :: REGEXP_CACHE_SIZE = 20

    !> A compiled pattern and what it was compiled for (struct
    !> regexp_cache).
    type :: regexp_cache_type
        logical :: valid = .false.
        character(len=:), allocatable :: regexp
        logical :: posix = .false.
        logical :: has_whitespace_regexp = .false.
        character(len=:), allocatable :: whitespace_regexp
        type(handle_type) :: syntax_table
        !> `syntax_modiff' when compiled: clear_regexp_cache drops entries
        !> that used a syntax table when any syntax entry changes.
        integer(int64) :: syntax_modiff = 0
        integer(int64) :: used_at = 0
        type(re_pattern_buffer_type) :: buf
    end type regexp_cache_type

    type(regexp_cache_type), save :: searchbufs(REGEXP_CACHE_SIZE)
    integer(int64) :: clock = 0

contains

    !> Compile PATTERN for case folding `translate' and Posix backtracking
    !> `posix', or find it already compiled, and return its index in the
    !> cache, moved to the front as most recently used.  Port of
    !> search.c:compile_pattern and compile_pattern_1.
    subroutine compile_pattern(pattern, translate, posix, k, status)
        character(len=*), intent(in) :: pattern
        logical, intent(in) :: translate, posix
        integer, intent(out) :: k
        type(status_type), intent(out) :: status
        integer :: i
        logical :: has_ws
        status = status_type()
        has_ws = allocated(search_spaces_regexp)
        clock = clock + 1
        do i = 1, REGEXP_CACHE_SIZE
            associate (cp => searchbufs(i))
                if (.not. cp%valid) cycle
                if (cp%regexp /= pattern .or. len(cp%regexp) /= len(pattern)) cycle
                if ((cp%buf%translate .neqv. translate) .or. (cp%posix .neqv. posix)) cycle
                if (cp%buf%used_syntax) then
                    if (.not. same_handle(cp%syntax_table, syntax_table())) cycle
                    if (cp%syntax_modiff /= syntax_modiff) cycle
                end if
                if (cp%has_whitespace_regexp .neqv. has_ws) cycle
                if (has_ws) then
                    if (cp%whitespace_regexp /= search_spaces_regexp .or. &
                        len(cp%whitespace_regexp) /= len(search_spaces_regexp)) cycle
                end if
                cp%used_at = clock
                k = i
                return
            end associate
        end do
        ! Compile into the least recently used entry.
        k = minloc(searchbufs%used_at, 1)
        associate (cp => searchbufs(k))
            cp%valid = .false.
            cp%used_at = clock
            cp%buf%translate = translate
            cp%posix = posix
            cp%has_whitespace_regexp = has_ws
            if (has_ws) then
                cp%whitespace_regexp = search_spaces_regexp
                call re_compile_pattern(pattern, cp%buf, status, posix, search_spaces_regexp)
            else
                call re_compile_pattern(pattern, cp%buf, status, posix)
            end if
            ! If the compiled pattern hard codes some of the contents of the
            ! syntax-table, it can only be reused with *this* syntax table.
            cp%syntax_table = syntax_table()
            cp%syntax_modiff = syntax_modiff
            if (.not. status%ok()) return
            cp%regexp = pattern
            cp%valid = .true.
        end associate
    end subroutine compile_pattern

    !> Whether two handles are the same.
    pure logical function same_handle(a, b)
        type(handle_type), intent(in) :: a, b
        same_handle = a%index == b%index .and. a%generation == b%generation
    end function same_handle

    !> Make the match data from `regs', offsets in the current buffer's
    !> accessible portion.
    subroutine set_regs_from_buffer(regs)
        type(re_registers_type), intent(in) :: regs
        integer :: i
        call clear_search_regs(size(regs%starts))
        do i = 0, size(regs%starts) - 1
            if (regs%starts(i) >= 0) then
                search_regs_start(i) = byte_to_char(regs%starts(i) + buffers(cur)%begv_byte)
                search_regs_end(i) = byte_to_char(regs%ends(i) + buffers(cur)%begv_byte)
            end if
        end do
        last_thing_searched = current_buffer()
        last_search_in_string = .false.
    end subroutine set_regs_from_buffer

    !> Make the match data from `regs', offsets in STR.
    subroutine set_regs_from_string(regs, str)
        type(re_registers_type), intent(in) :: regs
        character(len=*), intent(in) :: str
        integer :: i
        call clear_search_regs(size(regs%starts))
        do i = 0, size(regs%starts) - 1
            if (regs%starts(i) >= 0) then
                search_regs_start(i) = string_offset_to_index(str, regs%starts(i))
                search_regs_end(i) = string_offset_to_index(str, regs%ends(i))
            end if
        end do
        last_search_in_string = .true.
    end subroutine set_regs_from_string

    !> Port of search.c:looking_at_1.
    module procedure looking_at_1
    type(re_subject_type) :: subj
    type(re_registers_type) :: regs
    integer(int64) :: i
    integer :: k
    matched = .false.
    call compile_pattern(regexp, case_fold_search%value, posix, k, status)
    if (.not. status%ok()) return
    subj = subject_from_buffer()
    associate (b => buffers(cur))
        i = re_match(searchbufs(k)%buf, subj, b%pt_byte - b%begv_byte, regs, b%zv_byte - b%begv_byte)
    end associate
    if (i == -2) then
        call matcher_overflow(status)
        return
    end if
    matched = i >= 0
    if (modify_data .and. matched) call set_regs_from_buffer(regs)
    end procedure looking_at_1

    !> Port of search.c:string_match_1.
    module procedure string_match_1
    type(re_subject_type) :: subj
    type(re_registers_type) :: regs
    integer(int64) :: pos, pos_byte, nchars, val
    integer :: k
    idx = 0
    status = status_type()
    pos = 1
    if (present(start)) then
        nchars = string_chars(string)
        pos = start
        if (pos < 0 .and. -pos <= nchars) then
            pos = nchars + pos + 1
        else if (pos < 1 .or. pos > nchars + 1) then
            call set_error(status, FM_ARGS_OUT_OF_RANGE)
            return
        end if
    end if
    pos_byte = string_index_to_offset(string, pos)
    call compile_pattern(regexp, case_fold_search%value, posix, k, status)
    if (.not. status%ok()) return
    subj = subject_from_string(string)
    val = re_search(searchbufs(k)%buf, subj, pos_byte, len(string, kind=int64) - pos_byte, regs, &
                    len(string, kind=int64))
    if (val == -2) then
        call matcher_overflow(status)
        return
    end if
    if (val < 0) return
    if (modify_data) call set_regs_from_string(regs, string)
    idx = string_offset_to_index(string, val)
    end procedure string_match_1

    !> Regexp search for `search_buffer'.  Port of
    !> search.c:search_buffer_re.
    module procedure search_buffer_re
    type(re_subject_type) :: subj
    type(re_registers_type) :: regs
    integer(int64) :: n, pos, pos_byte, val, begv_byte
    integer :: k
    np = 0
    call compile_pattern(string, fold, posix, k, status)
    if (.not. status%ok()) return
    subj = subject_from_buffer()
    begv_byte = buffers(cur)%begv_byte
    n = n_in
    pos = pos_in
    pos_byte = pos_byte_in
    do while (n < 0)
        ! Don't allow match past current point.
        val = re_search(searchbufs(k)%buf, subj, pos_byte - begv_byte, lim_byte - pos_byte, regs, &
                        pos_byte - begv_byte)
        if (val == -2) then
            call matcher_overflow(status)
            return
        end if
        if (val < 0) then
            np = n
            return
        end if
        pos_byte = regs%starts(0) + begv_byte
        call set_regs_from_buffer(regs)
        ! Set pos to the new position.
        pos = search_regs_start(0)
        n = n + 1
    end do
    do while (n > 0)
        val = re_search(searchbufs(k)%buf, subj, pos_byte - begv_byte, lim_byte - pos_byte, regs, &
                        lim_byte - begv_byte)
        if (val == -2) then
            call matcher_overflow(status)
            return
        end if
        if (val < 0) then
            np = -n
            return
        end if
        pos_byte = regs%ends(0) + begv_byte
        call set_regs_from_buffer(regs)
        pos = search_regs_end(0)
        n = n - 1
    end do
    np = pos
    end procedure search_buffer_re

    ! ------------------------------------------------------------------
    ! Replacing the match
    ! ------------------------------------------------------------------

    module procedure replace_match
    integer, parameter :: NOCHANGE = 0, ALL_CAPS = 1, CAP_INITIAL = 2
    integer :: case_action, sub, num_regs, idx, k
    integer(int64) :: sub_start, sub_end, pos, last, opoint, newpoint, nchars, inschars
    integer(int32) :: c, prevc, clen
    logical :: is_literal, some_multiletter_word, some_lowercase, some_uppercase, some_nonuppercase_initial
    character(len=:), allocatable :: text, piece, before, after, buftext
    integer(int64) :: i
    status = status_type()
    ! Most replacement texts don't contain any backslash directives in
    ! the replacements.  Check whether that's the case, which will enable
    ! us to take the fast path later.
    is_literal = .false.
    if (present(literal)) is_literal = literal
    if (.not. is_literal .and. index(newtext, "\") == 0) is_literal = .true.
    case_action = NOCHANGE
    if (.not. allocated(search_regs_start)) then
        call set_error(status, FM_GENERIC_ERROR, format_message("`replace_match' called before any match found"))
        return
    end if
    num_regs = size(search_regs_start)
    sub = 0
    if (present(subexp)) sub = subexp
    if (sub < 0 .or. sub >= num_regs) then
        call set_error(status, FM_ARGS_OUT_OF_RANGE)
        return
    end if
    sub_start = search_regs_start(sub)
    sub_end = search_regs_end(sub)
    ! Check whether the text to replace is present in the buffer/string.
    if (present(string)) then
        nchars = string_chars(string)
        if (.not. (1 <= sub_start .and. sub_end <= nchars + 1)) call missing()
    else
        if (.not. (buffers(cur)%begv <= sub_start .and. sub_end <= buffers(cur)%zv)) call missing()
    end if
    if (.not. status%ok()) return
    if (.not. present_true(fixedcase)) then
        ! Decide how to casify by examining the matched text.
        if (present(string)) then
            buftext = string(string_index_to_offset(string, sub_start) + 1:string_index_to_offset(string, sub_end))
        else
            call buffer_substring(sub_start, sub_end, buftext, status)
        end if
        prevc = 10
        ! some_multiletter_word is set nonzero if any original word is
        ! more than one letter long.
        some_multiletter_word = .false.
        some_lowercase = .false.
        some_nonuppercase_initial = .false.
        some_uppercase = .false.
        i = 1
        do while (i <= len(buftext, kind=int64))
            call string_char_and_length(buftext, i, c, clen)
            i = i + clen
            if (lowercasep(c)) then
                ! Cannot be all caps if any original char is lower case.
                some_lowercase = .true.
                if (.not. word_or_symbol(prevc)) then
                    some_nonuppercase_initial = .true.
                else
                    some_multiletter_word = .true.
                end if
            else if (uppercasep(c)) then
                some_uppercase = .true.
                if (word_or_symbol(prevc)) some_multiletter_word = .true.
            else
                ! If the initial is a caseless word constituent, treat that
                ! like a lowercase initial.
                if (.not. word_or_symbol(prevc)) some_nonuppercase_initial = .true.
            end if
            prevc = c
        end do
        ! Convert to all caps if the old text is all caps and has at least
        ! one multiletter word.
        if (.not. some_lowercase .and. some_multiletter_word) then
            case_action = ALL_CAPS
        else if (.not. some_nonuppercase_initial .and. some_multiletter_word) then
            ! Capitalize each word, if the old text has all capitalized
            ! words.
            case_action = CAP_INITIAL
        else if (.not. some_nonuppercase_initial .and. some_uppercase) then
            ! Should x -> yz, operating on X, give Yz or YZ?  We'll assume
            ! the latter.
            case_action = ALL_CAPS
        else
            case_action = NOCHANGE
        end if
    end if
    ! Do replacement in a string.
    if (present(string)) then
        before = string(1:string_index_to_offset(string, sub_start))
        after = string(string_index_to_offset(string, sub_end) + 1:)
        text = newtext
        ! Substitute parts of the match into NEWTEXT if desired.
        if (.not. is_literal) then
            text = ""
            i = 1
            do while (i <= len(newtext, kind=int64))
                call string_char_and_length(newtext, i, c, clen)
                piece = newtext(i:i + clen - 1)
                i = i + clen
                if (c == iachar("\") .and. i <= len(newtext, kind=int64)) then
                    call string_char_and_length(newtext, i, c, clen)
                    i = i + clen
                    if (c == iachar("&")) then
                        piece = substring(string, sub_start, sub_end)
                    else if (c >= iachar("1") .and. c <= iachar("9")) then
                        k = c - iachar("0")
                        piece = ""
                        if (k < num_regs) then
                            if (search_regs_start(k) >= 0) piece = substring(string, search_regs_start(k), &
                                                                             search_regs_end(k))
                        end if
                    else if (c == iachar("\")) then
                        piece = "\"
                    else if (c == iachar("?")) then
                        piece = "\?"
                    else
                        call set_error(status, FM_GENERIC_ERROR, format_message("Invalid use of `\' in replacement text"))
                        return
                    end if
                end if
                text = text//piece
            end do
        end if
        ! Do case substitution in NEWTEXT if desired.
        if (case_action == ALL_CAPS) then
            text = upcase_string(text)
        else if (case_action == CAP_INITIAL) then
            text = upcase_initials_string(text)
        end if
        if (present(result)) result = before//text//after
        return
    end if
    ! Record point.  A nonpositive OPOINT is actually an offset from ZV.
    associate (b => buffers(cur))
        if (b%pt <= sub_start) then
            opoint = b%pt
        else
            opoint = max(b%pt, sub_end) - b%zv
        end if
    end associate
    ! If we want non-literal replacement, perform substitution on the
    ! replacement string.
    text = newtext
    if (.not. is_literal) then
        text = ""
        i = 1
        do while (i <= len(newtext, kind=int64))
            call string_char_and_length(newtext, i, c, clen)
            piece = newtext(i:i + clen - 1)
            i = i + clen
            idx = -1
            if (c == iachar("\")) then
                c = 0
                if (i <= len(newtext, kind=int64)) then
                    call string_char_and_length(newtext, i, c, clen)
                    i = i + clen
                end if
                piece = ""
                if (c == iachar("&")) then
                    idx = sub
                else if (iachar("1") <= c .and. c <= iachar("9") .and. c - iachar("0") < num_regs) then
                    if (search_regs_start(c - iachar("0")) >= 1) idx = c - iachar("0")
                else if (c == iachar("\")) then
                    piece = "\"
                else
                    call set_error(status, FM_GENERIC_ERROR, format_message("Invalid use of `\' in replacement text"))
                    return
                end if
            end if
            ! If we want to copy part of a previous match, use that text.
            if (idx >= 0) call buffer_substring(search_regs_start(idx), search_regs_end(idx), piece, status)
            text = text//piece
        end do
    end if
    inschars = string_chars(text)
    newpoint = sub_start + inschars
    ! Replace the old text with the new in the cleanest possible way.
    call replace_range(sub_start, sub_end, text, .true., status)
    if (.not. status%ok()) return
    call update_search_regs(sub_start, sub_end, sub_start + inschars)
    if (case_action == ALL_CAPS) then
        call upcase_region(search_regs_start(sub), newpoint, status)
    else if (case_action == CAP_INITIAL) then
        call upcase_initials_region(search_regs_start(sub), newpoint, status)
    end if
    ! Put point back where it was in the text, if possible.
    associate (b => buffers(cur))
        if (opoint <= 0) opoint = opoint + b%zv
        opoint = max(b%begv, min(opoint, b%zv))
        call temp_set_point_both(b, opoint, char_to_byte(opoint))
    end associate
    ! Now move point "officially" to the end of the inserted replacement.
    call goto_char(newpoint)
contains

    subroutine missing()
        if (sub_start < 0) then
            call set_error(status, FM_GENERIC_ERROR, "replace-match subexpression does not exist")
        else
            call set_error(status, FM_ARGS_OUT_OF_RANGE)
        end if
    end subroutine missing

    logical function present_true(flag)
        logical, intent(in), optional :: flag
        present_true = .false.
        if (present(flag)) present_true = flag
    end function present_true

    !> SYNTAX (prevc) is a word (or a symbol, with case-symbols-as-words).
    logical function word_or_symbol(ch)
        integer(int32), intent(in) :: ch
        integer :: syn
        syn = char_syntax_class(ch)
        word_or_symbol = syn == SWORD .or. (case_symbols_as_words .and. syn == SSYMBOL)
    end function word_or_symbol

    !> Characters B to E (exclusive, 1-based) of STR.
    function substring(str, b, e) result(sub_text)
        character(len=*), intent(in) :: str
        integer(int64), intent(in) :: b, e
        character(len=:), allocatable :: sub_text
        sub_text = str(string_index_to_offset(str, b) + 1:string_index_to_offset(str, e))
    end function substring
    end procedure replace_match

    !> Adjust the match data for replacing OLDSTART..OLDEND with text that
    !> ends at NEWEND.  Port of search.c:update_search_regs.
    subroutine update_search_regs(oldstart, oldend, newend)
        integer(int64), intent(in) :: oldstart, oldend, newend
        integer(int64) :: change
        integer :: i
        change = newend - oldend
        do i = 0, size(search_regs_start) - 1
            ! If the subgroup that `replace_match' is modifying encloses
            ! the subgroup I, then its start position should stay
            ! unchanged.
            if (search_regs_start(i) <= oldstart) then
                continue
            else if (search_regs_start(i) >= oldend) then
                search_regs_start(i) = search_regs_start(i) + change
            else
                search_regs_start(i) = oldstart
            end if
            if (search_regs_end(i) >= oldend) then
                search_regs_end(i) = search_regs_end(i) + change
            else if (search_regs_end(i) > oldstart) then
                search_regs_end(i) = oldstart
            end if
        end do
    end subroutine update_search_regs

    ! ------------------------------------------------------------------
    ! Commands
    ! ------------------------------------------------------------------

    !> The search commands read their argument in the minibuffer
    !> (interactive "MSearch: " and "sRE search: ").
    subroutine search_cmd(prompt, re, posix, direction, status)
        character(len=*), intent(in) :: prompt
        logical, intent(in) :: re, posix
        integer(int64), intent(in) :: direction
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: string
        integer(int64) :: found
        call read_string(prompt, string, status)
        if (.not. status%ok()) return
        call search_command(string, direction, re, posix, found, status)
    end subroutine search_cmd

    subroutine cmd_search_forward(status)
        type(status_type), intent(out) :: status
        call search_cmd("Search: ", .false., .false., 1_int64, status)
    end subroutine cmd_search_forward

    subroutine cmd_search_backward(status)
        type(status_type), intent(out) :: status
        call search_cmd("Search backward: ", .false., .false., -1_int64, status)
    end subroutine cmd_search_backward

    subroutine cmd_re_search_forward(status)
        type(status_type), intent(out) :: status
        call search_cmd("RE search: ", .true., .false., 1_int64, status)
    end subroutine cmd_re_search_forward

    subroutine cmd_re_search_backward(status)
        type(status_type), intent(out) :: status
        call search_cmd("RE search backward: ", .true., .false., -1_int64, status)
    end subroutine cmd_re_search_backward

    subroutine cmd_posix_search_forward(status)
        type(status_type), intent(out) :: status
        call search_cmd("Posix search: ", .true., .true., 1_int64, status)
    end subroutine cmd_posix_search_forward

    subroutine cmd_posix_search_backward(status)
        type(status_type), intent(out) :: status
        call search_cmd("Posix search backward: ", .true., .true., -1_int64, status)
    end subroutine cmd_posix_search_backward

    module procedure syms_of_search
    commands%search_forward%run => cmd_search_forward
    commands%search_backward%run => cmd_search_backward
    commands%re_search_forward%run => cmd_re_search_forward
    commands%re_search_backward%run => cmd_re_search_backward
    commands%posix_search_forward%run => cmd_posix_search_forward
    commands%posix_search_backward%run => cmd_posix_search_backward
    end procedure syms_of_search
end submodule search_regexp_fm
