! search_regular_expression_mod.f90 -- regexp search (the part of search.c that uses
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
!> search in buffers.  A submodule of search_mod, because the regexp engine
!> depends (through the syntax table) on modules that use search_mod.
submodule(search_mod) search_regular_expression_mod
    use commands_mod, only: commands
    use window_mod, only: same_handle
    use regular_expression_mod, only: regular_expression_pattern_buffer_type, regular_expression_registers_type, &
                                      regular_expression_subject_type, regular_expression_compile_pattern, &
                                      regular_expression_search, regular_expression_match, string_to_subject, buffer_to_subject
    use syntax_mod, only: syntax_table, syntax_modiff
    use minibuffer_reading_mod, only: read_string
    use casefiddle_mod, only: upcase_string, upcase_initials_string, upcase_region, upcase_initials_region, &
                              case_identifiers_as_words
    use case_table_mod, only: is_lowercase, is_uppercase
    use syntax_mod, only: character_syntax_class, SWORD, SSYMBOL
    use insert_delete_mod, only: replace_range
    use editing_functions_mod, only: buffer_substring, goto_character
    use buffer_mod, only: temporary_set_point_both
    use error_mod, only: format_message, ARGUMENTS_OUT_OF_RANGE_CODE
    implicit none(type, external)

    !> Number of compiled patterns kept (REGULAR_EXPRESSION_CACHE_SIZE).
    integer, parameter :: REGULAR_EXPRESSION_CACHE_SIZE = 20

    !> A compiled pattern and what it was compiled for (struct
    !> regular_expression_cache).
    type :: regular_expression_cache_type
        logical :: valid = .false.
        character(len=:), allocatable :: regular_expression
        logical :: posix = .false.
        logical :: has_whitespace_regular_expression = .false.
        character(len=:), allocatable :: whitespace_regular_expression
        type(handle_type) :: syntax_table
        !> `syntax_modiff' when compiled: clear_regular_expression_cache drops entries
        !> that used a syntax table when any syntax entry changes.
        integer(int64) :: syntax_modiff = 0
        integer(int64) :: used_at = 0
        !> In use by a match in progress, which a nested search (from
        !> syntax-propertize) must not recompile.
        logical :: busy = .false.
        type(regular_expression_pattern_buffer_type) :: buffer
    end type regular_expression_cache_type

    type(regular_expression_cache_type), save :: searchbufs(REGULAR_EXPRESSION_CACHE_SIZE)
    integer(int64) :: clock = 0

contains

    !> Compile PATTERN for case folding `translate' and Posix backtracking
    !> `posix', or find it already compiled, and return its index in the
    !> cache, moved to the front as most recently used.  Port of
    !> search.c:compile_pattern and search.c:compile_pattern_1.
    subroutine compile_pattern(pattern, translate, posix, k, status)
        character(len=*), intent(in) :: pattern
        logical, intent(in) :: translate, posix
        integer, intent(out) :: k
        type(status_type), intent(out) :: status
        integer :: i
        logical :: has_whitespace

        status = status_type()
        has_whitespace = allocated(search_spaces_regular_expression)
        clock = clock + 1

        do i = 1, REGULAR_EXPRESSION_CACHE_SIZE
            associate (cp => searchbufs(i))
                if (.not. cp%valid) cycle
                if (cp%busy) cycle
                if (cp%regular_expression /= pattern .or. len(cp%regular_expression) /= len(pattern)) cycle
                if ((cp%buffer%translate .neqv. translate) .or. (cp%posix .neqv. posix)) cycle

                if (cp%buffer%used_syntax) then
                    if (.not. same_handle(cp%syntax_table, syntax_table())) cycle
                    if (cp%syntax_modiff /= syntax_modiff) cycle
                end if

                if (cp%has_whitespace_regular_expression .neqv. has_whitespace) cycle

                if (has_whitespace) then
                    if (cp%whitespace_regular_expression /= search_spaces_regular_expression .or. &
                        len(cp%whitespace_regular_expression) /= len(search_spaces_regular_expression)) cycle
                end if

                cp%used_at = clock
                k = i
                return
            end associate
        end do

        ! Compile into the least recently used entry that is not busy.
        k = minloc(searchbufs%used_at, 1, mask=.not. searchbufs%busy)

        if (k == 0) then
            call set_error(status, GENERIC_ERROR_CODE, "Too much matching reentrancy")
            return
        end if

        associate (cp => searchbufs(k))
            cp%valid = .false.
            cp%used_at = clock
            cp%buffer%translate = translate
            cp%posix = posix
            cp%has_whitespace_regular_expression = has_whitespace

            if (has_whitespace) then
                cp%whitespace_regular_expression = search_spaces_regular_expression
                call regular_expression_compile_pattern(pattern, cp%buffer, status, posix, search_spaces_regular_expression)
            else
                call regular_expression_compile_pattern(pattern, cp%buffer, status, posix)
            end if

            ! If the compiled pattern hard codes some of the contents of the
            ! syntax-table, it can only be reused with *this* syntax table.
            cp%syntax_table = syntax_table()
            cp%syntax_modiff = syntax_modiff
            if (.not. status%ok()) return
            cp%regular_expression = pattern
            cp%valid = .true.
        end associate
    end subroutine compile_pattern

    !> Mark cache entry K busy while a match uses it, so that a nested
    !> search compiles elsewhere.  Port of search.c:freeze_pattern (the
    !> busy flag: unfreeze_pattern clears it).
    subroutine freeze_pattern(k)
        integer, intent(in) :: k

        searchbufs(k)%busy = .true.
    end subroutine freeze_pattern

    !> Port of search.c:unfreeze_pattern.
    subroutine unfreeze_pattern(k)
        integer, intent(in) :: k

        searchbufs(k)%busy = .false.
    end subroutine unfreeze_pattern

    !> Make the match data from `regs', offsets in the current buffer's
    !> accessible portion.  Not a port.
    subroutine set_regs_from_buffer(regs)
        type(regular_expression_registers_type), intent(in) :: regs
        integer :: i

        call ensure_search_regs(size(regs%starts))

        do i = 0, size(regs%starts) - 1
            if (regs%starts(i) >= 0) then
                search_regs_start(i) = byte_to_character(regs%starts(i) + buffers(current)%beginning_of_accessible_portion_byte)
                search_regs_end(i) = byte_to_character(regs%ends(i) + buffers(current)%beginning_of_accessible_portion_byte)
            end if
        end do

        last_thing_searched = current_buffer()
        last_search_in_string = .false.
    end subroutine set_regs_from_buffer

    !> Make the match data from `regs', offsets in STRING.  Not a port.
    subroutine set_regs_from_string(regs, string)
        type(regular_expression_registers_type), intent(in) :: regs
        character(len=*), intent(in) :: string
        integer :: i

        call ensure_search_regs(size(regs%starts))

        do i = 0, size(regs%starts) - 1
            if (regs%starts(i) >= 0) then
                search_regs_start(i) = string_offset_to_index(string, regs%starts(i))
                search_regs_end(i) = string_offset_to_index(string, regs%ends(i))
            end if
        end do

        last_search_in_string = .true.
    end subroutine set_regs_from_string

    !> Port of search.c:looking_at_1.
    module procedure looking_at_1
    type(regular_expression_subject_type) :: subj
    type(regular_expression_registers_type) :: regs
    integer(int64) :: i
    integer :: k

    matched = .false.
    if (running_asynchronous_code) call save_search_regs()
    call compile_pattern(regular_expression, case_fold_search%value, posix, k, status)
    if (.not. status%ok()) return
    subj = buffer_to_subject()

    associate (b => buffers(current))
        call freeze_pattern(k)
        i = regular_expression_match(searchbufs(k)%buffer, subj, b%point_byte - b%beginning_of_accessible_portion_byte, regs, &
                                     b%end_of_accessible_portion_byte - b%beginning_of_accessible_portion_byte)
        call unfreeze_pattern(k)
    end associate

    if (i == -2) then
        call matcher_overflow(status)
        return
    end if

    matched = i >= 0
    if (modify_data .and. matched) call set_regs_from_buffer(regs)
    end procedure looking_at_1

    !> Port of search.c:fast_string_match_internal.
    module procedure fast_string_match
    type(regular_expression_subject_type) :: subj
    integer(int64) :: value
    integer :: k

    index = 0
    call compile_pattern(regular_expression, fold, .false., k, status)
    if (.not. status%ok()) return
    subj = string_to_subject(string)
    call freeze_pattern(k)
    value = regular_expression_search(searchbufs(k)%buffer, subj, 0_int64, len(string, kind=int64), stop=len(string, kind=int64))
    call unfreeze_pattern(k)

    if (value == -2) then
        call matcher_overflow(status)
        return
    end if

    if (value >= 0) index = string_offset_to_index(string, value)
    end procedure fast_string_match

    !> Port of search.c:fast_looking_at.
    module procedure fast_looking_at
    type(regular_expression_subject_type) :: subj
    integer(int64) :: position_byte, limit_byte
    integer :: k

    length = -1
    call compile_pattern(regular_expression, .false., .false., k, status)
    if (.not. status%ok()) return

    if (present(string)) then
        position_byte = string_index_to_offset(string, position)
        limit_byte = string_index_to_offset(string, limit)
        subj = string_to_subject(string)
    else
        position_byte = character_to_byte(position) - buffers(current)%beginning_of_accessible_portion_byte
        limit_byte = character_to_byte(limit) - buffers(current)%beginning_of_accessible_portion_byte
        subj = buffer_to_subject()
    end if

    call freeze_pattern(k)
    length = regular_expression_match(searchbufs(k)%buffer, subj, position_byte, stop=limit_byte)
    call unfreeze_pattern(k)
    if (length == -2) call matcher_overflow(status)
    end procedure fast_looking_at

    !> Port of search.c:string_match_1.
    module procedure string_match_1
    type(regular_expression_subject_type) :: subj
    type(regular_expression_registers_type) :: regs
    integer(int64) :: position, position_byte, nchars, value
    integer :: k
    character(len=21) :: buffer

    index = 0
    status = status_type()
    if (running_asynchronous_code) call save_search_regs()
    position = 1

    if (present(start)) then
        nchars = string_characters(string)
        position = start

        if (position < 0 .and. -position <= nchars) then
            position = nchars + position + 1
        else if (position < 1 .or. position > nchars + 1) then
            ! arguments_out_of_range (string, start).
            write (buffer, "(i0)") start
            call set_error(status, ARGUMENTS_OUT_OF_RANGE_CODE, fortran_string(string)//", "//trim(buffer))
            return
        end if
    end if

    position_byte = string_index_to_offset(string, position)
    call compile_pattern(regular_expression, case_fold_search%value, posix, k, status)
    if (.not. status%ok()) return
    subj = string_to_subject(string)
    call freeze_pattern(k)
    value = regular_expression_search(searchbufs(k)%buffer, subj, position_byte, len(string, kind=int64) - position_byte, regs, &
                                      len(string, kind=int64))
    call unfreeze_pattern(k)

    if (value == -2) then
        call matcher_overflow(status)
        return
    end if

    if (value < 0) return
    if (modify_data) call set_regs_from_string(regs, string)
    index = string_offset_to_index(string, value)
    end procedure string_match_1

    !> Regexp search for `search_buffer'.  Port of
    !> search.c:search_buffer_re.
    module procedure search_buffer_regular_expression
    type(regular_expression_subject_type) :: subj
    type(regular_expression_registers_type) :: regs
    integer(int64) :: n, position, position_byte, value, beginning_of_accessible_portion_byte
    integer :: k

    np = 0
    call compile_pattern(string, fold, posix, k, status)
    if (.not. status%ok()) return
    subj = buffer_to_subject()
    beginning_of_accessible_portion_byte = buffers(current)%beginning_of_accessible_portion_byte
    n = n_in
    position = position_in
    position_byte = position_byte_in

    do while (n < 0)
        ! Don't allow match past current point.
        call freeze_pattern(k)
        value = regular_expression_search(searchbufs(k)%buffer, subj, position_byte - beginning_of_accessible_portion_byte, &
                                          limit_byte - position_byte, regs, &
                                          position_byte - beginning_of_accessible_portion_byte)
        call unfreeze_pattern(k)

        if (value == -2) then
            call matcher_overflow(status)
            return
        end if

        if (value < 0) then
            np = n
            return
        end if

        position_byte = regs%starts(0) + beginning_of_accessible_portion_byte
        call set_regs_from_buffer(regs)
        ! Set pos to the new position.
        position = search_regs_start(0)
        n = n + 1
    end do

    do while (n > 0)
        call freeze_pattern(k)
        value = regular_expression_search(searchbufs(k)%buffer, subj, position_byte - beginning_of_accessible_portion_byte, &
                                          limit_byte - position_byte, regs, &
                                          limit_byte - beginning_of_accessible_portion_byte)
        call unfreeze_pattern(k)

        if (value == -2) then
            call matcher_overflow(status)
            return
        end if

        if (value < 0) then
            np = -n
            return
        end if

        position_byte = regs%ends(0) + beginning_of_accessible_portion_byte
        call set_regs_from_buffer(regs)
        position = search_regs_end(0)
        n = n - 1
    end do

    np = position
    end procedure search_buffer_regular_expression

    ! ------------------------------------------------------------------
    ! Replacing the match
    ! ------------------------------------------------------------------

    module procedure replace_match
    integer, parameter :: NOCHANGE = 0, ALL_CAPS = 1, CAP_INITIAL = 2
    integer :: case_action, sub, number_regs, position, k
    integer(int64) :: sub_start, sub_end, original_point, newpoint, nchars, inschars
    integer(int32) :: c, prevc, clen
    logical :: is_literal, some_multiletter_word, some_lowercase, some_uppercase, some_nonuppercase_initial
    character(len=:), allocatable :: text, piece, before, after, buftext
    character(len=80) :: message_text
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
        call set_error(status, GENERIC_ERROR_CODE, format_message("`replace_match' called before any match found"))
        return
    end if

    number_regs = size(search_regs_start)
    sub = 0
    if (present(subexp)) sub = subexp

    if (sub < 0 .or. sub >= number_regs) then
        ! check_integer_range (subexp, 0, number_regs - 1).
        write (message_text, '(i0, ", 0, ", i0)') sub, number_regs - 1
        call set_error(status, ARGUMENTS_OUT_OF_RANGE_CODE, trim(message_text))
        return
    end if

    sub_start = search_regs_start(sub)
    sub_end = search_regs_end(sub)

    ! Check whether the text to replace is present in the buffer/string.
    if (present(string)) then
        nchars = string_characters(string)
        if (.not. (1 <= sub_start .and. sub_end <= nchars + 1)) call missing()
    else
        if (.not. (buffers(current)%beginning_of_accessible_portion <= sub_start .and. sub_end <= buffers( &
                   current)%end_of_accessible_portion)) call missing()
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
            call string_character_and_length(buftext, i, c, clen)
            i = i + clen

            if (is_lowercase(c)) then
                ! Cannot be all caps if any original char is lower case.
                some_lowercase = .true.

                if (.not. word_or_identifier(prevc)) then
                    some_nonuppercase_initial = .true.
                else
                    some_multiletter_word = .true.
                end if
            else if (is_uppercase(c)) then
                some_uppercase = .true.
                if (word_or_identifier(prevc)) some_multiletter_word = .true.
            else
                ! If the initial is a caseless word constituent, treat that
                ! like a lowercase initial.
                if (.not. word_or_identifier(prevc)) some_nonuppercase_initial = .true.
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
                call string_character_and_length(newtext, i, c, clen)
                piece = newtext(i:i + clen - 1)
                i = i + clen

                if (c == iachar("\")) then
                    ! A trailing backslash reads the string's terminating
                    ! null upstream, which is invalid.
                    c = 0

                    if (i <= len(newtext, kind=int64)) then
                        call string_character_and_length(newtext, i, c, clen)
                        i = i + clen
                    end if

                    if (c == iachar("&")) then
                        piece = substring(string, sub_start, sub_end)
                    else if (c >= iachar("1") .and. c <= iachar("9")) then
                        k = c - iachar("0")
                        piece = ""

                        if (k < number_regs) then
                            if (search_regs_start(k) >= 0) piece = substring(string, search_regs_start(k), &
                                                                             search_regs_end(k))
                        end if
                    else if (c == iachar("\")) then
                        piece = "\"
                    else if (c == iachar("?")) then
                        piece = "\?"
                    else
                        call set_error(status, GENERIC_ERROR_CODE, format_message("Invalid use of `\' in replacement text"))
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

    ! Record point.  A nonpositive ORIGINAL_POINT is actually an offset from END_OF_ACCESSIBLE_PORTION.
    associate (b => buffers(current))
        if (b%point <= sub_start) then
            original_point = b%point
        else
            original_point = max(b%point, sub_end) - b%end_of_accessible_portion
        end if
    end associate

    ! If we want non-literal replacement, perform substitution on the
    ! replacement string.
    text = newtext

    if (.not. is_literal) then
        text = ""
        i = 1

        do while (i <= len(newtext, kind=int64))
            call string_character_and_length(newtext, i, c, clen)
            piece = newtext(i:i + clen - 1)
            i = i + clen
            position = -1

            if (c == iachar("\")) then
                c = 0

                if (i <= len(newtext, kind=int64)) then
                    call string_character_and_length(newtext, i, c, clen)
                    i = i + clen
                end if

                piece = ""

                if (c == iachar("&")) then
                    position = sub
                else if (iachar("1") <= c .and. c <= iachar("9") .and. c - iachar("0") < number_regs) then
                    if (search_regs_start(c - iachar("0")) >= 1) position = c - iachar("0")
                else if (c == iachar("\")) then
                    piece = "\"
                else
                    call set_error(status, GENERIC_ERROR_CODE, format_message("Invalid use of `\' in replacement text"))
                    return
                end if
            end if

            ! If we want to copy part of a previous match, use that text.
            if (position >= 0) call buffer_substring(search_regs_start(position), search_regs_end(position), piece, status)
            text = text//piece
        end do
    end if

    inschars = string_characters(text)
    newpoint = sub_start + inschars
    ! Replace the old text with the new in the cleanest possible way.
    call replace_range(sub_start, sub_end, text, .true., status, adjust_match_data=.true.)
    if (.not. status%ok()) return

    if (case_action == ALL_CAPS) then
        call upcase_region(search_regs_start(sub), newpoint, status)
    else if (case_action == CAP_INITIAL) then
        call upcase_initials_region(search_regs_start(sub), newpoint, status)
    end if

    ! Put point back where it was in the text, if possible.
    associate (b => buffers(current))
        if (original_point <= 0) original_point = original_point + b%end_of_accessible_portion
        original_point = max(b%beginning_of_accessible_portion, min(original_point, b%end_of_accessible_portion))
        call temporary_set_point_both(b, original_point, character_to_byte(original_point))
    end associate

    ! Now move point "officially" to the end of the inserted replacement.
    call goto_character(newpoint)

contains

    !> (error "replace-match subexpression does not exist" SUBEXP), or
    !> args-out-of-range on the match bounds.
    subroutine missing()
        if (sub_start < 0) then
            if (present(subexp)) then
                write (message_text, '("replace_match subexpression does not exist: ", i0)') subexp
                call set_error(status, GENERIC_ERROR_CODE, trim(message_text))
            else
                call set_error(status, GENERIC_ERROR_CODE, "replace_match subexpression does not exist: .false.")
            end if
        else
            write (message_text, '(i0, ", ", i0)') sub_start, sub_end
            call set_error(status, ARGUMENTS_OUT_OF_RANGE_CODE, trim(message_text))
        end if
    end subroutine missing

    logical function present_true(flag)
        logical, intent(in), optional :: flag

        present_true = .false.
        if (present(flag)) present_true = flag
    end function present_true

    !> SYNTAX (prevc) is a word (or an identifier, with case-symbols-as-words).
    logical function word_or_identifier(ch)
        integer(int32), intent(in) :: ch
        integer :: syntax

        syntax = character_syntax_class(ch)
        word_or_identifier = syntax == SWORD .or. (case_identifiers_as_words%value .and. syntax == SSYMBOL)
    end function word_or_identifier

    !> Characters B to E (exclusive, 1-based) of STRING.
    function substring(replacement_text, b, e) result(sub_text)
        character(len=*), intent(in) :: replacement_text
        integer(int64), intent(in) :: b, e
        character(len=:), allocatable :: sub_text

        sub_text = replacement_text(string_index_to_offset(replacement_text, b) + 1:string_index_to_offset(replacement_text, e))
    end function substring
    end procedure replace_match

    ! ------------------------------------------------------------------
    ! Commands
    ! ------------------------------------------------------------------

    !> The search commands read their argument in the minibuffer
    !> (interactive "MSearch: " and "sRE search: ").  Not a port.
    subroutine search_with_prompt(prompt, regular_expression, posix, direction, status)
        character(len=*), intent(in) :: prompt
        logical, intent(in) :: regular_expression, posix
        integer(int64), intent(in) :: direction
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: string
        integer(int64) :: found

        call read_string(prompt, string, status)
        if (.not. status%ok()) return
        call search_command(string, direction, regular_expression, posix, found, status)
    end subroutine search_with_prompt

    subroutine execute_search_forward(status)
        type(status_type), intent(out) :: status

        call search_with_prompt("Search: ", .false., .false., 1_int64, status)
    end subroutine execute_search_forward

    subroutine execute_search_backward(status)
        type(status_type), intent(out) :: status

        call search_with_prompt("Search backward: ", .false., .false., -1_int64, status)
    end subroutine execute_search_backward

    subroutine execute_regular_expression_search_forward(status)
        type(status_type), intent(out) :: status

        call search_with_prompt("RE search: ", .true., .false., 1_int64, status)
    end subroutine execute_regular_expression_search_forward

    subroutine execute_regular_expression_search_backward(status)
        type(status_type), intent(out) :: status

        call search_with_prompt("RE search backward: ", .true., .false., -1_int64, status)
    end subroutine execute_regular_expression_search_backward

    subroutine execute_posix_search_forward(status)
        type(status_type), intent(out) :: status

        call search_with_prompt("Posix search: ", .true., .true., 1_int64, status)
    end subroutine execute_posix_search_forward

    subroutine execute_posix_search_backward(status)
        type(status_type), intent(out) :: status

        call search_with_prompt("Posix search backward: ", .true., .true., -1_int64, status)
    end subroutine execute_posix_search_backward

    module procedure initialize_search
    commands%search_forward%execute => execute_search_forward
    commands%search_backward%execute => execute_search_backward
    commands%regular_expression_search_forward%execute => execute_regular_expression_search_forward
    commands%regular_expression_search_backward%execute => execute_regular_expression_search_backward
    commands%posix_search_forward%execute => execute_posix_search_forward
    commands%posix_search_backward%execute => execute_posix_search_backward
    end procedure initialize_search
end submodule search_regular_expression_mod
