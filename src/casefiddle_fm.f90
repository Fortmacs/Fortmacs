! casefiddle_fm.f90 -- case conversion of characters, strings and regions.
! Ported from GNU Emacs 31.1 src/casefiddle.c.
!
! Copyright (C) 1985, 1994, 1997-1999, 2001-2026 Free Software Foundation,
! Inc.
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

!> Upcasing, downcasing, capitalizing and upcasing the initials of
!> characters, strings and buffer regions, with Unicode title case and the
!> unconditional special casings (uni_special_casing_fm: ß upcases to SS,
!> ﬁ to FI), and the Greek final sigma rule.  Words are runs of
!> word-constituent characters (and identifier constituents with
!> `case_identifiers_as_words') by the current syntax table.
!>
!> Strings are always multibyte here (casefiddle.c's unibyte string paths
!> are Not applicable); unibyte buffers are cased byte by byte, as
!> upstream.  The region commands work on each piece of a noncontiguous
!> region (`region_extract_function').  The word commands (`upcase_word'
!> and friends) are registered here; their keys come with the rest of the
!> case commands.  Deferred: compositions.
module casefiddle_fm
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use commands_fm, only: commands
    use error_fm, only: status_type
    use data_fm, only: logical_setting_type
    use character_fm, only: char_string, string_char_and_length, MAX_MULTIBYTE_LENGTH, CHAR_ALT, CHAR_SUPER, &
                            CHAR_HYPER, CHAR_SHIFT, CHAR_CTL, CHAR_META, unibyte_to_char, char_to_byte8
    use casetab_fm, only: downcase, upcase
    use syntax_fm, only: syntax_entry, char_syntax_class, scan_words, syntax_descriptor_type, SWORD, SSYMBOL
    use buffer_fm, only: buffers, current, buf_fetch_char, buf_next_char_len, buf_bytes, temp_set_point_both, &
                         buf_fetch_byte, buffer_byte_index
    use marker_fm, only: char_to_byte
    use insdel_fm, only: modify_text, replace_range_2, signal_after_change
    use undo_fm, only: record_insert, record_delete
    use editfns_fm, only: validate_region, point, goto_char, region_beginning, region_end
    use callint_fm, only: current_prefix_arg, prefix_numeric_value
    use uni_special_casing_fm, only: title_count, title_keys, title_values, special_count_1, special_keys_1, &
                                     special_lengths_1, special_values_1, special_count_2, special_keys_2, &
                                     special_lengths_2, special_values_2, special_count_3, special_keys_3, &
                                     special_lengths_3, special_values_3
    implicit none(type, external)
    private

    public :: upcase_string, downcase_string, capitalize_string, upcase_initials_string
    public :: upcase_char, downcase_char, capitalize_char, upcase_initials_char
    public :: upcase_region, downcase_region, capitalize_region, upcase_initials_region
    public :: upcase_word, downcase_word, capitalize_word, syms_of_casefiddle

    !> The case actions (enum case_action).
    integer, parameter, public :: CASE_UP = 0, CASE_DOWN = 1, CASE_CAPITALIZE = 2, CASE_CAPITALIZE_UP = 3

    !> If true, case functions treat identifier syntax as part of words.
    !>
    !> Functions such as `upcase_initials' and `replace_match' check or
    !> modify the case pattern of sequences of characters.  Normally, these
    !> operate on sequences of characters whose syntax is word constituent.
    !> If this variable is true, then they operate on sequences of
    !> characters whose syntax is either word constituent or identifier
    !> constituent.
    !>
    !> This is useful for programming languages and styles where only the
    !> first letter of an identifier's name is ever capitalized.  Port of
    !> casefiddle.c case-symbols-as-words (automatically buffer-local
    !> when set).
    type(logical_setting_type), public :: case_identifiers_as_words = &
                                          logical_setting_type(name="case_identifiers_as_words", value=.false.)

    !> In Greek, lower case sigma has two forms: one when used in the middle
    !> and one when used at the end of a word.
    integer(int32), parameter :: GREEK_CAPITAL_LETTER_SIGMA = int(z'03A3', int32)
    integer(int32), parameter :: GREEK_SMALL_LETTER_FINAL_SIGMA = int(z'03C2', int32)

    !> State for casing individual characters (struct casing_context_type).
    type :: casing_context_type
        !> User-requested action.
        integer :: flag = CASE_UP
        !> Whether the context operates on a buffer (syntax prefix flags
        !> count there).
        logical :: inbuffer = .false.
        !> Whether the context is within a word.
        logical :: inword = .false.
        !> What the last operation was.
        logical :: downcase_last = .false.
    end type casing_context_type

    !> The result of casing one character (struct casing_str_buf_type): up to
    !> three characters.
    type :: casing_str_buf_type
        integer(int32) :: chars(3) = 0
        integer :: len_chars = 0
    end type casing_str_buf_type

    abstract interface
        !> simple.el's `region_bounds': the region's pieces [STARTS(k),
        !> ENDS(k)).  Not a port.
        recursive subroutine region_bounds_procedure(starts, ends, status)
            import :: int64, status_type
            implicit none(type, external)
            integer(int64), allocatable, intent(out) :: starts(:), ends(:)
            type(status_type), intent(out) :: status
        end subroutine region_bounds_procedure

        !> simple.el's `is_region_noncontiguous'.  Not a port.
        recursive logical function region_noncontiguous_procedure()
            implicit none(type, external)
        end function region_noncontiguous_procedure
    end interface

    !> simple_fm's `region_bounds' (set by bindings_fm: simple_fm comes
    !> after this module).
    procedure(region_bounds_procedure), pointer, public :: region_bounds => null()

    !> simple_fm's `is_region_noncontiguous' (set by bindings_fm).
    procedure(region_noncontiguous_procedure), pointer, public :: is_region_noncontiguous => null()

contains

    !> Initialize CTX for casing characters.  Port of
    !> casefiddle.c:prepare_casing_context.
    function prepare_casing_context(flag, inbuffer) result(ctx)
        integer, intent(in) :: flag
        logical, intent(in) :: inbuffer
        type(casing_context_type) :: ctx

        ctx%flag = flag
        ctx%inbuffer = inbuffer
        ctx%inword = .false.
    end function prepare_casing_context

    !> Port of casefiddle.c:case_ch_is_word.
    logical function case_ch_is_word(syntax)
        integer, intent(in) :: syntax

        case_ch_is_word = syntax == SWORD .or. (case_identifiers_as_words%value .and. syntax == SSYMBOL)
    end function case_ch_is_word

    !> Whether C has the syntax prefix flag `p'.  Port of
    !> syntax.c:syntax_prefix_flag_p.
    logical function is_syntax_prefix_flag(c)
        integer(int32), intent(in) :: c
        type(syntax_descriptor_type) :: d

        d = syntax_entry(c)
        is_syntax_prefix_flag = btest(d%code, 20)
    end function is_syntax_prefix_flag

    !> The value for C in the sorted table KEYS/VALUES, or -1.  Not a port.
    pure integer(int32) function lookup(c, keys, values) result(v)
        integer(int32), intent(in) :: c
        integer(int32), intent(in) :: keys(:), values(:)
        integer :: lo, hi, mid

        v = -1
        lo = 1
        hi = size(keys)

        do while (lo <= hi)
            mid = (lo + hi)/2

            if (keys(mid) == c) then
                v = values(mid)
                return
            else if (keys(mid) < c) then
                lo = mid + 1
            else
                hi = mid - 1
            end if
        end do
    end function lookup

    !> The index of C in the sorted KEYS, or 0.  Not a port.
    pure integer function find_key(c, keys) result(k)
        integer(int32), intent(in) :: c
        integer(int32), intent(in) :: keys(:)
        integer :: lo, hi, mid

        k = 0
        lo = 1
        hi = size(keys)

        do while (lo <= hi)
            mid = (lo + hi)/2

            if (keys(mid) == c) then
                k = mid
                return
            else if (keys(mid) < c) then
                lo = mid + 1
            else
                hi = mid - 1
            end if
        end do
    end function find_key

    !> The special casing of CH for normalized FLAG into BUFFER, if there is
    !> one (CHAR_TABLE_REF of the special-*case table).  Not a port.
    logical function special_casing(flag, ch, buffer) result(found)
        integer, intent(in) :: flag
        integer(int32), intent(in) :: ch
        type(casing_str_buf_type), intent(inout) :: buffer
        integer :: k

        found = .false.

        select case (flag)
        case (CASE_UP)
            k = find_key(ch, special_keys_3)
            if (k == 0) return
            buffer%len_chars = special_lengths_3(k)
            buffer%chars = special_values_3(3*k - 2:3*k)
        case (CASE_DOWN)
            k = find_key(ch, special_keys_1)
            if (k == 0) return
            buffer%len_chars = special_lengths_1(k)
            buffer%chars = special_values_1(3*k - 2:3*k)
        case default
            k = find_key(ch, special_keys_2)
            if (k == 0) return
            buffer%len_chars = special_lengths_2(k)
            buffer%chars = special_values_2(3*k - 2:3*k)
        end select

        found = .true.
    end function special_casing

    !> Based on CTX, case character CH.  With BUFFER, save the result in it
    !> (using the special casings) and return whether the character has
    !> been changed (as `changed'); always return the cased character.
    !> Port of casefiddle.c:case_character_impl.
    integer(int32) function case_character_impl(ctx, ch, buffer, changed) result(cased)
        type(casing_context_type), intent(inout) :: ctx
        integer(int32), intent(in) :: ch
        type(casing_str_buf_type), intent(inout), optional :: buffer
        logical, intent(out), optional :: changed
        integer :: flag
        logical :: was_inword
        integer(int32) :: t

        ! Update inword state.
        was_inword = ctx%inword
        ctx%inword = case_ch_is_word(char_syntax_class(ch))
        if (ctx%inword .and. ctx%inbuffer .and. .not. was_inword) ctx%inword = .not. is_syntax_prefix_flag(ch)

        ! Normalize flag so its one of CASE_UP, CASE_DOWN or
        ! CASE_CAPITALIZE.
        if (ctx%flag == CASE_CAPITALIZE) then
            flag = merge(CASE_DOWN, CASE_CAPITALIZE, was_inword)
        else if (ctx%flag /= CASE_CAPITALIZE_UP) then
            flag = ctx%flag
        else if (.not. was_inword) then
            flag = CASE_CAPITALIZE
        else
            cased = ch
            call done()
            return
        end if

        ! Look through the special casing entries.
        if (present(buffer) .and. is_special_table(flag)) then
            if (special_casing(flag, ch, buffer)) then
                if (present(changed)) changed = .true.
                cased = ch
                return
            end if
        end if

        ! Handle simple, one-to-one case.
        if (flag == CASE_DOWN) then
            cased = downcase(ch)
            ctx%downcase_last = .true.
        else
            ctx%downcase_last = .false.
            cased = -1
            if (ctx%flag >= CASE_CAPITALIZE) cased = lookup(ch, title_keys, title_values)
            if (cased < 0) cased = upcase(ch)
        end if

        call done()

    contains

        !> Whether the special casing table for normalized FLAG is in use:
        !> upper case unless downcasing, lower case unless upcasing, title
        !> case when capitalizing.
        logical function is_special_table(normalized)
            integer, intent(in) :: normalized

            select case (normalized)
            case (CASE_UP)
                is_special_table = ctx%flag /= CASE_DOWN
            case (CASE_DOWN)
                is_special_table = ctx%flag /= CASE_UP
            case default
                is_special_table = ctx%flag >= CASE_CAPITALIZE
            end select
        end function is_special_table

        subroutine done()
            if (present(buffer)) then
                buffer%len_chars = 1
                buffer%chars = [cased, 0_int32, 0_int32]
            end if

            if (present(changed)) changed = cased /= ch
        end subroutine done
    end function case_character_impl

    !> Based on CTX, case character CH and return it.  Special casing rules
    !> (such as upcase(ﬁ) = FI) are not handled.  Port of
    !> casefiddle.c:case_single_character.
    integer(int32) function case_single_character(ctx, ch)
        type(casing_context_type), intent(inout) :: ctx
        integer(int32), intent(in) :: ch

        case_single_character = case_character_impl(ctx, ch)
    end function case_single_character

    !> Save in BUFFER the result of casing character CH and return whether
    !> casing changed it.  NEXT is the next character, or -1 if CH is the
    !> last one.  Like case_single_character but also handles one-to-many
    !> casing rules and the final sigma.  Port of
    !> casefiddle.c:case_character.
    logical function case_character(buffer, ctx, ch, next) result(changed)
        type(casing_str_buf_type), intent(inout) :: buffer
        type(casing_context_type), intent(inout) :: ctx
        integer(int32), intent(in) :: ch, next
        logical :: was_inword
        integer(int32) :: ignored

        was_inword = ctx%inword
        ignored = case_character_impl(ctx, ch, buffer, changed)

        ! If we have just down-cased a capital sigma and the next character
        ! no longer has a word syntax (i.e. current character is end of
        ! word), use final sigma.
        if (was_inword .and. ch == GREEK_CAPITAL_LETTER_SIGMA .and. changed) then
            if (next < 0) then
                buffer%chars = [GREEK_SMALL_LETTER_FINAL_SIGMA, 0_int32, 0_int32]
                buffer%len_chars = 1
            else if (.not. case_ch_is_word(char_syntax_class(next))) then
                buffer%chars = [GREEK_SMALL_LETTER_FINAL_SIGMA, 0_int32, 0_int32]
                buffer%len_chars = 1
            end if
        end if
    end function case_character

    !> The UTF-8 form of the characters in BUFFER.  Not a port.
    function buffer_text(buffer) result(s)
        type(casing_str_buf_type), intent(in) :: buffer
        character(len=:), allocatable :: s
        character(len=MAX_MULTIBYTE_LENGTH) :: tmp
        integer(int32) :: nbytes
        integer :: k
        type(status_type) :: status

        s = ""

        do k = 1, buffer%len_chars
            call char_string(buffer%chars(k), tmp, nbytes, status)
            s = s//tmp(1:nbytes)
        end do
    end function buffer_text

    !> Case character CH (with modifier bits) by FLAG.  Port of
    !> casefiddle.c:do_casify_natnum (and casefiddle.c:casify_object for a
    !> character).
    integer(int32) function casify_char(flag, ch) result(r)
        integer, intent(in) :: flag
        integer(int32), intent(in) :: ch
        integer(int32) :: flagbits, flags, c, cased
        type(casing_context_type) :: ctx
        logical :: multibyte

        flagbits = ior(ior(ior(CHAR_ALT, CHAR_SUPER), ior(CHAR_HYPER, CHAR_SHIFT)), ior(CHAR_CTL, CHAR_META))
        r = ch
        ! If the character has higher bits set above the flags, return it
        ! unchanged.  It is not a real character.
        if (.not. (0 <= ch .and. ch <= flagbits)) return
        flags = iand(ch, flagbits)
        c = iand(ch, not(flagbits))
        ctx = prepare_casing_context(flag, .false.)
        multibyte = c >= 256
        if (.not. multibyte) multibyte = current == 0
        if (.not. multibyte) multibyte = buffers(current)%multibyte
        if (.not. multibyte) c = unibyte_to_char(c)
        cased = case_single_character(ctx, c)
        if (cased == c) return
        if (.not. multibyte) cased = make_char_unibyte(cased)
        r = ior(cased, flags)
    end function casify_char

    !> If C is not ASCII, make it unibyte.  Port of
    !> casefiddle.c:make_char_unibyte.
    elemental integer(int32) function make_char_unibyte(c)
        integer(int32), intent(in) :: c

        make_char_unibyte = c
        if (c >= 128) make_char_unibyte = char_to_byte8(c)
    end function make_char_unibyte

    !> Case the multibyte string STRING by FLAG.  Port of
    !> casefiddle.c:do_casify_multibyte_string (and casefiddle.c:casify_object
    !> for a string).
    function casify_string(flag, string) result(out)
        integer, intent(in) :: flag
        character(len=*), intent(in) :: string
        character(len=:), allocatable :: out
        type(casing_context_type) :: ctx
        type(casing_str_buf_type) :: buffer
        integer(int64) :: i
        integer(int32) :: ch, clen, next, nlen
        logical :: ignored

        ctx = prepare_casing_context(flag, .false.)
        out = ""
        i = 1

        do while (i <= len(string, kind=int64))
            call string_char_and_length(string, i, ch, clen)
            next = -1
            if (i + clen <= len(string, kind=int64)) call string_char_and_length(string, i + clen, next, nlen)
            ignored = case_character(buffer, ctx, ch, next)
            out = out//buffer_text(buffer)
            i = i + clen
        end do
    end function casify_string

    !> Convert argument to upper case and return that.
    !> The argument object is not altered--the value is a copy.  See also
    !> `capitalize', `downcase' and `upcase_initials'.  Port of
    !> casefiddle.c:Fupcase for a string.
    function upcase_string(string) result(out)
        character(len=*), intent(in) :: string
        character(len=:), allocatable :: out

        out = casify_string(CASE_UP, string)
    end function upcase_string

    !> Convert argument to lower case and return that.
    !> The argument object is not altered--the value is a copy.  Port of
    !> casefiddle.c:Fdowncase for a string.
    function downcase_string(string) result(out)
        character(len=*), intent(in) :: string
        character(len=:), allocatable :: out

        out = casify_string(CASE_DOWN, string)
    end function downcase_string

    !> Convert argument to capitalized form and return that.
    !> This means that each word's first character is converted to either
    !> title case or upper case, and the rest to lower case.
    !> The argument object is not altered--the value is a copy.  Port of
    !> casefiddle.c:Fcapitalize for a string.
    function capitalize_string(string) result(out)
        character(len=*), intent(in) :: string
        character(len=:), allocatable :: out

        out = casify_string(CASE_CAPITALIZE, string)
    end function capitalize_string

    !> Convert the initial of each word in the argument to upper case.
    !> This means that each word's first character is converted to either
    !> title case or upper case, and the rest are left unchanged.
    !> The argument object is not altered--the value is a copy.  Port of
    !> casefiddle.c:Fupcase_initials for a string.
    function upcase_initials_string(string) result(out)
        character(len=*), intent(in) :: string
        character(len=:), allocatable :: out

        out = casify_string(CASE_CAPITALIZE_UP, string)
    end function upcase_initials_string

    !> Convert a character to upper case.  Characters which map to
    !> multiple code points when cased, e.g. ﬁ, are returned unchanged.
    !> Port of casefiddle.c:Fupcase for a character.
    integer(int32) function upcase_char(ch)
        integer(int32), intent(in) :: ch

        upcase_char = casify_char(CASE_UP, ch)
    end function upcase_char

    !> Convert a character to lower case.  Port of casefiddle.c:Fdowncase
    !> for a character.
    integer(int32) function downcase_char(ch)
        integer(int32), intent(in) :: ch

        downcase_char = casify_char(CASE_DOWN, ch)
    end function downcase_char

    !> Convert a character to capitalized form: title case or upper case.
    !> Port of casefiddle.c:Fcapitalize for a character.
    integer(int32) function capitalize_char(ch)
        integer(int32), intent(in) :: ch

        capitalize_char = casify_char(CASE_CAPITALIZE, ch)
    end function capitalize_char

    !> Convert the initial of a character "word" to upper case: title case
    !> or upper case.  Port of casefiddle.c:Fupcase_initials for a
    !> character.
    integer(int32) function upcase_initials_char(ch)
        integer(int32), intent(in) :: ch

        upcase_initials_char = casify_char(CASE_CAPITALIZE_UP, ch)
    end function upcase_initials_char

    !> Based on CTX, case region in a unibyte buffer from START to END.
    !> Save first and last positions that has changed in START and END
    !> respectively.  If no characters were changed, save -1 to START and
    !> leave END unspecified.  Always return 0, as
    !> `do_casify_multibyte_region' returns the number of added characters.
    !> Port of casefiddle.c:do_casify_unibyte_region.
    integer(int64) function do_casify_unibyte_region(ctx, start, end) result(added)
        type(casing_context_type), intent(inout) :: ctx
        integer(int64), intent(inout) :: start, end
        integer(int64) :: first, last, pos, k
        integer(int32) :: ch, cased

        first = -1
        last = -1

        do pos = start, end - 1
            ch = unibyte_to_char(buf_fetch_byte(buffers(current), pos))
            cased = case_single_character(ctx, ch)
            if (cased == ch) cycle
            last = pos + 1
            if (first < 0) first = pos
            k = buffer_byte_index(buffers(current), pos)
            buffers(current)%text%beginning(k:k) = achar(make_char_unibyte(cased))
        end do

        start = first
        end = last
        added = 0
    end function do_casify_unibyte_region

    !> Based on CTX, case the region of a multibyte buffer from START to
    !> END.  Return the number of added characters (negative if more were
    !> deleted than inserted), and the first and last positions that
    !> changed in START and END (START -1 if none).  Port of
    !> casefiddle.c:do_casify_multibyte_region.
    integer(int64) function do_casify_multibyte_region(ctx, start, end, status) result(added)
        type(casing_context_type), intent(inout) :: ctx
        integer(int64), intent(inout) :: start, end
        type(status_type), intent(out) :: status
        integer(int64) :: first, last, pos, pos_byte, size, opoint
        integer(int32) :: ch, len1, next
        type(casing_str_buf_type) :: buffer
        character(len=:), allocatable :: text

        status = status_type()
        first = -1
        last = -1
        pos = start
        pos_byte = char_to_byte(pos)
        size = end - pos
        opoint = point()
        added = 0

        do while (size > 0)
            ch = buf_fetch_char(buffers(current), pos_byte)
            len1 = buf_next_char_len(buffers(current), pos_byte)
            next = -1
            if (size > 1) next = buf_fetch_char(buffers(current), pos_byte + len1)

            if (.not. case_character(buffer, ctx, ch, next)) then
                pos_byte = pos_byte + len1
                pos = pos + 1
                size = size - 1
                cycle
            end if

            last = pos + buffer%len_chars
            if (first < 0) first = pos
            text = buffer_text(buffer)
            ! Replace one character with the other(s), keeping text
            ! properties the same.
            call replace_range_2(pos, pos_byte, pos + 1, pos_byte + len1, text, int(buffer%len_chars, int64), &
                                 .false., status)
            if (.not. status%ok()) return
            added = added + buffer%len_chars - 1
            if (opoint > pos) opoint = opoint + buffer%len_chars - 1
            pos_byte = pos_byte + len(text)
            pos = pos + buffer%len_chars
            size = size - 1
        end do

        if (point() /= opoint) call temp_set_point_both(buffers(current), opoint, char_to_byte(opoint))
        start = first
        end = last
    end function do_casify_multibyte_region

    !> Case the region from B to E by FLAG.  Return the end of the region
    !> after the changes.  Port of casefiddle.c:casify_region.
    recursive integer(int64) function casify_region(flag, b, e, status) result(new_end)
        integer, intent(in) :: flag
        integer(int64), intent(in) :: b, e
        type(status_type), intent(out) :: status
        integer(int64) :: start, end, orig_end, length, ostart, added
        type(casing_context_type) :: ctx

        start = b
        end = e
        new_end = e
        call validate_region(start, end, status)
        if (.not. status%ok()) return
        new_end = end
        ! Not modifying because nothing marked.
        if (start == end) return
        call modify_text(start, end, status)
        if (.not. status%ok()) return
        ctx = prepare_casing_context(flag, .true.)
        orig_end = end
        call record_delete(start, buf_bytes(buffers(current), char_to_byte(start), char_to_byte(end)), end - start)
        length = end - start
        ostart = start

        if (.not. buffers(current)%multibyte) then
            added = do_casify_unibyte_region(ctx, start, end)
        else
            added = do_casify_multibyte_region(ctx, start, end, status)
            if (.not. status%ok()) return
        end if

        call record_insert(ostart, length + added)
        new_end = orig_end + added
        if (start >= 0) call signal_after_change(start, end - start - added, end - start, status)
    end function casify_region

    !> Convert the region to upper case.  In programs, wants two arguments.
    !> These arguments specify the starting and ending character numbers
    !> of the region to operate on.  When used as a command, the text
    !> between point and the mark is operated on.
    !> See also `capitalize_region'.  Port of casefiddle.c:Fupcase_region.
    recursive subroutine upcase_region(beginning, end, status, region_noncontiguous)
        integer(int64), intent(in) :: beginning, end
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: region_noncontiguous

        call casify_pnc_region(CASE_UP, beginning, end, status, region_noncontiguous)
    end subroutine upcase_region

    !> Convert the region to lower case.  In programs, wants two arguments.
    !> These arguments specify the starting and ending character numbers
    !> of the region to operate on.  When used as a command, the text
    !> between point and the mark is operated on.  Port of
    !> casefiddle.c:Fdowncase_region.
    recursive subroutine downcase_region(beginning, end, status, region_noncontiguous)
        integer(int64), intent(in) :: beginning, end
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: region_noncontiguous

        call casify_pnc_region(CASE_DOWN, beginning, end, status, region_noncontiguous)
    end subroutine downcase_region

    !> Convert the region to capitalized form.
    !> This means that each word's first character is converted to either
    !> title case or upper case, and the rest to lower case.
    !> In programs, give two arguments, the starting and ending character
    !> positions to operate on.  Port of casefiddle.c:Fcapitalize_region.
    recursive subroutine capitalize_region(beginning, end, status, region_noncontiguous)
        integer(int64), intent(in) :: beginning, end
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: region_noncontiguous

        call casify_pnc_region(CASE_CAPITALIZE, beginning, end, status, region_noncontiguous)
    end subroutine capitalize_region

    !> Upcase the initial of each word in the region.
    !> This means that each word's first character is converted to either
    !> title case or upper case, and the rest are left unchanged.
    !> In programs, give two arguments, the starting and ending character
    !> positions to operate on.  Port of
    !> casefiddle.c:Fupcase_initials_region.
    recursive subroutine upcase_initials_region(beginning, end, status, region_noncontiguous)
        integer(int64), intent(in) :: beginning, end
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: region_noncontiguous

        call casify_pnc_region(CASE_CAPITALIZE_UP, beginning, end, status, region_noncontiguous)
    end subroutine upcase_initials_region

    !> Casify a possibly noncontiguous region according to FLAG.  BEGINNING and
    !> END specify the bounds, except that if REGION_NONCONTIGUOUS is
    !> true, the region's bounds are specified by `region_bounds'
    !> (`region_extract_function') instead.  Port of
    !> casefiddle.c:casify_pnc_region.
    recursive subroutine casify_pnc_region(flag, beginning, end, status, region_noncontiguous)
        integer, intent(in) :: flag
        integer(int64), intent(in) :: beginning, end
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: region_noncontiguous
        integer(int64), allocatable :: starts(:), ends(:)
        integer(int64) :: ignored
        integer :: k
        logical :: noncontiguous

        noncontiguous = .false.
        if (present(region_noncontiguous)) noncontiguous = region_noncontiguous

        if (noncontiguous .and. associated(region_bounds)) then
            call region_bounds(starts, ends, status)
            if (.not. status%ok()) return

            do k = 1, size(starts)
                ignored = casify_region(flag, starts(k), ends(k), status)
                if (.not. status%ok()) return
            end do
        else
            ignored = casify_region(flag, beginning, end, status)
        end if
    end subroutine casify_pnc_region

    !> Case from point over ARG words by FLAG.  Port of
    !> casefiddle.c:casify_word.
    subroutine casify_word(flag, arg, status)
        integer, intent(in) :: flag
        integer(int64), intent(in) :: arg
        type(status_type), intent(out) :: status
        integer(int64) :: farend, pos

        farend = scan_words(point(), arg)

        if (farend == 0) then
            if (arg <= 0) then
                farend = buffers(current)%begv
            else
                farend = buffers(current)%zv
            end if
        end if

        pos = casify_region(flag, point(), farend, status)
        if (status%ok()) call goto_char(pos)
    end subroutine casify_word

    !> Convert to upper case from point to end of word, moving over.
    !>
    !> If point is in the middle of a word, the part of that word before
    !> point is ignored when moving forward.
    !>
    !> With negative argument, convert previous words but do not move.
    !> See also `capitalize_word'.  Port of casefiddle.c:Fupcase_word.
    subroutine upcase_word(arg, status)
        integer(int64), intent(in) :: arg
        type(status_type), intent(out) :: status

        call casify_word(CASE_UP, arg, status)
    end subroutine upcase_word

    !> Convert to lower case from point to end of word, moving over.
    !>
    !> If point is in the middle of a word, the part of that word before
    !> point is ignored when moving forward.
    !>
    !> With negative argument, convert previous words but do not move.
    !> Port of casefiddle.c:Fdowncase_word.
    subroutine downcase_word(arg, status)
        integer(int64), intent(in) :: arg
        type(status_type), intent(out) :: status

        call casify_word(CASE_DOWN, arg, status)
    end subroutine downcase_word

    !> Capitalize from point to the end of word, moving over.
    !> With numerical argument ARG, capitalize the next ARG-1 words as
    !> well.  This gives the word(s) a first character in upper case and
    !> the rest lower case.
    !>
    !> If point is in the middle of a word, the part of that word before
    !> point is ignored when moving forward.
    !>
    !> With negative argument, capitalize previous words but do not move.
    !> Port of casefiddle.c:Fcapitalize_word.
    subroutine capitalize_word(arg, status)
        integer(int64), intent(in) :: arg
        type(status_type), intent(out) :: status

        call casify_word(CASE_CAPITALIZE, arg, status)
    end subroutine capitalize_word

    subroutine cmd_upcase_word(status)
        type(status_type), intent(out) :: status

        call upcase_word(prefix_numeric_value(current_prefix_arg), status)
    end subroutine cmd_upcase_word

    subroutine cmd_downcase_word(status)
        type(status_type), intent(out) :: status

        call downcase_word(prefix_numeric_value(current_prefix_arg), status)
    end subroutine cmd_downcase_word

    subroutine cmd_capitalize_word(status)
        type(status_type), intent(out) :: status

        call capitalize_word(prefix_numeric_value(current_prefix_arg), status)
    end subroutine cmd_capitalize_word

    !> The region commands take the region (interactive "(list
    !> (region-beginning) (region-end) (region-noncontiguous-p))").
    !> Not a port.
    subroutine region_command(flag, status)
        integer, intent(in) :: flag
        type(status_type), intent(out) :: status
        integer(int64) :: b, e
        logical :: noncontiguous

        call region_beginning(b, status)
        if (.not. status%ok()) return
        call region_end(e, status)
        if (.not. status%ok()) return
        noncontiguous = .false.
        if (associated(is_region_noncontiguous)) noncontiguous = is_region_noncontiguous()
        call casify_pnc_region(flag, b, e, status, noncontiguous)
    end subroutine region_command

    subroutine cmd_upcase_region(status)
        type(status_type), intent(out) :: status

        call region_command(CASE_UP, status)
    end subroutine cmd_upcase_region

    subroutine cmd_downcase_region(status)
        type(status_type), intent(out) :: status

        call region_command(CASE_DOWN, status)
    end subroutine cmd_downcase_region

    subroutine cmd_capitalize_region(status)
        type(status_type), intent(out) :: status

        call region_command(CASE_CAPITALIZE, status)
    end subroutine cmd_capitalize_region

    subroutine cmd_upcase_initials_region(status)
        type(status_type), intent(out) :: status

        call region_command(CASE_CAPITALIZE_UP, status)
    end subroutine cmd_upcase_initials_region

    !> Register the commands.  Port of casefiddle.c:syms_of_casefiddle.
    subroutine syms_of_casefiddle()
        commands%upcase_word%execute => cmd_upcase_word
        commands%downcase_word%execute => cmd_downcase_word
        commands%capitalize_word%execute => cmd_capitalize_word
        commands%upcase_region%execute => cmd_upcase_region
        commands%downcase_region%execute => cmd_downcase_region
        commands%capitalize_region%execute => cmd_capitalize_region
        commands%upcase_initials_region%execute => cmd_upcase_initials_region
    end subroutine syms_of_casefiddle
end module casefiddle_fm
