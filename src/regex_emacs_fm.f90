! regex_emacs_fm.f90 -- Emacs regular expressions.
! Ported from GNU Emacs 31.1 src/regex-emacs.c and src/regex-emacs.h.
!
! Copyright (C) 1993-2026 Free Software Foundation, Inc.
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

!> The regular expression compiler and matcher.
!>
!> A pattern compiles to the same byte code as upstream (`buffer(0:used-1)',
!> one byte per element), so the compiler, the first-character analysis and
!> the backtracking matcher follow regex-emacs.c closely.  Differences:
!>
!> - The text is a `re_subject_type': a string, or the accessible part of
!>   the current buffer read across the gap.  Offsets into it are 0-based
!>   byte offsets, as upstream's offsets into the virtual concatenation of
!>   STRING1 and STRING2.  Pointers into the pattern or the text become
!>   offsets, and NULL becomes -1.
!> - Patterns and text are always multibyte (Emacs's internal encoding), so
!>   the unibyte paths are left out.
!> - Case folding (`translate') is the case canon table of casetab_fm.
!> - Syntax comes from the current buffer's syntax table and, for \b, \<,
!>   \>, \_<, \_> and \s, its `syntax_table' properties (as upstream);
!>   categories come from characters_fm.
!> - Errors are returned as a status of `invalid_regexp'; a matcher that
!>   runs out of failure stack returns -2.
module regex_emacs_fm
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use error_fm, only: status_type, set_error, FM_INVALID_REGEXP
    use print_fm, only: fortran_string
    use character_fm, only: ascii_char_p, char_byte8_p, byte8_to_char, char_to_byte8, char_to_byte_safe, &
                            char_byte8_head_p, char_leading_code, char_string, char_head_p, bytes_by_char_head, &
                            string_char_and_length, MAX_MULTIBYTE_LENGTH, MIN_MULTIBYTE_LEADING_CODE, &
                            MAX_MULTIBYTE_LEADING_CODE, alphabeticp, alphanumericp, graphicp, printablep, blankp, &
                            char_hexdigit
    use casetab_fm, only: case_canon, uppercasep, lowercasep
    use syntax_fm, only: char_syntax_class, syntax_class_at, word_boundary_p, SWHITESPACE, SPUNCT, SWORD, SSYMBOL, SOPEN, &
                         SCLOSE, SQUOTE, SSTRING, SMATH, SESCAPE, SCHARQUOTE, SCOMMENT, SENDCOMMENT, SINHERIT, &
                         SCOMMENT_FENCE, SSTRING_FENCE
    use characters_fm, only: char_has_category
    use buffer_fm, only: buffers, cur
    use marker_fm, only: byte_to_char
    implicit none(type, external)
    private

    public :: re_pattern_buffer_type, re_registers_type, re_subject_type
    public :: re_compile_pattern, re_search, re_match, subject_from_string, subject_from_buffer
    public :: re_wctype_parse, re_iswctype

    !> The opcodes of compiled patterns (regex-emacs.c re_opcode_t).
    integer, parameter :: NO_OP = 0, SUCCEED = 1, EXACTN = 2, ANYCHAR = 3, CHARSET = 4, CHARSET_NOT = 5, &
                          START_MEMORY = 6, STOP_MEMORY = 7, DUPLICATE = 8, BEGLINE = 9, ENDLINE = 10, &
                          BEGBUF = 11, ENDBUF = 12, JUMP = 13, ON_FAILURE_JUMP = 14, &
                          ON_FAILURE_KEEP_STRING_JUMP = 15, ON_FAILURE_JUMP_LOOP = 16, &
                          ON_FAILURE_JUMP_NASTYLOOP = 17, ON_FAILURE_JUMP_SMART = 18, SUCCEED_N = 19, &
                          JUMP_N = 20, SET_NUMBER_AT = 21, WORDBEG = 22, WORDEND = 23, WORDBOUND = 24, &
                          NOTWORDBOUND = 25, SYMBEG = 26, SYMEND = 27, SYNTAXSPEC = 28, NOTSYNTAXSPEC = 29, &
                          AT_DOT = 30, CATEGORYSPEC = 31, NOTCATEGORYSPEC = 32

    !> Error codes of the compiler (regex-emacs.h reg_errcode_t).
    integer, parameter :: REG_NOERROR = 0, REG_NOMATCH = 1, REG_BADPAT = 2, REG_ECOLLATE = 3, &
                          REG_ECTYPE = 4, REG_EESCAPE = 5, REG_ESUBREG = 6, REG_EBRACK = 7, REG_EPAREN = 8, &
                          REG_EBRACE = 9, REG_BADBR = 10, REG_ERANGE = 11, REG_ESPACE = 12, REG_BADRPT = 13, &
                          REG_EEND = 14, REG_ESIZE = 15, REG_ERPAREN = 16, REG_ERANGEX = 17, REG_ESIZEBR = 18

    !> Character classes (regex-emacs.h re_wctype_t).
    integer, parameter, public :: RECC_ERROR = 0, RECC_ALNUM = 1, RECC_ALPHA = 2, RECC_WORD = 3, &
                                  RECC_GRAPH = 4, RECC_PRINT = 5, RECC_LOWER = 6, RECC_UPPER = 7, &
                                  RECC_PUNCT = 8, RECC_CNTRL = 9, RECC_DIGIT = 10, RECC_XDIGIT = 11, &
                                  RECC_BLANK = 12, RECC_SPACE = 13, RECC_MULTIBYTE = 14, RECC_NONASCII = 15, &
                                  RECC_ASCII = 16, RECC_UNIBYTE = 17

    !> Bits of the range table for character classes of multibyte
    !> characters.
    integer, parameter :: BIT_WORD = int(z'1'), BIT_LOWER = int(z'2'), BIT_PUNCT = int(z'4'), &
                          BIT_SPACE = int(z'8'), BIT_UPPER = int(z'10'), BIT_MULTIBYTE = int(z'20'), &
                          BIT_ALPHA = int(z'40'), BIT_ALNUM = int(z'80'), BIT_GRAPH = int(z'100'), &
                          BIT_PRINT = int(z'200'), BIT_BLANK = int(z'400')

    !> Offsets in the pattern are two bytes long.
    integer, parameter :: MAX_BUF_SIZE = 32768
    integer, parameter :: INIT_BUF_SIZE = 32
    integer, parameter :: MAX_REGNUM = 255
    integer, parameter :: RE_DUP_MAX = int(z'ffff')
    !> RE_DUP_MAX / 10 and RE_DUP_MAX % 10, for overflow checks.
    integer, parameter :: RE_DUP_MAX_TENTH = 6553, RE_DUP_MAX_LAST_DIGIT = 5
    !> The largest explicit group number that can take one more digit.
    integer, parameter :: REGNUM_LIMIT = 214748363

    !> Roughly the maximum number of failure points on the stack.
    integer(int64), parameter :: EMACS_RE_MAX_FAILURES = 40000
    integer(int64), parameter :: TYPICAL_FAILURE_SIZE = 20
    integer(int64), parameter :: INIT_FAILURE_ALLOC = 20

    !> What forall_firstchar calls on each operation it reaches.
    integer, parameter :: VISIT_NULL = 1, VISIT_FASTMAP = 2, VISIT_MUTEX = 3

    !> The error messages, indexed by error code.
    character(len=40), parameter :: re_error_msgid(0:18) = [character(len=40) :: &
                                                            "Success", "No match", "Invalid regular expression", &
                                                            "Invalid collation character", &
                                                            "Invalid character class name", "Trailing backslash", &
                                                            "Invalid back reference", "Unmatched [ or [^", &
                                                            "Unmatched ( or \(", "Unmatched \{", &
                                                            "Invalid content of \{\}", "Invalid range end", &
                                                            "Memory exhausted", &
                                                            "Invalid preceding regular expression", &
                                                            "Premature end of regular expression", &
                                                            "Regular expression too big", "Unmatched ) or \)", &
                                                            "Range striding over charsets", &
                                                            "Invalid content of \{\}"]

    !> A compiled pattern (regex-emacs.h struct re_pattern_buffer).
    type :: re_pattern_buffer_type
        !> The compiled pattern, `buffer(0:used-1)', one byte per element.
        !> The matcher changes it (loop counters and on_failure_jump_smart).
        integer, allocatable :: buffer(:)
        integer :: used = 0
        !> Number of subexpressions.
        integer :: re_nsub = 0
        !> Fold case through the case canon table.
        logical :: translate = .false.
        !> Which bytes can start a match, if `fastmap_accurate'.
        logical :: fastmap(0:255) = .false.
        logical :: fastmap_accurate = .false.
        !> The pattern can match the empty string.
        logical :: can_be_null = .false.
        !> The pattern depends on the syntax table.
        logical :: used_syntax = .false.
    end type re_pattern_buffer_type

    !> Where each group matched, as offsets into the subject; -1 if it
    !> didn't (regex-emacs.h struct re_registers).
    type :: re_registers_type
        integer(int64), allocatable :: starts(:), ends(:)
    end type re_registers_type

    !> The text to match: a string, or the accessible part of the current
    !> buffer (BEGV_BYTE to ZV_BYTE).  Offsets are 0-based.
    type :: re_subject_type
        logical :: is_buffer = .false.
        character(len=:), allocatable :: str
        !> Buffer: BEGV_BYTE, the gap position and size.
        integer(int64) :: base = 1, gpt_byte = 1, gap_size = 0
        integer(int64) :: size = 0
    end type re_subject_type

    !> State of forall_firstchar's visitors (anafirst_data, mutexcl_data).
    type :: visit_data_type
        integer :: mode = VISIT_NULL
        logical :: match_any_multibyte_characters = .false.
        integer :: p1 = 0
        logical :: unconstrained = .true.
    end type visit_data_type

contains

    !> The string `str' as a subject.  Not a port.
    function subject_from_string(str) result(s)
        character(len=*), intent(in) :: str
        type(re_subject_type) :: s

        s%is_buffer = .false.
        s%str = str
        s%size = len(str, kind=int64)
    end function subject_from_string

    !> The accessible part of the current buffer as a subject.  Not a port.
    function subject_from_buffer() result(s)
        type(re_subject_type) :: s

        s%is_buffer = .true.

        associate (b => buffers(cur))
            s%base = b%begv_byte
            s%gpt_byte = b%text%gpt_byte
            s%gap_size = b%text%gap_size
            s%size = b%zv_byte - b%begv_byte
        end associate
    end function subject_from_buffer

    !> The byte at offset `d' of the subject.  Not a port.
    integer function sbyte(s, d)
        type(re_subject_type), intent(in) :: s
        integer(int64), intent(in) :: d
        integer(int64) :: bp

        if (s%is_buffer) then
            bp = s%base + d
            if (bp >= s%gpt_byte) bp = bp + s%gap_size
            sbyte = ichar(buffers(cur)%text%beg(bp:bp))
        else
            sbyte = ichar(s%str(d + 1:d + 1))
        end if
    end function sbyte

    !> The character at offset `d' of the subject and its length
    !> (GET_CHAR_AFTER).  Port of regex-emacs.c:GET_CHAR_AFTER.
    subroutine schar(s, d, c, length)
        type(re_subject_type), intent(in) :: s
        integer(int64), intent(in) :: d
        integer(int32), intent(out) :: c, length
        character(len=MAX_MULTIBYTE_LENGTH) :: tmp
        integer(int64) :: n, k

        if (.not. s%is_buffer) then
            call string_char_and_length(s%str, d + 1, c, length)
            return
        end if

        n = min(int(bytes_by_char_head(int(sbyte(s, d), int32)), int64), s%size - d)

        do k = 1, n
            tmp(k:k) = achar(sbyte(s, d + k - 1))
        end do

        call string_char_and_length(tmp(1:n), 1_int64, c, length)
    end subroutine schar

    !> The character before offset `d' (GET_CHAR_BEFORE_2).  Port of
    !> regex-emacs.c:GET_CHAR_BEFORE_2.
    integer(int32) function schar_before(s, d) result(c)
        type(re_subject_type), intent(in) :: s
        integer(int64), intent(in) :: d
        integer(int64) :: dtemp
        integer(int32) :: length

        dtemp = d - 1

        do while (dtemp > 0)
            if (char_head_p(int(sbyte(s, dtemp), int32))) exit
            dtemp = dtemp - 1
        end do

        call schar(s, dtemp, c, length)
    end function schar_before

    !> The syntax class of C, the character at offset D of the subject (SHIFT
    !> 0) or the one before it (SHIFT -1): SYNTAX after UPDATE_SYNTAX_TABLE
    !> at its position, which consults `syntax_table' properties in a buffer
    !> (strings carry no text properties).  Not a port.
    recursive integer function subject_syntax(s, d, shift, c) result(code)
        type(re_subject_type), intent(in) :: s
        integer(int64), intent(in) :: d, shift
        integer(int32), intent(in) :: c

        if (s%is_buffer) then
            code = syntax_class_at(int(c), byte_to_char(s%base + d) + shift)
        else
            code = char_syntax_class(int(c))
        end if
    end function subject_syntax

    !> The character of the compiled pattern at `p' and its length.
    !> Not a port.
    subroutine pchar(buf, p, c, length)
        integer, intent(in) :: buf(0:)
        integer, intent(in) :: p
        integer(int32), intent(out) :: c, length
        character(len=MAX_MULTIBYTE_LENGTH) :: tmp
        integer :: n, k

        n = min(bytes_by_char_head(int(buf(p), int32)), size(buf) - p)

        do k = 1, n
            tmp(k:k) = achar(buf(p + k - 1))
        end do

        call string_char_and_length(tmp(1:n), 1_int64, c, length)
    end subroutine pchar

    !> Store NUMBER in two contiguous bytes at `at' (STORE_NUMBER).  Port of
    !> regex-emacs.c:STORE_NUMBER.
    pure subroutine store_number(buf, at, number)
        integer, intent(inout) :: buf(0:)
        integer, intent(in) :: at, number

        buf(at) = iand(number, 255)
        buf(at + 1) = iand(ishft(number, -8), 255)
    end subroutine store_number

    !> The number stored in two bytes at `at' (extract_number).  Port of
    !> regex-emacs.c:extract_number.
    pure integer function extract_number(buf, at) result(n)
        integer, intent(in) :: buf(0:)
        integer, intent(in) :: at
        integer :: leading_byte

        leading_byte = buf(at + 1)
        if (leading_byte >= 128) leading_byte = leading_byte - 256
        n = leading_byte*256 + buf(at)
    end function extract_number

    !> The target of the relative address at `at' (extract_address).
    !> Port of regex-emacs.c:extract_address.
    pure integer function extract_address(buf, at)
        integer, intent(in) :: buf(0:)
        integer, intent(in) :: at

        extract_address = at + 2 + extract_number(buf, at)
    end function extract_address

    !> Store a character in three bytes at `at' (STORE_CHARACTER_AND_INCR).
    !> Port of regex-emacs.c:STORE_CHARACTER_AND_INCR.
    pure subroutine store_character(buf, at, c)
        integer, intent(inout) :: buf(0:)
        integer, intent(in) :: at
        integer(int32), intent(in) :: c

        buf(at) = iand(c, 255)
        buf(at + 1) = iand(ishft(c, -8), 255)
        buf(at + 2) = ishft(c, -16)
    end subroutine store_character

    !> The character stored in three bytes at `at' (EXTRACT_CHARACTER).
    !> Port of regex-emacs.c:EXTRACT_CHARACTER.
    pure integer(int32) function extract_character(buf, at)
        integer, intent(in) :: buf(0:)
        integer, intent(in) :: at

        extract_character = ior(ior(buf(at), ishft(buf(at + 1), 8)), ishft(buf(at + 2), 16))
    end function extract_character

    !> Size of the bitmap of the charset at `p' (CHARSET_BITMAP_SIZE).
    !> Port of regex-emacs.c:CHARSET_BITMAP_SIZE.
    pure integer function charset_bitmap_size(buf, p)
        integer, intent(in) :: buf(0:)
        integer, intent(in) :: p

        charset_bitmap_size = iand(buf(p + 1), 127)
    end function charset_bitmap_size

    !> The charset at `p' has a range table (CHARSET_RANGE_TABLE_EXISTS_P).
    !> Port of regex-emacs.c:CHARSET_RANGE_TABLE_EXISTS_P.
    pure logical function charset_range_table_exists_p(buf, p)
        integer, intent(in) :: buf(0:)
        integer, intent(in) :: p

        charset_range_table_exists_p = iand(buf(p + 1), 128) /= 0
    end function charset_range_table_exists_p

    !> Where the count of the range table of the charset at `p' is
    !> (CHARSET_RANGE_TABLE).  Port of regex-emacs.c:CHARSET_RANGE_TABLE.
    pure integer function charset_range_table(buf, p)
        integer, intent(in) :: buf(0:)
        integer, intent(in) :: p

        charset_range_table = p + 4 + charset_bitmap_size(buf, p)
    end function charset_range_table

    !> The class bits that start the range table (CHARSET_RANGE_TABLE_BITS).
    !> Port of regex-emacs.c:CHARSET_RANGE_TABLE_BITS.
    pure integer function charset_range_table_bits(buf, p)
        integer, intent(in) :: buf(0:)
        integer, intent(in) :: p
        integer :: n

        n = charset_bitmap_size(buf, p)
        charset_range_table_bits = buf(p + 2 + n) + buf(p + 3 + n)*256
    end function charset_range_table_bits

    !> Syntax code for the character after \s or \S, or 255 for none
    !> (syntax.c syntax_spec_code).  Port of the syntax_spec_code table of
    !> syntax.c.
    pure integer function syntax_spec_code(c)
        integer(int32), intent(in) :: c

        select case (c)
        case (iachar(" "), iachar("-"))
            syntax_spec_code = SWHITESPACE
        case (iachar("!"))
            syntax_spec_code = SCOMMENT_FENCE
        case (iachar('"'))
            syntax_spec_code = SSTRING
        case (iachar("$"))
            syntax_spec_code = SMATH
        case (iachar("'"))
            syntax_spec_code = SQUOTE
        case (iachar("("))
            syntax_spec_code = SOPEN
        case (iachar(")"))
            syntax_spec_code = SCLOSE
        case (iachar("."))
            syntax_spec_code = SPUNCT
        case (iachar("/"))
            syntax_spec_code = SCHARQUOTE
        case (iachar("<"))
            syntax_spec_code = SCOMMENT
        case (iachar(">"))
            syntax_spec_code = SENDCOMMENT
        case (iachar("@"))
            syntax_spec_code = SINHERIT
        case (iachar("W"), iachar("w"))
            syntax_spec_code = SWORD
        case (iachar("\"))
            syntax_spec_code = SESCAPE
        case (iachar("_"))
            syntax_spec_code = SSYMBOL
        case (iachar("|"))
            syntax_spec_code = SSTRING_FENCE
        case default
            syntax_spec_code = 255
        end select
    end function syntax_spec_code

    !> Character class predicates of regex-emacs.c, for any character.
    !> Port of regex-emacs.c:ISALNUM.
    logical function isalnum(c)
        integer(int32), intent(in) :: c

        if (c < 128) then
            isalnum = is_ascii_letter(c) .or. (c >= iachar("0") .and. c <= iachar("9"))
        else
            isalnum = alphanumericp(c)
        end if
    end function isalnum

    ! Port of regex-emacs.c:ISALPHA.
    logical function isalpha(c)
        integer(int32), intent(in) :: c

        if (c < 128) then
            isalpha = is_ascii_letter(c)
        else
            isalpha = alphabeticp(c)
        end if
    end function isalpha

    ! Whether C is an ASCII letter.  Not a port.
    pure logical function is_ascii_letter(c)
        integer(int32), intent(in) :: c

        is_ascii_letter = (c >= iachar("a") .and. c <= iachar("z")) .or. (c >= iachar("A") .and. c <= iachar("Z"))
    end function is_ascii_letter

    ! Port of regex-emacs.c:ISBLANK.
    logical function isblank(c)
        integer(int32), intent(in) :: c

        if (c < 128) then
            isblank = c == 32 .or. c == 9
        else
            isblank = blankp(c)
        end if
    end function isblank

    ! Port of regex-emacs.c:ISGRAPH.
    logical function isgraph(c)
        integer(int32), intent(in) :: c

        if (c < 256) then
            isgraph = c > 32 .and. .not. (c >= 127 .and. c <= 160)
        else
            isgraph = graphicp(c)
        end if
    end function isgraph

    ! Port of regex-emacs.c:ISPRINT.
    logical function isprint(c)
        integer(int32), intent(in) :: c

        if (c < 256) then
            isprint = c >= 32 .and. .not. (c >= 127 .and. c <= 159)
        else
            isprint = printablep(c)
        end if
    end function isprint

    ! Port of regex-emacs.c:ISPUNCT.
    logical function ispunct(c)
        integer(int32), intent(in) :: c

        if (c < 128) then
            ! hazard-ok(short-circuit): isalnum() takes any character code.
            ispunct = c > 32 .and. c < 127 .and. .not. isalnum(c)
        else
            ispunct = char_syntax_class(c) /= SWORD
        end if
    end function ispunct

    ! Port of regex-emacs.c:ISSPACE.
    logical function isspace(c)
        integer(int32), intent(in) :: c

        isspace = char_syntax_class(c) == SWHITESPACE
    end function isspace

    ! Port of regex-emacs.c:ISWORD.
    logical function isword(c)
        integer(int32), intent(in) :: c

        isword = char_syntax_class(c) == SWORD
    end function isword

    !> Parse a character class "[:name:]" at `str(p:pend-1)'.  If it is
    !> one, advance `p' past it and return its RECC_ constant (RECC_ERROR
    !> for an unknown name); otherwise return -1.  Port of
    !> regex-emacs.c:re_wctype_parse.
    integer function re_wctype_parse(str, p, pend) result(cc)
        character(len=*), intent(in) :: str
        integer(int64), intent(inout) :: p
        integer(int64), intent(in) :: pend
        integer(int64) :: it, beg

        cc = -1
        if (pend - p < 4) return
        if (str(p:p + 1) /= "[:") return
        beg = p + 2
        it = beg

        do
            if (it + 1 >= pend) return
            if (str(it:it + 1) == ":]") exit
            it = it + 1
        end do

        p = it + 2
        cc = RECC_ERROR

        ! The name must match exactly: Fortran's comparison would pad the
        ! shorter string with blanks, upstream compares the length first.
        if (it > beg) then
            if (str(it - 1:it - 1) == " ") return
        end if

        select case (str(beg:it - 1))
        case ("word")
            cc = RECC_WORD
        case ("alnum")
            cc = RECC_ALNUM
        case ("alpha")
            cc = RECC_ALPHA
        case ("space")
            cc = RECC_SPACE
        case ("digit")
            cc = RECC_DIGIT
        case ("blank")
            cc = RECC_BLANK
        case ("upper")
            cc = RECC_UPPER
        case ("lower")
            cc = RECC_LOWER
        case ("punct")
            cc = RECC_PUNCT
        case ("ascii")
            cc = RECC_ASCII
        case ("graph")
            cc = RECC_GRAPH
        case ("print")
            cc = RECC_PRINT
        case ("cntrl")
            cc = RECC_CNTRL
        case ("xdigit")
            cc = RECC_XDIGIT
        case ("unibyte")
            cc = RECC_UNIBYTE
        case ("nonascii")
            cc = RECC_NONASCII
        case ("multibyte")
            cc = RECC_MULTIBYTE
        case default
            cc = RECC_ERROR
        end select
    end function re_wctype_parse

    !> True if CH is in the char class CC.  Port of
    !> regex-emacs.c:re_iswctype.
    logical function re_iswctype(ch, cc)
        integer(int32), intent(in) :: ch
        integer, intent(in) :: cc

        select case (cc)
        case (RECC_ALNUM)
            re_iswctype = isalnum(ch)
        case (RECC_ALPHA)
            re_iswctype = isalpha(ch)
        case (RECC_BLANK)
            re_iswctype = isblank(ch)
        case (RECC_CNTRL)
            re_iswctype = ch < 32
        case (RECC_DIGIT)
            re_iswctype = ch >= iachar("0") .and. ch <= iachar("9")
        case (RECC_GRAPH)
            re_iswctype = isgraph(ch)
        case (RECC_LOWER)
            re_iswctype = lowercasep(ch)
        case (RECC_PRINT)
            re_iswctype = isprint(ch)
        case (RECC_PUNCT)
            re_iswctype = ispunct(ch)
        case (RECC_SPACE)
            re_iswctype = isspace(ch)
        case (RECC_UPPER)
            re_iswctype = uppercasep(ch)
        case (RECC_XDIGIT)
            re_iswctype = char_hexdigit(ch) >= 0 .and. ch < 256
        case (RECC_ASCII)
            re_iswctype = ch < 128
        case (RECC_NONASCII)
            re_iswctype = ch >= 128
        case (RECC_UNIBYTE)
            re_iswctype = ch < 256
        case (RECC_MULTIBYTE)
            re_iswctype = ch >= 256
        case (RECC_WORD)
            re_iswctype = isword(ch)
        case default
            re_iswctype = .false.
        end select
    end function re_iswctype

    !> The range-table bits for multibyte characters of class CC.  Port of
    !> regex-emacs.c:re_wctype_to_bit.
    pure integer function re_wctype_to_bit(cc)
        integer, intent(in) :: cc

        select case (cc)
        case (RECC_NONASCII, RECC_MULTIBYTE)
            re_wctype_to_bit = BIT_MULTIBYTE
        case (RECC_ALPHA)
            re_wctype_to_bit = BIT_ALPHA
        case (RECC_ALNUM)
            re_wctype_to_bit = BIT_ALNUM
        case (RECC_WORD)
            re_wctype_to_bit = BIT_WORD
        case (RECC_LOWER)
            re_wctype_to_bit = BIT_LOWER
        case (RECC_UPPER)
            re_wctype_to_bit = BIT_UPPER
        case (RECC_PUNCT)
            re_wctype_to_bit = BIT_PUNCT
        case (RECC_SPACE)
            re_wctype_to_bit = BIT_SPACE
        case (RECC_GRAPH)
            re_wctype_to_bit = BIT_GRAPH
        case (RECC_PRINT)
            re_wctype_to_bit = BIT_PRINT
        case (RECC_BLANK)
            re_wctype_to_bit = BIT_BLANK
        case default
            re_wctype_to_bit = 0
        end select
    end function re_wctype_to_bit

    !> Compile PATTERN into `bufp', with `translate' (case folding) as set
    !> in `bufp'.  Unless `posix', the match succeeds the first time the
    !> end of the pattern is reached (no backtracking for a longer match).
    !> A nonempty `whitespace_regexp' replaces each run of spaces.  Port of
    !> regex-emacs.c:re_compile_pattern.
    subroutine re_compile_pattern(pattern, bufp, status, posix, whitespace_regexp)
        character(len=*), intent(in) :: pattern
        type(re_pattern_buffer_type), intent(inout) :: bufp
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: posix
        character(len=*), intent(in), optional :: whitespace_regexp
        integer :: err
        logical :: posix_backtracking

        status = status_type()
        posix_backtracking = .false.
        if (present(posix)) posix_backtracking = posix
        call regex_compile(pattern, posix_backtracking, whitespace_regexp, bufp, err)
        ! compile_pattern_1 signals (invalid-regexp MSG): the data print
        ! as a quoted string.
        if (err /= REG_NOERROR) call set_error(status, FM_INVALID_REGEXP, fortran_string(trim(re_error_msgid(err))))
    end subroutine re_compile_pattern

    !> Compile PATTERN into `bufp' (see re_compile_pattern); `err' is a
    !> REG_ code.  Port of regex-emacs.c:regex_compile.
    subroutine regex_compile(pattern, posix_backtracking, whitespace_regexp, bufp, err)
        character(len=*), intent(in) :: pattern
        logical, intent(in) :: posix_backtracking
        character(len=*), intent(in), optional :: whitespace_regexp
        type(re_pattern_buffer_type), intent(inout) :: bufp
        integer, intent(out) :: err
        character(len=:), allocatable :: src, main_src
        integer(int64) :: p, pend, main_p, main_pend, p1, p2, beg_interval
        integer(int32) :: c, c1, ch, clen
        integer :: b, pending_exact, laststart, begalt, fixup_alt_jump, regnum, reg, k
        integer :: lower_bound, upper_bound, nbytes, startoffset, cc, ofj, used, i, kp
        logical :: in_subpattern, normal, shy, zero_times_ok, many_times_ok, greedy, simple, emptyp
        logical :: has_ws
        ! The compile stack (compile_stack_type).
        integer, allocatable :: cs_begalt(:), cs_fixup(:), cs_laststart(:), cs_regnum(:)
        integer :: cs_avail
        ! The range table work area (struct range_table_work_area).
        integer(int32), allocatable :: rt(:)
        integer :: rt_used, rt_bits
        character(len=MAX_MULTIBYTE_LENGTH) :: cbuf
        type(status_type) :: st

        err = REG_NOERROR
        has_ws = .false.
        if (present(whitespace_regexp)) has_ws = len(whitespace_regexp) > 0
        allocate (cs_begalt(32), cs_fixup(32), cs_laststart(32), cs_regnum(32))
        cs_avail = 0
        allocate (rt(64))
        rt_used = 0
        rt_bits = 0
        bufp%fastmap_accurate = .false.
        bufp%used_syntax = .false.
        bufp%used = 0
        bufp%re_nsub = 0
        if (.not. allocated(bufp%buffer)) allocate (bufp%buffer(0:INIT_BUF_SIZE - 1))
        bufp%buffer = 0
        src = pattern
        p = 1
        pend = len(src, kind=int64) + 1
        in_subpattern = .false.
        b = 0
        begalt = 0
        pending_exact = -1
        laststart = -1
        fixup_alt_jump = -1
        beg_interval = -1
        main_p = 0
        main_pend = 0

        do
            if (err /= REG_NOERROR) return

            if (p == pend) then
                ! If this is the end of an included regexp, pop back to
                ! the main regexp and try again.
                if (in_subpattern) then
                    in_subpattern = .false.
                    src = main_src
                    p = main_p
                    pend = main_pend
                    cycle
                end if

                exit
            end if

            call patfetch(c)
            if (err /= REG_NOERROR) return
            normal = .false.

            select case (c)
            case (32)
                ! If there's no special whitespace regexp, treat spaces
                ! normally.  And don't try to do this recursively.
                if (.not. has_ws .or. in_subpattern) then
                    normal = .true.
                else
                    ! Peek past following spaces.
                    p1 = p

                    do while (p1 /= pend)
                        if (byte_of(p1) /= 32) exit
                        p1 = p1 + 1
                    end do

                    ! If the spaces are followed by a repetition op, treat
                    ! them normally.
                    if (p1 /= pend) then
                        select case (byte_of(p1))
                        case (iachar("*"), iachar("+"), iachar("?"))
                            normal = .true.
                        case (iachar("\"))
                            normal = p1 + 1 /= pend .and. byte_of(p1 + 1) == iachar("{")
                        case default
                            normal = .false.
                        end select
                    end if

                    if (.not. normal) then
                        ! Replace the spaces with the whitespace regexp.
                        in_subpattern = .true.
                        main_p = p1
                        main_pend = pend
                        main_src = src
                        src = whitespace_regexp
                        p = 1
                        pend = len(src, kind=int64) + 1
                    end if
                end if
            case (iachar("^"))
                normal = p /= 2
                if (normal) normal = .not. at_begline_loc_p(src, p)

                if (.not. normal) then
                    ! Special case for compatibility: postfix ops after ^
                    ! become literals.
                    laststart = -1
                    call buf_push(BEGLINE)
                end if
            case (iachar("$"))
                normal = p /= pend
                if (normal) normal = .not. at_endline_loc_p(src, p, pend)

                if (.not. normal) then
                    laststart = b
                    call buf_push(ENDLINE)
                end if
            case (iachar("+"), iachar("?"), iachar("*"))
                ! If there is no previous pattern...
                if (laststart < 0) then
                    normal = .true.
                else
                    call repetition()
                end if
            case (iachar("."))
                laststart = b
                call buf_push(ANYCHAR)
            case (iachar("["))
                call bracket()
            case (iachar("\"))
                if (p == pend) then
                    err = REG_EESCAPE
                    return
                end if

                ! Do not translate the character after the \, so that we
                ! can distinguish, e.g., \B from \b.
                call patfetch(c)
                if (err /= REG_NOERROR) return
                call backslash()
            case default
                normal = .true.
            end select

            if (err /= REG_NOERROR) return
            if (normal) call normal_char()
        end do

        ! Through the pattern now.
        call fixup_alt()

        if (cs_avail /= 0) then
            err = REG_EPAREN
            return
        end if

        ! If we don't want backtracking, force success the first time we
        ! reach the end of the compiled pattern.
        if (.not. posix_backtracking) call buf_push(SUCCEED)
        if (err /= REG_NOERROR) return
        bufp%used = b

    contains

        !> The byte at `i' of the pattern source, or 0 past its end.
        integer function byte_of(i)
            integer(int64), intent(in) :: i

            if (i >= 1 .and. i <= len(src, kind=int64)) then
                byte_of = ichar(src(i:i))
            else
                byte_of = 0
            end if
        end function byte_of

        !> Fetch the next character of the pattern (PATFETCH).
        subroutine patfetch(cc_out)
            integer(int32), intent(out) :: cc_out

            if (p == pend) then
                err = REG_EEND
                cc_out = 0
                return
            end if

            call string_char_and_length(src, p, cc_out, clen)
            p = p + clen
        end subroutine patfetch

        !> Ensure at least `n' more bytes of buffer space
        !> (GET_BUFFER_SPACE); too big a pattern is an error.
        subroutine buffer_space(n)
            integer, intent(in) :: n
            integer, allocatable :: grown(:)
            integer :: new_size

            if (size(bufp%buffer) - b >= n) return
            if (b + n > MAX_BUF_SIZE) err = REG_ESIZE
            new_size = max(2*size(bufp%buffer), b + n)
            allocate (grown(0:new_size - 1))
            grown = 0
            grown(0:size(bufp%buffer) - 1) = bufp%buffer
            call move_alloc(grown, bufp%buffer)
        end subroutine buffer_space

        !> Append a byte (BUF_PUSH).
        subroutine buf_push(byte)
            integer, intent(in) :: byte

            call buffer_space(1)
            bufp%buffer(b) = byte
            b = b + 1
        end subroutine buf_push

        !> Append two bytes (BUF_PUSH_2).
        subroutine buf_push_2(byte1, byte2)
            integer, intent(in) :: byte1, byte2

            call buffer_space(2)
            bufp%buffer(b) = byte1
            bufp%buffer(b + 1) = byte2
            b = b + 2
        end subroutine buf_push_2

        !> Store OP at LOC followed by the two-byte ARG (store_op1).
        subroutine store_op1(op, loc, arg)
            integer, intent(in) :: op, loc, arg

            bufp%buffer(loc) = op
            call store_number(bufp%buffer, loc + 1, arg)
        end subroutine store_op1

        !> Like store_op1, with two arguments (store_op2).
        subroutine store_op2(op, loc, arg1, arg2)
            integer, intent(in) :: op, loc, arg1, arg2

            bufp%buffer(loc) = op
            call store_number(bufp%buffer, loc + 1, arg1)
            call store_number(bufp%buffer, loc + 3, arg2)
        end subroutine store_op2

        !> Open up three bytes at LOC for OP and ARG (insert_op1).
        subroutine insert_op1(op, loc, arg)
            integer, intent(in) :: op, loc, arg

            bufp%buffer(loc + 3:b + 2) = bufp%buffer(loc:b - 1)
            call store_op1(op, loc, arg)
        end subroutine insert_op1

        !> Open up five bytes at LOC for OP, ARG1 and ARG2 (insert_op2).
        subroutine insert_op2(op, loc, arg1, arg2)
            integer, intent(in) :: op, loc, arg1, arg2

            bufp%buffer(loc + 5:b + 4) = bufp%buffer(loc:b - 1)
            call store_op2(op, loc, arg1, arg2)
        end subroutine insert_op2

        !> Store a jump with opcode OP at LOC to TO (STORE_JUMP).
        subroutine store_jump(op, loc, to)
            integer, intent(in) :: op, loc, to

            call store_op1(op, loc, to - loc - 3)
        end subroutine store_jump

        !> Store a two-argument jump (STORE_JUMP2).
        subroutine store_jump2(op, loc, to, arg)
            integer, intent(in) :: op, loc, to, arg

            call store_op2(op, loc, to - loc - 3, arg)
        end subroutine store_jump2

        !> Insert a jump at LOC to TO (INSERT_JUMP).
        subroutine insert_jump(op, loc, to)
            integer, intent(in) :: op, loc, to

            call insert_op1(op, loc, to - loc - 3)
        end subroutine insert_jump

        !> Insert a two-argument jump at LOC to TO (INSERT_JUMP2).
        subroutine insert_jump2(op, loc, to, arg)
            integer, intent(in) :: op, loc, to, arg

            call insert_op2(op, loc, to - loc - 3, arg)
        end subroutine insert_jump2

        !> Insert the jump from the end of the last alternative to here
        !> (FIXUP_ALT_JUMP).
        subroutine fixup_alt()
            if (fixup_alt_jump >= 0) call store_jump(JUMP, fixup_alt_jump, b)
        end subroutine fixup_alt

        !> Translate C if case folding (TRANSLATE).
        integer(int32) function tr(cc_in)
            integer(int32), intent(in) :: cc_in

            if (bufp%translate) then
                tr = case_canon(cc_in)
            else
                tr = cc_in
            end if
        end function tr

        !> Set the bit for character CC in the bitmap at `b'
        !> (SET_LIST_BIT).
        subroutine set_list_bit(cc_in)
            integer(int32), intent(in) :: cc_in

            bufp%buffer(b + cc_in/8) = ior(bufp%buffer(b + cc_in/8), ishft(1, mod(cc_in, 8)))
        end subroutine set_list_bit

        !> Add a range to the work area (SET_RANGE_TABLE_WORK_AREA).
        subroutine set_range_table_work_area(from, to)
            integer(int32), intent(in) :: from, to
            integer(int32), allocatable :: grown(:)

            if (rt_used + 2 > size(rt)) then
                allocate (grown(2*size(rt)))
                grown(1:rt_used) = rt(1:rt_used)
                call move_alloc(grown, rt)
            end if

            rt(rt_used + 1) = from
            rt(rt_used + 2) = to
            rt_used = rt_used + 2
        end subroutine set_range_table_work_area

        !> Both FROM and TO are ASCII characters (SETUP_ASCII_RANGE).
        subroutine setup_ascii_range(from, to)
            integer(int32), intent(in) :: from, to
            integer(int32) :: c0, cc1

            do c0 = from, to
                cc1 = tr(c0)

                if (.not. ascii_char_p(cc1)) then
                    call set_range_table_work_area(cc1, cc1)
                    cc1 = char_to_byte_safe(cc1)
                    if (cc1 < 0) cc1 = c0
                end if

                call set_list_bit(cc1)
            end do
        end subroutine setup_ascii_range

        !> Both FROM and TO are multibyte characters
        !> (SETUP_MULTIBYTE_RANGE).
        subroutine setup_multibyte_range(from, to)
            integer(int32), intent(in) :: from, to
            integer(int32) :: c0, cc1, cc2
            integer :: used0, ii
            logical :: merged

            used0 = rt_used
            call set_range_table_work_area(from, to)
            if (.not. bufp%translate) return

            do c0 = from, to
                cc1 = tr(c0)
                cc2 = char_to_byte_safe(cc1)
                if (cc2 < 0 .and. cc1 /= c0) cc2 = char_to_byte_safe(c0)
                if (cc2 >= 0) call set_list_bit(cc2)
                if (cc1 >= from .and. cc1 <= to) cycle
                merged = .false.

                do ii = rt_used - 1, used0 + 1, -2
                    if (cc1 >= rt(ii) - 1 .and. cc1 <= rt(ii + 1) + 1) then
                        if (cc1 == rt(ii) - 1) then
                            rt(ii) = rt(ii) - 1
                        else if (cc1 == rt(ii + 1) + 1) then
                            rt(ii + 1) = rt(ii + 1) + 1
                        end if

                        merged = .true.
                        exit
                    end if
                end do

                if (.not. merged) call set_range_table_work_area(cc1, cc1)
            end do
        end subroutine setup_multibyte_range

        !> The postfix operators + ? and *.
        subroutine repetition()
            ! 1 means zero (many) matches is allowed.
            zero_times_ok = .false.
            many_times_ok = .false.
            greedy = .true.

            ! If there is a sequence of repetition chars, collapse it down
            ! to just one (the right one).
            do
                if (c == iachar("?") .and. (zero_times_ok .or. many_times_ok)) then
                    greedy = .false.
                else
                    zero_times_ok = zero_times_ok .or. c /= iachar("+")
                    many_times_ok = many_times_ok .or. c /= iachar("?")
                end if

                if (p >= pend) exit
                if (.not. (byte_of(p) == iachar("*") .or. byte_of(p) == iachar("+") .or. &
                           byte_of(p) == iachar("?"))) exit
                ! If we get here, we found another repeat character.
                c = byte_of(p)
                p = p + 1
            end do

            ! Star, etc. applied to an empty pattern is equivalent to an
            ! empty pattern.
            if (laststart == b) return

            if (greedy) then
                if (many_times_ok) then
                    simple = skip_one_char(bufp%buffer, laststart) == b
                    startoffset = 0

                    ! Check if the loop can match the empty string.
                    if (simple) then
                        ofj = ON_FAILURE_JUMP
                    else if (.not. analyze_first(bufp, .false., laststart, b, .false.)) then
                        ofj = ON_FAILURE_JUMP
                    else
                        ofj = ON_FAILURE_JUMP_LOOP
                    end if

                    if (.not. zero_times_ok .and. simple) then
                        ! Since simple * loops can be made faster by using
                        ! on_failure_keep_string_jump, we turn P+ into PP*
                        ! if P is simple.
                        startoffset = b - laststart
                        call buffer_space(startoffset)
                        bufp%buffer(b:b + startoffset - 1) = bufp%buffer(laststart:b - 1)
                        b = b + startoffset
                        zero_times_ok = .true.
                    end if

                    call buffer_space(6)

                    if (.not. zero_times_ok) then
                        ! A + loop.
                        call store_jump(ofj, b, b + 6)
                    else if (simple) then
                        ! Simple * loops can use
                        ! on_failure_keep_string_jump depending on what
                        ! follows; on_failure_jump_smart decides.
                        call insert_jump(ON_FAILURE_JUMP_SMART, laststart + startoffset, b + 6)
                    else
                        call insert_jump(ofj, laststart + startoffset, b + 6)
                    end if

                    b = b + 3
                    call store_jump(JUMP, b, laststart + startoffset)
                    b = b + 3
                else
                    ! A simple ? pattern.
                    call buffer_space(3)
                    call insert_jump(ON_FAILURE_JUMP, laststart, b + 3)
                    b = b + 3
                end if
            else
                ! Not greedy.
                call buffer_space(7)

                if (many_times_ok) then
                    emptyp = analyze_first(bufp, .false., laststart, b, .false.)
                    ! The non-greedy multiple match looks like a
                    ! repeat..until: we only need a conditional jump at the
                    ! end of the loop.
                    if (emptyp) call buf_push(NO_OP)

                    if (emptyp) then
                        call store_jump(ON_FAILURE_JUMP_NASTYLOOP, b, laststart)
                    else
                        call store_jump(ON_FAILURE_JUMP, b, laststart)
                    end if

                    b = b + 3

                    if (zero_times_ok) then
                        ! To also match zero times, we need to first jump
                        ! to the end of the loop (its conditional jump).
                        call buffer_space(3)
                        call insert_jump(JUMP, laststart, b)
                        b = b + 3
                    end if
                else
                    ! Non-greedy a??.
                    call insert_jump(JUMP, laststart, b + 3)
                    b = b + 3
                    call insert_jump(ON_FAILURE_JUMP, laststart, laststart + 6)
                    b = b + 3
                end if
            end if

            pending_exact = -1
        end subroutine repetition

        !> A bracket expression [...].
        subroutine bracket()
            integer(int32) :: cc0

            rt_used = 0
            rt_bits = 0

            if (p == pend) then
                err = REG_EBRACK
                return
            end if

            ! Ensure that we have enough space to push a charset: the
            ! opcode, the length count, and the bitset; 34 bytes in all.
            call buffer_space(34)
            laststart = b

            if (byte_of(p) == iachar("^")) then
                call buf_push(CHARSET_NOT)
                p = p + 1
            else
                call buf_push(CHARSET)
            end if

            ! Remember the first position in the bracket expression.
            p1 = p
            ! Push the number of bytes in the bitmap and clear it.
            call buf_push(32)
            bufp%buffer(b:b + 31) = 0

            ! Read in characters and ranges, setting map bits.
            do
                p2 = p

                if (p == pend) then
                    err = REG_EBRACK
                    return
                end if

                ! See if we're at the beginning of a possible character
                ! class.
                cc = re_wctype_parse(src, p, pend)

                if (cc /= -1) then
                    if (cc == RECC_ERROR) then
                        err = REG_ECTYPE
                        return
                    end if

                    if (p == pend) then
                        err = REG_EBRACK
                        return
                    end if

                    do cc0 = 0, 127
                        if (re_iswctype(cc0, cc)) then
                            call set_list_bit(cc0)
                            c1 = tr(cc0)
                            if (c1 == cc0) cycle

                            if (ascii_char_p(c1)) then
                                call set_list_bit(c1)
                            else
                                c1 = char_to_byte_safe(c1)
                                if (c1 >= 0) call set_list_bit(c1)
                            end if
                        end if
                    end do

                    rt_bits = ior(rt_bits, re_wctype_to_bit(cc))
                    ! SPACE and WORD use the syntax table.
                    if (cc == RECC_SPACE .or. cc == RECC_WORD) bufp%used_syntax = .true.
                    cycle
                end if

                ! Don't translate yet.  The range TRANSLATE(X..Y) cannot
                ! always be determined from TRANSLATE(X) and TRANSLATE(Y).
                call patfetch(c)
                if (err /= REG_NOERROR) return
                ! Could be the end of the bracket expression.
                if (c == iachar("]") .and. p2 /= p1) exit

                ! hazard-ok(short-circuit): byte_of() checks its bounds.
                if (p < pend .and. byte_of(p) == iachar("-") .and. byte_of(p + 1) /= iachar("]")) then
                    ! Discard the '-' and fetch the character which ends
                    ! the range.
                    call patfetch(c1)
                    if (err /= REG_NOERROR) return
                    call patfetch(c1)
                    if (err /= REG_NOERROR) return
                    ! Treat the range from a multibyte character to
                    ! raw-byte character as empty.
                    if (char_byte8_p(c1) .and. .not. ascii_char_p(c) .and. .not. char_byte8_p(c)) c = c1 + 1
                else
                    ! Range from C to C.
                    c1 = c
                end if

                if (c <= c1) then
                    if (c < 128) then
                        ch = min(127, c1)
                        call setup_ascii_range(c, ch)
                        c = ch + 1
                        if (char_byte8_p(c1)) c = byte8_to_char(128)
                    end if

                    if (c <= c1) then
                        if (char_byte8_p(c)) then
                            do cc0 = char_to_byte8(c), char_to_byte8(c1)
                                call set_list_bit(cc0)
                            end do
                        else
                            call setup_multibyte_range(c, c1)
                        end if
                    end if
                end if
            end do

            ! Discard any (non)matching list bytes that are all 0 at the
            ! end of the map.  Decrease the map-length byte too.
            do while (bufp%buffer(b - 1) > 0)
                if (bufp%buffer(b + bufp%buffer(b - 1) - 1) /= 0) exit
                bufp%buffer(b - 1) = bufp%buffer(b - 1) - 1
            end do

            b = b + bufp%buffer(b - 1)

            ! Build real range table from work area.
            if (rt_used > 0 .or. rt_bits /= 0) then
                used = rt_used
                call buffer_space(4 + used*3)
                ! Indicate the existence of range table.
                bufp%buffer(laststart + 1) = ior(bufp%buffer(laststart + 1), 128)
                ! Store the character class flag bits into the range table.
                bufp%buffer(b) = iand(rt_bits, 255)
                bufp%buffer(b + 1) = ishft(rt_bits, -8)
                b = b + 2
                call store_number(bufp%buffer, b, used/2)
                b = b + 2

                do i = 1, used
                    call store_character(bufp%buffer, b, rt(i))
                    b = b + 3
                end do
            end if
        end subroutine bracket

        !> The character after a backslash, in `c'.
        subroutine backslash()
            select case (c)
            case (iachar("("))
                shy = .false.
                regnum = 0

                if (p + 1 < pend) then
                    ! Look for a special (?...) construct.
                    if (byte_of(p) == iachar("?")) then
                        ! Gobble up the '?'.
                        call patfetch(c)

                        do while (.not. shy)
                            call patfetch(c)
                            if (err /= REG_NOERROR) return

                            select case (c)
                            case (iachar(":"))
                                shy = .true.
                            case (iachar("0"):iachar("9"))
                                ! An explicitly specified regnum must start
                                ! with non-0.
                                if (c == iachar("0") .and. regnum == 0) then
                                    err = REG_BADPAT
                                    return
                                end if

                                if (regnum > REGNUM_LIMIT) then
                                    err = REG_ESIZE
                                    return
                                end if

                                regnum = regnum*10 + c - iachar("0")
                            case default
                                ! Only (?:...) is supported right now.
                                err = REG_BADPAT
                                return
                            end select
                        end do
                    end if
                end if

                if (.not. shy) then
                    bufp%re_nsub = bufp%re_nsub + 1
                    regnum = bufp%re_nsub
                else if (regnum /= 0) then
                    ! It's actually not shy, but explicitly numbered.
                    shy = .false.

                    if (regnum > bufp%re_nsub) then
                        bufp%re_nsub = regnum
                    else if (group_in_compile_stack(regnum)) then
                        err = REG_BADPAT
                        return
                    end if
                else
                    ! It's really shy.
                    regnum = -bufp%re_nsub
                end if

                if (cs_avail == size(cs_regnum)) call grow_compile_stack()
                ! These are the values to restore when we hit end of this
                ! group.
                cs_avail = cs_avail + 1
                cs_begalt(cs_avail) = begalt
                cs_fixup(cs_avail) = fixup_alt_jump
                cs_laststart(cs_avail) = b
                cs_regnum(cs_avail) = regnum
                ! Do not push a start_memory for groups beyond the last
                ! one we can represent in the compiled pattern.
                if (regnum <= MAX_REGNUM .and. regnum > 0) call buf_push_2(START_MEMORY, regnum)
                fixup_alt_jump = -1
                laststart = -1
                begalt = b
                pending_exact = -1
            case (iachar(")"))
                if (cs_avail == 0) then
                    err = REG_ERPAREN
                    return
                end if

                call fixup_alt()
                begalt = cs_begalt(cs_avail)
                fixup_alt_jump = cs_fixup(cs_avail)
                laststart = cs_laststart(cs_avail)
                reg = cs_regnum(cs_avail)
                cs_avail = cs_avail - 1
                pending_exact = -1
                ! We're at the end of the group, so now we know how many
                ! groups were inside this one.
                if (reg <= MAX_REGNUM .and. reg > 0) call buf_push_2(STOP_MEMORY, reg)
            case (iachar("|"))
                ! Insert before the previous alternative a jump which jumps
                ! to this alternative if the former fails.
                call buffer_space(3)
                call insert_jump(ON_FAILURE_JUMP, begalt, b + 6)
                pending_exact = -1
                b = b + 3
                ! The alternative before this one has a jump after it which
                ! gets executed if it gets matched.  Adjust that jump so it
                ! will jump to this alternative's analogous jump.
                call fixup_alt()
                ! Mark and leave space for a jump after this alternative.
                fixup_alt_jump = b
                call buffer_space(3)
                b = b + 3
                laststart = -1
                begalt = b
            case (iachar("{"))
                call interval()
            case (iachar("="))
                laststart = b
                call buf_push(AT_DOT)
            case (iachar("s"))
                laststart = b
                call patfetch(c)
                if (err /= REG_NOERROR) return
                call buf_push_2(SYNTAXSPEC, syntax_spec_code(c))
            case (iachar("S"))
                laststart = b
                call patfetch(c)
                if (err /= REG_NOERROR) return
                call buf_push_2(NOTSYNTAXSPEC, syntax_spec_code(c))
            case (iachar("c"))
                laststart = b
                call patfetch(c)
                if (err /= REG_NOERROR) return
                call buf_push_2(CATEGORYSPEC, iand(c, 255))
            case (iachar("C"))
                laststart = b
                call patfetch(c)
                if (err /= REG_NOERROR) return
                call buf_push_2(NOTCATEGORYSPEC, iand(c, 255))
            case (iachar("w"))
                laststart = b
                call buf_push_2(SYNTAXSPEC, SWORD)
            case (iachar("W"))
                laststart = b
                call buf_push_2(NOTSYNTAXSPEC, SWORD)
            case (iachar("<"))
                laststart = b
                call buf_push(WORDBEG)
            case (iachar(">"))
                laststart = b
                call buf_push(WORDEND)
            case (iachar("_"))
                laststart = b
                call patfetch(c)
                if (err /= REG_NOERROR) return

                if (c == iachar("<")) then
                    call buf_push(SYMBEG)
                else if (c == iachar(">")) then
                    call buf_push(SYMEND)
                else
                    err = REG_BADPAT
                end if
            case (iachar("b"))
                laststart = b
                call buf_push(WORDBOUND)
            case (iachar("B"))
                laststart = b
                call buf_push(NOTWORDBOUND)
            case (iachar("`"))
                ! Special case for compatibility: postfix ops after \`
                ! become literals, as for ^.
                laststart = -1
                call buf_push(BEGBUF)
            case (iachar("'"))
                laststart = b
                call buf_push(ENDBUF)
            case (iachar("1"):iachar("9"))
                reg = c - iachar("0")

                ! Can't back reference to a subexp before its end.
                ! hazard-ok(short-circuit): group_in_compile_stack() takes any number.
                if (reg > bufp%re_nsub .or. reg < 1 .or. group_in_compile_stack(reg)) then
                    err = REG_ESUBREG
                    return
                end if

                laststart = b
                call buf_push_2(DUPLICATE, reg)
            case default
                ! You might think it would be useful for \ to mean not to
                ! translate; but if we don't translate it it will never
                ! match anything.
                normal = .true.
            end select
        end subroutine backslash

        !> An interval \{M,N\}.
        subroutine interval()
            ! At least (most) this many matches must be made.
            lower_bound = 0
            upper_bound = -1
            beg_interval = p
            call get_interval_count(lower_bound)
            if (err /= REG_NOERROR) return

            if (c == iachar(",")) then
                call get_interval_count(upper_bound)
                if (err /= REG_NOERROR) return
            else
                ! Interval such as {1} => match exactly once.
                upper_bound = lower_bound
            end if

            if (lower_bound < 0 .or. (0 <= upper_bound .and. upper_bound < lower_bound) .or. c /= iachar("\")) then
                err = REG_BADBR
                return
            end if

            if (p == pend) then
                err = REG_EESCAPE
                return
            end if

            if (byte_of(p) /= iachar("}")) then
                err = REG_BADBR
                return
            end if

            p = p + 1

            ! We just parsed a valid interval.  If there is no preceding
            ! re, match the characters as literals.
            if (laststart < 0) then
                p = beg_interval
                beg_interval = -1
                c = iachar("{")
                normal = .true.
                return
            end if

            if (upper_bound == 0) then
                ! If the upper bound is zero, just drop the sub pattern
                ! altogether.
                b = laststart
            else if (lower_bound == 1 .and. upper_bound == 1) then
                ! Just match it once: nothing to do here.
                continue
            else
                ! Otherwise, we have a nontrivial interval.  When we're
                ! all done, the pattern will look like:
                !   set_number_at <jump count> <upper bound>
                !   set_number_at <succeed_n count> <lower bound>
                !   succeed_n <after jump addr> <succeed_n count>
                !   <body of loop>
                !   jump_n <succeed_n addr> <jump count>
                ! (The upper bound and 'jump_n' are omitted if
                ! 'upper_bound' is 1, though.)
                if (upper_bound < 0) then
                    nbytes = 3
                else if (upper_bound > 1) then
                    nbytes = 5
                else
                    nbytes = 0
                end if

                startoffset = 0
                call buffer_space(20)

                if (lower_bound == 0) then
                    ! A succeed_n that starts with 0 is really a simple
                    ! on_failure_jump_loop.
                    call insert_jump(ON_FAILURE_JUMP_LOOP, laststart, b + 3 + nbytes)
                    b = b + 3
                else
                    ! Initialize lower bound of the 'succeed_n', even
                    ! though it will be set during matching by its
                    ! attendant 'set_number_at' (inserted next), because
                    ! 're_compile_fastmap' needs to know.  Jump to the
                    ! 'jump_n' we might insert below.
                    call insert_jump2(SUCCEED_N, laststart, b + 5 + nbytes, lower_bound)
                    b = b + 5
                    ! Code to initialize the lower bound.  Insert before
                    ! the 'succeed_n'.
                    call insert_op2(SET_NUMBER_AT, laststart, 5, lower_bound)
                    b = b + 5
                    startoffset = startoffset + 5
                end if

                if (upper_bound < 0) then
                    ! A negative upper bound stands for infinity, in which
                    ! case it degenerates to a plain jump.
                    call store_jump(JUMP, b, laststart + startoffset)
                    b = b + 3
                else if (upper_bound > 1) then
                    ! More than one repetition is allowed, so append a
                    ! backward jump to the 'succeed_n' that starts this
                    ! interval.  When we've reached this during matching,
                    ! we'll have matched the interval once, so jump back
                    ! only 'upper_bound - 1' times.
                    call store_jump2(JUMP_N, b, laststart + startoffset, upper_bound - 1)
                    b = b + 5
                    ! Insert this at the beginning of the loop so that if
                    ! we fail during matching, we'll reinitialize the
                    ! bounds.
                    call insert_op2(SET_NUMBER_AT, laststart, b - laststart, upper_bound - 1)
                    b = b + 5
                end if
            end if

            pending_exact = -1
            beg_interval = -1
        end subroutine interval

        !> Get the next unsigned number in the pattern
        !> (GET_INTERVAL_COUNT).
        subroutine get_interval_count(num)
            integer, intent(inout) :: num

            if (p == pend) then
                err = REG_EBRACE
                return
            end if

            call patfetch(c)

            do while (c >= iachar("0") .and. c <= iachar("9"))
                if (num < 0) num = 0
                kp = 0
                if (RE_DUP_MAX_LAST_DIGIT < c - iachar("0")) kp = 1

                if (RE_DUP_MAX_TENTH - kp < num) then
                    err = REG_ESIZEBR
                    return
                end if

                num = num*10 + c - iachar("0")

                if (p == pend) then
                    err = REG_EBRACE
                    return
                end if

                call patfetch(c)
            end do
        end subroutine get_interval_count

        !> An ordinary character, in `c'.
        subroutine normal_char()
            logical :: new_exactn

            ! If no exactn currently being built, or the last one is not
            ! at the current position, or it is full, or the character is
            ! followed by a repetition operator: start a new exactn.
            new_exactn = pending_exact < 0

            if (.not. new_exactn) then
                new_exactn = pending_exact + bufp%buffer(pending_exact) + 1 /= b .or. &
                             bufp%buffer(pending_exact) >= 256 - MAX_MULTIBYTE_LENGTH
            end if

            if (.not. new_exactn .and. p /= pend) then
                new_exactn = byte_of(p) == iachar("*") .or. byte_of(p) == iachar("+") .or. &
                             byte_of(p) == iachar("?")
            end if

            if (.not. new_exactn .and. p + 1 < pend) then
                new_exactn = byte_of(p) == iachar("\") .and. byte_of(p + 1) == iachar("{")
            end if

            if (new_exactn) then
                laststart = b
                call buf_push_2(EXACTN, 0)
                pending_exact = b - 1
            end if

            call buffer_space(MAX_MULTIBYTE_LENGTH)
            c = tr(c)
            call char_string(c, cbuf, clen, st)

            do k = 1, clen
                bufp%buffer(b) = ichar(cbuf(k:k))
                b = b + 1
            end do

            bufp%buffer(pending_exact) = bufp%buffer(pending_exact) + clen
        end subroutine normal_char

        !> Whether REGNUM is an open group (group_in_compile_stack).
        logical function group_in_compile_stack(regnum_in)
            integer, intent(in) :: regnum_in

            group_in_compile_stack = any(cs_regnum(1:cs_avail) == regnum_in)
        end function group_in_compile_stack

        subroutine grow_compile_stack()
            integer, allocatable :: t1(:), t2(:), t3(:), t4(:)

            allocate (t1(2*cs_avail), t2(2*cs_avail), t3(2*cs_avail), t4(2*cs_avail))
            t1(1:cs_avail) = cs_begalt(1:cs_avail)
            t2(1:cs_avail) = cs_fixup(1:cs_avail)
            t3(1:cs_avail) = cs_laststart(1:cs_avail)
            t4(1:cs_avail) = cs_regnum(1:cs_avail)
            call move_alloc(t1, cs_begalt)
            call move_alloc(t2, cs_fixup)
            call move_alloc(t3, cs_laststart)
            call move_alloc(t4, cs_regnum)
        end subroutine grow_compile_stack
    end subroutine regex_compile

    !> P points just after a ^ in PATTERN.  Return true if that ^ comes
    !> after an alternative or a begin-subexpression.  Port of
    !> regex-emacs.c:at_begline_loc_p.
    logical function at_begline_loc_p(pattern, p)
        character(len=*), intent(in) :: pattern
        integer(int64), intent(in) :: p
        integer(int64) :: prev, q

        at_begline_loc_p = .false.
        prev = p - 2

        select case (pattern(prev:prev))
        case ("(", "|")
            ! After a subexpression or an alternative.
            continue
        case (":")
            ! After a shy subexpression: skip over optional regnum.
            do while (prev > 1)
                if (pattern(prev - 1:prev - 1) < "0" .or. pattern(prev - 1:prev - 1) > "9") exit
                prev = prev - 1
            end do

            if (.not. prev > 2) return
            if (pattern(prev - 1:prev - 1) /= "?" .or. pattern(prev - 2:prev - 2) /= "(") return
            prev = prev - 2
        case default
            return
        end select

        ! Count the number of preceding backslashes.
        q = prev

        do while (prev > 1)
            if (pattern(prev - 1:prev - 1) /= "\") exit
            prev = prev - 1
        end do

        at_begline_loc_p = mod(q - prev, 2_int64) == 1
    end function at_begline_loc_p

    !> The dual of at_begline_loc_p, for $.  Port of
    !> regex-emacs.c:at_endline_loc_p.
    logical function at_endline_loc_p(pattern, p, pend)
        character(len=*), intent(in) :: pattern
        integer(int64), intent(in) :: p, pend

        at_endline_loc_p = .false.
        if (pattern(p:p) /= "\" .or. p + 1 >= pend) return
        at_endline_loc_p = pattern(p + 1:p + 1) == ")" .or. pattern(p + 1:p + 1) == "|"
    end function at_endline_loc_p

    !> If the operation at P matches one or more characters, the next
    !> operation, else -1.  Port of regex-emacs.c:skip_one_char.
    pure integer function skip_one_char(buf, p) result(q)
        integer, intent(in) :: buf(0:)
        integer, intent(in) :: p
        integer :: mcnt

        q = p + 1

        select case (buf(p))
        case (ANYCHAR)
            continue
        case (EXACTN)
            q = q + buf(q) + 1
        case (CHARSET, CHARSET_NOT)
            if (charset_range_table_exists_p(buf, p)) then
                q = charset_range_table(buf, p)
                mcnt = extract_number(buf, q)
                q = q + 2 + mcnt*2*3
            else
                q = q + 1 + charset_bitmap_size(buf, p)
            end if
        case (SYNTAXSPEC, NOTSYNTAXSPEC, CATEGORYSPEC, NOTCATEGORYSPEC)
            q = q + 1
        case default
            q = -1
        end select
    end function skip_one_char

    !> Iterate through all the char-matching operations directly reachable
    !> from P; see forall_firstchar.  LOOP_BEG..LOOP_END delimit the
    !> current "block" of code.  Port of regex-emacs.c:forall_firstchar_1.
    recursive logical function forall_firstchar_1(bufp, p_in, pend, loop_beg_in, loop_end_in, data) result(safe)
        type(re_pattern_buffer_type), intent(inout) :: bufp
        integer, intent(in) :: p_in, pend, loop_beg_in, loop_end_in
        type(visit_data_type), intent(inout) :: data
        integer :: p, p_orig, newp1, newp2, tmp, p3, offset, loop_beg, loop_end
        logical :: twoway

        p = p_in
        loop_beg = loop_beg_in
        loop_end = loop_end_in

        do
            p_orig = p

            if (p == pend) then
                safe = .false.
                return
            else if (p == loop_end) then
                safe = .true.
                return
            else if (p > loop_end) then
                ! Broken assumption about the code shape.
                safe = .false.
                return
            end if

            twoway = .false.

            select case (bufp%buffer(p))
            case (NO_OP)
                p = p + 1
                cycle
            case (SUCCEED, EXACTN, CHARSET, CHARSET_NOT, ANYCHAR, SYNTAXSPEC, NOTSYNTAXSPEC, CATEGORYSPEC, &
                  NOTCATEGORYSPEC)
                ! Cases which stop the iteration.
                safe = visit(bufp, p, data)
                return
            case (AT_DOT, BEGBUF, WORDBOUND, NOTWORDBOUND, BEGLINE, ENDLINE, ENDBUF, WORDBEG, WORDEND, SYMBEG, &
                  SYMEND)
                ! Cases which may match the empty string.
                if (visit(bufp, p, data)) then
                    safe = .true.
                    return
                end if

                p = p + 1
                cycle
            case (JUMP, JUMP_N)
                newp1 = extract_address(bufp%buffer, p + 1)

                if (newp1 > p) then
                    ! Forward jump, boring.
                    p = newp1
                    cycle
                end if

                select case (bufp%buffer(newp1))
                case (ON_FAILURE_JUMP, ON_FAILURE_KEEP_STRING_JUMP, ON_FAILURE_JUMP_NASTYLOOP, &
                      ON_FAILURE_JUMP_LOOP, ON_FAILURE_JUMP_SMART, SUCCEED_N)
                    newp2 = extract_address(bufp%buffer, newp1 + 1)
                    twoway = .true.
                case default
                    ! "Safe" choice.
                    newp2 = loop_end
                end select
            case (ON_FAILURE_JUMP, ON_FAILURE_KEEP_STRING_JUMP, ON_FAILURE_JUMP_NASTYLOOP, ON_FAILURE_JUMP_LOOP, &
                  ON_FAILURE_JUMP_SMART)
                newp1 = extract_address(bufp%buffer, p + 1)
                newp2 = p + 3

                ! For `+` loops, we often have an `on_failure_jump` that
                ! skips forward over a subsequent `jump`.  Recognize this
                ! pattern since that subsequent `jump` is the one that
                ! jumps to the loop-entry.
                if (bufp%buffer(newp2) == JUMP) then
                    p3 = extract_address(bufp%buffer, newp2 + 1)
                    if (p3 > p_orig .or. newp1 > p_orig) newp2 = p3
                end if

                twoway = .true.
            case (SUCCEED_N)
                newp1 = extract_address(bufp%buffer, p + 1)
                ! Skip the two bytes containing the count.
                newp2 = p + 5
                twoway = .true.
            case (SET_NUMBER_AT)
                offset = extract_number(bufp%buffer, p + 1)

                ! If we're setting the counter of an immediately following
                ! `succeed_n`, then this next execution of `succeed_n` will
                ! do nothing but decrement its counter and "fall through".
                if (offset == 5 .and. bufp%buffer(p + 5) == SUCCEED_N) then
                    p = p + 10
                else
                    p = p + 5
                end if

                cycle
            case (START_MEMORY, STOP_MEMORY)
                p = p + 2
                cycle
            case (DUPLICATE)
                ! This could match the empty string, so we may need to
                ! continue, but in most cases, this can match "anything".
                if (.not. visit(bufp, p, data)) then
                    safe = .false.
                    return
                end if

                p = p + 2
                cycle
            case default
                safe = .false.
                return
            end select

            if (twoway) then
                ! We have to check that both destinations are safe.
                ! Arrange for `newp1` to be the smaller of the two.
                if (newp1 > newp2) then
                    tmp = newp1
                    newp1 = newp2
                    newp2 = tmp
                end if

                if (newp2 <= p_orig) then
                    ! Both destinations go backward!
                    safe = .false.
                    return
                end if

                if (.not. forall_firstchar_1(bufp, newp2, pend, loop_beg, loop_end, data)) then
                    safe = .false.
                    return
                end if
            end if

            ! do_jump:
            if (newp1 <= p_orig) then
                if (newp1 < loop_beg) then
                    safe = .false.
                    return
                else if (newp1 == loop_beg) then
                    ! If we jump backward to the entry point of the current
                    ! loop it means it's a zero-length cycle through that
                    ! loop; this cycle itself does not break safety.
                    safe = .true.
                    return
                else
                    ! We jump backward to a new loop, nested within the
                    ! current one.
                    loop_beg = newp1
                    loop_end = newp2
                end if
            end if

            p = newp1
        end do
    end function forall_firstchar_1

    !> Iterate through all the char-matching operations directly reachable
    !> from P.  Return true if P is "safe", meaning that PEND cannot be
    !> reached directly from P and all visits returned true.  With
    !> `whole', P is in the complete compiled pattern; otherwise P..PEND is
    !> a block being built.  PEND -1 is never reached.  Port of
    !> regex-emacs.c:forall_firstchar.
    recursive logical function forall_firstchar(bufp, whole, p, pend, data)
        type(re_pattern_buffer_type), intent(inout) :: bufp
        logical, intent(in) :: whole
        integer, intent(in) :: p, pend
        type(visit_data_type), intent(inout) :: data

        if (whole) then
            forall_firstchar = forall_firstchar_1(bufp, p, pend, -1, bufp%used + 1, data)
        else
            forall_firstchar = forall_firstchar_1(bufp, p, pend, p, pend, data)
        end if
    end function forall_firstchar

    !> The visitor of forall_firstchar, by `data%mode': analyze_first_null,
    !> analyze_first_fastmap or mutually_exclusive_one.  Not a port.
    recursive logical function visit(bufp, p, data)
        type(re_pattern_buffer_type), intent(inout) :: bufp
        integer, intent(in) :: p
        type(visit_data_type), intent(inout) :: data

        select case (data%mode)
        case (VISIT_NULL)
            visit = analyze_first_null(bufp%buffer, p)
        case (VISIT_FASTMAP)
            visit = analyze_first_fastmap(bufp, p, data)
        case default
            visit = mutually_exclusive_one(bufp, p, data)
        end select
    end function visit

    !> Port of regex-emacs.c:analyze_first_fastmap.
    logical function analyze_first_fastmap(bufp, p_in, data) result(ok)
        type(re_pattern_buffer_type), intent(inout) :: bufp
        integer, intent(in) :: p_in
        type(visit_data_type), intent(inout) :: data
        integer :: p, j, k, nbits, count, lc1, lc2
        integer(int32) :: c, length
        logical :: inverted

        p = p_in
        ok = .false.

        associate (buf => bufp%buffer, fastmap => bufp%fastmap)
            select case (buf(p))
            case (SUCCEED)
                ok = .false.
            case (DUPLICATE)
                ! If the first character has to match a backreference, the
                ! group was empty; build the fastmap from the rest.
                ok = .true.
            case (EXACTN)
                p = p + 1
                fastmap(buf(p + 1)) = .true.

                ! Cover the case of matching a raw char.
                if (char_byte8_head_p(int(buf(p + 1), int32))) then
                    call pchar(buf, p + 1, c, length)
                    fastmap(char_to_byte8(c)) = .true.
                end if

                ok = .true.
            case (ANYCHAR)
                ! We could put all the chars except for \n (and maybe \0)
                ! but we don't bother since it is generally not worth it.
                ok = .false.
            case (CHARSET, CHARSET_NOT)
                inverted = buf(p) == CHARSET_NOT

                if (inverted) then
                    ! Chars beyond end of bitmap are possible matches.
                    do j = charset_bitmap_size(buf, p)*8, 255
                        fastmap(j) = .true.
                    end do
                end if

                nbits = charset_bitmap_size(buf, p)*8

                do j = 0, nbits - 1
                    if (btest(buf(p + 2 + j/8), mod(j, 8)) .neqv. inverted) fastmap(j) = .true.
                end do

                ! To match raw bytes (in the 80..ff range) against
                ! multibyte strings, add their leading bytes to the
                ! fastmap.
                do j = 128, nbits - 1
                    if (btest(buf(p + 2 + j/8), mod(j, 8)) .neqv. inverted) then
                        fastmap(char_leading_code(byte8_to_char(j))) = .true.
                    end if
                end do

                if (inverted .or. (charset_range_table_exists_p(buf, p) .and. &
                                   charset_range_table_bits(buf, p) /= 0)) then
                    ! Any leading code can possibly start a character which
                    ! doesn't match the specified set of characters; if we
                    ! can match a character class, we can match any
                    ! multibyte characters.
                    if (.not. data%match_any_multibyte_characters) then
                        fastmap(MIN_MULTIBYTE_LEADING_CODE:MAX_MULTIBYTE_LEADING_CODE) = .true.
                        data%match_any_multibyte_characters = .true.
                    end if
                else if (.not. inverted .and. charset_range_table_exists_p(buf, p) .and. &
                         .not. data%match_any_multibyte_characters) then
                    ! Set fastmap[I] to 1 where I is a leading code of each
                    ! multibyte character in the range table.
                    k = charset_range_table(buf, p)
                    count = extract_number(buf, k)
                    k = k + 2

                    do j = 1, count
                        lc1 = char_leading_code(extract_character(buf, k))
                        lc2 = char_leading_code(extract_character(buf, k + 3))
                        fastmap(lc1:lc2) = .true.
                        k = k + 6
                    end do
                end if

                ok = .true.
            case (SYNTAXSPEC, NOTSYNTAXSPEC)
                ! This match depends on text properties.
                ok = .false.
            case (CATEGORYSPEC, NOTCATEGORYSPEC)
                inverted = buf(p) == NOTCATEGORYSPEC
                k = buf(p + 1)

                do j = 255, 0, -1
                    if (char_has_category(j, achar(k)) .neqv. inverted) fastmap(j) = .true.
                end do

                ! Any leading code can possibly start a character which has
                ! or doesn't have the specified category.
                if (.not. data%match_any_multibyte_characters) then
                    fastmap(MIN_MULTIBYTE_LEADING_CODE:MAX_MULTIBYTE_LEADING_CODE) = .true.
                    data%match_any_multibyte_characters = .true.
                end if

                ok = .true.
            case default
                ! at_dot, begbuf, ...: this false doesn't mean failure but
                ! rather "not succeeded yet".
                ok = .false.
            end select
        end associate
    end function analyze_first_fastmap

    !> Port of regex-emacs.c:analyze_first_null.
    pure logical function analyze_first_null(buf, p)
        integer, intent(in) :: buf(0:)
        integer, intent(in) :: p

        select case (buf(p))
        case (SUCCEED, DUPLICATE, EXACTN, ANYCHAR, CHARSET_NOT, CHARSET, SYNTAXSPEC, NOTSYNTAXSPEC, &
              CATEGORYSPEC, NOTCATEGORYSPEC)
            analyze_first_null = .true.
        case default
            analyze_first_null = .false.
        end select
    end function analyze_first_null

    !> If `with_fastmap', go through the pattern and fill the fastmap with
    !> all the possible leading chars.  Otherwise only return whether P to
    !> PEND could match the empty string.  Return false if P matches at
    !> least one char before reaching PEND.  Port of
    !> regex-emacs.c:analyze_first.
    recursive logical function analyze_first(bufp, whole, p, pend, with_fastmap)
        type(re_pattern_buffer_type), intent(inout) :: bufp
        logical, intent(in) :: whole, with_fastmap
        integer, intent(in) :: p, pend
        type(visit_data_type) :: data

        if (with_fastmap) then
            data%mode = VISIT_FASTMAP
        else
            data%mode = VISIT_NULL
        end if

        analyze_first = .not. forall_firstchar(bufp, whole, p, pend, data)
    end function analyze_first

    !> Compute the fastmap of the compiled pattern.  Port of
    !> regex-emacs.c:re_compile_fastmap.
    subroutine re_compile_fastmap(bufp)
        type(re_pattern_buffer_type), intent(inout) :: bufp

        bufp%fastmap = .false.
        bufp%fastmap_accurate = .true.
        bufp%can_be_null = analyze_first(bufp, bufp%used > 0, 0, bufp%used, .true.)
    end subroutine re_compile_fastmap

    !> Test if C matches the charset at P, and advance P past it.  C is
    !> the character to test (after translation), CORIG the original one.
    !> `unibyte' says C is a unibyte character.  Port of
    !> regex-emacs.c:execute_charset.
    logical function execute_charset(buf, p, c, corig, unibyte, canon) result(matched)
        integer, intent(in) :: buf(0:)
        integer, intent(inout) :: p
        integer(int32), intent(in) :: c, corig
        logical, intent(in) :: unibyte, canon
        integer :: rtp, count, class_bits, p0
        logical :: inverted

        p0 = p
        inverted = buf(p0) == CHARSET_NOT
        rtp = -1

        if (charset_range_table_exists_p(buf, p0)) then
            rtp = charset_range_table(buf, p0)
            count = extract_number(buf, rtp)
            rtp = rtp + 2
            p = rtp + count*2*3
        else
            p = p + 2 + charset_bitmap_size(buf, p0)
        end if

        if (unibyte .and. c < 256) then
            ! Lookup bitmap.
            if (c < charset_bitmap_size(buf, p0)*8) then
                if (btest(buf(p0 + 2 + c/8), mod(c, 8))) then
                    matched = .not. inverted
                    return
                end if
            end if
        else if (rtp >= 0) then
            class_bits = charset_range_table_bits(buf, p0)

            if (iand(class_bits, BIT_MULTIBYTE) /= 0 .or. class_match()) then
                matched = .not. inverted
                return
            end if

            do while (rtp < p)
                if (extract_character(buf, rtp) <= c .and. c <= extract_character(buf, rtp + 3)) then
                    matched = .not. inverted
                    return
                end if

                rtp = rtp + 6
            end do
        end if

        matched = inverted

    contains

        logical function class_match()
            class_match = .true.

            if (iand(class_bits, BIT_ALNUM) /= 0) then
                if (isalnum(c)) return
            end if

            if (iand(class_bits, BIT_ALPHA) /= 0) then
                if (isalpha(c)) return
            end if

            if (iand(class_bits, BIT_SPACE) /= 0) then
                if (isspace(c)) return
            end if

            if (iand(class_bits, BIT_BLANK) /= 0) then
                if (isblank(c)) return
            end if

            if (iand(class_bits, BIT_WORD) /= 0) then
                if (isword(c)) return
            end if

            if (iand(class_bits, BIT_UPPER) /= 0) then
                if (uppercasep(corig) .or. (canon .and. lowercasep(corig))) return
            end if

            if (iand(class_bits, BIT_LOWER) /= 0) then
                if (lowercasep(corig) .or. (canon .and. uppercasep(corig))) return
            end if

            if (iand(class_bits, BIT_PUNCT) /= 0) then
                if (ispunct(c)) return
            end if

            if (iand(class_bits, BIT_GRAPH) /= 0) then
                if (isgraph(c)) return
            end if

            if (iand(class_bits, BIT_PRINT) /= 0) then
                if (isprint(c)) return
            end if

            class_match = .false.
        end function class_match
    end function execute_charset

    !> Case where `p2' points to an `exactn' or `endline'.  Port of
    !> regex-emacs.c:mutually_exclusive_exactn.
    logical function mutually_exclusive_exactn(buf, p1, p2) result(excl)
        integer, intent(in) :: buf(0:)
        integer, intent(in) :: p1, p2
        integer(int32) :: c, c1, length
        integer :: q

        excl = .false.

        if (buf(p2) == ENDLINE) then
            c = 10
        else
            call pchar(buf, p2 + 2, c, length)
        end if

        if (buf(p1) == EXACTN) then
            call pchar(buf, p1 + 2, c1, length)
            if (c /= c1) excl = .true.
        else if (buf(p1) == CHARSET .or. buf(p1) == CHARSET_NOT) then
            q = p1
            if (.not. execute_charset(buf, q, c, c, ascii_char_p(c), .false.)) excl = .true.
        else if (buf(p1) == ANYCHAR .and. c == 10) then
            excl = .true.
        end if
    end function mutually_exclusive_exactn

    !> Case where `p2' points to a `charset'.  Port of
    !> regex-emacs.c:mutually_exclusive_charset.
    logical function mutually_exclusive_charset(buf, p1, p2) result(excl)
        integer, intent(in) :: buf(0:)
        integer, intent(in) :: p1, p2
        integer :: idx

        excl = .false.
        ! It is hard to list up all the character in charset P2 if it
        ! includes multibyte character.  Give up in such case.
        if (charset_range_table_exists_p(buf, p2)) return

        if (buf(p1) == CHARSET) then
            ! We win if the charset inside the loop has no overlap with
            ! the one after the loop.
            idx = 0

            do while (idx < buf(p2 + 1) .and. idx < charset_bitmap_size(buf, p1))
                if (iand(buf(p2 + 2 + idx), buf(p1 + 2 + idx)) /= 0) exit
                idx = idx + 1
            end do

            if (idx == buf(p2 + 1) .or. idx == charset_bitmap_size(buf, p1)) excl = .true.
        else if (buf(p1) == CHARSET_NOT) then
            ! We win if the charset_not inside the loop lists every
            ! character listed in the charset after.
            idx = 0

            do while (idx < buf(p2 + 1))
                if (buf(p2 + 2 + idx) /= 0) then
                    if (idx >= charset_bitmap_size(buf, p1)) exit
                    if (iand(buf(p2 + 2 + idx), not(buf(p1 + 2 + idx))) /= 0) exit
                end if

                idx = idx + 1
            end do

            if (idx == buf(p2 + 1)) excl = .true.
        end if
    end function mutually_exclusive_charset

    !> Port of regex-emacs.c:mutually_exclusive_one.
    logical function mutually_exclusive_one(bufp, p2, data) result(excl)
        type(re_pattern_buffer_type), intent(in) :: bufp
        integer, intent(in) :: p2
        type(visit_data_type), intent(inout) :: data

        associate (buf => bufp%buffer, p1 => data%p1)
            select case (buf(p2))
            case (SUCCEED)
                ! When N iterations of `p1` and N+1 iterations of `p1`
                ! match, the `succeed` after N+1 always takes precedence;
                ! this only holds if there is no other matching operator
                ! between the exit of the iteration and the `succeed`.
                excl = data%unconstrained
            case (ENDLINE)
                excl = constrain(mutually_exclusive_exactn(buf, p1, p2))
            case (EXACTN)
                excl = mutually_exclusive_exactn(buf, p1, p2)
            case (CHARSET)
                if (buf(p1) == EXACTN) then
                    excl = mutually_exclusive_exactn(buf, p2, p1)
                else
                    excl = mutually_exclusive_charset(buf, p1, p2)
                end if
            case (CHARSET_NOT)
                select case (buf(p1))
                case (EXACTN)
                    excl = mutually_exclusive_exactn(buf, p2, p1)
                case (CHARSET)
                    excl = mutually_exclusive_charset(buf, p2, p1)
                case default
                    ! When we have two charset_not, it's very unlikely that
                    ! they don't overlap.
                    excl = .false.
                end select
            case (ANYCHAR)
                excl = .false.
            case (SYNTAXSPEC)
                excl = buf(p1) == NOTSYNTAXSPEC .and. buf(p1 + 1) == buf(p2 + 1)
            case (NOTSYNTAXSPEC)
                excl = buf(p1) == SYNTAXSPEC .and. buf(p1 + 1) == buf(p2 + 1)
            case (CATEGORYSPEC)
                excl = buf(p1) == NOTCATEGORYSPEC .and. buf(p1 + 1) == buf(p2 + 1)
            case (NOTCATEGORYSPEC)
                excl = buf(p1) == CATEGORYSPEC .and. buf(p1 + 1) == buf(p2 + 1)
            case (ENDBUF)
                excl = .true.
            case (WORDBEG)
                excl = constrain(buf(p1) == NOTSYNTAXSPEC .and. buf(p1 + 1) == SWORD)
            case (WORDEND)
                excl = constrain(buf(p1) == SYNTAXSPEC .and. buf(p1 + 1) == SWORD)
            case (SYMBEG)
                excl = constrain(buf(p1) == NOTSYNTAXSPEC .and. (buf(p1 + 1) == SSYMBOL .or. buf(p1 + 1) == SWORD))
            case (SYMEND)
                excl = constrain(buf(p1) == SYNTAXSPEC .and. (buf(p1 + 1) == SSYMBOL .or. buf(p1 + 1) == SWORD))
            case (AT_DOT, BEGBUF, WORDBOUND, NOTWORDBOUND, BEGLINE)
                excl = constrain(.false.)
            case default
                ! duplicate: we know nothing about what this can match.
                excl = .false.
            end select
        end associate

    contains

        !> Remember that there may be an empty matching operator on the
        !> way (RETURN_CONSTRAIN).
        logical function constrain(v)
            logical, intent(in) :: v

            if (.not. v) data%unconstrained = .false.
            constrain = v
        end function constrain
    end function mutually_exclusive_one

    !> True if "p1 matches something" implies "p2 fails".  Port of
    !> regex-emacs.c:mutually_exclusive_p.
    recursive logical function mutually_exclusive_p(bufp, p1, p2)
        type(re_pattern_buffer_type), intent(inout) :: bufp
        integer, intent(in) :: p1, p2
        type(visit_data_type) :: data

        data%mode = VISIT_MUTEX
        data%p1 = p1
        data%unconstrained = .true.
        mutually_exclusive_p = forall_firstchar(bufp, .true., p2, -1, data)
    end function mutually_exclusive_p

    !> Using the compiled pattern, first try to match the subject starting
    !> at STARTPOS, then at STARTPOS + 1, and so on (back if RANGE is
    !> negative).  RANGE is how far to scan: the last start tried is
    !> STARTPOS + RANGE.  Don't consider matching past STOP.  Return the
    !> position at which the match was found, -1 if none, or -2 if the
    !> failure stack overflowed.  In REGS, return where the groups matched.
    !> Port of regex-emacs.c:re_search_2.
    recursive integer(int64) function re_search(bufp, subj, startpos_in, range_in, regs, stop) result(val)
        type(re_pattern_buffer_type), intent(inout) :: bufp
        type(re_subject_type), intent(in) :: subj
        integer(int64), intent(in) :: startpos_in, range_in, stop
        type(re_registers_type), intent(inout), optional :: regs
        integer(int64) :: startpos, range, total_size, endpos, irange, d
        integer(int32) :: buf_ch, buf_charlen, length
        logical :: anchored_start, skip

        startpos = startpos_in
        range = range_in
        total_size = subj%size
        endpos = startpos + range
        val = -1
        ! Check for out-of-range STARTPOS.
        if (startpos < 0 .or. startpos > total_size) return

        ! Fix up RANGE if it might eventually take us outside the subject.
        if (endpos < 0) then
            range = -startpos
        else if (endpos > total_size) then
            range = total_size - startpos
        end if

        ! If the search isn't to be a backwards one, don't waste time in a
        ! search for a pattern anchored at beginning of buffer.
        if (bufp%used > 0 .and. bufp%buffer(0) == BEGBUF .and. range > 0) then
            if (startpos > 0) return
            range = 0
        end if

        ! In a forward search for something that starts with \=, don't
        ! keep searching past point.
        if (bufp%used > 0 .and. bufp%buffer(0) == AT_DOT .and. range > 0 .and. subj%is_buffer) then
            range = buffers(cur)%pt_byte - subj%base - startpos
            if (range < 0) return
        end if

        ! Update the fastmap now if not correct already.
        if (.not. bufp%fastmap_accurate) call re_compile_fastmap(bufp)
        ! See whether the pattern is anchored.
        anchored_start = bufp%buffer(0) == BEGLINE

        ! Loop through the string, looking for a place to start matching.
        do
            skip = .false.

            ! If the pattern is anchored, skip quickly past places we
            ! cannot match.
            if (anchored_start .and. startpos > 0) then
                if (sbyte(subj, startpos - 1) /= 10) skip = .true.
            end if

            ! Skip quickly over characters that cannot be the start of a
            ! match.  If the pattern can match the null string, however,
            ! we don't need to skip characters; we want the first null
            ! string.
            if (.not. skip .and. startpos < total_size .and. .not. bufp%can_be_null) then
                d = startpos

                if (range > 0) then
                    ! Searching forwards.
                    irange = range

                    do while (range > 0)
                        call schar(subj, d, buf_ch, buf_charlen)
                        if (bufp%translate) buf_ch = case_canon(buf_ch)
                        if (bufp%fastmap(char_leading_code(buf_ch))) exit
                        range = range - buf_charlen
                        d = d + buf_charlen
                    end do

                    startpos = startpos + irange - range
                else
                    ! Searching backwards.
                    call schar(subj, d, buf_ch, length)
                    if (bufp%translate) buf_ch = case_canon(buf_ch)
                    if (.not. bufp%fastmap(char_leading_code(buf_ch))) skip = .true.
                end if
            end if

            if (.not. skip) then
                ! If can't match the null string, and that's all we have
                ! left, fail.
                if (range >= 0 .and. startpos == total_size .and. .not. bufp%can_be_null) return
                val = re_match_2_internal(bufp, subj, startpos, regs, stop)

                if (val >= 0) then
                    val = startpos
                    return
                end if

                if (val == -2) return
                val = -1
            end if

            ! advance:
            if (range == 0) then
                exit
            else if (range > 0) then
                ! Update STARTPOS to the next character boundary.
                length = bytes_by_char_head(int(sbyte(subj, startpos), int32))
                range = range - length
                if (range < 0) exit
                startpos = startpos + length
            else
                range = range + 1
                startpos = startpos - 1
                ! Update STARTPOS to the previous character boundary.
                d = startpos

                do while (d > 0 .and. startpos - d < MAX_MULTIBYTE_LENGTH - 1)
                    if (char_head_p(int(sbyte(subj, d), int32))) exit
                    d = d - 1
                end do

                length = int(startpos - d + 1, int32)
                range = range + length - 1
                if (range > 0) exit
                startpos = startpos - (length - 1)
            end if
        end do

        val = -1
    end function re_search

    !> Match the compiled pattern against the subject at POS, not past
    !> STOP.  Return the length of the match, -1 if none, or -2 if the
    !> failure stack overflowed.  In REGS, return where the groups
    !> matched.  Port of regex-emacs.c:re_match_2.
    recursive integer(int64) function re_match(bufp, subj, pos, regs, stop)
        type(re_pattern_buffer_type), intent(inout) :: bufp
        type(re_subject_type), intent(in) :: subj
        integer(int64), intent(in) :: pos, stop
        type(re_registers_type), intent(inout), optional :: regs

        re_match = re_match_2_internal(bufp, subj, pos, regs, stop)
    end function re_match

    !> The backtracking matcher.  Port of
    !> regex-emacs.c:re_match_2_internal.
    recursive integer(int64) function re_match_2_internal(bufp, subj, pos, regs, stop) result(retval)
        type(re_pattern_buffer_type), intent(inout) :: bufp
        type(re_subject_type), intent(in) :: subj
        integer(int64), intent(in) :: pos, stop
        type(re_registers_type), intent(inout), optional :: regs
        ! Where we are in the data, and where we were before an operator.
        integer(int64) :: d, dfail, d2, dend2, match_end, str
        ! Where we are in the pattern, and the end of the pattern.
        integer :: p, pend, pat, mcnt, num_regs, regno, op, p1, p2, p3
        ! The failure stack (fail_stack_type).
        integer(int64), allocatable :: fs(:)
        integer(int64) :: fs_avail, fs_frame
        ! Where each group matched (so far, and in the best match).
        integer(int64), allocatable :: regstart(:), regend(:), best_regstart(:), best_regend(:)
        integer(int32) :: c, c1, c2, len1, len2, pat_ch, pat_charlen, buf_ch, buf_charlen, corig
        integer :: s1, s2
        logical :: best_regs_set, best_match_p, do_fail, inverted, cycle_found, overflow, unibyte_char

        retval = -1
        pend = bufp%used
        num_regs = bufp%re_nsub + 1
        allocate (regstart(0:num_regs - 1), regend(0:num_regs - 1), best_regstart(0:num_regs - 1), &
                  best_regend(0:num_regs - 1))
        regstart = -1
        regend = -1
        best_regstart = -1
        best_regend = -1
        best_regs_set = .false.
        match_end = -1
        overflow = .false.
        allocate (fs(0:INIT_FAILURE_ALLOC*TYPICAL_FAILURE_SIZE - 1))
        fs_avail = 0
        fs_frame = 0
        p = 0
        d = pos

        ! This loops over pattern commands.  It exits by returning if the
        ! match is complete, or it drops through if the match fails at
        ! this starting point in the input data.
        main: do
            do_fail = .false.

            if (p == pend) then
                ! End of pattern means we might have succeeded.  If we
                ! haven't matched the entire string, and we want the
                ! longest match, try backtracking.
                if (d /= stop) then
                    ! True if this match is the best seen so far.
                    best_match_p = d > match_end

                    if (fs_frame /= 0) then
                        ! More failure points to try.  If exceeds best
                        ! match so far, save it.
                        if (.not. best_regs_set .or. best_match_p) then
                            best_regs_set = .true.
                            match_end = d
                            best_regstart(1:) = regstart(1:)
                            best_regend(1:) = regend(1:)
                        end if

                        do_fail = .true.
                    else if (best_regs_set .and. .not. best_match_p) then
                        ! If no failure points, don't restore garbage.  And
                        ! if last match is real best match, don't restore
                        ! second best one.
                        call restore_best_regs()
                    end if
                end if

                if (.not. do_fail) then
                    call accept()
                    return
                end if
            else
                op = bufp%buffer(p)
                p = p + 1

                select case (op)
                case (NO_OP)
                    ! Ignore these.  Used to ignore the n of succeed_n's
                    ! which currently have n == 0.
                    continue
                case (SUCCEED)
                    call accept()
                    return
                case (EXACTN)
                    ! Match the next n pattern characters exactly.
                    mcnt = bufp%buffer(p)
                    p = p + 1
                    ! Remember the start point to rollback upon failure.
                    dfail = d

                    do
                        if (d == stop) then
                            d = dfail
                            do_fail = .true.
                            exit
                        end if

                        call pchar(bufp%buffer, p, pat_ch, pat_charlen)
                        call schar(subj, d, buf_ch, buf_charlen)

                        if (tr(buf_ch) /= pat_ch) then
                            d = dfail
                            do_fail = .true.
                            exit
                        end if

                        p = p + pat_charlen
                        d = d + buf_charlen
                        mcnt = mcnt - pat_charlen
                        if (mcnt <= 0) exit
                    end do
                case (ANYCHAR)
                    ! Match any character except newline.
                    if (d == stop) then
                        do_fail = .true.
                    else
                        call schar(subj, d, buf_ch, buf_charlen)

                        if (tr(buf_ch) == 10) then
                            do_fail = .true.
                        else
                            d = d + buf_charlen
                        end if
                    end if
                case (CHARSET, CHARSET_NOT)
                    if (d == stop) then
                        do_fail = .true.
                    else
                        call schar(subj, d, corig, buf_charlen)
                        c = tr(corig)
                        c1 = char_to_byte_safe(c)
                        unibyte_char = c1 >= 0
                        if (unibyte_char) c = c1
                        p = p - 1

                        if (.not. execute_charset(bufp%buffer, p, c, corig, unibyte_char, bufp%translate)) then
                            do_fail = .true.
                        else
                            d = d + buf_charlen
                        end if
                    end if
                case (START_MEMORY)
                    ! In case we need to undo this operation (via
                    ! backtracking).
                    call push_failure_reg(bufp%buffer(p))
                    if (overflow) exit main
                    regstart(bufp%buffer(p)) = d
                    p = p + 1
                case (STOP_MEMORY)
                    regend(bufp%buffer(p)) = d
                    p = p + 1
                case (DUPLICATE)
                    ! Match a duplicate of something remembered.
                    regno = bufp%buffer(p)
                    p = p + 1

                    if (regend(regno) < 0) then
                        ! Can't back reference a group which we've never
                        ! matched.
                        do_fail = .true.
                    else
                        d2 = regstart(regno)
                        dend2 = regend(regno)
                        dfail = d

                        do while (d2 < dend2)
                            if (d == stop) then
                                d = dfail
                                do_fail = .true.
                                exit
                            end if

                            call schar(subj, d, c1, len1)
                            call schar(subj, d2, c2, len2)

                            if (tr(c1) /= tr(c2) .or. (.not. bufp%translate .and. len1 /= len2)) then
                                d = dfail
                                do_fail = .true.
                                exit
                            end if

                            d = d + len1
                            d2 = d2 + len2
                        end do

                        if (.not. do_fail .and. d > stop) then
                            d = dfail
                            do_fail = .true.
                        end if
                    end if
                case (BEGLINE)
                    ! begline matches the empty string at the beginning of
                    ! the string, and after newlines.
                    if (d /= 0) then
                        if (schar_before(subj, d) /= 10) do_fail = .true.
                    end if
                case (ENDLINE)
                    ! endline is the dual of begline.
                    if (d /= subj%size) then
                        if (sbyte(subj, d) /= 10) do_fail = .true.
                    end if
                case (BEGBUF)
                    ! Match at the very beginning of the data.
                    if (d /= 0) do_fail = .true.
                case (ENDBUF)
                    ! Match at the very end of the data.
                    if (d /= subj%size) do_fail = .true.
                case (ON_FAILURE_KEEP_STRING_JUMP)
                    ! Used to optimize '.*\n': pushes NULL as the value for
                    ! the string on the stack, so that failing back keeps
                    ! the current position in the string.
                    p = p + 2
                    call push_failure_point(p - 3, -1_int64)
                    if (overflow) exit main
                case (ON_FAILURE_JUMP_NASTYLOOP)
                    ! A nasty loop is introduced by the non-greedy *? and
                    ! +?: a special cycle-detection frame is pushed when
                    ! the failure point is *popped*, which marks the
                    ! beginning of one iteration through the loop.
                    p = p + 2
                    call check_infinite_loop(p - 4)
                    ! If there's a cycle, just continue without pushing
                    ! this failure point.
                    if (.not. cycle_found) call push_failure_point(p - 3, d)
                    if (overflow) exit main
                case (ON_FAILURE_JUMP_LOOP)
                    ! Simple loop detecting on_failure_jump: just check on
                    ! the failure stack if the same spot was already hit
                    ! earlier.
                    call on_failure_jump_loop_op()
                    if (overflow) exit main
                case (ON_FAILURE_JUMP)
                    ! Each alternative starts with an on_failure_jump that
                    ! points to the beginning of the next alternative;
                    ! repeats start with one that points past the
                    ! repetition.
                    p = p + 2
                    call push_failure_point(p - 3, d)
                    if (overflow) exit main
                case (ON_FAILURE_JUMP_SMART)
                    ! Compare the beginning of the repeat with what in the
                    ! pattern follows its end.  If nothing would match
                    ! both, use a non-backtracking loop based on
                    ! on_failure_keep_string_jump instead of
                    ! on_failure_jump.
                    mcnt = extract_number(bufp%buffer, p)
                    p = p + 2
                    p1 = p
                    p2 = p + mcnt
                    p3 = p - 3
                    ! Reset so that we will re-execute the instruction
                    ! once it's been changed.
                    p = p - 3
                    mcnt = extract_number(bufp%buffer, p2 - 2)

                    if (mutually_exclusive_p(bufp, p1, p2)) then
                        ! Use a fast 'on_failure_keep_string_jump' loop.
                        bufp%buffer(p3) = ON_FAILURE_KEEP_STRING_JUMP
                        call store_number(bufp%buffer, p2 - 2, mcnt + 3)
                    else
                        ! Default to a safe 'on_failure_jump' loop.
                        bufp%buffer(p3) = ON_FAILURE_JUMP
                    end if
                case (JUMP)
                    ! Unconditionally jump (without popping any failure
                    ! points).
                    call unconditional_jump()
                case (SUCCEED_N)
                    ! Have to succeed matching what follows at least n
                    ! times.  After that, handle like
                    ! 'on_failure_jump_loop'.
                    mcnt = extract_number(bufp%buffer, p + 2)

                    if (mcnt /= 0) then
                        mcnt = mcnt - 1
                        p = p + 4
                        call push_number(p - 2, mcnt)
                    else
                        call on_failure_jump_loop_op()
                    end if

                    if (overflow) exit main
                case (JUMP_N)
                    ! Originally, this is how many times we CAN jump.
                    mcnt = extract_number(bufp%buffer, p + 2)

                    if (mcnt /= 0) then
                        mcnt = mcnt - 1
                        call push_number(p + 2, mcnt)
                        if (overflow) exit main
                        call unconditional_jump()
                    else
                        ! If don't have to jump any more, skip over the
                        ! rest of command.
                        p = p + 4
                    end if
                case (SET_NUMBER_AT)
                    mcnt = extract_number(bufp%buffer, p)
                    p = p + 2
                    p2 = p + mcnt
                    mcnt = extract_number(bufp%buffer, p)
                    p = p + 2
                    call push_number(p2, mcnt)
                    if (overflow) exit main
                case (WORDBOUND, NOTWORDBOUND)
                    inverted = op == NOTWORDBOUND

                    ! Case 1: D is at the beginning or the end of string.
                    if (d == 0 .or. d == subj%size) then
                        inverted = .not. inverted
                    else
                        ! C1 is the character before D, S1 is the syntax of
                        ! C1, C2 is the character at D, and S2 is the
                        ! syntax of C2.
                        c1 = schar_before(subj, d)
                        s1 = subject_syntax(subj, d, -1_int64, c1)
                        call schar(subj, d, c2, len2)
                        s2 = subject_syntax(subj, d, 0_int64, c2)

                        ! Case 2: Only one of S1 and S2 is Sword.  Case 3:
                        ! Both are Sword, and WORD_BOUNDARY_P.
                        if ((s1 == SWORD) .neqv. (s2 == SWORD)) then
                            inverted = .not. inverted
                        else if (s1 == SWORD) then
                            if (word_boundary_p(c1, c2)) inverted = .not. inverted
                        end if
                    end if

                    if (.not. inverted) do_fail = .true.
                case (WORDBEG)
                    ! Case 1: D is at the end of string.
                    if (d == subj%size .or. d == stop) then
                        do_fail = .true.
                    else
                        call schar(subj, d, c2, len2)
                        s2 = subject_syntax(subj, d, 0_int64, c2)

                        ! Case 2: S2 is not Sword.
                        if (s2 /= SWORD) then
                            do_fail = .true.
                        else if (d /= 0) then
                            ! Case 3: D is not at the beginning of string
                            ! and S1 is Sword, and not WORD_BOUNDARY_P.
                            c1 = schar_before(subj, d)
                            s1 = subject_syntax(subj, d, -1_int64, c1)

                            if (s1 == SWORD) then
                                if (.not. word_boundary_p(c1, c2)) do_fail = .true.
                            end if
                        end if
                    end if
                case (WORDEND)
                    ! Case 1: D is at the beginning of string.
                    if (d == 0) then
                        do_fail = .true.
                    else
                        c1 = schar_before(subj, d)
                        s1 = subject_syntax(subj, d, -1_int64, c1)

                        ! Case 2: S1 is not Sword.
                        if (s1 /= SWORD) then
                            do_fail = .true.
                        else if (d /= subj%size) then
                            ! Case 3: D is not at the end of string, S2 is
                            ! Sword, and not WORD_BOUNDARY_P.
                            call schar(subj, d, c2, len2)
                            s2 = subject_syntax(subj, d, 0_int64, c2)

                            if (s2 == SWORD) then
                                if (.not. word_boundary_p(c1, c2)) do_fail = .true.
                            end if
                        end if
                    end if
                case (SYMBEG)
                    ! Case 1: D is at the end of string.
                    if (d == subj%size .or. d == stop) then
                        do_fail = .true.
                    else
                        call schar(subj, d, c2, len2)
                        s2 = subject_syntax(subj, d, 0_int64, c2)

                        ! Case 2: S2 is neither Sword nor Ssymbol.
                        if (s2 /= SWORD .and. s2 /= SSYMBOL) then
                            do_fail = .true.
                        else if (d /= 0) then
                            ! Case 3: D is not at the beginning of string
                            ! and S1 is Sword or Ssymbol.
                            c1 = schar_before(subj, d)
                            s1 = subject_syntax(subj, d, -1_int64, c1)
                            if (s1 == SWORD .or. s1 == SSYMBOL) do_fail = .true.
                        end if
                    end if
                case (SYMEND)
                    ! Case 1: D is at the beginning of string.
                    if (d == 0) then
                        do_fail = .true.
                    else
                        c1 = schar_before(subj, d)
                        s1 = subject_syntax(subj, d, -1_int64, c1)

                        ! Case 2: S1 is neither Ssymbol nor Sword.
                        if (s1 /= SWORD .and. s1 /= SSYMBOL) then
                            do_fail = .true.
                        else if (d /= subj%size) then
                            ! Case 3: D is not at the end of string and S2
                            ! is Sword or Ssymbol.
                            call schar(subj, d, c2, len2)
                            s2 = subject_syntax(subj, d, 0_int64, c2)
                            if (s2 == SWORD .or. s2 == SSYMBOL) do_fail = .true.
                        end if
                    end if
                case (SYNTAXSPEC, NOTSYNTAXSPEC)
                    inverted = op == NOTSYNTAXSPEC
                    mcnt = bufp%buffer(p)
                    p = p + 1

                    if (d == stop) then
                        do_fail = .true.
                    else
                        call schar(subj, d, c, len1)

                        if ((subject_syntax(subj, d, 0_int64, c) /= mcnt) .neqv. inverted) then
                            do_fail = .true.
                        else
                            d = d + len1
                        end if
                    end if
                case (AT_DOT)
                    if (.not. subj%is_buffer) then
                        do_fail = .true.
                    else if (subj%base + d /= buffers(cur)%pt_byte) then
                        do_fail = .true.
                    end if
                case (CATEGORYSPEC, NOTCATEGORYSPEC)
                    inverted = op == NOTCATEGORYSPEC
                    mcnt = bufp%buffer(p)
                    p = p + 1

                    if (d == stop) then
                        do_fail = .true.
                    else
                        call schar(subj, d, c, len1)

                        if ((.not. char_has_category(c, achar(mcnt))) .neqv. inverted) then
                            do_fail = .true.
                        else
                            d = d + len1
                        end if
                    end if
                case default
                    ! hazard-ok(stop): a corrupt compiled pattern (emacs_abort upstream).
                    error stop "regex_emacs_fm: bad opcode"
                end select
            end if

            if (.not. do_fail) cycle main

            ! We get here if a matching operation fails.
            fail: do
                if (fs_frame == 0) exit main
                ! A restart point is known.  Restore to that state.
                call pop_failure_point(str, pat)
                op = bufp%buffer(pat)
                pat = pat + 1

                select case (op)
                case (ON_FAILURE_KEEP_STRING_JUMP)
                    p = extract_address(bufp%buffer, pat)
                case (ON_FAILURE_JUMP_NASTYLOOP)
                    call push_failure_point(pat - 2, str)
                    if (overflow) exit main
                    d = str
                    p = extract_address(bufp%buffer, pat)
                case (ON_FAILURE_JUMP_LOOP, ON_FAILURE_JUMP, SUCCEED_N)
                    d = str
                    p = extract_address(bufp%buffer, pat)
                case (NO_OP)
                    ! A special frame used for nastyloops.
                    cycle fail
                case default
                    ! hazard-ok(stop): a corrupt failure stack (emacs_abort upstream).
                    error stop "regex_emacs_fm: bad failure point"
                end select

                exit fail
            end do fail
        end do main

        if (overflow) then
            retval = -2
            return
        end if

        if (best_regs_set) then
            call restore_best_regs()
            call accept()
        end if

    contains

        !> Translate C if case folding.
        integer(int32) function tr(cc)
            integer(int32), intent(in) :: cc

            if (bufp%translate) then
                tr = case_canon(cc)
            else
                tr = cc
            end if
        end function tr

        !> Restore the best match.
        subroutine restore_best_regs()
            d = match_end
            regstart(1:) = best_regstart(1:)
            regend(1:) = best_regend(1:)
        end subroutine restore_best_regs

        !> Accept the match: fill REGS and set the return value
        !> (succeed_label).
        subroutine accept()
            integer :: r

            if (present(regs)) then
                if (allocated(regs%starts)) deallocate (regs%starts)
                if (allocated(regs%ends)) deallocate (regs%ends)
                allocate (regs%starts(0:num_regs - 1), regs%ends(0:num_regs - 1))
                regs%starts(0) = pos
                regs%ends(0) = d

                do r = 1, num_regs - 1
                    if (regend(r) < 0) then
                        regs%starts(r) = -1
                        regs%ends(r) = -1
                    else
                        regs%starts(r) = regstart(r)
                        regs%ends(r) = regend(r)
                    end if
                end do
            end if

            retval = d - pos
        end subroutine accept

        !> Make room for `space' more items on the failure stack
        !> (ENSURE_FAIL_STACK).
        subroutine ensure_fail_stack(space)
            integer(int64), intent(in) :: space
            integer(int64), allocatable :: grown(:)
            integer(int64) :: new_size

            do while (size(fs, kind=int64) - fs_avail <= space)
                if (size(fs, kind=int64) >= EMACS_RE_MAX_FAILURES*TYPICAL_FAILURE_SIZE) then
                    overflow = .true.
                    return
                end if

                new_size = min(EMACS_RE_MAX_FAILURES*TYPICAL_FAILURE_SIZE, size(fs, kind=int64)*4)
                allocate (grown(0:new_size - 1))
                grown(0:fs_avail - 1) = fs(0:fs_avail - 1)
                call move_alloc(grown, fs)
            end do
        end subroutine ensure_fail_stack

        subroutine push_item(item)
            integer(int64), intent(in) :: item

            fs(fs_avail) = item
            fs_avail = fs_avail + 1
        end subroutine push_item

        integer(int64) function pop_item()
            fs_avail = fs_avail - 1
            pop_item = fs(fs_avail)
        end function pop_item

        !> Push register NUM onto the stack (PUSH_FAILURE_REG).
        subroutine push_failure_reg(num)
            integer, intent(in) :: num

            call ensure_fail_stack(3_int64)
            if (overflow) return
            call push_item(regstart(num))
            call push_item(regend(num))
            call push_item(int(num, int64))
        end subroutine push_failure_reg

        !> Change the counter at PTR to VAL, but make sure that it will be
        !> reset when backtracking (PUSH_NUMBER).
        subroutine push_number(ptr, val)
            integer, intent(in) :: ptr, val

            call ensure_fail_stack(3_int64)
            if (overflow) return
            call push_item(int(extract_number(bufp%buffer, ptr), int64))
            call push_item(int(ptr, int64))
            call push_item(-1_int64)
            call store_number(bufp%buffer, ptr, val)
        end subroutine push_number

        !> Pop a saved register or counter (POP_FAILURE_REG_OR_COUNT).
        subroutine pop_failure_reg_or_count()
            integer(int64) :: pfreg, ptr

            pfreg = pop_item()

            if (pfreg == -1) then
                ! It's a counter.
                ptr = pop_item()
                pfreg = pop_item()
                call store_number(bufp%buffer, int(ptr), int(pfreg))
            else
                regend(pfreg) = pop_item()
                regstart(pfreg) = pop_item()
            end if
        end subroutine pop_failure_reg_or_count

        !> Push the state we will need if we ever fail back to it
        !> (PUSH_FAILURE_POINT).
        subroutine push_failure_point(pattern, string_place)
            integer, intent(in) :: pattern
            integer(int64), intent(in) :: string_place

            call ensure_fail_stack(3_int64)
            if (overflow) return
            call push_item(fs_frame)
            call push_item(string_place)
            call push_item(int(pattern, int64))
            ! Close the frame by moving the frame pointer past it.
            fs_frame = fs_avail
        end subroutine push_failure_point

        !> Pop what push_failure_point pushed (POP_FAILURE_POINT).
        subroutine pop_failure_point(str_out, pat_out)
            integer(int64), intent(out) :: str_out
            integer, intent(out) :: pat_out

            ! Pop the saved registers.
            do while (fs_frame < fs_avail)
                call pop_failure_reg_or_count()
            end do

            pat_out = int(pop_item())
            ! If the saved string location is NULL, it came from an
            ! on_failure_keep_string_jump opcode, and we want to throw away
            ! the saved NULL, thus retaining our current position in the
            ! string.
            str_out = pop_item()
            if (str_out < 0) str_out = d
            fs_frame = pop_item()
        end subroutine pop_failure_point

        !> Check that we are not stuck in an infinite loop
        !> (CHECK_INFINITE_LOOP).
        subroutine check_infinite_loop(pat_cur)
            integer, intent(in) :: pat_cur
            integer(int64) :: failure

            cycle_found = .false.
            failure = fs_frame

            do while (failure > 0)
                if (.not. (fs(failure - 2) == d .or. fs(failure - 2) == -1)) exit

                if (fs(failure - 1) == pat_cur) then
                    cycle_found = .true.
                    exit
                end if

                failure = fs(failure - 3)
            end do
        end subroutine check_infinite_loop

        !> on_failure_jump_loop, also for a succeed_n whose count is 0.
        subroutine on_failure_jump_loop_op()
            mcnt = extract_number(bufp%buffer, p)
            p = p + 2
            call check_infinite_loop(p - 3)

            if (cycle_found) then
                ! If there's a cycle, get out of the loop, as if the
                ! matching had failed.
                p = p + mcnt
            else
                call push_failure_point(p - 3, d)
            end if
        end subroutine on_failure_jump_loop_op

        subroutine unconditional_jump()
            mcnt = extract_number(bufp%buffer, p)
            p = p + 2
            p = p + mcnt
        end subroutine unconditional_jump
    end function re_match_2_internal
end module regex_emacs_fm
