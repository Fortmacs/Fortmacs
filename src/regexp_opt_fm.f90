! regexp_opt_fm.f90 -- generate efficient regexps to match strings.
! Ported from GNU Emacs 31.1 lisp/emacs-lisp/regexp-opt.el.
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

!> `regexp_opt' returns a regexp matching any of a list of strings,
!> merging common prefixes and suffixes so the regexp matcher need not
!> backtrack; `regexp_opt_charset' a regexp matching any of a set of
!> characters; `regexp_opt_depth' the number of groups of a regexp.
!>
!> The strings are given as a character array whose elements' trailing
!> blanks are not part of the strings (a string cannot end with a space).
!> PAREN is one of the REGEXP_OPT_* constants, or the string OPEN.
module regexp_opt_fm
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use error_fm, only: status_type
    use search_fm, only: regexp_quote, string_match, match_beginning, match_end, match_data, set_match_data
    use subr_fm, only: is_subregexp_context
    use character_fm, only: char_string, MAX_MULTIBYTE_LENGTH
    implicit none(type, external)
    private

    public :: regexp_opt, regexp_opt_charset, regexp_opt_depth

    !> PAREN of `regexp_opt': REGEXP_OPT_SHY (a shy group if a postfix operator
    !> needs one), REGEXP_OPT_GROUP (a group), REGEXP_OPT_WORDS (\<\( ... \)\>) and REGEXP_OPT_IDENTIFIERS
    !> (\_<\( ... \)\_>).
    integer, parameter, public :: REGEXP_OPT_SHY = 0, REGEXP_OPT_GROUP = 1, REGEXP_OPT_WORDS = 2, REGEXP_OPT_IDENTIFIERS = 3

    !> A regexp that never matches anything.  Port of subr.el
    !> regexp-unmatchable.
    character(len=*), parameter, public :: regexp_unmatchable = "\`a\`"

    !> One string of a list.
    type :: str_type
        character(len=:), allocatable :: s
    end type str_type

contains

    !> Return the depth of REGEXP.
    !> This means the number of non-shy regexp grouping constructs
    !> (parenthesized expressions) in REGEXP.  Port of
    !> regexp-opt.el:regexp-opt-depth.
    recursive integer function regexp_opt_depth(regexp, status) result(count)
        character(len=*), intent(in) :: regexp
        type(status_type), intent(out) :: status
        integer(int64), allocatable :: saved(:)
        integer(int64) :: start, last, idx, mb
        logical :: context

        count = 0
        ! save-match-data
        saved = match_data()
        ! Hack to signal an error if REGEXP does not have balanced parentheses.
        idx = string_match(regexp, "", status)
        if (.not. status%ok()) return
        ! Count the number of open parentheses in REGEXP.
        start = 1
        last = 0

        do
            idx = string_match("\\(\(\?[0-9]*:\)?", regexp, status, start)
            if (.not. status%ok()) exit
            if (idx == 0) exit
            ! Start of next search.
            start = match_end(0, status)

            if (match_beginning(1, status) == 0) then
                mb = match_beginning(0, status)

                if (last > 0) then
                    context = is_subregexp_context(regexp, mb, status, last)
                else
                    context = is_subregexp_context(regexp, mb, status)
                end if

                if (.not. status%ok()) exit

                ! It's not a shy group and it's not inside brackets or after
                ! a backslash: it's really a group-open marker.
                if (context) then
                    ! Speed up next regexp-opt-re-context-p.
                    last = start
                    count = count + 1
                end if
            end if
        end do

        call set_match_data(saved)
    end function regexp_opt_depth

    !> Return a regexp to match a string in the list STRINGS.
    !> Each member of STRINGS is treated as a fixed string, not as a regexp.
    !> Optional PAREN specifies how the returned regexp is surrounded by
    !> grouping constructs.
    !>
    !> If STRINGS is the empty list, the return value is a regexp that
    !> never matches anything.
    !>
    !> The optional argument PAREN can be any of the following:
    !>
    !> OPEN, a string
    !>     the resulting regexp is preceded by OPEN and followed by
    !>     \), e.g.  use "\(?1:" to produce an explicitly numbered
    !>     group.
    !>
    !> REGEXP_OPT_WORDS
    !>     the resulting regexp is surrounded by \<\( and \)\>.
    !>
    !> REGEXP_OPT_IDENTIFIERS
    !>     the resulting regexp is surrounded by \_<\( and \)\_>.
    !>
    !> REGEXP_OPT_GROUP
    !>     the resulting regexp is surrounded by \( and \).
    !>
    !> REGEXP_OPT_SHY (the default)
    !>     the resulting regexp is surrounded by \(?: and \), if it is
    !>     necessary to ensure that a postfix operator appended to it will
    !>     apply to the whole expression.
    !>
    !> The returned regexp is ordered in such a way that it will always
    !> match the longest string possible.  Port of
    !> regexp-opt.el:regexp-opt.
    function regexp_opt(strings, paren, open) result(re)
        character(len=*), intent(in) :: strings(:)
        integer, intent(in), optional :: paren
        character(len=*), intent(in), optional :: open
        character(len=:), allocatable :: re
        type(str_type), allocatable :: list(:)
        character(len=:), allocatable :: op
        integer :: p
        logical :: has_open

        p = REGEXP_OPT_SHY
        if (present(paren)) p = paren
        has_open = .true.

        if (present(open)) then
            op = open
        else if (p /= REGEXP_OPT_SHY) then
            op = "\("
        else
            has_open = .false.
            op = ""
        end if

        call sorted_unique(strings, list)

        if (size(list) > 0) then
            if (has_open) then
                re = opt_group(list, .true., .false., op)
            else
                re = opt_group(list, .true., .true.)
            end if
        else
            if (has_open) then
                re = op//regexp_unmatchable//"\)"
            else
                re = "\(?:"//regexp_unmatchable//"\)"
            end if
        end if

        if (present(open)) return

        if (p == REGEXP_OPT_WORDS) then
            re = "\<"//re//"\>"
        else if (p == REGEXP_OPT_IDENTIFIERS) then
            re = "\_<"//re//"\_>"
        end if
    end function regexp_opt

    !> STRINGS sorted (string<) without duplicates.  Not a port.
    subroutine sorted_unique(strings, list)
        character(len=*), intent(in) :: strings(:)
        type(str_type), allocatable, intent(out) :: list(:)
        type(str_type) :: tmp
        integer :: i, j, n

        allocate (list(size(strings)))

        do i = 1, size(strings)
            list(i)%s = trim(strings(i))
        end do

        ! Insertion sort (the lists are short).
        do i = 2, size(list)
            call move_alloc(list(i)%s, tmp%s)
            j = i - 1

            do while (j >= 1)
                if (.not. llt(tmp%s, list(j)%s)) exit
                call move_alloc(list(j)%s, list(j + 1)%s)
                j = j - 1
            end do

            call move_alloc(tmp%s, list(j + 1)%s)
        end do

        n = 0

        do i = 1, size(list)
            if (n > 0) then
                if (list(i)%s == list(n)%s .and. len(list(i)%s) == len(list(n)%s)) cycle
            end if

            n = n + 1
            if (n /= i) list(n)%s = list(i)%s
        end do

        list = list(1:n)
    end subroutine sorted_unique

    !> The number of characters of S (UTF-8).  Not a port.
    pure integer function nchars(s)
        character(len=*), intent(in) :: s
        integer :: i

        nchars = 0

        do i = 1, len(s)
            if (iand(iachar(s(i:i)), 192) /= 128) nchars = nchars + 1
        end do
    end function nchars

    !> The length in bytes of the longest common prefix of the strings
    !> of LIST, ending on a character boundary (try-completion "").
    !> Not a port.
    integer function common_prefix(list) result(n)
        type(str_type), intent(in) :: list(:)
        integer :: i, k

        n = len(list(1)%s)

        do i = 2, size(list)
            n = min(n, len(list(i)%s))

            do k = 1, n
                if (list(i)%s(k:k) /= list(1)%s(k:k)) then
                    n = k - 1
                    exit
                end if
            end do
        end do

        ! Back off to a character boundary.
        do while (n > 0)
            if (n == len(list(1)%s)) exit
            if (iand(iachar(list(1)%s(n + 1:n + 1)), 192) /= 128) exit
            n = n - 1
        end do
    end function common_prefix

    !> The length in bytes of the longest common suffix of the strings of
    !> LIST, starting on a character boundary.  Not a port.
    integer function common_suffix(list) result(n)
        type(str_type), intent(in) :: list(:)
        integer :: i, k, l1, li

        l1 = len(list(1)%s)
        n = l1

        do i = 2, size(list)
            li = len(list(i)%s)
            n = min(n, li)

            do k = 1, n
                if (list(i)%s(li - k + 1:li - k + 1) /= list(1)%s(l1 - k + 1:l1 - k + 1)) then
                    n = k - 1
                    exit
                end if
            end do
        end do

        ! The suffix must start at a character boundary.
        do while (n > 0)
            if (iand(iachar(list(1)%s(l1 - n + 1:l1 - n + 1)), 192) /= 128) exit
            n = n - 1
        end do
    end function common_suffix

    !> Return a regexp to match a string in the sorted list STRINGS.
    !> If PAREN, output regexp parentheses around returned regexp (OPEN,
    !> or a shy group).  If LAX, don't output parentheses if it doesn't
    !> require them.  Merges keywords to avoid backtracking in Emacs's
    !> regexp matcher.  Port of regexp-opt.el:regexp-opt-group.
    recursive function opt_group(strings, paren, lax, open) result(re)
        type(str_type), intent(in) :: strings(:)
        logical, intent(in) :: paren, lax
        character(len=*), intent(in), optional :: open
        character(len=:), allocatable :: re
        character(len=:), allocatable :: open_group, close_group, open_charset, close_charset, prefix, xiffus, first
        ! (Results of recursive calls go through these first: gfortran
        ! garbles two such results in one concatenation.)
        character(len=:), allocatable :: sub1, sub2
        type(str_type), allocatable :: rest(:), parts(:)
        integer(int32), allocatable :: letters(:)
        integer :: i, n, nl, nr, k
        logical :: any_one

        if (present(open)) then
            open_group = open
        else if (paren) then
            open_group = "\(?:"
        else
            open_group = ""
        end if

        close_group = ""
        if (paren) close_group = "\)"
        open_charset = open_group
        close_charset = close_group

        if (lax) then
            open_charset = ""
            close_charset = ""
        end if

        if (size(strings) == 0) then
            re = ""
            return
        end if

        if (size(strings) == 1) then
            if (nchars(strings(1)%s) == 1) then
                re = open_charset//regexp_quote(strings(1)%s)//close_charset
            else
                re = open_group//regexp_quote(strings(1)%s)//close_group
            end if

            return
        end if

        if (len(strings(1)%s) == 0) then
            sub1 = opt_group(strings(2:), .true., .true.)
            re = open_charset//sub1//"?"//close_charset
            return
        end if

        any_one = .false.

        if (nchars(strings(1)%s) == 1) then
            do i = 2, size(strings)
                if (nchars(strings(i)%s) == 1) any_one = .true.
            end do
        end if

        if (any_one) then
            ! Collect one-char strings
            allocate (letters(0), rest(0))
            nl = 0
            nr = 0

            do i = 1, size(strings)
                if (nchars(strings(i)%s) == 1) then
                    letters = [letters, first_char(strings(i)%s)]
                else
                    rest = [rest, strings(i)]
                end if
            end do

            if (size(rest) > 0) then
                ! several one-char strings: take them and recurse
                ! on the rest (first so as to match the longest).
                sub1 = opt_group(rest, .false., .false.)
                re = open_group//sub1//"\|"//regexp_opt_charset(letters)//close_group
            else
                ! all are one-char strings: just return a character set.
                re = open_charset//regexp_opt_charset(letters)//close_charset
            end if

            return
        end if

        n = common_prefix(strings)

        if (n > 0) then
            ! common prefix: take it and recurse on the suffixes.
            prefix = strings(1)%s(1:n)
            allocate (parts(size(strings)))

            do i = 1, size(strings)
                parts(i)%s = strings(i)%s(n + 1:)
            end do

            sub1 = opt_group(parts, .true., .true.)
            re = open_group//regexp_quote(prefix)//sub1//close_group
            return
        end if

        n = common_suffix(strings)

        if (n > 0) then
            ! common suffix: take it and recurse on the prefixes.
            xiffus = strings(1)%s(len(strings(1)%s) - n + 1:)
            allocate (parts(size(strings)))

            do i = 1, size(strings)
                parts(i)%s = strings(i)%s(1:len(strings(i)%s) - n)
            end do

            ! Sorting is necessary in cases such as ("ad" "d").
            call sort_list(parts)
            sub1 = opt_group(parts, .true., .true.)
            re = open_group//sub1//regexp_quote(xiffus)//close_group
            return
        end if

        ! Otherwise, divide the list into those that start with a
        ! particular letter and those that do not, and recurse on them.
        first = char_prefix(strings(1)%s)
        k = 0

        do i = 1, size(strings)
            if (len(strings(i)%s) < len(first)) exit
            if (strings(i)%s(1:len(first)) /= first) exit
            k = i
        end do

        sub1 = opt_group(strings(1:k), .false., .false.)
        sub2 = opt_group(strings(k + 1:), .false., .false.)
        re = open_group//sub1//"\|"//sub2//close_group
    end function opt_group

    !> Sort LIST (string<), keeping duplicates.  Not a port.
    subroutine sort_list(list)
        type(str_type), intent(inout) :: list(:)
        type(str_type) :: tmp
        integer :: i, j

        do i = 2, size(list)
            call move_alloc(list(i)%s, tmp%s)
            j = i - 1

            do while (j >= 1)
                if (.not. llt(tmp%s, list(j)%s)) exit
                call move_alloc(list(j)%s, list(j + 1)%s)
                j = j - 1
            end do

            call move_alloc(tmp%s, list(j + 1)%s)
        end do
    end subroutine sort_list

    !> The first character of S, as a string.  Not a port.
    function char_prefix(s) result(c)
        character(len=*), intent(in) :: s
        character(len=:), allocatable :: c
        integer :: k

        k = 1

        do while (k < len(s))
            if (iand(iachar(s(k + 1:k + 1)), 192) /= 128) exit
            k = k + 1
        end do

        c = s(1:k)
    end function char_prefix

    !> The code point of the first character of S (UTF-8).  Not a port.
    integer(int32) function first_char(s) result(c)
        character(len=*), intent(in) :: s
        integer :: b, n, k

        b = iachar(s(1:1))

        if (b < 128) then
            c = b
            return
        else if (b >= 240) then
            c = iand(b, 7)
            n = 3
        else if (b >= 224) then
            c = iand(b, 15)
            n = 2
        else
            c = iand(b, 31)
            n = 1
        end if

        do k = 1, n
            if (k + 1 > len(s)) exit
            c = ior(ishft(c, 6), iand(iachar(s(k + 1:k + 1)), 63))
        end do
    end function first_char

    !> The UTF-8 string of character C.  Not a port.
    function utf8(c) result(s)
        integer(int32), intent(in) :: c
        character(len=:), allocatable :: s
        character(len=MAX_MULTIBYTE_LENGTH) :: buf
        integer(int32) :: nbytes
        type(status_type) :: st

        call char_string(c, buf, nbytes, st)
        s = buf(1:nbytes)
    end function utf8

    !> Return a regexp to match a character in CHARS.
    !> CHARS should be a list of characters.
    !> If CHARS is the empty list, the return value is a regexp that
    !> never matches anything.  Port of regexp-opt.el:regexp-opt-charset.
    function regexp_opt_charset(chars) result(re)
        integer(int32), intent(in) :: chars(:)
        character(len=:), allocatable :: re
        integer(int32), allocatable :: set(:)
        integer(int32) :: start, end, c, tmp
        character(len=:), allocatable :: charset, bracket, dash, caret, whole
        integer :: i, j

        bracket = ""
        dash = ""
        caret = ""
        allocate (set(0))

        do i = 1, size(chars)
            c = chars(i)

            if (c == iachar("]")) then
                bracket = "]"
            else if (c == iachar("^")) then
                caret = "^"
            else if (c == iachar("-")) then
                dash = "-"
            else if (all(set /= c)) then
                set = [set, c]
            end if
        end do

        ! In increasing order (as map-char-table walks the table).
        do i = 2, size(set)
            tmp = set(i)
            j = i - 1

            do while (j >= 1)
                if (set(j) <= tmp) exit
                set(j + 1) = set(j)
                j = j - 1
            end do

            set(j + 1) = tmp
        end do

        start = -1
        end = -2
        charset = ""

        do i = 1, size(set)
            c = set(i)

            if (c - 1 == end) then
                end = c
            else
                call flush ()
                start = c
                end = c
            end if
        end do

        call flush ()
        whole = bracket//charset//caret//dash

        if (len(whole) == 0) then
            re = regexp_unmatchable
        else if (nchars(whole) == 1) then
            re = regexp_quote(whole)
        else if (whole == "^-") then
            re = "[-^]"
        else
            re = "["//whole//"]"
        end if

    contains

        subroutine flush ()
            if (end > start + 2) then
                charset = charset//utf8(start)//"-"//utf8(end)
            else
                do while (end >= start)
                    charset = charset//utf8(start)
                    start = start + 1
                end do
            end if
        end subroutine flush
    end function regexp_opt_charset
end module regexp_opt_fm
