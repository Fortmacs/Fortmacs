! ietf_drums_mod.f90 -- functions for RFC 822 and its successors.
! Ported from GNU Emacs 31.1 lisp/mail/ietf-drums.el (in part: see
! docs/status/ietf_drums_mod.md).
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

!> The parts of the DRUMS (RFC 822 and its successors) header syntax that
!> parsing MIME parameters needs: removing comments and whitespace from a
!> header field's value.  Upstream scans a temporary buffer with
!> `ietf-drums-syntax-table'; here the value is scanned as a string with
!> the same rules: a quoted string ends at the next double quote not
!> quoted by a backslash, and a comment is balanced over the parentheses,
!> brackets and angle brackets of that table.  mail-parse.el's names
!> (mail-header-remove-comments, mail-header-remove-whitespace) are
!> aliases, collapsed to these names.  What is not ported yet is listed
!> in docs/status/ietf_drums_mod.md.
module ietf_drums_mod
    implicit none(type, external)
    private

    public :: remove_ietf_drums_comments, remove_ietf_drums_whitespace, unfold_ietf_drums_folding_whitespace, &
              ietf_drums_string_end, ietf_drums_comment_end

    !> The special characters of RFC 2045 parameters.  Port of
    !> ietf-drums.el ietf-drums-tspecials.
    character(len=*), parameter, public :: IETF_DRUMS_TSPECIALS = "][()<>@,;:\""/?="

    character(len=*), parameter :: LF = achar(10), CR = achar(13), TAB = achar(9)

contains

    !> The position of the double quote that ends the quoted string
    !> starting at position START of STRING (a double quote), or 0 if the
    !> string is not terminated.  A backslash quotes the character after
    !> it.  Not a port: what forward-sexp does over a string with
    !> ietf-drums.el's ietf-drums-syntax-table.
    pure integer function ietf_drums_string_end(string, start) result(end)
        character(len=*), intent(in) :: string
        integer, intent(in) :: start
        integer :: q

        end = 0
        q = start + 1

        do while (q <= len(string))
            if (string(q:q) == "\") then
                q = q + 2
            else if (string(q:q) == '"') then
                end = q
                return
            else
                q = q + 1
            end if
        end do
    end function ietf_drums_string_end

    !> The position just after the comment starting at position START of
    !> STRING (an open parenthesis), balanced over parentheses, brackets
    !> and angle brackets; LEN(STRING) + 1 if it is not terminated.  A
    !> backslash quotes the character after it, and a double quote is an
    !> ordinary character.  Port of ietf-drums.el:ietf-drums--skip-comment
    !> (on a string).
    pure integer function ietf_drums_comment_end(string, start) result(end)
        character(len=*), intent(in) :: string
        integer, intent(in) :: start
        integer :: q, depth

        depth = 0
        q = start

        do while (q <= len(string))
            select case (string(q:q))
            case ("\")
                q = q + 1
            case ("(", "[", "<")
                depth = depth + 1
            case (")", "]", ">")
                depth = depth - 1

                if (depth == 0) then
                    end = q + 1
                    return
                end if
            case default
                ! Any other character is part of the comment.
                continue
            end select

            q = q + 1
        end do

        end = len(string) + 1
    end function ietf_drums_comment_end

    !> Unfold the folding white space of STRING: each run of spaces and
    !> tabs, a newline and more spaces and tabs becomes one space.
    !> Port of ietf-drums.el:ietf-drums-unfold-fws (on a string).
    pure function unfold_ietf_drums_folding_whitespace(string) result(unfolded)
        character(len=*), intent(in) :: string
        character(len=:), allocatable :: unfolded
        integer :: i, j, k, start

        unfolded = ""
        start = 1
        i = 1

        do while (i <= len(string))
            if (string(i:i) /= LF) then
                i = i + 1
                cycle
            end if

            ! The white space after the newline.
            j = verify(string(i + 1:), " "//TAB)

            if (j == 0) then
                j = len(string) + 1
            else
                j = i + j
            end if

            if (j == i + 1) then
                i = i + 1
                cycle
            end if

            ! The white space before it.
            k = start + verify(string(start:i - 1), " "//TAB, back=.true.)
            unfolded = unfolded//string(start:k - 1)//" "
            start = j
            i = j
        end do

        unfolded = unfolded//string(start:)
    end function unfold_ietf_drums_folding_whitespace

    !> Remove comments from STRING.  Port of
    !> ietf-drums.el:ietf-drums-remove-comments (upstream's
    !> mail-header-remove-comments).
    pure function remove_ietf_drums_comments(string) result(removed)
        character(len=*), intent(in) :: string
        character(len=:), allocatable :: removed
        character(len=:), allocatable :: text
        integer :: p, q

        text = unfold_ietf_drums_folding_whitespace(string)
        removed = ""
        p = 1

        do while (p <= len(text))
            select case (text(p:p))
            case ('"')
                q = ietf_drums_string_end(text, p)
                if (q == 0) q = len(text)
                removed = removed//text(p:q)
                p = q + 1
            case ("(")
                p = ietf_drums_comment_end(text, p)
            case default
                removed = removed//text(p:p)
                p = p + 1
            end select
        end do
    end function remove_ietf_drums_comments

    !> Remove whitespace from STRING.  Port of
    !> ietf-drums.el:ietf-drums-remove-whitespace (upstream's
    !> mail-header-remove-whitespace).
    pure function remove_ietf_drums_whitespace(string) result(removed)
        character(len=*), intent(in) :: string
        character(len=:), allocatable :: removed
        character(len=:), allocatable :: text
        integer :: p, q

        text = unfold_ietf_drums_folding_whitespace(string)
        removed = ""
        p = 1

        do while (p <= len(text))
            select case (text(p:p))
            case ('"')
                q = ietf_drums_string_end(text, p)
                if (q == 0) q = len(text)
                removed = removed//text(p:q)
                p = q + 1
            case ("(")
                q = ietf_drums_comment_end(text, p)
                removed = removed//text(p:q - 1)
                p = q
            case (" ", TAB, LF, CR)
                p = p + 1
            case default
                removed = removed//text(p:p)
                p = p + 1
            end select
        end do
    end function remove_ietf_drums_whitespace
end module ietf_drums_mod
