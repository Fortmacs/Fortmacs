! page_fm.f90 -- page motion commands for Emacs.
! Ported from GNU Emacs 31.1 lisp/textmodes/page.el.
!
! Copyright (C) 1985, 2001-2026 Free Software Foundation, Inc.
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

!> The page-oriented movement commands: `forward_page' (C-x ]) and
!> `backward_page' (C-x [), pages being delimited by lines whose
!> beginning matches `page_delimiter'.
!>
!> Deferred: `mark_page' (C-x C-p), `narrow_to_page' (C-x n p),
!> `count_lines_page' (C-x l) and `what_page'.
module page_fm
    use, intrinsic :: iso_fortran_env, only: int64
    use error_fm, only: status_type
    use editfns_fm, only: point, goto_char, point_min, point_max, bolp, bobp, eobp
    use cmds_fm, only: forward_char
    use search_fm, only: looking_at, re_search_forward, re_search_backward, match_beginning, match_end, &
                         SEARCH_NOERROR
    use paragraphs_fm, only: page_delimiter, init_paragraph_variables
    use callint_fm, only: current_prefix_arg, prefix_numeric_value
    use commands_fm, only: commands
    implicit none(type, external)
    private

    public :: forward_page, backward_page, syms_of_page

contains

    !> Move forward to page boundary.  With arg, repeat, or go back if negative.
    !> A page boundary is any line whose beginning matches the regexp
    !> `page_delimiter'.  Port of page.el:forward-page.
    recursive subroutine forward_page(status, count)
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: count
        integer(int64) :: n, here, mb, me, found
        logical :: at_delimiter
        status = status_type()
        call init_paragraph_variables()
        n = 1
        if (present(count)) n = count
        do while (n > 0 .and. .not. eobp())
            at_delimiter = looking_at(page_delimiter%value, status)
            if (.not. status%ok()) return
            if (at_delimiter) at_delimiter = match_end(0, status) > point()
            if (at_delimiter) then
                ! If we're standing at the page delimiter, then just skip to
                ! the end of it.  (But only if it's not a zero-length
                ! delimiter, because then we wouldn't have forward progress.)
                call goto_char(match_end(0, status))
            else
                ! In case the page-delimiter matches the null string,
                ! don't find a match without moving.
                if (bolp()) call forward_char(status, 1_int64)
                if (.not. status%ok()) return
                call re_search_forward(page_delimiter%value, found, status, noerror=SEARCH_NOERROR)
                if (.not. status%ok()) return
                if (found == 0) call goto_char(point_max())
            end if
            n = n - 1
        end do
        do while (n < 0 .and. .not. bobp())
            ! In case the page-delimiter matches the null string,
            ! don't find a match without moving.
            here = point()
            call re_search_backward(page_delimiter%value, found, status, noerror=SEARCH_NOERROR)
            if (.not. status%ok()) return
            mb = 0
            me = 0
            if (found /= 0) then
                mb = match_beginning(0, status)
                me = match_end(0, status)
            end if
            call goto_char(here)
            if (found /= 0) then
                if (me == point()) call goto_char(mb)
            end if
            if (.not. bobp()) then
                call forward_char(status, -1_int64)
                if (.not. status%ok()) return
                call re_search_backward(page_delimiter%value, found, status, noerror=SEARCH_NOERROR)
                if (.not. status%ok()) return
                if (found /= 0) then
                    ! We found one--move to the end of it.
                    call goto_char(match_end(0, status))
                else
                    ! We found nothing--go to beg of buffer.
                    call goto_char(point_min())
                end if
            end if
            n = n + 1
        end do
    end subroutine forward_page

    !> Move backward to page boundary.  With arg, repeat, or go fwd if negative.
    !> A page boundary is any line whose beginning matches the regexp
    !> `page_delimiter'.  Port of page.el:backward-page.
    recursive subroutine backward_page(status, count)
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: count
        integer(int64) :: n
        n = 1
        if (present(count)) n = count
        call forward_page(status, -n)
    end subroutine backward_page

    recursive subroutine cmd_forward_page(status)
        type(status_type), intent(out) :: status
        call forward_page(status, prefix_numeric_value(current_prefix_arg))
    end subroutine cmd_forward_page

    recursive subroutine cmd_backward_page(status)
        type(status_type), intent(out) :: status
        call backward_page(status, prefix_numeric_value(current_prefix_arg))
    end subroutine cmd_backward_page

    !> Register the commands.
    subroutine syms_of_page()
        commands%forward_page%run => cmd_forward_page
        commands%backward_page%run => cmd_backward_page
    end subroutine syms_of_page
end module page_fm
