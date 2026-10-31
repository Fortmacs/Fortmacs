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

!> The page-oriented commands: `forward_page' (C-x ]), `backward_page'
!> (C-x [), `mark_page' (C-x C-p), `narrow_to_page' (C-x n p, disabled),
!> `count_lines_page' (C-x l) and `what_page', pages being delimited by
!> lines whose beginning matches `page_delimiter'.
module page_fm
    use, intrinsic :: iso_fortran_env, only: int64
    use error_fm, only: status_type
    use buffer_fm, only: buffers, cur
    use editfns_fm, only: point, goto_char, point_min, point_max, bolp, bobp, eobp, eolp, widen, narrow_to_region
    use cmds_fm, only: forward_char, forward_line, beginning_of_line, end_of_line
    use search_fm, only: looking_at, re_search_forward, re_search_backward, match_beginning, match_end, &
                         set_match_data, SEARCH_NOERROR
    use subr_fm, only: looking_back
    use simple_fm, only: push_mark, count_lines
    use xdisp_fm, only: message
    use paragraphs_fm, only: page_delimiter, init_paragraph_variables
    use callint_fm, only: current_prefix_arg, prefix_numeric_value
    use lisp_fm, only: nilp
    use commands_fm, only: commands
    implicit none(type, external)
    private

    public :: forward_page, backward_page, mark_page, narrow_to_page, count_lines_page, what_page, syms_of_page, &
              page_count_lines_page, page_what_page

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

    !> Put mark at end of page, point at beginning.
    !> A numeric arg specifies to move forward or backward by that many pages,
    !> thus marking a page other than the one point was originally in.
    !> Port of page.el:mark-page (ARG 0 for nil).
    recursive subroutine mark_page(arg, status)
        integer(int64), intent(in) :: arg
        type(status_type), intent(out) :: status

        if (arg > 0) then
            call forward_page(status, arg)
        else if (arg < 0) then
            call forward_page(status, arg - 1)
        end if

        call forward_page(status)
        if (.not. status%ok()) return
        call push_mark(nomsg=.true., activate=.true.)
        call forward_page(status, -1_int64)
    end subroutine mark_page

    !> Make text outside current page invisible.
    !> A numeric arg specifies to move forward or backward by that many pages,
    !> thus showing a page other than the one point was originally in.
    !> Port of page.el:narrow-to-page (ARG 0 for nil).
    recursive subroutine narrow_to_page(arg, status)
        integer(int64), intent(in) :: arg
        type(status_type), intent(out) :: status
        integer(int64) :: saved, opoint, adjust, mb, e, shortage
        integer(int64), allocatable :: none(:)
        type(status_type) :: st
        logical :: at_page

        ! (save-excursion)
        saved = point()
        call widen()

        if (arg > 0) then
            call forward_page(status, arg)
        else if (arg < 0) then
            adjust = 0
            opoint = point()
            ! If we are not now at the beginning of a page,
            ! move back one extra time, to get to the start of this page.
            call beginning_of_line()
            at_page = looking_at(page_delimiter%value, st)
            if (at_page) at_page = match_end(0, st) == opoint
            if (.not. at_page) adjust = 1
            call goto_char(opoint)
            call forward_page(status, arg - adjust)
        end if

        ! Find the end of the page.
        allocate (none(0))
        call set_match_data(none)
        call forward_page(status)
        if (.not. status%ok()) return
        ! If we stopped due to end of buffer, stay there.
        ! If we stopped after a page delimiter, put end of restriction
        ! at the beginning of that line.
        ! Before checking the match that was found,
        ! verify that forward-page actually set the match data.
        mb = match_beginning(0, st)

        if (st%ok() .and. mb > 0) then
            opoint = point()
            call goto_char(mb)
            at_page = looking_at(page_delimiter%value, st)
            call goto_char(opoint)
            if (at_page) call goto_char(mb)
        end if

        e = point()
        ! Find the top of the page.
        call forward_page(status, -1_int64)
        if (.not. status%ok()) return
        ! If we found beginning of buffer, stay there.
        ! If extra text follows page delimiter on same line,
        ! include it.
        ! Otherwise, show text starting with following line.
        if (eolp() .and. .not. bobp()) shortage = forward_line(1_int64)
        call narrow_to_region(e, point(), status)
        call goto_char(min(max(saved, point_min()), point_max()))
    end subroutine narrow_to_page

    !> The line counts of the current page: TOTAL, BEFORE and AFTER point.
    !> Port of page.el:page--count-lines-page.
    subroutine page_count_lines_page(total, before, after)
        integer(int64), intent(out) :: total, before, after
        integer(int64) :: opoint, end
        type(status_type) :: st

        opoint = point()
        call forward_page(st)
        call beginning_of_line()
        if (.not. looking_at(page_delimiter%value, st)) call end_of_line()
        end = point()
        call backward_page(st)
        total = count_lines(point(), end)
        before = count_lines(point(), opoint)
        after = count_lines(opoint, end)
        call goto_char(opoint)
    end subroutine page_count_lines_page

    !> Report number of lines on current page, and how many are before or after point.
    !> Port of page.el:count-lines-page.
    recursive subroutine count_lines_page(status)
        type(status_type), intent(out) :: status
        integer(int64) :: total, before, after

        status = status_type()
        call page_count_lines_page(total, before, after)

        if (total == 1) then
            call message("Page has "//itoa(total)//" line ("//itoa(before)//" + "//itoa(after)//")")
        else
            call message("Page has "//itoa(total)//" lines ("//itoa(before)//" + "//itoa(after)//")")
        end if
    end subroutine count_lines_page

    !> The PAGE and LINE number of point (the line relative to the start
    !> of the page).  Port of page.el:page--what-page.
    subroutine page_what_page(page, line)
        integer(int64), intent(out) :: page, line
        integer(int64) :: opoint, begv, zv_from_end, adjust, found
        type(status_type) :: st

        ! (save-restriction (widen) (save-excursion ...))
        begv = buffers(cur)%begv
        zv_from_end = buffers(cur)%text%z - buffers(cur)%zv
        call widen()
        opoint = point()
        page = 1
        adjust = 0
        if (bolp()) adjust = 1
        if (adjust == 0 .and. looking_back(page_delimiter%value, st)) adjust = 1
        call goto_char(point_min())

        do
            call re_search_forward(page_delimiter%value, found, st, bound=opoint, noerror=SEARCH_NOERROR)
            if (.not. st%ok() .or. found <= 0) exit
            if (match_beginning(0, st) == match_end(0, st)) call forward_char(st)
            page = page + 1
        end do

        line = adjust + count_lines(point(), opoint)
        call goto_char(opoint)
        call narrow_to_region(begv, buffers(cur)%text%z - zv_from_end, st)
    end subroutine page_what_page

    !> Display the page number, and the line number within that page.
    !> Port of page.el:what-page.
    recursive subroutine what_page(status)
        type(status_type), intent(out) :: status
        integer(int64) :: page, line

        status = status_type()
        call page_what_page(page, line)
        call message("Page "//itoa(page)//", line "//itoa(line))
    end subroutine what_page

    ! N as a decimal string.
    ! Not a port.
    function itoa(n) result(s)
        integer(int64), intent(in) :: n
        character(len=:), allocatable :: s
        character(len=24) :: buf

        write (buf, "(i0)") n
        s = trim(buf)
    end function itoa

    recursive subroutine cmd_mark_page(status)
        type(status_type), intent(out) :: status
        integer(int64) :: n

        n = 0
        if (.not. nilp(current_prefix_arg)) n = prefix_numeric_value(current_prefix_arg)
        call mark_page(n, status)
    end subroutine cmd_mark_page

    recursive subroutine cmd_narrow_to_page(status)
        type(status_type), intent(out) :: status
        integer(int64) :: n

        n = 0
        if (.not. nilp(current_prefix_arg)) n = prefix_numeric_value(current_prefix_arg)
        call narrow_to_page(n, status)
    end subroutine cmd_narrow_to_page

    !> Register the commands.  Not a port.
    subroutine syms_of_page()
        commands%forward_page%run => cmd_forward_page
        commands%backward_page%run => cmd_backward_page
        commands%mark_page%run => cmd_mark_page
        commands%narrow_to_page%run => cmd_narrow_to_page
        commands%narrow_to_page%disabled = .true.
        commands%count_lines_page%run => count_lines_page
        commands%what_page%run => what_page
    end subroutine syms_of_page
end module page_fm
