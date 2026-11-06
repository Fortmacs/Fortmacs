! page_mod.f90 -- page motion commands for Emacs.
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
module page_mod
    use, intrinsic :: iso_fortran_env, only: int64
    use execute_interactively_mod, only: PREFIX_NONE
    use error_mod, only: status_type
    use buffer_mod, only: buffers, current
    use editing_functions_mod, only: point, goto_character, point_minimum, point_maximum, is_beginning_of_line, &
                                     is_beginning_of_buffer, is_end_of_buffer, is_end_of_line, widen, narrow_to_region
    use editing_commands_mod, only: forward_character, forward_line, beginning_of_line, end_of_line
    use search_mod, only: looking_at, regular_expression_search_forward, regular_expression_search_backward, match_beginning, &
                          match_end, set_match_data, SEARCH_NOERROR
    use basic_subprograms_mod, only: looking_back
    use simple_mod, only: push_mark, count_lines
    use display_engine_mod, only: message
    use paragraphs_mod, only: page_delimiter, initialize_paragraph_variables
    use execute_interactively_mod, only: current_prefix_argument, prefix_numeric_value
    use commands_mod, only: commands
    implicit none(type, external)
    private

    public :: forward_page, backward_page, mark_page, narrow_to_page, count_lines_page, what_page, initialize_page, &
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
        call initialize_paragraph_variables()
        n = 1
        if (present(count)) n = count

        do while (n > 0 .and. .not. is_end_of_buffer())
            at_delimiter = looking_at(page_delimiter%value, status)
            if (.not. status%ok()) return
            if (at_delimiter) at_delimiter = match_end(0, status) > point()

            if (at_delimiter) then
                ! If we're standing at the page delimiter, then just skip to
                ! the end of it.  (But only if it's not a zero-length
                ! delimiter, because then we wouldn't have forward progress.)
                call goto_character(match_end(0, status))
            else
                ! In case the page-delimiter matches the null string,
                ! don't find a match without moving.
                if (is_beginning_of_line()) call forward_character(status, 1_int64)
                if (.not. status%ok()) return
                call regular_expression_search_forward(page_delimiter%value, found, status, noerror=SEARCH_NOERROR)
                if (.not. status%ok()) return
                if (found == 0) call goto_character(point_maximum())
            end if

            n = n - 1
        end do

        do while (n < 0 .and. .not. is_beginning_of_buffer())
            ! In case the page-delimiter matches the null string,
            ! don't find a match without moving.
            here = point()
            call regular_expression_search_backward(page_delimiter%value, found, status, noerror=SEARCH_NOERROR)
            if (.not. status%ok()) return
            mb = 0
            me = 0

            if (found /= 0) then
                mb = match_beginning(0, status)
                me = match_end(0, status)
            end if

            call goto_character(here)

            if (found /= 0) then
                if (me == point()) call goto_character(mb)
            end if

            if (.not. is_beginning_of_buffer()) then
                call forward_character(status, -1_int64)
                if (.not. status%ok()) return
                call regular_expression_search_backward(page_delimiter%value, found, status, noerror=SEARCH_NOERROR)
                if (.not. status%ok()) return

                if (found /= 0) then
                    ! We found one--move to the end of it.
                    call goto_character(match_end(0, status))
                else
                    ! We found nothing--go to beginning of buffer.
                    call goto_character(point_minimum())
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

    recursive subroutine execute_forward_page(status)
        type(status_type), intent(out) :: status

        call forward_page(status, prefix_numeric_value(current_prefix_argument))
    end subroutine execute_forward_page

    recursive subroutine execute_backward_page(status)
        type(status_type), intent(out) :: status

        call backward_page(status, prefix_numeric_value(current_prefix_argument))
    end subroutine execute_backward_page

    !> Put mark at end of page, point at beginning.
    !> A numeric arg specifies to move forward or backward by that many pages,
    !> thus marking a page other than the one point was originally in.
    !> Port of page.el:mark-page (ARGUMENT 0 for no prefix).
    recursive subroutine mark_page(argument, status)
        integer(int64), intent(in) :: argument
        type(status_type), intent(out) :: status

        if (argument > 0) then
            call forward_page(status, argument)
        else if (argument < 0) then
            call forward_page(status, argument - 1)
        end if

        call forward_page(status)
        if (.not. status%ok()) return
        call push_mark(nomsg=.true., activate=.true.)
        call forward_page(status, -1_int64)
    end subroutine mark_page

    !> Make text outside current page invisible.
    !> A numeric arg specifies to move forward or backward by that many pages,
    !> thus showing a page other than the one point was originally in.
    !> Port of page.el:narrow-to-page (ARGUMENT 0 for no prefix).
    recursive subroutine narrow_to_page(argument, status)
        integer(int64), intent(in) :: argument
        type(status_type), intent(out) :: status
        integer(int64) :: saved, original_point, adjust, mb, e, shortage
        integer(int64), allocatable :: none(:)
        type(status_type) :: st
        logical :: at_page

        ! (save-excursion)
        saved = point()
        call widen()

        if (argument > 0) then
            call forward_page(status, argument)
        else if (argument < 0) then
            adjust = 0
            original_point = point()
            ! If we are not now at the beginning of a page,
            ! move back one extra time, to get to the start of this page.
            call beginning_of_line()
            at_page = looking_at(page_delimiter%value, st)
            if (at_page) at_page = match_end(0, st) == original_point
            if (.not. at_page) adjust = 1
            call goto_character(original_point)
            call forward_page(status, argument - adjust)
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
            original_point = point()
            call goto_character(mb)
            at_page = looking_at(page_delimiter%value, st)
            call goto_character(original_point)
            if (at_page) call goto_character(mb)
        end if

        e = point()
        ! Find the top of the page.
        call forward_page(status, -1_int64)
        if (.not. status%ok()) return
        ! If we found beginning of buffer, stay there.
        ! If extra text follows page delimiter on same line,
        ! include it.
        ! Otherwise, show text starting with following line.
        if (is_end_of_line() .and. .not. is_beginning_of_buffer()) shortage = forward_line(1_int64)
        call narrow_to_region(e, point(), status)
        call goto_character(min(max(saved, point_minimum()), point_maximum()))
    end subroutine narrow_to_page

    !> The line counts of the current page: TOTAL, BEFORE and AFTER point.
    !> Port of page.el:page--count-lines-page.
    subroutine page_count_lines_page(total, before, after)
        integer(int64), intent(out) :: total, before, after
        integer(int64) :: original_point, end
        type(status_type) :: st

        original_point = point()
        call forward_page(st)
        call beginning_of_line()
        if (.not. looking_at(page_delimiter%value, st)) call end_of_line()
        end = point()
        call backward_page(st)
        total = count_lines(point(), end)
        before = count_lines(point(), original_point)
        after = count_lines(original_point, end)
        call goto_character(original_point)
    end subroutine page_count_lines_page

    !> Report number of lines on current page, and how many are before or after point.
    !> Port of page.el:count-lines-page.
    recursive subroutine count_lines_page(status)
        type(status_type), intent(out) :: status
        integer(int64) :: total, before, after
        character(len=80) :: text

        status = status_type()
        call page_count_lines_page(total, before, after)

        if (total == 1) then
            write (text, '("Page has ", i0, " line (", i0, " + ", i0, ")")') total, before, after
        else
            write (text, '("Page has ", i0, " lines (", i0, " + ", i0, ")")') total, before, after
        end if

        call message(trim(text))
    end subroutine count_lines_page

    !> The PAGE and LINE number of point (the line relative to the start
    !> of the page).  Port of page.el:page--what-page.
    subroutine page_what_page(page, line)
        integer(int64), intent(out) :: page, line
        integer(int64) :: original_point, beginning_of_accessible_portion, end_of_accessible_portion_from_end, adjust, found
        type(status_type) :: st

        ! (save-restriction (widen) (save-excursion ...))
        beginning_of_accessible_portion = buffers(current)%beginning_of_accessible_portion
        end_of_accessible_portion_from_end = buffers(current)%text%z - buffers(current)%end_of_accessible_portion
        call widen()
        original_point = point()
        page = 1
        adjust = 0
        if (is_beginning_of_line()) adjust = 1
        if (adjust == 0 .and. looking_back(page_delimiter%value, st)) adjust = 1
        call goto_character(point_minimum())

        do
            call regular_expression_search_forward(page_delimiter%value, found, st, bound=original_point, noerror=SEARCH_NOERROR)
            if (.not. st%ok() .or. found <= 0) exit
            if (match_beginning(0, st) == match_end(0, st)) call forward_character(st)
            page = page + 1
        end do

        line = adjust + count_lines(point(), original_point)
        call goto_character(original_point)
        call narrow_to_region(beginning_of_accessible_portion, buffers(current)%text%z - end_of_accessible_portion_from_end, st)
    end subroutine page_what_page

    !> Display the page number, and the line number within that page.
    !> Port of page.el:what-page.
    recursive subroutine what_page(status)
        type(status_type), intent(out) :: status
        integer(int64) :: page, line
        character(len=64) :: text

        status = status_type()
        call page_what_page(page, line)
        write (text, '("Page ", i0, ", line ", i0)') page, line
        call message(trim(text))
    end subroutine what_page

    recursive subroutine execute_mark_page(status)
        type(status_type), intent(out) :: status
        integer(int64) :: n

        n = 0
        if (current_prefix_argument%kind /= PREFIX_NONE) n = prefix_numeric_value(current_prefix_argument)
        call mark_page(n, status)
    end subroutine execute_mark_page

    recursive subroutine execute_narrow_to_page(status)
        type(status_type), intent(out) :: status
        integer(int64) :: n

        n = 0
        if (current_prefix_argument%kind /= PREFIX_NONE) n = prefix_numeric_value(current_prefix_argument)
        call narrow_to_page(n, status)
    end subroutine execute_narrow_to_page

    !> Register the commands.  Not a port.
    subroutine initialize_page()
        commands%forward_page%execute => execute_forward_page
        commands%backward_page%execute => execute_backward_page
        commands%mark_page%execute => execute_mark_page
        commands%narrow_to_page%execute => execute_narrow_to_page
        commands%narrow_to_page%disabled = .true.
        commands%count_lines_page%execute => count_lines_page
        commands%what_page%execute => what_page
    end subroutine initialize_page
end module page_mod
