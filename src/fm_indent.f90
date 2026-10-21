! fm_indent.f90 -- columns and screen-line motion.
! Ported from GNU Emacs 31.1 src/indent.c.
!
! Copyright (C) 1985-1988, 1993-1995, 1998, 2000-2026 Free Software
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

!> The column of point and moving to a column, counted as the text is
!> displayed (tabs to the next tab stop, control characters as ^X, raw
!> bytes as \ooo, wide characters as two columns); and moving by screen
!> lines, the way the selected window shows continued lines.
!>
!> Not yet: invisible text, display properties, selective display,
!> horizontal scrolling of ordinary windows, and truncated lines.
module fm_indent
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use fm_buffer, only: buffers, cur, buf_fetch_char, buf_next_char_len
    use fm_marker, only: char_to_byte
    use fm_editfns, only: goto_char, pos_bol
    use fm_xdisp, only: glyph_columns
    use fm_frame, only: selected_frame
    implicit none(type, external)
    private

    public :: current_column, move_to_column, vertical_motion, screen_column, window_text_width

    !> Width when no window is set up (as in batch use).
    integer, parameter :: DEFAULT_WIDTH = 80

contains

    !> Columns from the start of the line to point.
    !> Port of indent.c:Fcurrent_column (current_column).
    integer(int64) function current_column() result(col)
        integer(int64) :: pos, byte
        integer(int32) :: c

        pos = pos_bol()
        byte = char_to_byte(pos)
        col = 0
        do while (pos < buffers(cur)%pt)
            c = buf_fetch_char(buffers(cur), byte)
            col = col + glyph_columns(c, int(col))
            byte = byte + buf_next_char_len(buffers(cur), byte)
            pos = pos + 1
        end do
    end function current_column

    !> Move point to column `column` in the current line: after a character
    !> that spans it, or to the end of the line if it is too short.
    !> `reached` is the column point ends up at.  Port of
    !> indent.c:Fmove_to_column (no FORCE).
    subroutine move_to_column(column, reached)
        integer(int64), intent(in) :: column
        integer(int64), intent(out) :: reached
        integer(int64) :: pos, byte
        integer(int32) :: c

        pos = pos_bol()
        byte = char_to_byte(pos)
        reached = 0
        do while (pos < buffers(cur)%zv .and. reached < column)
            c = buf_fetch_char(buffers(cur), byte)
            if (c == 10) exit
            reached = reached + glyph_columns(c, int(reached))
            byte = byte + buf_next_char_len(buffers(cur), byte)
            pos = pos + 1
        end do
        call goto_char(pos)
    end subroutine move_to_column

    !> The width available for text in the selected window: its columns
    !> less the last, which holds the continuation glyph.
    integer function window_text_width() result(width)
        if (selected_frame%minibuffer_selected) then
            ! The minibuffer scrolls horizontally instead of continuing.
            width = huge(0)/2
        else
            width = selected_frame%root_window%total_cols - 1
            if (width < 1) width = DEFAULT_WIDTH - 1
        end if
    end function window_text_width

    !> Screen rows of the line starting at `bol`, `width` columns wide:
    !> where each row starts, and where the line ends.  Rows continue as
    !> redisplay continues them (xdisp.c's display_line on a terminal).
    subroutine line_rows(bol, width, starts, nrows, eol)
        integer(int64), intent(in) :: bol
        integer, intent(in) :: width
        integer(int64), allocatable, intent(out) :: starts(:)
        integer, intent(out) :: nrows
        integer(int64), intent(out) :: eol
        integer(int64), allocatable :: grown(:)
        integer(int64) :: pos, byte
        integer(int32) :: c
        integer :: col, cont, n

        allocate (starts(8))
        nrows = 1
        starts(1) = bol
        pos = bol
        byte = char_to_byte(pos)
        col = 0
        cont = 0
        do
            if (pos >= buffers(cur)%zv) exit
            c = buf_fetch_char(buffers(cur), byte)
            if (c == 10) exit
            n = glyph_columns(c, col + cont)
            if (col + n > width .and. col > 0) then
                nrows = nrows + 1
                if (nrows > size(starts)) then
                    allocate (grown(2*size(starts)))
                    grown(1:size(starts)) = starts
                    call move_alloc(grown, starts)
                end if
                starts(nrows) = pos
                cont = cont + width
                col = 0
                cycle
            end if
            col = col + n
            byte = byte + buf_next_char_len(buffers(cur), byte)
            pos = pos + 1
        end do
        eol = pos
    end subroutine line_rows

    !> The screen row (1-based) of `pos` among `starts(1:nrows)`.
    pure integer function row_of(pos, starts, nrows) result(k)
        integer(int64), intent(in) :: pos
        integer(int64), intent(in) :: starts(:)
        integer, intent(in) :: nrows

        k = nrows
        do while (k > 1)
            if (starts(k) <= pos) exit
            k = k - 1
        end do
    end function row_of

    !> Point's column on its screen line (the cursor's x in the window).
    integer function screen_column() result(x)
        integer(int64), allocatable :: starts(:)
        integer(int64) :: eol, pos, byte
        integer(int32) :: c
        integer :: nrows, k, width, cont

        width = window_text_width()
        call line_rows(pos_bol(), width, starts, nrows, eol)
        k = row_of(buffers(cur)%pt, starts, nrows)
        cont = (k - 1)*width
        pos = starts(k)
        byte = char_to_byte(pos)
        x = 0
        do while (pos < buffers(cur)%pt)
            c = buf_fetch_char(buffers(cur), byte)
            x = x + glyph_columns(c, x + cont)
            byte = byte + buf_next_char_len(buffers(cur), byte)
            pos = pos + 1
        end do
    end function screen_column

    !> Move point `lines` screen lines down (up if negative), to the start
    !> of that screen line or, with `goal`, to the character at column
    !> `goal` on it (the last one on a continued row if it is shorter).
    !> `moved` is the number of screen lines moved, closer to zero at the
    !> beginning or end of the buffer.  Port of indent.c:Fvertical_motion
    !> for the selected window (LINES or (COLS . LINES); no CUR-COL).
    subroutine vertical_motion(lines, moved, goal)
        integer(int64), intent(in) :: lines
        integer(int64), intent(out) :: moved
        integer, intent(in), optional :: goal
        integer(int64), allocatable :: starts(:)
        integer(int64) :: bol, eol, target, pos, byte, row_end
        integer(int32) :: c
        integer :: nrows, width, x, n, cont

        width = window_text_width()
        bol = pos_bol()
        call line_rows(bol, width, starts, nrows, eol)
        target = row_of(buffers(cur)%pt, starts, nrows) + lines
        moved = lines
        ! Walk whole lines until the target row is in the current one.
        do while (target > nrows)
            if (eol >= buffers(cur)%zv) then
                moved = moved - (target - nrows)
                target = nrows
                exit
            end if
            target = target - nrows
            bol = eol + 1
            call line_rows(bol, width, starts, nrows, eol)
        end do
        do while (target < 1)
            if (bol <= buffers(cur)%begv) then
                moved = moved + (1 - target)
                target = 1
                exit
            end if
            call goto_char(bol - 1)
            bol = pos_bol()
            call line_rows(bol, width, starts, nrows, eol)
            target = target + nrows
        end do

        pos = starts(target)
        if (present(goal)) then
            if (target < nrows) then
                row_end = starts(target + 1)
            else
                row_end = eol
            end if
            cont = int(target - 1)*width
            byte = char_to_byte(pos)
            x = 0
            do while (pos < row_end)
                c = buf_fetch_char(buffers(cur), byte)
                n = glyph_columns(c, x + cont)
                if (x + n > goal) exit
                x = x + n
                byte = byte + buf_next_char_len(buffers(cur), byte)
                pos = pos + 1
            end do
            ! The end of a continued row belongs to the next one.
            if (target < nrows .and. pos == row_end) pos = row_end - 1
        end if
        call goto_char(pos)
    end subroutine vertical_motion

end module fm_indent
