! indent_fm.f90 -- columns and screen-line motion.
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
module indent_fm
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use buffer_fm, only: buffers, cur, buf_fetch_char, buf_next_char_len
    use marker_fm, only: char_to_byte
    use error_fm, only: status_type
    use lisp_fm, only: value_type
    use data_fm, only: logical_setting, integer_setting, setting_type
    use editfns_fm, only: goto_char, pos_bol, point, insert_char_n
    use insdel_fm, only: del_range
    use xdisp_fm, only: glyph_columns, tab_width
    use frame_fm, only: selected_frame, frame_selected_window_ref
    use window_fm, only: window_type, window_text_cols
    implicit none(type, external)
    private

    public :: current_column, move_to_column, vertical_motion, screen_column, window_text_width, &
              indent_to, current_indentation, sane_tab_width

    !> Indentation can insert tabs if this is non-nil.  Port of indent.c
    !> indent-tabs-mode (one global value).
    type(logical_setting), public :: indent_tabs_mode = logical_setting(name="indent_tabs_mode", value=.true.)

    ! ------------------------------------------------------------------
    ! indent.el, in the submodule indent_cmds_fm
    ! ------------------------------------------------------------------

    public :: indent_line_proc, indent_region_proc
    public :: indent_according_to_mode, indent_for_tab_command, insert_tab, indent_rigidly, indent_rigidly_left, &
              indent_rigidly_right, indent_rigidly_left_to_tab_stop, indent_rigidly_right_to_tab_stop, &
              indent_line_to, current_left_margin, move_to_left_margin, indent_to_left_margin, &
              delete_to_left_margin, beginning_of_line_text, indent_region, indent_region_line_by_line, &
              indent_relative, indent_relative_first_indent_point, indent_next_tab_stop, &
              indent_line_function_name, indent_line_function_named, &
              indent_accumulate_tab_stops, tab_to_tab_stop, move_to_tab_stop, syms_of_indent

    abstract interface
        !> An `indent_line_function': indent the current line; `noindent'
        !> says it could not (the value `noindent').
        subroutine indent_line_proc(status, noindent)
            import :: status_type
            implicit none(type, external)
            type(status_type), intent(out) :: status
            logical, intent(out) :: noindent
        end subroutine indent_line_proc

        !> An `indent_region_function'.
        subroutine indent_region_proc(start, end, status)
            import :: int64, status_type
            implicit none(type, external)
            integer(int64), intent(in) :: start, end
            type(status_type), intent(out) :: status
        end subroutine indent_region_proc
    end interface

    !> Default number of columns for margin-changing functions to indent.
    !> Port of indent.el standard-indent.
    type(integer_setting), public :: standard_indent = integer_setting(name="standard_indent", value=4)

    !> Controls the operation of the TAB key.
    !> If t, hitting TAB always just indents the current line.
    !> If nil, hitting TAB indents the current line if point is at the left margin
    !> or in the line's indentation, otherwise it inserts a "real" TAB character.
    !> Port of indent.el tab-always-indent (no `complete').
    logical, public :: tab_always_indent = .true.

    !> List of tab stop positions used by `tab_to_tab_stop'.
    !> This should be nil, or a list of integers, ordered from smallest to largest.
    !> It implicitly extends to infinity through repetition of the last step.
    !> For example, (1 2 5) is equivalent to (1 2 5 8 11 ...).  If the list has
    !> fewer than 2 elements, `tab_width' is used as the "last step".
    !> A value of nil means a tab stop every `tab_width' columns.  Port of
    !> indent.el tab-stop-list (unallocated or empty is nil).
    integer(int64), allocatable, public :: tab_stop_list(:)

    !> A setting whose value is an indent-line function; null is the
    !> default, `indent_relative'.  Assign with `=>' to `value'.
    type, extends(setting_type), public :: indent_line_setting
        procedure(indent_line_proc), pointer, nopass :: value => null()
    contains
        generic :: set_local => set_local_indent_line
        procedure, private :: set_local_indent_line => indent_line_set_local
    end type indent_line_setting

    !> Function to indent the current line.
    !> This function will be called with no arguments.
    !> If it is called somewhere where it cannot auto-indent, the function
    !> should return `noindent' to signal that it didn't.
    !> Setting this function is all you need to make TAB indent appropriately.
    !> Don't rebind TAB unless you really need to.  Port of indent.el
    !> indent-line-function (null is the default, `indent_relative').
    type(indent_line_setting), public :: indent_line_function = indent_line_setting(name="indent_line_function")

    !> Short cut function to indent region using `indent_according_to_mode'.
    !> Default is to really run `indent_according_to_mode' on each line.
    !> Port of indent.el indent-region-function (null is
    !> `indent_region_line_by_line').
    procedure(indent_region_proc), pointer, public :: indent_region_function => null()

    interface
        module subroutine indent_according_to_mode(status, inhibit_widen)
            implicit none(type, external)
            type(status_type), intent(out) :: status
            logical, intent(in), optional :: inhibit_widen
        end subroutine indent_according_to_mode

        module subroutine indent_for_tab_command(arg, status)
            implicit none(type, external)
            type(value_type), intent(in) :: arg
            type(status_type), intent(out) :: status
        end subroutine indent_for_tab_command

        module subroutine insert_tab(arg, status)
            implicit none(type, external)
            type(value_type), intent(in) :: arg
            type(status_type), intent(out) :: status
        end subroutine insert_tab

        module subroutine indent_rigidly(start, end, arg, status)
            implicit none(type, external)
            integer(int64), intent(in) :: start, end, arg
            type(status_type), intent(out) :: status
        end subroutine indent_rigidly

        module subroutine indent_rigidly_left(beg, end, status)
            implicit none(type, external)
            integer(int64), intent(in) :: beg, end
            type(status_type), intent(out) :: status
        end subroutine indent_rigidly_left

        module subroutine indent_rigidly_right(beg, end, status)
            implicit none(type, external)
            integer(int64), intent(in) :: beg, end
            type(status_type), intent(out) :: status
        end subroutine indent_rigidly_right

        module subroutine indent_rigidly_left_to_tab_stop(beg, end, status)
            implicit none(type, external)
            integer(int64), intent(in) :: beg, end
            type(status_type), intent(out) :: status
        end subroutine indent_rigidly_left_to_tab_stop

        module subroutine indent_rigidly_right_to_tab_stop(beg, end, status)
            implicit none(type, external)
            integer(int64), intent(in) :: beg, end
            type(status_type), intent(out) :: status
        end subroutine indent_rigidly_right_to_tab_stop

        module subroutine indent_line_to(column, status)
            implicit none(type, external)
            integer(int64), intent(in) :: column
            type(status_type), intent(out) :: status
        end subroutine indent_line_to

        module integer(int64) function current_left_margin()
            implicit none(type, external)
        end function current_left_margin

        module subroutine move_to_left_margin(status, n, force)
            implicit none(type, external)
            type(status_type), intent(out) :: status
            integer(int64), intent(in), optional :: n
            logical, intent(in), optional :: force
        end subroutine move_to_left_margin

        module subroutine indent_to_left_margin(status, noindent)
            implicit none(type, external)
            type(status_type), intent(out) :: status
            logical, intent(out) :: noindent
        end subroutine indent_to_left_margin

        module subroutine delete_to_left_margin(status, from, to)
            implicit none(type, external)
            type(status_type), intent(out) :: status
            integer(int64), intent(in), optional :: from, to
        end subroutine delete_to_left_margin

        module subroutine beginning_of_line_text(n)
            implicit none(type, external)
            integer(int64), intent(in), optional :: n
        end subroutine beginning_of_line_text

        module subroutine indent_region(start, end, status, column)
            implicit none(type, external)
            integer(int64), intent(in) :: start, end
            type(status_type), intent(out) :: status
            integer(int64), intent(in), optional :: column
        end subroutine indent_region

        module subroutine indent_region_line_by_line(start, end, status)
            implicit none(type, external)
            integer(int64), intent(in) :: start, end
            type(status_type), intent(out) :: status
        end subroutine indent_region_line_by_line

        module subroutine indent_relative(status, first_only, unindented_ok, noindent)
            implicit none(type, external)
            type(status_type), intent(out) :: status
            logical, intent(in), optional :: first_only, unindented_ok
            logical, intent(out), optional :: noindent
        end subroutine indent_relative

        !> The name of indent-line function P: `indent_relative' for null,
        !> "" for a procedure without a name.  (In indent_cmds_fm.)
        module function indent_line_function_name(p) result(name)
            implicit none(type, external)
            procedure(indent_line_proc), pointer, intent(in) :: p
            character(len=:), allocatable :: name
        end function indent_line_function_name

        !> Point P at the indent-line function named NAME; any other
        !> function is called by name with `funcall'.  (In indent_cmds_fm.)
        module subroutine indent_line_function_named(name, p)
            implicit none(type, external)
            character(len=*), intent(in) :: name
            procedure(indent_line_proc), pointer, intent(inout) :: p
        end subroutine indent_line_function_named

        module subroutine indent_relative_first_indent_point(status, noindent)
            implicit none(type, external)
            type(status_type), intent(out) :: status
            logical, intent(out) :: noindent
        end subroutine indent_relative_first_indent_point

        module integer(int64) function indent_next_tab_stop(column, prev)
            implicit none(type, external)
            integer(int64), intent(in) :: column
            logical, intent(in), optional :: prev
        end function indent_next_tab_stop

        module function indent_accumulate_tab_stops(limit) result(stops)
            implicit none(type, external)
            integer(int64), intent(in) :: limit
            integer(int64), allocatable :: stops(:)
        end function indent_accumulate_tab_stops

        module subroutine tab_to_tab_stop(status)
            implicit none(type, external)
            type(status_type), intent(out) :: status
        end subroutine tab_to_tab_stop

        module subroutine move_to_tab_stop(status)
            implicit none(type, external)
            type(status_type), intent(out) :: status
        end subroutine move_to_tab_stop

        !> Register the indentation commands.
        module subroutine syms_of_indent()
            implicit none(type, external)
        end subroutine syms_of_indent
    end interface

    !> Width when no window is set up (as in batch use).
    integer, parameter :: DEFAULT_WIDTH = 80

contains

    !> Give the current buffer its own value V.  Port of subr.el:setq-local.
    subroutine indent_line_set_local(self, v)
        class(indent_line_setting), intent(inout) :: self
        procedure(indent_line_proc) :: v
        call self%make_local()
        self%value => v
        self%has_exact = .false.
        self%void = .false.
    end subroutine indent_line_set_local

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

    !> Move point to column COLUMN in the current line.
    !> The column of a character is calculated by adding together the widths
    !> as displayed of the previous characters in the line.
    !> This function ignores line-continuation;
    !> there is no upper limit on the column number a character can have
    !> and horizontal scrolling has no effect.
    !>
    !> If specified column is within a character, point goes after that character.
    !> If it's past end of line, point goes to end of line.
    !>
    !> Optional argument FORCE non-nil means if COLUMN is in the
    !> middle of a tab character, either change it to spaces (when
    !> `indent_tabs_mode' is nil), or insert enough spaces before it to reach
    !> COLUMN (otherwise).  In addition, if FORCE is t, and the line is too short
    !> to reach COLUMN, add spaces/tabs to get there.
    !>
    !> The return value is the current column (`reached').  Port of
    !> indent.c:Fmove_to_column (FORCE is t or nil).
    subroutine move_to_column(column, reached, force, status)
        integer(int64), intent(in) :: column
        integer(int64), intent(out) :: reached
        logical, intent(in), optional :: force
        type(status_type), intent(out), optional :: status
        integer(int64) :: pos, byte, prev_pos, prev_col, goal_pt, col
        integer(int32) :: c
        type(status_type) :: st
        pos = pos_bol()
        byte = char_to_byte(pos)
        reached = 0
        prev_pos = pos
        prev_col = 0
        c = 0
        do while (pos < buffers(cur)%zv .and. reached < column)
            c = buf_fetch_char(buffers(cur), byte)
            if (c == 10) exit
            prev_pos = pos
            prev_col = reached
            reached = reached + glyph_columns(c, int(reached))
            byte = byte + buf_next_char_len(buffers(cur), byte)
            pos = pos + 1
        end do
        call goto_char(pos)
        if (present(status)) status = status_type()
        if (.not. present(force)) return
        if (.not. force) return
        ! If a tab char made us overshoot, change it to spaces and scan
        ! through it again.
        if (reached > column .and. c == 9 .and. prev_col < column .and. prev_pos < pos) then
            ! Insert spaces in front of the tab to reach GOAL.  Do this
            ! first so that a marker at the end of the tab gets adjusted.
            call goto_char(prev_pos)
            call insert_char_n(32, column - prev_col, st)
            ! Now delete the tab, and indent to COL.
            if (st%ok()) call del_range(point(), point() + 1, st)
            goal_pt = point()
            if (st%ok()) col = indent_to(reached, st)
            call goto_char(goal_pt)
            reached = column
        end if
        ! If line ends prematurely, add space to the end.
        if (reached < column .and. st%ok()) reached = indent_to(column, st)
        if (present(status)) status = st
    end subroutine move_to_column

    !> Indent from point with tabs and spaces until COLUMN is reached.
    !> Optional second argument MINIMUM says always do at least MINIMUM spaces
    !> even if that goes past COLUMN; by default, MINIMUM is zero.
    !>
    !> Whether this uses tabs or spaces depends on `indent_tabs_mode'.
    !>
    !> The return value is the column where the insertion ends.  Port of
    !> indent.c:Findent_to.
    integer(int64) function indent_to(column, status, minimum) result(mincol)
        integer(int64), intent(in) :: column
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: minimum
        integer(int64) :: fromcol, n, tw
        status = status_type()
        tw = sane_tab_width()
        fromcol = current_column()
        mincol = fromcol
        if (present(minimum)) mincol = fromcol + minimum
        if (mincol < column) mincol = column
        if (fromcol == mincol) return
        if (indent_tabs_mode%value) then
            n = mincol/tw - fromcol/tw
            if (n /= 0) then
                call insert_char_n(9, n, status)
                if (.not. status%ok()) return
                fromcol = (mincol/tw)*tw
            end if
        end if
        call insert_char_n(32, mincol - fromcol, status)
    end function indent_to

    !> Return the indentation of the current line.
    !> This is the horizontal position of the character following any initial
    !> whitespace.  Port of indent.c:Fcurrent_indentation (no invisible
    !> text).
    integer(int64) function current_indentation() result(col)
        integer(int64) :: pos, byte, tw
        integer(int32) :: c
        tw = sane_tab_width()
        pos = pos_bol()
        byte = char_to_byte(pos)
        col = 0
        do while (pos < buffers(cur)%zv)
            c = buf_fetch_char(buffers(cur), byte)
            if (c == 32) then
                col = col + 1
            else if (c == 9) then
                col = col + tw - mod(col, tw)
            else
                exit
            end if
            byte = byte + 1
            pos = pos + 1
        end do
    end function current_indentation

    !> tab-width, or 8 if it is not in 1..1000 (SANE_TAB_WIDTH).
    integer(int64) function sane_tab_width() result(tw)
        tw = tab_width%value
        if (tw <= 0 .or. tw > 1000) tw = 8
    end function sane_tab_width

    !> The width available for text in the selected window: its columns
    !> less the last, which holds the continuation glyph.
    integer function window_text_width() result(width)
        type(window_type), pointer :: sw
        if (selected_frame%minibuffer_selected) then
            ! The minibuffer scrolls horizontally instead of continuing.
            width = huge(0)/2
        else
            sw => frame_selected_window_ref()
            width = window_text_cols(sw) - 1
            if (width < 1) width = DEFAULT_WIDTH - 1
        end if
    end function window_text_width

    !> Screen rows of the line starting at `bol', `width' columns wide:
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

    !> The screen row (1-based) of `pos' among `starts(1:nrows)'.
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

    !> Move point `lines' screen lines down (up if negative), to the start
    !> of that screen line or, with `goal', to the character at column
    !> `goal' on it (the last one on a continued row if it is shorter).
    !> `moved' is the number of screen lines moved, closer to zero at the
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
end module indent_fm
