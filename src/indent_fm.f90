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
!> lines, the way the selected window shows continued lines.  Columns
!> skip invisible text and count display strings and spaces.
!>
!> Not yet: images in columns, selective display,
!> horizontal scrolling of ordinary windows, and truncated lines.
module indent_fm
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use buffer_fm, only: buffers, cur, buf_fetch_char, buf_next_char_len, buffer_serial_live
    use characters_fm, only: char_has_category
    use marker_fm, only: char_to_byte
    use error_fm, only: status_type
    use lisp_fm, only: value_type, nilp, consp, stringp, xstring, fixnump, xfixnum, list_elements
    use lread_fm, only: sym
    use data_fm, only: value_eq
    use handle_fm, only: handle_type
    use character_fm, only: string_char_and_length
    use textprop_fm, only: get_char_property, get_char_property_and_overlay, text_prop_means_invisible, &
                           next_property_change, next_single_property_change, get_property_and_range
    use itree_fm, only: next_overlay_change, overlay_end
    use data_fm, only: logical_setting, integer_setting, setting_type
    use editfns_fm, only: goto_char, pos_bol, point, insert_char_n
    use insdel_fm, only: del_range
    use xdisp_fm, only: glyph_columns, tab_width, row_start_type, display_line_rows, display_row_of, display_row, &
                        line_beginning
    use frame_fm, only: selected_frame, frame_selected_window_ref
    use window_fm, only: window_type, window_text_cols
    implicit none(type, external)
    private

    public :: current_column, move_to_column, vertical_motion, screen_column, window_text_width, &
              indent_to, current_indentation, sane_tab_width

    !> Indentation can insert tabs if this is true.  Port of indent.c
    !> indent-tabs-mode (one global value).
    type(logical_setting), public :: indent_tabs_mode = logical_setting(name="indent_tabs_mode", value=.true.)

    ! ------------------------------------------------------------------
    ! indent.el, in the submodule indent_cmds_fm
    ! ------------------------------------------------------------------

    public :: indent_line_proc, indent_region_proc, kill_indent_locals
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
    !> If true, hitting TAB always just indents the current line.
    !> If false, hitting TAB indents the current line if point is at the left margin
    !> or in the line's indentation, otherwise it inserts a "real" TAB character.
    !> Port of indent.el tab-always-indent (no `complete').
    logical, public :: tab_always_indent = .true.

    !> Array of tab stop positions used by `tab_to_tab_stop'.
    !> This should be unallocated, or an array of integers, ordered from smallest to largest.
    !> It implicitly extends to infinity through repetition of the last step.
    !> For example, [1, 2, 5] is equivalent to [1, 2, 5, 8, 11, ...].  If the array has
    !> fewer than 2 elements, `tab_width' is used as the "last step".
    !> An unallocated or empty value means a tab stop every `tab_width' columns.  Port of
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
    !> should set its NOINDENT argument to true to signal that it didn't.
    !> Setting this function is all you need to make TAB indent appropriately.
    !> Don't rebind TAB unless you really need to.  Port of indent.el
    !> indent-line-function (null is the default, `indent_relative').
    type(indent_line_setting), public :: indent_line_function = indent_line_setting(name="indent_line_function")

    !> One buffer's `indent_region_function'.
    type :: indent_region_entry_type
        integer(int64) :: serial = 0
        procedure(indent_region_proc), pointer, nopass :: fn => null()
    end type indent_region_entry_type

    !> The type of `indent_region_function': a default `value' (assign
    !> with `=>'; null is `indent_region_line_by_line') and buffers' own
    !> (`set_local').
    type, public :: indent_region_function_type
        procedure(indent_region_proc), pointer, nopass :: value => null()
        type(indent_region_entry_type), allocatable :: entries(:)
    contains
        !> Make FN the current buffer's function (setq-local).
        procedure :: set_local => indent_region_set_local
        !> The current buffer's function (null for the default).
        procedure :: get => indent_region_get
        !> Make the current buffer use the default again.
        procedure :: kill_local => indent_region_kill_local
    end type indent_region_function_type

    !> Short cut function to indent region using `indent_according_to_mode'.
    !> Default is to really run `indent_according_to_mode' on each line.
    !> Port of indent.el indent-region-function (null is
    !> `indent_region_line_by_line').
    type(indent_region_function_type), public :: indent_region_function

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

    subroutine indent_region_set_local(self, fn)
        class(indent_region_function_type), intent(inout) :: self
        procedure(indent_region_proc) :: fn
        type(indent_region_entry_type), allocatable :: grown(:)
        integer :: i, n

        if (cur == 0) return
        if (.not. allocated(self%entries)) allocate (self%entries(0))

        do i = 1, size(self%entries)
            if (self%entries(i)%serial == buffers(cur)%serial) then
                self%entries(i)%fn => fn
                return
            end if
        end do

        ! Reuse the entry of a dead buffer.
        do i = 1, size(self%entries)
            if (.not. buffer_serial_live(self%entries(i)%serial)) then
                self%entries(i)%serial = buffers(cur)%serial
                self%entries(i)%fn => fn
                return
            end if
        end do

        n = size(self%entries)
        allocate (grown(n + 1))

        do i = 1, n
            grown(i)%serial = self%entries(i)%serial
            grown(i)%fn => self%entries(i)%fn
        end do

        grown(n + 1)%serial = buffers(cur)%serial
        grown(n + 1)%fn => fn
        call move_alloc(grown, self%entries)
    end subroutine indent_region_set_local

    function indent_region_get(self) result(fn)
        class(indent_region_function_type), intent(in) :: self
        procedure(indent_region_proc), pointer :: fn
        integer :: i

        fn => self%value
        if (cur == 0) return
        if (.not. allocated(self%entries)) return

        do i = 1, size(self%entries)
            if (self%entries(i)%serial == buffers(cur)%serial) then
                if (associated(self%entries(i)%fn)) fn => self%entries(i)%fn
            end if
        end do
    end function indent_region_get

    subroutine indent_region_kill_local(self)
        class(indent_region_function_type), intent(inout) :: self
        integer :: i

        if (cur == 0) return
        if (.not. allocated(self%entries)) return

        do i = 1, size(self%entries)
            if (self%entries(i)%serial == buffers(cur)%serial) then
                self%entries(i)%serial = 0
                self%entries(i)%fn => null()
            end if
        end do
    end subroutine indent_region_kill_local

    !> kill-all-local-variables: forget the current buffer's
    !> `indent_region_function'.
    subroutine kill_indent_locals()
        call indent_region_function%kill_local()
    end subroutine kill_indent_locals

    !> Give the current buffer its own value V.  Port of subr.el:setq-local.
    subroutine indent_line_set_local(self, v)
        class(indent_line_setting), intent(inout) :: self
        procedure(indent_line_proc) :: v

        call self%make_local()
        self%value => v
        self%has_exact = .false.
        self%void = .false.
    end subroutine indent_line_set_local

    !> Columns from the start of the line to point.  Invisible text
    !> (unless shown as an ellipsis) takes no columns, text with a display
    !> string or space as many as they take.
    !> Port of indent.c:Fcurrent_column (current_column).
    integer(int64) function current_column() result(col)
        integer(int64) :: pos, prev_pos, prev_col

        pos = buffers(cur)%pt
        call scan_for_column(pos, huge(0_int64), col, prev_pos, prev_col)
    end function current_column

    !> Scan from the start of the current line up to END (stopping at a
    !> newline) or until column GOAL is reached: END is where the scan
    !> stopped and COL its column; PREV_POS and PREV_COL the position and
    !> column of the last character scanned.  Port of
    !> indent.c:scan_for_column (no compositions or display tables).
    subroutine scan_for_column(end, goal, col, prev_pos, prev_col)
        integer(int64), intent(inout) :: end
        integer(int64), intent(in) :: goal
        integer(int64), intent(out) :: col, prev_pos, prev_col
        integer(int64) :: scan, byte, next_boundary, endp, width
        integer(int32) :: c

        scan = pos_bol()
        col = 0
        prev_col = 0
        next_boundary = scan
        prev_pos = scan

        ! Scan forward to the target position.
        do while (scan < end)
            ! Occasionally we may need to skip invisible text.
            do while (scan == next_boundary)
                ! This updates NEXT_BOUNDARY to the next place where we
                ! might need to skip more invisible text.
                scan = skip_invisible(scan, next_boundary, end)
                if (scan >= end) exit
            end do

            if (scan >= end) exit
            ! Test reaching the goal column.  We do this after skipping
            ! invisible characters, so that we put point before the
            ! character on which the cursor will appear.
            if (col >= goal) exit
            prev_col = col
            prev_pos = scan
            ! Check display property.
            width = check_display_width(scan, col, endp)

            if (width >= 0) then
                col = col + width

                ! Avoid infinite loops with 0-width overlays.
                if (endp > scan) then
                    scan = endp
                    cycle
                end if
            end if

            byte = char_to_byte(scan)
            c = buf_fetch_char(buffers(cur), byte)
            if (c == 10) exit
            col = col + glyph_columns(c, int(col))
            scan = scan + 1
        end do

        end = scan
    end subroutine scan_for_column

    !> If the text at POS is invisible (not shown as an ellipsis), the
    !> position to skip to, else POS; NEXT_BOUNDARY gets the next position
    !> where the invisibility might change.  Port of indent.c:skip_invisible
    !> for columns (WINDOW nil).
    integer(int64) function skip_invisible(pos, next_boundary, to) result(newpos)
        integer(int64), intent(in) :: pos, to
        integer(int64), intent(out) :: next_boundary
        integer(int64) :: overlay_limit, proplimit
        type(status_type) :: st
        logical :: found

        ! We must not advance farther than the next overlay change.  The
        ! overlay change might change the invisible property; or there
        ! might be overlay strings to be displayed there.
        overlay_limit = next_overlay_change(pos)
        ! As for text properties, this gives a lower bound for where the
        ! invisible text property could change.
        call next_property_change(pos, proplimit, found, st)
        if (.not. found) proplimit = buffers(cur)%zv
        proplimit = min(proplimit, overlay_limit)

        if (proplimit > pos + 100 .or. proplimit >= to) then
            next_boundary = proplimit
        else
            ! Don't scan terribly far, and no matter what, don't go past
            ! next overlay change.
            proplimit = min(pos + 100, to, overlay_limit)
            call next_single_property_change(pos, sym("invisible"), next_boundary, found, st, proplimit)
            if (.not. found) next_boundary = proplimit
        end if

        ! If the `invisible' property is set, we can skip to the next
        ! property change; when counting columns, don't skip over
        ! ellipsis text.
        newpos = pos
        if (text_prop_means_invisible(get_char_property(pos, sym("invisible"))) == 1) newpos = next_boundary
    end function skip_invisible

    !> The width in columns of text at POS (column COL) that a `display'
    !> property replaces with a space or a string, and in ENDPOS the end
    !> of that text; -1 if there is no such display property.  Port of
    !> indent.c:check_display_width (images are deferred).
    integer(int64) function check_display_width(pos, col, endpos) result(width)
        integer(int64), intent(in) :: pos, col
        integer(int64), intent(out) :: endpos
        type(value_type) :: val
        type(value_type), allocatable :: elts(:)
        type(handle_type) :: overlay
        integer(int64) :: start
        integer :: k

        width = -1
        endpos = pos
        call get_char_property_and_overlay(pos, sym("display"), val, overlay)
        if (nilp(val)) return

        if (consp(val)) then
            call list_elements(val, elts)

            ! Handle '(space ...)' display specs.
            if (value_eq(elts(1), sym("space"))) then
                do k = 2, size(elts) - 1, 2
                    if (value_eq(elts(k), sym(":width")) .or. value_eq(elts(k), sym(":relative-width"))) then
                        if (fixnump(elts(k + 1))) then
                            if (xfixnum(elts(k + 1)) >= 0) width = xfixnum(elts(k + 1))
                        end if

                        exit
                    end if
                end do

                if (width < 0) then
                    do k = 2, size(elts) - 1, 2
                        if (value_eq(elts(k), sym(":align-to"))) then
                            if (fixnump(elts(k + 1))) then
                                if (xfixnum(elts(k + 1)) >= col) width = xfixnum(elts(k + 1)) - col
                            end if

                            exit
                        end if
                    end do
                end if
            end if
        else if (stringp(val)) then
            ! Handle 'display' strings.
            width = display_string_width(xstring(val))
        end if

        if (width >= 0) then
            if (overlay%index /= 0) then
                endpos = overlay_end(overlay)
            else
                call get_property_and_range(pos, sym("display"), val, start, endpos)
            end if
        end if
    end function check_display_width

    !> Columns STRING takes on display.  (character.c:Fstring_width.)
    integer(int64) function display_string_width(string) result(width)
        character(len=*), intent(in) :: string
        integer(int64) :: i
        integer(int32) :: c, clen

        width = 0
        i = 1

        do while (i <= len(string, kind=int64))
            call string_char_and_length(string, i, c, clen)
            i = i + clen
            width = width + glyph_columns(c, int(width))
        end do
    end function display_string_width

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
    !> indent.c:Fmove_to_column.  FORCE true is a FORCE of t; COERCE true
    !> (without FORCE) is a FORCE that is neither nil nor t, such as the
    !> `coerce' rect.el passes: a tab is split, but a short line is not
    !> padded.
    recursive subroutine move_to_column(column, reached, force, status, coerce)
        integer(int64), intent(in) :: column
        integer(int64), intent(out) :: reached
        logical, intent(in), optional :: force
        type(status_type), intent(out), optional :: status
        logical, intent(in), optional :: coerce
        integer(int64) :: pos, prev_pos, prev_col, goal_pt, col
        integer(int32) :: c
        type(status_type) :: st
        logical :: split, pad

        pad = .false.
        if (present(force)) pad = force
        split = pad
        if (present(coerce)) split = split .or. coerce
        pos = buffers(cur)%zv
        call scan_for_column(pos, column, reached, prev_pos, prev_col)
        c = 0
        if (prev_pos < buffers(cur)%zv) c = buf_fetch_char(buffers(cur), char_to_byte(prev_pos))
        call goto_char(pos)
        if (present(status)) status = status_type()
        if (.not. split) return

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
        if (pad .and. reached < column .and. st%ok()) reached = indent_to(column, st)
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
    !> whitespace.  Port of indent.c:Fcurrent_indentation with
    !> position_indentation (no invisible text): in a unibyte buffer the
    !> byte 0240 counts as a space, in a multibyte one characters of the
    !> category ` ' (space for indent) do.
    integer(int64) function current_indentation() result(col)
        integer(int64) :: byte, tw
        integer(int32) :: c

        tw = sane_tab_width()
        byte = char_to_byte(pos_bol())
        col = 0

        do while (byte < buffers(cur)%zv_byte)
            c = buf_fetch_char(buffers(cur), byte)

            if (c == 32 .or. (c == int(o'240') .and. .not. buffers(cur)%multibyte)) then
                col = col + 1
            else if (c == 9) then
                col = col + tw - mod(col, tw)
            else if (c < 128 .or. .not. buffers(cur)%multibyte) then
                exit
            else if (char_has_category(c, " ")) then
                col = col + 1
            else
                exit
            end if

            byte = byte + buf_next_char_len(buffers(cur), byte)
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

    !> Point's column on its screen line (the cursor's x in the window).
    integer function screen_column() result(x)
        type(row_start_type), allocatable :: rows(:)
        type(row_start_type) :: row
        integer(int64) :: eol
        integer :: nrows, k, width, ends, used

        width = window_text_width()
        call display_line_rows(cur, line_beginning(cur, buffers(cur)%pt), width, rows, nrows, eol)
        k = display_row_of(buffers(cur)%pt, rows, nrows)
        row = rows(k)
        call display_row(cur, row, width, buffers(cur)%pt, ends, x, used)
        if (x < 0) x = 0
    end function screen_column

    !> Move point `lines' screen lines down (up if negative), to the start
    !> of that screen line or, with `goal', to the character at column
    !> `goal' on it (the last one on a continued row if it is shorter).
    !> `moved' is the number of screen lines moved, closer to zero at the
    !> beginning or end of the buffer.  Screen lines are those redisplay
    !> shows (xdisp_fm:display_line_rows), with invisible text, display
    !> strings and overlay strings.  Port of indent.c:Fvertical_motion
    !> (LINES or (COLS . LINES); no CUR-COL).  Lines are as wide as in
    !> WINDOW, by default the selected window.
    subroutine vertical_motion(lines, moved, goal, window)
        integer(int64), intent(in) :: lines
        integer(int64), intent(out) :: moved
        integer, intent(in), optional :: goal
        type(window_type), intent(in), optional :: window
        type(row_start_type), allocatable :: rows(:)
        type(row_start_type) :: row
        integer(int64) :: bol, eol, target, pos
        integer :: nrows, width, ends, col, used

        if (present(window)) then
            width = window_text_cols(window) - 1
            if (width < 1) width = DEFAULT_WIDTH - 1
        else
            width = window_text_width()
        end if

        bol = line_beginning(cur, buffers(cur)%pt)
        call display_line_rows(cur, bol, width, rows, nrows, eol)
        target = display_row_of(buffers(cur)%pt, rows, nrows) + lines
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
            call display_line_rows(cur, bol, width, rows, nrows, eol)
        end do

        do while (target < 1)
            if (bol <= buffers(cur)%begv) then
                moved = moved + (1 - target)
                target = 1
                exit
            end if

            bol = line_beginning(cur, bol - 1)
            call display_line_rows(cur, bol, width, rows, nrows, eol)
            target = target + nrows
        end do

        pos = rows(target)%pos

        if (present(goal)) then
            row = rows(target)
            call display_row(cur, row, width, -1_int64, ends, col, used, goal_x=goal, goal_pos=pos)
        end if

        call goto_char(pos)
    end subroutine vertical_motion
end module indent_fm
