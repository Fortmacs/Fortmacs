! rect_fm.f90 -- rectangle functions.
! Ported from GNU Emacs 31.1 lisp/rect.el.
!
! Copyright (C) 1985, 1999-2026 Free Software Foundation, Inc.
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

!> The operations on rectangles: the text between two columns on each
!> line from the line of one corner to the line of the other.  A
!> rectangle as data (upstream's list of strings) is an array of
!> `rectangle_line_type'; `killed_rectangle' holds the last one killed.
!> `apply_on_rectangle' runs a `rectangle_line_action' (upstream's
!> FUNCTION and its ARGS) on each line.
!>
!> The commands are on C-x r (bindings_fm): C-x r k, C-x r M-w, C-x r y,
!> C-x r d, C-x r o, C-x r c, C-x r t and C-x r N.
!>
!> Deferred: `rectangle_mark_mode' (C-x SPC) with its keymap and the
!> rectangular region (it needs `region_extract_function',
!> `region_insert_function', redisplay's region highlighting functions
!> and advice on `region_beginning'/`region_end'), and with it the
!> corner crutches (columns of point and mark where point cannot be);
!> the on-the-fly preview of `string_rectangle' (`rectangle_preview',
!> the `rectangle_preview' face: needs overlays and a post-command hook
!> in the minibuffer); `rectangle_indicate_zero_width_rectangle'; the
!> obsolete `operate_on_rectangle', `close_rectangle' and
!> `replace_rectangle'; `inherit_input_method' in the prompts (no input
!> methods); text properties of the extracted strings (the kill ring holds
!> plain strings); the floating-point directives %e, %f and %g of a
!> `rectangle_number_lines' FORMAT (an error).
module rect_fm
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use commands_fm, only: commands
    use error_fm, only: status_type, set_error, format_message, condition_matches, FM_GENERIC_ERROR, &
                        FM_BUFFER_READ_ONLY, FM_TEXT_READ_ONLY, FM_WRONG_TYPE_ARGUMENT
    use handle_fm, only: handle_type
    use lisp_fm, only: value_type, nilp, list_length, list_elt, stringp, xstring
    use lread_fm, only: sym
    use data_fm, only: symbol_value
    use character_fm, only: string_char_and_length, char_string, MAX_MULTIBYTE_LENGTH, MAX_CHAR
    use buffer_fm, only: buffers, cur, barf_if_buffer_read_only, vdeactivate_mark, undo_entry_type, UE_POINT
    use marker_fm, only: copy_marker_at, free_marker, marker_position, char_to_byte
    use undo_fm, only: push_undo_entry
    use insdel_fm, only: insert
    use editfns_fm, only: point, goto_char, line_beginning_position, line_end_position, bolp, buffer_substring, &
                          delete_region, delete_and_extract_region, region_beginning, region_end, char_after
    use cmds_fm, only: forward_line
    use indent_fm, only: current_column, move_to_column, indent_to, indent_tabs_mode
    use syntax_fm, only: skip_syntax_forward
    use xdisp_fm, only: message, glyph_width
    use simple_fm, only: push_mark, insert_for_yank, kill_read_only_ok, indicate_copied_region, count_lines, &
                         line_number_at_pos
    use minibuf_fm, only: read_string
    use subr_fm, only: read_number, defvar_nil
    use callint_fm, only: current_prefix_arg
    implicit none(type, external)
    private

    public :: apply_on_rectangle, rectangle_position_as_coordinates, rectangle_intersect_p, rectangle_dimensions, &
              delete_rectangle, delete_extract_rectangle, extract_rectangle, extract_rectangle_bounds, &
              kill_rectangle, copy_rectangle_as_kill, yank_rectangle, insert_rectangle, open_rectangle, &
              delete_whitespace_rectangle, string_rectangle, string_insert_rectangle, clear_rectangle, &
              rectangle_number_lines, spaces_string, syms_of_rect

    !> One line of a rectangle (a string of upstream's list of strings).
    type, public :: rectangle_line_type
        character(len=:), allocatable :: text
    end type rectangle_line_type

    !> Rectangle for `yank_rectangle' to insert.  Unallocated (or empty)
    !> for none.  Port of rect.el killed-rectangle.
    type(rectangle_line_type), allocatable, public :: killed_rectangle(:)

    !> What `apply_on_rectangle' does on each line of a rectangle:
    !> upstream's FUNCTION with its extra ARGS, here an extension of this
    !> type holding the arguments, with its `line' procedure.
    type, abstract, public :: rectangle_line_action
    contains
        procedure(rectangle_line_proc), deferred :: line
    end type rectangle_line_action

    abstract interface
        !> Act on the current line, point at its beginning; STARTCOL and
        !> ENDCOL are the rectangle's columns.
        recursive subroutine rectangle_line_proc(self, startcol, endcol, status)
            import :: rectangle_line_action, int64, status_type
            implicit none(type, external)
            class(rectangle_line_action), intent(inout) :: self
            integer(int64), intent(in) :: startcol, endcol
            type(status_type), intent(out) :: status
        end subroutine rectangle_line_proc
    end interface

    !> rect.el:delete-rectangle-line.
    type, extends(rectangle_line_action) :: delete_line_action
        logical :: fill = .false.
    contains
        procedure :: line => delete_line
    end type delete_line_action

    !> rect.el:delete-extract-rectangle-line, collecting the lines.
    type, extends(rectangle_line_action) :: delete_extract_line_action
        logical :: fill = .false.
        type(rectangle_line_type), allocatable :: lines(:)
    contains
        procedure :: line => delete_extract_line
    end type delete_extract_line_action

    !> rect.el:extract-rectangle-line, collecting the lines.
    type, extends(rectangle_line_action) :: extract_line_action
        type(rectangle_line_type), allocatable :: lines(:)
    contains
        procedure :: line => extract_line
    end type extract_line_action

    !> The function of rect.el:extract-rectangle-bounds.
    type, extends(rectangle_line_action) :: bounds_line_action
        integer(int64), allocatable :: starts(:), ends(:)
    contains
        procedure :: line => bounds_line
    end type bounds_line_action

    !> rect.el:open-rectangle-line.
    type, extends(rectangle_line_action) :: open_line_action
        logical :: fill = .false.
    contains
        procedure :: line => open_line
    end type open_line_action

    !> rect.el:delete-whitespace-rectangle-line.
    type, extends(rectangle_line_action) :: delete_whitespace_line_action
        logical :: fill = .false.
    contains
        procedure :: line => delete_whitespace_line
    end type delete_whitespace_line_action

    !> rect.el:string-rectangle-line.
    type, extends(rectangle_line_action) :: string_line_action
        character(len=:), allocatable :: string
        logical :: delete = .false.
    contains
        procedure :: line => string_line
    end type string_line_action

    !> rect.el:clear-rectangle-line.
    type, extends(rectangle_line_action) :: clear_line_action
        logical :: fill = .false.
    contains
        procedure :: line => clear_line
    end type clear_line_action

    !> rect.el:rectangle-number-line-callback with its counter
    !> (rectangle-number-line-counter).
    type, extends(rectangle_line_action) :: number_line_action
        character(len=:), allocatable :: format
        integer(int64) :: counter = 1
    contains
        procedure :: line => number_line
    end type number_line_action

contains

    ! ------------------------------------------------------------------
    ! Rectangle operations
    ! ------------------------------------------------------------------

    !> The columns of START and END (rect.el:rectangle--pos-cols without
    !> the crutches of `rectangle_mark_mode').
    subroutine rectangle_pos_cols(start, end, startcol, endcol)
        integer(int64), intent(in) :: start, end
        integer(int64), intent(out) :: startcol, endcol
        call goto_char(start)
        startcol = current_column()
        call goto_char(end)
        endcol = current_column()
    end subroutine rectangle_pos_cols

    !> Call ACTION's `line' for each line of rectangle with corners at
    !> START, END.  It is called with two arguments: the start and end
    !> columns of the rectangle (the extra ARGS are ACTION's components).
    !> Point is at the beginning of line when it is called.
    !> The final point after the last operation is returned in
    !> FINAL_POINT.  Point is restored afterwards, also on an error.
    !> Port of rect.el:apply-on-rectangle.
    recursive subroutine apply_on_rectangle(action, start, end, status, final_point)
        class(rectangle_line_action), intent(inout) :: action
        integer(int64), intent(in) :: start, end
        type(status_type), intent(out) :: status
        integer(int64), intent(out), optional :: final_point
        type(handle_type) :: saved, endpt
        type(status_type) :: st
        integer(int64) :: startcol, endcol, col, startpt, endpos, final, pos
        call copy_marker_at(point(), saved, .false., st)
        call rectangle_pos_cols(start, end, startcol, endcol)
        call goto_char(start)
        startpt = line_beginning_position()
        call goto_char(end)
        call copy_marker_at(line_end_position(), endpt, .false., st)
        ! Ensure the start column is the left one.
        if (endcol < startcol) then
            col = startcol
            startcol = endcol
            endcol = col
        end if
        ! Start looping over lines.
        call goto_char(startpt)
        final = point()
        do
            call action%line(startcol, endcol, status)
            if (.not. status%ok()) exit
            final = point()
            if (forward_line(1_int64) /= 0) exit
            if (.not. bolp()) exit
            call marker_position(endpt, endpos, st)
            if (point() > endpos) exit
        end do
        call free_marker(endpt, st)
        call marker_position(saved, pos, st)
        call goto_char(pos)
        call free_marker(saved, st)
        if (present(final_point)) final_point = final
    end subroutine apply_on_rectangle

    !> Return the column and line values of POSITION, as COLUMN and LINE.
    !> POSITION specifies a position of the current buffer.  Port of
    !> rect.el:rectangle-position-as-coordinates.
    subroutine rectangle_position_as_coordinates(position, column, line)
        integer(int64), intent(in) :: position
        integer(int64), intent(out) :: column, line
        integer(int64) :: saved
        saved = point()
        call goto_char(position)
        column = current_column()
        line = line_number_at_pos()
        call goto_char(saved)
    end subroutine rectangle_position_as_coordinates

    !> Return true if two rectangles intersect.
    !> POS1 and POS2 specify the positions of the upper-left corners of
    !> the first and second rectangles as arrays of the form [COLUMN, LINE].
    !> SIZE1 and SIZE2 specify the dimensions of the first and second
    !> rectangles, as arrays of the form [WIDTH, HEIGHT].  Port of
    !> rect.el:rectangle-intersect-p.
    pure logical function rectangle_intersect_p(pos1, size1, pos2, size2)
        integer(int64), intent(in) :: pos1(2), size1(2), pos2(2), size2(2)
        rectangle_intersect_p = .not. (pos1(1) + size1(1) <= pos2(1) .or. pos2(1) + size2(1) <= pos1(1) .or. &
                                       pos1(2) + size1(2) <= pos2(2) .or. pos2(2) + size2(2) <= pos1(2))
    end function rectangle_intersect_p

    !> Return the dimensions of the rectangle with corners at START and END.
    !> The returned values are WIDTH and HEIGHT.  Port of
    !> rect.el:rectangle-dimensions.
    subroutine rectangle_dimensions(start, end, width, height)
        integer(int64), intent(in) :: start, end
        integer(int64), intent(out) :: width, height
        integer(int64) :: saved, startcol, endcol
        saved = point()
        height = 1 + abs(line_number_at_pos(end) - line_number_at_pos(start))
        call rectangle_pos_cols(start, end, startcol, endcol)
        width = abs(endcol - startcol)
        call goto_char(saved)
    end subroutine rectangle_dimensions

    !> Append TEXT to LINES.
    subroutine append_line(lines, text)
        type(rectangle_line_type), allocatable, intent(inout) :: lines(:)
        character(len=*), intent(in) :: text
        type(rectangle_line_type), allocatable :: grown(:)
        integer :: n
        if (.not. allocated(lines)) allocate (lines(0))
        n = size(lines)
        allocate (grown(n + 1))
        grown(1:n) = lines
        grown(n + 1)%text = text
        call move_alloc(grown, lines)
    end subroutine append_line

    !> (move-to-column COLUMN (if FILL t 'coerce)): the column reached.
    recursive integer(int64) function move_to_column_fill(column, fill, status) result(reached)
        integer(int64), intent(in) :: column
        logical, intent(in) :: fill
        type(status_type), intent(out) :: status
        call move_to_column(column, reached, force=fill, status=status, coerce=.not. fill)
    end function move_to_column_fill

    !> Port of rect.el:delete-rectangle-line.
    recursive subroutine delete_rectangle_line(startcol, endcol, fill, status)
        integer(int64), intent(in) :: startcol, endcol
        logical, intent(in) :: fill
        type(status_type), intent(out) :: status
        integer(int64) :: from, col
        ! We use >= here, not =, for characters that use more than one
        ! column on display, when STARTCOL is in the middle of such a
        ! character.
        if (move_to_column_fill(startcol, fill, status) >= startcol) then
            if (.not. status%ok()) return
            from = point()
            call move_to_column(endcol, col, status=status, coerce=.true.)
            if (.not. status%ok()) return
            call delete_region(from, point(), status)
        end if
    end subroutine delete_rectangle_line

    recursive subroutine delete_line(self, startcol, endcol, status)
        class(delete_line_action), intent(inout) :: self
        integer(int64), intent(in) :: startcol, endcol
        type(status_type), intent(out) :: status
        call delete_rectangle_line(startcol, endcol, self%fill, status)
    end subroutine delete_line

    !> Port of rect.el:delete-extract-rectangle-line.
    recursive subroutine delete_extract_line(self, startcol, endcol, status)
        class(delete_extract_line_action), intent(inout) :: self
        integer(int64), intent(in) :: startcol, endcol
        type(status_type), intent(out) :: status
        integer(int64) :: pt, col
        character(len=:), allocatable :: text
        if (move_to_column_fill(startcol, self%fill, status) < startcol) then
            if (.not. status%ok()) return
            call append_line(self%lines, spaces_string(endcol - startcol))
        else
            if (.not. status%ok()) return
            pt = point()
            call move_to_column(endcol, col, force=.true., status=status)
            if (.not. status%ok()) return
            ! (filter-buffer-substring pt (point) t)
            call delete_and_extract_region(pt, point(), text, status)
            if (.not. status%ok()) return
            call append_line(self%lines, text)
        end if
    end subroutine delete_extract_line

    !> Port of rect.el:extract-rectangle-line.  This is actually the only
    !> function that needs to do complicated stuff like what's happening
    !> in `operate_on_rectangle', because the buffer might be read-only.
    recursive subroutine extract_line(self, startcol, endcol, status)
        class(extract_line_action), intent(inout) :: self
        integer(int64), intent(in) :: startcol, endcol
        type(status_type), intent(out) :: status
        integer(int64) :: start, end, begextra, endextra, col, pos, tail, k, width, tabcol
        integer(int32) :: c
        logical :: found
        character(len=:), allocatable :: line
        call move_to_column(startcol, col)
        start = point()
        begextra = col - startcol
        call move_to_column(endcol, col)
        end = point()
        endextra = endcol - col
        call buffer_substring(start, end, line, status)
        if (.not. status%ok()) return
        if (begextra < 0) then
            endextra = endextra + begextra
            begextra = 0
        end if
        if (endextra < 0) endextra = 0
        call goto_char(start)
        ! (while (search-forward "\t" end t) ...): each TAB becomes as many
        ! spaces as columns it takes.
        do pos = start, end - 1
            call char_after(c, found, pos)
            if (.not. found) exit
            if (c /= 9) cycle
            call goto_char(pos)
            tabcol = current_column()
            call goto_char(pos + 1)
            width = current_column() - tabcol
            ! The TAB is the byte before the TAIL bytes that follow it up
            ! to END (the text after it is still as in the buffer).
            tail = char_to_byte(end) - char_to_byte(pos + 1)
            k = len(line, kind=int64) - tail
            line = line(1:k - 1)//spaces_string(width)//line(k + 1:)
        end do
        if (begextra > 0 .or. endextra > 0) line = spaces_string(begextra)//line//spaces_string(endextra)
        call append_line(self%lines, line)
    end subroutine extract_line

    !> The function of rect.el:extract-rectangle-bounds.
    recursive subroutine bounds_line(self, startcol, endcol, status)
        class(bounds_line_action), intent(inout) :: self
        integer(int64), intent(in) :: startcol, endcol
        type(status_type), intent(out) :: status
        integer(int64) :: col, from
        integer(int64), allocatable :: grown(:)
        integer :: n
        status = status_type()
        call move_to_column(startcol, col)
        from = point()
        call move_to_column(endcol, col)
        if (.not. allocated(self%starts)) allocate (self%starts(0), self%ends(0))
        n = size(self%starts)
        allocate (grown(n + 1))
        grown(1:n) = self%starts
        grown(n + 1) = from
        call move_alloc(grown, self%starts)
        allocate (grown(n + 1))
        grown(1:n) = self%ends
        grown(n + 1) = point()
        call move_alloc(grown, self%ends)
    end subroutine bounds_line

    !> Return a string with N spaces.  Port of rect.el:spaces-string.
    pure function spaces_string(n) result(s)
        integer(int64), intent(in) :: n
        character(len=:), allocatable :: s
        s = repeat(" ", int(max(0_int64, n)))
    end function spaces_string

    !> Delete (don't save) text in the region-rectangle.
    !> The same range of columns is deleted in each line starting with the
    !> line where the region begins and ending with the line where the region
    !> ends.
    !>
    !> When called from a program the rectangle's corners are START and END.
    !> With a prefix (or a FILL) argument, also fill lines where nothing has
    !> to be deleted.  Port of rect.el:delete-rectangle.
    recursive subroutine delete_rectangle(start, end, status, fill)
        integer(int64), intent(in) :: start, end
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: fill
        type(delete_line_action) :: action
        logical :: saved
        if (present(fill)) action%fill = fill
        ! (let (indent-tabs-mode) ...)
        saved = indent_tabs_mode%value
        indent_tabs_mode%value = .false.
        call apply_on_rectangle(action, start, end, status)
        indent_tabs_mode%value = saved
    end subroutine delete_rectangle

    !> Delete the contents of the rectangle with corners at START and END.
    !> Return it as RECTANGLE, one string for each line of the rectangle.
    !>
    !> When called from a program the rectangle's corners are START and END.
    !> With an optional FILL argument, also fill lines where nothing has to be
    !> deleted.  Port of rect.el:delete-extract-rectangle.
    recursive subroutine delete_extract_rectangle(start, end, rectangle, status, fill)
        integer(int64), intent(in) :: start, end
        type(rectangle_line_type), allocatable, intent(out) :: rectangle(:)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: fill
        type(delete_extract_line_action) :: action
        if (present(fill)) action%fill = fill
        allocate (action%lines(0))
        call apply_on_rectangle(action, start, end, status)
        call move_alloc(action%lines, rectangle)
    end subroutine delete_extract_rectangle

    !> Return the contents of the rectangle with corners at START and END.
    !> Return it as RECTANGLE, one string for each line of the rectangle.
    !> Port of rect.el:extract-rectangle.
    recursive subroutine extract_rectangle(start, end, rectangle, status)
        integer(int64), intent(in) :: start, end
        type(rectangle_line_type), allocatable, intent(out) :: rectangle(:)
        type(status_type), intent(out) :: status
        type(extract_line_action) :: action
        allocate (action%lines(0))
        call apply_on_rectangle(action, start, end, status)
        call move_alloc(action%lines, rectangle)
    end subroutine extract_rectangle

    !> Return the bounds of the rectangle with corners at START and END.
    !> Return it as STARTS and ENDS, the start and end positions of each
    !> line of the rectangle.  Port of rect.el:extract-rectangle-bounds.
    subroutine extract_rectangle_bounds(start, end, starts, ends, status)
        integer(int64), intent(in) :: start, end
        integer(int64), allocatable, intent(out) :: starts(:), ends(:)
        type(status_type), intent(out) :: status
        type(bounds_line_action) :: action
        allocate (action%starts(0), action%ends(0))
        call apply_on_rectangle(action, start, end, status)
        call move_alloc(action%starts, starts)
        call move_alloc(action%ends, ends)
    end subroutine extract_rectangle_bounds

    !> Delete the region-rectangle and save it as the last killed one.
    !>
    !> When called from a program the rectangle's corners are START and END.
    !> You might prefer to use `delete_extract_rectangle' from a program.
    !>
    !> With a prefix (or a FILL) argument, also fill lines where nothing has to be
    !> deleted.
    !>
    !> If the buffer is read-only, Emacs will beep and refrain from deleting
    !> the rectangle, but put it in `killed_rectangle' anyway.  This means that
    !> you can use this command to copy text from a read-only buffer.
    !> (If the variable `kill_read_only_ok' is true, then this won't
    !> even beep.)  Port of rect.el:kill-rectangle.
    recursive subroutine kill_rectangle(start, end, status, fill)
        integer(int64), intent(in) :: start, end
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: fill
        type(rectangle_line_type), allocatable :: rect(:)
        type(status_type) :: st
        logical :: saved
        ! (let (indent-tabs-mode) ...)
        saved = indent_tabs_mode%value
        indent_tabs_mode%value = .false.
        call delete_extract_rectangle(start, end, rect, status, fill)
        indent_tabs_mode%value = saved
        if (status%ok()) then
            call move_alloc(rect, killed_rectangle)
            return
        end if
        if (.not. (condition_matches(status%code, FM_BUFFER_READ_ONLY) .or. &
                   condition_matches(status%code, FM_TEXT_READ_ONLY))) return
        vdeactivate_mark = .true.
        call extract_rectangle(start, end, rect, st)
        call move_alloc(rect, killed_rectangle)
        if (kill_read_only_ok) then
            status = status_type()
            call message(format_message("Read only text copied to `killed_rectangle'"))
            return
        end if
        call barf_if_buffer_read_only(status)
        if (status%ok()) call set_error(status, FM_TEXT_READ_ONLY, "#<buffer "//buffers(cur)%name//">")
    end subroutine kill_rectangle

    !> Copy the region-rectangle and save it as the last killed one.
    !> Port of rect.el:copy-rectangle-as-kill (called from a program; the
    !> command also indicates the copied region).
    recursive subroutine copy_rectangle_as_kill(start, end, status)
        integer(int64), intent(in) :: start, end
        type(status_type), intent(out) :: status
        type(rectangle_line_type), allocatable :: rect(:)
        call extract_rectangle(start, end, rect, status)
        call move_alloc(rect, killed_rectangle)
        vdeactivate_mark = .true.
    end subroutine copy_rectangle_as_kill

    !> Yank the last killed rectangle with upper left corner at point.
    !> Port of rect.el:yank-rectangle.
    recursive subroutine yank_rectangle(status)
        type(status_type), intent(out) :: status
        type(rectangle_line_type), allocatable :: rect(:)
        if (allocated(killed_rectangle)) then
            rect = killed_rectangle
        else
            allocate (rect(0))
        end if
        call insert_rectangle(rect, status)
    end subroutine yank_rectangle

    !> Insert text of RECTANGLE with upper left corner at point.
    !> RECTANGLE's first line is inserted at point, its second
    !> line is inserted at a point vertically under point, etc.
    !> RECTANGLE should be an array of strings.
    !> After this command, the mark is at the upper left corner
    !> and point is at the lower right corner.  Port of
    !> rect.el:insert-rectangle.
    recursive subroutine insert_rectangle(rectangle, status)
        type(rectangle_line_type), intent(in) :: rectangle(:)
        type(status_type), intent(out) :: status
        integer(int64) :: insertcolumn, col, shortage
        integer :: k
        status = status_type()
        insertcolumn = current_column()
        call push_mark()
        do k = 1, size(rectangle)
            if (k > 1) then
                shortage = forward_line(1_int64)
                if (.not. bolp()) then
                    call insert(char(10), status)
                    if (.not. status%ok()) return
                end if
                call move_to_column(insertcolumn, col, force=.true., status=status)
                if (.not. status%ok()) return
            end if
            if (allocated(rectangle(k)%text)) then
                call insert_for_yank(rectangle(k)%text, status)
            else
                call insert_for_yank("", status)
            end if
            if (.not. status%ok()) return
        end do
    end subroutine insert_rectangle

    !> Blank out the region-rectangle, shifting text right.
    !>
    !> The text previously in the region is not overwritten by the blanks,
    !> but instead winds up to the right of the rectangle.
    !>
    !> When called from a program the rectangle's corners are START and END.
    !> With a prefix (or a FILL) argument, fill with blanks even if there is
    !> no text on the right side of the rectangle.  Port of
    !> rect.el:open-rectangle.
    recursive subroutine open_rectangle(start, end, status, fill)
        integer(int64), intent(in) :: start, end
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: fill
        type(open_line_action) :: action
        if (present(fill)) action%fill = fill
        call apply_on_rectangle(action, start, end, status)
        if (status%ok()) call goto_char(start)
    end subroutine open_rectangle

    !> Port of rect.el:open-rectangle-line.
    recursive subroutine open_line(self, startcol, endcol, status)
        class(open_line_action), intent(inout) :: self
        integer(int64), intent(in) :: startcol, endcol
        type(status_type), intent(out) :: status
        integer(int64) :: col
        if (move_to_column_fill(startcol, self%fill, status) == startcol) then
            if (.not. status%ok()) return
            if (self%fill .or. point() /= line_end_position()) col = indent_to(endcol, status)
        end if
    end subroutine open_line

    !> Port of rect.el:delete-whitespace-rectangle-line.
    recursive subroutine delete_whitespace_line(self, startcol, endcol, status)
        class(delete_whitespace_line_action), intent(inout) :: self
        integer(int64), intent(in) :: startcol, endcol
        type(status_type), intent(out) :: status
        integer(int64) :: from, moved
        if (move_to_column_fill(startcol, self%fill, status) == startcol) then
            if (.not. status%ok()) return
            if (point() /= line_end_position()) then
                from = point()
                moved = skip_syntax_forward(" ", line_end_position())
                call delete_region(from, point(), status)
            end if
        end if
    end subroutine delete_whitespace_line

    !> Delete all whitespace following a specified column in each line.
    !> The left edge of the rectangle specifies the position in each line
    !> at which whitespace deletion should begin.  On each line in the
    !> rectangle, all contiguous whitespace starting at that column is deleted.
    !>
    !> When called from a program the rectangle's corners are START and END.
    !> With a prefix (or a FILL) argument, also fill too short lines.  Port
    !> of rect.el:delete-whitespace-rectangle.
    recursive subroutine delete_whitespace_rectangle(start, end, status, fill)
        integer(int64), intent(in) :: start, end
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: fill
        type(delete_whitespace_line_action) :: action
        if (present(fill)) action%fill = fill
        call apply_on_rectangle(action, start, end, status)
    end subroutine delete_whitespace_rectangle

    !> Port of rect.el:string-rectangle-line.
    recursive subroutine string_line(self, startcol, endcol, status)
        class(string_line_action), intent(inout) :: self
        integer(int64), intent(in) :: startcol, endcol
        type(status_type), intent(out) :: status
        integer(int64) :: col
        call move_to_column(startcol, col, force=.true., status=status)
        if (.not. status%ok()) return
        if (self%delete) then
            call delete_rectangle_line(startcol, endcol, .false., status)
            if (.not. status%ok()) return
        end if
        call insert(self%string, status)
    end subroutine string_line

    !> Replace rectangle contents with STRING on each line.
    !> The length of STRING need not be the same as the rectangle width.
    !>
    !> Called from a program, takes three args; START, END and STRING.
    !> Port of rect.el:string-rectangle (no preview).
    recursive subroutine string_rectangle(start, end, string, status)
        integer(int64), intent(in) :: start, end
        character(len=*), intent(in) :: string
        type(status_type), intent(out) :: status
        type(string_line_action) :: action
        type(undo_entry_type) :: entry
        integer(int64) :: final
        ! If we undo this change, we want to have the point back where we
        ! are now, and not after the first line in the rectangle (which is
        ! the first line to be changed by the following command).
        if (buffers(cur)%undo_enabled) then
            entry%kind = UE_POINT
            entry%beg = point()
            call push_undo_entry(entry)
        end if
        action%string = string
        action%delete = .true.
        call apply_on_rectangle(action, start, end, status, final)
        if (status%ok()) call goto_char(final)
    end subroutine string_rectangle

    !> Insert STRING on each line of region-rectangle, shifting text right.
    !>
    !> When called from a program, the rectangle's corners are START and END.
    !> The left edge of the rectangle specifies the column for insertion.
    !> This command does not delete or overwrite any existing text.  Port
    !> of rect.el:string-insert-rectangle.
    recursive subroutine string_insert_rectangle(start, end, string, status)
        integer(int64), intent(in) :: start, end
        character(len=*), intent(in) :: string
        type(status_type), intent(out) :: status
        type(string_line_action) :: action
        action%string = string
        action%delete = .false.
        call apply_on_rectangle(action, start, end, status)
    end subroutine string_insert_rectangle

    !> Blank out the region-rectangle.
    !> The text previously in the region is overwritten with blanks.
    !>
    !> When called from a program the rectangle's corners are START and END.
    !> With a prefix (or a FILL) argument, also fill with blanks the parts of the
    !> rectangle which were empty.  Port of rect.el:clear-rectangle.
    recursive subroutine clear_rectangle(start, end, status, fill)
        integer(int64), intent(in) :: start, end
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: fill
        type(clear_line_action) :: action
        if (present(fill)) action%fill = fill
        call apply_on_rectangle(action, start, end, status)
    end subroutine clear_rectangle

    !> Port of rect.el:clear-rectangle-line.
    recursive subroutine clear_line(self, startcol, endcol, status)
        class(clear_line_action), intent(inout) :: self
        integer(int64), intent(in) :: startcol, endcol
        type(status_type), intent(out) :: status
        integer(int64) :: pt, col, here, eolcol, ecol
        pt = line_end_position()
        if (move_to_column_fill(startcol, self%fill, status) == startcol) then
            if (.not. status%ok()) return
            ! (save-excursion (goto-char pt) (current-column))
            here = point()
            call goto_char(pt)
            eolcol = current_column()
            call goto_char(here)
            if (.not. self%fill .and. eolcol <= endcol) then
                call delete_region(point(), pt, status)
            else
                pt = point()
                call move_to_column(endcol, col, force=.true., status=status)
                if (.not. status%ok()) return
                ecol = current_column()
                call delete_region(pt, point(), status)
                if (.not. status%ok()) return
                col = indent_to(ecol, status)
            end if
        end if
    end subroutine clear_line

    !> Port of rect.el:rectangle-number-line-callback.
    recursive subroutine number_line(self, startcol, endcol, status)
        class(number_line_action), intent(inout) :: self
        integer(int64), intent(in) :: startcol, endcol
        type(status_type), intent(out) :: status
        integer(int64) :: col
        character(len=:), allocatable :: text
        call move_to_column(startcol, col, force=.true., status=status)
        if (.not. status%ok()) return
        call format_integer(self%format, self%counter, text, status)
        if (.not. status%ok()) return
        call insert(text, status)
        if (.not. status%ok()) return
        self%counter = self%counter + 1
    end subroutine number_line

    !> The decimal digits of N.
    pure function int_text(n) result(s)
        integer(int64), intent(in) :: n
        character(len=:), allocatable :: s
        character(len=24) :: buf
        write (buf, "(i0)") n
        s = trim(buf)
    end function int_text

    !> Port of rect.el:rectangle--default-line-number-format.
    function default_line_number_format(start, end, start_at) result(format)
        integer(int64), intent(in) :: start, end, start_at
        character(len=:), allocatable :: format
        format = "%"//int_text(len(int_text(count_lines(start, end) + start_at), kind=int64))//"d "
    end function default_line_number_format

    !> Insert numbers in front of the region-rectangle.
    !>
    !> START_AT, if present, should be a number from which to begin
    !> counting.  FORMAT, if present, should be a format string to pass
    !> to `format' along with the line count.  When called interactively
    !> with a prefix argument, prompt for START_AT and FORMAT.  Port of
    !> rect.el:rectangle-number-lines.
    recursive subroutine rectangle_number_lines(start, end, start_at, status, format)
        integer(int64), intent(in) :: start, end, start_at
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: format
        type(number_line_action) :: action
        if (present(format)) then
            action%format = format
        else
            action%format = default_line_number_format(start, end, start_at)
        end if
        action%counter = start_at
        call apply_on_rectangle(action, start, end, status)
    end subroutine rectangle_number_lines

    ! ------------------------------------------------------------------
    ! `format' with one integer argument
    ! ------------------------------------------------------------------

    !> Format the integer ARG by the format string STRING, as (format
    !> STRING ARG) does.  The part of editfns.c:styled_format that
    !> `rectangle_number_lines' needs: %%, field numbers, the flags - + SPC
    !> # 0, widths, precisions and the conversions d, i, o, x, X, b, B, c, s
    !> and S of a fixnum.  The floating-point conversions are deferred (an
    !> error); a field number 0 (the format string as argument) is an
    !> error as for a missing argument.
    subroutine format_integer(string, arg, result, status)
        character(len=*), intent(in) :: string
        integer(int64), intent(in) :: arg
        character(len=:), allocatable, intent(out) :: result
        type(status_type), intent(out) :: status
        integer(int64) :: i, n, num, field_width, precision, j, nchars, wsum, lim
        logical :: minus_flag, plus_flag, space_flag, sharp_flag, zero_flag, precision_given, num_given
        character :: conversion
        character(len=:), allocatable :: item, sign, prefix, digits
        integer(int32) :: c, clen, nbytes
        character(len=MAX_MULTIBYTE_LENGTH) :: buf
        result = ""
        n = 0
        i = 1
        do while (i <= len(string, kind=int64))
            if (string(i:i) /= "%") then
                result = result//string(i:i)
                i = i + 1
                cycle
            end if
            i = i + 1
            ! [field-number] [flags] [field-width] [precision] format
            call parse_number(string, i, num, j, num_given)
            if (num_given .and. j <= len(string, kind=int64)) then
                if (string(j:j) == "$") then
                    n = num - 1
                    i = j + 1
                end if
            end if
            minus_flag = .false.
            plus_flag = .false.
            space_flag = .false.
            sharp_flag = .false.
            zero_flag = .false.
            do while (i <= len(string, kind=int64))
                select case (string(i:i))
                case ("-")
                    minus_flag = .true.
                case ("+")
                    plus_flag = .true.
                case (" ")
                    space_flag = .true.
                case ("#")
                    sharp_flag = .true.
                case ("0")
                    zero_flag = .true.
                case default
                    exit
                end select
                i = i + 1
            end do
            ! Ignore flags when sprintf ignores them.
            space_flag = space_flag .and. .not. plus_flag
            zero_flag = zero_flag .and. .not. minus_flag
            call parse_number(string, i, field_width, j, num_given)
            i = j
            precision_given = .false.
            precision = huge(0_int64)
            if (i <= len(string, kind=int64)) then
                if (string(i:i) == ".") then
                    precision_given = .true.
                    call parse_number(string, i + 1, precision, j, num_given)
                    i = j
                end if
            end if
            if (i > len(string, kind=int64)) then
                call set_error(status, FM_GENERIC_ERROR, "Format string ends in middle of format specifier")
                return
            end if
            conversion = string(i:i)
            i = i + 1
            if (conversion == "%") then
                result = result//"%"
                cycle
            end if
            n = n + 1
            if (n /= 1) then
                call set_error(status, FM_GENERIC_ERROR, "Not enough arguments for format string")
                return
            end if
            if (conversion == "S" .or. conversion == "s") then
                item = int_text(arg)
                conversion = "s"
            else if (conversion == "c") then
                if (arg < 0 .or. arg > MAX_CHAR) then
                    call set_error(status, FM_WRONG_TYPE_ARGUMENT, "characterp, "//int_text(arg))
                    return
                end if
                call char_string(int(arg, int32), buf, nbytes, status)
                if (.not. status%ok()) return
                item = buf(1:nbytes)
                if (arg > 127) conversion = "s"
                zero_flag = .false.
                if (precision_given .and. precision == 0) item = ""
            else if (scan(conversion, "efg") > 0) then
                call set_error(status, FM_GENERIC_ERROR, "Floating-point format directives are not ported yet")
                return
            else if (scan(conversion, "dioxXbB") == 0) then
                call set_error(status, FM_GENERIC_ERROR, "Invalid format operation %"//conversion)
                return
            end if
            if (conversion == "s") then
                ! Truncate to PRECISION columns, then pad to FIELD_WIDTH
                ! columns (lisp_string_width).
                lim = huge(0_int64)
                if (precision_given) lim = precision
                j = 1
                wsum = 0
                nchars = 0
                do while (j <= len(item, kind=int64))
                    call string_char_and_length(item, j, c, clen)
                    if (wsum + glyph_width(c) > lim) exit
                    wsum = wsum + glyph_width(c)
                    j = j + clen
                    nchars = nchars + 1
                end do
                item = item(1:j - 1)
                if (wsum < field_width) then
                    if (minus_flag) then
                        item = item//spaces_string(field_width - wsum)
                    else
                        item = spaces_string(field_width - wsum)//item
                    end if
                end if
                result = result//item
                cycle
            end if
            if (conversion == "c") then
                if (len(item) < field_width) then
                    if (minus_flag) then
                        item = item//spaces_string(field_width - len(item, kind=int64))
                    else
                        item = spaces_string(field_width - len(item, kind=int64))//item
                    end if
                end if
                result = result//item
                cycle
            end if
            ! An integer conversion: sign, prefix, digits (sprintf with
            ! the precision), then padding or leading zeros.
            if (conversion == "d" .or. conversion == "i") sharp_flag = .false.
            zero_flag = zero_flag .and. .not. precision_given
            if (arg < 0) then
                sign = "-"
            else if (plus_flag) then
                sign = "+"
            else if (space_flag) then
                sign = " "
            else
                sign = ""
            end if
            select case (conversion)
            case ("o")
                digits = magnitude_digits(arg, 8_int64, .false.)
            case ("x")
                digits = magnitude_digits(arg, 16_int64, .false.)
            case ("X")
                digits = magnitude_digits(arg, 16_int64, .true.)
            case ("b", "B")
                digits = magnitude_digits(arg, 2_int64, .false.)
            case default
                digits = magnitude_digits(arg, 10_int64, .false.)
            end select
            if (precision_given) then
                if (precision == 0 .and. arg == 0) digits = ""
                if (len(digits, kind=int64) < precision) digits = repeat("0", int(precision - len(digits)))//digits
            end if
            prefix = ""
            if (sharp_flag) then
                select case (conversion)
                case ("o")
                    if (len(digits) == 0) then
                        digits = "0"
                    else if (digits(1:1) /= "0") then
                        digits = "0"//digits
                    end if
                case ("x", "X", "b", "B")
                    if (arg /= 0) prefix = "0"//conversion
                case default
                    continue
                end select
            end if
            item = sign//prefix//digits
            if (len(item, kind=int64) < field_width) then
                if (zero_flag .and. len(digits) > 0) then
                    item = sign//prefix//repeat("0", int(field_width - len(item)))//digits
                else if (minus_flag) then
                    item = item//spaces_string(field_width - len(item, kind=int64))
                else
                    item = spaces_string(field_width - len(item, kind=int64))//item
                end if
            end if
            result = result//item
        end do
        status = status_type()
    end subroutine format_integer

    !> The digits [0-9]* of STRING from I on: their value NUM (0 if none),
    !> the index after them NEXT and whether there were any.
    pure subroutine parse_number(string, i, num, next, given)
        character(len=*), intent(in) :: string
        integer(int64), intent(in) :: i
        integer(int64), intent(out) :: num, next
        logical, intent(out) :: given
        num = 0
        next = i
        given = .false.
        do while (next <= len(string, kind=int64))
            if (verify(string(next:next), "0123456789") /= 0) exit
            if (num < 100000000000000000_int64) num = num*10 + (iachar(string(next:next)) - iachar("0"))
            next = next + 1
            given = .true.
        end do
    end subroutine parse_number

    !> The digits of |N| in BASE.
    pure function magnitude_digits(n, base, upper) result(s)
        integer(int64), intent(in) :: n, base
        logical, intent(in) :: upper
        character(len=:), allocatable :: s
        character(len=*), parameter :: lower_digits = "0123456789abcdef", upper_digits = "0123456789ABCDEF"
        integer(int64) :: m, d
        s = ""
        m = abs(n)
        do
            d = mod(m, base)
            if (upper) then
                s = upper_digits(d + 1:d + 1)//s
            else
                s = lower_digits(d + 1:d + 1)//s
            end if
            m = m/base
            if (m == 0) exit
        end do
    end function magnitude_digits

    ! ------------------------------------------------------------------
    ! Commands
    ! ------------------------------------------------------------------

    !> The region's bounds for an interactive "r" spec.
    subroutine interactive_region(start, end, status)
        integer(int64), intent(out) :: start, end
        type(status_type), intent(out) :: status
        end = 0
        call region_beginning(start, status)
        if (status%ok()) call region_end(end, status)
    end subroutine interactive_region

    !> The value of the prefix argument as the FILL of a "P" spec.
    logical function prefix_fill()
        prefix_fill = .not. nilp(current_prefix_arg)
    end function prefix_fill

    recursive subroutine cmd_delete_rectangle(status)
        type(status_type), intent(out) :: status
        integer(int64) :: start, end
        call barf_if_buffer_read_only(status)
        if (.not. status%ok()) return
        call interactive_region(start, end, status)
        if (status%ok()) call delete_rectangle(start, end, status, prefix_fill())
    end subroutine cmd_delete_rectangle

    recursive subroutine cmd_kill_rectangle(status)
        type(status_type), intent(out) :: status
        integer(int64) :: start, end
        call interactive_region(start, end, status)
        if (status%ok()) call kill_rectangle(start, end, status, prefix_fill())
    end subroutine cmd_kill_rectangle

    !> Copy the region-rectangle and save it as the last killed one.
    !> Port of rect.el:copy-rectangle-as-kill.
    recursive subroutine cmd_copy_rectangle_as_kill(status)
        type(status_type), intent(out) :: status
        integer(int64) :: start, end
        call interactive_region(start, end, status)
        if (.not. status%ok()) return
        call copy_rectangle_as_kill(start, end, status)
        if (.not. status%ok()) return
        if (size(killed_rectangle) > 0) then
            call indicate_copied_region(len_chars(killed_rectangle(1)%text))
        else
            ! (length (car nil)) is 0.
            call indicate_copied_region(0_int64)
        end if
    end subroutine cmd_copy_rectangle_as_kill

    !> The number of characters of the UTF-8 text S.
    pure integer(int64) function len_chars(s) result(n)
        character(len=*), intent(in) :: s
        integer(int64) :: k
        n = 0
        do k = 1, len(s, kind=int64)
            if (iand(iachar(s(k:k)), 192) /= 128) n = n + 1
        end do
    end function len_chars

    recursive subroutine cmd_yank_rectangle(status)
        type(status_type), intent(out) :: status
        call barf_if_buffer_read_only(status)
        if (status%ok()) call yank_rectangle(status)
    end subroutine cmd_yank_rectangle

    recursive subroutine cmd_open_rectangle(status)
        type(status_type), intent(out) :: status
        integer(int64) :: start, end
        call barf_if_buffer_read_only(status)
        if (.not. status%ok()) return
        call interactive_region(start, end, status)
        if (status%ok()) call open_rectangle(start, end, status, prefix_fill())
    end subroutine cmd_open_rectangle

    recursive subroutine cmd_delete_whitespace_rectangle(status)
        type(status_type), intent(out) :: status
        integer(int64) :: start, end
        call barf_if_buffer_read_only(status)
        if (.not. status%ok()) return
        call interactive_region(start, end, status)
        if (status%ok()) call delete_whitespace_rectangle(start, end, status, prefix_fill())
    end subroutine cmd_delete_whitespace_rectangle

    recursive subroutine cmd_clear_rectangle(status)
        type(status_type), intent(out) :: status
        integer(int64) :: start, end
        call barf_if_buffer_read_only(status)
        if (.not. status%ok()) return
        call interactive_region(start, end, status)
        if (status%ok()) call clear_rectangle(start, end, status, prefix_fill())
    end subroutine cmd_clear_rectangle

    !> The newest element of `string_rectangle_history', or "" (the
    !> (or (car string-rectangle-history) "") of the prompts).
    function history_head() result(head)
        character(len=:), allocatable :: head
        type(value_type) :: hist, first
        type(status_type) :: st
        head = ""
        call symbol_value(sym("string_rectangle_history"), hist, st)
        if (.not. st%ok()) return
        if (list_length(hist) == 0) return
        first = list_elt(hist, 1_int64)
        if (stringp(first)) head = xstring(first)
    end function history_head

    !> (format-prompt PROMPT DEFAULT): " (default DEFAULT)" unless DEFAULT
    !> is empty, then ": ".
    function prompt_with_default(prompt, default) result(full)
        character(len=*), intent(in) :: prompt, default
        character(len=:), allocatable :: full
        full = prompt
        if (len(default) > 0) full = full//" (default "//default//")"
        full = full//": "
    end function prompt_with_default

    !> Read the string of `string_rectangle' and `string_insert_rectangle'
    !> with PROMPT (their interactive specs).
    recursive subroutine read_rectangle_string(prompt, string, status)
        character(len=*), intent(in) :: prompt
        character(len=:), allocatable, intent(out) :: string
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: head
        head = history_head()
        if (len(head) > 0) then
            call read_string(prompt_with_default(prompt, head), string, status, &
                             history="string_rectangle_history", default=head)
        else
            call read_string(prompt_with_default(prompt, head), string, status, history="string_rectangle_history")
        end if
    end subroutine read_rectangle_string

    !> Replace rectangle contents with STRING on each line.
    !> The length of STRING need not be the same as the rectangle width.
    !>
    !> When called interactively and option `rectangle_preview' is
    !> true, display the result as the user enters the string into
    !> the minibuffer.
    !>
    !> Called from a program, takes three args; START, END and STRING.
    !> Port of rect.el:string-rectangle (the preview is deferred).
    recursive subroutine cmd_string_rectangle(status)
        type(status_type), intent(out) :: status
        integer(int64) :: start, end
        character(len=:), allocatable :: string
        call region_beginning(start, status)
        if (status%ok()) call region_end(end, status)
        if (.not. status%ok()) return
        call barf_if_buffer_read_only(status)
        if (.not. status%ok()) return
        call read_rectangle_string("String rectangle", string, status)
        if (status%ok()) call string_rectangle(start, end, string, status)
    end subroutine cmd_string_rectangle

    recursive subroutine cmd_string_insert_rectangle(status)
        type(status_type), intent(out) :: status
        integer(int64) :: start, end
        character(len=:), allocatable :: string
        call barf_if_buffer_read_only(status)
        if (.not. status%ok()) return
        call region_beginning(start, status)
        if (status%ok()) call region_end(end, status)
        if (.not. status%ok()) return
        call read_rectangle_string("String insert rectangle", string, status)
        if (status%ok()) call string_insert_rectangle(start, end, string, status)
    end subroutine cmd_string_insert_rectangle

    recursive subroutine cmd_rectangle_number_lines(status)
        type(status_type), intent(out) :: status
        integer(int64) :: start, end, start_at
        character(len=:), allocatable :: format
        call region_beginning(start, status)
        if (status%ok()) call region_end(end, status)
        if (.not. status%ok()) return
        if (nilp(current_prefix_arg)) then
            call rectangle_number_lines(start, end, 1_int64, status)
            return
        end if
        call read_number("Number to count from: ", start_at, status, default=1_int64)
        if (.not. status%ok()) return
        call read_string("Format string: ", format, status, initial=default_line_number_format(start, end, start_at))
        if (status%ok()) call rectangle_number_lines(start, end, start_at, status, format)
    end subroutine cmd_rectangle_number_lines

    !> Register the commands and `string_rectangle_history'.
    subroutine syms_of_rect()
        call defvar_nil("string_rectangle_history")
        commands%delete_rectangle%run => cmd_delete_rectangle
        commands%kill_rectangle%run => cmd_kill_rectangle
        commands%copy_rectangle_as_kill%run => cmd_copy_rectangle_as_kill
        commands%yank_rectangle%run => cmd_yank_rectangle
        commands%open_rectangle%run => cmd_open_rectangle
        commands%delete_whitespace_rectangle%run => cmd_delete_whitespace_rectangle
        commands%string_rectangle%run => cmd_string_rectangle
        commands%string_insert_rectangle%run => cmd_string_insert_rectangle
        commands%clear_rectangle%run => cmd_clear_rectangle
        commands%rectangle_number_lines%run => cmd_rectangle_number_lines
    end subroutine syms_of_rect
end module rect_fm
