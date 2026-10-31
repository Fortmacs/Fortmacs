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
!> `rectangle_mark_mode' (C-x SPC) makes the region rectangular: its
!> keymap remaps the motion commands and C-x C-x to ones that keep a
!> corner's column where point can't be (the crutches), redisplay
!> highlights the rectangle with an overlay per line, `region_beginning'
!> and `region_end' give its corners, and the region's text is
!> extracted and inserted as a rectangle (a killed rectangle yanks as
!> one through its `yank_handler'); the mode ends with the region.
!>
!> Deferred: the on-the-fly preview of `string_rectangle'
!> (`rectangle_preview', the `rectangle_preview' face: needs a
!> post-command hook in the minibuffer) and with it
!> `rectangle__inhibit_region_highlight';
!> `rectangle_indicate_zero_width_rectangle' (a face height of 0.2: no
!> floats); `rectangle__duplicate_right' (duplicate-dwim is not ported);
!> the obsolete `operate_on_rectangle', `close_rectangle' and
!> `replace_rectangle'; `inherit_input_method' in the prompts (no input
!> methods); the floating-point directives %e, %f and %g of a
!> `rectangle_number_lines' FORMAT (an error).
module rect_fm
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use commands_fm, only: commands
    use error_fm, only: status_type, set_error, format_message, condition_matches, FM_GENERIC_ERROR, &
                        FM_BUFFER_READ_ONLY, FM_TEXT_READ_ONLY, FM_WRONG_TYPE_ARGUMENT, FM_END_OF_BUFFER, &
                        FM_BEGINNING_OF_BUFFER
    use handle_fm, only: handle_type
    use lisp_fm, only: value_type, nilp, list_length, list_elt, stringp, xstring, make_string, make_list, list, &
                       list_elements, t_value, nil, make_fixnum, make_window_ref
    use data_fm, only: value_eq
    use eval_fm, only: hook_type
    use modes_fm, only: minor_modes
    use textprop_fm, only: string_put_text_property
    use itree_fm, only: make_overlay, move_overlay, overlay_put, overlay_get, free_overlay
    use frame_fm, only: selected_window
    use window_fm, only: windows
    use dispnew_fm, only: ding
    use keyboard_fm, only: last_command
    use keymap_fm, only: define_key, key_parse
    use buffer_fm, only: current_buffer, buffer_chars_modified_tick
    use character_fm, only: multibyte_chars_in_text
    use editfns_fm, only: region_beginning_function, region_end_function, region_position_proc, eolp, mark_position
    use cmds_fm, only: forward_char
    use simple_fm, only: region_extract_function, region_insert_function, region_extract_proc, region_insert_proc, &
                         REGION_EXTRACT_COPY, REGION_EXTRACT_BOUNDS, yank_undo_function, undo_start, undo_more, &
                         pending_undo_list, define_yank_handler_function, deactivate_mark_hook, region_active_p, &
                         set_mark, exchange_point_and_mark
    use xdisp_fm, only: redisplay_highlight_region_function, redisplay_unhighlight_region_function, &
                        highlight_region_proc, unhighlight_region_proc
    use lread_fm, only: sym
    use data_fm, only: symbol_value
    use character_fm, only: string_char_and_length, char_string, MAX_MULTIBYTE_LENGTH, MAX_CHAR
    use buffer_fm, only: buffers, cur, barf_if_buffer_read_only, deactivate_mark_variable, undo_entry_type, UE_POINT
    use marker_fm, only: copy_marker_at, free_marker, marker_position, char_to_byte
    use undo_fm, only: push_undo_entry
    use insdel_fm, only: insert
    use editfns_fm, only: point, goto_char, line_beginning_position, line_end_position, bolp, buffer_substring, &
                          delete_region, delete_and_extract_region, region_beginning, region_end, char_after, &
                          interactive_region, char_before
    use cmds_fm, only: forward_line
    use indent_fm, only: current_column, move_to_column, indent_to, indent_tabs_mode
    use syntax_fm, only: skip_syntax_forward
    use xdisp_fm, only: message, glyph_width
    use simple_fm, only: push_mark, insert_for_yank, kill_read_only_ok, indicate_copied_region, count_lines, &
                         line_number_at_pos
    use minibuf_fm, only: read_string
    use subr_fm, only: read_number, defvar_nil
    use callint_fm, only: current_prefix_arg, this_command, prefix_numeric_value
    implicit none(type, external)
    private

    public :: apply_on_rectangle, rectangle_position_as_coordinates, rectangle_intersect_p, rectangle_dimensions, &
              delete_rectangle, delete_extract_rectangle, extract_rectangle, extract_rectangle_bounds, &
              kill_rectangle, copy_rectangle_as_kill, yank_rectangle, insert_rectangle, open_rectangle, &
              delete_whitespace_rectangle, string_rectangle, string_insert_rectangle, clear_rectangle, &
              rectangle_number_lines, spaces_string, syms_of_rect, format_integer
    public :: rectangle_mark_mode, rectangle_exchange_point_and_mark, rectangle_right_char, rectangle_left_char, &
              rectangle_forward_char, rectangle_backward_char, rectangle_next_line, rectangle_previous_line, &
              rectangle_pos_cols

    !> One line of a rectangle (a string of upstream's list of strings).
    type, public :: rectangle_line_type
        character(len=:), allocatable :: text
    end type rectangle_line_type

    !> Rectangle for `yank_rectangle' to insert.  Unallocated (or empty)
    !> for none.  Port of rect.el killed-rectangle.
    type(rectangle_line_type), allocatable, public :: killed_rectangle(:)

    !> A corner's crutch: the column COL to use for position POS where
    !> point can't be (in the middle of a TAB, or past the EOL), of a
    !> buffer or window OWNER (rect.el's (POS . COL)).
    type :: crutch_type
        type(handle_type) :: owner
        integer(int64) :: pos = 0, col = 0
    end type crutch_type

    !> The mark's crutches by buffer (rect.el rectangle--mark-crutches,
    !> buffer-local) and point's by window (the window parameter
    !> `rectangle--point-crutches').
    type(crutch_type), allocatable :: mark_crutches(:), point_crutches(:)

    !> The rectangle's highlighting in a window (the `rectangle' value of
    !> its `internal-region-overlay' parameter): what it was made for and
    !> its overlays.
    type :: rectangle_highlight_type
        type(handle_type) :: window
        integer(int64) :: tick = -1, start = 0, end = 0, pt = 0
        integer(int64) :: crutches(6) = 0
        type(handle_type), allocatable :: overlays(:)
    end type rectangle_highlight_type
    type(rectangle_highlight_type), allocatable :: highlights(:)

    !> The undo list entry before the last yanked rectangle.
    integer(int64) :: undo_at_start = 0

    !> The functions Rectangle-Mark mode goes around (add-function
    !> :around and advice-add), and whether `region_beginning' and
    !> `region_end' are advised yet.
    procedure(region_position_proc), pointer :: orig_region_beginning => null(), orig_region_end => null()
    procedure(region_extract_proc), pointer :: orig_region_extract => null()
    procedure(region_insert_proc), pointer :: orig_region_insert => null()
    procedure(highlight_region_proc), pointer :: orig_highlight => null()
    procedure(unhighlight_region_proc), pointer :: orig_unhighlight => null()
    logical :: advised = .false.

    !> Keymap used while marking a rectangular region.  Port of rect.el
    !> rectangle-mark-mode-map.
    type(handle_type), public :: rectangle_mark_mode_map

    !> Hook run after entering or leaving `rectangle_mark_mode'.
    type(hook_type), target, public :: rectangle_mark_mode_hook = hook_type(name="rectangle_mark_mode_hook")

    !> What `apply_on_rectangle' does on each line of a rectangle:
    !> upstream's FUNCTION with its extra ARGS, here an extension of this
    !> type holding the arguments, with its `line' procedure.
    type, abstract, public :: rectangle_line_action
    contains
        procedure(rectangle_line_proc), deferred :: line
    end type rectangle_line_action

    abstract interface
        !> Act on the current line, point at its beginning; STARTCOL and
        !> ENDCOL are the rectangle's columns.  Not a port.
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

    !> Append TEXT to LINES.  Not a port.
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
    !> Not a port.
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

    ! The action of `delete_rectangle': `delete_rectangle_line' on the line.
    ! Not a port.
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

    !> The function of rect.el:extract-rectangle-bounds.  Port of the procedure
    !> of rect.el:extract-rectangle-bounds.
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
        deactivate_mark_variable = .true.
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
        deactivate_mark_variable = .true.
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
    !> With a prefix (or a FILL) argument, also fill too short lines.  Port of
    !> rect.el:delete-whitespace-rectangle.
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
    !> This command does not delete or overwrite any existing text.  Port of
    !> rect.el:string-insert-rectangle.
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

    !> The decimal digits of N.  Not a port.
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
    !> error as for a missing argument.  Port of part of
    !> editfns.c:styled_format.
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
                    call set_error(status, FM_WRONG_TYPE_ARGUMENT, "expected integer (a character code), got "//int_text(arg))
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
    !> the index after them NEXT and whether there were any.  Not a port.
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

    !> The digits of |N| in BASE.  Not a port.
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
    ! Crutches to let rectangle's corners be where point can't be
    ! (e.g. in the middle of a TAB, or past the EOL).
    ! ------------------------------------------------------------------

    !> The crutch of OWNER in CRUTCHES: whether it has one, and its
    !> position and column.  Not a port.
    subroutine get_crutch(crutches, owner, found, pos, col)
        type(crutch_type), allocatable, intent(in) :: crutches(:)
        type(handle_type), intent(in) :: owner
        logical, intent(out) :: found
        integer(int64), intent(out) :: pos, col
        integer :: k

        found = .false.
        pos = 0
        col = 0
        if (.not. allocated(crutches)) return

        do k = 1, size(crutches)
            if (same_owner(crutches(k)%owner, owner)) then
                found = .true.
                pos = crutches(k)%pos
                col = crutches(k)%col
                return
            end if
        end do
    end subroutine get_crutch

    !> Give OWNER the crutch (POS . COL) in CRUTCHES.  Not a port.
    subroutine set_crutch(crutches, owner, pos, col)
        type(crutch_type), allocatable, intent(inout) :: crutches(:)
        type(handle_type), intent(in) :: owner
        integer(int64), intent(in) :: pos, col
        type(crutch_type), allocatable :: grown(:)
        integer :: k, n

        if (.not. allocated(crutches)) allocate (crutches(0))
        n = size(crutches)

        do k = 1, n
            if (same_owner(crutches(k)%owner, owner)) then
                crutches(k)%pos = pos
                crutches(k)%col = col
                return
            end if
        end do

        allocate (grown(n + 1))
        grown(1:n) = crutches
        grown(n + 1)%owner = owner
        grown(n + 1)%pos = pos
        grown(n + 1)%col = col
        call move_alloc(grown, crutches)
    end subroutine set_crutch

    !> Remove OWNER's crutch from CRUTCHES (clearing it).  Not a port.
    subroutine clear_crutch(crutches, owner)
        type(crutch_type), allocatable, intent(inout) :: crutches(:)
        type(handle_type), intent(in) :: owner
        type(crutch_type), allocatable :: kept(:)
        integer :: k

        if (.not. allocated(crutches)) return

        do k = 1, size(crutches)
            if (same_owner(crutches(k)%owner, owner)) then
                allocate (kept(size(crutches) - 1))
                kept(1:k - 1) = crutches(1:k - 1)
                kept(k:) = crutches(k + 1:)
                call move_alloc(kept, crutches)
                return
            end if
        end do
    end subroutine clear_crutch

    ! Whether A and B are the same buffer or window.  Not a port.
    pure logical function same_owner(a, b)
        type(handle_type), intent(in) :: a, b

        same_owner = a%index == b%index .and. a%generation == b%generation
    end function same_owner

    !> The columns of START and END (either may be point or the mark) in
    !> WINDOW (default the selected window): their own, or a crutch's.
    !> A crutch that is for neither is dropped.  Port of
    !> rect.el:rectangle--pos-cols.
    subroutine rectangle_pos_cols(start, end, startcol, endcol, window)
        integer(int64), intent(in) :: start, end
        integer(int64), intent(out) :: startcol, endcol
        type(handle_type), intent(in), optional :: window
        type(handle_type) :: w, b
        integer(int64) :: opoint, cwp, cwc, mp, mc, sc, ec
        logical :: have_p, have_m

        ! At this stage, we don't know which of start/end is point/mark :-(
        ! And in case start=end, it might still be that point and mark have
        ! different crutches!
        opoint = point()
        w = selected_window()
        if (present(window)) w = window
        b = current_buffer()
        call get_crutch(point_crutches, w, have_p, cwp, cwc)
        call get_crutch(mark_crutches, b, have_m, mp, mc)

        if (have_p .and. start == cwp) then
            sc = cwc

            if (have_m .and. end == mp) then
                ec = mc
            else
                if (have_m) call clear_crutch(mark_crutches, b)
                call goto_char(end)
                ec = current_column()
            end if

            call order(sc, ec)
        else if (have_p .and. end == cwp) then
            if (have_m .and. start == mp) then
                startcol = mc
            else
                if (have_m) call clear_crutch(mark_crutches, b)
                call goto_char(start)
                startcol = current_column()
            end if

            endcol = cwc
        else
            if (have_p) call clear_crutch(point_crutches, w)

            if (have_m .and. start == mp) then
                sc = mc
                call goto_char(end)
                ec = current_column()
                call order(sc, ec)
            else if (have_m .and. end == mp) then
                call goto_char(start)
                startcol = current_column()
                endcol = mc
            else
                if (have_m) call clear_crutch(mark_crutches, b)
                call goto_char(start)
                startcol = current_column()
                call goto_char(end)
                endcol = current_column()
            end if
        end if

        call goto_char(opoint)

    contains

        subroutine order(c1, c2)
            integer(int64), intent(in) :: c1, c2

            if (start == end) then
                startcol = min(c1, c2)
                endcol = max(c1, c2)
            else
                startcol = c1
                endcol = c2
            end if
        end subroutine order
    end subroutine rectangle_pos_cols

    !> Move to column COL on the current line, and if it can't be reached
    !> exactly (past the EOL, or in the middle of a character), give point
    !> (POINT_KIND) or the mark a crutch for it.  Port of
    !> rect.el:rectangle--col-pos.
    recursive subroutine rectangle_col_pos(col, point_kind)
        integer(int64), intent(in) :: col
        logical, intent(in) :: point_kind
        integer(int64) :: c

        call move_to_column(col, c)

        if (c == col .and. .not. eolp()) then
            if (point_kind) then
                call clear_crutch(point_crutches, selected_window())
            else
                call clear_crutch(mark_crutches, current_buffer())
            end if
        else
            ! If move-to-column overshot, move back one char so we're at
            ! the position where rectangle--highlight-for-redisplay will
            ! add the overlay (so that the cursor can be drawn at the right
            ! place).
            if (c > col) call goto_char(point() - 1)

            if (point_kind) then
                call set_crutch(point_crutches, selected_window(), point(), col)
            else
                call set_crutch(mark_crutches, current_buffer(), point(), col)
            end if
        end if
    end subroutine rectangle_col_pos

    !> The column of POS, which is point: its crutch's, or its own.  Port of
    !> rect.el:rectangle--point-col.
    integer(int64) function rectangle_point_col(pos) result(col)
        integer(int64), intent(in) :: pos
        integer(int64) :: pp
        logical :: found

        call get_crutch(point_crutches, selected_window(), found, pp, col)
        if (found .and. pos == pp) return
        call goto_char(pos)
        col = current_column()
    end function rectangle_point_col

    !> The crutches of the mark and point, for comparing (the value of
    !> rect.el:rectangle--crutches).  Port of rect.el:rectangle--crutches.
    function rectangle_crutches(window) result(state)
        type(handle_type), intent(in) :: window
        integer(int64) :: state(6)
        logical :: have_m, have_p

        call get_crutch(mark_crutches, current_buffer(), have_m, state(2), state(3))
        call get_crutch(point_crutches, window, have_p, state(5), state(6))
        state(1) = merge(1, 0, have_m)
        state(4) = merge(1, 0, have_p)
    end function rectangle_crutches

    !> Port of rect.el:rectangle--reset-point-crutches.
    subroutine rectangle_reset_point_crutches()
        call clear_crutch(point_crutches, selected_window())
    end subroutine rectangle_reset_point_crutches

    !> Port of rect.el:rectangle--reset-crutches.
    subroutine rectangle_reset_crutches()
        call clear_crutch(mark_crutches, current_buffer())
        call rectangle_reset_point_crutches()
    end subroutine rectangle_reset_crutches

    ! ------------------------------------------------------------------
    ! Rectangle integration with kill-ring.
    ! ------------------------------------------------------------------

    !> Toggle the region as rectangular.
    !>
    !> Activates the region if it's inactive and Transient Mark mode is
    !> on.  Only lasts until the region is next deactivated.  Port of
    !> rect.el:rectangle-mark-mode.
    recursive subroutine rectangle_mark_mode(status)
        type(status_type), intent(out) :: status

        call minor_modes%rectangle_mark_mode%command(status)
    end subroutine rectangle_mark_mode

    !> The body of Rectangle-Mark mode.  Port of rect.el:rectangle-mark-mode
    !> (its body).
    recursive subroutine rectangle_mark_mode_body(enable, status)
        logical, intent(in) :: enable
        type(status_type), intent(out) :: status

        status = status_type()
        call rectangle_reset_crutches()
        if (.not. enable) return

        ! (advice-add 'region-beginning ...) and region-end: installed
        ! once, as they check the mode.
        if (.not. advised) then
            orig_region_beginning => region_beginning_function
            orig_region_end => region_end_function
            region_beginning_function => rectangle_region_beginning
            region_end_function => rectangle_region_end
            advised = .true.
        end if

        call deactivate_mark_hook%add(rectangle_mark_mode_off)

        if (.not. region_active_p()) then
            call push_mark(point(), .true., .true.)
            call message("Mark set (rectangle mode)")
        end if
    end subroutine rectangle_mark_mode_body

    !> Turn `rectangle_mark_mode' off, on `deactivate_mark_hook'.
    !> Port of the deactivate-mark-hook procedure of
    !> rect.el:rectangle-mark-mode.
    recursive subroutine rectangle_mark_mode_off(status)
        type(status_type), intent(out) :: status

        call minor_modes%rectangle_mark_mode%disable(status)
    end subroutine rectangle_mark_mode_off

    !> Like `exchange_point_and_mark' but cycles through the rectangle's
    !> corners.  Port of rect.el:rectangle-exchange-point-and-mark.
    recursive subroutine rectangle_exchange_point_and_mark(arg, status)
        logical, intent(in) :: arg
        type(status_type), intent(out) :: status
        integer(int64) :: p, m, startcol, endcol, cp, cm
        logical :: repeat

        status = status_type()

        if (arg) then
            this_command = sym("exchange_point_and_mark")
            call exchange_point_and_mark(arg, status)
            return
        end if

        p = point()
        repeat = value_eq(this_command, last_command)
        m = mark_position()

        if (m == 0) then
            call set_error(status, FM_WRONG_TYPE_ARGUMENT, "expected integer or type(marker_type)")
            return
        end if

        if (p < m) then
            call rectangle_pos_cols(p, m, startcol, endcol)
            cp = startcol
            cm = endcol
        else
            call rectangle_pos_cols(m, p, startcol, endcol)
            cp = endcol
            cm = startcol
        end if

        if (repeat) this_command = sym("exchange_point_and_mark")
        call rectangle_reset_crutches()
        call goto_char(p)

        if (repeat) then
            call rectangle_col_pos(cm, .false.)
        else
            call rectangle_col_pos(cp, .false.)
        end if

        call set_mark(point())
        call goto_char(m)

        if (repeat) then
            call rectangle_col_pos(cp, .true.)
        else
            call rectangle_col_pos(cm, .true.)
        end if
    end subroutine rectangle_exchange_point_and_mark

    !> Move N times by one character with forward_char in DIRECTION (1 for
    !> right/forward, -1 for left/backward), stepping into wide chars and
    !> past the EOL through the point crutch.  Port of
    !> rect.el:rectangle--*-char.
    recursive subroutine rectangle_star_char(direction, n_in, status)
        integer, intent(in) :: direction
        integer(int64), intent(in) :: n_in
        type(status_type), intent(out) :: status
        integer(int64) :: n, col, step, bol, eol, curcol, nextcol, diff, curdiff, opoint
        type(status_type) :: st
        integer :: d

        status = status_type()
        n = n_in
        d = direction

        if (n < 0) then
            d = -d
            n = -n
        end if

        col = rectangle_point_col(point())
        step = 1

        do while (n > 0)
            bol = line_beginning_position()
            eol = line_end_position()
            curcol = current_column()
            ! (save-excursion (funcall cmd step) ...)
            opoint = point()
            call forward_char(st, d*step)

            if (.not. st%ok()) then
                if (condition_matches(st%code, FM_END_OF_BUFFER)) then
                    nextcol = col + (1 + n)
                else if (condition_matches(st%code, FM_BEGINNING_OF_BUFFER)) then
                    nextcol = curcol - 1
                else
                    call goto_char(opoint)
                    status = st
                    return
                end if
            else if (bol > point()) then
                nextcol = curcol - 1
            else if (eol < point()) then
                nextcol = col + (1 + n)
            else
                nextcol = current_column()
            end if

            call goto_char(opoint)
            diff = abs(nextcol - col)

            if (nextcol < curcol .and. curcol < col) then
                curdiff = col - curcol

                if (curdiff <= n) then
                    n = n - curdiff
                    col = curcol
                else
                    col = col - n
                    n = 0
                end if
            else if (nextcol < 0) then
                ! Bumping into BOL!
                call ding()
                n = 0
                col = 0
            else if (nextcol == curcol) then
                call forward_char(status, int(d, int64))
                if (.not. status%ok()) return
            else
                if (diff <= n) then
                    n = n - diff
                    col = nextcol
                else
                    if (col < nextcol) then
                        col = col + n
                    else
                        col = col - n
                    end if

                    n = 0
                end if
            end if

            step = step + 1
        end do

        call rectangle_col_pos(col, .true.)
    end subroutine rectangle_star_char

    !> Like `right_char' but steps into wide chars and moves past EOL.
    !> Port of rect.el:rectangle-right-char.
    recursive subroutine rectangle_right_char(n, status)
        integer(int64), intent(in) :: n
        type(status_type), intent(out) :: status

        call rectangle_star_char(1, n, status)
    end subroutine rectangle_right_char

    !> Like `left_char' but steps into wide chars and moves past EOL.
    !> Port of rect.el:rectangle-left-char.
    recursive subroutine rectangle_left_char(n, status)
        integer(int64), intent(in) :: n
        type(status_type), intent(out) :: status

        call rectangle_star_char(-1, n, status)
    end subroutine rectangle_left_char

    !> Like `forward_char' but steps into wide chars and moves past EOL.
    !> Port of rect.el:rectangle-forward-char.
    recursive subroutine rectangle_forward_char(n, status)
        integer(int64), intent(in) :: n
        type(status_type), intent(out) :: status

        call rectangle_star_char(1, n, status)
    end subroutine rectangle_forward_char

    !> Like `backward_char' but steps into wide chars and moves past EOL.
    !> Port of rect.el:rectangle-backward-char.
    recursive subroutine rectangle_backward_char(n, status)
        integer(int64), intent(in) :: n
        type(status_type), intent(out) :: status

        call rectangle_star_char(-1, n, status)
    end subroutine rectangle_backward_char

    !> Like `next_line' but steps into wide chars and moves past EOL.
    !> Ignores `line_move_visual'.  Port of rect.el:rectangle-next-line.
    recursive subroutine rectangle_next_line(n, status)
        integer(int64), intent(in) :: n
        type(status_type), intent(out) :: status
        integer(int64) :: col, shortage

        status = status_type()
        col = rectangle_point_col(point())
        shortage = forward_line(n)
        call rectangle_col_pos(col, .true.)
    end subroutine rectangle_next_line

    !> Like `previous_line' but steps into wide chars and moves past EOL.
    !> Ignores `line_move_visual'.  Port of rect.el:rectangle-previous-line.
    recursive subroutine rectangle_previous_line(n, status)
        integer(int64), intent(in) :: n
        type(status_type), intent(out) :: status
        integer(int64) :: col, shortage

        status = status_type()
        col = rectangle_point_col(point())
        shortage = forward_line(-n)
        call rectangle_col_pos(col, .true.)
    end subroutine rectangle_previous_line

    !> The corners of the rectangular region in order, and their columns.
    !> Not a port.
    subroutine region_corners(start, end, startcol, endcol, status)
        integer(int64), intent(out) :: start, end, startcol, endcol
        type(status_type), intent(out) :: status
        integer(int64) :: pt, mk

        status = status_type()
        start = 0
        end = 0
        startcol = 0
        endcol = 0
        pt = point()
        mk = mark_position()

        if (mk == 0) then
            call set_error(status, FM_WRONG_TYPE_ARGUMENT, "expected integer or type(marker_type)")
            return
        end if

        start = min(pt, mk)
        end = max(pt, mk)
        call rectangle_pos_cols(start, end, startcol, endcol)
    end subroutine region_corners

    !> Like `region_beginning' but supports rectangular regions.  Port of
    !> rect.el:rectangle--region-beginning.
    recursive subroutine rectangle_region_beginning(pos, status)
        integer(int64), intent(out) :: pos
        type(status_type), intent(out) :: status
        integer(int64) :: start, end, startcol, endcol, opoint, c

        if (.not. minor_modes%rectangle_mark_mode%enabled()) then
            call orig_region_beginning(pos, status)
            return
        end if

        pos = 0
        opoint = point()
        call region_corners(start, end, startcol, endcol, status)
        if (.not. status%ok()) return
        call goto_char(start)
        call move_to_column(min(startcol, endcol), c)
        pos = point()
        call goto_char(opoint)
    end subroutine rectangle_region_beginning

    !> Like `region_end' but supports rectangular regions.  Port of
    !> rect.el:rectangle--region-end.
    recursive subroutine rectangle_region_end(pos, status)
        integer(int64), intent(out) :: pos
        type(status_type), intent(out) :: status
        integer(int64) :: start, end, startcol, endcol, opoint, c

        if (.not. minor_modes%rectangle_mark_mode%enabled()) then
            call orig_region_end(pos, status)
            return
        end if

        pos = 0
        opoint = point()
        call region_corners(start, end, startcol, endcol, status)
        if (.not. status%ok()) return
        call goto_char(end)
        call move_to_column(max(startcol, endcol), c)
        pos = point()
        call goto_char(opoint)
    end subroutine rectangle_region_end

    !> The region's beginning and end as the primitives give them
    !> (with `rectangle_mark_mode' off meanwhile, saved and restored).  Not a port.
    recursive subroutine plain_region(beg, end, status)
        integer(int64), intent(out) :: beg, end
        type(status_type), intent(out) :: status

        end = 0
        call orig_region_beginning(beg, status)
        if (status%ok()) call orig_region_end(end, status)
    end subroutine plain_region

    !> `region_extract_function' in Rectangle-Mark mode: the rectangle's
    !> lines joined by newlines, with a `yank_handler' property that yanks
    !> them as a rectangle.  Port of rect.el:rectangle--extract-region.
    recursive subroutine rectangle_extract_region(method, string, starts, ends, status)
        integer, intent(in) :: method
        type(value_type), intent(out) :: string
        integer(int64), allocatable, intent(out) :: starts(:), ends(:)
        type(status_type), intent(out) :: status
        type(rectangle_line_type), allocatable :: lines(:)
        type(value_type) :: strs
        type(value_type), allocatable :: elems(:)
        character(len=:), allocatable :: text
        integer(int64) :: beg, end, nchars
        integer :: k

        if (.not. minor_modes%rectangle_mark_mode%enabled()) then
            call orig_region_extract(method, string, starts, ends, status)
            return
        end if

        allocate (starts(0), ends(0))
        ! Avoid recursive calls from advice.
        call plain_region(beg, end, status)
        if (.not. status%ok()) return

        select case (method)
        case (REGION_EXTRACT_BOUNDS)
            deallocate (starts, ends)
            call extract_rectangle_bounds(beg, end, starts, ends, status)
            return
        case (REGION_EXTRACT_COPY)
            call extract_rectangle(beg, end, lines, status)
        case default
            call delete_extract_rectangle(beg, end, lines, status)
        end select

        if (.not. status%ok()) return
        ! (A `delete-only' extraction is a deletion too: its value is not
        ! used.)
        allocate (elems(size(lines)))
        text = ""

        do k = 1, size(lines)
            elems(k) = make_string(lines(k)%text)
            if (k > 1) text = text//achar(10)
            text = text//lines(k)%text
        end do

        if (value_eq(last_command, sym("kill_region"))) then
            ! Try to prevent kill-region from appending this to some
            ! earlier element.
            last_command = sym("kill_region_dont_append")
        end if

        if (size(lines) == 0) return
        strs = make_list(elems)
        string = make_string(text)
        call multibyte_chars_in_text(text, nchars, status)
        if (.not. status%ok()) return
        call string_put_text_property(0_int64, nchars, sym("yank_handler"), &
                                      list(sym("rectangle__insert_for_yank"), strs, t_value()), string, status)
    end subroutine rectangle_extract_region

    !> `region_insert_function' in Rectangle-Mark mode: LINES as a
    !> rectangle.  Port of rect.el:rectangle--insert-region.
    recursive subroutine rectangle_insert_region(lines, status)
        type(value_type), intent(in) :: lines
        type(status_type), intent(out) :: status

        if (.not. minor_modes%rectangle_mark_mode%enabled()) then
            call orig_region_insert(lines, status)
            return
        end if

        call insert_rectangle(rectangle_of(lines), status)
    end subroutine rectangle_insert_region

    !> The strings of the list STRS as a rectangle.  Not a port.
    function rectangle_of(strs) result(rect)
        type(value_type), intent(in) :: strs
        type(rectangle_line_type), allocatable :: rect(:)
        type(value_type), allocatable :: elems(:)
        integer :: k

        call list_elements(strs, elems)
        allocate (rect(size(elems)))

        do k = 1, size(elems)
            rect(k)%text = xstring(elems(k))
        end do
    end function rectangle_of

    !> The yank handler of a killed rectangle: insert the strings STRS as
    !> a rectangle, `yank_pop' undoing that.  Port of
    !> rect.el:rectangle--insert-for-yank.
    recursive subroutine rectangle_insert_for_yank(strs, status)
        type(value_type), intent(in) :: strs
        type(status_type), intent(out) :: status
        type(undo_entry_type) :: entry

        status = status_type()
        ! (push (point) buffer-undo-list)
        undo_at_start = 0

        if (buffers(cur)%undo_enabled) then
            entry%kind = UE_POINT
            entry%beg = point()
            call push_undo_entry(entry)
            undo_at_start = buffers(cur)%undo_count
        end if

        call insert_rectangle(rectangle_of(strs), status)
        yank_undo_function => rectangle_yank_undo
    end subroutine rectangle_insert_for_yank

    !> The `yank_undo_function' of a yanked rectangle: undo back to the
    !> point entry recorded before it, turned into a boundary.  Port of the
    !> yank-undo-function procedure of rect.el:rectangle--insert-for-yank.
    recursive subroutine rectangle_yank_undo(start, end, status)
        integer(int64), intent(in) :: start, end
        type(status_type), intent(out) :: status

        call undo_start(status)
        if (.not. status%ok()) return
        if (undo_at_start <= 0 .or. undo_at_start > buffers(cur)%undo_count) return
        ! Turn it into a boundary.
        buffers(cur)%undo_list(undo_at_start) = undo_entry_type()

        do while (pending_undo_list > undo_at_start - 1)
            call undo_more(1_int64, status)
            if (.not. status%ok()) return
        end do
    end subroutine rectangle_yank_undo

    !> The highlighting of WINDOW's rectangle, as rect.el keeps it in the
    !> window's `internal-region-overlay' parameter: index in
    !> `highlights', 0 for none.  Not a port.
    integer function highlight_of(window) result(k)
        type(handle_type), intent(in) :: window

        if (allocated(highlights)) then
            do k = 1, size(highlights)
                if (same_owner(highlights(k)%window, window)) return
            end do
        end if

        k = 0
    end function highlight_of

    !> Add an entry for WINDOW at the end of `highlights'.  Not a port.
    subroutine add_highlight(window)
        type(handle_type), intent(in) :: window
        type(rectangle_highlight_type), allocatable :: grown(:)
        integer :: n, j

        n = 0
        if (allocated(highlights)) n = size(highlights)
        allocate (grown(n + 1))

        do j = 1, n
            call move_alloc(highlights(j)%overlays, grown(j)%overlays)
            grown(j)%window = highlights(j)%window
            grown(j)%tick = highlights(j)%tick
            grown(j)%start = highlights(j)%start
            grown(j)%end = highlights(j)%end
            grown(j)%pt = highlights(j)%pt
            grown(j)%crutches = highlights(j)%crutches
        end do

        grown(n + 1)%window = window
        allocate (grown(n + 1)%overlays(0))
        call move_alloc(grown, highlights)
    end subroutine add_highlight

    !> Delete the rectangle's overlays in WINDOW.  Not a port.
    subroutine delete_highlight(window)
        type(handle_type), intent(in) :: window
        type(rectangle_highlight_type), allocatable :: kept(:)
        integer :: k, j

        k = highlight_of(window)
        if (k == 0) return

        do j = 1, size(highlights(k)%overlays)
            call free_overlay(highlights(k)%overlays(j))
        end do

        allocate (kept(size(highlights) - 1))

        do j = 1, size(kept)
            associate (from => highlights(merge(j, j + 1, j < k)))
                call move_alloc(from%overlays, kept(j)%overlays)
                kept(j)%window = from%window
                kept(j)%tick = from%tick
                kept(j)%start = from%start
                kept(j)%end = from%end
                kept(j)%pt = from%pt
                kept(j)%crutches = from%crutches
            end associate
        end do

        call move_alloc(kept, highlights)
    end subroutine delete_highlight

    !> `redisplay_highlight_region_function' in Rectangle-Mark mode:
    !> highlight the rectangle with an overlay for each line within the
    !> window's height of point (padding past the EOL, or into a TAB, with
    !> before- and after-strings).  Port of
    !> rect.el:rectangle--highlight-for-redisplay
    !> (`rectangle_indicate_zero_width_rectangle' is Deferred).
    subroutine rectangle_highlight_for_redisplay(start, end, window, rb, re)
        integer(int64), intent(in) :: start, end
        type(handle_type), intent(in) :: window
        integer(int64), intent(out) :: rb, re
        type(handle_type), allocatable :: old(:), nrol(:)
        type(handle_type) :: ol
        type(value_type) :: str
        type(status_type) :: st
        integer(int64) :: pt, tick, startcol, endcol, leftcol, rightcol, height, start_pt, end_pt, mleft, &
                          left, mright, right, shortage, nchars
        integer(int64) :: state(6)
        integer :: k, used

        if (.not. minor_modes%rectangle_mark_mode%enabled()) then
            call delete_highlight(window)
            call orig_highlight(start, end, window, rb, re)
            return
        end if

        rb = 0
        re = 0
        call orig_unhighlight(window)
        call buffer_chars_modified_tick(current_buffer(), tick, st)
        pt = point()
        state = rectangle_crutches(window)
        k = highlight_of(window)

        if (k > 0) then
            if (highlights(k)%tick == tick .and. highlights(k)%start == start .and. highlights(k)%end == end .and. &
                all(highlights(k)%crutches == state) .and. highlights(k)%pt == pt) return
            old = highlights(k)%overlays
        else
            allocate (old(0))
        end if

        allocate (nrol(0))
        used = 0
        call rectangle_pos_cols(start, end, startcol, endcol, window)
        leftcol = min(startcol, endcol)
        rightcol = max(startcol, endcol)
        ! We don't know what lines will actually be displayed, so add
        ! highlight overlays on lines within the window height from point.
        height = windows(window%index)%total_lines
        shortage = forward_line(-height)
        start_pt = max(start, point())
        call goto_char(pt)
        shortage = forward_line(height)
        end_pt = min(end, point())
        call goto_char(start_pt)
        call goto_char(line_beginning_position())

        do
            call move_to_column(leftcol, mleft)
            left = point()
            ! BEWARE: In the presence of other overlays with
            ! before/after/display-strings, this happens to move to the
            ! column "as if the overlays were not applied", which is
            ! sometimes what we want, tho it can be considered a bug in
            ! move-to-column.
            call move_to_column(rightcol, mright)
            right = point()

            if (used < size(old)) then
                used = used + 1
                ol = old(used)
                call move_overlay(ol, left, right, st, current_buffer())
            else
                call make_overlay(ol, left, right, st)
                call overlay_put(ol, "window", make_window_ref(window), st)
                call overlay_put(ol, "face", sym("region"), st)
            end if

            ! `move-to-column' may stop before the column (if bumping into
            ! EOL) or overshoot it a little, when column is in the middle
            ! of a char.
            if (mleft < leftcol) then
                ! `leftcol' is past EOL.
                call overlay_put(ol, "before_string", space_to(leftcol), st)
                mright = max(mright, leftcol)
            else if (mleft > leftcol .and. tab_before(left)) then
                ! `leftcol' is in the middle of a char.
                left = left - 1
                call move_overlay(ol, left, right, st)
                call goto_char(left)
                call overlay_put(ol, "before_string", space_to(leftcol), st)
            else if (.not. nilp(overlay_get(ol, "before_string"))) then
                call overlay_put(ol, "before_string", nil(), st)
            end if

            if (mright < rightcol) then
                ! `rightcol' is past EOL.
                str = space_to(rightcol)
                call string_chars(str, nchars)
                call string_put_text_property(0_int64, nchars, sym("face"), sym("region"), str, st)
                ! If cursor happens to be here, draw it at the right place.
                call place_cursor(leftcol, left, str, window)
                call overlay_put(ol, "after_string", str, st)
            else if (mright > rightcol .and. tab_before(right)) then
                ! `rightcol' is in the middle of a char.
                right = right - 1
                call move_overlay(ol, left, right, st)

                if (rightcol == leftcol) then
                    call overlay_put(ol, "after_string", nil(), st)
                else
                    call goto_char(right)
                    str = space_to(rightcol)
                    call string_chars(str, nchars)
                    call string_put_text_property(0_int64, nchars, sym("face"), sym("region"), str, st)
                    if (left == right) call place_cursor(leftcol, left, str, window)
                    call overlay_put(ol, "after_string", str, st)
                end if
            else if (.not. nilp(overlay_get(ol, "after_string"))) then
                call overlay_put(ol, "after_string", nil(), st)
            end if

            nrol = [nrol, ol]
            if (forward_line(1_int64) /= 0) exit
            if (.not. bolp()) exit
            if (point() > end_pt) exit
        end do

        do k = used + 1, size(old)
            call free_overlay(old(k))
        end do

        call goto_char(pt)
        k = highlight_of(window)

        if (k == 0) then
            call add_highlight(window)
            k = size(highlights)
        end if

        highlights(k)%tick = tick
        highlights(k)%start = start
        highlights(k)%end = end
        highlights(k)%crutches = rectangle_crutches(window)
        highlights(k)%pt = pt
        call move_alloc(nrol, highlights(k)%overlays)

    contains

        !> Whether the character before POS is a TAB.
        logical function tab_before(pos)
            integer(int64), intent(in) :: pos
            integer(int32) :: c
            logical :: found

            call char_before(c, found, pos)
            tab_before = found .and. c == 9
        end function tab_before

        subroutine string_chars(s, n)
            type(value_type), intent(in) :: s
            integer(int64), intent(out) :: n
            type(status_type) :: cst

            call multibyte_chars_in_text(xstring(s), n, cst)
        end subroutine string_chars
    end subroutine rectangle_highlight_for_redisplay

    !> `redisplay_unhighlight_region_function' in Rectangle-Mark mode:
    !> remove the rectangle's overlays of WINDOW.  Port of
    !> rect.el:rectangle--unhighlight-for-redisplay.
    subroutine rectangle_unhighlight_for_redisplay(window)
        type(handle_type), intent(in) :: window

        if (highlight_of(window) == 0) then
            call orig_unhighlight(window)
            return
        end if

        call delete_highlight(window)
    end subroutine rectangle_unhighlight_for_redisplay

    !> A space up to column COL.  Port of rect.el:rectangle--space-to.
    function space_to(col) result(v)
        integer(int64), intent(in) :: col
        type(value_type) :: v
        type(status_type) :: st

        v = make_string(" ")
        call string_put_text_property(0_int64, 1_int64, sym("display"), list(sym("space"), sym(":align-to"), &
                                                                             make_fixnum(col)), v, st)
    end function space_to

    !> Put the cursor on the first character of STR if point's crutch is
    !> at LEFT with column LEFTCOL.  Port of rect.el:rectangle--place-cursor.
    subroutine place_cursor(leftcol, left, str, window)
        integer(int64), intent(in) :: leftcol, left
        type(value_type), intent(inout) :: str
        type(handle_type), intent(in) :: window
        integer(int64) :: pp, pc
        logical :: found
        type(status_type) :: st

        call get_crutch(point_crutches, window, found, pp, pc)

        if (found .and. left == pp .and. leftcol == pc) then
            call string_put_text_property(0_int64, 1_int64, sym("cursor"), make_fixnum(1_int64), str, st)
        end if
    end subroutine place_cursor

    ! ------------------------------------------------------------------
    ! Commands
    ! ------------------------------------------------------------------

    !> The value of the prefix argument as the FILL of a "P" spec.
    !> Not a port.
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

    !> The number of characters of the UTF-8 text S.  Not a port.
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
    !> (the first element of `string_rectangle_history', or "") of the prompts).  Not a port.
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
    !> is empty, then ": ".  Not a port.
    function prompt_with_default(prompt, default) result(full)
        character(len=*), intent(in) :: prompt, default
        character(len=:), allocatable :: full

        full = prompt
        if (len(default) > 0) full = full//" (default "//default//")"
        full = full//": "
    end function prompt_with_default

    !> Read the string of `string_rectangle' and `string_insert_rectangle'
    !> with PROMPT (how the commands read their arguments).  Port of the argument reading
    !> of rect.el:string-rectangle and rect.el:string-insert-rectangle.
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

    recursive subroutine cmd_rectangle_exchange_point_and_mark(status)
        type(status_type), intent(out) :: status

        call rectangle_exchange_point_and_mark(.not. nilp(current_prefix_arg), status)
    end subroutine cmd_rectangle_exchange_point_and_mark

    recursive subroutine cmd_rectangle_right_char(status)
        type(status_type), intent(out) :: status

        call rectangle_right_char(prefix_numeric_value(current_prefix_arg), status)
    end subroutine cmd_rectangle_right_char

    recursive subroutine cmd_rectangle_left_char(status)
        type(status_type), intent(out) :: status

        call rectangle_left_char(prefix_numeric_value(current_prefix_arg), status)
    end subroutine cmd_rectangle_left_char

    recursive subroutine cmd_rectangle_forward_char(status)
        type(status_type), intent(out) :: status

        call rectangle_forward_char(prefix_numeric_value(current_prefix_arg), status)
    end subroutine cmd_rectangle_forward_char

    recursive subroutine cmd_rectangle_backward_char(status)
        type(status_type), intent(out) :: status

        call rectangle_backward_char(prefix_numeric_value(current_prefix_arg), status)
    end subroutine cmd_rectangle_backward_char

    recursive subroutine cmd_rectangle_next_line(status)
        type(status_type), intent(out) :: status

        call rectangle_next_line(prefix_numeric_value(current_prefix_arg), status)
    end subroutine cmd_rectangle_next_line

    recursive subroutine cmd_rectangle_previous_line(status)
        type(status_type), intent(out) :: status

        call rectangle_previous_line(prefix_numeric_value(current_prefix_arg), status)
    end subroutine cmd_rectangle_previous_line

    !> Bind KEYS to command CMD in `rectangle_mark_mode_map'.  Not a port.
    subroutine bind(keys, cmd)
        character(len=*), intent(in) :: keys, cmd
        type(value_type), allocatable :: key(:)
        type(status_type) :: status

        call key_parse(keys, key, status)
        call define_key(rectangle_mark_mode_map, key, sym(cmd), status)
    end subroutine bind

    !> Bind [remap FROM] to command CMD in `rectangle_mark_mode_map'.
    !> Not a port.
    subroutine bind_remap(from, cmd)
        character(len=*), intent(in) :: from, cmd
        type(value_type), allocatable :: key(:)
        type(status_type) :: status

        call list_elements(list(sym("remap"), sym(from)), key)
        call define_key(rectangle_mark_mode_map, key, sym(cmd), status)
    end subroutine bind_remap

    !> Register the commands, `string_rectangle_history', Rectangle-Mark
    !> mode with its keymap, and the rectangular region's functions
    !> around the region's (rect.el's add-function calls).  Not a port.
    subroutine syms_of_rect()
        call defvar_nil("string_rectangle_history")
        call minor_modes%rectangle_mark_mode%define("rectangle_mark_mode", hook=rectangle_mark_mode_hook, &
                                                    keymap=.true., body=rectangle_mark_mode_body)
        rectangle_mark_mode_map = minor_modes%rectangle_mark_mode%map
        call bind("C-o", "open_rectangle")
        call bind("C-t", "string_rectangle")
        call bind_remap("exchange_point_and_mark", "rectangle_exchange_point_and_mark")
        call bind_remap("right_char", "rectangle_right_char")
        call bind_remap("left_char", "rectangle_left_char")
        call bind_remap("forward_char", "rectangle_forward_char")
        call bind_remap("backward_char", "rectangle_backward_char")
        call bind_remap("next_line", "rectangle_next_line")
        call bind_remap("previous_line", "rectangle_previous_line")
        commands%rectangle_mark_mode%run => rectangle_mark_mode
        commands%rectangle_exchange_point_and_mark%run => cmd_rectangle_exchange_point_and_mark
        commands%rectangle_right_char%run => cmd_rectangle_right_char
        commands%rectangle_left_char%run => cmd_rectangle_left_char
        commands%rectangle_forward_char%run => cmd_rectangle_forward_char
        commands%rectangle_backward_char%run => cmd_rectangle_backward_char
        commands%rectangle_next_line%run => cmd_rectangle_next_line
        commands%rectangle_previous_line%run => cmd_rectangle_previous_line

        if (.not. associated(orig_region_extract)) then
            orig_highlight => redisplay_highlight_region_function
            redisplay_highlight_region_function => rectangle_highlight_for_redisplay
            orig_unhighlight => redisplay_unhighlight_region_function
            redisplay_unhighlight_region_function => rectangle_unhighlight_for_redisplay
            orig_region_extract => region_extract_function
            region_extract_function => rectangle_extract_region
            orig_region_insert => region_insert_function
            region_insert_function => rectangle_insert_region
        end if

        call define_yank_handler_function("rectangle__insert_for_yank", rectangle_insert_for_yank)
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
