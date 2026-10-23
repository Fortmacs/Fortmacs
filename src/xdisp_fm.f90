! xdisp_fm.f90 -- redisplay: turning buffer text into screen rows.
! Ported from GNU Emacs 31.1 src/xdisp.c (with the terminal glyph
! production of term.c and the basic faces of xfaces.c).
!
! Copyright (C) 1985-2026 Free Software Foundation, Inc.
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

!> `redisplay' lays out the root window's buffer from its window start,
!> recentering when point is not visible, adds the mode line and the echo
!> area, and updates the terminal (dispnew_fm).
!>
!> Text appears as Emacs shows it by default: TAB to the next multiple of
!> tab-width, control characters as ^X, raw bytes as \ooo, wide
!> characters in two cells, and long lines continued with \ in the last
!> column (a wide character that does not fit moves whole to the next
!> row; a TAB, ^X or \ooo is split between the rows).  Recentering puts
!> point's screen line half the window's height from the top.
!>
!> Faces (xfaces_fm): text in the face of its `face' property, an active
!> region in the region face (extended to the end of a line where the
!> face has `extend'), ^X and \ooo in escape_glyph, non-ASCII spaces and
!> hyphens in nobreak_space and nobreak_hyphen, the minibuffer prompt in
!> minibuffer_prompt, mode lines in mode_line_active or
!> mode_line_inactive with the buffer name in mode_line_buffer_id, and
!> borders between windows in vertical_border.
!>
!> Simplified for now: the mode line is the standard mode-line-format
!> expanded by fixed code (no mode-line-format interpretation; coding
!> systems shown as UUU:, no base-line cache for %l), character widths
!> follow a built-in table of wide ranges (no char-width-table), there
!> are no overlays, display or invisible properties, display tables,
!> faces in echo-area messages, bidi or truncate-lines
!> (truncate-partial-width-windows is Deferred: narrow side-by-side
!> windows continue lines instead of truncating them, as that needs
!> auto-hscroll of windows; only the minibuffer line scrolls
!> horizontally), the echo area shows the first line of a message
!> (resize-mini-windows is not ported), and
!> the incremental redisplay shortcuts (try_window_reusing_current_matrix
!> and friends) are not ported: each redisplay lays out the whole window
!> and dispnew_fm sends only the differences.
module xdisp_fm
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use error_fm, only: status_type
    use handle_fm, only: handle_type
    use buffer_fm, only: buffers, resolve_buffer, current_buffer, buf_fetch_char, &
                         buf_fetch_byte, buf_next_char_len, buffer_modified_p, transient_mark_mode
    use marker_fm, only: buf_charpos_to_bytepos, buf_bytepos_to_charpos, markers
    use character_fm, only: char_byte8_p, char_to_byte8, string_char_and_length, multibyte_length, &
                            byte8_to_char
    use dispextern_fm, only: glyph_type, glyph_row_type, DEFAULT_FACE_ID, MODE_LINE_ACTIVE_FACE_ID, &
                             MODE_LINE_INACTIVE_FACE_ID, VERTICAL_BORDER_FACE_ID
    use xfaces_fm, only: face_at_buffer_position, merge_faces, realize_basic_faces, TEXT_PROP_DISTANCE_LIMIT
    use frame_fm, only: frame_type, selected_frame
    use window_fm, only: window_type, set_window_buffer, window_start, &
                         set_window_start, window_body_lines, window_point, window_text_cols, windows, &
                         window_valid_p, make_window, resize_window_tree, window_leaves, same_window, &
                         set_window_point
    use dispnew_fm, only: adjust_frame_glyphs, update_frame
    use keyboard_fm, only: last_message, minibuffer_message_text, set_minibuffer_message_text
    use lisp_fm, only: value_type, nilp
    use lread_fm, only: sym
    use data_fm, only: buffer_local_value, integer_setting
    implicit none(type, external)
    private

    public :: add_minor_mode_lighter, mode_line_modes
    public :: redisplay, init_frame_windows, glyph_width, &
              mode_line_text, layout_window, message, glyph_columns, display_line_rows, display_row_of, &
              mode_line_row, recenter_start, mode_line_percent

    !> xdisp.c cursor-in-echo-area: put the cursor after the echo area
    !> text (while a prompt there waits for an answer).
    logical, public :: cursor_in_echo_area = .false.

    !> Distance between tab stops (for display of tab characters), in columns.
    !> This controls the width of a TAB character on display.
    !> The value should be a positive integer.  Port of buffer.c tab-width
    !> (one value for all buffers until buffer-local variables exist).
    type(integer_setting), public :: tab_width = integer_setting(name="tab_width", value=8)

    !> Non-nil means truncate lines in windows narrower than the frame.
    !> For an integer value, truncate lines in each window narrower than the
    !> full frame width, provided the total window width in column units is less
    !> than that integer; otherwise, respect the value of `truncate_lines'.
    !> The total width of the window is as returned by `window_total_width', it
    !> includes the fringes, the continuation and truncation glyphs, the
    !> display margins (if any), and the scroll bar
    !>
    !> For any other non-nil value, truncate lines in all windows that do
    !> not span the full frame width.
    !>
    !> A value of nil means to respect the value of `truncate_lines'.
    !>
    !> In Fortran: 0 is nil, `huge(0)' any other non-nil value.  Port of
    !> xdisp.c truncate-partial-width-windows; the truncation itself is
    !> not ported yet (Deferred), only commands that test the variable.
    integer, public :: truncate_partial_width_windows = 50

    !> An entry of minor-mode-alist.
    type :: lighter_type
        character(len=:), allocatable :: variable, lighter
    end type lighter_type

    type(lighter_type), allocatable :: lighters(:)
    integer :: n_lighters = 0

    !> The most cells one character can take: a TAB at the largest sane
    !> tab-width (SANE_TAB_WIDTH).
    integer, parameter :: MAX_CHAR_CELLS = 1000

    !> How display_row ended a screen row: at the end of the accessible
    !> text, after a newline, or continued on the next row.
    integer, parameter :: ROW_ENDS_AT_ZV = 0, ROW_ENDS_IN_NEWLINE = 1, ROW_CONTINUED = 2

    !> Where a screen row starts: the state of xdisp.c's iterator at the
    !> start of a display line.
    type, public :: row_start_type
        !> The first character shown (charpos, bytepos).
        integer(int64) :: pos = 1, byte = 1
        !> it->continuation_lines_width: the columns of the rows before
        !> this one on the same line (TABs count from it).
        integer :: cont = 0
        !> it->starts_in_middle_of_char_p: the row starts with the rest of
        !> a ^X, \ooo or TAB begun on the row before ...
        logical :: mid = .false.
        !> ... whose first `skip' cells were shown there.
        integer :: skip = 0
    end type row_start_type

contains

    ! ------------------------------------------------------------------
    ! Frames, windows and faces
    ! ------------------------------------------------------------------

    !> Lay out the frame's windows, making them if need be: the window
    !> tree above (at first one window, selected), the one-line minibuffer
    !> window at the bottom.  The window part of frame.c:make_terminal_frame
    !> and window.c:make_frame_without_minibuffer.
    subroutine init_frame_windows(f)
        type(frame_type), intent(inout) :: f
        integer :: rows, cols
        rows = f%tty%wcm%cm_rows
        cols = f%tty%wcm%cm_cols
        if (.not. window_valid_p(f%root_window)) then
            call make_window(f%root_window)
            f%selected_window = f%root_window
        end if
        if (.not. window_valid_p(f%minibuffer_window)) then
            call make_window(f%minibuffer_window)
            windows(f%minibuffer_window%index)%mini = .true.
        end if
        call resize_window_tree(f%root_window, 0, 0, rows - 1, cols, cols)
        call resize_window_tree(f%minibuffer_window, rows - 1, 0, 1, cols, cols)
    end subroutine init_frame_windows

    ! ------------------------------------------------------------------
    ! Producing glyphs
    ! ------------------------------------------------------------------

    !> Columns character `c' takes: 2 for East Asian wide and fullwidth
    !> characters and emoji, else 1.  An approximation of
    !> character.c:char_width with characters.el's char-width-table.
    elemental integer function glyph_width(c)
        integer(int32), intent(in) :: c
        glyph_width = 1
        if ((c >= int(z'1100', int32) .and. c <= int(z'115F', int32)) .or. &
            (c >= int(z'2E80', int32) .and. c <= int(z'303E', int32)) .or. &
            (c >= int(z'3041', int32) .and. c <= int(z'33FF', int32)) .or. &
            (c >= int(z'3400', int32) .and. c <= int(z'4DBF', int32)) .or. &
            (c >= int(z'4E00', int32) .and. c <= int(z'9FFF', int32)) .or. &
            (c >= int(z'A000', int32) .and. c <= int(z'A4CF', int32)) .or. &
            (c >= int(z'AC00', int32) .and. c <= int(z'D7A3', int32)) .or. &
            (c >= int(z'F900', int32) .and. c <= int(z'FAFF', int32)) .or. &
            (c >= int(z'FE30', int32) .and. c <= int(z'FE4F', int32)) .or. &
            (c >= int(z'FF00', int32) .and. c <= int(z'FF60', int32)) .or. &
            (c >= int(z'FFE0', int32) .and. c <= int(z'FFE6', int32)) .or. &
            (c >= int(z'1F300', int32) .and. c <= int(z'1F64F', int32)) .or. &
            (c >= int(z'1F900', int32) .and. c <= int(z'1F9FF', int32)) .or. &
            (c >= int(z'20000', int32) .and. c <= int(z'3FFFD', int32))) glyph_width = 2
    end function glyph_width

    !> The cells that show character `c' at logical column `x' (the column
    !> counted from the start of the line, continuation lines included).
    !> The display-element part of xdisp.c:get_next_display_element with
    !> term.c:produce_glyphs: TAB, ^X, \ooo, wide characters.
    subroutine produce_char(c, x, cells, n)
        integer(int32), intent(in) :: c
        integer, intent(in) :: x
        type(glyph_type), intent(out) :: cells(:)
        integer, intent(out) :: n
        integer(int32) :: byte
        integer :: tw
        n = 1
        if (c == 9) then
            ! SANE_TAB_WIDTH
            tw = 8
            if (tab_width%value > 0 .and. tab_width%value <= 1000) tw = int(tab_width%value)
            n = tw - mod(x, tw)
            cells(1:n)%ch = 32
        else if (c < 32 .or. c == 127) then
            ! ctl-arrow: ^@ .. ^_, ^?
            n = 2
            cells(1)%ch = iachar("^")
            cells(2)%ch = ieor(c, 64_int32)
        else if (char_byte8_p(c)) then
            byte = char_to_byte8(c)
            n = 4
            cells(1)%ch = iachar("\")
            cells(2)%ch = iachar("0") + byte/64
            cells(3)%ch = iachar("0") + mod(byte/8, 8)
            cells(4)%ch = iachar("0") + mod(byte, 8)
        else if (glyph_width(c) == 2) then
            n = 2
            cells(1)%ch = c
            cells(2)%ch = c
            cells(2)%padding = .true.
        else
            cells(1)%ch = c
        end if
    end subroutine produce_char

    !> Columns character `c' takes when displayed at column `x' (for
    !> TAB, the distance to the next tab stop).
    integer function glyph_columns(c, x) result(n)
        integer(int32), intent(in) :: c
        integer, intent(in) :: x
        type(glyph_type) :: cells(MAX_CHAR_CELLS)
        call produce_char(c, x, cells, n)
    end function glyph_columns

    !> Lay out one screen row of buffer slot `bi' from `s' (updated to the
    !> start of the next row), with the continuation glyph \ at column
    !> `last_x' + 1 (it->last_visible_x is `last_x').  A character that
    !> does not fit continues on the next row: a wide character whole,
    !> after \ in every cell left (cont width grows by the row's columns
    !> before it); the cells of others (TAB, ^X, \ooo) are split, those
    !> that fit staying on this row (cont width grows by `last_x').
    !> `ends' says how the row ended.  With `glyphs', the cells are drawn
    !> there, and with frame `f' too in their faces: the face of the text
    !> (its `face' property and the region [rb, re)), `escape_glyph' merged
    !> into it for ^X and \ooo, `nobreak_space' and `nobreak_hyphen' for
    !> non-ASCII spaces and hyphens; after the text of a line a space in
    !> the face of the newline, and the rest of the row in the face the
    !> newline's face extends with (append_space_for_newline and
    !> extend_face_to_end_of_line).  `cursor_col' is the column of `pt' if
    !> it is shown on this row, else -1; `used' the columns used.  The
    !> terminal part of xdisp.c:display_line with
    !> xdisp.c:get_next_display_element and term.c:produce_glyphs.
    subroutine display_row(bi, s, last_x, pt, ends, cursor_col, used, glyphs, rb, re, f)
        integer(int64), intent(in) :: bi
        type(row_start_type), intent(inout) :: s
        integer, intent(in) :: last_x
        integer(int64), intent(in) :: pt
        integer, intent(out) :: ends, cursor_col, used
        type(glyph_type), intent(inout), optional :: glyphs(:)
        integer(int64), intent(in), optional :: rb, re
        type(frame_type), intent(inout), optional :: f
        type(glyph_type) :: cells(MAX_CHAR_CELLS)
        integer(int64) :: zv, stop, rbeg, rend, endpos
        integer(int32) :: c, face_id, extend_face_id
        integer :: col, n, first, ncells, k
        logical :: faces
        zv = buffers(bi)%zv
        col = 0
        cursor_col = -1
        faces = present(glyphs) .and. present(f)
        rbeg = 0
        rend = 0
        if (present(rb) .and. present(re)) then
            rbeg = rb
            rend = re
        end if
        face_id = DEFAULT_FACE_ID
        stop = -1
        do
            if (s%pos >= zv) then
                if (s%pos == pt) cursor_col = col
                ends = ROW_ENDS_AT_ZV
                exit
            end if
            ! handle_face_prop at the next face change.
            if (faces .and. s%pos >= stop) then
                face_id = face_at_buffer_position(f, bi, s%pos, stop, s%pos + TEXT_PROP_DISTANCE_LIMIT, rbeg, rend)
            end if
            c = buf_fetch_char(buffers(bi), s%byte)
            if (c == 10) then
                if (s%pos == pt) cursor_col = col
                if (faces .and. face_id /= DEFAULT_FACE_ID .and. col + 1 <= size(glyphs)) then
                    ! A space for the cursor at the end of the line, in the
                    ! newline's face.
                    glyphs(col + 1) = glyph_type(face_id=face_id)
                    col = col + 1
                    ! The rest of the row in the face that extends.
                    extend_face_id = face_at_buffer_position(f, bi, s%pos, endpos, s%pos + TEXT_PROP_DISTANCE_LIMIT, &
                                                             rbeg, rend, extend_only=.true.)
                    if (extend_face_id /= DEFAULT_FACE_ID .and. col < last_x + 1) then
                        glyphs(col + 1:last_x + 1) = glyph_type(face_id=extend_face_id)
                        col = last_x + 1
                    end if
                end if
                s = row_start_type(pos=s%pos + 1, byte=s%byte + 1)
                ends = ROW_ENDS_IN_NEWLINE
                exit
            end if
            cells = glyph_type()
            call produce_char(c, col + s%cont, cells, n)
            if (faces) cells(1:n)%face_id = char_face(f, c, face_id)
            ! The rest of a ^X or \ooo begun on the row before (a TAB's
            ! rest follows from the column).
            first = 1
            if (s%mid .and. c /= 9) first = min(n, s%skip + 1)
            ncells = n - first + 1
            if (col + ncells > last_x .and. col > 0) then
                ends = ROW_CONTINUED
                if (n == 2 .and. cells(2)%padding) then
                    ! A wide character is unbreakable: fill the rest of the
                    ! row with continuation glyphs, like in 20.x.
                    if (present(glyphs)) then
                        glyphs(col + 1:last_x + 1) = glyph_type()
                        glyphs(col + 1:last_x + 1)%ch = iachar("\")
                    end if
                    s%cont = s%cont + col
                else
                    k = last_x - col
                    if (k > 0) then
                        if (present(glyphs)) glyphs(col + 1:col + k) = cells(first:first + k - 1)
                        if (s%pos == pt .and. .not. s%mid) cursor_col = col
                        s%skip = first - 1 + k
                        s%mid = .true.
                    end if
                    if (present(glyphs)) then
                        glyphs(last_x + 1) = glyph_type()
                        glyphs(last_x + 1)%ch = iachar("\")
                    end if
                    s%cont = s%cont + last_x
                end if
                col = last_x + 1
                exit
            end if
            if (present(glyphs)) then
                ! (Only a character alone on a too narrow row is clipped.)
                k = min(ncells, size(glyphs) - col)
                if (k > 0) glyphs(col + 1:col + k) = cells(first:first + k - 1)
            end if
            if (s%pos == pt .and. .not. s%mid) cursor_col = col
            col = col + ncells
            s%pos = s%pos + 1
            s%byte = s%byte + buf_next_char_len(buffers(bi), s%byte)
            s%mid = .false.
            s%skip = 0
        end do
        used = min(col, last_x + 1)
    end subroutine display_row

    !> The face of character C in text whose face is FACE_ID on frame F:
    !> control characters and raw bytes shown as ^X or \ooo in the
    !> escape-glyph face merged in, non-ASCII spaces and hyphens with
    !> nobreak-space and nobreak-hyphen (nobreak-char-display t).  The
    !> face part of xdisp.c:get_next_display_element.
    integer(int32) function char_face(f, c, face_id) result(id)
        type(frame_type), intent(inout) :: f
        integer(int32), intent(in) :: c, face_id
        id = face_id
        if (c /= 9 .and. c /= 10 .and. (c < 32 .or. c == 127 .or. char_byte8_p(c))) then
            ! merge_escape_glyph_face.
            id = merge_faces(f, "escape_glyph", 0_int32, face_id)
        else if (nonascii_blank_p(c)) then
            ! Merge `nobreak-space' into the current face.
            id = merge_faces(f, "nobreak_space", 0_int32, face_id)
        else if (c == int(z'AD', int32) .or. c == int(z'2010', int32) .or. c == int(z'2011', int32)) then
            ! SOFT_HYPHEN, HYPHEN, NON_BREAKING_HYPHEN.
            id = merge_faces(f, "nobreak_hyphen", 0_int32, face_id)
        end if
    end function char_face

    !> Whether C is a non-ASCII blank (Unicode general category Zs), as
    !> character.c:blankp tells for the characters `nobreak_char_display'
    !> highlights.
    elemental logical function nonascii_blank_p(c)
        integer(int32), intent(in) :: c
        nonascii_blank_p = c == int(z'A0', int32) .or. c == int(z'1680', int32) .or. &
                           (c >= int(z'2000', int32) .and. c <= int(z'200A', int32)) .or. &
                           c == int(z'202F', int32) .or. c == int(z'205F', int32) .or. c == int(z'3000', int32)
    end function nonascii_blank_p

    !> The screen rows of the line starting at `bol' in buffer slot `bi',
    !> rows(1:nrows) where each starts, and `eol' where the line ends (its
    !> newline, or the end of the accessible text).  `last_x' as for
    !> display_row.
    subroutine display_line_rows(bi, bol, last_x, rows, nrows, eol)
        integer(int64), intent(in) :: bi, bol
        integer, intent(in) :: last_x
        type(row_start_type), allocatable, intent(out) :: rows(:)
        integer, intent(out) :: nrows
        integer(int64), intent(out) :: eol
        type(row_start_type), allocatable :: grown(:)
        type(row_start_type) :: s
        integer :: ends, cursor_col, used
        allocate (rows(8))
        nrows = 0
        s%pos = bol
        s%byte = buf_charpos_to_bytepos(bi, bol)
        do
            nrows = nrows + 1
            if (nrows > size(rows)) then
                allocate (grown(2*size(rows)))
                grown(1:size(rows)) = rows
                call move_alloc(grown, rows)
            end if
            rows(nrows) = s
            call display_row(bi, s, last_x, -1_int64, ends, cursor_col, used)
            if (ends /= ROW_CONTINUED) exit
        end do
        eol = s%pos
        if (ends == ROW_ENDS_IN_NEWLINE) eol = s%pos - 1
    end subroutine display_line_rows

    !> The row among rows(1:nrows) on which the display of `pos' begins.
    pure integer function display_row_of(pos, rows, nrows) result(k)
        integer(int64), intent(in) :: pos
        type(row_start_type), intent(in) :: rows(:)
        integer, intent(in) :: nrows
        k = nrows
        do while (k > 1)
            if (rows(k)%pos < pos) exit
            if (rows(k)%pos == pos .and. .not. rows(k)%mid) exit
            k = k - 1
        end do
    end function display_row_of

    !> The start of the line holding `pos' in buffer slot `bi'.
    integer(int64) function line_beginning(bi, pos) result(bol)
        integer(int64), intent(in) :: bi, pos
        integer(int64) :: byte, begv_byte
        begv_byte = buffers(bi)%begv_byte
        byte = buf_charpos_to_bytepos(bi, pos)
        do while (byte > begv_byte)
            if (buf_fetch_byte(buffers(bi), byte - 1) == 10) exit
            byte = byte - 1
        end do
        bol = buf_bytepos_to_charpos(bi, byte)
    end function line_beginning

    !> Where display starts for window start `start': the continuation
    !> width of the row it begins.  Port of the line-start part of
    !> xdisp.c:start_display.
    function window_start_row(bi, start, last_x) result(s)
        integer(int64), intent(in) :: bi, start
        integer, intent(in) :: last_x
        type(row_start_type) :: s
        type(row_start_type), allocatable :: rows(:)
        integer(int64) :: bol, eol
        integer :: nrows, k
        s%pos = start
        s%byte = buf_charpos_to_bytepos(bi, start)
        bol = line_beginning(bi, start)
        if (bol == start) return
        call display_line_rows(bi, bol, last_x, rows, nrows, eol)
        k = nrows
        do while (k > 1)
            if (rows(k)%pos <= start) exit
            k = k - 1
        end do
        s%cont = rows(k)%cont
    end function window_start_row

    !> Lay out buffer slot `bi' from `start' in `rows' (one per window
    !> body line) `width' columns wide.  Gives the cursor position of `pt'
    !> (cursor_row -1 if it is not shown), where the display ends, and
    !> whether it reached the end of the accessible text.  The core of
    !> xdisp.c:try_window with display_line.
    subroutine layout_window(bi, start, pt, width, rows, cursor_row, cursor_col, &
                             end_pos, reached_end, region_beg, region_end, f)
        integer(int64), intent(in) :: bi, start, pt
        integer, intent(in) :: width
        type(glyph_row_type), intent(inout) :: rows(:)
        integer, intent(out) :: cursor_row, cursor_col
        integer(int64), intent(out) :: end_pos
        logical, intent(out) :: reached_end
        !> The region to highlight with the region face, [region_beg, region_end).
        integer(int64), intent(in), optional :: region_beg, region_end
        !> The frame whose faces the glyphs get (without it, all are in the
        !> default face).
        type(frame_type), intent(inout), optional :: f
        type(row_start_type) :: s
        integer(int64) :: rb, re
        integer :: r, ends, col, used
        cursor_row = -1
        cursor_col = 0
        reached_end = .false.
        rb = 0
        re = 0
        if (present(region_beg) .and. present(region_end)) then
            rb = region_beg
            re = region_end
        end if
        s = window_start_row(bi, start, width - 1)
        do r = 1, size(rows)
            rows(r)%glyphs = glyph_type()
            rows(r)%used = 0
            rows(r)%enabled_p = .true.
            if (reached_end) cycle
            if (present(f)) then
                call display_row(bi, s, width - 1, pt, ends, col, used, rows(r)%glyphs, rb, re, f)
            else
                call display_row(bi, s, width - 1, pt, ends, col, used, rows(r)%glyphs, rb, re)
            end if
            rows(r)%used = used
            if (col >= 0 .and. cursor_row < 0) then
                cursor_row = r - 1
                cursor_col = col
            end if
            if (ends == ROW_ENDS_AT_ZV) reached_end = .true.
        end do
        end_pos = s%pos
        if (s%pos >= buffers(bi)%zv) reached_end = .true.
    end subroutine layout_window

    !> The window start that puts point's screen line `dy' screen lines
    !> from the top: the start of the screen line `dy' above the one
    !> holding `pt' (not before the accessible start).  The terminal part
    !> of xdisp.c:move_it_vertically_backward as redisplay_window's
    !> recentering uses it.
    integer(int64) function recenter_start(bi, pt, last_x, dy) result(start)
        integer(int64), intent(in) :: bi, pt
        integer, intent(in) :: last_x, dy
        type(row_start_type), allocatable :: rows(:)
        integer(int64) :: bol, eol
        integer :: nrows, target
        bol = line_beginning(bi, pt)
        call display_line_rows(bi, bol, last_x, rows, nrows, eol)
        target = display_row_of(pt, rows, nrows) - dy
        do while (target < 1)
            if (bol <= buffers(bi)%begv) then
                target = 1
                exit
            end if
            bol = line_beginning(bi, bol - 1)
            call display_line_rows(bi, bol, last_x, rows, nrows, eol)
            target = target + nrows
        end do
        start = rows(target)%pos
    end function recenter_start

    ! ------------------------------------------------------------------
    ! Windows, mode line, echo area
    ! ------------------------------------------------------------------

    !> Make row `vpos' of the desired matrix a full-width blank row, to be
    !> filled by the windows on it.
    subroutine blank_row(f, vpos)
        type(frame_type), intent(inout) :: f
        integer, intent(in) :: vpos
        integer :: cols
        cols = f%desired_matrix%ncols
        associate (m => f%desired_matrix%rows(vpos))
            if (allocated(m%glyphs)) then
                if (size(m%glyphs) /= cols) deallocate (m%glyphs)
            end if
            if (.not. allocated(m%glyphs)) allocate (m%glyphs(cols))
            m%glyphs = glyph_type()
            m%used = cols
            m%enabled_p = .true.
        end associate
    end subroutine blank_row

    !> Copy window rows into the frame's desired matrix, from column
    !> `left' (0-based), with the vertical border after them if `divider'.
    subroutine put_rows(f, top, left, rows, divider)
        type(frame_type), intent(inout) :: f
        integer, intent(in) :: top, left
        type(glyph_row_type), intent(in) :: rows(:)
        logical, intent(in) :: divider
        integer :: r, n, cols
        cols = f%desired_matrix%ncols
        do r = 1, size(rows)
            associate (m => f%desired_matrix%rows(top + r - 1))
                n = min(size(rows(r)%glyphs), cols - left)
                if (n > 0) m%glyphs(left + 1:left + n) = rows(r)%glyphs(1:n)
                ! The border between side-by-side windows (vertical-border).
                if (divider .and. left + n + 1 <= cols) then
                    m%glyphs(left + n + 1) = glyph_type(face_id=VERTICAL_BORDER_FACE_ID)
                    m%glyphs(left + n + 1)%ch = iachar("|")
                end if
            end associate
        end do
    end subroutine put_rows

    !> Display window `w': from its start if that shows point (or the
    !> start is forced), else recentered around point; then its mode line.
    !> `pt' is the window's point; `selected' says whether it is the
    !> selected window.  Port of the core of xdisp.c:redisplay_window.
    subroutine redisplay_window(f, w, pt, selected)
        type(frame_type), intent(inout) :: f
        type(window_type), intent(inout) :: w
        integer(int64), intent(in) :: pt
        logical, intent(in) :: selected
        type(glyph_row_type), allocatable :: rows(:)
        type(status_type) :: status
        integer(int64) :: bi, start, end_pos
        integer(int64) :: rb, re, m
        integer :: body, width, cursor_row, cursor_col, r
        logical :: reached_end
        call resolve_buffer(w%buffer, bi, status)
        if (.not. status%ok()) return
        body = window_body_lines(w)
        width = window_text_cols(w)
        allocate (rows(body))
        do r = 1, body
            allocate (rows(r)%glyphs(width))
        end do
        ! Transient Mark mode highlights an active region, in the selected
        ! window only (highlight-nonselected-windows nil).
        rb = 0
        re = 0
        if (transient_mark_mode%value .and. buffers(bi)%mark_active .and. .not. f%minibuffer_selected .and. &
            .not. w%mini .and. selected) then
            m = buffers(bi)%mark%index
            if (m > 0) then
                if (markers(m)%buffer == bi) then
                    rb = min(pt, markers(m)%charpos)
                    re = max(pt, markers(m)%charpos)
                end if
            end if
        end if
        start = max(buffers(bi)%begv, min(window_start(w), buffers(bi)%zv))
        call layout_window(bi, start, pt, width, rows, cursor_row, cursor_col, end_pos, &
                           reached_end, rb, re, f)
        if (cursor_row < 0 .and. .not. w%force_start) then
            ! Point is not visible: recenter (scroll-conservatively 0),
            ! the window start half the height of the window backward
            ! from point.
            start = recenter_start(bi, pt, width - 1, body/2)
            call layout_window(bi, start, pt, width, rows, cursor_row, cursor_col, end_pos, &
                               reached_end, rb, re, f)
        end if
        call set_window_start(w, start, status, noforce=.true.)
        w%window_end_pos = end_pos
        w%window_end_valid = reached_end
        w%cursor_vpos = cursor_row
        w%cursor_hpos = cursor_col
        call put_rows(f, w%top_line, w%left_col, rows, w%divider .and. .not. w%mini)
        if (.not. w%mini) call display_mode_line(f, w, bi, start, pt, selected)
    end subroutine redisplay_window

    !> 1 + the number of newlines before `pos' in buffer slot `bi'.
    integer(int64) function line_number(bi, pos) result(n)
        integer(int64), intent(in) :: bi, pos
        integer(int64) :: byte, end_byte
        n = 1
        byte = buffers(bi)%begv_byte
        end_byte = buf_charpos_to_bytepos(bi, pos)
        do while (byte < end_byte)
            if (buf_fetch_byte(buffers(bi), byte) == 10) n = n + 1
            byte = byte + 1
        end do
    end function line_number

    !> The characters of `text' (internal multibyte form), a byte that is
    !> not part of a valid form taken as a raw byte.
    subroutine decode_string(text, chars, n)
        character(len=*), intent(in) :: text
        integer(int32), allocatable, intent(out) :: chars(:)
        integer, intent(out) :: n
        integer(int64) :: pos
        integer(int32) :: length
        allocate (chars(len(text)))
        n = 0
        pos = 1
        do while (pos <= len(text, kind=int64))
            n = n + 1
            length = multibyte_length(text, pos, .true.)
            if (length == 0) then
                chars(n) = byte8_to_char(ichar(text(pos:pos), int32))
                length = 1
            else
                call string_char_and_length(text, pos, chars(n), length)
            end if
            pos = pos + length
        end do
    end subroutine decode_string

    !> `text' padded with spaces to `width' characters (display_string's
    !> FIELD_WIDTH).
    function pad_chars(text, width) result(s)
        character(len=*), intent(in) :: text
        integer, intent(in) :: width
        character(len=:), allocatable :: s
        integer(int32), allocatable :: chars(:)
        integer :: n
        call decode_string(text, chars, n)
        s = text//repeat(" ", max(0, width - n))
    end function pad_chars

    !> The first `width' characters of `text' (display_string's PRECISION).
    function truncate_chars(text, width) result(s)
        character(len=*), intent(in) :: text
        integer, intent(in) :: width
        character(len=:), allocatable :: s
        integer(int64) :: pos
        integer(int32) :: length
        integer :: n
        pos = 1
        n = 0
        do while (pos <= len(text, kind=int64) .and. n < width)
            length = max(1_int32, multibyte_length(text, pos, .true.))
            pos = pos + length
            n = n + 1
        end do
        s = text(1:pos - 1)
    end function truncate_chars

    !> The default mode line of a buffer named NAME on a text terminal,
    !> before it is cut to the window: bindings.el's
    !> standard-mode-line-format as xdisp.c:display_mode_element and
    !> decode_mode_spec expand it ("%e", mode-line-front-space "-",
    !> mode-line-mule-info "%z" with the end-of-line mnemonic,
    !> mode-line-modified "%1*%1+", mode-line-remote "%1@",
    !> mode-line-frame-identification " %F  ", "%12b", "   ",
    !> mode-line-position: (-3 "%p") and (6 " L%l"), "  ",
    !> mode-line-modes "%[(" MODES ")%] ", mode-line-end-spaces "-%-").
    !> POSITION is what %p gives (`All', `Top', `Bottom' or ` 5%'), LINE
    !> what %l gives, MODES the text in parentheses (mode_line_modes,
    !> default "Fundamental").  The dashes of %- are WIDTH long.
    !> Simplified: all coding systems are UTF-8 with Unix line ends
    !> (UUU:), the buffers are local (-) and the frame is F1.
    function mode_line_text(name, modified, read_only, position, line, width, modes) result(s)
        character(len=*), intent(in) :: name, position, line
        logical, intent(in) :: modified, read_only
        integer, intent(in) :: width
        character(len=*), intent(in), optional :: modes
        character(len=:), allocatable :: s
        character(len=1) :: star, plus
        ! %*: % if read-only, else * if modified; %+: * if modified, else
        ! % if read-only.
        star = "-"
        plus = "-"
        if (modified) then
            star = "*"
            plus = "*"
        end if
        if (read_only) then
            star = "%"
            if (.not. modified) plus = "%"
        end if
        s = "-UUU:"//star//plus//"-"//" F1  "//pad_chars(name, 12)//"   "// &
            truncate_chars(position, 3)//pad_chars(" L"//line, 6)//"  ("
        if (present(modes)) then
            s = s//modes
        else
            s = s//"Fundamental"
        end if
        s = s//") "//repeat("-", max(0, width))
    end function mode_line_text

    !> What %p shows: the percentage of the accessible text above window
    !> start `start', or All, Top or Bottom.  `at_end' says the window
    !> shows the end of the accessible text.  Port of the %p case of
    !> xdisp.c:decode_mode_spec, with percent99.
    function mode_line_percent(bi, start, at_end) result(s)
        integer(int64), intent(in) :: bi, start
        logical, intent(in) :: at_end
        character(len=:), allocatable :: s
        character(len=8) :: buf
        integer(int64) :: begv, d
        integer :: percent
        begv = buffers(bi)%begv
        if (at_end) then
            if (start <= begv) then
                s = "All"
            else
                s = "Bottom"
            end if
        else if (start <= begv) then
            s = "Top"
        else
            ! percent99: rounded up, at most 99.
            d = buffers(bi)%zv - begv
            percent = int(min(99_int64, (d - 1 + 100*(start - begv))/d))
            write (buf, '(i2,"%")') percent
            s = trim(buf)
        end if
    end function mode_line_percent

    !> What %l shows for a window `height' lines high starting at
    !> `start': point's line number, or ?? when the lines before the
    !> window start are too long to count (line-number-display-limit-width
    !> 200).  The counting part of the %l case of xdisp.c:decode_mode_spec
    !> (no base line cache, line-number-display-limit nil).
    function mode_line_line(bi, start, pt, height) result(s)
        integer(int64), intent(in) :: bi, start, pt
        integer, intent(in) :: height
        character(len=:), allocatable :: s
        character(len=20) :: buf
        integer(int64), parameter :: limit_width = 200
        integer(int64) :: distance, limit, byte, nlines, wanted
        if (start > buffers(bi)%begv .and. start <= buffers(bi)%zv) then
            ! Look back for height * 2 + 30 lines from the window start,
            ! within line-number-display-limit-width characters a line.
            wanted = 2_int64*height + 30
            distance = limit_width*wanted
            limit = buffers(bi)%begv
            if (start - distance > limit) then
                limit = start - distance
                byte = buf_charpos_to_bytepos(bi, start)
                nlines = 0
                do while (byte > buf_charpos_to_bytepos(bi, limit) .and. nlines < wanted)
                    byte = byte - 1
                    if (buf_fetch_byte(buffers(bi), byte) == 10) nlines = nlines + 1
                end do
                if (nlines < wanted) then
                    s = "??"
                    return
                end if
            end if
        end if
        write (buf, "(i0)") line_number(bi, pt)
        s = trim(buf)
    end function mode_line_line

    !> Lay out `text' as a mode line `width' columns wide in `row':
    !> truncated at the right edge (a TAB or wide character cut there
    !> shows as -), filled with spaces, in face `face_id' (default
    !> mode_line_active), characters `span_first' to `span_last' in face
    !> `span_face' (a propertized part of the mode line).  The terminal
    !> part of xdisp.c:display_string for mode lines, with pad_mode_line.
    subroutine mode_line_row(text, width, row, face_id, span_first, span_last, span_face)
        character(len=*), intent(in) :: text
        integer, intent(in) :: width
        type(glyph_type), intent(out) :: row(:)
        integer(int32), intent(in), optional :: face_id, span_face
        integer, intent(in), optional :: span_first, span_last
        type(glyph_type) :: cells(MAX_CHAR_CELLS)
        integer(int32), allocatable :: chars(:)
        integer(int32) :: base
        integer :: nchars, i, x, n
        base = MODE_LINE_ACTIVE_FACE_ID
        if (present(face_id)) base = face_id
        row = glyph_type()
        row(1:width)%face_id = base
        call decode_string(text, chars, nchars)
        x = 0
        do i = 1, nchars
            if (x >= width) exit
            call produce_char(chars(i), x, cells, n)
            cells(1:n)%face_id = base
            if (present(span_first) .and. present(span_last) .and. present(span_face)) then
                if (i >= span_first .and. i <= span_last) cells(1:n)%face_id = span_face
            end if
            if (chars(i) == 9 .or. n == 1 .or. (n == 2 .and. cells(min(n, 2))%padding)) then
                ! One display element.
                if (x + n > width) then
                    row(x + 1:width) = cells(1:width - x)
                    row(width) = glyph_type(face_id=base)
                    row(width)%ch = iachar("-")
                    x = width
                    exit
                end if
                row(x + 1:x + n) = cells(1:n)
            else
                ! ^X and \ooo are display vectors: one element a cell.
                n = min(n, width - x)
                row(x + 1:x + n) = cells(1:n)
            end if
            x = x + n
        end do
    end subroutine mode_line_row

    !> Show LIGHTER in the mode line while VARIABLE is non-nil (replacing
    !> VARIABLE's earlier lighter).  Port of the mode-line part of
    !> subr.el:add-minor-mode (minor-mode-alist).
    subroutine add_minor_mode_lighter(variable, lighter)
        character(len=*), intent(in) :: variable, lighter
        type(lighter_type), allocatable :: grown(:)
        integer :: i
        do i = 1, n_lighters
            if (lighters(i)%variable == variable) then
                lighters(i)%lighter = lighter
                return
            end if
        end do
        if (.not. allocated(lighters)) allocate (lighters(8))
        if (n_lighters == size(lighters)) then
            allocate (grown(2*n_lighters))
            grown(1:n_lighters) = lighters
            call move_alloc(grown, lighters)
        end if
        n_lighters = n_lighters + 1
        lighters(n_lighters)%variable = variable
        lighters(n_lighters)%lighter = lighter
    end subroutine add_minor_mode_lighter

    !> The modes part of buffer BI's mode line: `mode_name', " Narrow"
    !> if the buffer is narrowed (%n), and the lighters of
    !> `minor_mode_alist' whose variables are non-nil there.
    function mode_line_modes(bi) result(s)
        integer(int64), intent(in) :: bi
        character(len=:), allocatable :: s
        type(value_type) :: v
        type(status_type) :: st
        integer :: i
        if (allocated(buffers(bi)%mode_name)) then
            s = buffers(bi)%mode_name
        else
            s = "Fundamental"
        end if
        if (buffers(bi)%begv > 1 .or. buffers(bi)%zv < buffers(bi)%text%z) s = s//" Narrow"
        do i = 1, n_lighters
            call buffer_local_value(sym(lighters(i)%variable), bi, v, st)
            if (.not. st%ok()) cycle
            if (.not. nilp(v)) s = s//lighters(i)%lighter
        end do
    end function mode_line_modes

    !> Draw the mode line of window `w' (showing buffer slot `bi' from
    !> `start', point `pt'): mode_line_text cut to the window, less the
    !> column of the border glyph of a window with one to its right.  The
    !> mode line is in the mode_line_active face if `w' is the selected
    !> window (or was when the minibuffer was entered), else in
    !> mode_line_inactive (mode-line-in-non-selected-windows t); the
    !> buffer name (mode-line-buffer-identification, "%12b") has the
    !> mode_line_buffer_id face merged in.  Port of the terminal part of
    !> xdisp.c:display_mode_line (CURRENT_MODE_LINE_ACTIVE_FACE_ID_3),
    !> with the border of dispnew.c:build_frame_matrix_from_leaf_window.
    subroutine display_mode_line(f, w, bi, start, pt, selected)
        type(frame_type), intent(inout) :: f
        type(window_type), intent(in) :: w
        integer(int64), intent(in) :: bi, start, pt
        logical, intent(in) :: selected
        character(len=:), allocatable :: text
        type(glyph_type), allocatable :: row(:)
        integer(int32), allocatable :: chars(:)
        type(status_type) :: status
        logical :: modified
        integer :: width, nname
        integer(int32) :: face_id, id_face
        call buffer_modified_p(w%buffer, modified, status)
        width = window_text_cols(w)
        text = mode_line_text(buffers(bi)%name, modified, buffers(bi)%read_only, &
                              mode_line_percent(bi, start, w%window_end_valid), &
                              mode_line_line(bi, start, pt, w%total_lines), width, mode_line_modes(bi))
        face_id = MODE_LINE_INACTIVE_FACE_ID
        if (selected) face_id = MODE_LINE_ACTIVE_FACE_ID
        id_face = merge_faces(f, "mode_line_buffer_id", 0_int32, face_id)
        ! The buffer name, padded to 12, after "-UUU:**- F1  ".
        call decode_string(buffers(bi)%name, chars, nname)
        allocate (row(w%total_cols))
        call mode_line_row(text, width, row, face_id, 14, 13 + max(12, nname), id_face)
        if (width < w%total_cols) then
            row(width + 1:) = glyph_type(face_id=VERTICAL_BORDER_FACE_ID)
            row(w%total_cols)%ch = iachar("|")
        end if
        associate (m => f%desired_matrix%rows(w%top_line + w%total_lines - 1))
            m%glyphs(w%left_col + 1:w%left_col + w%total_cols) = row
        end associate
    end subroutine display_mode_line

    !> Show the last message in the minibuffer window, or blank it: its
    !> first screen line, a line that does not fit continued with \ in the
    !> last column.  Port of the display part of xdisp.c:echo_area_display
    !> (one line: resize-mini-windows is not ported).
    subroutine display_echo_area(f, endcol)
        type(frame_type), intent(inout) :: f
        !> Column after the text.
        integer, intent(out) :: endcol
        type(glyph_type) :: cells(MAX_CHAR_CELLS)
        integer(int32), allocatable :: chars(:)
        integer :: nchars, i, x, n, k, cols, last_x
        cols = windows(f%minibuffer_window%index)%total_cols
        last_x = cols - 1
        endcol = 0
        associate (m => f%desired_matrix%rows(windows(f%minibuffer_window%index)%top_line))
            m%glyphs = glyph_type()
            m%used = 0
            m%enabled_p = .true.
            if (.not. allocated(last_message)) return
            call decode_string(last_message, chars, nchars)
            x = 0
            do i = 1, nchars
                if (chars(i) == 10) exit
                cells = glyph_type()
                call produce_char(chars(i), x, cells, n)
                if (x + n > last_x .and. x > 0) then
                    if (n == 2 .and. cells(min(n, 2))%padding) then
                        m%glyphs(x + 1:cols)%ch = iachar("\")
                    else
                        k = last_x - x
                        if (k > 0) m%glyphs(x + 1:x + k) = cells(1:k)
                        m%glyphs(cols)%ch = iachar("\")
                    end if
                    x = cols
                    exit
                end if
                k = min(n, cols - x)
                m%glyphs(x + 1:x + k) = cells(1:k)
                x = x + n
            end do
            m%used = min(x, cols)
            endcol = m%used
        end associate
    end subroutine display_echo_area

    !> Show the active minibuffer's text on the minibuffer line, scrolled
    !> horizontally to keep point visible ("$" marks text cut off at
    !> either edge).  `cursor_col' is point's column on the screen line.
    !> The one-line case of xdisp.c:display_line for a minibuffer window
    !> with auto-hscroll.
    subroutine display_minibuffer(f, cursor_col)
        type(frame_type), intent(inout) :: f
        integer, intent(out) :: cursor_col
        type(glyph_type), allocatable :: line(:), grown(:)
        type(glyph_type) :: cells(MAX_CHAR_CELLS)
        type(status_type) :: status
        integer(int64) :: bi, pos, byte
        integer(int32) :: c
        integer :: x, n, cols, col_of_point
        integer(int64) :: mpos, stop
        integer(int32) :: clen, face_id
        cursor_col = 0
        call resolve_buffer(windows(f%minibuffer_window%index)%buffer, bi, status)
        if (.not. status%ok()) return
        cols = windows(f%minibuffer_window%index)%total_cols
        allocate (line(max(2*cols, 16)))
        ! Lay out the whole accessible text as one long line.
        x = 0
        col_of_point = -1
        pos = buffers(bi)%begv
        byte = buffers(bi)%begv_byte
        face_id = DEFAULT_FACE_ID
        stop = -1
        do while (pos < buffers(bi)%zv)
            if (pos == buffers(bi)%pt) col_of_point = x
            ! The faces of the text (the prompt's minibuffer_prompt).
            if (pos >= stop) face_id = face_at_buffer_position(f, bi, pos, stop, pos + TEXT_PROP_DISTANCE_LIMIT)
            c = buf_fetch_char(buffers(bi), byte)
            call put(c)
            byte = byte + buf_next_char_len(buffers(bi), byte)
            pos = pos + 1
        end do
        if (col_of_point < 0) col_of_point = x
        face_id = DEFAULT_FACE_ID
        ! The messages of minibuffer-message and set-minibuffer-message
        ! after the text (their overlays at the end).
        if (allocated(minibuffer_message_text)) then
            mpos = 1
            do while (mpos <= len(minibuffer_message_text, kind=int64))
                call string_char_and_length(minibuffer_message_text, mpos, c, clen)
                call put(c)
                mpos = mpos + clen
            end do
        end if
        if (allocated(set_minibuffer_message_text)) then
            mpos = 1
            do while (mpos <= len(set_minibuffer_message_text, kind=int64))
                call string_char_and_length(set_minibuffer_message_text, mpos, c, clen)
                call put(c)
                mpos = mpos + clen
            end do
        end if
        ! auto-hscroll: recenter point when it would leave the line.
        associate (w => windows(f%minibuffer_window%index))
            if (col_of_point < w%hscroll .or. col_of_point - w%hscroll >= cols - 1) then
                w%hscroll = max(0, col_of_point - cols/2)
            end if
            associate (m => f%desired_matrix%rows(w%top_line))
                m%glyphs = glyph_type()
                m%enabled_p = .true.
                n = min(cols, max(0, x - w%hscroll))
                if (n > 0) m%glyphs(1:n) = line(w%hscroll + 1:w%hscroll + n)
                m%used = n
                ! Truncation glyphs.
                if (w%hscroll > 0) then
                    m%glyphs(1) = glyph_type()
                    m%glyphs(1)%ch = iachar("$")
                end if
                if (x - w%hscroll > cols) then
                    m%glyphs(cols) = glyph_type()
                    m%glyphs(cols)%ch = iachar("$")
                end if
                ! A wide character cut by the left edge shows as a space.
                if (n > 0) then
                    if (m%glyphs(1)%padding) then
                        m%glyphs(1) = glyph_type()
                        m%glyphs(1)%ch = 32
                    end if
                end if
            end associate
            cursor_col = col_of_point - w%hscroll
        end associate
    contains

        !> Lay out character `c' at column x.
        subroutine put(c)
            integer(int32), intent(in) :: c
            call produce_char(c, x, cells, n)
            cells(1:n)%face_id = char_face(f, c, face_id)
            if (x + n > size(line)) then
                allocate (grown(2*size(line) + n))
                grown(1:x) = line(1:x)
                call move_alloc(grown, line)
            end if
            line(x + 1:x + n) = cells(1:n)
            x = x + n
        end subroutine put
    end subroutine display_minibuffer

    !> Show `text' in the echo area; an empty `text' clears it.  While the
    !> minibuffer is active the text goes after the minibuffer's contents
    !> instead, enclosed in [...] unless it already is, until the next
    !> input event.  Port of editfns.c:Fmessage (with a plain string; no
    !> *Messages* log yet) with xdisp.c:set_message and clear_message
    !> calling minibuffer.el's set-minibuffer-message and
    !> clear-minibuffer-message (set-message-functions,
    !> clear-message-function).
    subroutine message(text)
        character(len=*), intent(in) :: text
        if (len(text) == 0) then
            if (allocated(set_minibuffer_message_text)) deallocate (set_minibuffer_message_text)
            last_message = ""
        else if (minibuffer_active_p()) then
            if (bracketed_message_p(text)) then
                set_minibuffer_message_text = text
            else
                set_minibuffer_message_text = " ["//text//"]"
            end if
        else
            last_message = text
        end if
    end subroutine message

    !> Whether `text' matches "\` *\[.+\]\'".
    pure logical function bracketed_message_p(text) result(p)
        character(len=*), intent(in) :: text
        integer :: k
        p = .false.
        k = verify(text, " ")
        if (k == 0 .or. len(text) - k < 2) return
        if (text(k:k) /= "[" .or. text(len(text):len(text)) /= "]") return
        p = index(text(k + 1:len(text) - 1), achar(10)) == 0
    end function bracketed_message_p

    !> Whether the selected frame's minibuffer is active (the minibuffer
    !> window shows a minibuffer): window.c:Factive_minibuffer_window.
    logical function minibuffer_active_p() result(p)
        integer(int64) :: bi
        type(status_type) :: status
        p = .false.
        if (.not. window_valid_p(selected_frame%minibuffer_window)) return
        call resolve_buffer(windows(selected_frame%minibuffer_window%index)%buffer, bi, status)
        p = status%ok()
    end function minibuffer_active_p

    !> Redisplay the frame: each window with its mode line (the selected
    !> one showing the current buffer unless the minibuffer is selected),
    !> and the echo area or active minibuffer, then update the terminal.
    !> Port of the core of xdisp.c:redisplay_internal.
    subroutine redisplay(f, status)
        type(frame_type), intent(inout) :: f
        type(status_type), intent(out) :: status
        type(handle_type) :: cb
        type(handle_type), allocatable :: leaves(:)
        integer(int64) :: bi, pt
        integer :: endcol, mini_col, k, r
        logical :: showing_message, mini_active, selected
        if (.not. f%glyphs_initialized_p) call adjust_frame_glyphs(f)
        if (.not. window_valid_p(f%root_window)) call init_frame_windows(f)
        if (windows(f%root_window%index)%total_lines == 0) call init_frame_windows(f)
        if (.not. allocated(f%faces)) call realize_basic_faces(f)
        do r = 0, windows(f%minibuffer_window%index)%top_line - 1
            call blank_row(f, r)
        end do
        ! Each live window, at its point: the buffer's point for the
        ! selected one, its own (pointm) for the others.
        call window_leaves(f%root_window, leaves)
        do k = 1, size(leaves)
            associate (w => windows(leaves(k)%index))
                call resolve_buffer(w%buffer, bi, status)
                if (.not. status%ok()) cycle
                selected = same_window(leaves(k), f%selected_window)
                if (selected) then
                    pt = buffers(bi)%pt
                    ! Keep the window's own point current too.
                    call set_window_point(w, pt, status)
                else
                    pt = window_point(w)
                end if
                call redisplay_window(f, w, pt, selected)
            end associate
        end do
        status = status_type()
        ! The minibuffer line: a message if there is one, else the active
        ! minibuffer, else blank.
        showing_message = .false.
        if (allocated(last_message)) showing_message = len(last_message) > 0
        associate (mw => windows(f%minibuffer_window%index), sw => windows(f%selected_window%index))
            mini_active = f%minibuffer_selected .and. mw%buffer%index /= 0
            mini_col = 0
            if (mini_active .and. .not. showing_message) then
                call display_minibuffer(f, mini_col)
            else
                call display_echo_area(f, endcol)
            end if
            f%cursor_vpos = -1
            if (mini_active .and. .not. showing_message) then
                f%cursor_vpos = mw%top_line
                f%cursor_hpos = min(mini_col, mw%total_cols - 1)
            else if (cursor_in_echo_area .or. (mini_active .and. showing_message)) then
                f%cursor_vpos = mw%top_line
                f%cursor_hpos = min(endcol, mw%total_cols - 1)
            else if (sw%cursor_vpos >= 0) then
                f%cursor_vpos = sw%top_line + sw%cursor_vpos
                f%cursor_hpos = sw%left_col + sw%cursor_hpos
            end if
        end associate
        call update_frame(f, status)
    end subroutine redisplay
end module xdisp_fm
