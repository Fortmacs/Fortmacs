! dispnew_fm.f90 -- updating the terminal screen from glyph matrices.
! Ported from GNU Emacs 31.1 src/dispnew.c.
!
! Copyright (C) 1985-1988, 1993-1995, 1997-2026 Free Software Foundation,
! Inc.
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

!> Redisplay fills a frame's desired matrix; `update_frame' sends the
!> terminal the least output that turns the screen (the current matrix)
!> into it, row by row (`write_row': keep matching prefixes and suffixes,
!> insert or delete characters where that is cheaper than rewriting).
!>
!> A window-system frame (one with a redisplay interface) has no frame
!> matrices: redisplay hands each window's rows to `update_window_line',
!> which draws them through the interface, and `update_frame' flushes.
!>
!> Not ported yet: window matrices (redisplay writes frame rows on a
!> terminal; on a window-system frame every row is drawn whole, without
!> comparing it with what the window showed), line insertion/deletion
!> optimization (scroll.c), child frames, menus, pausing for input, frame
!> size changes.
module dispnew_fm
    use, intrinsic :: iso_fortran_env, only: int64
    use frame_fm, only: frame_type, selected_frame, frame_window_p, selected_window
    use window_fm, only: windows, window_valid_p
    use handle_fm, only: handle_type
    use commands_fm, only: commands
    use dispextern_fm, only: glyph_type, glyph_matrix_type, glyph_row_type, &
                             glyph_equal_p, char_glyph_space_p, &
                             FACE_TTY_DEFAULT_FG_COLOR, FACE_TTY_DEFAULT_BG_COLOR
    use termchar_fm, only: tty_flush, tty_put_bytes
    use term_fm, only: tty_cursor_to, tty_write_glyphs, tty_insert_glyphs, &
                       tty_delete_glyphs, tty_clear_end_of_line, tty_clear_frame, &
                       tty_update_end, tty_ring_bell
    use error_fm, only: status_type, set_error, format_message, FM_GENERIC_ERROR, FM_WRONG_TYPE_ARGUMENT
    use keyboard_fm, only: ring_bell, bitch_at_user, redisplay_hook, executing_kbd_macro_p
    implicit none(type, external)
    private

    public :: adjust_frame_glyphs, clear_current_matrices, clear_desired_matrices, &
              redraw_frame, update_frame, write_row, make_matrix_current, &
              set_desired_row, ding, frame_ring_bell, update_window_line
    public :: frame_or_buffer_changed_p
    public :: redraw_display, redisplay, send_string_to_terminal, internal_show_cursor, internal_show_cursor_p, &
              syms_of_display

    !> The state `frame_or_buffer_changed_p' compares with: the buffers
    !> (by serial number, in the order of the buffer list) and their
    !> read-only and modified flags; VALID false for none yet (nil).
    type, public :: frame_buffer_state_type
        logical :: valid = .false.
        integer(int64), allocatable :: serials(:)
        logical, allocatable :: read_only(:), modified(:)
    end type frame_buffer_state_type

contains

    ! ------------------------------------------------------------------
    ! Matrices
    ! ------------------------------------------------------------------

    !> Allocate M with NROWS blank rows of NCOLS glyphs.  Port of
    !> dispnew.c:new_glyph_matrix and dispnew.c:adjust_glyph_matrix for a
    !> frame matrix (its glyphs are allocatable arrays, not a glyph pool).
    subroutine allocate_matrix(m, nrows, ncols)
        type(glyph_matrix_type), intent(inout) :: m
        integer, intent(in) :: nrows, ncols
        integer :: r

        if (allocated(m%rows)) deallocate (m%rows)
        allocate (m%rows(0:nrows - 1))

        do r = 0, nrows - 1
            allocate (m%rows(r)%glyphs(ncols))
        end do

        m%nrows = nrows
        m%ncols = ncols
    end subroutine allocate_matrix

    !> Give `f' matrices of its terminal's size (contents unknown, so the
    !> next update redraws everything), and a default face if it has none.
    !> A window-system frame has no frame matrices.  Port of the frame part
    !> of dispnew.c:adjust_frame_glyphs, with
    !> dispnew.c:adjust_frame_glyphs_for_frame_redisplay and
    !> dispnew.c:allocate_matrices_for_frame_redisplay.
    subroutine adjust_frame_glyphs(f)
        type(frame_type), intent(inout) :: f
        integer :: nrows, ncols

        nrows = f%tty%wcm%cm_rows
        ncols = f%tty%wcm%cm_cols

        if (frame_window_p(f)) then
            nrows = 0
            ncols = 0
        end if

        if (f%current_matrix%nrows /= nrows .or. f%current_matrix%ncols /= ncols) then
            call allocate_matrix(f%current_matrix, nrows, ncols)
            call allocate_matrix(f%desired_matrix, nrows, ncols)
            f%garbaged = .true.
        end if

        if (.not. allocated(f%faces)) then
            ! The realized default face of a terminal (xfaces.c).
            allocate (f%faces(0:0))
            f%faces(0)%foreground = FACE_TTY_DEFAULT_FG_COLOR
            f%faces(0)%background = FACE_TTY_DEFAULT_BG_COLOR
        end if

        f%glyphs_initialized_p = .true.
    end subroutine adjust_frame_glyphs

    !> Port of dispnew.c:clear_current_matrices (with
    !> dispnew.c:clear_glyph_matrix, dispnew.c:clear_glyph_matrix_rows and
    !> dispnew.c:clear_glyph_row).
    subroutine clear_current_matrices(f)
        type(frame_type), intent(inout) :: f
        integer :: r

        do r = 0, f%current_matrix%nrows - 1
            f%current_matrix%rows(r)%enabled_p = .false.
            f%current_matrix%rows(r)%used = 0
        end do
    end subroutine clear_current_matrices

    !> Port of dispnew.c:clear_desired_matrices (with
    !> dispnew.c:clear_glyph_matrix).
    subroutine clear_desired_matrices(f)
        type(frame_type), intent(inout) :: f
        integer :: r

        do r = 0, f%desired_matrix%nrows - 1
            f%desired_matrix%rows(r)%enabled_p = .false.
            f%desired_matrix%rows(r)%used = 0
        end do
    end subroutine clear_desired_matrices

    !> Make row `vpos' of the desired matrix show `glyphs' (at most a
    !> line; the rest of the line is blank).  What redisplay does through
    !> its iterator.  Not a port.
    subroutine set_desired_row(f, vpos, glyphs)
        type(frame_type), intent(inout) :: f
        integer, intent(in) :: vpos
        type(glyph_type), intent(in) :: glyphs(:)
        integer :: n

        associate (row => f%desired_matrix%rows(vpos))
            n = min(size(glyphs), size(row%glyphs))
            row%glyphs = glyph_type()
            row%glyphs(1:n) = glyphs(1:n)
            row%used = n
            row%enabled_p = .true.
        end associate
    end subroutine set_desired_row

    !> Copy the desired rows that were updated into the current matrix.
    !> Port of dispnew.c:make_matrix_current.
    subroutine make_matrix_current(f)
        type(frame_type), intent(inout) :: f
        integer :: r

        do r = 0, f%desired_matrix%nrows - 1
            if (.not. f%desired_matrix%rows(r)%enabled_p) cycle
            f%current_matrix%rows(r)%glyphs = f%desired_matrix%rows(r)%glyphs
            f%current_matrix%rows(r)%used = f%desired_matrix%rows(r)%used
            f%current_matrix%rows(r)%enabled_p = .true.
            f%desired_matrix%rows(r)%enabled_p = .false.
        end do
    end subroutine make_matrix_current

    ! ------------------------------------------------------------------
    ! Updating
    ! ------------------------------------------------------------------

    !> Clear frame F and output again what is supposed to appear on it.
    !> Port of dispnew.c:Fredraw_frame and dispnew.c:redraw_frame: the
    !> screen is cleared and what it showed forgotten.
    subroutine redraw_frame(f)
        type(frame_type), intent(inout) :: f

        if (frame_window_p(f)) then
            call f%rif%clear_frame()
            f%garbaged = .false.
            return
        end if

        call tty_clear_frame(f%tty)
        call clear_current_matrices(f)
        call tty_update_end(f%tty, selected_cursor_off_p())
        f%garbaged = .false.
    end subroutine redraw_frame

    !> Whether the selected window shows no cursor (its `cursor_off_p',
    !> which term.c:tty_update_end reads).  Not a port.
    logical function selected_cursor_off_p() result(off)
        type(handle_type) :: w

        w = selected_window()
        off = .false.
        if (window_valid_p(w)) off = windows(w%index)%cursor_off_p
    end function selected_cursor_off_p

    !> Clear and redisplay all visible frames.  Port of
    !> dispnew.c:Fredraw_display (one frame).
    subroutine redraw_display()
        call redraw_frame(selected_frame)
    end subroutine redraw_display

    !> Clear and redisplay all visible frames.  Port of
    !> dispnew.c:Fredraw_display (the command).
    subroutine cmd_redraw_display(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call redraw_display()
    end subroutine cmd_redraw_display

    !> Perform redisplay.
    !> Value is true if redisplay has been performed, false if
    !> executing a keyboard macro.  Port of dispnew.c:Fredisplay (FORCE,
    !> ignored upstream, is left out; there are no window-system events to
    !> swallow), with xdisp.c:redisplay_preserve_echo_area (redisplay
    !> keeps the echo area as it is).
    logical function redisplay() result(done)
        done = .false.
        if (executing_kbd_macro_p()) return
        if (associated(redisplay_hook)) call redisplay_hook()
        done = .true.
    end function redisplay

    !> Send STRING to the terminal without alteration.
    !> Control characters in STRING will have terminal-dependent effects.
    !> Port of dispnew.c:Fsend_string_to_terminal (the selected frame's
    !> terminal; no termscript).
    subroutine send_string_to_terminal(string, status)
        character(len=*), intent(in) :: string
        type(status_type), intent(out) :: status

        status = status_type()

        if (frame_window_p(selected_frame)) then
            call set_error(status, FM_GENERIC_ERROR, format_message("Device 0 is not a termcap terminal device"))
            return
        end if

        call tty_put_bytes(selected_frame%tty, string)
        call tty_flush(selected_frame%tty, status)
    end subroutine send_string_to_terminal

    !> Set the cursor-visibility flag of WINDOW to SHOW.
    !> WINDOW absent means use the selected window.  SHOW true means
    !> show a cursor in WINDOW in the next redisplay.  SHOW false means
    !> don't show a cursor.  Port of dispnew.c:Finternal_show_cursor.
    subroutine internal_show_cursor(show, status, window)
        logical, intent(in) :: show
        type(status_type), intent(out) :: status
        type(handle_type), intent(in), optional :: window
        type(handle_type) :: w

        w = selected_window()
        if (present(window)) w = window

        if (.not. window_valid_p(w)) then
            call set_error(status, FM_WRONG_TYPE_ARGUMENT, "expected type(window_type)")
            return
        end if

        status = status_type()
        windows(w%index)%cursor_off_p = .not. show
    end subroutine internal_show_cursor

    !> Value is true if next redisplay will display a cursor in WINDOW.
    !> WINDOW absent means report on the selected window.  Port of
    !> dispnew.c:Finternal_show_cursor_p.
    logical function internal_show_cursor_p(status, window) result(show)
        type(status_type), intent(out) :: status
        type(handle_type), intent(in), optional :: window
        type(handle_type) :: w

        show = .false.
        w = selected_window()
        if (present(window)) w = window

        if (.not. window_valid_p(w)) then
            call set_error(status, FM_WRONG_TYPE_ARGUMENT, "expected type(window_type)")
            return
        end if

        status = status_type()
        show = .not. windows(w%index)%cursor_off_p
    end function internal_show_cursor_p

    !> Register this module's commands.  Port of the defsubr part of
    !> dispnew.c:syms_of_display.
    subroutine syms_of_display()
        commands%redraw_display%run => cmd_redraw_display
    end subroutine syms_of_display

    !> Leading blanks of `r(1:len)'.  Port of dispnew.c:count_blanks.
    integer function count_blanks(r, len) result(i)
        type(glyph_type), intent(in) :: r(:)
        integer, intent(in) :: len

        do i = 0, len - 1
            if (.not. char_glyph_space_p(r(i + 1))) return
        end do

        i = len
    end function count_blanks

    !> Length of the common prefix of `a' and `b'.  Port of
    !> dispnew.c:count_match.
    integer function count_match(a, b) result(n)
        type(glyph_type), intent(in) :: a(:), b(:)

        n = 0

        do while (n < size(a) .and. n < size(b))
            if (.not. glyph_equal_p(a(n + 1), b(n + 1))) exit
            n = n + 1
        end do
    end function count_match

    !> Write `body(first+1:first+n)' at column `first' of row `vpos'.
    !> Not a port.
    subroutine write_at(f, vpos, first, body, n)
        type(frame_type), intent(inout) :: f
        integer, intent(in) :: vpos, first, n
        type(glyph_type), intent(in) :: body(:)

        call tty_cursor_to(f%tty, vpos, first)

        if (first > 0) then
            call tty_write_glyphs(f%tty, body(first + 1:first + n), f%faces, body(first))
        else
            call tty_write_glyphs(f%tty, body(first + 1:first + n), f%faces)
        end if
    end subroutine write_at

    !> Make row `vpos' of the screen show the desired row, sending as
    !> little as possible.  Port of dispnew.c:write_row.
    subroutine write_row(f, vpos)
        type(frame_type), intent(inout) :: f
        integer, intent(in) :: vpos
        type(glyph_type), allocatable :: obody(:), nbody(:)
        integer :: osp, nsp, begmatch, endmatch, olen, nlen, tem, i, j, op1, np1, &
                   op2, out, del, cols
        logical :: must_write_whole_line_p, write_spaces_p, colored_spaces_p

        cols = f%tty%wcm%cm_cols
        write_spaces_p = f%tty%must_write_spaces
        colored_spaces_p = f%faces(0)%background /= FACE_TTY_DEFAULT_BG_COLOR
        if (colored_spaces_p) write_spaces_p = .true.
        ! What the screen shows.
        must_write_whole_line_p = .not. f%current_matrix%rows(vpos)%enabled_p
        olen = 0
        allocate (obody(cols))

        if (.not. must_write_whole_line_p) then
            obody = f%current_matrix%rows(vpos)%glyphs
            olen = f%current_matrix%rows(vpos)%used

            if (.not. write_spaces_p) then
                do while (olen > 0)
                    if (.not. char_glyph_space_p(obody(olen))) exit
                    olen = olen - 1
                end do
            end if
        end if

        f%current_matrix%rows(vpos)%enabled_p = .true.
        f%current_matrix%rows(vpos)%used = f%desired_matrix%rows(vpos)%used
        nlen = 0
        allocate (nbody(cols))

        if (f%desired_matrix%rows(vpos)%enabled_p) then
            nbody = f%desired_matrix%rows(vpos)%glyphs
            nlen = f%desired_matrix%rows(vpos)%used

            if (.not. write_spaces_p) then
                do while (nlen > 0)
                    if (.not. char_glyph_space_p(nbody(nlen))) exit
                    nlen = nlen - 1
                end do
            end if

            if (must_write_whole_line_p) then
                if (nlen > 0) call write_at(f, vpos, 0, nbody, nlen)

                if (nlen < cols) then
                    call tty_cursor_to(f%tty, vpos, nlen)
                    call tty_clear_end_of_line(f%tty, cols)
                else
                    call tty_cursor_to(f%tty, vpos, 0)
                end if

                return
            end if

            ! Without character insertion and deletion, rewrite each
            ! differing run.
            if (.not. f%tty%char_ins_del_ok) then
                i = 0

                do while (i < nlen)
                    if (i >= olen) then
                        j = i + 1
                    else if (.not. glyph_equal_p(nbody(i + 1), obody(i + 1))) then
                        j = i + 1
                    else
                        i = i + 1
                        cycle
                    end if

                    do while (j < nlen)
                        if (j < olen) then
                            if (glyph_equal_p(nbody(j + 1), obody(j + 1)) .and. &
                                .not. nbody(j + 1)%padding) exit
                        end if

                        j = j + 1
                    end do

                    call write_at(f, vpos, i, nbody, j - i)
                    i = j
                end do

                if (olen > nlen) then
                    call tty_cursor_to(f%tty, vpos, nlen)
                    call tty_clear_end_of_line(f%tty, olen)
                end if

                return
            end if

            ! The screen line is blank: just write the new text.
            if (olen == 0) then
                nsp = 0
                if (.not. write_spaces_p) nsp = count_blanks(nbody, nlen)
                if (nlen > nsp) call write_at(f, vpos, nsp, nbody, nlen - nsp)
                return
            end if

            ! Leading blanks, then the common prefix after them.
            osp = count_blanks(obody, olen)
            nsp = 0
            if (.not. colored_spaces_p) nsp = count_blanks(nbody, nlen)
            begmatch = count_match(obody(osp + 1:olen), nbody(nsp + 1:nlen))

            ! Trailing blanks in the new line count as matching an old line
            ! that ends here.
            if (.not. write_spaces_p .and. osp + begmatch == olen) then
                np1 = nsp

                do while (np1 + begmatch < nlen)
                    if (.not. char_glyph_space_p(nbody(np1 + begmatch + 1))) exit
                    begmatch = begmatch + 1
                end do
            end if

            ! Shifting blanks only pays if something matches after them.
            if (begmatch == 0 .and. osp /= nsp) then
                osp = min(osp, nsp)
                nsp = osp
            end if

            ! The common suffix.
            op1 = olen
            np1 = nlen
            op2 = op1 + begmatch - min(olen - osp, nlen - nsp)

            do while (op1 > op2)
                if (.not. glyph_equal_p(obody(op1), nbody(np1))) exit
                op1 = op1 - 1
                np1 = np1 - 1
            end do

            endmatch = olen - op1
            ! Keep the suffix only if moving it is cheaper than rewriting.
            tem = (nlen - nsp) - (olen - osp)

            if (endmatch /= 0 .and. tem /= 0) then
                if (endmatch <= ins_del_cost(f, tem)) endmatch = 0
            end if

            if (nsp /= osp) then
                if (begmatch + endmatch <= ins_del_cost(f, nsp - osp)) then
                    begmatch = 0
                    endmatch = 0
                    osp = min(osp, nsp)
                    nsp = osp
                end if
            end if

            ! Line up the leading blanks.
            if (osp > nsp) then
                call tty_cursor_to(f%tty, vpos, nsp)
                call tty_delete_glyphs(f%tty, osp - nsp)
            else if (nsp > osp) then
                ! Make room so the suffix is not pushed off the line.
                if (endmatch /= 0 .and. nlen < olen + nsp - osp) then
                    call tty_cursor_to(f%tty, vpos, nlen - endmatch + osp - nsp)
                    call tty_delete_glyphs(f%tty, olen + nsp - osp - nlen)
                    olen = nlen - (nsp - osp)
                end if

                call tty_cursor_to(f%tty, vpos, osp)
                call tty_insert_glyphs(f%tty, f%faces, len=nsp - osp)
            end if

            olen = olen + nsp - osp
            ! The middle.
            tem = nsp + begmatch + endmatch

            if (nlen /= tem .or. olen /= tem) then
                if (endmatch == 0 .or. nlen == olen) then
                    ! Writing a whole line leaves nothing to clear.
                    if (nlen == cols) olen = 0
                    if (nlen - tem > 0) call write_at(f, vpos, nsp + begmatch, nbody, nlen - tem)
                else if (nlen > olen) then
                    out = olen - tem

                    do while (out > 0)
                        if (.not. nbody(nsp + begmatch + out + 1)%padding) exit
                        out = out - 1
                    end do

                    call write_at(f, vpos, nsp + begmatch, nbody, out)
                    del = olen - tem - out
                    if (del > 0) call tty_delete_glyphs(f%tty, del)
                    call tty_insert_glyphs(f%tty, f%faces, &
                                           nbody(nsp + begmatch + out + 1:nsp + begmatch + out + nlen - olen + del))
                    olen = nlen
                else if (olen > nlen) then
                    call write_at(f, vpos, nsp + begmatch, nbody, nlen - tem)
                    call tty_delete_glyphs(f%tty, olen - nlen)
                    olen = nlen
                end if
            end if
        end if

        ! just_erase:
        if (olen > nlen) then
            call tty_cursor_to(f%tty, vpos, nlen)
            call tty_clear_end_of_line(f%tty, olen)
        end if
    end subroutine write_row

    !> dispnew.c's char_ins_del_cost (f)[k].
    !> Port of dispnew.c:char_ins_del_cost.
    integer function ins_del_cost(f, k)
        type(frame_type), intent(in) :: f
        integer, intent(in) :: k

        ins_del_cost = 9999
        if (.not. allocated(f%tty%char_ins_del_cost)) return
        if (k < lbound(f%tty%char_ins_del_cost, 1) .or. k > ubound(f%tty%char_ins_del_cost, 1)) return
        ins_del_cost = f%tty%char_ins_del_cost(k)
    end function ins_del_cost

    !> Write the changed rows, the last (echo area) row first.  Port of
    !> dispnew.c:write_matrix (without line scrolling).
    subroutine write_matrix(f)
        type(frame_type), intent(inout) :: f
        integer :: i, last_row

        last_row = f%desired_matrix%nrows - 1
        if (last_row < 0) return
        if (f%desired_matrix%rows(last_row)%enabled_p) call write_row(f, last_row)

        do i = 0, last_row - 1
            if (f%desired_matrix%rows(i)%enabled_p) call write_row(f, i)
        end do
    end subroutine write_matrix

    !> Leave the cursor where the frame wants it.  Port of
    !> dispnew.c:tty_set_cursor (the selected-window case).
    subroutine tty_set_cursor(f)
        type(frame_type), intent(inout) :: f

        if (f%cursor_vpos >= 0 .and. f%cursor_vpos < f%tty%wcm%cm_rows .and. &
            f%cursor_hpos >= 0) then
            call tty_cursor_to(f%tty, f%cursor_vpos, min(f%cursor_hpos, f%tty%wcm%cm_cols - 1))
        end if
    end subroutine tty_set_cursor

    !> Make the screen show the desired matrix, then flush.  Port of
    !> dispnew.c:update_frame, dispnew.c:combine_updates and
    !> dispnew.c:combine_updates_for_frame for a root terminal frame (no
    !> child frames), with dispnew.c:flush_terminal.
    subroutine update_frame(f, status)
        type(frame_type), intent(inout) :: f
        type(status_type), intent(out) :: status

        status = status_type()

        if (frame_window_p(f)) then
            ! The windows were drawn as redisplay laid them out.
            call f%rif%flush_display()
            return
        end if

        if (.not. f%glyphs_initialized_p) call adjust_frame_glyphs(f)
        if (f%garbaged) call redraw_frame(f)
        call write_matrix(f)
        call make_matrix_current(f)
        call tty_update_end(f%tty, selected_cursor_off_p())
        call tty_set_cursor(f)
        call tty_flush(f%tty, status)
        call clear_desired_matrices(f)
    end subroutine update_frame

    !> Draw row `row' of a window on window-system frame `f', the window's
    !> text area having its top left corner at frame pixel (left, top) and
    !> its right edge at `right': its glyphs, then clear to the right edge
    !> (a mode line row spans the whole window).  Port of
    !> dispnew.c:update_window_line with update_text_area for a row that
    !> must be drawn whole (its current row unknown).
    subroutine update_window_line(f, row, left, top, right)
        type(frame_type), intent(inout) :: f
        type(glyph_row_type), intent(in) :: row
        integer, intent(in) :: left, top, right
        integer :: x, k

        x = left + row%x
        if (row%used > 0) call f%rif%write_glyphs(row, 1, row%used, x, top + row%y, right)

        do k = 1, row%used
            x = x + row%glyphs(k)%pixel_width
        end do

        if (x < right) call f%rif%clear_end_of_line(x, top + row%y, row%height, right)
    end subroutine update_window_line

    !> Ring the bell of the selected frame's terminal; nothing without
    !> one.  The terminal's ring_bell_hook (term.c:tty_ring_bell).
    !> Not a port.
    subroutine frame_ring_bell()
        if (selected_frame%tty%output < 0) return
        call tty_ring_bell(selected_frame%tty)
    end subroutine frame_ring_bell

    !> Beep, or flash the screen.
    !> Also, unless an argument is given,
    !> terminate any keyboard macro currently executing.  Port of
    !> dispnew.c:Fding.  The macro is terminated by the error set in
    !> `status'; without `status' the bell just rings.
    subroutine ding(arg, status)
        logical, intent(in), optional :: arg
        type(status_type), intent(inout), optional :: status
        logical :: have_arg

        have_arg = .false.
        if (present(arg)) have_arg = arg

        if (have_arg .or. .not. present(status)) then
            call ring_bell()
        else
            call bitch_at_user(status)
        end if
    end subroutine ding

    ! ------------------------------------------------------------------
    ! Buffer menus
    ! ------------------------------------------------------------------

    !> Return true if the frame and buffer state appears to have changed.
    !> STATE is a state object that will be updated to contain all buffers,
    !> aside from buffers whose names start with space, along with the
    !> buffers' read-only and modified flags.  This allows a fast check to
    !> see whether buffer menus might need to be recomputed.  If this
    !> function returns true, it updates STATE to reflect the current
    !> state.  Port of dispnew.c:Fframe_or_buffer_changed_p (VARIABLE a
    !> state object; there is one frame, whose name does not change).
    logical function frame_or_buffer_changed_p(state) result(changed)
        use buffer_fm, only: buffers, buffer_list, buffer_modified_p
        use handle_fm, only: handle_type
        type(frame_buffer_state_type), intent(inout) :: state
        type(handle_type), allocatable :: bufs(:)
        integer(int64), allocatable :: serials(:)
        logical, allocatable :: read_only(:), modified(:)
        type(status_type) :: st
        integer :: k, n

        bufs = buffer_list()
        allocate (serials(size(bufs)), read_only(size(bufs)), modified(size(bufs)))
        n = 0

        do k = size(bufs), 1, -1
            ! Ignore buffers that aren't included in buffer lists.
            if (len(buffers(bufs(k)%index)%name) > 0) then
                if (buffers(bufs(k)%index)%name(1:1) == " ") cycle
            end if

            n = n + 1
            serials(n) = buffers(bufs(k)%index)%serial
            read_only(n) = buffers(bufs(k)%index)%read_only
            call buffer_modified_p(bufs(k), modified(n), st)
        end do

        changed = .true.

        if (state%valid) then
            if (size(state%serials) == n) then
                changed = any(state%serials /= serials(1:n)) .or. any(state%read_only .neqv. read_only(1:n)) &
                          .or. any(state%modified .neqv. modified(1:n))
            end if
        end if

        if (.not. changed) return
        state%valid = .true.
        state%serials = serials(1:n)
        state%read_only = read_only(1:n)
        state%modified = modified(1:n)
    end function frame_or_buffer_changed_p
end module dispnew_fm
