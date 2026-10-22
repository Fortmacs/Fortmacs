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

!> Redisplay fills a frame's desired matrix; `update_frame` sends the
!> terminal the least output that turns the screen (the current matrix)
!> into it, row by row (`write_row`: keep matching prefixes and suffixes,
!> insert or delete characters where that is cheaper than rewriting).
!>
!> Not ported yet: window matrices (there are no windows; redisplay
!> writes frame rows), line insertion/deletion optimization (scroll.c),
!> child frames, menus, pausing for input, frame size changes.
module dispnew_fm
    use frame_fm, only: frame_type, selected_frame
    use dispextern_fm, only: glyph_type, glyph_matrix_type, &
                             glyph_equal_p, char_glyph_space_p, &
                             FACE_TTY_DEFAULT_FG_COLOR, FACE_TTY_DEFAULT_BG_COLOR
    use termchar_fm, only: tty_flush
    use term_fm, only: tty_cursor_to, tty_write_glyphs, tty_insert_glyphs, &
                       tty_delete_glyphs, tty_clear_end_of_line, tty_clear_frame, &
                       tty_update_end, tty_ring_bell
    use error_fm, only: status_type
    implicit none(type, external)
    private

    public :: adjust_frame_glyphs, clear_current_matrices, clear_desired_matrices, &
              redraw_frame, update_frame, write_row, make_matrix_current, &
              set_desired_row, ding

contains

    ! ------------------------------------------------------------------
    ! Matrices
    ! ------------------------------------------------------------------

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

    !> Give `f` matrices of its terminal's size (contents unknown, so the
    !> next update redraws everything), and a default face if it has none.
    !> Port of the frame part of dispnew.c:adjust_frame_glyphs.
    subroutine adjust_frame_glyphs(f)
        type(frame_type), intent(inout) :: f
        integer :: nrows, ncols
        nrows = f%tty%wcm%cm_rows
        ncols = f%tty%wcm%cm_cols
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

    !> Port of dispnew.c:clear_current_matrices.
    subroutine clear_current_matrices(f)
        type(frame_type), intent(inout) :: f
        integer :: r
        do r = 0, f%current_matrix%nrows - 1
            f%current_matrix%rows(r)%enabled_p = .false.
            f%current_matrix%rows(r)%used = 0
        end do
    end subroutine clear_current_matrices

    !> Port of dispnew.c:clear_desired_matrices.
    subroutine clear_desired_matrices(f)
        type(frame_type), intent(inout) :: f
        integer :: r
        do r = 0, f%desired_matrix%nrows - 1
            f%desired_matrix%rows(r)%enabled_p = .false.
            f%desired_matrix%rows(r)%used = 0
        end do
    end subroutine clear_desired_matrices

    !> Make row `vpos` of the desired matrix show `glyphs` (at most a
    !> line; the rest of the line is blank).  What redisplay does through
    !> its iterator; not a port.
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

    !> Clear the screen and forget what it showed.  Port of
    !> dispnew.c:redraw_frame.
    subroutine redraw_frame(f)
        type(frame_type), intent(inout) :: f
        call tty_clear_frame(f%tty)
        call clear_current_matrices(f)
        call tty_update_end(f%tty, f%cursor_off_p)
        f%garbaged = .false.
    end subroutine redraw_frame

    !> Leading blanks of `r(1:len)`.  Port of dispnew.c:count_blanks.
    integer function count_blanks(r, len) result(i)
        type(glyph_type), intent(in) :: r(:)
        integer, intent(in) :: len
        do i = 0, len - 1
            if (.not. char_glyph_space_p(r(i + 1))) return
        end do
        i = len
    end function count_blanks

    !> Length of the common prefix of `a` and `b`.  Port of
    !> dispnew.c:count_match.
    integer function count_match(a, b) result(n)
        type(glyph_type), intent(in) :: a(:), b(:)
        n = 0
        do while (n < size(a) .and. n < size(b))
            if (.not. glyph_equal_p(a(n + 1), b(n + 1))) exit
            n = n + 1
        end do
    end function count_match

    !> Write `body(first+1:first+n)` at column `first` of row `vpos`.
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

    !> Make row `vpos` of the screen show the desired row, sending as
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
    !> dispnew.c:combine_updates_for_frame for a root terminal frame
    !> (update_frame's terminal path).
    subroutine update_frame(f, status)
        type(frame_type), intent(inout) :: f
        type(status_type), intent(out) :: status
        if (.not. f%glyphs_initialized_p) call adjust_frame_glyphs(f)
        if (f%garbaged) call redraw_frame(f)
        call write_matrix(f)
        call make_matrix_current(f)
        call tty_update_end(f%tty, f%cursor_off_p)
        call tty_set_cursor(f)
        call tty_flush(f%tty, status)
        call clear_desired_matrices(f)
    end subroutine update_frame

    !> Beep, or flash the screen.  Port of dispnew.c:Fding (with
    !> bitch_at_user; no keyboard macro termination), for the selected
    !> frame's terminal; nothing without one.
    subroutine ding()
        if (selected_frame%tty%output < 0) return
        call tty_ring_bell(selected_frame%tty)
    end subroutine ding
end module dispnew_fm
