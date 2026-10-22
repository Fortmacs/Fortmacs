! cm_fm.f90 -- cursor motion: the cheapest way to move the cursor.
! Ported from GNU Emacs 31.1 src/cm.c and the motion macros of src/cm.h.
!
! Copyright (C) 1985, 1995, 2001-2026 Free Software Foundation, Inc.
! (cm.c is based primarily on public domain code written by Chris Torek.)
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

!> Positions are 0-based rows and columns, as upstream's terminal code.
!> Costs are output characters (upstream counts them through tputs; with
!> padding dropped that is the string length).
module cm_fm
    use, intrinsic :: iso_fortran_env, only: int64
    use terminfo_fm, only: tgoto, delete_padding
    use termchar_fm, only: tty_display_info, cm_type, BIG, tty_output, tty_put_bytes
    implicit none(type, external)
    private

    public :: cmcostinit, cmgoto, cmcheckmagic, wcm_clear, wcm_init
    public :: cmat, cmplus, losecursor

    !> cm.c:cmgoto's choice of starting point.
    integer, parameter :: USEREL = 0, USEHOME = 1, USELL = 2, USECR = 3

    !> cm.c:tty_cursor_movement_use_TAB_BS (a Lisp variable upstream).
    logical, public :: tty_cursor_movement_use_tab_bs = .false.

contains

    !> The output cost of capability `cap`; BIG if absent.  cm.c's COST.
    integer function cost_of(cap) result(c)
        character(len=:), allocatable, intent(in) :: cap

        c = BIG
        if (allocated(cap)) c = len(delete_padding(cap))
    end function cost_of

    !> The minimum cost of parameterized motion `cap`.  cm.c's CMCOST.
    integer function cmcost(cap) result(c)
        character(len=:), allocatable, intent(in) :: cap

        c = BIG
        if (allocated(cap)) c = len(delete_padding(tgoto(cap, 0_int64, 0_int64)))
    end function cmcost

    !> (Re)initialize the cost factors.  Port of cm.c:cmcostinit.
    subroutine cmcostinit(tty)
        type(tty_display_info), intent(inout) :: tty

        associate (w => tty%wcm)
            w%cc_up = cost_of(w%cm_up)
            w%cc_down = cost_of(w%cm_down)
            w%cc_left = cost_of(w%cm_left)
            w%cc_right = cost_of(w%cm_right)
            w%cc_home = cost_of(w%cm_home)
            w%cc_cr = cost_of(w%cm_cr)
            w%cc_ll = cost_of(w%cm_ll)
            w%cc_tab = BIG
            if (w%cm_tabwidth /= 0) w%cc_tab = cost_of(w%cm_tab)
            w%cc_abs = cmcost(w%cm_abs)
            w%cc_habs = cmcost(w%cm_habs)
            w%cc_vabs = cmcost(w%cm_vabs)
        end associate
    end subroutine cmcostinit

    !> Assume the cursor is at `row`, `col`.  cm.h:cmat.
    subroutine cmat(tty, row, col)
        type(tty_display_info), intent(inout) :: tty
        integer, intent(in) :: row, col

        tty%wcm%cm_cury = row
        tty%wcm%cm_curx = col
    end subroutine cmat

    !> Forget where the cursor is.  cm.h:losecursor.
    subroutine losecursor(tty)
        type(tty_display_info), intent(inout) :: tty

        tty%wcm%cm_curx = -1
        tty%wcm%cm_cury = -1
    end subroutine losecursor

    !> Account for `n` columns written.  cm.h:cmplus.
    subroutine cmplus(tty, n)
        type(tty_display_info), intent(inout) :: tty
        integer, intent(in) :: n

        associate (w => tty%wcm)
            w%cm_curx = w%cm_curx + n

            if (w%cm_curx >= w%cm_cols .and. .not. w%cm_magicwrap) then
                if (w%cm_losewrap) then
                    call losecursor(tty)
                else if (w%cm_autowrap) then
                    w%cm_curx = 0
                    w%cm_cury = w%cm_cury + 1
                else
                    w%cm_curx = w%cm_curx - 1
                end if
            end if
        end associate
    end subroutine cmplus

    !> After writing the last column on a terminal with xn (magic wrap),
    !> move to a known place with CR LF.  Port of cm.c:cmcheckmagic; where
    !> upstream aborts (no magic wrap, or the last row) the position is
    !> forgotten instead.
    subroutine cmcheckmagic(tty)
        type(tty_display_info), intent(inout) :: tty

        if (tty%wcm%cm_curx /= tty%wcm%cm_cols) return

        if (.not. tty%wcm%cm_magicwrap .or. tty%wcm%cm_cury >= tty%wcm%cm_rows - 1) then
            call losecursor(tty)
            return
        end if

        call tty_put_bytes(tty, achar(13)//achar(10))
        tty%wcm%cm_curx = 0
        tty%wcm%cm_cury = tty%wcm%cm_cury + 1
    end subroutine cmcheckmagic

    !> Output `cap` `n` times.
    subroutine repeat_output(tty, cap, n)
        type(tty_display_info), intent(inout) :: tty
        character(len=:), allocatable, intent(in) :: cap
        integer, intent(in) :: n
        character(len=:), allocatable :: s
        integer :: k

        s = cap

        do k = 1, n
            call tty_output(tty, s)
        end do
    end subroutine repeat_output

    !> The cost of moving from (srcy, srcx) to (dsty, dstx) with relative
    !> motions and tabs; with `doit`, also make the motion.  Port of
    !> cm.c:calccost.
    integer function calccost(tty, srcy, srcx_in, dsty, dstx, doit) result(totalcost)
        type(tty_display_info), intent(inout) :: tty
        integer, intent(in) :: srcy, srcx_in, dsty, dstx
        logical, intent(in) :: doit
        integer :: deltay, deltax, c, ntabs, n2tabs, tabx, tab2x, tabcost, srcx
        character(len=:), allocatable :: p

        totalcost = BIG
        srcx = srcx_in

        associate (w => tty%wcm)
            ! Just wrapped on a terminal with xn: don't trust the position.
            if (w%cm_curx == w%cm_cols) return
            totalcost = 0
            deltay = dsty - srcy

            if (deltay /= 0) then
                if (deltay < 0) then
                    if (allocated(w%cm_up)) p = w%cm_up
                    c = w%cc_up
                    deltay = -deltay
                else
                    if (allocated(w%cm_down)) p = w%cm_down
                    c = w%cc_down
                end if

                if (c == BIG) then
                    totalcost = BIG
                    return
                end if

                totalcost = c*deltay
                if (doit) call repeat_output(tty, p, deltay)
            end if

            deltax = dstx - srcx
            if (deltax == 0) return

            if (deltax > 0 .and. w%cc_tab < BIG .and. w%cm_usetabs) then
                ! Tabs: ntabs is the number towards but not past dstx.
                ntabs = (deltax + mod(srcx, w%cm_tabwidth))/w%cm_tabwidth
                tabx = (srcx/w%cm_tabwidth + ntabs)*w%cm_tabwidth
                c = BIG

                if (tty_cursor_movement_use_tab_bs) then
                    ! One more tab, then back up, unless past the edge.
                    n2tabs = ntabs + 1
                    tab2x = tabx + w%cm_tabwidth
                    if (tab2x >= w%cm_cols) n2tabs = 0
                    if (n2tabs /= 0) c = n2tabs*w%cc_tab + (tab2x - dstx)*w%cc_left
                end if

                tabcost = BIG
                if (ntabs /= 0) tabcost = ntabs*w%cc_tab + (dstx - tabx)*w%cc_right

                if (tty_cursor_movement_use_tab_bs .and. c < tabcost) then
                    ntabs = n2tabs
                    tabcost = c
                    tabx = tab2x
                end if

                if (tabcost < BIG .and. tabcost < deltax*w%cc_right) then
                    totalcost = totalcost + tabcost
                    if (doit) call repeat_output(tty, w%cm_tab, ntabs)
                    srcx = tabx
                end if

                deltax = dstx - srcx
                if (deltax == 0) return
            end if

            if (deltax > 0) then
                if (allocated(w%cm_right)) p = w%cm_right
                c = w%cc_right
            else
                if (allocated(w%cm_left)) p = w%cm_left
                c = w%cc_left
                deltax = -deltax
            end if

            if (c == BIG) then
                totalcost = BIG
                return
            end if

            totalcost = totalcost + c*deltax
            if (doit) call repeat_output(tty, p, deltax)
        end associate
    end function calccost

    !> Move the cursor to (`row`, `col`) the cheapest way.  Port of
    !> cm.c:cmgoto.
    subroutine cmgoto(tty, row, col)
        type(tty_display_info), intent(inout) :: tty
        integer, intent(in) :: row, col
        integer :: homecost, crcost, llcost, relcost, directcost, use, ignored, which
        character(len=:), allocatable :: p

        if (row == tty%wcm%cm_cury .and. col == tty%wcm%cm_curx) return
        use = USEREL
        ! which: 1 cm_abs, 2 cm_habs, 3 cm_vabs.
        which = 1

        if (tty%wcm%cm_cury >= 0 .and. tty%wcm%cm_curx >= 0) then
            ! Quick ways to the upper-left, bottom-left, start-of-line, or
            ! from where we are: pick the cheapest.
            relcost = calccost(tty, tty%wcm%cm_cury, tty%wcm%cm_curx, row, col, .false.)
            homecost = tty%wcm%cc_home
            if (homecost < BIG) homecost = homecost + calccost(tty, 0, 0, row, col, .false.)

            if (homecost < relcost) then
                relcost = homecost
                use = USEHOME
            end if

            llcost = tty%wcm%cc_ll
            if (llcost < BIG) llcost = llcost + &
                                       calccost(tty, tty%wcm%cm_rows - 1, 0, row, col, .false.)

            if (llcost < relcost) then
                relcost = llcost
                use = USELL
            end if

            crcost = tty%wcm%cc_cr

            if (crcost < BIG) then
                if (tty%wcm%cm_autolf) then
                    if (tty%wcm%cm_cury + 1 >= tty%wcm%cm_rows) then
                        crcost = BIG
                    else
                        crcost = crcost + calccost(tty, tty%wcm%cm_cury + 1, 0, row, col, .false.)
                    end if
                else
                    crcost = crcost + calccost(tty, tty%wcm%cm_cury, 0, row, col, .false.)
                end if
            end if

            if (crcost < relcost) then
                relcost = crcost
                use = USECR
            end if

            directcost = tty%wcm%cc_abs

            if (row == tty%wcm%cm_cury .and. tty%wcm%cc_habs < BIG) then
                directcost = tty%wcm%cc_habs
                which = 2
            else if (col == tty%wcm%cm_curx .and. tty%wcm%cc_vabs < BIG) then
                directcost = tty%wcm%cc_vabs
                which = 3
            end if
        else
            directcost = 0
            relcost = 100000
        end if

        ! On a tie, moving directly looks nicer.
        if (directcost <= relcost) then
            select case (which)
            case (2)
                p = tgoto(tty%wcm%cm_habs, int(row, int64), int(col, int64))
            case (3)
                p = tgoto(tty%wcm%cm_vabs, int(col, int64), int(row, int64))
            case default
                if (allocated(tty%wcm%cm_abs)) p = tgoto(tty%wcm%cm_abs, int(col, int64), int(row, int64))
            end select

            if (allocated(p)) then
                if (len(delete_padding(p)) <= relcost) then
                    call tty_output(tty, p)
                    call cmat(tty, row, col)
                    return
                end if
            end if
        end if

        select case (use)
        case (USEHOME)
            call tty_output(tty, tty%wcm%cm_home)
            call cmat(tty, 0, 0)
        case (USELL)
            call tty_output(tty, tty%wcm%cm_ll)
            call cmat(tty, tty%wcm%cm_rows - 1, 0)
        case (USECR)
            call tty_output(tty, tty%wcm%cm_cr)
            if (tty%wcm%cm_autolf) tty%wcm%cm_cury = tty%wcm%cm_cury + 1
            tty%wcm%cm_curx = 0
        case default
            ! Nothing to do for other values.
        end select

        ignored = calccost(tty, tty%wcm%cm_cury, tty%wcm%cm_curx, row, col, .true.)
        call cmat(tty, row, col)
    end subroutine cmgoto

    !> Forget all cursor-motion capabilities.  Port of cm.c:Wcm_clear.
    subroutine wcm_clear(tty)
        type(tty_display_info), intent(inout) :: tty

        tty%wcm = cm_type()
    end subroutine wcm_clear

    !> 0 if the terminal can position the cursor, -1 if it cannot, -2 if
    !> its size is unknown.  Port of cm.c:Wcm_init.
    integer function wcm_init(tty) result(r)
        type(tty_display_info), intent(in) :: tty

        r = 0
        if (allocated(tty%wcm%cm_abs)) return

        ! Need up and left, and without absolute motion down and right.
        if (.not. allocated(tty%wcm%cm_up) .or. .not. allocated(tty%wcm%cm_left)) then
            r = -1
        else if (.not. allocated(tty%wcm%cm_down) .or. .not. allocated(tty%wcm%cm_right)) then
            r = -1
        else if (tty%wcm%cm_rows <= 0 .or. tty%wcm%cm_cols <= 0) then
            r = -2
        end if
    end function wcm_init
end module cm_fm
