! cursor_motion_mod.f90 -- cursor motion: the cheapest way to move the cursor.
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
module cursor_motion_mod
    use, intrinsic :: iso_fortran_env, only: int64
    use terminfo_mod, only: tgoto, delete_padding
    use terminal_state_mod, only: terminal_display_info_type, cursor_motion_type, BIG, terminal_output, terminal_put_bytes
    implicit none(type, external)
    private

    public :: cmcostinit, cmgoto, cmcheckmagic, wcm_clear, wcm_initialize
    public :: cmat, cmplus, losecursor

    !> cm.c:cmgoto's choice of starting point.
    integer, parameter :: USEREL = 0, USEHOME = 1, USELL = 2, USECR = 3

    !> cm.c:tty_cursor_movement_use_TAB_BS (a Lisp variable upstream).
    logical, public :: terminal_cursor_movement_use_tab_bs = .false.

contains

    !> The output cost of capability `cap'; BIG if absent.  Port of cm.c:COST.
    integer function cost_of(cap) result(c)
        character(len=:), allocatable, intent(in) :: cap

        c = BIG
        if (allocated(cap)) c = len(delete_padding(cap))
    end function cost_of

    !> The minimum cost of parameterized motion `cap'.  Port of cm.c:CMCOST.
    integer function cmcost(cap) result(c)
        character(len=:), allocatable, intent(in) :: cap

        c = BIG
        if (allocated(cap)) c = len(delete_padding(tgoto(cap, 0_int64, 0_int64)))
    end function cmcost

    !> (Re)initialize the cost factors.  Port of cm.c:cmcostinit.
    subroutine cmcostinit(terminal)
        type(terminal_display_info_type), intent(inout) :: terminal

        associate (w => terminal%wcm)
            w%cc_up = cost_of(w%cursor_motion_up)
            w%cc_down = cost_of(w%cursor_motion_down)
            w%cc_left = cost_of(w%cursor_motion_left)
            w%cc_right = cost_of(w%cursor_motion_right)
            w%cc_home = cost_of(w%cursor_motion_home)
            w%cc_cr = cost_of(w%cursor_motion_cr)
            w%cc_ll = cost_of(w%cursor_motion_ll)
            w%cc_tab = BIG
            if (w%cursor_motion_tabwidth /= 0) w%cc_tab = cost_of(w%cursor_motion_tab)
            w%cc_absolute = cmcost(w%cursor_motion_absolute)
            w%cc_habs = cmcost(w%cursor_motion_habs)
            w%cc_vabs = cmcost(w%cursor_motion_vabs)
        end associate
    end subroutine cmcostinit

    !> Assume the cursor is at `row', `column'.  Port of cm.h:cmat.
    subroutine cmat(terminal, row, column)
        type(terminal_display_info_type), intent(inout) :: terminal
        integer, intent(in) :: row, column

        terminal%wcm%cursor_motion_cury = row
        terminal%wcm%cursor_motion_curx = column
    end subroutine cmat

    !> Forget where the cursor is.  Port of cm.h:losecursor.
    subroutine losecursor(terminal)
        type(terminal_display_info_type), intent(inout) :: terminal

        terminal%wcm%cursor_motion_curx = -1
        terminal%wcm%cursor_motion_cury = -1
    end subroutine losecursor

    !> Account for `n' columns written.  Port of cm.h:cmplus.
    subroutine cmplus(terminal, n)
        type(terminal_display_info_type), intent(inout) :: terminal
        integer, intent(in) :: n

        associate (w => terminal%wcm)
            w%cursor_motion_curx = w%cursor_motion_curx + n

            if (w%cursor_motion_curx >= w%cursor_motion_columns .and. .not. w%cursor_motion_magicwrap) then
                if (w%cursor_motion_losewrap) then
                    call losecursor(terminal)
                else if (w%cursor_motion_autowrap) then
                    w%cursor_motion_curx = 0
                    w%cursor_motion_cury = w%cursor_motion_cury + 1
                else
                    w%cursor_motion_curx = w%cursor_motion_curx - 1
                end if
            end if
        end associate
    end subroutine cmplus

    !> After writing the last column on a terminal with xn (magic wrap),
    !> move to a known place with CR LF.  Port of cm.c:cmcheckmagic; where
    !> upstream aborts (no magic wrap, or the last row) the position is
    !> forgotten instead.
    subroutine cmcheckmagic(terminal)
        type(terminal_display_info_type), intent(inout) :: terminal

        if (terminal%wcm%cursor_motion_curx /= terminal%wcm%cursor_motion_columns) return

        if (.not. terminal%wcm%cursor_motion_magicwrap .or. terminal%wcm%cursor_motion_cury >= terminal%wcm%cursor_motion_rows &
            - 1) then
            call losecursor(terminal)
            return
        end if

        call terminal_put_bytes(terminal, achar(13)//achar(10))
        terminal%wcm%cursor_motion_curx = 0
        terminal%wcm%cursor_motion_cury = terminal%wcm%cursor_motion_cury + 1
    end subroutine cmcheckmagic

    !> Output `cap' `n' times.  Not a port.
    subroutine repeat_output(terminal, cap, n)
        type(terminal_display_info_type), intent(inout) :: terminal
        character(len=:), allocatable, intent(in) :: cap
        integer, intent(in) :: n
        character(len=:), allocatable :: s
        integer :: k

        s = cap

        do k = 1, n
            call terminal_output(terminal, s)
        end do
    end subroutine repeat_output

    !> The cost of moving from (srcy, srcx) to (dsty, dstx) with relative
    !> motions and tabs; with `doit', also make the motion.  Port of
    !> cm.c:calccost.
    integer function calccost(terminal, srcy, srcx_in, dsty, dstx, doit) result(totalcost)
        type(terminal_display_info_type), intent(inout) :: terminal
        integer, intent(in) :: srcy, srcx_in, dsty, dstx
        logical, intent(in) :: doit
        integer :: deltay, deltax, c, ntabs, n2tabs, tabx, tab2x, tabcost, srcx
        character(len=:), allocatable :: p

        totalcost = BIG
        srcx = srcx_in

        associate (w => terminal%wcm)
            ! Just wrapped on a terminal with xn: don't trust the position.
            if (w%cursor_motion_curx == w%cursor_motion_columns) return
            totalcost = 0
            deltay = dsty - srcy

            if (deltay /= 0) then
                if (deltay < 0) then
                    if (allocated(w%cursor_motion_up)) p = w%cursor_motion_up
                    c = w%cc_up
                    deltay = -deltay
                else
                    if (allocated(w%cursor_motion_down)) p = w%cursor_motion_down
                    c = w%cc_down
                end if

                if (c == BIG) then
                    totalcost = BIG
                    return
                end if

                totalcost = c*deltay
                if (doit) call repeat_output(terminal, p, deltay)
            end if

            deltax = dstx - srcx
            if (deltax == 0) return

            if (deltax > 0 .and. w%cc_tab < BIG .and. w%cursor_motion_usetabs) then
                ! Tabs: ntabs is the number towards but not past dstx.
                ntabs = (deltax + mod(srcx, w%cursor_motion_tabwidth))/w%cursor_motion_tabwidth
                tabx = (srcx/w%cursor_motion_tabwidth + ntabs)*w%cursor_motion_tabwidth
                c = BIG

                if (terminal_cursor_movement_use_tab_bs) then
                    ! One more tab, then back up, unless past the edge.
                    n2tabs = ntabs + 1
                    tab2x = tabx + w%cursor_motion_tabwidth
                    if (tab2x >= w%cursor_motion_columns) n2tabs = 0
                    if (n2tabs /= 0) c = n2tabs*w%cc_tab + (tab2x - dstx)*w%cc_left
                end if

                tabcost = BIG
                if (ntabs /= 0) tabcost = ntabs*w%cc_tab + (dstx - tabx)*w%cc_right

                if (terminal_cursor_movement_use_tab_bs .and. c < tabcost) then
                    ntabs = n2tabs
                    tabcost = c
                    tabx = tab2x
                end if

                if (tabcost < BIG .and. tabcost < deltax*w%cc_right) then
                    totalcost = totalcost + tabcost
                    if (doit) call repeat_output(terminal, w%cursor_motion_tab, ntabs)
                    srcx = tabx
                end if

                deltax = dstx - srcx
                if (deltax == 0) return
            end if

            if (deltax > 0) then
                if (allocated(w%cursor_motion_right)) p = w%cursor_motion_right
                c = w%cc_right
            else
                if (allocated(w%cursor_motion_left)) p = w%cursor_motion_left
                c = w%cc_left
                deltax = -deltax
            end if

            if (c == BIG) then
                totalcost = BIG
                return
            end if

            totalcost = totalcost + c*deltax
            if (doit) call repeat_output(terminal, p, deltax)
        end associate
    end function calccost

    !> Move the cursor to (`row', `column') the cheapest way.  Port of
    !> cm.c:cmgoto.
    subroutine cmgoto(terminal, row, column)
        type(terminal_display_info_type), intent(inout) :: terminal
        integer, intent(in) :: row, column
        integer :: homecost, crcost, llcost, relcost, directcost, use, ignored, which
        character(len=:), allocatable :: p

        if (row == terminal%wcm%cursor_motion_cury .and. column == terminal%wcm%cursor_motion_curx) return
        use = USEREL
        ! which: 1 cursor_motion_absolute, 2 cursor_motion_habs, 3 cursor_motion_vabs.
        which = 1

        if (terminal%wcm%cursor_motion_cury >= 0 .and. terminal%wcm%cursor_motion_curx >= 0) then
            ! Quick ways to the upper-left, bottom-left, start-of-line, or
            ! from where we are: pick the cheapest.
            relcost = calccost(terminal, terminal%wcm%cursor_motion_cury, terminal%wcm%cursor_motion_curx, row, column, .false.)
            homecost = terminal%wcm%cc_home
            if (homecost < BIG) homecost = homecost + calccost(terminal, 0, 0, row, column, .false.)

            if (homecost < relcost) then
                relcost = homecost
                use = USEHOME
            end if

            llcost = terminal%wcm%cc_ll
            if (llcost < BIG) llcost = llcost + &
                                       calccost(terminal, terminal%wcm%cursor_motion_rows - 1, 0, row, column, .false.)

            if (llcost < relcost) then
                relcost = llcost
                use = USELL
            end if

            crcost = terminal%wcm%cc_cr

            if (crcost < BIG) then
                if (terminal%wcm%cursor_motion_autolf) then
                    if (terminal%wcm%cursor_motion_cury + 1 >= terminal%wcm%cursor_motion_rows) then
                        crcost = BIG
                    else
                        crcost = crcost + calccost(terminal, terminal%wcm%cursor_motion_cury + 1, 0, row, column, .false.)
                    end if
                else
                    crcost = crcost + calccost(terminal, terminal%wcm%cursor_motion_cury, 0, row, column, .false.)
                end if
            end if

            if (crcost < relcost) then
                relcost = crcost
                use = USECR
            end if

            directcost = terminal%wcm%cc_absolute

            if (row == terminal%wcm%cursor_motion_cury .and. terminal%wcm%cc_habs < BIG) then
                directcost = terminal%wcm%cc_habs
                which = 2
            else if (column == terminal%wcm%cursor_motion_curx .and. terminal%wcm%cc_vabs < BIG) then
                directcost = terminal%wcm%cc_vabs
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
                p = tgoto(terminal%wcm%cursor_motion_habs, int(row, int64), int(column, int64))
            case (3)
                p = tgoto(terminal%wcm%cursor_motion_vabs, int(column, int64), int(row, int64))
            case default
                if (allocated(terminal%wcm%cursor_motion_absolute)) p = tgoto(terminal%wcm%cursor_motion_absolute, int(column, &
                                                                                                            int64), int(row, int64))
            end select

            if (allocated(p)) then
                if (len(delete_padding(p)) <= relcost) then
                    call terminal_output(terminal, p)
                    call cmat(terminal, row, column)
                    return
                end if
            end if
        end if

        select case (use)
        case (USEHOME)
            call terminal_output(terminal, terminal%wcm%cursor_motion_home)
            call cmat(terminal, 0, 0)
        case (USELL)
            call terminal_output(terminal, terminal%wcm%cursor_motion_ll)
            call cmat(terminal, terminal%wcm%cursor_motion_rows - 1, 0)
        case (USECR)
            call terminal_output(terminal, terminal%wcm%cursor_motion_cr)
            if (terminal%wcm%cursor_motion_autolf) terminal%wcm%cursor_motion_cury = terminal%wcm%cursor_motion_cury + 1
            terminal%wcm%cursor_motion_curx = 0
        case default
            ! Nothing to do for other values.
        end select

        ignored = calccost(terminal, terminal%wcm%cursor_motion_cury, terminal%wcm%cursor_motion_curx, row, column, .true.)
        call cmat(terminal, row, column)
    end subroutine cmgoto

    !> Forget all cursor-motion capabilities.  Port of cm.c:Wcm_clear.
    subroutine wcm_clear(terminal)
        type(terminal_display_info_type), intent(inout) :: terminal

        terminal%wcm = cursor_motion_type()
    end subroutine wcm_clear

    !> 0 if the terminal can position the cursor, -1 if it cannot, -2 if
    !> its size is unknown.  Port of cm.c:Wcm_init.
    integer function wcm_initialize(terminal) result(r)
        type(terminal_display_info_type), intent(in) :: terminal

        r = 0
        if (allocated(terminal%wcm%cursor_motion_absolute)) return

        ! Need up and left, and without absolute motion down and right.
        if (.not. allocated(terminal%wcm%cursor_motion_up) .or. .not. allocated(terminal%wcm%cursor_motion_left)) then
            r = -1
        else if (.not. allocated(terminal%wcm%cursor_motion_down) .or. .not. allocated(terminal%wcm%cursor_motion_right)) then
            r = -1
        else if (terminal%wcm%cursor_motion_rows <= 0 .or. terminal%wcm%cursor_motion_columns <= 0) then
            r = -2
        end if
    end function wcm_initialize
end module cursor_motion_mod
