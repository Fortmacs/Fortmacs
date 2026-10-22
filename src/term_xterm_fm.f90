! term_xterm_fm.f90 -- xterm escape sequences as function keys.
! Ported from GNU Emacs 31.1 lisp/term/xterm.el.
!
! Copyright (C) 1995, 2001-2026 Free Software Foundation, Inc.
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

!> `xterm_function_map` maps the escape sequences xterm (and most
!> terminals emulating it) send for cursor, editing, function and keypad
!> keys, with modifiers, to function-key events; `terminal_init_xterm`
!> installs it in input-decode-map.  Upstream spells the ~350 sequences
!> out; here the regular families are generated from xterm's modifier
!> encoding (parameter 1 + Shift 1 + Meta 2 + Control 4).
!>
!> Not ported: bracketed paste and focus events (translation functions),
!> terminal queries (colors, OSC 52), the rxvt variant, and
!> xterm-alternatives-map (needs terminfo function keys).
module term_xterm_fm
    use, intrinsic :: iso_fortran_env, only: int64
    use error_fm, only: status_type
    use handle_fm, only: handle_type
    use lisp_fm, only: value_type, make_fixnum, list
    use character_fm, only: CHAR_SHIFT, CHAR_META, CHAR_CTL
    use keymap_fm, only: make_sparse_keymap, define_key, set_keymap_parent, &
                         keymap_live_p, apply_modifiers
    use keyboard_fm, only: input_decode_map
    implicit none(type, external)
    private

    public :: xterm_function_map, xterm_rxvt_function_map, terminal_init_xterm

    integer(int64), parameter :: M_SHIFT = int(CHAR_SHIFT, int64), &
                                 M_META = int(CHAR_META, int64), &
                                 M_CTL = int(CHAR_CTL, int64)
    character, parameter :: ESC = achar(27)

    type(handle_type) :: rxvt_map, xterm_map

contains

    !> Modifier bits for xterm modifier parameter `m` (2..8; also 13 and
    !> 14, which some keyboards send for Control-Meta).
    integer(int64) function xterm_modifiers(m) result(bits)
        integer, intent(in) :: m
        integer :: p
        p = m - 1
        if (m == 13 .or. m == 14) p = m - 7
        bits = 0
        if (iand(p, 1) /= 0) bits = bits + M_SHIFT
        if (iand(p, 2) /= 0) bits = bits + M_META
        if (iand(p, 4) /= 0) bits = bits + M_CTL
    end function xterm_modifiers

    function itoa(n) result(s)
        integer, intent(in) :: n
        character(len=:), allocatable :: s
        character(len=12) :: buf
        write (buf, "(i0)") n
        s = trim(buf)
    end function itoa

    !> In `map`, translate byte sequence `seq` to the single event `ev`.
    subroutine def(map, seq, ev)
        type(handle_type), intent(in) :: map
        character(len=*), intent(in) :: seq
        type(value_type), intent(in) :: ev
        type(value_type), allocatable :: key(:)
        type(status_type) :: status
        integer :: k
        allocate (key(len(seq)))
        do k = 1, len(seq)
            key(k) = make_fixnum(int(iachar(seq(k:k)), int64))
        end do
        call define_key(map, key, list(ev), status)
    end subroutine def

    !> Translate `seq` to function key `name` with modifiers `bits`.
    subroutine defkey(map, seq, bits, name)
        type(handle_type), intent(in) :: map
        character(len=*), intent(in) :: seq, name
        integer(int64), intent(in) :: bits
        call def(map, seq, apply_modifiers(bits, name))
    end subroutine defkey

    !> Escape sequences shared between xterm and rxvt.  Port of
    !> term/xterm.el:xterm-rxvt-function-map.
    function xterm_rxvt_function_map() result(map)
        type(handle_type) :: map
        character(len=6), parameter :: tilde_names(12) = &
                                       [character(len=6) :: "insert", "delete", "select", "prior", "next", &
                                                             "f5", "f6", "f7", "f8", "f9", "f10", "f1"]
        integer, parameter :: tilde_codes(12) = [2, 3, 4, 5, 6, 15, 17, 18, 19, 20, 21, 11]
        character(len=5), parameter :: arrows(4) = [character(len=5) :: "up", "down", "right", "left"]
        character(len=4), parameter :: arrow_finals = "ABCD"
        integer :: k
        if (keymap_live_p(rxvt_map)) then
            map = rxvt_map
            return
        end if
        call make_sparse_keymap(map)
        do k = 1, size(tilde_codes)
            call defkey(map, ESC//"["//itoa(tilde_codes(k))//"~", 0_int64, trim(tilde_names(k)))
        end do
        call defkey(map, ESC//"[12~", 0_int64, "f2")
        call defkey(map, ESC//"[13~", 0_int64, "f3")
        call defkey(map, ESC//"[14~", 0_int64, "f4")
        call defkey(map, ESC//"[2;2~", M_SHIFT, "insert")
        do k = 1, 4
            call defkey(map, ESC//"["//arrow_finals(k:k), 0_int64, trim(arrows(k)))
        end do
        rxvt_map = map
    end function xterm_rxvt_function_map

    !> xterm's escape sequences.  Port of term/xterm.el:xterm-function-map.
    function xterm_function_map() result(map)
        type(handle_type) :: map
        type(status_type) :: status
        character(len=4), parameter :: pf_finals = "PQRS"
        character(len=6), parameter :: cursor_finals = "ABCDFH"
        character(len=5), parameter :: cursor_names(6) = &
                                       [character(len=5) :: "up", "down", "right", "left", "end", "home"]
        integer, parameter :: fkey_codes(8) = [15, 17, 18, 19, 20, 21, 23, 24]
        integer, parameter :: edit_codes(4) = [2, 3, 5, 6]
        character(len=6), parameter :: edit_names(4) = &
                                       [character(len=6) :: "insert", "delete", "prior", "next"]
        character(len=16), parameter :: kp_finals = "jklmopqrstuvwxyX"
        character(len=11), parameter :: kp_names(16) = &
                                        [character(len=11) :: "kp-multiply", "kp-add", "kp-separator", &
                                                               "kp-subtract", "kp-divide", "kp-0", "kp-1", "kp-2", "kp-3", "kp-4", &
                                                               "kp-5", "kp-6", "kp-7", "kp-8", "kp-9", "kp-equal"]
        integer, parameter :: fkey_mods(5) = [2, 5, 6, 3, 4]
        integer :: k, j, m
        character(len=3) :: fname
        if (keymap_live_p(xterm_map)) then
            map = xterm_map
            return
        end if
        call make_sparse_keymap(map)
        call set_keymap_parent(map, xterm_rxvt_function_map(), status)
        ! F1-F4 (SS3 P..S), F11, F12, and modified F1-F12.
        do k = 1, 4
            write (fname, '("f",i0)') k
            call defkey(map, ESC//"O"//pf_finals(k:k), 0_int64, trim(fname))
            call defkey(map, ESC//"[1;2"//pf_finals(k:k), M_SHIFT, trim(fname))
            do j = 1, size(fkey_mods)
                m = fkey_mods(j)
                call defkey(map, ESC//"O"//itoa(m)//pf_finals(k:k), xterm_modifiers(m), trim(fname))
            end do
        end do
        call defkey(map, ESC//"[23~", 0_int64, "f11")
        call defkey(map, ESC//"[24~", 0_int64, "f12")
        do k = 1, size(fkey_codes)
            write (fname, '("f",i0)') k + 4
            do j = 1, size(fkey_mods)
                m = fkey_mods(j)
                call defkey(map, ESC//"["//itoa(fkey_codes(k))//";"//itoa(m)//"~", &
                            xterm_modifiers(m), trim(fname))
            end do
        end do
        ! Cursor keys: SS3 form, and CSI 1;M with modifiers.
        do k = 1, 6
            call defkey(map, ESC//"O"//cursor_finals(k:k), 0_int64, trim(cursor_names(k)))
            do m = 2, 8
                call defkey(map, ESC//"[1;"//itoa(m)//cursor_finals(k:k), &
                            xterm_modifiers(m), trim(cursor_names(k)))
            end do
            call defkey(map, ESC//"O2"//cursor_finals(k:k), M_SHIFT, trim(cursor_names(k)))
            call defkey(map, ESC//"O5"//cursor_finals(k:k), M_CTL, trim(cursor_names(k)))
        end do
        ! Insert, Delete, PageUp, PageDown with modifiers.
        do k = 1, 4
            do m = 2, 8
                call defkey(map, ESC//"["//itoa(edit_codes(k))//";"//itoa(m)//"~", &
                            xterm_modifiers(m), trim(edit_names(k)))
            end do
        end do
        call defkey(map, ESC//"[29~", 0_int64, "menu")
        call defkey(map, ESC//"[1~", 0_int64, "home")
        ! Keypad (application mode).
        do k = 1, 16
            call defkey(map, ESC//"O"//kp_finals(k:k), 0_int64, trim(kp_names(k)))
            if (k == 16) cycle
            do m = 2, 8, 2
                call defkey(map, ESC//"O"//itoa(m)//kp_finals(k:k), xterm_modifiers(m), &
                            trim(kp_names(k)))
            end do
        end do
        call define_modify_other_keys(map)
        xterm_map = map
    end function xterm_function_map

    !> Keys xterm reports with modifyOtherKeys / formatOtherKeys:
    !> CSI 27;M;C~ and CSI C;Mu.  The dolist in xterm-function-map.
    subroutine define_modify_other_keys(map)
        type(handle_type), intent(in) :: map
        ! Characters reported with Control (M = 5, or 7/13 with Meta).
        integer, parameter :: ctl_chars(11) = [39, 44, 45, 46, 47, 48, 49, 57, 59, 61, 92]
        integer, parameter :: ctl_meta_chars(19) = &
                              [39, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 59, 61, 92, 32]
        ! Characters reported with Control-Shift (M = 6, or 8/14 with Meta).
        integer, parameter :: ctl_shift_chars(14) = &
                              [33, 34, 35, 36, 37, 38, 40, 41, 42, 43, 58, 60, 62, 63]
        integer, parameter :: shift_mods(3) = [6, 14, 8], tab_mods(5) = [5, 6, 13, 7, 2]
        integer :: k, j, m
        integer(int64) :: bits
        do k = 1, size(ctl_chars)
            call other_key(5, ctl_chars(k), make_fixnum(M_CTL + ctl_chars(k)))
        end do
        do j = 1, 2
            m = merge(13, 7, j == 1)
            do k = 1, size(ctl_meta_chars)
                ! C-M-SPC is only reported with M = 7.
                if (ctl_meta_chars(k) == 32 .and. m == 13) cycle
                call other_key(m, ctl_meta_chars(k), &
                               make_fixnum(M_CTL + M_META + ctl_meta_chars(k)))
            end do
        end do
        do j = 1, 3
            m = shift_mods(j)
            bits = M_CTL
            if (m /= 6) bits = M_CTL + M_META
            do k = 1, size(ctl_shift_chars)
                call other_key(m, ctl_shift_chars(k), make_fixnum(bits + ctl_shift_chars(k)))
            end do
        end do
        call other_key(3, 32, make_fixnum(M_META + 32))
        ! TAB and RET with modifiers become function keys.
        do j = 1, size(tab_mods)
            m = tab_mods(j)
            call other_key(m, 9, apply_modifiers(xterm_modifiers(m), "tab"))
            call other_key(m, 13, apply_modifiers(xterm_modifiers(m), "return"))
        end do
    contains

        subroutine other_key(m, c, ev)
            integer, intent(in) :: m, c
            type(value_type), intent(in) :: ev
            call def(map, ESC//"[27;"//itoa(m)//";"//itoa(c)//"~", ev)
            call def(map, ESC//"["//itoa(c)//";"//itoa(m)//"u", ev)
        end subroutine other_key
    end subroutine define_modify_other_keys

    !> Make input-decode-map translate xterm's escape sequences, leaving
    !> its own bindings in front.  Port of term/xterm.el:terminal-init-xterm
    !> (the xterm--push-map onto input-decode-map part); an existing parent
    !> of input-decode-map is replaced, not composed.
    subroutine terminal_init_xterm()
        type(status_type) :: status
        type(handle_type) :: map
        map = xterm_function_map()
        if (.not. keymap_live_p(input_decode_map)) then
            call make_sparse_keymap(input_decode_map)
        end if
        call set_keymap_parent(input_decode_map, map, status)
    end subroutine terminal_init_xterm
end module term_xterm_fm
