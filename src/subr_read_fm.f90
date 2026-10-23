! subr_read_fm.f90 -- read-number, a submodule of subr_fm.
! Ported from GNU Emacs 31.1 lisp/subr.el.
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

!> The minibuffer readers of subr.el: a submodule because minibuf_fm
!> uses subr_fm.
submodule(subr_fm) subr_read_fm
    use minibuf_fm, only: read_from_minibuffer
    use keyboard_fm, only: sit_for
    implicit none(type, external)

contains

    module procedure read_number
    character(len=:), allocatable :: full, str, def, histvar
    character(len=24) :: buf
    integer :: ios, k
    logical :: have
    status = status_type()
    n = 0
    full = prompt
    histvar = "read_number_history"
    if (present(hist)) histvar = hist
    def = ""
    if (present(default)) then
        write (buf, "(i0)") default
        def = trim(buf)
        k = index(full, ":", back=.true.)
        if (k > 0 .and. len_trim(full(k + 1:)) == 0) then
            full = full(1:k - 1)//" (default "//def//")"//full(k:)
        else
            full = trim(full)//" (default "//def//")"
        end if
    end if
    do
        if (present(default)) then
            call read_from_minibuffer(full, str, status, history=histvar, default=def)
        else
            call read_from_minibuffer(full, str, status, history=histvar)
        end if
        if (.not. status%ok()) return
        have = .false.
        if (len(str) == 0) then
            if (present(default)) then
                n = default
                have = .true.
            end if
        else if (verify(trim(adjustl(str)), "+-0123456789") == 0) then
            read (str, *, iostat=ios) n
            have = ios == 0
        end if
        if (have) exit
        call message("Please enter a number.")
        call sit_for(1000)
    end do
    end procedure read_number
end submodule subr_read_fm
