! tty_colors_startup_mod.f90 -- registering a terminal's colors at startup.
! Ported from GNU Emacs 31.1 lisp/startup.el and lisp/faces.el.
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

!> `tty_register_colors': the default colors, then xterm's.  A submodule
!> because term_xterm_mod uses tty_colors_mod.
submodule(tty_colors_mod) tty_colors_startup_mod
    use term_xterm_mod, only: xterm_register_default_colors, xterm_standard_colors
    implicit none(type, external)

contains

    !> Port of the color part of startup.el:command-line
    !> (tty-register-default-colors) and of term/xterm.el:xterm--init
    !> (xterm-register-default-colors).
    module subroutine tty_register_colors(ncolors, xterm, status)
        integer, intent(in) :: ncolors
        logical, intent(in) :: xterm
        type(status_type), intent(out) :: status

        tty_display_color_cells = ncolors
        call tty_register_default_colors()
        if (xterm) call xterm_register_default_colors(xterm_standard_colors, status)
    end subroutine tty_register_colors
end submodule tty_colors_startup_mod
