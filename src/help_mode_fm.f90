! help_mode_fm.f90 -- Help mode: the keymap of the *Help* buffer.
! Ported from GNU Emacs 31.1 lisp/help-mode.el.
!
! Copyright (C) 1985-1986, 1993-1994, 1998-2026 Free Software Foundation, Inc.
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

!> Help mode, the major mode of *Help*, derived from Special mode
!> (simple.el): a read-only buffer where `q` quits the window, SPC and
!> DEL scroll, digits and `-` make prefix arguments.
!>
!> Deferred: cross-reference buttons and the help-xref stack (`l`, `r`,
!> `n`, `p`), `s` (help-view-source), `i`/`I` (Info), `c` (customize),
!> `g` (revert), `h`/`?` (describe-mode).
module help_mode_fm
    use error_fm, only: status_type
    use handle_fm, only: handle_type
    use keymap_fm, only: keymap_live_p
    use callint_fm, only: defcommand
    use derived_fm, only: define_derived_mode, derived_mode_begin, derived_mode_end, derived_mode_map
    use simple_fm, only: syms_of_simple_modes
    implicit none(type, external)
    private

    public :: init_help_mode, help_mode

    !> Keymap for Help mode.  Port of help-mode.el help-mode-map (its
    !> parent is special-mode-map).
    type(handle_type), public :: help_mode_map

contains

    !> Define Help mode and its parent, Special mode.
    subroutine init_help_mode()
        call syms_of_simple_modes()
        call define_derived_mode("help-mode", "special-mode", "Help")
        help_mode_map = derived_mode_map("help-mode")
        call defcommand("help-mode", help_mode)
    end subroutine init_help_mode

    !> Major mode for viewing help text and navigating references in it.
    !> Port of help-mode.el:help-mode.
    recursive subroutine help_mode(status)
        type(status_type), intent(out) :: status
        logical :: saved
        if (.not. keymap_live_p(help_mode_map)) call init_help_mode()
        call derived_mode_begin("help-mode", saved, status)
        if (.not. status%ok()) return
        call derived_mode_end("help-mode", saved, status)
    end subroutine help_mode
end module help_mode_fm
