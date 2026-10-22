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

!> Help mode, the major mode of *Help*: a read-only buffer whose keymap
!> has special-mode-map's keys (simple.el): `q` quits the window, SPC
!> and DEL scroll, digits and `-` make prefix arguments.
!>
!> Deferred: cross-reference buttons and the help-xref stack (`l`, `r`,
!> `n`, `p`), `s` (help-view-source), `i`/`I` (Info), `c` (customize),
!> `g` (revert), `h`/`?` (describe-mode), `<` and `>`.
module help_mode_fm
    use error_fm, only: status_type
    use handle_fm, only: handle_type
    use keymap_fm, only: make_sparse_keymap, keymap_live_p, free_keymap, keymap_set, use_local_map
    use buffer_fm, only: current_buffer, set_buffer_read_only
    implicit none(type, external)
    private

    public :: init_help_mode, help_mode

    !> Keymap for Help mode.  Port of help-mode.el help-mode-map (with
    !> simple.el special-mode-map's keys, as its parent upstream).
    type(handle_type), public :: help_mode_map

contains

    !> Build `help_mode_map` (again, if it exists).
    subroutine init_help_mode()
        type(status_type) :: status
        integer :: d
        character(len=1) :: digit
        if (keymap_live_p(help_mode_map)) call free_keymap(help_mode_map, status)
        call make_sparse_keymap(help_mode_map)
        call keymap_set(help_mode_map, "q", "quit-window", status)
        call keymap_set(help_mode_map, "SPC", "scroll-up-command", status)
        call keymap_set(help_mode_map, "S-SPC", "scroll-down-command", status)
        call keymap_set(help_mode_map, "DEL", "scroll-down-command", status)
        call keymap_set(help_mode_map, "-", "negative-argument", status)
        do d = 0, 9
            write (digit, "(i1)") d
            call keymap_set(help_mode_map, digit, "digit-argument", status)
        end do
    end subroutine init_help_mode

    !> Major mode for viewing help text and navigating references in it.
    !> Make the current buffer read-only, with `help_mode_map` as its
    !> local map.  Port of help-mode.el:help-mode.
    subroutine help_mode(status)
        type(status_type), intent(out) :: status
        if (.not. keymap_live_p(help_mode_map)) call init_help_mode()
        call use_local_map(help_mode_map, status)
        if (.not. status%ok()) return
        call set_buffer_read_only(current_buffer(), .true., status)
    end subroutine help_mode
end module help_mode_fm
