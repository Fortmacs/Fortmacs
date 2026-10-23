! simple_modes_fm.f90 -- Fundamental and Special modes, a submodule of
! simple_fm.
! Ported from GNU Emacs 31.1 lisp/simple.el.
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

!> `fundamental-mode` and `special-mode`: a submodule because they use
!> derived_fm.
submodule(simple_fm) simple_modes_fm
    use derived_fm, only: define_derived_mode, derived_mode_begin, derived_mode_end, derived_mode_map
    use data_fm, only: kill_all_local_variables
    use subr_fm, only: run_mode_hooks
    use keymap_fm, only: keymap_set
    use lisp_fm, only: t_value
    use easy_mmode_fm, only: define_minor_mode, minor_mode_command
    implicit none(type, external)

contains

    module procedure fundamental_mode
    character(len=1) :: none(0)
    call kill_all_local_variables()
    call run_mode_hooks(none, status)
    end procedure fundamental_mode

    module procedure special_mode
    logical :: saved
    call derived_mode_begin("special-mode", saved, status)
    if (.not. status%ok()) return
    call set(sym("buffer-read-only"), t_value(), status)
    if (.not. status%ok()) return
    call derived_mode_end("special-mode", saved, status)
    end procedure special_mode

    !> Toggle Transient Mark mode.
    !>
    !> Transient Mark mode is a global minor mode.  When enabled, the
    !> region is highlighted with the `region' face whenever the mark
    !> is active.  The mark is "deactivated" after certain non-motion
    !> commands, including those that change the text in the buffer, and
    !> during shift or mouse selection by any unshifted cursor motion
    !> command.
    !>
    !> Many commands change their behavior when Transient Mark mode is
    !> in effect and the mark is active, by acting on the region instead
    !> of their usual default part of the buffer's text.  Port of
    !> simple.el:transient-mark-mode.
    subroutine cmd_transient_mark_mode(status)
        type(status_type), intent(out) :: status
        call minor_mode_command("transient-mark-mode", status)
    end subroutine cmd_transient_mark_mode

    !> Toggle whether indentation can insert TAB characters.  Port of
    !> simple.el:indent-tabs-mode.
    subroutine cmd_indent_tabs_mode(status)
        type(status_type), intent(out) :: status
        call minor_mode_command("indent-tabs-mode", status)
    end subroutine cmd_indent_tabs_mode

    module procedure syms_of_simple_modes
    type(status_type) :: st
    integer :: d
    character(len=1) :: digit
    type(handle_type) :: map
    call defcommand("fundamental-mode", fundamental_mode)
    call defcommand("special-mode", special_mode)
    call define_derived_mode("special-mode", "", "Special")
    call define_minor_mode("transient-mark-mode", "", global=.true.)
    call defcommand("transient-mark-mode", cmd_transient_mark_mode)
    call define_minor_mode("indent-tabs-mode", "")
    call defcommand("indent-tabs-mode", cmd_indent_tabs_mode)
    ! special-mode-map: (suppress-keymap map) and the viewing keys.
    map = derived_mode_map("special-mode")
    call keymap_set(map, "q", "quit-window", st)
    call keymap_set(map, "SPC", "scroll-up-command", st)
    call keymap_set(map, "S-SPC", "scroll-down-command", st)
    call keymap_set(map, "DEL", "scroll-down-command", st)
    call keymap_set(map, "<", "beginning-of-buffer", st)
    call keymap_set(map, ">", "end-of-buffer", st)
    call keymap_set(map, "-", "negative-argument", st)
    do d = 0, 9
        write (digit, "(i1)") d
        call keymap_set(map, digit, "digit-argument", st)
    end do
    end procedure syms_of_simple_modes
end submodule simple_modes_fm
