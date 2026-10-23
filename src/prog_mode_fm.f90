! prog_mode_fm.f90 -- Prog mode.
! Ported from GNU Emacs 31.1 lisp/progmodes/prog-mode.el.
!
! Copyright (C) 2013-2026 Free Software Foundation, Inc.
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

!> Prog mode, the parent of programming language modes.  Deferred: its
!> keys (C-M-q, M-q), the context menu and prettify-symbols.
module prog_mode_fm
    use commands_fm, only: commands
    use error_fm, only: status_type
    use lisp_fm, only: t_value, value_type
    use lread_fm, only: sym
    use data_fm, only: setq_local, symbol_value
    use derived_fm, only: define_derived_mode, derived_mode_begin, derived_mode_end
    use simple_fm, only: syms_of_simple_modes
    implicit none(type, external)
    private

    public :: prog_mode, syms_of_prog_mode

contains

    !> Major mode for editing programming language source code.  Port of
    !> prog-mode.el:prog-mode.
    recursive subroutine prog_mode(status)
        type(status_type), intent(out) :: status
        type(value_type) :: v
        type(status_type) :: st
        logical :: saved
        call derived_mode_begin("prog-mode", saved, status)
        if (.not. status%ok()) return
        call symbol_value(sym("mode-require-final-newline"), v, st)
        if (st%ok()) call setq_local("require-final-newline", v, status)
        call setq_local("parse-sexp-ignore-comments", t_value(), status)
        call derived_mode_end("prog-mode", saved, status)
    end subroutine prog_mode

    !> Define Prog mode (and its parent, Fundamental mode).
    subroutine syms_of_prog_mode()
        call syms_of_simple_modes()
        call define_derived_mode("prog-mode", "fundamental-mode", "Prog")
        commands%prog_mode%run => prog_mode
    end subroutine syms_of_prog_mode
end module prog_mode_fm
