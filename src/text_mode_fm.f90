! text_mode_fm.f90 -- Text mode.
! Ported from GNU Emacs 31.1 lisp/textmodes/text-mode.el.
!
! Copyright (C) 1985, 1992, 1994, 2001-2026 Free Software Foundation, Inc.
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

!> Text mode, the major mode for prose.  Deferred: Ispell completion,
!> the context menu, text conversion and paragraph-indent modes.
module text_mode_fm
    use commands_fm, only: commands
    use error_fm, only: status_type
    use handle_fm, only: handle_type
    use lisp_fm, only: t_value, value_type
    use lread_fm, only: sym
    use data_fm, only: setq_local, symbol_value, boundp, set
    use syntax_fm, only: make_syntax_table, modify_syntax_entry, syntax_table_live_p
    use derived_fm, only: define_derived_mode, derived_mode_begin, derived_mode_end
    implicit none(type, external)
    private

    public :: text_mode, syms_of_text_mode

    !> Syntax table used while in `text-mode'.
    type(handle_type), public :: text_mode_syntax_table

contains

    !> Build text-mode-syntax-table.
    subroutine init_text_mode_syntax_table()
        type(status_type) :: st
        call make_syntax_table(text_mode_syntax_table, st)
        call modify_syntax_entry(iachar('"'), iachar('"'), ".   ", st, text_mode_syntax_table)
        call modify_syntax_entry(iachar("\"), iachar("\"), ".   ", st, text_mode_syntax_table)
        ! We add `p' so that M-c on 'hello' leads to 'Hello' rather than
        ! 'hello'.
        call modify_syntax_entry(iachar("'"), iachar("'"), "w p", st, text_mode_syntax_table)
        ! UAX #29 says HEBREW PUNCTUATION GERESH behaves like a letter for
        ! the purposes of finding word boundaries.
        call modify_syntax_entry(int(z'5f3'), int(z'5f3'), "w   ", st, text_mode_syntax_table)
        ! GERSHAYIM, MIDDLE DOT, HYPHENATION POINT and FULLWIDTH COLON
        ! should not be word boundaries between letters.
        call modify_syntax_entry(int(z'5f4'), int(z'5f4'), "w   ", st, text_mode_syntax_table)
        call modify_syntax_entry(int(z'b7'), int(z'b7'), "w   ", st, text_mode_syntax_table)
        call modify_syntax_entry(int(z'2027'), int(z'2027'), "w   ", st, text_mode_syntax_table)
        call modify_syntax_entry(int(z'ff1a'), int(z'ff1a'), "w   ", st, text_mode_syntax_table)
    end subroutine init_text_mode_syntax_table

    !> Major mode for editing text written for humans to read.
    !> In this mode, paragraphs are delimited only by blank or white lines.
    !> You can thus get the full benefit of adaptive filling
    !>  (see the variable `adaptive-fill-mode').
    !> Turning on Text mode runs the normal hook `text-mode-hook'.  Port of
    !> text-mode.el:text-mode.
    recursive subroutine text_mode(status)
        type(status_type), intent(out) :: status
        type(value_type) :: v
        type(status_type) :: st
        logical :: saved
        if (.not. syntax_table_live_p(text_mode_syntax_table)) call syms_of_text_mode()
        call derived_mode_begin("text-mode", saved, status)
        if (.not. status%ok()) return
        call setq_local("text-mode-variant", t_value(), status)
        call symbol_value(sym("mode-require-final-newline"), v, st)
        if (st%ok()) call setq_local("require-final-newline", v, status)
        call derived_mode_end("text-mode", saved, status)
    end subroutine text_mode

    !> Define Text mode.
    subroutine syms_of_text_mode()
        type(status_type) :: st
        logical :: bound
        if (.not. syntax_table_live_p(text_mode_syntax_table)) call init_text_mode_syntax_table()
        call define_derived_mode("text-mode", "", "Text", text_mode_syntax_table)
        commands%text_mode%run => text_mode
        ! files.el mode-require-final-newline.
        call boundp(sym("mode-require-final-newline"), bound, st)
        if (.not. bound) call set(sym("mode-require-final-newline"), t_value(), st)
    end subroutine syms_of_text_mode
end module text_mode_fm
