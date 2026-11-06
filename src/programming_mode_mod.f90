! programming_mode_mod.f90 -- Prog mode.
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
module programming_mode_mod
    use commands_mod, only: commands
    use error_mod, only: status_type
    use modes_mod, only: major_modes
    use hooks_mod, only: hook_type
    use abbreviation_mod, only: abbreviation_table_type
    use handle_mod, only: handle_type
    use files_mod, only: set_require_final_newline_from_mode
    use syntax_mod, only: parse_expression_ignore_comments
    implicit none(type, external)
    private

    public :: programming_mode, initialize_programming_mode, programming_first_column

    !> Normal hook run when entering programming modes.  Port of
    !> prog-mode.el prog-mode-hook.
    type(hook_type), target, public :: programming_mode_hook = hook_type(name="programming_mode_hook")

    !> Keymap used for programming modes.  Port of prog-mode.el
    !> prog-mode-map.
    type(handle_type), public :: programming_mode_map

    !> Abbrev table for `programming_mode'.  Port of prog-mode.el
    !> prog-mode-abbrev-table (made by define-derived-mode).
    type(abbreviation_table_type), target, public :: programming_mode_abbreviation_table

    !> The value of `programming_indentation_context': none (`active' .false.) or
    !> FIRST-COLUMN and REST.
    type, public :: programming_indentation_context_type
        logical :: active = .false.
        integer :: first_column = 0
    end type programming_indentation_context_type

    !> When true (`active'), provides context for indenting embedded code chunks.
    !>
    !> There are languages where part of the code is actually written in
    !> a sub language, e.g., a Yacc/Bison or ANTLR grammar can also include
    !> JAVASCRIPT or Python code.  This variable enables the primary mode of the
    !> main language to use the indentation engine of the sub-mode for
    !> lines in code chunks written in the sub-mode's language.
    !>
    !> When a major mode of such a main language decides to delegate the
    !> indentation of a line/region to the indentation engine of the sub
    !> mode, it should bind this variable to true around the call.
    !>
    !> `first_column' is the column the indentation engine of the sub-mode
    !> should use for top-level language constructs inside the code
    !> chunk (instead of 0).  Port of prog-mode.el prog-indentation-context
    !> (its unused REST is left out).
    type(programming_indentation_context_type), public :: programming_indentation_context

contains

    !> Major mode for editing programming language source code.  Port of
    !> prog-mode.el:prog-mode.
    recursive subroutine programming_mode(status)
        type(status_type), intent(out) :: status
        logical :: saved

        call major_modes%programming_mode%begin(saved, status)
        if (.not. status%ok()) return
        call set_require_final_newline_from_mode()
        call parse_expression_ignore_comments%set_local(.true.)
        call major_modes%programming_mode%end(saved, status)
    end subroutine programming_mode

    !> Define Prog mode.  Its parent is fundamental-mode, which
    !> define-derived-mode takes as no parent.  Not a port.
    subroutine initialize_programming_mode()
        call major_modes%programming_mode%define("programming_mode", "Prog", programming_mode, programming_mode_hook, &
                                                 abbreviation_table=programming_mode_abbreviation_table)
        programming_mode_map = major_modes%programming_mode%map
        commands%programming_mode%execute => programming_mode
    end subroutine initialize_programming_mode

    !> Return the indentation column normally used for top-level constructs.
    !> Port of prog-mode.el:prog-first-column.
    integer function programming_first_column()
        programming_first_column = 0
        if (programming_indentation_context%active) programming_first_column = programming_indentation_context%first_column
    end function programming_first_column
end module programming_mode_mod
