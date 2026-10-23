! help_fns_fm.f90 -- describe-function (C-h f) and describe-variable (C-h v).
! Ported from GNU Emacs 31.1 lisp/help-fns.el.
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

!> `describe-function' and `describe-variable', from the documentation
!> docstrings_fm collects.  The functions are the commands and the
!> procedures of the config API (module `fortmacs'), the variables its
!> settings.  A function's report names the Fortran file defining it and
!> shows how Fortran calls it where the Lisp version shows its argument
!> list; a variable's shows its Fortran name and its value as Fortran.
!>
!> Deferred: the default from the symbol at point, cross-reference
!> buttons, "Probably introduced at" lines, obsolescence, aliases,
!> buffer-local values, `describe-symbol', `describe-command'.
module help_fns_fm
    use, intrinsic :: iso_fortran_env, only: int64
    use commands_fm, only: commands
    use error_fm, only: status_type
    use lisp_fm, only: value_type, nilp, make_string, make_list, list_elements
    use lread_fm, only: sym
    use data_fm, only: symbol_name
    use callint_fm, only: command_defined_p, fboundp
    use keymap_fm, only: key_description, where_is_internal
    use minibuf_fm, only: completion_obarray_table, completion_list_table
    use minibuffer_fm, only: completing_read, REQUIRE_MATCH_T
    use docstrings_fm, only: doc_entry_type, function_doc, variable_doc, function_names, variable_names
    use help_fm, only: substitute_command_keys, with_help_window, describe_function_1_hook, documentation_hook
    implicit none(type, external)
    private

    public :: describe_function, describe_function_1, describe_variable, describe_variable_text, &
              syms_of_help_fns

    character(len=*), parameter :: LF = achar(10)

    !> Left and right single quotes.
    character(len=*), parameter :: LQUOTE = char(226)//char(128)//char(152), &
                                   RQUOTE = char(226)//char(128)//char(153)

contains

    !> Whether `name' is a function help knows: a documented one or a
    !> command.
    logical function function_known_p(name)
        character(len=*), intent(in) :: name
        integer :: k
        function_known_p = .true.
        do k = 1, size(function_names)
            if (trim(function_names(k)) == name) return
        end do
        function_known_p = fboundp(sym(name))
    end function function_known_p

    !> Completion predicate for `describe-function'.
    logical function function_symbol_p(elt)
        type(value_type), intent(in) :: elt
        character(len=:), allocatable :: name
        type(status_type) :: status
        call symbol_name(elt, name, status)
        function_symbol_p = status%ok()
        if (function_symbol_p) function_symbol_p = function_known_p(name)
    end function function_symbol_p

    !> The keys that run command `name', as "C-f, <right>"; "" for none.
    function bindings_text(name) result(s)
        character(len=*), intent(in) :: name
        character(len=:), allocatable :: s
        type(value_type) :: all
        type(value_type), allocatable :: seqs(:), events(:)
        integer :: k
        s = ""
        call where_is_internal(sym(name), all)
        if (nilp(all)) return
        call list_elements(all, seqs)
        do k = 1, size(seqs)
            call list_elements(seqs(k), events)
            if (k > 1) s = s//", "
            s = s//key_description(events)
        end do
    end function bindings_text

    !> What describe-function says about function `name' after "NAME is ":
    !> its kind and file, its keys, how Fortran calls it and its
    !> documentation.  Port of help-fns.el:describe-function-1 (with
    !> help-fns-function-description-header, help-fns--key-bindings and
    !> help-fns--signature).
    subroutine describe_function_1(name, text)
        character(len=*), intent(in) :: name
        character(len=:), allocatable, intent(out) :: text
        type(doc_entry_type) :: entry
        character(len=:), allocatable :: keys, doc
        logical :: found, interactive, subroutine_p
        call function_doc(name, entry, found)
        interactive = command_defined_p(sym(name))
        if (found) interactive = interactive .or. entry%interactive
        subroutine_p = .true.
        if (found) subroutine_p = len(entry%fortran) == 0 .or. index(entry%fortran, "call ") == 1
        if (interactive) then
            text = "an interactive "
        else
            text = "a "
        end if
        if (subroutine_p) then
            text = text//"Fortran subroutine"
        else
            text = text//"Fortran function"
        end if
        if (found) text = text//" in "//LQUOTE//entry%file//RQUOTE
        text = text//"."//LF//LF
        if (interactive) then
            keys = bindings_text(name)
            if (len(keys) > 0) text = text//"It is bound to "//keys//"."//LF//LF
        end if
        if (found) then
            if (len(entry%fortran) > 0) text = text//entry%fortran//LF//LF
            doc = entry%doc
        else
            doc = ""
        end if
        if (len(doc) == 0) doc = "Not documented."
        text = text//substitute_command_keys(doc)//LF
    end subroutine describe_function_1

    !> Display the full documentation of FUNCTION (a symbol).  Port of
    !> help-fns.el:describe-function (FUNCTION is a name).
    subroutine describe_function(function, status)
        character(len=*), intent(in) :: function
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: text
        call describe_function_1(function, text)
        call with_help_window(function//" is "//text, status)
    end subroutine describe_function

    !> The report describe-variable shows for variable `name'; `found'
    !> false if help doesn't know it.  Port of the text help-fns.el:
    !> describe-variable inserts.
    subroutine describe_variable_text(name, text, found)
        character(len=*), intent(in) :: name
        character(len=:), allocatable, intent(out) :: text
        logical, intent(out) :: found
        type(doc_entry_type) :: entry
        character(len=:), allocatable :: doc
        call variable_doc(name, entry, found)
        if (.not. found) then
            text = name//" is void as a variable."//LF
            return
        end if
        text = name//" is a variable defined in "//LQUOTE//entry%file//RQUOTE//"."//LF//LF
        ! A value too long for the line starts on the next.
        if (len("Its value is "//entry%value) < 68) then
            text = text//"Its value is "//entry%value//LF
        else
            text = text//"Its value is"//LF//entry%value//LF
        end if
        if (len(entry%original) > 0 .and. entry%original /= entry%value) then
            text = text//"Original value was "//entry%original//LF
        end if
        text = text//"Its Fortran name is "//LQUOTE//entry%fortran//RQUOTE//"."//LF//LF
        doc = entry%doc
        if (len(doc) == 0) doc = "Not documented as a variable."
        text = text//substitute_command_keys(doc)//LF
    end subroutine describe_variable_text

    !> Display the full documentation of VARIABLE (a symbol).  Port of
    !> help-fns.el:describe-variable (VARIABLE is a name; global values
    !> only).
    subroutine describe_variable(variable, status)
        character(len=*), intent(in) :: variable
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: text
        logical :: found
        call describe_variable_text(variable, text, found)
        call with_help_window(text, status)
    end subroutine describe_variable

    !> Display the full documentation of FUNCTION (a symbol).
    !> Port of help-fns.el:describe-function (the command; no default).
    recursive subroutine cmd_describe_function(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: name
        call completing_read("Describe function: ", completion_obarray_table(), name, status, &
                             function_symbol_p, require_match=REQUIRE_MATCH_T)
        if (.not. status%ok() .or. len(name) == 0) return
        call describe_function(name, status)
    end subroutine cmd_describe_function

    !> Display the full documentation of VARIABLE (a symbol).
    !> Port of help-fns.el:describe-variable (the command; no default).
    recursive subroutine cmd_describe_variable(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: name
        type(value_type), allocatable :: names(:)
        integer :: k
        allocate (names(size(variable_names)))
        do k = 1, size(variable_names)
            names(k) = make_string(trim(variable_names(k)))
        end do
        call completing_read("Describe variable: ", completion_list_table(make_list(names)), name, status, &
                             require_match=REQUIRE_MATCH_T)
        if (.not. status%ok() .or. len(name) == 0) return
        call describe_variable(name, status)
    end subroutine cmd_describe_variable

    !> Register the commands, intern the documented function names (for
    !> completion) and install `describe_function_1' for describe-key.
    subroutine syms_of_help_fns()
        type(value_type) :: s
        integer :: k
        do k = 1, size(function_names)
            s = sym(trim(function_names(k)))
        end do
        describe_function_1_hook => describe_function_1
        documentation_hook => documentation
        commands%describe_function%run => cmd_describe_function
        commands%describe_variable%run => cmd_describe_variable
    end subroutine syms_of_help_fns

    !> The raw docstring of function NAME.  Port of doc.c:Fdocumentation
    !> (RAW t; the compiled-in docstrings).
    subroutine documentation(name, doc, found)
        character(len=*), intent(in) :: name
        character(len=:), allocatable, intent(out) :: doc
        logical, intent(out) :: found
        type(doc_entry_type) :: entry
        doc = ""
        call function_doc(name, entry, found)
        if (found .and. allocated(entry%doc)) doc = entry%doc
    end subroutine documentation
end module help_fns_fm
