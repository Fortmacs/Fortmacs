! test_quail_mod.f90 -- tests for quail_mod and the built-in Quail input
! methods.
! Own tests; upstream has no tests for Quail, and the expected results
! are those of GNU Emacs typing the same keys.
!
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

module test_quail_mod
    use, intrinsic :: iso_fortran_env, only: int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use test_support_mod, only: show_alone
    use error_mod, only: status_type, format_error
    use handle_mod, only: handle_type
    use value_mod, only: value_type
    use buffer_mod, only: get_or_make_buffer, set_buffer, close_buffer, buffers, current, current_buffer
    use editfns_mod, only: buffer_string
    use bindings_mod, only: init_bindings
    use xdisp_mod, only: mode_line_mule_info
    use keyboard_mod, only: kbd_store_bytes, command_loop, discard_input, last_message, recent_keys
    use mule_mod, only: activate_input_method, deactivate_input_method, default_input_method, input_methods, &
                        input_method_after_insert_chunk_hook, describe_current_input_method
    use quail_mod, only: quail_define_package, quail_map_type, quail_install_map, quail_defrule, find_quail_package, &
                         quail_translation_key_type
    implicit none(type, external)
    private
    public :: collect_quail_mod
    character(len=*), parameter :: LF = achar(10), TAB = achar(9), DEL = achar(127)
    ! The echo area when the input method last returned.
    character(len=:), allocatable :: guidance

contains

    !> Collect all exported unit tests.
    subroutine collect_quail_mod(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("latin-1-postfix", test_postfix), &
                    new_unittest("latin-1-prefix", test_prefix), &
                    new_unittest("candidates", test_candidates), &
                    new_unittest("guidance", test_guidance), &
                    new_unittest("latin-prefix-postfix", test_latin), &
                    new_unittest("define-package", test_define_package), &
                    new_unittest("toggle-and-mode-line", test_toggle), &
                    new_unittest("recent-keys", test_recent_keys), &
                    new_unittest("transient-input-method", test_transient), &
                    new_unittest("quail-help", test_help) &
                    ]
    end subroutine collect_quail_mod

    !> Make a fresh buffer shown alone, with input method IM active.
    subroutine setup(name, im, buffer, error)
        character(len=*), intent(in) :: name, im
        type(handle_type), intent(out) :: buffer
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st

        call init_bindings()
        call discard_input()
        call get_or_make_buffer(name, buffer, st)
        call set_buffer(buffer, st)
        call show_alone()
        call activate_input_method(im, st)
        call check(error, st%ok(), format_error(st))
    end subroutine setup

    !> Type KEYS in the current buffer.
    subroutine type_keys(keys)
        character(len=*), intent(in) :: keys
        type(status_type) :: st

        call kbd_store_bytes(keys)
        call command_loop(st)
    end subroutine type_keys

    !> Typing with latin-1-postfix: a letter, then the postfix; doubling
    !> the postfix.
    subroutine test_postfix(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: buffer
        type(status_type) :: st

        call setup("*postfix*", "latin-1-postfix", buffer, error)
        if (allocated(error)) return
        call check(error, buffers(current)%current_input_method_title, "1<")
        if (allocated(error)) return
        call type_keys("a' ")
        call check(error, buffer_string(), "á ")
        if (allocated(error)) return
        call type_keys("a'' ")
        call check(error, buffer_string(), "á a' ")
        if (allocated(error)) return
        call type_keys("e"""" ab'")
        call check(error, buffer_string(), "á a' e"" ab'")
        if (allocated(error)) return
        call type_keys(" a"//DEL//"b")
        call check(error, buffer_string(), "á a' e"" ab' b")
        call close_buffer(buffer, st)
    end subroutine test_postfix

    !> Typing with latin-1-prefix: an accent then a letter, a prefix
    !> followed by a key it doesn't translate, deleting the prefix.
    subroutine test_prefix(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: buffer
        type(status_type) :: st

        call setup("*prefix*", "latin-1-prefix", buffer, error)
        if (allocated(error)) return
        call type_keys("'a")
        call check(error, buffer_string(), "á")
        if (allocated(error)) return
        call type_keys("'x")
        call check(error, buffer_string(), "á'x")
        if (allocated(error)) return
        call type_keys("' ~n'"//DEL//"a")
        call check(error, buffer_string(), "á'x'ña")
        call close_buffer(buffer, st)
    end subroutine test_prefix

    !> A key with several candidates (latin-2-prefix): the first is
    !> chosen by default, a digit selects one, > and C-f (translation keys
    !> of the package) move to the next one, which is forgotten when the
    !> translation goes on (FORGET-LAST-SELECTION).
    subroutine test_candidates(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: buffer
        type(status_type) :: st

        call setup("*candidates*", "latin-2-prefix", buffer, error)
        if (allocated(error)) return
        call type_keys("`S2")
        call check(error, buffer_string(), "Ș")
        if (allocated(error)) return
        call type_keys("`S> ")
        call check(error, buffer_string(), "ȘŞ ")
        if (allocated(error)) return
        call type_keys("`S"//achar(6)//achar(6)//" `Sx")
        call check(error, buffer_string(), "ȘŞ Ş Şx")
        call close_buffer(buffer, st)
    end subroutine test_candidates

    !> The guidance in the echo area: the keys that can follow, and the
    !> list of candidates.
    subroutine test_guidance(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: buffer
        type(status_type) :: st

        call input_method_after_insert_chunk_hook%append(save_guidance)
        call setup("*guidance*", "latin-1-postfix", buffer, error)
        if (allocated(error)) return
        call type_keys("a")
        call check(error, guidance, "a[""'/^_`~]")
        if (allocated(error)) return
        call type_keys(" a'")
        call check(error, guidance, "a'[']")
        if (allocated(error)) return
        call activate_input_method("latin-2-prefix", st)
        call type_keys(" `S")
        call check(error, guidance, "`S           (01/01) 1.Ş 2.Ș")
        if (allocated(error)) return
        call type_keys(" `")
        call check(error, guidance, "`[ .ACELSTZ`acelstz]")
        if (allocated(error)) return
        call check(error, buffer_string(), "a á Ş `")
        call input_method_after_insert_chunk_hook%remove(save_guidance)
        call close_buffer(buffer, st)
    end subroutine test_guidance

    !> Keep the echo area in `guidance'.
    subroutine save_guidance(status)
        type(status_type), intent(out) :: status

        status = status_type()
        guidance = ""
        if (allocated(last_message)) guidance = last_message
    end subroutine save_guidance

    !> latin-prefix and latin-postfix, and a package with two candidates
    !> for a single key (british).
    subroutine test_latin(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: buffer
        type(status_type) :: st

        call setup("*latin*", "latin-prefix", buffer, error)
        if (allocated(error)) return
        call type_keys("~n ")
        call activate_input_method("latin-postfix", st)
        call type_keys("n~ ")
        call activate_input_method("british", st)
        call type_keys("#")
        call check(error, buffer_string(), "ñ ñ £")
        call close_buffer(buffer, st)
    end subroutine test_latin

    !> A package defined with the API: deterministic (no selection by
    !> digits), rules added one by one, appending a candidate.
    subroutine test_define_package(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: buffer
        type(quail_map_type) :: map
        type(status_type) :: st

        call init_bindings()
        call discard_input()
        call get_or_make_buffer("*define*", buffer, st)
        call set_buffer(buffer, st)
        call show_alone()
        call quail_define_package("test-quail", "English", "TQ", guidance=.false., docstring="A test.", &
                                  deterministic=.true., simple=.true.)
        call check(error, find_quail_package("test-quail") > 0 .and. trim(input_methods(1)%name) == "test-quail")
        if (allocated(error)) return
        call map%append("ab", "X")
        call map%append("abc", "YZ")
        call map%append("q", [character(len=2) :: "Q1", "Q2"])
        call quail_install_map(map, st)
        call quail_defrule("x", "1", st)
        call quail_defrule("x", "2", st, append=.true.)
        call activate_input_method("test-quail", st)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        ! "abc" has two candidates: deterministic takes the first.
        call type_keys("abc ab1 q x a ")
        call check(error, buffer_string(), "Y X1 Q1 1 a ")
        call deactivate_input_method(st)
        call close_buffer(buffer, st)
    end subroutine test_define_package

    !> C-\ turns the input method off and on again; the mode line shows
    !> its title; it can be described.
    subroutine test_toggle(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: buffer
        type(status_type) :: st

        call setup("*toggle*", "latin-1-prefix", buffer, error)
        if (allocated(error)) return
        call check(error, mode_line_mule_info(current, .false.), "1>UUU:")
        if (allocated(error)) return
        default_input_method%value = "latin-1-prefix"
        call type_keys("'e"//achar(28)//"'e"//achar(28)//"'e")
        call check(error, buffer_string(), "é'eé")
        if (allocated(error)) return
        call describe_current_input_method(st)
        call check(error, st%ok(), format_error(st))
        call deactivate_input_method(st)
        call close_buffer(buffer, st)
    end subroutine test_toggle

    !> The keys read again by Quail are not recorded twice.
    subroutine test_recent_keys(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: buffer
        type(status_type) :: st
        type(value_type), allocatable :: before(:), after(:)
        logical, allocatable :: command_flags(:)
        integer :: n

        call setup("*recent*", "latin-1-postfix", buffer, error)
        if (allocated(error)) return
        call recent_keys(.false., before, command_flags)
        call type_keys("a'x")
        call recent_keys(.false., after, command_flags)
        call check(error, buffer_string(), "áx")
        if (allocated(error)) return
        ! (The ring of recent keys may be full.)
        n = size(after)
        call check(error, n >= 3 .and. n >= size(before), "keys recorded")
        if (allocated(error)) return
        call check(error, after(n - 2)%a(1)%i == iachar("a") .and. after(n - 1)%a(1)%i == iachar("'") .and. &
                   after(n)%a(1)%i == iachar("x"), "a, ' and x recorded once")
        call close_buffer(buffer, st)
    end subroutine test_recent_keys

    !> C-x \ (activate-transient-input-method): the input method is turned
    !> off once it has inserted a chunk of text.
    subroutine test_transient(error)
        use mule_mod, only: default_transient_input_method
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: buffer
        type(status_type) :: st

        call init_bindings()
        call discard_input()
        call get_or_make_buffer("*transient*", buffer, st)
        call set_buffer(buffer, st)
        call show_alone()
        default_transient_input_method%value = "latin-1-postfix"
        call type_keys(achar(24)//"\a' a'")
        call check(error, buffer_string(), "á a'")
        if (allocated(error)) return
        call check(error,.not. allocated(buffers(current)%current_input_method))
        call close_buffer(buffer, st)
    end subroutine test_transient

    !> C-h I (quail-help): the docstring, the key sequences in columns or
    !> with their candidates, the translation keys of the package.
    subroutine test_help(error)
        use quail_mod, only: quail_help
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: buffer
        type(status_type) :: st
        character(len=:), allocatable :: text

        call setup("*help*", "esperanto-prefix", buffer, error)
        if (allocated(error)) return
        call quail_help(st)
        call check(error, help_text(), &
                   "Input method: esperanto-prefix (mode line indicator:EO>)"//LF// &
                   LF// &
                   "Esperanto input method with prefix modifiers"//LF// &
                   "Key translation rules are:"//LF// &
                   " ^H -> ?Ĥ   ^J -> ?Ĵ   ^h -> ?ĥ   ^j -> ?ĵ   ^C -> ?Ĉ   ^G -> ?Ĝ,"//LF// &
                   " ^S -> ?Ŝ   ^c -> ?ĉ   ^g -> ?ĝ   ^s -> ?ŝ   ~U -> ?Ŭ   ~u -> ?ŭ"//LF// &
                   LF// &
                   LF// &
                   "KEY SEQUENCE"//LF// &
                   "------------"//LF// &
                   "You can input characters by the following key sequences:"//LF// &
                   "key char  [type a key sequence to insert the corresponding character]"//LF// &
                   "--- ---- --- ---- --- ---- --- ---- --- ---- --- ----"//LF// &
                   "^C  Ĉ"//TAB//" ^H  Ĥ"//TAB//"  ^S  Ŝ"//TAB//"   ^g  ĝ    ^j"//TAB//"ĵ    ~U"//TAB//" Ŭ"//LF// &
                   "^G  Ĝ"//TAB//" ^J  Ĵ"//TAB//"  ^c  ĉ"//TAB//"   ^h  ĥ    ^s"//TAB//"ŝ    ~u"//TAB//" ŭ"//LF)
        if (allocated(error)) return
        call activate_input_method("british", st)
        call describe_current_input_method(st)
        call check(error, help_text(), &
                   "Input method: british (mode line indicator:£@)"//LF// &
                   LF// &
                   "British English input method with Latin-1 character £ (# -> £)"//LF// &
                   LF// &
                   LF// &
                   "KEY SEQUENCE"//LF// &
                   "------------"//LF// &
                   "You can input characters by the following key sequences:"//LF// &
                   "key character(s)  [type a key (sequence) and select one from the list]"//LF// &
                   "--- ------------"//LF// &
                   "#   £ #"//LF)
        if (allocated(error)) return
        call activate_input_method("latin-2-prefix", st)
        call quail_help(st)
        text = help_text()
        call check(error, text(index(text, "key character(s)"):index(text, "Binding") + 7), &
                   "key character(s)  [type a key (sequence) and select one from the list]"//LF// &
                   "--- ------------"//LF// &
                   "`S  Ş Ș"//LF// &
                   "`T  Ţ Ț"//LF// &
                   "`s  ş ș"//LF// &
                   "`t  ţ ț"//LF// &
                   LF// &
                   "KEY BINDINGS FOR TRANSLATION"//LF// &
                   "----------------------------"//LF// &
                   LF// &
                   "Key             Binding"//LF)
        if (allocated(error)) return
        ! A separator line as wide as the window follows.
        call check(error, text(index(text, LF//"C-b") + 1:), &
                   "C-b"//TAB//TAB//"quail_prev_translation"//LF// &
                   "C-f"//TAB//TAB//"quail_next_translation"//LF// &
                   "<"//TAB//TAB//"quail_prev_translation"//LF// &
                   ">"//TAB//TAB//"quail_next_translation"//LF// &
                   "<left>"//TAB//TAB//"quail_prev_translation"//LF// &
                   "<right>"//TAB//TAB//"quail_next_translation"//LF)
        call deactivate_input_method(st)
        call close_buffer(buffer, st)
    end subroutine test_help

    !> The text of the *Help* buffer.
    function help_text() result(text)
        character(len=:), allocatable :: text
        type(handle_type) :: old, h
        type(status_type) :: st

        old = current_buffer()
        call get_or_make_buffer("*Help*", h, st)
        call set_buffer(h, st)
        text = buffer_string()
        call set_buffer(old, st)
    end function help_text
end module test_quail_mod
