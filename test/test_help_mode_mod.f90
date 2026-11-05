! test_help_mode_mod.f90 -- tests for Help mode's cross-references (help_mode_mod).
! Ports GNU Emacs 31.1 test/lisp/help-mode-tests.el in part; help-buffer
! returning the current buffer, the Info button of help-mode-tests-make-xrefs
! and help-xref-on-pp are Deferred.
!
! Copyright (C) 2021-2026 Free Software Foundation, Inc.
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

!> Ports help-mode-tests.el's make-xrefs (its name button), xref-button,
!> xref-go-back, xref-go-forward, go-back, go-back-no-stack, go-forward,
!> go-forward-no-stack, do-xref, follow-symbol and
!> follow-symbol-no-symbol; own tests for the [back] and [forward]
!> buttons of *Help*.
module test_help_mode_mod
    use, intrinsic :: iso_fortran_env, only: int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_mod, only: status_type
    use handle_mod, only: handle_type
    use buffer_mod, only: get_or_make_buffer, get_buffer, set_buffer, current_buffer, erase_buffer, set_buffer_read_only
    use insert_delete_mod, only: insert
    use editing_functions_mod, only: point, goto_char, point_max, buffer_string, widen
    use search_mod, only: search_forward
    use frame_mod, only: selected_frame, ensure_frame_windows
    use window_mod, only: delete_other_windows, resize_window_tree, switch_to_buffer
    use bindings_mod, only: init_bindings
    use keyboard_mod, only: discard_input
    use help_mode_mod, only: help_mode, help_make_xrefs, help_xref_item_type, help_xref_stack, help_xref_forward_stack, &
                             help_xref_stack_item, help_xref_go_back, help_xref_go_forward, help_go_back, help_go_forward, &
                             help_follow_name
    use help_functions_mod, only: describe_function
    use button_mod, only: button_type, button_kind_type, button_at, push_button
    implicit none(type, external)
    private

    public :: collect_help_mode_mod

    character(len=*), parameter :: LF = achar(10)

    !> The curved quotes of format_message.
    character(len=*), parameter :: LQ = char(226)//char(128)//char(152), RQ = char(226)//char(128)//char(153)

contains

    !> Collect all exported unit tests.
    subroutine collect_help_mode_mod(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("help-mode-tests-make-xrefs", test_make_xrefs), &
                    new_unittest("help-mode-tests-xref-button", test_xref_button), &
                    new_unittest("help-mode-tests-xref-go-back", test_xref_go_back), &
                    new_unittest("help-mode-tests-xref-go-forward", test_xref_go_forward), &
                    new_unittest("help-mode-tests-go-back", test_go_back), &
                    new_unittest("help-mode-tests-go-back-no-stack", test_go_back_no_stack), &
                    new_unittest("help-mode-tests-go-forward", test_go_forward), &
                    new_unittest("help-mode-tests-go-forward-no-stack", test_go_forward_no_stack), &
                    new_unittest("help-mode-tests-do-xref", test_do_xref), &
                    new_unittest("help-mode-tests-follow-symbol", test_follow_symbol), &
                    new_unittest("help-mode-tests-follow-symbol-no-symbol", test_follow_symbol_no_symbol), &
                    new_unittest("help-mode-back-forward-buttons", test_back_forward_buttons) &
                    ]
    end subroutine collect_help_mode_mod

    !> Empty stacks, and a fresh empty buffer (with-temp-buffer).
    subroutine temp_buffer()
        type(handle_type) :: buffer
        type(status_type) :: st

        call init_bindings()
        call discard_input()
        call empty_stacks()
        call get_or_make_buffer(" *help-mode-test*", buffer, st)
        call set_buffer(buffer, st)
        call set_buffer_read_only(buffer, .false., st)
        call widen()
        call erase_buffer(st)
    end subroutine temp_buffer

    !> Empty xref stacks and no current item.
    subroutine empty_stacks()
        if (allocated(help_xref_stack)) deallocate (help_xref_stack)
        if (allocated(help_xref_forward_stack)) deallocate (help_xref_forward_stack)
        allocate (help_xref_stack(0), help_xref_forward_stack(0))
        help_xref_stack_item%describe => null()
    end subroutine empty_stacks

    !> A stack item's procedure: replace the buffer text with "bar".
    recursive subroutine insert_bar(argument, status, interactive)
        character(len=*), intent(in) :: argument
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: interactive

        call erase_buffer(status)
        if (status%ok()) call insert(argument, status)
    end subroutine insert_bar

    !> A stack of one item showing "bar" with point at 2.
    subroutine bar_stack(stack)
        type(help_xref_item_type), allocatable, intent(out) :: stack(:)

        allocate (stack(1))
        stack(1)%position = 2
        stack(1)%describe => insert_bar
        stack(1)%argument = "bar"
    end subroutine bar_stack

    !> The name of the type of the button at POSITION ("" if none).
    function button_type_name(position) result(name)
        integer(int64), intent(in) :: position
        character(len=:), allocatable :: name
        type(button_type) :: button
        type(button_kind_type), pointer :: kind
        logical :: found

        name = ""
        call button_at(position, button, found)
        if (.not. found) return
        kind => button%get_kind()
        if (associated(kind)) name = trim(kind%name)
    end function button_type_name

    !> The position where STRING starts, searching from the beginning.
    integer(int64) function position_of(string) result(pos)
        character(len=*), intent(in) :: string
        integer(int64) :: found
        type(status_type) :: st
        integer :: k, characters

        ! The characters of STRING: its bytes but UTF-8 continuation bytes.
        characters = 0

        do k = 1, len(string)
            if (iachar(string(k:k)) < 128 .or. iachar(string(k:k)) >= 192) characters = characters + 1
        end do

        call goto_char(1_int64)
        call search_forward(string, found, st)
        pos = found - characters
    end function position_of

    !> The text of *Help*.
    function help_text() result(text)
        character(len=:), allocatable :: text
        type(handle_type) :: old
        type(status_type) :: st

        old = current_buffer()
        call set_buffer(get_buffer("*Help*"), st)
        text = buffer_string()
        call set_buffer(old, st)
    end function help_text

    subroutine test_make_xrefs(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st

        call temp_buffer()
        call insert("forward_char is an interactive function in "//LQ//"editing_commands_mod.f90"//RQ//", like "//LQ// &
                    "backward_char"//RQ//"."//LF//LF//"Move point N characters forward.  See also "//LQ// &
                    "backward_char"//RQ//" and the variable "//LQ//"fill_column"//RQ//", and M-x backward_char."//LF//LF, st)
        call help_mode(st)
        call help_make_xrefs(st)
        call check(error, st%ok(), "help_make_xrefs")
        if (allocated(error)) return
        ! The first paragraph is skipped.
        call check(error, button_type_name(position_of("backward_char")) == "", "first paragraph")
        if (allocated(error)) return
        call check(error, button_type_name(position_of("backward_char"//RQ//" and")) == "help_name", "name")
        if (allocated(error)) return
        call check(error, button_type_name(position_of("fill_column")) == "help_variable", "variable")
        if (allocated(error)) return
        call check(error, button_type_name(position_of("backward_char.")) == "help_function", "M-x")
        if (allocated(error)) return
        ! One newline at the end, no navigation buttons.
        call check(error, buffer_string() == "forward_char is an interactive function in "//LQ//"editing_commands_mod.f90"//RQ// &
                   ", like "//LQ//"backward_char"//RQ//"."//LF//LF//"Move point N characters forward.  See also "//LQ// &
                   "backward_char"//RQ//" and the variable "//LQ//"fill_column"//RQ//", and M-x backward_char."//LF, &
                   buffer_string())
    end subroutine test_make_xrefs

    subroutine test_xref_button(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), parameter :: NAMES(3) = [character(len=14) :: "forward_char", "fill_paragraph", "upcase_word"]
        type(status_type) :: st
        integer(int64) :: beginning
        integer :: k

        call temp_buffer()

        do k = 1, size(NAMES)
            call erase_buffer(st)
            call insert("x"//LF//LF//"See also the function "//LQ//trim(NAMES(k))//RQ//".", st)
            call help_make_xrefs(st)
            beginning = position_of(trim(NAMES(k)))
            call check(error, button_type_name(beginning - 1) == "", "before "//trim(NAMES(k)))
            if (allocated(error)) return
            call check(error, button_type_name(beginning + len_trim(NAMES(k))) == "", "after "//trim(NAMES(k)))
            if (allocated(error)) return
            call check(error, button_type_name(beginning) == "help_function", trim(NAMES(k)))
            if (allocated(error)) return
        end do
    end subroutine test_xref_button

    subroutine test_xref_go_back(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st

        call temp_buffer()
        call bar_stack(help_xref_stack)
        call insert("foo", st)
        call help_xref_go_back(current_buffer(), st)
        call check(error, st%ok() .and. point() == 2 .and. buffer_string() == "bar", buffer_string())
    end subroutine test_xref_go_back

    subroutine test_xref_go_forward(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st

        call temp_buffer()
        call bar_stack(help_xref_forward_stack)
        call insert("foo", st)
        call help_xref_go_forward(current_buffer(), st)
        call check(error, st%ok() .and. point() == 2 .and. buffer_string() == "bar", buffer_string())
    end subroutine test_xref_go_forward

    subroutine test_go_back(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st

        call temp_buffer()
        call bar_stack(help_xref_stack)
        call insert("foo", st)
        call help_go_back(st)
        call check(error, st%ok() .and. point() == 2 .and. buffer_string() == "bar", buffer_string())
    end subroutine test_go_back

    subroutine test_go_back_no_stack(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st

        call temp_buffer()
        call help_go_back(st)
        call check(error,.not. st%ok(), "an error")
    end subroutine test_go_back_no_stack

    subroutine test_go_forward(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st

        call temp_buffer()
        call bar_stack(help_xref_forward_stack)
        call insert("foo", st)
        call help_go_forward(st)
        call check(error, st%ok() .and. point() == 2 .and. buffer_string() == "bar", buffer_string())
    end subroutine test_go_forward

    subroutine test_go_forward_no_stack(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st

        call temp_buffer()
        call help_go_forward(st)
        call check(error,.not. st%ok(), "an error")
    end subroutine test_go_forward_no_stack

    !> One window of 80 columns showing *scratch*.
    subroutine fresh_window()
        type(handle_type) :: buffer
        type(status_type) :: st

        call init_bindings()
        call get_or_make_buffer("*scratch*", buffer, st)
        call set_buffer(buffer, st)
        call ensure_frame_windows()
        call delete_other_windows(selected_frame%selected_window, st)
        call resize_window_tree(selected_frame%root_window, 0, 0, 23, 80, 80)
        call switch_to_buffer("*scratch*", st)
    end subroutine fresh_window

    subroutine test_do_xref(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        character(len=:), allocatable :: text

        call fresh_window()
        call temp_buffer()
        call insert("upcase_word", st)
        call help_mode(st)
        call help_follow_name(st, 3_int64)
        text = help_text()
        call check(error, st%ok() .and. index(text, "upcase_word is ") == 1, text)
    end subroutine test_do_xref

    subroutine test_follow_symbol(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        character(len=:), allocatable :: text

        call fresh_window()
        call temp_buffer()
        call insert("fill_column", st)
        call help_mode(st)
        call help_follow_name(st, 1_int64)
        text = help_text()
        call check(error, st%ok() .and. index(text, "fill_column is ") == 1, text)
    end subroutine test_follow_symbol

    subroutine test_follow_symbol_no_symbol(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st

        call temp_buffer()
        call insert("fXYEWnRHI0B9w6VJqQIw", st)
        call help_mode(st)
        call help_follow_name(st, 1_int64)
        call check(error,.not. st%ok(), "an error")
    end subroutine test_follow_symbol_no_symbol

    subroutine test_back_forward_buttons(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        character(len=:), allocatable :: text
        integer(int64) :: back

        call fresh_window()
        call temp_buffer()
        call describe_function("forward_char", st, interactive=.true.)
        text = help_text()
        call check(error, st%ok() .and. index(text, "[back]") == 0, "no [back] on the first page")
        if (allocated(error)) return
        call describe_function("backward_char", st, interactive=.true.)
        text = help_text()
        call check(error, index(text, "backward_char is ") == 1 .and. index(text, LF//LF//"[back]"//LF) == len(text) - 8, &
                   text)
        if (allocated(error)) return
        call check(error, size(help_xref_stack) == 1, "one item on the stack")
        if (allocated(error)) return
        ! Push [back] in *Help*.
        call set_buffer(get_buffer("*Help*"), st)
        back = index(text, "[back]")
        call check(error, button_type_name(back) == "help_back", "[back] is a button")
        if (allocated(error)) return
        call push_button(st, back)
        text = help_text()
        call check(error, st%ok() .and. index(text, "forward_char is ") == 1 .and. index(text, "[forward]") > 0 .and. &
                   index(text, "[back]") == 0, text)
        if (allocated(error)) return
        call set_buffer(get_buffer("*Help*"), st)
        call help_go_forward(st)
        text = help_text()
        call check(error, st%ok() .and. index(text, "backward_char is ") == 1 .and. index(text, "[back]") > 0 .and. &
                   index(text, "[forward]") == 0, text)
    end subroutine test_back_forward_buttons
end module test_help_mode_mod
