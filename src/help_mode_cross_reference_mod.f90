! help_mode_cross_reference_mod.f90 -- Help mode's cross-reference buttons and stacks.
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

!> The cross-reference buttons of help buffers, the stacks of pages `l'
!> and `r' move through, and the commands of Help mode: a submodule of
!> help_mode_mod because it uses button_mod and help_functions_mod, which use
!> help_mode_mod through help_mod.
!>
!> The button types are upstream's, named as `button_mod' names button
!> types: `help_xref' and its subtypes `help_function', `help_variable',
!> `help_name', `help_back' and `help_forward'; a button's
!> `button_data' is the name it describes.
submodule(help_mode_mod) help_mode_cross_reference_mod
    use error_mod, only: set_error, FM_USER_ERROR
    use value_mod, only: value_type, vtag, xstring, V_STRING
    use buffer_mod, only: buffers, current, current_buffer, get_or_make_buffer, set_buffer, is_buffer_live
    use derived_mod, only: is_derived_mode
    use insert_delete_mod, only: insert
    use editing_functions_mod, only: point, goto_char, point_min, point_max, buffer_substring_no_properties, delete_region, &
                                     is_bob, is_eob, is_bol
    use search_mod, only: re_search_forward, match_beginning, match_end, case_fold_search, SEARCH_NOERROR
    use syntax_mod, only: skip_syntax_backward, skip_syntax_forward
    use paragraphs_mod, only: forward_paragraph
    use page_mod, only: forward_page, backward_page
    use editing_commands_mod, only: forward_line
    use simple_mod, only: push_mark
    use window_mod, only: windows, get_buffer_window, is_window_live, set_window_point, same_handle
    use frame_mod, only: selected_frame
    use button_mod, only: button_kind_type, button_type, init_button, button_at, make_text_button, &
                          insert_text_button
    use help_mod, only: help_buffer_name
    use help_functions_mod, only: describe_function, describe_variable, describe_name, is_function_known, is_variable_known
    implicit none(type, external)

    !> The cross-reference button types.  Port of help-mode.el's
    !> define-button-type forms.
    type(button_kind_type), target :: help_xref, help_function, help_variable, help_name, help_back, help_forward

    !> The most items `help_setup_xref' leaves on `help_xref_stack' when
    !> a command shows a page (upstream's (nthcdr 10 ...), whose tail it
    !> cuts after the 11th).
    integer, parameter :: STACK_LIMIT = 11

    character(len=*), parameter :: LF = achar(10), TAB = achar(9), FF = achar(12)

    !> Left and right single quotes.
    character(len=*), parameter :: LQUOTE = char(226)//char(128)//char(152), &
                                   RQUOTE = char(226)//char(128)//char(153)

    !> Regexp matching doc string references to names.
    !>
    !> The words preceding the quoted name can be used in doc strings to
    !> distinguish references to variables, functions and names.  Port of
    !> help-mode.el help-xref-symbol-regexp.
    character(len=*), parameter :: help_xref_name_regexp = &
                                   "\(\<\(\(variable\|option\)\|" &
                                   //"\(function\|command\|call\)\|" &
                                   //"\(face\)\|" &
                                   //"\(symbol\|program\|property\)\|" &
                                   //"\(source \(?:code \)?\(?:of\|for\)\)\)" &
                                   //"[ "//TAB//LF//"]+\)?" &
                                   //"\(\\+\)?" &
                                   //"['`"//LQUOTE//"]\(\(?:\sw\|\s_\)+\|`\)['"//RQUOTE//"]"

    !> An obvious case of a key substitution: M-x and a command name made
    !> of word and identifier characters, ending with a word character.  Port
    !> of the regexp of help-mode.el:help-make-xrefs.
    character(len=*), parameter :: M_X_REGEXP = "\<M-x\s-+\(\sw\(\sw\|\s_\)*\sw\)"

contains

    ! ------------------------------------------------------------------
    ! Button types and actions
    ! ------------------------------------------------------------------

    module procedure init_help_xref
    call init_button()
    if (help_xref%index /= 0) return
    help_xref%follow_link = .true.
    call help_xref%define("help_xref")
    help_function%help_echo = "mouse-2, RET: describe this function"
    call help_function%define("help_function", supertype=help_xref)
    help_function%action => follow_function
    help_variable%help_echo = "mouse-2, RET: describe this variable"
    call help_variable%define("help_variable", supertype=help_xref)
    help_variable%action => follow_variable
    help_name%help_echo = "mouse-2, RET: describe this name"
    call help_name%define("help_name", supertype=help_xref)
    help_name%action => follow_name
    help_back%help_echo = "mouse-2, RET: go back to previous help buffer"
    call help_back%define("help_back", supertype=help_xref)
    help_back%action => follow_back
    help_forward%help_echo = "mouse-2, RET: move forward to next help buffer"
    call help_forward%define("help_forward", supertype=help_xref)
    help_forward%action => follow_forward
    commands%help_go_back%execute => help_go_back
    commands%help_go_forward%execute => help_go_forward
    commands%help_follow_name%execute => cmd_help_follow_name
    commands%help_goto_next_page%execute => help_goto_next_page
    commands%help_goto_previous_page%execute => help_goto_previous_page
    end procedure init_help_xref

    !> Call the help cross-reference procedure DESCRIBE with ARGUMENT.
    !> Things are set up properly so that the resulting help buffer has a
    !> proper [back] button.  Port of help-mode.el:help-do-xref.
    recursive subroutine help_do_xref(describe, argument, status)
        procedure(help_xref_procedure) :: describe
        character(len=*), intent(in) :: argument
        type(status_type), intent(out) :: status
        logical :: saved

        ! There is a reference at point.  Follow it.
        saved = help_xref_following
        help_xref_following = .true.
        call describe(argument, status)
        help_xref_following = saved
    end subroutine help_do_xref

    !> The name a button describes: its `button_data'.  Not a port.
    function data_name(data) result(name)
        type(value_type), intent(in) :: data
        character(len=:), allocatable :: name

        name = ""
        if (vtag(data) == V_STRING) name = xstring(data)
    end function data_name

    !> The action of `help_function' buttons.  Port of
    !> help-mode.el:help-button-action (help-function `describe-function').
    recursive subroutine follow_function(button, data, status)
        type(button_type), intent(in) :: button
        type(value_type), intent(in) :: data
        type(status_type), intent(out) :: status

        call help_do_xref(describe_function, data_name(data), status)
    end subroutine follow_function

    !> The action of `help_variable' buttons.  Port of
    !> help-mode.el:help-button-action (help-function `describe-variable').
    recursive subroutine follow_variable(button, data, status)
        type(button_type), intent(in) :: button
        type(value_type), intent(in) :: data
        type(status_type), intent(out) :: status

        call help_do_xref(describe_variable, data_name(data), status)
    end subroutine follow_variable

    !> The action of `help_name' buttons.  Port of
    !> help-mode.el:help-button-action (help-function `describe-symbol').
    recursive subroutine follow_name(button, data, status)
        type(button_type), intent(in) :: button
        type(value_type), intent(in) :: data
        type(status_type), intent(out) :: status

        call help_do_xref(describe_name, data_name(data), status)
    end subroutine follow_name

    !> The action of `help_back' buttons: go back from the buffer of the
    !> button (upstream passes it as the button's argument).  Port of
    !> help-mode.el:help-button-action (help-function `help-xref-go-back').
    recursive subroutine follow_back(button, data, status)
        type(button_type), intent(in) :: button
        type(value_type), intent(in) :: data
        type(status_type), intent(out) :: status
        logical :: saved

        saved = help_xref_following
        help_xref_following = .true.
        call help_xref_go_back(current_buffer(), status)
        help_xref_following = saved
    end subroutine follow_back

    !> The action of `help_forward' buttons.  Port of
    !> help-mode.el:help-button-action (help-function
    !> `help-xref-go-forward').
    recursive subroutine follow_forward(button, data, status)
        type(button_type), intent(in) :: button
        type(value_type), intent(in) :: data
        type(status_type), intent(out) :: status
        logical :: saved

        saved = help_xref_following
        help_xref_following = .true.
        call help_xref_go_forward(current_buffer(), status)
        help_xref_following = saved
    end subroutine follow_forward

    ! ------------------------------------------------------------------
    ! The stacks
    ! ------------------------------------------------------------------

    !> Install xref info for the help buffer.  Port of
    !> help-mode.el:help-setup-xref.
    recursive module subroutine help_setup_xref(interactive, status, describe, argument)
        logical, intent(in) :: interactive
        type(status_type), intent(out) :: status
        procedure(help_xref_procedure), optional :: describe
        character(len=*), intent(in), optional :: argument
        type(handle_type) :: previous, buffer
        type(help_xref_item_type) :: item
        type(help_xref_item_type), allocatable :: kept(:)
        type(status_type) :: st
        integer :: n

        previous = current_buffer()
        call get_or_make_buffer(help_buffer_name, buffer, status)
        if (.not. status%ok()) return
        call set_buffer(buffer, status)
        if (.not. status%ok()) return
        ! Re-enable major mode, killing all unrelated local vars.
        if (is_derived_mode(major_modes%help_mode)) call help_mode(status)
        if (.not. allocated(help_xref_stack)) allocate (help_xref_stack(0))
        if (.not. allocated(help_xref_forward_stack)) allocate (help_xref_forward_stack(0))

        if (associated(help_xref_stack_item%describe)) then
            item = help_xref_stack_item
            item%position = point()
            help_xref_stack = [help_xref_stack, item]
            deallocate (help_xref_forward_stack)
            allocate (help_xref_forward_stack(0))
        end if

        n = size(help_xref_stack)

        ! Truncate the stack.
        if (interactive .and. n > STACK_LIMIT) then
            kept = help_xref_stack(n - STACK_LIMIT + 1:n)
            call move_alloc(kept, help_xref_stack)
        end if

        help_xref_stack_item%describe => null()
        if (allocated(help_xref_stack_item%argument)) deallocate (help_xref_stack_item%argument)

        if (present(describe)) then
            help_xref_stack_item%describe => describe
            help_xref_stack_item%argument = ""
            if (present(argument)) help_xref_stack_item%argument = argument
        end if

        if (is_buffer_live(previous)) call set_buffer(previous, st)
    end subroutine help_setup_xref

    !> Return to the page ITEM shows, from BUFFER, after recording the
    !> current page as the top of ONTO: what help_xref_go_back and
    !> help_xref_go_forward share.  FROM is the stack ITEM is the top of.
    !> Not a port.
    recursive subroutine go_to_item(buffer, from, onto, status)
        type(handle_type), intent(in) :: buffer
        type(help_xref_item_type), allocatable, intent(inout) :: from(:), onto(:)
        type(status_type), intent(out) :: status
        type(help_xref_item_type) :: item, current
        type(help_xref_item_type), allocatable :: rest(:)
        type(handle_type) :: window
        integer :: n

        call set_buffer(buffer, status)
        if (.not. status%ok()) return
        if (.not. allocated(from)) allocate (from(0))
        if (.not. allocated(onto)) allocate (onto(0))
        current = help_xref_stack_item
        current%position = point()
        onto = [onto, current]
        n = size(from)
        if (n == 0) return
        item = from(n)
        rest = from(1:n - 1)
        call move_alloc(rest, from)
        ! Clear the current item so that it won't get pushed by the
        ! function we're about to call.
        help_xref_stack_item%describe => null()
        if (allocated(help_xref_stack_item%argument)) deallocate (help_xref_stack_item%argument)
        if (.not. associated(item%describe)) return
        call item%describe(item%argument, status)
        if (.not. status%ok()) return
        call set_buffer(buffer, status)
        if (.not. status%ok()) return
        window = get_buffer_window(buffer)

        if (is_window_live(window) .and. .not. same_handle(window, selected_frame%selected_window)) then
            call set_window_point(windows(window%index), item%position, status)
        else
            call goto_char(item%position)
        end if
    end subroutine go_to_item

    module procedure help_xref_go_back
    type(handle_type) :: previous
    type(status_type) :: st

    previous = current_buffer()
    call go_to_item(buffer, help_xref_stack, help_xref_forward_stack, status)
    if (is_buffer_live(previous)) call set_buffer(previous, st)
    end procedure help_xref_go_back

    module procedure help_xref_go_forward
    type(handle_type) :: previous
    type(status_type) :: st

    previous = current_buffer()
    call go_to_item(buffer, help_xref_forward_stack, help_xref_stack, status)
    if (is_buffer_live(previous)) call set_buffer(previous, st)
    end procedure help_xref_go_forward

    module procedure help_go_back
    status = status_type()

    if (allocated(help_xref_stack)) then
        if (size(help_xref_stack) > 0) then
            call help_xref_go_back(current_buffer(), status)
            return
        end if
    end if

    call set_error(status, FM_USER_ERROR, "No previous help buffer")
    end procedure help_go_back

    module procedure help_go_forward
    status = status_type()

    if (allocated(help_xref_forward_stack)) then
        if (size(help_xref_forward_stack) > 0) then
            call help_xref_go_forward(current_buffer(), status)
            return
        end if
    end if

    call set_error(status, FM_USER_ERROR, "No next help buffer")
    end procedure help_go_forward

    ! ------------------------------------------------------------------
    ! Making the buttons
    ! ------------------------------------------------------------------

    !> Make a hyperlink for cross-reference text previously matched.
    !> MATCH_NUMBER is the subexpression of interest in the last matched
    !> regexp.  KIND is the type of button to use.  NAME is passed to
    !> the button's action when it is invoked.
    !> See `help_make_xrefs'.  Port of help-mode.el:help-xref-button (no
    !> `help_clean_buttons').
    recursive subroutine help_xref_button(match_number, kind, name, status)
        integer, intent(in) :: match_number
        type(button_kind_type), intent(in), target :: kind
        character(len=*), intent(in) :: name
        type(status_type), intent(out) :: status
        type(button_type) :: button
        integer(int64) :: beginning, end
        logical :: found

        beginning = match_beginning(match_number, status)
        if (.not. status%ok()) return
        end = match_end(match_number, status)
        if (.not. status%ok()) return
        ! Don't mung properties we've added specially in some instances.
        call button_at(beginning, button, found)
        if (found) return
        call make_text_button(beginning, end, button, status, kind=kind, button_data=value_type(text=name))
    end subroutine help_xref_button

    !> Insert STRING and make a hyperlink from cross-reference text on it.
    !> KIND is the type of button to use.
    !> See `help_make_xrefs'.  Port of help-mode.el:help-insert-xref-button.
    recursive subroutine help_insert_xref_button(string, kind, status)
        character(len=*), intent(in) :: string
        type(button_kind_type), intent(in), target :: kind
        type(status_type), intent(out) :: status
        type(button_type) :: button
        logical :: found

        status = status_type()
        call button_at(point(), button, found)
        if (found) return
        call insert_text_button(string, button, status, kind=kind)
    end subroutine help_insert_xref_button

    !> The match of subexpression N of the last search, or "" if it did not
    !> match.  Not a port.
    function match_string(n) result(text)
        integer, intent(in) :: n
        character(len=:), allocatable :: text
        integer(int64) :: beginning, end
        type(status_type) :: st

        text = ""
        beginning = match_beginning(n, st)
        end = match_end(n, st)
        if (beginning > 0 .and. end >= beginning) call buffer_substring_no_properties(beginning, end, text, st)
    end function match_string

    module procedure help_make_xrefs
    logical :: saved_read_only

    ! (let ((inhibit-read-only t)) ...): the buffer's own flag, cleared
    ! while the buttons are made.
    saved_read_only = buffers(current)%read_only
    buffers(current)%read_only = .false.
    call make_xrefs(status)
    buffers(current)%read_only = saved_read_only
    end procedure help_make_xrefs

    !> The body of help_make_xrefs, the buffer writable.  Port of
    !> help-mode.el:help-make-xrefs.
    recursive subroutine make_xrefs(status)
        type(status_type), intent(out) :: status
        integer(int64) :: saved_point, start, found, left
        logical :: saved_fold
        character(len=:), allocatable :: name
        type(status_type) :: st

        status = status_type()
        saved_point = point()
        call goto_char(point_min())
        ! Skip the first bit, which has already been buttonized.
        left = forward_paragraph(1_int64)
        start = point()
        saved_fold = case_fold_search%value
        case_fold_search = .true.

        ! Quoted names.
        do
            call re_search_forward(help_xref_name_regexp, found, st, noerror=SEARCH_NOERROR)
            if (.not. st%ok() .or. found == 0) exit
            name = match_string(9)
            if (.not. (is_function_known(name) .or. is_variable_known(name))) cycle

            if (len(match_string(8)) > 0) then
                call delete_region(match_beginning(8, st), match_end(8, st), status)
            else if (len(match_string(3)) > 0) then
                ! `variable' &c
                if (is_variable_known(name)) call help_xref_button(9, help_variable, name, status)
            else if (len(match_string(4)) > 0) then
                ! `function' &c
                if (is_function_known(name)) call help_xref_button(9, help_function, name, status)
            else if (len(match_string(5)) > 0 .or. len(match_string(6)) > 0 .or. len(match_string(7)) > 0) then
                ! Nothing for `face' (no face buttons yet), `symbol' and
                ! `source of' (help-function-def is Deferred).
                continue
            else
                call help_xref_button(9, help_name, name, status)
            end if

            if (.not. status%ok()) exit
        end do

        ! An obvious case of a key substitution.
        if (status%ok()) then
            call goto_char(start)

            do
                call re_search_forward(M_X_REGEXP, found, st, noerror=SEARCH_NOERROR)
                if (.not. st%ok() .or. found == 0) exit
                name = match_string(1)
                if (is_function_known(name)) call help_xref_button(1, help_function, name, status)
                if (.not. status%ok()) exit
            end do
        end if

        case_fold_search = saved_fold
        if (.not. status%ok()) return
        ! Delete extraneous newlines at the end of the docstring.
        call goto_char(point_max())

        do while (.not. is_bob() .and. is_bol())
            call delete_region(point() - 1, point(), status)
            if (.not. status%ok()) return
        end do

        call insert(LF, status)
        if (.not. status%ok()) return
        call navigation_buttons(status)
        call goto_char(min(saved_point, point_max()))
    end subroutine make_xrefs

    !> Insert the [back] and [forward] buttons at point, after an empty
    !> line.  Port of help-mode.el:help-xref--navigation-buttons.
    recursive subroutine navigation_buttons(status)
        type(status_type), intent(out) :: status
        logical :: back, forward

        status = status_type()
        back = .false.
        forward = .false.
        if (allocated(help_xref_stack)) back = size(help_xref_stack) > 0
        if (allocated(help_xref_forward_stack)) forward = size(help_xref_forward_stack) > 0

        if (back .or. forward) then
            call ensure_empty_line(status)
            if (.not. status%ok()) return
        end if

        ! Make a back-reference in this buffer if appropriate.
        if (back) then
            call help_insert_xref_button(trim(help_back_label), help_back, status)
            if (.not. status%ok()) return
        end if

        ! Make a forward-reference in this buffer if appropriate.
        if (forward) then
            if (back) call insert(TAB, status)
            call help_insert_xref_button(trim(help_forward_label), help_forward, status)
            if (.not. status%ok()) return
        end if

        if (.not. is_bol()) call insert(LF, status)
    end subroutine navigation_buttons

    !> Ensure that there is exactly one empty line before point, inserting
    !> a newline first if point is not at the beginning of a line.  Port of
    !> subr.el:ensure-empty-lines (LINES 1).
    recursive subroutine ensure_empty_line(status)
        type(status_type), intent(out) :: status
        integer(int64) :: start
        character(len=:), allocatable :: before

        status = status_type()

        if (.not. is_bol()) then
            call insert(LF, status)
            if (.not. status%ok()) return
        end if

        ! Where the newlines before point start: after the last other
        ! character's line end, or at the beginning of the buffer.
        start = point()

        do while (start > point_min())
            call buffer_substring_no_properties(start - 1, start, before, status)
            if (before /= LF) exit
            start = start - 1
        end do

        if (start > point_min()) start = start + 1

        if (point() - start < 1) then
            call insert(LF, status)
        else if (point() - start > 1) then
            call delete_region(start, point() - 1, status)
        end if
    end subroutine ensure_empty_line

    ! ------------------------------------------------------------------
    ! Commands
    ! ------------------------------------------------------------------

    module procedure help_follow_name
    integer(int64) :: pos, moved, beginning
    character(len=:), allocatable :: name

    status = status_type()
    pos = point()
    if (present(position)) pos = position
    ! Check if the name under point is a function or variable.
    call goto_char(pos)
    moved = skip_syntax_backward("w_")
    beginning = point()
    moved = skip_syntax_forward("w_")
    call buffer_substring_no_properties(beginning, point(), name, status)
    call goto_char(pos)
    if (.not. status%ok()) return

    if (is_variable_known(name) .or. is_function_known(name)) then
        call help_do_xref(describe_name, name, status)
    else
        call set_error(status, FM_USER_ERROR, "No name here")
    end if
    end procedure help_follow_name

    !> In help buffer, show docs for the name at point.  Port of
    !> help-mode.el:help-follow-symbol (the command).
    recursive subroutine cmd_help_follow_name(status)
        type(status_type), intent(out) :: status

        call help_follow_name(status, point())
    end subroutine cmd_help_follow_name

    module procedure help_goto_next_page
    integer(int64) :: shortage

    call push_mark()
    call forward_page(status)
    if (.not. status%ok()) return
    if (.not. is_eob()) shortage = forward_line(1_int64)
    end procedure help_goto_next_page

    module procedure help_goto_previous_page
    integer(int64) :: shortage, count
    character(len=:), allocatable :: before

    call push_mark()
    count = 1
    before = ""
    if (point() - 2 >= point_min()) call buffer_substring_no_properties(point() - 2, point(), before, status)
    if (before == FF//LF) count = 2
    call backward_page(status, count)
    if (.not. status%ok()) return
    if (.not. is_bob()) shortage = forward_line(1_int64)
    end procedure help_goto_previous_page
end submodule help_mode_cross_reference_mod
