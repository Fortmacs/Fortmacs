! parenthesis_mod.f90 -- highlight matching paren.
! Ported from GNU Emacs 31.1 lisp/paren.el.
!
! Copyright (C) 1993, 1996, 2001-2026 Free Software Foundation, Inc.
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

!> Show Paren mode: after `show_parenthesis_delay' seconds of idle time, an
!> idle timer (timer_mod) highlights the paren near point and its match
!> with two overlays (interval_tree_mod) in the faces `show_parenthesis_match',
!> `show_parenthesis_match_expression' and `show_parenthesis_mismatch' (faces_mod).
!> The mode is on by default, as upstream (`:init-value t'): bindings_mod
!> turns it on at startup.
!>
!> Deferred: `show_parenthesis_context_when_offscreen' shown in an overlay or a
!> child frame (needs the `display' overlay property and child frames;
!> the echo area is ported), the `shadow' face of "Matches " in the echo
!> area (messages carry no text properties), and buffer tests
!> other than a procedure (buffer-match-p's conditions).
module parenthesis_mod
    use, intrinsic :: iso_fortran_env, only: int32, int64, real64
    use error_mod, only: status_type
    use handle_mod, only: handle_type, null_handle
    use value_mod, only: value_type, xfixnum, vtag, V_INTEGER
    use data_mod, only: is_local_variable, clear_local_variable, default_value, set_default, make_local_variable, &
                        symbol_value, set, append_locals_clearer
    use commands_mod, only: commands
    use hooks_mod, only: hook_type
    use buffer_mod, only: buffers, current, buffer_serial_live
    use editing_functions_mod, only: point, point_minimum, point_maximum, goto_character, narrow_to_region, widen, &
                                     character_after, character_before, line_end_position
    use syntax_mod, only: syntax_entry, syntax_descriptor_type, parse_state_type, syntax_parser_state, scan_expressions, &
                          skip_syntax_backward, skip_characters_backward, parse_expression_ignore_comments, SOPEN, SCLOSE, SMATH
    use interval_tree_mod, only: make_overlay, move_overlay, delete_overlay, overlay_put, is_overlay
    use timer_mod, only: execute_with_idle_timer, destroy_timer, is_timer
    use modes_mod, only: minor_modes
    use derived_mod, only: is_derived_mode
    use simple_mod, only: blink_matching_parenthesis_distance, blink_parenthesis_open_parenthesis_line_string, back_to_indentation
    use minibuffer_mod, only: minibuffer_message
    use window_mod, only: is_position_visible_in_window
    use frame_mod, only: selected_window
    use display_update_mod, only: ding
    use syntax_propertize_mod, only: syntax_propertize
    use data_mod, only: choice_setting_type
    implicit none(type, external)
    private

    public :: show_parenthesis_data_function_set_local, current_show_parenthesis_data_function, &
              is_show_parenthesis_data_function_local, &
              show_parenthesis_data_function_clear_local
    public :: initialize_parenthesis, show_parenthesis_mode, show_parenthesis_local_mode, show_parenthesis_function, &
              show_parenthesis_default, &
              is_show_parenthesis_unescaped, show_parenthesis_categorize_parenthesis, show_parenthesis_locate_near_parenthesis, &
              is_show_parenthesis_enabled, show_parenthesis_default_test, show_parenthesis_data_procedure, &
              show_parenthesis_test_procedure

    !> Values of `show_parenthesis_style' (it takes its constants only).
    integer, parameter, public :: SHOW_PARENTHESIS_STYLE_PARENTHESIS = 2, SHOW_PARENTHESIS_STYLE_EXPRESSION = 3, &
                                  SHOW_PARENTHESIS_STYLE_MIXED = 4

    !> Values of `show_parenthesis_not_in_comments_or_strings': never highlight
    !> parens inside comments and strings (.true.), don't highlight
    !> mismatched parens there (SHOW_PARENTHESIS_NOT_IN_COMMENTS_OR_STRINGS_ON_MISMATCH), always highlight parens
    !> (.false.).
    integer, parameter, public :: SHOW_PARENTHESIS_NOT_IN_COMMENTS_OR_STRINGS_ON_MISMATCH = 2

    !> Style used when showing a matching paren.
    !> Valid styles are `SHOW_PARENTHESIS_STYLE_PARENTHESIS' (meaning show the matching
    !> paren), `SHOW_PARENTHESIS_STYLE_EXPRESSION' (meaning show the entire expression
    !> enclosed by the paren) and `SHOW_PARENTHESIS_STYLE_MIXED' (meaning show the
    !> matching paren if it is visible, and the expression otherwise).
    !> Port of paren.el show-paren-style.
    type(choice_setting_type), public :: show_parenthesis_style = &
                                       choice_setting_type(name="show_parenthesis_style", true_valid=.false., false_valid=.false., &
                                                             choice=SHOW_PARENTHESIS_STYLE_PARENTHESIS)

    !> Time in seconds to delay before showing a matching paren.
    !> If you change this while `show_parenthesis_mode' is active, you must
    !> toggle the mode off and on again for this to take effect.  Port of
    !> paren.el show-paren-delay.
    real, public :: show_parenthesis_delay = 0.125

    !> Priority of paren highlighting overlays.  Port of paren.el
    !> show-paren-priority.
    integer, public :: show_parenthesis_priority = 1000

    !> If true, beep if mismatched paren is detected.  Port of paren.el
    !> show-paren-ring-bell-on-mismatch.
    logical, public :: show_parenthesis_ring_bell_on_mismatch = .false.

    !> If true, show parens when point is just inside one.
    !> This will only be done when point isn't also just outside a paren.
    !> Port of paren.el show-paren-when-point-inside-paren.
    logical, public :: show_parenthesis_when_point_inside_parenthesis = .false.

    !> If true, show parens when point is in the line's periphery.
    !> The periphery is at the beginning or end of a line or in any
    !> whitespace there.  Port of paren.el show-paren-when-point-in-periphery.
    logical, public :: show_parenthesis_when_point_in_periphery = .false.

    !> True turns on openparen highlighting when matching forward.
    !> When false, and point stands just before an open paren, the paren
    !> is not highlighted, the cursor being regarded as adequate to mark
    !> its position.  Port of paren.el show-paren-highlight-openparen.
    logical, public :: show_parenthesis_highlight_openparen = .true.

    !> If true, show context around the opening paren if it is offscreen.
    !> The context is usually the line that contains the openparen,
    !> except if the openparen is on its own line, in which case the
    !> context includes the previous nonblank line.
    !>
    !> The context is shown in the echo area.  Port of paren.el
    !> show-paren-context-when-offscreen (the values `overlay' and
    !> `child-frame' are not ported).
    logical, public :: show_parenthesis_context_when_offscreen = .false.

    !> If .true., never highlight parens inside
    !> comments and strings.  If `SHOW_PARENTHESIS_NOT_IN_COMMENTS_OR_STRINGS_ON_MISMATCH',
    !> do not highlight mismatched parens inside comments and strings.  If
    !> .false. (the default), always highlight
    !> parens wherever they are.  Port of paren.el
    !> show-paren-not-in-comments-or-strings.
    type(choice_setting_type), public :: show_parenthesis_not_in_comments_or_strings = &
                                         choice_setting_type(name="show_parenthesis_not_in_comments_or_strings")

    !> What `show_parenthesis_data_function' finds: the opener/closer "near"
    !> point from HERE_BEGINNING to HERE_END, its match from THERE_BEGINNING to
    !> THERE_END (0 for none) and whether they mismatch.
    type, public :: show_parenthesis_data_type
        integer(int64) :: here_beginning = 0, here_end = 0, there_beginning = 0, there_end = 0
        logical :: mismatch = .false.
    end type show_parenthesis_data_type

    abstract interface
        !> A `show_parenthesis_data_function': FOUND is false if there's no
        !> opener/closer near point, else DATA describes it.
        !> Not a port.
        subroutine show_parenthesis_data_procedure(found, data)
            import :: show_parenthesis_data_type
            implicit none(type, external)
            logical, intent(out) :: found
            type(show_parenthesis_data_type), intent(out) :: data
        end subroutine show_parenthesis_data_procedure

        !> A `show_parenthesis_test_function': whether to use `show_parenthesis_mode' in the
        !> current buffer.  Not a port.
        logical function show_parenthesis_test_procedure()
            implicit none(type, external)
        end function show_parenthesis_test_procedure
    end interface

    !> Function to find the opener/closer "near" point and its match.
    !> The function is called with no argument and should give either
    !> FOUND false if there's no opener/closer near point, or the
    !> positions HERE_BEGINNING HERE_END THERE_BEGINNING THERE_END and MISMATCH,
    !> where HERE_BEGINNING..HERE_END is expected to be near point.  Port of
    !> paren.el show-paren-data-function.
    procedure(show_parenthesis_data_procedure), pointer, public :: show_parenthesis_data_function => null()

    ! A buffer's own `show_parenthesis_data_function' (FN null is nil).
    type :: data_entry_type
        integer(int64) :: serial = 0
        procedure(show_parenthesis_data_procedure), pointer, nopass :: fn => null()
    end type data_entry_type

    ! Buffers' own values of `show_parenthesis_data_function'.
    type(data_entry_type), allocatable :: data_locals(:)

    !> Whether to use `show_parenthesis_mode' in a buffer.
    !> The default is to enable the mode in all buffers that don't
    !> derive from `special_mode', which means that it's on (by default)
    !> in all editing buffers.
    !> The test is called in the buffer.  Port of paren.el
    !> show-paren-predicate (a procedure; buffer-match-p's conditions are
    !> not ported).
    procedure(show_parenthesis_test_procedure), pointer, public :: show_parenthesis_test_function => null()

    !> Hook run after entering or leaving `show_parenthesis_mode'.  Port of the
    !> show-paren-mode-hook define-minor-mode defines.
    type(hook_type), target, public :: show_parenthesis_mode_hook = hook_type(name="show_parenthesis_mode_hook")

    !> Hook run after entering or leaving `show_parenthesis_local_mode'.
    !> Port of the show-paren-local-mode-hook define-minor-mode defines.
    type(hook_type), target, public :: show_parenthesis_local_mode_hook = hook_type(name="show_parenthesis_local_mode_hook")

    !> show-paren--idle-timer, show-paren--overlay (the matching paren)
    !> and show-paren--overlay-1 (the paren at point).
    type(handle_type) :: idle_timer, overlay, overlay_1

contains

    ! The index in `data_locals' of the current buffer's entry, 0 if none.
    ! Not a port.
    integer function data_local_index() result(i)
        i = 0
        if (current == 0 .or. .not. allocated(data_locals)) return
        i = findloc(data_locals%serial, buffers(current)%serial, dim=1)
    end function data_local_index

    !> Give the current buffer its own `show_parenthesis_data_function'
    !> FN (set_local); absent clears it.  Not a port.
    subroutine show_parenthesis_data_function_set_local(fn)
        procedure(show_parenthesis_data_procedure), optional :: fn
        type(data_entry_type) :: entry
        integer :: i

        if (current == 0) return
        if (.not. allocated(data_locals)) allocate (data_locals(0))
        i = data_local_index()

        if (i == 0) then
            do i = 1, size(data_locals)
                if (.not. buffer_serial_live(data_locals(i)%serial)) exit
            end do

            if (i > size(data_locals)) data_locals = [data_locals, entry]
            data_locals(i)%serial = buffers(current)%serial
        end if

        if (present(fn)) then
            data_locals(i)%fn => fn
        else
            data_locals(i)%fn => null()
        end if
    end subroutine show_parenthesis_data_function_set_local

    !> The value of `show_parenthesis_data_function' in the current buffer
    !> (null when unset).  Not a port.
    function current_show_parenthesis_data_function() result(fn)
        procedure(show_parenthesis_data_procedure), pointer :: fn
        integer :: i

        i = data_local_index()

        if (i > 0) then
            fn => data_locals(i)%fn
        else
            fn => show_parenthesis_data_function
        end if
    end function current_show_parenthesis_data_function

    !> Whether the current buffer has its own `show_parenthesis_data_function'.
    !> Not a port.
    logical function is_show_parenthesis_data_function_local()
        is_show_parenthesis_data_function_local = data_local_index() > 0
    end function is_show_parenthesis_data_function_local

    !> Make the current buffer use the default `show_parenthesis_data_function'
    !> (also what `clear_all_local_variables' does to it).  Not a port.
    subroutine show_parenthesis_data_function_clear_local()
        integer :: i

        i = data_local_index()
        if (i == 0) return
        data_locals(i)%serial = 0
        data_locals(i)%fn => null()
    end subroutine show_parenthesis_data_function_clear_local

    !> Define Show Paren mode and Show Paren Local mode (as loading
    !> paren.el does).  Not a port.
    subroutine initialize_parenthesis()
        call minor_modes%show_parenthesis_mode%define("show_parenthesis_mode", global=.true., body=show_parenthesis_mode_body, &
                                                      hook=show_parenthesis_mode_hook)
        commands%show_parenthesis_mode%execute => show_parenthesis_mode
        call minor_modes%show_parenthesis_local_mode%define("show_parenthesis_local_mode", body=show_parenthesis_local_mode_body, &
                                                          hook=show_parenthesis_local_mode_hook, getter=is_show_parenthesis_enabled)
        commands%show_parenthesis_local_mode%execute => show_parenthesis_local_mode
        if (.not. associated(show_parenthesis_data_function)) show_parenthesis_data_function => show_parenthesis_default
        call append_locals_clearer(show_parenthesis_data_function_clear_local)
        if (.not. associated(show_parenthesis_test_function)) show_parenthesis_test_function => show_parenthesis_default_test
    end subroutine initialize_parenthesis

    !> Toggle visualization of matching parens (Show Paren mode).
    !>
    !> When enabled, any matching parenthesis is highlighted in `show_parenthesis_style'
    !> after `show_parenthesis_delay' seconds of Fortmacs idle time.
    !>
    !> Also see `show_parenthesis_test_function', which controls which buffers
    !> this mode is enabled in.
    !>
    !> This is a global minor mode.  To toggle the mode in a single buffer,
    !> use `show_parenthesis_local_mode'.  Port of paren.el:show-paren-mode.
    subroutine show_parenthesis_mode(status)
        type(status_type), intent(out) :: status

        call minor_modes%show_parenthesis_mode%command(status)
    end subroutine show_parenthesis_mode

    !> The body of Show Paren mode: enable or disable the mechanism.
    !> Port of paren.el:show-paren-mode in part.
    subroutine show_parenthesis_mode_body(on, status)
        logical, intent(in) :: on
        type(status_type), intent(out) :: status

        status = status_type()
        if (on) continue
        ! First get rid of the old idle timer.
        if (is_timer(idle_timer)) call destroy_timer(idle_timer)
        idle_timer = null_handle()
        call execute_with_idle_timer(idle_timer, real(show_parenthesis_delay, real64), .true., show_parenthesis_function_timer, &
                                     status)
        if (.not. minor_modes%show_parenthesis_mode%state%value) call show_parenthesis_delete_overlays()
    end subroutine show_parenthesis_mode_body

    !> Port of paren.el:show-paren--delete-overlays.
    subroutine show_parenthesis_delete_overlays()
        call delete_overlay(overlay)
        call delete_overlay(overlay_1)
    end subroutine show_parenthesis_delete_overlays

    !> Toggle `show_parenthesis_mode' only in this buffer.  Port of
    !> paren.el:show-paren-local-mode.
    subroutine show_parenthesis_local_mode(status)
        type(status_type), intent(out) :: status

        call minor_modes%show_parenthesis_local_mode%command(status)
    end subroutine show_parenthesis_local_mode

    !> The body of Show Paren Local mode with its new state VALUE: the
    !> setter (`show_parenthesis_mode' set locally to VALUE), then the body.
    !> Port of paren.el:show-paren-local-mode in part.
    subroutine show_parenthesis_local_mode_body(value, status)
        logical, intent(in) :: value
        type(status_type), intent(out) :: status
        type(value_type) :: default
        type(status_type) :: st

        status = status_type()
        call minor_modes%show_parenthesis_mode%state%set_local(value)
        call default_value(value_type(name="show_parenthesis_mode"), default, st)

        if (minor_modes%show_parenthesis_mode%state%value .eqv. allocated(default%a)) then
            if (.not. minor_modes%show_parenthesis_mode%state%value) then
                call show_parenthesis_delete_overlays()
                call clear_local_variable(value_type(name="show_parenthesis_mode"), st)
            end if
        else if (.not. allocated(default%a)) then
            ! Locally enabled, but globally disabled.
            call minor_modes%show_parenthesis_mode%enable(status)
            ! But keep it globally disabled.
            call set_default(value_type(name="show_parenthesis_mode"), value_type(), st)
        else
            ! Locally disabled only.
            call show_parenthesis_delete_overlays()
        end if
    end subroutine show_parenthesis_local_mode_body

    !> Determine whether the paren after POSITION is unescaped.  Port of
    !> paren.el:show-paren--unescaped-p.
    logical function is_show_parenthesis_unescaped(position) result(unescaped)
        integer(int64), intent(in) :: position
        integer(int64) :: original_point, moved

        original_point = point()
        call goto_character(position)
        moved = skip_syntax_backward("/\")
        unescaped = iand(abs(moved), 1_int64) == 0
        call goto_character(original_point)
    end function is_show_parenthesis_unescaped

    !> The syntax class of the character after POSITION, or -1 if there is none
    !> (syntax-class of syntax-after).  Not a port.
    integer function syntax_class_after(position) result(syntax_class)
        integer(int64), intent(in) :: position
        integer :: c
        logical :: found
        type(syntax_descriptor_type) :: d

        syntax_class = -1
        if (position < point_minimum() .or. position >= point_maximum()) return
        call character_after(c, found, position)
        if (.not. found) return
        d = syntax_entry(c)
        syntax_class = iand(d%code, 255)
    end function syntax_class_after

    !> The matching character of the syntax of the character after POSITION
    !> (the matching character of `syntax_after'), or -1 for none.  Not a port.
    integer function syntax_match_after(position) result(m)
        integer(int64), intent(in) :: position
        integer :: c
        logical :: found
        type(syntax_descriptor_type) :: d

        m = -1
        if (position < point_minimum() .or. position >= point_maximum()) return
        call character_after(c, found, position)

        if (found) then
            d = syntax_entry(c)
            m = d%match
        end if
    end function syntax_match_after

    !> Determine whether the character after POSITION has paren syntax,
    !> and if so, give DIRECTORY 1 for an open paren, -1 for a close paren, and
    !> OUTSIDE the buffer position of the outside of the paren.  If the
    !> character isn't a paren, or it is an escaped paren, DIRECTORY is 0
    !> Port of paren.el:show-paren--categorize-paren.
    subroutine show_parenthesis_categorize_parenthesis(position, directory, outside)
        integer(int64), intent(in) :: position
        integer, intent(out) :: directory
        integer(int64), intent(out) :: outside

        directory = 0
        outside = 0

        if (syntax_class_after(position) == SOPEN) then
            if (is_show_parenthesis_unescaped(position)) then
                directory = 1
                outside = position
            end if
        else if (syntax_class_after(position) == SCLOSE) then
            if (is_show_parenthesis_unescaped(position)) then
                directory = -1
                outside = position + 1
            end if
        end if
    end subroutine show_parenthesis_categorize_parenthesis

    !> Locate an unescaped paren "near" point to show.
    !> If one is found, give DIRECTORY 1 for an open paren, -1 for a close
    !> paren, and OUTSIDE the buffer position of the outside of the paren.
    !> Otherwise DIRECTORY is 0.  Port of paren.el:show-paren--locate-near-paren.
    subroutine show_parenthesis_locate_near_parenthesis(directory, outside)
        integer, intent(out) :: directory
        integer(int64), intent(out) :: outside
        integer(int64) :: ind_position, end_of_line_position, original_point, moved, before_outside, after_outside
        integer :: before_directory, after_directory
        type(status_type) :: st

        original_point = point()
        call back_to_indentation()
        ind_position = point()
        call goto_character(line_end_position())
        moved = skip_characters_backward(" "//achar(9), st, ind_position)
        end_of_line_position = point()
        call goto_character(original_point)
        call show_parenthesis_categorize_parenthesis(original_point - 1, before_directory, before_outside)
        call show_parenthesis_categorize_parenthesis(original_point, after_directory, after_outside)
        directory = 0
        outside = 0

        if (before_directory == -1) then
            ! Point is immediately outside a paren.
            directory = before_directory
            outside = before_outside
        else if (after_directory == 1) then
            directory = after_directory
            outside = after_outside
        else if (show_parenthesis_when_point_inside_parenthesis .and. before_directory /= 0) then
            ! Point is immediately inside a paren.
            directory = before_directory
            outside = before_outside
        else if (show_parenthesis_when_point_inside_parenthesis .and. after_directory /= 0) then
            directory = after_directory
            outside = after_outside
        else if (show_parenthesis_when_point_in_periphery .and. original_point <= ind_position) then
            ! Point is in the whitespace before the code.
            call show_parenthesis_categorize_parenthesis(ind_position, directory, outside)
            if (directory == 0) call show_parenthesis_categorize_parenthesis(end_of_line_position - 1, directory, outside)
        else if (show_parenthesis_when_point_in_periphery .and. original_point >= end_of_line_position) then
            ! Point is in the whitespace after the code.
            call show_parenthesis_categorize_parenthesis(end_of_line_position - 1, directory, outside)
        end if
    end subroutine show_parenthesis_locate_near_parenthesis

    !> Find the opener/closer near point and its match.
    !>
    !> It is the default value of `show_parenthesis_data_function'.  Port of
    !> paren.el:show-paren--default.
    subroutine show_parenthesis_default(found, data)
        logical, intent(out) :: found
        type(show_parenthesis_data_type), intent(out) :: data
        type(parse_state_type) :: parser_state
        type(status_type) :: st
        integer(int64) :: outside, position, back, beginning, end, original_point, savedb, savedz
        integer :: directory, cb, ca
        logical :: saved_ignore, mismatch, has_position, fb, fa

        found = .false.
        call show_parenthesis_locate_near_parenthesis(directory, outside)
        ! If we're inside a comment, then we probably want to blink a
        ! matching parentheses in the comment.  So don't ignore comments
        ! in that case.
        original_point = point()
        call syntax_parser_state(parser_state)
        call goto_character(original_point)
        saved_ignore = parse_expression_ignore_comments%value
        if (parser_state%incomment /= 0) parse_expression_ignore_comments%value = .false.

        ! Find the other end of the expression.
        if (directory /= 0) then
            if (directory == 1) then
                data%here_beginning = outside
                data%here_end = outside + 1
            else
                data%here_beginning = outside - 1
                data%here_end = outside
            end if

            savedb = buffers(current)%beginning_of_accessible_portion
            savedz = buffers(current)%text%z - buffers(current)%end_of_accessible_portion

            ! Determine the range within which to look for a match.
            if (blink_matching_parenthesis_distance >= 0) then
                beginning = max(point_minimum(), point() - blink_matching_parenthesis_distance)
                ! `syntax_propertize' can't widen so make sure it won't
                ! need to (bug#81035).
                call syntax_propertize(beginning, st)
                call narrow_to_region(beginning, min(point_maximum(), point() + blink_matching_parenthesis_distance), st)
            end if

            ! Scan across one expression within that range.  Errors or nil mean
            ! there is a mismatch.
            mismatch = .false.
            has_position = .false.
            position = scan_expressions(outside, int(directory, int64), st)

            if (.not. st%ok()) then
                mismatch = .true.
            else if (position /= 0) then
                has_position = .true.
            end if

            ! Move back the other way and verify we get back to the
            ! starting point.  If not, these two parens don't really
            ! match.  Maybe the one at point is escaped and doesn't really
            ! count, or one is inside a comment.
            if (has_position) then
                back = scan_expressions(position, int(-directory, int64), st)
                if (.not. st%ok() .or. back /= outside) has_position = .false.
            end if

            ! If found a "matching" paren, see if it is the right kind of
            ! paren to match the one we started at.
            if (.not. has_position) then
                if (mismatch) then
                    found = .true.
                    data%mismatch = .true.
                end if
            else
                beginning = min(position, outside)
                end = max(position, outside)

                if (syntax_class_after(beginning) /= SMATH) then
                    call character_before(cb, fb, end)
                    call character_after(ca, fa, beginning)
                    mismatch = .true.

                    ! (The cdrs can give nil.)
                    if (fb .and. syntax_match_after(beginning) >= 0) then
                        if (cb == syntax_match_after(beginning)) mismatch = .false.
                    end if

                    if (fa .and. syntax_match_after(end - 1) >= 0) then
                        if (ca == syntax_match_after(end - 1)) mismatch = .false.
                    end if

                    ! The cdr might hold a new paren-class info rather
                    ! than a matching-char info, in which case the two
                    ! CDRs should match.
                    if (syntax_match_after(end - 1) == syntax_match_after(beginning)) mismatch = .false.
                end if

                found = .true.

                if (directory == 1) then
                    data%there_beginning = position - 1
                    data%there_end = position
                else
                    data%there_beginning = position
                    data%there_end = position + 1
                end if

                data%mismatch = mismatch
            end if

            call widen()
            call narrow_to_region(savedb, buffers(current)%text%z - savedz, st)
        end if

        parse_expression_ignore_comments%value = saved_ignore
    end subroutine show_parenthesis_default

    !> Whether Show Paren mode is on in the current buffer.  Port of
    !> paren.el:show-paren--enabled-p.
    logical function is_show_parenthesis_enabled() result(enabled)
        enabled = minor_modes%show_parenthesis_mode%state%value
        if (.not. enabled) return
        ! If we're using `show_parenthesis_local_mode', then always heed the
        ! value.
        if (is_local_variable(value_type(name="show_parenthesis_mode"))) return
        ! If not, check that the test matches.
        if (associated(show_parenthesis_test_function)) enabled = show_parenthesis_test_function()
    end function is_show_parenthesis_enabled

    !> The default `show_parenthesis_test_function': true unless the major mode
    !> derives from `special_mode' (paren.el's (not (derived-mode
    !> . special-mode))).  Not a port.
    logical function show_parenthesis_default_test() result(usable)
        usable = .not. is_derived_mode(["special_mode"])
    end function show_parenthesis_default_test

    !> Whether position POSITION is in a string or comment: the context of
    !> syntax-ppss-context (syntax-ppss POSITION), keeping point.  Not a port.
    logical function in_string_or_comment(position)
        integer(int64), intent(in) :: position
        type(parse_state_type) :: parser_state
        integer(int64) :: original_point

        original_point = point()
        call syntax_parser_state(parser_state, position)
        call goto_character(original_point)
        in_string_or_comment = parser_state%instring >= 0 .or. parser_state%incomment /= 0
    end function in_string_or_comment

    !> The overlays, made (empty and deleted) on first use, with
    !> front-advance, as upstream's `make-overlay' at point with FRONT-ADVANCE.
    !> Not a port.
    subroutine ensure_overlays()
        type(status_type) :: st

        if (.not. is_overlay(overlay)) then
            call make_overlay(overlay, point(), point(), st, front_advance=.true.)
            call delete_overlay(overlay)
        end if

        if (.not. is_overlay(overlay_1)) then
            call make_overlay(overlay_1, point(), point(), st, front_advance=.true.)
            call delete_overlay(overlay_1)
        end if
    end subroutine ensure_overlays

    !> `show_parenthesis_function' as the idle timer's function.  Not a port.
    subroutine show_parenthesis_function_timer(status)
        type(status_type), intent(out) :: status

        call show_parenthesis_function(status)
    end subroutine show_parenthesis_function_timer

    !> The last position of point for which `show_parenthesis_function' was
    !> called (show-paren--last-pos, buffer-local), or 0 for none.
    !> Not a port.
    integer(int64) function last_position()
        type(value_type) :: v
        type(status_type) :: st

        last_position = 0
        if (.not. is_local_variable(value_type(name="show_parenthesis_last_position"))) return
        call symbol_value(value_type(name="show_parenthesis_last_position"), v, st)
        if (vtag(v) == V_INTEGER) last_position = xfixnum(v)
    end function last_position

    ! Not a port.
    subroutine set_last_position(position)
        integer(int64), intent(in) :: position
        type(status_type) :: st

        call make_local_variable(value_type(name="show_parenthesis_last_position"), st)
        call set(value_type(name="show_parenthesis_last_position"), value_type(position), st)
    end subroutine set_last_position

    !> Highlight the parentheses until the next input arrives.  Port of
    !> paren.el:show-paren-function.
    subroutine show_parenthesis_function(status)
        type(status_type), intent(out) :: status
        type(show_parenthesis_data_type) :: data
        character(len=:), allocatable :: face, context
        integer(int64) :: closest, openparen
        logical :: found, highlight_expression
        procedure(show_parenthesis_data_procedure), pointer :: data_fn
        type(status_type) :: st

        status = status_type()
        if (current == 0) return
        call ensure_overlays()
        found = .false.

        if (is_show_parenthesis_enabled()) then
            data_fn => current_show_parenthesis_data_function()
            if (associated(data_fn)) call data_fn(found, data)
        end if

        if (.not. found) then
            ! If show-paren-mode is nil in this buffer or if not at a paren
            ! that has a match, turn off any previous paren highlighting.
            call delete_overlay(overlay)
            call delete_overlay(overlay_1)
            call set_last_position(point())
            return
        end if

        ! Found something to highlight.
        highlight_expression = show_parenthesis_style%choice == SHOW_PARENTHESIS_STYLE_EXPRESSION

        if (.not. highlight_expression .and. data%there_beginning /= 0 .and. show_parenthesis_style%choice &
            == SHOW_PARENTHESIS_STYLE_MIXED) then
            if (data%there_beginning < data%here_beginning) then
                closest = data%there_end - 1
            else
                closest = data%there_beginning + 1
            end if

            highlight_expression = .not. is_position_visible_in_window(closest, selected_window())
        end if

        if (data%mismatch) then
            if (show_parenthesis_not_in_comments_or_strings%choice == SHOW_PARENTHESIS_NOT_IN_COMMENTS_OR_STRINGS_ON_MISMATCH) then
                if (in_string_or_comment(data%here_beginning)) return
            end if

            if (show_parenthesis_ring_bell_on_mismatch) call ding()
            face = "show_parenthesis_mismatch"
        else if (highlight_expression) then
            face = "show_parenthesis_match_expression"
        else
            face = "show_parenthesis_match"
        end if

        if (show_parenthesis_not_in_comments_or_strings%value) then
            if (in_string_or_comment(data%here_beginning)) return
        end if

        ! If matching backwards, highlight the closeparen before point as
        ! well as its matching open.  If matching forward, and the
        ! openparen is unbalanced, highlight the paren at point to
        ! indicate misbalance.  Otherwise, turn off any such highlighting.
        if (data%here_beginning == 0 .or. (.not. show_parenthesis_highlight_openparen .and. data%here_end > point() .and. &
                                           data%here_beginning <= point() .and. data%there_beginning /= 0)) then
            call delete_overlay(overlay_1)
        else
            call move_overlay(overlay_1, data%here_beginning, data%here_end, st)
            ! Always set the overlay face, since it varies.
            call overlay_put(overlay_1, "priority", value_type(int(show_parenthesis_priority, int64)), st)
            call overlay_put(overlay_1, "face", value_type(name=face), st)
        end if

        ! Turn on highlighting for the matching paren, if found.  If it's
        ! an unmatched paren, turn off any such highlighting.
        if (data%there_beginning == 0) then
            call delete_overlay(overlay)
            return
        end if

        if (highlight_expression) then
            if (data%there_beginning < data%here_beginning) then
                call move_overlay(overlay, data%here_end, data%there_beginning, st)
            else
                call move_overlay(overlay, data%here_beginning, data%there_end, st)
            end if
        else
            call move_overlay(overlay, data%there_beginning, data%there_end, st)
        end if

        ! If `show_parenthesis_context_when_offscreen' is true and point is at a
        ! closing paren, show the context around the opening paren.
        openparen = min(data%here_beginning, data%there_beginning)

        if (show_parenthesis_context_when_offscreen .and. last_position() /= point() &
            .and. data%there_beginning < data%here_beginning) then
            if (.not. is_position_visible_in_window(openparen, selected_window())) then
                context = blink_parenthesis_open_parenthesis_line_string(openparen)
                call minibuffer_message("Matches "//context)
            end if
        end if

        call set_last_position(point())
        ! Always set the overlay face, since it varies.
        call overlay_put(overlay, "priority", value_type(int(show_parenthesis_priority, int64)), st)
        call overlay_put(overlay, "face", value_type(name=face), st)
    end subroutine show_parenthesis_function
end module parenthesis_mod
