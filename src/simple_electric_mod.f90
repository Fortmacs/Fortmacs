! simple_electric_mod.f90 -- blinking parens and newline-and-indent, a submodule of simple_mod.
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

!> Blinking matching parens (`blink_matching_open' from
!> `post_self_insert_hook') and `newline_and_indent': a submodule because
!> they read paren.el's and electric.el's modes (parenthesis_mod, modes_mod),
!> which use simple_mod.
!>
!> Deferred: `blink_matching_parenthesis_highlight_offscreen' and the face
!> `blink_matching_parenthesis_offscreen' (strings carry no text properties),
!> the `shadow' face of "Matches " in the echo area
!> and `noninteractive' (no batch mode).
submodule(simple_mod) simple_electric_mod
    use value_mod, only: vtag, V_INTEGER, value_type
    use syntax_mod, only: syntax_entry, syntax_descriptor_type, skip_syntax_backward, parse_expression_ignore_comments, &
                          character_syntax, SCLOSE, SQUOTE
    use expression_motion_mod, only: forward_expression
    use syntax_propertize_mod, only: syntax_propertize
    use editing_functions_mod, only: narrow_to_region, widen
    use window_mod, only: window_position_visible_p => is_position_visible_in_window
    use frame_mod, only: selected_window
    use interval_tree_mod, only: make_overlay, move_overlay, delete_overlay, overlay_put, is_overlay
    use parenthesis_mod, only: show_parenthesis_context_when_offscreen
    use modes_mod, only: minor_modes
    use minibuffer_mod, only: minibuffer_message
    use font_lock_mod, only: font_lock_ensure
    use editing_commands_mod, only: post_self_insert_hook
    use indent_mod, only: indent_according_to_mode
    use keyboard_mod, only: is_executing_keyboard_macro, sit_for
    implicit none(type, external)

    !> Overlay used to highlight the matching paren.  Port of simple.el
    !> blink-matching--overlay (made on first use).
    type(handle_type) :: blink_matching_overlay

contains

    module procedure initialize_simple_electric
    commands%blink_matching_open%execute => execute_blink_matching_open
    commands%newline_and_indent%execute => execute_newline_and_indent
    commands%reindent_then_newline_and_indent%execute => execute_reindent_then_newline_and_indent
    commands%backward_delete_character_untabify%execute => execute_backward_delete_character_untabify

    if (.not. associated(blink_matching_check_function%procedure)) then
        blink_matching_check_function%procedure => blink_matching_check_mismatch
    end if

    if (.not. associated(blink_parenthesis_function)) blink_parenthesis_function => blink_matching_open
    ! Most likely, this hook is nil, so this arg doesn't matter, but I use
    ! it as a reminder that this function usually likes to be run after
    ! others since it does `sit_for'.  That's also the reason it get a
    ! `priority' prop of 100.
    call post_self_insert_hook%append(blink_parenthesis_post_self_insert_function)
    end procedure initialize_simple_electric

    !> The syntax descriptor of the character after POSITION (syntax-after):
    !> FOUND false outside the accessible region.  Port of
    !> subr.el:syntax-after.
    subroutine syntax_after(position, d, found)
        integer(int64), intent(in) :: position
        type(syntax_descriptor_type), intent(out) :: d
        logical, intent(out) :: found
        integer(int32) :: c

        found = .false.
        if (position < point_minimum() .or. position >= point_maximum()) return
        call character_after(c, found, position)
        if (found) d = syntax_entry(int(c))
    end subroutine syntax_after

    module procedure blink_matching_check_mismatch
    type(syntax_descriptor_type) :: end_syntax, start_syntax
    integer(int32) :: c
    integer :: matching_parenthesis
    logical :: found, has_start

    mismatch = .false.
    call syntax_after(end - 1, end_syntax, found)
    matching_parenthesis = -1

    if (found) then
        if (iand(end_syntax%code, 255) == SCLOSE) matching_parenthesis = end_syntax%match
    end if

    ! For self-matched chars like " and $, we can't know when they're
    ! mismatched or unmatched, so we can do it only for parens.
    if (matching_parenthesis < 0) return
    mismatch = .true.
    if (start == 0) return
    call character_after(c, found, start)
    if (found .and. c == matching_parenthesis) mismatch = .false.
    ! The cdr might hold a new paren-class info rather than a
    ! matching-char info, in which case the two CDRs should match.
    call syntax_after(start, start_syntax, has_start)

    if (has_start) then
        if (start_syntax%match == matching_parenthesis) mismatch = .false.
    end if
    end procedure blink_matching_check_mismatch

    !> Momentarily highlight the beginning of the expression before point.
    !> Port of simple.el:blink-matching-open (the command).
    subroutine execute_blink_matching_open(status)
        type(status_type), intent(out) :: status

        call blink_matching_open(status)
    end subroutine execute_blink_matching_open

    module procedure blink_matching_open
    integer(int64) :: oldpos, blinkpos, savedb, savedz, original_point
    type(syntax_descriptor_type) :: code
    type(status_type) :: st
    logical :: saved_ignore, found, mismatch, visible
    procedure(blink_matching_check_procedure), pointer :: check

    status = status_type()
    if (is_beginning_of_buffer() .or. blink_matching_parenthesis%is_off()) return
    oldpos = point()
    ! (save-excursion (save-restriction ...))
    savedb = buffers(current)%beginning_of_accessible_portion
    savedz = buffers(current)%text%z - buffers(current)%end_of_accessible_portion
    call syntax_propertize(point(), st)

    if (blink_matching_parenthesis_distance >= 0) then
        ! (point-min) unless minibuf.
        call narrow_to_region(max(minibuffer_prompt_end(), point() - blink_matching_parenthesis_distance), oldpos, st)
    end if

    saved_ignore = parse_expression_ignore_comments%value
    parse_expression_ignore_comments%value = parse_expression_ignore_comments%value .and. &
                                             .not. blink_matching_parenthesis_dont_ignore_comments
    blinkpos = 0
    call forward_expression(-1_int64, st)

    if (st%ok()) then
        ! backward-sexp skips backward over prefix chars, so move back to
        ! the matching paren.
        do while (point() < oldpos - 1)
            call syntax_after(point(), code, found)
            if (.not. found) exit
            if (iand(code%code, 255) /= SQUOTE .and. iand(code%code, 1048576) /= 1048576) exit
            call goto_character(point() + 1)
        end do

        blinkpos = point()
    end if

    parse_expression_ignore_comments%value = saved_ignore
    call widen()
    call narrow_to_region(savedb, buffers(current)%text%z - savedz, st)
    call goto_character(oldpos)
    mismatch = .false.
    check => blink_matching_check_function%get_current()
    if (associated(check)) mismatch = check(blinkpos, oldpos)

    if (mismatch) then
        if (blinkpos /= 0) then
            if (is_minibuffer()) then
                call minibuffer_message("Mismatched parentheses")
            else
                call message("Mismatched parentheses")
            end if
        else
            if (is_minibuffer()) then
                call minibuffer_message("No matching parenthesis found")
            else
                call message("No matching parenthesis found")
            end if
        end if

        return
    end if

    if (blinkpos == 0) return
    visible = blink_matching_parenthesis%choice == BLINK_MATCHING_PARENTHESIS_JUMP_OFFSCREEN
    if (.not. visible) visible = window_position_visible_p(blinkpos, selected_window())

    if (visible) then
        ! Matching open within window, temporarily move to or highlight
        ! char after blinkpos but only if `blink_matching_parenthesis_on_screen'
        ! is true.
        if (.not. blink_matching_parenthesis_on_screen .or. minor_modes%show_parenthesis_mode%state%value) return

        if (blink_matching_parenthesis%choice == BLINK_MATCHING_PARENTHESIS_JUMP .or. &
            blink_matching_parenthesis%choice == BLINK_MATCHING_PARENTHESIS_JUMP_OFFSCREEN) then
            original_point = point()
            call goto_character(blinkpos)
            call sit_for(int(blink_matching_delay*1000, int32))
            call goto_character(original_point)
        else
            if (.not. is_overlay(blink_matching_overlay)) then
                call make_overlay(blink_matching_overlay, point(), point(), st, front_advance=.true.)
                call overlay_put(blink_matching_overlay, "face", value_type(name="show_parenthesis_match"), st)
                call delete_overlay(blink_matching_overlay)
            end if

            call move_overlay(blink_matching_overlay, blinkpos, blinkpos + 1, st)
            call sit_for(int(blink_matching_delay*1000, int32))
            call delete_overlay(blink_matching_overlay)
        end if
    else if (.not. show_parenthesis_context_when_offscreen) then
        call minibuffer_message("Matches "//blink_parenthesis_open_parenthesis_line_string(blinkpos))
    end if
    end procedure blink_matching_open

    module procedure blink_parenthesis_open_parenthesis_line_string
    integer(int64) :: original_point, beginning_of_line, moved, first_beginning, first_end, last_end
    character(len=:), allocatable :: a
    type(status_type) :: st
    logical :: not_beginning_of_line, not_end_of_line, not_beginning_of_buffer
    integer(int64), allocatable :: regions(:, :)
    integer :: k

    original_point = point()
    call goto_character(position)
    ! Capture the regions in terms of (beginning . end) pairs whose
    ! buffer-substrings we want to show as a context string.
    moved = skip_characters_backward(" "//TAB, st)
    not_beginning_of_line = .not. is_beginning_of_line()
    call goto_character(position)

    if (not_beginning_of_line) then
        ! Show what precedes the open in its line, if anything.
        beginning_of_line = line_beginning_position()
        allocate (regions(2, 1))
        regions(:, 1) = [beginning_of_line, position + 1]
    else
        call goto_character(position + 1)
        moved = skip_characters_forward(" "//TAB, st)
        not_end_of_line = .not. is_end_of_line()
        call goto_character(position)

        if (not_end_of_line) then
            ! Show what follows the open in its line, if anything.
            allocate (regions(2, 1))
            regions(:, 1) = [position, line_end_position()]
        else
            moved = skip_characters_backward(LF//" "//TAB, st)
            not_beginning_of_buffer = .not. is_beginning_of_buffer()
            call goto_character(position)

            if (not_beginning_of_buffer) then
                ! Otherwise show the previous nonblank line, if there is
                ! one.
                moved = skip_characters_backward(LF//" "//TAB, st)
                first_beginning = line_beginning_position()
                call end_of_line()
                moved = skip_characters_backward(" "//TAB, st)
                first_end = point()
                allocate (regions(2, 2))
                regions(:, 1) = [first_beginning, first_end]
                regions(:, 2) = [position, position + 1]
            else
                ! There is nothing to show except the char itself.
                allocate (regions(2, 1))
                regions(:, 1) = [position, position + 1]
            end if
        end if
    end if

    ! Ensure we've font-locked the context region.
    last_end = regions(2, size(regions, 2))
    call font_lock_ensure(st, regions(1, 1), last_end)
    line_string = ""

    do k = 1, size(regions, 2)
        if (k > 1) line_string = line_string//"..."
        call buffer_substring(regions(1, k), regions(2, k), a, st)
        line_string = line_string//a
    end do

    call goto_character(original_point)
    end procedure blink_parenthesis_open_parenthesis_line_string

    module procedure blink_parenthesis_post_self_insert_function
    integer(int32) :: c
    integer(int64) :: original_point, start, ev
    logical :: found, odd

    status = status_type()
    ! Sanity check.
    if (vtag(last_command_event) /= V_INTEGER) return
    ev = xfixnum(last_command_event)
    call character_before(c, found)
    if (.not. found .or. int(c, int64) /= ev) return
    if (ev < 0 .or. ev > int(huge(0_int32), int64)) return
    if (character_syntax(int(ev)) /= ")" .and. character_syntax(int(ev)) /= "$") return
    if (.not. associated(blink_parenthesis_function)) return
    if (is_executing_keyboard_macro()) return
    ! Verify an even number of quoting characters precede the close.
    original_point = point()
    call goto_character(original_point - 1)
    start = skip_syntax_backward("/\")
    start = point()
    call goto_character(original_point)
    odd = modulo(original_point - start, 2_int64) == 1
    if (.not. odd) return
    call blink_parenthesis_function(status)
    end procedure blink_parenthesis_post_self_insert_function

    !> Insert a newline, then indent according to major mode.
    !> Indentation is done using the value of `indent_line_function'.
    !> In programming language modes, this is the same as TAB.
    !> In some text modes, where TAB inserts a tab, this command indents to the
    !> column specified by the function `current_left_margin'.
    !>
    !> With ARGUMENT, perform this action that many times.  Port of
    !> simple.el:newline-and-indent (the command, "*p").
    subroutine execute_newline_and_indent(status)
        type(status_type), intent(out) :: status

        call barf_if_buffer_read_only(status)
        if (.not. status%ok()) return
        call newline_and_indent(status, prefix_numeric_value(current_prefix_argument))
    end subroutine execute_newline_and_indent

    module procedure newline_and_indent
    integer(int64) :: n, k
    logical :: saved

    call delete_horizontal_space(.true., status)
    if (.not. status%ok()) return
    n = 1
    if (present(argument)) n = argument
    saved = minor_modes%electric_indent_mode%state%value
    minor_modes%electric_indent_mode%state%value = .false.

    do k = 1, n
        call newline(status, interactive=.true.)
        if (.not. status%ok()) exit
        call indent_according_to_mode(status)
        if (.not. status%ok()) exit
    end do

    minor_modes%electric_indent_mode%state%value = saved
    end procedure newline_and_indent

    module procedure reindent_then_newline_and_indent
    type(handle_type) :: position, position2
    type(status_type) :: st
    integer(int64) :: opos
    type(handle_type) :: here
    logical :: saved

    ! Use a marker because the call to `newline' below could insert some
    ! text, e.g., if `abbreviation_mode' is turned on.
    call point_marker(position)
    saved = minor_modes%electric_indent_mode%state%value
    minor_modes%electric_indent_mode%state%value = .false.

    body: block
        ! Be careful to insert the newline before indenting the line.
        ! Otherwise, the indentation might be wrong.
        call newline(status)
        if (.not. status%ok()) exit body
        ! (save-excursion ...)
        call point_marker(here)
        call marker_position(position, opos, st)
        call goto_character(opos)
        ! We are at END_OF_LINE before the call to indent-according-to-mode, and
        ! after it we usually are as well, but not always.  We tried to
        ! address it with `save-excursion' but that uses a normal marker
        ! whereas we need `move after insertion', so we do the save/restore
        ! by hand.
        call copy_marker_at(opos, position2, .true., st)
        call indent_according_to_mode(status)

        if (status%ok()) then
            call marker_position(position2, opos, st)
            call goto_character(opos)
            ! Remove the trailing white-space after indentation because
            ! indentation may introduce the whitespace.
            call delete_horizontal_space(.true., status)
        end if

        call destroy_marker(position2, st)
        call marker_position(here, opos, st)
        call goto_character(opos)
        call destroy_marker(here, st)
        if (.not. status%ok()) exit body
        call indent_according_to_mode(status)
    end block body

    minor_modes%electric_indent_mode%state%value = saved
    call destroy_marker(position, st)
    end procedure reindent_then_newline_and_indent

    !> Reindent current line, insert newline, then indent the new line.
    !> Port of simple.el:reindent-then-newline-and-indent (the command,
    !> "*").
    subroutine execute_reindent_then_newline_and_indent(status)
        type(status_type), intent(out) :: status

        call barf_if_buffer_read_only(status)
        if (.not. status%ok()) return
        call reindent_then_newline_and_indent(status)
    end subroutine execute_reindent_then_newline_and_indent

    !> Delete characters backward, changing tabs into spaces.
    !> The exact behavior depends on `backward_delete_character_untabify_method'.
    !>
    !> Delete ARGUMENT chars, and cut (save in clipboard history) if CUT is true.
    !>
    !> If Transient Mark mode is enabled, the mark is active, and ARGUMENT is 1,
    !> delete the text in the region and deactivate the mark instead.
    !> To disable this, set option `delete_active_region' to false.
    !>
    !> Interactively, ARGUMENT is the prefix arg (default 1)
    !> and CUT is true if a prefix arg was specified.  Port of
    !> simple.el:backward-delete-char-untabify (the command, "*p\nP").
    subroutine execute_backward_delete_character_untabify(status)
        type(status_type), intent(out) :: status

        call barf_if_buffer_read_only(status)
        if (.not. status%ok()) return
        call backward_delete_character_untabify(prefix_numeric_value(current_prefix_argument), status, &
                                                current_prefix_argument%kind /= PREFIX_NONE)
    end subroutine execute_backward_delete_character_untabify

    module procedure backward_delete_character_untabify
    integer(int64) :: count, column, original_point, oldpt, wh, n, moved
    integer(int64), allocatable :: starts(:), ends(:)
    integer(int32) :: c
    logical :: found, kill
    character(len=:), allocatable :: skip
    type(status_type) :: st
    type(handle_type) :: here
    type(value_type) :: extracted

    kill = .false.
    if (present(cut)) kill = cut

    if (is_use_region() .and. delete_active_region .and. argument == 1) then
        ! Delete the text of the region and deactivate the mark, in the
        ! same way that `delete_backward_character' does.
        call region_extract_function(REGION_EXTRACT_DELETE_ONLY, extracted, starts, ends, status)
        return
    end if

    if (backward_delete_character_untabify_method%choice == BACKWARD_DELETE_CHARACTER_UNTABIFY_METHOD_UNTABIFY) then
        count = argument
        call copy_marker_at(point(), here, .false., st)

        do while (count > 0 .and. .not. is_beginning_of_buffer())
            call character_before(c, found)

            if (found .and. c == 9) then
                column = current_column()
                call goto_character(point() - 1)
                column = column - current_column()
                call insert_character_n(32_int32, column, status)
                if (status%ok()) call delete_character(1_int64, status)

                if (.not. status%ok()) then
                    call destroy_marker(here, st)
                    return
                end if
            end if

            call goto_character(point() - 1)
            count = count - 1
        end do

        call marker_position(here, original_point, st)
        call goto_character(original_point)
        call destroy_marker(here, st)
    end if

    skip = ""
    if (backward_delete_character_untabify_method%choice == BACKWARD_DELETE_CHARACTER_UNTABIFY_METHOD_HUNGRY) skip = " "//TAB
    if (backward_delete_character_untabify_method%choice == BACKWARD_DELETE_CHARACTER_UNTABIFY_METHOD_ALL) skip = " "//TAB//LF &
                                                                                                                  //achar(13)
    n = argument

    if (len(skip) > 0) then
        oldpt = point()
        moved = skip_characters_backward(skip, st)
        call goto_character(constrain_to_field(point(), oldpt, .false., .false.))
        wh = oldpt - point()
        call goto_character(oldpt)
        if (wh /= 0) n = argument + wh - 1
    end if

    call delete_backward_character(n, status, kill)
    end procedure backward_delete_character_untabify
end submodule simple_electric_mod
