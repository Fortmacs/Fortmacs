! editing_commands_mod.f90 -- simple built-in editing commands.
! Ported from GNU Emacs 31.1 src/cmds.c.
!
! Copyright (C) 1985, 1993-1998, 2001-2026 Free Software Foundation, Inc.
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

!> Motion, deletion and insertion commands on the current buffer, and
!> their registration as commands (`initialize_commands').
!>
!> Deferred: overwrite mode in `self_insert_command',
!> `translation_table_for_input', and intangible/invisible text.
module editing_commands_mod
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use execute_interactively_mod, only: PREFIX_NONE
    use commands_mod, only: commands
    use error_mod, only: status_type, set_error, GENERIC_ERROR_CODE, &
                         BEGINNING_OF_BUFFER_CODE, &
                         END_OF_BUFFER_CODE
    use buffer_mod, only: buffers, current, BUFFER_BYTES_MAXIMUM, buffer_fetch_byte, &
                          temporary_set_point_both, auto_fill_function
    use search_mod, only: scan_newline_from_point
    use insert_delete_mod, only: delete_range
    use editing_functions_mod, only: line_beginning_position, line_end_position, goto_character, &
                                     insert_character_n
    use value_mod, only: value_type, xfixnum, vtag, V_INTEGER
    use character_mod, only: MAXIMUM_CHARACTER
    use execute_interactively_mod, only: current_prefix_argument, prefix_numeric_value
    use keyboard_mod, only: last_command_event, this_command, last_command
    use value_mod, only: value_type
    use hooks_mod, only: hook_type
    use undo_mod, only: undo_auto_amalgamate, this_command_amalgamating
    use display_update_mod, only: ding
    use syntax_mod, only: character_syntax_class, SWORD
    use editing_functions_mod, only: preceding_character
    use abbreviation_mod, only: is_abbreviation_mode, expand_abbreviation, abbreviation_reference_type, abbreviation_no_self_insert
    implicit none(type, external)
    private

    public :: forward_character, backward_character, forward_line, beginning_of_line, &
              end_of_line, delete_character, self_insert_command, initialize_commands

    !> A range of characters FROM..TO, both included.
    type, public :: character_range_type
        integer :: from = 0
        integer :: to = -1
    end type character_range_type

    !> The characters which invoke auto-filling, as ranges of characters.
    !> Port of character.c
    !> auto-fill-chars: here the ranges whose value is t (unallocated is
    !> not a char-table: space and newline).  `initialize_commands' gives it
    !> space and newline (character.c) and characters.el's CJK ranges.
    type(character_range_type), allocatable, public :: auto_fill_characters(:)

    abstract interface
        !> simple.el:internal-auto-fill: `filled' is its value (true
        !> when the auto-fill function did any work).  Not a port.
        subroutine internal_auto_fill_procedure(status, filled)
            import :: status_type
            implicit none(type, external)
            type(status_type), intent(out) :: status
            logical, intent(out) :: filled
        end subroutine internal_auto_fill_procedure

        !> simple.el:kill-forward-chars: cut `n' characters after point
        !> (before, if negative).  Not a port.
        subroutine cut_characters_procedure(n, status)
            import :: int64, status_type
            implicit none(type, external)
            integer(int64), intent(in) :: n
            type(status_type), intent(out) :: status
        end subroutine cut_characters_procedure
    end interface

    !> simple.el:internal-auto-fill, which `self_insert_command' calls to
    !> perform auto-filling; set by simple_mod (null: no auto-filling).
    procedure(internal_auto_fill_procedure), pointer, public :: internal_auto_fill_hook => null()

    !> simple.el:kill-forward-chars, which `delete_character' calls with
    !> CUT; set by simple_mod (null: CUT deletes).
    procedure(cut_characters_procedure), pointer, public :: cut_forward_characters_hook => null()

    !> Hook run at the end of `self_insert_command'.
    !> This is run after inserting a character.
    !> The hook can access the inserted character via `last_command_event'.
    !> Port of cmds.c post-self-insert-hook.
    type(hook_type), target, public :: post_self_insert_hook = hook_type(name="post_self_insert_hook")

contains

    !> Move point `n' characters (backward if not `forward'), stopping at
    !> the accessible region's edge with beginning-/end-of-buffer.
    !> Port of cmds.c:move_point.
    subroutine move_point(n, forward, status)
        integer(int64), intent(in) :: n
        logical, intent(in) :: forward
        type(status_type), intent(out) :: status
        integer(int64) :: new_point

        if (forward) then
            new_point = buffers(current)%point + n
        else
            new_point = buffers(current)%point - n
        end if

        if (new_point < buffers(current)%beginning_of_accessible_portion) then
            call goto_character(buffers(current)%beginning_of_accessible_portion)
            call set_error(status, BEGINNING_OF_BUFFER_CODE)
        else if (new_point > buffers(current)%end_of_accessible_portion) then
            call goto_character(buffers(current)%end_of_accessible_portion)
            call set_error(status, END_OF_BUFFER_CODE)
        else
            call goto_character(new_point)
        end if
    end subroutine move_point

    !> Port of cmds.c:Fforward_char (N defaults to 1).
    subroutine forward_character(status, n)
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: n

        if (present(n)) then
            call move_point(n, .true., status)
        else
            call move_point(1_int64, .true., status)
        end if
    end subroutine forward_character

    !> Port of cmds.c:Fbackward_char (N defaults to 1).
    subroutine backward_character(status, n)
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: n

        if (present(n)) then
            call move_point(n, .false., status)
        else
            call move_point(1_int64, .false., status)
        end if
    end subroutine backward_character

    !> Move to the start of the line `n' lines away (default 1) and return
    !> the count of lines left to move (see the forward-line docstring:
    !> a non-empty last line counts as moved).  Port of cmds.c:Fforward_line.
    integer(int64) function forward_line(n) result(shortage)
        integer(int64), intent(in), optional :: n
        integer(int64) :: count, original_point, position, position_byte, counted
        logical :: excessive

        count = 1
        excessive = .false.

        if (present(n)) then
            count = max(-BUFFER_BYTES_MAXIMUM, min(n, BUFFER_BYTES_MAXIMUM))
            excessive = count /= n
        end if

        original_point = buffers(current)%point
        counted = scan_newline_from_point(count, position, position_byte)
        call temporary_set_point_both(buffers(current), position, position_byte)
        shortage = count - counted
        if (count <= 0) shortage = shortage - 1

        if (shortage /= 0) then
            if (count <= 0) then
                shortage = shortage + 1
            else
                associate (b => buffers(current))
                    if (b%beginning_of_accessible_portion < b%end_of_accessible_portion .and. b%point /= original_point) then
                        if (buffer_fetch_byte(b, b%point_byte - 1) /= 10) shortage = shortage - 1
                    end if
                end associate
            end if
        end if

        ! A count beyond any buffer size reports the shortage relative to N.
        if (excessive) shortage = shortage - count + n
    end function forward_line

    !> Move to the start of the line (after moving `n' - 1 lines).
    !> Port of cmds.c:Fbeginning_of_line (fields pending).
    subroutine beginning_of_line(n)
        integer(int64), intent(in), optional :: n

        call goto_character(line_beginning_position(n))
    end subroutine beginning_of_line

    !> Move to the end of the line (after moving `n' - 1 lines).
    !> Port of cmds.c:Fend_of_line; the intangible-text retry loop is moot
    !> until text properties exist.
    subroutine end_of_line(n)
        integer(int64), intent(in), optional :: n

        call goto_character(line_end_position(n))
    end subroutine end_of_line

    !> Delete `n' characters after point (before, if negative); a
    !> `beginning_of_buffer' or `end_of_buffer' error condition occurs instead of deleting fewer.  Optional
    !> CUT true means cut instead (save in clipboard history).
    !> Port of cmds.c:Fdelete_char.
    subroutine delete_character(n, status, cut)
        integer(int64), intent(in) :: n
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: cut
        integer(int64) :: position
        logical :: kill

        if (abs(n) < 2) call undo_auto_amalgamate(associated(this_command, last_command))
        position = buffers(current)%point + n
        kill = .false.
        if (present(cut)) kill = cut

        if (kill .and. associated(cut_forward_characters_hook)) then
            call cut_forward_characters_hook(n, status)
        else if (n < 0) then
            if (position < buffers(current)%beginning_of_accessible_portion) then
                call set_error(status, BEGINNING_OF_BUFFER_CODE)
            else
                call delete_range(position, buffers(current)%point, status)
            end if
        else
            if (position > buffers(current)%end_of_accessible_portion) then
                call set_error(status, END_OF_BUFFER_CODE)
            else
                call delete_range(buffers(current)%point, position, status)
            end if
        end if
    end subroutine delete_character

    !> Insert character `c' `n' times, then run `post_self_insert_hook';
    !> `c' becomes `last_command_event'.  Port of
    !> cmds.c:Fself_insert_command with internal_self_insert reduced to
    !> abbrev expansion, the insertion, auto-fill and the hook (no
    !> overwrite).  A non-character `c' (-1: `last_command_event' is not a
    !> character) rings the bell.
    recursive subroutine self_insert_command(n, c, status)
        integer(int64), intent(in) :: n, c
        type(status_type), intent(out) :: status
        character(len=24) :: buffer
        integer :: hairy

        status = status_type()
        ! Backward compatibility.
        if (c >= 0) last_command_event = value_type(c)

        if (n < 0) then
            write (buffer, "(i0)") n
            call set_error(status, GENERIC_ERROR_CODE, &
                           "Negative repetition argument "//trim(buffer))
            return
        end if

        if (n < 2) call undo_auto_amalgamate(associated(this_command, last_command))

        ! Barf if the key that invoked this was not a character.
        if (c < 0 .or. c > MAXIMUM_CHARACTER) then
            call ding()
            return
        end if

        call internal_self_insert(int(c, int32), n, hairy, status)
        if (hairy == 2) this_command_amalgamating = .false.
    end subroutine self_insert_command

    !> Expand an abbrev before point, insert character `c' `n' times,
    !> auto-fill and run `post_self_insert_hook'; `hairy' is 1 when an
    !> abbrev's hook said not to insert, 2 when an abbrev expanded or
    !> auto-filling did something (the undo amalgamation is then reset).
    !> Port of cmds.c:internal_self_insert (no overwrite mode or change
    !> functions).
    recursive subroutine internal_self_insert(c, n, hairy, status)
        integer(int32), intent(in) :: c
        integer(int64), intent(in) :: n
        integer, intent(out) :: hairy
        type(status_type), intent(out) :: status
        logical :: filled
        integer(int64) :: modiff
        type(abbreviation_reference_type) :: symbol

        hairy = 0
        status = status_type()

        if (is_abbreviation_mode()) then
            if (character_syntax_class(int(c)) /= SWORD .and. .not. buffers(current)%read_only .and. &
                buffers(current)%point > buffers(current)%beginning_of_accessible_portion) then
                if (character_syntax_class(int(preceding_character())) == SWORD) then
                    modiff = buffers(current)%text%modiff
                    call expand_abbreviation(status, symbol)
                    if (.not. status%ok()) return

                    ! If we expanded an abbrev which has a hook,
                    ! and the hook has a `no_self_insert' property,
                    ! return right away--don't really self-insert.
                    if (abbreviation_no_self_insert(symbol)) then
                        hairy = 1
                        return
                    end if

                    if (buffers(current)%text%modiff /= modiff) hairy = 2
                end if
            end if
        end if

        call insert_character_n(c, n, status)
        if (.not. status%ok()) return

        if (is_auto_fill_character(c) .and. associated(auto_fill_function%value) .and. associated(internal_auto_fill_hook)) then
            ! After inserting a newline, move to previous line and fill
            ! that.  Must have the newline in place already so filling and
            ! justification, if any, know where the end is going to be.
            if (c == 10) call goto_character(buffers(current)%point - 1)
            call internal_auto_fill_hook(status, filled)
            ! Test POINT < END_OF_ACCESSIBLE_PORTION in case the auto-fill-function is strange.
            if (c == 10 .and. buffers(current)%point < buffers(current)%end_of_accessible_portion) call goto_character(buffers( &
                                                                                                                 current)%point + 1)
            if (filled) hairy = 2
        end if

        ! Run hooks for electric keys.
        if (.not. status%ok()) return
        call post_self_insert_hook%execute(status)
    end subroutine internal_self_insert

    !> Whether `auto_fill_characters' contains `c' (space and
    !> newline when it is not a char-table).  Not a port.
    logical function is_auto_fill_character(c)
        integer(int32), intent(in) :: c
        integer :: k

        if (.not. allocated(auto_fill_characters)) then
            is_auto_fill_character = c == 32 .or. c == 10
            return
        end if

        is_auto_fill_character = .false.

        do k = 1, size(auto_fill_characters)
            if (c >= auto_fill_characters(k)%from .and. c <= auto_fill_characters(k)%to) then
                is_auto_fill_character = .true.
                return
            end if
        end do
    end function is_auto_fill_character

    ! ------------------------------------------------------------------
    ! Commands (the interactive side of the DEFUNs)
    ! ------------------------------------------------------------------

    !> Interactive "p": the numeric prefix argument.  Not a port.
    integer(int64) function numeric_prefix_argument()
        numeric_prefix_argument = prefix_numeric_value(current_prefix_argument)
    end function numeric_prefix_argument

    !> Move point N characters forward (backward if N is negative).
    !> On reaching end or beginning of buffer, stop, and an error condition occurs.
    !> Interactively, N is the numeric prefix argument.
    !> If N is omitted, move point 1 character forward.
    !>
    !> Depending on the bidirectional context, the movement may be to the
    !> right or to the left on the screen.  This is in contrast with
    !> \[right_character], which see.  Port of cmds.c:Fforward_char (the command).
    subroutine execute_forward_character(status)
        type(status_type), intent(out) :: status

        call forward_character(status, numeric_prefix_argument())
    end subroutine execute_forward_character

    !> Move point N characters backward (forward if N is negative).
    !> On attempt to pass beginning or end of buffer, stop, and an error condition occurs.
    !> Interactively, N is the numeric prefix argument.
    !> If N is omitted, move point 1 character backward.
    !>
    !> Depending on the bidirectional context, the movement may be to the
    !> right or to the left on the screen.  This is in contrast with
    !> \[left_character], which see.  Port of cmds.c:Fbackward_char (the command).
    subroutine execute_backward_character(status)
        type(status_type), intent(out) :: status

        call backward_character(status, numeric_prefix_argument())
    end subroutine execute_backward_character

    !> Move N lines forward (backward if N is negative).
    !> Precisely, if point is on line I, move to the start of line I + N
    !> ("start of line" in the logical order).
    !> If there isn't room, go as far as possible (no error).
    !> Interactively, N is the numeric prefix argument and defaults to 1.
    !>
    !> Returns the count of lines left to move.  If moving forward,
    !> that is N minus number of lines moved; if backward, N plus number
    !> moved.
    !>
    !> Exception: With positive N, a non-empty line at the end counts as one
    !> line successfully moved (for the return value).  This means in that case
    !> it returns one less than the number of lines left to move.
    !>
    !> The command C-n (\[next_line]) can also be used to move to the next
    !> line.  Port of cmds.c:Fforward_line (the command).
    subroutine execute_forward_line(status)
        type(status_type), intent(out) :: status
        integer(int64) :: shortage

        status = status_type()
        shortage = forward_line(numeric_prefix_argument())
    end subroutine execute_forward_line

    !> Move point to beginning of current line (in the logical order).
    !> With argument N not 1, move forward N - 1 lines first.
    !> If point reaches the beginning or end of buffer, it stops there.
    !>
    !> This function constrains point to the current field unless this moves
    !> point to a different line from the original, unconstrained result.
    !> If N is 1, and a front-sticky field starts at point, the point
    !> does not move.  To ignore field boundaries set
    !> `inhibit_field_text_motion' to true, or use the `forward_line' function
    !> instead.  Port of cmds.c:Fbeginning_of_line (the command).
    subroutine execute_beginning_of_line(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call beginning_of_line(numeric_prefix_argument())
    end subroutine execute_beginning_of_line

    !> Move point to end of current line (in the logical order).
    !> With argument N not 1, move forward N - 1 lines first.
    !> If point reaches the beginning or end of buffer, it stops there.
    !>
    !> This function constrains point to the current field unless this moves
    !> point to a different line from the original, unconstrained result.  If
    !> N is 1, and a rear-sticky field ends at point, the point does
    !> not move.  To ignore field boundaries set `inhibit_field_text_motion'
    !> to true.  Port of cmds.c:Fend_of_line (the command).
    subroutine execute_end_of_line(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call end_of_line(numeric_prefix_argument())
    end subroutine execute_end_of_line

    !> Delete the following N characters (previous if N is negative).
    !> Interactively, N is the prefix arg.
    !>
    !> Optional second arg CUT true means kill instead (save in clipboard history).
    !> Interactively, N is the prefix arg, and CUT is set if
    !> N was explicitly specified.
    !>
    !> The command `delete_forward_character' is preferable for interactive use, e.g.
    !> because it respects values of `delete_active_region' and `overwrite_mode'.
    !> Port of cmds.c:Fdelete_char (the command).
    subroutine execute_delete_character(status)
        type(status_type), intent(out) :: status

        call delete_character(numeric_prefix_argument(), status, current_prefix_argument%kind /= PREFIX_NONE)
    end subroutine execute_delete_character

    !> Insert the character you type.
    !> Whichever character C you type to run this command is inserted.
    !> The numeric prefix argument N says how many times to repeat the insertion.
    !> Before insertion, `expand_abbreviation' is executed if the inserted character does
    !> not have word syntax and the previous character in the buffer does.
    !> After insertion, `internal_auto_fill' is called if
    !> `auto_fill_function' is true and if the `auto_fill_characters' table has
    !> a true value for the inserted character.  At the end, it runs
    !> `post_self_insert_hook'.  Port of cmds.c:Fself_insert_command (the
    !> command).
    recursive subroutine execute_self_insert_command(status)
        type(status_type), intent(out) :: status

        status = status_type()

        if (vtag(last_command_event) == V_INTEGER) then
            call self_insert_command(numeric_prefix_argument(), xfixnum(last_command_event), status)
        else
            ! Not a character: self_insert_command barfs.
            call self_insert_command(numeric_prefix_argument(), -1_int64, status)
        end if
    end subroutine execute_self_insert_command

    !> Register this module's commands.  Port of the defsubr part of
    !> cmds.c:syms_of_cmds.
    subroutine initialize_commands()
        commands%forward_character%execute => execute_forward_character
        commands%backward_character%execute => execute_backward_character
        commands%forward_line%execute => execute_forward_line
        commands%beginning_of_line%execute => execute_beginning_of_line
        commands%end_of_line%execute => execute_end_of_line
        commands%delete_character%execute => execute_delete_character
        commands%self_insert_command%execute => execute_self_insert_command
        ! character.c: space and newline; characters.el: the charsets that
        ! should invoke auto-filling.
        auto_fill_characters = [character_range_type(32, 32), character_range_type(10, 10), &
                               character_range_type(int(z'3041'), int(z'30FF')), character_range_type(int(z'3400'), int(z'4DB5')), &
                               character_range_type(int(z'4E00'), int(z'9FBB')), character_range_type(int(z'F900'), int(z'FAFF')), &
                               character_range_type(int(z'FF00'), int(z'FF9F')), character_range_type(int(z'20000'), int(z'2FFFF'))]
    end subroutine initialize_commands
end module editing_commands_mod
