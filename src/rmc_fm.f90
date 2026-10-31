! rmc_fm.f90 -- read-multiple-choice.
! Ported from GNU Emacs 31.1 lisp/emacs-lisp/rmc.el.
!
! Copyright (C) 2016-2026 Free Software Foundation, Inc.
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

!> `read_multiple_choice' for a terminal: the key of each name is shown
!> in [brackets].  The answer is read in the minibuffer, with `read_key'
!> (read-char-choice-use-read-key), or as a long answer.  The help is
!> shown in a window split off the bottom of the selected one, removed
!> afterwards.
!>
!> Not ported: dialog boxes, touch-screen events.
module rmc_fm
    use, intrinsic :: iso_fortran_env, only: int64
    use error_fm, only: status_type, set_error, FM_QUIT
    use handle_fm, only: handle_type
    use lisp_fm, only: value_type, xfixnum, make_fixnum, vtag, V_INTEGER
    use buffer_fm, only: current_buffer, get_buffer_create, set_buffer, set_buffer_read_only, erase_buffer, &
                         kill_buffer
    use insdel_fm, only: insert
    use editfns_fm, only: goto_char
    use keymap_fm, only: single_key_description
    use subr_fm, only: read_key, query_replace_answer, ANSWER_RECENTER, ANSWER_SCROLL_UP, ANSWER_SCROLL_DOWN, &
                       ANSWER_SCROLL_OTHER_WINDOW, ANSWER_SCROLL_OTHER_WINDOW_DOWN, ANSWER_EDIT
    use window_fm, only: window_ref, window_type, set_window_buffer, split_window, delete_window, is_window_live, &
                         recenter, scroll_up_command, scroll_down_command
    use lisp_fm, only: make_keymap_ref, make_string, make_list, command_type
    use lread_fm, only: sym
    use data_fm, only: value_eq
    use character_fm, only: MAX_CHAR
    use keymap_fm, only: make_sparse_keymap, set_keymap_parent, define_key, define_key_range, lookup_key, &
                         current_global_map, free_keymap
    use keyboard_fm, only: recursive_edit, this_command, real_this_command, last_command_event, sit_for
    use minibuf_fm, only: read_from_minibuffer, minibuffer_local_map, enable_recursive_minibuffers, &
                          completion_list_table
    use minibuffer_fm, only: exit_minibuffer, minibuffer_message, completing_read, REQUIRE_MATCH_YES
    use frame_fm, only: selected_frame, selected_window
    use dispnew_fm, only: ding
    use xdisp_fm, only: message
    implicit none(type, external)
    private

    public :: read_multiple_choice, rmc_add_key_description, generated_help

    !> If true, use `read_key' when reading a character by
    !> `read_multiple_choice' (read-char-choice).  Otherwise, use the
    !> minibuffer (this is the default).  Port of subr.el
    !> read-char-choice-use-read-key.
    logical, public :: read_char_choice_use_read_key = .false.

    !> A choice of `read_multiple_choice': (CHAR NAME [DESCRIPTION]).
    type, public :: rmc_choice_type
        integer :: key = 0
        character(len=:), allocatable :: name
        character(len=:), allocatable :: description
    end type rmc_choice_type

    !> The question being asked: what the help and the minibuffer
    !> commands need (upstream's closures).
    character(len=:), allocatable :: rmc_prompt, rmc_help_string, rmc_buf_name
    type(rmc_choice_type), allocatable :: rmc_choices(:)
    logical :: rmc_have_help_string = .false., rmc_have_show_help = .false., rmc_shown = .false.
    type(handle_type) :: rmc_buf, rmc_help_window
    !> The key the minibuffer path read (tchar), -1 for none.
    integer :: rmc_tchar = -1
    type(command_type), target :: cmd_char, cmd_help, cmd_wrong

contains

    !> NAME of a choice with its key shown: "KEY NAME" if KEY is not in
    !> NAME or is a special key (ESC, TAB, RET, DEL, SPC), else NAME with
    !> the key upcased in [brackets] (a terminal without underlining).
    !> Port of rmc.el:rmc--add-key-description.
    function rmc_add_key_description(choice) result(altered)
        type(rmc_choice_type), intent(in) :: choice
        character(len=:), allocatable :: altered, desc
        type(value_type) :: key
        integer :: pos

        key = make_fixnum(int(choice%key, int64))
        desc = single_key_description(key)
        pos = 0
        if (choice%key < 128) pos = index(choice%name, achar(choice%key))

        if (pos == 0 .or. desc == "ESC" .or. desc == "TAB" .or. desc == "RET" .or. desc == "DEL" .or. &
            desc == "SPC") then
            altered = desc//" "//choice%name
        else
            altered = choice%name(1:pos - 1)//"["//upcase_ascii(choice%name(pos:pos))//"]"//choice%name(pos + 1:)
        end if
    end function rmc_add_key_description

    ! Not a port.
    pure function upcase_ascii(c) result(u)
        character, intent(in) :: c
        character :: u

        u = c
        if (c >= "a" .and. c <= "z") u = achar(iachar(c) - 32)
    end function upcase_ascii

    !> Ask user to select an entry from CHOICES, prompting with PROMPT.
    !> This function lets a user select an option from an array of options
    !> by typing a single character, and returns the index of the choice
    !> (`answer'; 0 for none).
    !>
    !> CHOICES is an array of `rmc_choice_type' (KEY, NAME, DESCRIPTION).  KEY is a character
    !> to be entered.  NAME is a short name for the entry to be displayed
    !> while prompting (if there's no room, it might be shortened).
    !> DESCRIPTION is an optional longer explanation for the entry that
    !> will be displayed in a help buffer if the user requests more help.
    !>
    !> HELP_STRING argument is a string that contains overall
    !> information about the choices.  If this argument is given, it is
    !> displayed in the help buffer instead of the generated help.
    !>
    !> SHOW_HELP, if present, means show the help buffer from the start
    !> (named SHOW_HELP when it is a non-empty string).
    !>
    !> If optional argument LONG_FORM is true, the user will have to
    !> type in long-form answers (using `completing_read') instead of
    !> hitting a single key.
    !>
    !> If the user types the `?' key, the help is displayed.  The answer
    !> is read in the minibuffer, or with `read_key' if
    !> `read_char_choice_use_read_key' is true.  Port of
    !> rmc.el:read-multiple-choice (no dialog boxes or touch-screen
    !> events).
    recursive subroutine read_multiple_choice(prompt, choices, answer, status, help_string, show_help, long_form)
        character(len=*), intent(in) :: prompt
        type(rmc_choice_type), intent(in) :: choices(:)
        integer, intent(out) :: answer
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: help_string, show_help
        logical, intent(in), optional :: long_form
        type(status_type) :: st
        logical :: long

        answer = 0
        status = status_type()
        long = .false.
        if (present(long_form)) long = long_form

        if (long) then
            call long_answers(prompt, choices, answer, status)
        else
            ! The state the help needs.
            rmc_prompt = prompt
            rmc_choices = choices
            rmc_have_help_string = present(help_string)
            if (rmc_have_help_string) rmc_help_string = help_string
            rmc_have_show_help = present(show_help)
            rmc_buf_name = "*Multiple Choice Help*"

            if (present(show_help)) then
                if (len(show_help) > 0) rmc_buf_name = show_help
            end if

            rmc_shown = .false.

            if (read_char_choice_use_read_key) then
                call short_answers(answer, status)
            else
                call from_minibuffer(answer, status)
            end if

            if (rmc_shown) then
                if (is_window_live(rmc_help_window)) call delete_window(rmc_help_window, st)
                call kill_buffer(rmc_buf, st)
            end if
        end if
    end subroutine read_multiple_choice

    !> The prompt: PROMPT, then the names with their keys, and "[?]"
    !> unless the help shows from the start.  Port of the full-prompt of
    !> rmc.el:read-multiple-choice--short-answers and
    !> rmc.el:read-multiple-choice--from-minibuffer.
    function full_prompt() result(full)
        character(len=:), allocatable :: full
        type(rmc_choice_type) :: question
        integer :: k

        full = rmc_prompt//" ("

        do k = 1, size(rmc_choices)
            if (k > 1) full = full//", "
            full = full//rmc_add_key_description(rmc_choices(k))
        end do

        if (.not. rmc_have_show_help) then
            question%key = iachar("?")
            question%name = "?"
            if (size(rmc_choices) > 0) full = full//", "
            full = full//rmc_add_key_description(question)
        end if

        full = full//"): "
    end function full_prompt

    !> Port of rmc.el:read-multiple-choice--short-answers.
    subroutine short_answers(answer, status)
        integer, intent(out) :: answer
        type(status_type), intent(out) :: status
        type(value_type) :: key
        type(status_type) :: st
        logical :: wrong_char, again
        integer :: k, c

        answer = 0
        status = status_type()
        if (rmc_have_show_help) call show_help_window()
        wrong_char = .false.

        do
            if (wrong_char) then
                call read_key("Invalid choice.  "//full_prompt(), key, status)
            else
                call read_key(full_prompt(), key, status)
            end if

            if (.not. status%ok()) return
            c = -1
            if (vtag(key) == V_INTEGER) c = int(xfixnum(key))

            if (c == 7) then
                call set_error(status, FM_QUIT)
                return
            end if

            again = .true.

            select case (query_replace_answer(key))
            case (ANSWER_RECENTER)
                call recenter(value_type(), .false., st)
            case (ANSWER_SCROLL_UP)
                call scroll_up_command(value_type(), st)
            case (ANSWER_SCROLL_DOWN)
                call scroll_down_command(value_type(), st)
            case (ANSWER_SCROLL_OTHER_WINDOW, ANSWER_SCROLL_OTHER_WINDOW_DOWN)
                ! scroll-other-window and scroll-other-window-down are not
                ! ported (Deferred).
                continue
            case (ANSWER_EDIT)
                call message("Recursive edit; type C-M-c to return to help screen")
                call recursive_edit(st)
            case default
                again = .false.
            end select

            if (again) then
                wrong_char = .false.
                cycle
            end if

            do k = 1, size(rmc_choices)
                if (rmc_choices(k)%key == c) answer = k
            end do

            if (answer /= 0) exit
            ! The user has entered an invalid choice, so display the help
            ! messages.
            wrong_char = c /= iachar("?") .and. c /= 8
            if (wrong_char) call ding()
            call show_help_window()
        end do
    end subroutine short_answers

    !> Read short answers from the minibuffer.  Port of
    !> rmc.el:read-multiple-choice--from-minibuffer (the keymap's
    !> [remap self-insert-command] is flattened, as keymap_fm has no
    !> remapping; its [help-char] binding binds a symbol event, which
    !> never arrives).
    recursive subroutine from_minibuffer(answer, status)
        type(value_type) :: key1(1)
        integer, intent(out) :: answer
        type(status_type), intent(out) :: status
        type(handle_type) :: map
        type(value_type) :: b, saved_this, saved_real_this
        type(status_type) :: st
        character(len=:), allocatable :: str
        integer :: k
        integer(int64) :: c
        logical :: saved_enable

        answer = 0
        rmc_tchar = -1
        call define_commands()
        if (rmc_have_show_help) call show_help_window()
        call make_sparse_keymap(map)
        call set_keymap_parent(map, minibuffer_local_map, st)

        ! [remap self-insert-command]
        do c = 0, 127
            key1(1) = make_fixnum(c)
            call lookup_key(make_keymap_ref(minibuffer_local_map), key1, b, st)
            if (.not. allocated(b%a)) call lookup_key(make_keymap_ref(current_global_map()), key1, b, st)

            if (value_eq(b, sym("self_insert_command"))) then
                call define_key(map, key1, sym("rmc__wrong"), st)
            end if
        end do

        key1(1) = make_fixnum(128_int64)
        call lookup_key(make_keymap_ref(current_global_map()), key1, b, st)

        if (value_eq(b, sym("self_insert_command"))) then
            call define_key_range(map, 128_int64, int(MAX_CHAR, int64), sym("rmc__wrong"), st)
        end if

        do k = 1, size(rmc_choices)
            key1(1) = make_fixnum(int(rmc_choices(k)%key, int64))
            call define_key(map, key1, sym("rmc__char"), st)
        end do

        key1(1) = make_fixnum(int(iachar("?"), int64))
        if (.not. rmc_have_show_help) call define_key(map, key1, &
                                                      sym("rmc__help"), st)
        saved_this = this_command
        saved_real_this = real_this_command
        saved_enable = enable_recursive_minibuffers
        enable_recursive_minibuffers = .true.
        call read_from_minibuffer(full_prompt(), str, status, keymap=map)
        enable_recursive_minibuffers = saved_enable
        this_command = saved_this
        real_this_command = saved_real_this
        call free_keymap(map, st)
        if (.not. status%ok()) return

        do k = 1, size(rmc_choices)
            if (rmc_choices(k)%key == rmc_tchar) answer = k
        end do
    end subroutine from_minibuffer

    !> Make the minibuffer path's commands known.  Not a port.
    subroutine define_commands()
        if (len_trim(cmd_char%name) > 0) return
        cmd_char = command_type("rmc__char", rmc_char)
        call cmd_char%define()
        cmd_help = command_type("rmc__help", rmc_help)
        call cmd_help%define()
        cmd_wrong = command_type("rmc__wrong", rmc_wrong)
        call cmd_wrong%define()
    end subroutine define_commands

    !> cmd-char: the key is the answer.  Port of the cmd-char lambda of
    !> rmc.el:read-multiple-choice--from-minibuffer.
    subroutine rmc_char(status)
        type(status_type), intent(out) :: status

        rmc_tchar = -1
        if (vtag(last_command_event) == V_INTEGER) rmc_tchar = int(xfixnum(last_command_event))
        call exit_minibuffer(status)
    end subroutine rmc_char

    !> cmd-help: show the help.  Port of the cmd-help lambda of
    !> rmc.el:read-multiple-choice--from-minibuffer.
    subroutine rmc_help(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call show_help_window()
    end subroutine rmc_help

    !> cmd-wrong: an invalid choice.  Port of the cmd-wrong lambda of
    !> rmc.el:read-multiple-choice--from-minibuffer.
    subroutine rmc_wrong(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call ding()
        call show_help_window()
        call minibuffer_message("Invalid choice")
        call sit_for(2000)
    end subroutine rmc_wrong

    !> Port of rmc.el:read-multiple-choice--long-answers.
    recursive subroutine long_answers(prompt, choices, answer, status)
        character(len=*), intent(in) :: prompt
        type(rmc_choice_type), intent(in) :: choices(:)
        integer, intent(out) :: answer
        type(status_type), intent(out) :: status
        type(value_type), allocatable :: names(:)
        character(len=:), allocatable :: full, result
        integer :: k

        answer = 0
        allocate (names(size(choices)))
        full = prompt//" ("

        do k = 1, size(choices)
            names(k) = make_string(choices(k)%name)
            if (k > 1) full = full//"/"
            full = full//choices(k)%name
        end do

        full = full//") "
        call completing_read(full, completion_list_table(make_list(names)), result, status, &
                             require_match=REQUIRE_MATCH_YES)
        if (.not. status%ok()) return

        do k = 1, size(choices)
            if (choices(k)%name == result .and. len(choices(k)%name) == len(result)) then
                answer = k
                return
            end if
        end do
    end subroutine long_answers

    !> rmc--show-help: fill the help buffer and show it at the bottom.
    !> Port of rmc.el:rmc--show-help.
    subroutine show_help_window()
        type(handle_type) :: previous
        character(len=:), allocatable :: text
        type(status_type) :: st
        integer :: lines, i

        previous = current_buffer()
        call get_buffer_create(rmc_buf_name, rmc_buf, st)
        if (.not. st%ok()) return
        call set_buffer(rmc_buf, st)
        call set_buffer_read_only(rmc_buf, .false., st)
        call erase_buffer(st)

        if (rmc_have_help_string) then
            text = rmc_help_string
        else
            text = rmc_prompt//achar(10)//achar(10)//generated_help(rmc_choices, help_columns())
        end if

        call insert(text, st)
        call goto_char(1_int64)
        call set_buffer_read_only(rmc_buf, .true., st)
        call set_buffer(previous, st)

        if (rmc_shown) then
            if (is_window_live(rmc_help_window)) return
        end if

        rmc_shown = .true.
        ! A window as high as the text (and its mode line) below the
        ! selected one; none if it doesn't fit.
        lines = 1

        do i = 1, len(text) - 1
            if (text(i:i) == achar(10)) lines = lines + 1
        end do

        call split_window(selected_window(), -(lines + 1), .true., .false., rmc_help_window, st)

        if (.not. st%ok()) then
            call split_window(selected_window(), 0, .false., .false., rmc_help_window, st)
            if (.not. st%ok()) return
        end if

        call set_window_buffer(rmc_help_window, rmc_buf, st)
    end subroutine show_help_window

    ! (/ (window-width) 25): the columns of the generated help.  Not a port.
    integer function help_columns() result(columns)
        type(window_type), pointer :: w

        columns = 80/25
        if (.not. is_window_live(selected_window())) return
        w => window_ref(selected_window())
        if (w%total_cols > 0) columns = max(1, w%total_cols/25)
    end function help_columns

    ! Number of characters of the UTF-8 string S.  Not a port.
    pure integer function nchars(s)
        character(len=*), intent(in) :: s
        integer :: i

        nchars = 0

        do i = 1, len(s)
            if (iand(iachar(s(i:i)), 192) /= 128) nchars = nchars + 1
        end do
    end function nchars

    ! TEXT (one line) filled at fill-column 21: the lines, each ended by
    ! a newline.  Not a port.
    function fill21(text) result(filled)
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: filled, line, word
        integer :: i, j

        filled = ""
        line = ""
        i = 1

        do while (i <= len(text))
            if (text(i:i) == " ") then
                i = i + 1
                cycle
            end if

            j = index(text(i:), " ")

            if (j == 0) then
                word = text(i:)
                i = len(text) + 1
            else
                word = text(i:i + j - 2)
                i = i + j
            end if

            if (len(line) == 0) then
                line = word
            else if (nchars(line) + 1 + nchars(word) <= 21) then
                line = line//" "//word
            else
                filled = filled//line//achar(10)
                line = word
            end if
        end do

        filled = filled//line//achar(10)
    end function fill21

    !> The help text rmc--show-help generates: each choice ("KEY: NAME"
    !> and its description, filled at column 21) in COLUMNS columns 25
    !> characters apart.  Port of the layout in rmc.el:rmc--show-help.
    function generated_help(choices, columns) result(text)
        type(rmc_choice_type), intent(in) :: choices(:)
        integer, intent(in) :: columns
        character(len=:), allocatable :: text, entry
        type :: line_type
            character(len=:), allocatable :: s
        end type line_type
        type(line_type), allocatable :: lines(:), grown(:)
        integer :: times, start, n, k, row, i, j, pad

        allocate (lines(0))
        n = 0
        start = 1
        times = 0

        do k = 1, size(choices)
            if (times /= 0) then
                if (mod(times, columns) == 0) then
                    ! Go to the next "line".
                    start = n + 1
                else
                    ! Add padding.
                    do row = start, n
                        lines(row)%s = lines(row)%s//repeat(" ", max(mod(times, columns)*25 - nchars(lines(row)%s), 0))
                    end do
                end if
            end if

            times = times + 1
            entry = fill21(achar(choices(k)%key)//": "//rmc_add_key_description(choices(k)))
            if (allocated(choices(k)%description)) entry = entry//fill21(choices(k)%description)
            pad = mod(times - 1, columns)*25
            row = start
            i = 1

            ! (split-string text "\n"): the lines, then a last empty one.
            do
                j = index(entry(i:), achar(10))

                if (row <= n) then
                    if (len(lines(row)%s) > 0) then
                        if (j > 0) then
                            lines(row)%s = lines(row)%s//entry(i:i + j - 2)
                        else
                            lines(row)%s = lines(row)%s//entry(i:)
                        end if

                        row = row + 1
                        if (j == 0) exit
                        i = i + j
                        cycle
                    end if
                end if

                ! At the beginning of a line: a new line before it.
                allocate (grown(n + 1))
                grown(1:row - 1) = lines(1:row - 1)

                if (j > 0) then
                    grown(row)%s = repeat(" ", pad)//entry(i:i + j - 2)
                else
                    grown(row)%s = repeat(" ", pad)//entry(i:)
                end if

                if (row <= n) grown(row + 1:) = lines(row:)
                call move_alloc(grown, lines)
                n = n + 1
                row = row + 1
                if (j == 0) exit
                i = i + j
            end do
        end do

        text = ""

        do row = 1, n
            text = text//lines(row)%s//achar(10)
        end do
    end function generated_help
end module rmc_fm
