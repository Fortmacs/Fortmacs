! simple_goto_fm.f90 -- going to lines and characters, narrowing and
! counting words, a submodule of simple_fm.
! Ported from GNU Emacs 31.1 lisp/simple.el (and the interactive forms of
! src/editfns.c goto-char, narrow-to-region and widen, src/indent.c
! move-to-column, subr.el goto-char--read-natnum-interactive and
! bindings.el narrow-map).
!
! Copyright (C) 1985-1987, 1993-2026 Free Software Foundation, Inc.
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

!> `goto_line' (M-g g), `goto_line_relative' (C-x n g), the commands
!> `goto_char' (M-g c), `move_to_column' (M-g TAB), `narrow_to_region'
!> (C-x n n, disabled) and `widen' (C-x n w), and `count_words' (M-=)
!> with `count_words_region'.
!>
!> Deferred: `goto_line_history_local' (buffer-local minibuffer
!> histories), `selective_display'.
submodule(simple_fm) simple_goto_fm
    use editfns_fm, only: point_min, widen, narrow_to_region, is_buffer_narrowed, interactive_region, region_beginning, &
                          region_end, inhibit_field_text_motion
    use buffer_fm, only: current_buffer
    use subr_fm, only: read_number
    use thingatpt_fm, only: number_at_point
    use paragraphs_fm, only: count_sentences
    use window_fm, only: get_buffer_window, select_window, switch_to_buffer_other_window, other_buffer
    implicit none(type, external)

contains

    ! Read the arguments of `goto_line' and `goto_line_relative': LINE, and
    ! BUFFER (unless null) after C-u.  Port of simple.el:goto-line-read-args.
    recursive subroutine goto_line_read_args(relative, line, buffer, status)
        logical, intent(in) :: relative
        integer(int64), intent(out) :: line
        type(handle_type), intent(out) :: buffer
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: buffer_prompt, which
        integer(int64) :: number, current_line, begv, zv_from_end
        logical :: found, is_integer
        type(status_type) :: st

        status = status_type()
        line = 1

        if (current_prefix_arg%kind /= PREFIX_NONE .and. current_prefix_arg%kind /= PREFIX_RAW) then
            line = prefix_numeric_value(current_prefix_arg)
            return
        end if

        ! Look for a default, a number in the buffer at point.
        call number_at_point(number, found, is_integer)
        found = found .and. is_integer
        if (found) found = number >= 0
        ! Decide if we're switching buffers.
        buffer_prompt = ""

        if (current_prefix_arg%kind == PREFIX_RAW) then
            buffer = other_buffer(current_buffer(), .true.)
            if (buffer%index /= 0) buffer_prompt = " in "//buffers(buffer%index)%name
        end if

        which = ""

        if (is_buffer_narrowed()) then
            if (relative) then
                which = " relative"
            else
                which = " absolute"
            end if
        end if

        if (relative .or. .not. is_buffer_narrowed()) then
            current_line = line_number_at_pos()
        else
            begv = buffers(current)%begv
            zv_from_end = buffers(current)%text%z - buffers(current)%zv
            call widen()
            current_line = line_number_at_pos()
            call narrow_to_region(begv, buffers(current)%text%z - zv_from_end, st)
        end if

        ! Read the argument, offering that number (if any) as default.
        if (found) then
            call read_number("Goto"//which//" line"//buffer_prompt//": ", line, status, hist="goto_line_history", &
                             defaults=[number, current_line])
        else
            call read_number("Goto"//which//" line"//buffer_prompt//": ", line, status, hist="goto_line_history", &
                             defaults=[current_line], no_default=.true.)
        end if
    end subroutine goto_line_read_args

    module procedure goto_line
    type(handle_type) :: window
    integer(int64) :: pos, begv, zv_from_end, shortage
    logical :: rel, inter
    type(status_type) :: st

    status = status_type()
    rel = .false.
    if (present(relative)) rel = relative
    inter = .false.
    if (present(interactive)) inter = interactive

    ! Switch to the desired buffer, one way or another.
    if (inter) then
        if (present(buffer)) then
            if (buffer%index /= 0) then
                window = get_buffer_window(buffer)

                if (window%index /= 0) then
                    call select_window(window, status)
                else
                    call switch_to_buffer_other_window(buffer, status)
                end if

                if (.not. status%ok()) return
            end if
        end if

        ! Leave mark at previous position
        if (.not. is_region_active()) call push_mark()
    end if

    ! Move to the specified line number in that buffer.
    begv = buffers(current)%begv
    zv_from_end = buffers(current)%text%z - buffers(current)%zv
    if (.not. rel) call widen()
    call goto_char(point_min())
    shortage = forward_line(line - 1)
    pos = point()
    call narrow_to_region(begv, buffers(current)%text%z - zv_from_end, st)

    if (.not. rel .and. is_buffer_narrowed() .and. widen_automatically) then
        ! Position is outside narrowed part of buffer
        if (point_min() > pos .or. pos > point_max()) call widen()
    end if

    call goto_char(pos)
    end procedure goto_line

    module procedure goto_line_relative
    call goto_line(line, status, buffer, .true., interactive)
    end procedure goto_line_relative

    !> Go to LINE, counting from line 1 at beginning of buffer (the
    !> command).  Port of simple.el:goto-line.
    recursive subroutine cmd_goto_line(status)
        type(status_type), intent(out) :: status
        integer(int64) :: line
        type(handle_type) :: buffer

        call goto_line_read_args(.false., line, buffer, status)
        if (.not. status%ok()) return

        if (buffer%index /= 0) then
            call goto_line(line, status, buffer, interactive=.true.)
        else
            call goto_line(line, status, interactive=.true.)
        end if
    end subroutine cmd_goto_line

    !> Go to LINE, counting from line at (point-min) (the command).  Port
    !> of simple.el:goto-line-relative.
    recursive subroutine cmd_goto_line_relative(status)
        type(status_type), intent(out) :: status
        integer(int64) :: line
        type(handle_type) :: buffer

        call goto_line_read_args(.true., line, buffer, status)
        if (.not. status%ok()) return

        if (buffer%index /= 0) then
            call goto_line_relative(line, status, buffer, interactive=.true.)
        else
            call goto_line_relative(line, status, interactive=.true.)
        end if
    end subroutine cmd_goto_line_relative

    ! Get a natural number argument, optionally prompting with PROMPT.
    ! If there is a natural number at point, use it as default.  Port of
    ! subr.el:goto-char--read-natnum-interactive.
    recursive subroutine goto_char_read_natnum_interactive(prompt, n, status)
        character(len=*), intent(in) :: prompt
        integer(int64), intent(out) :: n
        type(status_type), intent(out) :: status
        integer(int64) :: number
        logical :: found, is_integer

        status = status_type()

        if (current_prefix_arg%kind /= PREFIX_NONE .and. current_prefix_arg%kind /= PREFIX_RAW) then
            n = prefix_numeric_value(current_prefix_arg)
            return
        end if

        call number_at_point(number, found, is_integer)
        found = found .and. is_integer
        if (found) found = number >= 0

        if (found) then
            call read_number(prompt, n, status, defaults=[number, point()])
        else
            call read_number(prompt, n, status, defaults=[point()], no_default=.true.)
        end if
    end subroutine goto_char_read_natnum_interactive

    !> Set point to POSITION, a number or marker.
    !> Beginning of buffer is position (point-min), end is (point-max).
    !>
    !> The return value is POSITION.
    !>
    !> If called interactively, a numeric prefix argument specifies
    !> POSITION; without a numeric prefix argument, read POSITION from the
    !> minibuffer.  The default value is the number at point (if any).
    !> Port of editfns.c:Fgoto_char (the command).
    recursive subroutine cmd_goto_char(status)
        type(status_type), intent(out) :: status
        integer(int64) :: pos

        call goto_char_read_natnum_interactive("Go to char: ", pos, status)
        if (.not. status%ok()) return
        call goto_char(min(max(pos, point_min()), point_max()))
    end subroutine cmd_goto_char

    !> Move point to column COLUMN in the current line.
    !> Interactively, COLUMN is the value of prefix numeric argument.
    !> The column of a character is calculated by adding together the widths
    !> as displayed of the previous characters in the line.
    !> This function ignores line-continuation;
    !> there is no upper limit on the column number a character can have
    !> and horizontal scrolling has no effect.
    !> Text that has an invisible property is considered as having width 0,
    !> unless `buffer_invisibility_spec' specifies that it is replaced by
    !> an ellipsis.
    !>
    !> If specified column is within a character, point goes after that character.
    !> If it's past end of line, point goes to end of line.
    !>
    !> Optional second argument FORCE true means if COLUMN is in the
    !> middle of a tab character, either change it to spaces (when
    !> `indent_tabs_mode' is .false.), or insert enough spaces before it to reach
    !> COLUMN (otherwise).  In addition, if FORCE is .true., and the line is too short
    !> to reach COLUMN, add spaces/tabs to get there.
    !>
    !> The return value is the current column.  Port of
    !> indent.c:Fmove_to_column (the command).
    recursive subroutine cmd_move_to_column(status)
        type(status_type), intent(out) :: status
        integer(int64) :: column, reached

        if (current_prefix_arg%kind == PREFIX_NONE) then
            call read_number("Move to column: ", column, status)
            if (.not. status%ok()) return
        else
            column = prefix_numeric_value(current_prefix_arg)
        end if

        call move_to_column(column, reached, .false., status)
    end subroutine cmd_move_to_column

    !> Restrict editing in this buffer to the current region.
    !> The rest of the text becomes temporarily invisible and untouchable
    !> but is not deleted; if you save the buffer in a file, the invisible
    !> text is included in the file.  \[widen] makes all visible again.
    !> See also `save_restriction'.
    !>
    !> When calling from a program, pass two arguments START and END:
    !> positions (integers or markers) bounding the text that should
    !> remain visible.  Port of editfns.c:Fnarrow_to_region (the command).
    recursive subroutine cmd_narrow_to_region(status)
        type(status_type), intent(out) :: status
        integer(int64) :: b, e

        call interactive_region(b, e, status)
        if (.not. status%ok()) return
        call narrow_to_region(b, e, status)
    end subroutine cmd_narrow_to_region

    !> Remove restrictions (narrowing) from current buffer.
    !>
    !> This allows the buffer's full text to be seen and edited.  Port of
    !> editfns.c:Fwiden (the command).
    recursive subroutine cmd_widen(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call widen()
    end subroutine cmd_widen

    module procedure count_words
    integer(int64) :: opoint, begv, zv_from_end
    type(status_type) :: st
    logical :: saved, ok

    words = 0
    ! Count across field boundaries. (Bug#41761)
    saved = inhibit_field_text_motion
    inhibit_field_text_motion = .true.
    opoint = point()
    begv = buffers(current)%begv
    zv_from_end = buffers(current)%text%z - buffers(current)%zv
    call narrow_to_region(start, end, st)
    call goto_char(point_min())

    do
        call forward_word(1_int64, ok)
        if (.not. ok) exit
        words = words + 1
    end do

    call widen()
    call narrow_to_region(begv, buffers(current)%text%z - zv_from_end, st)
    call goto_char(opoint)
    inhibit_field_text_motion = saved
    end procedure count_words

    ! The message about the accessible part of the buffer.  Port of
    ! simple.el:count-words--buffer-format.
    function count_words_buffer_format() result(text)
        character(len=:), allocatable :: text

        if (is_buffer_narrowed()) then
            text = count_words_format("Narrowed part of buffer", point_min(), point_max())
        else
            text = count_words_format("Buffer", point_min(), point_max())
        end if
    end function count_words_buffer_format

    module procedure count_words_format
    integer(int64) :: lines, sentences, words, chars

    lines = count_lines(start, end)
    sentences = count_sentences(start, end)
    words = count_words(start, end)
    chars = end - start
    text = string//" has "//counted(lines, "line")//", "//counted(sentences, "sentence")//", "//counted(words, "word") &
           //", and "//counted(chars, "character")

contains

    ! "N NOUN", with an s unless N is 1.
    function counted(n, noun) result(s)
        integer(int64), intent(in) :: n
        character(len=*), intent(in) :: noun
        character(len=:), allocatable :: s
        character(len=24) :: buffer

        write (buffer, "(i0)") n
        s = trim(buffer)//" "//noun
        if (n /= 1) s = s//"s"
    end function counted
    end procedure count_words_format

    !> Count words between START and END (the command): report the number
    !> of lines, sentences, words, and chars of the region if it is active,
    !> else of the buffer; with a prefix argument, also those of the
    !> entire (un-narrowed) buffer.  Port of simple.el:count-words.
    recursive subroutine cmd_count_words(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: totals
        integer(int64) :: b, e, begv, zv_from_end
        type(status_type) :: st

        status = status_type()
        totals = ""

        if (current_prefix_arg%kind /= PREFIX_NONE .and. (is_use_region() .or. is_buffer_narrowed())) then
            begv = buffers(current)%begv
            zv_from_end = buffers(current)%text%z - buffers(current)%zv
            call widen()
            totals = count_words_format("; buffer in total", point_min(), point_max())
            call narrow_to_region(begv, buffers(current)%text%z - zv_from_end, st)
        end if

        if (is_use_region()) then
            call region_beginning(b, status)
            if (status%ok()) call region_end(e, status)
            if (.not. status%ok()) return
            call message(count_words_format("Region", b, e)//totals)
        else
            call message(count_words_buffer_format()//totals)
        end if
    end subroutine cmd_count_words

    !> Count the number of words in the region.
    !> If called interactively, print a message reporting the number of
    !> lines, words, and characters in the region (whether or not the
    !> region is active); with prefix ARG, report for the entire buffer
    !> rather than the region.
    !>
    !> If called from a program, return the number of words between positions
    !> START and END.  Port of simple.el:count-words-region (the command;
    !> `count_words' is the function).
    recursive subroutine cmd_count_words_region(status)
        type(status_type), intent(out) :: status
        integer(int64) :: b, e

        status = status_type()

        if (current_prefix_arg%kind /= PREFIX_NONE) then
            call message(count_words_buffer_format())
            return
        end if

        call region_beginning(b, status)
        if (status%ok()) call region_end(e, status)
        if (.not. status%ok()) return
        call message(count_words_format("Region", b, e))
    end subroutine cmd_count_words_region

    module procedure syms_of_simple_goto
    commands%goto_line%execute => cmd_goto_line
    commands%goto_line_relative%execute => cmd_goto_line_relative
    commands%goto_char%execute => cmd_goto_char
    commands%move_to_column%execute => cmd_move_to_column
    commands%narrow_to_region%execute => cmd_narrow_to_region
    commands%narrow_to_region%disabled = .true.
    commands%widen%execute => cmd_widen
    commands%count_words%execute => cmd_count_words
    commands%count_words_region%execute => cmd_count_words_region
    end procedure syms_of_simple_goto
end submodule simple_goto_fm
