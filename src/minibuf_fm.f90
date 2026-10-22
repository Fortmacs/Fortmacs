! minibuf_fm.f90 -- minibuffer input.
! Ported from GNU Emacs 31.1 src/minibuf.c.
!
! Copyright (C) 1985-1986, 1993-2026 Free Software Foundation, Inc.
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

!> Reading a line of input in the minibuffer: the prompt is read-only
!> text in a field at the start of buffer " *Minibuf-N*", shown in the
!> frame's minibuffer window, which is selected while a recursive edit
!> runs; a command throws to `exit` to finish.
!>
!> Not yet: completion, minibuffer-only frames, input methods, Lisp
!> reading (`read-minibuffer`), `minibuffer-setup-hook`, the
!> minibuffer-prompt face, and restoring window configurations.
module minibuf_fm
    use, intrinsic :: iso_fortran_env, only: int64
    use error_fm, only: status_type, set_error, FM_GENERIC_ERROR, FM_USER_ERROR
    use handle_fm, only: handle_type
    use lisp_fm, only: value_type, nil, t_value, list, nilp
    use lread_fm, only: sym
    use data_fm, only: set
    use buffer_fm, only: buffers, cur, current_buffer, set_buffer, get_buffer_create, &
                         erase_buffer, resolve_buffer
    use insdel_fm, only: insert
    use editfns_fm, only: field_end, buffer_substring, goto_char
    use textprop_fm, only: add_text_properties, get_text_property
    use keymap_fm, only: make_sparse_keymap, use_local_map, keymap_live_p
    use keyboard_fm, only: recursive_edit_1, throw_exit, command_loop_level, post_command_hook, &
                           redisplay_proc, last_message
    use frame_fm, only: selected_frame
    use window_fm, only: set_window_buffer
    use subr_fm, only: add_to_history
    implicit none(type, external)
    private

    public :: read_from_minibuffer, read_string, minibuffer_prompt_end, minibuffer_contents, &
              minibufferp, abort_minibuffers, init_minibuf
    !> minibuf_level: the depth of active minibuffers.
    integer, public, protected :: minibuf_level = 0
    !> Non-nil means to allow minibuffer commands while in the minibuffer.
    !> This variable makes a difference whenever the minibuffer window is active.
    !> Port of minibuf.c enable-recursive-minibuffers.
    logical, public :: enable_recursive_minibuffers = .false.
    !> minibuffer-local-map, the default keymap for reading.
    type(handle_type), public :: minibuffer_local_map
    !> Non-nil means to add new elements in history.
    !> If set to false, minibuffer reading functions don't add new elements to the
    !> history list, so it is possible to do this afterwards by calling
    !> `add_to_history` explicitly.  Port of minibuf.c history-add-new-input.
    logical, public :: history_add_new_input = .true.

    !> Vminibuffer_history_variable: the history list's symbol name.
    character(len=:), allocatable, public :: minibuffer_history_variable
    !> Vminibuffer_history_position: the element shown (0: the input).
    integer, public :: minibuffer_history_position = 0
    !> minibuffer-default: the default value, for M-n ("future history");
    !> unallocated for none.
    character(len=:), allocatable, public :: minibuffer_default
    !> simple.el minibuffer-text-before-history: the input before moving
    !> into the history; unallocated for nil.
    character(len=:), allocatable, public :: minibuffer_text_before_history

    !> Vminibuffer_list: the minibuffer of each depth (index depth + 1).
    type(handle_type), allocatable :: minibuffers(:)

contains

    !> Create minibuffer-local-map (its bindings are made by minibuffer.el
    !> and bindings.el, i.e. minibuffer_fm and bindings_fm).
    subroutine init_minibuf()
        if (.not. keymap_live_p(minibuffer_local_map)) call make_sparse_keymap(minibuffer_local_map)
        minibuf_level = 0
    end subroutine init_minibuf

    !> The minibuffer for depth `depth`, " *Minibuf-DEPTH*", made if
    !> needed.  Port of minibuf.c:get_minibuffer.
    subroutine get_minibuffer(depth, buf, status)
        integer, intent(in) :: depth
        type(handle_type), intent(out) :: buf
        type(status_type), intent(out) :: status
        type(handle_type), allocatable :: grown(:)
        character(len=20) :: num
        integer(int64) :: i

        if (.not. allocated(minibuffers)) allocate (minibuffers(4))

        if (depth + 1 > size(minibuffers)) then
            allocate (grown(2*(depth + 1)))
            grown(1:size(minibuffers)) = minibuffers
            call move_alloc(grown, minibuffers)
        end if

        call resolve_buffer(minibuffers(depth + 1), i, status)

        if (status%ok()) then
            buf = minibuffers(depth + 1)
            return
        end if

        write (num, "(i0)") depth
        call get_buffer_create(" *Minibuf-"//trim(num)//"*", buf, status)
        if (.not. status%ok()) return
        minibuffers(depth + 1) = buf
    end subroutine get_minibuffer

    !> Whether `buf` (default: the current buffer) is a minibuffer.
    !> Port of minibuf.c:Fminibufferp.
    logical function minibufferp(buf)
        type(handle_type), intent(in), optional :: buf
        type(handle_type) :: b
        integer :: k

        if (present(buf)) then
            b = buf
        else
            b = current_buffer()
        end if

        minibufferp = .false.
        if (.not. allocated(minibuffers)) return

        do k = 1, size(minibuffers)
            if (minibuffers(k)%index == b%index .and. minibuffers(k)%generation == b%generation .and. &
                b%index /= 0) then
                minibufferp = .true.
                return
            end if
        end do
    end function minibufferp

    !> Where the prompt ends in the current minibuffer (point-min if the
    !> current buffer is not one).  Port of minibuf.c:Fminibuffer_prompt_end.
    integer(int64) function minibuffer_prompt_end() result(pos)
        type(value_type) :: field
        type(status_type) :: status

        pos = buffers(cur)%begv
        if (.not. minibufferp()) return
        pos = field_end(buffers(cur)%begv, .false.)

        if (pos == buffers(cur)%zv) then
            call get_text_property(buffers(cur)%begv, sym("field"), field, status)
            if (nilp(field)) pos = buffers(cur)%begv
        end if
    end function minibuffer_prompt_end

    !> The text of the current minibuffer after the prompt.  Port of
    !> minibuf.c:Fminibuffer_contents_no_properties.
    function minibuffer_contents() result(text)
        character(len=:), allocatable :: text
        type(status_type) :: status

        call buffer_substring(minibuffer_prompt_end(), buffers(cur)%zv, text, status)
    end function minibuffer_contents

    !> Keep point out of the prompt after each command (the prompt's
    !> cursor-intangible property, as cursor-intangible-mode would).
    subroutine leave_prompt()
        integer(int64) :: pend

        if (.not. minibufferp()) return
        pend = minibuffer_prompt_end()
        if (buffers(cur)%pt < pend) call goto_char(pend)
    end subroutine leave_prompt

    !> Read a string in the minibuffer with `prompt`, starting with
    !> `initial` (point after it), using `keymap` (default
    !> minibuffer-local-map) and history list `history` (default
    !> "minibuffer-history").  C-g (abort-minibuffers) gives a quit.
    !> Port of minibuf.c:read_minibuf with Fread_from_minibuffer (no
    !> READ, no INHERIT-INPUT-METHOD, DEFAULT only for the history).
    recursive subroutine read_from_minibuffer(prompt, result, status, initial, keymap, history, default)
        character(len=*), intent(in) :: prompt
        character(len=:), allocatable, intent(out) :: result
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: initial, history, default
        type(handle_type), intent(in), optional :: keymap
        type(handle_type) :: calling_buffer, minibuffer, old_mini_buffer
        type(status_type) :: st, edit_status
        character(len=:), allocatable :: ambient_dir, saved_histvar, histvar, saved_default, &
                                         saved_before
        integer :: saved_histpos
        logical :: had_default, had_before
        logical :: changed, old_selected, have_dir
        procedure(redisplay_proc), pointer :: saved_hook

        result = ""

        if (.not. enable_recursive_minibuffers .and. minibuf_level > 0) then
            call set_error(status, FM_USER_ERROR, "Command attempted to use minibuffer while in minibuffer")
            return
        end if

        histvar = "minibuffer-history"
        if (present(history)) histvar = history

        calling_buffer = current_buffer()
        have_dir = allocated(buffers(cur)%directory)
        if (have_dir) ambient_dir = buffers(cur)%directory
        call get_minibuffer(minibuf_level + 1, minibuffer, status)
        if (.not. status%ok()) return
        minibuf_level = minibuf_level + 1

        ! minibuf_save_list
        if (allocated(minibuffer_history_variable)) then
            saved_histvar = minibuffer_history_variable
        else
            saved_histvar = ""
        end if

        saved_histpos = minibuffer_history_position
        minibuffer_history_variable = histvar
        minibuffer_history_position = 0
        had_default = allocated(minibuffer_default)
        if (had_default) call move_alloc(minibuffer_default, saved_default)
        if (present(default)) minibuffer_default = default
        had_before = allocated(minibuffer_text_before_history)
        if (had_before) call move_alloc(minibuffer_text_before_history, saved_before)
        old_selected = selected_frame%minibuffer_selected
        old_mini_buffer = selected_frame%minibuffer_window%buffer
        saved_hook => post_command_hook

        call set_buffer(minibuffer, status)

        if (.not. status%ok()) then
            minibuf_level = minibuf_level - 1
            return
        end if

        ! The minibuffer works in the caller's directory.
        if (have_dir) buffers(cur)%directory = ambient_dir
        call set_window_buffer(selected_frame%minibuffer_window, minibuffer, st)
        selected_frame%minibuffer_selected = .true.
        selected_frame%minibuffer_window%hscroll = 0

        ! The prompt: read-only text in its own field.
        call set(sym("inhibit-read-only"), t_value(), st)
        call erase_buffer(st)
        call set(sym("inhibit-read-only"), nil(), st)
        buffers(cur)%multibyte = .true.

        if (len(prompt) > 0) then
            call insert(prompt, st)
            ! front-sticky, rear-nonsticky and field, then
            ! minibuffer-prompt-properties (read-only, cursor-intangible;
            ! the minibuffer-prompt face is not ported).
            call add_text_properties(1_int64, buffers(cur)%pt, &
                                     list(sym("front-sticky"), t_value(), sym("rear-nonsticky"), t_value(), &
                                          sym("field"), t_value(), sym("read-only"), t_value()), changed, st)
            call add_text_properties(1_int64, buffers(cur)%pt, list(sym("cursor-intangible"), t_value()), &
                                     changed, st)
        end if

        if (present(initial)) call insert(initial, st)

        if (present(keymap)) then
            call use_local_map(keymap, st)
        else
            call use_local_map(minibuffer_local_map, st)
        end if

        post_command_hook => leave_prompt
        ! The minibuffer replaces whatever the echo area showed.
        if (allocated(last_message)) last_message = ""

        call recursive_edit_1(edit_status)

        ! Make the minibuffer contents into a string.
        call set_buffer(minibuffer, st)
        result = minibuffer_contents()

        ! read_minibuf_unwind
        post_command_hook => saved_hook
        minibuf_level = minibuf_level - 1
        call set(sym("inhibit-read-only"), t_value(), st)
        call erase_buffer(st)
        call set(sym("inhibit-read-only"), nil(), st)
        selected_frame%minibuffer_selected = old_selected
        selected_frame%minibuffer_window%buffer = old_mini_buffer
        call set_buffer(calling_buffer, st)
        minibuffer_history_variable = saved_histvar
        minibuffer_history_position = saved_histpos
        if (allocated(minibuffer_default)) deallocate (minibuffer_default)
        if (had_default) call move_alloc(saved_default, minibuffer_default)
        if (allocated(minibuffer_text_before_history)) deallocate (minibuffer_text_before_history)
        if (had_before) call move_alloc(saved_before, minibuffer_text_before_history)

        if (.not. edit_status%ok()) then
            status = edit_status
            result = ""
            return
        end if

        ! Add the value (or the default, for empty input) to the history.
        if (history_add_new_input) then
            if (len(result) > 0) then
                call add_to_history(histvar, result)
            else if (present(default)) then
                call add_to_history(histvar, default)
            end if
        end if
    end subroutine read_from_minibuffer

    !> Read a string; empty input gives `default` if present.
    !> Port of minibuf.c:Fread_string.
    recursive subroutine read_string(prompt, result, status, initial, history, default)
        character(len=*), intent(in) :: prompt
        character(len=:), allocatable, intent(out) :: result
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: initial, history, default

        call read_from_minibuffer(prompt, result, status, initial=initial, history=history, &
                                  default=default)
        if (.not. status%ok()) return
        if (len(result) == 0 .and. present(default)) result = default
    end subroutine read_string

    !> Abort the current minibuffer (C-g there).  Port of
    !> minibuf.c:Fabort_minibuffers for the innermost minibuffer.
    subroutine abort_minibuffers(status)
        type(status_type), intent(out) :: status

        if (minibuf_level == 0) then
            call set_error(status, FM_GENERIC_ERROR, "Not in a minibuffer")
            return
        end if

        if (command_loop_level < minibuf_level) then
            call set_error(status, FM_GENERIC_ERROR, "Not in most nested command loop")
            return
        end if

        ! minibuffer-quit-recursive-edit
        call throw_exit(.true.)
    end subroutine abort_minibuffers
end module minibuf_fm
