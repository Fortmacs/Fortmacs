! subr_read_fm.f90 -- read-number, a submodule of subr_fm.
! Ported from GNU Emacs 31.1 lisp/subr.el.
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

!> The minibuffer readers of subr.el (`read_number', `y_or_n_p'): a
!> submodule because minibuf_fm uses subr_fm.
submodule(subr_fm) subr_read_fm
    use commands_fm, only: commands
    use minibuf_fm, only: read_from_minibuffer, minibuffer_local_map, enable_recursive_minibuffers, minibufferp
    use minibuffer_fm, only: delete_minibuffer_contents, exit_minibuffer, minibuffer_message
    use keyboard_fm, only: sit_for, this_command, real_this_command, abort_recursive_edit
    use keymap_fm, only: set_keymap_parent, define_key_range, free_keymap
    use data_fm, only: value_eq
    use character_fm, only: MAX_CHAR
    use lisp_fm, only: make_fixnum
    use insdel_fm, only: insert
    use dispnew_fm, only: ding
    use window_fm, only: recenter, scroll_up_command, scroll_down_command
    implicit none(type, external)

    !> y-or-n-p-map, made by `init_y_or_n_p_map'.
    type(handle_type) :: y_or_n_p_map

contains

    module procedure read_number
    character(len=:), allocatable :: full, str, def, histvar
    character(len=24) :: buf
    integer :: ios, k
    logical :: have
    status = status_type()
    n = 0
    full = prompt
    histvar = "read_number_history"
    if (present(hist)) histvar = hist
    def = ""
    if (present(default)) then
        write (buf, "(i0)") default
        def = trim(buf)
        k = index(full, ":", back=.true.)
        if (k > 0 .and. len_trim(full(k + 1:)) == 0) then
            full = full(1:k - 1)//" (default "//def//")"//full(k:)
        else
            full = trim(full)//" (default "//def//")"
        end if
    end if
    do
        if (present(default)) then
            call read_from_minibuffer(full, str, status, history=histvar, default=def)
        else
            call read_from_minibuffer(full, str, status, history=histvar)
        end if
        if (.not. status%ok()) return
        have = .false.
        if (len(str) == 0) then
            if (present(default)) then
                n = default
                have = .true.
            end if
        else if (verify(trim(adjustl(str)), "+-0123456789") == 0) then
            read (str, *, iostat=ios) n
            have = ios == 0
        end if
        if (have) exit
        call message("Please enter a number.")
        call sit_for(1000)
    end do
    end procedure read_number
    module procedure y_or_n_p
    character(len=:), allocatable :: padded, str
    type(value_type) :: key, saved_this, saved_real_this
    type(status_type) :: st
    integer :: def
    logical :: saved_enable, again
    answer = .false.
    status = status_type()
    padded = prompt
    if (len(prompt) > 0) then
        if (prompt(len(prompt):len(prompt)) /= " ") padded = padded//" "
    end if
    padded = padded//"(y or n) "
    if (y_or_n_p_use_read_key) then
        def = ANSWER_RECENTER
        do
            if (def == ANSWER_RECENTER .or. def == ANSWER_SCROLL_UP .or. def == ANSWER_SCROLL_DOWN .or. &
                def == ANSWER_SCROLL_OTHER_WINDOW .or. def == ANSWER_SCROLL_OTHER_WINDOW_DOWN) then
                call read_key(padded, key, status)
            else
                call read_key("Please answer y or n.  "//padded, key, status)
            end if
            if (.not. status%ok()) return
            def = query_replace_answer(key)
            again = .true.
            select case (def)
            case (ANSWER_SKIP, ANSWER_ACT)
                again = .false.
            case (ANSWER_RECENTER)
                call recenter(nil(), .false., st)
            case (ANSWER_SCROLL_UP)
                call scroll_up_command(nil(), st)
            case (ANSWER_SCROLL_DOWN)
                call scroll_down_command(nil(), st)
            case (ANSWER_SCROLL_OTHER_WINDOW, ANSWER_SCROLL_OTHER_WINDOW_DOWN)
                ! scroll-other-window and scroll-other-window-down are not
                ! ported (Deferred).
                continue
            case (ANSWER_EXIT_PREFIX, ANSWER_QUIT)
                call set_error(status, FM_QUIT)
                return
            case default
                if (fixnump(key)) then
                    if (xfixnum(key) == 27) then
                        call set_error(status, FM_QUIT)
                        return
                    end if
                end if
            end select
            if (.not. again) exit
            call ding()
            call discard_input()
        end do
        answer = def == ANSWER_ACT
    else
        call init_y_or_n_p_map()
        saved_enable = enable_recursive_minibuffers
        enable_recursive_minibuffers = .true.
        ! Protect this-command, and preserve the actual command that
        ! eventually called `y_or_n_p'.
        saved_this = this_command
        saved_real_this = real_this_command
        if (allocated(y_or_n_p_history_variable)) then
            call read_from_minibuffer(padded, str, status, keymap=y_or_n_p_map, history=y_or_n_p_history_variable)
        else
            call read_from_minibuffer(padded, str, status, keymap=y_or_n_p_map, history="t")
        end if
        enable_recursive_minibuffers = saved_enable
        this_command = saved_this
        real_this_command = saved_real_this
        if (.not. status%ok()) return
        answer = str == "y" .or. str == "Y"
    end if
    if (answer) then
        call message(padded//"y")
    else
        call message(padded//"n")
    end if
    end procedure y_or_n_p

    !> Make `y_or_n_p_map': y-or-n-p-map composed with `query_replace_map'
    !> and its command remappings, flattened, as keymap_fm has neither
    !> composed keymaps nor command remapping: each key of
    !> query-replace-map that minibuffer-local-map (its parent) doesn't
    !> bind gets the command its answer is remapped to (or the answer
    !> itself when it isn't), and keys that would run
    !> `self_insert_command' run `y_or_n_p_insert_other'.  Port of
    !> subr.el y-or-n-p-map (minibuffer-scroll-other-window and
    !> minibuffer-scroll-other-window-down are not ported: Deferred).
    subroutine init_y_or_n_p_map()
        type(value_type), allocatable :: seq(:)
        type(value_type) :: b
        type(status_type) :: st
        integer :: k
        integer(int64) :: c
        if (keymap_live_p(y_or_n_p_map)) return
        call init_query_replace_map()
        call make_sparse_keymap(y_or_n_p_map)
        call set_keymap_parent(y_or_n_p_map, minibuffer_local_map, st)
        do k = 1, size(QRM_KEYS)
            call key_parse(trim(QRM_KEYS(k)), seq, st)
            if (.not. st%ok()) cycle
            call lookup_key(make_keymap_ref(minibuffer_local_map), seq, b, st)
            if (.not. nilp(b)) cycle
            call define_key(y_or_n_p_map, seq, sym(remapped(trim(QRM_ANSWERS(k)))), st)
        end do
        ! [remap self-insert-command]
        do c = 0, 127
            call lookup_key(make_keymap_ref(minibuffer_local_map), [make_fixnum(c)], b, st)
            if (nilp(b)) then
                call lookup_key(make_keymap_ref(query_replace_map), [make_fixnum(c)], b, st)
                if (.not. nilp(b)) cycle
                call lookup_key(make_keymap_ref(current_global_map()), [make_fixnum(c)], b, st)
            end if
            if (value_eq(b, sym("self_insert_command"))) then
                call define_key(y_or_n_p_map, [make_fixnum(c)], sym("y_or_n_p_insert_other"), st)
            end if
        end do
        call lookup_key(make_keymap_ref(current_global_map()), [make_fixnum(128_int64)], b, st)
        if (value_eq(b, sym("self_insert_command"))) then
            call define_key_range(y_or_n_p_map, 128_int64, int(MAX_CHAR, int64), sym("y_or_n_p_insert_other"), st)
        end if
    contains

        function remapped(answer) result(cmd)
            character(len=*), intent(in) :: answer
            character(len=:), allocatable :: cmd
            select case (answer)
            case ("act", "act_and_show", "act_and_exit", "automatic")
                cmd = "y_or_n_p_insert_y"
            case ("skip")
                cmd = "y_or_n_p_insert_n"
            case ("backup", "undo", "undo_all", "edit", "edit_replacement", "delete_and_edit", "exit")
                cmd = "y_or_n_p_insert_other"
            case ("recenter")
                cmd = "minibuffer_recenter_top_bottom"
            case ("scroll_up")
                cmd = "minibuffer_scroll_up_command"
            case ("scroll_down")
                cmd = "minibuffer_scroll_down_command"
            case ("exit_prefix", "quit")
                cmd = "abort_recursive_edit"
            case default
                cmd = answer
            end select
        end function remapped
    end subroutine init_y_or_n_p_map

    !> Insert the answer "y" and exit the minibuffer of `y_or_n_p'.
    !> Discard all previous input before inserting and exiting the minibuffer.
    !> Port of subr.el:y-or-n-p-insert-y.
    subroutine y_or_n_p_insert_y(status)
        type(status_type), intent(out) :: status
        call insert_answer("y", status)
    end subroutine y_or_n_p_insert_y

    !> Insert the answer "n" and exit the minibuffer of `y_or_n_p'.
    !> Discard all previous input before inserting and exiting the minibuffer.
    !> Port of subr.el:y-or-n-p-insert-n.
    subroutine y_or_n_p_insert_n(status)
        type(status_type), intent(out) :: status
        call insert_answer("n", status)
    end subroutine y_or_n_p_insert_n

    subroutine insert_answer(text, status)
        character(len=*), intent(in) :: text
        type(status_type), intent(out) :: status
        status = status_type()
        if (.not. minibufferp()) return
        call delete_minibuffer_contents(status)
        if (.not. status%ok()) return
        call insert(text, status)
        if (.not. status%ok()) return
        call exit_minibuffer(status)
    end subroutine insert_answer

    !> Handle inserting of other answers in the minibuffer of `y_or_n_p'.
    !> Display an error on trying to insert a disallowed character.
    !> Also discard all previous input in the minibuffer.
    !> Port of subr.el:y-or-n-p-insert-other.
    subroutine y_or_n_p_insert_other(status)
        type(status_type), intent(out) :: status
        status = status_type()
        if (.not. minibufferp()) return
        call delete_minibuffer_contents(status)
        if (.not. status%ok()) return
        call ding()
        call discard_input()
        call minibuffer_message("Please answer y or n")
        call sit_for(2000)
    end subroutine y_or_n_p_insert_other

    !> Abort the command that requested this recursive edit or minibuffer input.
    !> Port of keyboard.c:abort-recursive-edit.
    subroutine cmd_abort_recursive_edit(status)
        type(status_type), intent(out) :: status
        call abort_recursive_edit(status)
    end subroutine cmd_abort_recursive_edit

    module procedure syms_of_subr
    type(status_type) :: st
    commands%y_or_n_p_insert_y%run => y_or_n_p_insert_y
    commands%y_or_n_p_insert_n%run => y_or_n_p_insert_n
    commands%y_or_n_p_insert_other%run => y_or_n_p_insert_other
    commands%abort_recursive_edit%run => cmd_abort_recursive_edit
    ! The map is made again on first use (its parent and the global map
    ! may have been rebuilt).
    if (keymap_live_p(y_or_n_p_map)) call free_keymap(y_or_n_p_map, st)
    end procedure syms_of_subr

end submodule subr_read_fm
