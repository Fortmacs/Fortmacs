! map_ynp_ask_fm.f90 -- general-purpose boolean question-asker.
! Ported from GNU Emacs 31.1 lisp/emacs-lisp/map-ynp.el.
!
! Copyright (C) 1991-1995, 2000-2026 Free Software Foundation, Inc.
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

!> The body of `map_y_or_n_p': a submodule because the minibuffer and the
!> help window (which use files_fm, which uses map_ynp_fm) can't be used
!> by the module.
submodule(map_ynp_fm) map_ynp_ask_fm
    use, intrinsic :: iso_fortran_env, only: int64
    use error_fm, only: set_error, FM_QUIT
    use lisp_fm, only: nil, nilp, symbolp, make_fixnum, make_keymap_ref
    use lread_fm, only: sym
    use data_fm, only: value_eq
    use keymap_fm, only: key_description, make_sparse_keymap, set_keymap_parent, define_key, define_key_range, &
                         lookup_key, key_parse, keymap_live_p, current_global_map
    use keyboard_fm, only: this_command, real_this_command, this_command_keys, last_message, sit_for
    use callint_fm, only: commandp, call_interactively
    use character_fm, only: MAX_CHAR
    use xdisp_fm, only: message
    use dispnew_fm, only: ding
    use minibuf_fm, only: read_from_minibuffer, minibuffer_local_map, enable_recursive_minibuffers
    use minibuffer_fm, only: exit_minibuffer
    use help_fm, only: with_help_window
    use subr_fm, only: read_key, query_replace_answer, query_replace_map, init_query_replace_map, QRM_KEYS, &
                       y_or_n_p_use_read_key, y_or_n_p_history_variable, ANSWER_ACT, ANSWER_SKIP, ANSWER_EXIT, &
                       ANSWER_ACT_AND_EXIT, ANSWER_AUTOMATIC, ANSWER_QUIT, ANSWER_HELP
    implicit none(type, external)

contains

    !> See the interface in map_ynp_fm.
    !> Ask a boolean question per PROMPTER for each object in LIST.
    !> The PROMPTER is called for objects 1..N; it returns how to treat
    !> each: YNP_SKIP to ignore it, YNP_ACT to act on it without asking,
    !> or YNP_ASK with a prompt to ask the user about it.
    !>
    !> The user's answers are: `y' or SPC to act on the object, `n' or DEL
    !> to skip it, `!' to act on it and all remaining objects, `.' to act
    !> on it and exit, `q' or RET to exit, and C-g to quit; C-h shows
    !> help.  HELP-OBJECT, HELP-OBJECTS and HELP-ACTION (default
    !> "object", "objects", "act on") name the objects and the action for
    !> the help.  `actions' is the number of actions taken.  Port of
    !> map-ynp.el:map-y-or-n-p.
    module subroutine map_y_or_n_p(n, prompter, actor, actions, status, help_object, help_objects, help_action)
        integer, intent(in) :: n
        procedure(ynp_prompter) :: prompter
        procedure(ynp_actor) :: actor
        integer, intent(out) :: actions
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: help_object, help_objects, help_action
        character(len=:), allocatable :: prompt, full, msg, str, object, objects, action
        type(value_type) :: key, def, saved_this, saved_real_this
        type(status_type) :: st
        integer :: k, how, answer
        logical :: had_msg, saved_enable, again
        actions = 0
        status = status_type()
        object = "object"
        if (present(help_object)) object = help_object
        objects = "objects"
        if (present(help_objects)) objects = help_objects
        action = "act on"
        if (present(help_action)) action = help_action
        had_msg = .false.
        if (allocated(last_message)) had_msg = len(last_message) > 0
        if (had_msg) msg = last_message
        call init_query_replace_map()
        k = 0
        objects_loop: do while (k < n)
            k = k + 1
            call prompter(k, how, prompt)
            if (how == YNP_SKIP) cycle objects_loop
            if (how == YNP_ACT) then
                call actor(k, status)
                if (.not. status%ok()) exit objects_loop
                actions = actions + 1
                cycle objects_loop
            end if
            full = prompt//"(y, n, !, ., q, or C-h) "
            again = .true.
            do while (again)
                again = .false.
                if (allocated(chars)) deallocate (chars)
                if (y_or_n_p_use_read_key) then
                    ! Prompt in the echo area using `read_key'.
                    call read_key(full, key, status)
                    if (.not. status%ok()) exit objects_loop
                    chars = [key]
                    if (is_quit(key)) then
                        call set_error(status, FM_QUIT)
                        exit objects_loop
                    end if
                    ! Show the answer to the question.
                    call message(full//key_description(chars))
                else
                    ! Read from the minibuffer.
                    call init_remap()
                    saved_this = this_command
                    saved_real_this = real_this_command
                    saved_enable = enable_recursive_minibuffers
                    enable_recursive_minibuffers = .true.
                    if (allocated(y_or_n_p_history_variable)) then
                        call read_from_minibuffer(full, str, status, keymap=ynp_remap, &
                                                  history=y_or_n_p_history_variable)
                    else
                        call read_from_minibuffer(full, str, status, keymap=ynp_remap, history="t")
                    end if
                    enable_recursive_minibuffers = saved_enable
                    this_command = saved_this
                    real_this_command = saved_real_this
                    if (.not. status%ok()) exit objects_loop
                    if (allocated(chars)) then
                        call message(full//key_description(chars))
                    else
                        call message(full)
                    end if
                end if
                answer = -1
                def = nil()
                if (allocated(chars)) then
                    answer = query_replace_answer(chars(1))
                    call lookup_key(make_keymap_ref(query_replace_map), chars, def, st)
                end if
                select case (answer)
                case (ANSWER_EXIT)
                    exit objects_loop
                case (ANSWER_ACT)
                    ! Act on the object.
                    call actor(k, status)
                    if (.not. status%ok()) exit objects_loop
                    actions = actions + 1
                case (ANSWER_SKIP)
                    ! Skip the object.
                    continue
                case (ANSWER_ACT_AND_EXIT)
                    ! Act on the object and then exit.
                    call actor(k, status)
                    if (.not. status%ok()) exit objects_loop
                    actions = actions + 1
                    exit objects_loop
                case (ANSWER_QUIT)
                    call set_error(status, FM_QUIT)
                    exit objects_loop
                case (ANSWER_AUTOMATIC)
                    ! Act on this and all following objects.
                    call actor(k, status)
                    if (.not. status%ok()) exit objects_loop
                    actions = actions + 1
                    do while (k < n)
                        k = k + 1
                        call prompter(k, how, prompt)
                        if (how == YNP_SKIP) cycle
                        call actor(k, status)
                        if (.not. status%ok()) exit objects_loop
                        actions = actions + 1
                    end do
                case (ANSWER_HELP)
                    call with_help_window("Type SPC or y to "//action//" the current "//object//";"//achar(10)// &
                                          "DEL or n to skip the current "//object//";"//achar(10)// &
                                          "RET or q to skip the current and all remaining "//objects//";"// &
                                          achar(10)//"C-g to quit (cancel the whole command);"//achar(10)// &
                                          "! to "//action//" all remaining "//objects//";"//achar(10)// &
                                          "or . (period) to "//action//" the current "//object//" and exit.", st)
                    again = .true.
                case default
                    if (symbolp(def) .and. .not. nilp(def) .and. commandp(def)) then
                        call call_interactively(def, status)
                        if (.not. status%ok()) exit objects_loop
                    else
                        ! Random char.
                        call message("Type C-h for help")
                        call ding()
                        call sit_for(1000)
                    end if
                    again = .true.
                end select
            end do
        end do objects_loop
        ! Clear the last prompt from the minibuffer, and restore the
        ! previous echo-area message, if any.
        if (had_msg) then
            call message(msg)
        else
            call message("")
        end if
    contains

        ! [?\C-g] or [ESC ESC ESC]
        logical function is_quit(ev)
            type(value_type), intent(in) :: ev
            is_quit = value_eq(ev, make_fixnum(7_int64))
        end function is_quit
    end subroutine map_y_or_n_p

    !> Make `ynp_remap' (the remap keymap of map-y-or-n-p's minibuffer
    !> path, flattened: keymap_fm has no command remapping).
    subroutine init_remap()
        type(value_type) :: key1(1)
        type(value_type), allocatable :: seq(:)
        type(value_type) :: b
        type(status_type) :: st
        integer :: k
        integer(int64) :: c
        if (keymap_live_p(ynp_remap)) return
        cmd_char = command_type("map_y_or_n_p__char", ynp_char)
        call cmd_char%define()
        cmd_help = command_type("map_y_or_n_p__help", ynp_help)
        call cmd_help%define()
        call make_sparse_keymap(ynp_remap)
        call set_keymap_parent(ynp_remap, minibuffer_local_map, st)
        ! [remap self-insert-command]
        do c = 0, 127
            key1(1) = make_fixnum(c)
            call lookup_key(make_keymap_ref(minibuffer_local_map), key1, b, st)
            if (nilp(b)) call lookup_key(make_keymap_ref(current_global_map()), key1, b, st)
            if (value_eq(b, sym("self_insert_command"))) then
                call define_key(ynp_remap, key1, sym("map_y_or_n_p__help"), st)
            end if
        end do
        key1(1) = make_fixnum(128_int64)
        call lookup_key(make_keymap_ref(current_global_map()), key1, b, st)
        if (value_eq(b, sym("self_insert_command"))) then
            call define_key_range(ynp_remap, 128_int64, int(MAX_CHAR, int64), sym("map_y_or_n_p__help"), st)
        end if
        do k = 1, size(QRM_KEYS)
            call key_parse(trim(QRM_KEYS(k)), seq, st)
            if (st%ok()) call define_key(ynp_remap, seq, sym("map_y_or_n_p__char"), st)
        end do
    end subroutine init_remap

    !> cmd-char: record the keys and exit the minibuffer.
    subroutine ynp_char(status)
        type(status_type), intent(out) :: status
        chars = this_command_keys
        call exit_minibuffer(status)
    end subroutine ynp_char

    !> cmd-help: say how to get help.
    subroutine ynp_help(status)
        type(status_type), intent(out) :: status
        status = status_type()
        call message("Type C-h for help")
    end subroutine ynp_help
end submodule map_ynp_ask_fm
