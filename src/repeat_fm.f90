! repeat_fm.f90 -- convenient way to repeat the previous command.
! Ported from GNU Emacs 31.1 lisp/repeat.el.
!
! Copyright (C) 1998, 2001-2026 Free Software Foundation, Inc.
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

!> `repeat' (C-x z) and Repeat mode.  A command's repeat properties
!> (`repeat-map', `repeat-continue', `repeat-check-key',
!> `repeat-exit-timeout', `repeat-hint') are components of its
!> `command_type' (`commands%undo%repeat_map'); a repeat map is the
!> keymap itself, compared by handle where upstream compares symbols.
!>
!> Deferred: repeating a command with the arguments it read in the
!> minibuffer (`command-history': the command is called interactively
!> again), keyboard macros as commands, `repeat-echo-mode-line' (the
!> mode-line indicator), the faces of the keys in the echo-area hint
!> (read-multiple-choice-face), `repeat-message-function' and a
!> `repeat-on-final-keystroke' list (the variable is a logical), and the
!> `repeat-mode' customization setter for `repeat-keep-prefix'.
module repeat_fm
    use, intrinsic :: iso_fortran_env, only: int32, int64, real64
    use error_fm, only: status_type, set_error, format_message, FM_GENERIC_ERROR
    use handle_fm, only: handle_type, null_handle
    use lisp_fm, only: value_type, command_type, REPEAT_CHECK_KEY_YES, REPEAT_CHECK_KEY_NO, V_INTEGER, &
                       V_NAME, vtag, xfixnum, make_keymap_ref
    use eval_fm, only: hook_type, hook_current_prefix_arg
    use commands_fm, only: commands, builtin_command, BUILTIN_COMMAND_NAMES
    use modes_fm, only: minor_modes
    use callint_fm, only: call_interactively, current_prefix_arg, prefix_arg, this_command, current_minibuffer_command, &
                          command_of, prefix_arg_type, PREFIX_NONE, PREFIX_NUMBER, PREFIX_RAW, PREFIX_MINUS
    use keyboard_fm, only: real_this_command, last_command_event, last_prefix_arg, last_repeatable_command, &
                           num_input_keys, set_transient_map, exit_transient_map, pre_command_hook, post_command_hook, &
                           last_message
    use keymap_fm, only: make_sparse_keymap, free_keymap, is_keymap_live, define_key, lookup_key, keymap_events, &
                         access_keymap, copy_keymap, key_parse, key_description, parse_modifiers, is_keymap
    use character_fm, only: CHAR_MODIFIER_MASK
    use minibuf_fm, only: minibuf_level
    use timer_fm, only: run_with_idle_timer, free_timer, is_timer
    use xdisp_fm, only: message, message_nolog
    use rmc_fm, only: rmc_choice_type, rmc_add_key_description
    use editfns_fm, only: char_before
    implicit none(type, external)
    private

    public :: repeat, repeat_mode, repeat_exit, repeat_is_really_this_command, repeat_echo_message_string, &
              repeat_echo_message, repeat_post_hook, repeat_pre_hook, syms_of_repeat, repeat_echo_proc

    !> Commands too dangerous to repeat with \[repeat].  (Each name is
    !> used without its trailing blanks.)  Port of repeat.el
    !> repeat-too-dangerous.
    character(len=:), allocatable, public :: repeat_too_dangerous(:)

    !> Allow `repeat' to re-execute for repeating lastchar of a key sequence.
    !> If this variable is true, `repeat' determines what key sequence
    !> it was invoked by, extracts the final character of that sequence, and
    !> re-executes as many times as that final character is hit; so for example
    !> if `repeat' is bound to C-x z, typing C-x z z z repeats the previous command
    !> 3 times.  If this variable is false, no re-execution occurs.  Port of
    !> repeat.el repeat-on-final-keystroke (no list of characters).
    logical, public :: repeat_on_final_keystroke = .true.

    !> # key sequences read in Emacs session when `repeat' last invoked.
    !> Port of repeat.el repeat-num-input-keys-at-repeat.
    integer(int64), public :: repeat_num_input_keys_at_repeat = -1

    !> The previous repeated command.  Port of repeat.el
    !> repeat-previous-repeated-command.
    type(command_type), pointer, public :: repeat_previous_repeated_command => null()

    !> Key that stops the modal repeating of keys in sequence.
    !> For example, you can set it to <return> like `isearch_exit'.
    !> (Unallocated for none.)  Port of repeat.el repeat-exit-key.
    character(len=:), allocatable, public :: repeat_exit_key

    !> Break the repetition chain of keys after specified amount of idle time.
    !> When a positive number, exit the transient repeating mode after idle
    !> time of the specified number of seconds (zero or negative: none).
    !> You can also set the component `repeat_exit_timeout' of the command.
    !> This component can override the value of this variable.  Port of
    !> repeat.el repeat-exit-timeout.
    real, public :: repeat_exit_timeout = 0.0

    !> Whether to keep the prefix arg of the previous command when repeating.
    !> Port of repeat.el repeat-keep-prefix.
    logical, public :: repeat_keep_prefix = .false.

    !> Whether to check that the last key exists in the repeat map.
    !> When true, and the last typed key (with or without modifiers)
    !> doesn't exist in the keymap specified by the `repeat_map' component
    !> of the command, don't activate that keymap for the next command.
    !> Thus, when this is true, only the same keys among repeatable
    !> keys are allowed in the repeating sequence.  For example, with a
    !> true value, only \`C-x u u' repeats undo, whereas \`C-/ u' doesn't.
    !>
    !> You can also set the component `repeat_check_key' of the command.
    !> This component can override the value of this variable.
    !> When the variable value is true, but the component is `REPEAT_CHECK_KEY_NO',
    !> then don't check the last key.  Also when the variable value is false,
    !> but the component is `REPEAT_CHECK_KEY_YES', then check the last key.
    !> Port of repeat.el repeat-check-key.
    logical, public :: repeat_check_key = .true.

    abstract interface
        !> A `repeat_echo_function': display a hint about the keys of
        !> KEYMAP, or (the null handle) that repeating ended.  Not a port.
        recursive subroutine repeat_echo_proc(keymap)
            import :: handle_type
            implicit none(type, external)
            type(handle_type), intent(in) :: keymap
        end subroutine repeat_echo_proc
    end interface

    !> Function to display a hint about available keys.
    !> The function is called after every repeatable command with one argument:
    !> a repeating map, or the null handle after deactivating the transient
    !> repeating mode (null for no visual feedback).  Port of repeat.el
    !> repeat-echo-function.
    procedure(repeat_echo_proc), pointer, public :: repeat_echo_function => null()

    !> The repeating transient map while it is active (the null handle
    !> otherwise).  Port of repeat.el repeat-in-progress.
    type(handle_type), public :: repeat_in_progress

    !> The value of the repeating transient map for the next command.
    !> A command called from the map can set it again to the same map when
    !> the map can't be set on the command's `repeat_map'.  Port of
    !> repeat.el repeat-map.
    type(handle_type), public :: repeat_map

    !> Hook run after entering or leaving `repeat_mode'.
    type(hook_type), target, public :: repeat_mode_hook = hook_type(name="repeat_mode_hook")

    !> Whether the exit function of the transient map is pending
    !> (repeat--transient-exitfun), the timer of the idle exit
    !> (repeat-exit-timer), and the copy of a map with the exit key.
    logical :: transient_exit_pending = .false.
    type(handle_type) :: repeat_exit_timer, exit_key_map, transient_map
    !> The previous minibuffer state (repeat--prev-mb).
    integer :: prev_mb_depth = 0
    type(command_type), pointer :: prev_mb_command => null()

    !> The one-key map `repeat' sets up for its final keystroke.
    type(handle_type) :: final_key_map

contains

    !> Return true if this command is happening because user invoked `repeat'.
    !> Usually, when a command is executing, the Emacs builtin variable
    !> `this_command' identifies the command the user invoked.  Some commands modify
    !> that variable on the theory they're doing more good than harm; `repeat' does
    !> that, and usually does do more good than harm.  However, like all do-gooders,
    !> sometimes `repeat' gets surprising results from its altruism.  The value of
    !> this function is always whether the value of `this_command' would've been
    !> `repeat' if `repeat' hadn't modified it.  Port of
    !> repeat.el:repeat-is-really-this-command.
    logical function repeat_is_really_this_command()
        repeat_is_really_this_command = repeat_num_input_keys_at_repeat == num_input_keys
    end function repeat_is_really_this_command

    !> Repeat most recently executed command.
    !> If REPEAT_ARG is true (interactively, with a prefix argument),
    !> supply a prefix argument to that command.  Otherwise, give the
    !> command the same prefix argument it was given before, if any.
    !>
    !> If this command is invoked by a multi-character key sequence, it
    !> can then be repeated by repeating the final character of that
    !> sequence.  This behavior can be modified by the global variable
    !> `repeat_on_final_keystroke'.
    !>
    !> `repeat' ignores commands bound to input events.  Hence the term
    !> "most recently executed command" shall be read as "most
    !> recently executed command not bound to an input event".  Port of
    !> repeat.el:repeat.
    recursive subroutine repeat(status)
        type(status_type), intent(out) :: status
        type(prefix_arg_type) :: repeat_arg
        type(value_type) :: repeat_repeat_char
        type(value_type) :: key(1)
        type(status_type) :: st
        character(len=:), allocatable :: name
        integer(int32) :: c
        logical :: found

        status = status_type()
        repeat_arg = current_prefix_arg
        if (associated(last_repeatable_command, commands%repeat)) last_repeatable_command => repeat_previous_repeated_command

        ! (Upstream also refuses `mode-exit', a name that nothing sets.)
        if (.not. associated(last_repeatable_command)) then
            call set_error(status, FM_GENERIC_ERROR, "There is nothing to repeat")
            return
        end if

        name = last_repeatable_command%name

        if (allocated(repeat_too_dangerous)) then
            if (any(repeat_too_dangerous == name)) then
                call set_error(status, FM_GENERIC_ERROR, "Command "//name//" too dangerous to repeat automatically")
                return
            end if
        end if

        this_command => last_repeatable_command
        repeat_previous_repeated_command => last_repeatable_command
        repeat_num_input_keys_at_repeat = num_input_keys
        if (repeat_arg%kind == PREFIX_NONE) repeat_arg = last_prefix_arg
        repeat_repeat_char = value_type()
        if (repeat_on_final_keystroke) repeat_repeat_char = last_command_event

        if (repeat_arg%kind == PREFIX_NONE) then
            call repeat_message("Repeating command "//name)
        else
            current_prefix_arg = repeat_arg
            call repeat_message("Repeating command "//prefix_arg_text(repeat_arg)//" "//name)
        end if

        if (name == "self_insert_command") then
            call char_before(c, found)

            if (found) then
                last_command_event = value_type(int(c, int64))
            else
                last_command_event = value_type()
            end if
        end if

        call call_interactively(last_repeatable_command, status)
        if (.not. status%ok()) return

        if (allocated(repeat_repeat_char%a)) then
            if (is_keymap_live(final_key_map)) call free_keymap(final_key_map, st)
            call make_sparse_keymap(final_key_map)
            key(1) = repeat_repeat_char
            call define_key(final_key_map, key, value_type(name="repeat"), st)
            call set_transient_map(final_key_map)
        end if
    end subroutine repeat

    !> The printed form of prefix argument ARG (%S): "5", "-" or "(4)".
    !> Not a port.
    function prefix_arg_text(arg) result(text)
        type(prefix_arg_type), intent(in) :: arg
        character(len=:), allocatable :: text
        character(len=24) :: buf

        write (buf, "(i0)") arg%value

        select case (arg%kind)
        case (PREFIX_NUMBER)
            text = trim(buf)
        case (PREFIX_MINUS)
            text = "-"
        case (PREFIX_RAW)
            ! (4), (16) ...: a list of one number.
            text = "("//trim(buf)//")"
        case default
            text = ""
        end select
    end function prefix_arg_text

    !> Like `message'.  Port of repeat.el:repeat-message (with
    !> `repeat_message_function' unset).
    recursive subroutine repeat_message(text)
        character(len=*), intent(in) :: text

        call message(text)
    end subroutine repeat_message

    !> Toggle Repeat mode.
    !> When Repeat mode is enabled, certain commands bound to multi-key
    !> sequences can be repeated by typing a single key, after typing the
    !> full key sequence once.
    !>
    !> The commands that can be repeated in this way are those whose
    !> `repeat_map' component specifies a keymap of single keys for
    !> repeating.
    !>
    !> Normally, invoking a command outside that keymap terminates the
    !> repeating sequence.  However, if the command's `repeat_continue'
    !> component is set, it may instead continue the current repeating
    !> sequence: if the component lists keymaps, then the command continues
    !> when the current repeat map is in the list; if `repeat_continue_all'
    !> is true, the command always continues the sequence.
    !>
    !> See `describe_repeat_maps' for a list of all repeatable commands.
    !> Port of repeat.el:repeat-mode.
    recursive subroutine repeat_mode(status)
        type(status_type), intent(out) :: status
        character(len=24) :: ncmd, nmap
        integer :: commands_count, maps_count

        call minor_modes%repeat_mode%command(status)
        if (.not. status%ok()) return
        if (.not. minor_modes%repeat_mode%enabled()) return
        ! (called-interactively-p 'any): this is the command.
        call count_repeat_maps(commands_count, maps_count)
        write (ncmd, "(i0)") commands_count
        write (nmap, "(i0)") maps_count
        call message(format_message("Repeat mode is enabled for "//trim(ncmd)//" commands and "//trim(nmap)// &
                                    " keymaps; see `describe_repeat_maps'"))
    end subroutine repeat_mode

    !> The body of Repeat mode.  Port of repeat.el:repeat-mode in part.
    recursive subroutine repeat_mode_body(enable, status)
        logical, intent(in) :: enable
        type(status_type), intent(out) :: status

        status = status_type()

        if (.not. enable) then
            call pre_command_hook%remove(repeat_pre_hook)
            call post_command_hook%remove(repeat_post_hook)
        else
            if (repeat_keep_prefix) call pre_command_hook%add(repeat_pre_hook)
            call post_command_hook%add(repeat_post_hook)
        end if
    end subroutine repeat_mode_body

    !> The number of commands with a repeat map, and of their distinct
    !> maps.  Not a port.
    subroutine count_repeat_maps(ncommands, nmaps)
        integer, intent(out) :: ncommands, nmaps
        type(handle_type), allocatable :: maps(:)
        type(command_type), pointer :: p
        integer :: i, j, n
        logical :: seen

        ncommands = 0
        allocate (maps(size(BUILTIN_COMMAND_NAMES)))
        n = 0

        do i = 1, size(BUILTIN_COMMAND_NAMES)
            p => builtin_command(trim(BUILTIN_COMMAND_NAMES(i)))
            if (.not. associated(p)) cycle
            if (.not. associated(p%run) .or. .not. p%is_command) cycle
            if (.not. is_keymap_live(p%repeat_map)) cycle
            ncommands = ncommands + 1
            seen = .false.

            do j = 1, n
                if (same(maps(j), p%repeat_map)) seen = .true.
            end do

            if (.not. seen) then
                n = n + 1
                maps(n) = p%repeat_map
            end if
        end do

        nmaps = n
    end subroutine count_repeat_maps

    ! Not a port.
    logical function same(a, b)
        type(handle_type), intent(in) :: a, b

        same = a%index == b%index .and. a%generation == b%generation
    end function same

    !> The command `this_command' names, or else `real_this_command'
    !> (repeat--command-property's two places to look).
    !> Port of repeat.el:repeat--command-property in part.
    function this_command_object() result(p)
        type(command_type), pointer :: p

        p => this_command
        if (.not. associated(p)) p => real_this_command
    end function this_command_object

    !> Return a transient map possibly as a name (the null handle for
    !> none).  Port of repeat.el:repeat-get-map-sym (and repeat-get-map).
    function repeat_get_map_sym() result(map)
        type(handle_type) :: map
        type(command_type), pointer :: p, q
        logical :: continues
        integer :: k

        map = null_handle()
        if (.not. minor_modes%repeat_mode%enabled()) return
        map = repeat_map
        p => this_command
        q => real_this_command

        if (.not. is_keymap_live(map)) then
            if (associated(p)) map = p%repeat_map
            if (.not. is_keymap_live(map) .and. associated(q)) map = q%repeat_map
        end if

        if (is_keymap_live(repeat_in_progress)) then
            continues = .false.
            p => this_command_object()

            if (associated(p)) then
                continues = p%repeat_continue_all

                if (.not. continues .and. allocated(p%repeat_continue)) then
                    do k = 1, size(p%repeat_continue)
                        if (same(p%repeat_continue(k), repeat_in_progress)) continues = .true.
                    end do
                end if
            end if

            if (continues) map = repeat_in_progress
        end if
    end function repeat_get_map_sym

    !> The basic type of event EV: the character without its modifiers
    !> (and lower case), or the name without its modifier prefixes.
    !> Port of subr.el:event-basic-type.
    function event_basic_type(ev) result(basic)
        type(value_type), intent(in) :: ev
        type(value_type) :: basic
        character(len=:), allocatable :: base
        integer(int64) :: c, bits

        if (vtag(ev) == V_INTEGER) then
            c = iand(xfixnum(ev), int(not(CHAR_MODIFIER_MASK), int64))

            if (c < 32) then
                c = c + 96
            else if (c >= iachar("A") .and. c <= iachar("Z")) then
                c = c + 32
            end if

            basic = value_type(c)
        else if (vtag(ev) == V_NAME) then
            call parse_modifiers(ev, bits, base)
            basic = value_type(name=base)
        else
            basic = ev
        end if
    end function event_basic_type

    !> Check if the last KEY is suitable for activating the repeating MAP.
    !> Port of repeat.el:repeat-check-key.
    logical function is_repeat_check_key(key, map) result(ok)
        type(value_type), intent(in) :: key
        type(handle_type), intent(in) :: map
        type(command_type), pointer :: p
        type(value_type) :: binding, keys(1)
        type(status_type) :: st
        logical :: check
        integer :: prop

        prop = 0
        p => this_command_object()
        if (associated(p)) prop = p%repeat_check_key

        if (prop == REPEAT_CHECK_KEY_NO) then
            check = .false.
        else if (prop == REPEAT_CHECK_KEY_YES) then
            check = .true.
        else
            check = repeat_check_key
        end if

        ok = .not. check
        if (ok) return
        keys(1) = key
        call lookup_key(make_keymap_ref(map), keys, binding, st)
        ok = st%ok() .and. allocated(binding%a) .and. vtag(binding) /= V_INTEGER
        if (ok) return
        keys(1) = event_basic_type(key)
        call lookup_key(make_keymap_ref(map), keys, binding, st)
        ok = st%ok() .and. allocated(binding%a) .and. vtag(binding) /= V_INTEGER
    end function is_repeat_check_key

    !> Decide whether MAP can be used for the next command.  Port of
    !> repeat.el:repeat-check-map.
    logical function repeat_check_map(map) result(ok)
        type(handle_type), intent(in) :: map
        type(command_type), pointer :: p

        ok = is_keymap_live(map)
        if (.not. ok) return
        ok = minibuf_level < prev_mb_depth .or. associated(current_minibuffer_command, prev_mb_command) .or. &
             (.not. associated(current_minibuffer_command) .and. .not. associated(prev_mb_command))
        if (.not. ok) return
        p => this_command_object()
        ok = .false.
        if (associated(p)) ok = p%repeat_continue_all
        if (.not. ok) ok = is_repeat_check_key(last_command_event, map)
    end function repeat_check_map

    !> Function run before commands to handle repeatable keys.  Port of
    !> repeat.el:repeat-pre-hook.
    recursive subroutine repeat_pre_hook(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: map

        status = status_type()
        if (.not. (minor_modes%repeat_mode%enabled() .and. repeat_keep_prefix)) return
        if (.not. is_keymap_live(repeat_in_progress)) return
        ! The command's `current_prefix_arg' (a hook function sees nil).
        if (prefix_arg%kind /= PREFIX_NONE .or. hook_current_prefix_arg%kind == PREFIX_NONE) return
        map = repeat_get_map_sym()

        if (repeat_check_map(map)) then
            repeat_map = map
            prefix_arg = hook_current_prefix_arg
        end if
    end subroutine repeat_pre_hook

    !> Function run after commands to set transient keymap for repeatable keys.
    !> Port of repeat.el:repeat-post-hook.
    recursive subroutine repeat_post_hook(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: map, active
        type(value_type), allocatable :: key(:)
        type(command_type), pointer :: p
        type(status_type) :: st
        logical :: was_in_progress
        real :: timeout

        status = status_type()
        was_in_progress = is_keymap_live(repeat_in_progress)
        map = repeat_get_map_sym()
        repeat_in_progress = null_handle()

        if (repeat_check_map(map)) then
            if (associated(repeat_echo_function)) call repeat_echo_function(map)
            active = map

            if (allocated(repeat_exit_key)) then
                if (is_keymap_live(exit_key_map)) call free_keymap(exit_key_map, st)
                call copy_keymap(map, exit_key_map, st)
                call key_parse(repeat_exit_key, key, st)
                call define_key(exit_key_map, key, value_type(name="ignore"), st)
                active = exit_key_map
            end if

            repeat_in_progress = map
            call repeat_clear_prev()
            call set_transient_map(active)
            transient_exit_pending = .true.
            transient_map = active
            p => this_command_object()
            timeout = repeat_exit_timeout

            if (associated(p)) then
                if (p%repeat_exit_timeout_no) then
                    timeout = 0.0
                else if (p%repeat_exit_timeout >= 0.0) then
                    timeout = p%repeat_exit_timeout
                end if
            end if

            if (timeout > 0.0) call run_with_idle_timer(repeat_exit_timer, real(timeout, real64), .false., &
                                                        repeat_exit, st)
        end if

        repeat_map = null_handle()
        prev_mb_depth = minibuf_level
        prev_mb_command => current_minibuffer_command
        if (was_in_progress .and. .not. is_keymap_live(repeat_in_progress)) call repeat_exit(st)
    end subroutine repeat_post_hook

    !> Exit the repeating sequence.
    !> This function can be used to force exit of repetition while it's
    !> active.  Port of repeat.el:repeat-exit.
    recursive subroutine repeat_exit(status)
        type(status_type), intent(out) :: status

        status = status_type()
        repeat_in_progress = null_handle()
        call repeat_clear_prev()
        if (associated(repeat_echo_function)) call repeat_echo_function(null_handle())
    end subroutine repeat_exit

    !> Internal function to clean up previously set exit function and timer.
    !> Port of repeat.el:repeat--clear-prev.
    subroutine repeat_clear_prev()
        if (is_timer(repeat_exit_timer)) call free_timer(repeat_exit_timer)
        repeat_exit_timer = null_handle()

        if (transient_exit_pending) then
            call exit_transient_map(transient_map)
            transient_exit_pending = .false.
        end if
    end subroutine repeat_clear_prev

    !> Return a string with the list of repeating keys in KEYMAP.
    !> Port of repeat.el:repeat-echo-message-string.
    function repeat_echo_message_string(keymap) result(text)
        type(handle_type), intent(in) :: keymap
        character(len=:), allocatable :: text, keys_text, part
        type(value_type), allocatable :: prefix(:)

        allocate (prefix(0))
        keys_text = ""
        call collect(keymap, prefix)
        text = format_message("Repeat with "//keys_text)
        if (allocated(repeat_exit_key)) text = text//format_message(", or exit with `"//repeat_exit_key//"'")

    contains

        !> cl--map-keymap-recursively, the keys in the order upstream
        !> pushes them (so the last visited first, as `keymap_events'
        !> gives them).
        recursive subroutine collect(map, pre)
            type(handle_type), intent(in) :: map
            type(value_type), intent(in) :: pre(:)
            type(value_type), allocatable :: events(:), key(:)
            type(value_type) :: binding
            type(command_type), pointer :: p
            type(rmc_choice_type) :: choice
            type(value_type) :: last
            integer :: k

            call keymap_events(map, events)

            do k = 1, size(events)
                binding = access_keymap(make_keymap_ref(map), events(k), .false., .false.)
                if (.not. allocated(binding%a)) cycle
                key = [pre, events(k)]

                if (is_keymap(binding)) then
                    call collect(keymap_of(binding), key)
                    cycle
                end if

                last = key(size(key))
                p => command_of(binding)
                part = ""

                if (associated(p)) then
                    if (allocated(p%repeat_hint)) then
                        if (size(key) == 1 .and. vtag(last) == V_INTEGER) then
                            choice%key = int(xfixnum(last))
                            choice%name = p%repeat_hint
                            part = rmc_add_key_description(choice)
                        else if (vtag(event_basic_type(last)) == V_INTEGER) then
                            choice%key = int(xfixnum(event_basic_type(last)))
                            choice%name = p%repeat_hint
                            part = key_description(key)//" ("//rmc_add_key_description(choice)//")"
                        else
                            part = key_description(key)//" ("//p%repeat_hint//")"
                        end if
                    end if
                end if

                if (len(part) == 0) part = key_description(key)

                if (len(keys_text) == 0) then
                    keys_text = part
                else
                    keys_text = keys_text//", "//part
                end if
            end do
        end subroutine collect
    end function repeat_echo_message_string

    !> The keymap of keymap value V.  Not a port.
    function keymap_of(v) result(h)
        use lisp_fm, only: xhandle, value_type
        type(value_type), intent(in) :: v
        type(handle_type) :: h

        h = xhandle(v)
    end function keymap_of

    !> Display in the echo area the repeating keys defined by KEYMAP.
    !> See `repeat_echo_function' to enable/disable.  Port of
    !> repeat.el:repeat-echo-message.
    recursive subroutine repeat_echo_message(keymap)
        type(handle_type), intent(in) :: keymap
        character(len=:), allocatable :: msg, current
        integer :: at

        current = ""
        if (allocated(last_message)) current = last_message

        if (is_keymap_live(keymap)) then
            msg = repeat_echo_message_string(keymap)

            if (len(current) > 0) then
                call message_nolog(current//" ["//msg//"]")
            else
                call message_nolog(msg)
            end if
        else
            if (len(current) == 0) return

            if (index(current, "Repeat with ") == 1) then
                call message_nolog("")
            else
                at = index(current, " [Repeat with ")
                if (at > 0) call message_nolog(current(1:at - 1))
            end if
        end if
    end subroutine repeat_echo_message

    !> Define Repeat mode and the `repeat' command.  Not a port.
    subroutine syms_of_repeat()
        if (.not. allocated(repeat_too_dangerous)) then
            allocate (character(len=16) :: repeat_too_dangerous(1))
            repeat_too_dangerous(1) = "kill_this_buffer"
        end if

        if (.not. associated(repeat_echo_function)) repeat_echo_function => repeat_echo_message
        call minor_modes%repeat_mode%define("repeat_mode", global=.true., hook=repeat_mode_hook, body=repeat_mode_body)
        commands%repeat%run => repeat
        commands%repeat_mode%run => repeat_mode
        commands%repeat_exit%run => repeat_exit
    end subroutine syms_of_repeat
end module repeat_fm
