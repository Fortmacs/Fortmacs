! compile_fm.f90 -- run compiler as inferior of Emacs.
! Ported from GNU Emacs 31.1 lisp/progmodes/compile.el (the core), with
! lisp/comint.el comint-carriage-motion, lisp/files.el cd and
! lisp/simple.el next-error-find-buffer.
!
! Copyright (C) 1985-1987, 1993-1999, 2001-2026 Free Software
! Foundation, Inc.
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

!> Compilation: `compile' runs a shell command asynchronously with its
!> output in *compilation*, in Compilation mode, with the
!> "Compilation started at ..." header and the "Compilation finished at
!> ..., duration ..." line when it ends; `recompile' (g) runs it again and
!> `kill_compilation' (C-c C-k) interrupts it.
!>
!> Not ported yet (Deferred): parsing errors (`compilation_error_regexp_alist'
!> and its font-lock keywords, so nothing is highlighted, `next_error',
!> `compile_goto_error' and the other error motion commands and their
!> keys, `compilation_auto_jump_to_first_error' and the `first-error' value
!> of `compilation_scroll_output' beyond scrolling), Comint mode
!> compilations (C-u M-x compile, `compilation_shell_minor_mode'),
!> `compilation_minor_mode', `define_compilation_mode' and grep, the mode
!> line (`mode_line_process' ":run" and ":exit [N]", "Compiling" in
!> `mode_line_modes', `compilation_mode_line_errors'),
!> `compilation_max_output_line_length' (long lines are not abbreviated:
!> needs buttons and display properties), `compilation_hidden_output',
!> `compilation_window_height', `compilation_environment',
!> `compilation_search_path', `compilation_buffer_name_function',
!> `compilation_process_setup_function', `compilation_start_hook',
!> `compilation_finish_functions', `compilation_exit_message_function',
!> `compilation_save_buffers_predicate' (all modified file buffers are
!> offered), the CONTINUE argument of `compilation_start',
!> `revert_buffer_function' (g is `recompile' itself), wildcards in a
!> leading "cd" of the command (`file-expand-wildcards'), the menu and
!> tool bar.
module compile_fm
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use commands_fm, only: commands
    use error_fm, only: status_type, set_error, format_message, FM_GENERIC_ERROR
    use handle_fm, only: handle_type, null_handle
    use lisp_fm, only: value_type, nilp, consp, t_value, make_string, list, nil
    use lread_fm, only: sym
    use data_fm, only: string_setting, logical_setting, integer_setting, symbol_value, set, &
                       add_local_variables_killer
    use buffer_fm, only: buffers, cur, current_buffer, set_buffer, get_buffer_create, buffer_live_p, &
                         buffer_list, erase_buffer, restore_buffer_modified_p
    use marker_fm, only: copy_marker_at, free_marker, marker_position, set_marker
    use insdel_fm, only: insert
    use editfns_fm, only: point, point_min, point_max, goto_char, widen, narrow_to_region, delete_region, &
                          line_beginning_position, following_char, eobp, buffer_size
    use textprop_fm, only: put_text_property, get_text_property, remove_text_properties
    use cmds_fm, only: delete_char, forward_char
    use syntax_fm, only: skip_chars_forward
    use search_fm, only: string_match
    use subr_fm, only: match_string
    use env_fm, only: substitute_env_vars, getenv
    use fileio_fm, only: expand_file_name, file_name_as_directory, file_name_absolute_p, file_directory_p, &
                         file_exists_p, file_accessible_directory_p
    use files_fm, only: abbreviate_file_name, save_some_buffers
    use print_fm, only: prin1_to_string
    use fns_fm, only: yes_or_no_p
    use eval_fm, only: hook_type
    use keyboard_fm, only: sit_for
    use callint_fm, only: current_prefix_arg
    use xdisp_fm, only: message
    use window_fm, only: windows, display_buffer, set_window_start, set_window_point
    use frame_fm, only: selected_frame
    use keymap_fm, only: set_keymap_parent, keymap_set
    use modes_fm, only: major_modes
    use simple_fm, only: special_mode_map, read_shell_command, shell_command_switch
    use callproc_fm, only: shell_file_name, init_callproc
    use process_fm, only: process_arg_type, make_process, get_buffer_process, process_status, &
                          process_query_on_exit_flag, interrupt_process, delete_process, process_buffer, &
                          process_exit_status, set_process_buffer, set_process_query_on_exit_flag, process_mark, &
                          process_send_eof, num_processors
    use sysdep_fm, only: emacs_time, emacs_localtime_wday
    use version_fm, only: upstream_version
    implicit none(type, external)
    private

    public :: compile, recompile, compilation_start, compilation_mode, kill_compilation, compilation_filter, &
              compilation_sentinel, compilation_handle_exit, comint_carriage_motion, syms_of_compile

    !> Last shell command used to do a compilation; default for next
    !> compilation.
    !>
    !> Sometimes it is useful for files to supply local values for this
    !> variable.  You might also use mode hooks to specify it in certain
    !> modes.
    !>
    !> It's often useful to leave a space at the end of the value.  Port of
    !> compile.el compile-command ("make -k -jN " with N the number of
    !> processors divided by 1.5, rounded up).
    type(string_setting), public :: compile_command = string_setting(name="compile_command")

    !> True means \[compile] reads the compilation command to use.
    !> Otherwise, \[compile] just uses the value of `compile_command'.
    !>
    !> Note that changing this to false may be a security risk, because a
    !> file might define a malicious `compile_command' as a file local
    !> variable, and you might not notice.  Therefore, `compile_command'
    !> is considered unsafe if this variable is false.  Port of compile.el
    !> compilation-read-command.
    type(logical_setting), public :: compilation_read_command = &
                                     logical_setting(name="compilation_read_command", value=.true.)

    !> True means \[compile] asks which buffers to save before compiling.
    !> Otherwise, it saves all modified buffers without asking.  Port of
    !> compile.el compilation-ask-about-save.
    type(logical_setting), public :: compilation_ask_about_save = &
                                     logical_setting(name="compilation_ask_about_save", value=.true.)

    !> If true, always kill a running compilation process before starting
    !> a new one.  If false, ask to kill it.  Port of compile.el
    !> compilation-always-kill.
    type(logical_setting), public :: compilation_always_kill = &
                                     logical_setting(name="compilation_always_kill", value=.false.)

    !> If true, send end-of-file as compilation process input.
    !> This only affects platforms that support asynchronous processes (see
    !> `start_process'); synchronous compilation processes never accept
    !> input.  Port of compile.el compilation-disable-input.
    type(logical_setting), public :: compilation_disable_input = &
                                     logical_setting(name="compilation_disable_input", value=.false.)

    !> Values of `compilation_scroll_output'.
    integer, parameter, public :: COMPILATION_SCROLL_NIL = 0, COMPILATION_SCROLL_T = 1, &
                                  COMPILATION_SCROLL_FIRST_ERROR = 2

    !> True to scroll the *compilation* buffer window as output appears.
    !>
    !> Setting it causes the Compilation mode commands to put point at the
    !> end of their output window so that the end of the output is always
    !> visible rather than the beginning.
    !>
    !> COMPILATION_SCROLL_FIRST_ERROR stops scrolling at the first error,
    !> and leaves point on its location in the *compilation* buffer.  Port of
    !> compile.el compilation-scroll-output (COMPILATION_SCROLL_NIL is nil,
    !> COMPILATION_SCROLL_T t, COMPILATION_SCROLL_FIRST_ERROR `first-error').
    type(integer_setting), public :: compilation_scroll_output = &
                                     integer_setting(name="compilation_scroll_output", value=COMPILATION_SCROLL_NIL)

    !> Hook run after `compilation_filter' has inserted a string into the
    !> buffer.  It is called with the variable `compilation_filter_start'
    !> bound to the position of the start of the inserted text, and point at
    !> its end.
    !>
    !> If Emacs lacks asynchronous process support, this hook is run
    !> after `call_process' inserts the grep output into the buffer.  Port
    !> of compile.el compilation-filter-hook.
    type(hook_type), target, public :: compilation_filter_hook = hook_type(name="compilation_filter_hook")

    !> Position of the start of the text inserted by `compilation_filter'.
    !> This is bound before running `compilation_filter_hook'.  Port of
    !> compile.el compilation-filter-start.
    integer(int64), public :: compilation_filter_start = 0

    !> Hook run after entering Compilation mode.  Port of compile.el
    !> compilation-mode-hook.
    type(hook_type), target, public :: compilation_mode_hook = hook_type(name="compilation_mode_hook")

    !> Keymap for compilation log buffers.  Port of compile.el
    !> compilation-mode-map (the keys of ported commands).
    type(handle_type), public :: compilation_mode_map

    !> Directory to restore to when doing `recompile'.  Port of compile.el
    !> compilation-directory (this is its default value; a compilation
    !> buffer has a value of its own, the directory it was started in).
    type(string_setting), public :: compilation_directory = string_setting(name="compilation_directory")

    !> If false, Comint will interpret `carriage control' characters in
    !> output.  See `comint_carriage_motion' for details.  Port of
    !> comint.el comint-inhibit-carriage-motion.
    type(logical_setting), public :: comint_inhibit_carriage_motion = &
                                     logical_setting(name="comint_inhibit_carriage_motion", value=.false.)

    !> What a compilation buffer keeps (its buffer-local variables
    !> compilation-locs, compilation-arguments and compilation--start-time):
    !> forgotten when its local variables are killed.
    type :: compilation_buffer_type
        type(handle_type) :: buffer
        logical :: has_arguments = .false.
        character(len=:), allocatable :: command
        integer(int64) :: start_count = 0
        !> compilation-directory, if the buffer has a value of its own.
        character(len=:), allocatable :: directory
        !> mode-line-process (not displayed: the mode line is Deferred).
        character(len=:), allocatable :: mode_line_process
    end type compilation_buffer_type

    type(compilation_buffer_type), allocatable :: compilation_buffers(:)

    !> simple.el next-error-last-buffer.
    type(handle_type) :: next_error_last_buffer

    !> compile.el compilation-in-progress: the running compilations.
    type(handle_type), allocatable :: compilation_in_progress(:)

    character(len=3), parameter :: DAY_NAMES(0:6) = ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"]
    character(len=3), parameter :: MONTH_NAMES(12) = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", &
                                                      "Sep", "Oct", "Nov", "Dec"]

contains

    ! ------------------------------------------------------------------
    ! Compilation buffers
    ! ------------------------------------------------------------------

    logical function same(a, b)
        type(handle_type), intent(in) :: a, b
        same = a%index == b%index .and. a%generation == b%generation
    end function same

    !> The entry of buffer `h' in `compilation_buffers' (0 for none).
    integer function entry_of(h) result(k)
        type(handle_type), intent(in) :: h
        if (allocated(compilation_buffers)) then
            do k = 1, size(compilation_buffers)
                if (same(compilation_buffers(k)%buffer, h)) return
            end do
        end if
        k = 0
    end function entry_of

    !> The entry of the current buffer, made if needed.
    integer function current_entry() result(k)
        type(compilation_buffer_type), allocatable :: grown(:)
        integer :: n
        k = entry_of(current_buffer())
        if (k /= 0) return
        if (.not. allocated(compilation_buffers)) allocate (compilation_buffers(0))
        n = size(compilation_buffers)
        allocate (grown(n + 1))
        grown(1:n) = compilation_buffers
        grown(n + 1)%buffer = current_buffer()
        call move_alloc(grown, compilation_buffers)
        k = n + 1
    end function current_entry

    !> Forget what the current buffer had as a compilation buffer, when
    !> `kill_all_local_variables' runs (and buffers that are dead).
    subroutine kill_compilation_locals()
        type(handle_type) :: h
        logical, allocatable :: keep(:)
        integer :: k
        if (.not. allocated(compilation_buffers)) return
        h = current_buffer()
        allocate (keep(size(compilation_buffers)))
        do k = 1, size(compilation_buffers)
            keep(k) = .not. same(compilation_buffers(k)%buffer, h) .and. buffer_live_p(compilation_buffers(k)%buffer)
        end do
        compilation_buffers = pack(compilation_buffers, keep)
    end subroutine kill_compilation_locals

    !> Test if inside a compilation buffer.  Port of
    !> compile.el:compilation-buffer-internal-p.
    logical function compilation_buffer_internal_p()
        compilation_buffer_internal_p = entry_of(current_buffer()) /= 0
    end function compilation_buffer_internal_p

    !> Whether BUFFER is a live buffer where `next_error' could find error
    !> messages (a compilation buffer), and not the current buffer when
    !> AVOID_CURRENT.  Port of simple.el:next-error-buffer-p (with
    !> compilation buffers standing for those with a `next-error-function').
    logical function next_error_buffer_p(buffer, avoid_current)
        type(handle_type), intent(in) :: buffer
        logical, intent(in) :: avoid_current
        next_error_buffer_p = .false.
        if (.not. buffer_live_p(buffer)) return
        if (avoid_current .and. same(buffer, current_buffer())) return
        next_error_buffer_p = entry_of(buffer) /= 0
    end function next_error_buffer_p

    !> Return a compilation buffer.
    !> If AVOID_CURRENT is false, and the current buffer is a compilation
    !> buffer, return it.  If AVOID_CURRENT is true, return the current
    !> buffer only as a last resort.  Port of
    !> compile.el:compilation-find-buffer, with
    !> simple.el:next-error-find-buffer.
    subroutine compilation_find_buffer(buffer, status, avoid_current)
        type(handle_type), intent(out) :: buffer
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: avoid_current
        type(handle_type), allocatable :: all(:)
        logical :: avoid
        integer :: k
        avoid = .false.
        if (present(avoid_current)) avoid = avoid_current
        buffer = current_buffer()
        if (compilation_buffer_internal_p() .and. .not. avoid) return
        ! 2. If next-error-last-buffer is an acceptable buffer, use that.
        if (next_error_buffer_p(next_error_last_buffer, avoid)) then
            buffer = next_error_last_buffer
            return
        end if
        ! 3. If the current buffer is acceptable, choose it.
        if (next_error_buffer_p(current_buffer(), avoid)) return
        ! 4. Look for any acceptable buffer.
        all = buffer_list()
        do k = 1, size(all)
            if (next_error_buffer_p(all(k), avoid)) then
                buffer = all(k)
                return
            end if
        end do
        ! 5. Use the current buffer as a last resort if it qualifies, even
        ! despite AVOID-CURRENT.
        if (avoid .and. next_error_buffer_p(current_buffer(), .false.)) then
            call message("This is the only buffer with error message locations")
            return
        end if
        ! 6. Give up.
        call set_error(status, FM_GENERIC_ERROR, "No buffers contain error message locations")
    end subroutine compilation_find_buffer

    ! ------------------------------------------------------------------
    ! Compilation mode
    ! ------------------------------------------------------------------

    !> Major mode for compilation log buffers.
    !> \<compilation_mode_map>To kill the compilation, type
    !> \[kill_compilation].
    !>
    !> Runs `compilation_mode_hook' with `run_mode_hooks' (which see).
    !> Port of compile.el:compilation-mode (NAME-OF-MODE "Compilation").
    recursive subroutine compilation_mode(status)
        type(status_type), intent(out) :: status
        logical :: saved
        integer :: k
        if (.not. associated(major_modes%compilation_mode%run)) call syms_of_compile()
        ! kill-all-local-variables, use-local-map, major-mode, mode-name.
        call major_modes%compilation_mode%begin(saved, status)
        if (.not. status%ok()) return
        call compilation_setup()
        k = current_entry()
        buffers(cur)%read_only = .true.
        call major_modes%compilation_mode%finish(saved, status)
    end subroutine compilation_mode

    !> Prepare the buffer for the compilation parsing commands to work.
    !> Port of compile.el:compilation-setup (the buffer becomes a
    !> compilation buffer; error parsing and its font-lock keywords are
    !> Deferred: nothing is fontified).
    subroutine compilation_setup()
        use font_core_fm, only: font_lock_defaults
        call font_lock_defaults%set_local(keywords_only=.true.)
    end subroutine compilation_setup

    ! ------------------------------------------------------------------
    ! Starting
    ! ------------------------------------------------------------------

    !> The default value of `compile_command': "make -k -jN ", N being
    !> the number of processors divided by 1.5, rounded up.
    function default_compile_command() result(command)
        character(len=:), allocatable :: command
        character(len=16) :: n
        integer :: p
        p = num_processors()
        write (n, "(i0)") (2*p + 2)/3
        command = "make -k -j"//trim(n)//" "
    end function default_compile_command

    !> Read the compilation command, COMMAND as initial input.  Port of
    !> compile.el:compilation-read-command.
    subroutine compilation_read_command_from_user(command, result, status)
        use lisp_fm, only: stringp, xstring
        use data_fm, only: car
        character(len=*), intent(in) :: command
        character(len=:), allocatable, intent(out) :: result
        type(status_type), intent(out) :: status
        type(value_type) :: history, first
        logical :: same_first
        same_first = .false.
        call symbol_value(sym("compile_history"), history, status)
        if (status%ok()) call car(history, first, status)
        if (status%ok()) then
            if (stringp(first)) same_first = xstring(first) == command .and. len(xstring(first)) == len(command)
        end if
        if (same_first) then
            call read_shell_command("Compile command: ", result, status, initial=command, history="compile_history", &
                                    history_position=1)
        else
            call read_shell_command("Compile command: ", result, status, initial=command, history="compile_history")
        end if
    end subroutine compilation_read_command_from_user

    !> Compile the program including the current buffer.  Default: run
    !> `make'.
    !> Runs COMMAND, a shell command, in a separate process asynchronously
    !> with output going to the buffer `*compilation*'.
    !>
    !> Interactively, prompts for the command if the variable
    !> `compilation_read_command' is true; otherwise uses
    !> `compile_command'.  With prefix arg, always prompts.
    !>
    !> To run more than one compilation at once, start one then rename
    !> the `*compilation*' buffer to some other name with
    !> \[rename_buffer].  Then _switch buffers_ and start the new
    !> compilation.  It will create a new `*compilation*' buffer.
    !>
    !> On most systems, termination of the main compilation process kills
    !> its subprocesses.  Port of compile.el:compile (COMINT is Deferred).
    subroutine compile(command, status)
        character(len=*), intent(in) :: command
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: dir
        if (command /= compile_command_value() .or. len(command) /= len(compile_command_value())) then
            compile_command = command
        end if
        call save_some_buffers(.not. compilation_ask_about_save%value, .false., status)
        if (.not. status%ok()) return
        dir = ""
        if (allocated(buffers(cur)%directory)) dir = buffers(cur)%directory
        ! setq-default: the value of buffers without their own.
        compilation_directory = dir
        call compilation_start(command, status)
    end subroutine compile

    !> `compile_command', set to its default value first if needed.
    function compile_command_value() result(command)
        character(len=:), allocatable :: command
        if (.not. allocated(compile_command%value)) compile_command = default_compile_command()
        command = compile_command%value
    end function compile_command_value

    !> The command: interactively, read the command (if
    !> `compilation_read_command' or with a prefix argument), then compile.
    !> The interactive form of compile.el:compile.
    !>
    !> Compile the program including the current buffer.  Default: run
    !> `make'.
    subroutine cmd_compile(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: command
        command = compile_command_value()
        if (compilation_read_command%value .or. .not. nilp(current_prefix_arg)) then
            call compilation_read_command_from_user(command, command, status)
            if (.not. status%ok()) return
        end if
        if (consp(current_prefix_arg)) then
            call set_error(status, FM_GENERIC_ERROR, "Compilations in Comint mode are not supported yet")
            return
        end if
        call compile(command, status)
    end subroutine cmd_compile

    !> Re-compile the program including the current buffer.
    !> If this is run in a Compilation mode buffer, reuse the arguments from
    !> the original use.  Otherwise, recompile using `compile_command'.
    !> If the optional argument `edit_command' is true, the command can be
    !> edited.  Port of compile.el:recompile.
    subroutine recompile(status, edit_command)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: edit_command
        character(len=:), allocatable :: command, saved_dir, dir
        logical :: edit, had_dir
        integer :: k
        edit = .false.
        if (present(edit_command)) edit = edit_command
        call save_some_buffers(.not. compilation_ask_about_save%value, .false., status)
        if (.not. status%ok()) return
        ! (let ((default-directory (or compilation-directory default-directory)))
        had_dir = allocated(buffers(cur)%directory)
        if (had_dir) saved_dir = buffers(cur)%directory
        dir = compilation_directory_value()
        if (len(dir) > 0) buffers(cur)%directory = dir
        command = compile_command_value()
        k = entry_of(current_buffer())
        if (k /= 0) then
            if (.not. compilation_buffers(k)%has_arguments) k = 0
        end if
        if (edit) then
            if (k /= 0) then
                call compilation_read_command_from_user(compilation_buffers(k)%command, command, status)
            else
                call compilation_read_command_from_user(command, command, status)
            end if
            if (.not. status%ok()) then
                call restore_dir()
                return
            end if
            if (k /= 0) compilation_buffers(k)%command = command
        end if
        if (k /= 0) command = compilation_buffers(k)%command
        call compilation_start(command, status)
        call restore_dir()
    contains

        subroutine restore_dir()
            ! The let binding ends in the buffer that was current.
            if (had_dir) then
                buffers(cur)%directory = saved_dir
            else if (allocated(buffers(cur)%directory)) then
                deallocate (buffers(cur)%directory)
            end if
        end subroutine restore_dir
    end subroutine recompile

    !> The current buffer's `compilation_directory' ("" for nil).
    function compilation_directory_value() result(dir)
        character(len=:), allocatable :: dir
        integer :: k
        dir = ""
        k = entry_of(current_buffer())
        if (k /= 0) then
            if (allocated(compilation_buffers(k)%directory)) then
                dir = compilation_buffers(k)%directory
                return
            end if
        end if
        if (allocated(compilation_directory%value)) dir = compilation_directory%value
    end function compilation_directory_value

    !> The command: recompile, editing the command with a prefix argument.
    !>
    !> Re-compile the program including the current buffer.
    subroutine cmd_recompile(status)
        type(status_type), intent(out) :: status
        call recompile(status,.not. nilp(current_prefix_arg))
    end subroutine cmd_recompile

    !> Insert TEXT at point, adding the `compilation_annotation' text
    !> property.  This property is used to distinguish output of the
    !> compilation process from additional information inserted by Emacs.
    !> Port of compile.el:compilation-insert-annotation.
    subroutine compilation_insert_annotation(text)
        character(len=*), intent(in) :: text
        type(status_type) :: st
        integer(int64) :: start
        start = point()
        call insert(text, st)
        call put_text_property(start, point(), sym("compilation_annotation"), t_value(), st)
    end subroutine compilation_insert_annotation

    !> The first 19 characters of `current_time_string': "Sun Sep 16
    !> 01:03:52".  Cf. editfns.c:Fcurrent_time_string.
    function current_time_19() result(s)
        character(len=:), allocatable :: s
        integer :: t(7)
        character(len=19) :: buf
        t = emacs_localtime_wday(emacs_time())
        write (buf, "(a3, 1x, a3, 1x, i2, 1x, i2.2, ':', i2.2, ':', i2.2)") DAY_NAMES(t(7)), MONTH_NAMES(t(2)), t(3), &
            t(4), t(5), t(6)
        s = buf
    end function current_time_19

    !> Run compilation command COMMAND (low level interface).
    !> If COMMAND starts with a cd command, that becomes the
    !> `default_directory'.
    !>
    !> The buffer is the current buffer if it is in Compilation mode, else
    !> *compilation*.  Port of compile.el:compilation-start (MODE
    !> `compilation-mode'; NAME-FUNCTION, HIGHLIGHT-REGEXP and CONTINUE are
    !> Deferred).
    subroutine compilation_start(command, status)
        character(len=*), intent(in) :: command
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: thisdir, cd_dir, origdir
        type(handle_type) :: outbuf, comp_proc, outwin, old, proc
        type(process_arg_type) :: args(3), env(2)
        type(value_type) :: inhibit
        type(status_type) :: st
        logical :: answer, scroll
        integer(int64) :: desired
        integer :: k
        old = current_buffer()
        thisdir = ""
        if (allocated(buffers(cur)%directory)) thisdir = buffers(cur)%directory
        ! compilation--default-buffer-name.
        if (buffers(cur)%major_mode == "compilation_mode") then
            outbuf = current_buffer()
        else
            call get_buffer_create("*compilation*", outbuf, status)
            if (.not. status%ok()) return
        end if
        call set_buffer(outbuf, status)
        if (.not. status%ok()) return
        comp_proc = get_buffer_process(outbuf)
        if (comp_proc%index /= 0) then
            answer = process_status(comp_proc) /= "run" .or. .not. process_query_on_exit_flag(comp_proc)
            if (.not. answer) then
                call yes_or_no_p("A compilation process is running; kill it? ", answer, status)
                if (.not. status%ok()) return
            end if
            if (answer) then
                call interrupt_process(st, comp_proc)
                if (st%ok()) then
                    call sit_for(1000_int32)
                    call delete_process(comp_proc, st)
                end if
            else
                call set_error(status, FM_GENERIC_ERROR, format_message("Cannot have two processes in `")// &
                               buffers(cur)%name//format_message("' at once"))
                call set_buffer(old, st)
                return
            end if
        end if
        ! First transfer directory from where M-x compile was called.
        buffers(cur)%directory = thisdir
        ! Make compilation buffer read-only.  The filter can still write it.
        ! Clear out the compilation buffer.
        call symbol_value(sym("inhibit_read_only"), inhibit, st)
        call set(sym("inhibit_read_only"), t_value(), st)
        ! Then evaluate a cd command if any, but don't perform it yet, else
        ! start-command would do it again through the shell: (cd "..") AND
        ! sh -c "cd ..; make"
        call command_cd_directory(command, thisdir, cd_dir)
        call cd(cd_dir, status)
        if (.not. status%ok()) then
            call set(sym("inhibit_read_only"), inhibit, st)
            buffers(cur)%directory = thisdir
            return
        end if
        call erase_buffer(st)
        ! Select the desired mode.
        buffers(cur)%undo_enabled = .false.
        if (allocated(buffers(cur)%undo_list)) deallocate (buffers(cur)%undo_list)
        origdir = buffers(cur)%directory
        call compilation_mode(status)
        if (.not. status%ok()) then
            call set(sym("inhibit_read_only"), inhibit, st)
            return
        end if
        ! Remember the original dir, so we can use it when we recompile.
        k = current_entry()
        compilation_buffers(k)%directory = thisdir
        if (buffer_size() == 0) then
            ! Output a mode setter, for saving and later reloading this
            ! buffer.
            call compilation_insert_annotation("-*- mode: compilation; default-directory: "// &
                                               prin1_to_string(make_string(abbreviate_file_name(origdir)))// &
                                               " -*-"//new_line("a"))
        end if
        call compilation_insert_annotation(buffers(cur)%mode_name//" started at "//current_time_19()// &
                                           new_line("a")//new_line("a")//command//new_line("a"))
        k = current_entry()
        call system_clock(compilation_buffers(k)%start_count)
        call set(sym("inhibit_read_only"), inhibit, st)
        ! The let binding of default-directory ends here.
        buffers(cur)%directory = thisdir
        call restore_buffer_modified_p(.false.)
        ! Pop up the compilation buffer.
        call display_buffer(outbuf, outwin, st)
        scroll = compilation_scroll_output%value /= COMPILATION_SCROLL_NIL
        k = current_entry()
        compilation_buffers(k)%has_arguments = .true.
        compilation_buffers(k)%command = command
        if (outwin%index /= 0 .and. .not. scroll) call set_window_start(windows(outwin%index), point_min(), st)
        ! Position point as the user will see it.
        if (scroll) then
            desired = point_max()
        else
            desired = point_min()
        end if
        call goto_char(desired)
        if (outwin%index /= 0 .and. .not. same(outwin, selected_frame%selected_window)) then
            call set_window_point(windows(outwin%index), desired, st)
        end if
        ! Start the compilation.
        if (.not. allocated(shell_file_name%value)) call init_callproc()
        args(1)%s = shell_file_name%value
        args(2)%s = shell_command_switch%value
        args(3)%s = command
        env(1)%s = "INSIDE_EMACS="//upstream_version//",compile"
        ! Some external programs (like "git grep") use a pager; defeat that.
        env(2)%s = "PAGER="
        call make_process("compilation", args, proc, status, buffer=outbuf, environment=env)
        if (.not. status%ok()) then
            call set_buffer(old, st)
            return
        end if
        ! Set the process as killable without query by default.
        if (compilation_always_kill%value) call set_process_query_on_exit_flag(proc, .false., st)
        call set_process_sentinel_and_filter(proc)
        ! Use (point-max) here so that output comes in after the initial
        ! text, regardless of where the user sees point.
        call set_marker(process_mark(proc), point_max(), st, outbuf)
        if (compilation_disable_input%value) call process_send_eof(st, proc)
        call push_in_progress(proc)
        ! Now finally cd to where the shell started make/grep/...
        buffers(cur)%directory = origdir
        if (scroll) call goto_char(point_max())
        ! Make it so the next C-x ` will use this buffer.
        next_error_last_buffer = outbuf
        call set_buffer(old, st)
    end subroutine compilation_start

    subroutine set_process_sentinel_and_filter(proc)
        use process_fm, only: set_process_sentinel, set_process_filter
        type(handle_type), intent(in) :: proc
        type(status_type) :: st
        call set_process_sentinel(proc, st, compilation_sentinel)
        call set_process_filter(proc, st, compilation_filter)
    end subroutine set_process_sentinel_and_filter

    subroutine push_in_progress(proc)
        type(handle_type), intent(in) :: proc
        type(handle_type), allocatable :: grown(:)
        integer :: n
        if (.not. allocated(compilation_in_progress)) allocate (compilation_in_progress(0))
        n = size(compilation_in_progress)
        allocate (grown(n + 1))
        grown(1) = proc
        grown(2:) = compilation_in_progress
        call move_alloc(grown, compilation_in_progress)
    end subroutine push_in_progress

    !> The directory a leading "cd DIR;" of COMMAND changes to (DEFAULT if
    !> there is none): the cond of compile.el:compilation-start.
    subroutine command_cd_directory(command, default, dir)
        character(len=*), intent(in) :: command, default
        character(len=:), allocatable, intent(out) :: dir
        character(len=:), allocatable :: arg, out
        type(status_type) :: st
        logical :: matched
        integer :: k
        integer(int64) :: idx
        idx = string_match("\`\s *cd\(?:\s +\(\S +?\|'[^']*'\|""\(?:[^""`$\\]\|\\.\)*""\)\)?\s *[;&"//new_line("a")// &
                           "]", command, st)
        if (idx == 0) then
            dir = default
            return
        end if
        arg = match_string(1, command, matched)
        if (.not. matched) then
            dir = "~"
        else if (arg(1:1) == "'") then
            dir = arg(2:len(arg) - 1)
        else if (arg(1:1) == """") then
            ! (replace-regexp-in-string "\\\\\\(.\\)" "\\1" ...)
            out = ""
            k = 2
            do while (k <= len(arg) - 1)
                if (arg(k:k) == "\" .and. k < len(arg) - 1) then
                    k = k + 1
                end if
                out = out//arg(k:k)
                k = k + 1
            end do
            dir = out
        else
            ! Globbing (file-expand-wildcards) is Deferred.
            dir = substitute_env_vars(arg, .false.)
        end if
    end subroutine command_cd_directory

    !> Make DIR become the current buffer's default directory.
    !> If your environment includes a `CDPATH' variable, try each one of
    !> that list of directories (separated by occurrences of
    !> `path_separator') when resolving a relative directory name.  Port of
    !> files.el:cd and cd-absolute.
    subroutine cd(dir, status)
        character(len=*), intent(in) :: dir
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: cdpath, entry, candidate, absolute, base
        logical :: found
        integer :: b, e
        base = ""
        if (allocated(buffers(cur)%directory)) base = buffers(cur)%directory
        if (file_name_absolute_p(dir)) then
            call expand_file_name(dir, absolute, status, base)
            if (.not. status%ok()) return
        else
            ! cd-path: CDPATH's directories, or "./".
            call getenv("CDPATH", cdpath, found)
            if (.not. found) cdpath = "./"
            absolute = ""
            b = 1
            do while (b <= len(cdpath) + 1)
                e = index(cdpath(b:), ":")
                if (e == 0) then
                    e = len(cdpath) + 1
                else
                    e = b + e - 1
                end if
                entry = cdpath(b:e - 1)
                if (len(entry) == 0) entry = "./"
                call expand_file_name(entry, candidate, status, base)
                call expand_file_name(dir, candidate, status, file_name_as_directory(candidate))
                if (status%ok() .and. file_directory_p(candidate)) then
                    absolute = candidate
                    exit
                end if
                b = e + 1
            end do
            if (len(absolute) == 0) then
                call set_error(status, FM_GENERIC_ERROR, "No such directory found via CDPATH environment variable")
                return
            end if
        end if
        ! cd-absolute.
        absolute = abbreviate_file_name(file_name_as_directory(absolute))
        if (.not. file_directory_p(absolute)) then
            if (file_exists_p(absolute)) then
                call set_error(status, FM_GENERIC_ERROR, absolute//" is not a directory")
            else
                call set_error(status, FM_GENERIC_ERROR, absolute//": no such directory")
            end if
            return
        end if
        if (.not. file_accessible_directory_p(absolute)) then
            call set_error(status, FM_GENERIC_ERROR, "Cannot cd to "//absolute//":  Permission denied")
            return
        end if
        buffers(cur)%directory = absolute
    end subroutine cd

    ! ------------------------------------------------------------------
    ! Output and exit
    ! ------------------------------------------------------------------

    !> Process filter for compilation buffers.
    !> Just inserts the text, handles carriage motion (see
    !> `comint_inhibit_carriage_motion') and runs
    !> `compilation_filter_hook'.  Port of compile.el:compilation-filter
    !> (`compilation_max_output_line_length' and
    !> `compilation_hidden_output' are Deferred).
    recursive subroutine compilation_filter(proc, string, status)
        type(handle_type), intent(in) :: proc
        character(len=*), intent(in) :: string
        type(status_type), intent(out) :: status
        type(handle_type) :: buf, pos, min, max, mark
        type(value_type) :: inhibit
        type(status_type) :: st
        integer(int64) :: p, min_pos, max_pos
        status = status_type()
        buf = process_buffer(proc)
        if (.not. buffer_live_p(buf)) return
        call set_buffer(buf, status)
        if (.not. status%ok()) return
        call symbol_value(sym("inhibit_read_only"), inhibit, st)
        call set(sym("inhibit_read_only"), t_value(), st)
        ! `save-excursion' doesn't use the right insertion-type for us.
        call copy_marker_at(point(), pos, .true., st)
        call copy_marker_at(point_min(), min, .false., st)
        ! If we are inserting at the end of the accessible part of the
        ! buffer, keep the inserted text visible.
        call copy_marker_at(point_max(), max, .true., st)
        mark = process_mark(proc)
        call marker_position(mark, compilation_filter_start, st)
        call widen()
        call goto_char(compilation_filter_start)
        call insert(string, status)
        if (status%ok()) then
            if (.not. comint_inhibit_carriage_motion%value) then
                call marker_position(mark, p, st)
                call comint_carriage_motion(p, point())
            end if
            call set_marker(mark, point(), st)
            call compilation_filter_hook%run(status)
        end if
        call marker_position(pos, p, st)
        call goto_char(p)
        call marker_position(min, min_pos, st)
        call marker_position(max, max_pos, st)
        call narrow_to_region(min_pos, max_pos, st)
        call free_marker(pos, st)
        call free_marker(min, st)
        call free_marker(max, st)
        call set(sym("inhibit_read_only"), inhibit, st)
    end subroutine compilation_filter

    !> Interpret carriage control characters in the region from START to
    !> END.  Translate carriage return/linefeed sequences to linefeeds.
    !> Make single carriage returns delete to the beginning of the line.
    !> Make backspaces delete the previous character.  Port of
    !> comint.el:comint-carriage-motion.
    recursive subroutine comint_carriage_motion(start, end)
        integer(int64), intent(in) :: start, end
        type(value_type) :: v, inhibit
        type(handle_type) :: delete_end, saved_point
        type(status_type) :: st
        integer(int64) :: opoint, lbeg, de, moved
        logical :: changed
        integer(int32) :: ch
        logical :: have_delete_end
        call copy_marker_at(point(), saved_point, .false., st)
        call goto_char(start)
        call symbol_value(sym("inhibit_read_only"), inhibit, st)
        call set(sym("inhibit_read_only"), t_value(), st)
        lbeg = line_beginning_position()
        have_delete_end = .false.
        ! If the preceding text is marked as "must-overwrite", record it in
        ! delete-end.
        if (start > point_min()) then
            call get_text_property(start - 1, sym("comint_must_overwrite"), v, st)
            if (.not. nilp(v)) then
                call copy_marker_at(point(), delete_end, .false., st)
                have_delete_end = .true.
                call remove_text_properties(lbeg, start, list(sym("comint_must_overwrite"), nil()), changed, st)
            end if
        end if
        call narrow_to_region(lbeg, end, st)
        ! Handle BS, LF, and CR specially.
        do
            moved = skip_chars_forward("^"//achar(8)//new_line("a")//achar(13), st)
            if (eobp()) exit
            ch = following_char()
            if (ch == 8) then
                ! CH = BS
                call delete_char(1_int64, st)
                if (point() > lbeg) call delete_char(-1_int64, st)
            else if (ch == 10) then
                ! CH = LF
                if (have_delete_end) then
                    call marker_position(delete_end, de, st)
                    if (de < point()) call delete_region(lbeg, de, st)
                    call free_marker(delete_end, st)
                    have_delete_end = .false.
                end if
                call forward_char(st, 1_int64)
                lbeg = point()
            else
                ! CH = CR
                call delete_char(1_int64, st)
                if (have_delete_end) then
                    call marker_position(delete_end, de, st)
                    if (de < point()) then
                        call set_marker(delete_end, point(), st)
                    end if
                else
                    call copy_marker_at(point(), delete_end, .false., st)
                    have_delete_end = .true.
                end if
            end if
        end do
        if (have_delete_end) then
            call marker_position(delete_end, de, st)
            if (de < point()) then
                ! As there's a text after the last CR, make the current
                ! line contain only that text.
                call delete_region(lbeg, de, st)
            else
                ! Remember that the process output ends with CR, and thus
                ! the next output should overwrite the current line.
                call put_text_property(lbeg, point(), sym("comint_must_overwrite"), t_value(), st)
            end if
            call free_marker(delete_end, st)
        end if
        ! (The region stays narrowed, as upstream; save-excursion restores
        ! point.)
        call marker_position(saved_point, opoint, st)
        call goto_char(opoint)
        call free_marker(saved_point, st)
        call set(sym("inhibit_read_only"), inhibit, st)
    end subroutine comint_carriage_motion

    !> Sentinel for compilation buffers.  Port of
    !> compile.el:compilation-sentinel.
    recursive subroutine compilation_sentinel(proc, msg, status)
        type(handle_type), intent(in) :: proc
        character(len=*), intent(in) :: msg
        type(status_type), intent(out) :: status
        type(handle_type) :: buffer, old
        type(status_type) :: st
        character(len=:), allocatable :: state
        integer :: k
        status = status_type()
        state = process_status(proc)
        if (state /= "exit" .and. state /= "signal") return
        buffer = process_buffer(proc)
        if (.not. buffer_live_p(buffer)) then
            ! Buffer killed.
            call set_process_buffer(proc, null_handle(), st)
        else
            old = current_buffer()
            call set_buffer(buffer, st)
            ! Write something in the compilation buffer and hack its mode
            ! line.
            call compilation_handle_exit(state, process_exit_status(proc), msg)
            if (buffer_live_p(old)) call set_buffer(old, st)
        end if
        if (allocated(compilation_in_progress)) then
            compilation_in_progress = pack(compilation_in_progress, &
                                           [(.not. same(compilation_in_progress(k), proc), &
                                             k=1, size(compilation_in_progress))])
        end if
        ! Since the buffer and mode line will show that the process is dead,
        ! we can delete it now.
        call delete_process(proc, st)
    end subroutine compilation_sentinel

    !> Write MSG in the current buffer and hack its `mode_line_process'.
    !> Port of compile.el:compilation-handle-exit (the mode line is
    !> Deferred).
    subroutine compilation_handle_exit(process_status_name, exit_status, msg)
        character(len=*), intent(in) :: process_status_name, msg
        integer, intent(in) :: exit_status
        type(value_type) :: inhibit
        type(status_type) :: st
        integer(int64) :: omax, opoint, now, rate
        integer :: k
        real :: elapsed
        character(len=32) :: duration
        character(len=:), allocatable :: line
        call symbol_value(sym("inhibit_read_only"), inhibit, st)
        call set(sym("inhibit_read_only"), t_value(), st)
        omax = point_max()
        opoint = point()
        ! Record where we put the message, so we can ignore it later on.
        call goto_char(omax)
        call compilation_insert_annotation(new_line("a")//buffers(cur)%mode_name//" "//msg)
        if (point() > point_min()) then
            if (buffer_char_before() == 10) call goto_char(point() - 1)
        end if
        k = entry_of(current_buffer())
        call system_clock(now, rate)
        elapsed = 0
        if (k /= 0) elapsed = real(now - compilation_buffers(k)%start_count)/real(max(rate, 1_int64))
        if (elapsed < 10) then
            write (duration, "(f0.2, ' s')") elapsed
        else if (elapsed < 60) then
            write (duration, "(f0.1, ' s')") elapsed
        else
            write (duration, "(i0, ':', i2.2, ':', i2.2)") int(elapsed)/3600, mod(int(elapsed)/60, 60), mod(int(elapsed), 60)
        end if
        if (duration(1:1) == ".") duration = "0"//duration
        call compilation_insert_annotation(" at "//current_time_19()//", duration "//trim(duration))
        call goto_char(point_max())
        ! Prevent that message from being recognized as a compilation error.
        call put_text_property(omax, point(), sym("compilation_handle_exit"), t_value(), st)
        line = msg
        do while (len(line) > 0)
            if (line(len(line):len(line)) /= new_line("a")) exit
            line = line(1:len(line) - 1)
            exit
        end do
        if (k /= 0) then
            ! (Its face, compilation-mode-line-fail when EXIT_STATUS > 0, and
            ! the display are Deferred.)
            compilation_buffers(k)%mode_line_process = ":"//process_status_name//" ["//trim(code_string())//"]"
        end if
        call message(buffers(cur)%mode_name//" "//line)
        if (opoint < omax) call goto_char(opoint)
        call set(sym("inhibit_read_only"), inhibit, st)
    contains

        function code_string() result(c)
            character(len=16) :: c
            write (c, "(i0)") exit_status
        end function code_string
    end subroutine compilation_handle_exit

    !> The character before point.
    integer function buffer_char_before()
        use editfns_fm, only: char_before
        integer(int32) :: c
        logical :: found
        call char_before(c, found)
        buffer_char_before = c
    end function buffer_char_before

    !> Kill the process made by the \[compile] or \[grep] commands.  Port
    !> of compile.el:kill-compilation.
    subroutine kill_compilation(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: buffer, proc
        call compilation_find_buffer(buffer, status)
        if (.not. status%ok()) return
        proc = get_buffer_process(buffer)
        if (proc%index /= 0) then
            call interrupt_process(status, proc)
        else
            call set_error(status, FM_GENERIC_ERROR, "The compilation process is not running")
        end if
    end subroutine kill_compilation

    !> Define Compilation mode and register the commands.
    subroutine syms_of_compile()
        type(status_type) :: st
        call major_modes%compilation_mode%define("compilation_mode", "Compilation", compilation_mode, &
                                                 compilation_mode_hook)
        major_modes%compilation_mode%special = .true.
        compilation_mode_map = major_modes%compilation_mode%map
        call set_keymap_parent(compilation_mode_map, special_mode_map, st)
        call keymap_set(compilation_mode_map, "C-c C-k", "kill_compilation", st)
        call keymap_set(compilation_mode_map, "g", "recompile", st)
        commands%compilation_mode%run => compilation_mode
        commands%compile%run => cmd_compile
        commands%recompile%run => cmd_recompile
        commands%kill_compilation%run => kill_compilation
        call add_local_variables_killer(kill_compilation_locals)
        if (.not. allocated(compile_command%value)) compile_command = default_compile_command()
    end subroutine syms_of_compile
end module compile_fm
