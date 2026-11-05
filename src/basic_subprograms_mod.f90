! basic_subprograms_mod.f90 -- basic functions from subr.el.
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

!> Questions (`read_key', `read_y_or_n', `read_number'),
!> `add_to_history', and the match data helpers (`match_string',
!> `string_matches', `is_looking_at', `looking_back').
module basic_subprograms_mod
    use error_mod, only: status_type, set_error, QUIT_CODE, FM_INVALID_REGULAR_EXPRESSION, VOID_VARIABLE_CODE, GENERIC_ERROR_CODE
    use execute_interactively_mod, only: prefix_argument_type
    use time_functions_mod, only: current_time, real
    use, intrinsic :: iso_fortran_env, only: int32, int64, real64
    use value_mod, only: value_type, xfixnum, cons, list_length, list_element, make_keymap_reference, xsymbol, symbols, &
                         command_type, list_elements, make_list, xstring, vtag, V_NULL, V_INTEGER, V_NAME, V_STRING, V_ARRAY
    use data_mod, only: symbol_value, set, value_eq, history_setting_type, find_history, history_length, &
                        history_delete_duplicates, string_to_number
    use character_mod, only: string_character_and_length
    use case_table_mod, only: downcase
    use hooks_mod, only: hook_type
    use buffer_mod, only: buffers, current, current_buffer, is_buffer_modified, restore_buffer_modified, &
                          invisibility_entry_type
    use insert_delete_mod, only: inhibit_modification_hooks
    use keyboard_mod, only: read_event, discard_input, this_command_keys
    use display_update_mod, only: ding
    use commands_mod, only: commands
    use execute_interactively_mod, only: prefix_argument
    use display_engine_mod, only: message, message_nolog, cursor_in_echo_area
    use keymap_mod, only: define_key, current_global_map, key_description, make_sparse_keymap, is_keymap_live, &
                          key_parse, lookup_key
    use handle_mod, only: handle_type
    use search_mod, only: string_match, looking_at, regular_expression_search_backward, match_beginning, match_end, &
                          regular_expression_search_forward, SEARCH_MOVE, regular_expression_quote, &
                          is_match_in_string, string_index_to_offset, SEARCH_NOERROR, match_data, &
                          set_match_data, match_data_translate, replace_match, case_fold_search
    use syntax_mod, only: skip_syntax_backward
    use editing_functions_mod, only: buffer_substring, point, goto_character, point_minimum, point_maximum, narrow_to_region, widen
    implicit none(type, external)
    private

    public :: read_key, read_y_or_n, query_replace_answer, add_to_history, history_length, history_delete_duplicates
    public :: global_set_key, read_number, undefined, initialize_subr, defvar_false, forward_identifier, find_tag_default, &
              find_tag_default_as_regular_expression, find_tag_default_as_identifier_regular_expression
    public :: make_progress_reporter, progress_reporter_update, progress_reporter_done
    public :: replace_string_in_region, replace_regular_expression_in_region
    public :: add_to_invisibility_specification, remove_from_invisibility_specification, bounds_of_identifier_at_point
    public :: read_character_from_minibuffer, read_character_choice, shell_quote_argument

    !> A numerical progress reporter (`make_progress_reporter' with
    !> MINIMUM_VALUE and MAXIMUM_VALUE): the value at which to report next (the
    !> first of upstream's two values) and the parameters vector.
    type, public :: progress_reporter_type
        real(real64) :: next_value = 0
        !> The time of the next echo area update, in seconds.
        real(real64) :: update_time = 0
        integer(int64) :: minimum_value = 0, maximum_value = 0
        character(len=:), allocatable :: text
        integer :: minimum_change = 1
        real(real64) :: minimum_time = 0.2_real64
        !> A pulsing reporter (no MIN-VALUE and MAX-VALUE) and its indicator.
        logical :: pulsing = .false.
        integer :: pulse_index = 0
    end type progress_reporter_type

    !> A change group (`prepare_change_group'): the buffer, its number of
    !> undo entries when the group was prepared, and whether undo was
    !> disabled then (upstream's buffer-undo-list t).
    type, public :: change_group_type
        type(handle_type) :: buffer
        integer(int64) :: undo_count = 0
        logical :: undo_disabled = .false.
    end type change_group_type
    public :: prepare_change_group, activate_change_group, accept_change_group, cancel_change_group
    public :: after_hook_procedure, add_delayed_after_hook, silent_modifications_type, silent_modifications_begin, &
              silent_modifications_end
    public :: execute_mode_hooks, delay_mode_hooks_begin, delay_mode_hooks_end, delaying_mode_hooks, &
              clear_delayed_mode_hooks

    !> Normal hook run in major mode functions, before the mode hooks.
    !> Port of subr.el change-major-mode-after-body-hook.
    type(hook_type), public, target :: change_major_mode_after_body_hook = &
                                       hook_type(name="change_major_mode_after_body_hook")

    !> Normal hook run at the very end of major mode functions.  Port of
    !> subr.el after-change-major-mode-hook.
    type(hook_type), public, target :: after_change_major_mode_hook = hook_type(name="after_change_major_mode_hook")
    public :: copy_overlay, remove_overlays, split_string
    public :: match_string, match_string_no_properties, string_matches, is_looking_at, looking_back, &
              match_substitute_replacement, is_subregexp_context

    !> A version, as `version' gives it from a version string: upstream's
    !> version-to-list result, with the status of the parse.  Compare two
    !> with the operators <, <=, ==, /=, >= and >.
    type, public :: version_type
        !> The version as an array of integers ("1.0pre2" is [1, 0, -1, 2]).
        integer(int64), allocatable :: numbers(:)
        !> An `error' error condition for invalid version syntax.
        type(status_type) :: status
    end type version_type
    public :: version
    public :: operator(<), operator(<=), operator(>), operator(>=), operator(==), operator(/=)

    !> Version V1 lower (older) than V2.
    interface operator(<)
        module procedure version_less
    end interface operator(<)

    !> Version V1 lower (older) than or equal to V2.
    interface operator(<=)
        module procedure version_less_or_equal
    end interface operator(<=)

    !> Version V1 higher (newer) than V2.
    interface operator(>)
        module procedure version_greater
    end interface operator(>)

    !> Version V1 higher (newer) than or equal to V2.
    interface operator(>=)
        module procedure version_greater_or_equal
    end interface operator(>=)

    !> Version V1 equal to V2.
    interface operator(==)
        module procedure version_equal
    end interface operator(==)

    !> Version V1 not equal to V2.
    interface operator(/=)
        module procedure version_not_equal
    end interface operator(/=)

    !> Give KEY a global binding as COMMAND (a command name or any key
    !> definition).
    interface global_set_key
        module procedure global_set_key_value, global_set_key_command
    end interface global_set_key

    public :: ANSWER_ACT, ANSWER_SKIP, ANSWER_EXIT, ANSWER_ACT_AND_EXIT, &
              ANSWER_AUTOMATIC, ANSWER_RECENTER, ANSWER_HELP, ANSWER_QUIT, ANSWER_NONE, &
              ANSWER_EDIT_REPLACEMENT, ANSWER_EDIT_REPLACEMENT_EXACT_CASE, ANSWER_ACT_AND_SHOW, &
              ANSWER_EDIT, ANSWER_DELETE_AND_EDIT, ANSWER_BACKUP, ANSWER_UNDO, ANSWER_UNDO_ALL, &
              ANSWER_DIFF, ANSWER_SCROLL_UP, ANSWER_EXIT_PREFIX, ANSWER_SCROLL_DOWN, ANSWER_SCROLL_OTHER_WINDOW, &
              ANSWER_SCROLL_OTHER_WINDOW_DOWN

    !> query-replace-map (replace.el), made by `initialize_query_replace_map'.
    type(handle_type), public :: query_replace_map
    public :: initialize_query_replace_map

    !> The keys of `query_replace_map' and their answers.  (Binding ESC
    !> would prohibit the M-v binding.  Instead, callers should check for
    !> ESC specially.)
    character(len=*), parameter, public :: QRM_KEYS(38) = [character(len=11) :: &
                                                       "SPC", "DEL", "<delete>", "<backspace>", "y", "n", "Y", "N", "e", "E", ",", &
                                                           "q", "RET", "<return>", ".", "C-r", "C-w", "C-l", "!", "^", "u", "U", &
                                                          "d", "C-h", "<f1>", "<help>", "?", "C-g", "C-]", "C-v", "M-v", "<next>", &
                                                           "<prior>", "C-M-v", "M-<next>", "C-M-S-v", "M-<prior>", "<escape>"]
    !> The answer each key of `QRM_KEYS' gives in `query_replace_map'.
    character(len=*), parameter, public :: QRM_ANSWERS(38) = [character(len=27) :: &
                                                              "act", "skip", "skip", "skip", "act", "skip", "act", "skip", &
                                                              "edit_replacement", "edit_replacement_exact_case", "act_and_show", &
                                                              "exit", "exit", "exit", "act_and_exit", "edit", "delete_and_edit", &
                                                              "recenter", "automatic", "backup", "undo", "undo_all", "diff", &
                                                              "help", "help", "help", "help", "quit", "quit", "scroll_up", &
                                                              "scroll_down", "scroll_up", "scroll_down", "scroll_other_window", &
                                                              "scroll_other_window", "scroll_other_window_down", &
                                                              "scroll_other_window_down", "exit_prefix"]

    !> The answers of `query_replace_map'.
    integer, parameter :: ANSWER_NONE = 0, ANSWER_ACT = 1, ANSWER_SKIP = 2, ANSWER_EXIT = 3, &
                          ANSWER_ACT_AND_EXIT = 4, ANSWER_AUTOMATIC = 5, ANSWER_RECENTER = 6, &
                          ANSWER_HELP = 7, ANSWER_QUIT = 8, ANSWER_EDIT_REPLACEMENT = 9, &
                          ANSWER_EDIT_REPLACEMENT_EXACT_CASE = 10, ANSWER_ACT_AND_SHOW = 11, &
                          ANSWER_EDIT = 12, ANSWER_DELETE_AND_EDIT = 13, ANSWER_BACKUP = 14, &
                          ANSWER_UNDO = 15, ANSWER_UNDO_ALL = 16, ANSWER_DIFF = 17, &
                          ANSWER_SCROLL_UP = 18, ANSWER_EXIT_PREFIX = 19, ANSWER_SCROLL_DOWN = 20, &
                          ANSWER_SCROLL_OTHER_WINDOW = 21, ANSWER_SCROLL_OTHER_WINDOW_DOWN = 22

    !> Use `read_key' when reading answers to "y or n" questions by `read_y_or_n'.
    !> Otherwise, use the `read_from_minibuffer' to read the answers.
    !>
    !> When reading via the minibuffer, you can use the normal commands
    !> available in the minibuffer, and can, for instance, temporarily
    !> switch to another buffer, do things there, and then switch back
    !> to the minibuffer before entering the character.  This is not
    !> possible when using `read_key', but using `read_key' may be less
    !> confusing to some users.  Port of subr.el y-or-n-p-use-read-key.
    logical, public :: read_y_or_n_use_read_key = .false.

    !> Name of the history list to add `read_y_or_n' answers to (unallocated for
    !> no history).  Port of subr.el y-or-n-p-history-variable.
    character(len=:), allocatable, public :: read_y_or_n_history_variable

    abstract interface
        !> A SWITCH_FUNCTION of `compose_mail': switch to and display the
        !> buffer named BUFFER_NAME, made if needed.  Not a port.
        recursive subroutine mail_switch_procedure(buffer_name, status)
            import :: status_type
            implicit none(type, external)
            character(len=*), intent(in) :: buffer_name
            type(status_type), intent(out) :: status
        end subroutine mail_switch_procedure

        !> The COMPOSEFUNC of a mail user agent: compose an outgoing mail
        !> message buffer with the arguments of `compose_mail'.  Not a port.
        recursive subroutine mail_compose_procedure(status, to, subject, continue, switch_function)
            import :: status_type, mail_switch_procedure
            implicit none(type, external)
            type(status_type), intent(out) :: status
            character(len=*), intent(in), optional :: to, subject
            logical, intent(in), optional :: continue
            procedure(mail_switch_procedure), optional :: switch_function
        end subroutine mail_compose_procedure
    end interface
    public :: mail_switch_procedure, mail_compose_procedure

    !> A mail-sending package for `mail_user_agent' (upstream a name
    !> with the attributes `define_mail_user_agent' sets): its name, the
    !> procedure that composes a message, the commands that send it and
    !> abort it, and the hook run before it is sent.
    type, public :: mail_user_agent_type
        character(len=64) :: name = ""
        !> COMPOSEFUNC.
        procedure(mail_compose_procedure), pointer, nopass :: compose => null()
        !> SENDFUNC, the command a user would run to send the message.
        type(command_type), pointer :: send => null()
        !> ABORTFUNC, the command a user would run to abort the message
        !> (`close_buffer' unless given).
        type(command_type), pointer :: abort => null()
        !> HOOKVAR, run before the message is actually sent (null for none).
        type(hook_type), pointer :: hook => null()
    contains
        !> Define the agent (`define_mail_user_agent').
        procedure :: define => define_mail_user_agent
    end type mail_user_agent_type

    !> What `silent_modifications_begin' changed, for
    !> `silent_modifications_end' to restore (the bindings of
    !> with-silent-modifications).
    type :: silent_modifications_type
        logical :: modified = .false.
        logical :: undo_enabled = .true.
        logical :: inhibit_modification_hooks = .false.
        type(value_type) :: inhibit_read_only
    end type silent_modifications_type

    abstract interface
        !> A function run after a mode's hooks (a minor mode's :after-hook,
        !> an element of `delayed_after_hook_functions').  Not a port.
        subroutine after_hook_procedure(status)
            import :: status_type
            implicit none(type, external)
            type(status_type), intent(out) :: status
        end subroutine after_hook_procedure
    end interface

    interface
        !> Ask user a "y or n" question.
        !> Return true (in ANSWER) if answer is "y" and false if it is "n".
        !>
        !> PROMPT is the string to display to ask the question; `read_y_or_n'
        !> adds "(y or n) " to it.  If PROMPT is a non-empty string, and
        !> it ends with a non-space character, a space character will be
        !> appended to it.
        !>
        !> No confirmation of the answer is requested; a single character is
        !> enough.  SPC also means yes, and DEL means no.
        !>
        !> To be precise, this function translates user input into responses
        !> by consulting the bindings in `query_replace_map'; see the
        !> documentation of that variable for more information.  In this
        !> case, the useful bindings are `act', `skip', `recenter',
        !> `scroll_up', `scroll_down', and `quit'.
        !> An `act' response means yes, and a `skip' response means no.
        !> A `quit' response means to invoke `abort_recursive_edit'.
        !> If the user enters `recenter', `scroll_up', or `scroll_down'
        !> responses, perform the requested window recentering or scrolling
        !> and ask again.
        !>
        !> By default, this function uses the minibuffer to read the key.
        !> If `read_y_or_n_use_read_key' is true, `read_key' is used
        !> instead (which means that the user can't change buffers (and the
        !> like) while `read_y_or_n' is running).  Port of subr.el:y-or-n-p
        !> (no help-form or dialog boxes).  (In basic_subprograms_read_mod.)
        recursive module subroutine read_y_or_n(prompt, answer, status)
            implicit none(type, external)
            character(len=*), intent(in) :: prompt
            logical, intent(out) :: answer
            type(status_type), intent(out) :: status
        end subroutine read_y_or_n

        !> Register subr.el's commands (and keyboard.c's
        !> abort-recursive-edit).  (In basic_subprograms_read_mod.)
        !> Read a character from the minibuffer, prompting for it with PROMPT.
        !> Like `read_character', but uses the minibuffer to read and return a character.
        !> CHARS, if present, are the characters accepted; the function will
        !> ignore any input that is not one of CHARS.
        !> HISTORY, if present, names the history list variable to use for
        !> navigating in input history using \`M-p' and \`M-n', with \`RET' to
        !> select a character from history.
        !> If HELP_PROCEDURE is present (upstream's `help_form' bound around the
        !> call), then pressing `help_character' displays it.  There is no need to
        !> explicitly add `help_character' to CHARS; `help_character' is bound
        !> automatically to `help_procedure_show'.  Port of
        !> subr.el:read-char-from-minibuffer (exiting after other
        !> insertions, its `post_command_hook', is deferred).  (In
        !> basic_subprograms_read_mod.)
        recursive module subroutine read_character_from_minibuffer(prompt, c, status, chars, history, help_procedure)
            implicit none(type, external)
            character(len=*), intent(in) :: prompt
            integer, intent(out) :: c
            type(status_type), intent(out) :: status
            integer, intent(in), optional :: chars(:)
            character(len=*), intent(in), optional :: history, help_procedure
        end subroutine read_character_from_minibuffer

        !> Read and return one of the characters in CHARS, prompting with PROMPT.
        !> CHARS should be an array of character codes.
        !> The function discards any input character that is not one of CHARS,
        !> and by default shows a message to the effect that it is not one of
        !> the expected characters.
        !>
        !> By default, this function uses the minibuffer to read the key
        !> non-modally (see `read_character_from_minibuffer'), and the optional
        !> argument INHIBIT_KEYBOARD_QUIT is ignored.  However, if
        !> `read_character_choice_use_read_key' is true, the modal `read_key'
        !> function is used instead (see read_character_choice_with_read_key),
        !> and INHIBIT_KEYBOARD_QUIT is passed to it.  HELP_PROCEDURE is the text
        !> `help_character' shows (upstream's `help_form').  Port of
        !> subr.el:read-char-choice.  (In basic_subprograms_read_mod.)
        recursive module subroutine read_character_choice(prompt, chars, c, status, inhibit_keyboard_quit, help_procedure)
            implicit none(type, external)
            character(len=*), intent(in) :: prompt
            integer, intent(in) :: chars(:)
            integer, intent(out) :: c
            type(status_type), intent(out) :: status
            logical, intent(in), optional :: inhibit_keyboard_quit
            character(len=*), intent(in), optional :: help_procedure
        end subroutine read_character_choice

        !> Set up subr.el's commands.  Not a port: upstream defines them where
        !> subr.el is loaded.
        module subroutine initialize_subr()
            implicit none(type, external)
        end subroutine initialize_subr

        !> Read from the minibuffer and return a numeric value, prompting with PROMPT.
        !> DEFAULT specifies a default value to return if the user just types RET.
        !> For historical reasons, the value of DEFAULT is always inserted into
        !> PROMPT, so it's recommended to use `format_text' instead of `format_prompt'
        !> to generate PROMPT.  HISTORY specifies a history list variable.  See
        !> `read_from_minibuffer' for details of the HISTORY argument.
        !>
        !> DEFAULTS, in place of DEFAULT, is an array of defaults: the first
        !> is the default, all are offered by M-n; with NO_DEFAULT, the list
        !> starts with an empty default (there is none, the others are offered
        !> by M-n).  Port of subr.el:read-number (the answer is read as
        !> upstream's reader reads an integer; floats are not ported).  (In
        !> basic_subprograms_read_mod.)
        module subroutine read_number(prompt, n, status, default, history, defaults, no_default)
            implicit none(type, external)
            character(len=*), intent(in) :: prompt
            integer(int64), intent(out) :: n
            type(status_type), intent(out) :: status
            integer(int64), intent(in), optional :: default
            character(len=*), intent(in), optional :: history
            integer(int64), intent(in), optional :: defaults(:)
            logical, intent(in), optional :: no_default
        end subroutine read_number

        !> Run mode hooks `delayed_mode_hooks' and HOOK, or delay HOOK.
        !>
        !> If the variable `delay_mode_hooks' is true, does not do anything,
        !> just adds HOOK to `delayed_mode_hooks'.
        !> Otherwise, runs hooks in the sequence: `change_major_mode_after_body_hook',
        !> `delayed_mode_hooks' (in reverse order), HOOK, then
        !> runs the hook `after_change_major_mode_hook'.
        !>
        !> Major mode functions should use this instead of `run_hooks' when
        !> running their FOO_mode_hook.  Port of subr.el:run-mode-hooks
        !> (no file-local variables yet).  (In basic_subprograms_hooks_mod.)
        recursive module subroutine execute_mode_hooks(status, hook)
            implicit none(type, external)
            type(status_type), intent(out) :: status
            type(hook_type), intent(in), target, optional :: hook
        end subroutine execute_mode_hooks

        !> Start `delay_mode_hooks': until the matching
        !> delay_mode_hooks_end, `execute_mode_hooks' only records its hooks
        !> (in the current buffer).  SAVED keeps the outer value.  Port of
        !> subr.el:delay-mode-hooks.  (In basic_subprograms_hooks_mod.)
        module subroutine delay_mode_hooks_begin(saved)
            implicit none(type, external)
            logical, intent(out) :: saved
        end subroutine delay_mode_hooks_begin

        !> End `delay_mode_hooks', restoring the value SAVED.  (In
        !> basic_subprograms_hooks_mod.) Not a port.
        module subroutine delay_mode_hooks_end(saved)
            implicit none(type, external)
            logical, intent(in) :: saved
        end subroutine delay_mode_hooks_end

        !> Forget the current buffer's `delayed_mode_hooks' (a local
        !> variable that `clear_all_local_variables' kills).  (In
        !> basic_subprograms_hooks_mod.) Not a port.
        module subroutine clear_delayed_mode_hooks()
            implicit none(type, external)
        end subroutine clear_delayed_mode_hooks

        !> Whether mode hooks are being delayed in the current buffer (the
        !> variable `delay_mode_hooks').  (In basic_subprograms_hooks_mod.) Not a port.
        module logical function delaying_mode_hooks()
            implicit none(type, external)
        end function delaying_mode_hooks

        !> Arrange for FN to run after the delayed mode hooks: add it to
        !> the current buffer's `delayed_after_hook_functions', which
        !> `execute_mode_hooks' runs after `after_change_major_mode_hook'.
        !> (Upstream's minor modes push their :after-hook there while mode
        !> hooks are delayed.)  (In basic_subprograms_hooks_mod.) Not a port.
        module subroutine add_delayed_after_hook(fn)
            implicit none(type, external)
            procedure(after_hook_procedure) :: fn
        end subroutine add_delayed_after_hook

        !> Return a handle for the current buffer's state, for a change group.
        !>
        !> Pass the handle to `activate_change_group' afterward to initiate
        !> the actual changes of the change group.
        !>
        !> To finish the change group, call either `accept_change_group' or
        !> `cancel_change_group' passing the same handle as argument.  Call
        !> `accept_change_group' to accept the changes in the group as final;
        !> call `cancel_change_group' to undo them all.  Once you finish the
        !> group, don't use the handle again--don't try to finish the same
        !> group twice.
        !>
        !> As long as this handle is still in use, do not call procedures
        !> which edit the undo list: if it no longer contains its current
        !> value, Fortmacs will not be able to cancel the change group.  Port of
        !> subr.el:prepare-change-group (the current buffer; a group of several
        !> buffers is Deferred).  (In basic_subprograms_change_group_mod.)
        module subroutine prepare_change_group(handle)
            implicit none(type, external)
            type(change_group_type), intent(out) :: handle
        end subroutine prepare_change_group

        !> Activate a change group made with `prepare_change_group' (which
        !> see).  Port of subr.el:activate-change-group.  (In
        !> basic_subprograms_change_group_mod.)
        module subroutine activate_change_group(handle)
            implicit none(type, external)
            type(change_group_type), intent(in) :: handle
        end subroutine activate_change_group

        !> Finish a change group made with `prepare_change_group' (which see).
        !> This finishes the change group by accepting its changes as final.
        !> Port of subr.el:accept-change-group.  (In basic_subprograms_change_group_mod.)
        module subroutine accept_change_group(handle)
            implicit none(type, external)
            type(change_group_type), intent(in) :: handle
        end subroutine accept_change_group

        !> Finish a change group made with `prepare_change_group' (which see).
        !> This finishes the change group by reverting all of its changes.
        !> Port of subr.el:cancel-change-group.  (In basic_subprograms_change_group_mod.)
        recursive module subroutine cancel_change_group(handle, status)
            implicit none(type, external)
            type(change_group_type), intent(in) :: handle
            type(status_type), intent(out) :: status
        end subroutine cancel_change_group
    end interface

contains

    !> Define the agent, named NAME, to identify a mail-sending package for
    !> `mail_user_agent'.
    !>
    !> COMPOSE is a procedure that composes an outgoing
    !> mail message buffer.  This procedure should set up the basics of the
    !> buffer without requiring user interaction.  It should populate the
    !> standard mail headers, leaving the `to:' and `subject:' headers blank
    !> by default.
    !>
    !> COMPOSE should accept several optional arguments--the same
    !> arguments that `compose_mail' takes.  See that function's documentation.
    !>
    !> SEND is the command a user would run to send the message.
    !>
    !> Optional ABORT is the command a user would run to abort the
    !> message.  For mail packages that don't have a separate abort function,
    !> this can be `close_buffer' (the equivalent of omitting this argument).
    !>
    !> Optional HOOK is a hook that gets run before the message
    !> is actually sent.  Callers that use the `mail_user_agent' may
    !> install a hook function temporarily on this hook.  Port of
    !> subr.el:define-mail-user-agent.
    subroutine define_mail_user_agent(self, name, compose, send, abort, hook)
        class(mail_user_agent_type), intent(inout) :: self
        character(len=*), intent(in) :: name
        procedure(mail_compose_procedure) :: compose
        type(command_type), intent(in), target :: send
        type(command_type), intent(in), target, optional :: abort
        type(hook_type), intent(in), target, optional :: hook

        self%name = name
        self%compose => compose
        self%send => send
        self%abort => commands%close_buffer
        if (present(abort)) self%abort => abort
        self%hook => null()
        if (present(hook)) self%hook => hook
    end subroutine define_mail_user_agent

    !> Replace STRING with REPLACEMENT in the region from START to END.
    !> The number of replaced occurrences is returned in MATCHES (0 if
    !> STRING doesn't exist in the region).
    !>
    !> If START is absent, use the current point.  If END is absent, use
    !> `point_maximum'.
    !>
    !> Comparisons and replacements are done with fixed case.
    !> Port of subr.el:replace-string-in-region.
    recursive subroutine replace_string_in_region(string, replacement, matches, status, start, end)
        character(len=*), intent(in) :: string, replacement
        integer(int64), intent(out) :: matches
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: start, end

        call replace_in_region(string, replacement, .false., matches, status, start, end)
    end subroutine replace_string_in_region

    !> Replace REGULAR_EXPRESSION with REPLACEMENT in the region from START to END.
    !> The number of replaced occurrences is returned in MATCHES (0 if
    !> REGULAR_EXPRESSION doesn't exist in the region).
    !>
    !> If START is absent, use the current point.  If END is absent, use
    !> `point_maximum'.
    !>
    !> Comparisons and replacements are done with fixed case.
    !>
    !> REPLACEMENT can use the following special elements:
    !>
    !>   `\&' in NEWTEXT means substitute original matched text.
    !>   `\N' means substitute what matched the Nth `\(...\)'.
    !>        If Nth parens didn't match, substitute nothing.
    !>   `\\' means insert one `\'.
    !>   `\?' is treated literally.
    !> Port of subr.el:replace-regexp-in-region.
    recursive subroutine replace_regular_expression_in_region(regular_expression, replacement, matches, status, start, end)
        character(len=*), intent(in) :: regular_expression, replacement
        integer(int64), intent(out) :: matches
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: start, end

        call replace_in_region(regular_expression, replacement, .true., matches, status, start, end)
    end subroutine replace_regular_expression_in_region

    ! The body of `replace_string_in_region' (REGULAR_EXPRESSION_FLAG false) and
    ! `replace_regular_expression_in_region'.
    ! Port of subr.el:replace-string-in-region and
    ! subr.el:replace-regexp-in-region, in part (their shared body).
    recursive subroutine replace_in_region(pattern, replacement, regular_expression_flag, matches, status, start, end)
        use marker_mod, only: copy_marker_at, marker_position, destroy_marker
        use buffer_mod, only: buffers, current
        use search_mod, only: case_fold_search, search_forward, replace_match
        use error_mod, only: set_error, GENERIC_ERROR_CODE
        character(len=*), intent(in) :: pattern, replacement
        logical, intent(in) :: regular_expression_flag
        integer(int64), intent(out) :: matches
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: start, end
        type(handle_type) :: original_point, obeg, oend
        integer(int64) :: s, e, found, position
        logical :: narrowed, saved_fold
        type(status_type) :: st

        matches = 0
        status = status_type()
        s = point()

        if (present(start)) then
            s = start

            if (s < point_minimum()) then
                call set_error(status, GENERIC_ERROR_CODE, "Start before start of buffer")
                return
            end if
        end if

        e = point_maximum()

        if (present(end)) then
            e = end

            if (e > point_maximum()) then
                call set_error(status, GENERIC_ERROR_CODE, "End after end of buffer")
                return
            end if
        end if

        ! (save-excursion (goto-char start) (save-restriction ...))
        call copy_marker_at(point(), original_point, .false., st)
        narrowed = buffers(current)%beginning_of_accessible_portion /= 1 .or. buffers(current)%end_of_accessible_portion &
                   /= buffers(current)%text%z

        if (narrowed) then
            call copy_marker_at(buffers(current)%beginning_of_accessible_portion, obeg, .false., st)
            call copy_marker_at(buffers(current)%end_of_accessible_portion, oend, .true., st)
        end if

        call goto_character(s)
        call narrow_to_region(s, e, st)
        saved_fold = case_fold_search%value
        case_fold_search%value = .false.

        do
            if (regular_expression_flag) then
                call regular_expression_search_forward(pattern, found, status, noerror=SEARCH_NOERROR)
            else
                call search_forward(pattern, found, status, noerror=SEARCH_NOERROR)
            end if

            if (.not. status%ok() .or. found == 0) exit

            if (regular_expression_flag) then
                call replace_match(replacement, status, fixedcase=.true.)
            else
                call replace_match(replacement, status, fixedcase=.true., literal=.true.)
            end if

            if (.not. status%ok()) exit
            matches = matches + 1
        end do

        case_fold_search%value = saved_fold
        call widen()

        if (narrowed) then
            call marker_position(obeg, s, st)
            call marker_position(oend, e, st)
            call narrow_to_region(s, e, st)
            call destroy_marker(obeg, st)
            call destroy_marker(oend, st)
        end if

        call marker_position(original_point, position, st)
        call goto_character(position)
        call destroy_marker(original_point, st)
    end subroutine replace_in_region

    !> Add ELEMENT to `buffer_invisibility_specification'.
    !> See documentation for `buffer_invisibility_specification' for the kind of
    !> elements that can be added: the name ELEMENT, with ELLIPSIS when
    !> given.
    !>
    !> If `buffer_invisibility_specification' isn't an array before calling this
    !> function, `buffer_invisibility_specification' will afterwards be an array
    !> of the two entries .true. and ELEMENT.  This means that if text exists
    !> with invisibility values that aren't either .true. or ELEMENT, that
    !> text will become visible.  Port of subr.el:add-to-invisibility-spec.
    subroutine add_to_invisibility_specification(element, ellipsis)
        character(len=*), intent(in) :: element
        logical, intent(in), optional :: ellipsis
        type(invisibility_entry_type) :: e

        if (current == 0) return

        associate (b => buffers(current))
            if (b%invisibility_specification_t) then
                b%invisibility_specification_t = .false.
                if (allocated(b%invisibility_specification)) deallocate (b%invisibility_specification)
                allocate (b%invisibility_specification(1))
                b%invisibility_specification(1)%atom = value_type(.true.)
            end if

            if (.not. allocated(b%invisibility_specification)) allocate (b%invisibility_specification(0))
            e%atom = value_type(name=element)

            if (present(ellipsis)) then
                e%cons = .true.
                e%ellipsis = ellipsis
            end if

            b%invisibility_specification = [e, b%invisibility_specification]
        end associate
    end subroutine add_to_invisibility_specification

    !> Remove ELEMENT from `buffer_invisibility_specification' (the name ELEMENT,
    !> with ELLIPSIS when given).
    !> If `buffer_invisibility_specification' isn't an array before calling this
    !> function, it will be made into an array containing just .true. as
    !> its only entry.  This means that if text exists with invisibility
    !> values other than .true., that text will become visible.  Port of
    !> subr.el:remove-from-invisibility-spec.
    subroutine remove_from_invisibility_specification(element, ellipsis)
        character(len=*), intent(in) :: element
        logical, intent(in), optional :: ellipsis
        logical, allocatable :: keep(:)
        integer :: k

        if (current == 0) return

        associate (b => buffers(current))
            if (b%invisibility_specification_t .or. .not. allocated(b%invisibility_specification)) then
                b%invisibility_specification_t = .false.
                if (allocated(b%invisibility_specification)) deallocate (b%invisibility_specification)
                allocate (b%invisibility_specification(1))
                b%invisibility_specification(1)%atom = value_type(.true.)
                return
            end if

            ! (delete ELEMENT ...): `equal' elements.
            allocate (keep(size(b%invisibility_specification)))

            do k = 1, size(b%invisibility_specification)
                keep(k) = .not. same_entry(b%invisibility_specification(k))
            end do

            b%invisibility_specification = pack(b%invisibility_specification, keep)
        end associate

    contains

        logical function same_entry(e)
            type(invisibility_entry_type), intent(in) :: e

            same_entry = value_eq(e%atom, value_type(name=element)) .and. (e%cons .eqv. present(ellipsis))
            if (same_entry .and. present(ellipsis)) same_entry = e%ellipsis .eqv. ellipsis
        end function same_entry
    end subroutine remove_from_invisibility_specification

    !> Quote ARGUMENT for passing as argument to an inferior shell.
    !>
    !> This function is designed to work with the syntax of your system's
    !> standard shell, and might produce incorrect results with unusual shells.
    !> See Info node `(elisp)Security Considerations'.  Port of
    !> subr.el:shell-quote-argument (POSIX shells: the systems are Linux).
    pure function shell_quote_argument(argument) result(quoted)
        character(len=*), intent(in) :: argument
        character(len=:), allocatable :: quoted
        integer :: i, b

        if (len(argument) == 0) then
            quoted = "''"
            return
        end if

        ! Quote everything except POSIX filename characters.
        ! This should be safe enough even for really weird shells.
        quoted = ""

        do i = 1, len(argument)
            b = ichar(argument(i:i), kind=int32)

            if (index("-0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_./", argument(i:i)) > 0 .or. &
                b == 10) then
                if (b == 10) then
                    quoted = quoted//"'"//achar(10)//"'"
                else
                    quoted = quoted//argument(i:i)
                end if
            else if (b >= 128 .and. b < 192) then
                ! A continuation byte of the character quoted before.
                quoted = quoted//argument(i:i)
            else
                quoted = quoted//"\"//argument(i:i)
            end if
        end do
    end function shell_quote_argument

    !> Start modifying the current buffer without marking it modified,
    !> recording undo, running the change hooks or asking about read-only
    !> text; S keeps what to restore.  The start of subr.el's
    !> with-silent-modifications.
    !> Port of subr.el:with-silent-modifications, in part.
    subroutine silent_modifications_begin(s)
        type(silent_modifications_type), intent(out) :: s
        type(status_type) :: st

        if (current == 0) return
        call is_buffer_modified(current_buffer(), s%modified, st)
        s%undo_enabled = buffers(current)%undo_enabled
        buffers(current)%undo_enabled = .false.
        call symbol_value(value_type(name="inhibit_read_only"), s%inhibit_read_only, st)
        call set(value_type(name="inhibit_read_only"), value_type(.true.), st)
        s%inhibit_modification_hooks = inhibit_modification_hooks%value
        inhibit_modification_hooks = .true.
    end subroutine silent_modifications_begin

    !> Restore what `silent_modifications_begin' changed: the end of
    !> with-silent-modifications (the buffer's modified flag as it was).
    !> Port of subr.el:with-silent-modifications, in part.
    subroutine silent_modifications_end(s)
        type(silent_modifications_type), intent(in) :: s
        type(status_type) :: st

        if (current == 0) return
        inhibit_modification_hooks = s%inhibit_modification_hooks
        call set(value_type(name="inhibit_read_only"), s%inhibit_read_only, st)
        buffers(current)%undo_enabled = s%undo_enabled
        call restore_buffer_modified(s%modified)
    end subroutine silent_modifications_end

    !> Read one key with `prompt' in the echo area and the cursor after it.
    !> End of input reads as a quit.  Port of subr.el:read-key (single
    !> events; no keymap lookup of multi-event keys yet).
    subroutine read_key(prompt, key, status)
        character(len=*), intent(in) :: prompt
        type(value_type), intent(out) :: key
        type(status_type), intent(out) :: status
        logical :: have

        call message_nolog(prompt)
        cursor_in_echo_area = .true.
        call read_event(key, have, status)
        cursor_in_echo_area = .false.
        if (.not. status%ok()) return
        if (.not. have) call set_error(status, QUIT_CODE)
    end subroutine read_key

    !> Keymap of responses to questions posed by commands like `query_replace'.
    !> The "bindings" in this map are not commands; they are answers.
    !> The valid answers include `act', `skip', `act_and_show',
    !> `act_and_exit', `exit', `exit_prefix', `recenter', `scroll_up',
    !> `scroll_down', `scroll_other_window', `scroll_other_window_down',
    !> `edit', `edit_replacement', `edit_replacement_exact_case',
    !> `delete_and_edit', `automatic', `backup', `undo', `undo_all',
    !> `quit', and `help'.  Made on first use.  Port of replace.el
    !> query-replace-map.
    subroutine initialize_query_replace_map()
        type(value_type), allocatable :: seq(:)
        type(status_type) :: st
        integer :: k

        if (is_keymap_live(query_replace_map)) return
        call make_sparse_keymap(query_replace_map)

        do k = 1, size(QRM_KEYS)
            call key_parse(trim(QRM_KEYS(k)), seq, st)
            if (st%ok()) call define_key(query_replace_map, seq, value_type(name=trim(QRM_ANSWERS(k))), st)
        end do
    end subroutine initialize_query_replace_map

    !> The answer `key' is in `query_replace_map' (the bindings
    !> y-or-n-p, map-y-or-n-p and perform-replace use): an ANSWER_ code,
    !> ANSWER_NONE if it has none.  Not a port.
    integer function query_replace_answer(key) result(answer)
        type(value_type), intent(in) :: key
        type(value_type) :: def
        type(status_type) :: st
        character(len=:), allocatable :: name

        answer = ANSWER_NONE
        call initialize_query_replace_map()
        call lookup_key(make_keymap_reference(query_replace_map), [key], def, st, .true.)
        if (.not. st%ok() .or. (vtag(def) /= V_NAME .and. vtag(def) /= V_NULL)) return
        name = symbols(xsymbol(def))%name

        select case (name)
        case ("act")
            answer = ANSWER_ACT
        case ("skip")
            answer = ANSWER_SKIP
        case ("edit_replacement")
            answer = ANSWER_EDIT_REPLACEMENT
        case ("edit_replacement_exact_case")
            answer = ANSWER_EDIT_REPLACEMENT_EXACT_CASE
        case ("act_and_show")
            answer = ANSWER_ACT_AND_SHOW
        case ("exit")
            answer = ANSWER_EXIT
        case ("act_and_exit")
            answer = ANSWER_ACT_AND_EXIT
        case ("edit")
            answer = ANSWER_EDIT
        case ("delete_and_edit")
            answer = ANSWER_DELETE_AND_EDIT
        case ("recenter")
            answer = ANSWER_RECENTER
        case ("automatic")
            answer = ANSWER_AUTOMATIC
        case ("backup")
            answer = ANSWER_BACKUP
        case ("undo")
            answer = ANSWER_UNDO
        case ("undo_all")
            answer = ANSWER_UNDO_ALL
        case ("diff")
            answer = ANSWER_DIFF
        case ("help")
            answer = ANSWER_HELP
        case ("quit")
            answer = ANSWER_QUIT
        case ("scroll_up")
            answer = ANSWER_SCROLL_UP
        case ("scroll_down")
            answer = ANSWER_SCROLL_DOWN
        case ("scroll_other_window")
            answer = ANSWER_SCROLL_OTHER_WINDOW
        case ("scroll_other_window_down")
            answer = ANSWER_SCROLL_OTHER_WINDOW_DOWN
        case ("exit_prefix")
            answer = ANSWER_EXIT_PREFIX
        case default
            answer = ANSWER_NONE
        end select
    end function query_replace_answer

    !> Add NEWELT to the history list called HISTORY-VAR.
    !> If MAXELT is present, it specifies the maximum length of the history.
    !> Otherwise, the maximum history length is the history's `%length',
    !> if set, or the value of the `history_length' variable.  The possible
    !> values of maximum length have the same meaning as the values of
    !> `history_length'.
    !> Remove duplicates of NEWELT if `history_delete_duplicates' is .true..
    !> If optional fourth arg KEEP-ALL is .true., add NEWELT to history even
    !> if it is empty or duplicates the most recent entry in the history.
    !> Port of subr.el:add-to-history (the history is found by name, see
    !> `find_history'; `added' says whether the list changed; a name that
    !> is no history is a `void_variable' error in `status').
    subroutine add_to_history(history_variable, newelt, maxelt, keep_all, added, status)
        character(len=*), intent(in) :: history_variable, newelt
        integer, intent(in), optional :: maxelt
        logical, intent(in), optional :: keep_all
        logical, intent(out), optional :: added
        type(status_type), intent(out), optional :: status
        type(history_setting_type), pointer :: history

        if (present(added)) added = .false.
        if (present(status)) status = status_type()
        history => find_history(history_variable)

        if (.not. associated(history)) then
            if (present(status)) call set_error(status, VOID_VARIABLE_CODE, history_variable)
            return
        end if

        call history%append(newelt, maxelt, keep_all, added)
    end subroutine add_to_history

    !> Beep to tell the user this binding is undefined.  Port of
    !> subr.el:undefined (no keyboard macro definition or mouse events).
    subroutine undefined(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call ding(status=status)
        if (.not. status%ok()) return
        call message(key_description(this_command_keys)//" is undefined")
        ! If this is a down-mouse event, don't reset prefix-arg; pass it to
        ! the command run by the up event.
        prefix_argument = prefix_argument_type()
    end subroutine undefined

    !> Give the variable NAME an unallocated value unless it
    !> has a value already.  Port of eval.c:Fdefvar (INITVALUE empty).
    subroutine defvar_false(name)
        character(len=*), intent(in) :: name
        type(value_type) :: v
        type(status_type) :: st

        call symbol_value(value_type(name=name), v, st)
        if (.not. st%ok()) call set(value_type(name=name), value_type(), st)
    end subroutine defvar_false

    !> Give KEY a global binding as COMMAND.
    !> This is a legacy function; see `keymap_global_set' for the
    !> recommended function to use instead.
    !>
    !> COMMAND is the command definition to use; usually it is
    !> a name of an interactively-callable function.
    !> KEY is a key sequence: an array of events, as `key_parse' makes.
    !>
    !> Note that if KEY has a local binding in the current buffer,
    !> that local binding will continue to shadow any global binding
    !> that you make with this function.  Port of subr.el:global-set-key.
    subroutine global_set_key_value(key, command, status)
        type(value_type), intent(in) :: key(:), command
        type(status_type), intent(out) :: status

        call define_key(current_global_map(), key, command, status)
    end subroutine global_set_key_value

    !> `global_set_key' with the command named COMMAND.  Not a port.
    subroutine global_set_key_command(key, command, status)
        type(value_type), intent(in) :: key(:)
        character(len=*), intent(in) :: command
        type(status_type), intent(out) :: status

        call define_key(current_global_map(), key, value_type(name=command), status)
    end subroutine global_set_key_command

    !> Return the string of text matched by the previous search or regexp
    !> operation.  NUMBER specifies the number of the parenthesized
    !> sub-expression in the last regexp whose match to return.  Zero
    !> means the entire text matched by the whole regexp or whole string.
    !>
    !> The return value is an empty string (and `matched' false) if NUMth
    !> subexpression didn't match, or there were less than NUMBER subexpressions.
    !>
    !> STRING should be given if the last search was by `string_match' on
    !> STRING.  If STRING is absent, the current buffer should be the same
    !> buffer as the one in which the search/match was performed.  Port of
    !> subr.el:match-string.
    function match_string(number, string, matched) result(text)
        integer, intent(in) :: number
        character(len=*), intent(in), optional :: string
        logical, intent(out), optional :: matched
        character(len=:), allocatable :: text
        integer(int64) :: b, e
        type(status_type) :: status

        text = ""
        if (present(matched)) matched = .false.
        b = match_beginning(number, status)
        if (.not. status%ok() .or. b == 0) return
        e = match_end(number, status)
        if (present(matched)) matched = .true.

        if (present(string)) then
            text = string(string_index_to_offset(string, b) + 1:string_index_to_offset(string, e))
        else
            call buffer_substring(b, e, text, status)
        end if
    end function match_string

    !> Return string of text matched by last search, without text
    !> properties.  NUMBER specifies which parenthesized expression in the
    !> last regexp.  Value is an empty string if NUMth subexpression didn't match, or there
    !> were less than NUMBER subexpressions.  Zero means the entire text matched by
    !> the whole regexp or whole string.  STRING should be given if the
    !> last search was by `string_match' on STRING.  If STRING is absent, the
    !> current buffer should be the same buffer as the one in which the
    !> search/match was performed.  Port of
    !> subr.el:match-string-no-properties.
    function match_string_no_properties(number, string, matched) result(text)
        integer, intent(in) :: number
        character(len=*), intent(in), optional :: string
        logical, intent(out), optional :: matched
        character(len=:), allocatable :: text

        text = match_string(number, string, matched)
    end function match_string_no_properties

    !> Return REPLACEMENT as it will be inserted by `replace_match'.
    !> In other words, all back-references in the form `\&' and `\N'
    !> are substituted with actual strings matched by the last search.
    !> Optional FIXEDCASE, LITERAL, STRING and SUBEXP have the same
    !> meaning as for `replace_match'.  Port of
    !> subr.el:match-substitute-replacement.
    function match_substitute_replacement(replacement, status, fixedcase, literal, string, subexp) result(text)
        character(len=*), intent(in) :: replacement
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: fixedcase, literal
        character(len=*), intent(in), optional :: string
        integer, intent(in), optional :: subexp
        character(len=:), allocatable :: text, match
        integer(int64), allocatable :: saved(:)
        integer(int64) :: start

        text = ""
        match = match_string(0, string)
        ! save-match-data
        saved = match_data()
        start = match_beginning(0, status)
        if (.not. status%ok()) return
        call match_data_translate(1 - start)
        call replace_match(replacement, status, fixedcase, literal, match, subexp, text)
        call set_match_data(saved)
    end function match_substitute_replacement

    !> Return true if POSITION is in a normal subregexp context in REGULAR_EXPRESSION.
    !> A subregexp context is one where a sub-regexp can appear.
    !> A non-subregexp context is for example within brackets, or within a
    !> repetition bounds operator `\{...\}', or right after a `\'.
    !> If START is present, it should be a position in REGULAR_EXPRESSION, smaller
    !> than POSITION, and known to be in a subregexp context.  (POSITION and START are
    !> 1-based character indices: the text before the character at POSITION
    !> is examined.)  Port of subr.el:subregexp-context-p.
    recursive logical function is_subregexp_context(regular_expression, position, status, start) result(context)
        character(len=*), intent(in) :: regular_expression
        integer(int64), intent(in) :: position
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: start
        integer(int64) :: b, e, index

        b = 0
        if (present(start)) b = string_index_to_offset(regular_expression, start)
        e = string_index_to_offset(regular_expression, position)
        index = string_match(regular_expression(b + 1:e), "", status, inhibit_modify=.true.)
        context = .true.
        if (status%code /= FM_INVALID_REGULAR_EXPRESSION) return
        context = .not. (status%detail == """Unmatched [ or [^""" .or. status%detail == """Unmatched \{""" .or. &
                         status%detail == """Trailing backslash""")
        status = status_type()
        if (index < 0) continue
    end function is_subregexp_context

    !> Whether REGULAR_EXPRESSION matches STRING, as `string_match' finds; this function
    !> does not change the match data.  Port of subr.el:string-match-p.
    logical function string_matches(regular_expression, string, status, start) result(found)
        character(len=*), intent(in) :: regular_expression, string
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: start

        found = string_match(regular_expression, string, status, start, inhibit_modify=.true.) > 0
    end function string_matches

    !> The version that version string TEXT gives, as an array of
    !> integers (the NUMBERS component).  Compare two versions with the
    !> operators <, <=, ==, /=, >= and >: version(a) <= version(b).
    !>
    !> The version syntax is given by the following EBNF:
    !>
    !>    VERSION ::= NUMBER ( SEPARATOR NUMBER )*.
    !>
    !>    NUMBER ::= (0|1|2|3|4|5|6|7|8|9)+.
    !>
    !>    SEPARATOR ::= "." (upstream's version-separator)
    !>                | a match for version-regexp-alist.
    !>
    !> The NUMBER part is optional if SEPARATOR is a match for an element
    !> of version-regexp-alist: "snapshot", a lone "-", "_", "." or "+",
    !> "cvs", "git", "bzr", "svn", "hg", "darcs" and "unknown" give -4,
    !> "alpha" -3, "beta" -2, "pre" and "rc" -1, each after an optional
    !> "-", ".", "_", "+" or space, ignoring case.  A single letter at
    !> the end, after an optional separator, gives its place in the
    !> alphabet (22.3a is 22.3.1).
    !>
    !> Examples of valid version syntax:
    !>
    !>    1.0pre2   1.0.7.5   22.8beta3   0.9alpha1   6.9.30Beta   2.4.snapshot   .5
    !>
    !> Examples of invalid version syntax:
    !>
    !>    1.0prepre2   1.0..7.5   22.8X3   alpha3.2
    !>
    !> Examples of version conversion:
    !>
    !>    Version String    Version as an Array of Integers
    !>    ".5"              [0, 5]
    !>    "0.9 alpha"       [0, 9, -3]
    !>    "0.9AlphA1"       [0, 9, -3, 1]
    !>    "0.9snapshot"     [0, 9, -4]
    !>    "1.0-git"         [1, 0, -4]
    !>    "1.0.7.5"         [1, 0, 7, 5]
    !>    "1.0.cvs"         [1, 0, -4]
    !>    "1.0PRE2"         [1, 0, -1, 2]
    !>    "1.0pre2"         [1, 0, -1, 2]
    !>    "22.8 Beta3"      [22, 8, -2, 3]
    !>    "22.8beta3"       [22, 8, -2, 3]
    !>
    !> Invalid syntax is an `error' error condition in the result's STATUS
    !> component, which makes every comparison false except `/='.
    !> Port of subr.el:version-to-list (with version-separator and
    !> version-regexp-alist).
    type(version_type) function version(text) result(v)
        character(len=*), intent(in) :: text
        ! version-regexp-alist, in its order.
        character(len=*), parameter :: separator_regular_expressions(7) = [character(len=48) :: &
                                                                           "^[-._+ ]?snapshot$", "^[-._+]$", &
                                                               "^[-._+ ]?\(cvs\|git\|bzr\|svn\|hg\|darcs\)$", "^[-._+ ]?unknown$", &
                                                                       "^[-._+ ]?alpha$", "^[-._+ ]?beta$", "^[-._+ ]?\(pre\|rc\)$"]
        integer(int64), parameter :: priorities(7) = [-4, -4, -4, -4, -3, -2, -1]
        character(len=*), parameter :: DIGITS = "0123456789"
        character(len=:), allocatable :: normalized, s, letter
        integer(int64), allocatable :: saved(:)
        integer(int64) :: number
        integer(int32) :: c, clen
        integer :: i, e, k
        logical :: saved_fold, matched

        allocate (v%numbers(0))
        normalized = text
        ! Change .x.y to 0.x.y
        if (index(normalized, ".") == 1) normalized = "0"//normalized

        if (.not. string_matches("^[0-9]", normalized, v%status)) then
            call set_error(v%status, GENERIC_ERROR_CODE, "Invalid version syntax: `"//text//"' (must start with a number)")
            return
        end if

        ! save-match-data, and ignore case in matching.
        saved = match_data()
        saved_fold = case_fold_search%value
        case_fold_search = .true.
        i = 1

        ! Parse the version string up to a separator until there are none
        ! left.
        do while (i <= len(normalized))
            e = verify(normalized(i:), DIGITS) - 1
            if (e == -1) e = len(normalized) - i + 1
            if (e == 0) exit
            call string_to_number(normalized(i:i + e - 1), number)
            v%numbers = [v%numbers, number]
            i = i + e
            if (i > len(normalized)) exit
            ! The non-numeric part.
            e = scan(normalized(i:), DIGITS) - 1
            if (e == -1) e = len(normalized) - i + 1
            s = normalized(i:i + e - 1)
            i = i + e
            ! The alpha, beta, pre, etc. separator.
            if (s == ".") cycle

            do k = 1, size(separator_regular_expressions)
                if (string_matches(trim(separator_regular_expressions(k)), s, v%status)) exit
            end do

            if (k <= size(separator_regular_expressions)) then
                v%numbers = [v%numbers, priorities(k)]
                cycle
            end if

            ! Convert 22.3a to 22.3.1, 22.3b to 22.3.2, etc., but only if
            ! the letter is the end of the version string, to avoid 22.8X3
            ! being valid.
            matched = .false.

            if (i > len(normalized)) then
                if (string_match("^[-._+ ]?\([a-zA-Z]\)$", s, v%status) > 0) then
                    letter = match_string(1, s, matched)
                end if
            end if

            if (.not. matched) then
                call set_error(v%status, GENERIC_ERROR_CODE, "Invalid version syntax: `"//text//"'")
                exit
            end if

            call string_character_and_length(letter, 1_int64, c, clen)
            v%numbers = [v%numbers, int(downcase(c) - iachar("a") + 1, int64)]
        end do

        case_fold_search = saved_fold
        call set_match_data(saved)
    end function version

    !> Return true if version V1 is lower (older) than V2.
    !>
    !> Note that version "1" is equal to "1.0", "1.0.0", "1.0.0.0", etc.
    !> That is, the trailing ".0"s are insignificant.  Also, version "1"
    !> is higher (newer) than "1pre", which is higher than "1beta", which
    !> is higher than "1alpha", which is higher than "1snapshot".  Also,
    !> "-GIT", "-CVS" and "-NNN" are treated as snapshot versions.  False
    !> when either has invalid version syntax.  The operator `<' between
    !> two `version_type' values.  Port of subr.el:version< and
    !> version-list-<.
    elemental logical function version_less(v1, v2) result(less)
        type(version_type), intent(in) :: v1, v2

        less = .false.
        if (v1%status%ok() .and. v2%status%ok()) less = version_order(v1%numbers, v2%numbers) < 0
    end function version_less

    !> Return true if version V1 is lower (older) than or equal to V2
    !> (see `version_less').  False when either has invalid version
    !> syntax.  The operator `<=' between two `version_type' values.
    !> Port of subr.el:version<= and version-list-<=.
    elemental logical function version_less_or_equal(v1, v2) result(less)
        type(version_type), intent(in) :: v1, v2

        less = .false.
        if (v1%status%ok() .and. v2%status%ok()) less = version_order(v1%numbers, v2%numbers) <= 0
    end function version_less_or_equal

    !> Return true if version V1 is higher (newer) than V2 (see
    !> `version_less').  False when either has invalid version syntax.
    !> The operator `>' between two `version_type' values.  Port of
    !> subr.el:version< (with its arguments swapped).
    elemental logical function version_greater(v1, v2) result(greater)
        type(version_type), intent(in) :: v1, v2

        greater = version_less(v2, v1)
    end function version_greater

    !> Return true if version V1 is higher (newer) than or equal to V2
    !> (see `version_less').  False when either has invalid version
    !> syntax.  The operator `>=' between two `version_type' values.
    !> Port of subr.el:version<= (with its arguments swapped).
    elemental logical function version_greater_or_equal(v1, v2) result(greater)
        type(version_type), intent(in) :: v1, v2

        greater = version_less_or_equal(v2, v1)
    end function version_greater_or_equal

    !> Return true if version V1 is equal to V2.
    !>
    !> Note that version "1" is equal to "1.0", "1.0.0", "1.0.0.0", etc.
    !> That is, the trailing ".0"s are insignificant.  Also, version "1"
    !> is higher (newer) than "1pre", which is higher than "1beta", which
    !> is higher than "1alpha", which is higher than "1snapshot".  Also,
    !> "-GIT", "-CVS" and "-NNN" are treated as snapshot versions.  False
    !> when either has invalid version syntax.  The operator `==' between
    !> two `version_type' values.  Port of subr.el:version= and
    !> version-list-=.
    elemental logical function version_equal(v1, v2) result(equal)
        type(version_type), intent(in) :: v1, v2

        equal = .false.
        if (v1%status%ok() .and. v2%status%ok()) equal = version_order(v1%numbers, v2%numbers) == 0
    end function version_equal

    !> Return true unless version V1 is equal to V2 (`version_equal'):
    !> true when either has invalid version syntax, as `/=' is for an
    !> IEEE NaN.  The operator `/=' between two `version_type' values.
    !> Port of subr.el:version= (negated).
    elemental logical function version_not_equal(v1, v2) result(not_equal)
        type(version_type), intent(in) :: v1, v2

        not_equal = .not. version_equal(v1, v2)
    end function version_not_equal

    !> -1, 0 or 1 as version array L1 is lower than, equal to or higher
    !> than L2, past their common part comparing the rest of the longer
    !> one's first non-zero element (version-list-not-zero) to 0.  Port of
    !> subr.el:version-list-<, version-list-<=, version-list-= and
    !> version-list-not-zero (their common comparison).
    pure integer function version_order(l1, l2) result(order)
        integer(int64), intent(in) :: l1(:), l2(:)
        integer :: k, n

        n = min(size(l1), size(l2))
        k = findloc(l1(1:n) /= l2(1:n), .true., dim=1)
        order = 0

        if (k > 0) then
            order = merge(-1, 1, l1(k) < l2(k))
        else if (size(l1) > n) then
            k = findloc(l1(n + 1:) /= 0, .true., dim=1)
            if (k > 0) order = merge(-1, 1, l1(n + k) < 0)
        else if (size(l2) > n) then
            k = findloc(l2(n + 1:) /= 0, .true., dim=1)
            if (k > 0) order = merge(1, -1, l2(n + k) < 0)
        end if
    end function version_order

    !> Same as `looking_at' except this function does not change the match
    !> data.  Port of subr.el:looking-at-p.
    logical function is_looking_at(regular_expression, status)
        character(len=*), intent(in) :: regular_expression
        type(status_type), intent(out) :: status

        is_looking_at = looking_at(regular_expression, status, inhibit_modify=.true.)
    end function is_looking_at

    !> Return true if text before point matches regular expression
    !> REGULAR_EXPRESSION.  Like `looking_at' except matches before point, and is
    !> slower.  LIMIT if true speeds up the search by specifying a
    !> minimum starting position, to avoid checking matches that would
    !> start before LIMIT.
    !>
    !> If GREEDY is true, extend the match backwards as far as possible,
    !> stopping when a single additional previous character cannot be part
    !> of a match for REGULAR_EXPRESSION.  When the match is extended, its starting
    !> position is allowed to occur before LIMIT.
    !>
    !> As a general recommendation, try to avoid using `looking_back'
    !> wherever possible, since it is slow.  Port of subr.el:looking-back.
    logical function looking_back(regular_expression, status, limit, greedy) result(found)
        character(len=*), intent(in) :: regular_expression
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: limit
        logical, intent(in), optional :: greedy
        integer(int64) :: start, position, saved_beginning_of_accessible_portion, saved_end_of_accessible_portion
        logical :: ignored

        start = point()
        call regular_expression_search_backward("\(?:"//regular_expression//"\)\=", position, status, bound=limit, &
                                                noerror=SEARCH_NOERROR)
        found = status%ok() .and. position > 0
        if (found) position = point()
        call goto_character(start)
        if (.not. found) return
        if (.not. present(greedy)) return
        if (.not. greedy) return
        saved_beginning_of_accessible_portion = point_minimum()
        saved_end_of_accessible_portion = point_maximum()
        call narrow_to_region(point_minimum(), start, status)

        do while (position > point_minimum())
            call goto_character(position - 1)
            if (.not. looking_at("\(?:"//regular_expression//"\)\'", status)) exit
            position = position - 1
        end do

        call goto_character(position)
        ignored = looking_at("\(?:"//regular_expression//"\)\'", status)
        call widen()
        call narrow_to_region(saved_beginning_of_accessible_portion, saved_end_of_accessible_portion, status)
        call goto_character(start)
    end function looking_back

    !> Return progress reporter object for use with `progress_reporter_update'.
    !>
    !> MESSAGE is shown in the echo area, with a status indicator
    !> appended to the end.  When you call `progress_reporter_done', the
    !> word "done" is printed after the MESSAGE.
    !>
    !> MINIMUM_VALUE and MAXIMUM_VALUE are starting (0% complete) and final (100%
    !> complete) states of operation; the latter should be larger.  The
    !> status message shows the percentage progress.  The minimal change
    !> to report is 1%, the minimum interval between echo area updates
    !> 0.2 seconds.  Without MINIMUM_VALUE and MAXIMUM_VALUE, the reporter is a
    !> pulsing one (its indicator turns).  Port of
    !> subr.el:make-progress-reporter (default CURRENT-VALUE, MIN-CHANGE
    !> and MIN-TIME).
    subroutine make_progress_reporter(reporter, message_text, minimum_value, maximum_value)
        type(progress_reporter_type), intent(out) :: reporter
        character(len=*), intent(in) :: message_text
        integer(int64), intent(in), optional :: minimum_value, maximum_value
        type(status_type) :: st

        reporter%text = message_text
        if (string_match("[[:alnum:]]\'", message_text, st) > 0) reporter%text = message_text//"..."
        reporter%update_time = real(current_time())

        if (.not. (present(minimum_value) .and. present(maximum_value))) then
            ! A pulsing indicator.
            reporter%pulsing = .true.
            call progress_reporter_pulse(reporter)
            return
        end if

        reporter%next_value = real(minimum_value, real64)
        reporter%update_time = real(current_time())
        reporter%minimum_value = minimum_value
        reporter%maximum_value = maximum_value
        ! Force a call to `message' now.
        call progress_reporter_update(reporter, minimum_value)
    end subroutine make_progress_reporter

    !> Report progress of an operation in the echo area.
    !> VALUE should be a number between MINIMUM_VALUE and MAXIMUM_VALUE.
    !>
    !> This function is relatively inexpensive.  If the change since
    !> last update is too small or insufficient time has passed, it does
    !> nothing.  Port of subr.el:progress-reporter-update (with
    !> progress-reporter-do-update and progress-reporter-echo-area).
    subroutine progress_reporter_update(reporter, value)
        type(progress_reporter_type), intent(inout) :: reporter
        integer(int64), intent(in) :: value
        real(real64) :: one_percent, now
        integer(int64) :: percentage
        logical :: enough_time_passed
        character(len=24) :: buffer

        if (real(value, real64) < reporter%next_value) return
        ! See if enough time has passed since the last update.
        now = real(current_time())
        enough_time_passed = reporter%update_time < now
        ! Calculate time for the next update.
        if (enough_time_passed) reporter%update_time = reporter%update_time + reporter%minimum_time
        one_percent = real(reporter%maximum_value - reporter%minimum_value, real64)/100.0_real64

        if (reporter%maximum_value == reporter%minimum_value) then
            percentage = 0
        else
            percentage = int((value - reporter%minimum_value)/one_percent, int64)
        end if

        ! If we are not printing message because not enough time has
        ! passed, use 1 instead of MIN-CHANGE.
        if (enough_time_passed) then
            reporter%next_value = reporter%minimum_value + (percentage + reporter%minimum_change)*one_percent
        else
            reporter%next_value = reporter%minimum_value + (percentage + 1)*one_percent
        end if

        reporter%next_value = min(reporter%next_value, real(reporter%maximum_value, real64))
        reporter%next_value = real(ceiling(reporter%next_value, int64), real64)
        ! Print message only if enough time has passed.
        if (.not. enough_time_passed) return

        if (percentage > 0) then
            write (buffer, "(i0)") percentage
            call message(reporter%text//trim(buffer)//"%")
        else
            call message(reporter%text)
        end if
    end subroutine progress_reporter_update

    !> Print reporter's message followed by word "done" in echo area.
    !> Port of subr.el:progress-reporter-done.
    subroutine progress_reporter_done(reporter)
        type(progress_reporter_type), intent(in) :: reporter

        call message(reporter%text//"done")
    end subroutine progress_reporter_done

    !> The update of a pulsing reporter: turn its indicator when enough
    !> time has passed.  Port of the pulsing branch of
    !> subr.el:progress-reporter-do-update (with
    !> progress-reporter-echo-area).
    subroutine progress_reporter_pulse(reporter)
        type(progress_reporter_type), intent(inout) :: reporter
        character(len=4), parameter :: pulse_characters = "-\|/"
        integer :: k

        if (.not. (reporter%update_time < real(current_time()))) return
        reporter%update_time = reporter%update_time + reporter%minimum_time
        reporter%pulse_index = reporter%pulse_index + 1
        k = mod(reporter%pulse_index, 4) + 1
        call message(reporter%text//" "//pulse_characters(k:k))
    end subroutine progress_reporter_pulse

    !> Move point to the next position that is the end of an identifier.
    !> An identifier is any sequence of characters that are in either the
    !> word constituent or identifier constituent syntax class.
    !> With prefix argument ARGUMENT, do it ARGUMENT times if positive, or move
    !> backwards ARGUMENT times if negative.  Port of subr.el:forward-symbol.
    subroutine forward_identifier(argument)
        integer(int64), intent(in) :: argument
        integer(int64) :: found, moved, n
        type(status_type) :: st

        if (argument >= 0) then
            call regular_expression_search_forward("\(\sw\|\s_\)+", found, st, noerror=SEARCH_MOVE, count=argument)
        else
            n = argument

            do while (n < 0)
                call regular_expression_search_backward("\(\sw\|\s_\)+", found, st, noerror=SEARCH_MOVE)
                if (found > 0) moved = skip_syntax_backward("w_")
                n = n + 1
            end do
        end if
    end subroutine forward_identifier

    !> The bounds of the identifier at point (`found' false if none).
    !> Port of thingatpt.el:bounds-of-thing-at-point for `symbol' (end-op
    !> forward-symbol, beginning-op thing-at-point--beginning-of-symbol,
    !> whose search failing is an error ignored).
    subroutine bounds_of_identifier_at_point(beginning, end, found)
        integer(int64), intent(out) :: beginning, end
        logical, intent(out) :: found
        integer(int64) :: original, at, real_end, real_beginning
        type(status_type) :: st
        logical :: ok

        found = .false.
        beginning = 0
        end = 0
        original = point()
        ! If the buffer is totally empty, give up.
        call goto_character(point_minimum())
        call regular_expression_search_forward("[^"//achar(9)//achar(10)//" ]", at, st, noerror=SEARCH_NOERROR)

        if (at == 0) then
            call goto_character(original)
            return
        end if

        call goto_character(original)
        ! Try moving forward, then back.
        call forward_identifier(1_int64)
        call beginning_of_identifier(ok)

        if (.not. ok) then
            call goto_character(original)
            return
        end if

        beginning = point()

        if (beginning <= original) then
            ! If that brings us all the way back to ORIGINAL, it worked.  But
            ! END may not be the real end.  So find the real end that
            ! corresponds to BEGINNING.
            call forward_identifier(1_int64)
            real_end = point()

            if (original <= real_end .and. beginning < real_end) then
                end = real_end
                found = .true.
            end if
        else
            call goto_character(original)
            ! Try a second time, moving backward first and then forward,
            ! so that we can find a thing that ends at ORIGINAL.
            call beginning_of_identifier(ok)

            if (ok) then
                call forward_identifier(1_int64)
                end = point()
                call beginning_of_identifier(ok)

                if (ok) then
                    real_beginning = point()

                    if (real_beginning <= original .and. original <= end .and. real_beginning < end) then
                        beginning = real_beginning
                        found = .true.
                    end if
                end if
            end if
        end if

        call goto_character(original)

    contains

        ! thing-at-point--beginning-of-symbol: `ok' false when the search
        ! signals.
        subroutine beginning_of_identifier(ok)
            logical, intent(out) :: ok
            integer(int64) :: found, moved
            type(status_type) :: st

            call regular_expression_search_backward("\(\sw\|\s_\)+", found, st)
            ok = st%ok() .and. found > 0
            if (ok) moved = skip_syntax_backward("w_")
        end subroutine beginning_of_identifier
    end subroutine bounds_of_identifier_at_point

    !> Determine default tag to search for, based on text at point.
    !> If there is no plausible default, `found' is false.  Port of
    !> subr.el:find-tag-default (with find-tag-default-bounds).
    subroutine find_tag_default(tag, found)
        character(len=:), allocatable, intent(out) :: tag
        logical, intent(out) :: found
        integer(int64) :: beginning, end
        type(status_type) :: st

        tag = ""
        call bounds_of_identifier_at_point(beginning, end, found)
        if (found) call buffer_substring(beginning, end, tag, st)
    end subroutine find_tag_default

    !> Return regexp that matches the default tag at point.
    !> If there is no tag at point, `found' is false.  Port of
    !> subr.el:find-tag-default-as-regexp (find-tag-default-function is
    !> not ported: the default is `find_tag_default').
    subroutine find_tag_default_as_regular_expression(regular_expression, found)
        character(len=:), allocatable, intent(out) :: regular_expression
        logical, intent(out) :: found
        character(len=:), allocatable :: tag

        call find_tag_default(tag, found)
        regular_expression = ""
        if (found) regular_expression = regular_expression_quote(tag)
    end subroutine find_tag_default_as_regular_expression

    !> Return regexp that matches the default tag at point as identifier.
    !> If there is no tag at point, `found' is false.  Port of
    !> subr.el:find-tag-default-as-symbol-regexp.
    subroutine find_tag_default_as_identifier_regular_expression(regular_expression, found)
        character(len=:), allocatable, intent(out) :: regular_expression
        logical, intent(out) :: found

        call find_tag_default_as_regular_expression(regular_expression, found)
        if (found) regular_expression = "\_<"//regular_expression//"\_>"
    end subroutine find_tag_default_as_identifier_regular_expression

    ! ------------------------------------------------------------------
    ! Overlays and strings
    ! ------------------------------------------------------------------

    !> Return a copy of overlay OVERLAY in COPY.
    !> The copy will be deleted if OVERLAY is.  Port of
    !> subr.el:copy-overlay (the properties are copied at once).
    subroutine copy_overlay(overlay, copy, status)
        use interval_tree_mod, only: make_overlay, delete_overlay, overlay_buffer, overlay_start, overlay_end, &
                                     overlay_properties, overlay_put
        type(handle_type), intent(in) :: overlay
        type(handle_type), intent(out) :: copy
        type(status_type), intent(out) :: status
        type(handle_type) :: buffer
        type(value_type), allocatable :: properties(:)
        integer :: k

        buffer = overlay_buffer(overlay)

        if (buffer%index /= 0) then
            call make_overlay(copy, overlay_start(overlay), overlay_end(overlay), status, buffer=buffer)
        else
            call make_overlay(copy, point_minimum(), point_minimum(), status)
            if (status%ok()) call delete_overlay(copy)
        end if

        if (.not. status%ok()) return
        call list_elements(overlay_properties(overlay), properties)

        do k = 1, size(properties) - 1, 2
            call overlay_put(copy, symbols(xsymbol(properties(k)))%name, properties(k + 1), status)
            if (.not. status%ok()) return
        end do
    end subroutine copy_overlay

    !> Clear BEGINNING and END of overlays whose property NAME has value
    !> VALUE.  Overlays might be moved and/or split.  If any targeted
    !> overlays end up being empty, they will be deleted.  Port of
    !> subr.el:remove-overlays (BEGINNING and END given).
    subroutine remove_overlays(beginning, end, name, value, status)
        use interval_tree_mod, only: overlays_in, overlay_get, overlay_start, overlay_end, move_overlay, delete_overlay
        integer(int64), intent(in) :: beginning, end
        character(len=*), intent(in) :: name
        type(value_type), intent(in) :: value
        type(status_type), intent(out) :: status
        type(handle_type), allocatable :: found(:)
        type(handle_type) :: copy
        integer(int64) :: lo, hi
        integer :: k

        status = status_type()
        lo = min(beginning, end)
        hi = max(beginning, end)
        call overlays_in(lo, hi, found)

        do k = 1, size(found)
            if (.not. value_eq(overlay_get(found(k), name), value)) cycle

            ! Either push this overlay outside beginning...end or split it
            ! to exclude beginning...end or delete it entirely (if it is
            ! contained in beginning...end).
            if (overlay_start(found(k)) < lo) then
                if (overlay_end(found(k)) > hi) then
                    call copy_overlay(found(k), copy, status)
                    if (status%ok()) call move_overlay(copy, overlay_start(found(k)), lo, status)
                    if (status%ok()) call move_overlay(found(k), hi, overlay_end(found(k)), status)
                else
                    call move_overlay(found(k), overlay_start(found(k)), lo, status)
                end if
            else if (overlay_end(found(k)) > hi) then
                call move_overlay(found(k), hi, overlay_end(found(k)), status)
            else
                call delete_overlay(found(k))
            end if

            if (.not. status%ok()) return
        end do
    end subroutine remove_overlays

    !> Split STRING into substrings bounded by matches for SEPARATORS, a
    !> regexp, in PARTS.
    !>
    !> The beginning and end of STRING, and each match for SEPARATORS, are
    !> splitting points.  The substrings matching SEPARATORS are removed,
    !> and the substrings between the splitting points are collected as a
    !> list, which is returned.
    !>
    !> If OMIT_NULLS is true, zero-length substrings are omitted from the
    !> list; otherwise they are kept.  Port of subr.el:split-string
    !> (SEPARATORS given; no TRIM).
    subroutine split_string(string, separators, parts, status, omit_nulls)
        use utility_functions_mod, only: substring, count_characters
        use string_type_mod, only: string_type, append_string
        character(len=*), intent(in) :: string, separators
        type(string_type), allocatable, intent(out) :: parts(:)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: omit_nulls
        integer(int64) :: start, this_start, this_end, length, from, found, previous_beginning
        logical :: keep_nulls, notfirst
        type(status_type) :: st

        allocate (parts(0))
        keep_nulls = .true.
        if (present(omit_nulls)) keep_nulls = .not. omit_nulls
        length = count_characters(string)
        start = 0
        notfirst = .false.
        previous_beginning = -1

        do
            from = start

            if (notfirst .and. start < length) then
                if (start == previous_beginning) from = start + 1
            end if

            found = string_match(separators, string, status, start=from + 1)
            if (.not. status%ok()) return
            if (found == 0 .or. start >= length) exit
            notfirst = .true.
            this_start = start
            this_end = match_beginning(0, st) - 1
            previous_beginning = this_end
            start = match_end(0, st) - 1
            call push_one()
        end do

        ! Handle the substring at the end of STRING.
        this_start = start
        this_end = length
        call push_one()

    contains

        subroutine push_one()
            if (keep_nulls .or. this_start < this_end) call append_string(parts, substring(string, this_start, this_end))
        end subroutine push_one
    end subroutine split_string
end module basic_subprograms_mod
