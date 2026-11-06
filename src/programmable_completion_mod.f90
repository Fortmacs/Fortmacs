! programmable_completion_mod.f90 -- programmable, polymorphic completion engine.
! Ported from GNU Emacs 31.1 lisp/pcomplete.el.
!
! Copyright (C) 1999-2026 Free Software Foundation, Inc.
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

!> Programmable completion: `programmable_completion_completions_at_point' completes
!> the argument at point with the completion function of the command:
!> a function registered with `programmable_completion_command_functions' for the
!> command's name (pcomplete/NAME upstream), else
!> `programmable_completion_default_completion_function'; the command name itself is
!> completed by `programmable_completion_command_completion_function'.
!>
!> A completion function walks the arguments with `programmable_completion_here': it
!> returns true and moves to the next argument while the final argument
!> is not reached, and at the final argument gives the completion table
!> of its FORM and returns false (upstream's return to
!> `pcomplete-completions').  A completion function returns as soon as
!> `programmable_completion_here' returns false or `programmable_completion_thrown' is true (no
!> completions: upstream's return to `pcompleted').
!>
!> Deferred: pcomplete's own UI (`programmable_completion', `programmable_completion_reverse',
!> `programmable_completion_expand_and_complete', `programmable_completion_continue',
!> `programmable_completion_expand', `programmable_completion_help', `programmable_completion_list', cycling,
!> `programmable_completion_show_completions', `programmable_completion_insert_entry',
!> `programmable_completion_do_complete', `programmable_completion_stub') and so
!> `programmable_completion_command_interpreter_setup' and `programmable_completion_shell_setup', which install
!> it; `programmable_completion_option', `programmable_completion_from_help',
!> `programmable_completion_here_using_help', `programmable_completion_uniquify_list',
!> `programmable_completion_process_result' and host name completion, used by the
!> pcmpl-*.el command completions, which are not ported; argument lists
!> computed by the parse function (Eshell's globs); the
!> `programmable_completion_annotation' and `programmable_completion_help' properties of
!> candidates; `programmable_completion_compare_entry_function' (cycling is not
!> ported); completion-table-case-fold.
module programmable_completion_mod
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use error_mod, only: status_type
    use value_mod, only: value_type, make_list, list, list_elements, list_length, list_element, xstring, xfixnum, vtag, &
                         V_INTEGER
    use data_mod, only: string_setting_type, logical_setting_type, forward_setting, append_locals_clearer
    use buffer_mod, only: buffers, current
    use editing_functions_mod, only: point, point_maximum, goto_character, buffer_substring, delete_region, is_end_of_line, &
                                     character_after
    use insert_delete_mod, only: insert
    use syntax_mod, only: skip_characters_forward
    use search_mod, only: string_match, match_beginning, match_end, looking_at, regular_expression_quote
    use basic_subprograms_mod, only: match_string
    use utility_functions_mod, only: substring, count_characters
    use environment_mod, only: getenv
    use file_input_output_mod, only: file_exists, is_file_directory, is_file_executable, directory_file_name, &
                                     file_name_nondirectory
    use files_mod, only: file_truename
    use hooks_mod, only: hook_type
    use display_engine_mod, only: message
    use minibuffer_reading_tables_mod, only: completion_table_type, completion_test_procedure, completion_function_table, &
                                             completion_table_with_quoting, completion_at_point_function_data_type, &
                                             completion_unquote_procedure, completion_requote_procedure, COMPLETE_ALL, &
                                             COMPLETE_BOUNDARIES, COMPLETION_FINISHED
    use minibuffer_reading_mod, only: complete_with_action, completion_table_subvert, completion_table_with_test
    use directory_editor_mod, only: completion_ignored_extensions
    use command_interpreter_mod, only: command_interpreter_completion_file_name_table, command_interpreter_unquote_argument, &
                                       command_interpreter_requote_argument, command_interpreter_beginning_of_line
    use string_type_mod, only: string_type, prepend_string, append_string
    use buffer_local_setting_mod, only: buffer_local_setting_type
    implicit none(type, external)
    private

    public :: programmable_completion_here, programmable_completion_here_star, programmable_completion_thrown, &
              programmable_completion_argument, programmable_completion_actual_argument, &
              programmable_completion_begin, programmable_completion_next_argument, programmable_completion_command_name, &
              programmable_completion_match, programmable_completion_match_string, &
              programmable_completion_test, programmable_completion_parse_buffer_arguments, &
              programmable_completion_parse_command_interpreter_arguments, &
              programmable_completion_parse_arguments, programmable_completion_entries, programmable_completion_all_entries, &
              programmable_completion_directories, &
              programmable_completion_executables, programmable_completion_directories_or_entries, &
              programmable_completion_find_completion_function, &
              programmable_completion_completions, programmable_completion_completions_at_point, &
              programmable_completion_default_exit_function, &
              programmable_completion_ignore, initialize_programmable_completion
    public :: programmable_completion_function_procedure, programmable_completion_form_procedure, &
              programmable_completion_parse_procedure, programmable_completion_name_procedure, &
              programmable_completion_norm_procedure, programmable_completion_file_test_procedure

    abstract interface
        !> A completion function: pcomplete/NAME, and
        !> `programmable_completion_command_completion_function' and
        !> `programmable_completion_default_completion_function'.
        recursive subroutine programmable_completion_function_procedure(status)
            import :: status_type
            implicit none(type, external)
            type(status_type), intent(out) :: status
        end subroutine programmable_completion_function_procedure

        !> The FORM of `programmable_completion_here': the completion TABLE for the
        !> final argument (FOUND .false. for none).
        recursive subroutine programmable_completion_form_procedure(table, found)
            import :: completion_table_type
            implicit none(type, external)
            type(completion_table_type), intent(out) :: table
            logical, intent(out) :: found
        end subroutine programmable_completion_form_procedure

        !> A `programmable_completion_parse_arguments_function': the arguments ARGUMENTS
        !> (their final values) and the positions BEGINS where they
        !> begin, unallocated ARGUMENTS for none.
        recursive subroutine programmable_completion_parse_procedure(arguments, begins)
            import :: string_type, int64
            implicit none(type, external)
            type(string_type), allocatable, intent(out) :: arguments(:)
            integer(int64), allocatable, intent(out) :: begins(:)
        end subroutine programmable_completion_parse_procedure

        !> A `programmable_completion_command_name_function': the current command NAME.
        recursive subroutine programmable_completion_name_procedure(name)
            implicit none(type, external)
            character(len=:), allocatable, intent(out) :: name
        end subroutine programmable_completion_name_procedure

        !> A paring function of `programmable_completion_here': ARGUMENT normalized as NORM.
        recursive subroutine programmable_completion_norm_procedure(argument, norm)
            implicit none(type, external)
            character(len=*), intent(in) :: argument
            character(len=:), allocatable, intent(out) :: norm
        end subroutine programmable_completion_norm_procedure

        !> A test on file names (`programmable_completion_entries').
        recursive logical function programmable_completion_file_test_procedure(file)
            implicit none(type, external)
            character(len=*), intent(in) :: file
        end function programmable_completion_file_test_procedure
    end interface

    !> Values of PARING for `programmable_completion_here': the argument is pared
    !> against previous arguments normalized with `file_truename' or the
    !> PARING_FUNCTION (PROGRAMMABLE_COMPLETION_PARING_DEFAULT); it does not participate in paring
    !> (PROGRAMMABLE_COMPLETION_PARING_NONE); all previous arguments seen are cleared (PROGRAMMABLE_COMPLETION_PARING_CLEAR).
    integer, parameter, public :: PROGRAMMABLE_COMPLETION_PARING_DEFAULT = 0, PROGRAMMABLE_COMPLETION_PARING_NONE = 1, &
                                  PROGRAMMABLE_COMPLETION_PARING_CLEAR = 2

    !> Values of WHICH for `programmable_completion_argument' and `programmable_completion_begin': an
    !> index relative to the current argument, or the special indices
    !> `first' and `last'.
    integer, parameter, public :: PROGRAMMABLE_COMPLETION_ARGUMENT_RELATIVE = 0, PROGRAMMABLE_COMPLETION_ARGUMENT_FIRST = 1, &
                                  PROGRAMMABLE_COMPLETION_ARGUMENT_LAST = 2

    ! What was thrown: nothing, `pcomplete-completions' or `pcompleted'.
    integer, parameter :: THROWN_NONE = 0, THROWN_COMPLETIONS = 1, THROWN_PCOMPLETED = 2

    !> A regexp of filenames to be disregarded during file completion
    !> (unallocated when unset).  Port of pcomplete.el pcomplete-file-ignore.
    type(string_setting_type), target, public :: programmable_completion_file_ignore = string_setting_type( &
                                                 name="programmable_completion_file_ignore")

    !> A regexp of names to be disregarded during directory completion
    !> (unallocated when unset).  Port of pcomplete.el pcomplete-dir-ignore.
    type(string_setting_type), target, public :: programmable_completion_directory_ignore = string_setting_type( &
                                                 name="programmable_completion_directory_ignore")

    !> Whether to ignore remote file names.  Port of pcomplete.el
    !> pcomplete-remote-file-ignore (remote files are not ported).
    type(logical_setting_type), target, public :: programmable_completion_remote_file_ignore = &
                                                  logical_setting_type(name="programmable_completion_remote_file_ignore", &
                                                                       value=.false.)

    !> If true, automatically list possibilities on partial completion.
    !> This mirrors the optional behavior of tcsh.  Port of pcomplete.el
    !> pcomplete-autolist.
    type(logical_setting_type), target, public :: programmable_completion_autolist = &
                                                  logical_setting_type(name="programmable_completion_autolist", value=.false.)

    !> If true, use shortest completion if characters cannot be added.
    !> This mirrors the optional behavior of tcsh.
    !>
    !> A true value is useful if `programmable_completion_autolist' is true too.  Port
    !> of pcomplete.el pcomplete-recexact.
    type(logical_setting_type), target, public :: programmable_completion_recexact = &
                                                  logical_setting_type(name="programmable_completion_recexact", value=.false.)

    !> If true, expand the current argument before completing it.
    !> This means that typing something such as `$HOME/bi' followed by
    !> completion will cause the variable reference to be resolved
    !> first, and the resultant value that will be completed against to
    !> be inserted in the buffer.  Note that exactly what gets expanded
    !> and how is entirely up to the behavior of the
    !> `programmable_completion_parse_arguments_function'.  Port of pcomplete.el
    !> pcomplete-expand-before-complete.
    type(logical_setting_type), target, public :: programmable_completion_expand_before_complete = &
                                                  logical_setting_type(name="programmable_completion_expand_before_complete", &
                                                                       value=.false.)

    !> If true, allow effects in `programmable_completion_parse_arguments_function'.
    !> For `completion_at_point_functions' this is false, and it tells
    !> `programmable_completion_parse_arguments_function' whether it can do
    !> modifications to the buffer, like expanding variables and such,
    !> or whether it should refrain from doing so.  Port of pcomplete.el
    !> pcomplete-allow-modifications.
    logical, public :: programmable_completion_allow_modifications = .false.

    !> If true, pare alternatives that have already been used.
    !> If false, you will always see the completion set of possible
    !> options, no matter which of those options have already been used
    !> in previous command arguments.  Port of pcomplete.el
    !> pcomplete-use-paring.
    type(logical_setting_type), target, public :: programmable_completion_use_paring = &
                                                  logical_setting_type(name="programmable_completion_use_paring", value=.true.)

    !> A string that is inserted after any completion or expansion.
    !> This is usually a space character, useful when completing lists of
    !> words separated by spaces.  However, if your list uses a different
    !> separator character, or if the completion occurs in a word that is
    !> already terminated by a character, this variable should be locally
    !> modified to be an empty string, or the desired separation string.
    !> Port of pcomplete.el pcomplete-termination-string.
    type(string_setting_type), target, public :: programmable_completion_termination_string = &
                                                 string_setting_type(name="programmable_completion_termination_string")

    !> The name of the /etc/hosts file (unallocated for none).  Port of
    !> pcomplete.el pcomplete-hosts-file.
    type(string_setting_type), target, public :: programmable_completion_hosts_file = string_setting_type( &
                                                 name="programmable_completion_hosts_file")

    !> A list of functions which are called before completing an
    !> argument.  This can be used, for example, for completing things
    !> which might apply to all arguments, such as variable names after
    !> a $.  Port of pcomplete.el pcomplete-try-first-hook.
    type(hook_type), target, public :: programmable_completion_try_first_hook = hook_type( &
                                       name="programmable_completion_try_first_hook")

    !> One buffer's own value of
    !> `programmable_completion_command_completion_function' or `programmable_completion_default_completion_function'.
    type :: programmable_completion_function_entry_type
        procedure(programmable_completion_function_procedure), pointer, nopass :: procedure => null()
    end type programmable_completion_function_entry_type

    !> The type of
    !> `programmable_completion_command_completion_function' or `programmable_completion_default_completion_function':
    !> the global value `procedure' (set with `=>'; null for none) and
    !> the buffers' own values (`set_local').
    type, extends(buffer_local_setting_type), public :: programmable_completion_function_setting_type
        procedure(programmable_completion_function_procedure), pointer, nopass :: procedure => null()
        type(programmable_completion_function_entry_type), allocatable :: locals(:)
    contains
        !> Give the current buffer its own value (null when absent).
        procedure :: set_local => set_programmable_completion_function_local
        !> The current buffer's value: its own, else the global one.
        procedure :: get_current => get_programmable_completion_function_current
        procedure :: append_local_entry => append_programmable_completion_function_entry
        procedure :: clear_local_entry => clear_programmable_completion_function_entry
    end type programmable_completion_function_setting_type

    !> One buffer's own value of
    !> `programmable_completion_parse_arguments_function'.
    type :: programmable_completion_parse_entry_type
        procedure(programmable_completion_parse_procedure), pointer, nopass :: procedure => null()
    end type programmable_completion_parse_entry_type

    !> The type of
    !> `programmable_completion_parse_arguments_function':
    !> the global value `procedure' (set with `=>'; null for none) and
    !> the buffers' own values (`set_local').
    type, extends(buffer_local_setting_type), public :: programmable_completion_parse_setting_type
        procedure(programmable_completion_parse_procedure), pointer, nopass :: procedure => null()
        type(programmable_completion_parse_entry_type), allocatable :: locals(:)
    contains
        !> Give the current buffer its own value (null when absent).
        procedure :: set_local => set_programmable_completion_parse_local
        !> The current buffer's value: its own, else the global one.
        procedure :: get_current => get_programmable_completion_parse_current
        procedure :: append_local_entry => append_programmable_completion_parse_entry
        procedure :: clear_local_entry => clear_programmable_completion_parse_entry
    end type programmable_completion_parse_setting_type

    !> A function to call to parse the current line's arguments.
    !> It should be called with no parameters, and with point at the
    !> position of the argument that is to be completed.
    !>
    !> It gives the final value of each command line argument (which need
    !> not match the textual representation of that argument), and the
    !> beginning position of each argument, as it is seen by the user.
    !> This establishes a relationship between the fully resolved value
    !> of the argument, and the textual representation of the argument.
    !> Port of pcomplete.el pcomplete-parse-arguments-function.
    type(programmable_completion_parse_setting_type), public :: programmable_completion_parse_arguments_function

    !> Function called for completing the initial command argument.
    !> Port of pcomplete.el pcomplete-command-completion-function.
    type(programmable_completion_function_setting_type), public :: programmable_completion_command_completion_function

    !> Function called when no completion rule can be found.
    !> This function is used to generate completions for every argument.
    !> Port of pcomplete.el pcomplete-default-completion-function.
    type(programmable_completion_function_setting_type), public :: programmable_completion_default_completion_function

    !> Function called for determining the current command name.  Port of
    !> pcomplete.el pcomplete-command-name-function.
    procedure(programmable_completion_name_procedure), pointer, public :: programmable_completion_command_name_function => null()

    abstract interface
        !> An exit function: run after completion of STRING, FINISHED
        !> saying how (a COMPLETION_* value).
        recursive subroutine programmable_completion_exit_procedure(string, finished)
            implicit none(type, external)
            character(len=*), intent(in) :: string
            integer, intent(in) :: finished
        end subroutine programmable_completion_exit_procedure
    end interface

    !> The exit function to call in `programmable_completion_completions_at_point'.
    !>
    !> This variable is bound in `programmable_completion_completions_at_point', so you
    !> can change it in order to adjust the behavior for a specific
    !> completion, for example in a `programmable_completion_try_first_hook' function.
    !> Port of pcomplete.el pcomplete-exit-function.
    procedure(programmable_completion_exit_procedure), pointer, public :: programmable_completion_exit_function => null()

    !> The function unquoting the argument being completed
    !> (pcomplete-unquote-argument-function).
    procedure(completion_unquote_procedure), pointer, public :: programmable_completion_unquote_argument_function => &
        command_interpreter_unquote_argument

    !> The function requoting completions for the argument being
    !> completed (pcomplete-requote-argument-function).
    procedure(completion_requote_procedure), pointer, public :: programmable_completion_requote_argument_function => &
        command_interpreter_requote_argument

    ! The exit function `programmable_completion_completions_at_point' was called with.
    procedure(programmable_completion_exit_procedure), pointer :: completion_at_point_function_exit_function => null()

    public :: programmable_completion_exit_procedure

    !> A completion function for a command (pcomplete/NAME), or for a
    !> command in a major mode (pcomplete/MODE/NAME).
    type, public :: programmable_completion_command_function_type
        character(len=64) :: command = ""
        character(len=64) :: mode = ""
        procedure(programmable_completion_function_procedure), pointer, nopass :: fn => null()
    end type programmable_completion_command_function_type

    !> The completion functions of commands (upstream, the functions
    !> named pcomplete/NAME and pcomplete/MODE/NAME), added with `add'.
    type, public :: programmable_completion_command_functions_type
        type(programmable_completion_command_function_type), allocatable :: entries(:)
    contains
        !> Make FN the completion function of COMMAND (in major mode MODE).
        procedure :: append => command_functions_append
    end type programmable_completion_command_functions_type

    !> The completion functions of commands (pcomplete/NAME).
    type(programmable_completion_command_functions_type), public :: programmable_completion_command_functions

    ! pcomplete-args, pcomplete-begins, pcomplete-last, pcomplete-index,
    ! pcomplete-stub, pcomplete-seen and pcomplete-norm-func.
    type(string_type), allocatable :: arguments(:)
    integer(int64), allocatable :: begins(:)
    !> The index of the last argument (pcomplete-last).
    integer, public, protected :: programmable_completion_last = 0
    !> The index of the argument being examined: 1 is the first argument
    !> after the command name (pcomplete-index).
    integer, public, protected :: programmable_completion_index = 0
    !> The text being completed (pcomplete-stub).
    character(len=:), allocatable, public, protected :: programmable_completion_stub
    type(string_type), allocatable :: seen(:)
    procedure(programmable_completion_norm_procedure), pointer :: norm_function => null()

    ! What was thrown, and the completion table thrown with
    ! `pcomplete-completions' (none if not `has_thrown_table').
    integer :: thrown = THROWN_NONE
    type(completion_table_type) :: thrown_table
    logical :: has_thrown_table = .false.

    ! The closures of the tables of `programmable_completion_entries' (the last one
    ! made): its regexp, test and the ignore regexps it captured.
    character(len=:), allocatable :: entries_regular_expression, entries_file_ignore, entries_directory_ignore
    logical :: entries_has_regular_expression = .false., entries_has_file_ignore = .false., entries_has_directory_ignore = .false.
    procedure(programmable_completion_file_test_procedure), pointer :: entries_test => null()
    ! The closure of `programmable_completion_directories_or_entries'.
    character(len=:), allocatable :: doe_regular_expression
    logical :: doe_has_regular_expression = .false.
    procedure(programmable_completion_file_test_procedure), pointer :: doe_test => null()

    ! The closure of the table of `programmable_completion_completions_at_point': the
    ! completions, and whether they are subverted from S1 to S2.
    type(completion_table_type) :: completion_at_point_function_completions
    logical :: completion_at_point_function_subvert = .false.
    character(len=:), allocatable :: completion_at_point_function_s1, completion_at_point_function_s2
    ! The closure of its test: the arguments seen, normalized.
    type(string_type), allocatable :: completion_at_point_function_seen(:)
    procedure(programmable_completion_norm_procedure), pointer :: completion_at_point_function_norm_function => null()

    ! pcomplete--env-regexp.
    character(len=*), parameter :: ENVIRONMENT_REGULAR_EXPRESSION = &
                                   "\(?:\`\|[^\]\)\(?:\\\\\)*\(\$\(?:{\([^}]+\)}\|\(?2:[[:alnum:]_]+\)\)\)"

contains

    ! ------------------------------------------------------------------
    ! Function variables
    ! ------------------------------------------------------------------

    ! Forget the current buffer's values (kill-all-local-variables).
    ! Not a port.
    subroutine clear_programmable_completion_locals()
        call programmable_completion_command_completion_function%clear_local()
        call programmable_completion_default_completion_function%clear_local()
        call programmable_completion_parse_arguments_function%clear_local()
    end subroutine clear_programmable_completion_locals

    !> Give the current buffer its own value LOCAL_PROCEDURE (null when
    !> absent) of
    !> `programmable_completion_command_completion_function' or `programmable_completion_default_completion_function'.  Not a port.
    subroutine set_programmable_completion_function_local(self, local_procedure)
        class(programmable_completion_function_setting_type), intent(inout) :: self
        procedure(programmable_completion_function_procedure), optional :: local_procedure
        integer :: k

        call self%make_local_entry(k)
        if (k == 0) return
        self%locals(k)%procedure => null()
        if (present(local_procedure)) self%locals(k)%procedure => local_procedure
    end subroutine set_programmable_completion_function_local

    !> Set CURRENT_PROCEDURE to the current buffer's value of
    !> `programmable_completion_command_completion_function' or `programmable_completion_default_completion_function':
    !> its own, else the global one.  Not a port.
    subroutine get_programmable_completion_function_current(self, current_procedure)
        class(programmable_completion_function_setting_type), intent(in) :: self
        procedure(programmable_completion_function_procedure), pointer, intent(out) :: current_procedure
        integer :: k

        current_procedure => self%procedure
        k = self%get_local_index()
        if (k > 0) current_procedure => self%locals(k)%procedure
    end subroutine get_programmable_completion_function_current

    ! Grow `locals' by one entry.  Not a port.
    subroutine append_programmable_completion_function_entry(self)
        class(programmable_completion_function_setting_type), intent(inout) :: self
        type(programmable_completion_function_entry_type) :: entry

        if (.not. allocated(self%locals)) allocate (self%locals(0))
        self%locals = [self%locals, entry]
    end subroutine append_programmable_completion_function_entry

    ! Reset entry K of `locals'.  Not a port.
    subroutine clear_programmable_completion_function_entry(self, k)
        class(programmable_completion_function_setting_type), intent(inout) :: self
        integer, intent(in) :: k

        self%locals(k)%procedure => null()
    end subroutine clear_programmable_completion_function_entry

    !> Give the current buffer its own value LOCAL_PROCEDURE (null when
    !> absent) of
    !> `programmable_completion_parse_arguments_function'.  Not a port.
    subroutine set_programmable_completion_parse_local(self, local_procedure)
        class(programmable_completion_parse_setting_type), intent(inout) :: self
        procedure(programmable_completion_parse_procedure), optional :: local_procedure
        integer :: k

        call self%make_local_entry(k)
        if (k == 0) return
        self%locals(k)%procedure => null()
        if (present(local_procedure)) self%locals(k)%procedure => local_procedure
    end subroutine set_programmable_completion_parse_local

    !> Set CURRENT_PROCEDURE to the current buffer's value of
    !> `programmable_completion_parse_arguments_function':
    !> its own, else the global one.  Not a port.
    subroutine get_programmable_completion_parse_current(self, current_procedure)
        class(programmable_completion_parse_setting_type), intent(in) :: self
        procedure(programmable_completion_parse_procedure), pointer, intent(out) :: current_procedure
        integer :: k

        current_procedure => self%procedure
        k = self%get_local_index()
        if (k > 0) current_procedure => self%locals(k)%procedure
    end subroutine get_programmable_completion_parse_current

    ! Grow `locals' by one entry.  Not a port.
    subroutine append_programmable_completion_parse_entry(self)
        class(programmable_completion_parse_setting_type), intent(inout) :: self
        type(programmable_completion_parse_entry_type) :: entry

        if (.not. allocated(self%locals)) allocate (self%locals(0))
        self%locals = [self%locals, entry]
    end subroutine append_programmable_completion_parse_entry

    ! Reset entry K of `locals'.  Not a port.
    subroutine clear_programmable_completion_parse_entry(self, k)
        class(programmable_completion_parse_setting_type), intent(inout) :: self
        integer, intent(in) :: k

        self%locals(k)%procedure => null()
    end subroutine clear_programmable_completion_parse_entry

    ! Add the completion function FN for COMMAND (in major mode MODE).
    ! Not a port.
    subroutine command_functions_append(self, command, fn, mode)
        class(programmable_completion_command_functions_type), intent(inout) :: self
        character(len=*), intent(in) :: command
        procedure(programmable_completion_function_procedure) :: fn
        character(len=*), intent(in), optional :: mode
        type(programmable_completion_command_function_type) :: entry

        if (.not. allocated(self%entries)) allocate (self%entries(0))
        entry%command = command
        if (present(mode)) entry%mode = mode
        entry%fn => fn
        self%entries = [self%entries, entry]
    end subroutine command_functions_append

    !> Do nothing (#'ignore, as a completion function).  Not a port.
    recursive subroutine programmable_completion_ignore(status)
        type(status_type), intent(out) :: status

        status = status_type()
    end subroutine programmable_completion_ignore

    ! The default `programmable_completion_command_completion_function'.  Port of
    ! pcomplete.el:pcomplete-command-completion-function (its default).
    recursive subroutine default_command_completion(status)
        type(status_type), intent(out) :: status

        status = status_type()
        if (programmable_completion_here(executables_form)) continue
    end subroutine default_command_completion

    ! The default `programmable_completion_default_completion_function'.  Port of
    ! pcomplete.el:pcomplete-default-completion-function (its default).
    recursive subroutine default_default_completion(status)
        type(status_type), intent(out) :: status

        status = status_type()

        do while (programmable_completion_here(entries_form))
        end do
    end subroutine default_default_completion

    ! (pcomplete-executables) as a FORM.  Not a port.
    recursive subroutine executables_form(table, found)
        type(completion_table_type), intent(out) :: table
        logical, intent(out) :: found

        call programmable_completion_executables(table)
        found = .true.
    end subroutine executables_form

    ! (pcomplete-entries) as a FORM.  Not a port.
    recursive subroutine entries_form(table, found)
        type(completion_table_type), intent(out) :: table
        logical, intent(out) :: found

        call programmable_completion_entries(table)
        found = .true.
    end subroutine entries_form

    ! ------------------------------------------------------------------
    ! Arguments
    ! ------------------------------------------------------------------

    !> Whether completion was abandoned (upstream, `pcompleted' was
    !> returned) or its table given (`pcomplete-completions' returned): a
    !> completion function returns at once when this is true.  Not a port.
    logical function programmable_completion_thrown()
        programmable_completion_thrown = thrown /= THROWN_NONE
    end function programmable_completion_thrown

    ! The 0-based argument index WHICH, INDEX and OFFSET denote.  Not a port.
    integer function argument_position(which, index, offset) result(n)
        integer, intent(in), optional :: which, index, offset
        integer :: w

        w = PROGRAMMABLE_COMPLETION_ARGUMENT_RELATIVE
        if (present(which)) w = which

        select case (w)
        case (PROGRAMMABLE_COMPLETION_ARGUMENT_FIRST)
            n = 0
        case (PROGRAMMABLE_COMPLETION_ARGUMENT_LAST)
            n = programmable_completion_last
        case default
            n = programmable_completion_index
            if (present(index)) n = programmable_completion_index - index
        end select

        if (present(offset)) n = n + offset
    end function argument_position

    !> Return the textual content of the INDEXth argument (FOUND false if
    !> INDEX is outside of the argument list).  INDEX is based from the
    !> current processing position.  If INDEX is positive, values
    !> returned are closer to the command argument; if negative, they are
    !> closer to the last argument.  The default value for INDEX is 0,
    !> meaning the current argument being examined.
    !>
    !> WHICH PROGRAMMABLE_COMPLETION_ARGUMENT_FIRST and PROGRAMMABLE_COMPLETION_ARGUMENT_LAST give the special indices
    !> `first' and `last'.
    !>
    !> The OFFSET argument is added to/taken away from the index that will
    !> be used.  This is really only useful with `first' and `last', for
    !> accessing absolute argument positions.  Port of
    !> pcomplete.el:pcomplete-arg.
    function programmable_completion_argument(index, offset, which, found) result(argument)
        integer, intent(in), optional :: index, offset, which
        logical, intent(out), optional :: found
        character(len=:), allocatable :: argument
        integer :: n

        argument = ""
        if (present(found)) found = .false.
        if (.not. allocated(arguments)) return
        n = argument_position(which, index, offset)
        if (n < 0 .or. n >= size(arguments)) return
        argument = arguments(n + 1)%text
        if (present(found)) found = .true.
    end function programmable_completion_argument

    !> Return the beginning position of the INDEXth argument (0 if
    !> none).  See the documentation for `programmable_completion_argument'.  Port of
    !> pcomplete.el:pcomplete-begin.
    integer(int64) function programmable_completion_begin(index, offset, which) result(position)
        integer, intent(in), optional :: index, offset, which
        integer :: n

        position = 0
        if (.not. allocated(begins)) return
        n = argument_position(which, index, offset)
        if (n < 0 .or. n >= size(begins)) return
        position = begins(n + 1)
    end function programmable_completion_begin

    !> Return the actual text representation of the last argument.
    !> This is different from `programmable_completion_argument', which returns the textual
    !> value that the last argument evaluated to.  This function returns
    !> what the user actually typed in.  Port of
    !> pcomplete.el:pcomplete-actual-arg.
    function programmable_completion_actual_argument(index, offset, which) result(text)
        integer, intent(in), optional :: index, offset, which
        character(len=:), allocatable :: text
        type(status_type) :: st

        call buffer_substring(programmable_completion_begin(index, offset, which), point(), text, st)
    end function programmable_completion_actual_argument

    !> Move the various pointers to the next argument.  Port of
    !> pcomplete.el:pcomplete-next-arg.
    recursive subroutine programmable_completion_next_argument()
        programmable_completion_index = programmable_completion_index + 1
        programmable_completion_stub = programmable_completion_argument()

        if (programmable_completion_index > programmable_completion_last) then
            call message("No completions")
            thrown = THROWN_PCOMPLETED
        end if
    end subroutine programmable_completion_next_argument

    !> Return the command name of the first argument.  Port of
    !> pcomplete.el:pcomplete-command-name.
    recursive subroutine programmable_completion_command_name(name)
        character(len=:), allocatable, intent(out) :: name

        name = file_name_nondirectory(programmable_completion_argument(which=PROGRAMMABLE_COMPLETION_ARGUMENT_FIRST))
    end subroutine programmable_completion_command_name

    !> Like `string_match', but on the current completion argument:
    !> INDEX defaults to 1 (the previous argument).  Without such an
    !> argument, completion is abandoned (see `programmable_completion_thrown').
    !> Port of pcomplete.el:pcomplete-match.
    recursive logical function programmable_completion_match(regular_expression, index, offset, start, which) result(matched)
        character(len=*), intent(in) :: regular_expression
        integer, intent(in), optional :: index, offset, which
        integer(int64), intent(in), optional :: start
        character(len=:), allocatable :: argument
        type(status_type) :: st
        integer :: i
        logical :: found

        matched = .false.
        i = 1
        if (present(index)) i = index
        argument = programmable_completion_argument(i, offset, which, found)

        if (.not. found) then
            thrown = THROWN_PCOMPLETED
            return
        end if

        if (present(start)) then
            matched = string_match(regular_expression, argument, st, start=start + 1) > 0
        else
            matched = string_match(regular_expression, argument, st) > 0
        end if
    end function programmable_completion_match

    !> Like `match_string', but on the current completion argument:
    !> INDEX defaults to 1.  Port of pcomplete.el:pcomplete-match-string.
    recursive function programmable_completion_match_string(which_group, index, offset, which) result(text)
        integer, intent(in) :: which_group
        integer, intent(in), optional :: index, offset, which
        character(len=:), allocatable :: text
        character(len=:), allocatable :: argument
        integer :: i
        logical :: found

        text = ""
        i = 1
        if (present(index)) i = index
        argument = programmable_completion_argument(i, offset, which, found)

        if (.not. found) then
            thrown = THROWN_PCOMPLETED
            return
        end if

        text = match_string(which_group, argument)
    end function programmable_completion_match_string

    !> Whether the current programmable argument (INDEX defaults to 1)
    !> satisfies TEST or matches one of REGULAR_EXPRESSIONS entirely.  Port of
    !> pcomplete.el:pcomplete-test (tests are a function or
    !> regexps; t is TEST `programmable_completion_always').
    recursive logical function programmable_completion_test(test, regular_expressions, index, offset, which) result(ok)
        procedure(programmable_completion_file_test_procedure), optional :: test
        character(len=*), intent(in), optional :: regular_expressions(:)
        integer, intent(in), optional :: index, offset, which
        character(len=:), allocatable :: argument
        type(status_type) :: st
        integer :: i, k

        ok = .false.
        i = 1
        if (present(index)) i = index
        argument = programmable_completion_argument(i, offset, which)

        if (present(test)) then
            ok = test(argument)
            if (ok) return
        end if

        if (present(regular_expressions)) then
            do k = 1, size(regular_expressions)
                if (string_match("^"//trim(regular_expressions(k))//"$", argument, st) > 0) then
                    ok = .true.
                    return
                end if
            end do
        end if
    end function programmable_completion_test

    !> Parse whitespace separated arguments in the current region.
    !> Port of pcomplete.el:pcomplete-parse-buffer-arguments.
    recursive subroutine programmable_completion_parse_buffer_arguments(arguments, begins)
        type(string_type), allocatable, intent(out) :: arguments(:)
        integer(int64), allocatable, intent(out) :: begins(:)
        type(status_type) :: st
        integer(int64) :: original_point, start, moved
        character(len=:), allocatable :: text

        original_point = point()
        allocate (arguments(0), begins(0))
        call goto_character(1_int64)

        do while (point() < point_maximum())
            moved = skip_characters_forward(" "//achar(9)//achar(10), st)
            start = point()
            moved = skip_characters_forward("^ "//achar(9)//achar(10), st)
            call buffer_substring(start, point(), text, st)
            call append_argument(arguments, begins, text, start)
        end do

        call goto_character(original_point)
    end subroutine programmable_completion_parse_buffer_arguments

    !> Parse whitespace separated arguments in the current region (from
    !> the beginning of the input to point).  Port of
    !> pcomplete.el:pcomplete-parse-comint-arguments.
    recursive subroutine programmable_completion_parse_command_interpreter_arguments(arguments, begins)
        type(string_type), allocatable, intent(out) :: arguments(:)
        integer(int64), allocatable, intent(out) :: begins(:)
        type(status_type) :: st
        integer(int64) :: original_point, begin, start, moved
        integer(int32) :: c
        character(len=:), allocatable :: text
        logical :: have

        original_point = point()
        call command_interpreter_beginning_of_line(.false.)
        begin = point()
        allocate (arguments(0), begins(0))
        call goto_character(begin)

        do while (point() < original_point)
            moved = skip_characters_forward(" "//achar(9)//achar(10), st)
            start = point()

            do
                moved = skip_characters_forward("^ "//achar(9)//achar(10)//"\\", st)
                call character_after(c, have)
                if (.not. have .or. c /= iachar("\")) exit
                call goto_character(point() + 1)
                if (is_end_of_line()) exit
                call goto_character(point() + 1)
            end do

            call buffer_substring(start, point(), text, st)
            call append_argument(arguments, begins, text, start)
        end do

        call goto_character(original_point)
    end subroutine programmable_completion_parse_command_interpreter_arguments

    ! Add TEXT beginning at START to ARGUMENTS and BEGINS.  Not a port.
    subroutine append_argument(arguments, begins, text, start)
        type(string_type), allocatable, intent(inout) :: arguments(:)
        integer(int64), allocatable, intent(inout) :: begins(:)
        character(len=*), intent(in) :: text
        integer(int64), intent(in) :: start
        type(string_type) :: argument

        argument%text = text
        arguments = [arguments, argument]
        begins = [begins, start]
    end subroutine append_argument

    !> Parse the command line arguments.  Most completions need this
    !> info.  Returns false when there are none.  Port of
    !> pcomplete.el:pcomplete-parse-arguments (no argument lists computed
    !> during parsing).
    recursive logical function programmable_completion_parse_arguments(expand_p) result(ok)
        logical, intent(in) :: expand_p
        procedure(programmable_completion_parse_procedure), pointer :: fn
        type(string_type), allocatable :: parsed(:)
        integer(int64), allocatable :: parsed_begins(:)
        type(status_type) :: st
        integer(int64) :: begin

        ok = .false.
        call programmable_completion_parse_arguments_function%get_current(fn)
        if (.not. associated(fn)) return
        call fn(parsed, parsed_begins)
        if (.not. allocated(parsed)) return

        if (size(parsed) == 0) then
            deallocate (parsed)
            call append_string(parsed, "")
        end if

        if (.not. allocated(parsed_begins)) allocate (parsed_begins(0))
        if (size(parsed_begins) == 0) parsed_begins = [point()]
        call move_alloc(parsed, arguments)
        call move_alloc(parsed_begins, begins)
        programmable_completion_last = size(arguments) - 1
        programmable_completion_index = 0
        programmable_completion_stub = programmable_completion_argument(which=PROGRAMMABLE_COMPLETION_ARGUMENT_LAST)
        begin = programmable_completion_begin(which=PROGRAMMABLE_COMPLETION_ARGUMENT_LAST)

        if (expand_p .and. begin > 0) then
            call delete_region(begin, point(), st)
            call insert(programmable_completion_stub, st)
        end if

        ok = .true.
    end function programmable_completion_parse_arguments

    ! ------------------------------------------------------------------
    ! File-system completion lists
    ! ------------------------------------------------------------------

    !> Complete against a list of directory candidates: TABLE.
    !> If REGULAR_EXPRESSION is present, it is a regular expression used to refine the
    !> match (files not matching the REGULAR_EXPRESSION will be excluded).
    !> If TEST is present, it will also be used to refine the match
    !> (files for which the TEST returns false will be excluded).
    !> If no directory information can be extracted from the completed
    !> component, `default_directory' is used as the basis for
    !> completion.  Port of pcomplete.el:pcomplete-entries (with
    !> pcomplete--entries).
    recursive subroutine programmable_completion_entries(table, regular_expression, test)
        type(completion_table_type), intent(out) :: table
        character(len=*), intent(in), optional :: regular_expression
        procedure(programmable_completion_file_test_procedure), optional :: test

        ! Capture the values for later use.
        entries_has_regular_expression = present(regular_expression)
        if (present(regular_expression)) entries_regular_expression = regular_expression
        entries_test => null()
        if (present(test)) entries_test => test
        entries_has_file_ignore = allocated(programmable_completion_file_ignore%value)
        if (entries_has_file_ignore) entries_file_ignore = programmable_completion_file_ignore%value
        entries_has_directory_ignore = allocated(programmable_completion_directory_ignore%value)
        if (entries_has_directory_ignore) entries_directory_ignore = programmable_completion_directory_ignore%value
        table = completion_function_table(entries_table, "file")
    end subroutine programmable_completion_entries

    !> Like `programmable_completion_entries', but doesn't ignore any entries.  Port of
    !> pcomplete.el:pcomplete-all-entries.
    recursive subroutine programmable_completion_all_entries(table, regular_expression, test)
        type(completion_table_type), intent(out) :: table
        character(len=*), intent(in), optional :: regular_expression
        procedure(programmable_completion_file_test_procedure), optional :: test

        call programmable_completion_entries(table, regular_expression, test)
        entries_has_file_ignore = .false.
        entries_has_directory_ignore = .false.
    end subroutine programmable_completion_all_entries

    !> Complete amongst a list of directories.  Port of
    !> pcomplete.el:pcomplete-dirs.
    recursive subroutine programmable_completion_directories(table, regular_expression)
        type(completion_table_type), intent(out) :: table
        character(len=*), intent(in), optional :: regular_expression

        call programmable_completion_entries(table, regular_expression, is_directory)
    end subroutine programmable_completion_directories

    !> Complete amongst a list of directories and executables.  Port of
    !> pcomplete.el:pcomplete-executables.
    recursive subroutine programmable_completion_executables(table, regular_expression)
        type(completion_table_type), intent(out) :: table
        character(len=*), intent(in), optional :: regular_expression

        call programmable_completion_entries(table, regular_expression, is_executable)
    end subroutine programmable_completion_executables

    !> Return either directories, or qualified entries.  Port of
    !> pcomplete.el:pcomplete-dirs-or-entries.
    recursive subroutine programmable_completion_directories_or_entries(table, regular_expression, test)
        type(completion_table_type), intent(out) :: table
        character(len=*), intent(in), optional :: regular_expression
        procedure(programmable_completion_file_test_procedure), optional :: test

        doe_has_regular_expression = present(regular_expression)
        if (present(regular_expression)) doe_regular_expression = regular_expression
        doe_test => null()
        if (present(test)) doe_test => test
        call programmable_completion_entries(table, test=is_directory_or_entry)
    end subroutine programmable_completion_directories_or_entries

    ! `is_file_directory' as a file test.  Not a port.
    recursive logical function is_directory(file)
        character(len=*), intent(in) :: file

        is_directory = is_file_directory(file)
    end function is_directory

    ! `is_file_executable' as a file test.  Not a port.
    recursive logical function is_executable(file)
        character(len=*), intent(in) :: file

        is_executable = is_file_executable(file)
    end function is_executable

    ! The test of `programmable_completion_directories_or_entries'.  Port of
    ! pcomplete.el:pcomplete-dirs-or-entries, in part.
    recursive logical function is_directory_or_entry(file) result(ok)
        character(len=*), intent(in) :: file
        type(status_type) :: st

        ok = is_file_directory(file)
        if (ok) return

        if (doe_has_regular_expression) then
            if (string_match(doe_regular_expression, file, st) == 0) return
        end if

        if (associated(doe_test)) then
            ok = doe_test(file)
        else
            ok = .true.
        end if
    end function is_directory_or_entry

    ! The test of the table of `programmable_completion_entries' (ign-pred,
    ! reg-pred and TEST or `file_exists').  Port of
    ! pcomplete.el:pcomplete--entries, in part.
    recursive logical function entries_filter(element) result(ok)
        type(value_type), intent(in) :: element
        character(len=:), allocatable :: file
        type(status_type) :: st

        ok = .false.
        file = xstring(element)

        if (entries_has_regular_expression) then
            if (string_match(entries_regular_expression, file, st) == 0) return
        end if

        if (len(file) > 0) then
            if (file(len(file):len(file)) == "/") then
                if (entries_has_directory_ignore) then
                    if (string_match(entries_directory_ignore, file, st) > 0) return
                end if
            else if (entries_has_file_ignore) then
                if (string_match(entries_file_ignore, file, st) > 0) return
            end if
        end if

        if (associated(entries_test)) then
            ok = entries_test(file)
        else
            ok = file_exists(file)
        end if
    end function entries_filter

    ! TEST of `programmable_completion_entries' alone, as a completion test.
    ! Not a port.
    recursive logical function entries_test_only(element)
        type(value_type), intent(in) :: element

        entries_test_only = entries_test(xstring(element))
    end function entries_test_only

    ! The table of pcomplete--entries.  Port of
    ! pcomplete.el:pcomplete--entries, in part.
    recursive subroutine entries_inner_table(string, action, result, status, test, suffix)
        character(len=*), intent(in) :: string
        integer, intent(in) :: action
        type(value_type), intent(out) :: result
        type(status_type), intent(out) :: status
        procedure(completion_test_procedure), optional :: test
        character(len=*), intent(in), optional :: suffix
        type(completion_table_type) :: table
        character(len=len(completion_ignored_extensions)) :: saved_extensions(size(completion_ignored_extensions))

        ! `completion_file_name_table' calls `file_exists' when the
        ! test is absent.  So likewise, defer to TEST if it's
        ! there, else take ourselves to be responsible for calling
        ! `file_exists'.
        saved_extensions = completion_ignored_extensions
        completion_ignored_extensions = ""
        table = completion_function_table(command_interpreter_completion_file_name_table)

        if (entries_has_regular_expression .or. entries_has_file_ignore .or. entries_has_directory_ignore) then
            call completion_table_with_test(table, entries_filter, .true., string, action, result, status, &
                                            test, suffix)
        else if (associated(entries_test)) then
            call completion_table_with_test(table, entries_test_only, .true., string, action, result, &
                                            status, test, suffix)
        else
            call completion_table_with_test(table, strict=.true., string=string, action=action, &
                                            result=result, status=status, test=test, suffix=suffix)
        end if

        completion_ignored_extensions = saved_extensions
    end subroutine entries_inner_table

    ! The table of `programmable_completion_entries': environment variable references
    ! expanded, then pcomplete--entries.  Port of
    ! pcomplete.el:pcomplete-entries, in part.
    recursive subroutine entries_table(string, action, result, status, test, suffix)
        character(len=*), intent(in) :: string
        integer, intent(in) :: action
        type(value_type), intent(out) :: result
        type(status_type), intent(out) :: status
        procedure(completion_test_procedure), optional :: test
        character(len=*), intent(in), optional :: suffix
        type(completion_table_type) :: table
        type(value_type) :: bounds, comps
        type(value_type), allocatable :: elements(:)
        character(len=:), allocatable :: rest, newstring, value, usuffix
        type(status_type) :: st
        integer(int64) :: original_length, envpos, bstart, length_environment
        integer :: k
        logical :: found, expanded

        table = completion_function_table(entries_inner_table)
        original_length = count_characters(string)
        rest = string
        newstring = ""
        envpos = 0
        expanded = .false.

        ! Perform env-var expansion.
        do while (string_match(ENVIRONMENT_REGULAR_EXPRESSION, rest, st) > 0)
            expanded = .true.
            newstring = newstring//substring(rest, 0_int64, match_beginning(1, st) - 1)
            call getenv(match_string(2, rest), value, found)
            if (.not. found) value = ""
            newstring = newstring//value
            rest = substring(rest, match_end(1, st) - 1)
        end do

        envpos = count_characters(newstring)
        newstring = newstring//rest

        if (.not. (expanded .and. (action == COMPLETE_ALL .or. action == COMPLETE_BOUNDARIES))) then
            call complete_with_action(action, table, newstring, result, status, test, suffix)
            return
        end if

        usuffix = ""
        if (present(suffix)) usuffix = suffix
        call entries_inner_table(newstring, COMPLETE_BOUNDARIES, bounds, st, test, usuffix)
        bstart = 0

        if (list_length(bounds) == 2) then
            if (vtag(list_element(bounds, 1_int64)) == V_INTEGER) bstart = xfixnum(list_element(bounds, 1_int64))
        end if

        status = status_type()

        if (bstart >= envpos) then
            ! The env-var is "out of bounds".
            if (action == COMPLETE_ALL) then
                call complete_with_action(action, table, newstring, result, status, test)
            else
                result = list(value_type(bstart + original_length - count_characters(newstring)), list_element(bounds, 2_int64))
            end if
        else
            ! The env-var is in the file bounds.
            if (action == COMPLETE_ALL) then
                call complete_with_action(action, table, newstring, comps, status, test)
                length_environment = envpos - bstart
                ! Strip the part of each completion that's actually
                ! coming from the env-var.
                call list_elements(comps, elements)

                do k = 1, size(elements)
                    elements(k) = value_type(text=substring(xstring(elements(k)), length_environment))
                end do

                result = make_list(elements)
            else
                result = list(value_type(envpos + original_length - count_characters(newstring)), list_element(bounds, 2_int64))
            end if
        end if
    end subroutine entries_table

    ! ------------------------------------------------------------------
    ! Generation of completion lists
    ! ------------------------------------------------------------------

    !> Find the completion function FN to call for the given COMMAND
    !> (null if none): the one for COMMAND in the current major mode, else
    !> the one for COMMAND.  Port of
    !> pcomplete.el:pcomplete-find-completion-function.
    recursive subroutine programmable_completion_find_completion_function(command, fn)
        character(len=*), intent(in) :: command
        procedure(programmable_completion_function_procedure), pointer, intent(out) :: fn
        character(len=:), allocatable :: mode
        integer :: k

        fn => null()
        if (.not. allocated(programmable_completion_command_functions%entries)) return
        mode = "fundamental_mode"

        if (current /= 0) then
            if (allocated(buffers(current)%major_mode)) mode = buffers(current)%major_mode
        end if

        do k = 1, size(programmable_completion_command_functions%entries)
            associate (e => programmable_completion_command_functions%entries(k))
                if (trim(e%command) == command .and. trim(e%mode) == mode) then
                    fn => e%fn
                    return
                end if
            end associate
        end do

        do k = 1, size(programmable_completion_command_functions%entries)
            associate (e => programmable_completion_command_functions%entries(k))
                if (trim(e%command) == command .and. len_trim(e%mode) == 0) then
                    fn => e%fn
                    return
                end if
            end associate
        end do
    end subroutine programmable_completion_find_completion_function

    !> Return a list of completions for the current argument position:
    !> TABLE, FOUND false if there are none.  Port of
    !> pcomplete.el:pcomplete-completions.
    recursive subroutine programmable_completion_completions(table, found, status)
        type(completion_table_type), intent(out) :: table
        logical, intent(out) :: found
        type(status_type), intent(out) :: status
        procedure(programmable_completion_function_procedure), pointer :: fn
        procedure(programmable_completion_name_procedure), pointer :: name_fn
        character(len=:), allocatable :: name

        status = status_type()
        found = .false.
        thrown = THROWN_NONE
        has_thrown_table = .false.

        if (programmable_completion_parse_arguments(programmable_completion_expand_before_complete%value)) then
            if (programmable_completion_index == programmable_completion_last) then
                call programmable_completion_command_completion_function%get_current(fn)
                if (associated(fn)) call fn(status)
            else
                name_fn => programmable_completion_command_name_function

                if (associated(name_fn)) then
                    call name_fn(name)
                else
                    call programmable_completion_command_name(name)
                end if

                call programmable_completion_find_completion_function(name, fn)
                if (.not. associated(fn)) call programmable_completion_default_completion_function%get_current(fn)
                call programmable_completion_next_argument()
                if (thrown == THROWN_NONE .and. associated(fn)) call fn(status)
            end if
        end if

        if (thrown == THROWN_COMPLETIONS .and. has_thrown_table) then
            table = thrown_table
            found = .true.
        end if
    end subroutine programmable_completion_completions

    !> Complete against the current argument, if at the end.
    !> If completion is to be done here, call FORM to generate the
    !> completion table which will be used for completion purposes, and
    !> return false.  Otherwise move to the next argument and return
    !> true.  If STUB is present, use it as the completion stub instead
    !> of the default (which is the entire text of the current
    !> argument).
    !>
    !> For an example of when you might want to use STUB: if the current
    !> argument text is `long-path-name/', you don't want the completions
    !> list display to be cluttered by `long-path-name/' appearing at the
    !> beginning of every alternative.  Not only does this make things
    !> less intelligible, but it is also inefficient.  Yet, if the
    !> completion list does not begin with this string for every entry,
    !> the current argument won't complete correctly.
    !>
    !> The solution is to specify a relative stub.  It allows you to
    !> substitute a different argument from the current argument, almost
    !> always for the sake of efficiency.
    !>
    !> If PARING is PROGRAMMABLE_COMPLETION_PARING_DEFAULT, this argument will be pared
    !> against previous arguments using the function `file_truename' (or
    !> PARING_FUNCTION) to normalize them.  If PARING is
    !> PROGRAMMABLE_COMPLETION_PARING_NONE, the argument dealt with by this call will
    !> not participate in argument paring.  If it is
    !> PROGRAMMABLE_COMPLETION_PARING_CLEAR, all previous arguments that have been seen
    !> will be cleared.
    !>
    !> If FORM_ONLY is true, only the result of FORM will be used to
    !> generate the completions list.  This means that the hook
    !> `programmable_completion_try_first_hook' will not be run.  Port of
    !> pcomplete.el:pcomplete-here (with pcomplete--here).
    recursive logical function programmable_completion_here(form, stub, paring, paring_function, form_only) result(advanced)
        procedure(programmable_completion_form_procedure), optional :: form
        character(len=*), intent(in), optional :: stub
        integer, intent(in), optional :: paring
        procedure(programmable_completion_norm_procedure), optional :: paring_function
        logical, intent(in), optional :: form_only
        character(len=:), allocatable :: argument, norm
        type(status_type) :: st
        integer :: p
        logical :: only, found

        advanced = .false.
        if (thrown /= THROWN_NONE) return
        p = PROGRAMMABLE_COMPLETION_PARING_DEFAULT
        if (present(paring)) p = paring

        if (programmable_completion_index < programmable_completion_last) then
            if (p == PROGRAMMABLE_COMPLETION_PARING_CLEAR) then
                if (allocated(seen)) deallocate (seen)
            else if (p /= PROGRAMMABLE_COMPLETION_PARING_NONE) then
                argument = programmable_completion_argument()

                if (present(paring_function)) then
                    call paring_function(argument, norm)
                else
                    call file_truename(argument, norm, st)
                    if (.not. st%ok()) norm = argument
                end if

                call prepend_string(seen, norm)
            end if

            call programmable_completion_next_argument()
            advanced = thrown == THROWN_NONE
            return
        end if

        if (present(stub)) programmable_completion_stub = stub

        if (p == PROGRAMMABLE_COMPLETION_PARING_NONE .or. p == PROGRAMMABLE_COMPLETION_PARING_CLEAR) then
            if (allocated(seen)) deallocate (seen)
        else if (present(paring_function)) then
            norm_function => paring_function
        else
            norm_function => truename_norm
        end if

        only = .false.
        if (present(form_only)) only = form_only
        if (.not. only) call programmable_completion_try_first_hook%execute(st)
        thrown = THROWN_COMPLETIONS
        has_thrown_table = .false.

        if (present(form)) then
            call form(thrown_table, found)
            has_thrown_table = found
        end if
    end function programmable_completion_here

    !> An alternate form of `programmable_completion_here' which does not participate
    !> in argument paring.  Port of pcomplete.el:pcomplete-here*.
    recursive logical function programmable_completion_here_star(form, stub, form_only) result(advanced)
        procedure(programmable_completion_form_procedure), optional :: form
        character(len=*), intent(in), optional :: stub
        logical, intent(in), optional :: form_only

        advanced = programmable_completion_here(form, stub, PROGRAMMABLE_COMPLETION_PARING_NONE, form_only=form_only)
    end function programmable_completion_here_star

    ! `file_truename' as a paring function.  Not a port.
    recursive subroutine truename_norm(argument, norm)
        character(len=*), intent(in) :: argument
        character(len=:), allocatable, intent(out) :: norm
        type(status_type) :: st

        call file_truename(argument, norm, st)
        if (.not. st%ok()) norm = argument
    end subroutine truename_norm

    ! ------------------------------------------------------------------
    ! The standard completion UI
    ! ------------------------------------------------------------------

    !> The default exit function to use in
    !> `programmable_completion_completions_at_point'.  This just adds
    !> `programmable_completion_termination_string' after the completion if FINISHED
    !> is COMPLETION_FINISHED.  Port of
    !> pcomplete.el:pcomplete-default-exit-function.
    recursive subroutine programmable_completion_default_exit_function(string, finished)
        character(len=*), intent(in) :: string
        integer, intent(in) :: finished
        type(status_type) :: st

        ! (STRING is unused.)
        if (.not. allocated(programmable_completion_termination_string%value)) return
        if (len(programmable_completion_termination_string%value) == 0) return
        if (finished /= COMPLETION_FINISHED) return

        if (looking_at(regular_expression_quote(programmable_completion_termination_string%value), st)) then
            call goto_character(match_end(0, st))
        else
            call insert(programmable_completion_termination_string%value, st)
        end if
    end subroutine programmable_completion_default_exit_function

    ! The `:exit_function' of `programmable_completion_completions_at_point': the
    ! `programmable_completion_exit_function' it was called with.  Not a port.
    recursive subroutine completion_at_point_function_exit(string, finished)
        character(len=*), intent(in) :: string
        integer, intent(in) :: finished

        if (associated(completion_at_point_function_exit_function)) call completion_at_point_function_exit_function(string, &
                                                                                                                    finished)
    end subroutine completion_at_point_function_exit

    !> The length of the longest suffix of S1 that is also a suffix of S2
    !> (S2 is expected to be the "unquoted/expanded" version of S1).
    !> Port of pcomplete.el:pcomplete--common-suffix.
    integer(int64) function common_suffix(s1, s2) result(n)
        character(len=*), intent(in) :: s1, s2
        integer(int64) :: n1, n2

        n1 = count_characters(s1)
        n2 = count_characters(s2)
        n = 0

        do while (n < n1 .and. n < n2)
            if (substring(s1, n1 - n - 1, n1 - n) /= substring(s2, n2 - n - 1, n2 - n)) exit
            n = n + 1
        end do
    end function common_suffix

    ! The table of `programmable_completion_completions_at_point' (before quoting).
    ! Port of pcomplete.el:pcomplete-completions-at-point, in part.
    recursive subroutine completion_at_point_function_table(string, action, result, status, test, suffix)
        character(len=*), intent(in) :: string
        integer, intent(in) :: action
        type(value_type), intent(out) :: result
        type(status_type), intent(out) :: status
        procedure(completion_test_procedure), optional :: test
        character(len=*), intent(in), optional :: suffix

        if (completion_at_point_function_subvert) then
            call completion_table_subvert(completion_at_point_function_completions, completion_at_point_function_s1, &
                                          completion_at_point_function_s2, string, action, result, status, &
                                          test, suffix)
        else
            call complete_with_action(action, completion_at_point_function_completions, string, result, status, test, suffix)
        end if
    end subroutine completion_at_point_function_table

    ! The test of `programmable_completion_completions_at_point': pare the
    ! arguments already seen.  Port of
    ! pcomplete.el:pcomplete-completions-at-point, in part.
    recursive logical function completion_at_point_function_pare(element) result(ok)
        type(value_type), intent(in) :: element
        character(len=:), allocatable :: norm
        integer :: k

        ok = .true.
        call completion_at_point_function_norm_function(directory_file_name(xstring(element)), norm)

        do k = 1, size(completion_at_point_function_seen)
            if (completion_at_point_function_seen(k)%text == norm .and. len(completion_at_point_function_seen(k)%text) == len( &
                norm)) then
                ok = .false.
                return
            end if
        end do
    end function completion_at_point_function_pare

    !> Provide standard completion using pcomplete's completion tables.
    !> Same as `programmable_completion' but using the standard completion UI (for
    !> `completion_at_point_functions').  Port of
    !> pcomplete.el:pcomplete-completions-at-point.
    recursive subroutine programmable_completion_completions_at_point(data, status)
        type(completion_at_point_function_data_type), intent(out) :: data
        type(status_type), intent(out) :: status
        type(completion_table_type) :: completions
        procedure(programmable_completion_exit_procedure), pointer :: saved_exit
        character(len=:), allocatable :: buftext, temporary, norm
        type(status_type) :: st
        integer(int64) :: argbeg, beginning, end, suffix_length
        integer :: k
        logical :: found, saved_read_only

        status = status_type()
        data = completion_at_point_function_data_type()
        ! FIXME: it only completes the text before point, whereas the
        ! standard UI may also consider text after point.
        programmable_completion_stub = ""
        if (allocated(seen)) deallocate (seen)
        norm_function => null()
        if (allocated(arguments)) deallocate (arguments)
        if (allocated(begins)) deallocate (begins)
        programmable_completion_last = 0
        programmable_completion_index = 0
        saved_exit => programmable_completion_exit_function
        ! Make sure the function obeys `programmable_completion_allow_modifications'.
        saved_read_only = buffers(current)%read_only
        if (.not. programmable_completion_allow_modifications) buffers(current)%read_only = .true.
        ! pcomplete-completions is the core function that finds the chunk
        ! of text to complete (returned indirectly in `programmable_completion_stub')
        ! and the set of possible completions.
        call programmable_completion_completions(completions, found, status)
        buffers(current)%read_only = saved_read_only
        completion_at_point_function_exit_function => programmable_completion_exit_function
        programmable_completion_exit_function => saved_exit

        if (thrown == THROWN_PCOMPLETED .or. .not. status%ok()) then
            status = status_type()
            return
        end if

        ! Since we use minibuffer-complete, which doesn't know
        ! `programmable_completion_stub' and works from the buffer's text instead, we
        ! need to trick minibuffer-complete into using `programmable_completion_stub'
        ! without its knowledge: completion-table-subvert constructs a
        ! completion table which expects strings using a prefix from the
        ! buffer's text but internally uses the corresponding prefix from
        ! `programmable_completion_stub'.
        argbeg = programmable_completion_begin()
        beginning = max(point() - count_characters(programmable_completion_stub), argbeg)
        end = point()

        ! Try and improve our guess of `beginning' in case the difference
        ! between `programmable_completion_stub' and the buffer's text is simply due to
        ! some chars removed by unquoting.
        do
            call buffer_substring(beginning, end, temporary, st)
            call programmable_completion_unquote_argument_function(temporary, buftext)
            if (.not. (beginning > argbeg .and. count_characters(programmable_completion_stub) > count_characters(buftext))) exit
            beginning = max(argbeg, beginning - (count_characters(programmable_completion_stub) - count_characters(buftext)))
        end do

        ! Try and improve our guess of `end' in case it's not point.
        do while (count_characters(buftext) < count_characters(programmable_completion_stub) .and. end < point_maximum())
            call buffer_substring(beginning, end + 1, temporary, st)
            call programmable_completion_unquote_argument_function(temporary, norm)
            if (.not. prefix_of(norm, programmable_completion_stub)) exit
            end = end + 1
            buftext = norm
        end do

        if (.not. found) return
        completion_at_point_function_completions = completions
        completion_at_point_function_subvert = .not. (programmable_completion_stub == buftext .and. len( &
                                                      programmable_completion_stub) == len(buftext))

        if (completion_at_point_function_subvert) then
            ! This may not always be strictly right, but given the lack of
            ! any other info, it's about as good as it gets, and in
            ! practice it should work just fine (fingers crossed).
            suffix_length = common_suffix(programmable_completion_stub, buftext)
            completion_at_point_function_s1 = substring(buftext, 0_int64, count_characters(buftext) - suffix_length)
            completion_at_point_function_s2 = substring(programmable_completion_stub, 0_int64, &
                                                        count_characters(programmable_completion_stub) - suffix_length)
        end if

        data%found = .true.
        data%start = beginning
        data%end = end
        data%table = completion_table_with_quoting(completion_at_point_function_table, &
                                                   programmable_completion_unquote_argument_function, &
                                                   programmable_completion_requote_argument_function)

        ! Pare it down, if applicable.
        if (programmable_completion_use_paring%value .and. allocated(seen)) then
            if (size(seen) > 0) then
                ! Capture the values for later use.
                completion_at_point_function_norm_function => norm_function

                if (.not. associated(completion_at_point_function_norm_function)) then
                    completion_at_point_function_norm_function => truename_norm
                end if

                if (allocated(completion_at_point_function_seen)) deallocate (completion_at_point_function_seen)
                allocate (completion_at_point_function_seen(size(seen)))

                do k = 1, size(seen)
                    call completion_at_point_function_norm_function(directory_file_name(seen(k)%text), norm)
                    completion_at_point_function_seen(k)%text = norm
                end do

                data%test => completion_at_point_function_pare
            end if
        end if

        data%exit_function => completion_at_point_function_exit
    end subroutine programmable_completion_completions_at_point

    ! Whether PREFIX is a prefix of STRING (string-prefix-p).  Not a port.
    logical function prefix_of(prefix, string)
        character(len=*), intent(in) :: prefix, string

        prefix_of = .false.
        if (len(prefix) > len(string)) return
        prefix_of = string(1:len(prefix)) == prefix
    end function prefix_of

    !> Define pcomplete's settings and default functions.  Not a port.
    subroutine initialize_programmable_completion()
        if (associated(programmable_completion_command_completion_function%procedure)) return
        if (.not. allocated(programmable_completion_termination_string%value)) programmable_completion_termination_string = " "
        if (.not. allocated(programmable_completion_hosts_file%value)) programmable_completion_hosts_file = "/etc/hosts"
        call forward_setting(programmable_completion_termination_string)
        call forward_setting(programmable_completion_remote_file_ignore)
        programmable_completion_command_completion_function%procedure => default_command_completion
        programmable_completion_default_completion_function%procedure => default_default_completion
        programmable_completion_parse_arguments_function%procedure => programmable_completion_parse_buffer_arguments
        programmable_completion_exit_function => programmable_completion_default_exit_function
        call append_locals_clearer(clear_programmable_completion_locals)
    end subroutine initialize_programmable_completion
end module programmable_completion_mod
