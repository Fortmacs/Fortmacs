! pcomplete_mod.f90 -- programmable, polymorphic completion engine.
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

!> Programmable completion: `pcomplete_completions_at_point' completes
!> the argument at point with the completion function of the command:
!> a function registered with `pcomplete_command_functions' for the
!> command's name (pcomplete/NAME upstream), else
!> `pcomplete_default_completion_function'; the command name itself is
!> completed by `pcomplete_command_completion_function'.
!>
!> A completion function walks the arguments with `pcomplete_here': it
!> returns true and moves to the next argument while the final argument
!> is not reached, and at the final argument gives the completion table
!> of its FORM and returns false (upstream's return to
!> `pcomplete-completions').  A completion function returns as soon as
!> `pcomplete_here' returns false or `pcomplete_thrown' is true (no
!> completions: upstream's return to `pcompleted').
!>
!> Deferred: pcomplete's own UI (`pcomplete', `pcomplete_reverse',
!> `pcomplete_expand_and_complete', `pcomplete_continue',
!> `pcomplete_expand', `pcomplete_help', `pcomplete_list', cycling,
!> `pcomplete_show_completions', `pcomplete_insert_entry',
!> `pcomplete_do_complete', `pcomplete_stub') and so
!> `pcomplete_comint_setup' and `pcomplete_shell_setup', which install
!> it; `pcomplete_opt', `pcomplete_from_help',
!> `pcomplete_here_using_help', `pcomplete_uniquify_list',
!> `pcomplete_process_result' and host name completion, used by the
!> pcmpl-*.el command completions, which are not ported; argument lists
!> computed by the parse function (Eshell's globs); the
!> `pcomplete_annotation' and `pcomplete_help' properties of
!> candidates; `pcomplete_compare_entry_function' (cycling is not
!> ported); completion-table-case-fold.
module pcomplete_mod
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use error_mod, only: status_type
    use value_mod, only: value_type, make_list, list, list_elements, list_length, list_elt, xstring, xfixnum, vtag, &
                         V_INTEGER
    use data_mod, only: string_setting_type, logical_setting_type, forward_setting, append_locals_clearer
    use buffer_mod, only: buffers, current
    use editfns_mod, only: point, point_max, goto_char, buffer_substring, delete_region, is_eol, char_after
    use insdel_mod, only: insert
    use syntax_mod, only: skip_chars_forward
    use search_mod, only: string_match, match_beginning, match_end, looking_at, regexp_quote
    use subr_mod, only: match_string
    use fns_mod, only: substring, schars
    use env_mod, only: getenv
    use fileio_mod, only: file_exists, is_file_directory, is_file_executable, directory_file_name, &
                          file_name_nondirectory
    use files_mod, only: file_truename
    use hooks_mod, only: hook_type
    use xdisp_mod, only: message
    use process_mod, only: process_arg_type
    use minibuf_table_mod, only: completion_table_type, completion_test_procedure, completion_function_table, &
                                 completion_table_with_quoting, capf_data_type, completion_unquote_procedure, &
                                 completion_requote_procedure, COMPLETE_ALL, COMPLETE_BOUNDARIES, COMPLETION_FINISHED
    use minibuf_mod, only: complete_with_action, completion_table_subvert, completion_table_with_test
    use dired_mod, only: completion_ignored_extensions
    use comint_mod, only: comint_completion_file_name_table, comint_unquote_argument, comint_requote_argument, &
                          comint_bol
    implicit none(type, external)
    private

    public :: pcomplete_here, pcomplete_here_star, pcomplete_thrown, pcomplete_arg, pcomplete_actual_arg, &
              pcomplete_begin, pcomplete_next_arg, pcomplete_command_name, pcomplete_match, pcomplete_match_string, &
              pcomplete_test, pcomplete_parse_buffer_arguments, pcomplete_parse_comint_arguments, &
              pcomplete_parse_arguments, pcomplete_entries, pcomplete_all_entries, pcomplete_dirs, &
              pcomplete_executables, pcomplete_dirs_or_entries, pcomplete_find_completion_function, &
              pcomplete_completions, pcomplete_completions_at_point, pcomplete_default_exit_function, &
              pcomplete_ignore, init_pcomplete
    public :: pcomplete_function_procedure, pcomplete_form_procedure, pcomplete_parse_procedure, pcomplete_name_procedure, &
              pcomplete_norm_procedure, pcomplete_file_test_procedure

    abstract interface
        !> A completion function: pcomplete/NAME, and
        !> `pcomplete_command_completion_function' and
        !> `pcomplete_default_completion_function'.
        recursive subroutine pcomplete_function_procedure(status)
            import :: status_type
            implicit none(type, external)
            type(status_type), intent(out) :: status
        end subroutine pcomplete_function_procedure

        !> The FORM of `pcomplete_here': the completion TABLE for the
        !> final argument (FOUND .false. for none).
        recursive subroutine pcomplete_form_procedure(table, found)
            import :: completion_table_type
            implicit none(type, external)
            type(completion_table_type), intent(out) :: table
            logical, intent(out) :: found
        end subroutine pcomplete_form_procedure

        !> A `pcomplete_parse_arguments_function': the arguments ARGS
        !> (their final values) and the positions BEGINS where they
        !> begin, unallocated ARGS for none.
        recursive subroutine pcomplete_parse_procedure(args, begins)
            import :: process_arg_type, int64
            implicit none(type, external)
            type(process_arg_type), allocatable, intent(out) :: args(:)
            integer(int64), allocatable, intent(out) :: begins(:)
        end subroutine pcomplete_parse_procedure

        !> A `pcomplete_command_name_function': the current command NAME.
        recursive subroutine pcomplete_name_procedure(name)
            implicit none(type, external)
            character(len=:), allocatable, intent(out) :: name
        end subroutine pcomplete_name_procedure

        !> A paring function of `pcomplete_here': ARG normalized as NORM.
        recursive subroutine pcomplete_norm_procedure(arg, norm)
            implicit none(type, external)
            character(len=*), intent(in) :: arg
            character(len=:), allocatable, intent(out) :: norm
        end subroutine pcomplete_norm_procedure

        !> A test on file names (`pcomplete_entries').
        recursive logical function pcomplete_file_test_procedure(file)
            implicit none(type, external)
            character(len=*), intent(in) :: file
        end function pcomplete_file_test_procedure
    end interface

    !> Values of PARING for `pcomplete_here': the argument is pared
    !> against previous arguments normalized with `file_truename' or the
    !> PARING_FUNCTION (PCOMPLETE_PARING_DEFAULT); it does not participate in paring
    !> (PCOMPLETE_PARING_NONE); all previous arguments seen are cleared (PCOMPLETE_PARING_CLEAR).
    integer, parameter, public :: PCOMPLETE_PARING_DEFAULT = 0, PCOMPLETE_PARING_NONE = 1, PCOMPLETE_PARING_CLEAR = 2

    !> Values of WHICH for `pcomplete_arg' and `pcomplete_begin': an
    !> index relative to the current argument, or the special indices
    !> `first' and `last'.
    integer, parameter, public :: PCOMPLETE_ARG_RELATIVE = 0, PCOMPLETE_ARG_FIRST = 1, PCOMPLETE_ARG_LAST = 2

    ! What was thrown: nothing, `pcomplete-completions' or `pcompleted'.
    integer, parameter :: THROWN_NONE = 0, THROWN_COMPLETIONS = 1, THROWN_PCOMPLETED = 2

    !> A regexp of filenames to be disregarded during file completion
    !> (unallocated when unset).  Port of pcomplete.el pcomplete-file-ignore.
    type(string_setting_type), target, public :: pcomplete_file_ignore = string_setting_type(name="pcomplete_file_ignore")

    !> A regexp of names to be disregarded during directory completion
    !> (unallocated when unset).  Port of pcomplete.el pcomplete-dir-ignore.
    type(string_setting_type), target, public :: pcomplete_dir_ignore = string_setting_type(name="pcomplete_dir_ignore")

    !> Whether to ignore remote file names.  Port of pcomplete.el
    !> pcomplete-remote-file-ignore (remote files are not ported).
    type(logical_setting_type), target, public :: pcomplete_remote_file_ignore = &
                                                  logical_setting_type(name="pcomplete_remote_file_ignore", value=.false.)

    !> If true, automatically list possibilities on partial completion.
    !> This mirrors the optional behavior of tcsh.  Port of pcomplete.el
    !> pcomplete-autolist.
    type(logical_setting_type), target, public :: pcomplete_autolist = &
                                                  logical_setting_type(name="pcomplete_autolist", value=.false.)

    !> If true, use shortest completion if characters cannot be added.
    !> This mirrors the optional behavior of tcsh.
    !>
    !> A true value is useful if `pcomplete_autolist' is true too.  Port
    !> of pcomplete.el pcomplete-recexact.
    type(logical_setting_type), target, public :: pcomplete_recexact = &
                                                  logical_setting_type(name="pcomplete_recexact", value=.false.)

    !> If true, expand the current argument before completing it.
    !> This means that typing something such as `$HOME/bi' followed by
    !> completion will cause the variable reference to be resolved
    !> first, and the resultant value that will be completed against to
    !> be inserted in the buffer.  Note that exactly what gets expanded
    !> and how is entirely up to the behavior of the
    !> `pcomplete_parse_arguments_function'.  Port of pcomplete.el
    !> pcomplete-expand-before-complete.
    type(logical_setting_type), target, public :: pcomplete_expand_before_complete = &
                                                  logical_setting_type(name="pcomplete_expand_before_complete", value=.false.)

    !> If true, allow effects in `pcomplete_parse_arguments_function'.
    !> For `completion_at_point_functions' this is false, and it tells
    !> `pcomplete_parse_arguments_function' whether it can do
    !> modifications to the buffer, like expanding variables and such,
    !> or whether it should refrain from doing so.  Port of pcomplete.el
    !> pcomplete-allow-modifications.
    logical, public :: pcomplete_allow_modifications = .false.

    !> If true, pare alternatives that have already been used.
    !> If false, you will always see the completion set of possible
    !> options, no matter which of those options have already been used
    !> in previous command arguments.  Port of pcomplete.el
    !> pcomplete-use-paring.
    type(logical_setting_type), target, public :: pcomplete_use_paring = &
                                                  logical_setting_type(name="pcomplete_use_paring", value=.true.)

    !> A string that is inserted after any completion or expansion.
    !> This is usually a space character, useful when completing lists of
    !> words separated by spaces.  However, if your list uses a different
    !> separator character, or if the completion occurs in a word that is
    !> already terminated by a character, this variable should be locally
    !> modified to be an empty string, or the desired separation string.
    !> Port of pcomplete.el pcomplete-termination-string.
    type(string_setting_type), target, public :: pcomplete_termination_string = &
                                                 string_setting_type(name="pcomplete_termination_string")

    !> The name of the /etc/hosts file (unallocated for none).  Port of
    !> pcomplete.el pcomplete-hosts-file.
    type(string_setting_type), target, public :: pcomplete_hosts_file = string_setting_type(name="pcomplete_hosts_file")

    !> A list of functions which are called before completing an
    !> argument.  This can be used, for example, for completing things
    !> which might apply to all arguments, such as variable names after
    !> a $.  Port of pcomplete.el pcomplete-try-first-hook.
    type(hook_type), target, public :: pcomplete_try_first_hook = hook_type(name="pcomplete_try_first_hook")

    !> A function variable of pcomplete: a default `value', and the
    !> buffers' own values set with `set_local'.
    type, public :: pcomplete_function_variable_type
        character(len=48) :: name = ""
        procedure(pcomplete_function_procedure), pointer, nopass :: value => null()
    contains
        !> Make FN the current buffer's value (set_local).
        procedure :: set_local => function_set_local
        !> The current buffer's value.
        procedure :: current => current_function
    end type pcomplete_function_variable_type

    !> The `pcomplete_parse_arguments_function' variable: a default
    !> `value', and the buffers' own values set with `set_local'.
    type, public :: pcomplete_parse_variable_type
        character(len=48) :: name = ""
        procedure(pcomplete_parse_procedure), pointer, nopass :: value => null()
    contains
        !> Make FN the current buffer's value (set_local).
        procedure, nopass :: set_local => parse_set_local
        !> The current buffer's value.
        procedure :: current => current_parse
    end type pcomplete_parse_variable_type

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
    type(pcomplete_parse_variable_type), public :: pcomplete_parse_arguments_function = &
                                                   pcomplete_parse_variable_type(name="pcomplete_parse_arguments_function")

    !> Function called for completing the initial command argument.
    !> Port of pcomplete.el pcomplete-command-completion-function.
    type(pcomplete_function_variable_type), public :: pcomplete_command_completion_function = &
                                                      pcomplete_function_variable_type( &
                                                      name="pcomplete_command_completion_function")

    !> Function called when no completion rule can be found.
    !> This function is used to generate completions for every argument.
    !> Port of pcomplete.el pcomplete-default-completion-function.
    type(pcomplete_function_variable_type), public :: pcomplete_default_completion_function = &
                                                      pcomplete_function_variable_type( &
                                                      name="pcomplete_default_completion_function")

    !> Function called for determining the current command name.  Port of
    !> pcomplete.el pcomplete-command-name-function.
    procedure(pcomplete_name_procedure), pointer, public :: pcomplete_command_name_function => null()

    abstract interface
        !> An exit function: run after completion of STRING, FINISHED
        !> saying how (a COMPLETION_* value).
        recursive subroutine pcomplete_exit_procedure(string, finished)
            implicit none(type, external)
            character(len=*), intent(in) :: string
            integer, intent(in) :: finished
        end subroutine pcomplete_exit_procedure
    end interface

    !> The exit function to call in `pcomplete_completions_at_point'.
    !>
    !> This variable is bound in `pcomplete_completions_at_point', so you
    !> can change it in order to adjust the behavior for a specific
    !> completion, for example in a `pcomplete_try_first_hook' function.
    !> Port of pcomplete.el pcomplete-exit-function.
    procedure(pcomplete_exit_procedure), pointer, public :: pcomplete_exit_function => null()

    !> The function unquoting the argument being completed
    !> (pcomplete-unquote-argument-function).
    procedure(completion_unquote_procedure), pointer, public :: pcomplete_unquote_argument_function => comint_unquote_argument

    !> The function requoting completions for the argument being
    !> completed (pcomplete-requote-argument-function).
    procedure(completion_requote_procedure), pointer, public :: pcomplete_requote_argument_function => comint_requote_argument

    ! The exit function `pcomplete_completions_at_point' was called with.
    procedure(pcomplete_exit_procedure), pointer :: capf_exit_function => null()

    public :: pcomplete_exit_procedure

    !> A completion function for a command (pcomplete/NAME), or for a
    !> command in a major mode (pcomplete/MODE/NAME).
    type, public :: pcomplete_command_function_type
        character(len=64) :: command = ""
        character(len=64) :: mode = ""
        procedure(pcomplete_function_procedure), pointer, nopass :: fn => null()
    end type pcomplete_command_function_type

    !> The completion functions of commands (upstream, the functions
    !> named pcomplete/NAME and pcomplete/MODE/NAME), added with `add'.
    type, public :: pcomplete_command_functions_type
        type(pcomplete_command_function_type), allocatable :: entries(:)
    contains
        !> Make FN the completion function of COMMAND (in major mode MODE).
        procedure :: append => command_functions_append
    end type pcomplete_command_functions_type

    !> The completion functions of commands (pcomplete/NAME).
    type(pcomplete_command_functions_type), public :: pcomplete_command_functions

    ! The buffers' own function values.
    type :: local_functions_type
        integer(int64) :: serial = 0
        logical :: has_command = .false., has_default = .false., has_parse = .false.
        procedure(pcomplete_function_procedure), pointer, nopass :: command => null()
        procedure(pcomplete_function_procedure), pointer, nopass :: default => null()
        procedure(pcomplete_parse_procedure), pointer, nopass :: parse => null()
    end type local_functions_type
    type(local_functions_type), allocatable :: local_functions(:)

    ! pcomplete-args, pcomplete-begins, pcomplete-last, pcomplete-index,
    ! pcomplete-stub, pcomplete-seen and pcomplete-norm-func.
    type(process_arg_type), allocatable :: args(:)
    integer(int64), allocatable :: begins(:)
    !> The index of the last argument (pcomplete-last).
    integer, public, protected :: pcomplete_last = 0
    !> The index of the argument being examined: 1 is the first argument
    !> after the command name (pcomplete-index).
    integer, public, protected :: pcomplete_index = 0
    !> The text being completed (pcomplete-stub).
    character(len=:), allocatable, public, protected :: pcomplete_stub
    type(process_arg_type), allocatable :: seen(:)
    procedure(pcomplete_norm_procedure), pointer :: norm_func => null()

    ! What was thrown, and the completion table thrown with
    ! `pcomplete-completions' (none if not `has_thrown_table').
    integer :: thrown = THROWN_NONE
    type(completion_table_type) :: thrown_table
    logical :: has_thrown_table = .false.

    ! The closures of the tables of `pcomplete_entries' (the last one
    ! made): its regexp, test and the ignore regexps it captured.
    character(len=:), allocatable :: entries_regexp, entries_file_ignore, entries_dir_ignore
    logical :: entries_has_regexp = .false., entries_has_file_ignore = .false., entries_has_dir_ignore = .false.
    procedure(pcomplete_file_test_procedure), pointer :: entries_test => null()
    ! The closure of `pcomplete_dirs_or_entries'.
    character(len=:), allocatable :: doe_regexp
    logical :: doe_has_regexp = .false.
    procedure(pcomplete_file_test_procedure), pointer :: doe_test => null()

    ! The closure of the table of `pcomplete_completions_at_point': the
    ! completions, and whether they are subverted from S1 to S2.
    type(completion_table_type) :: capf_completions
    logical :: capf_subvert = .false.
    character(len=:), allocatable :: capf_s1, capf_s2
    ! The closure of its test: the arguments seen, normalized.
    type(process_arg_type), allocatable :: capf_seen(:)
    procedure(pcomplete_norm_procedure), pointer :: capf_norm_func => null()

    ! pcomplete--env-regexp.
    character(len=*), parameter :: ENV_REGEXP = &
                                   "\(?:\`\|[^\]\)\(?:\\\\\)*\(\$\(?:{\([^}]+\)}\|\(?2:[[:alnum:]_]+\)\)\)"

contains

    ! ------------------------------------------------------------------
    ! Function variables
    ! ------------------------------------------------------------------

    ! The current buffer's slot in `local_functions' (made if CREATE).
    ! Not a port.
    integer function local_slot(create) result(k)
        logical, intent(in) :: create
        type(local_functions_type), allocatable :: grown(:)
        integer :: i, n

        k = 0
        if (current == 0) return
        if (.not. allocated(local_functions)) allocate (local_functions(0))

        do i = 1, size(local_functions)
            if (local_functions(i)%serial == buffers(current)%serial) then
                k = i
                return
            end if
        end do

        if (.not. create) return

        do i = 1, size(local_functions)
            if (local_functions(i)%serial == 0) then
                k = i
                local_functions(k)%serial = buffers(current)%serial
                return
            end if
        end do

        n = size(local_functions)
        allocate (grown(n + 1))
        grown(1:n) = local_functions
        grown(n + 1)%serial = buffers(current)%serial
        call move_alloc(grown, local_functions)
        k = n + 1
    end function local_slot

    ! Forget the current buffer's values (kill-all-local-variables).
    ! Not a port.
    subroutine clear_pcomplete_locals()
        integer :: k

        k = local_slot(.false.)
        if (k > 0) local_functions(k) = local_functions_type()
    end subroutine clear_pcomplete_locals

    ! Set SELF in the current buffer (setq-local).  Not a port.
    subroutine function_set_local(self, fn)
        class(pcomplete_function_variable_type), intent(in) :: self
        procedure(pcomplete_function_procedure) :: fn
        integer :: k

        k = local_slot(.true.)
        if (k == 0) return

        if (self%name == "pcomplete_command_completion_function") then
            local_functions(k)%command => fn
            local_functions(k)%has_command = .true.
        else
            local_functions(k)%default => fn
            local_functions(k)%has_default = .true.
        end if
    end subroutine function_set_local

    ! The current buffer's value of SELF.  Not a port.
    subroutine current_function(self, fn)
        class(pcomplete_function_variable_type), intent(in) :: self
        procedure(pcomplete_function_procedure), pointer, intent(out) :: fn
        integer :: k

        fn => self%value
        k = local_slot(.false.)
        if (k == 0) return

        if (self%name == "pcomplete_command_completion_function") then
            if (local_functions(k)%has_command) fn => local_functions(k)%command
        else
            if (local_functions(k)%has_default) fn => local_functions(k)%default
        end if
    end subroutine current_function

    ! Set `pcomplete_parse_arguments_function' to FN in the current
    ! buffer (setq-local).  Not a port.
    subroutine parse_set_local(fn)
        procedure(pcomplete_parse_procedure) :: fn
        integer :: k

        k = local_slot(.true.)
        if (k == 0) return
        local_functions(k)%parse => fn
        local_functions(k)%has_parse = .true.
    end subroutine parse_set_local

    ! The current buffer's value of SELF.  Not a port.
    subroutine current_parse(self, fn)
        class(pcomplete_parse_variable_type), intent(in) :: self
        procedure(pcomplete_parse_procedure), pointer, intent(out) :: fn
        integer :: k

        fn => self%value
        k = local_slot(.false.)
        if (k == 0) return
        if (local_functions(k)%has_parse) fn => local_functions(k)%parse
    end subroutine current_parse

    ! Add the completion function FN for COMMAND (in major mode MODE).
    ! Not a port.
    subroutine command_functions_append(self, command, fn, mode)
        class(pcomplete_command_functions_type), intent(inout) :: self
        character(len=*), intent(in) :: command
        procedure(pcomplete_function_procedure) :: fn
        character(len=*), intent(in), optional :: mode
        type(pcomplete_command_function_type), allocatable :: grown(:)
        integer :: n

        if (.not. allocated(self%entries)) allocate (self%entries(0))
        n = size(self%entries)
        allocate (grown(n + 1))
        grown(1:n) = self%entries
        grown(n + 1)%command = command
        if (present(mode)) grown(n + 1)%mode = mode
        grown(n + 1)%fn => fn
        call move_alloc(grown, self%entries)
    end subroutine command_functions_append

    !> Do nothing (#'ignore, as a completion function).  Not a port.
    recursive subroutine pcomplete_ignore(status)
        type(status_type), intent(out) :: status

        status = status_type()
    end subroutine pcomplete_ignore

    ! The default `pcomplete_command_completion_function'.  Port of
    ! pcomplete.el:pcomplete-command-completion-function (its default).
    recursive subroutine default_command_completion(status)
        type(status_type), intent(out) :: status

        status = status_type()
        if (pcomplete_here(executables_form)) continue
    end subroutine default_command_completion

    ! The default `pcomplete_default_completion_function'.  Port of
    ! pcomplete.el:pcomplete-default-completion-function (its default).
    recursive subroutine default_default_completion(status)
        type(status_type), intent(out) :: status

        status = status_type()

        do while (pcomplete_here(entries_form))
        end do
    end subroutine default_default_completion

    ! (pcomplete-executables) as a FORM.  Not a port.
    recursive subroutine executables_form(table, found)
        type(completion_table_type), intent(out) :: table
        logical, intent(out) :: found

        call pcomplete_executables(table)
        found = .true.
    end subroutine executables_form

    ! (pcomplete-entries) as a FORM.  Not a port.
    recursive subroutine entries_form(table, found)
        type(completion_table_type), intent(out) :: table
        logical, intent(out) :: found

        call pcomplete_entries(table)
        found = .true.
    end subroutine entries_form

    ! ------------------------------------------------------------------
    ! Arguments
    ! ------------------------------------------------------------------

    !> Whether completion was abandoned (upstream, `pcompleted' was
    !> returned) or its table given (`pcomplete-completions' returned): a
    !> completion function returns at once when this is true.  Not a port.
    logical function pcomplete_thrown()
        pcomplete_thrown = thrown /= THROWN_NONE
    end function pcomplete_thrown

    ! The 0-based argument index WHICH, INDEX and OFFSET denote.  Not a port.
    integer function arg_position(which, index, offset) result(n)
        integer, intent(in), optional :: which, index, offset
        integer :: w

        w = PCOMPLETE_ARG_RELATIVE
        if (present(which)) w = which

        select case (w)
        case (PCOMPLETE_ARG_FIRST)
            n = 0
        case (PCOMPLETE_ARG_LAST)
            n = pcomplete_last
        case default
            n = pcomplete_index
            if (present(index)) n = pcomplete_index - index
        end select

        if (present(offset)) n = n + offset
    end function arg_position

    !> Return the textual content of the INDEXth argument (FOUND false if
    !> INDEX is outside of the argument list).  INDEX is based from the
    !> current processing position.  If INDEX is positive, values
    !> returned are closer to the command argument; if negative, they are
    !> closer to the last argument.  The default value for INDEX is 0,
    !> meaning the current argument being examined.
    !>
    !> WHICH PCOMPLETE_ARG_FIRST and PCOMPLETE_ARG_LAST give the special indices
    !> `first' and `last'.
    !>
    !> The OFFSET argument is added to/taken away from the index that will
    !> be used.  This is really only useful with `first' and `last', for
    !> accessing absolute argument positions.  Port of
    !> pcomplete.el:pcomplete-arg.
    function pcomplete_arg(index, offset, which, found) result(arg)
        integer, intent(in), optional :: index, offset, which
        logical, intent(out), optional :: found
        character(len=:), allocatable :: arg
        integer :: n

        arg = ""
        if (present(found)) found = .false.
        if (.not. allocated(args)) return
        n = arg_position(which, index, offset)
        if (n < 0 .or. n >= size(args)) return
        arg = args(n + 1)%s
        if (present(found)) found = .true.
    end function pcomplete_arg

    !> Return the beginning position of the INDEXth argument (0 if
    !> none).  See the documentation for `pcomplete_arg'.  Port of
    !> pcomplete.el:pcomplete-begin.
    integer(int64) function pcomplete_begin(index, offset, which) result(pos)
        integer, intent(in), optional :: index, offset, which
        integer :: n

        pos = 0
        if (.not. allocated(begins)) return
        n = arg_position(which, index, offset)
        if (n < 0 .or. n >= size(begins)) return
        pos = begins(n + 1)
    end function pcomplete_begin

    !> Return the actual text representation of the last argument.
    !> This is different from `pcomplete_arg', which returns the textual
    !> value that the last argument evaluated to.  This function returns
    !> what the user actually typed in.  Port of
    !> pcomplete.el:pcomplete-actual-arg.
    function pcomplete_actual_arg(index, offset, which) result(text)
        integer, intent(in), optional :: index, offset, which
        character(len=:), allocatable :: text
        type(status_type) :: st

        call buffer_substring(pcomplete_begin(index, offset, which), point(), text, st)
    end function pcomplete_actual_arg

    !> Move the various pointers to the next argument.  Port of
    !> pcomplete.el:pcomplete-next-arg.
    recursive subroutine pcomplete_next_arg()
        pcomplete_index = pcomplete_index + 1
        pcomplete_stub = pcomplete_arg()

        if (pcomplete_index > pcomplete_last) then
            call message("No completions")
            thrown = THROWN_PCOMPLETED
        end if
    end subroutine pcomplete_next_arg

    !> Return the command name of the first argument.  Port of
    !> pcomplete.el:pcomplete-command-name.
    recursive subroutine pcomplete_command_name(name)
        character(len=:), allocatable, intent(out) :: name

        name = file_name_nondirectory(pcomplete_arg(which=PCOMPLETE_ARG_FIRST))
    end subroutine pcomplete_command_name

    !> Like `string_match', but on the current completion argument:
    !> INDEX defaults to 1 (the previous argument).  Without such an
    !> argument, completion is abandoned (see `pcomplete_thrown').
    !> Port of pcomplete.el:pcomplete-match.
    recursive logical function pcomplete_match(regexp, index, offset, start, which) result(matched)
        character(len=*), intent(in) :: regexp
        integer, intent(in), optional :: index, offset, which
        integer(int64), intent(in), optional :: start
        character(len=:), allocatable :: arg
        type(status_type) :: st
        integer :: i
        logical :: found

        matched = .false.
        i = 1
        if (present(index)) i = index
        arg = pcomplete_arg(i, offset, which, found)

        if (.not. found) then
            thrown = THROWN_PCOMPLETED
            return
        end if

        if (present(start)) then
            matched = string_match(regexp, arg, st, start=start + 1) > 0
        else
            matched = string_match(regexp, arg, st) > 0
        end if
    end function pcomplete_match

    !> Like `match_string', but on the current completion argument:
    !> INDEX defaults to 1.  Port of pcomplete.el:pcomplete-match-string.
    recursive function pcomplete_match_string(which_group, index, offset, which) result(text)
        integer, intent(in) :: which_group
        integer, intent(in), optional :: index, offset, which
        character(len=:), allocatable :: text
        character(len=:), allocatable :: arg
        integer :: i
        logical :: found

        text = ""
        i = 1
        if (present(index)) i = index
        arg = pcomplete_arg(i, offset, which, found)

        if (.not. found) then
            thrown = THROWN_PCOMPLETED
            return
        end if

        text = match_string(which_group, arg)
    end function pcomplete_match_string

    !> Whether the current programmable argument (INDEX defaults to 1)
    !> satisfies TEST or matches one of REGEXPS entirely.  Port of
    !> pcomplete.el:pcomplete-test (tests are a function or
    !> regexps; t is TEST `pcomplete_always').
    recursive logical function pcomplete_test(test, regexps, index, offset, which) result(ok)
        procedure(pcomplete_file_test_procedure), optional :: test
        character(len=*), intent(in), optional :: regexps(:)
        integer, intent(in), optional :: index, offset, which
        character(len=:), allocatable :: arg
        type(status_type) :: st
        integer :: i, k

        ok = .false.
        i = 1
        if (present(index)) i = index
        arg = pcomplete_arg(i, offset, which)

        if (present(test)) then
            ok = test(arg)
            if (ok) return
        end if

        if (present(regexps)) then
            do k = 1, size(regexps)
                if (string_match("^"//trim(regexps(k))//"$", arg, st) > 0) then
                    ok = .true.
                    return
                end if
            end do
        end if
    end function pcomplete_test

    !> Parse whitespace separated arguments in the current region.
    !> Port of pcomplete.el:pcomplete-parse-buffer-arguments.
    recursive subroutine pcomplete_parse_buffer_arguments(args, begins)
        type(process_arg_type), allocatable, intent(out) :: args(:)
        integer(int64), allocatable, intent(out) :: begins(:)
        type(status_type) :: st
        integer(int64) :: opoint, start, moved
        character(len=:), allocatable :: text

        opoint = point()
        allocate (args(0), begins(0))
        call goto_char(1_int64)

        do while (point() < point_max())
            moved = skip_chars_forward(" "//achar(9)//achar(10), st)
            start = point()
            moved = skip_chars_forward("^ "//achar(9)//achar(10), st)
            call buffer_substring(start, point(), text, st)
            call append_arg(args, begins, text, start)
        end do

        call goto_char(opoint)
    end subroutine pcomplete_parse_buffer_arguments

    !> Parse whitespace separated arguments in the current region (from
    !> the beginning of the input to point).  Port of
    !> pcomplete.el:pcomplete-parse-comint-arguments.
    recursive subroutine pcomplete_parse_comint_arguments(args, begins)
        type(process_arg_type), allocatable, intent(out) :: args(:)
        integer(int64), allocatable, intent(out) :: begins(:)
        type(status_type) :: st
        integer(int64) :: opoint, begin, start, moved
        integer(int32) :: c
        character(len=:), allocatable :: text
        logical :: have

        opoint = point()
        call comint_bol(.false.)
        begin = point()
        allocate (args(0), begins(0))
        call goto_char(begin)

        do while (point() < opoint)
            moved = skip_chars_forward(" "//achar(9)//achar(10), st)
            start = point()

            do
                moved = skip_chars_forward("^ "//achar(9)//achar(10)//"\\", st)
                call char_after(c, have)
                if (.not. have .or. c /= iachar("\")) exit
                call goto_char(point() + 1)
                if (is_eol()) exit
                call goto_char(point() + 1)
            end do

            call buffer_substring(start, point(), text, st)
            call append_arg(args, begins, text, start)
        end do

        call goto_char(opoint)
    end subroutine pcomplete_parse_comint_arguments

    ! Add TEXT beginning at START to ARGS and BEGINS.  Not a port.
    subroutine append_arg(args, begins, text, start)
        type(process_arg_type), allocatable, intent(inout) :: args(:)
        integer(int64), allocatable, intent(inout) :: begins(:)
        character(len=*), intent(in) :: text
        integer(int64), intent(in) :: start
        type(process_arg_type), allocatable :: grown(:)
        integer :: n, k

        n = size(args)
        allocate (grown(n + 1))

        do k = 1, n
            call move_alloc(args(k)%s, grown(k)%s)
        end do

        grown(n + 1)%s = text
        call move_alloc(grown, args)
        begins = [begins, start]
    end subroutine append_arg

    !> Parse the command line arguments.  Most completions need this
    !> info.  Returns false when there are none.  Port of
    !> pcomplete.el:pcomplete-parse-arguments (no argument lists computed
    !> during parsing).
    recursive logical function pcomplete_parse_arguments(expand_p) result(ok)
        logical, intent(in) :: expand_p
        procedure(pcomplete_parse_procedure), pointer :: fn
        type(process_arg_type), allocatable :: parsed(:)
        integer(int64), allocatable :: parsed_begins(:)
        type(status_type) :: st
        integer(int64) :: begin

        ok = .false.
        call pcomplete_parse_arguments_function%current(fn)
        if (.not. associated(fn)) return
        call fn(parsed, parsed_begins)
        if (.not. allocated(parsed)) return

        if (size(parsed) == 0) then
            deallocate (parsed)
            allocate (parsed(1))
            parsed(1)%s = ""
        end if

        if (.not. allocated(parsed_begins)) allocate (parsed_begins(0))
        if (size(parsed_begins) == 0) parsed_begins = [point()]
        call move_alloc(parsed, args)
        call move_alloc(parsed_begins, begins)
        pcomplete_last = size(args) - 1
        pcomplete_index = 0
        pcomplete_stub = pcomplete_arg(which=PCOMPLETE_ARG_LAST)
        begin = pcomplete_begin(which=PCOMPLETE_ARG_LAST)

        if (expand_p .and. begin > 0) then
            call delete_region(begin, point(), st)
            call insert(pcomplete_stub, st)
        end if

        ok = .true.
    end function pcomplete_parse_arguments

    ! ------------------------------------------------------------------
    ! File-system completion lists
    ! ------------------------------------------------------------------

    !> Complete against a list of directory candidates: TABLE.
    !> If REGEXP is present, it is a regular expression used to refine the
    !> match (files not matching the REGEXP will be excluded).
    !> If TEST is present, it will also be used to refine the match
    !> (files for which the TEST returns false will be excluded).
    !> If no directory information can be extracted from the completed
    !> component, `default_directory' is used as the basis for
    !> completion.  Port of pcomplete.el:pcomplete-entries (with
    !> pcomplete--entries).
    recursive subroutine pcomplete_entries(table, regexp, test)
        type(completion_table_type), intent(out) :: table
        character(len=*), intent(in), optional :: regexp
        procedure(pcomplete_file_test_procedure), optional :: test

        ! Capture the values for later use.
        entries_has_regexp = present(regexp)
        if (present(regexp)) entries_regexp = regexp
        entries_test => null()
        if (present(test)) entries_test => test
        entries_has_file_ignore = allocated(pcomplete_file_ignore%value)
        if (entries_has_file_ignore) entries_file_ignore = pcomplete_file_ignore%value
        entries_has_dir_ignore = allocated(pcomplete_dir_ignore%value)
        if (entries_has_dir_ignore) entries_dir_ignore = pcomplete_dir_ignore%value
        table = completion_function_table(entries_table, "file")
    end subroutine pcomplete_entries

    !> Like `pcomplete_entries', but doesn't ignore any entries.  Port of
    !> pcomplete.el:pcomplete-all-entries.
    recursive subroutine pcomplete_all_entries(table, regexp, test)
        type(completion_table_type), intent(out) :: table
        character(len=*), intent(in), optional :: regexp
        procedure(pcomplete_file_test_procedure), optional :: test

        call pcomplete_entries(table, regexp, test)
        entries_has_file_ignore = .false.
        entries_has_dir_ignore = .false.
    end subroutine pcomplete_all_entries

    !> Complete amongst a list of directories.  Port of
    !> pcomplete.el:pcomplete-dirs.
    recursive subroutine pcomplete_dirs(table, regexp)
        type(completion_table_type), intent(out) :: table
        character(len=*), intent(in), optional :: regexp

        call pcomplete_entries(table, regexp, is_directory)
    end subroutine pcomplete_dirs

    !> Complete amongst a list of directories and executables.  Port of
    !> pcomplete.el:pcomplete-executables.
    recursive subroutine pcomplete_executables(table, regexp)
        type(completion_table_type), intent(out) :: table
        character(len=*), intent(in), optional :: regexp

        call pcomplete_entries(table, regexp, is_executable)
    end subroutine pcomplete_executables

    !> Return either directories, or qualified entries.  Port of
    !> pcomplete.el:pcomplete-dirs-or-entries.
    recursive subroutine pcomplete_dirs_or_entries(table, regexp, test)
        type(completion_table_type), intent(out) :: table
        character(len=*), intent(in), optional :: regexp
        procedure(pcomplete_file_test_procedure), optional :: test

        doe_has_regexp = present(regexp)
        if (present(regexp)) doe_regexp = regexp
        doe_test => null()
        if (present(test)) doe_test => test
        call pcomplete_entries(table, test=is_dir_or_entry)
    end subroutine pcomplete_dirs_or_entries

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

    ! The test of `pcomplete_dirs_or_entries'.  Port of
    ! pcomplete.el:pcomplete-dirs-or-entries, in part.
    recursive logical function is_dir_or_entry(file) result(ok)
        character(len=*), intent(in) :: file
        type(status_type) :: st

        ok = is_file_directory(file)
        if (ok) return

        if (doe_has_regexp) then
            if (string_match(doe_regexp, file, st) == 0) return
        end if

        if (associated(doe_test)) then
            ok = doe_test(file)
        else
            ok = .true.
        end if
    end function is_dir_or_entry

    ! The test of the table of `pcomplete_entries' (ign-pred,
    ! reg-pred and TEST or `file_exists').  Port of
    ! pcomplete.el:pcomplete--entries, in part.
    recursive logical function entries_filter(elt) result(ok)
        type(value_type), intent(in) :: elt
        character(len=:), allocatable :: file
        type(status_type) :: st

        ok = .false.
        file = xstring(elt)

        if (entries_has_regexp) then
            if (string_match(entries_regexp, file, st) == 0) return
        end if

        if (len(file) > 0) then
            if (file(len(file):len(file)) == "/") then
                if (entries_has_dir_ignore) then
                    if (string_match(entries_dir_ignore, file, st) > 0) return
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

    ! TEST of `pcomplete_entries' alone, as a completion test.
    ! Not a port.
    recursive logical function entries_test_only(elt)
        type(value_type), intent(in) :: elt

        entries_test_only = entries_test(xstring(elt))
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
        table = completion_function_table(comint_completion_file_name_table)

        if (entries_has_regexp .or. entries_has_file_ignore .or. entries_has_dir_ignore) then
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

    ! The table of `pcomplete_entries': environment variable references
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
        type(value_type), allocatable :: elts(:)
        character(len=:), allocatable :: rest, newstring, value, usuffix
        type(status_type) :: st
        integer(int64) :: orig_length, envpos, bstart, len_env
        integer :: k
        logical :: found, expanded

        table = completion_function_table(entries_inner_table)
        orig_length = schars(string)
        rest = string
        newstring = ""
        envpos = 0
        expanded = .false.

        ! Perform env-var expansion.
        do while (string_match(ENV_REGEXP, rest, st) > 0)
            expanded = .true.
            newstring = newstring//substring(rest, 0_int64, match_beginning(1, st) - 1)
            call getenv(match_string(2, rest), value, found)
            if (.not. found) value = ""
            newstring = newstring//value
            rest = substring(rest, match_end(1, st) - 1)
        end do

        envpos = schars(newstring)
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
            if (vtag(list_elt(bounds, 1_int64)) == V_INTEGER) bstart = xfixnum(list_elt(bounds, 1_int64))
        end if

        status = status_type()

        if (bstart >= envpos) then
            ! The env-var is "out of bounds".
            if (action == COMPLETE_ALL) then
                call complete_with_action(action, table, newstring, result, status, test)
            else
                result = list(value_type(bstart + orig_length - schars(newstring)), list_elt(bounds, 2_int64))
            end if
        else
            ! The env-var is in the file bounds.
            if (action == COMPLETE_ALL) then
                call complete_with_action(action, table, newstring, comps, status, test)
                len_env = envpos - bstart
                ! Strip the part of each completion that's actually
                ! coming from the env-var.
                call list_elements(comps, elts)

                do k = 1, size(elts)
                    elts(k) = value_type(text=substring(xstring(elts(k)), len_env))
                end do

                result = make_list(elts)
            else
                result = list(value_type(envpos + orig_length - schars(newstring)), list_elt(bounds, 2_int64))
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
    recursive subroutine pcomplete_find_completion_function(command, fn)
        character(len=*), intent(in) :: command
        procedure(pcomplete_function_procedure), pointer, intent(out) :: fn
        character(len=:), allocatable :: mode
        integer :: k

        fn => null()
        if (.not. allocated(pcomplete_command_functions%entries)) return
        mode = "fundamental_mode"

        if (current /= 0) then
            if (allocated(buffers(current)%major_mode)) mode = buffers(current)%major_mode
        end if

        do k = 1, size(pcomplete_command_functions%entries)
            associate (e => pcomplete_command_functions%entries(k))
                if (trim(e%command) == command .and. trim(e%mode) == mode) then
                    fn => e%fn
                    return
                end if
            end associate
        end do

        do k = 1, size(pcomplete_command_functions%entries)
            associate (e => pcomplete_command_functions%entries(k))
                if (trim(e%command) == command .and. len_trim(e%mode) == 0) then
                    fn => e%fn
                    return
                end if
            end associate
        end do
    end subroutine pcomplete_find_completion_function

    !> Return a list of completions for the current argument position:
    !> TABLE, FOUND false if there are none.  Port of
    !> pcomplete.el:pcomplete-completions.
    recursive subroutine pcomplete_completions(table, found, status)
        type(completion_table_type), intent(out) :: table
        logical, intent(out) :: found
        type(status_type), intent(out) :: status
        procedure(pcomplete_function_procedure), pointer :: fn
        procedure(pcomplete_name_procedure), pointer :: name_fn
        character(len=:), allocatable :: name

        status = status_type()
        found = .false.
        thrown = THROWN_NONE
        has_thrown_table = .false.

        if (pcomplete_parse_arguments(pcomplete_expand_before_complete%value)) then
            if (pcomplete_index == pcomplete_last) then
                call pcomplete_command_completion_function%current(fn)
                if (associated(fn)) call fn(status)
            else
                name_fn => pcomplete_command_name_function

                if (associated(name_fn)) then
                    call name_fn(name)
                else
                    call pcomplete_command_name(name)
                end if

                call pcomplete_find_completion_function(name, fn)
                if (.not. associated(fn)) call pcomplete_default_completion_function%current(fn)
                call pcomplete_next_arg()
                if (thrown == THROWN_NONE .and. associated(fn)) call fn(status)
            end if
        end if

        if (thrown == THROWN_COMPLETIONS .and. has_thrown_table) then
            table = thrown_table
            found = .true.
        end if
    end subroutine pcomplete_completions

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
    !> If PARING is PCOMPLETE_PARING_DEFAULT, this argument will be pared
    !> against previous arguments using the function `file_truename' (or
    !> PARING_FUNCTION) to normalize them.  If PARING is
    !> PCOMPLETE_PARING_NONE, the argument dealt with by this call will
    !> not participate in argument paring.  If it is
    !> PCOMPLETE_PARING_CLEAR, all previous arguments that have been seen
    !> will be cleared.
    !>
    !> If FORM_ONLY is true, only the result of FORM will be used to
    !> generate the completions list.  This means that the hook
    !> `pcomplete_try_first_hook' will not be run.  Port of
    !> pcomplete.el:pcomplete-here (with pcomplete--here).
    recursive logical function pcomplete_here(form, stub, paring, paring_function, form_only) result(advanced)
        procedure(pcomplete_form_procedure), optional :: form
        character(len=*), intent(in), optional :: stub
        integer, intent(in), optional :: paring
        procedure(pcomplete_norm_procedure), optional :: paring_function
        logical, intent(in), optional :: form_only
        character(len=:), allocatable :: arg, norm
        type(status_type) :: st
        integer :: p
        logical :: only, found

        advanced = .false.
        if (thrown /= THROWN_NONE) return
        p = PCOMPLETE_PARING_DEFAULT
        if (present(paring)) p = paring

        if (pcomplete_index < pcomplete_last) then
            if (p == PCOMPLETE_PARING_CLEAR) then
                if (allocated(seen)) deallocate (seen)
            else if (p /= PCOMPLETE_PARING_NONE) then
                arg = pcomplete_arg()

                if (present(paring_function)) then
                    call paring_function(arg, norm)
                else
                    call file_truename(arg, norm, st)
                    if (.not. st%ok()) norm = arg
                end if

                call append_seen(seen, norm)
            end if

            call pcomplete_next_arg()
            advanced = thrown == THROWN_NONE
            return
        end if

        if (present(stub)) pcomplete_stub = stub

        if (p == PCOMPLETE_PARING_NONE .or. p == PCOMPLETE_PARING_CLEAR) then
            if (allocated(seen)) deallocate (seen)
        else if (present(paring_function)) then
            norm_func => paring_function
        else
            norm_func => truename_norm
        end if

        only = .false.
        if (present(form_only)) only = form_only
        if (.not. only) call pcomplete_try_first_hook%execute(st)
        thrown = THROWN_COMPLETIONS
        has_thrown_table = .false.

        if (present(form)) then
            call form(thrown_table, found)
            has_thrown_table = found
        end if
    end function pcomplete_here

    !> An alternate form of `pcomplete_here' which does not participate
    !> in argument paring.  Port of pcomplete.el:pcomplete-here*.
    recursive logical function pcomplete_here_star(form, stub, form_only) result(advanced)
        procedure(pcomplete_form_procedure), optional :: form
        character(len=*), intent(in), optional :: stub
        logical, intent(in), optional :: form_only

        advanced = pcomplete_here(form, stub, PCOMPLETE_PARING_NONE, form_only=form_only)
    end function pcomplete_here_star

    ! `file_truename' as a paring function.  Not a port.
    recursive subroutine truename_norm(arg, norm)
        character(len=*), intent(in) :: arg
        character(len=:), allocatable, intent(out) :: norm
        type(status_type) :: st

        call file_truename(arg, norm, st)
        if (.not. st%ok()) norm = arg
    end subroutine truename_norm

    ! Add S to the list LIST.  Not a port.
    subroutine append_seen(list, s)
        type(process_arg_type), allocatable, intent(inout) :: list(:)
        character(len=*), intent(in) :: s
        type(process_arg_type), allocatable :: grown(:)
        integer :: n, k

        if (.not. allocated(list)) allocate (list(0))
        n = size(list)
        allocate (grown(n + 1))
        grown(1)%s = s

        do k = 1, n
            call move_alloc(list(k)%s, grown(k + 1)%s)
        end do

        call move_alloc(grown, list)
    end subroutine append_seen

    ! ------------------------------------------------------------------
    ! The standard completion UI
    ! ------------------------------------------------------------------

    !> The default exit function to use in
    !> `pcomplete_completions_at_point'.  This just adds
    !> `pcomplete_termination_string' after the completion if FINISHED
    !> is COMPLETION_FINISHED.  Port of
    !> pcomplete.el:pcomplete-default-exit-function.
    recursive subroutine pcomplete_default_exit_function(string, finished)
        character(len=*), intent(in) :: string
        integer, intent(in) :: finished
        type(status_type) :: st

        ! (STRING is unused.)
        if (.not. allocated(pcomplete_termination_string%value)) return
        if (len(pcomplete_termination_string%value) == 0) return
        if (finished /= COMPLETION_FINISHED) return

        if (looking_at(regexp_quote(pcomplete_termination_string%value), st)) then
            call goto_char(match_end(0, st))
        else
            call insert(pcomplete_termination_string%value, st)
        end if
    end subroutine pcomplete_default_exit_function

    ! The `:exit_function' of `pcomplete_completions_at_point': the
    ! `pcomplete_exit_function' it was called with.  Not a port.
    recursive subroutine capf_exit(string, finished)
        character(len=*), intent(in) :: string
        integer, intent(in) :: finished

        if (associated(capf_exit_function)) call capf_exit_function(string, finished)
    end subroutine capf_exit

    !> The length of the longest suffix of S1 that is also a suffix of S2
    !> (S2 is expected to be the "unquoted/expanded" version of S1).
    !> Port of pcomplete.el:pcomplete--common-suffix.
    integer(int64) function common_suffix(s1, s2) result(n)
        character(len=*), intent(in) :: s1, s2
        integer(int64) :: n1, n2

        n1 = schars(s1)
        n2 = schars(s2)
        n = 0

        do while (n < n1 .and. n < n2)
            if (substring(s1, n1 - n - 1, n1 - n) /= substring(s2, n2 - n - 1, n2 - n)) exit
            n = n + 1
        end do
    end function common_suffix

    ! The table of `pcomplete_completions_at_point' (before quoting).
    ! Port of pcomplete.el:pcomplete-completions-at-point, in part.
    recursive subroutine capf_table(string, action, result, status, test, suffix)
        character(len=*), intent(in) :: string
        integer, intent(in) :: action
        type(value_type), intent(out) :: result
        type(status_type), intent(out) :: status
        procedure(completion_test_procedure), optional :: test
        character(len=*), intent(in), optional :: suffix

        if (capf_subvert) then
            call completion_table_subvert(capf_completions, capf_s1, capf_s2, string, action, result, status, &
                                          test, suffix)
        else
            call complete_with_action(action, capf_completions, string, result, status, test, suffix)
        end if
    end subroutine capf_table

    ! The test of `pcomplete_completions_at_point': pare the
    ! arguments already seen.  Port of
    ! pcomplete.el:pcomplete-completions-at-point, in part.
    recursive logical function capf_pare(elt) result(ok)
        type(value_type), intent(in) :: elt
        character(len=:), allocatable :: norm
        integer :: k

        ok = .true.
        call capf_norm_func(directory_file_name(xstring(elt)), norm)

        do k = 1, size(capf_seen)
            if (capf_seen(k)%s == norm .and. len(capf_seen(k)%s) == len(norm)) then
                ok = .false.
                return
            end if
        end do
    end function capf_pare

    !> Provide standard completion using pcomplete's completion tables.
    !> Same as `pcomplete' but using the standard completion UI (for
    !> `completion_at_point_functions').  Port of
    !> pcomplete.el:pcomplete-completions-at-point.
    recursive subroutine pcomplete_completions_at_point(data, status)
        type(capf_data_type), intent(out) :: data
        type(status_type), intent(out) :: status
        type(completion_table_type) :: completions
        procedure(pcomplete_exit_procedure), pointer :: saved_exit
        character(len=:), allocatable :: buftext, tmp, norm
        type(status_type) :: st
        integer(int64) :: argbeg, beginning, end, suf_len
        integer :: k
        logical :: found, saved_read_only

        status = status_type()
        data = capf_data_type()
        ! FIXME: it only completes the text before point, whereas the
        ! standard UI may also consider text after point.
        pcomplete_stub = ""
        if (allocated(seen)) deallocate (seen)
        norm_func => null()
        if (allocated(args)) deallocate (args)
        if (allocated(begins)) deallocate (begins)
        pcomplete_last = 0
        pcomplete_index = 0
        saved_exit => pcomplete_exit_function
        ! Make sure the function obeys `pcomplete_allow_modifications'.
        saved_read_only = buffers(current)%read_only
        if (.not. pcomplete_allow_modifications) buffers(current)%read_only = .true.
        ! pcomplete-completions is the core function that finds the chunk
        ! of text to complete (returned indirectly in `pcomplete_stub')
        ! and the set of possible completions.
        call pcomplete_completions(completions, found, status)
        buffers(current)%read_only = saved_read_only
        capf_exit_function => pcomplete_exit_function
        pcomplete_exit_function => saved_exit

        if (thrown == THROWN_PCOMPLETED .or. .not. status%ok()) then
            status = status_type()
            return
        end if

        ! Since we use minibuffer-complete, which doesn't know
        ! `pcomplete_stub' and works from the buffer's text instead, we
        ! need to trick minibuffer-complete into using `pcomplete_stub'
        ! without its knowledge: completion-table-subvert constructs a
        ! completion table which expects strings using a prefix from the
        ! buffer's text but internally uses the corresponding prefix from
        ! `pcomplete_stub'.
        argbeg = pcomplete_begin()
        beginning = max(point() - schars(pcomplete_stub), argbeg)
        end = point()

        ! Try and improve our guess of `beginning' in case the difference
        ! between `pcomplete_stub' and the buffer's text is simply due to
        ! some chars removed by unquoting.
        do
            call buffer_substring(beginning, end, tmp, st)
            call pcomplete_unquote_argument_function(tmp, buftext)
            if (.not. (beginning > argbeg .and. schars(pcomplete_stub) > schars(buftext))) exit
            beginning = max(argbeg, beginning - (schars(pcomplete_stub) - schars(buftext)))
        end do

        ! Try and improve our guess of `end' in case it's not point.
        do while (schars(buftext) < schars(pcomplete_stub) .and. end < point_max())
            call buffer_substring(beginning, end + 1, tmp, st)
            call pcomplete_unquote_argument_function(tmp, norm)
            if (.not. prefix_of(norm, pcomplete_stub)) exit
            end = end + 1
            buftext = norm
        end do

        if (.not. found) return
        capf_completions = completions
        capf_subvert = .not. (pcomplete_stub == buftext .and. len(pcomplete_stub) == len(buftext))

        if (capf_subvert) then
            ! This may not always be strictly right, but given the lack of
            ! any other info, it's about as good as it gets, and in
            ! practice it should work just fine (fingers crossed).
            suf_len = common_suffix(pcomplete_stub, buftext)
            capf_s1 = substring(buftext, 0_int64, schars(buftext) - suf_len)
            capf_s2 = substring(pcomplete_stub, 0_int64, schars(pcomplete_stub) - suf_len)
        end if

        data%found = .true.
        data%start = beginning
        data%end = end
        data%table = completion_table_with_quoting(capf_table, pcomplete_unquote_argument_function, &
                                                   pcomplete_requote_argument_function)

        ! Pare it down, if applicable.
        if (pcomplete_use_paring%value .and. allocated(seen)) then
            if (size(seen) > 0) then
                ! Capture the values for later use.
                capf_norm_func => norm_func
                if (.not. associated(capf_norm_func)) capf_norm_func => truename_norm
                if (allocated(capf_seen)) deallocate (capf_seen)
                allocate (capf_seen(size(seen)))

                do k = 1, size(seen)
                    call capf_norm_func(directory_file_name(seen(k)%s), norm)
                    capf_seen(k)%s = norm
                end do

                data%test => capf_pare
            end if
        end if

        data%exit_function => capf_exit
    end subroutine pcomplete_completions_at_point

    ! Whether PREFIX is a prefix of STRING (string-prefix-p).  Not a port.
    logical function prefix_of(prefix, string)
        character(len=*), intent(in) :: prefix, string

        prefix_of = .false.
        if (len(prefix) > len(string)) return
        prefix_of = string(1:len(prefix)) == prefix
    end function prefix_of

    !> Define pcomplete's settings and default functions.  Not a port.
    subroutine init_pcomplete()
        if (associated(pcomplete_command_completion_function%value)) return
        if (.not. allocated(pcomplete_termination_string%value)) pcomplete_termination_string = " "
        if (.not. allocated(pcomplete_hosts_file%value)) pcomplete_hosts_file = "/etc/hosts"
        call forward_setting(pcomplete_termination_string)
        call forward_setting(pcomplete_remote_file_ignore)
        pcomplete_command_completion_function%value => default_command_completion
        pcomplete_default_completion_function%value => default_default_completion
        pcomplete_parse_arguments_function%value => pcomplete_parse_buffer_arguments
        pcomplete_exit_function => pcomplete_default_exit_function
        call append_locals_clearer(clear_pcomplete_locals)
    end subroutine init_pcomplete
end module pcomplete_mod
