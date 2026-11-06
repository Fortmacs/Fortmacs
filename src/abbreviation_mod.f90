! abbreviation_mod.f90 -- abbrev tables and Abbrev mode.
! Ported from GNU Emacs 31.1 lisp/abbrev.el, with the abbrev parts of
! src/buffer.c (abbrev-mode, local-abbrev-table) and src/cmds.c.
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

!> Abbrevs: abbrev tables are `abbreviation_table_type' objects named after
!> their Emacs variables (`global_abbreviation_table',
!> `fundamental_mode_abbreviation_table', and one per major mode, such as
!> `text_mode_abbreviation_table', which the mode's `define' makes known), and
!> abbrevs are defined with the table's `define':
!>
!>     call global_abbreviation_table%define("teh", "the")
!>     abbreviation_mode = .true.
!>
!> An abbrev table's properties are components (`parents', `case_fixed',
!> `regular_expression', `enable_function'); upstream's name table of names with
!> attributes becomes an array of `abbreviation_type' entries.  An abbrev
!> as `expand_abbreviation' and `last_abbreviation' give it (upstream's abbrev
!> symbol) is an `abbreviation_reference_type': its table and name.
!>
!> Saving abbrevs is a setting-saving command: `write_abbreviation_file'
!> writes the abbrevs as `define' calls into the init file (Fortran
!> compiled in, see `abbreviation_file_name'), so reading an abbrev file
!> (`read_abbreviation_file', `quietly_read_abbreviation_file') is not applicable.
!>
!> The procedures are in the submodule abbreviation_commands_mod, which can use the
!> modules (modes, windows, the minibuffer) that use this one: derived_mod
!> gives each major mode its abbrev table.
!>
!> Deferred: abbrevs whose EXPANSION is not a string (special abbrevs
!> that only run their hook), a list of tables as `local_abbreviation_table',
!> `abbreviation_minor_mode_tables', the obsolete wrapper hook
!> `abbreviation_expand_functions', other properties of abbrevs and tables
!> than the special ones (`abbreviation_table_get', `abbreviation_table_put',
!> `abbreviation_get', `abbreviation_put'), `abbreviation_suggest' (with
!> `abbreviation_suggest_hint_threshold' and `abbreviation_suggest_show_report'),
!> `abbreviation_table_menu', `define_abbreviations' and `edit_abbreviations_redefine'
!> with the keys of `edit_abbreviations_mode_map' (`abbreviation_edit_save_buffer',
!> `abbreviation_edit_save_to_file'), Edit-Abbrevs mode's font-lock keywords
!> and the `abbreviation_table_name' face, showing an abbrev's hook in the
!> *Abbrevs* buffer, and writing abbrevs that have a hook or an enable
!> function (a Fortran procedure has no name to write; the init file
!> defines them in code).  (Upstream 31.1 no longer has
!> `pre_abbreviation_expand_hook'.)
module abbreviation_mod
    use, intrinsic :: iso_fortran_env, only: int64
    use execute_interactively_mod, only: prefix_argument_type
    use error_mod, only: status_type
    use handle_mod, only: handle_type
    use data_mod, only: logical_setting_type, choice_setting_type
    use hooks_mod, only: hook_type
    use buffer_local_setting_mod, only: buffer_local_setting_type
    implicit none(type, external)
    private

    public :: abbreviation_hook_procedure, abbreviation_enable_procedure, abbreviation_expand_procedure
    public :: clear_all_abbreviations, copy_abbreviation_table, insert_abbreviations, list_abbreviations, abbreviation_table_name, &
              prepare_abbreviation_list_buffer, edit_abbreviations, write_abbreviation_file, add_mode_abbreviation, &
              add_global_abbreviation, &
              add_abbreviation, inverse_add_mode_abbreviation, inverse_add_global_abbreviation, inverse_add_abbreviation, &
              abbreviation_prefix_mark, &
              expand_region_abbreviations, define_global_abbreviation, define_mode_abbreviation, abbreviation_name, &
              abbreviation_expansion, &
              abbreviation_insert, expand_abbreviation, abbreviation_default_expand, unexpand_abbreviation, &
              insert_abbreviation_table_description, &
              abbreviation_table_names, abbreviation_possibly_save, &
              edit_abbreviations_mode, is_abbreviation_mode, abbreviation_no_self_insert, initialize_abbreviation

    !> The values of `save_abbreviations': never save, ask or save
    !> without asking (`silently').
    integer, parameter, public :: SAVE_ABBREVIATIONS_SILENTLY = 2

    abstract interface
        !> An abbrev's hook, called after its expansion is inserted;
        !> VALUE is its value (.true. or .false.), which matters for an
        !> abbrev whose `no_self_insert' is true.  Not a port.
        subroutine abbreviation_hook_procedure(value, status)
            import :: status_type
            implicit none(type, external)
            logical, intent(out) :: value
            type(status_type), intent(out) :: status
        end subroutine abbreviation_hook_procedure

        !> An enable function: whether the abbrevs of a table (or one
        !> abbrev) should be used for this call of `expand_abbreviation'.
        !> Not a port.
        logical function abbreviation_enable_procedure()
            implicit none(type, external)
        end function abbreviation_enable_procedure
    end interface

    !> One abbrev of a table (upstream's name in the table's name table,
    !> with its value, function and properties).
    type, public :: abbreviation_type
        !> The abbrev, as defined (usually lower case).
        character(len=:), allocatable :: name
        !> What it expands into; unallocated when it is not defined.
        character(len=:), allocatable :: expansion
        !> Called after the expansion is inserted (the name's function);
        !> null for none.
        procedure(abbreviation_hook_procedure), pointer, nopass :: hook => null()
        !> The hook's `no_self_insert' property: the value of the hook
        !> says whether the character that triggered the expansion is
        !> inserted.
        logical :: no_self_insert = .false.
        !> `:count': how many times it was expanded.
        integer(int64) :: count = 0
        !> `:system': a system abbrev, not saved in the abbrev file.
        logical :: system = .false.
        !> `:case_fixed': look it up without case folding, and do not
        !> capitalize or upcase the expansion.
        logical :: case_fixed = .false.
        !> `:enable_function'; null for none.
        procedure(abbreviation_enable_procedure), pointer, nopass :: enable_function => null()
        !> Whether it was ever defined (upstream's symbol is bound).
        logical :: bound = .false.
    end type abbreviation_type

    !> A pointer to an abbrev table.
    type, public :: abbreviation_table_reference_type
        type(abbreviation_table_type), pointer :: p => null()
    end type abbreviation_table_reference_type

    !> An abbrev table.  Port of the abbrev tables of abbrev.el (an
    !> name table with the table's attributes on its name "").
    type, public :: abbreviation_table_type
        !> The name of the variable holding the table
        !> (`abbreviation_table_name'), as in `abbreviation_tables'; empty for
        !> a table that is not listed.
        character(len=64) :: name = ""
        !> The abbrevs, in the order they were first defined.
        type(abbreviation_type), allocatable :: abbreviations(:)
        integer :: n = 0
        !> `:parents': the tables from which this table inherits abbrevs.
        type(abbreviation_table_reference_type), allocatable :: parents(:)
        !> `:case_fixed': look abbreviations up without case folding, and
        !> do not capitalize or upcase the expansion.
        logical :: case_fixed = .false.
        !> `:regexp': how to extract the name of the abbrev before point
        !> (its submatch 1); unallocated to use `backward_word' and
        !> `forward_word', so that a name is one word.
        character(len=:), allocatable :: regular_expression
        !> `:enable_function': whether the table's abbrevs should be used
        !> for this call of `expand_abbreviation'; null for always.
        procedure(abbreviation_enable_procedure), pointer, nopass :: enable_function => null()
        !> `:abbreviation_table_modiff': incremented when an abbrev is defined.
        integer(int64) :: modiff = 0
    contains
        !> Define an abbrev in the table.
        procedure :: define => abbreviation_table_define
        !> Make the table known by NAME (`define_abbreviation_table').
        procedure :: define_table => abbreviation_table_define_table
        !> Undefine all abbrevs (`clear_abbreviation_table').
        procedure :: clear => clear_abbreviation_table
        !> Whether there are no abbrevs (`is_abbreviation_table_empty').
        procedure :: empty => is_abbreviation_table_empty
        !> Add a parent table.
        procedure :: append_parent => abbreviation_table_append_parent
    end type abbreviation_table_type

    !> An abbrev as expansion functions return it (upstream's abbrev
    !> symbol): its table and name.  No table is the null handle.
    type, public :: abbreviation_reference_type
        type(abbreviation_table_type), pointer :: table => null()
        character(len=:), allocatable :: name
    end type abbreviation_reference_type

    abstract interface
        !> The function `expand_abbreviation' calls: ABBREVIATION is the abbrev that
        !> was expanded, if expansion took place.  Not a port.
        recursive subroutine abbreviation_expand_procedure(abbreviation, status)
            import :: abbreviation_reference_type, status_type
            implicit none(type, external)
            type(abbreviation_reference_type), intent(out) :: abbreviation
            type(status_type), intent(out) :: status
        end subroutine abbreviation_expand_procedure
    end interface

    !> Default name of file from which to read and where to save abbrevs.
    !> In Fortmacs it is the init file, whose `configure' gets the abbrevs
    !> as `define' calls (unallocated: the init file).  Port of abbrev.el
    !> abbrev-file-name.
    character(len=:), allocatable, public :: abbreviation_file_name

    !> True means user plans to use only global abbrevs.
    !> This makes the commands that normally define mode-specific abbrevs
    !> define global abbrevs instead.  Port of abbrev.el only-global-abbrevs.
    logical, public :: only_global_abbreviations = .false.

    !> True if Abbrev mode is enabled.
    !> Use the command `abbreviation_mode' to change the value of this variable in
    !> the current buffer.  Customize this variable to true to enable Abbrev
    !> mode by default in all buffers.  Port of buffer.c abbrev-mode (the
    !> state of the minor mode `abbreviation_mode', which `initialize_abbreviation'
    !> associates this with).
    type(logical_setting_type), pointer, public :: abbreviation_mode => null()

    !> List of the tables whose values are abbrev tables.  Port of
    !> abbrev.el abbrev-table-name-list (pointers to the tables, which
    !> carry their names).
    type(abbreviation_table_reference_type), allocatable, public :: abbreviation_tables(:)

    !> The abbrev table whose abbrevs affect all buffers.
    !> Each buffer may also have a local abbrev table.
    !> If it does, the local table overrides the global one
    !> for any particular abbrev defined in both.  Port of abbrev.el
    !> global-abbrev-table.
    type(abbreviation_table_type), target, public :: global_abbreviation_table = abbreviation_table_type( &
                                                     name="global_abbreviation_table")

    !> The abbrev table of mode-specific abbrevs for Fundamental Mode.
    !> Port of abbrev.el fundamental-mode-abbrev-table.
    type(abbreviation_table_type), target, public :: fundamental_mode_abbreviation_table = &
                                                     abbreviation_table_type(name="fundamental_mode_abbreviation_table")

    !> One buffer's own `local_abbreviation_table'.
    type :: local_abbreviation_table_entry_type
        type(abbreviation_table_type), pointer :: table => null()
    end type local_abbreviation_table_entry_type

    !> The type of `local_abbreviation_table': the global value `table'
    !> (set with `=>'; null for `fundamental_mode_abbreviation_table')
    !> and the buffers' own values (`set_local').
    type, extends(buffer_local_setting_type), public :: local_abbreviation_table_setting_type
        type(abbreviation_table_type), pointer :: table => null()
        type(local_abbreviation_table_entry_type), allocatable :: locals(:)
    contains
        !> Give the current buffer its own value, TABLE.
        procedure :: set_local => set_local_abbreviation_table_local
        !> The current buffer's value: its own, else the global one.
        procedure :: get_current => get_local_abbreviation_table_current
        procedure :: append_local_entry => append_local_abbreviation_table_entry
        procedure :: clear_local_entry => clear_local_abbreviation_table_entry
    end type local_abbreviation_table_setting_type

    !> Local (mode-specific) abbrev table of current buffer.  Port of
    !> buffer.c local-abbrev-table (a major mode gives its buffer its own
    !> with `%set_local'; kill-all-local-variables makes it use the global
    !> value again).
    type(local_abbreviation_table_setting_type), public :: local_abbreviation_table

    !> True if any word abbrevs were defined or altered.
    !> This causes `save_some_buffers' to offer to save the abbrevs.  Port
    !> of abbrev.el abbrevs-changed.
    logical, public :: abbreviations_changed = .false.

    !> True means expand multi-word abbrevs in all caps if the abbrev was so.
    !> Port of abbrev.el abbrev-all-caps.
    type(logical_setting_type), target, public :: abbreviation_all_caps = logical_setting_type(name="abbreviation_all_caps", &
                                                                                               value=.false.)

    !> Buffer position for `expand_abbreviation' to use as the start of the abbrev
    !> (a marker; the null handle for none).
    !> When none, use the word before point as the abbrev.
    !> Calling `expand_abbreviation' sets this to none.  Port of abbrev.el
    !> abbrev-start-location.
    type(handle_type), public :: abbreviation_start_location

    !> Buffer that `abbreviation_start_location' has been set for.
    !> Trying to expand an abbrev in any other buffer clears
    !> `abbreviation_start_location'.  Port of abbrev.el
    !> abbrev-start-location-buffer.
    type(handle_type), public :: abbreviation_start_location_buffer

    !> The abbrev of the last abbrev expanded.  See `abbreviation_name'.
    !> Port of abbrev.el last-abbrev.
    type(abbreviation_reference_type), public :: last_abbreviation

    !> The exact text of the last abbrev that was expanded.
    !> It is unallocated if the abbrev has already been unexpanded.  Port of
    !> abbrev.el last-abbrev-text.
    character(len=:), allocatable, public :: last_abbreviation_text

    !> The location of the start of the last abbrev that was expanded.
    !> Port of abbrev.el last-abbrev-location.
    integer(int64), public :: last_abbreviation_location = 0

    !> Function that `expand_abbreviation' uses to perform abbrev expansion.
    !> Takes no arguments, and should return the abbrev if expansion
    !> took place.  Port of abbrev.el abbrev-expand-function (null is
    !> `abbreviation_default_expand', its default).
    procedure(abbreviation_expand_procedure), pointer, public :: abbreviation_expand_function => null()

    !> True means save word abbrevs too when files are saved.
    !> If `SAVE_ABBREVIATIONS_SILENTLY', don't ask the user before saving.  Port
    !> of files.el save-abbrevs (.true. asks before saving, as upstream t).
    type(choice_setting_type), public :: save_abbreviations = choice_setting_type(name="save_abbreviations", value=.true.)

    !> Keymap used in `edit_abbreviations'.  Port of abbrev.el
    !> edit-abbrevs-mode-map (its keys run commands not ported yet).
    type(handle_type), public :: edit_abbreviations_mode_map

    !> Hook run after entering `edit_abbreviations_mode'.  Port of abbrev.el
    !> edit-abbrevs-mode-hook.
    type(hook_type), target, public :: edit_abbreviations_mode_hook = hook_type(name="edit_abbreviations_mode_hook")

    !> Abbrev table for `edit_abbreviations_mode'.  Port of abbrev.el
    !> edit-abbrevs-mode-abbrev-table (made by define-derived-mode).
    type(abbreviation_table_type), target, public :: edit_abbreviations_mode_abbreviation_table

    interface
        !> Define ABBREVIATION in the table, to expand into EXPANSION and optionally call HOOK.
        !> ABBREVIATION must be a string, and should be lower-case.
        !> To undefine an abbrev, define it without EXPANSION.
        !> If HOOK is present, it is called after EXPANSION is inserted.
        !>
        !> If NO_SELF_INSERT is true (upstream a `no_self_insert' property of
        !> HOOK), HOOK can control whether the character that triggered abbrev
        !> expansion is inserted.  If such a HOOK returns true, the character is
        !> not inserted.  If such a HOOK returns false, then so does `abbreviation_insert'
        !> (and `expand_abbreviation'), as if no abbrev expansion had taken place.
        !>
        !> These arguments are special:
        !> - COUNT: the value for the abbrev's usage-count, which is incremented each
        !>   time the abbrev is used (the default is zero).
        !> - SYSTEM: if true, says that this is a "system" abbreviation
        !>   which should not be saved in the user's abbreviation file.
        !>   Unless FORCE is also true, a system abbreviation will not
        !>   overwrite a non-system abbreviation of the same name.
        !> - CASE_FIXED: true means that abbreviations are looked up without
        !>   case-folding, and the expansion is not capitalized/upcased.
        !> - ENABLE_FUNCTION: a function of no arguments which returns true
        !>   if the abbrev should be used for a particular call of `expand_abbreviation'.
        !> Port of abbrev.el:define-abbrev.
        module subroutine abbreviation_table_define(self, abbreviation, expansion, hook, count, system, force, case_fixed, &
                                                    enable_function, no_self_insert)
            implicit none(type, external)
            class(abbreviation_table_type), intent(inout), target :: self
            character(len=*), intent(in) :: abbreviation
            character(len=*), intent(in), optional :: expansion
            procedure(abbreviation_hook_procedure), optional :: hook
            integer(int64), intent(in), optional :: count
            logical, intent(in), optional :: system, force, case_fixed, no_self_insert
            procedure(abbreviation_enable_procedure), optional :: enable_function
        end subroutine abbreviation_table_define

        !> Define NAME (the name of the variable holding the table) as an
        !> abbrev table name: the table is added to
        !> `abbreviation_tables'.  Its abbrevs are defined with
        !> `define', its properties are its components:
        !> - `parents' contains the abbrev tables from which this table inherits
        !>   abbreviations.
        !> - `case_fixed' true means that abbreviations are looked up without
        !>   case-folding, and the expansion is not capitalized/upcased.
        !> - `regular_expression' is a regular expression that specifies how to extract the
        !>   name of the abbrev before point.  The submatch 1 is treated
        !>   as the potential name of an abbrev.  If `regular_expression' is unallocated, the
        !>   default behavior uses `backward_word' and `forward_word' to extract
        !>   the name of the abbrev, which can therefore by default only be a
        !>   single word.
        !> - `enable_function' can be set to a function of no arguments which
        !>   returns true if and only if the abbrevs in this table should be used
        !>   for this instance of `expand_abbreviation'.
        !> Port of abbrev.el:define-abbrev-table (DEFINITIONS, DOCSTRING and
        !> PROPERTIES are the calls and assignments that follow).
        module subroutine abbreviation_table_define_table(self, name)
            implicit none(type, external)
            class(abbreviation_table_type), intent(inout), target :: self
            character(len=*), intent(in) :: name
        end subroutine abbreviation_table_define_table

        !> Undefine all abbrevs in abbrev table TABLE, leaving TABLE empty.
        !> Port of abbrev.el:clear-abbrev-table.
        module subroutine clear_abbreviation_table(self)
            implicit none(type, external)
            class(abbreviation_table_type), intent(inout) :: self
        end subroutine clear_abbreviation_table

        !> Return false if there are abbrev names in OBJECT.
        !> If IGNORE_SYSTEM is true, system definitions are ignored.  Port of
        !> abbrev.el:abbrev-table-empty-p.
        module logical function is_abbreviation_table_empty(self, ignore_system) result(empty)
            implicit none(type, external)
            class(abbreviation_table_type), intent(in) :: self
            logical, intent(in), optional :: ignore_system
        end function is_abbreviation_table_empty

        !> Add PARENT to the tables from which the table inherits abbrevs
        !> (its `parents').  Not a port.
        module subroutine abbreviation_table_append_parent(self, parent)
            implicit none(type, external)
            class(abbreviation_table_type), intent(inout) :: self
            type(abbreviation_table_type), intent(in), target :: parent
        end subroutine abbreviation_table_append_parent

        !> Undefine all defined abbrevs.  Port of abbrev.el:kill-all-abbrevs.
        module subroutine clear_all_abbreviations()
            implicit none(type, external)
        end subroutine clear_all_abbreviations

        !> Make a new abbrev-table NEW with the same abbrevs as TABLE.
        !> This function does not copy the attributes of the abbrevs.
        !> See `define_abbreviation' for the documentation of abbrev properties.
        !> Port of abbrev.el:copy-abbrev-table.
        module subroutine copy_abbreviation_table(table, new)
            implicit none(type, external)
            type(abbreviation_table_type), intent(in) :: table
            type(abbreviation_table_type), intent(out) :: new
        end subroutine copy_abbreviation_table

        !> Insert the description of all defined abbrevs after point.
        !> Set mark after the inserted text.  Port of abbrev.el:insert-abbrevs.
        module subroutine insert_abbreviations(status)
            implicit none(type, external)
            type(status_type), intent(out) :: status
        end subroutine insert_abbreviations

        !> Display a list of the defined abbrevs.
        !> If LOCAL is true (interactively, when invoked with a
        !> prefix arg), display only local, i.e. mode-specific, abbrevs.
        !> Otherwise display all the abbrevs.  Port of abbrev.el:list-abbrevs.
        recursive module subroutine list_abbreviations(status, local)
            implicit none(type, external)
            type(status_type), intent(out) :: status
            logical, intent(in), optional :: local
        end subroutine list_abbreviations

        !> Return the name of the specified abbrev TABLE ("" if it has
        !> none).  Port of abbrev.el:abbrev-table-name.
        module function abbreviation_table_name(table) result(name)
            implicit none(type, external)
            type(abbreviation_table_type), intent(in), target :: table
            character(len=:), allocatable :: name
        end function abbreviation_table_name

        !> Return buffer listing abbreviations and expansions for each abbrev table.
        !>
        !> If LOCAL is true, include in the buffer only the local abbrevs.
        !> Port of abbrev.el:prepare-abbrev-list-buffer.
        recursive module subroutine prepare_abbreviation_list_buffer(buffer, status, local)
            implicit none(type, external)
            type(handle_type), intent(out) :: buffer
            type(status_type), intent(out) :: status
            logical, intent(in), optional :: local
        end subroutine prepare_abbreviation_list_buffer

        !> Alter abbrev definitions by editing the list of abbrevs.
        !> This selects a buffer containing the list of abbrev definitions
        !> with point located in the abbrev table for the current buffer, and
        !> turns on `edit_abbreviations_mode' in the buffer with the list of abbrevs.
        !>
        !> The abbrevs editing buffer contains a header line for each
        !> abbrev table, which is the abbrev table name in parentheses.
        !>
        !> The header line is followed by one line per abbrev in that table:
        !>
        !>     NAME   USECOUNT   EXPANSION   HOOK
        !>
        !> where NAME and EXPANSION are strings with quotes,
        !> USECOUNT is an integer, and HOOK is any valid function
        !> that may be omitted (it is usually omitted).  Port of
        !> abbrev.el:edit-abbrevs (redefining the abbrevs from the
        !> edited buffer is not ported yet).
        recursive module subroutine edit_abbreviations(status)
            implicit none(type, external)
            type(status_type), intent(out) :: status
        end subroutine edit_abbreviations

        !> Write all user-level abbrev definitions to the init file, as
        !> Fortran.
        !> This does not include system abbrevs; it includes only the abbrev tables
        !> listed in `abbreviation_tables'.
        !> The file written defines the same abbrevs in the next session (after
        !> \[fortmacs --recompile]): they replace the abbrevs it defined before, at the
        !> end of `configure'.
        !> The argument FILE is the file name to write.  If omitted or empty, it defaults
        !> to the value of `abbreviation_file_name'.
        !> If VERBOSE is true, display a message indicating the file where the
        !> abbrevs have been saved.  Port of abbrev.el:write-abbrev-file
        !> (written as Fortran: a setting-saving command).
        recursive module subroutine write_abbreviation_file(status, file, verbose)
            implicit none(type, external)
            type(status_type), intent(out) :: status
            character(len=*), intent(in), optional :: file
            logical, intent(in), optional :: verbose
        end subroutine write_abbreviation_file

        !> Define a mode-specific abbrev whose expansion is the last word before point.
        !> If there's an active region, use that as the expansion.
        !>
        !> Prefix argument ARGUMENT says how many words before point to use for the expansion;
        !> zero means the entire region is the expansion.
        !>
        !> A negative ARGUMENT means to undefine the specified abbrev.
        !>
        !> This command reads the abbreviation from the minibuffer.
        !>
        !> See also `inverse_add_mode_abbreviation', which performs the opposite task:
        !> if the abbreviation is already in the buffer, use that command to define
        !> a mode-specific abbrev by specifying its expansion in the minibuffer.
        !>
        !> Don't use this function in a program; use `define_abbreviation' instead.
        !> Port of abbrev.el:add-mode-abbrev.
        recursive module subroutine add_mode_abbreviation(argument, status)
            implicit none(type, external)
            type(prefix_argument_type), intent(in) :: argument
            type(status_type), intent(out) :: status
        end subroutine add_mode_abbreviation

        !> Define a global (all modes) abbrev whose expansion is last word before point.
        !> If there's an active region, use that as the expansion.
        !>
        !> Prefix argument ARGUMENT says how many words before point to use for the expansion;
        !> zero means the entire region is the expansion.
        !>
        !> A negative ARGUMENT means to undefine the specified abbrev.
        !>
        !> This command reads the abbreviation from the minibuffer.
        !>
        !> See also `inverse_add_global_abbreviation', which performs the opposite task:
        !> if the abbreviation is already in the buffer, use that command to define
        !> a global abbrev by specifying its expansion in the minibuffer.
        !>
        !> Don't use this function in a program; use `define_abbreviation' instead.
        !> Port of abbrev.el:add-global-abbrev.
        recursive module subroutine add_global_abbreviation(argument, status)
            implicit none(type, external)
            type(prefix_argument_type), intent(in) :: argument
            type(status_type), intent(out) :: status
        end subroutine add_global_abbreviation

        !> Define abbrev in TABLE, whose expansion is ARGUMENT words before point.
        !> Read the abbreviation from the minibuffer, with prompt TYPE.
        !>
        !> ARGUMENT of zero means the entire region is the expansion.
        !>
        !> A negative ARGUMENT means to undefine the specified abbrev.
        !>
        !> TYPE is an arbitrary string used to prompt user for the kind of
        !> abbrev, such as "Global", "Mode".  (This has no influence on the
        !> choice of the actual TABLE).
        !>
        !> See `inverse_add_abbreviation' for the opposite task.
        !>
        !> Don't use this function in a program; use `define_abbreviation' instead.
        !> Port of abbrev.el:add-abbrev.
        recursive module subroutine add_abbreviation(table, type, argument, status)
            implicit none(type, external)
            type(abbreviation_table_type), intent(inout), target :: table
            character(len=*), intent(in) :: type
            type(prefix_argument_type), intent(in) :: argument
            type(status_type), intent(out) :: status
        end subroutine add_abbreviation

        !> Define the word before point as a mode-specific abbreviation.
        !> With prefix argument N, define the Nth word before point as the
        !> abbreviation.  Negative N means use the Nth word after point.
        !>
        !> If `only_global_abbreviations' is true, this command defines a
        !> global (mode-independent) abbrev instead of a mode-specific one.
        !>
        !> This command reads the expansion from the minibuffer, defines the
        !> abbrev, and then expands the abbreviation in the current buffer.
        !>
        !> See also `add_mode_abbreviation', which performs the opposite task:
        !> if the expansion is already in the buffer, use that command
        !> to define an abbrev by specifying the abbreviation in the minibuffer.
        !> Port of abbrev.el:inverse-add-mode-abbrev.
        recursive module subroutine inverse_add_mode_abbreviation(n, status)
            implicit none(type, external)
            integer(int64), intent(in) :: n
            type(status_type), intent(out) :: status
        end subroutine inverse_add_mode_abbreviation

        !> Define the word before point as a global (mode-independent) abbreviation.
        !> With prefix argument N, define the Nth word before point as the
        !> abbreviation.  Negative N means use the Nth word after point.
        !>
        !> This command reads the expansion from the minibuffer, defines the
        !> abbrev, and then expands the abbreviation in the current buffer.
        !>
        !> See also `add_global_abbreviation', which performs the opposite task:
        !> if the expansion is already in the buffer, use that command
        !> to define an abbrev by specifying the abbreviation in the minibuffer.
        !> Port of abbrev.el:inverse-add-global-abbrev.
        recursive module subroutine inverse_add_global_abbreviation(n, status)
            implicit none(type, external)
            integer(int64), intent(in) :: n
            type(status_type), intent(out) :: status
        end subroutine inverse_add_global_abbreviation

        !> Define the word before point as an abbrev in TABLE.
        !> Read the expansion from the minibuffer, using prompt TYPE, define
        !> the abbrev, and then expand the abbreviation in the current
        !> buffer.
        !>
        !> ARGUMENT means use the ARG-th word before point as the abbreviation.
        !> Negative ARGUMENT means use the ARG-th word after point.
        !>
        !> TYPE is an arbitrary string used to prompt user for the kind of
        !> abbrev, such as "Global", "Mode".  (This has no influence on the
        !> choice of the actual TABLE).
        !>
        !> See also `add_abbreviation', which performs the opposite task.  Port of
        !> abbrev.el:inverse-add-abbrev.
        recursive module subroutine inverse_add_abbreviation(table, type, argument, status)
            implicit none(type, external)
            type(abbreviation_table_type), intent(inout), target :: table
            character(len=*), intent(in) :: type
            integer(int64), intent(in) :: argument
            type(status_type), intent(out) :: status
        end subroutine inverse_add_abbreviation

        !> Mark point as the beginning of an abbreviation.
        !> The abbrev to be expanded starts at point rather than at the
        !> beginning of a word.  This way, you can expand an abbrev with
        !> a prefix: insert the prefix, use this command, then insert the
        !> abbrev.
        !>
        !> This command inserts a hyphen after the prefix, and if the abbrev
        !> is subsequently expanded, this hyphen will be removed.
        !>
        !> If the prefix is itself an abbrev, this command expands it,
        !> unless ARGUMENT is true.  Interactively, ARGUMENT is the prefix
        !> argument.  Port of abbrev.el:abbrev-prefix-mark.
        recursive module subroutine abbreviation_prefix_mark(status, argument)
            implicit none(type, external)
            type(status_type), intent(out) :: status
            logical, intent(in), optional :: argument
        end subroutine abbreviation_prefix_mark

        !> For each abbrev occurrence in the region, offer to expand it.
        !> Ask the user to type `y' or `n' for each occurrence.
        !> A prefix argument means don't query; expand all abbrevs.  Port of
        !> abbrev.el:expand-region-abbrevs.
        recursive module subroutine expand_region_abbreviations(start, end, status, noquery)
            implicit none(type, external)
            integer(int64), intent(in) :: start, end
            type(status_type), intent(out) :: status
            logical, intent(in), optional :: noquery
        end subroutine expand_region_abbreviations

        !> Define ABBREVIATION as a global abbreviation that expands into EXPANSION.
        !> The characters in ABBREVIATION must all be word constituents in the standard
        !> syntax table.  Port of abbrev.el:define-global-abbrev.
        module subroutine define_global_abbreviation(abbreviation, expansion, status)
            implicit none(type, external)
            character(len=*), intent(in) :: abbreviation, expansion
            type(status_type), intent(out) :: status
        end subroutine define_global_abbreviation

        !> Define ABBREVIATION as a mode-specific abbreviation that expands into EXPANSION.
        !> The characters in ABBREVIATION must all be word-constituents in the current mode.
        !> Port of abbrev.el:define-mode-abbrev.
        module subroutine define_mode_abbreviation(abbreviation, expansion, status)
            implicit none(type, external)
            character(len=*), intent(in) :: abbreviation, expansion
            type(status_type), intent(out) :: status
        end subroutine define_mode_abbreviation

        !> Return the abbrev named ABBREVIATION in TABLE, in RESULT (no table if
        !> such an abbrev is not defined).
        !> Optional second arg TABLE is the abbrev table to look it up in.
        !> The default is to try buffer's mode-specific abbrev table, then global table.
        !> Port of abbrev.el:abbrev-symbol.
        module subroutine abbreviation_name(abbreviation, result, table)
            implicit none(type, external)
            character(len=*), intent(in) :: abbreviation
            type(abbreviation_reference_type), intent(out) :: result
            type(abbreviation_table_type), intent(in), target, optional :: table
        end subroutine abbreviation_name

        !> Return the string that ABBREVIATION expands into in the current buffer,
        !> in EXPANSION (unallocated if none).
        !> Optionally specify an abbrev TABLE as second arg;
        !> then ABBREVIATION is looked up in that table only.  Port of
        !> abbrev.el:abbrev-expansion.
        module subroutine abbreviation_expansion(abbreviation, expansion, table)
            implicit none(type, external)
            character(len=*), intent(in) :: abbreviation
            character(len=:), allocatable, intent(out) :: expansion
            type(abbreviation_table_type), intent(in), target, optional :: table
        end subroutine abbreviation_expansion

        !> Insert abbrev ABBREVIATION at point.
        !> If present, NAME is the name by which this abbrev was found.
        !> If present, WORDSTART is the buffer position where to insert the abbrev.
        !> If WORDEND is present, it is a buffer position; the abbrev replaces the
        !> previous text between WORDSTART and WORDEND.
        !> RESULT is ABBREVIATION if the expansion should be considered as having taken place.
        !> The result can be influenced by a `no_self_insert' property;
        !> see `define_abbreviation' for details.  Port of abbrev.el:abbrev-insert.
        recursive module subroutine abbreviation_insert(abbreviation, result, status, name, wordstart, wordend)
            implicit none(type, external)
            type(abbreviation_reference_type), intent(in) :: abbreviation
            type(abbreviation_reference_type), intent(out) :: result
            type(status_type), intent(out) :: status
            character(len=*), intent(in), optional :: name
            integer(int64), intent(in), optional :: wordstart, wordend
        end subroutine abbreviation_insert

        !> Expand the abbrev before point, if there is an abbrev there.
        !> Effective when explicitly called even when `abbreviation_mode' is false.
        !> Calls the value of `abbreviation_expand_function' with no argument to do
        !> the work, and returns whatever it does in EXPANDED.  (That value should
        !> be the abbrev if expansion occurred, else none.)  Port of
        !> abbrev.el:expand-abbrev.
        recursive module subroutine expand_abbreviation(status, expanded)
            implicit none(type, external)
            type(status_type), intent(out) :: status
            type(abbreviation_reference_type), intent(out), optional :: expanded
        end subroutine expand_abbreviation

        !> Default function to use for `abbreviation_expand_function'.
        !> Calls `abbreviation_insert' to insert any expansion, and returns what it does.
        !> Port of abbrev.el:abbrev--default-expand (without the obsolete
        !> wrapper hook `abbreviation_expand_functions').
        recursive module subroutine abbreviation_default_expand(abbreviation, status)
            implicit none(type, external)
            type(abbreviation_reference_type), intent(out) :: abbreviation
            type(status_type), intent(out) :: status
        end subroutine abbreviation_default_expand

        !> Undo the expansion of the last abbrev that expanded.
        !> This differs from ordinary undo in that other editing done since then
        !> is not undone.  Port of abbrev.el:unexpand-abbrev.
        recursive module subroutine unexpand_abbreviation(status)
            implicit none(type, external)
            type(status_type), intent(out) :: status
        end subroutine unexpand_abbreviation

        !> Insert before point a full description of abbrev table TABLE.
        !> If optional 2nd arg READABLE is true, insert a human-readable
        !> description.
        !>
        !> If READABLE is false, insert Fortran: the `define' calls that,
        !> in `configure', define the abbrevs of the table exactly as they
        !> are currently defined.
        !> Abbrevs marked as "system abbrevs" are ignored.  Port of
        !> abbrev.el:insert-abbrev-table-description.
        recursive module subroutine insert_abbreviation_table_description(table, status, readable)
            implicit none(type, external)
            type(abbreviation_table_type), intent(in) :: table
            type(status_type), intent(out) :: status
            logical, intent(in), optional :: readable
        end subroutine insert_abbreviation_table_description

        !> Return the user abbrevs in the abbrev table TABLE, in NAMES.
        !> System abbrevs are omitted unless SYSTEM is true.  Port of
        !> abbrev.el:abbrev--table-symbols.
        module subroutine abbreviation_table_names(table, names, system)
            implicit none(type, external)
            type(abbreviation_table_type), intent(in) :: table
            character(len=:), allocatable, intent(out) :: names(:)
            logical, intent(in), optional :: system
        end subroutine abbreviation_table_names

        !> Hook function for use by `save_some_buffers_functions'.
        !>
        !> Maybe save abbrevs, and record whether we either saved them or asked to.
        !> With QUERY, RESULT tells whether there are abbrevs to save; else
        !> RESULT true inhibits the message of `save_some_buffers'.  Port of
        !> abbrev.el:abbrev--possibly-save.
        recursive module subroutine abbreviation_possibly_save(query, argument, result, status)
            implicit none(type, external)
            logical, intent(in) :: query, argument
            logical, intent(out) :: result
            type(status_type), intent(out) :: status
        end subroutine abbreviation_possibly_save

        !> Give the current buffer its own `local_abbreviation_table', TABLE.
        !> Not a port.
        module subroutine set_local_abbreviation_table_local(self, table)
            implicit none(type, external)
            class(local_abbreviation_table_setting_type), intent(inout) :: self
            type(abbreviation_table_type), intent(in), target :: table
        end subroutine set_local_abbreviation_table_local

        !> The current buffer's `local_abbreviation_table': its own value,
        !> else the global one (`fundamental_mode_abbreviation_table' when
        !> null).  Not a port.
        module function get_local_abbreviation_table_current(self) result(table)
            implicit none(type, external)
            class(local_abbreviation_table_setting_type), intent(in) :: self
            type(abbreviation_table_type), pointer :: table
        end function get_local_abbreviation_table_current

        !> Grow `locals' by one entry.  Not a port.
        module subroutine append_local_abbreviation_table_entry(self)
            implicit none(type, external)
            class(local_abbreviation_table_setting_type), intent(inout) :: self
        end subroutine append_local_abbreviation_table_entry

        !> Reset entry K of `locals'.  Not a port.
        module subroutine clear_local_abbreviation_table_entry(self, k)
            implicit none(type, external)
            class(local_abbreviation_table_setting_type), intent(inout) :: self
            integer, intent(in) :: k
        end subroutine clear_local_abbreviation_table_entry

        !> Major mode for editing the list of abbrev definitions.
        !> This mode is for editing abbrevs in a buffer prepared by `edit_abbreviations',
        !> which see.  Port of abbrev.el:edit-abbrevs-mode.
        recursive module subroutine edit_abbreviations_mode(status)
            implicit none(type, external)
            type(status_type), intent(out) :: status
        end subroutine edit_abbreviations_mode

        !> Whether ABBREVIATION has a hook with the `no_self_insert' property
        !> (cmds.c's test after expanding in self-insert-command).
        !> Not a port.
        module logical function abbreviation_no_self_insert(abbreviation)
            implicit none(type, external)
            type(abbreviation_reference_type), intent(in) :: abbreviation
        end function abbreviation_no_self_insert

        !> Whether Abbrev mode is on in the current buffer (BVAR
        !> abbreviation_mode, for cmds.c's self-insert-command).  Not a port.
        module logical function is_abbreviation_mode()
            implicit none(type, external)
        end function is_abbreviation_mode

        !> Define Abbrev mode, Edit-Abbrevs mode and the tables, and
        !> register the commands.  Not a port.
        module subroutine initialize_abbreviation()
            implicit none(type, external)
        end subroutine initialize_abbreviation
    end interface
end module abbreviation_mod
