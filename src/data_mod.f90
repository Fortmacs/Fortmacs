! data_mod.f90 -- primitive operations on values and symbols.
! Ported from GNU Emacs 31.1 src/data.c.
!
! Copyright (C) 1985-1986, 1988, 1993-1995, 1997-2026 Free Software
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

!> Type checks, `eq', list access, symbol values, and (in the submodule
!> data_buffer_mod) buffer-local values and the variables forwarded to
!> Fortran settings.
!>
!> Buffer-local values work as upstream's swapped-in bindings: the value
!> of a variable (its symbol slot, or the Fortran variable it forwards
!> to) is always the current buffer's; switching buffers saves the old
!> buffer's local values and loads the new buffer's.  So Fortran code
!> reads settings such as `fill_column' directly.
!>
!> Wrong-type errors carry the test name as detail (upstream's DATA is
!> `(TEST VALUE)').  Dynamic binding (`let'), variable watchers and
!> aliases are not here yet.
module data_mod
    use, intrinsic :: iso_fortran_env, only: int64, real64
    use, intrinsic :: ieee_arithmetic, only: ieee_is_nan, ieee_value, ieee_positive_inf, ieee_quiet_nan
    use error_mod, only: status_type, set_error, FM_WRONG_TYPE_ARGUMENT, &
                         FM_VOID_VARIABLE, FM_SETTING_CONSTANT, FM_VOID_FUNCTION, FM_ARGS_OUT_OF_RANGE
    use value_mod, only: list, list_elt, list_tail, symbols, v_buffer, V_INTEGER, v_keymap, V_ARRAY, v_marker, V_NULL, &
                         V_NAME, v_syntax_table, v_window, value_type, vtag, xsymbol, V_STRING
    use names_mod, only: ensure_obarray
    use value_mod, only: command_procedure, command_type, value_type, set_choice_state, is_choice_off, choice_type
    use string_type_mod, only: string_type
    implicit none(type, external)
    private

    !> A setting: a variable that a buffer can have its own value of
    !> (upstream any variable can be buffer-local).  Assigning to it sets
    !> the value seen in the current buffer (the default, unless the
    !> buffer has its own); `set_local' gives the current buffer its own
    !> value, as upstream's `setq_local':
    !>
    !>     fill_column = 72               ! the default
    !>     call fill_column%set_local(60) ! this buffer only
    !>
    !> Fortran code reads `fill_column%value'.
    type, abstract, public :: setting_type
        !> The variable's name, as the help commands show it.
        character(len=48) :: name = ""
        !> Void (`makunbound'); cleared by any assignment.
        logical :: void = .false.
    contains
        !> Make the current buffer use the default value again.  Port of
        !> data.c:Fkill_local_variable.
        procedure :: clear_local => setting_clear_local
        !> Whether the current buffer has its own value.  Port of
        !> data.c:Flocal_variable_p.
        procedure :: local_p => is_setting_local
        !> Start giving the current buffer its own value.
        procedure :: make_local => setting_make_local
        !> Forget the value's other kinds that an extension adds (a
        !> procedure, R6-37; a number or a string in a named component,
        !> R6-39); every assignment calls it.  This type has none.
        procedure :: clear_kinds => setting_clear_kinds
        !> The Fortran text of a value of another kind that is set
        !> (`a procedure set by the configuration', `%lines = 5'), for the
        !> help commands; "" when the main value is the value.
        procedure :: other_value => setting_other_value
    end type setting_type

    !> An integer setting.
    type, extends(setting_type), public :: integer_setting_type
        integer(int64) :: value = 0
    contains
        generic :: assignment(=) => assign_int, assign_int64
        generic :: set_local => set_local_int, set_local_int64, set_local_setting
        procedure, private :: assign_int => integer_assign_int, assign_int64 => integer_assign_int64
        procedure, private :: set_local_int => integer_set_local_int, set_local_int64 => integer_set_local_int64
        procedure, private :: set_local_setting => integer_set_local_setting
    end type integer_setting_type

    !> A true/false setting (.true. or .false.).
    type, extends(setting_type), public :: logical_setting_type
        logical :: value = .false.
    contains
        generic :: assignment(=) => assign
        generic :: set_local => set_local_logical
        procedure, private :: assign => logical_assign
        procedure, private :: set_local_logical => logical_set_local
    end type logical_setting_type

    !> A string setting; unallocated when unset.
    type, extends(setting_type), public :: string_setting_type
        character(len=:), allocatable :: value
    contains
        generic :: assignment(=) => assign
        generic :: set_local => set_local_string
        procedure, private :: assign => string_assign
        procedure, private :: set_local_string => string_set_local
        !> Deallocate it.
        procedure :: clear => string_clear
    end type string_setting_type

    !> A setting whose value is a function of no arguments (a
    !> `command_type' procedure); null when unset.  Assign with `=>' to
    !> `value'.
    type, extends(setting_type), public :: function_setting_type
        procedure(command_procedure), pointer, nopass :: value => null()
        !> The command a name given at run time resolved to (the value
        !> then reads back as its name); null otherwise.
        type(command_type), pointer :: command => null()
    contains
        generic :: set_local => set_local_function
        procedure, private :: set_local_function => function_set_local
    end type function_setting_type

    !> A setting with more than two states (R6-38): `=' takes .true. and
    !> .false. for its on and off states (upstream t and nil) and its
    !> named constants for the others:
    !>
    !>     compilation_scroll_output = .true.
    !>     compilation_scroll_output = COMPILATION_SCROLL_OUTPUT_FIRST_ERROR
    !>
    !> Fortran code reads on and off as `%value' and the other states as
    !> `%choice == X_CONSTANT' (a named state has `%value' .false.: the
    !> two words of value_mod's `choice_type').
    !> .true. and .false. mean what the option's name plainly
    !> says (R6-46); an upstream t with a special meaning is a named
    !> constant.  An option whose value can also be a function (R6-37), a
    !> number or a string (R6-39) is an extension of this type with a
    !> `procedure' component, set with `=>', or an allocatable component
    !> named for what the value is (`%lines = 5'); assigning with `='
    !> clears them (`clear_kinds').  Not a port.
    type, extends(setting_type), public :: choice_setting_type
        !> On (.true.) or off (.false.); .false. in a named state.
        logical :: value = .false.
        !> The named constant of the state; 0 for on and off.
        integer :: choice = 0
    contains
        generic :: assignment(=) => assign_choice, assign_logical, assign_state
        generic :: set_local => set_local_choice, set_local_logical, set_local_setting
        procedure, private :: assign_choice => choice_assign_choice, assign_logical => choice_assign_logical
        procedure, private :: assign_state => choice_assign_state
        procedure, private :: set_local_choice => choice_set_local_choice
        procedure, private :: set_local_logical => choice_set_local_logical
        procedure, private :: set_local_setting => choice_set_local_setting
        !> Whether the state is off: neither on, a named constant nor a
        !> value of another kind (upstream nil; any other state is
        !> `.not. x%is_off()').
        procedure :: is_off => is_choice_setting_off
        !> The value of another kind (R6-39) that is set, as the value a
        !> buffer keeps of a forwarded setting (data_buffer_mod); FOUND is
        !> false when none is set.  This type has none.
        procedure :: get_kind_value => choice_get_kind_value
        !> Set the value of another kind that V, a value a buffer kept,
        !> holds; DONE is false when V is none.  This type has none.
        procedure :: set_kind_value => choice_set_kind_value
    end type choice_setting_type

    !> The `%length' of a history that uses `history_length'.  Not a port.
    integer, parameter, public :: HISTORY_LENGTH_DEFAULT = -huge(0)
    !> The `%length' of a history that is never truncated (upstream's
    !> `history-length' property t).  Not a port.
    integer, parameter, public :: HISTORY_LENGTH_UNLIMITED = huge(0)

    !> Maximum length of history lists before truncation takes place.
    !> A number means truncate to that length; truncation deletes old
    !> elements, and is done just after inserting a new element.
    !>
    !> This variable only affects history lists that don't specify their own
    !> maximum lengths.  Setting the `%length' component of a history
    !> overrides this default.  Port of minibuf.c history-length
    !> (a value meaning no truncation is not ported: Deferred).
    integer, public :: history_length = 100
    !> True means to delete duplicates in history.
    !> If set to true when adding a new history element, all previous identical
    !> elements are deleted from the history list.  Port of minibuf.c
    !> history-delete-duplicates.
    logical, public :: history_delete_duplicates = .false.

    !> A history list: the strings a prompt offers with M-p and M-n, newest
    !> first.  A prompt names its history (read_from_minibuffer's HISTORY),
    !> which `find_history' finds through the settings registry:
    !>
    !>     type(history_setting_type), target, public :: grep_history = &
    !>         history_setting_type(name="grep_history")
    !>     call forward_setting(grep_history)  ! in the module's init
    !>     grep_history%length = 50
    !>     call grep_history%append("make -k")
    !>
    !> Port of minibuf.c's history variables (the value and its
    !> `history-length' property).
    type, extends(setting_type), public :: history_setting_type
        !> The entries, newest first.
        type(string_type), allocatable :: items(:)
        !> The maximum length: HISTORY_LENGTH_DEFAULT uses `history_length',
        !> HISTORY_LENGTH_UNLIMITED never truncates.
        integer :: length = HISTORY_LENGTH_DEFAULT
    contains
        procedure :: append => history_append
    end type history_setting_type

    public :: clear_locals_procedure, append_locals_clearer

    abstract interface
        !> Forget the current buffer's values of some buffer-local
        !> variables kept outside the symbols (Fortran objects), as
        !> `clear_all_local_variables' does.  Not a port.
        subroutine clear_locals_procedure()
            implicit none(type, external)
        end subroutine clear_locals_procedure
    end interface

    !> A function `clear_all_local_variables' runs.
    type :: locals_clearer_type
        procedure(clear_locals_procedure), pointer, nopass :: fn => null()
    end type locals_clearer_type

    !> The functions forgetting buffer-local values kept in Fortran objects,
    !> run by `clear_all_local_variables'.
    type(locals_clearer_type), allocatable :: locals_clearers(:)

    !> A setting of a module above this one forwarded by its name, like
    !> the built-in table of data_buffer_mod (DEFVAR_LISP and friends).
    type :: forwarded_ref_type
        class(setting_type), pointer :: p => null()
        !> Buffer-local when set (make-variable-buffer-local).
        logical :: local_if_set = .false.
    end type forwarded_ref_type

    !> The settings forwarded with `forward_setting'.
    type(forwarded_ref_type), allocatable :: forwarded(:)

    public :: forward_setting, find_history, number_to_string, choice_state

    !> The decimal text of the integer N (as `write' with "(i0)").  Port
    !> of data.c:Fnumber_to_string (integers; data.c:fixnum_to_string).
    interface number_to_string
        module procedure number_to_string_int, number_to_string_int64
    end interface number_to_string

    public :: string_to_number

    !> Parse STRING as a decimal number and return the number.
    !> Ignore leading spaces and tabs, and all trailing chars.  Return 0 if
    !> STRING cannot be parsed as an integer or floating point number.
    !>
    !> If BASE, interpret STRING as a number in that base.  If BASE isn't
    !> present, base 10 is used.  BASE must be between 2 and 16 (inclusive).
    !> If the base used is not 10, STRING is always parsed as an integer.
    !>
    !> `call string_to_number(string, number, base, status)': NUMBER is an
    !> integer(int64) or a real(real64); a BASE out of range is an
    !> `args_out_of_range' error condition in STATUS (with STATUS absent the
    !> caller guarantees it; NUMBER is then 0).  Port of
    !> data.c:Fstring_to_number.
    interface string_to_number
        module procedure string_to_number_int64, string_to_number_real64
    end interface string_to_number

    public :: check_fixnum, check_symbol, check_string, check_list
    public :: value_eq
    public :: symbol_name, boundp, symbol_value, set, makunbound
    public :: make_local_variable, make_variable_buffer_local, clear_local_variable, clear_all_local_variables, &
              is_local_variable, is_local_variable_if_set, is_automatically_local, is_forwarded_variable, default_value, &
              function_setting_default, &
              set_default, default_boundp, buffer_local_value, buffer_local_variables, setq_local

    interface
        !> The value of the Fortran variable forwarded to by the symbol
        !> with id ID: `found' is false if it is a plain variable.  (In
        !> data_buffer_mod.)  Not a port.
        module subroutine forwarded_get(id, v, found, bound)
            implicit none(type, external)
            integer(int64), intent(in) :: id
            type(value_type), intent(out) :: v
            logical, intent(out) :: found
            !> False when the variable is void.
            logical, intent(out), optional :: bound
        end subroutine forwarded_get

        !> makunbound on the forwarded variable of symbol ID; `found' is
        !> false if it is a plain variable.  (In data_buffer_mod.)  Not a port.
        module subroutine forwarded_makunbound(id, status, found)
            implicit none(type, external)
            integer(int64), intent(in) :: id
            type(status_type), intent(out) :: status
            logical, intent(out) :: found
        end subroutine forwarded_makunbound

        !> Set the Fortran variable forwarded to by symbol ID; `found' is
        !> false if it is a plain variable.  (In data_buffer_mod.)  Not a port.
        module subroutine forwarded_set(id, v, status, found)
            implicit none(type, external)
            integer(int64), intent(in) :: id
            type(value_type), intent(in) :: v
            type(status_type), intent(out) :: status
            logical, intent(out) :: found
        end subroutine forwarded_set

        !> Whether the current buffer has a local value for symbol ID.
        !> (In data_buffer_mod.)  Not a port.
        module logical function local_in_current(id)
            implicit none(type, external)
            integer(int64), intent(in) :: id
        end function local_in_current

        !> Whether symbol ID becomes buffer-local when set (made so by
        !> `make_variable_buffer_local', or a per-buffer variable of
        !> buffer.c).  (In data_buffer_mod.)  Not a port.
        module logical function auto_local(id)
            implicit none(type, external)
            integer(int64), intent(in) :: id
        end function auto_local

        !> Make VARIABLE have a separate value in the current buffer.
        !> Other buffers will continue to share a common default value.
        !> (The buffer-local value of VARIABLE starts out as the same value
        !> VARIABLE previously had.)
        !>
        !> If the variable is already arranged to become local when set,
        !> this function causes a local value to exist for this buffer,
        !> just as setting the variable would do.  Port of
        !> data.c:Fmake_local_variable.  (In data_buffer_mod.)
        module subroutine make_local_variable(variable, status)
            implicit none(type, external)
            type(value_type), intent(in) :: variable
            type(status_type), intent(out) :: status
        end subroutine make_local_variable

        !> Make VARIABLE become buffer-local whenever it is set.
        !> At any time, the value for the current buffer is in effect,
        !> unless the variable has never been set in this buffer,
        !> in which case the default value is in effect.
        !> Note that binding the variable with `let', or setting it while
        !> a `let'-style binding made in this buffer is in effect,
        !> does not make the variable buffer-local.  Port of
        !> data.c:Fmake_variable_buffer_local.  (In data_buffer_mod.)
        module subroutine make_variable_buffer_local(variable, status)
            implicit none(type, external)
            type(value_type), intent(in) :: variable
            type(status_type), intent(out) :: status
        end subroutine make_variable_buffer_local

        !> Make VARIABLE no longer have a separate value in the current buffer.
        !> From now on the default value will apply in this buffer.  Port of
        !> data.c:Fkill_local_variable.  (In data_buffer_mod.)
        module subroutine clear_local_variable(variable, status)
            implicit none(type, external)
            type(value_type), intent(in) :: variable
            type(status_type), intent(out) :: status
        end subroutine clear_local_variable

        !> Switch to Fundamental mode by clearing current buffer's local variables.
        !> Most local variable bindings are eliminated so that the default values
        !> become effective once more.  Also, the syntax table is set from
        !> `standard_syntax_table', the local keymap is set to none,
        !> and the abbrev table from `fundamental_mode_abbrev_table'.
        !> This function also forces redisplay of the mode line.
        !>
        !> Every function to select a new major mode starts by
        !> calling this function.
        !>
        !> As a special exception, local variables whose names have
        !> a `permanent_local' attribute that is .true. are not eliminated by this function.
        !> If the optional KILL-PERMANENT argument is .true., clear out these local
        !> variables, too.
        !>
        !> The first thing this function does is run
        !> the normal hook `change_major_mode_hook'.  Port of
        !> buffer.c:Fkill_all_local_variables.  (In data_buffer_mod.)
        module subroutine clear_all_local_variables(clear_permanent)
            implicit none(type, external)
            logical, intent(in), optional :: clear_permanent
        end subroutine clear_all_local_variables

        !> True if VARIABLE has a local binding in buffer BUFFER (the
        !> slot number; the current buffer if absent).  Port of
        !> data.c:Flocal_variable_p.  (In data_buffer_mod.)
        module logical function is_local_variable(variable, buffer)
            implicit none(type, external)
            type(value_type), intent(in) :: variable
            integer(int64), intent(in), optional :: buffer
        end function is_local_variable

        !> True if VARIABLE is local in buffer BUFFER when set there.
        !> BUFFER defaults to the current buffer.
        !>
        !> More precisely, return .true. if either VARIABLE already has a local
        !> value in BUFFER, or if VARIABLE is automatically buffer-local (see
        !> `make_variable_buffer_local').  Port of
        !> data.c:Flocal_variable_if_set_p.  (In data_buffer_mod.)
        module logical function is_local_variable_if_set(variable, buffer)
            implicit none(type, external)
            type(value_type), intent(in) :: variable
            integer(int64), intent(in), optional :: buffer
        end function is_local_variable_if_set

        !> Whether VARIABLE is automatically buffer-local: it gets a local
        !> value in any buffer where it is set (`make_variable_buffer_local').
        !> (Ours; data.c's SYMBOL_LOCALIZED with local_if_set.)  (In
        !> data_buffer_mod.)  Not a port.
        module logical function is_automatically_local(variable)
            implicit none(type, external)
            type(value_type), intent(in) :: variable
        end function is_automatically_local

        !> Whether VARIABLE is forwarded to a Fortran setting, so its
        !> buffer-local values are those of the setting object.  (Ours;
        !> data.c's SYMBOL_FORWARDED.)  (In data_buffer_mod.)  Not a port.
        module logical function is_forwarded_variable(variable)
            implicit none(type, external)
            type(value_type), intent(in) :: variable
        end function is_forwarded_variable

        !> The default value of the forwarded function setting S (its value
        !> in buffers without their own), in FN.  Not a port: what
        !> default-value gives for such a variable.  (In data_buffer_mod.)
        module subroutine function_setting_default(s, fn)
            implicit none(type, external)
            type(function_setting_type), intent(in) :: s
            procedure(command_procedure), pointer, intent(out) :: fn
        end subroutine function_setting_default

        !> Return SYMBOL's default value.
        !> This is the value that is seen in buffers that do not have their own values
        !> for this variable.  The default value is meaningful for variables with
        !> local bindings in certain buffers.  Port of data.c:Fdefault_value.
        !> (In data_buffer_mod.)
        module subroutine default_value(symbol, v, status)
            implicit none(type, external)
            type(value_type), intent(in) :: symbol
            type(value_type), intent(out) :: v
            type(status_type), intent(out) :: status
        end subroutine default_value

        !> Return t if SYMBOL has a non-void default value.
        !> A variable may have a buffer-local value.  This function says whether
        !> the variable has a non-void value outside of the current buffer
        !> context.  Port of data.c:Fdefault_boundp.  (In data_buffer_mod.)
        module logical function default_boundp(symbol)
            implicit none(type, external)
            type(value_type), intent(in) :: symbol
        end function default_boundp

        !> Set SYMBOL's default value to VALUE.  SYMBOL and VALUE are evaluated.
        !> The default value is seen in buffers that do not have their own values
        !> for this variable.  Port of data.c:Fset_default
        !> (set_default_internal).  (In
        !> data_buffer_mod.)
        module subroutine set_default(symbol, v, status)
            implicit none(type, external)
            type(value_type), intent(in) :: symbol, v
            type(status_type), intent(out) :: status
        end subroutine set_default

        !> Return the value of VARIABLE in BUFFER (a slot number).
        !> If VARIABLE does not have a buffer-local binding in BUFFER, the value
        !> is the default binding of the variable.  Port of
        !> buffer.c:Fbuffer_local_value (with buffer.c:buffer_local_value).  (In
        !> data_buffer_mod.)
        module subroutine buffer_local_value(variable, buffer, v, status)
            implicit none(type, external)
            type(value_type), intent(in) :: variable
            integer(int64), intent(in) :: buffer
            type(value_type), intent(out) :: v
            type(status_type), intent(out) :: status
        end subroutine buffer_local_value

        !> Return a table of variables that are buffer-local in BUFFER.
        !> Most elements look like (SYMBOL VALUE), describing one variable.
        !> For a symbol that is locally unbound, just the symbol appears in the value.
        !> Note that storing new VALUEs in these elements doesn't change the variables.
        !> An absent argument means use current buffer as BUFFER.
        !> Port of buffer.c:Fbuffer_local_variables (with
        !> buffer.c:buffer_lisp_local_variables and buffer.c:buffer_local_variables_1;
        !> elements are two-element
        !> lists, since values hold proper lists only).
        !> (In data_buffer_mod.)
        module function buffer_local_variables(buffer) result(table)
            implicit none(type, external)
            integer(int64), intent(in), optional :: buffer
            type(value_type) :: table
        end function buffer_local_variables

        !> Make VARIABLE buffer-local in the current buffer and set it to
        !> VALUE.  Port of subr.el:setq-local (one pair).  (In
        !> data_buffer_mod.)
        module subroutine setq_local(variable, v, status)
            implicit none(type, external)
            character(len=*), intent(in) :: variable
            type(value_type), intent(in) :: v
            type(status_type), intent(out) :: status
        end subroutine setq_local
    end interface

contains

    !> The decimal text of N, a default integer.  Port of
    !> data.c:Fnumber_to_string (an integer; data.c:fixnum_to_string).
    pure function number_to_string_int(n) result(text)
        integer, intent(in) :: n
        character(len=:), allocatable :: text
        character(len=11) :: buffer

        write (buffer, "(i0)") n
        text = trim(buffer)
    end function number_to_string_int

    !> The decimal text of N.  Port of data.c:Fnumber_to_string (an
    !> integer; data.c:fixnum_to_string).
    pure function number_to_string_int64(n) result(text)
        integer(int64), intent(in) :: n
        character(len=:), allocatable :: text
        character(len=20) :: buffer

        write (buffer, "(i0)") n
        text = trim(buffer)
    end function number_to_string_int64

    !> STRING as an integer: a real number converted with `int'
    !> (truncated; a NaN gives 0), an integer beyond int64 the nearest
    !> int64 (upstream's bignums are not applicable).  Port of
    !> data.c:Fstring_to_number.
    subroutine string_to_number_int64(string, number, base, status)
        character(len=*), intent(in) :: string
        integer(int64), intent(out) :: number
        integer, intent(in), optional :: base
        type(status_type), intent(out), optional :: status
        real(real64) :: x
        logical :: is_float

        call string_to_number_parse(string, number, x, is_float, base, status)
        if (.not. is_float) return

        if (ieee_is_nan(x)) then
            number = 0
        else if (x >= 2.0_real64**63) then
            number = huge(0_int64)
        else if (x < -2.0_real64**63) then
            number = -huge(0_int64) - 1
        else
            number = int(x, int64)
        end if
    end subroutine string_to_number_int64

    !> STRING as a real value: the real number upstream returns, or its
    !> integer converted with `real'.  Port of data.c:Fstring_to_number.
    subroutine string_to_number_real64(string, number, base, status)
        character(len=*), intent(in) :: string
        real(real64), intent(out) :: number
        integer, intent(in), optional :: base
        type(status_type), intent(out), optional :: status
        integer(int64) :: n
        logical :: is_float

        call string_to_number_parse(string, n, number, is_float, base, status)
    end subroutine string_to_number_real64

    !> Parse STRING as `string_to_number' does: N (and X) for integer
    !> syntax, X with IS_FLOAT for real syntax in base 10, both 0 if it is
    !> neither.  Port of data.c:Fstring_to_number and
    !> lread.c:string_to_number.
    subroutine string_to_number_parse(string, n, x, is_float, base, status)
        character(len=*), intent(in) :: string
        integer(int64), intent(out) :: n
        real(real64), intent(out) :: x
        logical, intent(out) :: is_float
        integer, intent(in), optional :: base
        type(status_type), intent(out), optional :: status
        integer :: b, cp, first, digit, ios
        real(real64) :: value
        logical :: negative, signed, lead_int, trail_int, e_exp, overflow, value_known

        n = 0
        x = 0
        is_float = .false.
        b = 10

        if (present(base)) then
            if (.not. (base >= 2 .and. base <= 16)) then
                if (present(status)) call set_error(status, FM_ARGS_OUT_OF_RANGE, number_to_string(base))
                return
            end if

            b = base
        end if

        ! Ignore leading spaces and tabs.
        first = verify(string, " "//achar(9))
        if (first == 0) return
        cp = first
        negative = string(cp:cp) == "-"
        signed = negative .or. string(cp:cp) == "+"
        if (signed) cp = cp + 1
        value = 0
        value_known = .false.
        lead_int = .false.
        trail_int = .false.
        e_exp = .false.
        overflow = .false.

        do while (cp <= len(string))
            digit = digit_to_number(string(cp:cp), b)
            if (digit < 0) exit
            lead_int = .true.

            if (n > (huge(0_int64) - digit)/b) then
                overflow = .true.
            else
                n = n*b + digit
            end if

            x = x*b + digit
            cp = cp + 1
        end do

        if (cp <= len(string)) then
            if (string(cp:cp) == ".") cp = cp + 1
        end if

        if (b == 10) then
            do while (cp <= len(string))
                if (verify(string(cp:cp), "0123456789") /= 0) exit
                trail_int = .true.
                cp = cp + 1
            end do

            call parse_exponent()
            ! A float has digits after the dot or an exponent.
            ! This excludes numbers like "1." which are lexed as integers.
            is_float = trail_int .or. (lead_int .and. e_exp)
        end if

        if (is_float) then
            ! Convert to floating point, unless the value is already known
            ! because it is infinite or a NaN.
            if (.not. value_known) then
                read (string(merge(first + 1, first, signed):cp - 1), *, iostat=ios) value
                if (ios /= 0) value = 0
            end if

            x = merge(-value, value, negative)
            n = 0
            return
        end if

        if (.not. lead_int) then
            n = 0
            x = 0
            return
        end if

        if (overflow) n = huge(0_int64)

        if (negative) then
            n = -n
            if (overflow) n = n - 1
            x = -x
        end if

        if (.not. overflow) x = real(n, real64)

    contains

        !> Skip an exponent (e or E, an optional sign and digits, or +INF
        !> and +NaN for an infinity and a NaN).
        subroutine parse_exponent()
            integer :: ecp

            if (cp > len(string)) return
            if (string(cp:cp) /= "e" .and. string(cp:cp) /= "E") return
            ecp = cp
            cp = cp + 1

            if (cp <= len(string)) then
                if (string(cp:cp) == "+" .or. string(cp:cp) == "-") cp = cp + 1
            end if

            if (cp <= len(string)) then
                if (verify(string(cp:cp), "0123456789") == 0) then
                    e_exp = .true.

                    do while (cp <= len(string))
                        if (verify(string(cp:cp), "0123456789") /= 0) exit
                        cp = cp + 1
                    end do

                    return
                end if
            end if

            if (string(cp - 1:cp - 1) == "+" .and. cp + 2 <= len(string)) then
                if (string(cp:cp + 2) == "INF") then
                    e_exp = .true.
                    cp = cp + 3
                    value = ieee_value(value, ieee_positive_inf)
                    value_known = .true.
                    return
                else if (string(cp:cp + 2) == "NaN") then
                    e_exp = .true.
                    cp = cp + 3
                    value = ieee_value(value, ieee_quiet_nan)
                    value_known = .true.
                    return
                end if
            end if

            cp = ecp
        end subroutine parse_exponent
    end subroutine string_to_number_parse

    !> The value of digit C in BASE, or -1 if it is not one.  Port of
    !> lread.c:digit_to_number.
    pure integer function digit_to_number(c, base) result(digit)
        character, intent(in) :: c
        integer, intent(in) :: base

        select case (c)
        case ("0":"9")
            digit = iachar(c) - iachar("0")
        case ("a":"z")
            digit = iachar(c) - iachar("a") + 10
        case ("A":"Z")
            digit = iachar(c) - iachar("A") + 10
        case default
            digit = -1
        end select

        if (digit >= base) digit = -1
    end function digit_to_number

    !> Make the variable named after setting S stand for S, as the
    !> built-in settings do: its value is S's, `make_local_variable' and
    !> `set_local' give a buffer its own value of S, swapped in with the
    !> buffer.  With LOCAL_IF_SET it is buffer-local whenever it is set
    !> (make-variable-buffer-local).  For the logical, integer and string
    !> settings of modules this one cannot see (data.c's DEFVAR_*
    !> forwarding).  Not a port.
    subroutine forward_setting(s, local_if_set)
        class(setting_type), intent(inout), target :: s
        logical, intent(in), optional :: local_if_set
        type(forwarded_ref_type) :: ref
        integer(int64) :: id
        integer :: k

        if (.not. allocated(forwarded)) allocate (forwarded(0))

        do k = 1, size(forwarded)
            if (associated(forwarded(k)%p, s)) return
        end do

        ref%p => s
        if (present(local_if_set)) ref%local_if_set = local_if_set
        forwarded = [forwarded, ref]
        ! Resolve the symbol again.
        call ensure_obarray()
        id = xsymbol(value_type(name=trim(s%name)))
        symbols(id)%fwd = 0
    end subroutine forward_setting

    !> Have `clear_all_local_variables' run FN in the buffer whose local
    !> variables it clears (once, however often it is added): for modules
    !> keeping buffer-local variables of derived types in Fortran objects.
    !> Not a port.
    subroutine append_locals_clearer(fn)
        procedure(clear_locals_procedure) :: fn
        integer :: k

        if (.not. allocated(locals_clearers)) allocate (locals_clearers(0))

        do k = 1, size(locals_clearers)
            if (associated(locals_clearers(k)%fn, fn)) return
        end do

        locals_clearers = [locals_clearers, locals_clearer_type(fn)]
    end subroutine append_locals_clearer

    ! ------------------------------------------------------------------
    ! Settings
    ! ------------------------------------------------------------------

    ! Not a port.
    subroutine setting_make_local(self)
        class(setting_type), intent(inout) :: self
        type(status_type) :: st

        call make_local_variable(value_type(name=trim(self%name)), st)
    end subroutine setting_make_local

    ! Not a port.
    subroutine setting_clear_local(self)
        class(setting_type), intent(inout) :: self
        type(status_type) :: st

        call clear_local_variable(value_type(name=trim(self%name)), st)
    end subroutine setting_clear_local

    ! Not a port.
    logical function is_setting_local(self)
        class(setting_type), intent(in) :: self

        is_setting_local = is_local_variable(value_type(name=trim(self%name)))
    end function is_setting_local

    !> Forget voidness and the value's other kinds: the Fortran value is
    !> the value now.  Not a port.
    subroutine settled(self)
        class(setting_type), intent(inout) :: self

        self%void = .false.
        call self%clear_kinds()
    end subroutine settled

    !> Nothing to forget: this type has no other kinds.  Not a port.
    subroutine setting_clear_kinds(self)
        class(setting_type), intent(inout) :: self

        if (self%void) return
    end subroutine setting_clear_kinds

    !> "": this type has no other kinds.  Not a port.
    function setting_other_value(self) result(text)
        class(setting_type), intent(in) :: self
        character(len=:), allocatable :: text

        text = ""
        if (self%void) return
    end function setting_other_value

    ! Not a port.
    subroutine integer_assign_int(self, v)
        class(integer_setting_type), intent(inout) :: self
        integer, intent(in) :: v

        self%value = v
        call settled(self)
    end subroutine integer_assign_int

    ! Not a port.
    subroutine integer_assign_int64(self, v)
        class(integer_setting_type), intent(inout) :: self
        integer(int64), intent(in) :: v

        self%value = v
        call settled(self)
    end subroutine integer_assign_int64

    !> Give the current buffer its own value V.  Port of subr.el:setq-local.
    subroutine integer_set_local_int(self, v)
        class(integer_setting_type), intent(inout) :: self
        integer, intent(in) :: v

        call self%make_local()
        self%value = v
        call settled(self)
    end subroutine integer_set_local_int

    !> Give the current buffer its own value V.  Port of subr.el:setq-local.
    subroutine integer_set_local_int64(self, v)
        class(integer_setting_type), intent(inout) :: self
        integer(int64), intent(in) :: v

        call self%make_local()
        self%value = v
        call settled(self)
    end subroutine integer_set_local_int64

    !> Give the current buffer its own value, the value of V (as
    !> `(setq-local SELF V)').  Port of subr.el:setq-local.
    subroutine integer_set_local_setting(self, v)
        class(integer_setting_type), intent(inout) :: self
        class(integer_setting_type), intent(in) :: v

        call self%make_local()
        self%value = v%value
        call settled(self)
    end subroutine integer_set_local_setting

    ! Not a port.
    subroutine logical_assign(self, v)
        class(logical_setting_type), intent(inout) :: self
        logical, intent(in) :: v

        self%value = v
        call settled(self)
    end subroutine logical_assign

    !> Give the current buffer its own value V.  Port of subr.el:setq-local.
    subroutine logical_set_local(self, v)
        class(logical_setting_type), intent(inout) :: self
        logical, intent(in) :: v

        call self%make_local()
        self%value = v
        call settled(self)
    end subroutine logical_set_local

    ! Not a port.
    subroutine string_assign(self, v)
        class(string_setting_type), intent(inout) :: self
        character(len=*), intent(in) :: v

        self%value = v
        call settled(self)
    end subroutine string_assign

    !> Give the current buffer its own value V.  Port of subr.el:setq-local.
    subroutine string_set_local(self, v)
        class(string_setting_type), intent(inout) :: self
        character(len=*), intent(in) :: v

        call self%make_local()
        self%value = v
        call settled(self)
    end subroutine string_set_local

    ! Not a port.
    subroutine string_clear(self)
        class(string_setting_type), intent(inout) :: self

        if (allocated(self%value)) deallocate (self%value)
        call settled(self)
    end subroutine string_clear

    !> Give the current buffer its own value V.  Port of subr.el:setq-local.
    subroutine function_set_local(self, v)
        class(function_setting_type), intent(inout) :: self
        procedure(command_procedure) :: v

        call self%make_local()
        self%value => v
        self%command => null()
        call settled(self)
    end subroutine function_set_local

    !> None: this type has no other kinds.  Not a port.
    subroutine choice_get_kind_value(self, v, found)
        class(choice_setting_type), intent(in) :: self
        type(value_type), intent(out) :: v
        logical, intent(out) :: found

        found = .false.
        v = value_type()
        if (self%void) return
    end subroutine choice_get_kind_value

    !> None: this type has no other kinds.  Not a port.
    subroutine choice_set_kind_value(self, v, done)
        class(choice_setting_type), intent(inout) :: self
        type(value_type), intent(in) :: v
        logical, intent(out) :: done

        done = .false.
        if (self%void .or. .not. allocated(v%a)) return
    end subroutine choice_set_kind_value

    ! Not a port.
    logical function is_choice_setting_off(self)
        class(choice_setting_type), intent(in) :: self

        is_choice_setting_off = is_choice_off(self%value, self%choice) .and. len(self%other_value()) == 0
    end function is_choice_setting_off

    !> Set the state to STATE, a `choice_type' (as `choice_state' gives
    !> it).  Not a port.
    subroutine choice_assign_state(self, state)
        class(choice_setting_type), intent(inout) :: self
        type(choice_type), intent(in) :: state

        self%value = state%value
        self%choice = state%choice
        call settled(self)
    end subroutine choice_assign_state

    !> The state of choice setting S as a `choice_type', to keep or pass
    !> on: `saved = choice_state(x)', later `x = saved'.  Not a port.
    function choice_state(s) result(state)
        class(choice_setting_type), intent(in) :: s
        type(choice_type) :: state

        state%value = s%value
        state%choice = s%choice
    end function choice_state

    !> Give the current buffer its own state, the state of V (as
    !> `(setq-local SELF V)').  Port of subr.el:setq-local.
    subroutine choice_set_local_setting(self, v)
        class(choice_setting_type), intent(inout) :: self
        class(choice_setting_type), intent(in) :: v

        call self%make_local()
        self%value = v%value
        self%choice = v%choice
        call settled(self)
    end subroutine choice_set_local_setting

    !> Set the state to the named constant NAMED.  Not a port.
    subroutine choice_assign_choice(self, named)
        class(choice_setting_type), intent(inout) :: self
        integer, intent(in) :: named

        call set_choice_state(self%value, self%choice, named=named)
        call settled(self)
    end subroutine choice_assign_choice

    !> Set the choice to its on (.true.) or off (.false.) state.
    !> Not a port.
    subroutine choice_assign_logical(self, on)
        class(choice_setting_type), intent(inout) :: self
        logical, intent(in) :: on

        call set_choice_state(self%value, self%choice, on=on)
        call settled(self)
    end subroutine choice_assign_logical

    !> Give the current buffer its own state, the named constant NAMED.
    !> Port of subr.el:setq-local.
    subroutine choice_set_local_choice(self, named)
        class(choice_setting_type), intent(inout) :: self
        integer, intent(in) :: named

        call self%make_local()
        call choice_assign_choice(self, named)
    end subroutine choice_set_local_choice

    !> Give the current buffer its own on (.true.) or off (.false.)
    !> state.  Port of subr.el:setq-local.
    subroutine choice_set_local_logical(self, on)
        class(choice_setting_type), intent(inout) :: self
        logical, intent(in) :: on

        call self%make_local()
        call choice_assign_logical(self, on)
    end subroutine choice_set_local_logical

    ! ------------------------------------------------------------------
    ! Type checks (lisp.h CHECK_*)
    ! ------------------------------------------------------------------

    ! Not a port.
    subroutine wrong_type(status, test)
        type(status_type), intent(inout) :: status
        character(len=*), intent(in) :: test

        call set_error(status, FM_WRONG_TYPE_ARGUMENT, test)
    end subroutine wrong_type

    !> Port of lisp.h:CHECK_FIXNUM.
    subroutine check_fixnum(v, status)
        type(value_type), intent(in) :: v
        type(status_type), intent(out) :: status

        if (vtag(v) /= V_INTEGER) call wrong_type(status, "expected integer")
    end subroutine check_fixnum

    !> Port of lisp.h:CHECK_SYMBOL.
    subroutine check_symbol(v, status)
        type(value_type), intent(in) :: v
        type(status_type), intent(out) :: status

        if ((vtag(v) /= V_NAME .and. vtag(v) /= V_NULL)) call wrong_type(status, "expected name")
    end subroutine check_symbol

    !> Port of lisp.h:CHECK_STRING.
    subroutine check_string(v, status)
        type(value_type), intent(in) :: v
        type(status_type), intent(out) :: status

        if (vtag(v) /= V_STRING) call wrong_type(status, "expected character(len=*)")
    end subroutine check_string

    !> Port of lisp.h:CHECK_LIST.
    subroutine check_list(v, status)
        type(value_type), intent(in) :: v
        type(status_type), intent(out) :: status

        if ((vtag(v) /= V_ARRAY .and. vtag(v) /= V_NULL)) call wrong_type(status, "expected array")
    end subroutine check_list

    ! ------------------------------------------------------------------
    ! Identity and lists
    ! ------------------------------------------------------------------

    !> `eq': same integer, symbol or object handle.  Strings and lists have
    !> no identity once copied, so they are never `eq' (upstream: only the
    !> very same object is).  Port of data.c:Feq.
    elemental logical function value_eq(a, b)
        type(value_type), intent(in) :: a, b

        value_eq = .false.
        if (vtag(a) /= vtag(b)) return

        select case (vtag(a))
        case (V_NULL)
            value_eq = .true.
        case (V_INTEGER, V_NAME)
            value_eq = a%a(1)%i == b%a(1)%i
        case (V_BUFFER, V_MARKER, V_KEYMAP, V_SYNTAX_TABLE, V_WINDOW)
            value_eq = a%a(1)%h%index == b%a(1)%h%index .and. &
                       a%a(1)%h%generation == b%a(1)%h%generation
        case default
            ! Nothing to do for other values.
        end select
    end function value_eq

    ! ------------------------------------------------------------------
    ! Symbols
    ! ------------------------------------------------------------------

    !> Port of data.c:Fsymbol_name.
    subroutine symbol_name(symbol, name, status)
        type(value_type), intent(in) :: symbol
        character(len=:), allocatable, intent(out) :: name
        type(status_type), intent(out) :: status

        call check_symbol(symbol, status)
        if (.not. status%ok()) return
        call ensure_obarray()
        name = symbols(xsymbol(symbol))%name
    end subroutine symbol_name

    !> Port of data.c:Fboundp (global value only).
    subroutine boundp(symbol, bound, status)
        type(value_type), intent(in) :: symbol
        logical, intent(out) :: bound
        type(status_type), intent(out) :: status
        type(value_type) :: v
        logical :: found

        bound = .false.
        call check_symbol(symbol, status)
        if (.not. status%ok()) return
        call ensure_obarray()
        call forwarded_get(xsymbol(symbol), v, found, bound)
        if (.not. found) bound = symbols(xsymbol(symbol))%bound
    end subroutine boundp

    !> The value of `symbol'; void-variable if unbound.
    !> Port of data.c:Fsymbol_value (global value only).
    subroutine symbol_value(symbol, v, status)
        type(value_type), intent(in) :: symbol
        type(value_type), intent(out) :: v
        type(status_type), intent(out) :: status
        integer(int64) :: id
        logical :: found, bound

        call check_symbol(symbol, status)
        if (.not. status%ok()) return
        call ensure_obarray()
        id = xsymbol(symbol)
        call forwarded_get(id, v, found, bound)

        if (found) then
            if (.not. bound) call set_error(status, FM_VOID_VARIABLE, symbols(id)%name)
            return
        end if

        if (.not. symbols(id)%bound) then
            call set_error(status, FM_VOID_VARIABLE, symbols(id)%name)
            return
        end if

        v = symbols(id)%val
    end subroutine symbol_value

    !> Give `symbol' the value `newval'.  Constants (nil, t, keywords)
    !> signal setting-constant, except a keyword set to itself.
    !> Port of data.c:Fset (data.c:set_internal; global value only).
    recursive subroutine set(symbol, newval, status)
        type(value_type), intent(in) :: symbol, newval
        type(status_type), intent(out) :: status
        integer(int64) :: id
        logical :: found

        call check_symbol(symbol, status)
        if (.not. status%ok()) return
        call ensure_obarray()
        id = xsymbol(symbol)

        if (symbols(id)%constant) then
            if (.not. (index(symbols(id)%name, ":") == 1 .and. value_eq(newval, symbols(id)%val))) then
                call set_error(status, FM_SETTING_CONSTANT, symbols(id)%name)
            end if

            return
        end if

        ! An automatically buffer-local variable becomes local when set.
        if (auto_local(id)) then
            if (.not. local_in_current(id)) then
                call make_local_variable(symbol, status)
                if (.not. status%ok()) return
            end if
        end if

        call forwarded_set(id, newval, status, found)
        if (found) return
        symbols(id)%val = newval
        symbols(id)%bound = .true.
    end subroutine set

    !> Make `symbol' unbound.  Port of data.c:Fmakunbound.
    subroutine makunbound(symbol, status)
        type(value_type), intent(in) :: symbol
        type(status_type), intent(out) :: status
        integer(int64) :: id
        logical :: found

        call check_symbol(symbol, status)
        if (.not. status%ok()) return
        call ensure_obarray()
        id = xsymbol(symbol)

        if (symbols(id)%constant) then
            call set_error(status, FM_SETTING_CONSTANT, symbols(id)%name)
            return
        end if

        ! Fset (symbol, Qunbound): an automatically buffer-local variable
        ! gets a void local binding.
        if (auto_local(id)) then
            if (.not. local_in_current(id)) then
                call make_local_variable(symbol, status)
                if (.not. status%ok()) return
            end if
        end if

        call forwarded_makunbound(id, status, found)
        if (found) return
        symbols(id)%val = value_type()
        symbols(id)%bound = .false.
    end subroutine makunbound

    ! ------------------------------------------------------------------
    ! Histories
    ! ------------------------------------------------------------------

    !> The history called NAME in the settings registry (null if there
    !> is none).  With CREATE, a history not registered yet is made
    !> (taking the value a variable of that name had), as read_minibuf
    !> makes an unbound history variable.  Not a port.
    function find_history(name, create) result(h)
        character(len=*), intent(in) :: name
        logical, intent(in), optional :: create
        type(history_setting_type), pointer :: h
        type(value_type) :: old
        type(status_type) :: st
        integer :: k

        h => null()

        if (allocated(forwarded)) then
            do k = 1, size(forwarded)
                select type (p => forwarded(k)%p)
                type is (history_setting_type)
                    if (trim(p%name) == name) then
                        h => p
                        return
                    end if
                end select
            end do
        end if

        if (.not. present(create)) return
        if (.not. create) return
        call symbol_value(value_type(name=name), old, st)
        allocate (h)
        h%name = name
        call forward_setting(h)
        if (st%ok()) call set(value_type(name=name), old, st)
    end function find_history

    !> Add NEWELT to the history list SELF.
    !> If MAXELT is present, it specifies the maximum length of the history.
    !> Otherwise, the maximum history length is the history's `%length',
    !> if set, or the value of the `history_length' variable.  The
    !> possible values of maximum length have the same meaning as the
    !> values of `history_length'.
    !> Remove duplicates of NEWELT if `history_delete_duplicates' is .true..
    !> If optional fourth arg KEEP-ALL is .true., add NEWELT to history even
    !> if it is empty or duplicates the most recent entry in the history.
    !> Port of subr.el:add-to-history (`added' says whether the list
    !> changed).
    subroutine history_append(self, newelt, maxelt, keep_all, added)
        class(history_setting_type), intent(inout) :: self
        character(len=*), intent(in) :: newelt
        integer, intent(in), optional :: maxelt
        logical, intent(in), optional :: keep_all
        logical, intent(out), optional :: added
        type(string_type), allocatable :: kept(:)
        logical :: all
        integer :: limit, k, n

        if (present(added)) added = .false.
        limit = self%length
        if (limit == HISTORY_LENGTH_DEFAULT) limit = history_length
        if (present(maxelt)) limit = maxelt
        all = .false.
        if (present(keep_all)) all = keep_all
        if (.not. allocated(self%items)) allocate (self%items(0))
        if (.not. all .and. len(newelt) == 0) return

        if (.not. all .and. size(self%items) > 0) then
            if (self%items(1)%text == newelt .and. len(self%items(1)%text) == len(newelt)) return
        end if

        allocate (kept(size(self%items) + 1))
        kept(1)%text = newelt
        n = 1

        do k = 1, size(self%items)
            ! (delete newelt history)
            if (history_delete_duplicates) then
                if (self%items(k)%text == newelt .and. len(self%items(k)%text) == len(newelt)) cycle
            end if

            n = n + 1
            kept(n)%text = self%items(k)%text
        end do

        if (limit <= 0) then
            n = 0
        else
            n = min(n, limit)
        end if

        self%items = kept(1:n)
        self%void = .false.
        if (present(added)) added = .true.
    end subroutine history_append
end module data_mod
