! eval_fm.f90 -- hooks and calling functions.
! Ported from GNU Emacs 31.1 src/eval.c and lisp/subr.el (add-hook,
! remove-hook).
!
! Copyright (C) 1985-1987, 1993-1995, 1999-2026 Free Software Foundation,
! Inc.
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

!> Hooks.  A hook is an object holding procedures, run in order:
!>
!>     call text_mode_hook%append(turn_on_auto_fill)
!>     call after_change_functions%append(my_after_change, local=.true.)
!>
!> Upstream a hook is a variable whose value is a list of functions; a
!> buffer-local value holds `t' where the default value's functions run.
!> Here a buffer's own functions are kept in the hook by buffer, with a
!> `t' entry in the same place.
!>
!> The kinds of hook differ only in the arguments their functions take:
!> `hook_type' (normal hooks: no arguments, `command_procedure'),
!> `change_hook_type' (`change_procedure': the bounds of a change) and
!> `after_change_hook_type' (`after_change_procedure': the bounds and the
!> old length), and others such as `capf_hook_type' (`capf_procedure':
!> the completion data at point).  They extend `hook_base_type', which keeps the functions,
!> buffers' own values and depths (subr.el's add-hook and remove-hook)
!> once for all kinds; an entry holds a procedure pointer of its hook's
!> kind, and each kind adds the type-bound procedures taking that kind of
!> procedure and running it with its arguments (eval.c's
!> run_hook_with_args).
!>
!> `until_success_hook_type' runs its functions until one succeeds
!> (`run-hook-with-args-until-success').
!>
!> Deferred: the evaluator, and `run-hook-with-args-until-failure' and
!> `run-hook-wrapped' as such (no hook uses them yet).
module eval_fm
    use, intrinsic :: iso_fortran_env, only: int64
    use error_fm, only: status_type, set_error, FM_GENERIC_ERROR, FM_VOID_FUNCTION
    use lisp_fm, only: value_type, command_procedure, symbols, xsymbol, vtag, V_NULL, V_NAME
    use handle_fm, only: handle_type
    use callint_fm, only: current_prefix_arg, prefix_arg_type
    use buffer_fm, only: buffers, current
    use minibuf_table_fm, only: capf_data_type
    implicit none(type, external)
    private

    public :: clear_local_hooks, hook_error_procedure
    public :: change_procedure, after_change_procedure, position_procedure, region_procedure, region_entry_type
    public :: string_procedure, string_filter_procedure, capf_procedure, capf_entry_type, until_success_procedure
    public :: overlay_change_procedure, execute_property_function, execute_overlay_change_function

    !> The value `current_prefix_arg' had where the normal hook whose
    !> function is running was run.  The functions of a normal hook run
    !> as functions of no arguments, so they see no prefix argument;
    !> those that read the dynamic value of current-prefix-arg upstream
    !> (`post_self_insert_hook' functions) read this.
    type(prefix_arg_type), public :: hook_current_prefix_arg

    abstract interface
        !> A function of a `change_hook_type' hook such as
        !> `before_change_functions': BEGINNING and END are the bounds of the
        !> text about to change.  Not a port.
        subroutine change_procedure(beginning, end, status)
            import :: int64, status_type
            implicit none(type, external)
            integer(int64), intent(in) :: beginning, end
            type(status_type), intent(out) :: status
        end subroutine change_procedure

        !> A function of an `after_change_hook_type' hook such as
        !> `after_change_functions': BEGINNING and END are the bounds of the
        !> changed text, OLD_LEN the length of the text it replaced.
        !> Not a port.
        subroutine after_change_procedure(beginning, end, old_len, status)
            import :: int64, status_type
            implicit none(type, external)
            integer(int64), intent(in) :: beginning, end, old_len
            type(status_type), intent(out) :: status
        end subroutine after_change_procedure

        !> A function of a `position_hook_type' hook such as
        !> `fontification_functions': POS is the position to work from.
        !> Not a port.
        subroutine position_procedure(pos, status)
            import :: int64, status_type
            implicit none(type, external)
            integer(int64), intent(in) :: pos
            type(status_type), intent(out) :: status
        end subroutine position_procedure

        !> A function of a `region_hook_type' hook such as
        !> `jit_lock_functions': do something to the text from BEGINNING to END,
        !> giving in DONE_BEGINNING and DONE_END the part it actually did
        !> (upstream's return value (jit-lock-bounds BEGINNING . END); by default
        !> BEGINNING and END).  Not a port.
        subroutine region_procedure(beginning, end, done_beginning, done_end, status)
            import :: int64, status_type
            implicit none(type, external)
            integer(int64), intent(in) :: beginning, end
            integer(int64), intent(out) :: done_beginning, done_end
            type(status_type), intent(out) :: status
        end subroutine region_procedure

        !> Report the error STATUS that FN, a function of the hook named
        !> HOOK_NAME, returned with (`hook_type%execute_safe').  Not a port.
        subroutine hook_error_procedure(hook_name, fn, status)
            import :: command_procedure, status_type
            implicit none(type, external)
            character(len=*), intent(in) :: hook_name
            procedure(command_procedure) :: fn
            type(status_type), intent(in) :: status
        end subroutine hook_error_procedure

        !> A function of a `string_hook_type' hook such as
        !> `comint_output_filter_functions': STRING is the text it is
        !> about.  Not a port.
        subroutine string_procedure(string, status)
            import :: status_type
            implicit none(type, external)
            character(len=*), intent(in) :: string
            type(status_type), intent(out) :: status
        end subroutine string_procedure

        !> A function of a `string_filter_hook_type' hook such as
        !> `comint_preoutput_filter_functions': RESULT is STRING
        !> transformed (perhaps STRING itself).  Not a port.
        subroutine string_filter_procedure(string, result, status)
            import :: status_type
            implicit none(type, external)
            character(len=*), intent(in) :: string
            character(len=:), allocatable, intent(out) :: result
            type(status_type), intent(out) :: status
        end subroutine string_filter_procedure

        !> A function of a `capf_hook_type' hook such as
        !> `completion_at_point_functions': the completion data for the
        !> entity at point, if it applies there.  Not a port.
        recursive subroutine capf_procedure(data, status)
            import :: capf_data_type, status_type
            implicit none(type, external)
            type(capf_data_type), intent(out) :: data
            type(status_type), intent(out) :: status
        end subroutine capf_procedure

        !> A function of an `until_success_hook_type' hook such as
        !> `write_file_functions': DONE .true. (stop at the first function that succeeds)
        !> stops the hook.
        recursive subroutine until_success_procedure(done, status)
            import :: status_type
            implicit none(type, external)
            logical, intent(out) :: done
            type(status_type), intent(out) :: status
        end subroutine until_success_procedure

        !> A function named by an overlay's `modification_hooks',
        !> `insert_in_front_hooks' or `insert_behind_hooks' property:
        !> OVERLAY is the overlay, AFTER false before the change and true
        !> after it, BEGINNING and END the bounds of the change, and OLD_LEN
        !> (present only after the change) the length of the text it
        !> replaced.  Not a port.
        recursive subroutine overlay_change_procedure(overlay, after, beginning, end, status, old_len)
            import :: handle_type, int64, status_type
            implicit none(type, external)
            type(handle_type), intent(in) :: overlay
            logical, intent(in) :: after
            integer(int64), intent(in) :: beginning, end
            type(status_type), intent(out) :: status
            integer(int64), intent(in), optional :: old_len
        end subroutine overlay_change_procedure
    end interface

    !> A function of a `capf_hook_type' hook, as `capf_hook_functions'
    !> lists them.
    type :: capf_entry_type
        procedure(capf_procedure), pointer, nopass :: fn => null()
    end type capf_entry_type

    !> A function of a `region_hook_type' hook, as `region_hook_functions'
    !> lists them.
    type :: region_entry_type
        procedure(region_procedure), pointer, nopass :: fn => null()
    end type region_entry_type

    !> One function of a hook, or the `t' marker of a buffer's own value.
    !> The pointer of the hook's kind is the one set.  In a hook's depths
    !> (subr.el's `hook--depth-alist'), the depth a function was added
    !> with.
    type :: hook_entry_type
        procedure(command_procedure), pointer, nopass :: fn => null()
        procedure(change_procedure), pointer, nopass :: change_fn => null()
        procedure(after_change_procedure), pointer, nopass :: after_change_fn => null()
        procedure(position_procedure), pointer, nopass :: position_fn => null()
        procedure(region_procedure), pointer, nopass :: region_fn => null()
        procedure(string_procedure), pointer, nopass :: string_fn => null()
        procedure(string_filter_procedure), pointer, nopass :: string_filter_fn => null()
        procedure(capf_procedure), pointer, nopass :: capf_fn => null()
        procedure(until_success_procedure), pointer, nopass :: until_success_fn => null()
        integer :: depth = 0
        logical :: is_t = .false.
    end type hook_entry_type

    !> One buffer's own depths (the depth table made buffer-local).
    type :: local_depths_type
        integer(int64) :: serial = 0
        type(hook_entry_type), allocatable :: depths(:)
    end type local_depths_type

    !> The functions of one buffer's own (buffer-local) value.
    type :: local_hook_type
        !> The buffer's serial number (buffer slots are reused).
        integer(int64) :: serial = 0
        type(hook_entry_type), allocatable :: entries(:)
    end type local_hook_type

    !> What every hook has: its functions in order, buffers' own
    !> functions and the depths they were added with.  The kinds of hook
    !> extend it.
    type, abstract, public :: hook_base_type
        !> The hook's name, as the help commands show it.
        character(len=64) :: name = ""
        !> The default (global) functions.
        type(hook_entry_type), allocatable :: entries(:)
        !> Buffers' own functions.
        type(local_hook_type), allocatable :: locals(:)
        !> The depths functions were added with, and buffers' own.
        type(hook_entry_type), allocatable :: depths(:)
        type(local_depths_type), allocatable :: local_depths(:)
        !> Keep buffers' own functions when the major mode changes (the
        !> `permanent_local' property).
        logical :: permanent_local = .false.
    contains
        !> Forget the current buffer's own functions.
        procedure :: clear_local => hook_clear_local
        !> Whether the current buffer has its own functions.
        procedure :: local_p => is_hook_local
        !> Whether the hook's value in the current buffer is empty.
        procedure :: empty => hook_empty
        !> Make the hook's value in the current buffer empty.
        procedure :: clear => hook_clear
        procedure, private :: append_entry => hook_append_entry
        procedure, private :: remove_entry => hook_remove_entry
        procedure, private :: member_entry => hook_member_entry
        procedure, private :: execute_entries => hook_execute_entries
    end type hook_base_type

    !> A pointer to a hook.
    type :: hook_ref_type
        class(hook_base_type), pointer :: p => null()
    end type hook_ref_type

    !> Hooks that have been given buffers' own functions.
    type(hook_ref_type), allocatable :: local_hooks(:)

    !> A normal hook: functions of no arguments (`command_procedure').
    type, extends(hook_base_type), public :: hook_type
    contains
        !> Add a function to the hook (subr.el's add-hook).
        procedure :: append => hook_append
        !> Remove a function from the hook (subr.el's remove-hook).
        procedure :: remove => hook_remove
        !> Run the hook's functions (run-hooks).
        procedure :: execute => hook_execute
        !> Whether FN is one of the functions run in the current buffer.
        procedure :: member => hook_member
        !> Run the hook's functions, reporting and removing the ones that
        !> return with an error condition (keyboard.c:safe_run_hooks).
        procedure :: execute_safe => hook_execute_safe
    end type hook_type

    !> A hook of functions taking the bounds of a change
    !> (`change_procedure'), like `before_change_functions'.
    type, extends(hook_base_type), public :: change_hook_type
    contains
        !> Add a function to the hook (subr.el's add-hook).
        procedure :: append => change_hook_append
        !> Remove a function from the hook (subr.el's remove-hook).
        procedure :: remove => change_hook_remove
        !> Run the hook's functions with the bounds of a change
        !> (run-hook-with-args).
        procedure :: execute => change_hook_execute
        !> Whether FN is one of the functions run in the current buffer.
        procedure :: member => change_hook_member
    end type change_hook_type

    !> A hook of functions taking the bounds of a change and the length
    !> of the text it replaced (`after_change_procedure'), like
    !> `after_change_functions'.
    type, extends(hook_base_type), public :: after_change_hook_type
    contains
        !> Add a function to the hook (subr.el's add-hook).
        procedure :: append => after_change_hook_append
        !> Remove a function from the hook (subr.el's remove-hook).
        procedure :: remove => after_change_hook_remove
        !> Run the hook's functions with the bounds of a change and the
        !> old length (run-hook-with-args).
        procedure :: execute => after_change_hook_execute
        !> Whether FN is one of the functions run in the current buffer.
        procedure :: member => after_change_hook_member
    end type after_change_hook_type

    !> A hook of functions taking a position (`position_procedure'), like
    !> `fontification_functions'.
    type, extends(hook_base_type), public :: position_hook_type
    contains
        !> Add a function to the hook (subr.el's add-hook).
        procedure :: append => position_hook_append
        !> Remove a function from the hook (subr.el's remove-hook).
        procedure :: remove => position_hook_remove
        !> Run the hook's functions with a position (run-hook-with-args).
        procedure :: execute => position_hook_execute
        !> Whether FN is one of the functions run in the current buffer.
        procedure :: member => position_hook_member
    end type position_hook_type

    !> A hook of functions taking a region and telling what part of it
    !> they did (`region_procedure'), like `jit_lock_functions'.
    type, extends(hook_base_type), public :: region_hook_type
    contains
        !> Add a function to the hook (subr.el's add-hook).
        procedure :: append => region_hook_append
        !> Remove a function from the hook (subr.el's remove-hook).
        procedure :: remove => region_hook_remove
        !> The functions run in the current buffer, in order (for
        !> run-hook-wrapped).
        procedure :: functions => region_hook_functions
        !> Whether FN is one of the functions run in the current buffer.
        procedure :: member => region_hook_member
    end type region_hook_type

    !> A hook of functions taking a string (`string_procedure'), like
    !> `comint_output_filter_functions'.
    type, extends(hook_base_type), public :: string_hook_type
    contains
        !> Add a function to the hook (subr.el's add-hook).
        procedure :: append => string_hook_append
        !> Remove a function from the hook (subr.el's remove-hook).
        procedure :: remove => string_hook_remove
        !> Run the hook's functions with a string (run-hook-with-args).
        procedure :: execute => string_hook_execute
        !> Whether FN is one of the functions run in the current buffer.
        procedure :: member => string_hook_member
    end type string_hook_type

    !> A hook of functions each transforming a string
    !> (`string_filter_procedure'), like `comint_preoutput_filter_functions'.
    type, extends(hook_base_type), public :: string_filter_hook_type
    contains
        !> Add a function to the hook (subr.el's add-hook).
        procedure :: append => string_filter_hook_append
        !> Remove a function from the hook (subr.el's remove-hook).
        procedure :: remove => string_filter_hook_remove
        !> Pass a string through the hook's functions in turn, each given
        !> what the previous one returned.
        procedure :: execute => string_filter_hook_execute
        !> Whether FN is one of the functions run in the current buffer.
        procedure :: member => string_filter_hook_member
    end type string_filter_hook_type

    !> A hook of functions giving the completion data at point
    !> (`capf_procedure'), like `completion_at_point_functions'.
    type, extends(hook_base_type), public :: capf_hook_type
    contains
        !> Add a function to the hook (subr.el's add-hook).
        procedure :: append => capf_hook_append
        !> Remove a function from the hook (subr.el's remove-hook).
        procedure :: remove => capf_hook_remove
        !> The functions run in the current buffer, in order (for
        !> run-hook-wrapped).
        procedure :: functions => capf_hook_functions
        !> Whether FN is one of the functions run in the current buffer.
        procedure :: member => capf_hook_member
    end type capf_hook_type

    !> A hook whose functions are run until one of them succeeds
    !> (`until_success_procedure'), like `write_file_functions'.
    type, extends(hook_base_type), public :: until_success_hook_type
    contains
        !> Add a function to the hook (subr.el's add-hook).
        procedure :: append => until_success_hook_append
        !> Remove a function from the hook (subr.el's remove-hook).
        procedure :: remove => until_success_hook_remove
        !> Run the hook's functions until one succeeds
        !> (run-hook-with-args-until-success).
        procedure :: execute => until_success_hook_execute
        !> Whether FN is one of the functions run in the current buffer.
        procedure :: member => until_success_hook_member
    end type until_success_hook_type

    !> Normal hook run before changing the major mode of a buffer.
    !> The function `clear_all_local_variables' runs this before doing
    !> anything else.  Port of buffer.c change-major-mode-hook.
    type(hook_type), public, target :: change_major_mode_hook = hook_type(name="change_major_mode_hook")

    !> Hook run when the buffer list changes.
    !> Functions (implicitly) running this hook are `get_or_make_buffer',
    !> `make_indirect_buffer', `rename_buffer', `close_buffer', `bury_buffer'
    !> and `select_window'.  This hook is not run for internal or temporary
    !> buffers created by `get_or_make_buffer' or `generate_new_buffer' with
    !> argument INHIBIT_BUFFER_HOOKS true.
    !>
    !> Functions run by this hook should avoid calling `select_window' with a
    !> false NORECORD argument since it may lead to infinite recursion.
    !> Port of buffer.c buffer-list-update-hook.
    type(hook_type), public, target :: buffer_list_update_hook = hook_type(name="buffer_list_update_hook")

    !> A function that text properties and overlay properties name with a
    !> name: the hooks of the `modification_hooks',
    !> `insert_in_front_hooks', `insert_behind_hooks', `point_entered' and
    !> `point_left' properties.  Upstream such a property holds functions
    !> (names with a function cell); here the name must be that
    !> of a defined property function, which gives the procedure:
    !> `positions' for the functions taking two positions (the text
    !> property hooks, called with the bounds of the change or the old
    !> and new point), `overlay_change' for those of overlays.  Define one
    !> as a `target' variable:
    !>
    !>     type(property_function_type), target :: my_freeze
    !>     my_freeze = property_function_type("my_freeze", overlay_change=my_freeze_adapter)
    !>     call my_freeze%define()
    type, public :: property_function_type
        !> The name.
        character(len=:), allocatable :: name
        !> The procedure taking two positions, or null.
        procedure(change_procedure), pointer, nopass :: positions => null()
        !> The procedure taking an overlay, or null.
        procedure(overlay_change_procedure), pointer, nopass :: overlay_change => null()
    contains
        !> Make the function known by its name.
        procedure :: define => define_property_function
    end type property_function_type

    !> The pointer to a defined property function.
    type :: property_function_ref_type
        type(property_function_type), pointer :: p => null()
    end type property_function_ref_type

    !> Property functions defined with `define'.
    type(property_function_ref_type), allocatable :: property_functions(:)
    integer :: property_function_count = 0

contains

    ! ------------------------------------------------------------------
    ! Entries
    ! ------------------------------------------------------------------

    !> Whether A and B are the same function (neither being `t').
    !> Not a port.
    logical function same_function(a, b) result(same)
        type(hook_entry_type), intent(in) :: a, b

        same = .false.
        if (a%is_t .or. b%is_t) return

        if (associated(a%fn)) then
            if (associated(b%fn)) same = associated(a%fn, b%fn)
        else if (associated(a%change_fn)) then
            if (associated(b%change_fn)) same = associated(a%change_fn, b%change_fn)
        else if (associated(a%after_change_fn)) then
            if (associated(b%after_change_fn)) same = associated(a%after_change_fn, b%after_change_fn)
        else if (associated(a%position_fn)) then
            if (associated(b%position_fn)) same = associated(a%position_fn, b%position_fn)
        else if (associated(a%region_fn)) then
            if (associated(b%region_fn)) same = associated(a%region_fn, b%region_fn)
        else if (associated(a%string_fn)) then
            if (associated(b%string_fn)) same = associated(a%string_fn, b%string_fn)
        else if (associated(a%string_filter_fn)) then
            if (associated(b%string_filter_fn)) same = associated(a%string_filter_fn, b%string_filter_fn)
        else if (associated(a%capf_fn)) then
            if (associated(b%capf_fn)) same = associated(a%capf_fn, b%capf_fn)
        else if (associated(a%until_success_fn)) then
            if (associated(b%until_success_fn)) same = associated(a%until_success_fn, b%until_success_fn)
        end if
    end function same_function

    !> The index of the function of E in ENTRIES, or 0.  Not a port.
    integer function find_entry(entries, e) result(k)
        type(hook_entry_type), intent(in) :: entries(:)
        type(hook_entry_type), intent(in) :: e
        integer :: i

        k = 0

        do i = 1, size(entries)
            if (same_function(entries(i), e)) then
                k = i
                return
            end if
        end do
    end function find_entry

    !> Whether ENTRIES has the `t' of a buffer's own value.  Not a port.
    logical function has_t(entries)
        type(hook_entry_type), intent(in) :: entries(:)
        integer :: i

        has_t = .false.

        do i = 1, size(entries)
            if (entries(i)%is_t) then
                has_t = .true.
                return
            end if
        end do
    end function has_t

    !> Add the function of E to ENTRIES at DEPTH (after equal depths when
    !> DEPTH > 0, before them otherwise), keeping them sorted by depth.
    !> Not a port.
    subroutine insert_entry(entries, e, depth)
        type(hook_entry_type), allocatable, intent(inout) :: entries(:)
        type(hook_entry_type), intent(in) :: e
        integer, intent(in) :: depth
        type(hook_entry_type), allocatable :: grown(:)
        integer :: n, at, i

        n = size(entries)

        if (depth > 0) then
            at = n + 1

            do i = n, 1, -1
                if (entries(i)%depth <= depth) exit
                at = i
            end do
        else
            at = 1

            do i = 1, n
                if (entries(i)%depth >= depth) exit
                at = i + 1
            end do
        end if

        allocate (grown(n + 1))
        grown(1:at - 1) = entries(1:at - 1)
        grown(at) = e
        grown(at)%depth = depth
        grown(at + 1:n + 1) = entries(at:n)
        call move_alloc(grown, entries)
    end subroutine insert_entry

    !> ENTRIES without element K.  Not a port.
    subroutine delete_entry(entries, k)
        type(hook_entry_type), allocatable, intent(inout) :: entries(:)
        integer, intent(in) :: k
        type(hook_entry_type), allocatable :: kept(:)
        integer :: n

        n = size(entries)
        allocate (kept(n - 1))
        kept(1:k - 1) = entries(1:k - 1)
        kept(k:n - 1) = entries(k + 1:n)
        call move_alloc(kept, entries)
    end subroutine delete_entry

    ! ------------------------------------------------------------------
    ! Buffers' own values and depths
    ! ------------------------------------------------------------------

    !> The index of the current buffer's own functions in HOOK, or 0.
    !> Not a port.
    integer function local_index(hook) result(k)
        class(hook_base_type), intent(in) :: hook
        integer :: i

        k = 0
        if (current == 0 .or. .not. allocated(hook%locals)) return

        do i = 1, size(hook%locals)
            if (hook%locals(i)%serial == buffers(current)%serial) then
                k = i
                return
            end if
        end do
    end function local_index

    !> The index of the current buffer's own depths in HOOK, or 0.
    !> Not a port.
    integer function local_depths_index(hook) result(k)
        class(hook_base_type), intent(in) :: hook
        integer :: i

        k = 0
        if (current == 0 .or. .not. allocated(hook%local_depths)) return

        do i = 1, size(hook%local_depths)
            if (hook%local_depths(i)%serial == buffers(current)%serial) then
                k = i
                return
            end if
        end do
    end function local_depths_index

    !> The remembered depth of the function of E in DEPTHS, or 0.
    !> Not a port.
    integer function depth_of(depths, e) result(d)
        type(hook_entry_type), allocatable, intent(in) :: depths(:)
        type(hook_entry_type), intent(in) :: e
        integer :: k

        d = 0
        if (.not. allocated(depths)) return
        k = find_entry(depths, e)
        if (k > 0) d = depths(k)%depth
    end function depth_of

    !> Remember that the function of E was added with DEPTH.  Not a port.
    subroutine remember_depth(depths, e, depth)
        type(hook_entry_type), allocatable, intent(inout) :: depths(:)
        type(hook_entry_type), intent(in) :: e
        integer, intent(in) :: depth
        type(hook_entry_type), allocatable :: grown(:)
        integer :: k, n

        if (.not. allocated(depths)) allocate (depths(0))
        k = find_entry(depths, e)

        if (k > 0) then
            depths(k)%depth = depth
            return
        end if

        n = size(depths)
        allocate (grown(n + 1))
        grown(1:n) = depths
        grown(n + 1) = e
        grown(n + 1)%depth = depth
        call move_alloc(grown, depths)
    end subroutine remember_depth

    !> Forget the depth of the function of E in DEPTHS.  Not a port.
    subroutine forget_depth(depths, e)
        type(hook_entry_type), allocatable, intent(inout) :: depths(:)
        type(hook_entry_type), intent(in) :: e
        integer :: k

        if (.not. allocated(depths)) return
        k = find_entry(depths, e)
        if (k > 0) call delete_entry(depths, k)
    end subroutine forget_depth

    !> The depth the function of E goes in at: DEPTH if not zero (which is
    !> remembered), else the depth it was added with before.  LOCAL uses
    !> the current buffer's own depths, made from the default ones when
    !> first needed.  Not a port.
    integer function entry_depth(hook, e, depth, local) result(d)
        class(hook_base_type), intent(inout) :: hook
        type(hook_entry_type), intent(in) :: e
        integer, intent(in) :: depth
        logical, intent(in) :: local
        type(local_depths_type), allocatable :: grown(:)
        integer :: k, n

        d = depth
        k = 0
        if (local) k = local_depths_index(hook)

        if (depth == 0) then
            if (k > 0) then
                d = depth_of(hook%local_depths(k)%depths, e)
            else
                d = depth_of(hook%depths, e)
            end if

            return
        end if

        if (.not. local) then
            call remember_depth(hook%depths, e, depth)
            return
        end if

        if (k == 0) then
            ! (make-local-variable depth-sym): starts as the default.
            if (.not. allocated(hook%local_depths)) allocate (hook%local_depths(0))
            n = size(hook%local_depths)
            allocate (grown(n + 1))
            grown(1:n) = hook%local_depths
            grown(n + 1)%serial = buffers(current)%serial

            if (allocated(hook%depths)) then
                grown(n + 1)%depths = hook%depths
            else
                allocate (grown(n + 1)%depths(0))
            end if

            call move_alloc(grown, hook%local_depths)
            k = n + 1
        end if

        call remember_depth(hook%local_depths(k)%depths, e, depth)
    end function entry_depth

    !> Remember HOOK among the hooks with buffers' own functions.
    !> Not a port.
    subroutine note_local_hook(hook)
        class(hook_base_type), intent(inout), target :: hook
        type(hook_ref_type), allocatable :: grown(:)
        integer :: i, n

        if (.not. allocated(local_hooks)) allocate (local_hooks(0))
        n = size(local_hooks)

        do i = 1, n
            if (associated(local_hooks(i)%p, hook)) return
        end do

        allocate (grown(n + 1))
        grown(1:n) = local_hooks
        grown(n + 1)%p => hook
        call move_alloc(grown, local_hooks)
    end subroutine note_local_hook

    !> Forget the current buffer's own hook functions, except those of
    !> permanent-local hooks unless CLEAR_PERMANENT.  Port of
    !> buffer.c:reset_buffer_local_variables, in part (the hooks).
    subroutine clear_local_hooks(clear_permanent)
        logical, intent(in) :: clear_permanent
        integer :: i

        if (.not. allocated(local_hooks)) return

        do i = 1, size(local_hooks)
            ! The depth table is not permanent.
            call clear_local_depths(local_hooks(i)%p)
            if (local_hooks(i)%p%permanent_local .and. .not. clear_permanent) cycle
            call local_hooks(i)%p%clear_local()
        end do
    end subroutine clear_local_hooks

    !> Forget the current buffer's own depths of HOOK.  Not a port.
    subroutine clear_local_depths(hook)
        class(hook_base_type), intent(inout) :: hook
        type(local_depths_type), allocatable :: kept(:)
        integer :: k, n

        k = local_depths_index(hook)
        if (k == 0) return
        n = size(hook%local_depths)
        allocate (kept(n - 1))
        kept(1:k - 1) = hook%local_depths(1:k - 1)
        kept(k:n - 1) = hook%local_depths(k + 1:n)
        call move_alloc(kept, hook%local_depths)
    end subroutine clear_local_depths

    ! ------------------------------------------------------------------
    ! All kinds of hook
    ! ------------------------------------------------------------------

    !> Add the function of E to the hook: see `hook_append'.
    !> Port of subr.el:add-hook.
    subroutine hook_append_entry(self, e, depth, local)
        class(hook_base_type), intent(inout), target :: self
        type(hook_entry_type), intent(in) :: e
        integer, intent(in), optional :: depth
        logical, intent(in), optional :: local
        type(local_hook_type), allocatable :: grown(:)
        integer :: d, k, n
        logical :: loc

        d = 0
        if (present(depth)) d = depth
        loc = .false.
        if (present(local)) loc = local
        if (.not. allocated(self%entries)) allocate (self%entries(0))
        if (.not. allocated(self%locals)) allocate (self%locals(0))
        k = local_index(self)

        if (loc) then
            if (current == 0) return

            if (k == 0) then
                ! (set (make-local-variable hook) (list t))
                n = size(self%locals)
                allocate (grown(n + 1))
                grown(1:n) = self%locals
                grown(n + 1)%serial = buffers(current)%serial
                allocate (grown(n + 1)%entries(1))
                grown(n + 1)%entries(1)%is_t = .true.
                call move_alloc(grown, self%locals)
                k = n + 1
                call note_local_hook(self)
            end if
        else if (k > 0) then
            ! A buffer's own value without `t' (made empty by `clear'):
            ! change that value.
            if (.not. has_t(self%locals(k)%entries)) loc = .true.
        end if

        if (loc) then
            if (find_entry(self%locals(k)%entries, e) == 0) then
                call insert_entry(self%locals(k)%entries, e, entry_depth(self, e, d, .true.))
            end if
        else if (find_entry(self%entries, e) == 0) then
            call insert_entry(self%entries, e, entry_depth(self, e, d, .false.))
        end if
    end subroutine hook_append_entry

    !> Remove the function of E from the hook: see `hook_remove'.
    !> Port of subr.el:remove-hook.
    subroutine hook_remove_entry(self, e, local)
        class(hook_base_type), intent(inout) :: self
        type(hook_entry_type), intent(in) :: e
        logical, intent(in), optional :: local
        integer :: k, i, kd
        logical :: loc

        loc = .false.
        if (present(local)) loc = local
        k = local_index(self)
        ! Do nothing if LOCAL is true but this hook has no local binding.
        if (loc .and. k == 0) return

        ! A buffer's own value without `t': change that value.
        if (k > 0) then
            if (.not. has_t(self%locals(k)%entries)) loc = .true.
        end if

        if (loc) then
            i = find_entry(self%locals(k)%entries, e)
            if (i == 0) return
            call delete_entry(self%locals(k)%entries, i)
            ! Remove auxiliary depth info too: the buffer's own if it has
            ! any, else the default.
            kd = local_depths_index(self)

            if (kd > 0) then
                call forget_depth(self%local_depths(kd)%depths, e)
            else
                call forget_depth(self%depths, e)
            end if

            ! A local value of just (t) is killed.
            if (size(self%locals(k)%entries) == 1) then
                if (self%locals(k)%entries(1)%is_t) call self%clear_local()
            end if
        else
            if (.not. allocated(self%entries)) return
            i = find_entry(self%entries, e)
            if (i == 0) return
            call delete_entry(self%entries, i)
            call forget_depth(self%depths, e)
        end if
    end subroutine hook_remove_entry

    !> Whether the function of E is run in the current buffer.  Not a port.
    logical function hook_member_entry(self, e) result(found)
        class(hook_base_type), intent(in) :: self
        type(hook_entry_type), intent(in) :: e
        integer :: k

        found = .false.
        k = local_index(self)
        if (k > 0) found = find_entry(self%locals(k)%entries, e) > 0
        if (.not. found .and. allocated(self%entries)) found = find_entry(self%entries, e) > 0
    end function hook_member_entry

    !> Execute each function of the hook in the current buffer, in order,
    !> stopping at the first error.  In a buffer with its own functions,
    !> those run, with the default functions in the place of `t'.  BEGINNING,
    !> END and OLD_LEN are the arguments for the functions of the kinds
    !> that take them.  Port of eval.c:run_hook_with_args.
    recursive subroutine hook_execute_entries(self, beginning, end, old_len, status, safe)
        class(hook_base_type), intent(in) :: self
        integer(int64), intent(in) :: beginning, end, old_len
        type(status_type), intent(out) :: status
        !> Ignore errors: an error in one function doesn't stop the others
        !> (redisplay's safe calls).
        logical, intent(in), optional :: safe
        type(hook_entry_type), allocatable :: entries(:), global(:)
        integer :: i, j, k
        logical :: keep_going

        status = status_type()
        keep_going = .false.
        if (present(safe)) keep_going = safe
        k = local_index(self)

        ! Copies, in case a function changes the hook.
        if (k > 0) then
            entries = self%locals(k)%entries
        else if (allocated(self%entries)) then
            entries = self%entries
        else
            return
        end if

        do i = 1, size(entries)
            if (entries(i)%is_t) then
                ! t indicates this hook has a local binding; it means to
                ! run the global binding too.
                if (.not. allocated(self%entries)) cycle
                global = self%entries

                do j = 1, size(global)
                    ! In a global value, t should not occur.  If it does,
                    ! we must ignore it to avoid an endless loop.
                    if (global(j)%is_t) cycle
                    call execute_entry(global(j), beginning, end, old_len, status)
                    if (keep_going) status = status_type()
                    if (.not. status%ok()) return
                end do
            else
                call execute_entry(entries(i), beginning, end, old_len, status)
                if (keep_going) status = status_type()
                if (.not. status%ok()) return
            end if
        end do
    end subroutine hook_execute_entries

    !> Execute the function of E with the arguments its kind takes.  A
    !> normal hook's functions are commands: they run without a prefix
    !> argument.  Not a port.
    recursive subroutine execute_entry(e, beginning, end, old_len, status)
        type(hook_entry_type), intent(in) :: e
        integer(int64), intent(in) :: beginning, end, old_len
        type(status_type), intent(out) :: status
        type(prefix_arg_type) :: saved, saved_outer

        status = status_type()

        if (associated(e%fn)) then
            saved = current_prefix_arg
            saved_outer = hook_current_prefix_arg
            hook_current_prefix_arg = saved
            current_prefix_arg = prefix_arg_type()
            call e%fn(status)
            current_prefix_arg = saved
            hook_current_prefix_arg = saved_outer
        else if (associated(e%change_fn)) then
            call e%change_fn(beginning, end, status)
        else if (associated(e%after_change_fn)) then
            call e%after_change_fn(beginning, end, old_len, status)
        else if (associated(e%position_fn)) then
            call e%position_fn(beginning, status)
        end if
    end subroutine execute_entry

    ! Not a port.
    subroutine hook_clear_local(self)
        class(hook_base_type), intent(inout) :: self
        type(local_hook_type), allocatable :: kept(:)
        integer :: k, n

        k = local_index(self)
        if (k == 0) return
        n = size(self%locals)
        allocate (kept(n - 1))
        kept(1:k - 1) = self%locals(1:k - 1)
        kept(k:n - 1) = self%locals(k + 1:n)
        call move_alloc(kept, self%locals)
    end subroutine hook_clear_local

    ! Not a port.
    logical function is_hook_local(self)
        class(hook_base_type), intent(in) :: self

        is_hook_local = local_index(self) > 0
    end function is_hook_local

    !> Whether the hook's value in the current buffer is empty: no functions,
    !> and for a buffer's own value no `t' (run the default functions) either.
    !> Not a port.
    logical function hook_empty(self)
        class(hook_base_type), intent(in) :: self
        integer :: k

        k = local_index(self)

        if (k > 0) then
            hook_empty = size(self%locals(k)%entries) == 0
        else if (allocated(self%entries)) then
            hook_empty = size(self%entries) == 0
        else
            hook_empty = .true.
        end if
    end function hook_empty

    !> Make the hook's value in the current buffer empty, as setting the
    !> variable to an empty value does upstream: the buffer's own value if it has
    !> one (which then runs nothing, not even the default functions),
    !> else the default value.  Depths are kept.  Not a port.
    subroutine hook_clear(self)
        class(hook_base_type), intent(inout) :: self
        integer :: k

        k = local_index(self)

        if (k > 0) then
            deallocate (self%locals(k)%entries)
            allocate (self%locals(k)%entries(0))
        else
            if (allocated(self%entries)) deallocate (self%entries)
            allocate (self%entries(0))
        end if
    end subroutine hook_clear

    ! ------------------------------------------------------------------
    ! Normal hooks
    ! ------------------------------------------------------------------

    !> Add to the value of HOOK the function FUNCTION.
    !> FUNCTION is not added if already present.
    !>
    !> The place where the function is added depends on the DEPTH
    !> parameter.  DEPTH defaults to 0.  By convention, it should be
    !> a number between -100 and 100 where 100 means that the function
    !> should be at the very end of the list, whereas -100 means that
    !> the function should always come first.
    !> Since nothing is "always" true, don't use 100 nor -100.
    !> When two functions have the same depth, the new one gets added after the
    !> old one if depth is strictly positive and before otherwise.
    !>
    !> The optional argument LOCAL, if true, says to modify
    !> the hook's buffer-local value rather than its global value.
    !> This makes the hook buffer-local, and it makes t a member of the
    !> buffer-local value.  That acts as a flag to run the hook
    !> functions of the global value as well as in the local value.
    !> Port of subr.el:add-hook (`t' for DEPTH is 90).
    subroutine hook_append(self, fn, depth, local)
        class(hook_type), intent(inout), target :: self
        procedure(command_procedure) :: fn
        integer, intent(in), optional :: depth
        logical, intent(in), optional :: local
        type(hook_entry_type) :: e

        e%fn => fn
        call self%append_entry(e, depth, local)
    end subroutine hook_append

    !> Remove FUNCTION from HOOK's functions.
    !> Do nothing if HOOK does not currently contain FUNCTION.
    !>
    !> The optional argument LOCAL, if true, says to modify
    !> the hook's buffer-local value rather than its default value.
    !> Port of subr.el:remove-hook.
    subroutine hook_remove(self, fn, local)
        class(hook_type), intent(inout) :: self
        procedure(command_procedure) :: fn
        logical, intent(in), optional :: local
        type(hook_entry_type) :: e

        e%fn => fn
        call self%remove_entry(e, local)
    end subroutine hook_remove

    !> Execute each function of the hook, in order, stopping at the first
    !> error.  In a buffer with its own functions, those run, with the
    !> default functions in the place of `t'.  Port of eval.c:run_hook
    !> (via run_hook_with_args).
    recursive subroutine hook_execute(self, status)
        class(hook_type), intent(in) :: self
        type(status_type), intent(out) :: status

        call self%execute_entries(0_int64, 0_int64, 0_int64, status)
    end subroutine hook_execute

    !> Execute each function of the hook as `hook_execute' does, but an error in
    !> one doesn't stop the others: REPORT is told of it, and the function
    !> is removed from the hook, from the current buffer's own functions
    !> if it is one of them, else from the default ones.  Port of
    !> keyboard.c:safe_run_hooks_error (the removal; keyboard_fm's
    !> safe_execute_hooks reports).
    recursive subroutine hook_execute_safe(self, report)
        class(hook_type), intent(inout) :: self
        procedure(hook_error_procedure) :: report
        type(hook_entry_type), allocatable :: entries(:), global(:)
        type(status_type) :: status
        integer :: i, j, k

        k = local_index(self)

        ! Copies, in case a function changes the hook.
        if (k > 0) then
            entries = self%locals(k)%entries
        else if (allocated(self%entries)) then
            entries = self%entries
        else
            return
        end if

        do i = 1, size(entries)
            if (entries(i)%is_t) then
                ! t indicates this hook has a local binding; it means to
                ! run the global binding too.
                if (.not. allocated(self%entries)) cycle
                global = self%entries

                do j = 1, size(global)
                    ! In a global value, t should not occur.
                    if (global(j)%is_t) cycle
                    call execute_entry(global(j), 0_int64, 0_int64, 0_int64, status)
                    if (.not. status%ok()) call fail(global(j))
                end do
            else
                call execute_entry(entries(i), 0_int64, 0_int64, 0_int64, status)
                if (.not. status%ok()) call fail(entries(i))
            end if
        end do

    contains

        !> Report the error of E's function and take it out of the hook:
        !> out of the buffer's own functions if it is there, else out of
        !> the default ones.
        recursive subroutine fail(e)
            type(hook_entry_type), intent(in) :: e
            integer :: kk, n

            call report(trim(self%name), e%fn, status)
            kk = local_index(self)

            if (kk > 0) then
                n = find_entry(self%locals(kk)%entries, e)

                if (n > 0) then
                    call delete_entry(self%locals(kk)%entries, n)
                    return
                end if
            end if

            if (.not. allocated(self%entries)) return
            n = find_entry(self%entries, e)
            if (n > 0) call delete_entry(self%entries, n)
        end subroutine fail
    end subroutine hook_execute_safe

    ! Not a port.
    logical function hook_member(self, fn) result(found)
        class(hook_type), intent(in) :: self
        procedure(command_procedure) :: fn
        type(hook_entry_type) :: e

        e%fn => fn
        found = self%member_entry(e)
    end function hook_member

    ! ------------------------------------------------------------------
    ! Hooks of change functions
    ! ------------------------------------------------------------------

    !> Add FN to the hook, as `hook_append' does.  Port of subr.el:add-hook.
    subroutine change_hook_append(self, fn, depth, local)
        class(change_hook_type), intent(inout), target :: self
        procedure(change_procedure) :: fn
        integer, intent(in), optional :: depth
        logical, intent(in), optional :: local
        type(hook_entry_type) :: e

        e%change_fn => fn
        call self%append_entry(e, depth, local)
    end subroutine change_hook_append

    !> Remove FN from the hook, as `hook_remove' does.
    !> Port of subr.el:remove-hook.
    subroutine change_hook_remove(self, fn, local)
        class(change_hook_type), intent(inout) :: self
        procedure(change_procedure) :: fn
        logical, intent(in), optional :: local
        type(hook_entry_type) :: e

        e%change_fn => fn
        call self%remove_entry(e, local)
    end subroutine change_hook_remove

    !> Execute each function of the hook with BEGINNING and END, in order, stopping
    !> at the first error; buffers' own functions as `hook_execute'.
    !> Port of eval.c:Frun_hook_with_args.
    recursive subroutine change_hook_execute(self, beginning, end, status)
        class(change_hook_type), intent(in) :: self
        integer(int64), intent(in) :: beginning, end
        type(status_type), intent(out) :: status

        call self%execute_entries(beginning, end, 0_int64, status)
    end subroutine change_hook_execute

    ! Not a port.
    logical function change_hook_member(self, fn) result(found)
        class(change_hook_type), intent(in) :: self
        procedure(change_procedure) :: fn
        type(hook_entry_type) :: e

        e%change_fn => fn
        found = self%member_entry(e)
    end function change_hook_member

    !> Add FN to the hook, as `hook_append' does.  Port of subr.el:add-hook.
    subroutine after_change_hook_append(self, fn, depth, local)
        class(after_change_hook_type), intent(inout), target :: self
        procedure(after_change_procedure) :: fn
        integer, intent(in), optional :: depth
        logical, intent(in), optional :: local
        type(hook_entry_type) :: e

        e%after_change_fn => fn
        call self%append_entry(e, depth, local)
    end subroutine after_change_hook_append

    !> Remove FN from the hook, as `hook_remove' does.
    !> Port of subr.el:remove-hook.
    subroutine after_change_hook_remove(self, fn, local)
        class(after_change_hook_type), intent(inout) :: self
        procedure(after_change_procedure) :: fn
        logical, intent(in), optional :: local
        type(hook_entry_type) :: e

        e%after_change_fn => fn
        call self%remove_entry(e, local)
    end subroutine after_change_hook_remove

    !> Execute each function of the hook with BEGINNING, END and OLD_LEN, in order,
    !> stopping at the first error; buffers' own functions as
    !> `hook_execute'.  Port of eval.c:Frun_hook_with_args.
    recursive subroutine after_change_hook_execute(self, beginning, end, old_len, status)
        class(after_change_hook_type), intent(in) :: self
        integer(int64), intent(in) :: beginning, end, old_len
        type(status_type), intent(out) :: status

        call self%execute_entries(beginning, end, old_len, status)
    end subroutine after_change_hook_execute

    ! Not a port.
    logical function after_change_hook_member(self, fn) result(found)
        class(after_change_hook_type), intent(in) :: self
        procedure(after_change_procedure) :: fn
        type(hook_entry_type) :: e

        e%after_change_fn => fn
        found = self%member_entry(e)
    end function after_change_hook_member

    ! ------------------------------------------------------------------
    ! Position and region hooks
    ! ------------------------------------------------------------------

    !> Add FN to the hook, as `hook_append' does.  Port of subr.el:add-hook.
    subroutine position_hook_append(self, fn, depth, local)
        class(position_hook_type), intent(inout), target :: self
        procedure(position_procedure) :: fn
        integer, intent(in), optional :: depth
        logical, intent(in), optional :: local
        type(hook_entry_type) :: e

        e%position_fn => fn
        call self%append_entry(e, depth, local)
    end subroutine position_hook_append

    !> Remove FN from the hook, as `hook_remove' does.
    !> Port of subr.el:remove-hook.
    subroutine position_hook_remove(self, fn, local)
        class(position_hook_type), intent(inout) :: self
        procedure(position_procedure) :: fn
        logical, intent(in), optional :: local
        type(hook_entry_type) :: e

        e%position_fn => fn
        call self%remove_entry(e, local)
    end subroutine position_hook_remove

    !> Execute each function of the hook with POS, in order, stopping at the
    !> first error; buffers' own functions as `hook_execute'.  Port of
    !> eval.c:Frun_hook_with_args.
    recursive subroutine position_hook_execute(self, pos, status, safe)
        class(position_hook_type), intent(in) :: self
        integer(int64), intent(in) :: pos
        type(status_type), intent(out) :: status
        !> Ignore errors, calling every function (redisplay's safe calls).
        logical, intent(in), optional :: safe

        call self%execute_entries(pos, 0_int64, 0_int64, status, safe)
    end subroutine position_hook_execute

    ! Not a port.
    logical function position_hook_member(self, fn) result(found)
        class(position_hook_type), intent(in) :: self
        procedure(position_procedure) :: fn
        type(hook_entry_type) :: e

        e%position_fn => fn
        found = self%member_entry(e)
    end function position_hook_member

    !> Add FN to the hook, as `hook_append' does.  Port of subr.el:add-hook.
    subroutine region_hook_append(self, fn, depth, local)
        class(region_hook_type), intent(inout), target :: self
        procedure(region_procedure) :: fn
        integer, intent(in), optional :: depth
        logical, intent(in), optional :: local
        type(hook_entry_type) :: e

        e%region_fn => fn
        call self%append_entry(e, depth, local)
    end subroutine region_hook_append

    !> Remove FN from the hook, as `hook_remove' does.
    !> Port of subr.el:remove-hook.
    subroutine region_hook_remove(self, fn, local)
        class(region_hook_type), intent(inout) :: self
        procedure(region_procedure) :: fn
        logical, intent(in), optional :: local
        type(hook_entry_type) :: e

        e%region_fn => fn
        call self%remove_entry(e, local)
    end subroutine region_hook_remove

    ! Not a port.
    logical function region_hook_member(self, fn) result(found)
        class(region_hook_type), intent(in) :: self
        procedure(region_procedure) :: fn
        type(hook_entry_type) :: e

        e%region_fn => fn
        found = self%member_entry(e)
    end function region_hook_member

    !> The functions of the hook run in the current buffer, in order: the
    !> buffer's own, with the global ones in place of `t'.  Not a port.
    function region_hook_functions(self) result(fns)
        class(region_hook_type), intent(in) :: self
        type(region_entry_type), allocatable :: fns(:)
        type(hook_entry_type), allocatable :: entries(:)
        integer :: i, j, k, n

        allocate (fns(0))
        k = local_index(self)

        if (k > 0) then
            entries = self%locals(k)%entries
        else if (allocated(self%entries)) then
            entries = self%entries
        else
            return
        end if

        n = 0

        do i = 1, size(entries)
            if (entries(i)%is_t) then
                if (.not. allocated(self%entries)) cycle

                do j = 1, size(self%entries)
                    if (self%entries(j)%is_t) cycle
                    call append(self%entries(j))
                end do
            else
                call append(entries(i))
            end if
        end do

    contains

        subroutine append(e)
            type(hook_entry_type), intent(in) :: e
            type(region_entry_type), allocatable :: grown(:)

            allocate (grown(size(fns) + 1))
            grown(1:size(fns)) = fns
            grown(size(fns) + 1)%fn => e%region_fn
            call move_alloc(grown, fns)
            n = n + 1
        end subroutine append
    end function region_hook_functions

    !> Add FN to the hook, as `hook_append' does.  Port of subr.el:add-hook.
    subroutine capf_hook_append(self, fn, depth, local)
        class(capf_hook_type), intent(inout), target :: self
        procedure(capf_procedure) :: fn
        integer, intent(in), optional :: depth
        logical, intent(in), optional :: local
        type(hook_entry_type) :: e

        e%capf_fn => fn
        call self%append_entry(e, depth, local)
    end subroutine capf_hook_append

    !> Remove FN from the hook, as `hook_remove' does.
    !> Port of subr.el:remove-hook.
    subroutine capf_hook_remove(self, fn, local)
        class(capf_hook_type), intent(inout) :: self
        procedure(capf_procedure) :: fn
        logical, intent(in), optional :: local
        type(hook_entry_type) :: e

        e%capf_fn => fn
        call self%remove_entry(e, local)
    end subroutine capf_hook_remove

    ! Not a port.
    logical function capf_hook_member(self, fn) result(found)
        class(capf_hook_type), intent(in) :: self
        procedure(capf_procedure) :: fn
        type(hook_entry_type) :: e

        e%capf_fn => fn
        found = self%member_entry(e)
    end function capf_hook_member

    !> The functions of the hook run in the current buffer, in order: the
    !> buffer's own, with the global ones in place of `t'.  Not a port.
    function capf_hook_functions(self) result(fns)
        class(capf_hook_type), intent(in) :: self
        type(capf_entry_type), allocatable :: fns(:)
        type(hook_entry_type), allocatable :: entries(:)
        integer :: i, j, k, n

        allocate (fns(0))
        k = local_index(self)

        if (k > 0) then
            entries = self%locals(k)%entries
        else if (allocated(self%entries)) then
            entries = self%entries
        else
            return
        end if

        n = 0

        do i = 1, size(entries)
            if (entries(i)%is_t) then
                if (.not. allocated(self%entries)) cycle

                do j = 1, size(self%entries)
                    if (self%entries(j)%is_t) cycle
                    call append(self%entries(j))
                end do
            else
                call append(entries(i))
            end if
        end do

    contains

        subroutine append(e)
            type(hook_entry_type), intent(in) :: e
            type(capf_entry_type), allocatable :: grown(:)

            allocate (grown(size(fns) + 1))
            grown(1:size(fns)) = fns
            grown(size(fns) + 1)%fn => e%capf_fn
            call move_alloc(grown, fns)
            n = n + 1
        end subroutine append
    end function capf_hook_functions

    !> Add FN to the hook, as `hook_append' does.  Port of subr.el:add-hook.
    subroutine until_success_hook_append(self, fn, depth, local)
        class(until_success_hook_type), intent(inout), target :: self
        procedure(until_success_procedure) :: fn
        integer, intent(in), optional :: depth
        logical, intent(in), optional :: local
        type(hook_entry_type) :: e

        e%until_success_fn => fn
        call self%append_entry(e, depth, local)
    end subroutine until_success_hook_append

    !> Remove FN from the hook, as `hook_remove' does.
    !> Port of subr.el:remove-hook.
    subroutine until_success_hook_remove(self, fn, local)
        class(until_success_hook_type), intent(inout) :: self
        procedure(until_success_procedure) :: fn
        logical, intent(in), optional :: local
        type(hook_entry_type) :: e

        e%until_success_fn => fn
        call self%remove_entry(e, local)
    end subroutine until_success_hook_remove

    ! Not a port.
    logical function until_success_hook_member(self, fn) result(found)
        class(until_success_hook_type), intent(in) :: self
        procedure(until_success_procedure) :: fn
        type(hook_entry_type) :: e

        e%until_success_fn => fn
        found = self%member_entry(e)
    end function until_success_hook_member

    !> Execute the hook's functions in the current buffer, in order, until one
    !> of them succeeds (DONE); an error stops the hook.  Port of
    !> eval.c:Frun_hook_with_args_until_success.
    recursive subroutine until_success_hook_execute(self, done, status)
        class(until_success_hook_type), intent(in) :: self
        logical, intent(out) :: done
        type(status_type), intent(out) :: status
        type(hook_entry_type), allocatable :: run(:)
        integer :: i

        status = status_type()
        done = .false.
        run = effective_entries(self)

        do i = 1, size(run)
            if (.not. associated(run(i)%until_success_fn)) cycle
            call run(i)%until_success_fn(done, status)
            if (done .or. .not. status%ok()) return
        end do
    end subroutine until_success_hook_execute

    !> The entries run in the current buffer, in order: the buffer's own,
    !> with the global ones in place of `t' (copies, in case a function
    !> changes the hook).  Not a port.
    function effective_entries(self) result(run)
        class(hook_base_type), intent(in) :: self
        type(hook_entry_type), allocatable :: run(:)
        type(hook_entry_type), allocatable :: entries(:)
        logical, allocatable :: keep(:)
        integer :: i, k

        allocate (run(0))
        k = local_index(self)

        if (k > 0) then
            entries = self%locals(k)%entries
        else if (allocated(self%entries)) then
            entries = self%entries
        else
            return
        end if

        do i = 1, size(entries)
            if (entries(i)%is_t) then
                ! In a global value, t should not occur.  If it does, we
                ! must ignore it to avoid an endless loop.
                if (.not. allocated(self%entries)) cycle
                allocate (keep(size(self%entries)))
                keep = .not. self%entries%is_t
                run = [run, pack(self%entries, keep)]
                deallocate (keep)
            else
                run = [run, entries(i)]
            end if
        end do
    end function effective_entries

    !> Add FN to the hook, as `hook_append' does.  Port of subr.el:add-hook.
    subroutine string_hook_append(self, fn, depth, local)
        class(string_hook_type), intent(inout), target :: self
        procedure(string_procedure) :: fn
        integer, intent(in), optional :: depth
        logical, intent(in), optional :: local
        type(hook_entry_type) :: e

        e%string_fn => fn
        call self%append_entry(e, depth, local)
    end subroutine string_hook_append

    !> Remove FN from the hook, as `hook_remove' does.
    !> Port of subr.el:remove-hook.
    subroutine string_hook_remove(self, fn, local)
        class(string_hook_type), intent(inout) :: self
        procedure(string_procedure) :: fn
        logical, intent(in), optional :: local
        type(hook_entry_type) :: e

        e%string_fn => fn
        call self%remove_entry(e, local)
    end subroutine string_hook_remove

    !> Execute each function of the hook with STRING, in order, stopping at
    !> the first error; buffers' own functions as `hook_execute'.  Port of
    !> eval.c:Frun_hook_with_args.
    recursive subroutine string_hook_execute(self, string, status)
        class(string_hook_type), intent(in) :: self
        character(len=*), intent(in) :: string
        type(status_type), intent(out) :: status
        type(hook_entry_type), allocatable :: entries(:)
        integer :: i

        status = status_type()
        entries = effective_entries(self)

        do i = 1, size(entries)
            if (.not. associated(entries(i)%string_fn)) cycle
            call entries(i)%string_fn(string, status)
            if (.not. status%ok()) return
        end do
    end subroutine string_hook_execute

    ! Not a port.
    logical function string_hook_member(self, fn) result(found)
        class(string_hook_type), intent(in) :: self
        procedure(string_procedure) :: fn
        type(hook_entry_type) :: e

        e%string_fn => fn
        found = self%member_entry(e)
    end function string_hook_member

    !> Add FN to the hook, as `hook_append' does.  Port of subr.el:add-hook.
    subroutine string_filter_hook_append(self, fn, depth, local)
        class(string_filter_hook_type), intent(inout), target :: self
        procedure(string_filter_procedure) :: fn
        integer, intent(in), optional :: depth
        logical, intent(in), optional :: local
        type(hook_entry_type) :: e

        e%string_filter_fn => fn
        call self%append_entry(e, depth, local)
    end subroutine string_filter_hook_append

    !> Remove FN from the hook, as `hook_remove' does.
    !> Port of subr.el:remove-hook.
    subroutine string_filter_hook_remove(self, fn, local)
        class(string_filter_hook_type), intent(inout) :: self
        procedure(string_filter_procedure) :: fn
        logical, intent(in), optional :: local
        type(hook_entry_type) :: e

        e%string_filter_fn => fn
        call self%remove_entry(e, local)
    end subroutine string_filter_hook_remove

    !> Give STRING to the first function of the hook, what it returns to
    !> the next, and so on; RESULT is what the last one returns (STRING if
    !> there are none).  Stops at the first error.  A `t' in a buffer's own
    !> value runs the default functions there.  Not a port.
    recursive subroutine string_filter_hook_execute(self, string, result, status)
        class(string_filter_hook_type), intent(in) :: self
        character(len=*), intent(in) :: string
        character(len=:), allocatable, intent(out) :: result
        type(status_type), intent(out) :: status
        type(hook_entry_type), allocatable :: entries(:)
        character(len=:), allocatable :: next
        integer :: i

        status = status_type()
        result = string
        entries = effective_entries(self)

        do i = 1, size(entries)
            if (.not. associated(entries(i)%string_filter_fn)) cycle
            call entries(i)%string_filter_fn(result, next, status)
            if (.not. status%ok()) return
            call move_alloc(next, result)
        end do
    end subroutine string_filter_hook_execute

    ! Not a port.
    logical function string_filter_hook_member(self, fn) result(found)
        class(string_filter_hook_type), intent(in) :: self
        procedure(string_filter_procedure) :: fn
        type(hook_entry_type) :: e

        e%string_filter_fn => fn
        found = self%member_entry(e)
    end function string_filter_hook_member

    ! ------------------------------------------------------------------
    ! Property functions
    ! ------------------------------------------------------------------

    !> Make property function SELF known by its name (replacing an earlier
    !> one of the same name).  Not a port.
    subroutine define_property_function(self)
        class(property_function_type), target, intent(inout) :: self
        type(property_function_ref_type), allocatable :: grown(:)
        integer :: i

        do i = 1, property_function_count
            if (property_functions(i)%p%name == self%name .and. &
                len(property_functions(i)%p%name) == len(self%name)) then
                property_functions(i)%p => self
                return
            end if
        end do

        if (.not. allocated(property_functions)) allocate (property_functions(16))

        if (property_function_count == size(property_functions)) then
            allocate (grown(2*property_function_count))
            grown(1:property_function_count) = property_functions
            call move_alloc(grown, property_functions)
        end if

        property_function_count = property_function_count + 1
        property_functions(property_function_count)%p => self
    end subroutine define_property_function

    !> The property function that FN (a name) names, or null; STATUS is
    !> `void_function' (or `invalid_function' for a non-symbol) when there
    !> is none.  Not a port.
    function property_function(fn, status) result(p)
        type(value_type), intent(in) :: fn
        type(status_type), intent(out) :: status
        type(property_function_type), pointer :: p
        character(len=:), allocatable :: name
        integer :: i

        p => null()

        if ((vtag(fn) /= V_NAME .and. vtag(fn) /= V_NULL) .or. .not. allocated(fn%a)) then
            call set_error(status, FM_GENERIC_ERROR, "Invalid function")
            return
        end if

        name = symbols(xsymbol(fn))%name

        do i = property_function_count, 1, -1
            if (property_functions(i)%p%name == name .and. len(property_functions(i)%p%name) == len(name)) then
                p => property_functions(i)%p
                return
            end if
        end do

        call set_error(status, FM_VOID_FUNCTION, name)
    end function property_function

    !> Execute the property function FN (a name) with the positions A and B.
    !> Port of eval.c:Ffuncall (for a property function of two positions).
    recursive subroutine execute_property_function(fn, a, b, status)
        type(value_type), intent(in) :: fn
        integer(int64), intent(in) :: a, b
        type(status_type), intent(out) :: status
        type(property_function_type), pointer :: p

        p => property_function(fn, status)
        if (.not. associated(p)) return

        if (.not. associated(p%positions)) then
            call set_error(status, FM_GENERIC_ERROR, "Wrong number of arguments: "//p%name//", 2")
            return
        end if

        call p%positions(a, b, status)
    end subroutine execute_property_function

    !> Execute the property function FN (a name) with OVERLAY, AFTER, BEGINNING,
    !> END and, if present, OLD_LEN.  Port of eval.c:Ffuncall (for the
    !> functions of overlay properties).
    recursive subroutine execute_overlay_change_function(fn, overlay, after, beginning, end, status, old_len)
        type(value_type), intent(in) :: fn
        type(handle_type), intent(in) :: overlay
        logical, intent(in) :: after
        integer(int64), intent(in) :: beginning, end
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: old_len
        type(property_function_type), pointer :: p

        p => property_function(fn, status)
        if (.not. associated(p)) return

        if (.not. associated(p%overlay_change)) then
            call set_error(status, FM_GENERIC_ERROR, "Wrong number of arguments: "//p%name)
            return
        end if

        call p%overlay_change(overlay, after, beginning, end, status, old_len)
    end subroutine execute_overlay_change_function
end module eval_fm
