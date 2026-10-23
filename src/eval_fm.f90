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

!> Hooks.  A hook is a `hook_type` object holding procedures of no
!> arguments (`command_proc`), run in order:
!>
!>     call major_modes%text_mode%hook%add(turn_on_auto_fill)
!>
!> Upstream a hook is a variable whose value is a list of functions; a
!> buffer-local value holds `t` where the default value's functions run.
!> Here a buffer's own functions are kept in the hook by buffer, with a
!> `t` entry in the same place.
module eval_fm
    use, intrinsic :: iso_fortran_env, only: int64
    use error_fm, only: status_type
    use lisp_fm, only: value_type, nil, command_proc
    use callint_fm, only: funcall, current_prefix_arg
    use buffer_fm, only: buffers, cur
    implicit none(type, external)
    private

    public :: hook_type, funcall_noargs, kill_local_hooks

    !> A pointer to a hook.
    type :: hook_ref_type
        type(hook_type), pointer :: p => null()
    end type hook_ref_type

    !> Hooks that have been given buffers' own functions.
    type(hook_ref_type), allocatable :: local_hooks(:)

    !> One function of a hook, or the `t` marker of a buffer's own value.
    type :: hook_entry_type
        procedure(command_proc), pointer, nopass :: fn => null()
        integer :: depth = 0
        logical :: is_t = .false.
    end type hook_entry_type

    !> The depth a function was last added with (subr.el's
    !> `hook--depth-alist'): it stays when the function is removed, and
    !> applies when it is added again without a depth.
    type :: depth_type
        procedure(command_proc), pointer, nopass :: fn => null()
        integer :: depth = 0
    end type depth_type

    !> One buffer's own depths (the depth alist made buffer-local).
    type :: local_depths_type
        integer(int64) :: serial = 0
        type(depth_type), allocatable :: depths(:)
    end type local_depths_type

    !> The functions of one buffer's own (buffer-local) value.
    type :: local_hook_type
        !> The buffer's serial number (buffer slots are reused).
        integer(int64) :: serial = 0
        type(hook_entry_type), allocatable :: entries(:)
    end type local_hook_type

    !> A hook: functions to run in order.
    type, public :: hook_type
        !> The hook's name, as the help commands show it.
        character(len=64) :: name = ""
        !> The default (global) functions.
        type(hook_entry_type), allocatable :: entries(:)
        !> Buffers' own functions.
        type(local_hook_type), allocatable :: locals(:)
        !> The depths functions were added with, and buffers' own.
        type(depth_type), allocatable :: depths(:)
        type(local_depths_type), allocatable :: local_depths(:)
        !> Keep buffers' own functions when the major mode changes (the
        !> `permanent-local' property).
        logical :: permanent_local = .false.
    contains
        !> Add a function to the hook (subr.el's add-hook).
        procedure :: add => hook_add
        !> Remove a function from the hook (subr.el's remove-hook).
        procedure :: remove => hook_remove
        !> Run the hook's functions (run-hooks).
        procedure :: run => hook_run
        !> Whether FN is one of the functions run in the current buffer.
        procedure :: member => hook_member
        !> Forget the current buffer's own functions.
        procedure :: kill_local => hook_kill_local
        !> Whether the current buffer has its own functions.
        procedure :: local_p => hook_local_p
    end type hook_type

    !> Normal hook run before changing the major mode of a buffer.
    !> The function `kill-all-local-variables' runs this before doing
    !> anything else.  Port of buffer.c change-major-mode-hook.
    type(hook_type), public, target :: change_major_mode_hook = hook_type(name="change-major-mode-hook")

contains

    !> Call function FN with no arguments (and no prefix argument).
    !> Port of eval.c:funcall_nil.
    recursive subroutine funcall_noargs(fn, status)
        type(value_type), intent(in) :: fn
        type(status_type), intent(out) :: status
        type(value_type) :: saved
        saved = current_prefix_arg
        current_prefix_arg = nil()
        call funcall(fn, status)
        current_prefix_arg = saved
    end subroutine funcall_noargs

    !> The index of the current buffer's own functions in HOOK, or 0.
    integer function local_index(hook) result(k)
        class(hook_type), intent(in) :: hook
        integer :: i
        k = 0
        if (cur == 0 .or. .not. allocated(hook%locals)) return
        do i = 1, size(hook%locals)
            if (hook%locals(i)%serial == buffers(cur)%serial) then
                k = i
                return
            end if
        end do
    end function local_index

    !> The index of FN in ENTRIES, or 0.
    integer function find_fn(entries, fn) result(k)
        type(hook_entry_type), intent(in) :: entries(:)
        procedure(command_proc) :: fn
        integer :: i
        k = 0
        do i = 1, size(entries)
            if (entries(i)%is_t) cycle
            if (associated(entries(i)%fn, fn)) then
                k = i
                return
            end if
        end do
    end function find_fn

    !> Add FN to ENTRIES at DEPTH (after equal depths when DEPTH > 0,
    !> before them otherwise), keeping them sorted by depth.
    subroutine insert_entry(entries, fn, depth)
        type(hook_entry_type), allocatable, intent(inout) :: entries(:)
        procedure(command_proc) :: fn
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
        grown(at)%fn => fn
        grown(at)%depth = depth
        grown(at + 1:n + 1) = entries(at:n)
        call move_alloc(grown, entries)
    end subroutine insert_entry

    !> The remembered depth of FN in DEPTHS, or 0.
    integer function depth_of(depths, fn) result(d)
        type(depth_type), allocatable, intent(in) :: depths(:)
        procedure(command_proc) :: fn
        integer :: i
        d = 0
        if (.not. allocated(depths)) return
        do i = 1, size(depths)
            if (associated(depths(i)%fn, fn)) then
                d = depths(i)%depth
                return
            end if
        end do
    end function depth_of

    !> Remember that FN was added with DEPTH.
    subroutine remember_depth(depths, fn, depth)
        type(depth_type), allocatable, intent(inout) :: depths(:)
        procedure(command_proc) :: fn
        integer, intent(in) :: depth
        type(depth_type), allocatable :: grown(:)
        integer :: i, n
        if (.not. allocated(depths)) allocate (depths(0))
        n = size(depths)
        do i = 1, n
            if (associated(depths(i)%fn, fn)) then
                depths(i)%depth = depth
                return
            end if
        end do
        allocate (grown(n + 1))
        grown(1:n) = depths
        grown(n + 1)%fn => fn
        grown(n + 1)%depth = depth
        call move_alloc(grown, depths)
    end subroutine remember_depth

    !> The index of the current buffer's own depths in HOOK, or 0.
    integer function local_depths_index(hook) result(k)
        class(hook_type), intent(in) :: hook
        integer :: i
        k = 0
        if (cur == 0 .or. .not. allocated(hook%local_depths)) return
        do i = 1, size(hook%local_depths)
            if (hook%local_depths(i)%serial == buffers(cur)%serial) then
                k = i
                return
            end if
        end do
    end function local_depths_index

    !> The depth FN goes in at: DEPTH if not zero (which is remembered),
    !> else the depth it was added with before.  LOCAL uses the current
    !> buffer's own depths, made from the default ones when first needed.
    integer function entry_depth(hook, fn, depth, local) result(d)
        class(hook_type), intent(inout) :: hook
        procedure(command_proc) :: fn
        integer, intent(in) :: depth
        logical, intent(in) :: local
        type(local_depths_type), allocatable :: grown(:)
        integer :: k, n
        d = depth
        k = 0
        if (local) k = local_depths_index(hook)
        if (depth == 0) then
            if (k > 0) then
                d = depth_of(hook%local_depths(k)%depths, fn)
            else
                d = depth_of(hook%depths, fn)
            end if
            return
        end if
        if (.not. local) then
            call remember_depth(hook%depths, fn, depth)
            return
        end if
        if (k == 0) then
            ! (make-local-variable depth-sym): starts as the default.
            if (.not. allocated(hook%local_depths)) allocate (hook%local_depths(0))
            n = size(hook%local_depths)
            allocate (grown(n + 1))
            grown(1:n) = hook%local_depths
            grown(n + 1)%serial = buffers(cur)%serial
            if (allocated(hook%depths)) then
                grown(n + 1)%depths = hook%depths
            else
                allocate (grown(n + 1)%depths(0))
            end if
            call move_alloc(grown, hook%local_depths)
            k = n + 1
        end if
        call remember_depth(hook%local_depths(k)%depths, fn, depth)
    end function entry_depth

    !> ENTRIES without element K.
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
    !> Port of subr.el:add-hook (`t` for DEPTH is 90).
    subroutine hook_add(self, fn, depth, local)
        class(hook_type), intent(inout), target :: self
        procedure(command_proc) :: fn
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
        if (.not. loc) then
            if (find_fn(self%entries, fn) == 0) call insert_entry(self%entries, fn, entry_depth(self, fn, d, .false.))
            return
        end if
        if (cur == 0) return
        k = local_index(self)
        if (k == 0) then
            ! (set (make-local-variable hook) (list t))
            n = size(self%locals)
            allocate (grown(n + 1))
            grown(1:n) = self%locals
            grown(n + 1)%serial = buffers(cur)%serial
            allocate (grown(n + 1)%entries(1))
            grown(n + 1)%entries(1)%is_t = .true.
            call move_alloc(grown, self%locals)
            k = n + 1
            call note_local_hook(self)
        end if
        if (find_fn(self%locals(k)%entries, fn) == 0) then
            call insert_entry(self%locals(k)%entries, fn, entry_depth(self, fn, d, .true.))
        end if
    end subroutine hook_add

    !> Remove FUNCTION from HOOK's functions.
    !> Do nothing if HOOK does not currently contain FUNCTION.
    !>
    !> The optional argument LOCAL, if true, says to modify
    !> the hook's buffer-local value rather than its default value.
    !> Port of subr.el:remove-hook.
    subroutine hook_remove(self, fn, local)
        class(hook_type), intent(inout) :: self
        procedure(command_proc) :: fn
        logical, intent(in), optional :: local
        integer :: k, i
        logical :: loc
        loc = .false.
        if (present(local)) loc = local
        k = local_index(self)
        if (loc .and. k == 0) return
        if (loc) then
            i = find_fn(self%locals(k)%entries, fn)
            if (i == 0) return
            call delete_entry(self%locals(k)%entries, i)
            ! A local value of just (t) is killed.
            if (size(self%locals(k)%entries) == 1) call self%kill_local()
        else
            if (.not. allocated(self%entries)) return
            i = find_fn(self%entries, fn)
            if (i > 0) call delete_entry(self%entries, i)
        end if
    end subroutine hook_remove

    !> Run each function of the hook, in order, stopping at the first
    !> error.  In a buffer with its own functions, those run, with the
    !> default functions in the place of `t`.  Port of eval.c:run_hook
    !> (via run_hook_with_args).
    recursive subroutine hook_run(self, status)
        class(hook_type), intent(in) :: self
        type(status_type), intent(out) :: status
        type(hook_entry_type), allocatable :: entries(:)
        integer :: k
        status = status_type()
        k = local_index(self)
        if (k > 0) then
            ! Copies, in case a function changes the hook.
            entries = self%locals(k)%entries
        else if (allocated(self%entries)) then
            entries = self%entries
        else
            return
        end if
        call run_entries(self, entries, status)
    end subroutine hook_run

    recursive subroutine run_entries(self, entries, status)
        class(hook_type), intent(in) :: self
        type(hook_entry_type), intent(in) :: entries(:)
        type(status_type), intent(out) :: status
        type(hook_entry_type), allocatable :: global(:)
        type(value_type) :: saved
        integer :: i
        status = status_type()
        do i = 1, size(entries)
            if (entries(i)%is_t) then
                ! t indicates this hook has a local binding; it means to
                ! run the global binding too.
                if (.not. allocated(self%entries)) cycle
                global = self%entries
                call run_entries(self, global, status)
            else
                saved = current_prefix_arg
                current_prefix_arg = nil()
                call entries(i)%fn(status)
                current_prefix_arg = saved
            end if
            if (.not. status%ok()) return
        end do
    end subroutine run_entries

    logical function hook_member(self, fn) result(found)
        class(hook_type), intent(in) :: self
        procedure(command_proc) :: fn
        integer :: k
        found = .false.
        k = local_index(self)
        if (k > 0) found = find_fn(self%locals(k)%entries, fn) > 0
        if (.not. found .and. allocated(self%entries)) found = find_fn(self%entries, fn) > 0
    end function hook_member

    subroutine hook_kill_local(self)
        class(hook_type), intent(inout) :: self
        type(local_hook_type), allocatable :: kept(:)
        integer :: k, n
        k = local_index(self)
        if (k == 0) return
        n = size(self%locals)
        allocate (kept(n - 1))
        kept(1:k - 1) = self%locals(1:k - 1)
        kept(k:n - 1) = self%locals(k + 1:n)
        call move_alloc(kept, self%locals)
    end subroutine hook_kill_local

    !> Remember HOOK among the hooks with buffers' own functions.
    subroutine note_local_hook(hook)
        class(hook_type), intent(in), target :: hook
        type(hook_ref_type), allocatable :: grown(:)
        integer :: i, n
        if (.not. allocated(local_hooks)) allocate (local_hooks(0))
        n = size(local_hooks)
        do i = 1, n
            if (associated(local_hooks(i)%p, hook)) return
        end do
        allocate (grown(n + 1))
        grown(1:n) = local_hooks
        select type (hook)
        type is (hook_type)
            grown(n + 1)%p => hook
        end select
        call move_alloc(grown, local_hooks)
    end subroutine note_local_hook

    !> Forget the current buffer's own hook functions, except those of
    !> permanent-local hooks unless KILL_PERMANENT (part of
    !> buffer.c:reset_buffer_local_variables).
    subroutine kill_local_hooks(kill_permanent)
        logical, intent(in) :: kill_permanent
        integer :: i
        if (.not. allocated(local_hooks)) return
        do i = 1, size(local_hooks)
            ! The depth alist is not permanent.
            call kill_local_depths(local_hooks(i)%p)
            if (local_hooks(i)%p%permanent_local .and. .not. kill_permanent) cycle
            call local_hooks(i)%p%kill_local()
        end do
    end subroutine kill_local_hooks

    !> Forget the current buffer's own depths of HOOK.
    subroutine kill_local_depths(hook)
        type(hook_type), intent(inout) :: hook
        type(local_depths_type), allocatable :: kept(:)
        integer :: k, n
        k = local_depths_index(hook)
        if (k == 0) return
        n = size(hook%local_depths)
        allocate (kept(n - 1))
        kept(1:k - 1) = hook%local_depths(1:k - 1)
        kept(k:n - 1) = hook%local_depths(k + 1:n)
        call move_alloc(kept, hook%local_depths)
    end subroutine kill_local_depths

    logical function hook_local_p(self)
        class(hook_type), intent(in) :: self
        hook_local_p = local_index(self) > 0
    end function hook_local_p
end module eval_fm
