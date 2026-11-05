! basic_subprograms_hooks_mod.f90 -- mode hooks, a submodule of basic_subprograms_mod.
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

!> `execute_mode_hooks' and `delay_mode_hooks': a submodule because they use
!> syntax_propertize_mod (which uses basic_subprograms_mod) and
!> the buffers.  `delay_mode_hooks' (permanent-local) and
!> `delayed_mode_hooks' (buffer-local) are kept per buffer, by the
!> buffer's serial number.
submodule(basic_subprograms_mod) basic_subprograms_hooks_mod
    use buffer_mod, only: buffers, current
    use syntax_mod, only: parse_expression_lookup_properties
    use syntax_propertize_mod, only: syntax_propertize_function
    implicit none(type, external)

    !> A pointer to a hook.
    type :: hook_reference_type
        type(hook_type), pointer :: p => null()
    end type hook_reference_type

    !> A function of `delayed_after_hook_functions'.
    type :: after_fn_type
        procedure(after_hook_procedure), pointer, nopass :: fn => null()
    end type after_fn_type

    !> One buffer's `delay_mode_hooks', `delayed_mode_hooks' and
    !> `delayed_after_hook_functions'.
    type :: delay_type
        integer(int64) :: serial = 0
        logical :: delay = .false.
        type(hook_reference_type), allocatable :: delayed(:)
        type(after_fn_type), allocatable :: after(:)
    end type delay_type

    type(delay_type), allocatable :: delays(:)

contains

    !> The index of the current buffer's entry in `delays' (0 if none).
    !> Not a port.
    integer function delay_index() result(k)
        k = 0
        if (current == 0) return
        if (.not. allocated(delays)) return
        k = findloc(delays%serial, buffers(current)%serial, dim=1)
    end function delay_index

    !> Find or make the current buffer's entry in `delays', its index K (0
    !> without a current buffer).  Not a port.
    subroutine make_delay_entry(k)
        integer, intent(out) :: k
        type(delay_type) :: entry

        k = delay_index()
        if (k > 0 .or. current == 0) return
        if (.not. allocated(delays)) allocate (delays(0))
        entry%serial = buffers(current)%serial
        allocate (entry%delayed(0))
        allocate (entry%after(0))
        delays = [delays, entry]
        k = size(delays)
    end subroutine make_delay_entry

    module procedure clear_delayed_mode_hooks
    integer :: k

    k = delay_index()
    if (k == 0) return
    deallocate (delays(k)%delayed)
    allocate (delays(k)%delayed(0))
    deallocate (delays(k)%after)
    allocate (delays(k)%after(0))
    end procedure clear_delayed_mode_hooks

    module procedure add_delayed_after_hook
    integer :: k

    call make_delay_entry(k)
    if (k == 0) return
    ! push: the newest first.
    delays(k)%after = [after_fn_type(fn), delays(k)%after]
    end procedure add_delayed_after_hook

    module procedure delaying_mode_hooks
    integer :: k

    k = delay_index()
    delaying_mode_hooks = .false.
    if (k > 0) delaying_mode_hooks = delays(k)%delay
    end procedure delaying_mode_hooks

    module procedure delay_mode_hooks_begin
    integer :: k

    saved = delaying_mode_hooks()
    call make_delay_entry(k)
    if (k > 0) delays(k)%delay = .true.
    end procedure delay_mode_hooks_begin

    module procedure delay_mode_hooks_end
    integer :: k

    call make_delay_entry(k)
    if (k > 0) delays(k)%delay = saved
    end procedure delay_mode_hooks_end

    module procedure execute_mode_hooks
    type(hook_reference_type), allocatable :: delayed(:)
    type(after_fn_type), allocatable :: after(:)
    integer :: k, i

    status = status_type()
    call make_delay_entry(k)
    if (k == 0) return

    if (delays(k)%delay) then
        ! Delaying case.
        if (present(hook)) delays(k)%delayed = [delays(k)%delayed, hook_reference_type(hook)]
        return
    end if

    ! Normal case, just run the hook as before plus any delayed hooks.
    call move_alloc(delays(k)%delayed, delayed)
    allocate (delays(k)%delayed(0))

    ! `syntax_propertize' sets `parse_expression_lookup_properties' for us, but
    ! in order for the expression primitives to automatically call
    ! `syntax_propertize' we need `parse_expression_lookup_properties' to be
    ! set first.
    if (associated(syntax_propertize_function%current()) .and. .not. parse_expression_lookup_properties%local_p()) then
        call parse_expression_lookup_properties%set_local(.true.)
    end if

    call change_major_mode_after_body_hook%execute(status)
    if (.not. status%ok()) return

    do i = 1, size(delayed)
        call delayed(i)%p%execute(status)
        if (.not. status%ok()) return
    end do

    if (present(hook)) then
        call hook%execute(status)
        if (.not. status%ok()) return
    end if

    call after_change_major_mode_hook%execute(status)
    if (.not. status%ok()) return
    ! The delayed after-hook functions, oldest first (nreverse).
    k = delay_index()
    if (k == 0) return
    call move_alloc(delays(k)%after, after)
    allocate (delays(k)%after(0))

    do i = size(after), 1, -1
        call after(i)%fn(status)
        if (.not. status%ok()) return
    end do
    end procedure execute_mode_hooks
end submodule basic_subprograms_hooks_mod
