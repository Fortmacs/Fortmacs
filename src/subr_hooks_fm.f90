! subr_hooks_fm.f90 -- mode hooks, a submodule of subr_fm.
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

!> `run-mode-hooks' and `delay-mode-hooks': a submodule because they use
!> the buffers.  `delay-mode-hooks' (permanent-local) and
!> `delayed-mode-hooks' (buffer-local) are kept per buffer, by the
!> buffer's serial number.
submodule(subr_fm) subr_hooks_fm
    use buffer_fm, only: buffers, cur
    implicit none(type, external)

    !> A pointer to a hook.
    type :: hook_ref_type
        type(hook_type), pointer :: p => null()
    end type hook_ref_type

    !> One buffer's `delay-mode-hooks' and `delayed-mode-hooks'.
    type :: delay_type
        integer(int64) :: serial = 0
        logical :: delay = .false.
        type(hook_ref_type), allocatable :: delayed(:)
    end type delay_type

    type(delay_type), allocatable :: delays(:)

contains

    !> The index of the current buffer's entry in `delays', made if MAKE.
    integer function delay_index(make) result(k)
        logical, intent(in) :: make
        type(delay_type), allocatable :: grown(:)
        integer :: i, n
        k = 0
        if (cur == 0) return
        if (.not. allocated(delays)) allocate (delays(0))
        do i = 1, size(delays)
            if (delays(i)%serial == buffers(cur)%serial) then
                k = i
                return
            end if
        end do
        if (.not. make) return
        n = size(delays)
        allocate (grown(n + 1))
        grown(1:n) = delays
        grown(n + 1)%serial = buffers(cur)%serial
        allocate (grown(n + 1)%delayed(0))
        call move_alloc(grown, delays)
        k = n + 1
    end function delay_index

    module procedure kill_delayed_mode_hooks
    integer :: k
    k = delay_index(.false.)
    if (k == 0) return
    deallocate (delays(k)%delayed)
    allocate (delays(k)%delayed(0))
    end procedure kill_delayed_mode_hooks

    module procedure delaying_mode_hooks
    integer :: k
    k = delay_index(.false.)
    delaying_mode_hooks = .false.
    if (k > 0) delaying_mode_hooks = delays(k)%delay
    end procedure delaying_mode_hooks

    module procedure delay_mode_hooks_begin
    integer :: k
    saved = delaying_mode_hooks()
    k = delay_index(.true.)
    if (k > 0) delays(k)%delay = .true.
    end procedure delay_mode_hooks_begin

    module procedure delay_mode_hooks_end
    integer :: k
    k = delay_index(.true.)
    if (k > 0) delays(k)%delay = saved
    end procedure delay_mode_hooks_end

    module procedure run_mode_hooks
    type(hook_ref_type), allocatable :: delayed(:)
    integer :: k, i, n
    status = status_type()
    k = delay_index(.true.)
    if (k == 0) return
    if (delays(k)%delay) then
        ! Delaying case.
        if (present(hook)) then
            n = size(delays(k)%delayed)
            allocate (delayed(n + 1))
            delayed(1:n) = delays(k)%delayed
            delayed(n + 1)%p => hook
            call move_alloc(delayed, delays(k)%delayed)
        end if
        return
    end if
    ! Normal case, just run the hook as before plus any delayed hooks.
    call move_alloc(delays(k)%delayed, delayed)
    allocate (delays(k)%delayed(0))
    call change_major_mode_after_body_hook%run(status)
    if (.not. status%ok()) return
    do i = 1, size(delayed)
        call delayed(i)%p%run(status)
        if (.not. status%ok()) return
    end do
    if (present(hook)) then
        call hook%run(status)
        if (.not. status%ok()) return
    end if
    call after_change_major_mode_hook%run(status)
    end procedure run_mode_hooks
end submodule subr_hooks_fm
