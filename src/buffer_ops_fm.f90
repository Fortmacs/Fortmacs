! buffer_ops_fm.f90 -- buffer.c commands that need markers or insdel.
! Ported from GNU Emacs 31.1 src/buffer.c.
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

!> Part of buffer_fm.  A submodule can use marker_fm and insdel_fm even
!> though both use buffer_fm, which a module cannot.
submodule(buffer_fm) buffer_ops_fm
    use marker_fm, only: unchain_all_markers, clear_charpos_cache, destroy_marker
    use insdel_fm, only: del_range
    use itree_fm, only: delete_all_overlays
    use eval_fm, only: buffer_list_update_hook
    use data_fm, only: buffer_local_value
    use fns_fm, only: value_equal
    use commands_fm, only: commands
    implicit none(type, external)

contains

    !> If it was current, the most recent other live, non-hidden buffer
    !> becomes current, creating *scratch* if there is none (as
    !> buffer.c:other_buffer_safely).  Its processes get SIGHUP and its
    !> overlays are deleted.  Hooks and indirect buffers are not handled
    !> yet.
    recursive module subroutine close_buffer(h, status)
        type(handle_type), intent(in) :: h
        type(status_type), intent(out) :: status
        type(handle_type) :: other
        type(status_type) :: ignore
        integer(int64) :: i, k
        logical :: was_current, inhibit_hooks

        call resolve_buffer(h, i, status)
        if (.not. status%ok()) return

        ! Run hooks with the buffer to be killed as the current buffer.
        if (associated(execute_close_buffer_hook)) then
            other = current_buffer()
            call set_buffer_internal(i)
            call execute_close_buffer_hook(h)
            call resolve_buffer(other, k, ignore)

            if (ignore%ok()) then
                call set_buffer_internal(k)
            else
                call set_buffer_internal(i)
            end if

            ! The hook may have killed the buffer.
            call resolve_buffer(h, i, status)

            if (.not. status%ok()) then
                status = status_type()
                return
            end if
        end if

        ! Make this buffer not be displayed.
        if (associated(replace_buffer_in_windows_hook)) call replace_buffer_in_windows_hook(h)
        ! Kill the processes of this buffer.
        if (associated(kill_buffer_processes_hook)) call kill_buffer_processes_hook(h)
        was_current = i == current
        ! Its local variables go with it: make it not current first.
        if (was_current) call set_buffer_internal(0_int64)
        buffers(i)%last_name = buffers(i)%name
        call buffer_order_remove(h)
        call frames_discard_buffer(h)
        call delete_all_overlays(h)
        ! Unchain all markers of this buffer and leave them pointing nowhere.
        call unchain_all_markers(i)
        call clear_charpos_cache(i)
        ! The mark and the mark ring belong to the buffer (upstream they are
        ! left to GC).
        if (buffers(i)%mark%index /= 0) call destroy_marker(buffers(i)%mark, ignore)

        if (allocated(buffers(i)%mark_ring)) then
            do k = 1, size(buffers(i)%mark_ring)
                call destroy_marker(buffers(i)%mark_ring(k), ignore)
            end do
        end if

        call table%release(h, status)
        if (.not. status%ok()) return
        if (allocated(buffers(i)%text%beginning)) deallocate (buffers(i)%text%beginning)
        inhibit_hooks = buffers(i)%inhibit_buffer_hooks
        buffers(i) = buffer_type()
        if (was_current) call other_buffer_current()
        if (.not. status%ok()) return
        if (.not. inhibit_hooks) call buffer_list_update_hook%execute(status)

    contains

        !> Make the most recent live, non-hidden buffer current.
        recursive subroutine other_buffer_current()
            do k = buffer_order_count, 1, -1
                if (buffers(buffer_order(k)%index)%name(1:1) /= " ") then
                    call set_buffer_internal(buffer_order(k)%index)
                    return
                end if
            end do

            call get_or_make_buffer("*scratch*", other, status)
            if (status%ok()) call set_buffer_internal(other%index)
        end subroutine other_buffer_current
    end subroutine close_buffer

    module subroutine erase_buffer(status)
        type(status_type), intent(out) :: status

        if (current == 0) return

        ! Fwiden (editfns.c), inline until editfns_fm exists.
        associate (b => buffers(current))
            b%begv = BEGINNING
            b%begv_byte = BEGINNING_BYTE
            b%zv = b%text%z
            b%zv_byte = b%text%z_byte
        end associate

        call del_range(BEGINNING, buffers(current)%text%z, status)
    end subroutine erase_buffer

    !> Drop `h' from the live-buffer list.  Lives here, its only user:
    !> gfortran drops private parent procedures that only a submodule uses.
    !> Not a port.
    subroutine buffer_order_remove(h)
        type(handle_type), intent(in) :: h
        integer(int64) :: k

        do k = 1, buffer_order_count
            if (buffer_order(k)%index == h%index .and. buffer_order(k)%generation == h%generation) then
                buffer_order(k:buffer_order_count - 1) = buffer_order(k + 1:buffer_order_count)
                buffer_order_count = buffer_order_count - 1
                return
            end if
        end do
    end subroutine buffer_order_remove

    recursive module subroutine execute_buffer_list_update_hook(i, status)
        integer(int64), intent(in) :: i
        type(status_type), intent(out) :: status

        status = status_type()
        if (i <= 0) return
        if (buffers(i)%inhibit_buffer_hooks) return
        call buffer_list_update_hook%execute(status)
    end subroutine execute_buffer_list_update_hook

    module function find_buffer(variable, value) result(h)
        type(value_type), intent(in) :: variable, value
        type(handle_type) :: h
        type(value_type) :: v
        type(status_type) :: st
        integer(int64) :: k, i

        h = null_handle()

        do k = buffer_order_count, 1, -1
            i = buffer_order(k)%index
            call buffer_local_value(variable, i, v, st)

            if (st%ok()) then
                if (value_equal(value, v)) then
                    h = buffer_order(k)
                    return
                end if
            end if
        end do
    end function find_buffer

    module subroutine syms_of_buffer()
        commands%buffer_enable_undo%execute => cmd_buffer_enable_undo
    end subroutine syms_of_buffer

    !> Start keeping undo information for buffer BUFFER.
    !> An absent argument means do this for the current buffer.
    !> Port of buffer.c:Fbuffer_enable_undo (the command: interactive "",
    !> the current buffer).
    subroutine cmd_buffer_enable_undo(status)
        type(status_type), intent(out) :: status

        call buffer_enable_undo(status)
    end subroutine cmd_buffer_enable_undo
end submodule buffer_ops_fm
