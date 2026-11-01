! subr_change_group_mod.f90 -- change groups, a submodule of subr_mod.
! Ported from GNU Emacs 31.1 lisp/subr.el (in part: the change groups).
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

!> The change groups of subr.el (`prepare_change_group',
!> `activate_change_group', `accept_change_group', `cancel_change_group'):
!> upstream's atomic-change-group is written with them as prepare,
!> activate, the body, then accept on success or cancel on an error.  A
!> submodule because simple_mod (undo) uses subr_mod.
submodule(subr_mod) subr_change_group_mod
    use buffer_mod, only: buffers, current, current_buffer, set_buffer, UE_INSERTION, UE_BOUNDARY, undo_entry_type
    use undo_mod, only: append_undo_entry
    use simple_mod, only: primitive_undo
    use editfns_mod, only: point, goto_char, point_min, point_max, widen, narrow_to_region
    use marker_mod, only: copy_marker_at, marker_position, destroy_marker
    implicit none(type, external)

contains

    !> Port of subr.el:prepare-change-group.
    module subroutine prepare_change_group(handle)
        type(change_group_type), intent(out) :: handle

        handle%buffer = current_buffer()
        handle%undo_disabled = .not. buffers(current)%undo_enabled
        handle%undo_count = buffers(current)%undo_count
    end subroutine prepare_change_group

    !> Port of subr.el:activate-change-group.
    module subroutine activate_change_group(handle)
        type(change_group_type), intent(in) :: handle
        type(handle_type) :: old
        type(undo_entry_type) :: empty
        type(status_type) :: st
        integer(int64) :: n

        old = current_buffer()
        call set_buffer(handle%buffer, st)
        if (.not. st%ok()) return

        if (.not. buffers(current)%undo_enabled) then
            buffers(current)%undo_enabled = .true.
            buffers(current)%undo_count = 0
        else
            ! Add a boundary to make sure the upcoming changes won't be
            ! merged/combined with any previous changes (bug#33341): an
            ! "empty insertion" at the position of the last insertion,
            ! which acts like a boundary w.r.t. preventing merges while
            ! being harmless.
            n = buffers(current)%undo_count

            if (n > 0) then
                if (buffers(current)%undo_entries(n)%kind == UE_INSERTION) then
                    empty%kind = UE_INSERTION
                    empty%beginning = buffers(current)%undo_entries(n)%beginning
                    empty%end = empty%beginning
                    call append_undo_entry(empty)
                end if
            end if
        end if

        call set_buffer(old, st)
    end subroutine activate_change_group

    !> Port of subr.el:accept-change-group.
    module subroutine accept_change_group(handle)
        type(change_group_type), intent(in) :: handle
        type(handle_type) :: old
        type(status_type) :: st

        if (.not. handle%undo_disabled) return
        old = current_buffer()
        call set_buffer(handle%buffer, st)
        if (.not. st%ok()) return
        buffers(current)%undo_enabled = .false.
        buffers(current)%undo_count = 0
        call set_buffer(old, st)
    end subroutine accept_change_group

    !> Port of subr.el:cancel-change-group.
    recursive module subroutine cancel_change_group(handle, status)
        type(change_group_type), intent(in) :: handle
        type(status_type), intent(out) :: status
        type(handle_type) :: old
        type(undo_entry_type) :: saved_entry
        type(status_type) :: st
        type(handle_type) :: opoint
        integer(int64) :: begv, zv_from_end, list, rest, saved, pos

        old = current_buffer()
        call set_buffer(handle%buffer, status)
        if (.not. status%ok()) return
        ! Widen buffer temporarily so if the buffer was narrowed within
        ! the body of the group all changes can be undone.
        begv = point_min()
        zv_from_end = buffers(current)%text%z - point_max()
        call widen()
        saved = 0
        if (.not. handle%undo_disabled) saved = handle%undo_count
        ! Temporarily truncate the undo log at the saved state.
        if (saved > 0) then
            saved_entry = buffers(current)%undo_entries(saved)
            buffers(current)%undo_entries(saved)%kind = UE_BOUNDARY
        end if

        ! Undo it all.
        call copy_marker_at(point(), opoint, .false., st)
        list = buffers(current)%undo_count

        do while (list > saved)
            call primitive_undo(1_int64, list, rest, status)
            if (.not. status%ok()) exit
            list = rest
        end do

        call marker_position(opoint, pos, st)
        call goto_char(pos)
        call destroy_marker(opoint, st)
        ! Reset the modified entry to its original content, and revert the
        ! undo info to what it was when we grabbed the state.
        if (saved > 0) buffers(current)%undo_entries(saved) = saved_entry
        buffers(current)%undo_count = saved
        if (handle%undo_disabled) buffers(current)%undo_enabled = .false.
        call narrow_to_region(begv, buffers(current)%text%z - zv_from_end, st)
        call set_buffer(old, st)
    end subroutine cancel_change_group
end submodule subr_change_group_mod
