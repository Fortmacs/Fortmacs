! undo_fm.f90 -- recording changes for undo.
! Ported from GNU Emacs 31.1 src/undo.c, plus the undo-auto boundary
! functions of lisp/simple.el, which keyboard.c and cmds.c call.
!
! Copyright (C) 1990, 1993-1994, 2000-2026 Free Software Foundation,
! Inc.
! Copyright (C) 1985-2026 Free Software Foundation, Inc. (simple.el)
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

!> Every change to a buffer is recorded in its `buffer-undo-list' (here
!> the `undo_list' array of buffer_type, newest entry last): insertions,
!> deletions with the deleted text, where point was, and the first change
!> after the buffer was saved.  Boundaries (nil entries) separate the
!> changes of one command from the next; the command loop adds them before
!> each command (`undo_auto_add_boundary'), and runs of self-inserting or
!> deleting commands are merged up to `amalgamating_undo_limit' at a time.
!>
!> The undo-auto functions are simple.el's, here because the C side
!> calls them (keyboard.c before each command, cmds.c for
!> self-insert-command and delete-char).
!>
!> Not ported yet: marker adjustments, text property changes,
!> `combine-change-calls', the boundary timer, and truncating the list
!> (undo-limit and friends): undo information grows without bound.
module undo_fm
    use, intrinsic :: iso_fortran_env, only: int64
    use handle_fm, only: handle_type
    use buffer_fm, only: buffers, cur, buffer_handle, buffer_live_p, resolve_buffer, set_buffer, &
                         undo_entry_type, UE_BOUNDARY, UE_INSERTION, UE_DELETION, UE_POINT, &
                         UE_FIRST_CHANGE
    use error_fm, only: status_type
    implicit none(type, external)
    private

    public :: record_insert, record_delete, record_first_change, record_point, undo_boundary
    public :: push_undo_entry, undo_last_kind
    public :: undo_auto_add_boundary, undo_auto_amalgamate, undo_auto_undoable_change

    !> Where point was before the last command or `undo_boundary', and in
    !> which buffer (its serial number), for `record_point'.  Port of
    !> undo.c point_before_last_command_or_undo and
    !> buffer_before_last_command_or_undo.
    integer(int64), public :: point_before_last_command_or_undo = 0
    integer(int64), public :: buffer_before_last_command_or_undo = -1

    !> The maximum number of changes to possibly amalgamate when undoing
    !> changes.  The `undo' command will normally consider "similar"
    !> changes (like inserting characters) to be part of the same change.
    !> Port of simple.el amalgamating-undo-limit.
    integer, public :: amalgamating_undo_limit = 20

    ! simple.el undo-auto--last-boundary-cause: why the last boundary was
    ! added; for `amalgamate`, how many amalgamating commands ran and which
    ! buffers changed.
    integer, parameter :: CAUSE_NONE = 0, CAUSE_EXPLICIT = 1, CAUSE_COMMAND = 2, &
                          CAUSE_AMALGAMATE = 3
    integer :: last_boundary_cause = CAUSE_NONE
    integer :: last_amalgamating_count = 0
    type(handle_type), allocatable :: last_boundary_buffers(:)
    ! simple.el undo-auto--undoably-changed-buffers and
    ! undo-auto--this-command-amalgamating.
    type(handle_type), allocatable :: changed_buffers(:)
    logical :: this_command_amalgamating = .false.

contains

    !> The kind of the newest entry of the current buffer's undo list, or
    !> UE_BOUNDARY if it is empty.
    integer function undo_last_kind() result(kind)
        kind = UE_BOUNDARY
        if (buffers(cur)%undo_count > 0) kind = buffers(cur)%undo_list(buffers(cur)%undo_count)%kind
    end function undo_last_kind

    !> Push `entry' onto the current buffer's undo list.
    subroutine push_undo_entry(entry)
        type(undo_entry_type), intent(in) :: entry
        type(undo_entry_type), allocatable :: grown(:)
        integer(int64), allocatable :: equiv(:)
        integer(int64) :: n
        associate (b => buffers(cur))
            if (.not. allocated(b%undo_list)) allocate (b%undo_list(64))
            if (b%undo_count == size(b%undo_list, kind=int64)) then
                allocate (grown(2*b%undo_count))
                grown(1:b%undo_count) = b%undo_list(1:b%undo_count)
                call move_alloc(grown, b%undo_list)
            end if
            n = b%undo_count + 1
            b%undo_list(n) = entry
            b%undo_count = n
            ! The list position is new: nothing in undo-equiv-table maps it.
            if (.not. allocated(b%undo_equiv)) then
                allocate (b%undo_equiv(0:size(b%undo_list, kind=int64)))
                b%undo_equiv = -2
            else if (ubound(b%undo_equiv, 1) < size(b%undo_list, kind=int64)) then
                allocate (equiv(0:size(b%undo_list, kind=int64)))
                equiv = -2
                equiv(0:ubound(b%undo_equiv, 1)) = b%undo_equiv
                call move_alloc(equiv, b%undo_equiv)
            end if
            b%undo_equiv(n) = -2
        end associate
    end subroutine push_undo_entry

    !> Record point, if necessary, as it was at beginning of this command.
    !> BEG is the position of point that will naturally occur as a result
    !> of the undo record that will be added just after this command
    !> terminates.  Port of undo.c:record_point.
    subroutine record_point(beg)
        integer(int64), intent(in) :: beg
        type(undo_entry_type) :: entry
        logical :: at_boundary
        at_boundary = undo_last_kind() == UE_BOUNDARY
        ! If this is the first change since save, then record this.
        if (buffers(cur)%text%modiff <= buffers(cur)%text%save_modiff) call record_first_change()
        ! We may need to record point if we are immediately after a
        ! boundary, so that this will be restored correctly after undo.
        if (at_boundary .and. point_before_last_command_or_undo /= beg .and. &
            buffer_before_last_command_or_undo == buffers(cur)%serial) then
            entry%kind = UE_POINT
            entry%beg = point_before_last_command_or_undo
            call push_undo_entry(entry)
        end if
    end subroutine record_point

    !> Record an insertion of LENGTH characters at position BEG.
    !> Port of undo.c:record_insert.
    subroutine record_insert(beg, length)
        integer(int64), intent(in) :: beg, length
        type(undo_entry_type) :: entry
        integer(int64) :: n
        if (.not. buffers(cur)%undo_enabled) return
        call undo_auto_undoable_change()
        call record_point(beg)
        n = buffers(cur)%undo_count
        ! If this is following another insertion and consecutive with it
        ! in the buffer, combine the two.
        if (n > 0) then
            if (buffers(cur)%undo_list(n)%kind == UE_INSERTION .and. buffers(cur)%undo_list(n)%end == beg) then
                buffers(cur)%undo_list(n)%end = beg + length
                return
            end if
        end if
        entry%kind = UE_INSERTION
        entry%beg = beg
        entry%end = beg + length
        call push_undo_entry(entry)
    end subroutine record_insert

    !> Record that a deletion is about to take place, of the NCHARS
    !> characters in STRING (internal encoding), at location BEG.  Port of
    !> undo.c:record_delete (no marker adjustments).
    subroutine record_delete(beg, string, nchars)
        integer(int64), intent(in) :: beg, nchars
        character(len=*), intent(in) :: string
        type(undo_entry_type) :: entry
        if (.not. buffers(cur)%undo_enabled) return
        call undo_auto_undoable_change()
        call record_point(beg)
        entry%kind = UE_DELETION
        entry%text = string
        if (buffers(cur)%pt == beg + nchars) then
            entry%beg = -beg
        else
            entry%beg = beg
        end if
        call push_undo_entry(entry)
    end subroutine record_delete

    !> Record that an unmodified buffer is about to be changed.
    !> Record the file modification date so that when undoing this entry
    !> we can tell whether it is obsolete because the file was saved again.
    !> Port of undo.c:record_first_change.
    subroutine record_first_change()
        type(undo_entry_type) :: entry
        if (.not. buffers(cur)%undo_enabled) return
        entry%kind = UE_FIRST_CHANGE
        entry%known = buffers(cur)%modtime_known
        if (entry%known) then
            entry%beg = buffers(cur)%modtime_sec
            entry%end = buffers(cur)%modtime_nsec
        end if
        call push_undo_entry(entry)
    end subroutine record_first_change

    !> Mark a boundary between units of undo.
    !> An undo command will stop at this point,
    !> but another undo command will undo to the previous boundary.
    !> Port of undo.c:Fundo_boundary.
    subroutine undo_boundary()
        type(undo_entry_type) :: entry
        if (.not. buffers(cur)%undo_enabled) return
        if (undo_last_kind() /= UE_BOUNDARY) call push_undo_entry(entry)
        last_boundary_cause = CAUSE_EXPLICIT
        point_before_last_command_or_undo = buffers(cur)%pt
        buffer_before_last_command_or_undo = buffers(cur)%serial
    end subroutine undo_boundary

    ! ------------------------------------------------------------------
    ! simple.el's undo-auto
    ! ------------------------------------------------------------------

    !> Called after every undoable buffer change.  Port of
    !> simple.el:undo-auto--undoable-change (no boundary timer).
    subroutine undo_auto_undoable_change()
        type(handle_type) :: h
        h = buffer_handle(cur)
        if (.not. member(changed_buffers, h)) call add_handle(changed_buffers, h)
    end subroutine undo_auto_undoable_change

    !> Whether `h' is in `list'.
    logical function member(list, h)
        type(handle_type), allocatable, intent(in) :: list(:)
        type(handle_type), intent(in) :: h
        integer :: k
        member = .false.
        if (.not. allocated(list)) return
        do k = 1, size(list)
            if (list(k)%index == h%index .and. list(k)%generation == h%generation) member = .true.
        end do
    end function member

    subroutine add_handle(list, h)
        type(handle_type), allocatable, intent(inout) :: list(:)
        type(handle_type), intent(in) :: h
        type(handle_type), allocatable :: longer(:)
        integer :: n
        if (.not. allocated(list)) allocate (list(0))
        n = size(list)
        allocate (longer(n + 1))
        longer(1:n) = list
        longer(n + 1) = h
        call move_alloc(longer, list)
    end subroutine add_handle

    !> Add an `undo_boundary' to the current buffer if needed.  Port of
    !> simple.el:undo-auto--ensure-boundary.
    subroutine ensure_boundary(cause)
        integer, intent(in) :: cause
        integer :: last_amalgamating
        logical :: amalgamating
        if (undo_last_kind() == UE_BOUNDARY) return
        amalgamating = last_boundary_cause == CAUSE_AMALGAMATE
        last_amalgamating = last_amalgamating_count
        call undo_boundary()
        if (cause == CAUSE_AMALGAMATE) then
            last_boundary_cause = CAUSE_AMALGAMATE
            last_amalgamating_count = 0
            if (amalgamating) last_amalgamating_count = last_amalgamating + 1
            last_boundary_buffers = changed_buffers
        else
            last_boundary_cause = cause
        end if
    end subroutine ensure_boundary

    !> Check recently changed buffers and add a boundary if necessary.
    !> Port of simple.el:undo-auto--boundaries.
    subroutine undo_auto_boundaries(cause)
        integer, intent(in) :: cause
        type(handle_type) :: old
        type(handle_type), allocatable :: list(:)
        type(status_type) :: st
        integer :: k
        if (cur == 0) return
        ! All commands should ensure that there is an undo boundary whether
        ! they have changed the current buffer or not (bug#23785).
        if (cause == CAUSE_COMMAND) then
            if (.not. member(changed_buffers, buffer_handle(cur))) call add_handle(changed_buffers, buffer_handle(cur))
        end if
        if (.not. allocated(changed_buffers)) return
        old = buffer_handle(cur)
        list = changed_buffers
        do k = 1, size(list)
            if (.not. buffer_live_p(list(k))) cycle
            call set_buffer(list(k), st)
            if (buffers(cur)%undo_enabled) call ensure_boundary(cause)
        end do
        call set_buffer(old, st)
        deallocate (changed_buffers)
    end subroutine undo_auto_boundaries

    !> Add an `undo-boundary' in appropriate buffers.  The command loop
    !> calls this before each command.  Port of
    !> simple.el:undo-auto--add-boundary.
    subroutine undo_auto_add_boundary()
        integer :: cause
        cause = CAUSE_COMMAND
        if (this_command_amalgamating) cause = CAUSE_AMALGAMATE
        this_command_amalgamating = .false.
        call undo_auto_boundaries(cause)
    end subroutine undo_auto_add_boundary

    !> Amalgamate undo if necessary.
    !> This function can be called before an amalgamating command.  It
    !> removes the previous `undo-boundary' if a series of such calls
    !> have been made.  `self-insert-command' and `delete-char' are the
    !> most common amalgamating commands.  SAME_COMMAND says whether
    !> this-command is last-command.  Port of simple.el:undo-auto-amalgamate.
    subroutine undo_auto_amalgamate(same_command)
        logical, intent(in) :: same_command
        type(handle_type) :: old
        type(status_type) :: st
        integer :: k
        this_command_amalgamating = .true.
        if (last_boundary_cause /= CAUSE_AMALGAMATE) return
        if (last_amalgamating_count < amalgamating_undo_limit .and. same_command) then
            ! Amalgamate all buffers that have changed.
            if (.not. allocated(last_boundary_buffers) .or. cur == 0) return
            old = buffer_handle(cur)
            do k = 1, size(last_boundary_buffers)
                if (.not. buffer_live_p(last_boundary_buffers(k))) cycle
                call set_buffer(last_boundary_buffers(k), st)
                associate (b => buffers(cur))
                    ! The head of `buffer-undo-list` is nil: drop it.
                    if (b%undo_count > 0) then
                        if (b%undo_list(b%undo_count)%kind == UE_BOUNDARY) b%undo_count = b%undo_count - 1
                    end if
                end associate
            end do
            call set_buffer(old, st)
        else
            last_boundary_cause = CAUSE_NONE
        end if
    end subroutine undo_auto_amalgamate
end module undo_fm
