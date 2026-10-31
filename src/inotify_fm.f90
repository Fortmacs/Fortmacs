! inotify_fm.f90 -- inotify support.
! Ported from GNU Emacs 31.1 src/inotify.c.
!
! Copyright (C) 2012-2026 Free Software Foundation, Inc.
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

!> Watching files with inotify.  One inotify instance serves every
!> watch; its descriptor is watched while Emacs waits for input
!> (keyboard_fm's `add_read_fd'), and each event it reports becomes a
!> special event that calls the watch's callback (inotify.c's
!> FILE_NOTIFY_EVENT, run through `special_event_map').
!>
!> inotify returns the same watch descriptor for every watch of one
!> file, so a watch is identified by that descriptor and an ID unique
!> among the watches sharing it (`watch_descriptor_type', upstream's
!> (DESCRIPTOR . ID)).  Aspects are masks of the `INOTIFY_*' constants
!> (upstream's symbols, `ior'ed together), the values of the IN_* bits.
!>
!> Deferred: the INOTIFY_DEBUG functions `inotify_watch_list' and
!> `inotify_allocated_p'; the Android special directories.
module inotify_fm
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use error_fm, only: status_type, set_error, FM_FILE_NOTIFY_ERROR
    use sysdep_fm, only: inotify_raw_event_type, emacs_inotify_init, emacs_inotify_add_watch, emacs_inotify_rm_watch, &
                         emacs_inotify_read, emacs_strerror, close_fd, EINVAL, &
                         IN_ACCESS, IN_MODIFY, IN_ATTRIB, IN_CLOSE_WRITE, IN_CLOSE_NOWRITE, IN_OPEN, IN_MOVED_FROM, &
                         IN_MOVED_TO, IN_CREATE, IN_DELETE, IN_DELETE_SELF, IN_MOVE_SELF, IN_UNMOUNT, IN_Q_OVERFLOW, &
                         IN_IGNORED, IN_ONLYDIR, IN_DONT_FOLLOW, IN_EXCL_UNLINK, IN_MASK_ADD, IN_ISDIR, &
                         IN_ALL_EVENTS, IN_MOVE, IN_CLOSE
    use keyboard_fm, only: add_read_fd, delete_read_fd, insert_special_event
    implicit none(type, external)
    private

    public :: watch_descriptor_type, inotify_event_type, inotify_callback_proc
    public :: inotify_add_watch, inotify_rm_watch, is_inotify_valid, operator(==)

    !> Aspects: what a watch looks for, and what an event reports.
    integer(int32), parameter, public :: INOTIFY_ACCESS = IN_ACCESS, INOTIFY_ATTRIB = IN_ATTRIB, &
                                         INOTIFY_CLOSE_WRITE = IN_CLOSE_WRITE, INOTIFY_CLOSE_NOWRITE = IN_CLOSE_NOWRITE, &
                                         INOTIFY_CREATE = IN_CREATE, INOTIFY_DELETE = IN_DELETE, &
                                         INOTIFY_DELETE_SELF = IN_DELETE_SELF, INOTIFY_MODIFY = IN_MODIFY, &
                                         INOTIFY_MOVE_SELF = IN_MOVE_SELF, INOTIFY_MOVED_FROM = IN_MOVED_FROM, &
                                         INOTIFY_MOVED_TO = IN_MOVED_TO, INOTIFY_OPEN = IN_OPEN, &
                                         INOTIFY_ALL_EVENTS = IN_ALL_EVENTS, INOTIFY_MOVE = IN_MOVE, &
                                         INOTIFY_CLOSE = IN_CLOSE
    !> Aspects controlling whether the watch is created.
    integer(int32), parameter, public :: INOTIFY_DONT_FOLLOW = IN_DONT_FOLLOW, INOTIFY_ONLYDIR = IN_ONLYDIR
    !> Aspects reported whether or not they were asked for (`ignored' and
    !> `unmount' may be asked for too).
    integer(int32), parameter, public :: INOTIFY_IGNORED = IN_IGNORED, INOTIFY_ISDIR = IN_ISDIR, &
                                         INOTIFY_Q_OVERFLOW = IN_Q_OVERFLOW, INOTIFY_UNMOUNT = IN_UNMOUNT

    !> The aspects `inotify_add_watch' accepts.
    integer(int32), parameter :: VALID_ASPECTS = ior(ior(ior(IN_ALL_EVENTS, IN_DONT_FOLLOW), ior(IN_ONLYDIR, IN_IGNORED)), &
                                                     IN_UNMOUNT)

    !> A watch, as `inotify_add_watch' returns it: the inotify watch
    !> descriptor and the watch's ID among those sharing it.
    type :: watch_descriptor_type
        integer(int32) :: wd = -1
        integer(int64) :: id = -1
    end type watch_descriptor_type

    !> An event: the watch it is for, its aspects, the name of the file
    !> it concerns (in a watched directory; else the watched file), and
    !> the cookie that pairs the `moved_from' and `moved_to' halves of a
    !> rename.  Upstream's (WATCH-DESCRIPTOR ASPECTS NAME COOKIE).
    type :: inotify_event_type
        type(watch_descriptor_type) :: descriptor
        integer(int32) :: aspects = 0
        character(len=:), allocatable :: name
        integer(int32) :: cookie = 0
    end type inotify_event_type

    abstract interface
        !> A watch's CALLBACK, called with each EVENT.  Not a port.
        recursive subroutine inotify_callback_proc(event, status)
            import :: inotify_event_type, status_type
            implicit none(type, external)
            type(inotify_event_type), intent(in) :: event
            type(status_type), intent(out) :: status
        end subroutine inotify_callback_proc
    end interface

    !> Compare two watch descriptors (upstream's `equal').
    interface operator(==)
        ! Not a port.
        module procedure descriptor_equal
    end interface operator(==)

    !> One watch of the watch list (upstream's (ID FILENAME CALLBACK
    !> MASK) under its DESCRIPTOR).
    type :: watch_type
        logical :: used = .false.
        integer(int32) :: wd = -1
        integer(int64) :: id = 0
        character(len=:), allocatable :: filename
        procedure(inotify_callback_proc), pointer, nopass :: callback => null()
        integer(int32) :: mask = 0
    end type watch_type

    !> An event waiting to be handled, with the callback to call.
    type :: pending_type
        logical :: used = .false.
        type(inotify_event_type) :: event
        procedure(inotify_callback_proc), pointer, nopass :: callback => null()
    end type pending_type

    !> File handle for inotify.
    integer(int32) :: inotifyfd = -1

    !> The files being watched (see the module note).
    type(watch_type), allocatable :: watch_list(:)

    !> Events stored as special events, by the argument they were stored
    !> with.
    type(pending_type), allocatable :: pending(:)

contains

    ! Not a port.
    logical function descriptor_equal(a, b)
        type(watch_descriptor_type), intent(in) :: a, b

        descriptor_equal = a%wd == b%wd .and. a%id == b%id
    end function descriptor_equal

    !> Signal a `file_notify_error': STRING, the message of ERR and NAME
    !> (if given).  Port of fileio.c:report_file_notify_error.
    subroutine report_file_notify_error(string, err, status, name)
        character(len=*), intent(in) :: string
        integer(int32), intent(in) :: err
        type(status_type), intent(inout) :: status
        character(len=*), intent(in), optional :: name

        if (present(name)) then
            call set_error(status, FM_FILE_NOTIFY_ERROR, string//": "//emacs_strerror(err)//", "//name)
        else
            call set_error(status, FM_FILE_NOTIFY_ERROR, string//": "//emacs_strerror(err))
        end if
    end subroutine report_file_notify_error

    !> The event for WATCH of inotify event EV, or false if the watch
    !> does not look for it.  Port of inotify.c:inotifyevent_to_event.
    logical function inotifyevent_to_event(watch, ev, event) result(wanted)
        type(watch_type), intent(in) :: watch
        type(inotify_raw_event_type), intent(in) :: ev
        type(inotify_event_type), intent(out) :: event

        wanted = .false.
        if (iand(watch%mask, ev%mask) == 0 &
            ! These event types are supposed to be reported whether or not
            ! they appeared in the ASPECT list when monitoring commenced.
            .and. iand(ev%mask, ior(ior(IN_IGNORED, IN_Q_OVERFLOW), ior(IN_ISDIR, IN_UNMOUNT))) == 0) return
        wanted = .true.
        event%descriptor%wd = ev%wd
        event%descriptor%id = watch%id
        event%aspects = ev%mask

        if (len(ev%name) > 0) then
            event%name = ev%name
        else
            event%name = watch%filename
        end if

        event%cookie = ev%cookie
    end function inotifyevent_to_event

    !> Add a new watch to watch descriptor WD watching FILENAME and using
    !> IMASK and CALLBACK.  The result uniquely identifies the new watch.
    !> Port of inotify.c:add_watch.
    function add_watch(wd, filename, imask, callback) result(desc)
        integer(int32), intent(in) :: wd, imask
        character(len=*), intent(in) :: filename
        procedure(inotify_callback_proc) :: callback
        type(watch_descriptor_type) :: desc
        type(watch_type), allocatable :: grown(:)
        integer(int64) :: id
        integer :: k, slot

        if (.not. allocated(watch_list)) allocate (watch_list(0))
        ! Assign a watch ID that is not already in use, by looking for a
        ! gap among the existing ones.
        id = 0

        do while (id_in_use(wd, id))
            id = id + 1
        end do

        slot = 0

        do k = 1, size(watch_list)
            if (.not. watch_list(k)%used) then
                slot = k
                exit
            end if
        end do

        if (slot == 0) then
            allocate (grown(size(watch_list) + 1))

            do k = 1, size(watch_list)
                call copy_watch(watch_list(k), grown(k))
            end do

            call move_alloc(grown, watch_list)
            slot = size(watch_list)
        end if

        watch_list(slot)%used = .true.
        watch_list(slot)%wd = wd
        watch_list(slot)%id = id
        watch_list(slot)%filename = filename
        watch_list(slot)%callback => callback
        watch_list(slot)%mask = imask
        desc%wd = wd
        desc%id = id
    end function add_watch

    ! Not a port.
    subroutine copy_watch(from, to)
        type(watch_type), intent(inout) :: from
        type(watch_type), intent(inout) :: to

        to%used = from%used
        to%wd = from%wd
        to%id = from%id
        if (allocated(from%filename)) call move_alloc(from%filename, to%filename)
        to%callback => from%callback
        to%mask = from%mask
    end subroutine copy_watch

    !> Whether watch ID of descriptor WD exists.  Not a port.
    logical function id_in_use(wd, id)
        integer(int32), intent(in) :: wd
        integer(int64), intent(in) :: id
        integer :: k

        id_in_use = .false.
        if (.not. allocated(watch_list)) return

        do k = 1, size(watch_list)
            if (watch_list(k)%used .and. watch_list(k)%wd == wd .and. watch_list(k)%id == id) then
                id_in_use = .true.
                return
            end if
        end do
    end function id_in_use

    !> Whether some watch uses descriptor WD.  Port of
    !> inotify.c:find_descriptor.
    logical function find_descriptor(wd)
        integer(int32), intent(in) :: wd

        find_descriptor = .false.
        if (.not. allocated(watch_list)) return
        find_descriptor = any(watch_list%used .and. watch_list%wd == wd)
    end function find_descriptor

    !> The watches of descriptor WD, in the order of their IDs.  Not a port.
    subroutine descriptor_watches(wd, idx)
        integer(int32), intent(in) :: wd
        integer, allocatable, intent(out) :: idx(:)
        integer :: k, j, n, t

        n = count(watch_list%used .and. watch_list%wd == wd)
        allocate (idx(n))
        j = 0

        do k = 1, size(watch_list)
            if (watch_list(k)%used .and. watch_list(k)%wd == wd) then
                j = j + 1
                idx(j) = k
            end if
        end do

        do k = 2, n
            t = idx(k)
            j = k - 1

            do while (j >= 1)
                if (watch_list(idx(j))%id <= watch_list(t)%id) exit
                idx(j + 1) = idx(j)
                j = j - 1
            end do

            idx(j + 1) = t
        end do
    end subroutine descriptor_watches

    !> Remove all watches associated with descriptor WD.  If INVALID_P
    !> is true, the descriptor is already invalid, i.e., it received an
    !> IN_IGNORED event.  In this case skip calling inotify_rm_watch.
    !> Port of inotify.c:remove_descriptor.
    subroutine remove_descriptor(wd, invalid_p, status)
        integer(int32), intent(in) :: wd
        logical, intent(in) :: invalid_p
        type(status_type), intent(inout) :: status
        integer(int32) :: inotify_errno
        integer :: k
        character(len=16) :: buf

        inotify_errno = 0
        if (.not. invalid_p) inotify_errno = emacs_inotify_rm_watch(inotifyfd, wd)

        do k = 1, size(watch_list)
            if (watch_list(k)%used .and. watch_list(k)%wd == wd) then
                watch_list(k)%used = .false.
                watch_list(k)%callback => null()
                if (allocated(watch_list(k)%filename)) deallocate (watch_list(k)%filename)
            end if
        end do

        if (.not. any(watch_list%used)) then
            call delete_read_fd(inotifyfd)
            call close_fd(inotifyfd)
            inotifyfd = -1
        end if

        if (inotify_errno /= 0) then
            write (buf, "(i0)") wd
            call report_file_notify_error("Could not rm watch", inotify_errno, status, trim(buf))
        end if
    end subroutine remove_descriptor

    !> Remove the watch identified by WD and ID.  Port of
    !> inotify.c:remove_watch.
    subroutine remove_watch(wd, id, status)
        integer(int32), intent(in) :: wd
        integer(int64), intent(in) :: id
        type(status_type), intent(inout) :: status
        integer :: k

        if (.not. find_descriptor(wd)) return

        do k = 1, size(watch_list)
            if (watch_list(k)%used .and. watch_list(k)%wd == wd .and. watch_list(k)%id == id) then
                if (count(watch_list%used .and. watch_list%wd == wd) == 1) then
                    call remove_descriptor(wd, .false., status)
                else
                    watch_list(k)%used = .false.
                    watch_list(k)%callback => null()
                end if

                return
            end if
        end do
    end subroutine remove_watch

    !> Called when the inotify descriptor FD is readable: read its events
    !> and store them as special events.  Port of
    !> inotify.c:inotify_callback.
    recursive subroutine inotify_callback(fd)
        integer(int32), intent(in) :: fd
        type(inotify_raw_event_type), allocatable :: evs(:)
        type(inotify_event_type) :: event
        type(status_type) :: st
        integer, allocatable :: idx(:)
        integer(int32) :: err
        integer :: i, k

        call emacs_inotify_read(fd, evs, err)

        ! A read error (report_file_notify_error "Error while reading file
        ! system events") has nobody to report to here.
        do i = 1, size(evs)
            if (.not. find_descriptor(evs(i)%wd)) cycle
            call descriptor_watches(evs(i)%wd, idx)

            do k = 1, size(idx)
                if (inotifyevent_to_event(watch_list(idx(k)), evs(i), event)) then
                    call store_event(event, watch_list(idx(k))%callback)
                end if
            end do

            ! If event was removed automatically: Drop it from watch list.
            if (iand(evs(i)%mask, IN_IGNORED) /= 0) call remove_descriptor(evs(i)%wd, .true., st)
        end do
    end subroutine inotify_callback

    !> Store EVENT for CALLBACK as a special event (kbd_buffer_store_event).
    !> Not a port.
    subroutine store_event(event, callback)
        type(inotify_event_type), intent(in) :: event
        procedure(inotify_callback_proc) :: callback
        type(pending_type), allocatable :: grown(:)
        integer :: k, slot

        if (.not. allocated(pending)) allocate (pending(0))
        slot = 0

        do k = 1, size(pending)
            if (.not. pending(k)%used) then
                slot = k
                exit
            end if
        end do

        if (slot == 0) then
            allocate (grown(max(8, 2*size(pending))))

            do k = 1, size(pending)
                grown(k)%used = pending(k)%used
                grown(k)%event%descriptor = pending(k)%event%descriptor
                grown(k)%event%aspects = pending(k)%event%aspects
                grown(k)%event%cookie = pending(k)%event%cookie
                if (allocated(pending(k)%event%name)) call move_alloc(pending(k)%event%name, grown(k)%event%name)
                grown(k)%callback => pending(k)%callback
            end do

            slot = size(pending) + 1
            call move_alloc(grown, pending)
        end if

        pending(slot)%used = .true.
        pending(slot)%event%descriptor = event%descriptor
        pending(slot)%event%aspects = event%aspects
        pending(slot)%event%name = event%name
        pending(slot)%event%cookie = event%cookie
        pending(slot)%callback => callback
        call insert_special_event(handle_event, int(slot, int64))
    end subroutine store_event

    !> Handle the stored event SLOT: call its callback.  Port of
    !> filenotify.el:file-notify-handle-event for inotify's events.
    recursive subroutine handle_event(slot, status)
        integer(int64), intent(in) :: slot
        type(status_type), intent(out) :: status
        type(inotify_event_type) :: event
        procedure(inotify_callback_proc), pointer :: callback

        status = status_type()
        event%descriptor = pending(slot)%event%descriptor
        event%aspects = pending(slot)%event%aspects
        call move_alloc(pending(slot)%event%name, event%name)
        event%cookie = pending(slot)%event%cookie
        callback => pending(slot)%callback
        pending(slot)%used = .false.
        pending(slot)%callback => null()
        if (associated(callback)) call callback(event, status)
    end subroutine handle_event

    !> Add a watch for FILE_NAME to inotify.
    !> Return a watch descriptor.  The watch will look for ASPECT events and
    !> invoke CALLBACK when an event occurs.
    !>
    !> ASPECT is one of the following constants, or several of them
    !> combined with `ior':
    !>
    !> INOTIFY_ACCESS
    !> INOTIFY_ATTRIB
    !> INOTIFY_CLOSE_WRITE
    !> INOTIFY_CLOSE_NOWRITE
    !> INOTIFY_CREATE
    !> INOTIFY_DELETE
    !> INOTIFY_DELETE_SELF
    !> INOTIFY_MODIFY
    !> INOTIFY_MOVE_SELF
    !> INOTIFY_MOVED_FROM
    !> INOTIFY_MOVED_TO
    !> INOTIFY_OPEN
    !>
    !> INOTIFY_ALL_EVENTS
    !> INOTIFY_MOVE
    !> INOTIFY_CLOSE
    !>
    !> ASPECT can also contain the following constants, which control whether
    !> the watch descriptor will be created:
    !>
    !> INOTIFY_DONT_FOLLOW
    !> INOTIFY_ONLYDIR
    !>
    !> Watching a directory is not recursive.  CALLBACK is passed a single argument
    !> EVENT which contains an event structure of the format
    !>
    !> (WATCH_DESCRIPTOR ASPECTS NAME COOKIE)
    !>
    !> WATCH_DESCRIPTOR is the same object that was returned by this function.  It can
    !> be tested for equality using `=='.  ASPECTS describes the event.  It is a
    !> combination of the ASPECT constants described above and can also contain one
    !> of the following constants
    !>
    !> INOTIFY_IGNORED
    !> INOTIFY_ISDIR
    !> INOTIFY_Q_OVERFLOW
    !> INOTIFY_UNMOUNT
    !>
    !> If a directory is watched then NAME is the name of file that caused the event.
    !>
    !> COOKIE is an object that can be compared using `==' to identify two matching
    !> renames (INOTIFY_MOVED_FROM and INOTIFY_MOVED_TO).
    !>
    !> See inotify(7) and inotify_add_watch(2) for further information.  The
    !> inotify fd is managed internally and there is no corresponding
    !> inotify_init.  Use `inotify_rm_watch' to remove a watch.
    !>
    !> The following inotify bit-masks cannot be used because descriptors are
    !> shared across different callers.
    !>
    !> IN_EXCL_UNLINK
    !> IN_MASK_ADD
    !> IN_ONESHOT
    !>
    !> Port of inotify.c:Finotify_add_watch.
    subroutine inotify_add_watch(file_name, aspect, callback, desc, status)
        character(len=*), intent(in) :: file_name
        integer(int32), intent(in) :: aspect
        procedure(inotify_callback_proc) :: callback
        type(watch_descriptor_type), intent(out) :: desc
        type(status_type), intent(out) :: status
        integer(int32) :: imask, mask, wd, err
        character(len=16) :: buf

        if (iand(aspect, not(VALID_ASPECTS)) /= 0) then
            write (buf, "(i0)") aspect
            call report_file_notify_error("Unknown aspect", EINVAL, status, trim(buf))
            return
        end if

        imask = aspect
        mask = ior(ior(imask, IN_MASK_ADD), IN_EXCL_UNLINK)

        if (inotifyfd < 0) then
            err = emacs_inotify_init(inotifyfd)

            if (err /= 0) then
                inotifyfd = -1
                call report_file_notify_error("File watching is not available", err, status)
                return
            end if

            call add_read_fd(inotifyfd, inotify_callback)
        end if

        err = emacs_inotify_add_watch(inotifyfd, file_name, mask, wd)

        if (err /= 0) then
            call report_file_notify_error("Could not add watch for file", err, status, file_name)
            return
        end if

        desc = add_watch(wd, file_name, imask, callback)
    end subroutine inotify_add_watch

    ! Not a port.
    logical function valid_watch_descriptor(wd)
        type(watch_descriptor_type), intent(in) :: wd

        valid_watch_descriptor = wd%wd >= 0 .and. wd%id >= 0
    end function valid_watch_descriptor

    !> Remove an existing WATCH_DESCRIPTOR.
    !>
    !> WATCH_DESCRIPTOR should be an object returned by `inotify_add_watch'.
    !>
    !> See inotify_rm_watch(2) for more information.  Port of
    !> inotify.c:Finotify_rm_watch.
    subroutine inotify_rm_watch(watch_descriptor, status)
        type(watch_descriptor_type), intent(in) :: watch_descriptor
        type(status_type), intent(out) :: status
        character(len=48) :: buf

        if (.not. valid_watch_descriptor(watch_descriptor)) then
            write (buf, "(i0, ' . ', i0)") watch_descriptor%wd, watch_descriptor%id
            call report_file_notify_error("Invalid descriptor ", EINVAL, status, "("//trim(buf)//")")
            return
        end if

        call remove_watch(watch_descriptor%wd, watch_descriptor%id, status)
    end subroutine inotify_rm_watch

    !> Check a watch specified by its WATCH_DESCRIPTOR.
    !>
    !> WATCH_DESCRIPTOR should be an object returned by `inotify_add_watch'.
    !>
    !> A watch can become invalid if the file or directory it watches is
    !> deleted, or if the watcher thread exits abnormally for any other
    !> reason.  Removing the watch by calling `inotify_rm_watch' also makes
    !> it invalid.  Port of inotify.c:Finotify_valid_p.
    logical function is_inotify_valid(watch_descriptor)
        type(watch_descriptor_type), intent(in) :: watch_descriptor

        is_inotify_valid = .false.
        if (.not. valid_watch_descriptor(watch_descriptor)) return
        is_inotify_valid = id_in_use(watch_descriptor%wd, watch_descriptor%id)
    end function is_inotify_valid
end module inotify_fm
