! filenotify_fm.f90 -- watch files for changes on disk.
! Ported from GNU Emacs 31.1 lisp/filenotify.el.
!
! Copyright (C) 2013-2026 Free Software Foundation, Inc.
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

!> The file notification library, on top of inotify (the only backend
!> on Linux; kqueue, gfilenotify and w32notify are not applicable).
!> Flags are `ior'ed `FILE_NOTIFY_CHANGE' and
!> `FILE_NOTIFY_ATTRIBUTE_CHANGE' (upstream's list of `change' and
!> `attribute-change'); actions are the `FILE_NOTIFY_*' constants
!> (upstream's symbols); a descriptor is inotify_fm's
!> `watch_descriptor_type'.
!>
!> Deferred: file name handlers (remote files: `file_notify_callback'
!> and the handler operations), `file_notify_debug', the `renamed' action
!> with a new name from a backend (inotify reports renames as two halves,
!> which this library pairs).
module filenotify_fm
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use error_fm, only: status_type, set_error, condition_matches, FM_FILE_NOTIFY_ERROR, FM_WRONG_TYPE_ARGUMENT
    use inotify_fm, only: watch_descriptor_type, inotify_event_type, inotify_add_watch, inotify_rm_watch, &
                          is_inotify_valid, operator(==), INOTIFY_CREATE, INOTIFY_DELETE, INOTIFY_DELETE_SELF, &
                          INOTIFY_MODIFY, INOTIFY_MOVE_SELF, INOTIFY_MOVE, INOTIFY_ATTRIB, INOTIFY_DONT_FOLLOW, &
                          INOTIFY_IGNORED, INOTIFY_UNMOUNT, INOTIFY_MOVED_FROM, INOTIFY_MOVED_TO
    use keyboard_fm, only: insert_special_event
    use fileio_fm, only: expand_file_name, is_file_directory, directory_file_name, file_name_directory, &
                         file_name_nondirectory, is_file_name_absolute
    use files_fm, only: is_backup_file_name
    implicit none(type, external)
    private

    public :: file_notify_event_type, file_notify_callback_proc
    public :: file_notify_add_watch, file_notify_rm_watch, file_notify_rm_all_watches, is_file_notify_valid, &
              file_notify_watch_count

    !> Flags of `file_notify_add_watch': watch for file changes, and for
    !> changes of file attributes.
    integer, parameter, public :: FILE_NOTIFY_CHANGE = 1, FILE_NOTIFY_ATTRIBUTE_CHANGE = 2

    !> The actions of an event.
    integer, parameter, public :: FILE_NOTIFY_CREATED = 1, FILE_NOTIFY_DELETED = 2, FILE_NOTIFY_CHANGED = 3, &
                                  FILE_NOTIFY_RENAMED = 4, FILE_NOTIFY_ATTRIBUTE_CHANGED = 5, FILE_NOTIFY_STOPPED = 6
    !> Actions inside the library only: the halves of a rename.
    integer, parameter :: RENAMED_FROM = 7, RENAMED_TO = 8

    !> An event passed to a watch's callback: (DESCRIPTOR ACTION FILE
    !> [FILE1]), FILE1 (the new name of a renamed file) being unallocated
    !> when absent.
    type :: file_notify_event_type
        type(watch_descriptor_type) :: descriptor
        integer :: action = 0
        character(len=:), allocatable :: file
        character(len=:), allocatable :: file1
    end type file_notify_event_type

    abstract interface
        !> The CALLBACK of `file_notify_add_watch'.  Not a port.
        recursive subroutine file_notify_callback_proc(event, status)
            import :: file_notify_event_type, status_type
            implicit none(type, external)
            type(file_notify_event_type), intent(in) :: event
            type(status_type), intent(out) :: status
        end subroutine file_notify_callback_proc
    end interface

    !> The internal struct for bookkeeping watched files or directories,
    !> under its descriptor: an entry of `file_notify_descriptors'.
    !> Port of filenotify.el's file-notify--watch (FILENAME unallocated for a
    !> watched directory).
    type :: watch_type
        logical :: used = .false.
        type(watch_descriptor_type) :: desc
        character(len=:), allocatable :: directory
        character(len=:), allocatable :: filename
        procedure(file_notify_callback_proc), pointer, nopass :: callback => null()
    end type watch_type

    !> Registered file notification descriptors (upstream's hash table
    !> file-notify-descriptors).
    type(watch_type), allocatable :: file_notify_descriptors(:)

    !> A pending rename event awaiting the destination file name
    !> (file-notify--pending-rename): the watch's descriptor, the old
    !> name and the cookie, when `pending_rename'.
    logical :: pending_rename = .false.
    type(watch_descriptor_type) :: pending_desc
    character(len=:), allocatable :: pending_from_file
    integer(int32) :: pending_cookie = 0

    !> `stopped' events sent by `file_notify_rm_descriptor' and not yet
    !> handled, with their callbacks.
    type :: stopped_type
        logical :: used = .false.
        type(file_notify_event_type) :: event
        procedure(file_notify_callback_proc), pointer, nopass :: callback => null()
    end type stopped_type
    type(stopped_type), allocatable :: stopped(:)

contains

    !> The entry of DESC in `file_notify_descriptors' (0 for none).
    !> Not a port.
    integer function find_watch(desc) result(k)
        type(watch_descriptor_type), intent(in) :: desc
        integer :: i

        k = 0
        if (.not. allocated(file_notify_descriptors)) return

        do i = 1, size(file_notify_descriptors)
            if (file_notify_descriptors(i)%used) then
                if (file_notify_descriptors(i)%desc == desc) then
                    k = i
                    return
                end if
            end if
        end do
    end function find_watch

    !> The number of registered watches (the count of
    !> `file_notify_descriptors').  Not a port.
    integer function file_notify_watch_count()
        file_notify_watch_count = 0
        if (allocated(file_notify_descriptors)) file_notify_watch_count = count(file_notify_descriptors%used)
    end function file_notify_watch_count

    !> Return the absolute filename observed by watch K.  Port of
    !> filenotify.el:file-notify--watch-absolute-filename.
    function watch_absolute_filename(k) result(name)
        integer, intent(in) :: k
        character(len=:), allocatable :: name
        type(status_type) :: st

        if (allocated(file_notify_descriptors(k)%filename)) then
            call expand_file_name(file_notify_descriptors(k)%filename, name, st, file_notify_descriptors(k)%directory)
        else
            name = file_notify_descriptors(k)%directory
        end if
    end function watch_absolute_filename

    !> Remove DESCRIPTOR from `file_notify_descriptors'.
    !> DESCRIPTOR should be an object returned by `file_notify_add_watch'.
    !> If it is registered in `file_notify_descriptors', a `stopped' event is sent.
    !> Port of filenotify.el:file-notify--rm-descriptor.
    subroutine file_notify_rm_descriptor(descriptor)
        type(watch_descriptor_type), intent(in) :: descriptor
        type(stopped_type), allocatable :: grown(:)
        integer :: k, i, slot

        k = find_watch(descriptor)
        if (k == 0) return
        if (.not. allocated(stopped)) allocate (stopped(0))
        slot = 0

        do i = 1, size(stopped)
            if (.not. stopped(i)%used) then
                slot = i
                exit
            end if
        end do

        if (slot == 0) then
            allocate (grown(size(stopped) + 1))

            do i = 1, size(stopped)
                grown(i)%used = stopped(i)%used
                grown(i)%event%descriptor = stopped(i)%event%descriptor
                grown(i)%event%action = stopped(i)%event%action
                if (allocated(stopped(i)%event%file)) call move_alloc(stopped(i)%event%file, grown(i)%event%file)
                grown(i)%callback => stopped(i)%callback
            end do

            call move_alloc(grown, stopped)
            slot = size(stopped)
        end if

        stopped(slot)%used = .true.
        stopped(slot)%event%descriptor = descriptor
        stopped(slot)%event%action = FILE_NOTIFY_STOPPED
        stopped(slot)%event%file = watch_absolute_filename(k)
        stopped(slot)%callback => file_notify_descriptors(k)%callback
        call insert_special_event(handle_stopped, int(slot, int64))
        file_notify_descriptors(k)%callback => null()
        file_notify_descriptors(k)%used = .false.
        if (allocated(file_notify_descriptors(k)%directory)) deallocate (file_notify_descriptors(k)%directory)
        if (allocated(file_notify_descriptors(k)%filename)) deallocate (file_notify_descriptors(k)%filename)
    end subroutine file_notify_rm_descriptor

    !> Handle the `stopped' event SLOT: call its callback.  Port of
    !> filenotify.el:file-notify-handle-event.
    recursive subroutine handle_stopped(slot, status)
        integer(int64), intent(in) :: slot
        type(status_type), intent(out) :: status
        type(file_notify_event_type) :: event
        procedure(file_notify_callback_proc), pointer :: callback

        status = status_type()
        event%descriptor = stopped(slot)%event%descriptor
        event%action = stopped(slot)%event%action
        call move_alloc(stopped(slot)%event%file, event%file)
        callback => stopped(slot)%callback
        stopped(slot)%used = .false.
        stopped(slot)%callback => null()
        if (associated(callback)) call callback(event, status)
    end subroutine handle_stopped

    !> Full file name of FILE reported for watch K.  Port of
    !> filenotify.el:file-notify--expand-file-name (no remote files).
    function notify_expand_file_name(k, file) result(name)
        integer, intent(in) :: k
        character(len=*), intent(in) :: file
        character(len=:), allocatable :: name
        type(status_type) :: st

        if (is_file_name_absolute(file)) then
            name = directory_file_name(file)
        else
            call expand_file_name(file, name, st, file_notify_descriptors(k)%directory)
            name = directory_file_name(name)
        end if
    end function notify_expand_file_name

    !> Notification callback for inotify.  Port of
    !> filenotify.el:file-notify--callback-inotify (the actions in the
    !> order inotify.c:mask_to_aspects lists the aspects).
    recursive subroutine file_notify_callback_inotify(event, status)
        type(inotify_event_type), intent(in) :: event
        type(status_type), intent(out) :: status
        integer, parameter :: NBITS = 10
        integer(int32), parameter :: bits(NBITS) = [INOTIFY_UNMOUNT, INOTIFY_IGNORED, INOTIFY_MOVED_TO, INOTIFY_MOVED_FROM, &
                                                    INOTIFY_MOVE_SELF, INOTIFY_MODIFY, INOTIFY_DELETE_SELF, INOTIFY_DELETE, &
                                                    INOTIFY_CREATE, INOTIFY_ATTRIB]
        integer, parameter :: actions_of(NBITS) = [FILE_NOTIFY_STOPPED, FILE_NOTIFY_STOPPED, RENAMED_TO, RENAMED_FROM, &
                                                   FILE_NOTIFY_DELETED, FILE_NOTIFY_CHANGED, FILE_NOTIFY_DELETED, &
                                                   FILE_NOTIFY_DELETED, FILE_NOTIFY_CREATED, FILE_NOTIFY_ATTRIBUTE_CHANGED]
        integer :: actions(NBITS), n, k

        n = 0

        do k = 1, NBITS
            if (iand(event%aspects, bits(k)) /= 0) then
                n = n + 1
                actions(n) = actions_of(k)
            end if
        end do

        call file_notify_handle_event(event%descriptor, actions(1:n), event%name, event%cookie, status)
    end subroutine file_notify_callback_inotify

    !> Call the handler of watch K with DESC, ACTION, FILE and FILE1 (if
    !> present).  Port of filenotify.el:file-notify--call-handler.
    recursive subroutine call_handler(k, desc, action, file, status, file1)
        integer, intent(in) :: k
        type(watch_descriptor_type), intent(in) :: desc
        integer, intent(in) :: action
        character(len=*), intent(in) :: file
        type(status_type), intent(inout) :: status
        character(len=*), intent(in), optional :: file1
        type(file_notify_event_type) :: ev
        procedure(file_notify_callback_proc), pointer :: callback
        logical :: wanted

        wanted = .not. allocated(file_notify_descriptors(k)%filename)
        if (.not. wanted) wanted = file_notify_descriptors(k)%filename == file_name_nondirectory(file)
        if (.not. wanted .and. present(file1)) wanted = file_notify_descriptors(k)%filename == file_name_nondirectory(file1)
        if (.not. wanted) return
        callback => file_notify_descriptors(k)%callback
        if (.not. associated(callback)) return
        ev%descriptor = desc
        ev%action = action
        ev%file = file
        if (present(file1)) ev%file1 = file1
        call callback(ev, status)
    end subroutine call_handler

    !> Call the callback of the watch with descriptor DESC, if it is
    !> still registered, with ACTION, FILE and FILE1 (if present).
    !> Not a port.
    recursive subroutine call_callback(desc, action, file, status, file1)
        type(watch_descriptor_type), intent(in) :: desc
        integer, intent(in) :: action
        character(len=*), intent(in) :: file
        type(status_type), intent(inout) :: status
        character(len=*), intent(in), optional :: file1
        type(file_notify_event_type) :: ev
        integer :: k

        k = find_watch(desc)
        if (k == 0) return
        if (.not. associated(file_notify_descriptors(k)%callback)) return
        ev%descriptor = desc
        ev%action = action
        ev%file = file
        if (present(file1)) ev%file1 = file1
        call file_notify_descriptors(k)%callback(ev, status)
    end subroutine call_callback

    !> Handle an event returned from file notification.
    !> DESC is the back-end descriptor.  ACTIONS is a list of:
    !>  `created'
    !>  `changed'
    !>  `attribute_changed'
    !>  `deleted'
    !>  `renamed_from'      -- FILE is old name, COOKIE identifies the rename
    !>  `renamed_to'        -- FILE is new name, COOKIE identifies the rename
    !>  `stopped'           -- no more events after this should be sent
    !> Port of filenotify.el:file-notify--handle-event.
    recursive subroutine file_notify_handle_event(desc, actions, file_in, cookie, status)
        type(watch_descriptor_type), intent(in) :: desc
        integer, intent(in) :: actions(:)
        character(len=*), intent(in) :: file_in
        integer(int32), intent(in) :: cookie
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: file, file1, from_file
        type(watch_descriptor_type) :: p_desc
        integer :: i, k, action
        logical :: have_file1

        status = status_type()
        k = find_watch(desc)
        if (k == 0) return
        file = notify_expand_file_name(k, file_in)

        do i = 1, size(actions)
            action = actions(i)

            if (pending_rename) then
                if (.not. (pending_cookie == cookie .and. action == RENAMED_TO)) then
                    pending_rename = .false.
                    call call_callback(pending_desc, FILE_NOTIFY_DELETED, pending_from_file, status)
                end if
            end if

            have_file1 = .false.

            select case (action)
            case (FILE_NOTIFY_STOPPED)
                call file_notify_rm_watch(desc)
                exit
            case (RENAMED_FROM)
                pending_rename = .true.
                pending_desc = desc
                pending_from_file = file
                pending_cookie = cookie
                action = 0
            case (RENAMED_TO)
                if (pending_rename) then
                    p_desc = pending_desc
                    from_file = pending_from_file
                    file1 = file
                    have_file1 = .true.
                    file = from_file
                    pending_rename = .false.

                    if (.not. (desc == p_desc)) then
                        call call_callback(p_desc, FILE_NOTIFY_RENAMED, file, status, file1)
                    end if

                    action = FILE_NOTIFY_RENAMED
                else
                    action = FILE_NOTIFY_CREATED
                end if
            case default
            end select

            ! The watch may have been removed by a callback.
            k = find_watch(desc)

            if (action /= 0 .and. k /= 0) then
                if (have_file1) then
                    call call_handler(k, desc, action, file, status, file1)
                else
                    call call_handler(k, desc, action, file, status)
                end if
            end if

            k = find_watch(desc)
            if (k == 0) cycle

            if (action == FILE_NOTIFY_DELETED .or. action == FILE_NOTIFY_RENAMED) then
                if (have_file1) then
                    if (is_backup_file_name(file1)) cycle
                end if

                if (file == watch_absolute_filename(k)) call file_notify_rm_watch(desc)
            end if
        end do
    end subroutine file_notify_handle_event

    !> Add a watch for filesystem events pertaining to FILE.
    !> This arranges for filesystem events pertaining to FILE to be reported
    !> to Emacs.  Use `file_notify_rm_watch' to cancel the watch.
    !>
    !> The returned value is a descriptor for the added watch.  If the
    !> file cannot be watched for some reason, this function signals a
    !> `file_notify_error' error.
    !>
    !> FLAGS is a list of conditions to set what will be watched for.  It can
    !> include the following constants, combined with `ior':
    !>
    !>   `FILE_NOTIFY_CHANGE'           -- watch for file changes
    !>   `FILE_NOTIFY_ATTRIBUTE_CHANGE' -- watch for file attributes changes, like
    !>                                     permissions or modification time
    !>
    !> If FILE is a directory, `FILE_NOTIFY_CHANGE' watches for file creation or
    !> deletion in that directory.  Some of the file notification
    !> backends report also file changes.  This does not work
    !> recursively.
    !>
    !> When any event happens, Emacs will call the CALLBACK function passing
    !> it a single argument EVENT, which is of the form
    !>
    !>   (DESCRIPTOR ACTION FILE [FILE1])
    !>
    !> DESCRIPTOR is the same object as the one returned by this function.
    !> ACTION is the description of the event.  It could be any one of the
    !> following:
    !>
    !>   `FILE_NOTIFY_CREATED'           -- FILE was created
    !>   `FILE_NOTIFY_DELETED'           -- FILE was deleted
    !>   `FILE_NOTIFY_CHANGED'           -- FILE has changed
    !>   `FILE_NOTIFY_RENAMED'           -- FILE has been renamed to FILE1
    !>   `FILE_NOTIFY_ATTRIBUTE_CHANGED' -- a FILE attribute was changed
    !>   `FILE_NOTIFY_STOPPED'           -- watching FILE has been stopped
    !>
    !> FILE is the name of the file whose event is being reported.
    !> Port of filenotify.el:file-notify-add-watch.
    subroutine file_notify_add_watch(file_in, flags, callback, desc, status)
        character(len=*), intent(in) :: file_in
        integer, intent(in) :: flags
        procedure(file_notify_callback_proc) :: callback
        type(watch_descriptor_type), intent(out) :: desc
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: file, dir
        type(watch_type), allocatable :: grown(:)
        character(len=16) :: buf
        integer(int32) :: aspect
        integer :: k, i

        call expand_file_name(file_in, file, status)
        if (.not. status%ok()) return

        if (flags == 0 .or. iand(flags, not(ior(FILE_NOTIFY_CHANGE, FILE_NOTIFY_ATTRIBUTE_CHANGE))) /= 0) then
            write (buf, "(i0)") flags
            call set_error(status, FM_WRONG_TYPE_ARGUMENT, "("//trim(buf)//")")
            return
        end if

        if (is_file_directory(file)) then
            dir = directory_file_name(file)
        else
            dir = directory_file_name(file_name_directory(file))
        end if

        if (.not. is_file_directory(dir)) then
            call set_error(status, FM_FILE_NOTIFY_ERROR, "Directory does not exist: "//dir)
            return
        end if

        ! file-notify--add-watch-inotify.
        aspect = ior(INOTIFY_DONT_FOLLOW, ior(INOTIFY_IGNORED, INOTIFY_UNMOUNT))

        if (iand(flags, FILE_NOTIFY_CHANGE) /= 0) then
            aspect = ior(aspect, ior(ior(INOTIFY_CREATE, INOTIFY_DELETE), ior(INOTIFY_DELETE_SELF, INOTIFY_MODIFY)))
            aspect = ior(aspect, ior(INOTIFY_MOVE_SELF, INOTIFY_MOVE))
        end if

        if (iand(flags, FILE_NOTIFY_ATTRIBUTE_CHANGE) /= 0) aspect = ior(aspect, INOTIFY_ATTRIB)
        call inotify_add_watch(dir, aspect, file_notify_callback_inotify, desc, status)
        if (.not. status%ok()) return
        ! puthash: replace an entry for the same descriptor.
        k = find_watch(desc)

        if (k == 0) then
            if (.not. allocated(file_notify_descriptors)) allocate (file_notify_descriptors(0))

            do i = 1, size(file_notify_descriptors)
                if (.not. file_notify_descriptors(i)%used) then
                    k = i
                    exit
                end if
            end do
        end if

        if (k == 0) then
            allocate (grown(size(file_notify_descriptors) + 1))

            do i = 1, size(file_notify_descriptors)
                grown(i)%used = file_notify_descriptors(i)%used
                grown(i)%desc = file_notify_descriptors(i)%desc

                if (allocated(file_notify_descriptors(i)%directory)) then
                    call move_alloc(file_notify_descriptors(i)%directory, grown(i)%directory)
                end if

                if (allocated(file_notify_descriptors(i)%filename)) then
                    call move_alloc(file_notify_descriptors(i)%filename, grown(i)%filename)
                end if

                grown(i)%callback => file_notify_descriptors(i)%callback
            end do

            call move_alloc(grown, file_notify_descriptors)
            k = size(file_notify_descriptors)
        end if

        file_notify_descriptors(k)%used = .true.
        file_notify_descriptors(k)%desc = desc
        file_notify_descriptors(k)%directory = dir
        if (allocated(file_notify_descriptors(k)%filename)) deallocate (file_notify_descriptors(k)%filename)
        if (.not. is_file_directory(file)) file_notify_descriptors(k)%filename = file_name_nondirectory(file)
        file_notify_descriptors(k)%callback => callback
    end subroutine file_notify_add_watch

    !> Remove an existing watch specified by its DESCRIPTOR.
    !> DESCRIPTOR should be an object returned by `file_notify_add_watch'.
    !> Port of filenotify.el:file-notify-rm-watch.
    recursive subroutine file_notify_rm_watch(descriptor)
        type(watch_descriptor_type), intent(in) :: descriptor
        type(status_type) :: st
        integer :: k

        k = find_watch(descriptor)
        if (k == 0) return
        if (.not. associated(file_notify_descriptors(k)%callback)) return
        ! A `file_notify_error' is ignored.
        call inotify_rm_watch(descriptor, st)
        call file_notify_rm_descriptor(descriptor)
    end subroutine file_notify_rm_watch

    !> Remove all existing file notification watches from Emacs.  Port of
    !> filenotify.el:file-notify-rm-all-watches.
    subroutine file_notify_rm_all_watches()
        integer :: k

        if (.not. allocated(file_notify_descriptors)) return

        do k = 1, size(file_notify_descriptors)
            if (file_notify_descriptors(k)%used) call file_notify_rm_watch(file_notify_descriptors(k)%desc)
        end do

        deallocate (file_notify_descriptors)
    end subroutine file_notify_rm_all_watches

    !> Check a watch specified by its DESCRIPTOR.
    !> DESCRIPTOR should be an object returned by `file_notify_add_watch'.
    !> Port of filenotify.el:file-notify-valid-p.
    logical function is_file_notify_valid(descriptor)
        type(watch_descriptor_type), intent(in) :: descriptor

        is_file_notify_valid = .false.
        if (find_watch(descriptor) == 0) return
        is_file_notify_valid = is_inotify_valid(descriptor)
    end function is_file_notify_valid
end module filenotify_fm
