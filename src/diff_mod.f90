! diff_mod.f90 -- run `diff'.
! Ported from GNU Emacs 31.1 lisp/vc/diff.el.
!
! Copyright (C) 1992-2026 Free Software Foundation, Inc.
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

!> Comparing files and buffers with `diff': the commands `diff',
!> `diff_backup', `diff_buffer_with_file' and `diff_buffers', and
!> `diff_no_select', which runs `diff_command' (asynchronously, unless
!> asked otherwise) into a *Diff* buffer in Diff mode that `revert_buffer'
!> (g) runs again.  Buffers are compared through temporary files.
!>
!> Deferred: file name handlers (remote files: `file_local_copy'),
!> `with_file_modes' for the temporary files (create_temporary_file makes them
!> 0600 already), the coding system of buffers
!> (diff--coding-system-for-buffer) and `diff_switches' as a list of
!> strings (a string of switches, separated by spaces, is the same).
module diff_mod
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use execute_interactively_mod, only: PREFIX_NONE
    use commands_mod, only: commands
    use error_mod, only: status_type, set_error, GENERIC_ERROR_CODE
    use handle_mod, only: handle_type, null_handle
    use value_mod, only: value_type, xfixnum, list, vtag, V_INTEGER
    use data_mod, only: symbol_value, set, choice_setting_type
    use buffer_mod, only: buffers, current, current_buffer, set_buffer, get_buffer, get_or_make_buffer, is_buffer_live, &
                          resolve_buffer, erase_buffer
    use editing_functions_mod, only: point, goto_character, point_minimum, point_maximum, widen, narrow_to_region
    use time_functions_mod, only: current_time_string
    use insert_delete_mod, only: insert
    use search_mod, only: search_backward, SEARCH_NOERROR
    use marker_mod, only: marker_position, set_marker
    use execute_interactively_mod, only: current_prefix_argument
    use basic_subprograms_mod, only: shell_quote_argument
    use minibuffer_reading_mod, only: read_string
    use minibuffer_mod, only: read_file_name, read_buffer, REQUIRE_MATCH_YES
    use file_input_output_mod, only: expand_file_name, file_exists, file_name_directory, file_name_nondirectory, write_region
    use files_mod, only: create_temporary_file, file_newest_backup, is_backup_file_name, file_name_sans_versions, &
                         delete_file, revert_buffer_function, temporary_file_directory
    use execute_process_mod, only: execute_process, shell_file_name, initialize_callproc
    use process_mod, only: make_process, process_buffer, process_mark, process_exit_status, &
                           set_process_filter, set_process_sentinel
    use simple_mod, only: shell_command_switch
    use window_mod, only: display_buffer, other_buffer
    use diff_mode_mod, only: diff_mode, set_diff_default_directory
    use string_type_mod, only: string_type, append_string
    implicit none(type, external)
    private

    public :: initialize_diff, diff, diff_no_select, diff_backup, diff_latest_backup_file, diff_buffer_with_file, &
              diff_buffers, diff_sentinel, diff_check_labels, diff_operand_type

    !> A string specifying switches to be passed to diff.
    !>
    !> This variable is also used in the `vc_diff' command (and related
    !> commands) if the backend-specific diff switch variable isn't set
    !> (`vc_git_diff_switches' for git, for instance), and
    !> `vc_diff_switches' isn't set.  Port of diff.el diff-switches.
    character(len=:), allocatable, public :: diff_switches

    !> The command to use to run diff.  Port of diff.el diff-command.
    character(len=:), allocatable, public :: diff_command

    !> If true, diff the entire buffers, not just the visible part.
    !> If false, only use the narrowed-to parts of the buffers.  Port of
    !> diff.el diff-entire-buffers.
    logical, public :: diff_entire_buffers = .true.

    !> Values of `diff_use_labels' besides .true. and .false.
    integer, parameter, public :: DIFF_USE_LABELS_CHECK = 2

    !> Whether `diff_command' understands the "--label" option.
    !> Possible values are:
    !>   .true.                -- yes, it does
    !>   .false.               -- no, it does not
    !>   DIFF_USE_LABELS_CHECK -- try to probe whether it does
    !> Port of diff.el diff-use-labels.
    type(choice_setting_type), public :: diff_use_labels = &
                                         choice_setting_type(name="diff_use_labels", choice=DIFF_USE_LABELS_CHECK)

    !> What `diff' compares: a file (by name) or a buffer.
    type, public :: diff_operand_type
        logical :: is_buffer = .false.
        character(len=:), allocatable :: file
        type(handle_type) :: buffer
    end type diff_operand_type

    !> A *Diff* buffer's `revert_buffer_function' arguments, by buffer
    !> serial number.
    type :: revert_arguments_type
        integer(int64) :: serial = 0
        type(diff_operand_type) :: old, new
        character(len=:), allocatable :: switches
        logical :: no_async = .false.
    end type revert_arguments_type

    type(revert_arguments_type), allocatable :: revert_arguments(:)

    !> The temporary files of a running diff process, deleted by its
    !> sentinel.
    type :: diff_process_type
        type(handle_type) :: process
        character(len=:), allocatable :: old_temporary, new_temporary
    end type diff_process_type

    type(diff_process_type), allocatable :: diff_processes(:)

contains

    !> The switches to use: read from the minibuffer with a prefix
    !> argument (DIFFER false otherwise: use the default).  Port of
    !> diff.el:diff-switches.
    subroutine read_diff_switches(switches, given, status)
        character(len=:), allocatable, intent(out) :: switches
        logical, intent(out) :: given
        type(status_type), intent(out) :: status

        status = status_type()
        given = current_prefix_argument%kind /= PREFIX_NONE
        if (given) call read_string("Diff switches: ", switches, status, initial_input=diff_switches)
    end subroutine read_diff_switches

    !> Code run when the diff process exits.
    !> CODE is the exit code of the process.  It should be 0 only if no
    !> diffs were found.  If OLD_TEMPORARY_FILE and/or NEW_TEMPORARY_FILE are
    !> present, delete the temporary files so named.  Port of
    !> diff.el:diff-sentinel (diff-setup-whitespace is deferred).
    subroutine diff_sentinel(code, old_temporary_file, new_temporary_file)
        integer, intent(in) :: code
        character(len=*), intent(in), optional :: old_temporary_file, new_temporary_file
        type(status_type) :: st
        type(value_type) :: saved
        integer(int64) :: original_point
        character(len=:), allocatable :: what

        if (present(old_temporary_file)) then
            if (len(old_temporary_file) > 0) call delete_file(old_temporary_file, st)
        end if

        if (present(new_temporary_file)) then
            if (len(new_temporary_file) > 0) call delete_file(new_temporary_file, st)
        end if

        call goto_character(point_minimum())
        original_point = point()
        call goto_character(point_maximum())
        call symbol_value(value_type(name="inhibit_read_only"), saved, st)
        call set(value_type(name="inhibit_read_only"), value_type(.true.), st)

        select case (code)
        case (0)
            what = " (no differences)"
        case (2)
            what = " (diff error)"
        case default
            what = ""
        end select

        call insert(achar(10)//"Diff finished"//what//".  "//current_time_string()//achar(10), st)
        call set(value_type(name="inhibit_read_only"), saved, st)
        call goto_character(original_point)
    end subroutine diff_sentinel

    !> Find and display the differences between OLD and NEW files.
    !> When called interactively, read NEW, then OLD, using the
    !> minibuffer.  The default for NEW is the current buffer's file name,
    !> and the default for OLD is a backup file for NEW, if one exists.
    !> If NO_ASYNC is true, call diff synchronously.
    !>
    !> When called interactively with a prefix argument SWITCHES, prompt
    !> interactively for diff switches.  Otherwise, the switches
    !> specified in the variable `diff_switches' are passed to the diff
    !> command.
    !>
    !> Non-interactively, OLD and NEW may each be a file or a buffer.
    !> Port of diff.el:diff.
    recursive subroutine diff(old, new, status, switches, no_async)
        type(diff_operand_type), intent(in) :: old, new
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: switches
        logical, intent(in), optional :: no_async
        type(handle_type) :: buffer, window

        call diff_no_select(old, new, buffer, status, switches, no_async)
        if (.not. status%ok()) return
        call display_buffer(buffer, window, status)
    end subroutine diff

    !> Like `file_local_copy' but also supports a buffer as the argument.
    !> When FILE_OR_BUFFER is a buffer, give in TEMPFILE the filename of a
    !> local temporary file with the buffer's contents (else nothing:
    !> files are local).  Port of diff.el:diff-file-local-copy.
    subroutine diff_file_local_copy(file_or_buffer, tempfile, status)
        type(diff_operand_type), intent(in) :: file_or_buffer
        character(len=:), allocatable, intent(out) :: tempfile
        type(status_type), intent(out) :: status
        type(handle_type) :: old
        type(status_type) :: st
        integer(int64) :: savedb, savedz
        character(len=:), allocatable :: message_text

        status = status_type()
        if (.not. file_or_buffer%is_buffer) return
        old = current_buffer()
        call set_buffer(file_or_buffer%buffer, status)
        if (.not. status%ok()) return
        call create_temporary_file("buffer-content-", tempfile, status)

        if (status%ok()) then
            if (diff_entire_buffers) then
                savedb = buffers(current)%beginning_of_accessible_portion
                savedz = buffers(current)%end_of_accessible_portion
                call widen()
                call write_region(tempfile, .false., status, point_minimum(), point_maximum(), message=message_text)
                call narrow_to_region(savedb, savedz, st)
            else
                call write_region(tempfile, .false., status, point_minimum(), point_maximum(), message=message_text)
            end if
        end if

        call set_buffer(old, st)
    end subroutine diff_file_local_copy

    !> Whether `diff_command' understands "--label", probing it once (or
    !> again with FORCE).  Port of diff.el:diff-check-labels.
    subroutine diff_check_labels(force)
        logical, intent(in), optional :: force
        type(handle_type) :: old, temporary
        type(value_type) :: result
        type(status_type) :: st
        integer(int64) :: found
        logical :: again

        again = .false.
        if (present(force)) again = force
        if (.not. again .and. diff_use_labels%choice /= DIFF_USE_LABELS_CHECK) return
        diff_use_labels = .false.
        old = current_buffer()
        call get_or_make_buffer(" *diff-check-labels*", temporary, st)
        call set_buffer(temporary, st)
        call erase_buffer(st)
        call execute_process(diff_command, result, st, destination=value_type(.true.), arguments=list(value_type(text="--help")))

        if (st%ok()) then
            call search_backward("--label", found, st, noerror=SEARCH_NOERROR)
            if (found /= 0) diff_use_labels = .true.
        end if

        call set_buffer(old, st)
        call close_temporary(temporary)

    contains

        subroutine close_temporary(b)
            use buffer_mod, only: close_buffer
            type(handle_type), intent(in) :: b
            type(status_type) :: st2

            call close_buffer(b, st2)
        end subroutine close_temporary
    end subroutine diff_check_labels

    !> How OPERAND reads in a label: its file name or the buffer
    !> (prin1-to-string).  Not a port.
    function operand_label(operand) result(s)
        type(diff_operand_type), intent(in) :: operand
        character(len=:), allocatable :: s
        integer(int64) :: bi
        type(status_type) :: st

        if (.not. operand%is_buffer) then
            s = operand%file
        else
            call resolve_buffer(operand%buffer, bi, st)
            s = "#<buffer "//buffers(bi)%name//">"
        end if
    end function operand_label

    !> Compare the OLD and NEW file/buffer.
    !> If SWITCHES is absent, the switches specified in the variable
    !> `diff_switches' are passed to the diff command, otherwise SWITCHES
    !> is used.
    !>
    !> If NO_ASYNC is true, call diff synchronously.
    !>
    !> By default, this function creates the diff in the "*Diff*" buffer.
    !> If BUFFER is given, BUFFER is used instead.  This function gives the
    !> buffer used in RESULT.  Port of diff.el:diff-no-select.
    recursive subroutine diff_no_select(old_in, new_in, result, status, switches, no_async, buffer)
        type(diff_operand_type), intent(in) :: old_in, new_in
        type(handle_type), intent(out) :: result
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: switches
        logical, intent(in), optional :: no_async
        type(handle_type), intent(in), optional :: buffer
        type(diff_operand_type) :: old, new
        character(len=:), allocatable :: sw, old_alt, new_alt, command, thisdir, olddir
        type(string_type), allocatable :: arguments(:)
        type(handle_type) :: output_buffer, process
        type(value_type) :: saved, code
        type(status_type) :: st
        logical :: synchronize
        integer :: k

        status = status_type()
        old = old_in
        new = new_in

        if (.not. new%is_buffer) then
            call expand_file_name(new_in%file, new%file, status)
            if (.not. status%ok()) return
        end if

        if (.not. old%is_buffer) then
            call expand_file_name(old_in%file, old%file, status)
            if (.not. status%ok()) return
        end if

        ! If not specified, use default.
        sw = diff_switches
        if (present(switches)) sw = switches
        synchronize = .false.
        if (present(no_async)) synchronize = no_async

        if (present(buffer)) then
            result = buffer
        else
            call get_or_make_buffer("*Diff*", result, status)
            if (.not. status%ok()) return
        end if

        call diff_check_labels()
        call diff_file_local_copy(old, old_alt, status)
        if (status%ok()) call diff_file_local_copy(new, new_alt, status)
        if (.not. status%ok()) return
        command = diff_command
        if (len(sw) > 0) command = command//" "//sw

        if ((allocated(old_alt) .or. allocated(new_alt)) .and. diff_use_labels%value) then
            command = command//" "//shell_quote_argument("--label")//" "//shell_quote_argument(operand_label(old))// &
                      " "//shell_quote_argument("--label")//" "//shell_quote_argument(operand_label(new))
        end if

        if (allocated(old_alt)) then
            command = command//" "//shell_quote_argument(old_alt)
        else
            command = command//" "//shell_quote_argument(old%file)
        end if

        if (allocated(new_alt)) then
            command = command//" "//shell_quote_argument(new_alt)
        else
            command = command//" "//shell_quote_argument(new%file)
        end if

        thisdir = ""
        if (allocated(buffers(current)%directory)) thisdir = buffers(current)%directory
        output_buffer = current_buffer()
        call set_buffer(result, st)
        buffers(current)%read_only = .true.
        call symbol_value(value_type(name="inhibit_read_only"), saved, st)
        call set(value_type(name="inhibit_read_only"), value_type(.true.), st)
        call erase_buffer(st)
        call set(value_type(name="inhibit_read_only"), saved, st)
        call diff_mode(status)

        if (.not. status%ok()) then
            call set_buffer(output_buffer, st)
            return
        end if

        call remember_revert_arguments(old, new, sw, synchronize)
        call revert_buffer_function%set_local(diff_revert)
        buffers(current)%directory = thisdir
        call set_diff_default_directory(thisdir)
        call set(value_type(name="inhibit_read_only"), value_type(.true.), st)
        call insert(command//achar(10), st)
        call set(value_type(name="inhibit_read_only"), saved, st)
        if (.not. allocated(shell_file_name%value)) call initialize_callproc()
        ! (let ((default-directory temporary-file-directory)) ...)
        olddir = buffers(current)%directory
        buffers(current)%directory = temporary_file_directory()

        if (.not. synchronize) then
            call append_string(arguments, shell_file_name%value, shell_command_switch%value, command)
            call make_process("Diff", arguments, process, status, buffer=result)
            buffers(current)%directory = olddir

            if (status%ok()) then
                call set_process_filter(process, st, diff_process_filter)
                call set_process_sentinel(process, st, diff_process_sentinel)
                call remember_process(process, old_alt, new_alt)
            end if
        else
            ! Async processes aren't available.
            call set(value_type(name="inhibit_read_only"), value_type(.true.), st)
            call execute_process(shell_file_name%value, code, status, destination=value_type(.true.), &
                                 arguments=list(value_type(text=shell_command_switch%value), value_type(text=command)))
            buffers(current)%directory = olddir
            k = 2
            if (vtag(code) == V_INTEGER) k = int(xfixnum(code))

            if (allocated(old_alt) .and. allocated(new_alt)) then
                call diff_sentinel(k, old_alt, new_alt)
            else if (allocated(old_alt)) then
                call diff_sentinel(k, old_temporary_file=old_alt)
            else if (allocated(new_alt)) then
                call diff_sentinel(k, new_temporary_file=new_alt)
            else
                call diff_sentinel(k)
            end if

            call set(value_type(name="inhibit_read_only"), saved, st)
        end if

        call set_buffer(output_buffer, st)
    end subroutine diff_no_select

    !> The current buffer's `revert_buffer_function' arguments.  Not a port.
    subroutine remember_revert_arguments(old, new, switches, no_async)
        type(diff_operand_type), intent(in) :: old, new
        character(len=*), intent(in) :: switches
        logical, intent(in) :: no_async
        type(revert_arguments_type) :: entry
        integer :: k

        if (.not. allocated(revert_arguments)) allocate (revert_arguments(0))
        k = findloc(revert_arguments%serial, buffers(current)%serial, dim=1)

        if (k == 0) then
            revert_arguments = [revert_arguments, entry]
            k = size(revert_arguments)
        end if

        revert_arguments(k)%serial = buffers(current)%serial
        revert_arguments(k)%old = old
        revert_arguments(k)%new = new
        revert_arguments(k)%switches = switches
        revert_arguments(k)%no_async = no_async
    end subroutine remember_revert_arguments

    !> The `revert_buffer_function' of *Diff* buffers: run the diff
    !> again (the anonymous procedure of diff-no-select).  Port of the
    !> revert-buffer-function procedure of diff.el:diff-no-select.
    recursive subroutine diff_revert(ignore_auto, noconfirm, status)
        logical, intent(in) :: ignore_auto, noconfirm
        type(status_type), intent(out) :: status
        type(handle_type) :: result
        integer :: k

        status = status_type()
        if (ignore_auto .or. noconfirm) continue
        k = findloc(revert_arguments%serial, buffers(current)%serial, dim=1)
        if (k == 0) return

        associate (a => revert_arguments(k))
            call diff_no_select(a%old, a%new, result, status, a%switches, a%no_async, current_buffer())
        end associate
    end subroutine diff_revert

    !> Record the temporary files of diff process PROCESS.  Not a port.
    subroutine remember_process(process, old_temporary, new_temporary)
        type(handle_type), intent(in) :: process
        character(len=:), allocatable, intent(in) :: old_temporary, new_temporary
        type(diff_process_type) :: entry

        if (.not. allocated(diff_processes)) allocate (diff_processes(0))
        entry%process = process
        entry%old_temporary = ""
        entry%new_temporary = ""
        if (allocated(old_temporary)) entry%old_temporary = old_temporary
        if (allocated(new_temporary)) entry%new_temporary = new_temporary
        diff_processes = [diff_processes, entry]
    end subroutine remember_process

    !> The process filter of diff: insert the output at the process mark,
    !> point following it if it was there.  Port of
    !> diff.el:diff-process-filter.
    recursive subroutine diff_process_filter(process, string, status)
        type(handle_type), intent(in) :: process
        character(len=*), intent(in) :: string
        type(status_type), intent(out) :: status
        type(handle_type) :: output_buffer, pbuf, mark
        type(value_type) :: saved
        type(status_type) :: st
        integer(int64) :: marked_position, original_point
        logical :: moving

        status = status_type()
        pbuf = process_buffer(process)
        if (.not. is_buffer_live(pbuf)) return
        output_buffer = current_buffer()
        call set_buffer(pbuf, st)
        mark = process_mark(process)
        call marker_position(mark, marked_position, st)
        moving = point() == marked_position
        original_point = point()
        ! Insert the text, advancing the process marker.
        call goto_character(marked_position)
        call symbol_value(value_type(name="inhibit_read_only"), saved, st)
        call set(value_type(name="inhibit_read_only"), value_type(.true.), st)
        call insert(string, status)
        call set(value_type(name="inhibit_read_only"), saved, st)
        call set_marker(mark, point(), st)

        if (moving) then
            call goto_character(point())
        else
            if (original_point > marked_position) original_point = original_point + (point() - marked_position)
            call goto_character(original_point)
        end if

        call set_buffer(output_buffer, st)
    end subroutine diff_process_filter

    !> The process sentinel of diff: run `diff_sentinel' in the process
    !> buffer.  Port of the process sentinel procedure of
    !> diff.el:diff-no-select.
    recursive subroutine diff_process_sentinel(process, event, status)
        type(handle_type), intent(in) :: process
        character(len=*), intent(in) :: event
        type(status_type), intent(out) :: status
        type(handle_type) :: output_buffer, pbuf
        type(status_type) :: st
        integer :: k

        status = status_type()
        if (len(event) < 0) return
        pbuf = process_buffer(process)
        if (.not. is_buffer_live(pbuf)) return

        do k = 1, size(diff_processes)
            if (diff_processes(k)%process%index == process%index .and. &
                diff_processes(k)%process%generation == process%generation) exit
        end do

        output_buffer = current_buffer()
        call set_buffer(pbuf, st)

        if (k <= size(diff_processes)) then
            call diff_sentinel(process_exit_status(process), diff_processes(k)%old_temporary, diff_processes(k)%new_temporary)
            call drop_process(k)
        else
            call diff_sentinel(process_exit_status(process))
        end if

        call set_buffer(output_buffer, st)
    end subroutine diff_process_sentinel

    !> Forget entry K of `diff_processes' (copied element by element: an
    !> array constructor of this type reallocates to size 0 when it empties).
    !> Not a port.
    subroutine drop_process(k)
        integer, intent(in) :: k
        type(diff_process_type), allocatable :: rest(:)
        integer :: i, j

        allocate (rest(size(diff_processes) - 1))
        j = 0

        do i = 1, size(diff_processes)
            if (i == k) cycle
            j = j + 1
            rest(j) = diff_processes(i)
        end do

        call move_alloc(rest, diff_processes)
    end subroutine drop_process

    !> Diff this file with its backup file or vice versa.
    !> Uses the latest backup, if there are several numerical backups.
    !> If this file is a backup, diff it with its original.
    !> The backup file is the first file given to `diff'.
    !> With prefix arg SWITCHES, prompt for diff switches.  Port of
    !> diff.el:diff-backup.
    recursive subroutine diff_backup(file, status, switches)
        character(len=*), intent(in) :: file
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: switches
        type(diff_operand_type) :: bak, ori
        character(len=:), allocatable :: latest
        logical :: found

        status = status_type()

        if (is_backup_file_name(file)) then
            bak%file = file
            ori%file = file_name_sans_versions(file)
        else
            call diff_latest_backup_file(file, latest, found)

            if (.not. found) then
                call set_error(status, GENERIC_ERROR_CODE, "No backup found for "//file)
                return
            end if

            bak%file = latest
            ori%file = file
        end if

        call diff(bak, ori, status, switches)
    end subroutine diff_backup

    !> Return the latest existing backup of file FN, or FOUND .false. if there is none.
    !> Port of diff.el:diff-latest-backup-file (no file name handlers).
    subroutine diff_latest_backup_file(fn, backup, found)
        character(len=*), intent(in) :: fn
        character(len=:), allocatable, intent(out) :: backup
        logical, intent(out) :: found

        call file_newest_backup(fn, backup, found)
    end subroutine diff_latest_backup_file

    !> View the differences between BUFFER and its associated file.
    !> This requires the external program `diff' to be in your
    !> `exec_path'.  Port of diff.el:diff-buffer-with-file.
    recursive subroutine diff_buffer_with_file(status, buffer)
        type(status_type), intent(out) :: status
        type(handle_type), intent(in), optional :: buffer
        type(diff_operand_type) :: old, new
        type(handle_type) :: b
        integer(int64) :: bi

        b = current_buffer()
        if (present(buffer)) b = buffer
        call resolve_buffer(b, bi, status)
        if (.not. status%ok()) return

        if (.not. allocated(buffers(bi)%filename)) then
            call set_error(status, GENERIC_ERROR_CODE, "Buffer is not visiting a file")
            return
        end if

        old%file = buffers(bi)%filename
        new%is_buffer = .true.
        new%buffer = b
        call diff(old, new, status, no_async=.true.)
    end subroutine diff_buffer_with_file

    !> Find and display the differences between OLD and NEW buffers.
    !>
    !> When called interactively, read NEW, then OLD, using the
    !> minibuffer.  The default for NEW is the current buffer, and the
    !> default for OLD is the most recently selected other buffer.
    !> If NO_ASYNC is true, call diff synchronously.
    !>
    !> When called interactively with a prefix argument, prompt
    !> interactively for diff switches.  Otherwise, the switches specified
    !> in the variable `diff_switches' are passed to the diff command.
    !>
    !> Also see the `diff_entire_buffers' variable.  Port of
    !> diff.el:diff-buffers.
    recursive subroutine diff_buffers(old, new, status, switches, no_async)
        type(handle_type), intent(in) :: old, new
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: switches
        logical, intent(in), optional :: no_async
        type(diff_operand_type) :: o, n

        o%is_buffer = .true.
        o%buffer = old
        n%is_buffer = .true.
        n%buffer = new
        call diff(o, n, status, switches, no_async)
    end subroutine diff_buffers

    ! ------------------------------------------------------------------
    ! Commands
    ! ------------------------------------------------------------------

    recursive subroutine execute_diff(status)
        type(status_type), intent(out) :: status
        type(diff_operand_type) :: old, new
        character(len=:), allocatable :: newf, oldf, sw, backup
        logical :: given, found

        if (allocated(buffers(current)%filename)) then
            if (file_exists(buffers(current)%filename)) then
                call read_file_name("Diff new file (default "//file_name_nondirectory(buffers(current)%filename)//"): ", &
                                    newf, status, default_filename=buffers(current)%filename, mustmatch=REQUIRE_MATCH_YES)
            else
                call read_file_name("Diff new file: ", newf, status, mustmatch=REQUIRE_MATCH_YES)
            end if
        else
            call read_file_name("Diff new file: ", newf, status, mustmatch=REQUIRE_MATCH_YES)
        end if

        if (.not. status%ok()) return
        call file_newest_backup(newf, backup, found)
        if (found) found = file_exists(backup)

        if (found) then
            call read_file_name("Diff original file (default "//file_name_nondirectory(backup)//"): ", oldf, status, &
                                directory=file_name_directory(backup), default_filename=backup, mustmatch=REQUIRE_MATCH_YES)
        else
            call read_file_name("Diff original file: ", oldf, status, directory=file_name_directory(newf), &
                                mustmatch=REQUIRE_MATCH_YES)
        end if

        if (.not. status%ok()) return
        call read_diff_switches(sw, given, status)
        if (.not. status%ok()) return
        old%file = oldf
        new%file = newf

        if (given) then
            call diff(old, new, status, sw)
        else
            call diff(old, new, status)
        end if
    end subroutine execute_diff

    recursive subroutine execute_diff_backup(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: file, sw
        logical :: given

        call read_file_name("Diff (file with backup): ", file, status, mustmatch=REQUIRE_MATCH_YES)
        if (.not. status%ok()) return
        call read_diff_switches(sw, given, status)
        if (.not. status%ok()) return

        if (given) then
            call diff_backup(file, status, sw)
        else
            call diff_backup(file, status)
        end if
    end subroutine execute_diff_backup

    recursive subroutine execute_diff_buffer_with_file(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: name

        ! (interactive "bBuffer: ")
        call read_buffer("Buffer: ", name, status, def=buffers(current)%name, require_match=REQUIRE_MATCH_YES)
        if (.not. status%ok()) return
        call diff_buffer_with_file(status, get_buffer(name))
    end subroutine execute_diff_buffer_with_file

    recursive subroutine execute_diff_buffers(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: newb, oldb, sw, other
        integer(int64) :: bi
        type(handle_type) :: ob
        type(status_type) :: st
        logical :: given

        call read_buffer("Diff new buffer (default "//buffers(current)%name//"): ", newb, status, def=buffers(current)%name, &
                         require_match=REQUIRE_MATCH_YES)
        if (.not. status%ok()) return
        ob = other_buffer(current_buffer(), .true.)
        call resolve_buffer(ob, bi, st)
        other = ""
        if (st%ok()) other = buffers(bi)%name
        call read_buffer("Diff original buffer (default "//other//"): ", oldb, status, def=other, &
                         require_match=REQUIRE_MATCH_YES)
        if (.not. status%ok()) return
        call read_diff_switches(sw, given, status)
        if (.not. status%ok()) return

        if (given) then
            call diff_buffers(get_buffer(oldb), get_buffer(newb), status, sw)
        else
            call diff_buffers(get_buffer(oldb), get_buffer(newb), status)
        end if
    end subroutine execute_diff_buffers

    !> Register the commands of diff.el and give the settings their
    !> defaults.  Not a port.
    subroutine initialize_diff()
        commands%diff%execute => execute_diff
        commands%diff_backup%execute => execute_diff_backup
        commands%diff_buffer_with_file%execute => execute_diff_buffer_with_file
        commands%diff_buffers%execute => execute_diff_buffers
        if (.not. allocated(diff_switches)) diff_switches = "-u"
        if (.not. allocated(diff_command)) diff_command = "diff"
    end subroutine initialize_diff
end module diff_mod
