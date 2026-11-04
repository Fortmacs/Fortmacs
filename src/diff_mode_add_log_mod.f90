! diff_mode_add_log_mod.f90 -- Diff mode's change log support, a submodule
! of diff_mode_mod.
! Ported from GNU Emacs 31.1 lisp/vc/diff-mode.el.
!
! Copyright (C) 1998-2026 Free Software Foundation, Inc.
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

!> The names of the subprograms a diff changes
!> (`diff_add_log_current_subprograms', `diff_current_subprogram') and
!> ChangeLog entries for every hunk (\[add_diff_change_log_entries_other_window]):
!> a submodule because it uses vc_mod (`vc_root_dir'), which uses
!> diff_mode_mod.
submodule(diff_mode_mod) diff_mode_add_log_mod
    use add_log_mod, only: add_log_current_subprogram, change_log_default_name, add_change_log_entry
    use string_type_mod, only: string_type
    use buffer_mod, only: get_or_make_buffer, close_buffer, generate_new_buffer_name, is_buffer_live
    use derived_mod, only: major_mode_type, major_mode_named
    use subr_mod, only: delay_mode_hooks_begin, delay_mode_hooks_end
    use expression_motion_mod, only: end_of_subprogram
    use files_mod, only: file_relative_name
    use vc_mod, only: vc_root_dir
    use xdisp_mod, only: message
    implicit none(type, external)

    !> The state of one subprogram context follower of
    !> `diff_add_log_current_subprograms' (upstream's make-defun-context-follower):
    !> where the current subprogram ends (0 for none), its name, and whether
    !> it follows the other text (goto-otherbuf) rather than the source
    !> buffer (gotobuf).
    type :: follower_type
        integer(int64) :: end = 0
        character(len=:), allocatable :: name
        logical :: other = .false.
    end type follower_type

contains

    module procedure diff_add_log_current_subprograms
    type(follower_type) :: same, removed, added
    type(diff_source_location_type) :: loc
    type(string_type), allocatable :: names(:)
    type(string_type), allocatable :: mismatches(:)
    type(handle_type) :: diff_buffer, other_buffer
    character(len=:), allocatable :: filename, root, def, joined
    type(status_type) :: st
    integer(int64) :: opoint, found, hunk_end, shortage, same_lines, added_lines, removed_lines
    integer(int32) :: patch_char
    logical :: has_file, has_root, has_char
    integer :: k

    status = status_type()
    allocate (entries(0), mismatches(0))
    diff_buffer = current_buffer()
    opoint = point()
    call goto_char(point_min())

    ! Might need to skip over file headers between diff
    ! hunks (e.g., "diff --git ..." etc).
    do
        call re_search_forward(diff_hunk_header_regexp, found, status, noerror=SEARCH_NOERROR)
        if (.not. status%ok() .or. found <= 0) exit
        found = point()
        hunk_end = diff_end_of_hunk()
        call goto_char(found)
        call diff_find_file_name(filename, has_file, status)
        if (.not. status%ok()) exit
        if (.not. has_file) filename = ""

        if (diff_add_log_use_relative_names) then
            call vc_root_dir(root, has_root, st)
            if (st%ok() .and. has_root) filename = file_relative_name(filename, root)
        end if

        ! Try to use the vc integration of
        ! `diff-find-source-location', unless it
        ! would look for non-existent files like
        ! /dev/null.
        call diff_hunk_file_names(names, .true.)
        has_file = .true.

        if (size(names) > 0) then
            has_file = names(1)%text /= "/dev/null" .or. len(names(1)%text) /= len("/dev/null")
        end if

        call diff_find_source_location(loc, status, has_file)
        if (.not. status%ok()) exit
        same_lines = 0
        added_lines = 0
        removed_lines = 0
        other_buffer = handle_type()
        same = follower_type()
        ! If APPLIED, we have NEW-TEXT in BUFFER, so we
        ! need to a buffer with OLD-TEXT to follow
        ! -lines.
        removed = follower_type(other=loc%switched)
        added = follower_type(other=.not. loc%switched)
        if (.not. loc%found .or. loc%line_offset /= 0) call append_mismatch(filename)
        ! Some modes always return nil for `add-log-current-defun',
        ! make sure at least the filename is included.
        k = entry_index(filename)

        do
            shortage = forward_line(1_int64)
            if (point() >= hunk_end) exit
            call char_after(patch_char, has_char)
            if (.not. has_char) patch_char = 0

            select case (patch_char)
            case (iachar("+"))
                added_lines = added_lines + 1
            case (iachar("-"))
                removed_lines = removed_lines + 1
            case (iachar(" "))
                same_lines = same_lines + 1
            case default
                continue
            end select

            call check_end(same)
            call check_end(added)
            call check_end(removed)

            if (patch_char == iachar(" ")) then
                ! Just updating context subprogram.
                call check_subprogram(same, def)
                if (allocated(def)) deallocate (def)
            else if (allocated(same%name)) then
                ! + or - in existing subprogram.
                def = same%name
            else if (patch_char == iachar("+")) then
                ! Check added or removed subprogram.
                call check_subprogram(added, def)
            else
                call check_subprogram(removed, def)
            end if

            call set_buffer(diff_buffer, st)
            if (allocated(def)) call append_subprogram(k, def)
            if (allocated(def)) deallocate (def)
        end do

        if (is_buffer_live(other_buffer)) call close_buffer(other_buffer, st)
        call set_buffer(diff_buffer, st)
    end do

    call set_buffer(diff_buffer, st)
    call goto_char(opoint)
    if (.not. status%ok()) return

    if (size(mismatches) > 0) then
        joined = mismatches(1)%text

        do k = 2, size(mismatches)
            joined = joined//", "//mismatches(k)%text
        end do

        call message("Diff didn't match for "//joined//".")
    end if

contains

    ! (cl-pushnew FILENAME hunk-mismatch-files :test #'equal).
    subroutine append_mismatch(file)
        character(len=*), intent(in) :: file
        type(string_type) :: item
        integer :: i

        do i = 1, size(mismatches)
            if (mismatches(i)%text == file .and. len(mismatches(i)%text) == len(file)) return
        end do

        item%text = file
        mismatches = [item, mismatches]
    end subroutine append_mismatch

    ! The index of FILE's entry in ENTRIES, made if there is none.
    integer function entry_index(file) result(i)
        character(len=*), intent(in) :: file
        type(change_log_entry_type) :: item

        do i = 1, size(entries)
            if (entries(i)%file == file .and. len(entries(i)%file) == len(file)) return
        end do

        item%file = file
        allocate (item%subprograms(0))
        entries = [entries, item]
        i = size(entries)
    end function entry_index

    ! (cl-pushnew DEF (alist-get FILENAME subprograms ...)), in the order
    ! the names are found.
    subroutine append_subprogram(i, name)
        integer, intent(in) :: i
        character(len=*), intent(in) :: name
        type(string_type) :: item
        integer :: j

        do j = 1, size(entries(i)%subprograms)
            if (entries(i)%subprograms(j)%text == name .and. len(entries(i)%subprograms(j)%text) == len(name)) return
        end do

        item%text = name
        entries(i)%subprograms = [entries(i)%subprograms, item]
    end subroutine append_subprogram

    ! Make the buffer FOLLOWER follows current at its line of the hunk
    ! (the goline functions gotobuf and goto-otherbuf).
    recursive subroutine go_line(follower)
        type(follower_type), intent(in) :: follower
        type(major_mode_type), pointer :: mode
        type(status_type) :: st
        integer(int64) :: n
        logical :: saved

        if (.not. follower%other) then
            call set_buffer(loc%buffer, st)
            call goto_char(loc%beginning)

            if (loc%switched) then
                n = same_lines - 1 + added_lines
            else
                n = same_lines - 1 + removed_lines
            end if

            shortage = forward_line(n)
            return
        end if

        if (is_buffer_live(other_buffer)) then
            call set_buffer(other_buffer, st)
        else
            call get_or_make_buffer(generate_new_buffer_name(" *diff-other-text*"), other_buffer, st)
            if (.not. st%ok()) return
            call set_buffer(other_buffer, st)

            if (loc%switched) then
                call insert(loc%src%text, st)
            else
                call insert(loc%dst%text, st)
            end if

            mode => null()

            if (is_buffer_live(loc%buffer)) then
                if (allocated(buffers(loc%buffer%index)%major_mode)) then
                    mode => major_mode_named(buffers(loc%buffer%index)%major_mode)
                end if
            end if

            if (associated(mode)) then
                call delay_mode_hooks_begin(saved)
                call mode%execute(st)
                call delay_mode_hooks_end(saved)
            end if
        end if

        call goto_char(point_min())

        if (loc%switched) then
            n = same_lines - 1 + removed_lines
        else
            n = same_lines - 1 + added_lines
        end if

        shortage = forward_line(n)
    end subroutine go_line

    ! Check for end of current subprogram.
    recursive subroutine check_end(follower)
        type(follower_type), intent(inout) :: follower

        if (follower%end == 0) return
        call go_line(follower)

        if (point() >= follower%end) then
            if (allocated(follower%name)) deallocate (follower%name)
            follower%end = 0
        end if

        call set_buffer(diff_buffer, st)
    end subroutine check_end

    ! Check for new subprogram: its name in NAME (unallocated for none).
    recursive subroutine check_subprogram(follower, name)
        type(follower_type), intent(inout) :: follower
        character(len=:), allocatable, intent(out) :: name
        type(status_type) :: st
        integer(int64) :: saved

        if (follower%end /= 0) return
        call go_line(follower)
        call add_log_current_subprogram(name)

        if (allocated(name)) then
            saved = point()
            call end_of_subprogram(1_int64, st)

            if (st%ok()) then
                follower%end = point()
            else
                follower%end = hunk_end
            end if

            call goto_char(saved)
            follower%name = name
        end if

        call set_buffer(diff_buffer, st)
    end subroutine check_subprogram
    end procedure diff_add_log_current_subprograms

    module procedure diff_current_subprogram
    type(diff_source_location_type) :: loc
    type(handle_type) :: diff_buffer, temp
    type(major_mode_type), pointer :: mode
    type(status_type) :: st
    integer(int64) :: opoint, found, shortage
    integer(int32) :: c
    logical :: has_c, done

    ! Kill change-log-default-name so it gets recomputed each time, since
    ! each hunk may belong to another file which may belong to another
    ! directory and hence have a different ChangeLog file.
    call change_log_default_name%clear_local()
    diff_buffer = current_buffer()
    opoint = point()

    if (looking_at(diff_hunk_header_regexp, st)) then
        shortage = forward_line(1_int64)
        call re_search_forward("^[^ ]", found, st, noerror=SEARCH_NOERROR)
    end if

    ! Use `noprompt' since this is used in which-function-mode
    ! and such.
    call diff_find_source_location(loc, st, noprompt=.true.)

    if (st%ok() .and. is_buffer_live(loc%buffer)) then
        call beginning_of_line()
        call char_after(c, has_c)
        done = .false.

        if (has_c .and. (c == iachar("<") .or. c == iachar("-"))) then
            ! Cursor is pointing at removed text.  This could be a removed
            ! function, in which case, going to the source buffer will
            ! not help since the function is now removed.  Instead,
            ! try to figure out the function name just from the
            ! code-fragment.
            call get_or_make_buffer(generate_new_buffer_name(" *temp*"), temp, st)
            if (st%ok()) call set_buffer(temp, st)

            if (st%ok()) then
                if (loc%switched) then
                    call insert(loc%dst%text, st)
                else
                    call insert(loc%src%text, st)
                end if

                mode => null()

                if (allocated(buffers(loc%buffer%index)%major_mode)) then
                    mode => major_mode_named(buffers(loc%buffer%index)%major_mode)
                end if

                if (associated(mode)) call mode%execute(st)

                if (loc%switched) then
                    call goto_char(point_min() + loc%dst%offset)
                else
                    call goto_char(point_min() + loc%src%offset)
                end if

                call add_log_current_subprogram(name)
                done = allocated(name)
                call close_buffer(temp, st)
            end if
        end if

        if (.not. done) then
            call set_buffer(loc%buffer, st)
            call goto_char(loc%beginning + loc%src%offset)
            call add_log_current_subprogram(name)
        end if
    end if

    call set_buffer(diff_buffer, st)
    call goto_char(opoint)
    end procedure diff_current_subprogram

    module procedure get_diff_buffer_file_name
    type(status_type) :: st
    logical :: found

    call diff_find_file_name(name, found, st, noprompt=.true.)

    if (.not. st%ok() .or. .not. found) then
        if (allocated(name)) deallocate (name)
    end if
    end procedure get_diff_buffer_file_name

    module procedure add_diff_change_log_entries_other_window
    type(handle_type) :: diff_buffer
    type(status_type) :: st, entry_status
    integer(int64) :: opoint, found

    ! XXX: Currently add-change-log-entry-other-window is only called
    ! once per hunk.  Some hunks have multiple changes, it would be
    ! good to call it for each change.
    status = status_type()
    diff_buffer = current_buffer()
    opoint = point()
    call goto_char(point_min())

    ! Call add-change-log-entry-other-window for each hunk in
    ! the diff buffer.
    do
        call diff_hunk_next(st)
        if (.not. st%ok()) exit
        ! Move to where the changes are,
        ! `add-change-log-entry-other-window' works better in
        ! that case.  If the hunk is a context hunk with an empty first
        ! half, recognize the "--- NNN,MMM ----" line and skip to the next
        ! non-context line.
        call re_search_forward(achar(10)//"[!+<>-]\(-- [0-9]+\(,[0-9]+\)? ----"//achar(10)//"\( .*"//achar(10)// &
                               "\)*[+]\)?", found, st, noerror=SEARCH_NOERROR)
        if (.not. st%ok() .or. found <= 0) exit
        found = point()
        ! FIXME: this pops up windows of all the buffers.
        call add_change_log_entry(entry_status, other_window=.true., put_new_entry_on_new_line=.true.)
        call set_buffer(diff_buffer, st)
        call goto_char(found)
        ! When there's no more hunks, diff-hunk-next signals an error.
        if (.not. entry_status%ok()) exit
    end do

    call set_buffer(diff_buffer, st)
    call goto_char(opoint)
    end procedure add_diff_change_log_entries_other_window
end submodule diff_mode_add_log_mod
