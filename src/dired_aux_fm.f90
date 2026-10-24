! dired_aux_fm.f90 -- less commonly used parts of Dired, a submodule of
! dired_mode_fm.
! Ported from GNU Emacs 31.1 lisp/dired-aux.el.
!
! Copyright (C) 1985-1986, 1992, 1994, 1998, 2000-2026 Free Software
! Foundation, Inc.
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

!> The parts of dired-aux.el Dired uses: creating directories (`+'),
!> redisplaying files (`l'), adding and updating file lines, and the
!> subdirectory functions (`dired_goto_subdir', `dired_kill_subdir',
!> `dired_mark_subdir_files', `dired_prev_subdir').  A submodule of
!> dired_mode_fm, whose procedures they use and which uses them.
!>
!> Deferred: the rest of dired-aux.el (copying, renaming, linking,
!> changing attributes, compressing, shell commands, diffs, isearch,
!> `dired_create_empty_file', inserting and hiding subdirectories,
!> `dired_do_kill_lines', `dired_query').
submodule(dired_mode_fm) dired_aux_fm
    implicit none(type, external)

contains

    !> Return the topmost nonexistent parent dir of FILENAME.
    !> FILENAME is a full file name.  Port of
    !> dired-aux.el:dired--find-topmost-parent-dir.
    function find_topmost_parent_dir(filename) result(new)
        character(len=*), intent(in) :: filename
        character(len=:), allocatable :: new, try
        try = filename
        new = ""
        do while (len(try) > 0)
            if (file_exists_p(try)) exit
            if (new == try .and. len(new) == len(try)) exit
            new = try
            try = directory_file_name(file_name_directory(try))
        end do
    end function find_topmost_parent_dir

    recursive module subroutine dired_create_directory(directory, status)
        character(len=*), intent(in) :: directory
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: expanded, new
        integer(int64) :: pos
        call expand_file_name(directory, expanded, status)
        if (.not. status%ok()) return
        expanded = directory_file_name(expanded)
        if (file_exists_p(expanded)) then
            call set_error(status, FM_GENERIC_ERROR, "Cannot create directory "//expanded//": file exists")
            return
        end if
        new = find_topmost_parent_dir(expanded)
        call make_directory(expanded, status, parents=.true.)
        if (.not. status%ok()) return
        if (len(new) > 0) then
            call dired_add_file(new, status)
            if (.not. status%ok()) return
            pos = dired_move_to_filename(status)
        end if
    end subroutine dired_create_directory

    !> Create a directory called DIRECTORY.
    !> Parent directories of DIRECTORY are created as needed.
    !> If DIRECTORY already exists, signal an error.  Port of
    !> dired-aux.el:dired-create-directory (interactive).
    recursive subroutine cmd_dired_create_directory(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: directory, dir
        dir = dired_current_directory(status)
        if (.not. status%ok()) return
        call read_file_name("Create directory: ", directory, status, dir=dir)
        if (.not. status%ok()) return
        call dired_create_directory(directory, status)
    end subroutine cmd_dired_create_directory

    recursive module subroutine dired_add_file(filename, status, marker_char)
        character(len=*), intent(in) :: filename
        type(status_type), intent(out) :: status
        character, intent(in), optional :: marker_char
        type(handle_type), allocatable :: bufs(:)
        type(handle_type) :: old
        type(status_type) :: st
        logical :: added
        integer :: k
        status = status_type()
        ! dired-fun-in-all-buffers
        bufs = dired_buffers_for_dir(file_name_directory(filename), file_name_nondirectory(filename))
        old = current_buffer()
        do k = 1, size(bufs)
            call set_buffer(bufs(k), st)
            call dired_add_entry(filename, added, status, marker_char)
            if (.not. status%ok()) exit
        end do
        call set_buffer(old, st)
    end subroutine dired_add_file

    recursive module subroutine dired_add_entry(filename, added, status, marker_char, relative)
        character(len=*), intent(in) :: filename
        logical, intent(out) :: added
        type(status_type), intent(out) :: status
        character, intent(in), optional :: marker_char
        logical, intent(in), optional :: relative
        character(len=:), allocatable :: file, cur_dir, directory, saved_dir, inserted_name, reason
        type(dired_string_type) :: list1(1)
        type(value_type) :: saved_ro, props
        type(status_type) :: st
        integer(int64) :: opoint, p, line_start, pt
        logical :: rel, found, changed
        character :: saved_marker
        status = status_type()
        added = .false.
        rel = .false.
        if (present(relative)) rel = relative
        ! Entry is always for files, even if they happen to also be
        ! directories.
        file = directory_file_name(filename)
        opoint = point()
        cur_dir = dired_current_directory(status)
        if (.not. status%ok()) return
        if (rel) then
            directory = cur_dir
            file = file_relative_name(file, directory)
        else
            directory = file_name_directory(file)
            file = file_name_nondirectory(file)
        end if
        reason = ""
        if (directory == cur_dir) then
            call end_of_line()
            ! (Hidden subdirectories are deferred.)
            ! We are already where we should be, except when point is
            ! before the subdir line or its total line.
            p = dired_after_subdir_garbage(cur_dir, status)
            if (.not. status%ok()) return
            if (point() < p) call goto_char(p)
        else if (dired_goto_subdir(directory) /= 0) then
            ! found - skip subdir and `total' line and uninteresting files
            ! like . and ..  This better not move into the next subdir!
            call dired_goto_next_nontrivial_file()
        else
            ! not found
            reason = "Subdir not found"
        end if
        if (len(reason) == 0) then
            call aux_inhibit_read_only_begin(saved_ro)
            call beginning_of_line()
            line_start = point()
            ! Don't expand `.'.  Show just the file name within directory.
            saved_dir = aux_default_directory()
            buffers(cur)%directory = directory
            list1(1)%s = file
            call dired_insert_directory(directory, dired_actual_switches_of()//" -d", status, file_list=list1)
            buffers(cur)%directory = saved_dir
            if (.not. status%ok()) then
                call aux_inhibit_read_only_end(saved_ro)
                return
            end if
            call goto_char(line_start)
            ! Put in desired marker char.
            if (present(marker_char)) then
                saved_marker = dired_marker_char
                dired_marker_char = marker_char
                call dired_mark_1(1_int64, .false., status)
                dired_marker_char = saved_marker
            end if
            ! Compensate for a bug in ange-ftp: it inserts the file's
            ! absolute name, rather than the relative one.
            call goto_char(line_start)
            call dired_get_filename(inserted_name, found, status, DIRED_LOCALP_VERBATIM)
            if (status%ok() .and. found) then
                if (len(file_name_directory(inserted_name)) > 0) then
                    call end_of_line()
                    call goto_char(point() - len_chars(inserted_name))
                    call text_properties_at(point(), props, st)
                    call delete_char(len_chars(inserted_name), st)
                    pt = point()
                    call insert(file, st)
                    call set_text_properties(pt, point(), props, changed, st)
                    call goto_char(point() + 1)
                else
                    call aux_fwd_line(1_int64)
                end if
            end if
            call aux_fwd_line(-1_int64)
            ! The subdir-alist is not affected, so we can run the hook right
            ! now (narrowed to the new line).
            call run_after_readin_on_line()
            p = dired_move_to_filename(st)
            call aux_inhibit_read_only_end(saved_ro)
            added = status%ok()
        end if
        ! don't move away on failure
        if (len(reason) > 0) call goto_char(opoint)
    contains

        integer(int64) function len_chars(s)
            character(len=*), intent(in) :: s
            integer :: j
            len_chars = 0
            do j = 1, len(s)
                if (iand(ichar(s(j:j)), 192) /= 128) len_chars = len_chars + 1
            end do
        end function len_chars
    end subroutine dired_add_entry

    !> Run `dired_after_readin_hook' with the buffer narrowed to the
    !> current line.
    subroutine run_after_readin_on_line()
        integer(int64) :: opoint, begv, zv
        type(status_type) :: st
        opoint = point()
        begv = buffers(cur)%begv
        zv = buffers(cur)%zv
        call beginning_of_line()
        call narrow_to_region(point(), aux_bol(2_int64), st)
        call dired_after_readin_hook%run(st)
        call widen()
        call narrow_to_region(begv, min(zv, point_max()), st)
        call goto_char(opoint)
    end subroutine run_after_readin_on_line

    module function dired_after_subdir_garbage(dir, status) result(pos)
        character(len=*), intent(in) :: dir
        type(status_type), intent(out) :: status
        integer(int64) :: pos
        integer(int64) :: opoint
        type(status_type) :: st
        status = status_type()
        opoint = point()
        pos = 0
        if (dired_goto_subdir(dir) == 0) then
            call set_error(status, FM_GENERIC_ERROR, "This cannot happen")
            call goto_char(opoint)
            return
        end if
        call aux_fwd_line(1_int64)
        ! don't cross subdir boundary
        do while (.not. eolp())
            if (dired_move_to_filename(st) /= 0) exit
            call aux_fwd_line(1_int64)
        end do
        pos = point()
        call goto_char(opoint)
    end function dired_after_subdir_garbage

    recursive module subroutine dired_do_redisplay(arg_kind, arg_n, test_for_subdir, status)
        integer, intent(in) :: arg_kind
        integer(int64), intent(in) :: arg_n
        logical, intent(in) :: test_for_subdir
        type(status_type), intent(out) :: status
        integer(int64) :: p
        logical :: found, upward
        type(status_type) :: st
        status = status_type()
        if (test_for_subdir .and. len(dired_get_subdir()) > 0) then
            call set_error(status, FM_GENERIC_ERROR, "Redisplaying a subdirectory (dired-insert-subdir) is not "// &
                           "supported yet")
            return
        end if
        ! message much faster than making dired-map-over-marks show
        ! progress
        call message("Redisplaying...")
        call dired_map_over_marks(redisplay_line, arg_kind, arg_n, status, found, upward)
        if (.not. status%ok()) return
        call dired_after_readin_hook%run(status)
        p = dired_move_to_filename(st)
        call message("Redisplaying...done")
    end subroutine dired_do_redisplay

    !> The body of `dired_do_redisplay' (postponing the readin hook till
    !> we map over all marked files).
    recursive subroutine redisplay_line(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: fname
        logical :: found
        call dired_get_filename(fname, found, status, no_error_if_not_filep=.true.)
        if (.not. status%ok()) return
        if (.not. found) then
            call set_error(status, FM_GENERIC_ERROR, "No file on this line")
            return
        end if
        call message("Redisplaying... "//fname)
        call dired_update_file_line(status, fname)
    end subroutine redisplay_line

    !> Redisplay all marked (or next ARG) files.
    !> If on a subdir line, redisplay that subdirectory.  In that case,
    !> a prefix arg lets you edit the `ls' switches used for the new listing.
    !>
    !> Dired remembers switches specified with a prefix arg, so that reverting
    !> the buffer will not reset them.  However, using `dired_undo' to re-insert
    !> or delete subdirectories can bypass this machinery.  Hence, you sometimes
    !> may have to reset some subdirectory switches after a `dired_undo'.
    !> Port of dired-aux.el:dired-do-redisplay (interactive; redisplaying
    !> a subdirectory is deferred).
    recursive subroutine cmd_dired_do_redisplay(status)
        type(status_type), intent(out) :: status
        integer :: kind
        integer(int64) :: n
        call aux_prefix_kind(kind, n)
        call dired_do_redisplay(kind, n, .true., status)
    end subroutine cmd_dired_do_redisplay

    recursive module subroutine dired_update_file_line(status, file)
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: file
        type(value_type) :: saved_ro
        integer(int64) :: opoint, p
        character :: c
        logical :: added, had_char
        type(status_type) :: st
        status = status_type()
        opoint = aux_bol()
        had_char = opoint < point_max()
        c = aux_char_at(opoint)
        ! (buffer-read-only nil)
        call aux_inhibit_read_only_begin(saved_ro)
        call goto_char(opoint)
        call aux_fwd_line(1_int64)
        call delete_region(opoint, point(), status)
        if (status%ok() .and. present(file)) then
            call dired_add_entry(file, added, status, relative=.true.)
            ! Replace space by old marker without moving point.
            if (status%ok() .and. had_char .and. c /= achar(0)) then
                call subst_char_in_region(opoint, opoint + 1, iachar(" "), iachar(c), status)
            end if
        end if
        call aux_inhibit_read_only_end(saved_ro)
        p = dired_move_to_filename(st)
    end subroutine dired_update_file_line

    module function dired_goto_subdir(dir) result(pos)
        character(len=*), intent(in) :: dir
        integer(int64) :: pos
        character(len=:), allocatable :: d
        integer :: k, i
        pos = 0
        d = file_name_as_directory(dir)
        k = aux_local_index()
        if (k == 0) return
        if (.not. allocated(locals(k)%subdir_alist)) return
        do i = 1, size(locals(k)%subdir_alist)
            if (locals(k)%subdir_alist(i)%dir == d) then
                call goto_char(aux_mpos(locals(k)%subdir_alist(i)%marker))
                ! dired-subdir-hidden-p and dired-add-entry depend on point
                ! being at \n after this function succeeds.
                call end_of_line()
                pos = point()
                return
            end if
        end do
    end function dired_goto_subdir

    recursive module subroutine dired_kill_subdir(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: cur_dir
        type(value_type) :: saved_ro
        type(dired_subdir_type), allocatable :: kept(:)
        integer(int64) :: beg, end
        logical :: modflag
        integer :: k, i, n
        beg = dired_subdir_min(status)
        if (.not. status%ok()) return
        end = dired_subdir_max()
        call buffer_modified_p(current_buffer(), modflag, status)
        if (.not. status%ok()) return
        cur_dir = dired_current_directory(status)
        if (.not. status%ok()) return
        if (cur_dir == aux_expand(aux_default_directory())) then
            call set_error(status, FM_GENERIC_ERROR, "Attempt to kill top level directory")
            return
        end if
        call aux_inhibit_read_only_begin(saved_ro)
        call delete_region(beg, end, status)
        ! don't leave final blank line
        if (status%ok() .and. eobp()) call delete_char(-1_int64, status)
        call aux_inhibit_read_only_end(saved_ro)
        ! dired-unsubdir
        k = aux_local_index()
        if (k > 0) then
            n = 0
            allocate (kept(size(locals(k)%subdir_alist)))
            do i = 1, size(locals(k)%subdir_alist)
                if (locals(k)%subdir_alist(i)%dir == cur_dir) then
                    call aux_release_marker(locals(k)%subdir_alist(i)%marker)
                    cur_dir = char(0)
                else
                    n = n + 1
                    kept(n) = locals(k)%subdir_alist(i)
                end if
            end do
            deallocate (locals(k)%subdir_alist)
            allocate (locals(k)%subdir_alist(n))
            locals(k)%subdir_alist(:) = kept(1:n)
        end if
        call restore_buffer_modified_p(modflag)
    end subroutine dired_kill_subdir

    !> Remove all lines of current subdirectory.
    !> Lower levels are unaffected.  Port of dired-aux.el:dired-kill-subdir
    !> (interactive).
    recursive subroutine cmd_dired_kill_subdir(status)
        type(status_type), intent(out) :: status
        call dired_kill_subdir(status)
    end subroutine cmd_dired_kill_subdir

    !> The start of the current subdirectory.  Port of
    !> dired.el:dired-subdir-min.
    integer(int64) function dired_subdir_min(status) result(pos)
        type(status_type), intent(out) :: status
        integer(int64) :: opoint
        opoint = point()
        pos = dired_prev_subdir(0_int64, status, .true., .true.)
        if (status%ok() .and. pos == 0) call set_error(status, FM_GENERIC_ERROR, "Not in a subdir!")
        call goto_char(opoint)
    end function dired_subdir_min

    recursive module subroutine dired_mark_subdir_files(status)
        type(status_type), intent(out) :: status
        integer(int64) :: p_min
        p_min = dired_subdir_min(status)
        if (.not. status%ok()) return
        call dired_mark_files_in_region(p_min, dired_subdir_max(), status)
    end subroutine dired_mark_subdir_files

    !> Mark all files except `.' and `..' in current subdirectory.
    !> If the Dired buffer shows multiple directories, this command
    !> marks the files listed in the subdirectory that point is in.  Port
    !> of dired-aux.el:dired-mark-subdir-files (interactive).
    recursive subroutine cmd_dired_mark_subdir_files(status)
        type(status_type), intent(out) :: status
        call dired_mark_subdir_files(status)
    end subroutine cmd_dired_mark_subdir_files

    module function dired_prev_subdir(arg, status, no_error_if_not_found, no_skip) result(pos)
        integer(int64), intent(in) :: arg
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: no_error_if_not_found, no_skip
        integer(int64) :: pos
        pos = dired_next_subdir(-arg, status, no_error_if_not_found, no_skip)
    end function dired_prev_subdir

    module subroutine syms_of_dired_aux()
        commands%dired_create_directory%run => cmd_dired_create_directory
        commands%dired_do_redisplay%run => cmd_dired_do_redisplay
        commands%dired_kill_subdir%run => cmd_dired_kill_subdir
        commands%dired_mark_subdir_files%run => cmd_dired_mark_subdir_files
    end subroutine syms_of_dired_aux
    ! ------------------------------------------------------------------
    ! Helpers.  Copies of dired_mode_fm's private ones: gfortran drops
    ! private parent procedures that submodules call.
    ! ------------------------------------------------------------------

    !> The position N lines after this one's start (line-beginning-position N).
    integer(int64) function aux_bol(n)
        integer(int64), intent(in), optional :: n
        aux_bol = line_beginning_position(n)
    end function aux_bol

    !> Move N lines (forward-line), ignoring the shortage.
    subroutine aux_fwd_line(n)
        integer(int64), intent(in) :: n
        integer(int64) :: shortage
        shortage = forward_line(n)
    end subroutine aux_fwd_line

    !> The character after POS (default point) as an ASCII character, or
    !> achar(0) when there is none or it is not ASCII.
    character function aux_char_at(pos)
        integer(int64), intent(in), optional :: pos
        integer(int32) :: c
        logical :: found
        if (present(pos)) then
            call char_after(c, found, pos)
        else
            call char_after(c, found)
        end if
        aux_char_at = achar(0)
        if (found .and. c >= 0 .and. c < 128) aux_char_at = achar(c)
    end function aux_char_at

    !> The position of marker M (0 if it points nowhere).
    integer(int64) function aux_mpos(m)
        type(handle_type), intent(in) :: m
        type(status_type) :: st
        call marker_position(m, aux_mpos, st)
        if (.not. st%ok()) aux_mpos = 0
    end function aux_mpos

    !> Free marker M, ignoring a stale handle.
    subroutine aux_release_marker(m)
        type(handle_type), intent(in) :: m
        type(status_type) :: st
        call free_marker(m, st)
    end subroutine aux_release_marker

    !> Bind `inhibit_read_only' to true (SAVED keeps the old value).
    subroutine aux_inhibit_read_only_begin(saved)
        type(value_type), intent(out) :: saved
        type(status_type) :: st
        saved = inhibit_read_only()
        call set(sym("inhibit_read_only"), t_value(), st)
    end subroutine aux_inhibit_read_only_begin

    !> Restore `inhibit_read_only' to SAVED.
    subroutine aux_inhibit_read_only_end(saved)
        type(value_type), intent(in) :: saved
        type(status_type) :: st
        call set(sym("inhibit_read_only"), saved, st)
    end subroutine aux_inhibit_read_only_end

    !> The current buffer's `default_directory'.
    function aux_default_directory() result(dir)
        character(len=:), allocatable :: dir
        dir = "./"
        if (cur == 0) return
        if (allocated(buffers(cur)%directory)) dir = buffers(cur)%directory
    end function aux_default_directory

    !> `expand_file_name' as a function (relative to `default_directory').
    function aux_expand(name) result(full)
        character(len=*), intent(in) :: name
        character(len=:), allocatable :: full
        type(status_type) :: st
        call expand_file_name(name, full, st)
        if (.not. st%ok()) full = name
    end function aux_expand

    !> The index of the current buffer's entry in `locals' (0 if none).
    integer function aux_local_index() result(k)
        integer :: i
        k = 0
        if (cur == 0) return
        if (.not. allocated(locals)) return
        do i = 1, size(locals)
            if (locals(i)%set .and. locals(i)%serial == buffers(cur)%serial) then
                k = i
                return
            end if
        end do
    end function aux_local_index

    !> The kind and number of the prefix argument ("P"): none, a number
    !> (or `-'), or another value (C-u).
    subroutine aux_prefix_kind(arg_kind, arg_n)
        integer, intent(out) :: arg_kind
        integer(int64), intent(out) :: arg_n
        arg_n = 0
        if (nilp(current_prefix_arg)) then
            arg_kind = DIRED_ARG_NIL
        else if (consp(current_prefix_arg)) then
            arg_kind = DIRED_ARG_OTHER
        else
            arg_kind = DIRED_ARG_INTEGER
            arg_n = prefix_numeric_value(current_prefix_arg)
        end if
    end subroutine aux_prefix_kind

end submodule dired_aux_fm
