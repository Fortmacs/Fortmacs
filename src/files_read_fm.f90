! files_read_fm.f90 -- reading file names for files.el commands.
!
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

!> Part of files_fm: reading file names and `find_file_read_only'.  A
!> submodule can use minibuffer_fm even though it
!> depends on files_fm, which a module cannot.
submodule(files_fm) files_read_fm
    use modes_fm, only: minor_modes
    use minibuffer_fm, only: read_file_name, REQUIRE_MATCH_NIL, REQUIRE_MATCH_CONFIRM_AFTER_COMPLETION, &
                             read_file_name_completion_ignore_case, read_file_name_internal, completion_table_with_context
    use minibuf_fm, only: all_completions
    use minibuf_table_fm, only: completion_table_type, COMPLETE_BOUNDARIES
    use lisp_fm, only: list, make_list, list_length, list_elt, xstring, make_string
    use dired_fm, only: file_name_all_completions
    use regexp_opt_fm, only: regexp_opt, REGEXP_OPT_T
    use search_fm, only: string_match, match_beginning
    use fileio_fm, only: file_name_absolute_p
    use fns_fm, only: compare_strings, COMPARE_EQUAL
    implicit none(type, external)

contains

    module procedure locate_file_completion_table
    type(value_type), allocatable :: names(:), fullnames(:), items(:)
    type(value_type) :: files, matched
    type(completion_table_type) :: table
    character(len=:), allocatable :: sufre, string_dir, string_file, dir, file, sfx, x, first
    integer(int64) :: k, mb
    integer :: d, nn, nf, i
    type(status_type) :: st

    status = status_type()

    if (file_name_absolute_p(string)) then
        ! FIXME: maybe we should use completion-file-name-table instead,
        ! tho at least for `load', the arg is passed through
        ! substitute-in-file-name for historical reasons.
        call read_file_name_internal(string, action, result, status, pred, suffix)
        return
    end if

    if (action == COMPLETE_BOUNDARIES) then
        sfx = ""
        if (present(suffix)) sfx = suffix
        x = file_name_directory(sfx)

        if (len(x) > 0) then
            result = list(make_fixnum(int(len(file_name_directory(string)), int64)), make_fixnum(int(len(x) - 1, int64)))
        else
            result = list(make_fixnum(int(len(file_name_directory(string)), int64)), make_fixnum(int(len(sfx), int64)))
        end if

        return
    end if

    ! If we have files like "foo.el" and "foo.elc", we could load one of
    ! them with "foo.el", "foo.elc", or "foo", where just "foo" is the
    ! preferred way.  So if we list all 3, that gives a lot of redundant
    ! entries for the poor soul looking just for "foo".  OTOH, sometimes
    ! the user does want to pay attention to the extension.  We try to
    ! diffuse this tension by stripping the suffix, except when the
    ! result is a single element (i.e. usually we list only "foo" unless
    ! it's the only remaining element in the list, in which case we do
    ! list "foo", "foo.elc" and "foo.el").
    allocate (names(0), fullnames(0))
    sufre = regexp_opt(suffixes, REGEXP_OPT_T)//"\'"
    string_dir = file_name_directory(string)
    string_file = file_name_nondirectory(string)

    do d = 1, size(dirs)
        dir = trim(dirs(d))

        if (len(dir) == 0) then
            dir = "./"
            if (cur > 0 .and. allocated(buffers(cur)%directory)) dir = buffers(cur)%directory
        end if

        if (len(string_dir) > 0) then
            call expand_file_name(string_dir, x, st, dir)
            dir = x
        end if

        if (.not. file_directory_p(dir)) cycle
        call file_name_all_completions(string_file, dir, files, st)

        do k = 1, list_length(files)
            file = xstring(list_elt(files, k))

            if (string_match(sufre, file, st) <= 0) then
                call push(names, file)
            else
                call push(fullnames, file)
                mb = match_beginning(0, st)
                call push(names, file(1:mb - 1))
            end if
        end do
    end do

    ! Switching from names to names+fullnames creates a non-monotonicity
    ! which can cause problems with things like partial-completion.
    ! To minimize the problem, filter out completion-regexp-list, so that
    ! M-x load-library RET t/x.e TAB finds some files.  Also remove elements
    ! from `names' that matched `string' only when they still had
    ! their suffix.
    table%list = make_list(names)
    call all_completions(string_file, table, matched, st)
    nn = int(list_length(matched))
    deallocate (names)
    allocate (names(nn))

    do i = 1, nn
        names(i) = list_elt(matched, int(i, int64))
    end do

    ! Remove duplicates of the first element, so that we can easily check
    ! if `names' really contains only a single element.
    if (nn > 1) then
        first = xstring(names(1))
        allocate (items(1))
        items(1) = names(1)

        do i = 2, nn
            if (xstring(names(i)) == first .and. len(xstring(names(i))) == len(first)) cycle
            call append(items, names(i))
        end do

        call move_alloc(items, names)
    end if

    ! There's no more than one matching non-suffixed element, so expand
    ! the list by adding the suffixed elements as well.
    if (size(names) <= 1) then
        nf = size(fullnames)

        do i = 1, nf
            call append(names, fullnames(i))
        end do
    end if

    table%list = make_list(names)
    call completion_table_with_context(string_dir, table, string_file, action, result, status, pred, suffix)

contains

    ! (push NAME LIST)
    subroutine push(values, name)
        type(value_type), allocatable, intent(inout) :: values(:)
        character(len=*), intent(in) :: name
        type(value_type), allocatable :: grown(:)
        integer :: j

        allocate (grown(size(values) + 1))
        grown(1) = make_string(name)

        do j = 1, size(values)
            grown(j + 1) = values(j)
        end do

        call move_alloc(grown, values)
    end subroutine push

    ! VALUES with V added at the end.
    subroutine append(values, v)
        type(value_type), allocatable, intent(inout) :: values(:)
        type(value_type), intent(in) :: v
        type(value_type), allocatable :: grown(:)
        integer :: j

        allocate (grown(size(values) + 1))

        do j = 1, size(values)
            grown(j) = values(j)
        end do

        grown(size(values) + 1) = v
        call move_alloc(grown, values)
    end subroutine append
    end procedure locate_file_completion_table

    !> Convert FILENAME to be relative to DIRECTORY (default:
    !> `default_directory').
    !> This function returns a relative file name that is equivalent to
    !> FILENAME when used with that default directory as the default.
    !> If FILENAME is a relative file name, it will be interpreted as
    !> existing in `default_directory'.  Port of files.el:file-relative-name
    !> (no remote files; case is folded when
    !> `read_file_name_completion_ignore_case' is true).
    module procedure file_relative_name
    character(len=:), allocatable :: dir, file, file_dir, ancestor, base, rest
    type(status_type) :: st
    logical :: fold

    if (present(directory)) then
        base = directory
    else
        base = ""

        if (cur /= 0) then
            if (allocated(buffers(cur)%directory)) base = buffers(cur)%directory
        end if
    end if

    call expand_file_name(base, dir, st)
    dir = file_name_as_directory(dir)
    call expand_file_name(filename, file, st)
    fold = read_file_name_completion_ignore_case
    ancestor = "."
    file_dir = file_name_as_directory(file)

    do while (.not. (prefix_p(dir, file_dir) .or. prefix_p(dir, file)))
        dir = file_name_directory(dir(1:len(dir) - 1))

        if (ancestor == ".") then
            ancestor = ".."
        else
            ancestor = "../"//ancestor
        end if
    end do

    ! Now ancestor is empty, or .., or ../.., etc.
    if (prefix_p(dir, file)) then
        ! We matched within FILENAME's directory part.  Add the rest of
        ! FILENAME onto ANCESTOR.
        rest = file(len(dir) + 1:)

        if (ancestor == "." .and. len(rest) > 0) then
            ! But don't bother with ANCESTOR if it would give us `./'.
            rel = rest
        else
            rel = file_name_as_directory(ancestor)//rest
        end if
    else
        ! We matched FILENAME's directory equivalent.
        rel = ancestor
    end if

contains

    !> subr.el:string-prefix-p.
    logical function prefix_p(prefix, string)
        character(len=*), intent(in) :: prefix, string

        prefix_p = .false.
        if (len(string) < len(prefix)) return

        if (fold) then
            prefix_p = compare_strings(prefix, string(1:len(prefix)), ignore_case=.true.) == COMPARE_EQUAL
        else
            prefix_p = string(1:len(prefix)) == prefix
        end if
    end function prefix_p
    end procedure file_relative_name

    recursive module subroutine read_file_name_in_minibuffer(prompt, result, status, dir, &
                                                             default_filename, initial, confirm_nonexistent)
        character(len=*), intent(in) :: prompt
        character(len=:), allocatable, intent(out) :: result
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: dir, default_filename, initial
        logical, intent(in), optional :: confirm_nonexistent
        integer :: mustmatch

        mustmatch = REQUIRE_MATCH_NIL

        if (present(confirm_nonexistent)) then
            if (confirm_nonexistent) mustmatch = REQUIRE_MATCH_CONFIRM_AFTER_COMPLETION
        end if

        call read_file_name(prompt, result, status, dir, default_filename, mustmatch=mustmatch, initial=initial)
    end subroutine read_file_name_in_minibuffer

    !> Visit FILENAME with FUN (`find_file' or `find_file_other_window')
    !> and make its buffer read-only with `read_only_mode'.  Port of
    !> files.el:find-file--read-only (no wildcards).
    recursive subroutine find_file_read_only_1(other_window, filename, status)
        logical, intent(in) :: other_window
        character(len=*), intent(in) :: filename
        type(status_type), intent(out) :: status

        status = status_type()

        if (.not. file_exists_p(filename)) then
            call set_error(status, FM_GENERIC_ERROR, filename//" does not exist")
            return
        end if

        if (other_window) then
            call find_file_other_window(filename, status)
        else
            call find_file(filename, status)
        end if

        if (.not. status%ok()) return
        ! (with-current-buffer b (read-only-mode 1))
        call minor_modes%read_only_mode%enable(status)
    end subroutine find_file_read_only_1

    recursive module subroutine find_file_read_only(filename, status)
        character(len=*), intent(in) :: filename
        type(status_type), intent(out) :: status

        call find_file_read_only_1(.false., filename, status)
    end subroutine find_file_read_only

    recursive module subroutine find_file_read_only_other_window(filename, status)
        character(len=*), intent(in) :: filename
        type(status_type), intent(out) :: status

        call find_file_read_only_1(.true., filename, status)
    end subroutine find_file_read_only_other_window

    !> Edit file FILENAME but don't allow changes.
    !> Like \[find_file], but marks buffer as read-only.
    !> Use \[read_only_mode] to permit editing.  Port of
    !> files.el:find-file-read-only (interactive), with find-file-read-args.
    recursive subroutine cmd_find_file_read_only(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: filename, dir

        dir = "~/"
        if (allocated(buffers(cur)%directory)) dir = buffers(cur)%directory
        call read_file_name_in_minibuffer("Find file read-only: ", filename, status, default_filename=dir, &
                                          confirm_nonexistent=.true.)
        if (.not. status%ok()) return
        call find_file_read_only(filename, status)
    end subroutine cmd_find_file_read_only

    !> Edit file FILENAME in another window but don't allow changes.
    !> Like \[find_file_other_window], but marks buffer as read-only.
    !> Use \[read_only_mode] to permit editing.
    !> If this command needs to split the current window, it by default obeys
    !> the user options `split_height_threshold' and `split_width_threshold',
    !> when it decides whether to split the window horizontally or vertically.
    !> Port of files.el:find-file-read-only-other-window (interactive), with
    !> find-file-read-args.
    recursive subroutine cmd_find_file_read_only_other_window(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: filename, dir

        dir = "~/"
        if (allocated(buffers(cur)%directory)) dir = buffers(cur)%directory
        call read_file_name_in_minibuffer("Find file read-only other window: ", filename, status, &
                                          default_filename=dir, confirm_nonexistent=.true.)
        if (.not. status%ok()) return
        call find_file_read_only_other_window(filename, status)
    end subroutine cmd_find_file_read_only_other_window

    module subroutine syms_of_files_read(ctl_x_map, ctl_x_4_map)
        type(handle_type), intent(in) :: ctl_x_map
        type(handle_type), intent(in), optional :: ctl_x_4_map
        type(value_type) :: key(1)
        type(status_type) :: status

        commands%find_file_read_only%run => cmd_find_file_read_only
        commands%find_file_read_only_other_window%run => cmd_find_file_read_only_other_window
        ! files.el: (define-key ctl-x-map "\C-r" 'find-file-read-only)
        key(1) = make_fixnum(18_int64)
        call define_key(ctl_x_map, key, sym("find_file_read_only"), status)
        ! files.el: (define-key ctl-x-map "\C-q" 'read-only-mode)
        key(1) = make_fixnum(17_int64)
        call define_key(ctl_x_map, key, sym("read_only_mode"), status)

        if (present(ctl_x_4_map)) then
            ! files.el: (define-key ctl-x-4-map "r" 'find-file-read-only-other-window)
            key(1) = make_fixnum(int(iachar("r"), int64))
            call define_key(ctl_x_4_map, key, sym("find_file_read_only_other_window"), status)
        end if
    end subroutine syms_of_files_read
end submodule files_read_fm
