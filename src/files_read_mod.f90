! files_read_mod.f90 -- reading file names for files.el commands.
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

!> Part of files_mod: reading file names and `find_file_read_only'.  A
!> submodule can use minibuffer_mod even though it
!> depends on files_mod, which a module cannot.
submodule(files_mod) files_read_mod
    use modes_mod, only: minor_modes
    use minibuffer_mod, only: read_file_name, REQUIRE_MATCH_NO, REQUIRE_MATCH_CONFIRM_AFTER_COMPLETION, &
                              read_file_name_completion_ignore_case, read_file_name_internal, completion_table_with_context
    use minibuffer_reading_mod, only: all_completions
    use minibuffer_reading_tables_mod, only: completion_table_type, COMPLETE_BOUNDARIES
    use value_mod, only: list, make_list, list_length, list_element, xstring, value_type
    use directory_editor_mod, only: file_name_all_completions
    use regular_expression_optimization_mod, only: regular_expression_optimization, REGULAR_EXPRESSION_OPTIMIZATION_GROUP
    use search_mod, only: string_match, match_beginning
    use file_input_output_mod, only: is_file_name_absolute
    use utility_functions_mod, only: compare_strings, COMPARE_EQUAL
    implicit none(type, external)

    abstract interface
        !> A way to visit file FILENAME (`find_file' and its variants).
        !> Not a port.
        recursive subroutine find_file_procedure(filename, status)
            import :: status_type
            implicit none(type, external)
            character(len=*), intent(in) :: filename
            type(status_type), intent(out) :: status
        end subroutine find_file_procedure
    end interface

contains

    module procedure locate_file_completion_table
    type(value_type), allocatable :: names(:), fullnames(:), items(:)
    type(value_type) :: files, matched
    type(completion_table_type) :: table
    character(len=:), allocatable :: sufre, string_directory, string_file, directory, file, sfx, x, first
    integer(int64) :: k, mb
    integer :: d, nn, nf, i
    type(status_type) :: st

    status = status_type()

    if (is_file_name_absolute(string)) then
        ! FIXME: maybe we should use completion-file-name-table instead,
        ! tho at least for `load', the arg is passed through
        ! substitute-in-file-name for historical reasons.
        call read_file_name_internal(string, action, result, status, test, suffix)
        return
    end if

    if (action == COMPLETE_BOUNDARIES) then
        sfx = ""
        if (present(suffix)) sfx = suffix
        x = file_name_directory(sfx)

        if (len(x) > 0) then
            result = list(value_type(int(len(file_name_directory(string)), int64)), value_type(int(len(x) - 1, int64)))
        else
            result = list(value_type(int(len(file_name_directory(string)), int64)), value_type(int(len(sfx), int64)))
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
    sufre = regular_expression_optimization(suffixes, REGULAR_EXPRESSION_OPTIMIZATION_GROUP)//"\'"
    string_directory = file_name_directory(string)
    string_file = file_name_nondirectory(string)

    do d = 1, size(directories)
        directory = trim(directories(d))

        if (len(directory) == 0) then
            directory = "./"

            if (current > 0) then
                if (allocated(buffers(current)%directory)) directory = buffers(current)%directory
            end if
        end if

        if (len(string_directory) > 0) then
            call expand_file_name(string_directory, x, st, directory)
            directory = x
        end if

        if (.not. is_file_directory(directory)) cycle
        call file_name_all_completions(string_file, directory, files, st)

        do k = 1, list_length(files)
            file = xstring(list_element(files, k))

            if (string_match(sufre, file, st) <= 0) then
                call prepend(names, file)
            else
                call prepend(fullnames, file)
                mb = match_beginning(0, st)
                call prepend(names, file(1:mb - 1))
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
        names(i) = list_element(matched, int(i, int64))
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
    call completion_table_with_context(string_directory, table, string_file, action, result, status, test, suffix)

contains

    ! (push NAME LIST)
    subroutine prepend(values, name)
        type(value_type), allocatable, intent(inout) :: values(:)
        character(len=*), intent(in) :: name
        type(value_type) :: item

        item = value_type(text=name)
        values = [item, values]
    end subroutine prepend

    ! VALUES with V added at the end.
    subroutine append(values, v)
        type(value_type), allocatable, intent(inout) :: values(:)
        type(value_type), intent(in) :: v

        values = [values, v]
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
    character(len=:), allocatable :: directory_name, file, file_directory, ancestor, base, rest
    type(status_type) :: st
    logical :: fold

    if (present(directory)) then
        base = directory
    else
        base = ""

        if (current /= 0) then
            if (allocated(buffers(current)%directory)) base = buffers(current)%directory
        end if
    end if

    call expand_file_name(base, directory_name, st)
    directory_name = file_name_as_directory(directory_name)
    call expand_file_name(filename, file, st)
    fold = read_file_name_completion_ignore_case
    ancestor = "."
    file_directory = file_name_as_directory(file)

    do while (.not. (is_prefix(directory_name, file_directory) .or. is_prefix(directory_name, file)))
        directory_name = file_name_directory(directory_name(1:len(directory_name) - 1))

        if (ancestor == ".") then
            ancestor = ".."
        else
            ancestor = "../"//ancestor
        end if
    end do

    ! Now ancestor is empty, or .., or ../.., etc.
    if (is_prefix(directory_name, file)) then
        ! We matched within FILENAME's directory part.  Add the rest of
        ! FILENAME onto ANCESTOR.
        rest = file(len(directory_name) + 1:)

        if (ancestor == "." .and. len(rest) > 0) then
            ! But don't bother with ANCESTOR if it would give us `./'.
            relative = rest
        else
            relative = file_name_as_directory(ancestor)//rest
        end if
    else
        ! We matched FILENAME's directory equivalent.
        relative = ancestor
    end if

contains

    !> subr.el:string-prefix-p.
    logical function is_prefix(prefix, string)
        character(len=*), intent(in) :: prefix, string

        is_prefix = .false.
        if (len(string) < len(prefix)) return

        if (fold) then
            is_prefix = compare_strings(prefix, string(1:len(prefix)), ignore_case=.true.) == COMPARE_EQUAL
        else
            is_prefix = string(1:len(prefix)) == prefix
        end if
    end function is_prefix
    end procedure file_relative_name

    recursive module subroutine read_file_name_in_minibuffer(prompt, result, status, directory, &
                                                             default_filename, initial_input, confirm_nonexistent)
        character(len=*), intent(in) :: prompt
        character(len=:), allocatable, intent(out) :: result
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: directory, default_filename, initial_input
        logical, intent(in), optional :: confirm_nonexistent
        integer :: mustmatch

        mustmatch = REQUIRE_MATCH_NO

        if (present(confirm_nonexistent)) then
            if (confirm_nonexistent) mustmatch = REQUIRE_MATCH_CONFIRM_AFTER_COMPLETION
        end if

        call read_file_name(prompt, result, status, directory, default_filename, mustmatch=mustmatch, initial_input=initial_input)
    end subroutine read_file_name_in_minibuffer

    !> Visit FILENAME with FIND (`find_file', `find_file_other_window' or
    !> `find_file_other_frame') and make its buffer read-only with
    !> `read_only_mode'.  Port of files.el:find-file--read-only (no
    !> wildcards).
    recursive subroutine find_file_read_only_1(find, filename, status)
        procedure(find_file_procedure) :: find
        character(len=*), intent(in) :: filename
        type(status_type), intent(out) :: status

        status = status_type()

        if (.not. file_exists(filename)) then
            call set_error(status, GENERIC_ERROR_CODE, filename//" does not exist")
            return
        end if

        call find(filename, status)
        if (.not. status%ok()) return
        ! (with-current-buffer b (read-only-mode 1))
        call minor_modes%read_only_mode%enable(status)
    end subroutine find_file_read_only_1

    recursive module subroutine find_file_read_only(filename, status)
        character(len=*), intent(in) :: filename
        type(status_type), intent(out) :: status

        call find_file_read_only_1(find_file, filename, status)
    end subroutine find_file_read_only

    recursive module subroutine find_file_read_only_other_window(filename, status)
        character(len=*), intent(in) :: filename
        type(status_type), intent(out) :: status

        call find_file_read_only_1(find_file_other_window, filename, status)
    end subroutine find_file_read_only_other_window

    recursive module subroutine find_file_read_only_other_frame(filename, status)
        character(len=*), intent(in) :: filename
        type(status_type), intent(out) :: status

        call find_file_read_only_1(find_file_other_frame, filename, status)
    end subroutine find_file_read_only_other_frame

    !> Edit file FILENAME but don't allow changes.
    !> Like \[find_file], but marks buffer as read-only.
    !> Use \[read_only_mode] to permit editing.  Port of
    !> files.el:find-file-read-only (interactive), with find-file-read-args.
    recursive subroutine execute_find_file_read_only(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: filename, directory

        directory = "~/"
        if (allocated(buffers(current)%directory)) directory = buffers(current)%directory
        call read_file_name_in_minibuffer("Find file read-only: ", filename, status, default_filename=directory, &
                                          confirm_nonexistent=.true.)
        if (.not. status%ok()) return
        call find_file_read_only(filename, status)
    end subroutine execute_find_file_read_only

    !> Edit file FILENAME in another window but don't allow changes.
    !> Like \[find_file_other_window], but marks buffer as read-only.
    !> Use \[read_only_mode] to permit editing.
    !> If this command needs to split the current window, it by default obeys
    !> the user options `split_height_threshold' and `split_width_threshold',
    !> when it decides whether to split the window horizontally or vertically.
    !> Port of files.el:find-file-read-only-other-window (interactive), with
    !> find-file-read-args.
    recursive subroutine execute_find_file_read_only_other_window(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: filename, directory

        directory = "~/"
        if (allocated(buffers(current)%directory)) directory = buffers(current)%directory
        call read_file_name_in_minibuffer("Find file read-only other window: ", filename, status, &
                                          default_filename=directory, confirm_nonexistent=.true.)
        if (.not. status%ok()) return
        call find_file_read_only_other_window(filename, status)
    end subroutine execute_find_file_read_only_other_window

    !> Edit file FILENAME in another frame but don't allow changes.
    !> Like \[find_file_other_frame], but marks buffer as read-only.
    !> Use \[read_only_mode] to permit editing.  Port of
    !> files.el:find-file-read-only-other-frame (interactive), with
    !> find-file-read-args.
    recursive subroutine execute_find_file_read_only_other_frame(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: filename, directory

        directory = "~/"
        if (allocated(buffers(current)%directory)) directory = buffers(current)%directory
        call read_file_name_in_minibuffer("Find file read-only other frame: ", filename, status, &
                                          default_filename=directory, confirm_nonexistent=.true.)
        if (.not. status%ok()) return
        call find_file_read_only_other_frame(filename, status)
    end subroutine execute_find_file_read_only_other_frame

    module subroutine initialize_files_read(control_x_map, control_x_4_map)
        type(handle_type), intent(in) :: control_x_map
        type(handle_type), intent(in), optional :: control_x_4_map
        type(value_type) :: key(1)
        type(status_type) :: status

        commands%find_file_read_only%execute => execute_find_file_read_only
        commands%find_file_read_only_other_window%execute => execute_find_file_read_only_other_window
        commands%find_file_read_only_other_frame%execute => execute_find_file_read_only_other_frame
        ! files.el: (define-key ctl-x-map "\C-r" 'find-file-read-only)
        key(1) = value_type(18_int64)
        call define_key(control_x_map, key, value_type(name="find_file_read_only"), status)
        ! files.el: (define-key ctl-x-map "\C-q" 'read-only-mode)
        key(1) = value_type(17_int64)
        call define_key(control_x_map, key, value_type(name="read_only_mode"), status)

        if (present(control_x_4_map)) then
            ! files.el: (define-key ctl-x-4-map "r" 'find-file-read-only-other-window)
            key(1) = value_type(int(iachar("r"), int64))
            call define_key(control_x_4_map, key, value_type(name="find_file_read_only_other_window"), status)
        end if
    end subroutine initialize_files_read
end submodule files_read_mod
