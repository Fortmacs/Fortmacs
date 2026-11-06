! directory_editor_auxiliary_mod.f90 -- less commonly used parts of Dired, a submodule of
! directory_editor_mode_mod.
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

!> The less commonly used parts of Dired: creating directories and empty
!> files (`+'), redisplaying files (`l'), adding, updating and removing
!> file lines (`k'), the subdirectory functions (`directory_editor_goto_subdirectory',
!> `directory_editor_remove_subdirectory', `directory_editor_mark_subdirectory_files', `directory_editor_previous_subdirectory'),
!> and the operations on marked files: copying, renaming and linking (C,
!> R, H, S, Y and their `%' regexp forms, `% u', `% l') through
!> `directory_editor_create_files' with the overwrite query (`directory_editor_query'), changing
!> modes, owners, groups and times (M, O, G, T), compressing (Z, c),
!> viewing (v, dired.el's `directory_editor_view_file'), file types (y) and diffs (=, `directory_editor_backup_diff').  A
!> submodule of directory_editor_mode_mod, whose procedures they use and which uses
!> them; dired-aux.el's options are declared in directory_editor_mode_mod.
!>
!> Deferred: shell commands (`!', `&', `X', `directory_editor_do_shell_command',
!> `directory_editor_do_asynchronous_shell_command', the guessing of commands,
!> `directory_editor_do_open' and `E': the guessing of commands needs mailcap.el and
!> xdg.el, asynchronous shell commands are not ported), printing (`P'),
!> `directory_editor_compare_directories' (Lisp tests), searching
!> and replacing in marked files (`A', `Q', `directory_editor_do_search',
!> `directory_editor_do_query_replace_regular_expression', `directory_editor_do_incremental_search': xref and
!> fileloop), `directory_editor_clean_directory' (`.': numbered backups),
!> `directory_editor_backup_overwrite' (false), `directory_editor_version_control_rename_file' and
!> `directory_editor_version_control_next_action' (VERSION_CONTROL), HOW-TO functions of
!> `directory_editor_do_create_files' and `directory_editor_copy_how_to_fn' (archives),
!> inserting and hiding subdirectories (`i', `$', `M-$'),
!> `directory_editor_incremental_search_filenames', the further defaults of
!> `directory_editor_do_what_i_mean_target_defaults' (offered by M-n: `read_file_name' takes
!> one default) and a procedure as `directory_editor_do_what_i_mean_target', the error data in
!> the log of a failed file operation (its name and message are logged,
!> not the condition's data list), file name handlers.  Loading Lisp
!> files (`L', `B') is not applicable.
submodule(directory_editor_mode_mod) directory_editor_auxiliary_mod
    use value_mod, only: make_list, xfixnum, value_type, vtag, V_INTEGER, choice_type, assignment(=)
    use data_mod, only: choice_setting_type, choice_state, string_to_number
    use error_mod, only: FILE_ERROR_CODE, FILE_DATE_ERROR_CODE
    use system_dependent_mod, only: S_IFMT, S_IFDIR, S_IFLNK
    use time_functions_mod, only: format_date, date_format_type, time_type, TIMESPEC_COUNT_RATE, DATE_YEAR, DATE_MONTH, DATE_DAY, &
                                  DATE_HOUR, DATE_MINUTE, DATE_SECOND
    use buffer_mod, only: buffer_list, rename_buffer
    use file_input_output_mod, only: rename_file, copy_file, add_name_to_file, create_symbolic_link, file_symlink_target, &
                                     is_file_executable, file_modes, set_file_modes, is_directory_name
    use files_mod, only: copy_directory, is_file_in_directory, create_empty_file, file_modes_number_to_symbolic, &
                         file_modes_symbolic_to_number, set_visited_file_name
    use directory_editor_mod, only: system_users, system_groups, file_attribute_names
    use minibuffer_reading_mod, only: read_from_minibuffer, completion_list_table
    use minibuffer_mod, only: completing_read
    use basic_subprograms_mod, only: read_y_or_n, read_character_choice, shell_quote_argument
    use casefiddle_mod, only: capitalize_string, upcase_string, downcase_string
    use window_mod, only: pop_to_buffer, same_handle
    use view_mod, only: view_file
    use keyboard_mod, only: sit_for
    use print_mod, only: fortran_text
    use diff_mod, only: diff, diff_backup, diff_latest_backup_file, diff_operand_type, diff_switches
    use minibuffer_mod, only: REQUIRE_MATCH_YES
    use editing_functions_mod, only: mark_position, ngettext
    use buffer_mod, only: transient_mark_mode
    use files_mod, only: file_name_sans_versions
    use string_type_mod, only: string_type, append_string, prepend_string
    implicit none(type, external)

    !> File names `directory_editor_create_files' records as failing (and
    !> `directory_editor_copy_file_recursive' too).  Port of dired-aux.el
    !> dired-create-files-failures.
    type(string_type), allocatable :: create_files_failures(:)

    !> The results of the body of `directory_editor_map_over_marks_check'.
    type(string_type), allocatable :: check_results(:)

    !> The request and the answer of the readers that
    !> `directory_editor_mark_pop_up_body' runs.
    character(len=:), allocatable :: pop_prompt, pop_directory, pop_default, pop_result
    logical :: pop_has_default = .false., pop_used_default = .false., pop_answer = .false.
    type(value_type) :: pop_collection

    !> The target of `directory_editor_do_create_files' (upstream's fluid `target').
    character(len=:), allocatable :: create_target

    !> The state of `directory_editor_do_create_files_regular_expression''s name constructor
    !> (`rename_regular_expression_query' and the arguments it closes over).
    character(len=:), allocatable :: regular_expression_operation, regular_expression_regular_expression, &
                                     regular_expression_newname, regular_expression_help
    logical :: regular_expression_whole_name = .false.
    integer :: regular_expression_query = 0

    !> The state of `directory_editor_rename_non_directory''s name constructor
    !> (`rename_non_directory_query' and what it closes over).
    character(len=:), allocatable :: non_directory_operation, non_directory_help
    logical :: non_directory_upcase = .false.
    integer :: non_directory_query = 0

contains

    !> Return the topmost nonexistent parent dir of FILENAME.
    !> FILENAME is a full file name.  Port of
    !> dired-aux.el:dired--find-topmost-parent-dir.
    function find_topmost_parent_directory(filename) result(new)
        character(len=*), intent(in) :: filename
        character(len=:), allocatable :: new, try

        try = filename
        new = ""

        do while (len(try) > 0)
            if (file_exists(try)) exit
            if (new == try .and. len(new) == len(try)) exit
            new = try
            try = directory_file_name(file_name_directory(try))
        end do
    end function find_topmost_parent_directory

    recursive module subroutine directory_editor_create_directory(directory, status)
        character(len=*), intent(in) :: directory
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: expanded, new
        integer(int64) :: position

        call expand_file_name(directory, expanded, status)
        if (.not. status%ok()) return
        expanded = directory_file_name(expanded)

        if (file_exists(expanded)) then
            call set_error(status, GENERIC_ERROR_CODE, "Cannot create directory "//expanded//": file exists")
            return
        end if

        new = find_topmost_parent_directory(expanded)
        call create_directory(expanded, status, parents=.true.)
        if (.not. status%ok()) return

        if (len(new) > 0) then
            call directory_editor_add_file(new, status)
            if (.not. status%ok()) return
            position = directory_editor_move_to_filename(status)
        end if
    end subroutine directory_editor_create_directory

    !> Create a directory called DIRECTORY.
    !> Parent directories of DIRECTORY are created as needed.
    !> If DIRECTORY already exists, an error condition occurs.  Port of
    !> dired-aux.el:dired-create-directory (interactive).
    recursive subroutine execute_directory_editor_create_directory(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: directory, directory_name

        directory_name = directory_editor_current_directory(status)
        if (.not. status%ok()) return
        call read_file_name("Create directory: ", directory, status, directory=directory_name)
        if (.not. status%ok()) return
        call directory_editor_create_directory(directory, status)
    end subroutine execute_directory_editor_create_directory

    recursive module subroutine directory_editor_add_file(filename, status, marker_character)
        character(len=*), intent(in) :: filename
        type(status_type), intent(out) :: status
        character, intent(in), optional :: marker_character
        type(handle_type), allocatable :: buffers(:)
        type(handle_type) :: old
        type(status_type) :: st
        logical :: added
        integer :: k

        status = status_type()
        ! dired-fun-in-all-buffers
        buffers = directory_editor_buffers_for_directory(file_name_directory(filename), file_name_nondirectory(filename))
        old = current_buffer()

        do k = 1, size(buffers)
            call set_buffer(buffers(k), st)
            call directory_editor_add_entry(filename, added, status, marker_character)
            if (.not. status%ok()) exit
        end do

        call set_buffer(old, st)
    end subroutine directory_editor_add_file

    recursive module subroutine directory_editor_add_entry(filename, added, status, marker_character, relative)
        character(len=*), intent(in) :: filename
        logical, intent(out) :: added
        type(status_type), intent(out) :: status
        character, intent(in), optional :: marker_character
        logical, intent(in), optional :: relative
        character(len=:), allocatable :: file, current_directory, directory, saved_directory, inserted_name, reason
        type(string_type) :: list1(1)
        type(value_type) :: saved_ro, properties
        type(status_type) :: st
        integer(int64) :: original_point, p, line_start, point_value
        logical :: relative_value, found, changed
        character :: saved_marker

        status = status_type()
        added = .false.
        relative_value = .false.
        if (present(relative)) relative_value = relative
        ! Entry is always for files, even if they happen to also be
        ! directories.
        file = directory_file_name(filename)
        original_point = point()
        current_directory = directory_editor_current_directory(status)
        if (.not. status%ok()) return

        if (relative_value) then
            directory = current_directory
            file = file_relative_name(file, directory)
        else
            directory = file_name_directory(file)
            file = file_name_nondirectory(file)
        end if

        reason = ""

        if (directory == current_directory) then
            call end_of_line()
            ! (Hidden subdirectories are deferred.)
            ! We are already where we should be, except when point is
            ! before the subdir line or its total line.
            p = directory_editor_after_subdirectory_garbage(current_directory, status)
            if (.not. status%ok()) return
            if (point() < p) call goto_character(p)
        else if (directory_editor_goto_subdirectory(directory) /= 0) then
            ! found - skip subdir and `total' line and uninteresting files
            ! like . and ..  This better not move into the next subdir!
            call directory_editor_goto_next_nontrivial_file()
        else
            ! not found
            reason = "Subdir not found"
        end if

        if (len(reason) == 0) then
            call aux_inhibit_read_only_begin(saved_ro)
            call beginning_of_line()
            line_start = point()
            ! Don't expand `.'.  Show just the file name within directory.
            saved_directory = aux_default_directory()
            buffers(current)%directory = directory
            list1(1)%text = file
            call directory_editor_insert_directory(directory, directory_editor_actual_switches_of()//" -d", status, file_list=list1)
            buffers(current)%directory = saved_directory

            if (.not. status%ok()) then
                call aux_inhibit_read_only_end(saved_ro)
                return
            end if

            call goto_character(line_start)

            ! Put in desired marker char.
            if (present(marker_character)) then
                saved_marker = directory_editor_marker_character
                directory_editor_marker_character = marker_character
                call directory_editor_mark_1(1_int64, .false., status)
                directory_editor_marker_character = saved_marker
            end if

            ! Compensate for a bug in ange-ftp: it inserts the file's
            ! absolute name, rather than the relative one.
            call goto_character(line_start)
            call directory_editor_get_filename(inserted_name, found, status, DIRECTORY_EDITOR_LOCALP_VERBATIM)

            if (status%ok() .and. found) then
                if (len(file_name_directory(inserted_name)) > 0) then
                    call end_of_line()
                    call goto_character(point() - count_characters(inserted_name))
                    call text_properties_at(point(), properties, st)
                    call delete_character(count_characters(inserted_name), st)
                    point_value = point()
                    call insert(file, st)
                    call set_text_properties(point_value, point(), properties, changed, st)
                    call goto_character(point() + 1)
                else
                    call aux_fwd_line(1_int64)
                end if
            end if

            call aux_fwd_line(-1_int64)
            ! The subdir-alist is not affected, so we can run the hook right
            ! now (narrowed to the new line).
            call execute_after_readin_on_line()
            p = directory_editor_move_to_filename(st)
            call aux_inhibit_read_only_end(saved_ro)
            added = status%ok()
        end if

        ! don't move away on failure
        if (len(reason) > 0) call goto_character(original_point)
    end subroutine directory_editor_add_entry

    !> Execute `directory_editor_after_readin_hook' with the buffer narrowed to the
    !> current line.  Port of part of dired-aux.el:dired-do-redisplay.
    subroutine execute_after_readin_on_line()
        integer(int64) :: original_point, beginning_of_accessible_portion, end_of_accessible_portion
        type(status_type) :: st

        original_point = point()
        beginning_of_accessible_portion = buffers(current)%beginning_of_accessible_portion
        end_of_accessible_portion = buffers(current)%end_of_accessible_portion
        call beginning_of_line()
        call narrow_to_region(point(), aux_beginning_of_line(2_int64), st)
        call directory_editor_after_readin_hook%execute(st)
        call widen()
        call narrow_to_region(beginning_of_accessible_portion, min(end_of_accessible_portion, point_maximum()), st)
        call goto_character(original_point)
    end subroutine execute_after_readin_on_line

    module function directory_editor_after_subdirectory_garbage(directory, status) result(position)
        character(len=*), intent(in) :: directory
        type(status_type), intent(out) :: status
        integer(int64) :: position
        integer(int64) :: original_point
        type(status_type) :: st

        status = status_type()
        original_point = point()
        position = 0

        if (directory_editor_goto_subdirectory(directory) == 0) then
            call set_error(status, GENERIC_ERROR_CODE, "This cannot happen")
            call goto_character(original_point)
            return
        end if

        call aux_fwd_line(1_int64)

        ! don't cross subdir boundary
        do while (.not. is_end_of_line())
            if (directory_editor_move_to_filename(st) /= 0) exit
            call aux_fwd_line(1_int64)
        end do

        position = point()
        call goto_character(original_point)
    end function directory_editor_after_subdirectory_garbage

    recursive module subroutine directory_editor_do_redisplay(argument_kind, argument_n, test_for_subdirectory, status)
        integer, intent(in) :: argument_kind
        integer(int64), intent(in) :: argument_n
        logical, intent(in) :: test_for_subdirectory
        type(status_type), intent(out) :: status
        integer(int64) :: p
        logical :: found, upward
        type(status_type) :: st

        status = status_type()

        if (test_for_subdirectory .and. len(directory_editor_get_subdirectory()) > 0) then
            call set_error(status, GENERIC_ERROR_CODE, &
                           "Redisplaying a subdirectory (directory_editor_insert_subdirectory) is not "// &
                           "supported yet")
            return
        end if

        ! message much faster than making dired-map-over-marks show
        ! progress
        call message("Redisplaying...")
        call directory_editor_map_over_marks(redisplay_line, argument_kind, argument_n, status, found, upward)
        if (.not. status%ok()) return
        call directory_editor_after_readin_hook%execute(status)
        p = directory_editor_move_to_filename(st)
        call message("Redisplaying...done")
    end subroutine directory_editor_do_redisplay

    !> The body of `directory_editor_do_redisplay' (postponing the readin hook till
    !> we map over all marked files).
    !> Port of part of dired-aux.el:dired-do-redisplay.
    recursive subroutine redisplay_line(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: file_name
        logical :: found

        call directory_editor_get_filename(file_name, found, status, no_error_if_not_filep=.true.)
        if (.not. status%ok()) return

        if (.not. found) then
            call set_error(status, GENERIC_ERROR_CODE, "No file on this line")
            return
        end if

        call message("Redisplaying... "//file_name)
        call directory_editor_update_file_line(status, file_name)
    end subroutine redisplay_line

    !> Redisplay all marked (or next ARGUMENT) files.
    !> If on a subdir line, redisplay that subdirectory.  In that case,
    !> a prefix arg lets you edit the `ls' switches used for the new listing.
    !>
    !> Dired remembers switches specified with a prefix arg, so that reverting
    !> the buffer will not reset them.  However, using `directory_editor_undo' to re-insert
    !> or delete subdirectories can bypass this machinery.  Hence, you sometimes
    !> may have to reset some subdirectory switches after a `directory_editor_undo'.
    !> Port of dired-aux.el:dired-do-redisplay (interactive; redisplaying
    !> a subdirectory is deferred).
    recursive subroutine execute_directory_editor_do_redisplay(status)
        type(status_type), intent(out) :: status
        integer :: kind
        integer(int64) :: n

        call aux_prefix_kind(kind, n)
        call directory_editor_do_redisplay(kind, n, .true., status)
    end subroutine execute_directory_editor_do_redisplay

    recursive module subroutine directory_editor_update_file_line(status, file)
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: file
        type(value_type) :: saved_ro
        integer(int64) :: original_point, p
        character :: c
        logical :: added, had_character
        type(status_type) :: st

        status = status_type()
        original_point = aux_beginning_of_line()
        had_character = original_point < point_maximum()
        c = aux_character_at(original_point)
        ! (buffer-read-only nil)
        call aux_inhibit_read_only_begin(saved_ro)
        call goto_character(original_point)
        call aux_fwd_line(1_int64)
        call delete_region(original_point, point(), status)

        if (status%ok() .and. present(file)) then
            call directory_editor_add_entry(file, added, status, relative=.true.)

            ! Replace space by old marker without moving point.
            if (status%ok() .and. had_character .and. c /= achar(0)) then
                call substitute_character_in_region(original_point, original_point + 1, iachar(" "), iachar(c), status)
            end if
        end if

        call aux_inhibit_read_only_end(saved_ro)
        p = directory_editor_move_to_filename(st)
    end subroutine directory_editor_update_file_line

    module function directory_editor_goto_subdirectory(directory) result(position)
        character(len=*), intent(in) :: directory
        integer(int64) :: position
        character(len=:), allocatable :: d
        integer :: k, i

        position = 0
        d = file_name_as_directory(directory)
        k = aux_local_index()
        if (k == 0) return
        if (.not. allocated(locals(k)%subdirectory_table)) return

        do i = 1, size(locals(k)%subdirectory_table)
            if (locals(k)%subdirectory_table(i)%directory == d) then
                call goto_character(aux_marker_position(locals(k)%subdirectory_table(i)%marker))
                ! dired-subdir-hidden-p and dired-add-entry depend on point
                ! being at \n after this function succeeds.
                call end_of_line()
                position = point()
                return
            end if
        end do
    end function directory_editor_goto_subdirectory

    recursive module subroutine directory_editor_remove_subdirectory(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: current_directory
        type(value_type) :: saved_ro
        type(directory_editor_subdirectory_type), allocatable :: kept(:)
        integer(int64) :: beginning, end
        logical :: modflag
        integer :: k, i, n

        beginning = directory_editor_subdirectory_minimum(status)
        if (.not. status%ok()) return
        end = directory_editor_subdirectory_maximum()
        call is_buffer_modified(current_buffer(), modflag, status)
        if (.not. status%ok()) return
        current_directory = directory_editor_current_directory(status)
        if (.not. status%ok()) return

        if (current_directory == aux_expand(aux_default_directory())) then
            call set_error(status, GENERIC_ERROR_CODE, "Attempt to kill top level directory")
            return
        end if

        call aux_inhibit_read_only_begin(saved_ro)
        call delete_region(beginning, end, status)
        ! don't leave final blank line
        if (status%ok() .and. is_end_of_buffer()) call delete_character(-1_int64, status)
        call aux_inhibit_read_only_end(saved_ro)
        ! dired-unsubdir
        k = aux_local_index()

        if (k > 0) then
            n = 0
            allocate (kept(size(locals(k)%subdirectory_table)))

            do i = 1, size(locals(k)%subdirectory_table)
                if (locals(k)%subdirectory_table(i)%directory == current_directory) then
                    call aux_destroy_marker_if_live(locals(k)%subdirectory_table(i)%marker)
                    current_directory = char(0)
                else
                    n = n + 1
                    kept(n) = locals(k)%subdirectory_table(i)
                end if
            end do

            deallocate (locals(k)%subdirectory_table)
            allocate (locals(k)%subdirectory_table(n))
            locals(k)%subdirectory_table(:) = kept(1:n)
        end if

        call restore_buffer_modified(modflag)
    end subroutine directory_editor_remove_subdirectory

    !> Remove all lines of current subdirectory.
    !> Lower levels are unaffected.  Port of dired-aux.el:dired-kill-subdir
    !> (interactive).
    recursive subroutine execute_directory_editor_remove_subdirectory(status)
        type(status_type), intent(out) :: status

        call directory_editor_remove_subdirectory(status)
    end subroutine execute_directory_editor_remove_subdirectory

    !> The start of the current subdirectory.  Port of
    !> dired.el:dired-subdir-min.
    integer(int64) function directory_editor_subdirectory_minimum(status) result(position)
        type(status_type), intent(out) :: status
        integer(int64) :: original_point

        original_point = point()
        position = directory_editor_previous_subdirectory(0_int64, status, .true., .true.)
        if (status%ok() .and. position == 0) call set_error(status, GENERIC_ERROR_CODE, "Not in a subdir!")
        call goto_character(original_point)
    end function directory_editor_subdirectory_minimum

    recursive module subroutine directory_editor_mark_subdirectory_files(status)
        type(status_type), intent(out) :: status
        integer(int64) :: p_minimum

        p_minimum = directory_editor_subdirectory_minimum(status)
        if (.not. status%ok()) return
        call directory_editor_mark_files_in_region(p_minimum, directory_editor_subdirectory_maximum(), status)
    end subroutine directory_editor_mark_subdirectory_files

    !> Mark all files except `.' and `..' in current subdirectory.
    !> If the Dired buffer shows multiple directories, this command
    !> marks the files listed in the subdirectory that point is in.
    !> Port of dired-aux.el:dired-mark-subdir-files (interactive).
    recursive subroutine execute_directory_editor_mark_subdirectory_files(status)
        type(status_type), intent(out) :: status

        call directory_editor_mark_subdirectory_files(status)
    end subroutine execute_directory_editor_mark_subdirectory_files

    module function directory_editor_previous_subdirectory(argument, status, no_error_if_not_found, no_skip) result(position)
        integer(int64), intent(in) :: argument
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: no_error_if_not_found, no_skip
        integer(int64) :: position

        position = directory_editor_next_subdirectory(-argument, status, no_error_if_not_found, no_skip)
    end function directory_editor_previous_subdirectory

    module subroutine initialize_directory_editor_aux()
        commands%directory_editor_create_directory%execute => execute_directory_editor_create_directory
        commands%directory_editor_do_redisplay%execute => execute_directory_editor_do_redisplay
        commands%directory_editor_remove_subdirectory%execute => execute_directory_editor_remove_subdirectory
        commands%directory_editor_mark_subdirectory_files%execute => execute_directory_editor_mark_subdirectory_files
        commands%directory_editor_do_chmod%execute => execute_directory_editor_do_chmod
        commands%directory_editor_do_chgrp%execute => execute_directory_editor_do_chgrp
        commands%directory_editor_do_chown%execute => execute_directory_editor_do_chown
        commands%directory_editor_do_touch%execute => execute_directory_editor_do_touch
        commands%directory_editor_remove_line%execute => execute_directory_editor_remove_line
        commands%directory_editor_do_remove_lines%execute => execute_directory_editor_do_remove_lines
        commands%directory_editor_do_compress%execute => execute_directory_editor_do_compress
        commands%directory_editor_do_compress_to%execute => execute_directory_editor_do_compress_to
        commands%directory_editor_do_copy%execute => execute_directory_editor_do_copy
        commands%directory_editor_do_symlink%execute => execute_directory_editor_do_symlink
        commands%directory_editor_do_relsymlink%execute => execute_directory_editor_do_relsymlink
        commands%directory_editor_do_hardlink%execute => execute_directory_editor_do_hardlink
        commands%directory_editor_do_rename%execute => execute_directory_editor_do_rename
        commands%directory_editor_do_rename_regular_expression%execute => execute_directory_editor_do_rename_regular_expression
        commands%directory_editor_do_copy_regular_expression%execute => execute_directory_editor_do_copy_regular_expression
        commands%directory_editor_do_hardlink_regular_expression%execute => execute_directory_editor_do_hardlink_regular_expression
        commands%directory_editor_do_symlink_regular_expression%execute => execute_directory_editor_do_symlink_regular_expression
        commands%directory_editor_do_relsymlink_regular_expression%execute => &
            execute_directory_editor_do_relsymlink_regular_expression
        commands%directory_editor_upcase%execute => execute_directory_editor_upcase
        commands%directory_editor_downcase%execute => execute_directory_editor_downcase
        commands%directory_editor_create_empty_file%execute => execute_directory_editor_create_empty_file
        commands%directory_editor_view_file%execute => execute_directory_editor_view_file
        commands%directory_editor_show_file_type%execute => execute_directory_editor_show_file_type
        commands%directory_editor_diff%execute => execute_directory_editor_diff
        commands%directory_editor_backup_diff%execute => execute_directory_editor_backup_diff
        call initialize_directory_editor_aux_options()
    end subroutine initialize_directory_editor_aux

    ! ------------------------------------------------------------------
    ! Marked-files prompts
    ! ------------------------------------------------------------------

    !> Run BODY (which reads input) after showing which files are marked,
    !> as `directory_editor_mark_pop_up' does for its FUNCTION: the files FILES are
    !> listed in a window showing a buffer named BUFFER_NAME (" *Marked
    !> Files*" if empty), unless there is just one file (ONE_MARKED: a
    !> file that was marked, upstream's (t FILENAME), still shows it),
    !> `directory_editor_no_confirm' has `all' set, or OP_SYMBOL is among its
    !> `commands'.  Port of dired.el:dired-mark-pop-up.
    recursive subroutine directory_editor_mark_pop_up_body(buffer_name, op_symbol, files, one_marked, body, status)
        character(len=*), intent(in) :: buffer_name, op_symbol
        type(string_type), intent(in) :: files(:)
        logical, intent(in) :: one_marked
        interface
            recursive subroutine body(status)
                import :: status_type
                implicit none(type, external)
                type(status_type), intent(out) :: status
            end subroutine body
        end interface
        type(status_type), intent(out) :: status
        type(handle_type) :: buffer, window, owindow, old
        type(display_action_parameters_type) :: table
        type(status_type) :: st
        character(len=:), allocatable :: name
        integer :: saved_dedicated, nfiles
        logical :: quiet

        nfiles = size(files)
        if (one_marked) nfiles = 2
        quiet = directory_editor_no_confirm%all .or. nfiles == 1

        if (allocated(directory_editor_no_confirm%commands)) then
            if (any(directory_editor_no_confirm%commands == op_symbol)) quiet = .true.
        end if

        if (quiet) then
            call body(status)
            return
        end if

        name = buffer_name
        if (len(name) == 0) name = " *Marked Files*"
        old = current_buffer()
        call temporary_buffer_window_setup(name, buffer, status)
        if (.not. status%ok()) return
        ! Mark *Marked Files* window as softly-dedicated, to prevent other
        ! buffers e.g. *Completions* from reusing it (bug#17554).
        saved_dedicated = display_buffer_mark_dedicated
        display_buffer_mark_dedicated = DEDICATED_WEAK
        pop_up_files = files
        table%window_height%procedure => fit_window
        table%body_function => insert_marked_files
        call temporary_buffer_window_show(buffer, window, status, [DISPLAY_ACTION_BELOW_SELECTED], table)
        display_buffer_mark_dedicated = saved_dedicated
        call set_buffer(old, st)
        if (.not. status%ok()) return

        if (.not. is_window_live(window)) then
            call body(status)
            return
        end if

        owindow = selected_window()
        call select_window(window, st, norecord=.true.)
        call body(status)
        if (is_window_live(window)) call quit_restore_window(window, BK_CLOSE, st)
        if (is_window_live(owindow)) call select_window(owindow, st, norecord=.true.)
        if (is_buffer_live(old)) call set_buffer(old, st)
    end subroutine directory_editor_mark_pop_up_body

    !> The reading `directory_editor_mark_pop_up_body' runs: `read_file_name' with
    !> `pop_prompt', `pop_directory' and `pop_default'.  Not a port.
    recursive subroutine pop_read_file_name(status)
        type(status_type), intent(out) :: status

        if (pop_has_default) then
            call read_file_name(pop_prompt, pop_result, status, directory=pop_directory, default_filename=pop_default)
        else
            call read_file_name(pop_prompt, pop_result, status, directory=pop_directory)
        end if
    end subroutine pop_read_file_name

    !> `completing_read' with `pop_prompt', the collection `pop_collection'
    !> and `pop_default' (`pop_used_default' tells whether the default
    !> was returned for empty input).  Not a port.
    recursive subroutine pop_completing_read(status)
        type(status_type), intent(out) :: status

        if (pop_has_default) then
            call completing_read(pop_prompt, completion_list_table(pop_collection), pop_result, status, &
                                 default=pop_default, used_default=pop_used_default)
        else
            pop_used_default = .false.
            call completing_read(pop_prompt, completion_list_table(pop_collection), pop_result, status)
        end if
    end subroutine pop_completing_read

    !> `read_from_minibuffer' with `pop_prompt' and the default `pop_default'.
    !> Not a port.
    recursive subroutine pop_read_from_minibuffer(status)
        type(status_type), intent(out) :: status

        pop_used_default = .false.

        if (pop_has_default) then
            call read_from_minibuffer(pop_prompt, pop_result, status, default=pop_default)
        else
            call read_from_minibuffer(pop_prompt, pop_result, status)
        end if
    end subroutine pop_read_from_minibuffer

    !> `read_y_or_n' with `pop_prompt' (the answer in `pop_answer').
    !> Not a port.
    recursive subroutine pop_read_y_or_n(status)
        type(status_type), intent(out) :: status

        call read_y_or_n(pop_prompt, pop_answer, status)
    end subroutine pop_read_y_or_n

    !> Read args for a Dired marked-files command, prompting with
    !> PROMPT_BEGINNING, the marked files (`directory_editor_marker_prompt')
    !> and PROMPT_END (the text around the files in upstream's PROMPT).
    !> RESULT is the user input (a string).
    !>
    !> OP_SYMBOL is an operation name (see `directory_editor_no_confirm').
    !> ARGUMENT_KIND and ARGUMENT_N are normally the prefix argument for the calling
    !> command; they are passed to `directory_editor_marker_prompt'.
    !> FILES should be a list of marked files' names.
    !>
    !> DEFAULT_VALUE, if present, is a default value, passed to
    !> `completing_read' (or, for `touch', `read_from_minibuffer');
    !> USED_DEFAULT tells whether it was returned for empty input.
    !>
    !> COLLECTION, if present, is a list of possible completions.
    !> Port of dired-aux.el:dired-mark-read-string (no INITIAL).
    recursive subroutine directory_editor_mark_read_string(prompt_beginning, prompt_end, op_symbol, argument_kind, argument_n, &
                                                           files, result, &
                                                           used_default, &
                                                           status, default_value, collection)
        character(len=*), intent(in) :: prompt_beginning, prompt_end, op_symbol
        integer, intent(in) :: argument_kind
        integer(int64), intent(in) :: argument_n
        type(string_type), intent(in) :: files(:)
        character(len=:), allocatable, intent(out) :: result
        logical, intent(out) :: used_default
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: default_value
        type(value_type), intent(in), optional :: collection

        pop_prompt = prompt_beginning//directory_editor_marker_prompt(argument_kind, argument_n, files)//prompt_end
        pop_has_default = present(default_value)
        if (pop_has_default) pop_default = default_value

        if (present(collection)) then
            pop_collection = collection
        else
            pop_collection = value_type()
        end if

        if (op_symbol == "touch") then
            call directory_editor_mark_pop_up_body("", op_symbol, files, .false., pop_read_from_minibuffer, status)
        else
            call directory_editor_mark_pop_up_body("", op_symbol, files, .false., pop_completing_read, status)
        end if

        used_default = pop_used_default

        if (status%ok()) then
            result = pop_result
        else
            result = ""
        end if
    end subroutine directory_editor_mark_read_string

    !> Read a file name for a marked-files command, perhaps popping up the
    !> list of marked files.  ARGUMENT is the prefix arg and indicates whether
    !> the files came from marks (DIRECTORY_EDITOR_ARGUMENT_NONE) or a repeat factor.
    !> DEFAULT is the default value to return if the user just hits RET.
    !> The prompt is PROMPT_BEGINNING, the marked files and PROMPT_END
    !> (the text around the files in upstream's PROMPT).
    !> Port of dired-aux.el:dired-mark-read-file-name.
    recursive subroutine directory_editor_mark_read_file_name(prompt_beginning, prompt_end, directory, op_symbol, argument_kind, &
                                                              argument_n, files, &
                                                              result, status, &
                                                              default)
        character(len=*), intent(in) :: prompt_beginning, prompt_end, directory, op_symbol
        integer, intent(in) :: argument_kind
        integer(int64), intent(in) :: argument_n
        type(string_type), intent(in) :: files(:)
        character(len=:), allocatable, intent(out) :: result
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: default

        pop_prompt = prompt_beginning//directory_editor_marker_prompt(argument_kind, argument_n, files)//prompt_end
        pop_directory = directory
        pop_has_default = present(default)
        if (pop_has_default) pop_default = default
        call directory_editor_mark_pop_up_body("", op_symbol, files, .false., pop_read_file_name, status)

        if (status%ok()) then
            result = pop_result
        else
            result = ""
        end if
    end subroutine directory_editor_mark_read_file_name

    module procedure directory_editor_query
    integer :: chars(7), c
    character(len=:), allocatable :: full

    answer = .false.
    status = status_type()

    if (state == iachar("!")) then
        ! accept, and don't ask again
        answer = .true.
        return
    end if

    if (state == iachar("q") .or. state == 27) then
        ! skip, and don't ask again
        return
    end if

    ! no previous answer - ask now
    chars = [iachar("y"), iachar(" "), iachar("n"), 127, iachar("!"), iachar("q"), 27]

    if (present(help_procedure)) then
        full = prompt//" [Type yn!q or C-h] "
        call read_character_choice(full, chars, c, status, help_procedure=help_procedure)
    else
        full = prompt//" [Type y, n, q or !] "
        call read_character_choice(full, chars, c, status)
    end if

    if (.not. status%ok()) return
    state = c
    answer = c == iachar("y") .or. c == iachar(" ") .or. c == iachar("!")
    end procedure directory_editor_query

    !> Request confirmation from the user that the operation described
    !> by OP_SYMBOL is to be performed on the marked files.
    !> Confirmation consists in a y-or-n question with a file list
    !> pop-up unless OP_SYMBOL is in `directory_editor_no_confirm'.
    !> The files used are determined by ARGUMENT (as in `directory_editor_get_marked_files').
    !> Port of dired-aux.el:dired-mark-confirm.
    recursive subroutine directory_editor_mark_confirm(op_symbol, argument_kind, argument_n, confirmed, status)
        character(len=*), intent(in) :: op_symbol
        integer, intent(in) :: argument_kind
        integer(int64), intent(in) :: argument_n
        logical, intent(out) :: confirmed
        type(status_type), intent(out) :: status
        type(string_type), allocatable :: files(:)
        logical :: one_marked
        integer :: k

        status = status_type()
        confirmed = .true.
        if (directory_editor_no_confirm%all) return

        if (allocated(directory_editor_no_confirm%commands)) then
            do k = 1, size(directory_editor_no_confirm%commands)
                if (trim(directory_editor_no_confirm%commands(k)) == op_symbol) return
            end do
        end if

        ! Pass t for DISTINGUISH-ONE-MARKED so that a single file which
        ! is marked pops up a window.  That will help the user see
        ! it isn't the current line file.
        call directory_editor_get_marked_files(files, status, DIRECTORY_EDITOR_LOCALP_RELATIVE, argument_kind, argument_n, &
                                               distinguish_one_marked=.true., &
                                               one_marked=one_marked, error="")
        if (.not. status%ok()) return
        pop_prompt = operation_string(op_symbol)//" "//directory_editor_marker_prompt(argument_kind, argument_n, files)//"? "
        call directory_editor_mark_pop_up_body("", op_symbol, files, one_marked, pop_read_y_or_n, status)
        confirmed = status%ok() .and. pop_answer
    end subroutine directory_editor_mark_confirm

    !> What an operation is called in messages: "Compress or uncompress"
    !> for `compress', else the capitalized name.  Not a port.
    function operation_string(op_symbol) result(s)
        character(len=*), intent(in) :: op_symbol
        character(len=:), allocatable :: s

        if (op_symbol == "compress") then
            s = "Compress or uncompress"
        else
            s = capitalize_string(op_symbol)
        end if
    end function operation_string

    ! ------------------------------------------------------------------
    ! Change file attributes
    ! ------------------------------------------------------------------

    !> The program `directory_editor_check_process' runs, logging what it writes:
    !> MESSAGE is displayed, then PROGRAM runs with ARGUMENTS; FAILED tells
    !> whether it exited abnormally.  If it did, the command that invoked
    !> PROGRAM and the messages it emitted are saved in
    !> `directory_editor_log_buffer'.  Port of dired-aux.el:dired-check-process.
    recursive subroutine directory_editor_check_process(message, program, arguments, failed, status)
        character(len=*), intent(in) :: message, program
        type(string_type), intent(in) :: arguments(:)
        logical, intent(out) :: failed
        type(status_type), intent(out) :: status
        type(progress_reporter_type) :: reporter
        type(handle_type) :: old, err_buffer
        type(value_type) :: result, arglist
        type(value_type), allocatable :: elements(:)
        character(len=:), allocatable :: directory, output
        type(status_type) :: st
        integer :: k

        failed = .false.
        directory = aux_default_directory()
        call make_progress_reporter(reporter, message)
        old = current_buffer()
        ! Get a clean buffer for error output:
        call get_or_make_buffer(" *dired-check-process output*", err_buffer, status)
        if (.not. status%ok()) return
        call set_buffer(err_buffer, status)
        if (.not. status%ok()) return
        call erase_buffer(st)
        ! caller's default-directory
        buffers(current)%directory = directory
        allocate (elements(size(arguments)))

        do k = 1, size(arguments)
            elements(k) = value_type(text=arguments(k)%text)
        end do

        arglist = make_list(elements)
        call execute_process(program, result, status, destination=value_type(.true.), arguments=arglist)

        if (.not. status%ok()) then
            call set_buffer(old, st)
            return
        end if

        failed = .true.
        if (vtag(result) == V_INTEGER) failed = xfixnum(result) /= 0

        if (failed) then
            call buffer_substring(point_minimum(), point_maximum(), output, st)
            call set_buffer(old, st)
            call directory_editor_log(program//" "//fortran_text(arglist)//LF)
            call directory_editor_log(output)
        else
            call set_buffer(old, st)
            call close_buffer(err_buffer, st)
            call progress_reporter_done(reporter)
        end if
    end subroutine directory_editor_check_process

    !> Process all the files in FILES in batches of a convenient size, by
    !> running PROGRAM (with `directory_editor_check_process', with message MESSAGE) on
    !> ARGUMENTS and some of the files.  Batches are chosen to need less than
    !> MAXIMUM_CHARACTERS chars for the file names, allowing 3 extra characters of
    !> separator per file name.  FAILED tells whether a batch failed.
    !> Port of dired-aux.el:dired-bunch-files (for dired-check-process).
    recursive subroutine directory_editor_bunch_files(maximum_characters, message, program, arguments, files, failed, status)
        integer, intent(in) :: maximum_characters
        character(len=*), intent(in) :: message, program
        type(string_type), intent(in) :: arguments(:), files(:)
        logical, intent(out) :: failed
        type(status_type), intent(out) :: status
        integer :: first, k, pending_length, thislength
        logical :: f

        status = status_type()
        failed = .false.
        first = 1
        pending_length = 0

        do k = 1, size(files)
            thislength = len(files(k)%text) + 3

            ! If we have at least 1 pending file and this file won't fit
            ! in the length limit, process now.
            if (k > first .and. thislength + pending_length > maximum_characters) then
                call execute_batch(first, k - 1)
                if (.not. status%ok()) return
                first = k
                pending_length = 0
            end if

            pending_length = pending_length + thislength
        end do

        call execute_batch(first, size(files))

    contains

        recursive subroutine execute_batch(from, to)
            integer, intent(in) :: from, to
            type(string_type), allocatable :: argv(:)
            integer :: j

            allocate (argv(size(arguments) + max(0, to - from + 1)))

            do j = 1, size(arguments)
                argv(j)%text = arguments(j)%text
            end do

            do j = from, to
                argv(size(arguments) + j - from + 1)%text = files(j)%text
            end do

            call directory_editor_check_process(message, program, argv, f, status)
            if (f) failed = .true.
        end subroutine execute_batch
    end subroutine directory_editor_bunch_files

    !> Change file attributes (group, owner, timestamp) of marked files and
    !> refresh their file lines.
    !> ATTRIBUTE_NAME is a string describing the attribute to the user.
    !> PROGRAM is the program used to change the attribute.
    !> OP_SYMBOL is the type of operation (for use in `directory_editor_mark_pop_up').
    !> ARGUMENT describes which files to use, as in `directory_editor_get_marked_files'.
    !> Port of dired-aux.el:dired-do-chxxx.
    recursive subroutine directory_editor_do_chxxx(attribute_name, program, op_symbol, argument_kind, argument_n, status)
        character(len=*), intent(in) :: attribute_name, program, op_symbol
        integer, intent(in) :: argument_kind
        integer(int64), intent(in) :: argument_n
        type(status_type), intent(out) :: status
        type(string_type), allocatable :: files(:), arguments(:)
        character(len=:), allocatable :: default_file, default, prompt, new_attribute, operation, user, group
        type(value_type) :: collection
        type(file_status_type) :: attributes
        type(status_type) :: st
        logical :: found, has_default, used_default, failed
        integer :: n

        call directory_editor_get_marked_files(files, status, DIRECTORY_EDITOR_LOCALP_RELATIVE, argument_kind, argument_n, error="")
        if (.not. status%ok()) return
        ! The source of default file attributes is the file at point.
        call directory_editor_get_filename(default_file, found, st, DIRECTORY_EDITOR_LOCALP_RELATIVE, .true.)
        has_default = .false.

        if (st%ok() .and. found) then
            select case (op_symbol)
            case ("touch")
                call file_attributes(default_file, attributes, has_default, nofollow=.true.)

                if (has_default) then
                    default = format_date(date_format_type('(i4.4, 4i2.2, ".", i2.2)', [DATE_YEAR, DATE_MONTH, DATE_DAY, &
                                                                                        DATE_HOUR, DATE_MINUTE, DATE_SECOND]), &
                                          time_type(attributes%mtime_second, int(attributes%mtime_nsec, int64), &
                                                    TIMESPEC_COUNT_RATE), status=status)
                    if (.not. status%ok()) return
                end if
            case ("chown")
                call file_attribute_names(default_file, user, group, has_default)
                if (has_default) default = user
            case ("chgrp")
                call file_attribute_names(default_file, user, group, has_default)
                if (has_default) default = group
            case default
                continue
            end select
        end if

        ! (format-prompt "Change %s of %%s to" (when touch "now") ATTRIBUTE-NAME):
        ! the text after the marked files.
        if (op_symbol == "touch") then
            prompt = " to (default now): "
        else
            prompt = " to: "
        end if

        if (op_symbol == "chown") then
            call system_users(collection)
        else if (op_symbol == "chgrp") then
            call system_groups(collection)
        else
            collection = value_type()
        end if

        if (has_default) then
            call directory_editor_mark_read_string("Change "//attribute_name//" of ", prompt, op_symbol, argument_kind, &
                                                   argument_n, files, new_attribute, &
                                                   used_default, &
                                                   status, default_value=default, collection=collection)
        else
            call directory_editor_mark_read_string("Change "//attribute_name//" of ", prompt, op_symbol, argument_kind, &
                                                   argument_n, files, new_attribute, &
                                                   used_default, &
                                                   status, collection=collection)
        end if

        if (.not. status%ok()) return
        operation = program//" "//new_attribute
        n = 0
        allocate (arguments(3))

        ! Use `eq' instead of `equal' to detect empty input (bug#12399).
        if (.not. (len(new_attribute) == 0 .or. used_default)) then
            if (op_symbol == "touch") then
                arguments(1)%text = "-t"
                arguments(2)%text = new_attribute
                n = 2
            else
                arguments(1)%text = new_attribute
                n = 1
            end if
        end if

        ! (string-match-p "gnu" system-configuration): a GNU/Linux system.
        n = n + 1
        arguments(n)%text = "--"
        call directory_editor_bunch_files(10000, operation, program, arguments(1:n), files, failed, status)
        if (.not. status%ok()) return
        ! moves point if ARGUMENT is an integer
        call directory_editor_do_redisplay(argument_kind, argument_n, .false., status)
        if (.not. status%ok()) return
        if (failed) call directory_editor_log_summary(operation//": error", files(1:0))
    end subroutine directory_editor_do_chxxx

    module procedure directory_editor_do_chmod
    type(string_type), allocatable :: files(:)
    character(len=:), allocatable :: default_file, modestr, default, modes
    type(file_status_type) :: attributes
    type(status_type) :: st
    integer(int32) :: mode_count, mode
    integer(int64) :: octal
    logical :: found, has_default, used_default, has_number
    integer :: k

    call directory_editor_get_marked_files(files, status, DIRECTORY_EDITOR_LOCALP_RELATIVE, argument_kind, argument_n, error="")
    if (.not. status%ok()) return
    ! The source of default file attributes is the file at point.
    call directory_editor_get_filename(default_file, found, st, DIRECTORY_EDITOR_LOCALP_RELATIVE, .true.)
    has_default = .false.

    if (st%ok() .and. found) then
        call file_attributes(default_file, attributes, has_default, nofollow=.true.)

        if (has_default) then
            modestr = file_modes_number_to_symbolic(attributes%mode)
            default = "u="//strip_dashes(modestr(2:4))//",g="//strip_dashes(modestr(5:7))//",o="// &
                      strip_dashes(modestr(8:10))
        end if
    end if

    if (has_default) then
        call directory_editor_mark_read_string("Change mode of ", " to: ", "chmod", argument_kind, argument_n, files, modes, &
                                               used_default, &
                                               status, default_value=default)
    else
        call directory_editor_mark_read_string("Change mode of ", " to: ", "chmod", argument_kind, argument_n, files, modes, &
                                               used_default, &
                                               status)
    end if

    if (.not. status%ok()) return

    ! Use `eq' instead of `equal' to detect empty input (bug#12399).
    if (len(modes) == 0 .or. used_default) then
        ! We used to treat empty input as DEFAULT, but that is not such
        ! a good idea (Bug#9361).
        call set_error(status, GENERIC_ERROR_CODE, "No file mode specified")
        return
    end if

    has_number = string_match("^[0-7]+", modes, st) > 0

    if (has_number) then
        call string_to_number(modes, octal, 8)
        mode_count = int(octal)
    end if

    do k = 1, size(files)
        if (has_number) then
            mode = mode_count
        else
            call file_modes_symbolic_to_number(modes, mode, status, file_modes(files(k)%text, nofollow=.true.))
            if (.not. status%ok()) return
        end if

        call set_file_modes(files(k)%text, mode, status, nofollow=.true.)
        if (.not. status%ok()) return
    end do

    call directory_editor_do_redisplay(argument_kind, argument_n, .false., status)

contains

    function strip_dashes(s) result(t)
        character(len=*), intent(in) :: s
        character(len=:), allocatable :: t
        integer :: j

        t = ""

        do j = 1, len(s)
            if (s(j:j) /= "-") t = t//s(j:j)
        end do
    end function strip_dashes
    end procedure directory_editor_do_chmod

    !> Change the mode of the marked (or next ARGUMENT) files.
    !> Both octal numeric modes like `644' and symbolic modes like `g+w'
    !> are supported.
    !> After invoking the command, type
    !> \<minibuffer_local_completion_map>\[next_history_element] to pull
    !> the file attributes of the file at point into the minibuffer.
    !>
    !> See Info node `(coreutils)File permissions' for more information.
    !> Alternatively, see the man page for "chmod(1)".  Port of
    !> dired-aux.el:dired-do-chmod (interactive).
    recursive subroutine execute_directory_editor_do_chmod(status)
        type(status_type), intent(out) :: status
        integer :: kind
        integer(int64) :: n

        call aux_prefix_kind(kind, n)
        call directory_editor_do_chmod(kind, n, status)
    end subroutine execute_directory_editor_do_chmod

    module procedure directory_editor_do_chgrp
    call directory_editor_do_chxxx("Group", "chgrp", "chgrp", argument_kind, argument_n, status)
    end procedure directory_editor_do_chgrp

    !> Change the group of the marked (or next ARGUMENT) files.
    !> After invoking the command, type
    !> \<minibuffer_local_completion_map>\[next_history_element] to pull
    !> the file attributes of the file at point into the minibuffer.
    !> Port of dired-aux.el:dired-do-chgrp (interactive).
    recursive subroutine execute_directory_editor_do_chgrp(status)
        type(status_type), intent(out) :: status
        integer :: kind
        integer(int64) :: n

        call aux_prefix_kind(kind, n)
        call directory_editor_do_chgrp(kind, n, status)
    end subroutine execute_directory_editor_do_chgrp

    module procedure directory_editor_do_chown
    call directory_editor_do_chxxx("Owner", directory_editor_chown_program, "chown", argument_kind, argument_n, status)
    end procedure directory_editor_do_chown

    !> Change the owner of the marked (or next ARGUMENT) files.
    !> After invoking the command, type
    !> \<minibuffer_local_completion_map>\[next_history_element] to pull
    !> the file attributes of the file at point into the minibuffer.
    !> Port of dired-aux.el:dired-do-chown (interactive).
    recursive subroutine execute_directory_editor_do_chown(status)
        type(status_type), intent(out) :: status
        integer :: kind
        integer(int64) :: n

        call aux_prefix_kind(kind, n)
        call directory_editor_do_chown(kind, n, status)
    end subroutine execute_directory_editor_do_chown

    module procedure directory_editor_do_touch
    call directory_editor_do_chxxx("Timestamp", directory_editor_touch_program, "touch", argument_kind, argument_n, status)
    end procedure directory_editor_do_touch

    !> Change the timestamp of the marked (or next ARGUMENT) files.
    !> This calls touch.
    !> After invoking the command, type
    !> \<minibuffer_local_completion_map>\[next_history_element] to pull
    !> the file attributes of the file at point into the minibuffer.
    !> Port of dired-aux.el:dired-do-touch (interactive).
    recursive subroutine execute_directory_editor_do_touch(status)
        type(status_type), intent(out) :: status
        integer :: kind
        integer(int64) :: n

        call aux_prefix_kind(kind, n)
        call directory_editor_do_touch(kind, n, status)
    end subroutine execute_directory_editor_do_touch

    ! ------------------------------------------------------------------
    ! Commands that delete or redisplay part of the Dired buffer
    ! ------------------------------------------------------------------

    module procedure directory_editor_remove_line
    type(value_type) :: saved_ro
    character(len=:), allocatable :: file
    type(status_type) :: st
    integer(int64) :: n, b, p
    logical :: found

    status = status_type()
    n = argument
    call aux_inhibit_read_only_begin(saved_ro)

    do while (n /= 0)
        call directory_editor_get_filename(file, found, status, no_error_if_not_filep=.true.)
        if (.not. status%ok()) exit

        if (.not. found) then
            call set_error(status, GENERIC_ERROR_CODE, "Can only kill file lines")
            exit
        end if

        p = point()
        if (directory_editor_goto_subdirectory(file) /= 0) call directory_editor_remove_subdirectory(st)
        call goto_character(min(p, point_maximum()))
        b = aux_beginning_of_line()
        call delete_region(b, aux_beginning_of_line(2_int64), status)
        if (.not. status%ok()) exit

        if (n > 0) then
            n = n - 1
        else
            n = n + 1
            call aux_fwd_line(-1_int64)
        end if
    end do

    call aux_inhibit_read_only_end(saved_ro)
    p = directory_editor_move_to_filename(st)
    end procedure directory_editor_remove_line

    !> Kill the current line (not the files).
    !> With a prefix argument, kill that many lines starting with the current line.
    !> (A negative argument kills backward.)  Port of
    !> dired-aux.el:dired-kill-line (interactive).
    recursive subroutine execute_directory_editor_remove_line(status)
        type(status_type), intent(out) :: status

        call directory_editor_remove_line(prefix_numeric_value(current_prefix_argument), status)
    end subroutine execute_directory_editor_remove_line

    module procedure directory_editor_do_remove_lines
    type(value_type) :: saved_ro
    character(len=40) :: text
    integer(int64) :: original_point, b, n

    status = status_type()
    n = 0

    if (argument_kind /= DIRECTORY_EDITOR_ARGUMENT_NONE) then
        if (len(directory_editor_get_subdirectory()) > 0) then
            call directory_editor_remove_subdirectory(status)
        else
            call directory_editor_remove_line(argument_n, status)
        end if

        if (present(count)) count = 0
        return
    end if

    original_point = point()
    call goto_character(point_minimum())
    if (present(initialize_count)) n = initialize_count
    call aux_inhibit_read_only_begin(saved_ro)

    do while (.not. is_end_of_buffer())
        if (.not. aux_regular_expression_search_forward("^"//regular_expression_quote(directory_editor_marker_character))) exit
        n = n + 1
        b = aux_beginning_of_line()
        call delete_region(b, aux_beginning_of_line(2_int64), status)
        if (.not. status%ok()) exit
    end do

    call aux_inhibit_read_only_end(saved_ro)
    call goto_character(min(original_point, point_maximum()))
    if (present(count)) count = n

    if (present(quiet)) then
        if (quiet) return
    end if

    if (n == 1) then
        write (text, '("Killed ", i0, " line.")') n
    else
        write (text, '("Killed ", i0, " lines.")') n
    end if

    call message(trim(text))
    end procedure directory_editor_do_remove_lines

    !> Remove all marked lines, or the next ARGUMENT lines.
    !> The files or directories on those lines are _not_ deleted.  Only the
    !> Dired listing is affected.  To restore the removals, use \[revert_buffer].
    !>
    !> With a numeric prefix arg, remove that many lines going forward,
    !> starting with the current line.  (A negative prefix arg removes lines
    !> going backward.)
    !>
    !> If you use a prefix arg to remove the line for a subdir whose listing
    !> you have inserted into the Dired buffer, then that subdir listing is
    !> also removed.
    !>
    !> To remove a subdir listing _without_ removing the subdir's line in its
    !> parent listing, go to the header line of the subdir listing and use
    !> this command with any prefix arg.  Port of
    !> dired-aux.el:dired-do-kill-lines (interactive).
    recursive subroutine execute_directory_editor_do_remove_lines(status)
        type(status_type), intent(out) :: status

        if (current_prefix_argument%kind == PREFIX_NONE) then
            call directory_editor_do_remove_lines(DIRECTORY_EDITOR_ARGUMENT_NONE, 0_int64, status)
        else
            call directory_editor_do_remove_lines(DIRECTORY_EDITOR_ARGUMENT_INTEGER, &
                                                  prefix_numeric_value(current_prefix_argument), status)
        end if
    end subroutine execute_directory_editor_do_remove_lines

    ! ------------------------------------------------------------------
    ! Compression
    ! ------------------------------------------------------------------

    !> Run COMMAND (with the shell), and check for output.
    !> On error, pop up the log buffer.
    !> RES is the result of `process_file': zero for success (or a signal
    !> description).  Port of dired-aux.el:dired-shell-command.
    recursive subroutine directory_editor_shell_command(command, res, status)
        character(len=*), intent(in) :: command
        type(value_type), intent(out) :: res
        type(status_type), intent(out) :: status
        type(handle_type) :: old, out_buffer
        type(value_type) :: arguments
        character(len=:), allocatable :: directory
        type(status_type) :: st
        logical :: zero

        directory = aux_default_directory()
        old = current_buffer()
        call get_or_make_buffer(" *dired-check-process output*", out_buffer, status)
        if (.not. status%ok()) return
        call set_buffer(out_buffer, status)
        if (.not. status%ok()) return
        call erase_buffer(st)
        buffers(current)%directory = directory
        arguments = list(value_type(text="-c"), value_type(text=command))
        call execute_process(aux_shell_file_name(), res, status, destination=value_type(.true.), arguments=arguments)
        call set_buffer(old, st)
        if (.not. status%ok()) return
        zero = .false.
        if (vtag(res) == V_INTEGER) zero = xfixnum(res) == 0
        if (.not. zero) call pop_to_buffer(out_buffer, st)
    end subroutine directory_editor_shell_command

    !> The shell `shell_command' uses: $SHELL, else "/bin/sh" (callproc.c
    !> shell-file-name).  Not a port.
    function aux_shell_file_name() result(shell)
        character(len=:), allocatable :: shell
        integer :: length, stat

        call get_environment_variable("SHELL", length=length, status=stat)

        if (stat == 0 .and. length > 0) then
            allocate (character(len=length) :: shell)
            call get_environment_variable("SHELL", shell)
        else
            shell = "/bin/sh"
        end if
    end function aux_shell_file_name

    !> COMMAND with every %i replaced by I and every %o by O, literally
    !> (the replace-regexp-in-string calls of dired-compress-file).
    !> Not a port.
    function replace_input_output(command, i, o) result(s)
        character(len=*), intent(in) :: command, i, o
        character(len=:), allocatable :: s, t

        t = replace_all(command, "%i", i)
        s = replace_all(t, "%o", o)
    end function replace_input_output

    !> TEXT with every occurrence of FROM replaced by TO, left to right.
    !> Not a port.
    function replace_all(text, from, to) result(s)
        character(len=*), intent(in) :: text, from, to
        character(len=:), allocatable :: s
        integer :: k, start

        s = ""
        start = 1

        do
            k = index(text(start:), from)
            if (k == 0) exit
            s = s//text(start:start + k - 2)//to
            start = start + k - 1 + len(from)
        end do

        s = s//text(start:)
    end function replace_all

    module procedure directory_editor_compress_file
    character(len=:), allocatable :: command, suffix, out_name, directory_saved, message
    type(string_type), allocatable :: arguments(:)
    type(value_type) :: res
    type(status_type) :: st
    integer(int64) :: mb
    integer :: k, rule, match
    logical :: saved_fold, has_newname, failed, answer, matched

    found = .false.
    newname = ""
    status = status_type()
    ! See if any suffix rule matches this file name.
    saved_fold = case_fold_search%value
    case_fold_search%value = .false.
    rule = 0

    do k = 1, size(directory_editor_compress_file_suffixes)
        if (string_match(directory_editor_compress_file_suffixes(k)%regular_expression, file, st) > 0) then
            rule = k
            exit
        end if
    end do

    has_newname = .false.

    if (rule > 0) then
        mb = match_beginning(0, st)
        newname = file(1:string_index_to_offset(file, mb))//directory_editor_compress_file_suffixes(rule)%new_suffix
        has_newname = .true.
    end if

    case_fold_search%value = saved_fold
    if (is_file_symlink(file)) return

    if (rule > 0) then
        if (len(directory_editor_compress_file_suffixes(rule)%program) > 0) then
            command = directory_editor_compress_file_suffixes(rule)%program
            matched = index(command, "%i") > 0 .or. index(command, "%o") > 0

            if (matched) then
                newname = file_name_as_directory(newname)
                found = .true.
                call directory_editor_shell_command(replace_input_output(command, shell_quote_argument(file), &
                                                                         shell_quote_argument(newname)), &
                                                    res, status)
                return
            end if

            ! We found an uncompression rule.
            match = index(command, " ")
            message = "Uncompressing "//file

            if (match > 0) then
                call append_string(arguments, command(match + 1:), file)
                call directory_editor_check_process(message, command(1:match - 1), arguments, failed, status)
            else
                call append_string(arguments, file)
                call directory_editor_check_process(message, command, arguments, failed, status)
            end if

            if (.not. status%ok()) return
            found = .not. failed
            return
        end if
    end if

    ! We don't recognize the file as compressed, so compress it.
    ! Try gzip; if we don't have that, use compress.
    if (is_file_directory(file)) then
        suffix = directory_editor_compress_directory_default_suffix
        if (len(suffix) == 0) suffix = ".tar.gz"
        k = rule_for(directory_editor_compress_files_commands, suffix)

        if (k == 0) then
            call set_error(status, USER_ERROR_CODE, substitute_quotes("No compression rule found for "// &
                                                                      "`directory_editor_compress_directory_default_suffix' ") &
                           //suffix// &
                           substitute_quotes(", see `directory_editor_compress_files_commands' for the supported suffixes list"))
            return
        end if

        out_name = file//suffix
        directory_saved = aux_default_directory()
        buffers(current)%directory = file_name_directory(file)
        call directory_editor_shell_command(replace_input_output(directory_editor_compress_files_commands(k)%command, &
                                                                 shell_quote_argument(file_name_nondirectory(file)), &
                                                                 shell_quote_argument(out_name)), res, status)
        buffers(current)%directory = directory_saved

        if (.not. status%ok()) then
            if (condition_matches(status%code, FILE_ERROR_CODE)) call compress_with_compress()
            return
        end if

        newname = out_name
        found = .true.
        return
    end if

    suffix = directory_editor_compress_file_default_suffix
    if (len(suffix) == 0) suffix = ".gz"
    out_name = file//suffix
    k = rule_for(directory_editor_compress_file_commands, suffix)

    if (k == 0) then
        call set_error(status, USER_ERROR_CODE, &
                       "No compression rule found for suffix "//directory_editor_compress_file_default_suffix// &
                       substitute_quotes(", see `directory_editor_compress_file_commands' for the supported suffixes list"))
        return
    end if

    if (.not. file_exists(file)) return

    if (file_exists(out_name)) then
        call read_y_or_n("File "//out_name//" already exists.  Really compress? ", answer, status)
        if (.not. status%ok()) return
        if (.not. answer) return
    end if

    call directory_editor_shell_command(replace_input_output(directory_editor_compress_file_commands(k)%command, &
                                                             shell_quote_argument(file), &
                                                             shell_quote_argument(out_name)), res, status)

    if (.not. status%ok()) then
        if (condition_matches(status%code, FILE_ERROR_CODE)) call compress_with_compress()
        return
    end if

    if (.not. file_exists(out_name)) out_name = file//".z"

    ! Rename the compressed file to NEWNAME if it hasn't got that name
    ! already.
    if (has_newname .and. newname /= out_name) then
        call rename_file(out_name, newname, status, ok_if_already_exists=.true.)

        if (.not. status%ok()) then
            if (condition_matches(status%code, FILE_ERROR_CODE)) call compress_with_compress()
            return
        end if
    else
        newname = out_name
    end if

    found = .true.

contains

    !> The `file_error' handler: try compress.  Don't use NEWNAME with
    !> `compress'.
    recursive subroutine compress_with_compress()
        type(string_type), allocatable :: cargs(:)
        logical :: cfailed

        status = status_type()
        call append_string(cargs, "-f", file)
        call directory_editor_check_process("Compressing "//file, "compress", cargs, cfailed, status)
        if (.not. status%ok()) return

        if (.not. cfailed) then
            newname = file//".Z"
            found = .true.
        end if
    end subroutine compress_with_compress

    !> The first rule of TABLE whose regexp matches SUFFIX (0 if none).
    integer function rule_for(table, suffix) result(r)
        type(directory_editor_compress_rule_type), intent(in) :: table(:)
        character(len=*), intent(in) :: suffix
        type(status_type) :: s
        integer :: j

        r = 0

        do j = 1, size(table)
            if (string_match(table(j)%regular_expression, suffix, s, inhibit_modify=.true.) > 0) then
                r = j
                return
            end if
        end do
    end function rule_for
    end procedure directory_editor_compress_file

    !> Compress or uncompress the current file.  FAILURE is the offending
    !> file name, or "" for success.  Port of dired-aux.el:dired-compress.
    recursive subroutine directory_editor_compress(failure, status)
        character(len=:), allocatable, intent(out) :: failure
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: from_file, new_file
        type(handle_type), allocatable :: buffers(:)
        type(handle_type) :: old
        type(status_type) :: st
        integer(int64) :: start
        logical :: found, added
        integer :: k

        failure = ""
        call directory_editor_get_filename(from_file, found, status)
        if (.not. status%ok()) return
        call directory_editor_compress_file(from_file, new_file, found, status)
        if (.not. status%ok()) return

        if (found) then
            start = point()
            ! Remove any preexisting entry for the name NEW-FILE.
            call directory_editor_remove_entry(new_file)
            call goto_character(start)

            ! Now replace the current line with an entry for NEW-FILE, if
            ! it exists.  But don't remove the current line if either
            ! FROM-FILE or NEW-FILE is a directory, because
            ! compressing/uncompressing directories doesn't remove the
            ! original.  If NEW-FILE doesn't exist, assume that we are out
            ! of sync with the current directory, and revert it.
            if (file_exists(new_file)) then
                if (is_file_directory(from_file) .or. is_file_directory(new_file)) then
                    call directory_editor_add_entry(new_file, added, status, relative=.true.)
                else
                    call directory_editor_update_file_line(status, new_file)
                end if
            else
                buffers = directory_editor_buffers_for_directory(directory_editor_current_directory(st))
                old = current_buffer()

                do k = 1, size(buffers)
                    call set_buffer(buffers(k), st)
                    call revert_buffer(status)
                    if (.not. status%ok()) exit
                end do

                call set_buffer(old, st)
            end if
        else
            call directory_editor_log("Failed to (un)compress "//from_file)
            failure = from_file
        end if
    end subroutine directory_editor_compress

    !> The body of `directory_editor_do_compress': `directory_editor_compress', recording the
    !> results, then `sit_for' 0 (SHOW-PROGRESS).
    !> Port of part of dired-aux.el:dired-do-compress.
    recursive subroutine compress_body(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: failure

        call directory_editor_compress(failure, status)
        if (.not. status%ok()) return
        call append_check_result(failure)
        call sit_for(0)
    end subroutine compress_body

    !> Record one result of a `directory_editor_map_over_marks_check' body.
    !> Not a port.
    subroutine append_check_result(failure)
        character(len=*), intent(in) :: failure

        call append_string(check_results, failure)
    end subroutine append_check_result

    !> Map FUNCTION over marked files (with ARGUMENT like in `directory_editor_map_over_marks')
    !> and display failures.  FUNCTION records, with `append_check_result', the
    !> offending object (e.g. the short form of the filename) for a
    !> failure, or "", and probably logs a detailed error explanation
    !> using `directory_editor_log'.  OP_SYMBOL is a name describing the operation
    !> performed (e.g. `compress').  Port of
    !> dired-aux.el:dired-map-over-marks-check.
    recursive subroutine directory_editor_map_over_marks_check(function, argument_kind, argument_n, op_symbol, status)
        interface
            recursive subroutine function(status)
                import :: status_type
                implicit none(type, external)
                type(status_type), intent(out) :: status
            end subroutine function
        end interface
        integer, intent(in) :: argument_kind
        integer(int64), intent(in) :: argument_n
        character(len=*), intent(in) :: op_symbol
        type(status_type), intent(out) :: status
        type(string_type), allocatable :: failures(:)
        character(len=:), allocatable :: string
        character(len=48) :: text
        integer(int64) :: total, count
        logical :: confirmed, found, upward
        integer :: k

        call directory_editor_mark_confirm(op_symbol, argument_kind, argument_n, confirmed, status)
        if (.not. status%ok() .or. .not. confirmed) return
        if (allocated(check_results)) deallocate (check_results)
        allocate (check_results(0))
        call directory_editor_map_over_marks(function, argument_kind, argument_n, status, found, upward)
        if (.not. status%ok()) return
        total = size(check_results)
        count = 0

        do k = 1, size(check_results)
            if (len(check_results(k)%text) > 0) count = count + 1
        end do

        allocate (failures(count))
        count = 0

        do k = 1, size(check_results)
            if (len(check_results(k)%text) == 0) cycle
            count = count + 1
            failures(count)%text = check_results(k)%text
        end do

        string = operation_string(op_symbol)

        if (count == 0) then
            write (text, '(": ", i0)') total
            call message(string//trim(text)//ngettext(" file.", " files.", total))
        else
            ! end this bunch of errors:
            write (text, '(" ", i0, " of ", i0)') count, total
            call directory_editor_log_summary("Failed to "//downcase_string(string)//trim(text)//ngettext(" file", " files", &
                                                                                                          total), failures)
        end if
    end subroutine directory_editor_map_over_marks_check

    module procedure directory_editor_do_compress
    call directory_editor_map_over_marks_check(compress_body, argument_kind, argument_n, "compress", status)
    end procedure directory_editor_do_compress

    !> Compress or uncompress marked (or next ARGUMENT) files.
    !> If invoked on a directory, compress all of the files in
    !> the directory and all of its subdirectories, recursively,
    !> into a .tar.gz archive.
    !> If invoked on a .tar.gz or a .tgz or a .zip or a .7z archive,
    !> uncompress and unpack all the files in the archive.  Port of
    !> dired-aux.el:dired-do-compress (interactive).
    recursive subroutine execute_directory_editor_do_compress(status)
        type(status_type), intent(out) :: status
        integer :: kind
        integer(int64) :: n

        call aux_prefix_kind(kind, n)
        call directory_editor_do_compress(kind, n, status)
    end subroutine execute_directory_editor_do_compress

    !> FMT with %o replaced by O and %i by I, as format-spec does with
    !> the specification ((?o . O) (?i . I)): flags, width and precision
    !> are honored, %% is a %.  Port of format-spec.el:format-spec (no
    !> IGNORE-MISSING or SPLIT).
    subroutine format_specification_input_output(fmt, o, i, result, status)
        character(len=*), intent(in) :: fmt, o, i
        character(len=:), allocatable, intent(out) :: result
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: flags, value
        integer :: k, j, width, precision

        status = status_type()
        result = ""
        k = 1

        do while (k <= len(fmt))
            if (fmt(k:k) /= "%") then
                result = result//fmt(k:k)
                k = k + 1
                cycle
            end if

            j = k + 1

            if (j <= len(fmt)) then
                if (fmt(j:j) == "%") then
                    result = result//"%"
                    k = j + 1
                    cycle
                end if
            end if

            flags = ""

            do while (j <= len(fmt))
                if (index("-0 ^_<>", fmt(j:j)) == 0) exit
                flags = flags//fmt(j:j)
                j = j + 1
            end do

            width = -1

            do while (j <= len(fmt))
                if (index("0123456789", fmt(j:j)) == 0) exit
                if (width < 0) width = 0
                width = width*10 + iachar(fmt(j:j)) - iachar("0")
                j = j + 1
            end do

            precision = -1

            if (j <= len(fmt)) then
                if (fmt(j:j) == ".") then
                    j = j + 1
                    precision = 0

                    do while (j <= len(fmt))
                        if (index("0123456789", fmt(j:j)) == 0) exit
                        precision = precision*10 + iachar(fmt(j:j)) - iachar("0")
                        j = j + 1
                    end do
                end if
            end if

            if (j > len(fmt)) then
                call set_error(status, GENERIC_ERROR_CODE, substitute_quotes("Invalid format string"))
                return
            end if

            select case (fmt(j:j))
            case ("o")
                value = o
            case ("i")
                value = i
            case default
                call set_error(status, GENERIC_ERROR_CODE, substitute_quotes("Invalid format character: `%")//fmt(j:j)// &
                               substitute_quotes("'"))
                return
            end select

            if (precision >= 0 .and. len(value) > precision) value = value(1:precision)
            if (index(flags, "^") > 0) value = upcase_string(value)
            if (index(flags, "_") > 0) value = downcase_string(value)

            if (width >= 0 .and. len(value) > width) then
                if (index(flags, "<") > 0) then
                    value = value(len(value) - width + 1:)
                else if (index(flags, ">") > 0) then
                    value = value(1:width)
                end if
            end if

            if (width > len(value)) then
                if (index(flags, "-") > 0) then
                    value = value//repeat(" ", width - len(value))
                else if (index(flags, "0") > 0) then
                    value = repeat("0", width - len(value))//value
                else
                    value = repeat(" ", width - len(value))//value
                end if
            end if

            result = result//value
            k = j + 1
        end do
    end subroutine format_specification_input_output

    module procedure directory_editor_do_compress_to
    type(string_type), allocatable :: in_files(:)
    character(len=:), allocatable :: out_file, input, ins, command
    character(len=32) :: text
    type(value_type) :: res
    type(status_type) :: st
    integer :: k, rule
    logical :: answer, zero

    call directory_editor_get_marked_files(in_files, status, error="")
    if (.not. status%ok()) return
    call read_file_name("Compress to: ", input, status)
    if (.not. status%ok()) return
    out_file = aux_expand(input)
    rule = 0

    do k = 1, size(directory_editor_compress_files_commands)
        if (string_match(directory_editor_compress_files_commands(k)%regular_expression, out_file, st) > 0) then
            rule = k
            exit
        end if
    end do

    if (rule == 0) then
        call set_error(status, GENERIC_ERROR_CODE, "No compression rule found for "//out_file// &
                       substitute_quotes(", see `directory_editor_compress_files_commands'"))
        return
    end if

    if (file_exists(out_file)) then
        call read_y_or_n(abbreviate_file_name(out_file)//" exists, overwrite?", answer, status)
        if (.not. status%ok()) return

        if (.not. answer) then
            call message("Compression aborted")
            return
        end if
    end if

    ins = ""

    do k = 1, size(in_files)
        if (k > 1) ins = ins//" "
        ins = ins//shell_quote_argument(file_relative_name(in_files(k)%text))
    end do

    call format_specification_input_output(directory_editor_compress_files_commands(rule)%command, shell_quote_argument(out_file), &
                                           ins, command, status)
    if (.not. status%ok()) return
    call directory_editor_shell_command(command, res, status)
    if (.not. status%ok()) return
    zero = .false.
    if (vtag(res) == V_INTEGER) zero = xfixnum(res) == 0

    if (zero) then
        write (text, '("Compressed ", i0)') size(in_files)
        call message(trim(text)//ngettext(" file to ", " files to ", int(size(in_files), int64))//file_name_nondirectory(out_file))
    end if
    end procedure directory_editor_do_compress_to

    !> Compress selected files and directories to an archive.
    !> Prompt for the archive file name.
    !> Choose the archiving command based on the archive file-name extension
    !> and `directory_editor_compress_files_commands'.  Port of
    !> dired-aux.el:dired-do-compress-to (interactive).
    recursive subroutine execute_directory_editor_do_compress_to(status)
        type(status_type), intent(out) :: status

        call directory_editor_do_compress_to(status)
    end subroutine execute_directory_editor_do_compress_to

    ! ------------------------------------------------------------------
    ! Updating Dired buffers
    ! ------------------------------------------------------------------

    module procedure directory_editor_remove_file
    type(handle_type), allocatable :: buffers(:)
    type(handle_type) :: old
    type(status_type) :: st
    integer :: k

    buffers = directory_editor_buffers_for_directory(file_name_directory(file), file_name_nondirectory(file))
    old = current_buffer()

    do k = 1, size(buffers)
        call set_buffer(buffers(k), st)
        call directory_editor_remove_entry(file)
    end do

    call set_buffer(old, st)
    end procedure directory_editor_remove_file

    module procedure directory_editor_relist_file
    type(handle_type), allocatable :: buffers(:)
    type(handle_type) :: old
    type(status_type) :: st
    integer :: k

    status = status_type()
    buffers = directory_editor_buffers_for_directory(file_name_directory(file), file_name_nondirectory(file))
    old = current_buffer()

    do k = 1, size(buffers)
        call set_buffer(buffers(k), st)
        call directory_editor_relist_entry(file, status)
        if (.not. status%ok()) exit
    end do

    call set_buffer(old, st)
    end procedure directory_editor_relist_file

    !> Relist the line for FILE, or just add it if it did not exist.
    !> FILE must be an absolute file name.  Port of
    !> dired-aux.el:dired-relist-entry.
    recursive subroutine directory_editor_relist_entry(file, status)
        character(len=*), intent(in) :: file
        type(status_type), intent(out) :: status
        type(value_type) :: saved_ro
        type(status_type) :: st
        integer(int64) :: original_point, b
        character :: marker
        logical :: added, has_marker

        status = status_type()
        original_point = point()
        has_marker = .false.
        marker = " "
        call aux_inhibit_read_only_begin(saved_ro)

        if (directory_editor_goto_file(file, st) /= 0) then
            call beginning_of_line()
            marker = aux_character_at()
            has_marker = .true.
            b = point()
            call delete_region(b, aux_beginning_of_line(2_int64), st)
        end if

        if (has_marker .and. marker /= " " .and. marker /= achar(0)) then
            call directory_editor_add_entry(directory_file_name(file), added, status, marker_character=marker)
        else
            call directory_editor_add_entry(directory_file_name(file), added, status)
        end if

        call aux_inhibit_read_only_end(saved_ro)
        call goto_character(min(original_point, point_maximum()))
    end subroutine directory_editor_relist_entry

    !> Return FILE's marker (MARKER), or FOUND false if unmarked.  Port of
    !> dired.el:dired-file-marker.
    subroutine directory_editor_file_marker(file, marker, found)
        character(len=*), intent(in) :: file
        character, intent(out) :: marker
        logical, intent(out) :: found
        type(status_type) :: st
        integer(int64) :: original_point

        found = .false.
        marker = " "
        original_point = point()

        if (directory_editor_goto_file(file, st) /= 0) then
            call beginning_of_line()
            marker = aux_character_at()
            found = marker /= " "
        end if

        call goto_character(original_point)
    end subroutine directory_editor_file_marker

    !> Remove the entry of DIRECTORY from `directory_editor_buffers'.  Port of
    !> dired.el:dired-unadvertise.
    subroutine directory_editor_unadvertise(directory)
        character(len=*), intent(in) :: directory
        type(directory_editor_buffer_entry_type), allocatable :: kept(:)
        character(len=:), allocatable :: expanded
        integer :: k, n
        logical :: removed

        expanded = aux_expand(directory)
        allocate (kept(size(directory_editor_buffers)))
        n = 0
        removed = .false.

        do k = 1, size(directory_editor_buffers)
            ! (delq (assoc ...)): the first matching entry
            if (.not. removed .and. directory_editor_buffers(k)%directory == expanded) then
                removed = .true.
                cycle
            end if

            n = n + 1
            kept(n) = directory_editor_buffers(k)
        end do

        deallocate (directory_editor_buffers)
        allocate (directory_editor_buffers(n))
        directory_editor_buffers(:) = kept(1:n)
    end subroutine directory_editor_unadvertise

    ! ------------------------------------------------------------------
    ! Copy, move/rename, making hard and symbolic links
    ! ------------------------------------------------------------------

    !> Save old version of file TO that is to be overwritten.  Port of
    !> dired-aux.el:dired-handle-overwrite: `directory_editor_backup_overwrite' is
    !> not ported (it is false), so nothing is backed up.
    subroutine directory_editor_handle_overwrite(to)
        character(len=*), intent(in) :: to

        if (len(to) < 0) return
    end subroutine directory_editor_handle_overwrite

    module procedure directory_editor_copy_file
    type(choice_type) :: recursive

    call directory_editor_handle_overwrite(to)
    recursive = choice_state(directory_editor_recursive_copies)
    call directory_editor_copy_file_recursive(from, to, ok_flag, status, directory_editor_copy_preserve_time, .true., recursive, &
                                              directory_editor_copy_dereference)
    end procedure directory_editor_copy_file

    !> Create DIRECTORY if doesn't exist according to `directory_editor_create_destination_directories'.
    !> Port of dired-aux.el:dired-maybe-create-dirs.
    recursive subroutine directory_editor_maybe_create_directories(directory, status)
        character(len=*), intent(in) :: directory
        type(status_type), intent(out) :: status
        logical :: answer

        status = status_type()
        if (directory_editor_create_destination_directories%is_off()) return
        if (file_exists(directory)) return
        answer = directory_editor_create_destination_directories%value

        if (.not. answer) then
            call read_yes_or_no("Create destination dir `"//directory//"'? ", answer, status)
            if (.not. status%ok()) return
        end if

        if (answer) call directory_editor_create_directory(directory, status)
    end subroutine directory_editor_maybe_create_directories

    module procedure directory_editor_copy_file_recursive
    type(file_status_type) :: attributes
    type(choice_type) :: rec
    logical :: keep, at_top, deref
    character(len=:), allocatable :: target
    logical :: found, is_directory, is_link, answer, do_directory

    call file_attributes(from, attributes, found, nofollow=.true.)
    is_directory = .false.
    is_link = .false.

    if (found) then
        is_directory = iand(attributes%mode, S_IFMT) == S_IFDIR
        is_link = iand(attributes%mode, S_IFMT) == S_IFLNK
    end if

    status = status_type()
    keep = .false.
    if (present(preserve_time)) keep = preserve_time
    at_top = .false.
    if (present(top)) at_top = top
    rec = .false.
    if (present(recursive)) rec = recursive
    deref = .false.
    if (present(dereference)) deref = dereference

    if (is_directory) then
        if (is_file_in_directory(to, from)) then
            call set_error(status, GENERIC_ERROR_CODE, substitute_quotes("Cannot copy `")//from// &
                           substitute_quotes("' into its subdirectory `")//to//substitute_quotes("'"))
            return
        end if
    end if

    do_directory = .false.

    if (.not. rec%is_off() .and. is_directory) then
        if (rec%value) then
            do_directory = .true.
        else
            call read_yes_or_no("Copy "//from//" recursively? ", answer, status)
            if (.not. status%ok()) return
            do_directory = answer
        end if
    end if

    if (do_directory) then
        call copy_directory(from, to, status, keep_time=keep)
        return
    end if

    if (.not. at_top) call directory_editor_handle_overwrite(to)

    if (.not. deref .and. is_link) then
        ! It is a symlink
        target = file_symlink_target(from)
        call create_symbolic_link(target, to, status, ok_flag)
    else
        call directory_editor_maybe_create_directories(file_name_directory(to), status)
        if (.not. status%ok()) return
        call copy_file(from, to, status, ok_if_already_exists=ok_flag, keep_time=keep)
    end if

    if (status%code == FILE_DATE_ERROR_CODE) then
        call prepend_create_files_failure(directory_editor_make_relative(from))
        call directory_editor_log(substitute_quotes("Can't set date on ")//from//":"//LF//format_error(status)//LF)
        status = status_type()
    end if
    end procedure directory_editor_copy_file_recursive

    !> Push NAME on `directory_editor_create_files_failures'.  Not a port.
    subroutine prepend_create_files_failure(name)
        character(len=*), intent(in) :: name

        if (.not. allocated(create_files_failures)) allocate (create_files_failures(0))
        call prepend_string(create_files_failures, name)
    end subroutine prepend_create_files_failure

    module procedure directory_editor_rename_file
    type(handle_type) :: b, old
    type(status_type) :: st
    character(len=:), allocatable :: absname
    logical :: file_is_is_directory

    file_is_is_directory = is_file_directory(file)
    call directory_editor_handle_overwrite(newname)
    call directory_editor_maybe_create_directories(file_name_directory(newname), status)
    if (.not. status%ok()) return
    ! error is caught in -create-files
    call rename_file(file, newname, status, ok_if_already_exists)
    if (.not. status%ok()) return
    ! Silently rename the visited file of any buffer visiting this file.
    absname = aux_expand(file)
    b = get_file_buffer(absname)

    if (b%index /= 0) then
        old = current_buffer()
        call set_buffer(b, st)
        call set_visited_file_name(newname, st, no_query=.true., along_with_file=.true.)
        call set_buffer(old, st)
    end if

    call directory_editor_remove_file(file)
    ! See if it's an inserted subdir, and rename that, too.
    if (file_is_is_directory) call directory_editor_rename_subdirectory(file, newname, status)
    end procedure directory_editor_rename_file

    !> Rename the subdirectory FROM_DIRECTORY to TO_DIRECTORY in all Dired buffers, and
    !> the visited file names of the buffers visiting files in it.
    !> Port of dired-aux.el:dired-rename-subdir.
    recursive subroutine directory_editor_rename_subdirectory(from_directory_in, to_directory_in, status)
        character(len=*), intent(in) :: from_directory_in, to_directory_in
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: from_directory, to_directory, expanded_from_directory, to_file
        type(handle_type), allocatable :: buffer_handles(:), blist(:)
        type(handle_type) :: old
        type(status_type) :: st
        logical :: modflag
        integer :: k
        integer(int64) :: b

        status = status_type()
        from_directory = file_name_as_directory(from_directory_in)
        to_directory = file_name_as_directory(to_directory_in)
        buffer_handles = directory_editor_buffers_for_directory(from_directory)
        old = current_buffer()

        do k = 1, size(buffer_handles)
            call set_buffer(buffer_handles(k), st)
            call directory_editor_rename_subdirectory_1(from_directory, to_directory, status)
            if (.not. status%ok()) exit
        end do

        call set_buffer(old, st)
        if (.not. status%ok()) return
        ! Update visited file name of all affected buffers
        expanded_from_directory = aux_expand(from_directory)
        blist = buffer_list()

        do k = 1, size(blist)
            call resolve_buffer(blist(k), b, st)
            if (.not. st%ok()) cycle
            if (.not. allocated(buffers(b)%filename)) cycle
            if (.not. is_directory_editor_in_this_tree(buffers(b)%filename, expanded_from_directory)) cycle
            call set_buffer(blist(k), st)
            call is_buffer_modified(blist(k), modflag, st)

            ! (replace-regexp-in-string (concat "^" (regexp-quote from-dir)) to-dir buffer-file-name)
            if (index(buffers(current)%filename, from_directory) == 1) then
                to_file = to_directory//buffers(current)%filename(len(from_directory) + 1:)
            else
                to_file = buffers(current)%filename
            end if

            call set_visited_file_name(to_file, st)
            call restore_buffer_modified(modflag)
        end do

        call set_buffer(old, st)
    end subroutine directory_editor_rename_subdirectory

    !> Rename DIRECTORY to TO in headerlines and `directory_editor_subdirectories', if DIRECTORY or
    !> one of its subdirectories is expanded in this buffer.  Port of
    !> dired-aux.el:dired-rename-subdir-1.
    recursive subroutine directory_editor_rename_subdirectory_1(directory, to, status)
        character(len=*), intent(in) :: directory, to
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: expanded_directory, new_name, ddir, actual
        type(handle_type) :: existing
        integer :: k, i

        status = status_type()
        expanded_directory = aux_expand(directory)
        k = aux_local_index()

        if (k > 0) then
            if (allocated(locals(k)%subdirectory_table)) then
                do i = 1, size(locals(k)%subdirectory_table)
                    ! ELEMENT's subdir is affected by the rename
                    if (is_directory_editor_in_this_tree(locals(k)%subdirectory_table(i)%directory, expanded_directory)) then
                        call directory_editor_rename_subdirectory_2(i, directory, to, status)
                        if (.not. status%ok()) return
                    end if
                end do
            end if
        end if

        if (directory == aux_default_directory()) then
            ! if top level directory was renamed, lots of things have to be
            ! updated:
            ! we no longer dired DIRECTORY...
            call directory_editor_unadvertise(directory)
            buffers(current)%directory = to

            if (k > 0) then
                ddir = ""
                if (allocated(locals(k)%directory)) ddir = locals(k)%directory
                ! this is correct with and without wildcards
                locals(k)%directory = aux_expand_in(file_name_nondirectory(ddir), to)
                new_name = file_name_nondirectory(directory_file_name(locals(k)%directory))
                ! try to rename buffer, but just leave old name if new name
                ! would already exist (don't try appending "<%d>")
                existing = get_buffer(new_name)
                if (existing%index == 0) call rename_buffer(new_name, .false., actual, status)
            end if

            ! ... we dired TO now:
            call directory_editor_advertise()
        end if
    end subroutine directory_editor_rename_subdirectory_1

    !> `expand_file_name' of NAME in DIRECTORY as a function.  Not a port.
    function aux_expand_in(name, directory) result(full)
        character(len=*), intent(in) :: name, directory
        character(len=:), allocatable :: full
        type(status_type) :: st

        call expand_file_name(name, full, st, directory)
        if (.not. st%ok()) full = name
    end function aux_expand_in

    !> Update the headerline and `directory_editor_subdirectories' element I of the
    !> directory to reflect the moving of DIRECTORY to TO.  Thus, the element
    !> describes either DIRECTORY itself or a subdir of DIRECTORY.  Port of
    !> dired-aux.el:dired-rename-subdir-2 (no `directory_editor_switches_by_directory').
    recursive subroutine directory_editor_rename_subdirectory_2(i, directory, to, status)
        integer, intent(in) :: i
        character(len=*), intent(in) :: directory, to
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: regular_expression, newtext, element_directory
        type(value_type) :: saved_ro
        type(status_type) :: st
        integer(int64) :: original_point, b1, e1, found
        integer :: k

        status = status_type()
        k = aux_local_index()
        original_point = point()
        regular_expression = regular_expression_quote(directory_file_name(directory))
        newtext = directory_file_name(to)
        element_directory = locals(k)%subdirectory_table(i)%directory
        call aux_inhibit_read_only_begin(saved_ro)
        call goto_character(aux_marker_position(locals(k)%subdirectory_table(i)%marker))

        ! Update subdir headerline in buffer
        if (.not. looking_at("^. \(.+\)\(:\)"//LF, st)) then
            call set_error(status, GENERIC_ERROR_CODE, &
                           directory//" not found where expected - directory_editor_subdirectories broken?")
        else
            b1 = match_beginning(1, st)
            e1 = match_end(1, st)
            call goto_character(b1)
            call regular_expression_search_forward(regular_expression, found, st, bound=e1, noerror=SEARCH_NOERROR)

            if (st%ok() .and. found /= 0) then
                call replace_match(newtext, st, fixedcase=.true., literal=.true.)
            else
                call set_error(status, GENERIC_ERROR_CODE, substitute_quotes("Expected to find `")//directory// &
                               substitute_quotes("' in headerline of ")//element_directory)
            end if
        end if

        call aux_inhibit_read_only_end(saved_ro)
        call goto_character(original_point)
        if (.not. status%ok()) return
        ! Update buffer-local dired-subdir-alist
        locals(k)%subdirectory_table(i)%directory = directory_editor_normalize_subdirectory(replace_all(element_directory, &
                                                                                           directory_file_name(directory), newtext))
    end subroutine directory_editor_rename_subdirectory_2

    !> Default is to create files with the same names as the originals.
    module procedure directory_editor_hardlink
    call directory_editor_handle_overwrite(newname)
    ! error is caught in -create-files
    call add_name_to_file(file, newname, status, ok_if_already_exists)
    if (.not. status%ok()) return
    ! Update the link count
    call directory_editor_relist_file(file, status)
    end procedure directory_editor_hardlink

    module procedure directory_editor_make_relative_symlink
    character(len=:), allocatable :: f1, f2, name1, name2, sub, tem
    integer :: len1, len2, position, next, start, count, k

    f1 = aux_expand(file1)
    f2 = aux_expand(file2)
    len1 = len(f1)
    len2 = len(f2)
    position = 0

    ! Find common initial file name components:
    do
        k = index(f1(position + 1:), "/")
        if (k == 0) exit
        next = position + k
        ! For the comparison, both substrings must end in `/', so NEXT is
        ! *one plus* the result of the string-search.
        if (.not. (next < min(len1, len2))) exit
        if (f1(1:next) /= f2(1:next)) exit
        position = next
    end do

    name2 = f2
    sub = f1(1:position)
    name1 = f1(position + 1:)

    if (sub == "/" .and. len(sub) == 1) then
        ! No common initial file name found
        name1 = f1
    else
        ! Else they have a common parent directory
        tem = f2(position + 1:)
        start = 0
        count = 0

        ! Count number of slashes we must compensate for ...
        do
            k = index(tem(start + 1:), "/")
            if (k == 0) exit
            count = count + 1
            start = start + k
        end do

        ! ... and prepend a "../" for each slash found:
        do k = 1, count
            name1 = "../"//name1
        end do
    end if

    ! must not link to foo/ (trailing slash!)
    call create_symbolic_link(directory_file_name(name1), name2, status, ok_if_already_exists)
    end procedure directory_editor_make_relative_symlink

    !> Make the symbolic link `directory_editor_do_symlink' makes (a file creator:
    !> `create_symbolic_link' with FROM as the target).  Not a port.
    recursive subroutine symlink_creator(from, to, ok_flag, status)
        character(len=*), intent(in) :: from, to
        logical, intent(in) :: ok_flag
        type(status_type), intent(out) :: status

        call create_symbolic_link(from, to, status, ok_flag)
    end subroutine symlink_creator

    !> `add_name_to_file' as a file creator (dired-do-hardlink-regexp).
    !> Not a port.
    recursive subroutine add_name_creator(from, to, ok_flag, status)
        character(len=*), intent(in) :: from, to
        logical, intent(in) :: ok_flag
        type(status_type), intent(out) :: status

        call add_name_to_file(from, to, status, ok_flag)
    end subroutine add_name_creator

    ! ------------------------------------------------------------------
    ! The basic function for half a dozen variations on cp/mv/ln/ln -s
    ! ------------------------------------------------------------------

    recursive module subroutine directory_editor_create_files(file_creator, creator_kind, operation, file_names, name_constructor, &
                                                              marker_character, status)
        procedure(directory_editor_file_creator_procedure) :: file_creator
        integer, intent(in) :: creator_kind
        character(len=*), intent(in) :: operation
        type(string_type), intent(in) :: file_names(:)
        procedure(directory_editor_name_constructor_procedure) :: name_constructor
        character, intent(in) :: marker_character
        type(status_type), intent(out) :: status
        type(string_type), allocatable :: failures(:), skipped(:), all(:)
        character(len=:), allocatable :: from, to, destname, help, lower_op
        character(len=48) :: text, requests_text
        type(file_status_type) :: dattrs
        type(status_type) :: st
        integer(int64) :: success_count, total, p, nf
        integer :: k, overwrite_query
        character :: actual_marker_character
        logical :: skip, overwrite, overwrite_confirmed, has_marker, found, ok

        status = status_type()
        if (allocated(create_files_failures)) deallocate (create_files_failures)
        allocate (create_files_failures(0), failures(0), skipped(0))
        success_count = 0
        total = size(file_names)
        overwrite_query = 0
        lower_op = downcase_string(operation)

        do k = 1, size(file_names)
            from = file_names(k)%text
            ! Position point on the current file -- this is useful if
            ! handling a number of files to show where we're working at.
            p = directory_editor_goto_file(from, st)
            call name_constructor(from, to, skip, status)
            if (.not. status%ok()) return

            if (.not. skip) then
                if (to == from .and. len(to) == len(from)) then
                    skip = .true.
                    call directory_editor_log("Cannot "//lower_op//" to same file: "//from//LF)
                end if
            end if

            if (skip) then
                call prepend_string(skipped, directory_editor_make_relative(from))
                cycle
            end if

            overwrite = file_exists(to)
            overwrite_confirmed = .false.

            if (overwrite) then
                help = substitute_command_keys("Type \`SPC' or \`y' to overwrite file `")//to// &
                       substitute_command_keys("',"//LF//"\`DEL' or \`n' to skip to next,"//LF// &
                                               "\`ESC' or \`q' to not overwrite any of the remaining "// &
                                               "files,"//LF//"\`!' to overwrite all remaining files "// &
                                               "with no more questions.")
                call directory_editor_query(overwrite_query, substitute_quotes("Overwrite `")//to//substitute_quotes("'?"), &
                                            overwrite_confirmed, status, help_procedure=help)
                if (.not. status%ok()) return
            end if

            ! must determine if FROM is marked before file-creator gets a
            ! chance to delete it (in case of a move).
            has_marker = .false.

            if (marker_character == DIRECTORY_EDITOR_KEEP_MARKER) then
                call directory_editor_file_marker(from, actual_marker_character, has_marker)
            else if (marker_character /= " ") then
                actual_marker_character = marker_character
                has_marker = .true.
            end if

            ! Handle the `directory_editor_copy_file' file-creator specially When copying
            ! a directory to another directory or possibly to itself or one of
            ! its subdirectories.
            destname = file_name_directory(to)
            if (is_file_directory(from) .and. is_file_directory(to) .and. creator_kind == DIRECTORY_EDITOR_CREATOR_COPY) to = &
                destname
            ! If DESTNAME is a subdirectory of FROM, not a symlink, and the
            ! method in use is copying, signal an error.
            call file_attributes(destname, dattrs, found, nofollow=.true.)

            if (found .and. creator_kind == DIRECTORY_EDITOR_CREATOR_COPY) then
                if (iand(dattrs%mode, S_IFMT) == S_IFDIR) then
                    if (is_file_in_directory(destname, from)) then
                        call set_error(status, GENERIC_ERROR_CODE, substitute_quotes("Cannot copy `")//from// &
                                       substitute_quotes("' into its subdirectory `")//to//substitute_quotes("'"))
                        return
                    end if
                end if
            end if

            call file_creator(from, to, overwrite_confirmed, st)
            ok = st%ok()

            if (ok) then
                ! If we get here, file-creator hasn't been aborted and the old
                ! entry (if any) has to be deleted before adding the new entry.
                if (overwrite) call directory_editor_remove_file(to)
                success_count = success_count + 1
                write (text, '(": ", i0, " of ", i0)') success_count, total
                call message(operation//trim(text))

                if (has_marker) then
                    call directory_editor_add_file(to, status, actual_marker_character)
                else
                    call directory_editor_add_file(to, status)
                end if

                if (.not. status%ok()) return
            else if (condition_matches(st%code, FILE_ERROR_CODE)) then
                ! FILE-CREATOR aborted
                call prepend_string(failures, directory_editor_make_relative(from))
                call directory_editor_log(operation//substitute_quotes(": `")//from//substitute_quotes("' to `")//to// &
                                          substitute_quotes("' failed:")//LF//"("//error_name(st%code)//" "// &
                                          format_error(st)//")"//LF)
            else
                status = st
                return
            end if
        end do

        if (size(create_files_failures) > 0) then
            allocate (all(size(failures) + size(create_files_failures)))
            all(1:size(failures)) = failures
            all(size(failures) + 1:) = create_files_failures
            nf = size(all)
            write (text, '(" failed for ", i0)') nf
            write (requests_text, '(" in ", i0, " requests")') total
            call directory_editor_log_summary(operation//trim(text)//ngettext(" file", " files", nf)//trim(requests_text), all)
        else if (size(failures) > 0) then
            nf = size(failures)
            write (text, '(": ", i0, " of ", i0)') nf, total
            call directory_editor_log_summary(operation//trim(text)//ngettext(" file", " files", total)//" failed", failures)
        else if (size(skipped) > 0) then
            nf = size(skipped)
            write (text, '(": ", i0, " of ", i0)') nf, total
            call directory_editor_log_summary(operation//trim(text)//ngettext(" file", " files", total)//" skipped", skipped)
        else
            write (text, '(": ", i0)') success_count
            call message(operation//trim(text)//ngettext(" file done", " files done", success_count))
        end if

        p = directory_editor_move_to_filename(st)
    end subroutine directory_editor_create_files

    !> The name constructor of a target directory: FROM's name in
    !> `create_target'.  Port of part of dired-aux.el:dired-do-create-files.
    recursive subroutine into_directory_constructor(from, to, skip, status)
        character(len=*), intent(in) :: from
        character(len=:), allocatable, intent(out) :: to
        logical, intent(out) :: skip
        type(status_type), intent(out) :: status

        status = status_type()
        skip = .false.
        to = aux_expand_in(file_name_nondirectory(from), create_target)
    end subroutine into_directory_constructor

    !> The name constructor of a single target: `create_target'.
    !> Port of part of dired-aux.el:dired-do-create-files.
    recursive subroutine target_constructor(from, to, skip, status)
        character(len=*), intent(in) :: from
        character(len=:), allocatable, intent(out) :: to
        logical, intent(out) :: skip
        type(status_type), intent(out) :: status

        status = status_type()
        skip = .false.
        to = create_target
        if (len(from) < 0) skip = .true.
    end subroutine target_constructor

    !> The target directory the user may want: if `directory_editor_do_what_i_mean_target' is
    !> set and a Dired buffer is displayed in one of the next windows, its
    !> current subdir, else the current subdir of this Dired buffer.
    !> Port of dired-aux.el:dired-dwim-target-directory.
    function directory_editor_do_what_i_mean_target_directory() result(directory)
        character(len=:), allocatable :: directory
        type(string_type), allocatable :: directories(:)
        type(status_type) :: st

        directory = ""

        if (allocated(buffers(current)%major_mode)) then
            if (buffers(current)%major_mode == "directory_editor_mode") directory = directory_editor_current_directory(st)
        end if

        ! non-dired buffer may want to profit from this function, e.g.
        ! vm-uudecode
        if (.not. directory_editor_do_what_i_mean_target%is_off()) then
            call directory_editor_do_what_i_mean_target_directories(directories)
            if (size(directories) > 0) directory = directories(1)%text
        end if
    end function directory_editor_do_what_i_mean_target_directory

    !> The directories of the Dired buffers in the other windows, as
    !> `directory_editor_do_what_i_mean_target' chooses them.  Port of
    !> dired-aux.el:dired-dwim-target-directories with
    !> dired-dwim-target-next, -next-visible and -recent (one frame; a
    !> function as `directory_editor_do_what_i_mean_target' is deferred).
    subroutine directory_editor_do_what_i_mean_target_directories(directories)
        type(string_type), allocatable, intent(out) :: directories(:)
        type(handle_type), allocatable :: leaves(:), order(:)
        type(handle_type) :: sel, old
        type(status_type) :: st
        integer(int64), allocatable :: times(:)
        integer(int64) :: b, tk
        type(string_type) :: td
        integer :: k, n, first, j

        if (associated(directory_editor_do_what_i_mean_target%procedure)) then
            call directory_editor_do_what_i_mean_target%procedure(directories)
            return
        end if

        allocate (directories(0))
        call window_leaves(selected_frame%root_window, leaves)
        sel = selected_window()
        n = size(leaves)
        first = 0

        do k = 1, n
            if (same_handle(leaves(k), sel)) first = k
        end do

        ! (window-list-1 (next-window nil 'nomini) 'nomini): the windows
        ! after the selected one, cyclically, without it.
        allocate (order(0))

        do j = 1, n - 1
            k = mod(first + j - 1, n) + 1
            if (.not. same_handle(leaves(k), sel)) order = [order, leaves(k)]
        end do

        if (first == 0) order = leaves
        old = current_buffer()
        allocate (times(0))

        do k = 1, size(order)
            call resolve_buffer(windows(order(k)%index)%buffer, b, st)
            if (.not. st%ok()) cycle
            if (.not. allocated(buffers(b)%major_mode)) cycle
            if (buffers(b)%major_mode /= "directory_editor_mode") cycle
            call set_buffer(windows(order(k)%index)%buffer, st)
            call append_string(directories, directory_editor_current_directory(st))
            times = [times, windows(order(k)%index)%use_time]
        end do

        call set_buffer(old, st)

        if (directory_editor_do_what_i_mean_target%choice == DIRECTORY_EDITOR_DO_WHAT_I_MEAN_TARGET_RECENT) then
            ! Ordered by most-recently-used.
            do k = 2, size(directories)
                j = k

                do while (j > 1)
                    if (times(j - 1) >= times(j)) exit
                    tk = times(j)
                    times(j) = times(j - 1)
                    times(j - 1) = tk
                    td = directories(j)
                    directories(j) = directories(j - 1)
                    directories(j - 1) = td
                    j = j - 1
                end do
            end do
        end if
    end subroutine directory_editor_do_what_i_mean_target_directories

    recursive module subroutine directory_editor_do_create_files(op_symbol, file_creator, creator_kind, operation, argument_kind, &
                                                                 argument_n, &
                                                                 marker_character, status, op1, how_to_plain)
        character(len=*), intent(in) :: op_symbol, operation
        procedure(directory_editor_file_creator_procedure) :: file_creator
        integer, intent(in) :: creator_kind, argument_kind
        integer(int64), intent(in) :: argument_n
        character, intent(in) :: marker_character
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: op1
        logical, intent(in), optional :: how_to_plain
        type(string_type), allocatable :: file_names(:), relative_file_names(:)
        character(len=:), allocatable :: target_directory, default, target, input, op, prompt
        type(handle_type), allocatable :: buffers(:)
        type(handle_type) :: old
        type(status_type) :: st
        logical :: one_file, into_directory
        integer :: k

        call directory_editor_get_marked_files(file_names, status, argument_kind=argument_kind, argument_n=argument_n, error="")
        if (.not. status%ok()) return
        allocate (relative_file_names(size(file_names)))

        do k = 1, size(file_names)
            relative_file_names(k)%text = directory_editor_make_relative(file_names(k)%text)
        end do

        one_file = size(file_names) == 1
        target_directory = directory_editor_do_what_i_mean_target_directory()
        ! The first of dired-dwim-target-defaults: TARGET-DIR, else the
        ! current directory (the other defaults, for M-n, are deferred:
        ! `read_file_name' takes one default).
        default = target_directory
        if (len(default) == 0) default = directory_editor_current_directory(st)
        op = operation
        if (one_file .and. present(op1)) op = op1

        if (op_symbol == "symlink" .or. op_symbol == "hardlink") then
            ! Linking operations create links from the prompted file name;
            ! the other operations copy (etc) to the prompted file name.
            prompt = " from: "
        else
            prompt = " to: "
        end if

        call directory_editor_mark_read_file_name(op//" ", prompt, target_directory, op_symbol, argument_kind, argument_n, &
                                                  relative_file_names, input, &
                                                  status, default)
        if (.not. status%ok()) return
        target = aux_expand(input)

        if (.not. one_file .or. (directory_editor_create_directories_on_trailing_separator .and. is_directory_name(target))) then
            call directory_editor_maybe_create_directories(target, status)
            if (.not. status%ok()) return
        end if

        if (present(how_to_plain)) then
            into_directory = .not. how_to_plain .and. is_file_directory(target)
        else
            into_directory = is_file_directory(target)
        end if

        if (.not. (one_file .or. into_directory)) then
            call set_error(status, GENERIC_ERROR_CODE, "Marked "//operation//": target must be a directory: "//target)
            return
        end if

        if (.not. is_file_directory(file_names(1)%text) .and. .not. is_file_directory(target) .and. is_directory_name(target)) then
            call set_error(status, GENERIC_ERROR_CODE, operation//": Target directory does not exist: "//target)
            return
        end if

        ! rename-file bombs when moving directories unless we do this:
        if (.not. into_directory) target = directory_file_name(target)
        create_target = target

        if (into_directory) then
            call directory_editor_create_files(file_creator, creator_kind, operation, file_names, into_directory_constructor, &
                                               marker_character, status)
        else
            call directory_editor_create_files(file_creator, creator_kind, operation, file_names, target_constructor, &
                                               marker_character, status)
        end if

        if (.not. status%ok()) return

        if (directory_editor_do_revert_buffer) then
            buffers = directory_editor_buffers_for_directory(file_name_directory(target))
            old = current_buffer()

            do k = 1, size(buffers)
                call set_buffer(buffers(k), st)
                call revert_buffer(status)
                if (.not. status%ok()) exit
            end do

            call set_buffer(old, st)
        end if
    end subroutine directory_editor_do_create_files

    !> `directory_editor_copy_file' as a file creator.  Not a port.
    recursive subroutine copy_creator(from, to, ok_flag, status)
        character(len=*), intent(in) :: from, to
        logical, intent(in) :: ok_flag
        type(status_type), intent(out) :: status

        call directory_editor_copy_file(from, to, ok_flag, status)
    end subroutine copy_creator

    !> `directory_editor_rename_file' as a file creator.  Not a port.
    recursive subroutine rename_creator(from, to, ok_flag, status)
        character(len=*), intent(in) :: from, to
        logical, intent(in) :: ok_flag
        type(status_type), intent(out) :: status

        call directory_editor_rename_file(from, to, ok_flag, status)
    end subroutine rename_creator

    !> `directory_editor_hardlink' as a file creator.  Not a port.
    recursive subroutine hardlink_creator(from, to, ok_flag, status)
        character(len=*), intent(in) :: from, to
        logical, intent(in) :: ok_flag
        type(status_type), intent(out) :: status

        call directory_editor_hardlink(from, to, ok_flag, status)
    end subroutine hardlink_creator

    !> `directory_editor_make_relative_symlink' as a file creator.  Not a port.
    recursive subroutine relsymlink_creator(from, to, ok_flag, status)
        character(len=*), intent(in) :: from, to
        logical, intent(in) :: ok_flag
        type(status_type), intent(out) :: status

        call directory_editor_make_relative_symlink(from, to, status, ok_flag)
    end subroutine relsymlink_creator

    module procedure directory_editor_do_copy
    logical :: saved_dereference
    type(choice_setting_type) :: saved_recursive

    ! (let ((dired-recursive-copies dired-recursive-copies)
    !       (dired-copy-dereference (if (equal arg '(4)) ...))) ...)
    saved_recursive = directory_editor_recursive_copies
    saved_dereference = directory_editor_copy_dereference

    if (present(invert_dereference)) then
        if (invert_dereference) directory_editor_copy_dereference = .not. directory_editor_copy_dereference
    end if

    call directory_editor_do_create_files("copy", copy_creator, DIRECTORY_EDITOR_CREATOR_COPY, "Copy", argument_kind, argument_n, &
                                          directory_editor_keep_marker_copy, status)
    directory_editor_recursive_copies = saved_recursive
    directory_editor_copy_dereference = saved_dereference
    end procedure directory_editor_do_copy

    !> Copy all marked (or next ARGUMENT) files, or copy the current file.
    !> ARGUMENT has to be numeric for above functionality.  See
    !> `directory_editor_get_marked_files' for more details.
    !>
    !> When operating on just the current file, prompt for the new name.
    !>
    !> When operating on multiple or marked files, prompt for a target
    !> directory, and make the new copies in that directory, with the
    !> same names as the original files.  The initial suggestion for the
    !> target directory is the Dired buffer's current directory (or, if
    !> `directory_editor_do_what_i_mean_target' is set, the current directory of a
    !> neighboring Dired window).
    !>
    !> If `directory_editor_copy_preserve_time' is true, this command preserves
    !> the modification time of each old file in the copy, similar to
    !> the "-p" option for the "cp" shell command.
    !>
    !> The `directory_editor_keep_marker_copy' user option controls how this
    !> command handles file marking.  The default is to mark all new
    !> copies of files with a "C" mark.
    !>
    !> This command copies symbolic links by creating new ones,
    !> similar to the "-d" option for the "cp" shell command.
    !> But if `directory_editor_copy_dereference' is true, the symbolic
    !> links are dereferenced and then copied, similar to the "-L"
    !> option for the "cp" shell command.  If ARGUMENT is
    !> \[universal_argument] alone, the inverted value of
    !> `directory_editor_copy_dereference' will be used.
    !>
    !> Also see `directory_editor_do_revert_buffer'.  Port of
    !> dired-aux.el:dired-do-copy (interactive; `directory_editor_copy_how_to_fn' is
    !> deferred).
    recursive subroutine execute_directory_editor_do_copy(status)
        type(status_type), intent(out) :: status
        integer :: kind
        integer(int64) :: n
        logical :: invert

        call aux_prefix_kind(kind, n)
        invert = .false.
        if (current_prefix_argument%kind == PREFIX_RAW) invert = prefix_numeric_value(current_prefix_argument) == 4
        call directory_editor_do_copy(kind, n, status, invert)
    end subroutine execute_directory_editor_do_copy

    module procedure directory_editor_do_symlink
    call directory_editor_do_create_files("symlink", symlink_creator, DIRECTORY_EDITOR_CREATOR_SYMLINK, "Symlink", argument_kind, &
                                          argument_n, &
                                          directory_editor_keep_marker_symlink, status)
    end procedure directory_editor_do_symlink

    !> Make symbolic links to current file or all marked (or next ARGUMENT) files.
    !> When operating on just the current file, you specify the new name.
    !> When operating on multiple or marked files, you specify a directory
    !> and new symbolic links are made in that directory
    !> with the same names that the files currently have.  The default
    !> suggested for the target directory depends on the value of
    !> `directory_editor_do_what_i_mean_target', which see.
    !>
    !> For relative symlinks, use \[directory_editor_do_relsymlink].
    !>
    !> Also see `directory_editor_do_revert_buffer'.  Port of
    !> dired-aux.el:dired-do-symlink (interactive).
    recursive subroutine execute_directory_editor_do_symlink(status)
        type(status_type), intent(out) :: status
        integer :: kind
        integer(int64) :: n

        call aux_prefix_kind(kind, n)
        call directory_editor_do_symlink(kind, n, status)
    end subroutine execute_directory_editor_do_symlink

    module procedure directory_editor_do_relsymlink
    call directory_editor_do_create_files("relsymlink", relsymlink_creator, DIRECTORY_EDITOR_CREATOR_OTHER, "RelSymLink", &
                                          argument_kind, argument_n, &
                                          directory_editor_keep_marker_relsymlink, status)
    end procedure directory_editor_do_relsymlink

    !> Relative symlink all marked (or next ARGUMENT) files into a directory.
    !> Otherwise make a relative symbolic link to the current file.
    !> This creates relative symbolic links like
    !>
    !>     foo -> ../bar/foo
    !>
    !> not absolute ones like
    !>
    !>     foo -> /ugly/file/name/that/may/change/any/day/bar/foo
    !>
    !> For absolute symlinks, use \[directory_editor_do_symlink].  Port of
    !> dired-aux.el:dired-do-relsymlink (interactive).
    recursive subroutine execute_directory_editor_do_relsymlink(status)
        type(status_type), intent(out) :: status
        integer :: kind
        integer(int64) :: n

        call aux_prefix_kind(kind, n)
        call directory_editor_do_relsymlink(kind, n, status)
    end subroutine execute_directory_editor_do_relsymlink

    module procedure directory_editor_do_hardlink
    call directory_editor_do_create_files("hardlink", hardlink_creator, DIRECTORY_EDITOR_CREATOR_OTHER, "Hardlink", argument_kind, &
                                          argument_n, &
                                          directory_editor_keep_marker_hardlink, status)
    end procedure directory_editor_do_hardlink

    !> Add names (hard links) current file or all marked (or next ARGUMENT) files.
    !> When operating on just the current file, you specify the new name.
    !> When operating on multiple or marked files, you specify a directory
    !> and new hard links are made in that directory
    !> with the same names that the files currently have.  The default
    !> suggested for the target directory depends on the value of
    !> `directory_editor_do_what_i_mean_target', which see.
    !>
    !> Also see `directory_editor_do_revert_buffer'.  Port of
    !> dired-aux.el:dired-do-hardlink (interactive).
    recursive subroutine execute_directory_editor_do_hardlink(status)
        type(status_type), intent(out) :: status
        integer :: kind
        integer(int64) :: n

        call aux_prefix_kind(kind, n)
        call directory_editor_do_hardlink(kind, n, status)
    end subroutine execute_directory_editor_do_hardlink

    module procedure directory_editor_do_rename
    type(string_type), allocatable :: files(:)
    character(len=:), allocatable :: name
    integer :: k

    call directory_editor_get_marked_files(files, status, argument_kind=argument_kind, argument_n=argument_n)
    if (.not. status%ok()) return

    do k = 1, size(files)
        name = file_name_nondirectory(files(k)%text)

        if (name == "." .or. name == "..") then
            call set_error(status, USER_ERROR_CODE, "Can't rename ""."" or "".."" files")
            return
        end if
    end do

    call directory_editor_do_create_files("move", rename_creator, DIRECTORY_EDITOR_CREATOR_OTHER, "Move", argument_kind, &
                                          argument_n, &
                                          directory_editor_keep_marker_rename, status, op1="Rename")
    end procedure directory_editor_do_rename

    !> Rename current file or all marked (or next ARGUMENT) files.
    !> When renaming just the current file, you specify the new name.
    !> When renaming multiple or marked files, you specify a directory.
    !> This command also renames any buffers that are visiting the files.
    !> The default suggested for the target directory depends on the value
    !> of `directory_editor_do_what_i_mean_target', which see.
    !>
    !> Also see `directory_editor_do_revert_buffer'.  Port of
    !> dired-aux.el:dired-do-rename (interactive; `directory_editor_version_control_rename_file' is
    !> deferred).
    recursive subroutine execute_directory_editor_do_rename(status)
        type(status_type), intent(out) :: status
        integer :: kind
        integer(int64) :: n

        call aux_prefix_kind(kind, n)
        call directory_editor_do_rename(kind, n, status)
    end subroutine execute_directory_editor_do_rename

    ! ------------------------------------------------------------------
    ! Operate on files matched by regexp
    ! ------------------------------------------------------------------

    !> Replace first match of REGULAR_EXPRESSION in STRING with NEWTEXT (NEW; FOUND
    !> false if it does not match).  Port of
    !> dired.el:dired-string-replace-match (LITERAL and GLOBAL .false.).
    recursive subroutine directory_editor_string_replace_match(regular_expression, string, newtext, new, found, status)
        character(len=*), intent(in) :: regular_expression, string, newtext
        character(len=:), allocatable, intent(out) :: new
        logical, intent(out) :: found
        type(status_type), intent(out) :: status

        new = ""
        found = string_match(regular_expression, string, status) > 0

        if (.not. status%ok() .or. .not. found) then
            found = .false.
            return
        end if

        call replace_match(newtext, status, fixedcase=.true., literal=.false., string=string, result=new)
        if (.not. status%ok()) found = .false.
    end subroutine directory_editor_string_replace_match

    !> The name constructor of `directory_editor_do_create_files_regular_expression'.
    !> Port of part of dired-aux.el:dired-do-create-files-regexp.
    recursive subroutine regular_expression_name_constructor(from, to, skip, status)
        character(len=*), intent(in) :: from
        character(len=:), allocatable, intent(out) :: to
        logical, intent(out) :: skip
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: new, prompt
        logical :: found, answer

        skip = .true.
        to = ""

        if (regular_expression_whole_name) then
            ! easy (but rare) case
            call directory_editor_string_replace_match(regular_expression_regular_expression, from, regular_expression_newname, &
                                                       new, found, &
                                                       status)
            if (.not. status%ok()) return

            if (found) then
                to = new
                prompt = regular_expression_operation//substitute_quotes(" `")//from//substitute_quotes("' to `")//to// &
                         substitute_quotes("'?")
                call directory_editor_query(regular_expression_query, prompt, answer, status, &
                                            help_procedure=regular_expression_help)
                if (.not. status%ok()) return
                skip = .not. answer
            else
                call directory_editor_log(regular_expression_operation//": "//from//" did not match regexp " &
                                          //regular_expression_regular_expression//LF)
            end if
        else
            ! not whole-name, replace non-directory part only
            call directory_editor_string_replace_match(regular_expression_regular_expression, file_name_nondirectory(from), &
                                                       regular_expression_newname, new, found, &
                                                       status)
            if (.not. status%ok()) return

            if (found) then
                ! nil means there was no match
                to = aux_expand_in(new, file_name_directory(from))
                prompt = regular_expression_operation//substitute_quotes(" `")//directory_editor_make_relative(from)// &
                         substitute_quotes("' to `")//directory_editor_make_relative(to)//substitute_quotes("'?")
                call directory_editor_query(regular_expression_query, prompt, answer, status, &
                                            help_procedure=regular_expression_help)
                if (.not. status%ok()) return
                skip = .not. answer
            else
                call directory_editor_log(regular_expression_operation//": "//file_name_nondirectory(from) &
                                          //" did not match regexp "// &
                                          regular_expression_regular_expression//LF)
            end if
        end if
    end subroutine regular_expression_name_constructor

    !> Create a new file for each marked file using regexps.
    !> FILE_CREATOR and OPERATION as in `directory_editor_create_files'.
    !> ARGUMENT as in `directory_editor_get_marked_files'.
    !> Matches each marked file against REGULAR_EXPRESSION and constructs the new
    !> filename from NEWNAME (like in function `replace_match').
    !> WHOLE_NAME means match/replace the whole file name instead of only
    !> the non-directory part of the file.
    !> MARKER_CHARACTER as in `directory_editor_create_files'.  Port of
    !> dired-aux.el:dired-do-create-files-regexp.
    recursive subroutine directory_editor_do_create_files_regular_expression(file_creator, creator_kind, operation, argument_kind, &
                                                                             argument_n, &
                                                                  regular_expression, newname, whole_name, marker_character, status)
        procedure(directory_editor_file_creator_procedure) :: file_creator
        integer, intent(in) :: creator_kind, argument_kind
        character(len=*), intent(in) :: operation, regular_expression, newname
        integer(int64), intent(in) :: argument_n
        logical, intent(in) :: whole_name
        character, intent(in) :: marker_character
        type(status_type), intent(out) :: status
        type(string_type), allocatable :: file_names(:)
        character(len=:), allocatable :: lower

        call directory_editor_get_marked_files(file_names, status, argument_kind=argument_kind, argument_n=argument_n)
        if (.not. status%ok()) return
        lower = downcase_string(operation)
        regular_expression_help = substitute_command_keys("Type \`SPC' or \`y' to ")//lower// &
                                  substitute_command_keys(" one match, \`DEL' or \`n' to skip to next,"//LF//"\`!' to ")//lower// &
                                  " all remaining matches with no more questions."
        regular_expression_operation = operation
        regular_expression_regular_expression = regular_expression
        regular_expression_newname = newname
        regular_expression_whole_name = whole_name
        regular_expression_query = 0
        call directory_editor_create_files(file_creator, creator_kind, operation, file_names, regular_expression_name_constructor, &
                                           marker_character, &
                                           status)
    end subroutine directory_editor_do_create_files_regular_expression

    !> Prompt user about performing OPERATION.  Read the REGULAR_EXPRESSION and
    !> NEWNAME, and the ARGUMENT and WHOLE_NAME of the regexp commands from the
    !> prefix argument.  Port of dired-aux.el:dired-mark-read-regexp.
    recursive subroutine directory_editor_mark_read_regular_expression(operation, regular_expression, newname, argument_kind, &
                                                                       argument_n, &
                                                                       whole_name, status)
        character(len=*), intent(in) :: operation
        character(len=:), allocatable, intent(out) :: regular_expression, newname
        integer, intent(out) :: argument_kind
        integer(int64), intent(out) :: argument_n
        logical, intent(out) :: whole_name
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: abs

        whole_name = prefix_numeric_value(current_prefix_argument) == 0 .and. current_prefix_argument%kind /= PREFIX_NONE

        if (whole_name) then
            argument_kind = DIRECTORY_EDITOR_ARGUMENT_NONE
            argument_n = 0
            abs = "Abs. "
        else
            call aux_prefix_kind(argument_kind, argument_n)
            abs = ""
        end if

        newname = ""
        call read_regular_expression(abs//operation//" from (regexp): ", regular_expression, status, &
                                     history="directory_editor_regular_expression_history")
        if (.not. status%ok()) return
        call read_string(abs//operation//" "//regular_expression//" to: ", newname, status)
    end subroutine directory_editor_mark_read_regular_expression

    module procedure directory_editor_do_rename_regular_expression
    call directory_editor_do_create_files_regular_expression(rename_creator, DIRECTORY_EDITOR_CREATOR_OTHER, "Rename", &
                                                             argument_kind, argument_n, &
                                                             regular_expression, newname, &
                                                             whole_name, directory_editor_keep_marker_rename, status)
    end procedure directory_editor_do_rename_regular_expression

    !> Rename selected files whose names match REGULAR_EXPRESSION to NEWNAME.
    !>
    !> With non-zero prefix argument ARGUMENT, the command operates on the next ARGUMENT
    !> files.  Otherwise, it operates on all the marked files, or the current
    !> file if none are marked.
    !>
    !> As each match is found, the user must type a character saying
    !>   what to do with it.  For directions, type \[help_command] at that time.
    !> NEWNAME may contain \<n> or \& as in `query_replace_regular_expression'.
    !> REGULAR_EXPRESSION defaults to the last regexp used.
    !>
    !> With a zero prefix arg, renaming by regexp affects the absolute file name.
    !> Normally, only the non-directory part of the file name is used and changed.
    !> Port of dired-aux.el:dired-do-rename-regexp (interactive).
    recursive subroutine execute_directory_editor_do_rename_regular_expression(status)
        type(status_type), intent(out) :: status

        call regular_expression_command("Rename", status)
    end subroutine execute_directory_editor_do_rename_regular_expression

    !> Read the arguments of a regexp command (OPERATION) and run it.
    !> Not a port.
    recursive subroutine regular_expression_command(operation, status)
        character(len=*), intent(in) :: operation
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: regular_expression, newname
        integer :: kind
        integer(int64) :: n
        logical :: whole

        call directory_editor_mark_read_regular_expression(operation, regular_expression, newname, kind, n, whole, status)
        if (.not. status%ok()) return

        select case (operation)
        case ("Rename")
            call directory_editor_do_rename_regular_expression(regular_expression, newname, kind, n, whole, status)
        case ("Copy")
            call directory_editor_do_copy_regular_expression(regular_expression, newname, kind, n, whole, status)
        case ("HardLink")
            call directory_editor_do_hardlink_regular_expression(regular_expression, newname, kind, n, whole, status)
        case ("SymLink")
            call directory_editor_do_symlink_regular_expression(regular_expression, newname, kind, n, whole, status)
        case default
            call directory_editor_do_relsymlink_regular_expression(regular_expression, newname, kind, n, whole, status)
        end select
    end subroutine regular_expression_command

    module procedure directory_editor_do_copy_regular_expression
    type(choice_setting_type) :: saved

    ! No recursive copies.
    saved = directory_editor_recursive_copies
    directory_editor_recursive_copies = .false.

    if (directory_editor_copy_preserve_time) then
        call directory_editor_do_create_files_regular_expression(copy_creator, DIRECTORY_EDITOR_CREATOR_COPY, "Copy [-p]", &
                                                                 argument_kind, argument_n, &
                                                                 regular_expression, &
                                                                 newname, whole_name, directory_editor_keep_marker_copy, status)
    else
        call directory_editor_do_create_files_regular_expression(copy_creator, DIRECTORY_EDITOR_CREATOR_COPY, "Copy", &
                                                                 argument_kind, argument_n, &
                                                                 regular_expression, newname, &
                                                                 whole_name, directory_editor_keep_marker_copy, status)
    end if

    directory_editor_recursive_copies = saved
    end procedure directory_editor_do_copy_regular_expression

    !> Copy selected files whose names match REGULAR_EXPRESSION to NEWNAME.
    !> See function `directory_editor_do_rename_regular_expression' for more info.  Port of
    !> dired-aux.el:dired-do-copy-regexp (interactive).
    recursive subroutine execute_directory_editor_do_copy_regular_expression(status)
        type(status_type), intent(out) :: status

        call regular_expression_command("Copy", status)
    end subroutine execute_directory_editor_do_copy_regular_expression

    module procedure directory_editor_do_hardlink_regular_expression
    call directory_editor_do_create_files_regular_expression(add_name_creator, DIRECTORY_EDITOR_CREATOR_OTHER, "HardLink", &
                                                             argument_kind, argument_n, &
                                                             regular_expression, &
                                                             newname, whole_name, directory_editor_keep_marker_hardlink, status)
    end procedure directory_editor_do_hardlink_regular_expression

    !> Hardlink selected files whose names match REGULAR_EXPRESSION to NEWNAME.
    !> See function `directory_editor_do_rename_regular_expression' for more info.  Port of
    !> dired-aux.el:dired-do-hardlink-regexp (interactive).
    recursive subroutine execute_directory_editor_do_hardlink_regular_expression(status)
        type(status_type), intent(out) :: status

        call regular_expression_command("HardLink", status)
    end subroutine execute_directory_editor_do_hardlink_regular_expression

    module procedure directory_editor_do_symlink_regular_expression
    call directory_editor_do_create_files_regular_expression(symlink_creator, DIRECTORY_EDITOR_CREATOR_SYMLINK, "SymLink", &
                                                             argument_kind, argument_n, &
                                                             regular_expression, &
                                                             newname, whole_name, directory_editor_keep_marker_symlink, status)
    end procedure directory_editor_do_symlink_regular_expression

    !> Symlink selected files whose names match REGULAR_EXPRESSION to NEWNAME.
    !> See function `directory_editor_do_rename_regular_expression' for more info.  Port of
    !> dired-aux.el:dired-do-symlink-regexp (interactive).
    recursive subroutine execute_directory_editor_do_symlink_regular_expression(status)
        type(status_type), intent(out) :: status

        call regular_expression_command("SymLink", status)
    end subroutine execute_directory_editor_do_symlink_regular_expression

    module procedure directory_editor_do_relsymlink_regular_expression
    call directory_editor_do_create_files_regular_expression(relsymlink_creator, DIRECTORY_EDITOR_CREATOR_OTHER, "RelSymLink", &
                                                             argument_kind, &
                                                             argument_n, regular_expression, &
                                                             newname, whole_name, directory_editor_keep_marker_relsymlink, status)
    end procedure directory_editor_do_relsymlink_regular_expression

    !> RelSymlink all marked files containing REGULAR_EXPRESSION to NEWNAME.
    !> See functions `directory_editor_do_rename_regular_expression' and `directory_editor_do_relsymlink'
    !> for more info.  Port of dired-aux.el:dired-do-relsymlink-regexp
    !> (interactive).
    recursive subroutine execute_directory_editor_do_relsymlink_regular_expression(status)
        type(status_type), intent(out) :: status

        call regular_expression_command("RelSymLink", status)
    end subroutine execute_directory_editor_do_relsymlink_regular_expression

    ! ------------------------------------------------------------------
    ! Change case of file names
    ! ------------------------------------------------------------------

    !> The name constructor of `directory_editor_create_files_non_directory'.
    !> Port of part of dired-aux.el:dired-create-files-non-directory.
    recursive subroutine non_directory_constructor(from, to, skip, status)
        character(len=*), intent(in) :: from
        character(len=:), allocatable, intent(out) :: to
        logical, intent(out) :: skip
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: prompt
        logical :: answer

        if (non_directory_upcase) then
            to = file_name_directory(from)//upcase_string(file_name_nondirectory(from))
        else
            to = file_name_directory(from)//downcase_string(file_name_nondirectory(from))
        end if

        prompt = non_directory_operation//substitute_quotes(" `")//directory_editor_make_relative(from)// &
                 substitute_quotes("' to `")// &
                 directory_editor_make_relative(to)//substitute_quotes("'")
        call directory_editor_query(non_directory_query, prompt, answer, status, help_procedure=non_directory_help)
        skip = .not. (status%ok() .and. answer)
    end subroutine non_directory_constructor

    !> Perform `directory_editor_rename_file' on the non-directory part of marked
    !> files using upcase (UPCASE) or downcase, with query for each file.
    !> OPERATION like in `directory_editor_create_files', ARGUMENT as in
    !> `directory_editor_get_marked_files'.  Port of
    !> dired-aux.el:dired-create-files-non-directory with
    !> dired-rename-non-directory.
    recursive subroutine directory_editor_rename_non_directory(upcase, operation, argument_kind, argument_n, status)
        logical, intent(in) :: upcase
        character(len=*), intent(in) :: operation
        integer, intent(in) :: argument_kind
        integer(int64), intent(in) :: argument_n
        type(status_type), intent(out) :: status
        type(string_type), allocatable :: file_names(:)
        character(len=:), allocatable :: lower

        call directory_editor_get_marked_files(file_names, status, argument_kind=argument_kind, argument_n=argument_n)
        if (.not. status%ok()) return
        lower = downcase_string(operation)
        non_directory_help = substitute_command_keys("Type \`SPC' or \`y' to ")//lower// &
                             substitute_command_keys(" one file, \`DEL' or \`n' to skip to next,"//LF//"\`!' to ")// &
                             lower//" all remaining matches with no more questions."
        non_directory_upcase = upcase
        non_directory_operation = operation
        non_directory_query = 0
        call directory_editor_create_files(rename_creator, DIRECTORY_EDITOR_CREATOR_OTHER, operation, file_names, &
                                           non_directory_constructor, &
                                           directory_editor_keep_marker_rename, status)
    end subroutine directory_editor_rename_non_directory

    module procedure directory_editor_upcase
    call directory_editor_rename_non_directory(.true., "Rename upcase", argument_kind, argument_n, status)
    end procedure directory_editor_upcase

    !> Rename all marked (or next ARGUMENT) files to upper case.  Port of
    !> dired-aux.el:dired-upcase (interactive).
    recursive subroutine execute_directory_editor_upcase(status)
        type(status_type), intent(out) :: status
        integer :: kind
        integer(int64) :: n

        call aux_prefix_kind(kind, n)
        call directory_editor_upcase(kind, n, status)
    end subroutine execute_directory_editor_upcase

    module procedure directory_editor_downcase
    call directory_editor_rename_non_directory(.false., "Rename downcase", argument_kind, argument_n, status)
    end procedure directory_editor_downcase

    !> Rename all marked (or next ARGUMENT) files to lower case.  Port of
    !> dired-aux.el:dired-downcase (interactive).
    recursive subroutine execute_directory_editor_downcase(status)
        type(status_type), intent(out) :: status
        integer :: kind
        integer(int64) :: n

        call aux_prefix_kind(kind, n)
        call directory_editor_downcase(kind, n, status)
    end subroutine execute_directory_editor_downcase

    ! ------------------------------------------------------------------
    ! Creating files, viewing them and their types
    ! ------------------------------------------------------------------

    module procedure directory_editor_create_empty_file
    character(len=:), allocatable :: expanded, new
    integer(int64) :: position

    call expand_file_name(file, expanded, status)
    if (.not. status%ok()) return

    if (file_exists(expanded)) then
        call set_error(status, GENERIC_ERROR_CODE, "Cannot create file "//expanded//": file exists")
        return
    end if

    new = find_topmost_parent_directory(expanded)
    call create_empty_file(file, status, parents=.true.)
    if (.not. status%ok()) return

    if (len(new) > 0) then
        call directory_editor_add_file(new, status)
        if (.not. status%ok()) return
        position = directory_editor_move_to_filename(status)
    end if
    end procedure directory_editor_create_empty_file

    !> Create an empty file called FILE.
    !> Parent directories of FILE are created as needed.
    !> Add an entry in the Dired buffer for the topmost new parent
    !> directory of FILE, if created, otherwise for the new file.
    !> If user option `directory_editor_create_empty_file_in_current_directory' is
    !> true, act on the current subdirectory by default, otherwise act on
    !> the default directory by default.
    !> If FILE already exists, an error condition occurs.  Port of
    !> dired-aux.el:dired-create-empty-file (interactive).
    recursive subroutine execute_directory_editor_create_empty_file(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: file, directory

        if (directory_editor_create_empty_file_in_current_directory) then
            directory = directory_editor_current_directory(status)
            if (.not. status%ok()) return
            call read_file_name("Create empty file: ", file, status, directory=directory)
        else
            call read_file_name("Create empty file: ", file, status)
        end if

        if (.not. status%ok()) return
        call directory_editor_create_empty_file(file, status)
    end subroutine execute_directory_editor_create_empty_file

    module procedure directory_editor_view_file
    character(len=:), allocatable :: file
    integer :: k
    logical :: shown

    call directory_editor_get_file_for_visit(file, status)
    if (.not. status%ok()) return

    if (is_file_directory(file)) then
        shown = .false.
        k = aux_local_index()

        if (k > 0) then
            if (allocated(locals(k)%subdirectory_table)) then
                if (size(locals(k)%subdirectory_table) > 1) shown = directory_editor_goto_subdirectory(file) /= 0
            end if
        end if

        if (.not. shown) call directory_editor(file, status)
    else
        call view_file(file, status)
    end if
    end procedure directory_editor_view_file

    !> In Dired, examine a file in view mode, returning to Dired when done.
    !> When file is a directory, show it in this buffer if it is inserted.
    !> Otherwise, display it in another buffer.  Port of
    !> dired.el:dired-view-file (interactive).
    recursive subroutine execute_directory_editor_view_file(status)
        type(status_type), intent(out) :: status

        call directory_editor_view_file(status)
    end subroutine execute_directory_editor_view_file

    module procedure directory_editor_show_file_type
    type(handle_type) :: old, temporary
    type(value_type) :: res, arguments
    character(len=:), allocatable :: directory, text
    type(status_type) :: st

    directory = aux_default_directory()
    old = current_buffer()
    call get_or_make_buffer(generate_new_buffer_name(" *temp*"), temporary, status)
    if (.not. status%ok()) return
    call set_buffer(temporary, status)
    if (.not. status%ok()) return
    buffers(current)%directory = directory

    if (deref_symlinks) then
        arguments = list(value_type(text="-L"), value_type(text="--"), value_type(text=file))
    else
        arguments = list(value_type(text="--"), value_type(text=file))
    end if

    call execute_process("file", res, status, destination=value_type(.true.), arguments=arguments)

    if (status%ok()) then
        if (is_beginning_of_line() .and. point() > point_minimum()) call delete_character(-1_int64, st)
        call buffer_substring(point_minimum(), point_maximum(), text, st)
    end if

    call set_buffer(old, st)
    call close_buffer(temporary, st)
    if (status%ok()) call message(text)
    end procedure directory_editor_show_file_type

    !> Print the type of FILE, according to the `file' command.
    !> If you give a prefix to this command, and FILE is a symbolic
    !> link, then the command will print the type of the target of the link
    !> instead.  Port of dired-aux.el:dired-show-file-type (interactive).
    recursive subroutine execute_directory_editor_show_file_type(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: file
        logical :: found

        call directory_editor_get_filename(file, found, status, DIRECTORY_EDITOR_LOCALP_RELATIVE)
        if (.not. status%ok()) return

        if (.not. found) then
            call set_error(status, GENERIC_ERROR_CODE, "No file on this line")
            return
        end if

        call directory_editor_show_file_type(file, current_prefix_argument%kind /= PREFIX_NONE, status)
    end subroutine execute_directory_editor_show_file_type

    !> Compare file at point with FILE using `diff'.
    !> If called interactively, prompt for FILE.
    !> If the mark is active in Transient Mark mode, use the file at the mark
    !> as the default for FILE.  (That's the mark set by C-SPC,
    !> not by Dired's m command.)
    !> If the file at point has a backup file, use that as the default FILE.
    !> If the file at point is a backup file, use its original, if that exists
    !> and can be found.  Note that customizations of `backup_directories'
    !> and `make_backup_file_name_function' change where this function searches
    !> for the backup file, and affect its ability to find the original of a
    !> backup file.
    !>
    !> FILE is the first argument given to the `diff' function.  The file at
    !> point is the second argument given to `diff'.
    !>
    !> With prefix arg, prompt for second argument SWITCHES, which is
    !> the string of command switches used as the third argument of `diff'.
    !> Port of dired-aux.el:dired-diff (the M-n defaults of the prompt are
    !> deferred: `read_file_name' takes one default).
    recursive subroutine execute_directory_editor_diff(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: current_file, oldf, file_at_mark, default_file, default, target_directory, file, sw, prompt
        integer(int64) :: original_point, m
        logical :: found, has_oldf, separate_directory, has_default
        type(diff_operand_type) :: a, b
        type(status_type) :: st

        call directory_editor_get_filename(current_file, found, status, DIRECTORY_EDITOR_LOCALP_RELATIVE)
        if (.not. status%ok()) return

        if (.not. found) then
            call set_error(status, GENERIC_ERROR_CODE, "No file on this line")
            return
        end if

        ! Get the latest existing backup file or its original.
        if (is_backup_file_name(current_file)) then
            oldf = file_name_sans_versions(current_file)
            has_oldf = .true.
        else
            call diff_latest_backup_file(current_file, oldf, has_oldf)
        end if

        ! Get the file at the mark.
        file_at_mark = ""
        m = mark_position()

        if (transient_mark_mode%value .and. buffers(current)%mark_active .and. m /= 0) then
            original_point = point()
            call goto_character(m)
            call directory_editor_get_filename(file_at_mark, found, st, DIRECTORY_EDITOR_LOCALP_RELATIVE, &
                                               no_error_if_not_filep=.true.)
            if (.not. found) file_at_mark = ""
            call goto_character(original_point)
        end if

        separate_directory = .false.
        if (has_oldf) separate_directory = aux_default_directory() /= file_name_directory(oldf)
        default_file = file_at_mark

        if (len(default_file) == 0 .and. has_oldf) then
            ! If the file with which to compare doesn't exist, or we cannot
            ! intuit it, we forget that name and don't show it as the
            ! default, as an indication to the user that she should type
            ! the file name.
            if (is_file_readable(oldf)) then
                if (separate_directory) then
                    default_file = file_relative_name(oldf)
                else
                    default_file = file_name_nondirectory(oldf)
                end if
            end if
        end if

        ! Use it as default if it's not the same as the current file, and
        ! the target dir is current or there is a default file.
        has_default = default_file /= current_file .and. &
                      (directory_editor_do_what_i_mean_target_directory() == directory_editor_current_directory(st) .or. len( &
                       default_file) > 0)
        default = ""
        if (has_default) default = default_file

        if (has_default) then
            if (separate_directory) then
                target_directory = file_name_directory(default)
            else
                target_directory = directory_editor_current_directory(st)
            end if
        else
            target_directory = directory_editor_do_what_i_mean_target_directory()
        end if

        prompt = "Diff "//current_file//" with"
        if (has_default) prompt = prompt//" (default "//default//")"
        prompt = prompt//": "
        ! A relative backup's directory is nil: `default_directory'.
        if (len(target_directory) == 0) target_directory = aux_default_directory()

        if (has_default) then
            call read_file_name(prompt, file, status, directory=target_directory, default_filename=default, &
                                mustmatch=REQUIRE_MATCH_YES)
        else
            call read_file_name(prompt, file, status, directory=target_directory, mustmatch=REQUIRE_MATCH_YES)
        end if

        if (.not. status%ok()) return
        has_default = current_prefix_argument%kind /= PREFIX_NONE

        if (has_default) then
            call read_string("Options for diff: ", sw, status, initial_input=diff_switches)
            if (.not. status%ok()) return
        end if

        if (aux_expand(file) == aux_expand(current_file) .or. &
            (is_file_directory(file) .and. aux_expand_in(current_file, file) == aux_expand(current_file))) then
            call set_error(status, GENERIC_ERROR_CODE, "Attempt to compare the file to itself")
            return
        end if

        if (is_backup_file_name(current_file) .and. file == file_name_sans_versions(current_file)) then
            a%file = current_file
            b%file = file
        else
            a%file = file
            b%file = current_file
        end if

        if (has_default) then
            call diff(a, b, status, sw)
        else
            call diff(a, b, status)
        end if
    end subroutine execute_directory_editor_diff

    !> Diff this file with its backup file or vice versa.
    !> Uses the latest backup, if there are several numerical backups.
    !> If this file is a backup, diff it with its original.
    !> The backup file is the first file given to `diff'.
    !> With prefix arg, prompt for argument SWITCHES which is options for `diff'.
    !> Port of dired-aux.el:dired-backup-diff.
    recursive subroutine execute_directory_editor_backup_diff(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: sw, file
        logical :: given, found

        given = current_prefix_argument%kind /= PREFIX_NONE

        if (given) then
            call read_string("Options for diff: ", sw, status, initial_input=diff_switches)
            if (.not. status%ok()) return
        end if

        call directory_editor_get_filename(file, found, status)
        if (.not. status%ok()) return

        if (.not. found) then
            call set_error(status, GENERIC_ERROR_CODE, "No file on this line")
            return
        end if

        if (given) then
            call diff_backup(file, status, sw)
        else
            call diff_backup(file, status)
        end if
    end subroutine execute_directory_editor_backup_diff

    !> The first program named NAME on PATH (as executable-find finds it
    !> with `exec_path' from $PATH), or "" if none.  Not a port.
    function find_on_path(name) result(path)
        character(len=*), intent(in) :: name
        character(len=:), allocatable :: path
        character(len=:), allocatable :: environment, directory
        integer :: length, stat, start, k

        path = ""
        call get_environment_variable("PATH", length=length, status=stat)
        if (stat /= 0 .or. length == 0) return
        allocate (character(len=length) :: environment)
        call get_environment_variable("PATH", environment)
        start = 1

        do while (start <= len(environment) + 1)
            k = index(environment(start:), ":")

            if (k == 0) then
                directory = environment(start:)
                start = len(environment) + 2
            else
                directory = environment(start:start + k - 2)
                start = start + k
            end if

            if (len(directory) == 0) directory = "."

            if (is_file_executable(file_name_as_directory(directory)//name)) then
                if (.not. is_file_directory(file_name_as_directory(directory)//name)) then
                    path = file_name_as_directory(directory)//name
                    return
                end if
            end if
        end do
    end function find_on_path

    !> Add a rule to `directory_editor_compress_file_suffixes'.  Not a port.
    subroutine append_compress_suffix(regular_expression, new_suffix, program)
        character(len=*), intent(in) :: regular_expression, new_suffix, program
        type(directory_editor_compress_suffix_type) :: suffix

        suffix%regular_expression = regular_expression
        suffix%new_suffix = new_suffix
        suffix%program = program
        directory_editor_compress_file_suffixes = [directory_editor_compress_file_suffixes, suffix]
    end subroutine append_compress_suffix

    !> Add a rule with REGULAR_EXPRESSION and COMMAND to TABLE.  Not a port.
    subroutine append_compress_rule(table, regular_expression, command)
        type(directory_editor_compress_rule_type), allocatable, intent(inout) :: table(:)
        character(len=*), intent(in) :: regular_expression, command
        type(directory_editor_compress_rule_type) :: rule

        rule%regular_expression = regular_expression
        rule%command = command
        table = [table, rule]
    end subroutine append_compress_rule

    !> Give the dired-aux.el options that are not set yet their standard
    !> values.  Not a port.
    subroutine initialize_directory_editor_aux_options()
        if (.not. allocated(directory_editor_chown_program)) then
            ! (cond ((executable-find "chown") "chown") ...)
            if (len(find_on_path("chown")) > 0) then
                directory_editor_chown_program = "chown"
            else if (is_file_executable("/usr/sbin/chown")) then
                directory_editor_chown_program = "/usr/sbin/chown"
            else if (is_file_executable("/etc/chown")) then
                directory_editor_chown_program = "/etc/chown"
            else
                directory_editor_chown_program = "chown"
            end if
        end if

        if (.not. allocated(directory_editor_touch_program)) directory_editor_touch_program = "touch"
        if (.not. allocated(directory_editor_compress_file_default_suffix)) directory_editor_compress_file_default_suffix = ""

        if (.not. allocated(directory_editor_compress_directory_default_suffix)) then
            directory_editor_compress_directory_default_suffix = ""
        end if

        if (.not. allocated(directory_editor_compress_file_suffixes)) then
            allocate (directory_editor_compress_file_suffixes(0))
            ! "tar -zxf" isn't used because it's not available on the
            ! Solaris 10 version of tar (obsolete in 2024?).
            ! Same thing on AIX 7.1 (obsolete 2023?) and 7.2 (obsolete 2022?).
            call append_compress_suffix("\.tar\.gz\'", "", "gzip -dc %i | tar -xf -")
            call append_compress_suffix("\.tar\.xz\'", "", "xz -dc %i | tar -xf -")
            call append_compress_suffix("\.tgz\'", "", "gzip -dc %i | tar -xf -")
            call append_compress_suffix("\.gz\'", "", "gzip -d")
            call append_compress_suffix("\.lz\'", "", "lzip -d")
            call append_compress_suffix("\.Z\'", "", "uncompress")
            ! For .z, try gunzip.  It might be an old gzip file, or it might
            ! be from compact? pack? (which?) but gunzip handles both.
            call append_compress_suffix("\.z\'", "", "gzip -d")
            call append_compress_suffix("\.dz\'", "", "dictunzip")
            call append_compress_suffix("\.tbz\'", ".tar", "bunzip2")
            call append_compress_suffix("\.bz2\'", "", "bunzip2")
            call append_compress_suffix("\.xz\'", "", "unxz")
            call append_compress_suffix("\.zip\'", "", "unzip -o -d %o %i")
            call append_compress_suffix("\.tar\.zst\'", "", "unzstd -c %i | tar -xf -")
            call append_compress_suffix("\.tzst\'", "", "unzstd -c %i | tar -xf -")
            call append_compress_suffix("\.zst\'", "", "unzstd --rm")
            call append_compress_suffix("\.7z\'", "", "7z x -aoa -o%o %i")
            ! This item controls naming for compression.
            call append_compress_suffix("\.tar\'", ".tgz", "")
        end if

        if (.not. allocated(directory_editor_compress_file_commands)) then
            allocate (directory_editor_compress_file_commands(0))
            call append_compress_rule(directory_editor_compress_file_commands, "\.gz\'", "gzip -9f %i")
            call append_compress_rule(directory_editor_compress_file_commands, "\.bz2\'", "bzip2 -9f %i")
            call append_compress_rule(directory_editor_compress_file_commands, "\.xz\'", "xz -9f %i")
            call append_compress_rule(directory_editor_compress_file_commands, "\.zst\'", "zstd -qf -19 --rm -o %o %i")
        end if

        if (.not. allocated(directory_editor_compress_files_commands)) then
            allocate (directory_editor_compress_files_commands(0))
            call append_compress_rule(directory_editor_compress_files_commands, "\.tar\.gz\'", "tar -cf - %i | gzip -c9 > %o")
            call append_compress_rule(directory_editor_compress_files_commands, "\.tar\.bz2\'", "tar -cf - %i | bzip2 -c9 > %o")
            call append_compress_rule(directory_editor_compress_files_commands, "\.tar\.xz\'", "tar -cf - %i | xz -c9 > %o")
            call append_compress_rule(directory_editor_compress_files_commands, "\.tar\.zst\'", "tar -cf - %i | zstd -19 -o %o")
            call append_compress_rule(directory_editor_compress_files_commands, "\.tar\.lz\'", "tar -cf - %i | lzip -c9 > %o")
            call append_compress_rule(directory_editor_compress_files_commands, "\.tar\.lzo\'", "tar -cf - %i | lzop -c9 > %o")
            call append_compress_rule(directory_editor_compress_files_commands, "\.zip\'", "zip %o -r --filesync %i")
            call append_compress_rule(directory_editor_compress_files_commands, "\.pax\'", "pax -wf %o %i")
        end if
    end subroutine initialize_directory_editor_aux_options

    module procedure directory_editor_aux_keys
    ! Upper case keys (except !) for operating on the marked files
    call aux_bind("C", "directory_editor_do_copy")
    call aux_bind("G", "directory_editor_do_chgrp")
    call aux_bind("H", "directory_editor_do_hardlink")
    call aux_bind("M", "directory_editor_do_chmod")
    call aux_bind("O", "directory_editor_do_chown")
    call aux_bind("R", "directory_editor_do_rename")
    call aux_bind("S", "directory_editor_do_symlink")
    call aux_bind("T", "directory_editor_do_touch")
    call aux_bind("Y", "directory_editor_do_relsymlink")
    call aux_bind("Z", "directory_editor_do_compress")
    call aux_bind("c", "directory_editor_do_compress_to")
    ! Make all regexp commands share a `%' prefix:
    call aux_bind("% u", "directory_editor_upcase")
    call aux_bind("% l", "directory_editor_downcase")
    call aux_bind("% r", "directory_editor_do_rename_regular_expression")
    call aux_bind("% C", "directory_editor_do_copy_regular_expression")
    call aux_bind("% H", "directory_editor_do_hardlink_regular_expression")
    call aux_bind("% R", "directory_editor_do_rename_regular_expression")
    call aux_bind("% S", "directory_editor_do_symlink_regular_expression")
    call aux_bind("% Y", "directory_editor_do_relsymlink_regular_expression")
    ! Lower keys for commands not operating on all the marked files
    call aux_bind("k", "directory_editor_do_remove_lines")
    call aux_bind("v", "directory_editor_view_file")
    call aux_bind("y", "directory_editor_show_file_type")
    call aux_bind("=", "directory_editor_diff")
    end procedure directory_editor_aux_keys

    !> `regular_expression_search_forward' of REGULAR_EXPRESSION (NOERROR .true.) as a logical.  Not a port.
    logical function aux_regular_expression_search_forward(regular_expression) result(found)
        character(len=*), intent(in) :: regular_expression
        integer(int64) :: position
        type(status_type) :: st

        call regular_expression_search_forward(regular_expression, position, st, noerror=SEARCH_NOERROR)
        found = st%ok() .and. position /= 0
    end function aux_regular_expression_search_forward

    ! ------------------------------------------------------------------
    ! Helpers.  Copies of directory_editor_mode_mod's private ones: gfortran drops
    ! private parent procedures that submodules call.
    ! ------------------------------------------------------------------

    !> The position N lines after this one's start (line-beginning-position N).
    !> Not a port.
    integer(int64) function aux_beginning_of_line(n)
        integer(int64), intent(in), optional :: n

        aux_beginning_of_line = line_beginning_position(n)
    end function aux_beginning_of_line

    !> Move N lines (forward-line), ignoring the shortage.  Not a port.
    subroutine aux_fwd_line(n)
        integer(int64), intent(in) :: n
        integer(int64) :: shortage

        shortage = forward_line(n)
    end subroutine aux_fwd_line

    !> The character after POSITION (default point) as an ASCII character, or
    !> achar(0) when there is none or it is not ASCII.  Not a port.
    character function aux_character_at(position)
        integer(int64), intent(in), optional :: position
        integer(int32) :: c
        logical :: found

        if (present(position)) then
            call character_after(c, found, position)
        else
            call character_after(c, found)
        end if

        aux_character_at = achar(0)
        if (found .and. c >= 0 .and. c < 128) aux_character_at = achar(c)
    end function aux_character_at

    !> The position of marker M (0 if it points nowhere).  Not a port.
    integer(int64) function aux_marker_position(m)
        type(handle_type), intent(in) :: m
        type(status_type) :: st

        call marker_position(m, aux_marker_position, st)
        if (.not. st%ok()) aux_marker_position = 0
    end function aux_marker_position

    !> Free marker M, ignoring a stale handle.  Not a port.
    subroutine aux_destroy_marker_if_live(m)
        type(handle_type), intent(in) :: m
        type(status_type) :: st

        call destroy_marker(m, st)
    end subroutine aux_destroy_marker_if_live

    !> Bind `inhibit_read_only' to true (SAVED keeps the old value).
    !> Not a port.
    subroutine aux_inhibit_read_only_begin(saved)
        type(value_type), intent(out) :: saved
        type(status_type) :: st

        saved = inhibit_read_only()
        call set(value_type(name="inhibit_read_only"), value_type(.true.), st)
    end subroutine aux_inhibit_read_only_begin

    !> Restore `inhibit_read_only' to SAVED.  Not a port.
    subroutine aux_inhibit_read_only_end(saved)
        type(value_type), intent(in) :: saved
        type(status_type) :: st

        call set(value_type(name="inhibit_read_only"), saved, st)
    end subroutine aux_inhibit_read_only_end

    !> The current buffer's `default_directory'.  Not a port.
    function aux_default_directory() result(directory)
        character(len=:), allocatable :: directory

        directory = "./"
        if (current == 0) return
        if (allocated(buffers(current)%directory)) directory = buffers(current)%directory
    end function aux_default_directory

    !> `expand_file_name' as a function (relative to `default_directory').
    !> Not a port.
    function aux_expand(name) result(full)
        character(len=*), intent(in) :: name
        character(len=:), allocatable :: full
        type(status_type) :: st

        call expand_file_name(name, full, st)
        if (.not. st%ok()) full = name
    end function aux_expand

    !> The index of the current buffer's entry in `locals' (0 if none).
    !> Not a port.
    integer function aux_local_index() result(k)
        k = 0
        if (current == 0) return
        if (.not. allocated(locals)) return
        k = findloc(locals%serial, buffers(current)%serial, mask=locals%set, dim=1)
    end function aux_local_index

    !> The kind and number of the prefix argument ("P"): none, a number
    !> (or `-'), or another value (C-u).  Not a port.
    subroutine aux_prefix_kind(argument_kind, argument_n)
        integer, intent(out) :: argument_kind
        integer(int64), intent(out) :: argument_n

        argument_n = 0

        if (current_prefix_argument%kind == PREFIX_NONE) then
            argument_kind = DIRECTORY_EDITOR_ARGUMENT_NONE
        else if (current_prefix_argument%kind == PREFIX_RAW) then
            argument_kind = DIRECTORY_EDITOR_ARGUMENT_OTHER
        else
            argument_kind = DIRECTORY_EDITOR_ARGUMENT_INTEGER
            argument_n = prefix_numeric_value(current_prefix_argument)
        end if
    end subroutine aux_prefix_kind

    !> Bind KEYS to COMMAND in `directory_editor_mode_map'.  Not a port.
    subroutine aux_bind(keys, command)
        character(len=*), intent(in) :: keys, command
        type(status_type) :: st

        call keymap_set(directory_editor_mode_map, keys, command, st)
    end subroutine aux_bind
end submodule directory_editor_auxiliary_mod
