! add_log_mod.f90 -- change log maintenance commands for Emacs.
! Ported from GNU Emacs 31.1 lisp/vc/add-log.el.
!
! Copyright (C) 1985-1986, 1988, 1993-1994, 1997-1998, 2000-2026 Free
! Software Foundation, Inc.
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

!> What Log Edit mode needs of add-log.el: finding the ChangeLog file
!> of a file (`find_change_log'), the buffer for it, or the
!> `*changes to DIR*' buffer standing for one that doesn't exist
!> (`add_log_find_changelog_buffer'), the options naming the user and the
!> date in ChangeLog headers, Change Log mode with its faces, and
!> `change_log_read_subprograms' (for `log_edit_fill_entry').
!>
!> Deferred: adding entries (`add_change_log_entry', C-x 4 a, and
!> `add_change_log_entry_other_window', with `add_log_current_subprogram'
!> and its language support), the commands of `change_log_mode_map'
!> (`add_log_edit_prev_comment', `add_log_edit_next_comment',
!> `change_log_find_file', `change_log_goto_source') and next-error
!> support, `change_log_font_lock_keywords', Change Log mode's
!> filling, indentation, subprogram motion, `show_trailing_whitespace',
!> `adaptive_fill_regexp', `comment_start' and
!> `version_control' settings (those settings are not buffer-local
!> yet), merging change logs, `add_log_mailing_address' as a list,
!> `add_log_time_zone_rule', and the version number options.
module add_log_mod
    use, intrinsic :: iso_fortran_env, only: int64
    use error_mod, only: status_type
    use handle_mod, only: handle_type
    use data_mod, only: string_setting_type, forward_setting
    use hooks_mod, only: hook_type
    use buffer_mod, only: buffers, current, current_buffer, set_buffer, get_or_make_buffer, get_file_buffer, &
                          is_buffer_live
    use fileio_mod, only: expand_file_name, file_name_directory, file_name_nondirectory, is_file_directory, &
                          file_exists, file_name_as_directory, directory_file_name
    use files_mod, only: find_file_noselect, abbreviate_file_name, file_chase_links
    use derived_mod, only: is_derived_mode
    use modes_mod, only: major_modes
    use syntax_mod, only: make_syntax_table, modify_syntax_entry
    use faces_mod, only: named_face_type, display_when, face_attrs
    use buffer_mod, only: fill_column, left_margin
    use indent_mod, only: indent_tabs_mode
    use xdisp_mod, only: tab_width
    use paragraphs_mod, only: paragraph_start, paragraph_separate, page_delimiter
    implicit none(type, external)
    private

    public :: find_change_log, change_log_name, add_log_find_changelog_buffer, add_log_pseudo_changelog_buffer_name, &
              add_log_iso8601_time_string, change_log_mode, init_add_log
    public :: change_log_string_type, change_log_read_subprograms

    !> A string of a list (function names).
    type :: change_log_string_type
        character(len=:), allocatable :: s
    end type change_log_string_type

    !> Regexp matching the file names of unindented ChangeLog entries.
    !> Port of add-log.el change-log-unindented-file-names-re.
    character(len=*), parameter, public :: change_log_unindented_file_names_regexp = "^[*] \([^ ,:(["//achar(10)//"]+\)"

    !> Regexp matching a tag name in change log entries.  Port of
    !> add-log.el change-log-tag-re.
    character(len=*), parameter, public :: change_log_tag_regexp = &
                                           "(\(\(?:\sw\|\s_\)+\(?:[, "//achar(9)//"]+\(?:\sw\|\s_\)+\)*\))"

    !> A function giving (in S) the date for a ChangeLog header.
    abstract interface
        ! Not a port.
        subroutine add_log_time_procedure(s)
            implicit none(type, external)
            character(len=:), allocatable, intent(out) :: s
        end subroutine add_log_time_procedure
    end interface

    !> Name of a change log file for \[add_change_log_entry].  Port of
    !> add-log.el change-log-default-name (unallocated when unset; becomes
    !> buffer-local when `find_change_log' sets it).
    type(string_setting_type), public :: change_log_default_name = string_setting_type(name="change_log_default_name")

    !> Normal hook run by `change_log_mode'.  Port of add-log.el
    !> change-log-mode-hook.
    type(hook_type), target, public :: change_log_mode_hook = hook_type(name="change_log_mode_hook")

    !> Full name of user, for inclusion in ChangeLog daily headers.
    !> This defaults to the value returned by the function `user_full_name'.
    !> Port of add-log.el add-log-full-name (unallocated for the default).
    character(len=:), allocatable, public :: add_log_full_name

    !> Email addresses of user, for inclusion in ChangeLog headers.
    !> This defaults to the value of `user_mail_address'.  Port of add-log.el
    !> add-log-mailing-address (one address; unallocated for the default).
    character(len=:), allocatable, public :: add_log_mailing_address

    !> Function that defines the time format.
    !> For example, `add_log_iso8601_time_string', which gives the
    !> date in international ISO 8601 format,
    !> and `current_time_string' are two valid values.  Port of add-log.el
    !> add-log-time-format.
    procedure(add_log_time_procedure), pointer, public :: add_log_time_format => add_log_iso8601_time_string

    !> If true, don't create ChangeLog files for log entries.
    !> If a ChangeLog file does not already exist, a true value
    !> means to put log entries in a suitably named buffer.  Port of
    !> add-log.el add-log-dont-create-changelog-file.
    logical, public :: add_log_dont_create_changelog_file = .true.

    !> List of files that cause `find_change_log' to stop in containing directory.
    !> This applies if no pre-existing ChangeLog is found.  If empty, then in such
    !> a case simply use the directory containing the changed file.  Port of
    !> add-log.el change-log-directory-files.
    character(len=8), allocatable, public :: change_log_directory_files(:)

    !> Face used to highlight dates in date lines.  Port of add-log.el
    !> change-log-date.
    type(named_face_type), target, public :: change_log_date
    !> Face for highlighting author names.
    type(named_face_type), target, public :: change_log_name_face
    !> Face for highlighting author email addresses.
    type(named_face_type), target, public :: change_log_email
    !> Face for highlighting file names.
    type(named_face_type), target, public :: change_log_file
    !> Face for highlighting parenthesized lists of functions or variables.
    type(named_face_type), target, public :: change_log_list
    !> Face for highlighting conditionals of the form `[...]'.
    type(named_face_type), target, public :: change_log_conditionals
    !> Face for highlighting items of the form `<....>'.
    type(named_face_type), target, public :: change_log_function
    !> Face for highlighting acknowledgments.
    type(named_face_type), target, public :: change_log_acknowledgment

    !> Keymap for Change Log major mode.  Port of add-log.el
    !> change-log-mode-map.
    type(handle_type), public :: change_log_mode_map

    !> Syntax table used while in `change_log_mode'.  Port of add-log.el
    !> change-log-mode-syntax-table.
    type(handle_type), public :: change_log_mode_syntax_table

contains

    !> Read ChangeLog formatted function names at point until END.
    !> Move point to the end of names read and return the function names
    !> as a list of strings.  Port of add-log.el:change-log-read-defuns.
    recursive subroutine change_log_read_subprograms(names, status, end)
        use editfns_mod, only: point, goto_char
        use syntax_mod, only: skip_chars_forward, skip_chars_backward
        use search_mod, only: looking_at, match_end
        use subr_mod, only: match_string_no_properties
        type(change_log_string_type), allocatable, intent(out) :: names(:)
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: end
        character(len=:), allocatable :: tag
        integer(int64) :: moved
        logical :: tagged

        allocate (names(0))

        do
            moved = skip_chars_forward(":"//achar(10)//"[:blank:]", status, end)
            if (.not. status%ok()) return

            if (present(end)) then
                if (point() >= end) exit
            end if

            tagged = looking_at(change_log_tag_regexp, status)
            if (.not. status%ok()) return
            if (.not. tagged) exit
            tag = match_string_no_properties(1)
            call goto_char(match_end(0, status))
            call split_tag(tag)
        end do

        moved = skip_chars_backward(achar(10)//"[:blank:]", status)

    contains

        ! (split-string TAG ",[[:blank:]]*" t), appended to NAMES.
        subroutine split_tag(tag)
            character(len=*), intent(in) :: tag
            integer :: i, j

            i = 1

            do
                j = index(tag(i:), ",")

                if (j == 0) then
                    call append(tag(i:))
                    exit
                end if

                call append(tag(i:i + j - 2))
                i = i + j

                do while (i <= len(tag))
                    if (tag(i:i) /= " " .and. tag(i:i) /= achar(9)) exit
                    i = i + 1
                end do
            end do
        end subroutine split_tag

        subroutine append(name)
            character(len=*), intent(in) :: name
            type(change_log_string_type), allocatable :: grown(:)
            integer :: n

            if (len(name) == 0) return
            n = size(names)
            allocate (grown(n + 1))
            grown(1:n) = names
            grown(n + 1)%s = name
            call move_alloc(grown, names)
        end subroutine append
    end subroutine change_log_read_subprograms

    !> Return the date in ISO 8601 form (YYYY-MM-DD), local time, as S.
    !> Port of add-log.el:add-log-iso8601-time-string (no
    !> `add_log_time_zone_rule'; a subroutine, as `add_log_time_format'
    !> points to it).
    subroutine add_log_iso8601_time_string(s)
        character(len=:), allocatable, intent(out) :: s
        character(len=10) :: buffer
        integer :: v(8)

        call date_and_time(values=v)
        write (buffer, "(i4.4,'-',i2.2,'-',i2.2)") v(1), v(2), v(3)
        s = buffer
    end subroutine add_log_iso8601_time_string

    !> Return (system-dependent) default name for a change log file.
    !> Port of add-log.el:change-log-name.
    function change_log_name() result(name)
        character(len=:), allocatable :: name

        if (allocated(change_log_default_name%value)) then
            name = change_log_default_name%value
        else
            name = "ChangeLog"
        end if
    end function change_log_name

    !> Find a change log file for \[add_change_log_entry] and return the name.
    !>
    !> Optional arg FILE_NAME specifies the file to use.
    !> If FILE_NAME is absent, use the value of `change_log_default_name'.
    !> If `change_log_default_name' is unset, behave as though it were "ChangeLog".
    !>
    !> If `change_log_default_name' contains a leading directory component, then
    !> simply find it in the current directory.  Otherwise, search in the current
    !> directory and its successive parents for a file so named.  Stop at the first
    !> such file that exists (or has a buffer visiting it), or the first directory
    !> that contains any of `change_log_directory_files'.  If no match is found,
    !> use the current directory.  To override the choice of this function,
    !> simply create an empty ChangeLog file first by hand in the desired place.
    !>
    !> Once a file is found, `change_log_default_name' is set locally in the
    !> current buffer to the complete file name.
    !> Optional arg BUFFER_FILE overrides `buffer_file_name'.  Port of
    !> add-log.el:find-change-log (a Diff buffer's source buffer is not
    !> consulted).
    function find_change_log(file_name_in, buffer_file) result(file_name)
        character(len=*), intent(in), optional :: file_name_in, buffer_file
        character(len=:), allocatable :: file_name, cbase, dir, try, up, source, chased
        type(status_type) :: st
        logical :: found
        integer :: k

        if (present(file_name_in)) then
            file_name = file_name_in
        else if (allocated(change_log_default_name%value)) then
            if (len(file_name_directory(change_log_default_name%value)) > 0) then
                file_name = change_log_default_name%value
            end if
        end if

        if (.not. allocated(file_name)) then
            ! Chase links in the source file
            ! and use the change log in the dir where it points.
            source = ""

            if (present(buffer_file)) then
                source = buffer_file
            else if (allocated(buffers(current)%filename)) then
                source = buffers(current)%filename
            end if

            if (len(source) > 0) then
                call file_chase_links(source, chased, st)
                file_name = file_name_directory(chased)
            else
                file_name = buffers(current)%directory
            end if

            if (is_file_directory(file_name)) then
                dir = file_name
                call expand_file_name(change_log_name(), file_name, st, dir)
            end if

            ! Chase links before visiting the file.
            ! This makes it easier to use a single change log file
            ! for several related directories.
            call file_chase_links(file_name, chased, st)
            call expand_file_name(chased, file_name, st)
            cbase = file_name_nondirectory(change_log_name())
            ! locate-dominating-file with the test.
            dir = file_name_directory(abbreviate_file_name(file_name))
            found = .false.

            do while (len(dir) > 0)
                call expand_file_name(cbase, try, st, dir)

                if (is_buffer_live(get_file_buffer(try)) .or. file_exists(try)) then
                    found = .true.
                else if (allocated(change_log_directory_files)) then
                    ! Stop at VCS root?
                    do k = 1, size(change_log_directory_files)
                        call expand_file_name(trim(change_log_directory_files(k)), try, st, dir)

                        if (file_exists(try)) then
                            found = .true.
                            exit
                        end if
                    end do
                end if

                if (found) exit
                up = file_name_directory(directory_file_name(dir))
                if (up == dir) exit
                dir = up
            end do

            if (found) call expand_file_name(cbase, file_name, st, file_name_as_directory(dir))
        end if

        ! Make a local variable in this buffer so we needn't search again.
        call change_log_default_name%set_local(file_name)
    end function find_change_log

    !> Compute a suitable name for a non-file visiting ChangeLog buffer.
    !> CHANGELOG_FILE_NAME is the file name of the actual ChangeLog file
    !> if it were to exist.  Port of
    !> add-log.el:add-log--pseudo-changelog-buffer-name.
    function add_log_pseudo_changelog_buffer_name(changelog_file_name) result(name)
        character(len=*), intent(in) :: changelog_file_name
        character(len=:), allocatable :: name

        name = "*changes to "//abbreviate_file_name(file_name_directory(changelog_file_name))//"*"
    end function add_log_pseudo_changelog_buffer_name

    !> Find a ChangeLog buffer for CHANGELOG_FILE_NAME.
    !> Respect `add_log__pseudo_changelog_buffer_name', which see.  Port of
    !> add-log.el:add-log-find-changelog-buffer.
    subroutine add_log_find_changelog_buffer(changelog_file_name, buffer, status)
        character(len=*), intent(in) :: changelog_file_name
        type(handle_type), intent(out) :: buffer
        type(status_type), intent(out) :: status

        status = status_type()

        if (file_exists(changelog_file_name) .or. .not. add_log_dont_create_changelog_file) then
            call find_file_noselect(changelog_file_name, buffer, status)
        else
            call get_or_make_buffer(add_log_pseudo_changelog_buffer_name(changelog_file_name), buffer, status)
        end if
    end subroutine add_log_find_changelog_buffer

    !> Major mode for editing change logs; like Indented Text mode.
    !> Prevents numeric backups and sets `left_margin' to 8 and `fill_column' to 74.
    !> New log entries are usually made with \[add_change_log_entry] or \[add_change_log_entry_other_window].
    !> Each entry behaves as a paragraph, and the entries for one day as a page.
    !> Runs `change_log_mode_hook'.
    !>
    !> \{change_log_mode_map}
    !>
    !> Port of add-log.el:change-log-mode (the settings the module
    !> documentation lists are deferred).
    recursive subroutine change_log_mode(status)
        type(status_type), intent(out) :: status
        logical :: saved

        if (.not. associated(major_modes%change_log_mode%execute)) call init_add_log()
        call major_modes%change_log_mode%begin(saved, status)
        if (.not. status%ok()) return
        call left_margin%set_local(8_int64)
        call fill_column%set_local(74_int64)
        call indent_tabs_mode%set_local(.true.)
        call tab_width%set_local(8_int64)
        ! We really do want "^" in paragraph-start below: it is only the
        ! lines that begin at column 0 (despite the left-margin of 8) that
        ! we are looking for.  Adding `* ' allows eliding the blank line
        ! between entries for different files.
        call paragraph_start%set_local("\s *$\|"//achar(12)//"\|^\<")
        call paragraph_separate%set_local(paragraph_start%value)
        call page_delimiter%set_local("^\<\|^"//achar(12))
        call major_modes%change_log_mode%end(saved, status)
    end subroutine change_log_mode

    !> The faces of add-log.el.  Not a port.
    subroutine define_faces()
        type(named_face_type), pointer :: f

        f => change_log_date
        call f%define("change_log_date", "Face used to highlight dates in date lines.")
        call f%spec_entry(display_when(), face_attrs(inherit="font_lock_string_face"))
        f => change_log_name_face
        call f%define("change_log_name", "Face for highlighting author names.")
        call f%spec_entry(display_when(), face_attrs(inherit="font_lock_constant_face"))
        f => change_log_email
        call f%define("change_log_email", "Face for highlighting author email addresses.")
        call f%spec_entry(display_when(), face_attrs(inherit="font_lock_variable_name_face"))
        f => change_log_file
        call f%define("change_log_file", "Face for highlighting file names.")
        call f%spec_entry(display_when(), face_attrs(inherit="font_lock_function_name_face"))
        f => change_log_list
        call f%define("change_log_list", "Face for highlighting parenthesized lists of functions or variables.")
        call f%spec_entry(display_when(), face_attrs(inherit="font_lock_keyword_face"))
        f => change_log_conditionals
        call f%define("change_log_conditionals", "Face for highlighting conditionals of the form `[...]'.")
        call f%spec_entry(display_when(), face_attrs(inherit="font_lock_variable_name_face"))
        f => change_log_function
        call f%define("change_log_function", "Face for highlighting items of the form `<....>'.")
        call f%spec_entry(display_when(), face_attrs(inherit="font_lock_variable_name_face"))
        f => change_log_acknowledgment
        call f%define("change_log_acknowledgment", "Face for highlighting acknowledgments.")
        call f%spec_entry(display_when(), face_attrs(inherit="font_lock_comment_face"))
    end subroutine define_faces

    !> Define Change Log mode, its syntax table and faces.  Not a port.
    subroutine init_add_log()
        type(status_type) :: st

        call define_faces()

        if (.not. allocated(change_log_directory_files)) then
            change_log_directory_files = [character(len=8) :: ".bzr", ".git", ".hg", ".svn"]
        end if

        call forward_setting(change_log_default_name, local_if_set=.true.)

        if (change_log_mode_syntax_table%index == 0) then
            call make_syntax_table(change_log_mode_syntax_table, st)
            call modify_syntax_entry(iachar("`"), iachar("`"), "'   ", st, change_log_mode_syntax_table)
            call modify_syntax_entry(iachar("'"), iachar("'"), "'   ", st, change_log_mode_syntax_table)
        end if

        call major_modes%change_log_mode%define("change_log_mode", "Change Log", change_log_mode, &
                                                change_log_mode_hook, parent=major_modes%text_mode, &
                                                syntax_table=change_log_mode_syntax_table)
        change_log_mode_map = major_modes%change_log_mode%map
    end subroutine init_add_log
end module add_log_mod
