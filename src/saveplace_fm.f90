! saveplace_fm.f90 -- automatically save place in files.
! Ported from GNU Emacs 31.1 lisp/saveplace.el.
!
! Copyright (C) 1993-1994, 2001-2026 Free Software Foundation, Inc.
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

!> Automatically save place in files, so that visiting them later
!> (even during a different Fortmacs session) automatically moves point
!> to the saved position, when the file is first found.  Uses the value
!> of buffer-local variable `save_place_mode' to determine whether to
!> save position or not.
!>
!> `save_place_table' is an array of `save_place_type' records, saved
!> in `save_place_file' in namelist syntax (namelist_fm):
!>
!>     &saveplace
!>      place(1)%file = "/home/v/notes.txt",
!>      place(1)%position = 1234,
!>      place(2)%file = "/home/v/src/",
!>      place(2)%dired_filename = "/home/v/src/Fortmacs",
!>     /
!>
!> Deferred: Hexl mode's addresses (hexl.el is not ported), and the
!> effect of setting `save_place_abbreviate_file_names' after the places
!> are loaded (upstream's :set function rewrites the table; here it is
!> rewritten the next time the file is loaded).
module saveplace_fm
    use, intrinsic :: iso_fortran_env, only: int64, real64
    use error_fm, only: status_type
    use handle_fm, only: handle_type
    use lread_fm, only: sym
    use data_fm, only: symbol_value, set, make_variable_buffer_local
    use lisp_fm, only: value_type, t_value
    use eval_fm, only: hook_type
    use modes_fm, only: minor_modes
    use commands_fm, only: commands
    use timer_fm, only: run_with_timer, free_timer, is_timer
    use buffer_fm, only: buffers, cur, buffer_list, set_buffer, current_buffer, get_buffer_create, &
                         generate_new_buffer_name, kill_buffer
    use insdel_fm, only: insert
    use editfns_fm, only: point, goto_char, buffer_string
    use fileio_fm, only: is_file_readable, expand_file_name, write_region, insert_file_contents
    use files_fm, only: find_file_hook, kill_buffer_hook, abbreviate_file_name, revert_buffer_in_progress
    use config_fm, only: state_directory
    use coding_fm, only: coding_system_for_write
    use derived_fm, only: is_derived_mode
    use dired_mode_fm, only: dired_initial_position_hook, dired_current_directory, dired_get_filename, &
                             dired_goto_file, dired_subdir_dirs, dired_string_type
    use search_fm, only: string_match
    use keyboard_fm, only: kill_emacs_hook
    use easy_mmode_fm, only: mode_called_interactively
    use xdisp_fm, only: message
    use namelist_fm, only: namelist_group_type, namelist_item_type, read_namelist, namelist_text, &
                           NAMELIST_STRING, NAMELIST_INTEGER
    implicit none(type, external)
    private

    public :: save_place_mode, save_place_local_mode, save_place_to_table, save_place_forget_unreadable_files, &
              save_place_table_to_file, save_places_to_table, save_place_find_file_hook, save_place_dired_hook, &
              save_place_load_table_from_file, save_place_kill_emacs_hook, syms_of_saveplace

    !> An element of `save_place_table': FILE with the POSITION to go back
    !> to (upstream's (FILENAME . POSITION)), or a directory FILE ending
    !> in a slash with the DIRED_FILENAME point was on in its Dired
    !> listing ((DIRECTORY (dired-filename . FILENAME))).
    type, public :: save_place_type
        character(len=:), allocatable :: file
        integer(int64) :: position = 0
        !> Unallocated for a position.
        character(len=:), allocatable :: dired_filename
    end type save_place_type

    !> Table of saved places to go back to when revisiting files.
    !> Each element gives a FILE and a POSITION; visiting file FILE goes
    !> automatically to position POSITION rather than the beginning of the
    !> buffer.  An element can also give a DIRED_FILENAME, where FILE is
    !> the name of a directory ending in a slash, and DIRED_FILENAME is the
    !> name of a file in that directory.  This form is used for saving
    !> places in Dired buffers, see the function `save_place_dired_hook';
    !> the DIRED_FILENAME is the file where point was located in the Dired
    !> listing of the directory when the place in that buffer was recorded.
    !>
    !> This table is saved between Fortmacs sessions.  Port of
    !> saveplace.el save-place-alist.
    type(save_place_type), allocatable, public :: save_place_table(:)

    !> Name of the file that records `save_place_table' value.  Port of
    !> saveplace.el save-place-file (in the state directory,
    !> `~/.local/state/fortmacs/places').
    character(len=:), allocatable, public :: save_place_file

    !> Values of `save_place_version_control': SAVE_PLACE_VERSION_CONTROL_NUMBERED, SAVE_PLACE_VERSION_CONTROL_EXISTING,
    !> SAVE_PLACE_VERSION_CONTROL_NEVER and SAVE_PLACE_VERSION_CONTROL_NOSPECIAL.
    integer, parameter, public :: SAVE_PLACE_VERSION_CONTROL_NUMBERED = 1, SAVE_PLACE_VERSION_CONTROL_EXISTING = 0, &
                                  SAVE_PLACE_VERSION_CONTROL_NEVER = 2, SAVE_PLACE_VERSION_CONTROL_NOSPECIAL = 3

    !> Controls whether to make numbered backups of master `save_place'
    !> file.  It can have four values: SAVE_PLACE_VERSION_CONTROL_NUMBERED,
    !> _NIL, _NEVER, and _NOSPECIAL.  The first three have the same
    !> meaning that they do for the variable `version_control', and the
    !> final value _NOSPECIAL means just use the value of
    !> `version_control'.  Port of saveplace.el save-place-version-control
    !> (as upstream, writing the file with `write_region' makes no
    !> backups).
    integer, public :: save_place_version_control = SAVE_PLACE_VERSION_CONTROL_EXISTING

    !> True means that the `save_place_file' has been loaded.  Port of
    !> saveplace.el save-place-loaded.
    logical, public :: save_place_loaded = .false.

    !> `save_place_limit' for no limit.
    integer, parameter, public :: SAVE_PLACE_NO_LIMIT = huge(0)

    !> Maximum number of entries to retain in the list; SAVE_PLACE_NO_LIMIT
    !> means no limit.  Port of saveplace.el save-place-limit.
    integer, public :: save_place_limit = 400

    !> True means forget place in unreadable files.
    !>
    !> The filenames in `save_place_table' that do not match
    !> `save_place_skip_check_regexp' are filtered through
    !> `is_file_readable'.  If false, their table entries are removed.
    !>
    !> You may do this anytime by calling the complementary function,
    !> `save_place_forget_unreadable_files'.  When this option is turned
    !> on, this happens automatically before saving `save_place_table' to
    !> `save_place_file'.  Port of saveplace.el
    !> save-place-forget-unreadable-files.
    logical, public :: save_place_forget_unreadable_files_option = .true.

    !> If true, abbreviate file names before saving them.
    !> This can simplify sharing the `save_place_file' file across
    !> different hosts.  Port of saveplace.el
    !> save-place-abbreviate-file-names.
    logical, public :: save_place_abbreviate_file_names = .false.

    !> If true, remember files matching `save_place_skip_check_regexp'.
    !>
    !> When filtering `save_place_table' for unreadable files, some will
    !> not be checked, based on said regexp, and instead saved or forgotten
    !> based on this flag.  Port of saveplace.el save-place-save-skipped.
    logical, public :: save_place_save_skipped = .true.

    !> Regexp whose file names shall not be checked for readability.
    !>
    !> When forgetting unreadable files, file names matching this regular
    !> expression shall not be checked for readability, but instead be
    !> subject to `save_place_save_skipped'.
    !>
    !> Files for which such a check may be inconvenient include those on
    !> removable and network volumes.  Port of saveplace.el
    !> save-place-skip-check-regexp.
    character(len=:), allocatable, public :: save_place_skip_check_regexp

    !> Regexp matching files whose positions should not be recorded.
    !> Useful to exclude temporary files, such as commit message files
    !> that are automatically created by VCSes.  If unallocated, this
    !> feature is disabled, i.e., no files are excluded.  Port of
    !> saveplace.el save-place-ignore-files-regexp.
    character(len=:), allocatable, public :: save_place_ignore_files_regexp

    !> The interval between auto saves of buffer places, in seconds.
    !> If set to 0, disables timer-based auto saving.  Port of saveplace.el
    !> save-place-autosave-interval (0 for none).
    integer, public :: save_place_autosave_interval = 0

    !> Hook run at the end of `save_place_find_file_hook'.  Port of
    !> saveplace.el save-place-after-find-file-hook.
    type(hook_type), target, public :: save_place_after_find_file_hook = &
                                       hook_type(name="save_place_after_find_file_hook")

    !> Hook run at the end of `save_place_mode'.
    type(hook_type), target, public :: save_place_mode_hook = hook_type(name="save_place_mode_hook")
    !> Hook run at the end of `save_place_local_mode'.
    type(hook_type), target, public :: save_place_local_mode_hook = hook_type(name="save_place_local_mode_hook")

    type(handle_type) :: save_place_autosave_timer

contains

    !> Read the places of `save_place_file' into `save_place_table', once.
    !> Port of saveplace.el:save-place-load-alist-from-file.
    recursive subroutine save_place_load_table_from_file()
        character(len=:), allocatable :: file, text
        type(namelist_group_type), allocatable :: groups(:)
        type(status_type) :: st
        type(save_place_type), allocatable :: places(:)
        integer :: g, k, n, idx

        if (save_place_loaded) return
        save_place_loaded = .true.
        file = expand(save_place_file)

        if (is_file_readable(file)) then
            call file_text(file, text, st)
            if (st%ok()) call read_namelist(text, groups, st)

            if (.not. st%ok()) then
                ! (with-demoted-errors "Error reading save-place-file: %S")
                call message("Error reading save-place-file: "//file)
                if (allocated(save_place_table)) deallocate (save_place_table)
                allocate (save_place_table(0))
            else
                n = 0

                do g = 1, size(groups)
                    if (groups(g)%name /= "saveplace" .or. .not. allocated(groups(g)%items)) cycle

                    do k = 1, size(groups(g)%items)
                        if (groups(g)%items(k)%name == "place") n = max(n, int(groups(g)%items(k)%lower))
                    end do
                end do

                allocate (places(n))

                do g = 1, size(groups)
                    if (groups(g)%name /= "saveplace" .or. .not. allocated(groups(g)%items)) cycle

                    do k = 1, size(groups(g)%items)
                        associate (it => groups(g)%items(k))
                            if (it%name /= "place" .or. .not. it%subscripted) cycle
                            if (size(it%values) == 0) cycle
                            idx = int(it%lower)
                            if (idx < 1) cycle

                            select case (it%component)
                            case ("file")
                                if (it%values(1)%kind == NAMELIST_STRING) places(idx)%file = it%values(1)%string
                            case ("position")
                                if (it%values(1)%kind == NAMELIST_INTEGER) places(idx)%position = it%values(1)%integer
                            case ("dired_filename")
                                if (it%values(1)%kind == NAMELIST_STRING) then
                                    places(idx)%dired_filename = it%values(1)%string
                                end if
                            case default
                                ! Not a component of a place.
                                continue
                            end select
                        end associate
                    end do
                end do

                if (allocated(save_place_table)) deallocate (save_place_table)
                allocate (save_place_table(0))

                do k = 1, n
                    if (allocated(places(k)%file)) call append_place(places(k))
                end do

                if (save_place_limit /= SAVE_PLACE_NO_LIMIT) then
                    if (save_place_limit <= 0) then
                        deallocate (save_place_table)
                        allocate (save_place_table(0))
                    else if (size(save_place_table) > save_place_limit) then
                        call truncate_places(save_place_limit)
                    end if
                end if
            end if
        end if

        call save_place_normalize_table()
    end subroutine save_place_load_table_from_file

    !> The text of FILE.  Not a port.
    recursive subroutine file_text(file, text, status)
        character(len=*), intent(in) :: file
        character(len=:), allocatable, intent(out) :: text
        type(status_type), intent(out) :: status
        type(handle_type) :: old, h
        type(status_type) :: st
        integer(int64) :: inserted

        text = ""
        old = current_buffer()
        call get_buffer_create(generate_new_buffer_name(" *temp*"), h, st)
        call set_buffer(h, st)
        call insert_file_contents(file, .false., inserted, status)
        if (status%ok()) text = buffer_string()
        call kill_buffer(h, st)
        call set_buffer(old, st)
    end subroutine file_text

    !> Append PLACE to `save_place_table'.  Not a port.
    subroutine append_place(place)
        type(save_place_type), intent(in) :: place
        type(save_place_type), allocatable :: grown(:)
        integer :: k, n

        if (.not. allocated(save_place_table)) allocate (save_place_table(0))
        n = size(save_place_table)
        allocate (grown(n + 1))

        do k = 1, n
            call move_place(save_place_table(k), grown(k))
        end do

        grown(n + 1) = place
        call move_alloc(grown, save_place_table)
    end subroutine append_place

    ! Move place FROM to TO.  Not a port.
    subroutine move_place(from, to)
        type(save_place_type), intent(inout) :: from
        type(save_place_type), intent(out) :: to

        call move_alloc(from%file, to%file)
        to%position = from%position
        if (allocated(from%dired_filename)) call move_alloc(from%dired_filename, to%dired_filename)
    end subroutine move_place

    !> Keep the first N places.  Not a port.
    subroutine truncate_places(n)
        integer, intent(in) :: n
        type(save_place_type), allocatable :: kept(:)
        integer :: k

        allocate (kept(n))

        do k = 1, n
            call move_place(save_place_table(k), kept(k))
        end do

        call move_alloc(kept, save_place_table)
    end subroutine truncate_places

    !> Remove place K.  Not a port.
    subroutine delete_place(k)
        integer, intent(in) :: k
        type(save_place_type), allocatable :: kept(:)
        integer :: j

        allocate (kept(size(save_place_table) - 1))

        do j = 1, size(kept)
            call move_place(save_place_table(merge(j, j + 1, j < k)), kept(j))
        end do

        call move_alloc(kept, save_place_table)
    end subroutine delete_place

    !> FILE abbreviated or expanded, as `save_place_abbreviate_file_names'
    !> says.  Not a port.
    function normal_name(file) result(name)
        character(len=*), intent(in) :: file
        character(len=:), allocatable :: name

        if (save_place_abbreviate_file_names) then
            name = abbreviate_file_name(file)
        else
            name = expand(file)
        end if
    end function normal_name

    !> Abbreviate or expand the file names of `save_place_table', and drop
    !> the later of two places of the same file.  Port of
    !> saveplace.el:save-place--normalize-alist.
    subroutine save_place_normalize_table()
        integer :: k, j
        logical :: dup

        if (.not. allocated(save_place_table)) allocate (save_place_table(0))

        do k = 1, size(save_place_table)
            save_place_table(k)%file = normal_name(save_place_table(k)%file)

            if (allocated(save_place_table(k)%dired_filename)) then
                save_place_table(k)%dired_filename = normal_name(save_place_table(k)%dired_filename)
            end if
        end do

        k = 2

        do while (k <= size(save_place_table))
            dup = .false.

            do j = 1, k - 1
                if (same(save_place_table(j)%file, save_place_table(k)%file)) then
                    dup = .true.
                    exit
                end if
            end do

            if (dup) then
                call delete_place(k)
            else
                k = k + 1
            end if
        end do
    end subroutine save_place_normalize_table

    !> FILE expanded (expand_file_name), or FILE itself if that fails.
    !> Not a port.
    recursive function expand(file) result(name)
        character(len=*), intent(in) :: file
        character(len=:), allocatable :: name
        type(status_type) :: st

        call expand_file_name(file, name, st)
        if (.not. st%ok()) name = file
    end function expand

    ! Whether file names A and B are the same.  Not a port.
    pure logical function same(a, b)
        character(len=*), intent(in) :: a, b

        same = len(a) == len(b)
        if (same) same = a == b
    end function same

    !> The index of FILE's place in `save_place_table', 0 for none.
    !> Not a port.
    integer function place_index(file) result(k)
        character(len=*), intent(in) :: file

        if (allocated(save_place_table)) then
            do k = 1, size(save_place_table)
                if (same(save_place_table(k)%file, file)) return
            end do
        end if

        k = 0
    end function place_index

    !> Cancel `save_place_autosave' timer, if set.  Port of
    !> saveplace.el:save-place--cancel-timer.
    subroutine save_place_cancel_timer()
        if (is_timer(save_place_autosave_timer)) call free_timer(save_place_autosave_timer)
        save_place_autosave_timer = handle_type()
    end subroutine save_place_cancel_timer

    !> Set or cancel an invocation of `save_place_autosave' on a timer.
    !> If `save_place_mode' is enabled, set the timer, otherwise cancel the
    !> timer.  Port of saveplace.el:save-place--manage-timer.
    subroutine save_place_manage_timer()
        type(status_type) :: st

        if (save_place_mode_on() .and. save_place_autosave_interval > 0 .and. &
            .not. is_timer(save_place_autosave_timer)) then
            call run_with_timer(save_place_autosave_timer, real(save_place_autosave_interval, real64), &
                                save_place_autosave, st, real(save_place_autosave_interval, real64))
        else
            call save_place_cancel_timer()
        end if
    end subroutine save_place_manage_timer

    !> Called by `save_place_autosave_timer'.  Port of
    !> saveplace.el:save-place--autosave.
    recursive subroutine save_place_autosave(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call save_places_to_table()
        call save_place_table_to_file()
    end subroutine save_place_autosave

    !> The value of `save_place_mode' (buffer-local) in the current buffer.
    !> Not a port.
    logical function save_place_mode_on() result(on)
        type(value_type) :: v
        type(status_type) :: st

        call symbol_value(sym("save_place_mode"), v, st)
        on = st%ok() .and. allocated(v%a)
    end function save_place_mode_on

    !> Port of saveplace.el:save-place--setup-hooks (nothing is removed
    !> when the mode is turned off, as upstream).
    subroutine save_place_setup_hooks(add)
        logical, intent(in) :: add

        if (.not. add) return
        call find_file_hook%add(save_place_find_file_hook_proc, depth=90)
        call dired_initial_position_hook%add(save_place_dired_hook_proc)
        call kill_emacs_hook%add(save_place_kill_emacs_hook_proc)
        call kill_buffer_hook%add(save_place_to_table_proc)
    end subroutine save_place_setup_hooks

    !> True means automatically save place in each file.
    !> This means when you visit a file, point goes to the last place
    !> where it was when you previously visited the same file.  Port of
    !> saveplace.el:save-place-mode.
    recursive subroutine save_place_mode(status)
        type(status_type), intent(out) :: status

        call minor_modes%save_place_mode%command(status)
    end subroutine save_place_mode

    !> The body of Save-Place mode.
    !> Port of saveplace.el:save-place-mode, in part (its body).
    recursive subroutine save_place_mode_body(enable, status)
        logical, intent(in) :: enable
        type(status_type), intent(out) :: status

        status = status_type()
        call save_place_setup_hooks(enable)
        call save_place_manage_timer()
    end subroutine save_place_mode_body

    !> Toggle whether to save your place in this file between sessions.
    !> If this mode is enabled, point is recorded when you kill the buffer
    !> or exit Fortmacs.  Visiting this file again will go to that
    !> position, even in a later Fortmacs session.
    !>
    !> To save places automatically in all files, turn on
    !> `save_place_mode'.  Port of saveplace.el:save-place-local-mode.
    recursive subroutine save_place_local_mode(status)
        type(status_type), intent(out) :: status

        call minor_modes%save_place_local_mode%command(status)
    end subroutine save_place_local_mode

    !> The body of Save-Place-Local mode.
    !> Port of saveplace.el:save-place-local-mode, in part (its body).
    recursive subroutine save_place_local_mode_body(enable, status)
        logical, intent(in) :: enable
        type(status_type), intent(out) :: status
        type(status_type) :: st

        status = status_type()

        ! (:variable save-place-mode: the body sets it.)
        if (enable) then
            call set(sym("save_place_mode"), t_value(), st)
        else
            call set(sym("save_place_mode"), value_type(), st)
        end if

        if (.not. (allocated(buffers(cur)%filename) .or. len(dired_directory()) > 0)) then
            call message("Buffer `"//buffers(cur)%name//"' not visiting a file or directory")
        else
            call save_place_setup_hooks(enable)
            call save_place_manage_timer()
        end if
    end subroutine save_place_local_mode_body

    !> The current Dired directory of a Dired buffer, else "".  Not a port.
    function dired_directory() result(dir)
        character(len=:), allocatable :: dir
        type(dired_string_type), allocatable :: dirs(:)
        type(status_type) :: st

        dir = ""
        if (.not. is_derived_mode("dired_mode")) return
        dirs = dired_subdir_dirs()
        if (size(dirs) == 0) return
        dir = dired_current_directory(st)
        if (.not. st%ok()) dir = ""
    end function dired_directory

    !> Add current buffer filename and position to `save_place_table'.
    !> Put filename and point in a record at the front of the
    !> `save_place_table', if `save_place_mode' is true.  Otherwise, just
    !> delete that file from the table.
    !>
    !> If `save_place_abbreviate_file_names' is true, abbreviate the file
    !> names.  Port of saveplace.el:save-place-to-alist.
    recursive subroutine save_place_to_table()
        type(save_place_type) :: place
        type(save_place_type), allocatable :: grown(:)
        type(status_type) :: st
        character(len=:), allocatable :: directory, item, filename
        logical :: found
        integer :: k, n

        if (.not. save_place_loaded) call save_place_load_table_from_file()
        directory = dired_directory()

        if (allocated(buffers(cur)%filename)) then
            item = buffers(cur)%filename
        else if (len(directory) > 0) then
            item = expand(directory)
        else
            return
        end if

        if (allocated(save_place_ignore_files_regexp)) then
            if (string_match(save_place_ignore_files_regexp, item, st) > 0) return
        end if

        if (save_place_abbreviate_file_names) item = abbreviate_file_name(item)
        place%file = item
        place%position = point()

        if (is_derived_mode("dired_mode") .and. len(directory) > 0) then
            call dired_get_filename(filename, found, st, no_error_if_not_filep=.true.)

            if (found) then
                if (save_place_abbreviate_file_names) filename = abbreviate_file_name(filename)
                place%dired_filename = filename
                place%position = 0
            end if
        end if

        k = place_index(item)
        if (k > 0) call delete_place(k)

        ! Optimize out the degenerate case.
        if (save_place_mode_on() .and. .not. (.not. allocated(place%dired_filename) .and. place%position == 1)) then
            n = size(save_place_table)
            allocate (grown(n + 1))
            grown(1) = place

            do k = 1, n
                call move_place(save_place_table(k), grown(k + 1))
            end do

            call move_alloc(grown, save_place_table)
        end if
    end subroutine save_place_to_table

    ! `save_place_to_table' as a hook function.  Not a port.
    recursive subroutine save_place_to_table_proc(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call save_place_to_table()
    end subroutine save_place_to_table_proc

    !> Whether the place of FILE is kept when forgetting unreadable files.
    !> Not a port.
    logical function keep_place(file) result(keep)
        character(len=*), intent(in) :: file
        type(status_type) :: st

        if (string_match(save_place_skip_check_regexp, file, st) > 0) then
            keep = save_place_save_skipped
        else
            keep = is_file_readable(file)
        end if
    end function keep_place

    !> Remove unreadable files from `save_place_table'.
    !> For each entry in the table, if `is_file_readable' returns false for
    !> the filename, remove the entry.  Port of
    !> saveplace.el:save-place-forget-unreadable-files.
    recursive subroutine save_place_forget_unreadable_files()
        integer :: k

        if (.not. allocated(save_place_table)) return
        k = 1

        do while (k <= size(save_place_table))
            if (keep_place(save_place_table(k)%file)) then
                k = k + 1
            else
                call delete_place(k)
            end if
        end do
    end subroutine save_place_forget_unreadable_files

    recursive subroutine cmd_save_place_forget_unreadable_files(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call save_place_forget_unreadable_files()
    end subroutine cmd_save_place_forget_unreadable_files

    !> Write `save_place_table' to `save_place_file'.  Port of
    !> saveplace.el:save-place-alist-to-file.
    recursive subroutine save_place_table_to_file()
        type(namelist_group_type) :: group
        type(namelist_item_type) :: item
        type(handle_type) :: old, h
        type(status_type) :: st, wst
        character(len=:), allocatable :: file, wrote
        integer :: k

        file = expand(save_place_file)
        if (save_place_forget_unreadable_files_option) call save_place_forget_unreadable_files()
        group%name = "saveplace"

        if (allocated(save_place_table)) then
            do k = 1, size(save_place_table)
                item = namelist_item_type(name="place", component="file", subscripted=.true., lower=k, upper=k)
                call item%add_string(save_place_table(k)%file)
                call group%add(item)

                if (allocated(save_place_table(k)%dired_filename)) then
                    item = namelist_item_type(name="place", component="dired_filename", subscripted=.true., &
                                              lower=k, upper=k)
                    call item%add_string(save_place_table(k)%dired_filename)
                else
                    item = namelist_item_type(name="place", component="position", subscripted=.true., &
                                              lower=k, upper=k)
                    call item%add_integer(save_place_table(k)%position)
                end if

                call group%add(item)
            end do
        end if

        old = current_buffer()
        call get_buffer_create(generate_new_buffer_name(" *temp*"), h, st)
        call set_buffer(h, st)
        call insert("! Fortmacs places, automatically generated by `save_place_mode'."//achar(10)// &
                    namelist_text(group), st)
        coding_system_for_write = "utf-8"
        call write_region(file, .false., wst, message=wrote)
        call coding_system_for_write%clear()
        call kill_buffer(h, st)
        call set_buffer(old, st)

        if (.not. wst%ok()) then
            call message("Saving places: can't write "//file)
        else if (mode_called_interactively) then
            call message(wrote)
        end if
    end subroutine save_place_table_to_file

    !> Save all buffer filenames and positions to `save_place_table'.
    !> See `save_place_to_table'.  Port of
    !> saveplace.el:save-places-to-alist (no `find_file_literally'
    !> buffers: that command is not ported).
    recursive subroutine save_places_to_table()
        type(handle_type), allocatable :: list(:)
        type(handle_type) :: old
        type(status_type) :: st
        integer :: k

        list = buffer_list()
        old = current_buffer()

        do k = 1, size(list)
            call set_buffer(list(k), st)
            if (.not. st%ok()) cycle
            if (allocated(buffers(cur)%filename) .or. len(dired_directory()) > 0) call save_place_to_table()
        end do

        call set_buffer(old, st)
    end subroutine save_places_to_table

    !> Function added to `find_file_hook' by `save_place_mode'.
    !> It runs the hook `save_place_after_find_file_hook'.  Port of
    !> saveplace.el:save-place-find-file-hook.
    recursive subroutine save_place_find_file_hook(status)
        type(status_type), intent(out) :: status
        type(status_type) :: st
        integer :: k

        if (.not. save_place_loaded) call save_place_load_table_from_file()
        k = 0

        if (allocated(buffers(cur)%filename)) then
            if (save_place_abbreviate_file_names) then
                k = place_index(abbreviate_file_name(buffers(cur)%filename))
            else
                k = place_index(buffers(cur)%filename)
            end if
        end if

        if (k > 0) then
            if (.not. revert_buffer_in_progress .and. .not. allocated(save_place_table(k)%dired_filename)) then
                call goto_char(save_place_table(k)%position)
            end if

            call set(sym("save_place_mode"), t_value(), st)
        end if

        call save_place_after_find_file_hook%run(status)
    end subroutine save_place_find_file_hook

    ! `save_place_find_file_hook' as a hook function.  Not a port.
    recursive subroutine save_place_find_file_hook_proc(status)
        type(status_type), intent(out) :: status

        call save_place_find_file_hook(status)
    end subroutine save_place_find_file_hook_proc

    !> Position point in a Dired buffer according to its saved place.
    !> This is run via `dired_initial_position_hook', which see.  Port of
    !> saveplace.el:save-place-dired-hook.
    recursive subroutine save_place_dired_hook()
        character(len=:), allocatable :: directory, item
        type(status_type) :: st
        integer(int64) :: pos
        integer :: k

        if (.not. save_place_loaded) call save_place_load_table_from_file()
        directory = dired_directory()
        if (len(directory) == 0) return
        item = expand(directory)
        if (save_place_abbreviate_file_names) item = abbreviate_file_name(item)
        k = place_index(item)
        if (k == 0) return

        if (.not. revert_buffer_in_progress) then
            if (allocated(save_place_table(k)%dired_filename)) then
                pos = dired_goto_file(expand(save_place_table(k)%dired_filename), st)
            else
                call goto_char(save_place_table(k)%position)
            end if
        end if

        call set(sym("save_place_mode"), t_value(), st)
    end subroutine save_place_dired_hook

    ! `save_place_dired_hook' as a hook function.  Not a port.
    recursive subroutine save_place_dired_hook_proc(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call save_place_dired_hook()
    end subroutine save_place_dired_hook_proc

    !> Port of saveplace.el:save-place-kill-emacs-hook.
    recursive subroutine save_place_kill_emacs_hook()
        call save_places_to_table()
        if (save_place_loaded) call save_place_table_to_file()
    end subroutine save_place_kill_emacs_hook

    ! `save_place_kill_emacs_hook' as a hook function.  Not a port.
    recursive subroutine save_place_kill_emacs_hook_proc(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call save_place_kill_emacs_hook()
    end subroutine save_place_kill_emacs_hook_proc

    !> Define the modes, the command and the options' defaults.  Not a port.
    subroutine syms_of_saveplace()
        type(status_type) :: st

        if (.not. allocated(save_place_file)) save_place_file = state_directory()//"places"

        if (.not. allocated(save_place_skip_check_regexp)) then
            save_place_skip_check_regexp = "\`/\(?:cdrom\|floppy\|mnt\|\(?:[^@/:]*@\)?[^@/:]*[^@/:.]:\)"
        end if

        if (.not. allocated(save_place_ignore_files_regexp)) then
            save_place_ignore_files_regexp = "\(?:COMMIT_EDITMSG\|hg-editor-[[:alnum:]]+\.txt\|svn-commit\.tmp\|" &
                                             //"bzr_log\.[[:alnum:]]+\)$"
        end if

        if (.not. allocated(save_place_table)) allocate (save_place_table(0))
        call minor_modes%save_place_mode%define("save_place_mode", global=.true., hook=save_place_mode_hook, &
                                                body=save_place_mode_body)
        call make_variable_buffer_local(sym("save_place_mode"), st)
        call minor_modes%save_place_local_mode%define("save_place_local_mode", variable="save_place_mode", &
                                                      hook=save_place_local_mode_hook, &
                                                      body=save_place_local_mode_body)
        commands%save_place_mode%run => save_place_mode
        commands%save_place_local_mode%run => save_place_local_mode
        commands%save_place_forget_unreadable_files%run => cmd_save_place_forget_unreadable_files
    end subroutine syms_of_saveplace
end module saveplace_fm
