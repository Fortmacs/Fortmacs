! savehist_fm.f90 -- Save minibuffer history.
! Ported from GNU Emacs 31.1 lisp/savehist.el.
!
! Copyright (C) 1997, 2005-2026 Free Software Foundation, Inc.
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

!> Many editors (e.g. Vim) have the feature of saving minibuffer
!> history to an external file after exit.  This package provides the
!> same feature in Fortmacs.  When set up, it saves recorded minibuffer
!> histories to a file (`~/.local/state/fortmacs/history' by default).
!> Additional variables may be specified by customizing
!> `savehist_additional_variables'.
!>
!> To use savehist, turn on `savehist_mode'.  You can also explicitly
!> save history with `M-x savehist_save'.
!>
!> The file is in namelist syntax (namelist_fm) rather than Lisp, so it
!> is read, not loaded: its objects are the variables to set.  A value
!> is saved if it is a string, an integer, true or false, or a list of
!> those; other values (symbols, nested lists) are not printable.
!> Deferred: the text properties of saved strings, the warning about a
!> file that can't be written (warnings.el is not ported), and the
!> obsolete `savehist_trim_history'.
module savehist_fm
    use, intrinsic :: iso_fortran_env, only: int32, int64, real64
    use error_fm, only: status_type, set_error, format_error, FM_GENERIC_ERROR
    use handle_fm, only: handle_type
    use lisp_fm, only: value_type, t_value, make_string, make_fixnum, make_list, list_elements, &
                       vtag, xstring, xfixnum, xsymbol, symbols, V_NULL, V_INTEGER, V_STRING, V_ARRAY, V_NAME, SYM_T
    use lread_fm, only: sym, intern_soft
    use data_fm, only: set, symbol_value, boundp
    use fns_fm, only: value_equal
    use eval_fm, only: hook_type
    use easy_mmode_fm, only: mode_called_interactively
    use modes_fm, only: minor_modes
    use commands_fm, only: commands
    use timer_fm, only: run_with_timer, free_timer, is_timer
    use buffer_fm, only: get_buffer_create, generate_new_buffer_name, set_buffer, kill_buffer, current_buffer
    use insdel_fm, only: insert
    use editfns_fm, only: buffer_string
    use fileio_fm, only: file_exists, file_attributes, file_stat_type, set_file_modes, write_region, &
                         insert_file_contents, file_name_directory, expand_file_name
    use files_fm, only: make_directory
    use config_fm, only: state_directory
    use coding_fm, only: coding_system_for_write
    use minibuf_fm, only: minibuffer_history_variable, minibuffer_setup_hook
    use keyboard_fm, only: kill_emacs_hook
    use xdisp_fm, only: message
    use namelist_fm, only: namelist_group_type, namelist_item_type, namelist_value_type, read_namelist, namelist_text, &
                           NAMELIST_STRING, NAMELIST_INTEGER, NAMELIST_LOGICAL
    implicit none(type, external)
    private

    public :: savehist_mode, savehist_save, savehist_autosave, savehist_install, savehist_uninstall, &
              savehist_minibuffer_hook, savehist_printable, syms_of_savehist

    !> If true, save all recorded minibuffer histories.
    !> If you want to save only specific histories, use `savehist_save_hook'
    !> to modify the value of `savehist_minibuffer_history_variables'.
    !> Port of savehist.el savehist-save-minibuffer-history.
    logical, public :: savehist_save_minibuffer_history = .true.

    !> An element of `savehist_additional_variables': the variable NAME,
    !> and MAX_SIZE, the most elements of a list value to save (negative
    !> for no limit; upstream's (VAR . MAX-SIZE)).
    type, public :: savehist_variable_type
        character(len=64) :: name = ""
        integer :: max_size = -1
    end type savehist_variable_type

    !> List of additional variables to save.
    !> Each element is a variable that will be persisted across Fortmacs
    !> sessions that use Savehist.
    !>
    !> An element names the variable, and may give a MAX_SIZE, which means
    !> to truncate the variable's value to at most MAX_SIZE elements (if
    !> the value is a list) before saving the value:
    !>
    !>     savehist_additional_variables = [savehist_variable_type(name="search_ring"), &
    !>                                      savehist_variable_type(name="kill_ring", max_size=20)]
    !>
    !> The contents of variables should be printable.  You don't need to
    !> add minibuffer history variables to this list, all minibuffer
    !> histories will be saved automatically as long as
    !> `savehist_save_minibuffer_history' is true.
    !>
    !> User options should be saved with the Customize interface.  This
    !> list is useful for saving automatically updated variables that are
    !> not minibuffer histories, such as `compile_command' or `kill_ring'.
    !>
    !> NOTE: If this variable is not empty, then Fortmacs will not try to
    !> handle gracefully concurrent modifications to the `savehist_file',
    !> e.g. by other Fortmacs sessions or other tools like
    !> file-synchronization systems.  Port of savehist.el
    !> savehist-additional-variables.
    type(savehist_variable_type), allocatable, public :: savehist_additional_variables(:)

    !> List of additional variables not to save.  Port of savehist.el
    !> savehist-ignored-variables.
    character(len=64), allocatable, public :: savehist_ignored_variables(:)

    !> File name where minibuffer history is saved to and loaded from.
    !> The minibuffer history is a namelist read automatically when
    !> Savehist mode is turned on.  See `savehist_mode' for more details.
    !> Port of savehist.el savehist-file (in the state directory,
    !> `~/.local/state/fortmacs/history').
    character(len=:), allocatable, public :: savehist_file

    !> Default permissions of the history file.
    !> This is decimal, not octal.  The default is 384 (0600 in octal).
    !> Set to -1 to use the default permissions that Fortmacs uses,
    !> typically mandated by umask.  The default is a bit more restrictive
    !> to protect the user's privacy.  Port of savehist.el
    !> savehist-file-modes (-1 for nil).
    integer, public :: savehist_file_modes = 384

    !> The interval between autosaves of minibuffer history, in seconds.
    !> If set to 0, disables timer-based autosaving.  Port of savehist.el
    !> savehist-autosave-interval (0 for nil; it takes effect when the
    !> timer is next set, as Savehist mode does).
    integer, public :: savehist_autosave_interval = 5*60

    !> Hook called when Savehist mode is turned on.  Port of savehist.el
    !> savehist-mode-hook.
    type(hook_type), target, public :: savehist_mode_hook = hook_type(name="savehist_mode_hook")

    !> Hook called by `savehist_save' before saving the variables.
    !> You can use this hook to influence choice and content of variables
    !> to save.  Port of savehist.el savehist-save-hook.
    type(hook_type), target, public :: savehist_save_hook = hook_type(name="savehist_save_hook")

    !> The coding system Savehist uses for saving the minibuffer history.
    !> Changing this value while Fortmacs is running is supported, but
    !> considered unwise, unless you know what you are doing.  Port of
    !> savehist.el savehist-coding-system.
    character(len=:), allocatable, public :: savehist_coding_system

    !> List of minibuffer histories.
    !> The contents of this variable is built while Fortmacs is running,
    !> and saved along with minibuffer history.  You can change its value
    !> off `savehist_save_hook' to influence which variables are saved.
    !> Port of savehist.el savehist-minibuffer-history-variables.
    character(len=64), allocatable, public :: savehist_minibuffer_history_variables(:)

    !> Whether the history has already been loaded.  Port of savehist.el
    !> savehist-loaded (obsolete since 31.1).
    logical, public :: savehist_loaded = .false.

    !> Modtime of the `savehist_file' when we last sync'd up with it
    !> (savehist--file-sync-modtime); KNOWN false for nil.  A file that
    !> doesn't exist has the modtime t (EXISTS false).
    type :: modtime_type
        logical :: known = .false., exists = .false.
        integer(int64) :: sec = 0
        integer(int32) :: nsec = 0
    end type modtime_type
    type(modtime_type) :: savehist_file_sync_modtime

    !> The text last written (savehist.el compares its md5,
    !> savehist-last-checksum); unallocated for none.
    character(len=:), allocatable :: savehist_last_text

    type(handle_type) :: savehist_timer
    logical :: savehist_has_given_file_warning = .false.

contains

    !> Cancel `savehist_autosave' timer, if set.  Port of
    !> savehist.el:savehist--cancel-timer.
    subroutine savehist_cancel_timer()
        if (is_timer(savehist_timer)) call free_timer(savehist_timer)
        savehist_timer = handle_type()
    end subroutine savehist_cancel_timer

    !> Set or cancel an invocation of `savehist_autosave' on a timer.
    !> If `savehist_mode' is enabled, set the timer, otherwise cancel the
    !> timer.  Port of savehist.el:savehist--manage-timer.
    subroutine savehist_manage_timer()
        type(status_type) :: st

        if (minor_modes%savehist_mode%enabled() .and. savehist_autosave_interval > 0 .and. &
            .not. is_timer(savehist_timer)) then
            call run_with_timer(savehist_timer, real(savehist_autosave_interval, real64), savehist_autosave_timer, &
                                st, real(savehist_autosave_interval, real64))
        else
            call savehist_cancel_timer()
        end if
    end subroutine savehist_manage_timer

    !> Toggle saving of minibuffer history (Savehist mode).
    !>
    !> When Savehist mode is enabled, minibuffer history is saved to
    !> `savehist_file' periodically and when exiting Fortmacs.  When
    !> Savehist mode is enabled for the first time in a Fortmacs session,
    !> it loads the previous minibuffer histories from `savehist_file'.
    !> The variable `savehist_autosave_interval' controls the periodicity
    !> of saving minibuffer histories.
    !>
    !> If `savehist_save_minibuffer_history' is true (the default), all
    !> recorded minibuffer histories will be saved.  You can arrange for
    !> additional history variables to be saved and restored by customizing
    !> `savehist_additional_variables', which by default is an empty list.
    !>
    !> Alternatively, you could customize `savehist_save_minibuffer_history'
    !> to false, and add to `savehist_additional_variables' only those
    !> history variables you want to save.
    !>
    !> To ignore some history variables, add their names to the list in
    !> `savehist_ignored_variables'.
    !>
    !> This mode should normally be turned on from your init file.
    !> Calling it at any other time replaces your current minibuffer
    !> histories, which is probably undesirable.  Port of
    !> savehist.el:savehist-mode.
    recursive subroutine savehist_mode(status)
        type(status_type), intent(out) :: status

        call minor_modes%savehist_mode%command(status)
    end subroutine savehist_mode

    !> The body of Savehist mode.  Port of savehist.el:savehist-mode (its
    !> body).
    recursive subroutine savehist_mode_body(enable, status)
        logical, intent(in) :: enable
        type(status_type), intent(out) :: status

        status = status_type()

        if (.not. enable) then
            call savehist_uninstall()
        else
            call savehist_reload(mode_called_interactively, status)
            if (.not. status%ok()) return
            call savehist_install()
        end if
    end subroutine savehist_mode_body

    !> The modtime of `savehist_file'.  Port of
    !> savehist.el:savehist--file-modtime.
    function savehist_file_modtime() result(m)
        type(modtime_type) :: m
        type(file_stat_type) :: st
        logical :: found

        m%known = .true.
        call file_attributes(savehist_file, st, found)
        m%exists = found

        if (found) then
            m%sec = st%mtime_sec
            m%nsec = st%mtime_nsec
        end if
    end function savehist_file_modtime

    ! Not a port.
    logical function same_modtime(a, b)
        type(modtime_type), intent(in) :: a, b

        same_modtime = (a%known .eqv. b%known) .and. (a%exists .eqv. b%exists) .and. a%sec == b%sec .and. &
                       a%nsec == b%nsec
    end function same_modtime

    !> Load the history data from `savehist_file'.
    !> Be careful to do it while preserving the current history data.
    !> Port of savehist.el:savehist--reload.
    recursive subroutine savehist_reload(interactively, status)
        logical, intent(in) :: interactively
        type(status_type), intent(out) :: status
        character(len=64), allocatable :: old_names(:)
        type(value_type), allocatable :: old_values(:)
        type(value_type) :: v, cur_value
        type(status_type) :: st
        logical :: bound
        integer :: k, n

        status = status_type()
        if (.not. file_exists(savehist_file)) return
        if (savehist_loaded .and. same_modtime(savehist_file_sync_modtime, savehist_file_modtime())) return
        ! The history variables we know about, with their values.
        n = 0
        if (allocated(savehist_minibuffer_history_variables)) n = size(savehist_minibuffer_history_variables)
        allocate (old_names(n), old_values(n))

        do k = 1, n
            old_names(k) = savehist_minibuffer_history_variables(k)
            call boundp(sym(trim(old_names(k))), bound, st)
            if (bound) call symbol_value(sym(trim(old_names(k))), old_values(k), st)
            if (.not. bound) old_names(k) = ""
        end do

        if (interactively) call message("Loading "//savehist_file//" (source)...")
        call load_savehist_file(status)

        if (.not. status%ok()) then
            ! Don't install the mode if reading failed.  Doing so would
            ! effectively destroy the user's data at the next save.
            call minor_modes%savehist_mode%set_state(.false.)
            call savehist_uninstall()
            return
        end if

        if (interactively) call message("Loading "//savehist_file//" (source)...done")
        savehist_file_sync_modtime = savehist_file_modtime()
        savehist_loaded = .true.

        ! In case we're loading the file late, there was info in the
        ! history variables that may have been overwritten by the info
        ! extracted from the file, so put it back in.
        do k = 1, n
            if (len_trim(old_names(k)) == 0) cycle
            call symbol_value(sym(trim(old_names(k))), cur_value, st)
            if (.not. st%ok()) cycle
            if (value_equal(old_values(k), cur_value)) cycle
            v = savehist_merge(old_values(k), cur_value)
            call set(sym(trim(old_names(k))), v, st)
        end do
    end subroutine savehist_reload

    !> Set the variables of `savehist_file' to their saved values.
    !> Not a port.
    recursive subroutine load_savehist_file(status)
        type(status_type), intent(out) :: status
        type(namelist_group_type), allocatable :: groups(:)
        character(len=:), allocatable :: text
        integer :: g, k

        call file_text(savehist_file, text, status)
        if (.not. status%ok()) return
        call read_namelist(text, groups, status)
        if (.not. status%ok()) return

        do g = 1, size(groups)
            if (groups(g)%name /= "savehist") cycle
            if (.not. allocated(groups(g)%items)) cycle

            do k = 1, size(groups(g)%items)
                call set_saved(groups(g)%items(k), status)
                if (.not. status%ok()) return
            end do
        end do
    end subroutine load_savehist_file

    !> Set the variable of ITEM to its value.  Not a port.
    recursive subroutine set_saved(item, status)
        type(namelist_item_type), intent(in) :: item
        type(status_type), intent(out) :: status
        type(value_type), allocatable :: elems(:)
        integer :: k

        status = status_type()

        if (item%name == "savehist_minibuffer_history_variables") then
            if (allocated(savehist_minibuffer_history_variables)) deallocate (savehist_minibuffer_history_variables)

            if (.not. item%subscripted) then
                allocate (savehist_minibuffer_history_variables(0))
                return
            end if

            allocate (savehist_minibuffer_history_variables(size(item%values)))

            do k = 1, size(item%values)
                savehist_minibuffer_history_variables(k) = item%values(k)%string
            end do

            return
        end if

        if (item%subscripted) then
            allocate (elems(size(item%values)))

            do k = 1, size(item%values)
                elems(k) = saved_value(item%values(k))
            end do

            call set(sym(item%name), make_list(elems), status)
        else if (size(item%values) > 0) then
            call set(sym(item%name), saved_value(item%values(1)), status)
        end if
    end subroutine set_saved

    !> A value as savehist saved it.  Not a port.
    function saved_value(nv) result(v)
        type(namelist_value_type), intent(in) :: nv
        type(value_type) :: v

        select case (nv%kind)
        case (NAMELIST_INTEGER)
            v = make_fixnum(nv%integer)
        case (NAMELIST_LOGICAL)
            if (nv%logical) v = t_value()
        case default
            v = make_string(nv%string)
        end select
    end function saved_value

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

    !> Merge the OLD history we had with the NEW history we just loaded.
    !> We don't know the relative age of the various entries in OLD and
    !> NEW, so the two lists are interleaved, then duplicates dropped.
    !> Port of savehist.el:savehist--merge.
    function savehist_merge(old, new) result(merged)
        type(value_type), intent(in) :: old, new
        type(value_type) :: merged
        type(value_type), allocatable :: o(:), n(:), res(:)
        integer :: i, j, k, m
        logical :: dup

        call list_elements(old, o)
        call list_elements(new, n)
        allocate (res(size(o) + size(n)))
        m = 0
        i = 1
        j = 1

        ! Keep the first element first.
        do while (i <= size(o) .and. j <= size(n))
            call push(o(i))
            call push(n(j))
            i = i + 1
            j = j + 1
        end do

        do while (i <= size(o))
            call push(o(i))
            i = i + 1
        end do

        do while (j <= size(n))
            call push(n(j))
            j = j + 1
        end do

        merged = make_list(res(1:m))

    contains

        !> Append E unless an equal element is there already (delete-dups).
        subroutine push(e)
            type(value_type), intent(in) :: e

            dup = .false.

            do k = 1, m
                if (value_equal(res(k), e)) then
                    dup = .true.
                    exit
                end if
            end do

            if (dup) return
            m = m + 1
            res(m) = e
        end subroutine push
    end function savehist_merge

    !> Hook Savehist into Fortmacs.
    !> Normally invoked by calling `savehist_mode' to set the minor mode.
    !> Installs `savehist_autosave' in `kill_emacs_hook' and on a timer.
    !> To undo this, call `savehist_uninstall'.  Port of
    !> savehist.el:savehist-install.
    subroutine savehist_install()
        call minibuffer_setup_hook%add(savehist_minibuffer_hook_proc)
        call kill_emacs_hook%add(savehist_autosave_timer)
        call savehist_manage_timer()
    end subroutine savehist_install

    !> Undo installing savehist.
    !> Normally invoked by calling `savehist_mode' to unset the minor mode.
    !> Port of savehist.el:savehist-uninstall.
    subroutine savehist_uninstall()
        call minibuffer_setup_hook%remove(savehist_minibuffer_hook_proc)
        call kill_emacs_hook%remove(savehist_autosave_timer)
        call savehist_manage_timer()
    end subroutine savehist_uninstall

    !> Whether NAME is in the list NAMES.  Not a port.
    pure logical function member(name, names)
        character(len=*), intent(in) :: name
        character(len=64), allocatable, intent(in) :: names(:)
        integer :: k

        member = .false.
        if (.not. allocated(names)) return

        do k = 1, size(names)
            if (trim(names(k)) == trim(name)) then
                member = .true.
                return
            end if
        end do
    end function member

    !> Save the values of minibuffer history variables.
    !> Unbound variables named in `savehist_additional_variables' are
    !> ignored.  If AUTO_SAVE is true, compare the saved contents to the
    !> one last saved, and don't save the buffer if they are the same.
    !> Port of savehist.el:savehist-save.
    recursive subroutine savehist_save(status, auto_save, interactive)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: auto_save, interactive
        type(namelist_group_type) :: group
        type(namelist_item_type) :: item
        type(value_type) :: value
        type(value_type), allocatable :: elems(:)
        type(handle_type) :: old, h
        type(status_type) :: st
        character(len=:), allocatable :: text, wrote, dir, name
        logical :: auto, bound, ok
        integer :: k, j, n, m

        status = status_type()
        auto = .false.
        if (present(auto_save)) auto = auto_save
        ! We don't know how to merge old and new values of those vars in
        ! `savehist_additional_variables', so only do the auto-sync if
        ! there aren't such variables.
        n = 0
        if (allocated(savehist_additional_variables)) n = size(savehist_additional_variables)

        if (n == 0 .and. .not. same_modtime(savehist_file_sync_modtime, savehist_file_modtime())) then
            ! The file has been changed since last time we saw it.
            ! Probably some other Fortmacs session.  Load the corresponding
            ! info so we don't end up throwing it away by blindly
            ! overwriting it.
            call savehist_reload(.false., st)
        end if

        call savehist_save_hook%run(st)
        group%name = "savehist"

        ! Save the minibuffer histories, along with the value of
        ! savehist_minibuffer_history_variables itself.
        if (savehist_save_minibuffer_history) then
            item = namelist_item_type(name="savehist_minibuffer_history_variables")
            if (.not. allocated(savehist_minibuffer_history_variables)) allocate (savehist_minibuffer_history_variables(0))

            if (size(savehist_minibuffer_history_variables) == 0) then
                call item%add_logical(.false.)
            else
                call subscript(item, size(savehist_minibuffer_history_variables))

                do k = 1, size(savehist_minibuffer_history_variables)
                    call item%add_string(trim(savehist_minibuffer_history_variables(k)))
                end do
            end if

            call group%add(item)

            do k = 1, size(savehist_minibuffer_history_variables)
                ! (A variable, not an associate name: with gfortran, `cycle' out
                ! of an associate block whose selector is an expression frees
                ! its temporary twice.)
                name = trim(savehist_minibuffer_history_variables(k))
                call boundp(sym(name), bound, st)
                if (.not. bound .or. member(name, savehist_ignored_variables)) cycle
                call symbol_value(sym(name), value, st)
                ! Don't save empty histories.
                if (.not. allocated(value%a)) cycle
                ! Print elements of VALUE one by one, carefully: those
                ! that can't be written are left out.
                item = namelist_item_type(name=name)
                call list_elements(value, elems)

                do j = 1, size(elems)
                    call add_value(item, elems(j), ok)
                end do

                if (.not. allocated(item%values)) cycle
                call subscript(item, size(item%values))
                call group%add(item)
            end do
        end if

        ! Save the additional variables.
        do k = 1, n
            associate (var => savehist_additional_variables(k))
                if (member(var%name, savehist_minibuffer_history_variables)) cycle
                call boundp(sym(trim(var%name)), bound, st)
                if (.not. bound) cycle
                call symbol_value(sym(trim(var%name)), value, st)
                if (.not. savehist_printable(value)) cycle
                item = namelist_item_type(name=trim(var%name))

                if (vtag(value) == V_ARRAY) then
                    ! When we have a max-size, chop off the last elements.
                    call list_elements(value, elems)
                    m = size(elems)
                    if (var%max_size >= 0) m = min(m, var%max_size)

                    do j = 1, m
                        call add_value(item, elems(j), ok)
                    end do

                    if (m == 0) then
                        call item%add_logical(.false.)
                    else
                        call subscript(item, m)
                    end if
                else
                    call add_value(item, value, ok)
                end if

                call group%add(item)
            end associate
        end do

        text = "! Minibuffer history file, automatically generated by `savehist'."//achar(10)//achar(10)// &
               namelist_text(group)

        ! If autosaving, avoid writing if nothing has changed since the
        ! last write.
        if (auto .and. allocated(savehist_last_text)) then
            if (savehist_last_text == text .and. len(savehist_last_text) == len(text)) return
        end if

        ! Ensure that the directory exists before saving.
        dir = file_name_directory(savehist_file)
        if (.not. file_exists(dir)) call make_directory(dir, st, parents=.true.)
        old = current_buffer()
        call get_buffer_create(generate_new_buffer_name(" *temp*"), h, st)
        call set_buffer(h, st)
        call insert(text, st)
        coding_system_for_write = savehist_coding_system
        call write_region(savehist_file, .false., status, message=wrote)
        call coding_system_for_write%clear()
        call kill_buffer(h, st)
        call set_buffer(old, st)

        if (.not. status%ok()) then
            ! (The warning is Deferred: warnings.el is not ported.)
            savehist_has_given_file_warning = .true.
            status = status_type()
            return
        end if

        if (present(interactive)) then
            if (interactive) call message(wrote)
        end if

        if (savehist_file_modes >= 0) call set_file_modes(savehist_file, int(savehist_file_modes, int32), st)
        savehist_file_sync_modtime = savehist_file_modtime()
        savehist_last_text = text

    contains

        subroutine subscript(it, count)
            type(namelist_item_type), intent(inout) :: it
            integer, intent(in) :: count

            it%subscripted = .true.
            it%lower = 1
            it%upper = count
        end subroutine subscript
    end subroutine savehist_save

    !> Add V to ITEM's values if it can be written (OK).  Not a port.
    subroutine add_value(item, v, ok)
        type(namelist_item_type), intent(inout) :: item
        type(value_type), intent(in) :: v
        logical, intent(out) :: ok

        ok = .true.

        select case (vtag(v))
        case (V_STRING)
            call item%add_string(xstring(v))
        case (V_INTEGER)
            call item%add_integer(xfixnum(v))
        case (V_NULL)
            call item%add_logical(.false.)
        case (V_NAME)
            ok = xsymbol(v) == SYM_T
            if (ok) call item%add_logical(.true.)
        case default
            ok = .false.
        end select
    end subroutine add_value

    !> Save the minibuffer history if it has been modified since the
    !> last save.  Does nothing if Savehist mode is off.  Port of
    !> savehist.el:savehist-autosave.
    recursive subroutine savehist_autosave(status)
        type(status_type), intent(out) :: status

        status = status_type()
        if (minor_modes%savehist_mode%enabled()) call savehist_save(status, auto_save=.true.)
    end subroutine savehist_autosave

    !> savehist_autosave for the timer and `kill_emacs_hook'.  Not a port.
    recursive subroutine savehist_autosave_timer(status)
        type(status_type), intent(out) :: status

        call savehist_autosave(status)
    end subroutine savehist_autosave_timer

    !> Return true if VALUE is printable: a string, an integer, true or
    !> false, or a list of those (what the namelist can hold).  Port of
    !> savehist.el:savehist-printable.
    logical function savehist_printable(value) result(printable)
        type(value_type), intent(in) :: value
        type(value_type), allocatable :: elems(:)
        type(namelist_item_type) :: scratch
        integer :: k

        select case (vtag(value))
        case (V_ARRAY)
            printable = .true.
            call list_elements(value, elems)

            do k = 1, size(elems)
                if (vtag(elems(k)) == V_ARRAY) then
                    printable = .false.
                    return
                end if

                call add_value(scratch, elems(k), printable)
                if (.not. printable) return
            end do
        case default
            call add_value(scratch, value, printable)
        end select
    end function savehist_printable

    !> Record the minibuffer's history variable in
    !> `savehist_minibuffer_history_variables', unless it records no
    !> history (t), is ignored, or is not interned.  Port of
    !> savehist.el:savehist-minibuffer-hook.
    subroutine savehist_minibuffer_hook()
        character(len=64), allocatable :: grown(:)
        integer :: n

        if (.not. allocated(minibuffer_history_variable)) return
        if (minibuffer_history_variable == "t") return
        if (member(minibuffer_history_variable, savehist_ignored_variables)) return
        if (intern_soft(minibuffer_history_variable) == 0) return
        if (.not. allocated(savehist_minibuffer_history_variables)) allocate (savehist_minibuffer_history_variables(0))
        if (member(minibuffer_history_variable, savehist_minibuffer_history_variables)) return
        ! (add-to-list ...): at the front.
        n = size(savehist_minibuffer_history_variables)
        allocate (grown(n + 1))
        grown(1) = minibuffer_history_variable
        grown(2:) = savehist_minibuffer_history_variables
        call move_alloc(grown, savehist_minibuffer_history_variables)
    end subroutine savehist_minibuffer_hook

    !> savehist_minibuffer_hook on `minibuffer_setup_hook'.  Not a port.
    recursive subroutine savehist_minibuffer_hook_proc(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call savehist_minibuffer_hook()
    end subroutine savehist_minibuffer_hook_proc

    recursive subroutine cmd_savehist_save(status)
        type(status_type), intent(out) :: status

        call savehist_save(status, interactive=.true.)
    end subroutine cmd_savehist_save

    !> Define Savehist mode and its command, and the file's default name.
    !> Not a port.
    subroutine syms_of_savehist()
        if (.not. allocated(savehist_file)) savehist_file = state_directory()//"history"
        if (.not. allocated(savehist_coding_system)) savehist_coding_system = "utf-8-emacs-unix"
        if (.not. allocated(savehist_additional_variables)) allocate (savehist_additional_variables(0))
        if (.not. allocated(savehist_ignored_variables)) allocate (savehist_ignored_variables(0))
        if (.not. allocated(savehist_minibuffer_history_variables)) allocate (savehist_minibuffer_history_variables(0))
        call minor_modes%savehist_mode%define("savehist_mode", global=.true., hook=savehist_mode_hook, &
                                              body=savehist_mode_body)
        commands%savehist_mode%run => savehist_mode
        commands%savehist_save%run => cmd_savehist_save
    end subroutine syms_of_savehist
end module savehist_fm
