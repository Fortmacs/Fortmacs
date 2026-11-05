! save_history_mod.f90 -- Save minibuffer history.
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
!> `save_history_additional_variables'.
!>
!> To use savehist, turn on `save_history_mode'.  You can also explicitly
!> save history with `M-x save_history_save'.
!>
!> The file is in namelist syntax (namelist_mod) rather than Lisp, so it
!> is read, not loaded: its objects are the variables to set.  A value
!> is saved if it is a string, an integer, true or false, or a list of
!> those; other values (names, nested lists) are not printable.
!> Deferred: the text properties of saved strings, the warning about a
!> file that can't be written (warnings.el is not ported), and the
!> obsolete `save_history_trim_history'.
module save_history_mod
    use, intrinsic :: iso_fortran_env, only: int32, int64, real64
    use error_mod, only: status_type, set_error, format_error, GENERIC_ERROR_CODE
    use handle_mod, only: handle_type
    use value_mod, only: value_type, make_list, list_elements, vtag, xstring, xfixnum, xsymbol, symbols, V_NULL, &
                         V_INTEGER, V_STRING, V_ARRAY, V_NAME, SYMBOL_T
    use names_mod, only: intern_soft
    use data_mod, only: set, symbol_value, boundp
    use utility_functions_mod, only: value_equal
    use hooks_mod, only: hook_type
    use easy_minor_mode_mod, only: mode_called_interactively
    use modes_mod, only: minor_modes
    use commands_mod, only: commands
    use timer_mod, only: execute_with_timer, destroy_timer, is_timer
    use buffer_mod, only: get_or_make_buffer, generate_new_buffer_name, set_buffer, close_buffer, current_buffer
    use insert_delete_mod, only: insert
    use editing_functions_mod, only: buffer_string
    use file_input_output_mod, only: file_exists, file_attributes, file_status_type, set_file_modes, write_region, &
                                     insert_file_contents, file_name_directory, expand_file_name
    use files_mod, only: create_directory
    use configuration_mod, only: state_directory
    use coding_mod, only: coding_system_for_write
    use minibuffer_reading_mod, only: minibuffer_history_variable, minibuffer_setup_hook
    use keyboard_mod, only: exit_fortmacs_hook
    use display_engine_mod, only: message
    use namelist_mod, only: namelist_group_type, namelist_item_type, namelist_value_type, read_namelist, namelist_text, &
                            NAMELIST_STRING, NAMELIST_INTEGER, NAMELIST_LOGICAL
    implicit none(type, external)
    private

    public :: save_history_mode, save_history_save, save_history_autosave, save_history_install, save_history_uninstall, &
              save_history_minibuffer_hook, save_history_printable, initialize_save_history

    !> If true, save all recorded minibuffer histories.
    !> If you want to save only specific histories, use `save_history_save_hook'
    !> to modify the value of `save_history_minibuffer_history_variables'.
    !> Port of savehist.el savehist-save-minibuffer-history.
    logical, public :: save_history_save_minibuffer_history = .true.

    !> An element of `save_history_additional_variables': the variable NAME,
    !> and MAXIMUM_SIZE, the most elements of a list value to save (negative
    !> for no limit; upstream's (VARIABLE . MAX-SIZE)).
    type, public :: save_history_variable_type
        character(len=64) :: name = ""
        integer :: maximum_size = -1
    end type save_history_variable_type

    !> List of additional variables to save.
    !> Each element is a variable that will be persisted across Fortmacs
    !> sessions that use Savehist.
    !>
    !> An element names the variable, and may give a MAXIMUM_SIZE, which means
    !> to truncate the variable's value to at most MAXIMUM_SIZE elements (if
    !> the value is a list) before saving the value:
    !>
    !>     save_history_additional_variables = [save_history_variable_type(name="search_ring"), &
    !>                                      save_history_variable_type(name="clipboard_history", maximum_size=20)]
    !>
    !> The contents of variables should be printable.  You don't need to
    !> add minibuffer history variables to this list, all minibuffer
    !> histories will be saved automatically as long as
    !> `save_history_save_minibuffer_history' is true.
    !>
    !> User options should be saved with the Customize interface.  This
    !> list is useful for saving automatically updated variables that are
    !> not minibuffer histories, such as `compile_command' or `clipboard_history'.
    !>
    !> NOTE: If this variable is not empty, then Fortmacs will not try to
    !> handle gracefully concurrent modifications to the `save_history_file',
    !> e.g. by other Fortmacs sessions or other tools like
    !> file-synchronization systems.  Port of savehist.el
    !> savehist-additional-variables.
    type(save_history_variable_type), allocatable, public :: save_history_additional_variables(:)

    !> List of additional variables not to save.  Port of savehist.el
    !> savehist-ignored-variables.
    character(len=64), allocatable, public :: save_history_ignored_variables(:)

    !> File name where minibuffer history is saved to and loaded from.
    !> The minibuffer history is a namelist read automatically when
    !> Savehist mode is turned on.  See `save_history_mode' for more details.
    !> Port of savehist.el savehist-file (in the state directory,
    !> `~/.local/state/fortmacs/history').
    character(len=:), allocatable, public :: save_history_file

    !> Default permissions of the history file.
    !> This is decimal, not octal.  The default is 384 (0600 in octal).
    !> Set to -1 to use the default permissions that Fortmacs uses,
    !> typically mandated by umask.  The default is a bit more restrictive
    !> to protect the user's privacy.  Port of savehist.el
    !> savehist-file-modes (-1 for none).
    integer, public :: save_history_file_modes = 384

    !> The interval between autosaves of minibuffer history, in seconds.
    !> If set to 0, disables timer-based autosaving.  Port of savehist.el
    !> savehist-autosave-interval (0 for none; it takes effect when the
    !> timer is next set, as Savehist mode does).
    integer, public :: save_history_autosave_interval = 5*60

    !> Hook called when Savehist mode is turned on.  Port of savehist.el
    !> savehist-mode-hook.
    type(hook_type), target, public :: save_history_mode_hook = hook_type(name="save_history_mode_hook")

    !> Hook called by `save_history_save' before saving the variables.
    !> You can use this hook to influence choice and content of variables
    !> to save.  Port of savehist.el savehist-save-hook.
    type(hook_type), target, public :: save_history_save_hook = hook_type(name="save_history_save_hook")

    !> The coding system Savehist uses for saving the minibuffer history.
    !> Changing this value while Fortmacs is running is supported, but
    !> considered unwise, unless you know what you are doing.  Port of
    !> savehist.el savehist-coding-system.
    character(len=:), allocatable, public :: save_history_coding_system

    !> List of minibuffer histories.
    !> The contents of this variable is built while Fortmacs is running,
    !> and saved along with minibuffer history.  You can change its value
    !> off `save_history_save_hook' to influence which variables are saved.
    !> Port of savehist.el savehist-minibuffer-history-variables.
    character(len=64), allocatable, public :: save_history_minibuffer_history_variables(:)

    !> Whether the history has already been loaded.  Port of savehist.el
    !> savehist-loaded (obsolete since 31.1).
    logical, public :: save_history_loaded = .false.

    !> Modtime of the `save_history_file' when we last sync'd up with it
    !> (savehist--file-sync-modtime); KNOWN .false. for none.  A file that
    !> doesn't exist has the modtime t (EXISTS false).
    type :: modtime_type
        logical :: known = .false., exists = .false.
        integer(int64) :: second = 0
        integer(int32) :: nsec = 0
    end type modtime_type
    type(modtime_type) :: save_history_file_synchronize_modtime

    !> The text last written (savehist.el compares its md5,
    !> savehist-last-checksum); unallocated for none.
    character(len=:), allocatable :: save_history_last_text

    type(handle_type) :: save_history_timer
    logical :: save_history_has_given_file_warning = .false.

contains

    !> Cancel `save_history_autosave' timer, if set.  Port of
    !> savehist.el:savehist--cancel-timer.
    subroutine save_history_cancel_timer()
        if (is_timer(save_history_timer)) call destroy_timer(save_history_timer)
        save_history_timer = handle_type()
    end subroutine save_history_cancel_timer

    !> Set or cancel an invocation of `save_history_autosave' on a timer.
    !> If `save_history_mode' is enabled, set the timer, otherwise cancel the
    !> timer.  Port of savehist.el:savehist--manage-timer.
    subroutine save_history_manage_timer()
        type(status_type) :: st

        if (minor_modes%save_history_mode%enabled() .and. save_history_autosave_interval > 0 .and. &
            .not. is_timer(save_history_timer)) then
            call execute_with_timer(save_history_timer, real(save_history_autosave_interval, real64), save_history_autosave_timer, &
                                    st, real(save_history_autosave_interval, real64))
        else
            call save_history_cancel_timer()
        end if
    end subroutine save_history_manage_timer

    !> Toggle saving of minibuffer history (Savehist mode).
    !>
    !> When Savehist mode is enabled, minibuffer history is saved to
    !> `save_history_file' periodically and when exiting Fortmacs.  When
    !> Savehist mode is enabled for the first time in a Fortmacs session,
    !> it loads the previous minibuffer histories from `save_history_file'.
    !> The variable `save_history_autosave_interval' controls the periodicity
    !> of saving minibuffer histories.
    !>
    !> If `save_history_save_minibuffer_history' is true (the default), all
    !> recorded minibuffer histories will be saved.  You can arrange for
    !> additional history variables to be saved and restored by customizing
    !> `save_history_additional_variables', which by default is an empty list.
    !>
    !> Alternatively, you could customize `save_history_save_minibuffer_history'
    !> to false, and add to `save_history_additional_variables' only those
    !> history variables you want to save.
    !>
    !> To ignore some history variables, add their names to the list in
    !> `save_history_ignored_variables'.
    !>
    !> This mode should normally be turned on from your init file.
    !> Calling it at any other time replaces your current minibuffer
    !> histories, which is probably undesirable.  Port of
    !> savehist.el:savehist-mode.
    recursive subroutine save_history_mode(status)
        type(status_type), intent(out) :: status

        call minor_modes%save_history_mode%command(status)
    end subroutine save_history_mode

    !> The body of Savehist mode.  Port of savehist.el:savehist-mode (its
    !> body).
    recursive subroutine save_history_mode_body(enable, status)
        logical, intent(in) :: enable
        type(status_type), intent(out) :: status

        status = status_type()

        if (.not. enable) then
            call save_history_uninstall()
        else
            call save_history_reload(mode_called_interactively, status)
            if (.not. status%ok()) return
            call save_history_install()
        end if
    end subroutine save_history_mode_body

    !> The modtime of `save_history_file'.  Port of
    !> savehist.el:savehist--file-modtime.
    function save_history_file_modtime() result(m)
        type(modtime_type) :: m
        type(file_status_type) :: st
        logical :: found

        m%known = .true.
        call file_attributes(save_history_file, st, found)
        m%exists = found

        if (found) then
            m%second = st%mtime_second
            m%nsec = st%mtime_nsec
        end if
    end function save_history_file_modtime

    ! Not a port.
    logical function same_modtime(a, b)
        type(modtime_type), intent(in) :: a, b

        same_modtime = (a%known .eqv. b%known) .and. (a%exists .eqv. b%exists) .and. a%second == b%second .and. &
                       a%nsec == b%nsec
    end function same_modtime

    !> Load the history data from `save_history_file'.
    !> Be careful to do it while preserving the current history data.
    !> Port of savehist.el:savehist--reload.
    recursive subroutine save_history_reload(interactively, status)
        logical, intent(in) :: interactively
        type(status_type), intent(out) :: status
        character(len=64), allocatable :: old_names(:)
        type(value_type), allocatable :: old_values(:)
        type(value_type) :: v, current_value
        type(status_type) :: st
        logical :: bound
        integer :: k, n

        status = status_type()
        if (.not. file_exists(save_history_file)) return
        if (save_history_loaded .and. same_modtime(save_history_file_synchronize_modtime, save_history_file_modtime())) return
        ! The history variables we know about, with their values.
        n = 0
        if (allocated(save_history_minibuffer_history_variables)) n = size(save_history_minibuffer_history_variables)
        allocate (old_names(n), old_values(n))

        do k = 1, n
            old_names(k) = save_history_minibuffer_history_variables(k)
            call boundp(value_type(name=trim(old_names(k))), bound, st)
            if (bound) call symbol_value(value_type(name=trim(old_names(k))), old_values(k), st)
            if (.not. bound) old_names(k) = ""
        end do

        if (interactively) call message("Loading "//save_history_file//" (source)...")
        call load_save_history_file(status)

        if (.not. status%ok()) then
            ! Don't install the mode if reading failed.  Doing so would
            ! effectively destroy the user's data at the next save.
            call minor_modes%save_history_mode%set_state(.false.)
            call save_history_uninstall()
            return
        end if

        if (interactively) call message("Loading "//save_history_file//" (source)...done")
        save_history_file_synchronize_modtime = save_history_file_modtime()
        save_history_loaded = .true.

        ! In case we're loading the file late, there was info in the
        ! history variables that may have been overwritten by the info
        ! extracted from the file, so put it back in.
        do k = 1, n
            if (len_trim(old_names(k)) == 0) cycle
            call symbol_value(value_type(name=trim(old_names(k))), current_value, st)
            if (.not. st%ok()) cycle
            if (value_equal(old_values(k), current_value)) cycle
            v = save_history_merge(old_values(k), current_value)
            call set(value_type(name=trim(old_names(k))), v, st)
        end do
    end subroutine save_history_reload

    !> Set the variables of `save_history_file' to their saved values.
    !> Not a port.
    recursive subroutine load_save_history_file(status)
        type(status_type), intent(out) :: status
        type(namelist_group_type), allocatable :: groups(:)
        character(len=:), allocatable :: text
        integer :: g, k

        call file_text(save_history_file, text, status)
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
    end subroutine load_save_history_file

    !> Set the variable of ITEM to its value.  Not a port.
    recursive subroutine set_saved(item, status)
        type(namelist_item_type), intent(in) :: item
        type(status_type), intent(out) :: status
        type(value_type), allocatable :: elements(:)
        integer :: k

        status = status_type()

        if (item%name == "save_history_minibuffer_history_variables") then
            if (allocated(save_history_minibuffer_history_variables)) deallocate (save_history_minibuffer_history_variables)

            if (.not. item%subscripted) then
                allocate (save_history_minibuffer_history_variables(0))
                return
            end if

            allocate (save_history_minibuffer_history_variables(size(item%values)))

            do k = 1, size(item%values)
                save_history_minibuffer_history_variables(k) = item%values(k)%string
            end do

            return
        end if

        if (item%subscripted) then
            allocate (elements(size(item%values)))

            do k = 1, size(item%values)
                elements(k) = saved_value(item%values(k))
            end do

            call set(value_type(name=item%name), make_list(elements), status)
        else if (size(item%values) > 0) then
            call set(value_type(name=item%name), saved_value(item%values(1)), status)
        end if
    end subroutine set_saved

    !> A value as savehist saved it.  Not a port.
    function saved_value(nv) result(v)
        type(namelist_value_type), intent(in) :: nv
        type(value_type) :: v

        select case (nv%kind)
        case (NAMELIST_INTEGER)
            v = value_type(nv%integer)
        case (NAMELIST_LOGICAL)
            if (nv%logical) v = value_type(.true.)
        case default
            v = value_type(text=nv%string)
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
        call get_or_make_buffer(generate_new_buffer_name(" *temp*"), h, st)
        call set_buffer(h, st)
        call insert_file_contents(file, .false., inserted, status)
        if (status%ok()) text = buffer_string()
        call close_buffer(h, st)
        call set_buffer(old, st)
    end subroutine file_text

    !> Merge the OLD history we had with the NEW history we just loaded.
    !> We don't know the relative age of the various entries in OLD and
    !> NEW, so the two lists are interleaved, then duplicates dropped.
    !> Port of savehist.el:savehist--merge.
    function save_history_merge(old, new) result(merged)
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
            call append(o(i))
            call append(n(j))
            i = i + 1
            j = j + 1
        end do

        do while (i <= size(o))
            call append(o(i))
            i = i + 1
        end do

        do while (j <= size(n))
            call append(n(j))
            j = j + 1
        end do

        merged = make_list(res(1:m))

    contains

        !> Append E unless an equal element is there already (delete-dups).
        subroutine append(e)
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
        end subroutine append
    end function save_history_merge

    !> Hook Savehist into Fortmacs.
    !> Normally invoked by calling `save_history_mode' to set the minor mode.
    !> Installs `save_history_autosave' in `exit_fortmacs_hook' and on a timer.
    !> To undo this, call `save_history_uninstall'.  Port of
    !> savehist.el:savehist-install.
    subroutine save_history_install()
        call minibuffer_setup_hook%prepend(save_history_minibuffer_hook_adapter)
        call exit_fortmacs_hook%prepend(save_history_autosave_timer)
        call save_history_manage_timer()
    end subroutine save_history_install

    !> Undo installing savehist.
    !> Normally invoked by calling `save_history_mode' to unset the minor mode.
    !> Port of savehist.el:savehist-uninstall.
    subroutine save_history_uninstall()
        call minibuffer_setup_hook%remove(save_history_minibuffer_hook_adapter)
        call exit_fortmacs_hook%remove(save_history_autosave_timer)
        call save_history_manage_timer()
    end subroutine save_history_uninstall

    !> Whether NAME is in the list NAMES.  Not a port.
    pure logical function has_name(name, names)
        character(len=*), intent(in) :: name
        character(len=64), allocatable, intent(in) :: names(:)

        has_name = .false.
        if (allocated(names)) has_name = any(names == name)
    end function has_name

    !> Save the values of minibuffer history variables.
    !> Unbound variables named in `save_history_additional_variables' are
    !> ignored.  If AUTO_SAVE is true, compare the saved contents to the
    !> one last saved, and don't save the buffer if they are the same.
    !> Port of savehist.el:savehist-save.
    recursive subroutine save_history_save(status, auto_save, interactive)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: auto_save, interactive
        type(namelist_group_type) :: group
        type(namelist_item_type) :: item
        type(value_type) :: value
        type(value_type), allocatable :: elements(:)
        type(handle_type) :: old, h
        type(status_type) :: st
        character(len=:), allocatable :: text, wrote, directory, name
        logical :: auto, bound, ok
        integer :: k, j, n, m

        status = status_type()
        auto = .false.
        if (present(auto_save)) auto = auto_save
        ! We don't know how to merge old and new values of those vars in
        ! `save_history_additional_variables', so only do the auto-sync if
        ! there aren't such variables.
        n = 0
        if (allocated(save_history_additional_variables)) n = size(save_history_additional_variables)

        if (n == 0 .and. .not. same_modtime(save_history_file_synchronize_modtime, save_history_file_modtime())) then
            ! The file has been changed since last time we saw it.
            ! Probably some other Fortmacs session.  Load the corresponding
            ! info so we don't end up throwing it away by blindly
            ! overwriting it.
            call save_history_reload(.false., st)
        end if

        call save_history_save_hook%execute(st)
        group%name = "savehist"

        ! Save the minibuffer histories, along with the value of
        ! save_history_minibuffer_history_variables itself.
        if (save_history_save_minibuffer_history) then
            item = namelist_item_type(name="save_history_minibuffer_history_variables")
            if (.not. allocated(save_history_minibuffer_history_variables)) allocate (save_history_minibuffer_history_variables(0))

            if (size(save_history_minibuffer_history_variables) == 0) then
                call item%append_logical(.false.)
            else
                call subscript(item, size(save_history_minibuffer_history_variables))

                do k = 1, size(save_history_minibuffer_history_variables)
                    call item%append_string(trim(save_history_minibuffer_history_variables(k)))
                end do
            end if

            call group%append(item)

            do k = 1, size(save_history_minibuffer_history_variables)
                ! (A variable, not an associate name: with gfortran, `cycle' out
                ! of an associate block whose selector is an expression frees
                ! its temporary twice.)
                name = trim(save_history_minibuffer_history_variables(k))
                call boundp(value_type(name=name), bound, st)
                if (.not. bound .or. has_name(name, save_history_ignored_variables)) cycle
                call symbol_value(value_type(name=name), value, st)
                ! Don't save empty histories.
                if (.not. allocated(value%a)) cycle
                ! Print elements of VALUE one by one, carefully: those
                ! that can't be written are left out.
                item = namelist_item_type(name=name)
                call list_elements(value, elements)

                do j = 1, size(elements)
                    call append_value(item, elements(j), ok)
                end do

                if (.not. allocated(item%values)) cycle
                call subscript(item, size(item%values))
                call group%append(item)
            end do
        end if

        ! Save the additional variables.
        do k = 1, n
            associate (variable => save_history_additional_variables(k))
                if (has_name(variable%name, save_history_minibuffer_history_variables)) cycle
                call boundp(value_type(name=trim(variable%name)), bound, st)
                if (.not. bound) cycle
                call symbol_value(value_type(name=trim(variable%name)), value, st)
                if (.not. save_history_printable(value)) cycle
                item = namelist_item_type(name=trim(variable%name))

                if (vtag(value) == V_ARRAY) then
                    ! When we have a max-size, chop off the last elements.
                    call list_elements(value, elements)
                    m = size(elements)
                    if (variable%maximum_size >= 0) m = min(m, variable%maximum_size)

                    do j = 1, m
                        call append_value(item, elements(j), ok)
                    end do

                    if (m == 0) then
                        call item%append_logical(.false.)
                    else
                        call subscript(item, m)
                    end if
                else
                    call append_value(item, value, ok)
                end if

                call group%append(item)
            end associate
        end do

        text = "! Minibuffer history file, automatically generated by `savehist'."//achar(10)//achar(10)// &
               namelist_text(group)

        ! If autosaving, avoid writing if nothing has changed since the
        ! last write.
        if (auto .and. allocated(save_history_last_text)) then
            if (save_history_last_text == text .and. len(save_history_last_text) == len(text)) return
        end if

        ! Ensure that the directory exists before saving.
        directory = file_name_directory(save_history_file)
        if (.not. file_exists(directory)) call create_directory(directory, st, parents=.true.)
        old = current_buffer()
        call get_or_make_buffer(generate_new_buffer_name(" *temp*"), h, st)
        call set_buffer(h, st)
        call insert(text, st)
        coding_system_for_write = save_history_coding_system
        call write_region(save_history_file, .false., status, message=wrote)
        call coding_system_for_write%clear()
        call close_buffer(h, st)
        call set_buffer(old, st)

        if (.not. status%ok()) then
            ! (The warning is Deferred: warnings.el is not ported.)
            save_history_has_given_file_warning = .true.
            status = status_type()
            return
        end if

        if (present(interactive)) then
            if (interactive) call message(wrote)
        end if

        if (save_history_file_modes >= 0) call set_file_modes(save_history_file, int(save_history_file_modes, int32), st)
        save_history_file_synchronize_modtime = save_history_file_modtime()
        save_history_last_text = text

    contains

        subroutine subscript(it, count)
            type(namelist_item_type), intent(inout) :: it
            integer, intent(in) :: count

            it%subscripted = .true.
            it%lower = 1
            it%upper = count
        end subroutine subscript
    end subroutine save_history_save

    !> Add V to ITEM's values if it can be written (OK).  Not a port.
    subroutine append_value(item, v, ok)
        type(namelist_item_type), intent(inout) :: item
        type(value_type), intent(in) :: v
        logical, intent(out) :: ok

        ok = .true.

        select case (vtag(v))
        case (V_STRING)
            call item%append_string(xstring(v))
        case (V_INTEGER)
            call item%append_integer(xfixnum(v))
        case (V_NULL)
            call item%append_logical(.false.)
        case (V_NAME)
            ok = xsymbol(v) == SYMBOL_T
            if (ok) call item%append_logical(.true.)
        case default
            ok = .false.
        end select
    end subroutine append_value

    !> Save the minibuffer history if it has been modified since the
    !> last save.  Does nothing if Savehist mode is off.  Port of
    !> savehist.el:savehist-autosave.
    recursive subroutine save_history_autosave(status)
        type(status_type), intent(out) :: status

        status = status_type()
        if (minor_modes%save_history_mode%enabled()) call save_history_save(status, auto_save=.true.)
    end subroutine save_history_autosave

    !> save_history_autosave for the timer and `exit_fortmacs_hook'.  Not a port.
    recursive subroutine save_history_autosave_timer(status)
        type(status_type), intent(out) :: status

        call save_history_autosave(status)
    end subroutine save_history_autosave_timer

    !> Return true if VALUE is printable: a string, an integer, true or
    !> false, or a list of those (what the namelist can hold).  Port of
    !> savehist.el:savehist-printable.
    logical function save_history_printable(value) result(printable)
        type(value_type), intent(in) :: value
        type(value_type), allocatable :: elements(:)
        type(namelist_item_type) :: scratch
        integer :: k

        select case (vtag(value))
        case (V_ARRAY)
            printable = .true.
            call list_elements(value, elements)

            do k = 1, size(elements)
                if (vtag(elements(k)) == V_ARRAY) then
                    printable = .false.
                    return
                end if

                call append_value(scratch, elements(k), printable)
                if (.not. printable) return
            end do
        case default
            call append_value(scratch, value, printable)
        end select
    end function save_history_printable

    !> Record the minibuffer's history variable in
    !> `save_history_minibuffer_history_variables', unless it records no
    !> history (t), is ignored, or is not interned.  Port of
    !> savehist.el:savehist-minibuffer-hook.
    subroutine save_history_minibuffer_hook()
        if (.not. allocated(minibuffer_history_variable)) return
        if (minibuffer_history_variable == "t") return
        if (has_name(minibuffer_history_variable, save_history_ignored_variables)) return
        if (intern_soft(minibuffer_history_variable) == 0) return
        if (.not. allocated(save_history_minibuffer_history_variables)) allocate (save_history_minibuffer_history_variables(0))
        if (has_name(minibuffer_history_variable, save_history_minibuffer_history_variables)) return
        ! (add-to-list ...): at the front.
        save_history_minibuffer_history_variables = [character(len=64) :: minibuffer_history_variable, &
                                                     save_history_minibuffer_history_variables]
    end subroutine save_history_minibuffer_hook

    !> save_history_minibuffer_hook on `minibuffer_setup_hook'.  Not a port.
    recursive subroutine save_history_minibuffer_hook_adapter(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call save_history_minibuffer_hook()
    end subroutine save_history_minibuffer_hook_adapter

    recursive subroutine execute_save_history_save(status)
        type(status_type), intent(out) :: status

        call save_history_save(status, interactive=.true.)
    end subroutine execute_save_history_save

    !> Define Savehist mode and its command, and the file's default name.
    !> Not a port.
    subroutine initialize_save_history()
        if (.not. allocated(save_history_file)) save_history_file = state_directory()//"history"
        if (.not. allocated(save_history_coding_system)) save_history_coding_system = "utf-8-emacs-unix"
        if (.not. allocated(save_history_additional_variables)) allocate (save_history_additional_variables(0))
        if (.not. allocated(save_history_ignored_variables)) allocate (save_history_ignored_variables(0))
        if (.not. allocated(save_history_minibuffer_history_variables)) allocate (save_history_minibuffer_history_variables(0))
        call minor_modes%save_history_mode%define("save_history_mode", global=.true., hook=save_history_mode_hook, &
                                                  body=save_history_mode_body)
        commands%save_history_mode%execute => save_history_mode
        commands%save_history_save%execute => execute_save_history_save
    end subroutine initialize_save_history
end module save_history_mod
