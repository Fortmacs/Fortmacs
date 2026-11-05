! which_function_mod.f90 -- print current function in mode line.
! Ported from GNU Emacs 31.1 lisp/progmodes/which-func.el.
!
! Copyright (C) 1994-2026 Free Software Foundation, Inc.
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

!> Which Function mode: a global minor mode that shows the name of the
!> function point is in ("[NAME]", in the face `which_function_face') in the mode
!> line, after the modes, as upstream's mode-line-misc-info does.  An idle timer
!> (`which_function_update_delay') updates the selected window's name with
!> `which_function', which asks `which_function_functions', then
!> `add_log_current_subprogram', then the buffer's Imenu index.  What is
!> not ported yet is listed in docs/status/which_function_mod.md.
module which_function_mod
    use, intrinsic :: iso_fortran_env, only: int64, real64
    use error_mod, only: status_type, set_error, format_error, GENERIC_ERROR_CODE, INDEX_MENU_UNAVAILABLE_CODE
    use handle_mod, only: handle_type, null_handle
    use data_mod, only: logical_setting_type, forward_setting
    use hooks_mod, only: hook_type, string_result_hook_type
    use commands_mod, only: commands
    use modes_mod, only: minor_modes
    use buffer_mod, only: buffers, current, current_buffer, buffer_list, is_buffer_live, set_buffer_internal, &
                          buffer_characters_modified_tick, force_mode_line_update, header_line_format_type
    use editing_functions_mod, only: point, point_maximum, goto_character
    use derived_mod, only: is_derived_mode
    use faces_mod, only: named_face_type, display_when, face_attributes, FACE_CLASS_COLOR, FACE_CLASS_GRAYSCALE, &
                         FACE_CLASS_MONO, FACE_BACKGROUND_LIGHT, FACE_BACKGROUND_DARK
    use timer_mod, only: execute_with_idle_timer, destroy_timer, is_timer
    use basic_subprograms_mod, only: after_change_major_mode_hook
    use display_engine_mod, only: message, prepend_mode_line_miscellaneous_info
    use frame_mod, only: selected_window
    use window_mod, only: window_buffer
    use add_log_mod, only: add_log_current_subprogram, add_log_current_subprogram_function
    use index_menu_mod, only: index_menu_index_type, index_menu_create_index_procedure, index_menu_create_index_function, &
                              make_index_menu_index, has_index_menu_index, get_index_menu_index, set_index_menu_index, &
                              current_index_menu_menubar_modified_tick
    implicit none(type, external)
    private

    public :: initialize_which_function, which_function_mode, which_function, which_function_ff_hook, update_which_function, &
              update_which_function_window, try_to_enable_which_function, format_which_function, which_function_last_name
    public :: which_function_index_menu_joiner_procedure, which_function_cleanup_procedure

    !> String to display in the mode line when current function is unknown.
    !> Port of which-func.el which-func-unknown.
    character(len=:), allocatable, public :: which_function_unknown

    !> The major modes for which Which Function mode should be used
    !> (`which_function_modes'): all modes that support it if `all' (the
    !> default), else only the major modes in `modes'.  Port of
    !> which-func.el which-func-modes.
    type, public :: which_function_modes_type
        logical :: all = .true.
        character(len=64), allocatable :: modes(:)
    end type which_function_modes_type

    !> List of major modes for which Which Function mode should be used.
    !> For other modes it is disabled.  If `all' is .true., then Which
    !> Function mode is enabled in any major mode that supports it.  Port of
    !> which-func.el which-func-modes.
    type(which_function_modes_type), public :: which_function_modes

    !> List of major modes where Which Function mode is inactive till Imenu is used.
    !> This means that Which Function mode won't really do anything
    !> until you use Imenu, in these modes.  Note that files
    !> larger than `which_function_maxout' behave in this way too;
    !> Which Function mode doesn't do anything until you use Imenu.  Port of
    !> which-func.el which-func-non-auto-modes.
    character(len=64), allocatable, public :: which_function_non_auto_modes(:)

    !> Don't automatically compute the Imenu menu if buffer is this big or bigger.
    !> Zero means compute the Imenu menu regardless of size.  Port of
    !> which-func.el which-func-maxout.
    integer(int64), public :: which_function_maxout = 500000

    !> Idle time delay before `which_function_mode' updates its display.
    !> When point moves, wait this many seconds after Fortmacs becomes idle
    !> before doing an update.  Port of which-func.el which-func-update-delay.
    real, public :: which_function_update_delay = 0.5

    !> Face used to highlight mode line function names.  Port of
    !> which-func.el:which-func.
    type(named_face_type), target, public :: which_function_face

    abstract interface
        !> A `which_function_index_menu_joiner_function': NAME gets the name to show
        !> for the entry K of INDEX, from its name and those of the
        !> submenus holding it (the names of the menus we had to traverse to
        !> get to the item).  Not a port.
        subroutine which_function_index_menu_joiner_procedure(index, k, name)
            import :: index_menu_index_type
            implicit none(type, external)
            type(index_menu_index_type), intent(in) :: index
            integer, intent(in) :: k
            character(len=:), allocatable, intent(out) :: name
        end subroutine which_function_index_menu_joiner_procedure

        !> A `which_function_cleanup_function': transform NAME before it is
        !> displayed in the mode line.  Not a port.
        subroutine which_function_cleanup_procedure(name)
            implicit none(type, external)
            character(len=:), allocatable, intent(inout) :: name
        end subroutine which_function_cleanup_procedure
    end interface

    !> Function to join together multiple levels of imenu nomenclature.
    !> Called with the index and one of its entries, whose name and the
    !> names of the menus we had to traverse to get to the item it joins
    !> into a single string, the new name of the item.  By default the
    !> item's own name.  Port of which-func.el
    !> which-func-imenu-joiner-function.
    procedure(which_function_index_menu_joiner_procedure), pointer, public :: which_function_index_menu_joiner_function => null()

    !> Function to transform a string before displaying it in the mode line.
    !> The function is called with one argument, the string to display.
    !> Its return value is displayed in the mode line.
    !> If null, no function is called.  The default value is null.
    !>
    !> This feature can be useful if Imenu is set up to make more
    !> detailed entries (e.g., containing the argument list of a function),
    !> and you want to simplify them for the mode line
    !> (e.g., removing the parameter list to just have the function name.)
    !> Port of which-func.el which-func-cleanup-function.
    procedure(which_function_cleanup_procedure), pointer, public :: which_function_cleanup_function => null()

    !> True means display current function name in mode line.
    !> This makes a difference only if variable `which_function_mode' is
    !> true.  Port of which-func.el which-func-mode.
    type(logical_setting_type), target, public :: which_function_mode_variable = logical_setting_type( &
                                                  name="which_function_mode_variable")

    !> Locally .true. in a buffer if `make_index_menu_index' found nothing there.
    !> Port of which-func.el which-function-imenu-failed.
    type(logical_setting_type), target, public :: which_function_index_menu_failed = &
                                                  logical_setting_type(name="which_function_index_menu_failed")

    !> List of functions for `which_function' to call with no arguments.
    !> It calls them sequentially, and if any returns a name,
    !> `which_function' uses that name and stops looking for the name.
    !> Port of which-func.el which-func-functions.
    type(string_result_hook_type), target, public :: which_function_functions = &
                                                     string_result_hook_type(name="which_function_functions")

    !> Hook run after entering or leaving `which_function_mode'.  Port of
    !> the which-function-mode-hook define-minor-mode defines.
    type(hook_type), target, public :: which_function_mode_hook = hook_type(name="which_function_mode_hook")

    ! The name `which_function' found for a window (which-func-table).
    type :: window_name_type
        type(handle_type) :: window
        logical :: has_name = .false.
        character(len=:), allocatable :: name
    end type window_name_type

    type(window_name_type), allocatable :: window_names(:)

    ! which-func-update-timer.
    type(handle_type) :: update_timer

contains

    !> Define Which Function mode, its face and the mode line element (as
    !> loading which-func.el does).  Not a port.
    subroutine initialize_which_function()
        type(named_face_type), pointer :: f
        integer, parameter :: C = FACE_CLASS_COLOR

        call forward_setting(which_function_mode_variable, local_if_set=.true.)
        call forward_setting(which_function_index_menu_failed)
        if (.not. allocated(which_function_unknown)) which_function_unknown = "n/a"
        if (.not. allocated(which_function_modes%modes)) allocate (which_function_modes%modes(0))
        if (.not. allocated(which_function_non_auto_modes)) allocate (which_function_non_auto_modes(0))
        if (.not. allocated(window_names)) allocate (window_names(0))

        if (.not. associated(which_function_index_menu_joiner_function)) then
            which_function_index_menu_joiner_function => which_function_last_name
        end if

        f => which_function_face
        call f%define("which_function_face", "Face used to highlight mode line function names.")
        call f%specification_entry(display_when(classes=[C], minimum_colors=88, backgrounds=[FACE_BACKGROUND_LIGHT]), &
                                   face_attributes(inherit="font_lock_function_name_face"))
        call f%specification_entry(display_when(classes=[FACE_CLASS_GRAYSCALE, FACE_CLASS_MONO], &
                                                backgrounds=[FACE_BACKGROUND_DARK]), &
                                   face_attributes(inherit="font_lock_function_name_face"))
        call f%specification_entry(display_when(classes=[C], backgrounds=[FACE_BACKGROUND_LIGHT]), &
                                   face_attributes(inherit="font_lock_function_name_face"))
        call f%specification_entry(display_when(classes=[C], minimum_colors=88, backgrounds=[FACE_BACKGROUND_DARK]), &
                                   face_attributes(foreground="Blue1"))
        call f%specification_entry(display_when(backgrounds=[FACE_BACKGROUND_DARK]), face_attributes(foreground="Blue1"))
        call f%specification_entry(display_when(), face_attributes(foreground="LightSkyBlue"))
        call minor_modes%which_function_mode%define("which_function_mode", global=.true., &
                                                    body=which_function_mode_body, hook=which_function_mode_hook)
        commands%which_function_mode%execute => which_function_mode
        ! (add-hook 'after-change-major-mode-hook #'which-func-ff-hook t)
        call after_change_major_mode_hook%append(which_function_ff_hook)
        ! (add-to-list 'mode-line-misc-info '(which-function-mode ...))
        call prepend_mode_line_miscellaneous_info(format_which_function)
    end subroutine initialize_which_function

    !> The default `which_function_index_menu_joiner_function': the name of entry K
    !> of INDEX itself (the last of the names).  Port of which-func.el's
    !> default value of which-func-imenu-joiner-function.
    subroutine which_function_last_name(index, k, name)
        type(index_menu_index_type), intent(in) :: index
        integer, intent(in) :: k
        character(len=:), allocatable, intent(out) :: name

        name = index%entries(k)%name
    end subroutine which_function_last_name

    !> The element of upstream's mode-line-misc-info that shows the function point is
    !> in: "[NAME] " in the mode line of WINDOW, NAME in the face
    !> `which_function_face', while `which_function_mode' and `which_function_mode_variable' are
    !> on (SEGMENTS empty otherwise).  Port of which-func.el's
    !> which-func-format, which-func-current and its entry of
    !> mode-line-misc-info, with the mouse keymap and help text left out.
    !> (The name's "%" is not doubled: segments are shown as they are.)
    recursive subroutine format_which_function(window, segments)
        type(handle_type), intent(in) :: window
        type(header_line_format_type), intent(out) :: segments
        integer :: k

        ! Only display if mode is enabled, and if the buffer supports it.
        if (.not. minor_modes%which_function_mode%enabled()) return
        if (.not. which_function_mode_variable%value) return
        call segments%append("")
        call segments%append("[")
        k = window_name_index(window)

        if (k > 0) then
            if (window_names(k)%has_name) then
                call segments%append(window_names(k)%name, "which_function_face")
            else
                call segments%append(which_function_unknown, "which_function_face")
            end if
        else
            call segments%append(which_function_unknown, "which_function_face")
        end if

        call segments%append("]")
        call segments%append(" ")
    end subroutine format_which_function

    ! The index of WINDOW's entry in `window_names', 0 if none.  Not a port.
    integer function window_name_index(window) result(k)
        type(handle_type), intent(in) :: window

        do k = 1, size(window_names)
            if (window_names(k)%window%index == window%index .and. &
                window_names(k)%window%generation == window%generation) return
        end do

        k = 0
    end function window_name_index

    !> Turn on `which_function_mode_variable' in the current buffer if `which_function_mode'
    !> is on and the major mode is one of `which_function_modes'.  Port of
    !> which-func.el:which-func-try-to-enable (the header line is not
    !> ported).
    subroutine try_to_enable_which_function()
        if (.not. minor_modes%which_function_mode%enabled()) return
        if (which_function_mode_variable%local_p()) return

        if (which_function_modes%all) then
            call which_function_mode_variable%set_local(.true.)
        else
            call which_function_mode_variable%set_local(is_derived_mode(which_function_modes%modes))
        end if
    end subroutine try_to_enable_which_function

    !> Turn off `which_function_mode_variable' in the current buffer.  Port of
    !> which-func.el:which-func--disable (the header line is not ported).
    subroutine disable_which_function()
        call which_function_mode_variable%set_local(.false.)
    end subroutine disable_which_function

    !> `after_change_major_mode_hook' for Which Function mode.
    !> It creates the Imenu index for the buffer, if necessary.  Port of
    !> which-func.el:which-func-ff-hook.
    recursive subroutine which_function_ff_hook(status)
        type(status_type), intent(out) :: status
        type(index_menu_index_type) :: index
        procedure(index_menu_create_index_procedure), pointer :: create
        integer(int64) :: here
        logical :: non_auto

        status = status_type()
        call try_to_enable_which_function()
        if (.not. which_function_mode_variable%value) return
        non_auto = .false.
        if (size(which_function_non_auto_modes) > 0) non_auto = is_derived_mode(which_function_non_auto_modes)
        if (non_auto) return
        if (.not. (buffers(current)%save_length < which_function_maxout .or. which_function_maxout == 0)) return
        here = point()
        create => index_menu_create_index_function%current()
        call create(index, status)
        call goto_character(here)

        if (status%ok()) then
            ! An empty index is none (upstream's nil).
            if (size(index%entries) > 0) then
                call set_index_menu_index(index)
            else
                call set_index_menu_index()
            end if
        else if (status%code == INDEX_MENU_UNAVAILABLE_CODE) then
            call disable_which_function()
            status = status_type()
        else
            call message("which_function_ff_hook error: "//format_error(status))
            call disable_which_function()
            status = status_type()
        end if
    end subroutine which_function_ff_hook

    !> Update the Which-Function mode display in the current window.
    !> Port of which-func.el:which-func-update.
    recursive subroutine update_which_function(status)
        type(status_type), intent(out) :: status

        call update_which_function_window(selected_window(), status)
    end subroutine update_which_function

    !> Update the Which Function mode display for window WINDOW.  Port of
    !> which-func.el:which-func-update-1.
    recursive subroutine update_which_function_window(window, status)
        type(handle_type), intent(in) :: window
        type(status_type), intent(out) :: status
        type(handle_type) :: buffer
        type(window_name_type) :: entry
        character(len=:), allocatable :: name
        integer(int64) :: saved
        integer :: k
        logical :: same

        status = status_type()
        buffer = window_buffer(window, status)
        if (.not. status%ok()) return
        if (.not. is_buffer_live(buffer)) return
        saved = current
        call set_buffer_internal(buffer%index)

        if (which_function_mode_variable%value) then
            call which_function(name, status)

            if (.not. status%ok()) then
                call disable_which_function()
                call set_error(status, GENERIC_ERROR_CODE, "Error in update_which_function: "//format_error(status))
            else
                k = window_name_index(window)

                if (k == 0) then
                    entry%window = window
                    window_names = [window_names, entry]
                    k = size(window_names)
                    same = .not. allocated(name)
                else if (allocated(name) .neqv. window_names(k)%has_name) then
                    same = .false.
                else if (allocated(name)) then
                    same = name == window_names(k)%name .and. len(name) == len(window_names(k)%name)
                else
                    same = .true.
                end if

                if (.not. same) then
                    window_names(k)%has_name = allocated(name)
                    if (allocated(name)) window_names(k)%name = name
                    call force_mode_line_update(.false.)
                end if
            end if
        end if

        if (saved > 0) call set_buffer_internal(saved)
    end subroutine update_which_function_window

    !> Toggle mode line display of current function (Which Function mode).
    !>
    !> Which Function mode is a global minor mode.  When enabled, the
    !> current function name is continuously displayed in the mode line,
    !> in certain major modes.  Port of which-func.el:which-function-mode.
    subroutine which_function_mode(status)
        type(status_type), intent(out) :: status

        call minor_modes%which_function_mode%command(status)
    end subroutine which_function_mode

    !> The body of Which Function mode: the idle timer, and
    !> `which_function_ff_hook' in every buffer.  Port of
    !> which-func.el:which-function-mode (its body).
    recursive subroutine which_function_mode_body(enable, status)
        logical, intent(in) :: enable
        type(status_type), intent(out) :: status
        type(handle_type), allocatable :: list(:)
        type(status_type) :: st
        integer(int64) :: saved
        integer :: i

        status = status_type()
        if (enable) continue
        if (is_timer(update_timer)) call destroy_timer(update_timer)
        update_timer = null_handle()

        if (minor_modes%which_function_mode%enabled()) then
            ! Turn it on.
            call execute_with_idle_timer(update_timer, real(which_function_update_delay, real64), .true., update_which_function, &
                                         status)
            if (.not. status%ok()) return
        end if

        list = buffer_list()
        saved = current

        do i = 1, size(list)
            if (.not. is_buffer_live(list(i))) cycle
            call set_buffer_internal(list(i)%index)
            call which_function_ff_hook(st)
        end do

        if (saved > 0) then
            if (is_buffer_live(current_buffer_of(saved))) call set_buffer_internal(saved)
        end if

    contains

        ! The handle of buffer slot B.
        function current_buffer_of(b) result(h)
            integer(int64), intent(in) :: b
            type(handle_type) :: h
            integer(int64) :: here

            here = current
            call set_buffer_internal(b)
            h = current_buffer()
            if (here > 0) call set_buffer_internal(here)
        end function current_buffer_of
    end subroutine which_function_mode_body

    !> Return current function name based on point (NAME, unallocated if no
    !> function name is found).
    !> Uses `which_function_functions', `add_log_current_subprogram'
    !> or the Imenu index.  Port of which-func.el:which-function.
    recursive subroutine which_function(name, status)
        character(len=:), allocatable, intent(out) :: name
        type(status_type), intent(out) :: status
        type(index_menu_index_type) :: index
        type(status_type) :: st
        integer(int64) :: minoffset, tick

        ! Try the `which_function_functions' functions first.
        call which_function_functions%execute(name, status)
        if (.not. status%ok()) return
        ! Try using add-log support.
        if (.not. allocated(name)) call add_log_current_subprogram(name)

        ! If Imenu is loaded, try to make an index with it.
        ! If `add_log_current_subprogram' ran and gave nothing, accept that.
        if (.not. allocated(name) .and. .not. associated(add_log_current_subprogram_function%current())) then
            call buffer_characters_modified_tick(current_buffer(), tick, st)
            if (.not. st%ok()) tick = 0

            if (.not. which_function_index_menu_failed%value) then
                if (.not. has_index_menu_index() .or. tick /= current_index_menu_menubar_modified_tick()) then
                    call make_index_menu_index(index, st, noerror=.true.)
                    if (.not. has_index_menu_index()) call which_function_index_menu_failed%set_local(.true.)
                end if
            end if

            ! If we have an index, use it.
            if (has_index_menu_index()) then
                call get_index_menu_index(index)
                minoffset = point_maximum()
                call closest(0)
            end if
        end if

        ! Filter the name if requested.
        if (allocated(name) .and. associated(which_function_cleanup_function)) call which_function_cleanup_function(name)

    contains

        ! Find the closest item at or before point among the entries of
        ! submenu PARENT and below.
        recursive subroutine closest(parent)
            integer, intent(in) :: parent
            integer, allocatable :: list(:)
            integer(int64) :: offset
            integer :: i

            list = index%children(parent)

            do i = 1, size(list)
                if (index%entries(list(i))%submenu) then
                    call closest(list(i))
                else
                    offset = point() - index%position_of(list(i))

                    if (offset >= 0 .and. offset < minoffset) then
                        minoffset = offset

                        if (associated(which_function_index_menu_joiner_function)) then
                            call which_function_index_menu_joiner_function(index, list(i), name)
                        else
                            name = index%entries(list(i))%name
                        end if
                    end if
                end if
            end do
        end subroutine closest
    end subroutine which_function
end module which_function_mod
