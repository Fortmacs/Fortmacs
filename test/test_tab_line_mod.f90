! test_tab_line_mod.f90 -- tests for tab_line_mod.
! Own tests; GNU Emacs 31.1 has no test/lisp/tab-line-tests.el.
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

!> Tests of Tab Line mode and Global Tab-Line mode, the tabs of a window
!> (fixed window buffers, groups), the tab line's text and its automatic
!> and manual scrolling, switching, moving and closing tabs, and the keys
!> of C-x.  Each test starts and ends with the initial frame alone, 24
!> lines by 80 columns, showing buffer "tab-a" of the buffers "tab-a",
!> "tab-b" and "tab-c".
module test_tab_line_mod
    use, intrinsic :: iso_fortran_env, only: int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_mod, only: status_type
    use handle_mod, only: handle_type
    use modes_mod, only: minor_modes
    use bindings_mod, only: initialize_bindings, control_x_map
    use keyboard_mod, only: last_message, unread_command_events, command_loop, last_command
    use keymap_mod, only: key_parse, lookup_key
    use value_mod, only: value_type, make_keymap_reference, vtag, xsymbol, symbols, V_NAME
    use commands_mod, only: commands
    use execute_interactively_mod, only: this_command
    use buffer_mod, only: get_or_make_buffer, set_buffer, close_buffer, current_buffer, is_buffer_live, buffers, current, &
                          header_line_segment_type
    use frame_mod, only: selected_frame, ensure_frame_windows, delete_other_frames
    use window_mod, only: windows, resize_window_tree, set_window_buffer, same_handle, initialize_window_display, &
                          delete_other_windows, switch_to_buffer, window_wants_tab_line, destroy_previous_buffer
    use tab_line_mod, only: format_tab_line, tab_line_tabs_function, get_tab_line_tabs_fixed_window_buffers, &
                            get_tab_line_tabs_buffer_groups, get_tab_line_tabs_window_buffers, tab_line_tab_type, &
                            switch_to_tab_line_previous_tab, switch_to_tab_line_next_tab, move_tab_line_tab_forward, &
                            close_tab_line_tab, close_tab_line_other_tabs, scroll_tab_line, tab_line_switch_cycling, &
                            tab_line_close_tab_function, TAB_LINE_CLOSE_TAB_FUNCTION_BURY_BUFFER, &
                            TAB_LINE_CLOSE_TAB_FUNCTION_CLOSE_BUFFER, tab_line_exclude, force_tab_line_update
    implicit none(type, external)
    private

    public :: collect_tab_line_mod

    !> The test buffers.
    type(handle_type) :: a, b, c

contains

    !> Collect all exported unit tests.
    subroutine collect_tab_line_mod(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("tab-line-mode", test_mode), &
                    new_unittest("tab-line-fixed-window-buffers", test_fixed_window_buffers), &
                    new_unittest("tab-line-switch-and-move", test_switch_and_move), &
                    new_unittest("tab-line-close", test_close), &
                    new_unittest("tab-line-hscroll", test_hscroll), &
                    new_unittest("tab-line-groups", test_groups), &
                    new_unittest("tab-line-global-mode-keys", test_global_mode_keys) &
                    ]
    end subroutine collect_tab_line_mod

    !> The initial frame alone, 24 by 80, showing buffer "tab-a".
    subroutine enter()
        type(status_type) :: st

        call initialize_bindings()
        call ensure_frame_windows()
        call delete_other_frames(st)
        call initialize_window_display()
        selected_frame%terminal%wcm%cursor_motion_rows = 24
        selected_frame%terminal%wcm%cursor_motion_columns = 80
        call delete_other_windows(selected_frame%selected_window, st)
        call resize_window_tree(selected_frame%root_window, 0, 0, 23, 80, 80)
        call resize_window_tree(selected_frame%minibuffer_window, 23, 0, 1, 80, 80)
        call get_or_make_buffer("tab-a", a, st)
        call get_or_make_buffer("tab-b", b, st)
        call get_or_make_buffer("tab-c", c, st)
        call set_buffer(a, st)
        call set_window_buffer(selected_frame%selected_window, a, st)
        call forget_window_tabs()
    end subroutine enter

    !> Forget the tab line parameters and buffer lists of the selected
    !> window.
    subroutine forget_window_tabs()
        integer :: k

        associate (w => windows(selected_frame%selected_window%index))
            if (allocated(w%tab_line%buffers)) deallocate (w%tab_line%buffers)
            w%tab_line%hscroll%tab = -1
            w%tab_line%hscroll%automatic_tab = -1
            w%tab_line%groups = .false.
            if (allocated(w%tab_line%group)) deallocate (w%tab_line%group)
            if (allocated(w%next_buffers)) w%next_buffers = w%next_buffers(1:0)

            if (allocated(w%previous_buffers)) then
                do k = 1, size(w%previous_buffers)
                    call destroy_previous_buffer(w%previous_buffers(k))
                end do

                w%previous_buffers = w%previous_buffers(1:0)
            end if
        end associate

        call force_tab_line_update(.true.)
    end subroutine forget_window_tabs

    subroutine leave()
        type(status_type) :: st

        this_command => null()
        if (minor_modes%global_tab_line_mode%enabled()) call minor_modes%global_tab_line_mode%disable(st)
        tab_line_tabs_function => get_tab_line_tabs_fixed_window_buffers
        tab_line_switch_cycling = .true.
        tab_line_close_tab_function = TAB_LINE_CLOSE_TAB_FUNCTION_BURY_BUFFER
        call forget_window_tabs()
        call delete_other_windows(selected_frame%selected_window, st)
        if (is_buffer_live(a)) call close_buffer(a, st)
        if (is_buffer_live(b)) call close_buffer(b, st)
        if (is_buffer_live(c)) call close_buffer(c, st)
    end subroutine leave

    !> The text of the selected window's tab line.
    function line_text() result(text)
        character(len=:), allocatable :: text
        type(header_line_segment_type), allocatable :: segments(:)
        integer :: k

        call format_tab_line(selected_frame%selected_window, segments)
        text = ""
        if (.not. allocated(segments)) return

        do k = 1, size(segments)
            text = text//segments(k)%text
        end do
    end function line_text

    !> The window shows "tab-b" and then "tab-c" after "tab-a".
    subroutine show_three()
        type(status_type) :: st

        call switch_to_buffer("tab-b", st)
        call switch_to_buffer("tab-c", st)
    end subroutine show_three

    !> Tab Line mode sets the buffer's tab line to `format_tab_line' and
    !> clears it; a tab line set outside of the mode stays, with a
    !> message.
    subroutine test_mode(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st

        call enter()

        checks: block
            call minor_modes%tab_line_mode%enable(st)
            call check(error, associated(buffers(current)%tab_line_format%procedure, format_tab_line), "format set")
            if (allocated(error)) exit checks
            call check(error, window_wants_tab_line(windows(selected_frame%selected_window%index)), "wants a tab line")
            if (allocated(error)) exit checks
            call minor_modes%tab_line_mode%disable(st)
            call check(error,.not. buffers(current)%tab_line_format%is_set(), "format cleared")
            if (allocated(error)) exit checks
            call buffers(current)%tab_line_format%append("mine")
            call minor_modes%tab_line_mode%enable(st)
            call check(error, last_message, "tab_line_format set outside of tab_line_mode, currently `[""mine""]'")
            if (allocated(error)) exit checks
            call check(error,.not. associated(buffers(current)%tab_line_format%procedure), "kept")
            call buffers(current)%tab_line_format%clear()
            call minor_modes%tab_line_mode%disable(st)
        end block checks

        call leave()
    end subroutine test_mode

    !> The tabs are the window's buffers in the order they were first
    !> shown, which switching back keeps.
    subroutine test_fixed_window_buffers(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st

        call enter()

        checks: block
            call show_three()
            call check(error, line_text(), "|tab-a x|tab-b x|tab-c x| + ")
            if (allocated(error)) exit checks
            call switch_to_buffer("tab-a", st)
            call check(error, line_text(), "|tab-a x|tab-b x|tab-c x| + ")
            if (allocated(error)) exit checks
            call check(error, size(windows(selected_frame%selected_window%index)%tab_line%buffers) == 3, "kept order")
            if (allocated(error)) exit checks
            ! The window buffers: those shown before, then this one.
            tab_line_tabs_function => get_tab_line_tabs_window_buffers
            call check(error, line_text(), "|tab-b x|tab-c x|tab-a x| + ")
        end block checks

        call leave()
    end subroutine test_fixed_window_buffers

    !> Switching cycles through the tabs in the order the tab line first
    !> showed them (or stops at the ends), moving a tab changes the order.
    subroutine test_switch_and_move(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st

        call enter()

        checks: block
            call show_three()
            ! The tab line shown fixes the order.
            call check(error, line_text(), "|tab-a x|tab-b x|tab-c x| + ")
            if (allocated(error)) exit checks
            call switch_to_buffer("tab-a", st)
            call switch_to_tab_line_next_tab(st)
            call check(error, same_handle(current_buffer(), b), "next is tab-b")
            if (allocated(error)) exit checks
            call switch_to_tab_line_previous_tab(st, 2_int64)
            call check(error, same_handle(current_buffer(), c), "two back cycles to tab-c")
            if (allocated(error)) exit checks
            tab_line_switch_cycling = .false.
            call switch_to_tab_line_next_tab(st)
            call check(error, same_handle(current_buffer(), c), "stops at tab-c")
            if (allocated(error)) exit checks
            call move_tab_line_tab_forward(st, -2_int64)
            call check(error, line_text(), "|tab-c x|tab-a x|tab-b x| + ")
        end block checks

        call leave()
    end subroutine test_switch_and_move

    !> Closing the other tabs buries their buffers out of the tab line;
    !> closing a tab can close its buffer.
    subroutine test_close(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st

        call enter()

        checks: block
            call show_three()
            call close_tab_line_other_tabs(st)
            call check(error, st%ok() .and. is_buffer_live(a) .and. is_buffer_live(b), "buried")
            if (allocated(error)) exit checks
            call force_tab_line_update(.true.)

            if (allocated(windows(selected_frame%selected_window%index)%tab_line%buffers)) then
                deallocate (windows(selected_frame%selected_window%index)%tab_line%buffers)
            end if

            call check(error, line_text(), "|tab-c x| + ")
            if (allocated(error)) exit checks
            tab_line_close_tab_function = TAB_LINE_CLOSE_TAB_FUNCTION_CLOSE_BUFFER
            call close_tab_line_tab(st)
            call check(error, st%ok() .and. .not. is_buffer_live(c), "closed")
        end block checks

        call leave()
    end subroutine test_close

    !> Tabs that do not fit scroll to show the selected one, with arrows;
    !> the user's scrolling stays.
    subroutine test_hscroll(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: long(5)
        type(status_type) :: st
        character(len=20) :: name
        integer :: k

        call enter()

        do k = 1, 5
            write (name, "(a,i2.2)") "tab-buffer-number-", k
            call get_or_make_buffer(name, long(k), st)
            call switch_to_buffer(name, st)
        end do

        checks: block
            call check(error, line_text(), "| <> |tab-buffer-number-03 x|tab-buffer-number-04 x|tab-buffer-number-05 x| + ")
            if (allocated(error)) exit checks
            call check(error, windows(selected_frame%selected_window%index)%tab_line%hscroll%automatic_tab == 3, &
                       "scrolled to the fourth tab")
            if (allocated(error)) exit checks
            call scroll_tab_line(-1)
            call check(error, line_text(), &
                       "| <> |tab-buffer-number-02 x|tab-buffer-number-03 x|tab-buffer-number-04 x|tab-buffer-number-05 x| + ")
        end block checks

        do k = 1, 5
            call close_buffer(long(k), st)
        end do

        call leave()
    end subroutine test_hscroll

    !> Grouped tabs: the group tab, then the group's buffers by name; the
    !> groups when the window shows them.
    subroutine test_groups(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: text

        call enter()

        checks: block
            call show_three()
            tab_line_tabs_function => get_tab_line_tabs_buffer_groups
            text = line_text()
            call check(error, index(text, "|Fundamental|") == 1, "the group tab first: "//text)
            if (allocated(error)) exit checks
            call check(error, index(text, "|tab-a x|tab-b x|tab-c x|") > 0, "the buffers by name: "//text)
            if (allocated(error)) exit checks
            windows(selected_frame%selected_window%index)%tab_line%groups = .true.
            text = line_text()
            call check(error, index(text, "|Fundamental") > 0 .and. index(text, "tab-a") == 0, "the groups: "//text)
        end block checks

        call leave()
    end subroutine test_groups

    !> The binding of KEY in the C-x map, by name.
    function control_x_binding_name(key) result(name)
        character(len=*), intent(in) :: key
        character(len=:), allocatable :: name
        type(value_type), allocatable :: events(:)
        type(value_type) :: binding
        type(status_type) :: st

        name = ""
        call key_parse(key, events, st)
        call lookup_key(make_keymap_reference(control_x_map), events, binding, st)
        if (vtag(binding) == V_NAME) name = trim(symbols(xsymbol(binding))%name)
    end function control_x_binding_name

    !> Run KEYS through the command loop.
    subroutine execute_keys(keys)
        character(len=*), intent(in) :: keys
        type(value_type), allocatable :: events(:)
        type(status_type) :: st

        call key_parse(keys, events, st)
        call unread_command_events(events)
        call command_loop(st)
    end subroutine execute_keys

    !> Global Tab-Line mode turns Tab Line mode on in the pertinent buffers
    !> and binds C-x <left> and C-x <right> to switch tabs, and back.
    subroutine test_global_mode_keys(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: hidden
        type(status_type) :: st

        call enter()
        call get_or_make_buffer(" hidden", hidden, st)
        call set_buffer(b, st)
        call tab_line_exclude%set_local(.true.)
        call set_buffer(a, st)

        checks: block
            call show_three()
            call minor_modes%global_tab_line_mode%enable(st)
            call check(error, minor_modes%tab_line_mode%enabled(), "on in tab-c")
            if (allocated(error)) exit checks
            call set_buffer(hidden, st)
            call check(error,.not. minor_modes%tab_line_mode%enabled(), "off in a hidden buffer")
            if (allocated(error)) exit checks
            call set_buffer(b, st)
            call check(error,.not. minor_modes%tab_line_mode%enabled(), "off where excluded")
            if (allocated(error)) exit checks
            call set_buffer(c, st)
            call check(error, control_x_binding_name("<left>"), "switch_to_tab_line_previous_tab")
            if (allocated(error)) exit checks
            call check(error, control_x_binding_name("M-<right>"), "move_tab_line_tab_forward")
            if (allocated(error)) exit checks
            call execute_keys("C-x <left>")
            call check(error, associated(last_command, commands%switch_to_tab_line_previous_tab) .and. &
                       same_handle(current_buffer(), b), "C-x <left> switches to tab-b")
            if (allocated(error)) exit checks
            call minor_modes%global_tab_line_mode%disable(st)
            call check(error, control_x_binding_name("<left>"), "previous_buffer")
            if (allocated(error)) exit checks
            call check(error, control_x_binding_name("M-<right>"), "")
            if (allocated(error)) exit checks
            call set_buffer(c, st)
            call check(error,.not. minor_modes%tab_line_mode%enabled(), "off again")
        end block checks

        call close_buffer(hidden, st)
        call leave()
    end subroutine test_global_mode_keys
end module test_tab_line_mod
