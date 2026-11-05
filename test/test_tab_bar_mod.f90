! test_tab_bar_mod.f90 -- tests for tab_bar_mod.
! Ports GNU Emacs 31.1 test/lisp/tab-bar-tests.el.
!
! Copyright (C) 2021-2026 Free Software Foundation, Inc.
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

!> Ports tab-bar-tests-close-other-tabs-default and
!> tab-bar-tests-close-other-tabs-with-arg; own tests of making,
!> selecting, renaming and closing tabs, the tab bar line and its items,
!> duplicated tabs and the tab history.  Each test starts and ends with
!> the initial frame alone, 24 lines by 80 columns, without tabs.
module test_tab_bar_mod
    use, intrinsic :: iso_fortran_env, only: int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_mod, only: status_type
    use handle_mod, only: handle_type
    use modes_mod, only: minor_modes
    use bindings_mod, only: init_bindings
    use keyboard_mod, only: last_message, pre_command_hook, unread_command_events, command_loop, last_command
    use keymap_mod, only: key_parse
    use value_mod, only: value_type
    use callint_mod, only: this_command
    use commands_mod, only: commands
    use buffer_mod, only: get_or_make_buffer, set_buffer, close_buffer, current_buffer
    use frame_mod, only: selected_frame, ensure_frame_windows, delete_other_frames, set_tab_bar_lines, default_frame
    use window_mod, only: windows, resize_window_tree, set_window_buffer, same_handle, init_window_display, &
                          window_leaves, split_window, delete_other_windows, window_configuration_change_hook
    use xdisp_mod, only: tab_bar_run_type
    use tab_bar_mod, only: make_tab, close_tab, close_other_tabs, undo_close_tab, rename_tab, select_tab, &
                           switch_to_next_tab, switch_to_recent_tab, duplicate_tab, move_tab, update_tab_bar_tabs, &
                           make_tab_bar_items, tab_bar_item_type, tab_bar_closed_tabs, tab_bar_show, &
                           go_back_in_tab_bar_history, change_tab_group
    implicit none(type, external)
    private

    public :: collect_tab_bar_mod

contains

    !> Collect all exported unit tests.
    subroutine collect_tab_bar_mod(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("tab-bar-tests-close-other-tabs-default", test_close_other_tabs_default), &
                    new_unittest("tab-bar-tests-close-other-tabs-with-arg", test_close_other_tabs_with_arg), &
                    new_unittest("tab-bar-new-select", test_new_select), &
                    new_unittest("tab-bar-line", test_tab_bar_line), &
                    new_unittest("tab-bar-duplicate-tab", test_duplicate), &
                    new_unittest("tab-bar-move-and-group", test_move_and_group), &
                    new_unittest("tab-bar-history", test_history), &
                    new_unittest("tab-bar-alias-keys", test_alias_keys) &
                    ]
    end subroutine collect_tab_bar_mod

    !> The initial frame alone, 24 by 80, showing buffer "tab-test", with
    !> no tabs.
    subroutine enter(h)
        type(handle_type), intent(out) :: h
        type(status_type) :: st

        call init_bindings()
        call ensure_frame_windows()
        call delete_other_frames(st)
        call init_window_display()
        selected_frame%tty%wcm%cm_rows = 24
        selected_frame%tty%wcm%cm_cols = 80
        call delete_other_windows(selected_frame%selected_window, st)
        call resize_window_tree(selected_frame%root_window, 0, 0, 23, 80, 80)
        call resize_window_tree(selected_frame%minibuffer_window, 23, 0, 1, 80, 80)
        call get_or_make_buffer("tab-test", h, st)
        call set_buffer(h, st)
        call set_window_buffer(selected_frame%selected_window, h, st)
        call clear_tabs()
    end subroutine enter

    !> Forget the frame's tabs and the closed ones (tab-bar-tabs-set nil
    !> and tab-bar-closed-tabs nil).
    subroutine clear_tabs()
        integer :: k

        if (allocated(selected_frame%parameters%tabs)) then
            do k = 1, size(selected_frame%parameters%tabs)
                call selected_frame%parameters%tabs(k)%destroy()
            end do

            deallocate (selected_frame%parameters%tabs)
        end if

        if (allocated(tab_bar_closed_tabs)) then
            do k = 1, size(tab_bar_closed_tabs)
                call tab_bar_closed_tabs(k)%tab%destroy()
            end do

            deallocate (tab_bar_closed_tabs)
        end if
    end subroutine clear_tabs

    subroutine leave(h)
        type(handle_type), intent(in) :: h
        type(status_type) :: st

        this_command => null()
        if (minor_modes%tab_bar_history_mode%enabled()) call minor_modes%tab_bar_history_mode%disable(st)
        if (minor_modes%tab_bar_mode%enabled()) call minor_modes%tab_bar_mode%disable(st)
        call clear_tabs()
        call set_tab_bar_lines(selected_frame, 0)
        selected_frame%parameters%tab_bar_lines_keep_state = .false.
        default_frame%tab_bar_lines = 0
        call delete_other_windows(selected_frame%selected_window, st)
        call resize_window_tree(selected_frame%root_window, 0, 0, 23, 80, 80)
        call close_buffer(h, st)
    end subroutine leave

    !> The name of the current tab.
    function current_name() result(name)
        character(len=:), allocatable :: name
        integer :: k

        name = ""
        k = findloc(selected_frame%parameters%tabs%current, .true., dim=1)
        if (k > 0) name = selected_frame%parameters%tabs(k)%name
    end function current_name

    !> tab-bar-tests-close-other-tabs with ARG (0 for nil).
    subroutine close_other_tabs_with(arg, error)
        integer, intent(in) :: arg
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        character(len=8) :: expected

        call clear_tabs()
        call rename_tab("1", st)
        call make_tab(st)
        call rename_tab("2", st)
        call make_tab(st)
        call rename_tab("3", st)
        call check(error, size(selected_frame%parameters%tabs) == 3, "3 tabs")
        if (allocated(error)) return
        call check(error, current_name() == "3", "current 3")
        if (allocated(error)) return

        if (arg == 0) then
            call close_other_tabs(st)
            expected = "3"
        else
            call close_other_tabs(st, arg)
            write (expected, "(i0)") max(1, min(arg, 3))
        end if

        call check(error, current_name() == trim(expected), "kept tab "//trim(expected))
        if (allocated(error)) return
        call check(error, size(selected_frame%parameters%tabs) == 1, "1 tab")
        if (allocated(error)) return
        call check(error, size(tab_bar_closed_tabs) == 2, "2 closed tabs")
        if (allocated(error)) return
        call undo_close_tab(st)
        call undo_close_tab(st)
        call undo_close_tab(st)
        call check(error, last_message == "No more closed tabs to undo", "no more closed tabs")
        if (allocated(error)) return
        call check(error, size(selected_frame%parameters%tabs) == 3, "3 tabs again")
        if (allocated(error)) return
        call check(error, size(tab_bar_closed_tabs) == 0, "no closed tabs")
    end subroutine close_other_tabs_with

    subroutine test_close_other_tabs_default(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h

        call enter(h)
        call close_other_tabs_with(0, error)
        call leave(h)
    end subroutine test_close_other_tabs_default

    subroutine test_close_other_tabs_with_arg(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        integer :: i

        call enter(h)

        do i = 0, 4
            call close_other_tabs_with(i, error)
            if (allocated(error)) exit
        end do

        call leave(h)
    end subroutine test_close_other_tabs_with_arg

    !> A new tab keeps the current buffer; selecting tabs restores their
    !> windows; an automatic name follows the buffer, a given one stays.
    subroutine test_new_select(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, other, new
        type(handle_type), allocatable :: leaves(:)
        type(status_type) :: st

        call enter(h)

        checks: block
            call update_tab_bar_tabs()
            call check(error, size(selected_frame%parameters%tabs) == 1 .and. current_name() == "tab-test", "first tab")
            if (allocated(error)) exit checks
            call split_window(selected_frame%selected_window, 0, .false., .false., new, st)
            call make_tab(st)
            call check(error, st%ok() .and. size(selected_frame%parameters%tabs) == 2, "made")
            if (allocated(error)) exit checks
            ! The new tab, to the right, has one window with the buffer, and
            ! the tab bar (tab_bar_show .true. turns Tab Bar mode on).
            leaves = leaves_of()
            call check(error, size(leaves) == 1 .and. selected_frame%parameters%tabs(2)%current, "one window")
            if (allocated(error)) exit checks
            call check(error, same_handle(windows(leaves(1)%index)%buffer, h), "the buffer")
            if (allocated(error)) exit checks
            call check(error, minor_modes%tab_bar_mode%enabled() .and. selected_frame%tab_bar_lines == 1 .and. &
                       windows(selected_frame%root_window%index)%top_line == 1 .and. &
                       windows(selected_frame%root_window%index)%total_lines == 22, "tab bar line")
            if (allocated(error)) exit checks
            call get_or_make_buffer("tab-test-2", other, st)
            call set_window_buffer(selected_frame%selected_window, other, st)
            call set_buffer(other, st)
            call update_tab_bar_tabs()
            call check(error, current_name() == "tab-test-2", "name follows the buffer")
            if (allocated(error)) exit checks
            ! Back to the first tab: its two windows, below the tab bar.
            call select_tab(st, 1)
            leaves = leaves_of()
            call check(error, st%ok() .and. size(leaves) == 2 .and. selected_frame%parameters%tabs(1)%current, &
                       "first tab again")
            if (allocated(error)) exit checks
            call check(error, windows(leaves(1)%index)%top_line == 1, "below the tab bar")
            if (allocated(error)) exit checks
            call check(error, selected_frame%parameters%tabs(2)%name == "tab-test-2", "second tab's name")
            if (allocated(error)) exit checks
            call switch_to_recent_tab(st)
            call check(error, selected_frame%parameters%tabs(2)%current .and. &
                       same_handle(current_buffer(), other), "recent tab")
            if (allocated(error)) exit checks
            call rename_tab("mine", st)
            call set_window_buffer(selected_frame%selected_window, h, st)
            call update_tab_bar_tabs()
            call check(error, current_name() == "mine", "given name stays")
            if (allocated(error)) exit checks
            call rename_tab("", st)
            call check(error, current_name() == "tab-test" .and. .not. selected_frame%parameters%tabs(2)%explicit_name, &
                       "automatic name again")
            if (allocated(error)) exit checks
            ! Closing the current tab selects the most recent one.
            call close_tab(st)
            call check(error, st%ok() .and. size(selected_frame%parameters%tabs) == 1, "closed")
            if (allocated(error)) exit checks
            call check(error, selected_frame%parameters%tabs(1)%current, "the other is current")
            if (allocated(error)) exit checks
            call close_tab(st)
            call check(error,.not. st%ok(), "the sole tab")
        end block checks

        call close_buffer(other, st)
        call leave(h)
    end subroutine test_new_select

    !> The live windows of the selected frame.
    function leaves_of() result(leaves)
        type(handle_type), allocatable :: leaves(:)

        call window_leaves(selected_frame%root_window, leaves)
    end function leaves_of

    !> The text of the tab bar's items.
    function items_text() result(text)
        character(len=:), allocatable :: text
        type(tab_bar_item_type), allocatable :: items(:)
        integer :: k, j

        call make_tab_bar_items(items)
        text = ""

        do k = 1, size(items)
            do j = 1, size(items(k)%runs)
                text = text//items(k)%runs(j)%text
            end do
        end do
    end function items_text

    !> The tab bar of Emacs 31.1 on a terminal, 80 columns: each tab is
    !> padded to 19 columns (20 at most), with its close button.
    subroutine test_tab_bar_line(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        character(len=:), allocatable :: text

        call enter(h)

        checks: block
            call make_tab(st)
            text = items_text()
            call check(error, text == "|tab-test          x|tab-test          x| + ", "two tabs: '"//text//"'")
            if (allocated(error)) exit checks
            ! A long name is cut to 20 columns, the last two in `shadow'.
            call rename_tab("a-very-long-tab-name-indeed", st)
            text = items_text()
            call check(error, text == "|tab-test          x|a-very-long-tab-namx| + ", "a long name: '"//text//"'")
            if (allocated(error)) exit checks
            ! tab_bar_show%tabs = 1: no tab bar line for one tab.
            tab_bar_show%tabs = 1
            call close_tab(st)
            call check(error, selected_frame%tab_bar_lines == 0 .and. &
                       windows(selected_frame%root_window%index)%top_line == 0, "no line for one tab")
        end block checks

        tab_bar_show = .true.
        call leave(h)
    end subroutine test_tab_bar_line

    !> A duplicated tab has new windows with the same layout and buffers.
    subroutine test_duplicate(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, new
        type(handle_type), allocatable :: before(:), after(:)
        type(status_type) :: st

        call enter(h)

        checks: block
            call split_window(selected_frame%selected_window, 0, .false., .true., new, st)
            before = leaves_of()
            call duplicate_tab(st)
            after = leaves_of()
            call check(error, st%ok() .and. size(after) == 2 .and. size(selected_frame%parameters%tabs) == 2, "two windows")
            if (allocated(error)) exit checks
            call check(error,.not. any(same_handle(after(1), before)) .and. &
                       windows(after(1)%index)%total_cols == windows(before(1)%index)%total_cols .and. &
                       same_handle(windows(after(2)%index)%buffer, h), "new windows, same layout")
            if (allocated(error)) exit checks
            call select_tab(st, 1)
            after = leaves_of()
            call check(error, same_handle(after(1), before(1)) .and. same_handle(after(2), before(2)), "first tab's windows")
        end block checks

        call leave(h)
    end subroutine test_duplicate

    !> Moving tabs and changing their group.
    subroutine test_move_and_group(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call enter(h)

        checks: block
            call rename_tab("1", st)
            call make_tab(st)
            call rename_tab("2", st)
            call make_tab(st)
            call rename_tab("3", st)
            call move_tab(1, st)
            call check(error, selected_frame%parameters%tabs(1)%name == "3" .and. &
                       selected_frame%parameters%tabs(1)%current, "moved around to the first place")
            if (allocated(error)) exit checks
            call switch_to_next_tab(st)
            call check(error, current_name() == "1", "next tab")
            if (allocated(error)) exit checks
            call change_tab_group("g", st)
            call select_tab(st, 1)
            call change_tab_group("g", st)
            ! "1" went to the end (a new group), then "3" next to it.
            call check(error, selected_frame%parameters%tabs(1)%name == "2" .and. selected_frame%parameters%tabs(2)%name == "3" &
                       .and. .not. allocated(selected_frame%parameters%tabs(1)%group), "moved to its group")
            if (allocated(error)) exit checks
            call check(error, allocated(selected_frame%parameters%tabs(2)%group) .and. &
                       allocated(selected_frame%parameters%tabs(3)%group), "in the group")
        end block checks

        call leave(h)
    end subroutine test_move_and_group

    !> Tab history mode goes back to the window configuration before a
    !> change.
    subroutine test_history(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, new
        type(handle_type), allocatable :: leaves(:)
        type(status_type) :: st

        call enter(h)

        checks: block
            call minor_modes%tab_bar_history_mode%enable(st)
            call go_back_in_tab_bar_history(st)
            call check(error, last_message == "No more tab back history", "no history")
            if (allocated(error)) exit checks
            this_command => commands%split_window_below
            ! A command splits the window.
            call pre_command_hook%execute(st)
            call split_window(selected_frame%selected_window, 0, .false., .false., new, st)
            call window_configuration_change_hook%execute(st)
            leaves = leaves_of()
            call check(error, size(leaves) == 2 .and. size(selected_frame%tab_history_back) == 1, "split, recorded")
            if (allocated(error)) exit checks
            call pre_command_hook%execute(st)
            call go_back_in_tab_bar_history(st)
            leaves = leaves_of()
            call check(error, size(leaves) == 1 .and. size(selected_frame%tab_history_forward) == 1, "back")
        end block checks

        call leave(h)
    end subroutine test_history

    !> Execute KEYS (an is_key_valid string) through the command loop.
    subroutine execute_keys(keys)
        character(len=*), intent(in) :: keys
        type(value_type), allocatable :: events(:)
        type(status_type) :: st

        call key_parse(keys, events, st)
        call unread_command_events(events)
        call command_loop(st)
    end subroutine execute_keys

    !> C-x t runs the aliases, commands of their own: tab-new, tab-next,
    !> tab-close.
    subroutine test_alias_keys(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h

        call enter(h)

        checks: block
            call execute_keys("C-x t 2")
            call check(error, associated(last_command, commands%make_tab) .and. &
                       size(selected_frame%parameters%tabs) == 2, "C-x t 2 runs make_tab")
            if (allocated(error)) exit checks
            call execute_keys("C-x t o")
            call check(error, associated(last_command, commands%next_tab) .and. &
                       selected_frame%parameters%tabs(1)%current, "C-x t o runs next_tab")
            if (allocated(error)) exit checks
            call execute_keys("C-x t 0")
            call check(error, associated(last_command, commands%close_tab) .and. &
                       size(selected_frame%parameters%tabs) == 1, "C-x t 0 runs close_tab")
        end block checks

        call leave(h)
    end subroutine test_alias_keys
end module test_tab_bar_mod
