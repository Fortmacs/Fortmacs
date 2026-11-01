! test_buff_menu_mod.f90 -- tests for buff_menu_mod.
! Ports GNU Emacs 31.1 test/lisp/buff-menu-tests.el.
!
! Copyright (C) 2016-2026 Free Software Foundation, Inc.
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

!> Ports buff-menu-24962 of buff-menu-tests.el; own tests of the listing,
!> the marks and `buffer_menu_execute'.
module test_buff_menu_mod
    use, intrinsic :: iso_fortran_env, only: int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_mod, only: status_type, format_error
    use handle_mod, only: handle_type
    use buffer_mod, only: buffers, current_buffer, set_buffer, close_buffer, get_or_make_buffer, get_buffer, &
                          is_buffer_live, rename_buffer, restore_buffer_modified
    use insdel_mod, only: insert
    use editfns_mod, only: goto_char, point_min, pos_bol, pos_eol, buffer_substring
    use cmds_mod, only: beginning_of_line
    use search_mod, only: search_forward
    use files_mod, only: create_temp_file, delete_file, find_file_noselect
    use window_mod, only: switch_to_buffer
    use bindings_mod, only: init_bindings
    use tabulated_list_mod, only: tabulated_list_entry_type
    use buff_menu_mod, only: list_buffers, list_buffers_noselect, buffer_menu_buffer, buffer_menu_delete, &
                             buffer_menu_execute, buffer_menu_save, buffer_menu_mark, buffer_menu_unmark_all, &
                             buffer_menu_not_modified, buffer_menu_toggle_internal, buffer_menu_marked_buffers, &
                             tabulated_list_entry_size_greater
    implicit none(type, external)
    private

    public :: collect_buff_menu_mod

contains

    !> Collect all exported unit tests.
    subroutine collect_buff_menu_mod(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("buff-menu-24962", test_24962), &
                    new_unittest("buff-menu-listing", test_listing), &
                    new_unittest("buff-menu-marks", test_marks), &
                    new_unittest("buff-menu-execute", test_execute), &
                    new_unittest("buff-menu-toggle-internal", test_toggle_internal), &
                    new_unittest("buff-menu-size-predicate", test_size_test) &
                    ]
    end subroutine collect_buff_menu_mod

    ! A new buffer NAME holding TEXT.
    function make_buffer(name, text) result(h)
        character(len=*), intent(in) :: name, text
        type(handle_type) :: h, old
        type(status_type) :: st

        old = current_buffer()
        call get_or_make_buffer(name, h, st)
        call set_buffer(h, st)
        call insert(text, st)
        call set_buffer(old, st)
    end function make_buffer

    ! In the current buffer (the Buffer Menu), move to the line listing
    ! NAME; FOUND false if there is none.
    subroutine goto_entry(name, found)
        character(len=*), intent(in) :: name
        logical, intent(out) :: found
        integer(int64) :: pos
        type(status_type) :: st

        call goto_char(point_min())
        call search_forward(" "//name//" ", pos, st, noerror=1)
        found = st%ok() .and. pos /= 0
        if (found) call beginning_of_line()
    end subroutine goto_entry

    function current_line() result(line)
        character(len=:), allocatable :: line
        type(status_type) :: st

        call buffer_substring(pos_bol(), pos_eol(), line, st)
    end function current_line

    !> Test for https://debbugs.gnu.org/24962 .
    subroutine test_24962(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: file, actual
        type(handle_type) :: buffer, old, list, b
        type(status_type) :: st

        call init_bindings()
        old = current_buffer()
        call create_temp_file("foo", file, st)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        call find_file_noselect(file, buffer, st)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        call switch_to_buffer(buffers(buffer%index)%name, st)
        call set_buffer(buffer, st)
        call rename_buffer(" foo", .false., actual, st)
        call list_buffers(st)
        call check(error, st%ok(), format_error(st))

        if (.not. allocated(error)) then
            list = get_buffer("*Buffer List*")
            call set_buffer(list, st)
            call goto_char(point_min())
            b = buffer_menu_buffer(st)
            call check(error, b%index /= 0, "no buffer on the first line")
            if (.not. allocated(error)) call check(error, buffers(b%index)%name == " foo", buffers(b%index)%name)
        end if

        if (is_buffer_live(buffer)) call close_buffer(buffer, st)
        call delete_file(file, st)
        call set_buffer(old, st)
    end subroutine test_24962

    !> The columns of the listing, the "." of the buffer it was called
    !> from, and internal buffers left out.
    subroutine test_listing(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: a, internal, old, list
        type(status_type) :: st
        character(len=:), allocatable :: line
        logical :: found

        call init_bindings()
        old = current_buffer()
        a = make_buffer("bm-alpha", "12345")
        internal = make_buffer(" bm-internal", "")
        call set_buffer(a, st)
        buffers(a%index)%read_only = .true.
        call list_buffers_noselect(list, st)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        call set_buffer(list, st)
        call goto_entry("bm-alpha", found)
        call check(error, found, "bm-alpha not listed")

        if (.not. allocated(error)) then
            line = current_line()
            ! C (the buffer it was called from), R, M, name, size, mode.
            call check(error, line(1:3) == ".%*", line)
            if (.not. allocated(error)) call check(error, index(line, " 5 ") > 0, line)
            if (.not. allocated(error)) call check(error, index(line, "Fundamental") > 0, line)
        end if

        if (.not. allocated(error)) then
            call goto_entry("bm-internal", found)
            call check(error,.not. found, "internal buffer listed")
        end if

        if (.not. allocated(error)) then
            call goto_entry("*Buffer List*", found)
            call check(error,.not. found, "the Buffer Menu lists itself")
        end if

        call set_buffer(a, st)
        call restore_buffer_modified(.false.)
        call close_buffer(a, st)
        call close_buffer(internal, st)
        call close_buffer(list, st)
        call set_buffer(old, st)
    end subroutine test_listing

    !> m, s, ~ and U.
    subroutine test_marks(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: line
        type(handle_type) :: a, old, list
        type(handle_type), allocatable :: marked(:)
        type(status_type) :: st
        logical :: found

        call init_bindings()
        old = current_buffer()
        a = make_buffer("bm-beta", "text")
        call list_buffers_noselect(list, st)
        call set_buffer(list, st)
        call goto_entry("bm-beta", found)
        call check(error, found, "bm-beta not listed")
        if (allocated(error)) return
        call buffer_menu_mark(st)
        call goto_entry("bm-beta", found)
        line = current_line()
        call check(error, line(1:1) == ">", line)

        if (.not. allocated(error)) then
            call buffer_menu_marked_buffers(marked, st)
            call check(error, size(marked) == 1, "marked buffers")
            if (.not. allocated(error)) call check(error, marked(1)%index == a%index, "marked buffer")
        end if

        if (.not. allocated(error)) then
            call buffer_menu_save(st)
            call goto_entry("bm-beta", found)
            line = current_line()
            call check(error, line(1:3) == "> S", line)
        end if

        if (.not. allocated(error)) then
            call buffer_menu_not_modified(st)
            line = current_line()
            call check(error, line(1:3) == ">  ", line)
        end if

        if (.not. allocated(error)) then
            call buffer_menu_unmark_all(st)
            call goto_entry("bm-beta", found)
            line = current_line()
            call check(error, line(1:3) == "   ", line)
        end if

        call close_buffer(a, st)
        call close_buffer(list, st)
        call set_buffer(old, st)
    end subroutine test_marks

    !> d then x closes the buffer and removes its line.
    subroutine test_execute(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: line
        type(handle_type) :: a, b, old, list
        type(status_type) :: st
        logical :: found

        call init_bindings()
        old = current_buffer()
        a = make_buffer("bm-gamma", "")
        b = make_buffer("bm-delta", "")
        call list_buffers_noselect(list, st)
        call set_buffer(list, st)
        call goto_entry("bm-gamma", found)
        call buffer_menu_delete(st)
        call goto_entry("bm-gamma", found)
        line = current_line()
        call check(error, line(1:1) == "D", line)

        if (.not. allocated(error)) then
            call buffer_menu_execute(st)
            call check(error, st%ok(), format_error(st))
        end if

        if (.not. allocated(error)) call check(error,.not. is_buffer_live(a), "bm-gamma not killed")
        if (.not. allocated(error)) call check(error, is_buffer_live(b), "bm-delta killed")

        if (.not. allocated(error)) then
            call goto_entry("bm-gamma", found)
            call check(error,.not. found, "bm-gamma still listed")
        end if

        if (is_buffer_live(a)) call close_buffer(a, st)
        call close_buffer(b, st)
        call close_buffer(list, st)
        call set_buffer(old, st)
    end subroutine test_execute

    !> I lists internal buffers.
    subroutine test_toggle_internal(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: internal, old, list
        type(status_type) :: st
        logical :: found

        call init_bindings()
        old = current_buffer()
        internal = make_buffer(" bm-hidden", "")
        call list_buffers_noselect(list, st)
        call set_buffer(list, st)
        call buffer_menu_toggle_internal(st)
        call check(error, st%ok(), format_error(st))

        if (.not. allocated(error)) then
            call goto_entry("bm-hidden", found)
            call check(error, found, "internal buffer not listed")
        end if

        call close_buffer(internal, st)
        call close_buffer(list, st)
        call set_buffer(old, st)
    end subroutine test_toggle_internal

    !> The Size column sorts by number, human-readable sizes too.
    subroutine test_size_test(error)
        type(error_type), allocatable, intent(out) :: error
        type(tabulated_list_entry_type) :: a, b
        integer :: k

        allocate (a%cols(7), b%cols(7))

        do k = 1, 7
            a%cols(k)%text = ""
            a%cols(k)%face = ""
            b%cols(k)%text = ""
            b%cols(k)%face = ""
        end do

        a%cols(5)%text = "120"
        b%cols(5)%text = "9"
        call check(error, tabulated_list_entry_size_greater(a, b), "120 > 9")
        if (allocated(error)) return
        call check(error,.not. tabulated_list_entry_size_greater(b, a), "9 > 120")
        if (allocated(error)) return
        a%cols(5)%text = "1.5k"
        b%cols(5)%text = "900"
        call check(error,.not. tabulated_list_entry_size_greater(a, b), "1.5k > 900 (string-to-number)")
    end subroutine test_size_test
end module test_buff_menu_mod
