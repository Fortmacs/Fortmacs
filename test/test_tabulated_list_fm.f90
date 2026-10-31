! test_tabulated_list_fm.f90 -- tests for tabulated_list_fm.
! Ports GNU Emacs 31.1 test/lisp/emacs-lisp/tabulated-list-tests.el in
! part; the group tests wait for groups.
!
! Copyright (C) 2015-2026 Free Software Foundation, Inc.
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

!> Ports tabulated-list-print and tabulated-list-sort of
!> tabulated-list-tests.el (the group tests are deferred with groups), and
!> checks the header line it makes.
module test_tabulated_list_fm
    use, intrinsic :: iso_fortran_env, only: int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_fm, only: status_type, format_error, FM_USER_ERROR
    use handle_fm, only: handle_type
    use lisp_fm, only: value_type, xstring, vtag, V_STRING
    use buffer_fm, only: buffers, cur, current_buffer, set_buffer, kill_buffer, get_buffer_create, &
                         generate_new_buffer_name
    use editfns_fm, only: buffer_string, point, goto_char, point_min, pos_bol, pos_eol, buffer_substring
    use cmds_fm, only: forward_line
    use syntax_fm, only: skip_chars_forward
    use textprop_fm, only: get_text_property
    use bindings_fm, only: init_bindings
    use tabulated_list_fm, only: tabulated_list_mode, tabulated_list_column_type, tabulated_list_entry_type, &
                                 set_tabulated_list_format, set_tabulated_list_entries, set_tabulated_list_padding, &
                                 tabulated_list_entries, tabulated_list_init_header, tabulated_list_print, &
                                 tabulated_list_sort
    implicit none(type, external)
    private

    public :: collect_tabulated_list_fm

    character(len=*), parameter :: LF = achar(10)

contains

    !> Collect all exported unit tests.
    subroutine collect_tabulated_list_fm(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("tabulated-list-print", test_print), &
                    new_unittest("tabulated-list-sort", test_sort), &
                    new_unittest("tabulated-list-header", test_header) &
                    ]
    end subroutine collect_tabulated_list_fm

    logical function sort_car(a, b)
        type(tabulated_list_entry_type), intent(in) :: a, b

        sort_car = llt(a%id, b%id)
    end function sort_car

    !> An entry of `tabulated-list--test-entries'.
    subroutine make_entry(e, name, version, status, description)
        type(tabulated_list_entry_type), intent(out) :: e
        character(len=*), intent(in) :: name, version, status, description
        integer :: k

        e%id = name
        allocate (e%cols(5))
        e%cols(1)%text = name
        e%cols(2)%text = name
        e%cols(3)%text = version
        e%cols(4)%text = status
        e%cols(5)%text = description

        do k = 1, 5
            e%cols(k)%face = ""
        end do
    end subroutine make_entry

    subroutine test_entries(entries)
        type(tabulated_list_entry_type), allocatable, intent(out) :: entries(:)

        allocate (entries(4))
        call make_entry(entries(1), "zzzz-game", "2113", "installed", " play zzzz in Emacs")
        call make_entry(entries(2), "4clojure", "1507", "obsolete", " Open and evaluate 4clojure.com questions")
        call make_entry(entries(3), "abc-mode", "944", "available", " Major mode for editing abc music files")
        call make_entry(entries(4), "mode", "1128", "installed", " A simple mode for editing Actionscript 3 files")
    end subroutine test_entries

    subroutine test_format(format)
        type(tabulated_list_column_type), allocatable, intent(out) :: format(:)

        allocate (format(5))
        format(1)%name = "name"
        format(1)%width = 10
        format(1)%sort = .true.
        format(1)%predicate => sort_car
        format(2)%name = "name-2"
        format(2)%width = 10
        format(2)%sort = .true.
        format(3)%name = "Version"
        format(3)%width = 9
        format(4)%name = "Status"
        format(4)%width = 10
        format(5)%name = "Description"
        format(5)%width = 0
    end subroutine test_format

    !> (tabulated-list--test-with-buffer): a temporary buffer in Tabulated
    !> List mode with the test entries printed; OLD is the buffer before.
    subroutine with_buffer(old)
        type(handle_type), intent(out) :: old
        type(handle_type) :: h
        type(tabulated_list_entry_type), allocatable :: entries(:)
        type(tabulated_list_column_type), allocatable :: format(:)
        type(status_type) :: st

        call init_bindings()
        old = current_buffer()
        call get_buffer_create(generate_new_buffer_name(" *temp*"), h, st)
        call set_buffer(h, st)
        call tabulated_list_mode(st)
        call test_entries(entries)
        call set_tabulated_list_entries(entries)
        call test_format(format)
        call set_tabulated_list_format(format)
        call set_tabulated_list_padding(7)
        call tabulated_list_init_header()
        call tabulated_list_print(st)
    end subroutine with_buffer

    subroutine done(old)
        type(handle_type), intent(in) :: old
        type(status_type) :: st

        call kill_buffer(current_buffer(), st)
        call set_buffer(old, st)
    end subroutine done

    function line_at_point() result(line)
        character(len=:), allocatable :: line
        type(status_type) :: st

        call buffer_substring(pos_bol(), pos_eol() + 1, line, st)
    end function line_at_point

    !> The entries without the first.
    subroutine pop_entry()
        type(tabulated_list_entry_type), allocatable :: entries(:)

        entries = tabulated_list_entries()
        call set_tabulated_list_entries(entries(2:))
    end subroutine pop_entry

    subroutine test_print(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: old
        type(tabulated_list_entry_type), allocatable :: entries(:)
        type(status_type) :: st
        character(len=:), allocatable :: pos
        integer(int64) :: shortage

        call with_buffer(old)

        steps: block
            ! Basic printing.
            call check(error, buffer_string(), &
                       "       zzzz-game  zzzz-game  2113      installed   play zzzz in Emacs"//LF// &
                       "       4clojure   4clojure   1507      obsolete    Open and evaluate 4clojure.com questions"//LF// &
                       "       abc-mode   abc-mode   944       available   Major mode for editing abc music files"//LF// &
                       "       mode       mode       1128      installed   A simple mode for editing Actionscript 3 files"//LF)
            if (allocated(error)) exit steps
            ! Preserve position.
            shortage = forward_line(3_int64)
            pos = line_at_point()
            call pop_entry()
            call tabulated_list_print(st, remember_pos=.true.)
            call check(error, line_at_point(), pos)
            if (allocated(error)) exit steps
            call check(error, buffer_string(), &
                       "       4clojure   4clojure   1507      obsolete    Open and evaluate 4clojure.com questions"//LF// &
                       "       abc-mode   abc-mode   944       available   Major mode for editing abc music files"//LF// &
                       "       mode       mode       1128      installed   A simple mode for editing Actionscript 3 files"//LF)
            if (allocated(error)) exit steps
            ! Check the UPDATE argument
            call pop_entry()
            entries = tabulated_list_entries()
            entries(1)%cols(1)%text = "x"
            entries(1)%cols(2)%text = "x"
            entries(1)%cols(3)%text = "944"
            entries(1)%cols(4)%text = "available"
            entries(1)%cols(5)%text = " XX"
            call set_tabulated_list_entries(entries)
            call tabulated_list_print(st, remember_pos=.true., update=.true.)
            call check(error, buffer_string(), &
                       "       x          x          944       available   XX"//LF// &
                       "       mode       mode       1128      installed   A simple mode for editing Actionscript 3 files"//LF)
            if (allocated(error)) exit steps
            call check(error, line_at_point(), pos)
        end block steps

        call done(old)
    end subroutine test_print

    subroutine test_sort(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: old
        type(value_type) :: v
        type(status_type) :: st
        character(len=:), allocatable :: text
        integer(int64) :: moved

        call with_buffer(old)

        steps: block
            ! Basic sorting
            call goto_char(point_min())
            moved = skip_chars_forward("[:blank:]", st)
            call tabulated_list_sort(st)
            text = buffer_string()
            call check(error, text, &
                       "       4clojure   4clojure   1507      obsolete    Open and evaluate 4clojure.com questions"//LF// &
                       "       abc-mode   abc-mode   944       available   Major mode for editing abc music files"//LF// &
                       "       mode       mode       1128      installed   A simple mode for editing Actionscript 3 files"//LF// &
                       "       zzzz-game  zzzz-game  2113      installed   play zzzz in Emacs"//LF)
            if (allocated(error)) exit steps
            moved = skip_chars_forward("^[:blank:]", st)
            moved = skip_chars_forward("[:blank:]", st)
            call get_text_property(point(), value_type(name="tabulated_list_column_name"), v, st)
            call check(error, vtag(v) == V_STRING, "a column name")
            if (allocated(error)) exit steps
            call check(error, xstring(v), "name-2")
            if (allocated(error)) exit steps
            call tabulated_list_sort(st)
            ! Check a t as the sorting predicate.
            call check(error, buffer_string(), text)
            if (allocated(error)) exit steps
            ! Invert.
            call tabulated_list_sort(st, 1)
            call check(error, buffer_string(), &
                       "       zzzz-game  zzzz-game  2113      installed   play zzzz in Emacs"//LF// &
                       "       mode       mode       1128      installed   A simple mode for editing Actionscript 3 files"//LF// &
                       "       abc-mode   abc-mode   944       available   Major mode for editing abc music files"//LF// &
                       "       4clojure   4clojure   1507      obsolete    Open and evaluate 4clojure.com questions"//LF)
            if (allocated(error)) exit steps
            ! Again
            call tabulated_list_sort(st, 1)
            call check(error, buffer_string(), text)
            if (allocated(error)) exit steps
            ! Check that you can't sort some cols.
            moved = skip_chars_forward("^[:blank:]", st)
            moved = skip_chars_forward("[:blank:]", st)
            call tabulated_list_sort(st)
            call check(error, st%code == FM_USER_ERROR, "user-error")
            if (allocated(error)) exit steps
            call tabulated_list_sort(st, 4)
            call check(error, st%code == FM_USER_ERROR, "user-error for 4")
        end block steps

        call done(old)
    end subroutine test_sort

    !> The header line of the test buffer: the padding, then the labels at
    !> their columns, "name" in bold with the sort indicator once sorted.
    subroutine test_header(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: old
        type(status_type) :: st
        integer(int64) :: moved

        call with_buffer(old)

        steps: block
            associate (segs => buffers(cur)%header_line_format%segments)
                call check(error, size(segs) > 0, "a header line")
                if (allocated(error)) exit steps
                call check(error, segs(1)%align_to == 7, "the padding")
                if (allocated(error)) exit steps
                call check(error, segs(2)%text, "name")
                if (allocated(error)) exit steps
                call check(error, segs(3)%align_to == 7 + 1 + 10, "the next column")
                if (allocated(error)) exit steps
            end associate

            call goto_char(point_min())
            moved = skip_chars_forward("[:blank:]", st)
            call tabulated_list_sort(st)

            associate (segs => buffers(cur)%header_line_format%segments)
                call check(error, segs(2)%text, "name v")
                if (allocated(error)) exit steps
                call check(error, segs(2)%face, "bold")
            end associate
        end block steps

        call done(old)
    end subroutine test_header
end module test_tabulated_list_fm
