! test_fill_fm.f90 -- tests for filling (fill_fm) and auto-fill.
!
! Copyright (C) 2017-2026 Free Software Foundation, Inc.
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

!> Ports test/lisp/textmodes/fill-tests.el: fill-test-no-fill-polish-nobreak-p,
!> fill-test-breakable-paragraph, fill-test-end-period, and the erts
!> files of fill-test-fill-paragraph, -fill-region-as-paragraph-default,
!> -fill-region-as-paragraph and -fill-region (copied to
!> test/fill-resources) through a small reader of the erts format whose
!> Code forms are recognized, not evaluated.  Not ported: the
!> semantic-linefeed variants, comment filling (fill-test-haskell,
!> fill-paragraph-handle-comment) and the expected-failure test.
module test_fill_fm
    use, intrinsic :: iso_fortran_env, only: int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_fm, only: status_type
    use handle_fm, only: handle_type
    use lisp_fm, only: nil
    use buffer_fm, only: get_buffer_create, set_buffer, kill_buffer, erase_buffer, fill_column, auto_fill_function
    use insdel_fm, only: insert
    use editfns_fm, only: point, point_min, point_max, goto_char, buffer_string, delete_region, pos_bol, pos_eol
    use cmds_fm, only: forward_line, beginning_of_line, self_insert_command
    use search_fm, only: search_forward
    use simple_fm, only: fill_prefix, deactivate_mark, syms_of_simple
    use modes_fm, only: minor_modes
    use paragraphs_fm, only: sentence_end_double_space
    use bindings_fm, only: init_bindings
    use fill_fm, only: fill_paragraph, fill_region, fill_region_as_paragraph, fill_separate_heterogeneous_words_with_space, &
                       fill_nobreak_predicate, fill_polish_nobreak_p, set_fill_prefix, fill_context_prefix
    implicit none(type, external)
    private

    public :: collect_fill_fm

    character(len=*), parameter :: LF = achar(10)

contains

    subroutine collect_fill_fm(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("fill-test-no-fill-polish-nobreak-p", test_polish), &
                    new_unittest("fill-test-breakable-paragraph", test_breakable), &
                    new_unittest("fill-test-end-period", test_end_period), &
                    new_unittest("fill-test-fill-region-as-paragraph-default", test_erts_default), &
                    new_unittest("fill-test-fill-region-as-paragraph", test_erts_frap), &
                    new_unittest("fill-test-fill-region", test_erts_region), &
                    new_unittest("fill-test-fill-paragraph", test_erts_paragraph), &
                    new_unittest("fill-justification-and-prefix", test_justify), &
                    new_unittest("fill-adaptive-prefix-length", test_prefix_length) &
                    ]
    end subroutine collect_fill_fm

    !> A fresh buffer with the default variables (as a temporary buffer
    !> has its buffer-local values).
    subroutine temp_buffer(text, h)
        character(len=*), intent(in) :: text
        type(handle_type), intent(out) :: h
        type(status_type) :: st

        call init_bindings()
        call get_buffer_create(" *fill-test*", h, st)
        call set_buffer(h, st)
        call erase_buffer(st)
        call deactivate_mark(.true.)
        call insert(text, st)
        fill_column = 70
        sentence_end_double_space = .true.
        fill_separate_heterogeneous_words_with_space = .false.
        if (allocated(fill_prefix%value)) deallocate (fill_prefix%value)
        if (allocated(fill_nobreak_predicate)) deallocate (fill_nobreak_predicate)
        auto_fill_function%value => null()
    end subroutine temp_buffer

    subroutine leave(h)
        type(handle_type), intent(in) :: h
        type(status_type) :: st

        fill_column = 70
        sentence_end_double_space = .true.
        fill_separate_heterogeneous_words_with_space = .false.
        if (allocated(fill_prefix%value)) deallocate (fill_prefix%value)
        if (allocated(fill_nobreak_predicate)) deallocate (fill_nobreak_predicate)
        auto_fill_function%value => null()
        call kill_buffer(h, st)
    end subroutine leave

    subroutine test_polish(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call temp_buffer("Abc d efg (h ijk).", h)
        fill_column = 8
        call fill_paragraph(st)
        call check(error, buffer_string(), "Abc d"//LF//"efg (h"//LF//"ijk).")
        call leave(h)
        if (allocated(error)) return
        call temp_buffer("Abc d efg (h ijk).", h)
        fill_column = 8
        allocate (fill_nobreak_predicate(1))
        fill_nobreak_predicate(1)%fn => fill_polish_nobreak_p
        call fill_paragraph(st)
        call check(error, buffer_string(), "Abc"//LF//"d efg"//LF//"(h ijk).")
        call leave(h)
    end subroutine test_polish

    subroutine test_breakable(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        integer(int64) :: pos, beg, end

        call temp_buffer("aaa =   baaaaaaaa aaaaaaaaaa aaaaaaaaaa"//LF, h)
        call goto_char(point_min())
        call search_forward("b", pos, st)
        pos = point()
        beg = pos_bol()
        end = pos_eol()
        fill_prefix = repeat(" ", int(pos - beg))
        ! `fill_column' is too small to accommodate the current line
        fill_column = end - beg - 10
        call fill_region_as_paragraph(beg, end, st, squeeze_after=pos)
        call check(error, buffer_string(), "aaa =   baaaaaaaa aaaaaaaaaa"//LF//"         aaaaaaaaaa"//LF)
        call leave(h)
    end subroutine test_breakable

    subroutine test_end_period(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call syms_of_simple()
        call temp_buffer("", h)
        call minor_modes%auto_fill_mode%enable(st)
        call insert("Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eius.", st)
        call self_insert_command(1_int64, 32_int64, st)
        call check(error, buffer_string(), "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eius. ")
        call leave(h)
        if (allocated(error)) return
        call temp_buffer("", h)
        call minor_modes%auto_fill_mode%enable(st)
        call insert("Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eius.Foo", st)
        call goto_char(point() - 3)
        call self_insert_command(1_int64, 32_int64, st)
        call check(error, buffer_string(), "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do"//LF// &
                   "eius. Foo")
        call leave(h)
    end subroutine test_end_period

    !> Fortmacs: right, center and full justification; set-fill-prefix
    !> and the fill prefix.
    subroutine test_justify(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call temp_buffer("aaa bbb ccc ddd eee fff", h)
        fill_column = 10
        call fill_paragraph(st, 3)
        call check(error, buffer_string(), "aaa    bbb"//LF//"ccc    ddd"//LF//"eee fff")
        call leave(h)
        if (allocated(error)) return
        call temp_buffer("aaa bbb ccc", h)
        fill_column = 11
        call fill_paragraph(st, 2)
        call check(error, buffer_string(), "aaa bbb ccc")
        call leave(h)
        if (allocated(error)) return
        call temp_buffer("aaa bbb", h)
        fill_column = 15
        call fill_paragraph(st, 2)
        ! (indent-tabs-mode is t)
        call check(error, buffer_string(), achar(9)//"aaa bbb")
        call leave(h)
        if (allocated(error)) return
        call temp_buffer("aaa bbb", h)
        fill_column = 15
        call fill_paragraph(st, 4)
        call check(error, buffer_string(), "    aaa bbb")
        call leave(h)
        if (allocated(error)) return
        ! A fill prefix set from the line is reinserted on each line.
        call temp_buffer(";; one two three four five six", h)
        call goto_char(4_int64)
        call set_fill_prefix(.false.)
        call check(error, fill_prefix%value, ";; ")
        if (allocated(error)) return
        fill_column = 14
        call fill_paragraph(st)
        call check(error, buffer_string(), ";; one two"//LF//";; three four"//LF//";; five six")
        call leave(h)
        if (allocated(error)) return
        ! Adaptive fill: the prefix comes from the first two lines.
        call temp_buffer("  > one two three"//LF//"  > four five six", h)
        fill_column = 15
        call fill_paragraph(st)
        call check(error, buffer_string(), "  > one two"//LF//"  > three four"//LF//"  > five six")
        call leave(h)
    end subroutine test_justify

    ! ------------------------------------------------------------------
    ! erts files
    ! ------------------------------------------------------------------

    subroutine test_erts_default(error)
        type(error_type), allocatable, intent(out) :: error

        call run_erts_file(error, "test/fill-resources/fill-region-as-paragraph-default.erts")
    end subroutine test_erts_default

    subroutine test_erts_frap(error)
        type(error_type), allocatable, intent(out) :: error

        call run_erts_file(error, "test/fill-resources/fill-region-as-paragraph.erts")
    end subroutine test_erts_frap

    subroutine test_erts_region(error)
        type(error_type), allocatable, intent(out) :: error

        call run_erts_file(error, "test/fill-resources/fill-region.erts")
    end subroutine test_erts_region

    subroutine test_erts_paragraph(error)
        type(error_type), allocatable, intent(out) :: error

        call run_erts_file(error, "test/fill-resources/fill-paragraph.erts")
    end subroutine test_erts_paragraph

    !> Run the tests of an erts file (ert-test-erts-file): each has a
    !> Name, inherits the last Code and Point-Char, and has the text
    !> before and after between =-= lines.
    subroutine run_erts_file(error, file)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), intent(in) :: file
        character(len=:), allocatable :: data, line, code, name, before, after, point_char
        integer :: u, ios, pos, state, nl, ran
        integer(int64) :: fsize

        open (newunit=u, file=file, access="stream", form="unformatted", status="old", action="read", iostat=ios)
        call check(error, ios == 0, "can't open "//file)
        if (allocated(error)) return
        inquire (unit=u, size=fsize)
        allocate (character(len=fsize) :: data)
        read (u, iostat=ios) data
        close (u)
        call check(error, ios == 0, "can't read "//file)
        if (allocated(error)) return
        code = ""
        name = ""
        point_char = ""
        before = ""
        after = ""
        ! 0: headers, 1: in Code, 2: before text, 3: after text.
        state = 0
        ran = 0
        pos = 1

        do while (pos <= len(data))
            nl = index(data(pos:), LF)

            if (nl == 0) then
                line = data(pos:)
                pos = len(data) + 1
            else
                line = data(pos:pos + nl - 2)
                pos = pos + nl
            end if

            select case (state)
            case (2)
                if (line == "=-=") then
                    state = 3
                    after = ""
                else
                    before = before//line//LF
                end if

                cycle
            case (3)
                if (line == "=-=-=") then
                    state = 0
                    ! The texts end with a newline (ert-test--erts-test).
                    call run_erts_test(error, name, code, point_char, before, after)
                    if (allocated(error)) return
                    ran = ran + 1
                else
                    after = after//line//LF
                end if

                cycle
            case (1)
                ! (.and. does not short-circuit: test the length first.)
                if (len_trim(line) > 0) then
                    if (line(1:1) == " ") then
                        code = code//" "//trim(adjustl(line))
                        cycle
                    end if
                end if

                state = 0
            case default
                continue
            end select

            if (index(line, "Name: ") == 1) then
                name = line(7:)
            else if (index(line, "Point-Char: ") == 1) then
                point_char = line(13:)
            else if (line == "Code:") then
                code = ""
                state = 1
            else if (line == "=-=") then
                before = ""
                state = 2
            end if
        end do

        call check(error, ran > 0, "no tests in "//file)
    end subroutine run_erts_file

    ! Run one erts test: recognize the forms of CODE (setq-local of
    ! fill-column, sentence-end-double-space and
    ! fill-separate-heterogeneous-words-with-space; the fill call) and
    ! compare the result with AFTER.
    subroutine run_erts_test(error, name, code, point_char, before, after)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), intent(in) :: name, code, point_char, before, after
        type(handle_type) :: h
        type(status_type) :: st
        integer(int64) :: start, end, shortage
        integer :: at
        character(len=:), allocatable :: expected, got

        ! The semantic-linefeed functions are not ported.
        if (index(code, "semlf") > 0) return
        call temp_buffer("", h)
        at = 0
        if (len(point_char) > 0) at = index(before, point_char)

        if (at > 0) then
            call insert(before(1:at - 1)//before(at + len(point_char):), st)
            call goto_char(int(at, int64))
        else
            call insert(before, st)
            call goto_char(point_min())
        end if

        if (index(code, "(setq-local fill-column 35)") > 0) fill_column%value = 35
        if (index(code, "(setq-local sentence-end-double-space nil)") > 0) sentence_end_double_space%value = .false.
        if (index(code, "(setq-local sentence-end-double-space t)") > 0) sentence_end_double_space%value = .true.

        if (index(code, "(setq-local fill-separate-heterogeneous-words-with-space t)") > 0) then
            fill_separate_heterogeneous_words_with_space = .true.
        end if

        if (index(code, "(point-min) (point-max)") > 0) then
            call fill_region_as_paragraph(point_min(), point_max(), st)
        else if (index(code, "(fill-paragraph)") > 0) then
            call fill_paragraph(st)
        else
            ! FROM is point, TO the start of the last line.
            start = point()
            call goto_char(point_max())
            shortage = forward_line(-1_int64)
            call beginning_of_line()
            end = point()

            if (index(code, "(fill-region-as-paragraph") > 0) then
                call fill_region_as_paragraph(start, end, st)
            else
                call fill_region(start, end, st)
            end if
        end if

        expected = after

        if (len(point_char) > 0) then
            at = index(after, point_char)
            if (at > 0) expected = after(1:at - 1)//after(at + len(point_char):)
        end if

        got = buffer_string()
        call check(error, got == expected .and. len(got) == len(expected), name//":"//LF//got)
        call leave(h)
    end subroutine run_erts_test

    !> fill-match-adaptive-prefix limits the prefix by its length in
    !> characters, not its width: a tab (width 8) is a prefix with
    !> fill-column 5.
    subroutine test_prefix_length(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        character(len=:), allocatable :: prefix
        logical :: found

        call get_buffer_create("*fill-prefix-length*", h, st)
        call set_buffer(h, st)
        call insert(achar(9)//"abc"//LF//achar(9)//"def"//LF, st)
        call init_bindings()
        fill_column = 5_int64
        call fill_context_prefix(point_min(), point_max(), prefix, found)
        fill_column = 70_int64
        call check(error, found .and. prefix == achar(9), "["//prefix//"]")
        call kill_buffer(h, st)
    end subroutine test_prefix_length
end module test_fill_fm
