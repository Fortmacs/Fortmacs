! test_simple_goto_fm.f90 -- tests for goto-line, the narrowing commands,
! count-words (simple_goto_fm) and number-at-point (thingatpt_fm).
! Ports GNU Emacs 31.1 test/lisp/simple-tests.el and
! test/lisp/thingatpt-tests.el in part; the count-words and
! number-at-point tests.
!
! Copyright (C) 2013-2026 Free Software Foundation, Inc.
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

!> Ports simple-tests.el's simple-test-count-words-bug-41761 and
!> thingatpt-tests.el's test-numbers-none, -decimal, -hex-lisp and
!> -hex-c, plus own tests of `goto_line', `goto_line_relative',
!> `count_words_format' and the keys.
module test_simple_goto_fm
    use, intrinsic :: iso_fortran_env, only: int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_fm, only: status_type
    use handle_fm, only: handle_type
    use lisp_fm, only: list, value_type
    use buffer_fm, only: get_buffer_create, set_buffer, erase_buffer
    use insdel_fm, only: insert
    use editfns_fm, only: point, goto_char, point_min, point_max, widen, narrow_to_region, is_buffer_narrowed
    use cmds_fm, only: forward_char
    use textprop_fm, only: put_text_property
    use keymap_fm, only: key_parse, key_binding
    use bindings_fm, only: init_bindings
    use lisp_fm, only: value_type
    use data_fm, only: value_eq
    use simple_fm, only: goto_line, goto_line_relative, count_words, count_words_format, line_number_at_pos
    use thingatpt_fm, only: number_at_point
    implicit none(type, external)
    private

    public :: collect_simple_goto_fm

    character(len=*), parameter :: LF = achar(10)

contains

    !> Collect all exported unit tests.
    subroutine collect_simple_goto_fm(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("simple-test-count-words-bug-41761", test_count_words_41761), &
                    new_unittest("count-words-format", test_count_words_format), &
                    new_unittest("goto-line", test_goto_line), &
                    new_unittest("goto-line-relative", test_goto_line_relative), &
                    new_unittest("test-numbers-none", test_numbers_none), &
                    new_unittest("test-numbers-decimal", test_numbers_decimal), &
                    new_unittest("test-numbers-hex-lisp", test_numbers_hex_lisp), &
                    new_unittest("test-numbers-hex-c", test_numbers_hex_c), &
                    new_unittest("goto-and-narrow-keys", test_keys) &
                    ]
    end subroutine collect_simple_goto_fm

    subroutine temp_buffer(text)
        character(len=*), intent(in) :: text
        type(handle_type) :: buf
        type(status_type) :: st

        call init_bindings()
        call get_buffer_create(" *goto-test*", buf, st)
        call set_buffer(buf, st)
        call widen()
        call erase_buffer(st)
        call insert(text, st)
    end subroutine temp_buffer

    subroutine test_count_words_41761(error)
        type(error_type), allocatable, intent(out) :: error
        integer :: i
        integer(int64) :: start
        type(status_type) :: st

        call temp_buffer("")

        do i = 1, 10
            start = point()
            call insert("test ", st)
            ! (propertize "test " 'field (cons nil nil)): a field of its own.
            call put_text_property(start, point(), value_type(name="field"), list(value_type(name="x"), value_type(.true.)), st)
        end do

        call check(error, count_words(point_min(), point_max()) == 10, "10 words")
    end subroutine test_count_words_41761

    subroutine test_count_words_format(error)
        type(error_type), allocatable, intent(out) :: error

        call temp_buffer("One two.  Three four five."//LF//"Six.")
        call check(error, count_words_format("Buffer", point_min(), point_max()), &
                   "Buffer has 2 lines, 3 sentences, 6 words, and 31 characters")
        if (allocated(error)) return
        call temp_buffer("a")
        call check(error, count_words_format("Region", point_min(), point_max()), &
                   "Region has 1 line, 1 sentence, 1 word, and 1 character")
    end subroutine test_count_words_format

    subroutine test_goto_line(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st

        call temp_buffer("1"//LF//"2"//LF//"3"//LF//"4"//LF//"5"//LF)

        checks: block
            call goto_line(3_int64, st)
            call check(error, point() == 5, "line 3")
            if (allocated(error)) exit checks
            ! A line outside the narrowing widens (widen_automatically).
            call narrow_to_region(1_int64, 4_int64, st)
            call goto_line(5_int64, st)
            call check(error, point() == 9 .and. .not. is_buffer_narrowed(), "line 5 widens")
            if (allocated(error)) exit checks
            ! A line inside it keeps the narrowing.
            call narrow_to_region(3_int64, 8_int64, st)
            call goto_line(3_int64, st)
            call check(error, point() == 5 .and. is_buffer_narrowed(), "line 3 in the narrowing")
        end block checks

        call widen()
    end subroutine test_goto_line

    subroutine test_goto_line_relative(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st

        call temp_buffer("1"//LF//"2"//LF//"3"//LF//"4"//LF//"5"//LF)
        call narrow_to_region(5_int64, 11_int64, st)
        call goto_line_relative(2_int64, st)
        call check(error, point() == 7 .and. line_number_at_pos() == 2, "relative line 2")
        call widen()
    end subroutine test_goto_line_relative

    !> The number at position POS of TEXT (thingatpt-tests.el's
    !> test--number), FOUND false for nil.
    subroutine number_in(text, pos, n, found)
        character(len=*), intent(in) :: text
        integer, intent(in) :: pos
        integer(int64), intent(out) :: n
        logical, intent(out) :: found
        logical :: is_integer
        type(status_type) :: st

        call temp_buffer(text//LF)
        call goto_char(point_min())
        call forward_char(st, int(pos, int64))
        call number_at_point(n, found, is_integer)
        found = found .and. is_integer
    end subroutine number_in

    subroutine test_numbers_none(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int64) :: n
        logical :: found

        call number_in("foo", 0, n, found)
        call check(error,.not. found, "no number")
    end subroutine test_numbers_none

    subroutine test_numbers_decimal(error)
        type(error_type), allocatable, intent(out) :: error

        call expect("42", [0, 1, 2], 42_int64, error)
    end subroutine test_numbers_decimal

    subroutine test_numbers_hex_lisp(error)
        type(error_type), allocatable, intent(out) :: error

        call expect("#x42", [0, 1, 2], 66_int64, error)
        if (allocated(error)) return
        call expect("#xf00", [0, 1, 2, 3], 3840_int64, error)
    end subroutine test_numbers_hex_lisp

    subroutine test_numbers_hex_c(error)
        type(error_type), allocatable, intent(out) :: error

        call expect("0x42", [0, 1, 2], 66_int64, error)
        if (allocated(error)) return
        call expect("0xf00", [0, 1, 2, 3], 3840_int64, error)
    end subroutine test_numbers_hex_c

    subroutine expect(text, positions, value, error)
        character(len=*), intent(in) :: text
        integer, intent(in) :: positions(:)
        integer(int64), intent(in) :: value
        type(error_type), allocatable, intent(out) :: error
        integer(int64) :: n
        logical :: found
        integer :: k

        do k = 1, size(positions)
            call number_in(text, positions(k), n, found)
            call check(error, found .and. n == value, text//" at a position")
            if (allocated(error)) return
        end do
    end subroutine expect

    subroutine test_keys(error)
        type(error_type), allocatable, intent(out) :: error

        call init_bindings()
        call expect_key("M-g g", "goto_line", error)
        if (allocated(error)) return
        call expect_key("M-g c", "goto_char", error)
        if (allocated(error)) return
        call expect_key("M-g TAB", "move_to_column", error)
        if (allocated(error)) return
        call expect_key("C-x n n", "narrow_to_region", error)
        if (allocated(error)) return
        call expect_key("C-x n w", "widen", error)
        if (allocated(error)) return
        call expect_key("C-x n g", "goto_line_relative", error)
        if (allocated(error)) return
        call expect_key("C-x n d", "narrow_to_subprogram", error)
        if (allocated(error)) return
        call expect_key("C-x n p", "narrow_to_page", error)
        if (allocated(error)) return
        call expect_key("C-x C-p", "mark_page", error)
        if (allocated(error)) return
        call expect_key("C-x l", "count_lines_page", error)
        if (allocated(error)) return
        call expect_key("M-=", "count_words_region", error)
        if (allocated(error)) return
        call expect_key("C-M-a", "beginning_of_subprogram", error)
        if (allocated(error)) return
        call expect_key("C-M-e", "end_of_subprogram", error)
        if (allocated(error)) return
        call expect_key("C-M-h", "mark_subprogram", error)
        if (allocated(error)) return
        call expect_key("M-(", "insert_parentheses", error)
    end subroutine test_keys

    subroutine expect_key(keys, command, error)
        character(len=*), intent(in) :: keys, command
        type(error_type), allocatable, intent(out) :: error
        type(value_type), allocatable :: key(:)
        type(value_type) :: binding
        type(status_type) :: st

        call temp_buffer("")
        call key_parse(keys, key, st)
        call key_binding(key, binding, st)
        call check(error, value_eq(binding, value_type(name=command)), keys//" is not "//command)
    end subroutine expect_key
end module test_simple_goto_fm
