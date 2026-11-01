! test_newcomment_mod.f90 -- tests for commenting (newcomment_mod).
! Ports GNU Emacs 31.1 test/lisp/newcomment-tests.el.
!
! Copyright (C) 2018-2026 Free Software Foundation, Inc.
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

!> Ports test/lisp/newcomment-tests.el (test-uncomment-space-comment-continue,
!> with C mode's comment variables and syntax set by hand, as C mode is
!> not ported), plus tests of commenting with `#' comments.
module test_newcomment_mod
    use, intrinsic :: iso_fortran_env, only: int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_mod, only: status_type
    use handle_mod, only: handle_type
    use buffer_mod, only: get_or_make_buffer, set_buffer, close_buffer, erase_buffer
    use insdel_mod, only: insert
    use editfns_mod, only: point, point_min, point_max, goto_char, buffer_string
    use simple_mod, only: deactivate_mark
    use indent_mod, only: indent_tabs_mode
    use bindings_mod, only: init_bindings
    use syntax_mod, only: make_syntax_table, set_syntax_table, modify_syntax_entry, destroy_syntax_table
    use newcomment_mod, only: comment_start, comment_end, comment_start_skip, comment_continue, comment_style, &
                              comment_region, uncomment_region, comment_indent, comment_line, comment_dwim, &
                              comment_string_strip, comment_string_reverse, comment_padright, comment_padleft, &
                              comment_normalize_vars, COMMENT_STYLE_MULTI_LINE, COMMENT_STYLE_INDENT
    implicit none(type, external)
    private

    public :: collect_newcomment_mod

    character(len=*), parameter :: LF = achar(10)

contains

    !> Collect all exported unit tests.
    subroutine collect_newcomment_mod(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("test-uncomment-space-comment-continue", test_space_continue), &
                    new_unittest("comment-strings", test_strings), &
                    new_unittest("comment-region-hash", test_region_hash), &
                    new_unittest("comment-indent-hash", test_indent_hash), &
                    new_unittest("comment-line-hash", test_line_hash), &
                    new_unittest("comment-dwim-blank-line", test_dwim_blank) &
                    ]
    end subroutine collect_newcomment_mod

    !> A fresh buffer holding TEXT, with its own syntax TABLE.
    subroutine temp_buffer(text, h, table)
        character(len=*), intent(in) :: text
        type(handle_type), intent(out) :: h, table
        type(status_type) :: st

        call init_bindings()
        call get_or_make_buffer(" *newcomment-test*", h, st)
        call set_buffer(h, st)
        call erase_buffer(st)
        call deactivate_mark(.true.)
        call make_syntax_table(table, st)
        call set_syntax_table(table, st)
        call insert(text, st)
        indent_tabs_mode = .false.
    end subroutine temp_buffer

    !> A buffer with `#' comments ending at end of line (as in sh).
    subroutine hash_buffer(text, h, table)
        character(len=*), intent(in) :: text
        type(handle_type), intent(out) :: h, table
        type(status_type) :: st

        call temp_buffer(text, h, table)
        call modify_syntax_entry(iachar("#"), iachar("#"), "<", st)
        call modify_syntax_entry(10, 10, ">", st)
        call comment_start%set_local("# ")
    end subroutine hash_buffer

    subroutine leave(h, table)
        type(handle_type), intent(in) :: h, table
        type(status_type) :: st

        indent_tabs_mode = .true.
        call close_buffer(h, st)
        call destroy_syntax_table(table, st)
    end subroutine leave

    !> Port of newcomment-tests.el:test-uncomment-space-comment-continue.
    subroutine test_space_continue(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, table
        type(status_type) :: st
        integer(int64) :: saved_style

        call temp_buffer("", h, table)

        checks: block
            ! C mode's comments.
            call modify_syntax_entry(iachar("/"), iachar("/"), ". 124b", st)
            call modify_syntax_entry(iachar("*"), iachar("*"), ". 23", st)
            call modify_syntax_entry(10, 10, "> b", st)
            call comment_start%set_local("/* ")
            call comment_end%set_local(" */")
            call comment_start_skip%set_local("\(?://+\|/\*+\)\s *")
            saved_style = comment_style%value
            comment_style = COMMENT_STYLE_MULTI_LINE
            call comment_continue%set_local("   ")
            call insert("  a"//LF//"  b", st)
            call comment_region(point_min(), point_max(), st)
            call check(error, st%ok(), "comment_region failed")
            if (allocated(error)) exit checks
            call uncomment_region(point_min(), point_max(), st)
            call check(error, st%ok(), "uncomment_region failed")
            if (allocated(error)) exit checks
            call check(error, buffer_string(), "  a"//LF//"  b")
        end block checks

        comment_style = saved_style
        call leave(h, table)
    end subroutine test_space_continue

    subroutine test_strings(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, table
        type(status_type) :: st

        call check(error, comment_string_strip("  ab  ", .true., .true.), "ab")
        if (allocated(error)) return
        call check(error, comment_string_strip("  ab  ", .false., .true.), "  ab")
        if (allocated(error)) return
        call check(error, comment_string_reverse("/* "), " */")
        if (allocated(error)) return
        call hash_buffer("", h, table)

        checks: block
            call comment_normalize_vars(st)
            call check(error, comment_padright("# "), "# ")
            if (allocated(error)) exit checks
            call check(error, comment_padright("#", 2_int64), "### ")
            if (allocated(error)) exit checks
            call check(error, comment_padleft(" */", 1_int64), " **/")
        end block checks

        call leave(h, table)
    end subroutine test_strings

    subroutine test_region_hash(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, table
        type(status_type) :: st

        call hash_buffer("a"//LF//"  b"//LF, h, table)

        checks: block
            call comment_region(point_min(), point_max(), st)
            call check(error, st%ok(), "comment_region failed")
            if (allocated(error)) exit checks
            call check(error, buffer_string(), "# a"//LF//"#   b"//LF)
            if (allocated(error)) exit checks
            call uncomment_region(point_min(), point_max(), st)
            call check(error, buffer_string(), "a"//LF//"  b"//LF)
        end block checks

        call leave(h, table)
    end subroutine test_region_hash

    subroutine test_indent_hash(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, table
        type(status_type) :: st

        call hash_buffer("foo", h, table)

        checks: block
            call goto_char(1_int64)
            call comment_indent(st)
            call check(error, st%ok(), "comment_indent failed")
            if (allocated(error)) exit checks
            call check(error, buffer_string(), "foo"//repeat(" ", 29)//"# ")
            if (allocated(error)) exit checks
            call check(error, point() == point_max(), "point is not after the comment starter")
        end block checks

        call leave(h, table)
    end subroutine test_indent_hash

    subroutine test_line_hash(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, table
        type(status_type) :: st

        call hash_buffer("a"//LF//"b"//LF, h, table)

        checks: block
            call goto_char(1_int64)
            call comment_line(1_int64, st)
            call check(error, st%ok(), "comment_line failed")
            if (allocated(error)) exit checks
            call check(error, buffer_string(), "# a"//LF//"b"//LF)
            if (allocated(error)) exit checks
            call check(error, point() == 5, "point is not on the next line")
            if (allocated(error)) exit checks
            call goto_char(1_int64)
            call comment_line(1_int64, st)
            call check(error, buffer_string(), "a"//LF//"b"//LF)
        end block checks

        call leave(h, table)
    end subroutine test_line_hash

    subroutine test_dwim_blank(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, table
        type(status_type) :: st

        call hash_buffer("", h, table)

        checks: block
            call comment_dwim(st)
            call check(error, st%ok(), "comment_dwim failed")
            if (allocated(error)) exit checks
            call check(error, buffer_string(), "# ")
        end block checks

        call leave(h, table)
    end subroutine test_dwim_blank
end module test_newcomment_mod
