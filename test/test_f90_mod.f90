! test_f90_mod.f90 -- tests for F90 mode (f90_mod).
! Ports GNU Emacs 31.1 test/lisp/progmodes/f90-tests.el and
! test/lisp/emacs-lisp/regexp-opt-tests.el in part; f90-test-bug3729 is
! an expected failure upstream.
!
! Copyright (C) 2011-2026 Free Software Foundation, Inc.
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

!> Ports test/lisp/progmodes/f90-tests.el (all tests but the expected
!> failure f90-test-bug3729), test/lisp/emacs-lisp/regexp-opt-tests.el,
!> and tests of comment filling and `f90_mode' settings.
module test_f90_mod
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_mod, only: status_type
    use handle_mod, only: handle_type
    use buffer_mod, only: buffers, current, get_or_make_buffer, set_buffer, close_buffer, erase_buffer
    use insdel_mod, only: insert
    use editfns_mod, only: point, point_min, point_max, goto_char, buffer_string, buffer_substring, line_end_position
    use cmds_mod, only: forward_line, beginning_of_line
    use indent_mod, only: current_indentation, move_to_column, indent_rigidly
    use syntax_mod, only: skip_chars_forward, char_syntax
    use search_mod, only: string_match, match_beginning, match_end
    use bindings_mod, only: init_bindings
    use fill_mod, only: fill_paragraph
    use regexp_opt_mod, only: regexp_opt, regexp_opt_charset, regexp_unmatchable
    use f90_mod, only: f90_mode, f90_indent_region, f90_do_auto_fill, f90_indent_subprogram, f90_indent_line, &
                       f90_end_of_subprogram, f90_block_type, f90_smart_end, F90_SMART_END_NO_BLINK, &
                       f90_keywords_regexp, f90_program_block_regexp
    implicit none(type, external)
    private

    public :: collect_f90_mod

    character(len=*), parameter :: LF = achar(10)

contains

    !> Collect all exported unit tests.
    subroutine collect_f90_mod(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("f90-test-indent", test_indent), &
                    new_unittest("f90-test-bug3730", test_bug3730), &
                    new_unittest("f90-test-bug8691", test_bug8691), &
                    new_unittest("f90-test-bug8820", test_bug8820), &
                    new_unittest("f90-test-bug9553a", test_bug9553a), &
                    new_unittest("f90-test-bug9553b", test_bug9553b), &
                    new_unittest("f90-test-bug9690", test_bug9690), &
                    new_unittest("f90-test-bug13138", test_bug13138), &
                    new_unittest("f90-test-bug-19809", test_bug19809), &
                    new_unittest("f90-test-bug20680", test_bug20680), &
                    new_unittest("f90-test-bug20680b", test_bug20680b), &
                    new_unittest("f90-test-bug20969", test_bug20969), &
                    new_unittest("f90-test-bug20969b", test_bug20969b), &
                    new_unittest("f90-test-bug21794", test_bug21794), &
                    new_unittest("f90-test-bug25039", test_bug25039), &
                    new_unittest("f90-test-bug38415", test_bug38415), &
                    new_unittest("regexp-opt-longest-match", test_longest_match), &
                    new_unittest("regexp-opt-charset", test_charset), &
                    new_unittest("regexp-opt-groups", test_groups), &
                    new_unittest("f90-fill-comment", test_fill_comment) &
                    ]
    end subroutine collect_f90_mod

    !> A fresh buffer in F90 mode (with-temp-buffer (f90-mode)).
    subroutine f90_buffer(h)
        type(handle_type), intent(out) :: h
        type(status_type) :: st

        call init_bindings()
        call get_or_make_buffer(" *f90-test*", h, st)
        call set_buffer(h, st)
        call erase_buffer(st)
        call f90_mode(st)
    end subroutine f90_buffer

    subroutine leave(h)
        type(handle_type), intent(in) :: h
        type(status_type) :: st

        call close_buffer(h, st)
    end subroutine leave

    !> (insert S)
    subroutine ins(s)
        character(len=*), intent(in) :: s
        type(status_type) :: st

        call insert(s, st)
    end subroutine ins

    !> (forward-line N)
    subroutine fl(n)
        integer(int64), intent(in) :: n
        integer(int64) :: shortage

        shortage = forward_line(n)
    end subroutine fl

    !> The text from point to point + N.
    function ahead(n) result(s)
        integer(int64), intent(in) :: n
        character(len=:), allocatable :: s
        type(status_type) :: st

        call buffer_substring(point(), min(point() + n, point_max()), s, st)
    end function ahead

    !> Test F90 indentation.  Port of f90-tests.el:f90-test-indent.
    subroutine test_indent(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        character(len=*), parameter :: text = &
                                       "!! Comment before code."//LF// &
                                       "!!! Comments before code."//LF// &
                                       "#preprocessor before code"//LF// &
                                       LF// &
                                       "program progname"//LF// &
                                       LF// &
                                       "  implicit none"//LF// &
                                       LF// &
                                       "  integer :: i"//LF// &
                                       LF// &
                                       "  !! Comment."//LF// &
                                       LF// &
                                       "  do i = 1, 10"//LF// &
                                       LF// &
                                       "#preprocessor"//LF// &
                                       LF// &
                                       "     !! Comment."//LF// &
                                       "     if ( i % 2 == 0 ) then"//LF// &
                                       "        !! Comment."//LF// &
                                       "        cycle"//LF// &
                                       "     else"//LF// &
                                       "        write(*,*) i"//LF// &
                                       "     end if"//LF// &
                                       "  end do"//LF// &
                                       LF// &
                                       "!!! Comment."//LF// &
                                       LF// &
                                       "end program progname"//LF

        call f90_buffer(h)
        call ins(text)
        call indent_rigidly(point_min(), point_max(), -999_int64, st)
        call f90_indent_region(point_min(), point_max(), st)
        call check(error, st%ok(), "f90_indent_region failed")
        if (.not. allocated(error)) call check(error, buffer_string(), text)
        call leave(h)
    end subroutine test_indent

    !> Port of f90-tests.el:f90-test-bug3730.
    subroutine test_bug3730(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        integer(int64) :: col, moved

        call f90_buffer(h)
        call ins("a")
        call move_to_column(68_int64, col, force=.true.)
        call ins("(/ x /)")
        call f90_do_auto_fill(st)
        call beginning_of_line(1_int64)
        moved = skip_chars_forward(" "//achar(9), st)
        call check(error, ahead(3_int64), "&(/")
        call leave(h)
    end subroutine test_bug3730

    !> Indent the subprogram of TEXT and check the indentation of the line
    !> UP lines back from the end.
    subroutine indent_check(error, text, up, expected)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), intent(in) :: text
        integer(int64), intent(in) :: up, expected
        type(handle_type) :: h
        type(status_type) :: st

        call f90_buffer(h)
        call ins(text)
        call f90_indent_subprogram(st)
        call check(error, st%ok(), "f90_indent_subprogram failed")

        if (.not. allocated(error)) then
            call fl(-up)
            call check(error, current_indentation() == expected, "wrong indentation")
        end if

        call leave(h)
    end subroutine indent_check

    !> Port of f90-tests.el:f90-test-bug8691.
    subroutine test_bug8691(error)
        type(error_type), allocatable, intent(out) :: error

        call indent_check(error, "module modname"//LF//"type, bind(c) :: type1"//LF//"integer :: part1"//LF// &
                          "end type type1"//LF//"end module modname", 1_int64, 2_int64)
    end subroutine test_bug8691

    !> Port of f90-tests.el:f90-test-bug8820.
    subroutine test_bug8820(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h

        call f90_buffer(h)
        call check(error, char_syntax(iachar("%")), ".")
        call leave(h)
    end subroutine test_bug8820

    !> Port of f90-tests.el:f90-test-bug9553a.
    subroutine test_bug9553a(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call f90_buffer(h)
        call ins("!!!"//repeat(" aaaa", 20))
        call f90_do_auto_fill(st)
        call beginning_of_line(1_int64)
        call check(error, ahead(5_int64), "!!! a")
        call leave(h)
    end subroutine test_bug9553a

    !> Port of f90-tests.el:f90-test-bug9553b.
    subroutine test_bug9553b(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call f90_buffer(h)
        call ins("!!!"//repeat(" aaaa", 13)//"a, aaaa")
        call f90_do_auto_fill(st)
        call beginning_of_line(1_int64)
        call check(error, ahead(5_int64), "!!! a")
        call leave(h)
    end subroutine test_bug9553b

    !> Port of f90-tests.el:f90-test-bug9690.
    subroutine test_bug9690(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call f90_buffer(h)
        call ins("#include ""foo.h""")
        call f90_indent_line(st)
        call check(error, current_indentation() == 0, "indented")
        call leave(h)
    end subroutine test_bug9690

    !> Port of f90-tests.el:f90-test-bug13138.
    subroutine test_bug13138(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call f90_buffer(h)
        call ins("program prog"//LF//"  integer :: i = &"//LF//"#ifdef foo"//LF//"       & 1"//LF//"#else"//LF// &
                 "       & 2"//LF//"#endif"//LF//LF//"  write(*,*) i"//LF//"end program prog")
        call goto_char(point_min())
        call fl(2_int64)
        call f90_indent_subprogram(st)
        call check(error, current_indentation() == 0, "indented")
        call leave(h)
    end subroutine test_bug13138

    !> Port of f90-tests.el:f90-test-bug-19809.
    subroutine test_bug19809(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(f90_block_type) :: b

        call f90_buffer(h)
        ! The Fortran standard says that continued strings should have
        ! '&' at the start of continuation lines, but it seems gfortran
        ! allows them to be absent (albeit with a warning).
        call ins("program prog"//LF//"  write (*,*), '&"//LF//"end program prog'"//LF//"end program prog")
        call goto_char(point_min())
        b = f90_end_of_subprogram()
        call check(error, point() == point_max(), "not at the end")
        call leave(h)
    end subroutine test_bug19809

    !> Port of f90-tests.el:f90-test-bug20680.
    subroutine test_bug20680(error)
        type(error_type), allocatable, intent(out) :: error

        call indent_check(error, "module modname"//LF//"type, extends ( sometype ) :: type1"//LF// &
                          "integer :: part1"//LF//"end type type1"//LF//"end module modname", 1_int64, 2_int64)
    end subroutine test_bug20680

    !> Port of f90-tests.el:f90-test-bug20680b.
    subroutine test_bug20680b(error)
        type(error_type), allocatable, intent(out) :: error

        call indent_check(error, "module modname"//LF//"enum, bind(c)"//LF//"enumerator :: e1 = 0"//LF// &
                          "end enum"//LF//"end module modname", 1_int64, 2_int64)
    end subroutine test_bug20680b

    !> Port of f90-tests.el:f90-test-bug20969.
    subroutine test_bug20969(error)
        type(error_type), allocatable, intent(out) :: error

        call indent_check(error, "module modname"//LF//"type, extends ( sometype ), private :: type1"//LF// &
                          "integer :: part1"//LF//"end type type1"//LF//"end module modname", 1_int64, 2_int64)
    end subroutine test_bug20969

    !> Port of f90-tests.el:f90-test-bug20969b.
    subroutine test_bug20969b(error)
        type(error_type), allocatable, intent(out) :: error

        call indent_check(error, "module modname"//LF//"type, private, extends ( sometype ) :: type1"//LF// &
                          "integer :: part1"//LF//"end type type1"//LF//"end module modname", 1_int64, 2_int64)
    end subroutine test_bug20969b

    !> Port of f90-tests.el:f90-test-bug21794.
    subroutine test_bug21794(error)
        type(error_type), allocatable, intent(out) :: error

        call indent_check(error, "program prog"//LF//"do i=1,10"//LF//"associate (x => xa(i), y => ya(i))"//LF// &
                          "a(x,y,i) = fun(x,y,i)"//LF//"end associate"//LF//"end do"//LF//"end program prog", &
                          2_int64, 5_int64)
    end subroutine test_bug21794

    !> Port of f90-tests.el:f90-test-bug25039.
    subroutine test_bug25039(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call f90_buffer(h)
        call ins("program prog"//LF//"select type (a)"//LF//"type is (t1)"//LF//"x = 2"//LF//"class is (c1)"//LF// &
                 "x = 1"//LF//"class default"//LF//"x=3"//LF//"end select"//LF//"end program prog")
        call f90_indent_subprogram(st)

        checks: block
            call fl(-3_int64)
            call check(error, current_indentation() == 2, "class default")
            if (allocated(error)) exit checks
            call fl(-2_int64)
            call check(error, current_indentation() == 2, "class is")
            if (allocated(error)) exit checks
            call fl(-2_int64)
            call check(error, current_indentation() == 2, "type is")
        end block checks

        call leave(h)
    end subroutine test_bug25039

    !> Port of f90-tests.el:f90-test-bug38415.
    subroutine test_bug38415(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        character(len=:), allocatable :: s

        call f90_buffer(h)
        call f90_smart_end%set_local(F90_SMART_END_NO_BLINK)

        checks: block
            call ins("module function foo(x)"//LF//"real :: x"//LF//"end")
            call f90_indent_line(st)
            call buffer_substring(point(), line_end_position(), s, st)
            call check(error, s, " function foo")
            if (allocated(error)) exit checks
            call goto_char(point_max())
            call ins(LF//"module subroutine bar(x)"//LF//"real :: x"//LF//"end")
            call f90_indent_line(st)
            call buffer_substring(point(), line_end_position(), s, st)
            call check(error, s, " subroutine bar")
        end block checks

        call leave(h)
    end subroutine test_bug38415

    !> Check that the regexp always matches as much as possible.  Port of
    !> regexp-opt-tests.el:regexp-opt-longest-match (all permutations).
    subroutine test_longest_match(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=3), parameter :: words(4) = [character(len=3) :: "a", "ab", "ac", "abc"]
        character(len=3) :: perm(4)
        integer :: i, j, k, l
        integer(int64) :: index
        type(status_type) :: st

        do i = 1, 4
            do j = 1, 4
                do k = 1, 4
                    do l = 1, 4
                        if (i == j .or. i == k .or. i == l .or. j == k .or. j == l .or. k == l) cycle
                        perm = [words(i), words(j), words(k), words(l)]
                        index = string_match(regexp_opt(perm), "abcd", st)
                        call check(error, index == 1 .and. match_end(0, st) == 4, "not the longest match")
                        if (allocated(error)) return
                    end do
                end do
            end do
        end do
    end subroutine test_longest_match

    !> Port of regexp-opt-tests.el:regexp-opt-charset.
    subroutine test_charset(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, regexp_opt_charset([97, 98, 97]), "[ab]")
        if (allocated(error)) return
        call check(error, regexp_opt_charset([68, 100, 66, 97, 98, 67, 55, 97, 99, 65]), "[7A-Da-d]")
        if (allocated(error)) return
        call check(error, regexp_opt_charset([97]), "a")
        if (allocated(error)) return
        call check(error, regexp_opt_charset([94]), "\^")
        if (allocated(error)) return
        call check(error, regexp_opt_charset([45]), "-")
        if (allocated(error)) return
        call check(error, regexp_opt_charset([93]), "]")
        if (allocated(error)) return
        call check(error, regexp_opt_charset([94, 93]), "[]^]")
        if (allocated(error)) return
        call check(error, regexp_opt_charset([94, 45]), "[-^]")
        if (allocated(error)) return
        call check(error, regexp_opt_charset([45, 93]), "[]-]")
        if (allocated(error)) return
        call check(error, regexp_opt_charset([45, 93, 94]), "[]^-]")
        if (allocated(error)) return
        call check(error, regexp_opt_charset([94, 97]), "[a^]")
        if (allocated(error)) return
        call check(error, regexp_opt_charset([45, 97]), "[a-]")
        if (allocated(error)) return
        call check(error, regexp_opt_charset([93, 97]), "[]a]")
        if (allocated(error)) return
        call check(error, regexp_opt_charset([94, 93, 97]), "[]a^]")
        if (allocated(error)) return
        call check(error, regexp_opt_charset([94, 45, 97]), "[a^-]")
        if (allocated(error)) return
        call check(error, regexp_opt_charset([45, 93, 97]), "[]a-]")
        if (allocated(error)) return
        call check(error, regexp_opt_charset([45, 93, 94, 97]), "[]a^-]")
        if (allocated(error)) return
        call check(error, regexp_opt_charset([integer(int32) ::]), regexp_unmatchable)
    end subroutine test_charset

    !> The groups regexp-opt makes for F90 mode's regexps.
    subroutine test_groups(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        integer(int64) :: index

        call f90_buffer(h)
        call check(error, f90_program_block_regexp, "\(function\|module\|program\|sub\(?:\(?:modul\|routin\)e\)\)")

        if (.not. allocated(error)) then
            index = string_match(f90_keywords_regexp, "x = error stop", st)
            call check(error, index == 5, "keywords: error stop")
        end if

        call leave(h)
    end subroutine test_groups

    !> M-q in an F90 comment fills the comment (fill_comment_paragraph).
    subroutine test_fill_comment(error)
        use buffer_mod, only: fill_column
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        integer(int64) :: saved

        call f90_buffer(h)
        saved = fill_column%value
        fill_column = 20
        call ins("! aaaa bbbb cccc dddd eeee"//LF//"x = 1"//LF)
        call goto_char(3_int64)
        call fill_paragraph(st)
        call check(error, buffer_string(), "! aaaa bbbb cccc"//LF//"! dddd eeee"//LF//"x = 1"//LF)
        fill_column = saved
        call leave(h)
    end subroutine test_fill_comment
end module test_f90_mod
