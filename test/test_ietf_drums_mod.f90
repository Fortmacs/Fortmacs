! test_ietf_drums_mod.f90 -- tests for ietf_drums_mod.
! Ports GNU Emacs 31.1 test/lisp/mail/ietf-drums-tests.el in part; the
! rest tests functions that are not ported yet (docs/status/ietf_drums_mod.md).
!
! Copyright (C) 2022-2026 Free Software Foundation, Inc.
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

!> The ietf-drums-tests.el cases of removing comments and whitespace.
module test_ietf_drums_mod
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use ietf_drums_mod, only: remove_ietf_drums_comments, remove_ietf_drums_whitespace, &
                              unfold_ietf_drums_folding_whitespace
    implicit none(type, external)
    private

    public :: collect_ietf_drums_mod

    character(len=*), parameter :: CR = achar(13), LF = achar(10), TAB = achar(9)

contains

    !> Collect all exported unit tests.
    subroutine collect_ietf_drums_mod(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("ietf-drums-tests", test_ietf_drums), &
                    new_unittest("ietf-drums-unfold-folding-whitespace", test_unfold) &
                    ]
    end subroutine collect_ietf_drums_mod

    !> Test ietf-drums functionality: removing comments and whitespace.
    !> Ports ietf-drums-tests.el:ietf-drums-tests (its remove-comments and
    !> remove-whitespace cases).
    subroutine test_ietf_drums(error)
        type(error_type), allocatable, intent(out) :: error

        ! remove_ietf_drums_comments
        call expect(remove_ietf_drums_comments("random string"), "random string")
        if (allocated(error)) return
        call expect(remove_ietf_drums_comments('random "non comment" string'), 'random "non comment" string')
        if (allocated(error)) return
        call expect(remove_ietf_drums_comments("random (comment) string"), "random  string")
        if (allocated(error)) return
        call expect(remove_ietf_drums_comments("random (comment) (string)"), "random  ")
        if (allocated(error)) return
        call expect(remove_ietf_drums_comments("random (first) (second (and)) (third) not fourth"), "random    not fourth")
        if (allocated(error)) return
        ! Test some unterminated comments.
        call expect(remove_ietf_drums_comments("test an (unterminated comment"), "test an ")
        if (allocated(error)) return
        ! Returns the string unchanged (and doesn't barf).
        call expect(remove_ietf_drums_comments('test an "unterminated quote'), 'test an "unterminated quote')
        if (allocated(error)) return
        ! Note that double-quote is not special.
        call expect(remove_ietf_drums_comments('test (unterminated comments with "quoted (" )stuff'), "test ")
        if (allocated(error)) return
        ! remove_ietf_drums_whitespace
        call expect(remove_ietf_drums_whitespace("random string"), "randomstring")
        if (allocated(error)) return
        call expect(remove_ietf_drums_whitespace("random (comment) string"), "random(comment)string")
        if (allocated(error)) return
        call expect(remove_ietf_drums_whitespace('random "non comment" string'), 'random"non comment"string')
        if (allocated(error)) return
        call expect(remove_ietf_drums_whitespace("random (comment)"//CR//LF//"(string)"), "random(comment)(string)")
        if (allocated(error)) return
        call expect(remove_ietf_drums_whitespace("random (first) (second (and)) (third) not fourth"), &
                    "random(first)(second (and))(third)notfourth")
        if (allocated(error)) return
        ! Test some unterminated comments and quotes.
        call expect(remove_ietf_drums_whitespace("random (first) (second (and)) (third unterminated"), &
                    "random(first)(second (and))(third unterminated")
        if (allocated(error)) return
        call expect(remove_ietf_drums_whitespace('random "non terminated string'), 'random"non terminated string')

    contains

        subroutine expect(got, expected)
            character(len=*), intent(in) :: got, expected

            call check(error, got == expected, "got '"//got//"', expected '"//expected//"'")
        end subroutine expect
    end subroutine test_ietf_drums

    !> Folding white space becomes one space; a newline without white
    !> space after it stays.
    subroutine test_unfold(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, unfold_ietf_drums_folding_whitespace("text/plain;"//LF//TAB//"charset=utf-8") == &
                   "text/plain; charset=utf-8")
        if (allocated(error)) return
        call check(error, unfold_ietf_drums_folding_whitespace("a  "//LF//"  b"//LF//"c") == "a b"//LF//"c")
    end subroutine test_unfold
end module test_ietf_drums_mod
