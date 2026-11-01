! test_bidi_mod.f90 -- tests for the bidi iterator (bidi_mod).
! Ports GNU Emacs 31.1 test/src/xdisp-tests.el in part; the rest needs
! windows, pixel sizes, display properties and Lisp.
!
! Copyright (C) 2020-2026 Free Software Foundation, Inc.
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

!> Ports xdisp-tests--find-directional-overrides-case-1 to -3; own
!> tests of the bidi classes, the paragraph direction, the resolved levels
!> and the visual order.
module test_bidi_mod
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_mod, only: status_type
    use handle_mod, only: handle_type
    use buffer_mod, only: get_or_make_buffer, set_buffer, close_buffer, buffers, current
    use insdel_mod, only: insert
    use editfns_mod, only: goto_char
    use bidi_mod, only: bidi_it_type, bidi_init_it, bidi_paragraph_init, bidi_move_to_visually_next, bidi_get_type, &
                        bidi_mirror_char, L2R, R2L, NEUTRAL_DIR, STRONG_L, STRONG_R, STRONG_AL, WEAK_EN, WEAK_AN, &
                        NEUTRAL_ON, RLO, PDI
    use xdisp_mod, only: bidi_find_overridden_directionality, current_bidi_paragraph_direction
    implicit none(type, external)
    private

    public :: collect_bidi_mod

    character(len=*), parameter :: LF = achar(10), TAB = achar(9)
    !> Hebrew alef, bet, gimel.
    character(len=*), parameter :: HEB = "אבג"

contains

    !> Collect all exported unit tests.
    subroutine collect_bidi_mod(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("bidi-classes", test_classes), &
                    new_unittest("bidi-paragraph-direction", test_paragraph_direction), &
                    new_unittest("bidi-visual-order", test_visual_order), &
                    new_unittest("bidi-numbers-in-rtl", test_numbers), &
                    new_unittest("xdisp-tests--find-directional-overrides-case-1", test_overrides_1), &
                    new_unittest("xdisp-tests--find-directional-overrides-case-2", test_overrides_2), &
                    new_unittest("xdisp-tests--find-directional-overrides-case-3", test_overrides_3), &
                    new_unittest("bidi-find-overridden-directionality-string", test_overrides_string) &
                    ]
    end subroutine collect_bidi_mod

    !> A fresh buffer holding TEXT, point at its start.
    subroutine buffer_with(text, h)
        character(len=*), intent(in) :: text
        type(handle_type), intent(out) :: h
        type(status_type) :: st

        call get_or_make_buffer(" *bidi-test*", h, st)
        call set_buffer(h, st)
        call insert(text, st)
        call goto_char(1_int64)
    end subroutine buffer_with

    !> The character positions of the current buffer's first line in
    !> visual order, the paragraph's direction DIR, and their levels.
    subroutine visual_order(dir, order, levels, n)
        integer, intent(in) :: dir
        integer(int64), intent(out) :: order(:)
        integer, intent(out) :: levels(:)
        integer, intent(out) :: n
        type(bidi_it_type) :: it

        call bidi_init_it(1_int64, 1_int64, .false., it)
        call bidi_paragraph_init(dir, it, .false.)
        n = 0

        do while (n < size(order))
            call bidi_move_to_visually_next(it)
            if (it%charpos >= buffers(current)%zv .or. it%ch == 10) exit
            n = n + 1
            order(n) = it%charpos
            levels(n) = it%resolved_level
        end do
    end subroutine visual_order

    !> The bidi classes of the generated table, mirrors.
    subroutine test_classes(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, bidi_get_type(iachar("a", int32), NEUTRAL_DIR) == STRONG_L .and. &
                   bidi_get_type(int(z'5D0', int32), NEUTRAL_DIR) == STRONG_R .and. &
                   bidi_get_type(int(z'627', int32), NEUTRAL_DIR) == STRONG_AL .and. &
                   bidi_get_type(iachar("1", int32), NEUTRAL_DIR) == WEAK_EN .and. &
                   bidi_get_type(int(z'660', int32), NEUTRAL_DIR) == WEAK_AN .and. &
                   bidi_get_type(iachar("!", int32), NEUTRAL_DIR) == NEUTRAL_ON .and. &
                   bidi_get_type(int(z'202E', int32), NEUTRAL_DIR) == RLO .and. &
                   bidi_get_type(int(z'2069', int32), NEUTRAL_DIR) == PDI, "classes")
        if (allocated(error)) return
        ! Unassigned code points get the defaults of their block.
        call check(error, bidi_get_type(int(z'5FF', int32), NEUTRAL_DIR) == STRONG_R .and. &
                   bidi_get_type(int(z'7BF', int32), NEUTRAL_DIR) == STRONG_AL .and. &
                   bidi_get_type(int(z'E0080', int32), NEUTRAL_DIR) == STRONG_L, "defaults")
        if (allocated(error)) return
        ! An override makes non-formatting characters strong.
        call check(error, bidi_get_type(iachar("1", int32), R2L) == STRONG_R, "override")
        if (allocated(error)) return
        call check(error, bidi_mirror_char(iachar("(", int32)) == iachar(")") .and. &
                   bidi_mirror_char(iachar("a", int32)) == iachar("a"), "mirroring")
    end subroutine test_classes

    !> current-bidi-paragraph-direction: from the first strong character
    !> of the paragraph, or `bidi_paragraph_direction'.
    subroutine test_paragraph_direction(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call buffer_with("  "//HEB//" abc"//LF//LF//"123 xyz"//LF, h)
        call check(error, current_bidi_paragraph_direction() == R2L, "right to left")
        if (allocated(error)) return
        call goto_char(buffers(current)%zv)
        call check(error, current_bidi_paragraph_direction() == L2R, "the second paragraph")
        if (allocated(error)) return
        buffers(current)%bidi_paragraph_direction = R2L
        call check(error, current_bidi_paragraph_direction() == R2L, "forced")
        if (allocated(error)) return
        buffers(current)%bidi_paragraph_direction = NEUTRAL_DIR
        buffers(current)%bidi_display_reordering = .false.
        call goto_char(1_int64)
        call check(error, current_bidi_paragraph_direction() == L2R, "no reordering")
        call close_buffer(h, st)
    end subroutine test_paragraph_direction

    !> Hebrew in a left-to-right line is shown reversed (UAX#9 L2).
    subroutine test_visual_order(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        integer(int64) :: order(20)
        integer :: levels(20), n

        call buffer_with("ab "//HEB//" cd"//LF, h)
        call visual_order(L2R, order, levels, n)
        call check(error, n == 9, "nine characters")
        if (allocated(error)) return
        call check(error, all(order(1:9) == [1, 2, 3, 6, 5, 4, 7, 8, 9]), "order")
        if (allocated(error)) return
        call check(error, all(levels(1:9) == [0, 0, 0, 1, 1, 1, 0, 0, 0]), "levels")
        call close_buffer(h, st)
    end subroutine test_visual_order

    !> European numbers after Hebrew in a left-to-right paragraph: the
    !> numbers at level 2, inside the reversed Hebrew run.
    subroutine test_numbers(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        integer(int64) :: order(20)
        integer :: levels(20), n

        call buffer_with(HEB//" 12"//LF, h)
        call visual_order(L2R, order, levels, n)
        call check(error, n == 6, "six characters")
        if (allocated(error)) return
        call check(error, all(order(1:6) == [5, 6, 4, 3, 2, 1]), "order")
        if (allocated(error)) return
        call check(error, all(levels(1:6) == [2, 2, 1, 1, 1, 1]), "levels")
        call close_buffer(h, st)
    end subroutine test_numbers

    subroutine test_overrides_1(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call buffer_with( &
            "int main() {"//LF// &
            "  bool isAdmin = false;"//LF// &
            "  /*‮ }⁦if (isAdmin)⁩ ⁦ begin admins only */"//LF// &
            '  printf("You are an admin.\n");'//LF// &
            "  /* end admins only ‮ { ⁦*/"//LF// &
            "  return 0;"//LF// &
            "}", h)
        call check(error, bidi_find_overridden_directionality(1_int64, buffers(current)%zv, st) == 46, "46")
        call close_buffer(h, st)
    end subroutine test_overrides_1

    subroutine test_overrides_2(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call buffer_with( &
            "#define is_restricted_user(user)"//TAB//TAB//TAB//"\"//LF// &
            '  !strcmp (user, "root") ? 0 :'//TAB//TAB//TAB//"\"//LF// &
            '  !strcmp (user, "admin") ? 0 :'//TAB//TAB//TAB//"\"//LF// &
            '  !strcmp (user, "superuser‮⁦? 0 : 1⁩ ⁦")⁩‬'//LF// &
            LF// &
            "int main () {"//LF// &
            '  printf ("root: %d\n", is_restricted_user ("root"));'//LF// &
            '  printf ("admin: %d\n", is_restricted_user ("admin"));'//LF// &
            '  printf ("superuser: %d\n", is_restricted_user ("superuser"));'//LF// &
            '  printf ("luser: %d\n", is_restricted_user ("luser"));'//LF// &
            '  printf ("nobody: %d\n", is_restricted_user ("nobody"));'//LF// &
            "}", h)
        call check(error, bidi_find_overridden_directionality(1_int64, buffers(current)%zv, st) == 138, "138")
        call close_buffer(h, st)
    end subroutine test_overrides_2

    subroutine test_overrides_3(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call buffer_with( &
            "#define is_restricted_user(user)"//TAB//TAB//TAB//"\"//LF// &
            '  !strcmp (user, "root") ? 0 :'//TAB//TAB//TAB//"\"//LF// &
            '  !strcmp (user, "admin") ? 0 :'//TAB//TAB//TAB//"\"//LF// &
            '  !strcmp (user, "superuser‮⁦? '//"'#' : '!'"//'⁩ ⁦")⁩‬'//LF// &
            LF// &
            "int main () {"//LF// &
            '  printf ("root: %d\n", is_restricted_user ("root"));'//LF// &
            '  printf ("admin: %d\n", is_restricted_user ("admin"));'//LF// &
            '  printf ("superuser: %d\n", is_restricted_user ("superuser"));'//LF// &
            '  printf ("luser: %d\n", is_restricted_user ("luser"));'//LF// &
            '  printf ("nobody: %d\n", is_restricted_user ("nobody"));'//LF// &
            "}", h)
        call check(error, bidi_find_overridden_directionality(1_int64, buffers(current)%zv, st) == 138, "138")
        call close_buffer(h, st)
    end subroutine test_overrides_3

    !> A string OBJECT: positions are 0-based; nothing overridden is -1.
    subroutine test_overrides_string(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st

        call check(error, bidi_find_overridden_directionality(0_int64, 5_int64, st, "ab‮cd") == 3, "RLO")
        if (allocated(error)) return
        call check(error, bidi_find_overridden_directionality(0_int64, 5_int64, st, "abcde") == -1, "none")
    end subroutine test_overrides_string
end module test_bidi_mod
