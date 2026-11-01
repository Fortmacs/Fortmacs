! test_smie_mod.f90 -- tests for SMIE (smie_mod).
! Own tests; upstream has no SMIE tests.
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

!> Upstream has no SMIE tests; these are own tests of the grammar
!> conversions (the levels obey the precedences and the BNF), expression
!> motion, `forward_expression' through `forward_expression_function', indentation
!> and `smie_close_block' with a small Pascal-like grammar.
module test_smie_mod
    use, intrinsic :: iso_fortran_env, only: int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_mod, only: status_type
    use handle_mod, only: handle_type
    use buffer_mod, only: get_or_make_buffer, set_buffer, close_buffer, erase_buffer
    use insdel_mod, only: insert
    use editfns_mod, only: point, point_min, point_max, goto_char, buffer_string
    use bindings_mod, only: init_bindings
    use expression_motion_mod, only: forward_expression
    use indent_mod, only: indent_region
    use newcomment_mod, only: comment_start
    use smie_mod, only: smie_bnf_type, smie_precs_type, smie_prec2_type, smie_grammar_type, smie_rule_result_type, &
                        smie_precs_to_prec2, smie_bnf_to_prec2, smie_prec2_to_grammar, smie_setup, &
                        smie_forward_expression_command, smie_close_block, smie_grammar_token, smie_toklevels_type, &
                        SMIE_LEFT, SMIE_ASSOC, SMIE_LEVEL_NUMBER, SMIE_LEVEL_LIST
    implicit none(type, external)
    private

    public :: collect_smie_mod

    character(len=*), parameter :: LF = achar(10)

    type(smie_grammar_type), target, save :: pascal_grammar
    logical, save :: has_grammar = .false.

contains

    !> Collect all exported unit tests.
    subroutine collect_smie_mod(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("smie-precs->prec2", test_precs), &
                    new_unittest("smie-bnf->prec2", test_bnf), &
                    new_unittest("smie-forward-sexp-command", test_expression), &
                    new_unittest("smie-indent-line", test_indent), &
                    new_unittest("smie-close-block", test_close_block) &
                    ]
    end subroutine collect_smie_mod

    ! The rules: no opinion (the defaults).
    subroutine no_rules(method, arg, result, status)
        integer, intent(in) :: method
        character(len=*), intent(in) :: arg
        type(smie_rule_result_type), intent(out) :: result
        type(status_type), intent(out) :: status

        status = status_type()
        result = smie_rule_result_type()
        if (method < 0 .or. len(arg) < 0) continue
    end subroutine no_rules

    subroutine make_grammar(status)
        type(status_type), intent(out) :: status
        type(smie_bnf_type) :: bnf
        type(smie_precs_type) :: precs(1)
        type(smie_prec2_type) :: prec2

        status = status_type()
        if (has_grammar) return
        call bnf%append("inst", [character(len=6) :: "begin", "insts", "end"])
        call bnf%append("inst", [character(len=6) :: "if", "exp", "then", "inst", "else", "inst"])
        call bnf%append("inst", [character(len=6) :: "exp", ":=", "exp"])
        call bnf%append("inst", [character(len=6) :: "exp"])
        call bnf%append("insts", [character(len=6) :: "insts", ";", "insts"])
        call bnf%append("insts", [character(len=6) :: "inst"])
        call bnf%append("exp", [character(len=6) :: "exp", "+", "exp"])
        call bnf%append("exp", [character(len=6) :: "exp", "*", "exp"])
        call precs(1)%append(SMIE_ASSOC, [character(len=1) :: ";"])
        call precs(1)%append(SMIE_ASSOC, [character(len=1) :: "+"])
        call precs(1)%append(SMIE_ASSOC, [character(len=1) :: "*"])
        call smie_bnf_to_prec2(bnf, prec2, status, precs=precs)
        if (.not. status%ok()) return
        call smie_prec2_to_grammar(prec2, pascal_grammar, status)
        has_grammar = status%ok()
    end subroutine make_grammar

    subroutine temp_buffer(text, h)
        character(len=*), intent(in) :: text
        type(handle_type), intent(out) :: h
        type(status_type) :: st

        call init_bindings()
        call get_or_make_buffer(" *smie-test*", h, st)
        call set_buffer(h, st)
        call erase_buffer(st)
        call insert(text, st)
        ! A mode using SMIE defines its comment syntax (smie-indent-fixindent
        ! normalizes it, prompting otherwise).
        call comment_start%set_local("# ")
        call make_grammar(st)
        call smie_setup(pascal_grammar, no_rules)
    end subroutine temp_buffer

    subroutine leave(h)
        type(handle_type), intent(in) :: h
        type(status_type) :: st

        call close_buffer(h, st)
    end subroutine leave

    subroutine test_precs(error)
        type(error_type), allocatable, intent(out) :: error
        type(smie_precs_type) :: precs
        type(smie_prec2_type) :: prec2
        type(smie_grammar_type) :: g
        type(status_type) :: st
        integer :: lp, rp, lt, rt

        call precs%append(SMIE_LEFT, [character(len=1) :: "+"])
        call precs%append(SMIE_LEFT, [character(len=1) :: "*"])
        call smie_precs_to_prec2(precs, prec2)
        call smie_prec2_to_grammar(prec2, g, st)
        call check(error, st%ok(), "the levels")
        if (allocated(error)) return
        call check(error, size(g%tokens), 2)
        if (allocated(error)) return
        call levels_of(g, "+", lp, rp)
        call levels_of(g, "*", lt, rt)
        ! Left associative: + > +, * > *; * binds tighter: + < *, * > +.
        call check(error, rp > lp .and. rt > lt .and. rp < lt .and. rt > lp, "precedences")
    end subroutine test_precs

    subroutine levels_of(g, token, left, right)
        type(smie_grammar_type), intent(in) :: g
        character(len=*), intent(in) :: token
        integer, intent(out) :: left, right
        integer :: k

        left = -1
        right = -1

        do k = 1, size(g%tokens)
            if (g%tokens(k)%s == token) then
                left = g%left(k)%value
                right = g%right(k)%value
            end if
        end do
    end subroutine levels_of

    subroutine test_bnf(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        type(smie_toklevels_type) :: b, e, t, el
        type(handle_type) :: h
        integer :: k
        logical :: found

        call temp_buffer("", h)

        checks: block
            call make_grammar(st)
            call check(error, st%ok(), "the grammar")
            if (allocated(error)) exit checks
            b = smie_grammar_token("begin")
            e = smie_grammar_token("end")
            ! An opener does not bind on its left, a closer on its right.
            call check(error, b%left%kind == SMIE_LEVEL_LIST .and. e%right%kind == SMIE_LEVEL_LIST, "opener and closer")
            if (allocated(error)) exit checks
            ! "begin" insts "end": begin = end.
            call check(error, b%right%kind == SMIE_LEVEL_NUMBER .and. e%left%kind == SMIE_LEVEL_NUMBER, "numbers")
            if (allocated(error)) exit checks
            call check(error, b%right%value == e%left%value, "begin = end")
            if (allocated(error)) exit checks
            t = smie_grammar_token("then")
            el = smie_grammar_token("else")
            call check(error, t%right%value == el%left%value, "then = else")
            if (allocated(error)) exit checks
            found = .false.

            do k = 1, size(pascal_grammar%openers)
                if (pascal_grammar%openers(k)%s == "begin" .and. pascal_grammar%closers(k)%s == "end") found = .true.
            end do

            call check(error, found, "(begin . end) closes")
        end block checks

        call leave(h)
    end subroutine test_bnf

    subroutine test_expression(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call temp_buffer("x := begin a := b; c end + 1", h)

        checks: block
            call goto_char(6_int64)
            call smie_forward_expression_command(1_int64, st)
            call check(error, st%ok(), "no error")
            if (allocated(error)) exit checks
            call check(error, point(), 25_int64)
            if (allocated(error)) exit checks
            ! forward_expression goes through `forward_expression_function'.
            call forward_expression(-1_int64, st)
            call check(error, point(), 6_int64)
        end block checks

        call leave(h)
    end subroutine test_expression

    subroutine test_indent(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call temp_buffer("begin"//LF//"a := b;"//LF//"c"//LF//"end"//LF, h)
        call indent_region(point_min(), point_max(), st)
        call check(error, buffer_string(), "begin"//LF//"    a := b;"//LF//"    c"//LF//"end"//LF)
        call leave(h)
    end subroutine test_indent

    subroutine test_close_block(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call temp_buffer("begin"//LF//"    a", h)

        checks: block
            call goto_char(point_max())
            call smie_close_block(st)
            call check(error, st%ok(), "no error")
            if (allocated(error)) exit checks
            call check(error, buffer_string(), "begin"//LF//"    a"//LF//"end")
        end block checks

        call leave(h)
    end subroutine test_close_block
end module test_smie_mod
