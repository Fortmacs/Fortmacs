! test_simple_minded_indentation_engine_mod.f90 -- tests for SIMPLE_MINDED_INDENTATION_ENGINE
! (simple_minded_indentation_engine_mod).
! Own tests; upstream has no SIMPLE_MINDED_INDENTATION_ENGINE tests.
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

!> Upstream has no SIMPLE_MINDED_INDENTATION_ENGINE tests; these are own tests of the grammar
!> conversions (the levels obey the precedences and the BACKUS_NAUR_FORM), expression
!> motion, `forward_expression' through `forward_expression_function', indentation
!> and `simple_minded_indentation_engine_close_block' with a small Pascal-like grammar.
module test_simple_minded_indentation_engine_mod
    use, intrinsic :: iso_fortran_env, only: int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_mod, only: status_type
    use handle_mod, only: handle_type
    use buffer_mod, only: get_or_make_buffer, set_buffer, close_buffer, erase_buffer
    use insert_delete_mod, only: insert
    use editing_functions_mod, only: point, point_minimum, point_maximum, goto_character, buffer_string
    use bindings_mod, only: initialize_bindings
    use expression_motion_mod, only: forward_expression
    use indent_mod, only: indent_region
    use newcomment_mod, only: comment_start
    use simple_minded_indentation_engine_mod, only: simple_minded_indentation_engine_backus_naur_form_type, &
                                                    simple_minded_indentation_engine_precs_type, &
                                                    simple_minded_indentation_engine_prec2_type, &
                                                    simple_minded_indentation_engine_grammar_type, &
                                                    simple_minded_indentation_engine_rule_result_type, &
                                                    simple_minded_indentation_engine_precs_to_prec2, &
                                                    simple_minded_indentation_engine_backus_naur_form_to_prec2, &
                                                    simple_minded_indentation_engine_prec2_to_grammar, &
                                                    simple_minded_indentation_engine_setup, &
                                                    simple_minded_indentation_engine_forward_expression_command, &
                                                    simple_minded_indentation_engine_close_block, &
                                                    simple_minded_indentation_engine_grammar_token, &
                                                    simple_minded_indentation_engine_toklevels_type, &
                                                    SIMPLE_MINDED_INDENTATION_ENGINE_LEFT, SIMPLE_MINDED_INDENTATION_ENGINE_ASSOC, &
                                                    SIMPLE_MINDED_INDENTATION_ENGINE_LEVEL_NUMBER, &
                                                    SIMPLE_MINDED_INDENTATION_ENGINE_LEVEL_LIST
    implicit none(type, external)
    private

    public :: collect_simple_minded_indentation_engine_mod

    character(len=*), parameter :: LF = achar(10)

    type(simple_minded_indentation_engine_grammar_type), target, save :: pascal_grammar
    logical, save :: has_grammar = .false.

contains

    !> Collect all exported unit tests.
    subroutine collect_simple_minded_indentation_engine_mod(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("smie-precs->prec2", test_precs), &
                    new_unittest("smie-bnf->prec2", test_backus_naur_form), &
                    new_unittest("smie-forward-sexp-command", test_expression), &
                    new_unittest("smie-indent-line", test_indent), &
                    new_unittest("smie-close-block", test_close_block) &
                    ]
    end subroutine collect_simple_minded_indentation_engine_mod

    ! The rules: no opinion (the defaults).
    subroutine no_rules(method, argument, result, status)
        integer, intent(in) :: method
        character(len=*), intent(in) :: argument
        type(simple_minded_indentation_engine_rule_result_type), intent(out) :: result
        type(status_type), intent(out) :: status

        status = status_type()
        result = simple_minded_indentation_engine_rule_result_type()
        if (method < 0 .or. len(argument) < 0) continue
    end subroutine no_rules

    subroutine make_grammar(status)
        type(status_type), intent(out) :: status
        type(simple_minded_indentation_engine_backus_naur_form_type) :: backus_naur_form
        type(simple_minded_indentation_engine_precs_type) :: precs(1)
        type(simple_minded_indentation_engine_prec2_type) :: prec2

        status = status_type()
        if (has_grammar) return
        call backus_naur_form%append("inst", [character(len=6) :: "begin", "insts", "end"])
        call backus_naur_form%append("inst", [character(len=6) :: "if", "exp", "then", "inst", "else", "inst"])
        call backus_naur_form%append("inst", [character(len=6) :: "exp", ":=", "exp"])
        call backus_naur_form%append("inst", [character(len=6) :: "exp"])
        call backus_naur_form%append("insts", [character(len=6) :: "insts", ";", "insts"])
        call backus_naur_form%append("insts", [character(len=6) :: "inst"])
        call backus_naur_form%append("exp", [character(len=6) :: "exp", "+", "exp"])
        call backus_naur_form%append("exp", [character(len=6) :: "exp", "*", "exp"])
        call precs(1)%append(SIMPLE_MINDED_INDENTATION_ENGINE_ASSOC, [character(len=1) :: ";"])
        call precs(1)%append(SIMPLE_MINDED_INDENTATION_ENGINE_ASSOC, [character(len=1) :: "+"])
        call precs(1)%append(SIMPLE_MINDED_INDENTATION_ENGINE_ASSOC, [character(len=1) :: "*"])
        call simple_minded_indentation_engine_backus_naur_form_to_prec2(backus_naur_form, prec2, status, precs=precs)
        if (.not. status%ok()) return
        call simple_minded_indentation_engine_prec2_to_grammar(prec2, pascal_grammar, status)
        has_grammar = status%ok()
    end subroutine make_grammar

    subroutine temporary_buffer(text, h)
        character(len=*), intent(in) :: text
        type(handle_type), intent(out) :: h
        type(status_type) :: st

        call initialize_bindings()
        call get_or_make_buffer(" *smie-test*", h, st)
        call set_buffer(h, st)
        call erase_buffer(st)
        call insert(text, st)
        ! A mode using SIMPLE_MINDED_INDENTATION_ENGINE defines its comment syntax (smie-indent-fixindent
        ! normalizes it, prompting otherwise).
        call comment_start%set_local("# ")
        call make_grammar(st)
        call simple_minded_indentation_engine_setup(pascal_grammar, no_rules)
    end subroutine temporary_buffer

    subroutine leave(h)
        type(handle_type), intent(in) :: h
        type(status_type) :: st

        call close_buffer(h, st)
    end subroutine leave

    subroutine test_precs(error)
        type(error_type), allocatable, intent(out) :: error
        type(simple_minded_indentation_engine_precs_type) :: precs
        type(simple_minded_indentation_engine_prec2_type) :: prec2
        type(simple_minded_indentation_engine_grammar_type) :: g
        type(status_type) :: st
        integer :: lp, rp, lt, rt

        call precs%append(SIMPLE_MINDED_INDENTATION_ENGINE_LEFT, [character(len=1) :: "+"])
        call precs%append(SIMPLE_MINDED_INDENTATION_ENGINE_LEFT, [character(len=1) :: "*"])
        call simple_minded_indentation_engine_precs_to_prec2(precs, prec2)
        call simple_minded_indentation_engine_prec2_to_grammar(prec2, g, st)
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
        type(simple_minded_indentation_engine_grammar_type), intent(in) :: g
        character(len=*), intent(in) :: token
        integer, intent(out) :: left, right
        integer :: k

        left = -1
        right = -1

        do k = 1, size(g%tokens)
            if (g%tokens(k)%text == token) then
                left = g%left(k)%value
                right = g%right(k)%value
            end if
        end do
    end subroutine levels_of

    subroutine test_backus_naur_form(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        type(simple_minded_indentation_engine_toklevels_type) :: b, e, t, element
        type(handle_type) :: h
        integer :: k
        logical :: found

        call temporary_buffer("", h)

        checks: block
            call make_grammar(st)
            call check(error, st%ok(), "the grammar")
            if (allocated(error)) exit checks
            b = simple_minded_indentation_engine_grammar_token("begin")
            e = simple_minded_indentation_engine_grammar_token("end")
            ! An opener does not bind on its left, a closer on its right.
            call check(error, &
                       b%left%kind == SIMPLE_MINDED_INDENTATION_ENGINE_LEVEL_LIST .and. e%right%kind &
                       == SIMPLE_MINDED_INDENTATION_ENGINE_LEVEL_LIST, "opener and closer")
            if (allocated(error)) exit checks
            ! "begin" insts "end": begin = end.
            call check(error, &
                       b%right%kind == SIMPLE_MINDED_INDENTATION_ENGINE_LEVEL_NUMBER .and. e%left%kind &
                       == SIMPLE_MINDED_INDENTATION_ENGINE_LEVEL_NUMBER, "numbers")
            if (allocated(error)) exit checks
            call check(error, b%right%value == e%left%value, "begin = end")
            if (allocated(error)) exit checks
            t = simple_minded_indentation_engine_grammar_token("then")
            element = simple_minded_indentation_engine_grammar_token("else")
            call check(error, t%right%value == element%left%value, "then = else")
            if (allocated(error)) exit checks
            found = .false.

            do k = 1, size(pascal_grammar%openers)
                if (pascal_grammar%openers(k)%text == "begin" .and. pascal_grammar%closers(k)%text == "end") found = .true.
            end do

            call check(error, found, "(begin . end) closes")
        end block checks

        call leave(h)
    end subroutine test_backus_naur_form

    subroutine test_expression(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call temporary_buffer("x := begin a := b; c end + 1", h)

        checks: block
            call goto_character(6_int64)
            call simple_minded_indentation_engine_forward_expression_command(1_int64, st)
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

        call temporary_buffer("begin"//LF//"a := b;"//LF//"c"//LF//"end"//LF, h)
        call indent_region(point_minimum(), point_maximum(), st)
        call check(error, buffer_string(), "begin"//LF//"    a := b;"//LF//"    c"//LF//"end"//LF)
        call leave(h)
    end subroutine test_indent

    subroutine test_close_block(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call temporary_buffer("begin"//LF//"    a", h)

        checks: block
            call goto_character(point_maximum())
            call simple_minded_indentation_engine_close_block(st)
            call check(error, st%ok(), "no error")
            if (allocated(error)) exit checks
            call check(error, buffer_string(), "begin"//LF//"    a"//LF//"end")
        end block checks

        call leave(h)
    end subroutine test_close_block
end module test_simple_minded_indentation_engine_mod
