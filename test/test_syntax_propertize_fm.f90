! test_syntax_propertize_fm.f90 -- tests for syntax-propertize and
! syntax-ppss.
! Ports GNU Emacs 31.1 test/lisp/emacs-lisp/syntax-tests.el.
!
! Copyright (C) 2021-2026 Free Software Foundation, Inc.
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

!> Ports test/lisp/emacs-lisp/syntax-tests.el (its one test), plus tests
!> of `regexp_opt_depth', `is_subregexp_context', rules applying
!> `syntax_table' properties that the scanner and the regexp matcher
!> obey, the `syntax_ppss' cache and `syntax_propertize_wholelines'.
module test_syntax_propertize_fm
    use, intrinsic :: iso_fortran_env, only: int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_fm, only: status_type
    use handle_fm, only: handle_type
    use lisp_fm, only: value_type, vtag, V_ARRAY
    use buffer_fm, only: get_or_make_buffer, set_buffer, close_buffer, erase_buffer
    use insdel_fm, only: insert
    use editfns_fm, only: point, point_min, point_max, goto_char
    use search_fm, only: re_search_forward, SEARCH_NOERROR
    use textprop_fm, only: get_text_property
    use bindings_fm, only: init_bindings
    use syntax_fm, only: parse_state_type, parse_partial_expression, syntax_ppss, syntax_propertize_done, &
                         parse_expression_lookup_properties, forward_comment
    use subr_fm, only: is_subregexp_context
    use regexp_opt_fm, only: regexp_opt_depth
    use syntax_propertize_fm, only: syntax_propertize_shift_groups_and_backrefs, syntax_propertize_function, &
                                    syntax_propertize_rules_type, syntax_propertize_rule_type, syntax_highlight_type, &
                                    syntax_propertize_wholelines, syntax_ppss_max_span, syntax_ppss_context, &
                                    SYNTAX_PPSS_COMMENT, SYNTAX_PPSS_NONE
    implicit none(type, external)
    private

    public :: collect_syntax_propertize_fm

    character(len=*), parameter :: LF = achar(10)

    type(syntax_propertize_rules_type) :: comment_rules

contains

    !> Collect all exported unit tests.
    subroutine collect_syntax_propertize_fm(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("syntax-propertize--shift-groups-and-backrefs", test_shift), &
                    new_unittest("regexp-opt-depth", test_depth), &
                    new_unittest("subregexp-context-p", test_context), &
                    new_unittest("syntax-propertize-rules", test_rules), &
                    new_unittest("syntax-ppss-cache", test_cache), &
                    new_unittest("syntax-propertize-wholelines", test_wholelines) &
                    ]
    end subroutine collect_syntax_propertize_fm

    subroutine temp_buffer(text, h)
        character(len=*), intent(in) :: text
        type(handle_type), intent(out) :: h
        type(status_type) :: st

        call init_bindings()
        call get_or_make_buffer(" *syntax-test*", h, st)
        call set_buffer(h, st)
        call erase_buffer(st)
        call insert(text, st)
    end subroutine temp_buffer

    subroutine leave(h)
        type(handle_type), intent(in) :: h
        type(status_type) :: st

        call close_buffer(h, st)
    end subroutine leave

    !> Test shifting of numbered groups and back-references in regexps.
    subroutine test_shift(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        character(len=*), parameter :: rx_with_class = "\(a\)[\1-2]", rx_with_rep = "\(a\)\{1,\1\}"
        character(len=:), allocatable :: s

        ! A numbered group must be shifted.
        call check(error, syntax_propertize_shift_groups_and_backrefs("\(?2:[abc]+\)foobar", 2, st), &
                   "\(?4:[abc]+\)foobar")
        if (allocated(error)) return
        ! A back-reference \1 on a normal sub-regexp context must be
        ! shifted.
        call check(error, syntax_propertize_shift_groups_and_backrefs("\(a\)\1", 2, st), "\(a\)\3")
        if (allocated(error)) return
        ! Shifting must not happen if the \1 appears in a character class,
        ! or in a \{\} repetition construct (although \1 isn't valid there
        ! anyway).
        call check(error, syntax_propertize_shift_groups_and_backrefs(rx_with_class, 2, st), rx_with_class)
        if (allocated(error)) return
        call check(error, syntax_propertize_shift_groups_and_backrefs(rx_with_rep, 2, st), rx_with_rep)
        if (allocated(error)) return
        ! Now numbered groups and back-references in combination.
        call check(error, syntax_propertize_shift_groups_and_backrefs("\(?2:[abc]+\)foo\(\2\)", 2, st), &
                   "\(?4:[abc]+\)foo\(\4\)")
        if (allocated(error)) return
        ! Emacs supports only the back-references \1,...,\9, so when a
        ! shift would result in \10 or more, an error must be signaled.
        s = syntax_propertize_shift_groups_and_backrefs("\(a\)\3", 7, st)
        call check(error,.not. st%ok(), "a shift past \9 is an error")
    end subroutine test_shift

    subroutine test_depth(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st

        call check(error, regexp_opt_depth("abc", st), 0)
        if (allocated(error)) return
        call check(error, regexp_opt_depth("\(a\)\(b\(c\)\)", st), 3)
        if (allocated(error)) return
        ! Shy and explicitly numbered groups are not counted.
        call check(error, regexp_opt_depth("\(a\)\(?:b\)\(?3:c\)", st), 1)
        if (allocated(error)) return
        ! Nor is \( in brackets.
        call check(error, regexp_opt_depth("[\(]\(a\)", st), 1)
        if (allocated(error)) return
        ! Unbalanced parentheses are an error.
        call check(error, regexp_opt_depth("\(a", st) == 0 .and. .not. st%ok(), "unbalanced")
    end subroutine test_depth

    subroutine test_context(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st

        call check(error, is_subregexp_context("a\(b", 2_int64, st), "after a")
        if (allocated(error)) return
        call check(error,.not. is_subregexp_context("a[b]", 3_int64, st), "in brackets")
        if (allocated(error)) return
        call check(error,.not. is_subregexp_context("a\{1,2\}", 5_int64, st), "in \{\}")
        if (allocated(error)) return
        call check(error,.not. is_subregexp_context("a\b", 3_int64, st), "after \")
        if (allocated(error)) return
        call check(error, is_subregexp_context("[a]b", 4_int64, st, 1_int64), "after brackets")
    end subroutine test_context

    ! `#' starts a comment and newline ends it, by properties only.
    subroutine comment_propertize(start, end, status)
        integer(int64), intent(in) :: start, end
        type(status_type), intent(out) :: status

        call comment_rules%apply(start, end, status)
    end subroutine comment_propertize

    subroutine setup_comment_rules()
        type(syntax_highlight_type) :: hl

        if (allocated(comment_rules%rules)) return
        allocate (comment_rules%rules(2))
        hl%group = 0
        hl%syntax = "<"
        comment_rules%rules(1)%regexp = "#"
        comment_rules%rules(1)%highlights = [hl]
        hl%group = 1
        hl%syntax = ">"
        comment_rules%rules(2)%regexp = "\("//LF//"\)"
        comment_rules%rules(2)%highlights = [hl]
    end subroutine setup_comment_rules

    subroutine test_rules(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(parse_state_type) :: state
        type(value_type) :: prop
        type(status_type) :: st
        integer(int64) :: found

        call temp_buffer("a # b"//LF//"c # d"//LF//"e", h)
        call setup_comment_rules()
        call syntax_propertize_function%set_local(comment_propertize)

        checks: block
            call syntax_ppss(state, 6_int64)
            call check(error, syntax_ppss_context(state), SYNTAX_PPSS_COMMENT)
            if (allocated(error)) exit checks
            call check(error, state%comstr_start, 3_int64)
            if (allocated(error)) exit checks
            call check(error, parse_expression_lookup_properties%value, "lookup turned on")
            if (allocated(error)) exit checks
            call check(error, syntax_propertize_done%value >= 6, "propertized that far")
            if (allocated(error)) exit checks
            call get_text_property(3_int64, value_type(name="syntax_table"), prop, st)
            call check(error, vtag(prop) == V_ARRAY, "a raw descriptor on #")
            if (allocated(error)) exit checks
            call syntax_ppss(state, 7_int64)
            call check(error, syntax_ppss_context(state), SYNTAX_PPSS_NONE)
            if (allocated(error)) exit checks
            call syntax_ppss(state, 10_int64)
            call check(error, syntax_ppss_context(state), SYNTAX_PPSS_COMMENT)
            if (allocated(error)) exit checks
            ! forward_comment skips the comment by its properties.
            call goto_char(3_int64)
            call check(error, forward_comment(1_int64), "a comment at #")
            if (allocated(error)) exit checks
            call check(error, point(), 7_int64)
            if (allocated(error)) exit checks
            ! \s< sees the property.
            call goto_char(point_min())
            call re_search_forward("\s<", found, st, noerror=SEARCH_NOERROR)
            call check(error, found, 4_int64)
        end block checks

        call leave(h)
    end subroutine test_rules

    subroutine test_cache(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(parse_state_type) :: cached, direct
        type(status_type) :: st
        integer(int64) :: saved, positions(6), i
        character(len=:), allocatable :: text

        text = ""

        do i = 1, 20
            text = text//"((a) (b"//LF
        end do

        call temp_buffer(text, h)
        saved = syntax_ppss_max_span
        syntax_ppss_max_span = 10
        positions = [150_int64, 40_int64, 120_int64, 5_int64, 161_int64, 77_int64]

        checks: block
            do i = 1, size(positions)
                call syntax_ppss(cached, positions(i))
                call parse_partial_expression(point_min(), positions(i), direct, st)
                call check(error, cached%depth, direct%depth)
                if (allocated(error)) exit checks
                call check(error, cached%innermost_start, direct%innermost_start)
                if (allocated(error)) exit checks
            end do

            ! A change flushes the cache.
            call goto_char(point_min())
            call insert("(", st)

            do i = 1, size(positions)
                call syntax_ppss(cached, positions(i))
                call parse_partial_expression(point_min(), positions(i), direct, st)
                call check(error, cached%depth, direct%depth)
                if (allocated(error)) exit checks
            end do
        end block checks

        syntax_ppss_max_span = saved
        call leave(h)
    end subroutine test_cache

    subroutine test_wholelines(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        integer(int64) :: b, e

        call temp_buffer("abc"//LF//"def"//LF//"ghi", h)

        checks: block
            call syntax_propertize_wholelines(6_int64, 6_int64, b, e, st)
            call check(error, b, 5_int64)
            if (allocated(error)) exit checks
            call check(error, e, 9_int64)
            if (allocated(error)) exit checks
            call syntax_propertize_wholelines(5_int64, 9_int64, b, e, st)
            call check(error, b == 5 .and. e == 9, "whole lines stay")
            if (allocated(error)) exit checks
            call check(error, point_max(), 12_int64)
        end block checks

        call leave(h)
    end subroutine test_wholelines
end module test_syntax_propertize_fm
