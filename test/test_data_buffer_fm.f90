! test_data_buffer_fm.f90 -- tests for buffer-local variables (data_fm's
! submodule data_buffer_fm).
! Ports GNU Emacs 31.1 test/src/data-tests.el in part; the `let' parts
! of the binding tests need dynamic binding.
!
! Copyright (C) 2008-2026 Free Software Foundation, Inc.
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

!> Buffers are global state; every test creates its buffers and kills
!> them before returning.  The `let` parts of the upstream binding tests
!> are left out (no dynamic binding yet).
module test_data_buffer_fm
    use, intrinsic :: iso_fortran_env, only: int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_fm, only: status_type, FM_WRONG_TYPE_ARGUMENT, FM_VOID_VARIABLE, FM_VOID_FUNCTION
    use handle_fm, only: handle_type
    use lisp_fm, only: value_type, nil, nilp, make_fixnum, make_string, t_value, xfixnum, xstring, &
                       list_length, list_elt
    use lread_fm, only: sym
    use buffer_fm, only: get_buffer_create, kill_buffer, set_buffer, buffers, cur, fill_column, &
                         auto_fill_function
    use search_fm, only: case_fold_search
    use indent_fm, only: indent_tabs_mode
    use casefiddle_fm, only: case_symbols_as_words
    use data_fm, only: symbol_value, set, boundp, makunbound, value_eq, make_local_variable, &
                       make_variable_buffer_local, kill_local_variable, kill_all_local_variables, &
                       local_variable_p, local_variable_if_set_p, default_value, set_default, &
                       default_boundp, buffer_local_value, buffer_local_variables, setq_local
    use fns_fm, only: put
    implicit none(type, external)
    private

    public :: collect_data_buffer_fm

contains

    !> Collect all exported unit tests.
    subroutine collect_data_buffer_fm(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("binding-test-manual", test_manual), &
                    new_unittest("binding-test-setq-default", test_setq_default), &
                    new_unittest("binding-test-makunbound", test_makunbound), &
                    new_unittest("make-variable-buffer-local", test_always_local), &
                    new_unittest("forwarded-per-buffer", test_forwarded), &
                    new_unittest("forwarded-procedure", test_forwarded_procedure), &
                    new_unittest("case-symbols-as-words-local", test_case_symbols_local), &
                    new_unittest("kill-all-local-variables", test_kill_all), &
                    new_unittest("buffer-local-variables", test_buffer_local_variables), &
                    new_unittest("kill-buffer-restores-defaults", test_kill_current), &
                    new_unittest("forwarded-exact-values", test_exact), &
                    new_unittest("makunbound-forwarded", test_makunbound_forwarded) &
                    ]
    end subroutine collect_data_buffer_fm

    !> Make buffers A and B (A current) with the binding-test setup of
    !> data-tests.el.
    subroutine setup(error, a, b)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type), intent(out) :: a, b
        type(status_type) :: st

        call get_buffer_create("binding-test-A", a, st)
        call get_buffer_create("binding-test-B", b, st)
        call set(sym("binding-test-always-local"), sym("always"), st)
        call make_variable_buffer_local(sym("binding-test-always-local"), st)
        call check(error, st%ok(), "make_variable_buffer_local")
        if (allocated(error)) return
        call set(sym("binding-test-some-local"), sym("some"), st)
        call set_buffer(a, st)
        call setq_local("binding-test-some-local", sym("local"), st)
        call check(error, st%ok(), "setq_local")
    end subroutine setup

    subroutine teardown(a, b)
        type(handle_type), intent(in) :: a, b
        type(status_type) :: st

        call kill_buffer(a, st)
        call kill_buffer(b, st)
    end subroutine teardown

    !> The value of variable NAME is the symbol EXPECTED.
    subroutine check_value(error, name, expected)
        type(error_type), allocatable, intent(inout) :: error
        character(len=*), intent(in) :: name, expected
        type(value_type) :: v
        type(status_type) :: st

        if (allocated(error)) return
        call symbol_value(sym(name), v, st)
        call check(error, st%ok(), name//" is void")
        if (allocated(error)) return
        call check(error, value_eq(v, sym(expected)), name//" should be "//expected)
    end subroutine check_value

    !> data-tests.el: binding-test-manual (without the `let`).
    subroutine test_manual(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: a, b
        type(status_type) :: st

        call setup(error, a, b)
        if (allocated(error)) return
        call check_value(error, "binding-test-some-local", "local")
        call set_buffer(b, st)
        call check_value(error, "binding-test-some-local", "some")
        call set_buffer(a, st)
        call check_value(error, "binding-test-some-local", "local")
        call teardown(a, b)
    end subroutine test_manual

    !> data-tests.el: binding-test-setq-default (a `setq-default' has no
    !> effect where there is a local binding; no `let`).
    subroutine test_setq_default(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: a, b
        type(status_type) :: st
        type(value_type) :: v

        call setup(error, a, b)
        if (allocated(error)) return
        call set_default(sym("binding-test-some-local"), sym("new-default"), st)
        call check_value(error, "binding-test-some-local", "local")
        call default_value(sym("binding-test-some-local"), v, st)
        if (.not. allocated(error)) call check(error, value_eq(v, sym("new-default")), "default-value")
        call set_buffer(b, st)
        call check_value(error, "binding-test-some-local", "new-default")
        call set_default(sym("binding-test-some-local"), sym("some"), st)
        call teardown(a, b)
    end subroutine test_setq_default

    !> data-tests.el: binding-test-makunbound (a local binding made void;
    !> no `let`).
    subroutine test_makunbound(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: a, b
        type(status_type) :: st
        logical :: bound

        call setup(error, a, b)
        if (allocated(error)) return
        call makunbound(sym("binding-test-some-local"), st)
        call boundp(sym("binding-test-some-local"), bound, st)
        call check(error,.not. bound, "void locally")
        if (allocated(error)) return
        call check(error, default_boundp(sym("binding-test-some-local")), "default still bound")
        if (allocated(error)) return
        call set_buffer(b, st)
        call check_value(error, "binding-test-some-local", "some")
        call set_buffer(a, st)
        call boundp(sym("binding-test-some-local"), bound, st)
        if (.not. allocated(error)) call check(error,.not. bound, "void again in A")
        call teardown(a, b)
    end subroutine test_makunbound

    !> make-variable-buffer-local: setting makes the variable local.
    subroutine test_always_local(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: a, b
        type(status_type) :: st
        type(value_type) :: v

        call setup(error, a, b)
        if (allocated(error)) return
        call check(error, local_variable_if_set_p(sym("binding-test-always-local")), "local if set")
        if (allocated(error)) return
        call check(error,.not. local_variable_p(sym("binding-test-always-local")), "not yet local")
        if (allocated(error)) return
        call set(sym("binding-test-always-local"), sym("in-a"), st)
        call check(error, local_variable_p(sym("binding-test-always-local")), "local after set")
        if (allocated(error)) return
        call set_buffer(b, st)
        call check_value(error, "binding-test-always-local", "always")
        call buffer_local_value(sym("binding-test-always-local"), a%index, v, st)
        if (.not. allocated(error)) call check(error, value_eq(v, sym("in-a")), "buffer-local-value")
        call set_buffer(a, st)
        call kill_local_variable(sym("binding-test-always-local"), st)
        call check_value(error, "binding-test-always-local", "always")
        call teardown(a, b)
    end subroutine test_always_local

    !> A variable forwarded to a Fortran setting: fill-column is local
    !> when set, and the Fortran variable follows the current buffer.
    subroutine test_forwarded(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: a, b
        type(status_type) :: st
        type(value_type) :: v
        integer(int64) :: saved

        call setup(error, a, b)
        if (allocated(error)) return
        saved = fill_column%value
        call set(sym("fill_column"), make_fixnum(50_int64), st)
        call check(error, fill_column%value == 50, "fill_column set")
        if (allocated(error)) return
        call check(error, local_variable_p(sym("fill_column")), "fill-column local after set")
        if (allocated(error)) return
        call set_buffer(b, st)
        call check(error, fill_column%value == saved, "default in B")
        if (allocated(error)) return
        call buffer_local_value(sym("fill_column"), a%index, v, st)
        call check(error, xfixnum(v) == 50, "buffer-local-value of A")
        if (allocated(error)) return
        call set(sym("fill_column"), make_string("x"), st)
        call check(error, st%code == FM_WRONG_TYPE_ARGUMENT, "wrong_type_argument")
        if (allocated(error)) return
        call set_buffer(a, st)
        call check(error, fill_column%value == 50, "local again in A")
        if (allocated(error)) return
        call set_default(sym("fill_column"), make_fixnum(60_int64), st)
        call check(error, fill_column%value == 50, "set-default keeps the local value")
        if (allocated(error)) return
        call kill_local_variable(sym("fill_column"), st)
        call check(error, fill_column%value == 60, "default after kill-local-variable")
        fill_column = saved
        call teardown(a, b)
    end subroutine test_forwarded

    !> case-symbols-as-words is a DEFVAR_BOOL made automatically
    !> buffer-local (casefiddle.c syms_of_casefiddle).
    subroutine test_case_symbols_local(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: a, b
        type(status_type) :: st
        type(value_type) :: v

        call setup(error, a, b)
        if (allocated(error)) return
        call set(sym("case_symbols_as_words"), t_value(), st)
        call check(error, st%ok() .and. case_symbols_as_words%value, "set in A")
        if (allocated(error)) return
        call check(error, local_variable_p(sym("case_symbols_as_words")), "local after set")
        if (allocated(error)) return
        call set_buffer(b, st)
        call check(error,.not. case_symbols_as_words%value, "default in B")
        if (allocated(error)) return
        call buffer_local_value(sym("case_symbols_as_words"), a%index, v, st)
        call check(error, value_eq(v, t_value()), "buffer-local-value of A")
        if (allocated(error)) return
        call set_buffer(a, st)
        call kill_local_variable(sym("case_symbols_as_words"), st)
        call check(error,.not. case_symbols_as_words%value, "default after kill-local-variable")
        call teardown(a, b)
    end subroutine test_case_symbols_local

    !> auto-fill-function, a procedure pointer, follows the buffer.
    subroutine test_forwarded_procedure(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: a, b
        type(status_type) :: st
        type(value_type) :: v

        call setup(error, a, b)
        if (allocated(error)) return
        call set(sym("auto_fill_function"), sym("do_auto_fill"), st)
        call check(error, associated(auto_fill_function%value), "auto-fill on in A")
        if (allocated(error)) return
        call set_buffer(b, st)
        call check(error,.not. associated(auto_fill_function%value), "off in B")
        if (allocated(error)) return
        call buffer_local_value(sym("auto_fill_function"), a%index, v, st)
        call check(error, value_eq(v, sym("do_auto_fill")), "named in A")
        if (allocated(error)) return
        call set_buffer(a, st)
        call check(error, associated(auto_fill_function%value), "on again in A")
        if (allocated(error)) return
        ! Any value is kept; calling it then fails as upstream.
        call set(sym("auto_fill_function"), sym("no-such-function"), st)
        call symbol_value(sym("auto_fill_function"), v, st)
        call check(error, value_eq(v, sym("no-such-function")), "kept exactly")
        if (allocated(error)) return
        call auto_fill_function%value(st)
        call check(error, st%code == FM_VOID_FUNCTION, "void-function when called")
        call teardown(a, b)
        if (allocated(error)) return
        call check(error,.not. associated(auto_fill_function%value), "off after killing A")
    end subroutine test_forwarded_procedure

    !> kill-all-local-variables keeps permanent-local variables and
    !> resets the major mode.
    subroutine test_kill_all(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: a, b
        type(status_type) :: st
        type(value_type) :: v

        call setup(error, a, b)
        if (allocated(error)) return
        call setq_local("binding-test-permanent", sym("kept"), st)
        call put(sym("binding-test-permanent"), sym("permanent_local"), t_value(), st)
        call set(sym("major_mode"), sym("text_mode"), st)
        call check(error, buffers(cur)%major_mode == "text_mode", "major-mode slot")
        if (allocated(error)) return
        call kill_all_local_variables()
        call check_value(error, "binding-test-some-local", "some")
        call check_value(error, "binding-test-permanent", "kept")
        call check_value(error, "major_mode", "fundamental_mode")
        call symbol_value(sym("mode_name"), v, st)
        if (.not. allocated(error)) call check(error, xstring(v) == "Fundamental", "mode_name")
        if (allocated(error)) return
        call kill_all_local_variables(.true.)
        call check(error,.not. local_variable_p(sym("binding-test-permanent")), "permanent killed too")
        call put(sym("binding-test-permanent"), sym("permanent_local"), nil(), st)
        call teardown(a, b)
    end subroutine test_kill_all

    !> buffer-local-variables lists the locals and the slots.
    subroutine test_buffer_local_variables(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: a, b
        type(value_type) :: table, elt

        call setup(error, a, b)
        if (allocated(error)) return
        table = buffer_local_variables()
        ! binding-test-some-local plus buffer-read-only, major-mode, mode-name.
        call check(error, list_length(table) == 4, "four entries")
        if (allocated(error)) return
        elt = list_elt(table, 1_int64)
        call check(error, value_eq(list_elt(elt, 1_int64), sym("binding-test-some-local")) .and. &
                   value_eq(list_elt(elt, 2_int64), sym("local")), "first entry")
        if (allocated(error)) return
        table = buffer_local_variables(b%index)
        call check(error, list_length(table) == 3, "only the slots in B")
        call teardown(a, b)
    end subroutine test_buffer_local_variables

    !> Killing the current buffer brings back the defaults.
    subroutine test_kill_current(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: a, b
        type(status_type) :: st
        type(value_type) :: v

        call setup(error, a, b)
        if (allocated(error)) return
        call kill_buffer(a, st)
        call check_value(error, "binding-test-some-local", "some")
        call symbol_value(sym("binding-test-nothing"), v, st)
        if (.not. allocated(error)) call check(error, st%code == FM_VOID_VARIABLE, "void")
        call teardown(a, b)
    end subroutine test_kill_current

    !> Settings keep any value set by name, as upstream's variables do;
    !> a Fortran assignment replaces it.
    subroutine test_exact(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        type(value_type) :: v
        logical :: saved

        saved = case_fold_search%value
        call set_default(sym("case_fold_search"), sym("data-test"), st)
        call symbol_value(sym("case_fold_search"), v, st)
        call check(error, value_eq(v, sym("data-test")) .and. case_fold_search%value, "symbol kept, true in Fortran")
        if (allocated(error)) return
        case_fold_search = saved
        call symbol_value(sym("case_fold_search"), v, st)
        call check(error, value_eq(v, t_value()) .eqv. saved, "Fortran assignment wins")
        if (allocated(error)) return
        call set_default(sym("fill_column"), nil(), st)
        call check(error, st%ok(), "fill-column may be nil")
        if (allocated(error)) return
        call symbol_value(sym("fill_column"), v, st)
        call check(error, nilp(v), "nil kept")
        if (allocated(error)) return
        call set_default(sym("fill_column"), make_string("x"), st)
        call check(error, st%code == FM_WRONG_TYPE_ARGUMENT, "expected integer")
        fill_column = 70
    end subroutine test_exact

    !> makunbound on forwarded variables follows store_symval_forwarding.
    subroutine test_makunbound_forwarded(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: a, b
        type(status_type) :: st
        type(value_type) :: v
        logical :: bound

        call setup(error, a, b)
        if (allocated(error)) return
        call makunbound(sym("fill_column"), st)
        call check(error, st%code == FM_WRONG_TYPE_ARGUMENT, "fill-column: expected integer")
        if (allocated(error)) return
        indent_tabs_mode = .false.
        call makunbound(sym("indent_tabs_mode"), st)
        call check(error, indent_tabs_mode%value, "a DEFVAR_BOOL becomes t")
        if (allocated(error)) return
        call makunbound(sym("fill_prefix"), st)
        call boundp(sym("fill_prefix"), bound, st)
        call check(error,.not. bound, "fill-prefix void here")
        if (allocated(error)) return
        call symbol_value(sym("fill_prefix"), v, st)
        call check(error, st%code == FM_VOID_VARIABLE, "void_variable")
        if (allocated(error)) return
        call set_buffer(b, st)
        call boundp(sym("fill_prefix"), bound, st)
        call check(error, bound, "still bound in B (local void binding in A)")
        call teardown(a, b)
    end subroutine test_makunbound_forwarded
end module test_data_buffer_fm
