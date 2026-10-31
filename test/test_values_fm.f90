! test_values_fm.f90 -- tests for lisp_fm, lread_fm, data_fm and fns_fm.
! Ports GNU Emacs 31.1 test/src/fns-tests.el and test/src/data-tests.el
! in part; only the cases of the ported value functions.
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

module test_values_fm
    use, intrinsic :: iso_fortran_env, only: int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_fm, only: status_type, FM_WRONG_TYPE_ARGUMENT, FM_VOID_VARIABLE, &
                        FM_SETTING_CONSTANT
    use lisp_fm, only: cons, consp, list, list_elt, list_length, listp, make_fixnum, make_list, &
                       make_string, nil, nilp, sym_nil, sym_t, symbolp, t_value, value_type, xfixnum, &
                       xstring
    use lread_fm, only: intern, intern_soft, sym
    use data_fm, only: boundp, car, car_safe, cdr, keywordp, makunbound, set, symbol_name, &
                       symbol_value, value_eq
    use fns_fm, only: get, length, member, memq, nth, nthcdr, plist_get, plist_member, plist_put, &
                      put, is_string_greater, is_string_less, value_equal
    implicit none(type, external)
    private

    public :: collect_values_fm

contains

    !> Collect all exported unit tests.
    subroutine collect_values_fm(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("value-semantics", test_value_semantics), &
                    new_unittest("intern", test_intern), &
                    new_unittest("intern-many", test_intern_many), &
                    new_unittest("eq-and-equal", test_eq_equal), &
                    new_unittest("car-cdr-cons-nth", test_lists), &
                    new_unittest("fns-tests-length", test_length), &
                    new_unittest("symbol-values", test_symbol_values), &
                    new_unittest("binding-test-set-constant", test_set_constant), &
                    new_unittest("plist-get/odd-number-of-elements", test_plist_get_odd), &
                    new_unittest("plist-put/odd-number-of-elements", test_plist_put_odd), &
                    new_unittest("test-plist", test_plist), &
                    new_unittest("get-put", test_get_put) &
                    ]
    end subroutine collect_values_fm

    !> Copies are independent: there is no shared structure to mutate.
    subroutine test_value_semantics(error)
        type(error_type), allocatable, intent(out) :: error
        type(value_type) :: a, b, no_values(0)

        a = list(make_fixnum(1_int64), make_string("x"))
        b = a
        ! Poke the copy's string atom directly ([LIST] [1] ["x"]).
        b%a(3)%s = "changed"
        call check(error, xstring(list_elt(a, 2_int64)), "x")
        if (allocated(error)) return
        call check(error, nilp(make_list(no_values)), "empty list is not nil")
        if (allocated(error)) return
        call check(error, nilp(sym("nil")) .and. symbolp(nil()), "nil")
        if (allocated(error)) return
        call check(error, consp(a) .and. listp(a) .and. listp(nil()) .and. .not. consp(nil()), "list predicates")
    end subroutine test_value_semantics

    subroutine test_intern(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        character(len=:), allocatable :: name

        call check(error, intern("nil"), SYM_NIL)
        if (allocated(error)) return
        call check(error, intern("t"), SYM_T)
        if (allocated(error)) return
        call check(error, intern("tv-foo"), intern("tv-foo"))
        if (allocated(error)) return
        call check(error, intern_soft("tv-never-interned"), 0_int64)
        if (allocated(error)) return
        call symbol_name(sym("tv-foo"), name, st)
        call check(error, name, "tv-foo")
    end subroutine test_intern

    !> Enough symbols to force several rehashes.
    subroutine test_intern_many(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=16) :: name
        integer(int64) :: ids(3000), k

        do k = 1, size(ids, kind=int64)
            write (name, '("tv-sym-", i0)') k
            ids(k) = intern(trim(name))
        end do

        do k = 1, size(ids, kind=int64)
            write (name, '("tv-sym-", i0)') k
            call check(error, intern_soft(trim(name)), ids(k))
            if (allocated(error)) return
        end do

        call check(error, ids(1) /= ids(3000), "distinct names share an id")
    end subroutine test_intern_many

    subroutine test_eq_equal(error)
        type(error_type), allocatable, intent(out) :: error
        type(value_type) :: s1, s2

        s1 = make_string("abc")
        s2 = make_string("abc")
        call check(error, value_eq(sym("a"), sym("a")), "symbols eq")
        if (allocated(error)) return
        call check(error, value_eq(make_fixnum(5_int64), make_fixnum(5_int64)), "fixnums eq")
        if (allocated(error)) return
        call check(error,.not. value_eq(s1, s2), "strings must not be eq")
        if (allocated(error)) return
        call check(error, value_equal(s1, s2), "strings equal")
        if (allocated(error)) return
        call check(error, value_equal(list(sym("a"), s1), list(sym("a"), s2)), "lists equal")
        if (allocated(error)) return
        call check(error,.not. value_equal(list(sym("a")), list(sym("a"), sym("b"))), &
                   "different lengths equal")
        if (allocated(error)) return
        call check(error,.not. value_equal(make_fixnum(1_int64), make_string("1")), "types differ")
    end subroutine test_eq_equal

    subroutine test_lists(error)
        type(error_type), allocatable, intent(out) :: error
        type(value_type) :: l, v
        type(status_type) :: st
        logical :: ok

        l = list(sym("a"), sym("b"), sym("c"))
        call car(l, v, st)
        call check(error, value_eq(v, sym("a")), "car")
        if (allocated(error)) return
        call cdr(l, v, st)
        call check(error, value_equal(v, list(sym("b"), sym("c"))), "cdr")
        if (allocated(error)) return
        call cdr(list(sym("a")), v, st)
        call check(error, nilp(v), "cdr of one-element list")
        if (allocated(error)) return
        call car(make_fixnum(1_int64), v, st)
        call check(error, st%code, FM_WRONG_TYPE_ARGUMENT)
        if (allocated(error)) return
        call check(error, nilp(car_safe(make_fixnum(1_int64))), "car-safe")
        if (allocated(error)) return
        call cons(sym("z"), l, v, ok)
        call check(error, ok .and. list_length(v) == 4, "cons")
        if (allocated(error)) return
        call cons(sym("z"), sym("y"), v, ok)
        call check(error,.not. ok, "dotted pair accepted")
        if (allocated(error)) return
        call nth(2_int64, l, v, st)
        call check(error, value_eq(v, sym("c")), "nth 2")
        if (allocated(error)) return
        call nth(9_int64, l, v, st)
        call check(error, nilp(v), "nth past end")
        if (allocated(error)) return
        call nthcdr(1_int64, l, v, st)
        call check(error, list_length(v), 2_int64)
        if (allocated(error)) return
        call check(error, list_length(memq(sym("b"), l)), 2_int64)
        if (allocated(error)) return
        call check(error, nilp(memq(make_string("b"), list(make_string("b")))), "memq string")
        if (allocated(error)) return
        call check(error, list_length(member(make_string("b"), list(make_string("b")))), 1_int64)
    end subroutine test_lists

    !> fns-tests.el: fns-tests-length (lists, strings, error on t).
    subroutine test_length(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: n

        call length(nil(), n, st)
        call check(error, n, 0_int64)
        if (allocated(error)) return
        call length(list(make_fixnum(1_int64), make_fixnum(2_int64), make_fixnum(3_int64)), n, st)
        call check(error, n, 3_int64)
        if (allocated(error)) return
        call length(make_string("foo"), n, st)
        call check(error, n, 3_int64)
        if (allocated(error)) return
        ! Characters, not bytes.
        call length(make_string("a"//char(226)//char(130)//char(172)), n, st)
        call check(error, n, 2_int64)
        if (allocated(error)) return
        call length(t_value(), n, st)
        call check(error, st%code, FM_WRONG_TYPE_ARGUMENT)
    end subroutine test_length

    subroutine test_symbol_values(error)
        type(error_type), allocatable, intent(out) :: error
        type(value_type) :: v
        type(status_type) :: st
        logical :: bound

        call boundp(sym("tv-var"), bound, st)
        call check(error,.not. bound, "fresh symbol bound")
        if (allocated(error)) return
        call symbol_value(sym("tv-var"), v, st)
        call check(error, st%code, FM_VOID_VARIABLE)
        if (allocated(error)) return
        call set(sym("tv-var"), make_fixnum(42_int64), st)
        call symbol_value(sym("tv-var"), v, st)
        call check(error, st%ok() .and. xfixnum(v) == 42, "set/symbol-value")
        if (allocated(error)) return
        ! Part of data-tests.el binding-test-makunbound (no `let` yet).
        call makunbound(sym("tv-var"), st)
        call boundp(sym("tv-var"), bound, st)
        call check(error,.not. bound, "makunbound")
        if (allocated(error)) return
        call symbol_value(t_value(), v, st)
        call check(error, value_eq(v, t_value()), "t evaluates to t")
        if (allocated(error)) return
        call symbol_value(sym(":kw"), v, st)
        call check(error, value_eq(v, sym(":kw")) .and. keywordp(v), "keyword evaluates to itself")
    end subroutine test_symbol_values

    !> data-tests.el: binding-test-set-constant-{t,nil,keyword,itself}.
    subroutine test_set_constant(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st

        call set(t_value(), sym("bob"), st)
        call check(error, st%code, FM_SETTING_CONSTANT)
        if (allocated(error)) return
        call set(nil(), sym("bob"), st)
        call check(error, st%code, FM_SETTING_CONSTANT)
        if (allocated(error)) return
        call set(sym(":keyword"), sym("bob"), st)
        call check(error, st%code, FM_SETTING_CONSTANT)
        if (allocated(error)) return
        call set(sym(":keyword"), sym(":keyword"), st)
        call check(error, st%ok(), "setting a keyword to itself")
        if (allocated(error)) return
        call makunbound(t_value(), st)
        call check(error, st%code, FM_SETTING_CONSTANT)
    end subroutine test_set_constant

    !> fns-tests.el: plist-get/odd-number-of-elements.
    subroutine test_plist_get_odd(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, nilp(plist_get(list(sym(":foo"), make_fixnum(1_int64), sym(":bar")), &
                                         sym(":bar"))), "degenerate plist")
    end subroutine test_plist_get_odd

    !> fns-tests.el: plist-put/odd-number-of-elements (bug#27726).
    subroutine test_plist_put_odd(error)
        type(error_type), allocatable, intent(out) :: error
        type(value_type) :: plist
        type(status_type) :: st

        plist = list(sym(":foo"), make_fixnum(1_int64), sym(":bar"))
        call plist_put(plist, sym(":zot"), make_fixnum(2_int64), st)
        call check(error, st%code, FM_WRONG_TYPE_ARGUMENT)
        if (allocated(error)) return
        call check(error, st%detail, "plistp")
    end subroutine test_plist_put_odd

    !> fns-tests.el: test-plist (all four blocks, predicates included).
    subroutine test_plist(error)
        type(error_type), allocatable, intent(out) :: error
        type(value_type) :: plist
        type(status_type) :: st

        plist = list(sym(":a"), make_string("b"))
        call plist_put(plist, sym(":b"), make_string("c"), st)
        call check(error, value_equal(plist_get(plist, sym(":b")), make_string("c")), "block 1 get")
        if (allocated(error)) return
        call check(error, value_equal(plist_member(plist, sym(":b")), &
                                      list(sym(":b"), make_string("c"))), "block 1 member")
        if (allocated(error)) return
        ! eq never matches a fresh string: the pair is appended.
        plist = list(make_string("1"), make_string("2"), make_string("a"), make_string("b"))
        call plist_put(plist, make_string("a"), make_string("c"), st)
        call check(error, value_equal(plist, list(make_string("1"), make_string("2"), make_string("a"), &
                                                  make_string("b"), make_string("a"), make_string("c"))), &
                   "block 2 put")
        if (allocated(error)) return
        call check(error, nilp(plist_get(plist, make_string("a"))), "block 2 get")
        if (allocated(error)) return
        call check(error, nilp(plist_member(plist, make_string("a"))), "block 2 member")
        if (allocated(error)) return
        plist = list(make_string("1"), make_string("2"), make_string("a"), make_string("b"))
        call plist_put(plist, make_string("a"), make_string("c"), st, value_equal)
        call check(error, value_equal(plist, list(make_string("1"), make_string("2"), make_string("a"), &
                                                  make_string("c"))), "block 3 put")
        if (allocated(error)) return
        call check(error, value_equal(plist_get(plist, make_string("a"), value_equal), make_string("c")), &
                   "block 3 get")
        if (allocated(error)) return
        call check(error, value_equal(plist_member(plist, make_string("a"), value_equal), &
                                      list(make_string("a"), make_string("c"))), "block 3 member")
        if (allocated(error)) return
        plist = list(sym(":a"), make_fixnum(1_int64), sym(":b"), make_fixnum(2_int64), &
                     sym(":c"), make_fixnum(3_int64))
        call plist_put(plist, make_string(":a"), make_fixnum(4_int64), st, is_string_greater)
        call check(error, value_equal(plist, list(sym(":a"), make_fixnum(1_int64), sym(":b"), &
                                                  make_fixnum(4_int64), sym(":c"), make_fixnum(3_int64))), &
                   "block 4 put")
        if (allocated(error)) return
        call check(error, value_equal(plist_get(plist, make_string(":b"), is_string_greater), &
                                      make_fixnum(3_int64)), "block 4 get")
        if (allocated(error)) return
        call check(error, value_equal(plist_member(plist, make_string(":c"), is_string_less), plist), &
                   "block 4 member")
        if (allocated(error)) return
        call check(error, nilp(plist_get(plist, make_string(":a"), is_string_less)) .and. &
                   nilp(plist_member(plist, make_string(":a"), is_string_less)) .and. &
                   nilp(plist_get(plist, make_string(":c"), is_string_greater)) .and. &
                   nilp(plist_member(plist, make_string(":c"), is_string_greater)), "block 4 misses")
    end subroutine test_plist

    subroutine test_get_put(error)
        type(error_type), allocatable, intent(out) :: error
        type(value_type) :: v
        type(status_type) :: st

        call put(sym("tv-cmd"), sym("disabled"), t_value(), st)
        call get(sym("tv-cmd"), sym("disabled"), v, st)
        call check(error, value_eq(v, t_value()), "get after put")
        if (allocated(error)) return
        call get(sym("tv-cmd"), sym("other"), v, st)
        call check(error, nilp(v), "missing property")
        if (allocated(error)) return
        call put(make_fixnum(1_int64), sym("p"), nil(), st)
        call check(error, st%code, FM_WRONG_TYPE_ARGUMENT)
    end subroutine test_get_put
end module test_values_fm
