! test_values_mod.f90 -- tests for value_mod, names_mod, data_mod and fns_mod.
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

module test_values_mod
    use, intrinsic :: iso_fortran_env, only: int64
    use value_mod, only: vtag, V_NULL, V_NAME, V_ARRAY, value_type
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_mod, only: status_type, FM_WRONG_TYPE_ARGUMENT, FM_VOID_VARIABLE, &
                         FM_SETTING_CONSTANT
    use value_mod, only: cons, list, list_elt, list_length, list_tail, make_list, sym_nil, sym_t, value_type, xfixnum, &
                         xstring
    use names_mod, only: intern, intern_soft
    use data_mod, only: boundp, makunbound, set, symbol_name, &
                        symbol_value, value_eq
    use fns_mod, only: get, plist_get, plist_member, plist_put, &
                       put, is_string_greater, is_string_less, value_equal
    implicit none(type, external)
    private

    public :: collect_values_mod

contains

    !> Collect all exported unit tests.
    subroutine collect_values_mod(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("value-semantics", test_value_semantics), &
                    new_unittest("intern", test_intern), &
                    new_unittest("intern-many", test_intern_many), &
                    new_unittest("eq-and-equal", test_eq_equal), &
                    new_unittest("value-arrays", test_lists), &
                    new_unittest("symbol-values", test_symbol_values), &
                    new_unittest("binding-test-set-constant", test_set_constant), &
                    new_unittest("plist-get/odd-number-of-elements", test_plist_get_odd), &
                    new_unittest("plist-put/odd-number-of-elements", test_plist_put_odd), &
                    new_unittest("test-plist", test_plist), &
                    new_unittest("get-put", test_get_put) &
                    ]
    end subroutine collect_values_mod

    !> Copies are independent: there is no shared structure to mutate.
    subroutine test_value_semantics(error)
        type(error_type), allocatable, intent(out) :: error
        type(value_type) :: a, b, no_values(0)

        a = list(value_type(1_int64), value_type(text="x"))
        b = a
        ! Poke the copy's string atom directly ([LIST] [1] ["x"]).
        b%a(3)%s = "changed"
        call check(error, xstring(list_elt(a, 2_int64)), "x")
        if (allocated(error)) return
        call check(error, vtag(make_list(no_values)) == V_NULL, "empty list is not nil")
        if (allocated(error)) return
        call check(error, vtag(value_type(name="nil")) == V_NULL .and. (vtag(value_type()) == V_NAME &
                                                                        .or. vtag(value_type()) == V_NULL), "nil")
        if (allocated(error)) return
        call check(error, &
                   vtag(a) == V_ARRAY .and. (vtag(a) == V_ARRAY .or. vtag(a) == V_NULL) .and. (vtag(value_type()) &
                                    == V_ARRAY .or. vtag(value_type()) == V_NULL) .and. vtag(value_type()) /= V_ARRAY, "list tests")
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
        call symbol_name(value_type(name="tv-foo"), name, st)
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

        s1 = value_type(text="abc")
        s2 = value_type(text="abc")
        call check(error, value_eq(value_type(name="a"), value_type(name="a")), "symbols eq")
        if (allocated(error)) return
        call check(error, value_eq(value_type(5_int64), value_type(5_int64)), "fixnums eq")
        if (allocated(error)) return
        call check(error,.not. value_eq(s1, s2), "strings must not be eq")
        if (allocated(error)) return
        call check(error, value_equal(s1, s2), "strings equal")
        if (allocated(error)) return
        call check(error, value_equal(list(value_type(name="a"), s1), list(value_type(name="a"), s2)), "lists equal")
        if (allocated(error)) return
        call check(error,.not. value_equal(list(value_type(name="a")), list(value_type(name="a"), value_type(name="b"))), &
                   "different lengths equal")
        if (allocated(error)) return
        call check(error,.not. value_equal(value_type(1_int64), value_type(text="1")), "types differ")
    end subroutine test_eq_equal

    !> Value arrays: elements by position, tails, building (C2c: no
    !> car, cdr, nth, nthcdr, memq, member or length helpers).
    subroutine test_lists(error)
        type(error_type), allocatable, intent(out) :: error
        type(value_type) :: l, v
        logical :: ok

        l = list(value_type(name="a"), value_type(name="b"), value_type(name="c"))
        call check(error, value_eq(list_elt(l, 1_int64), value_type(name="a")), "first element")
        if (allocated(error)) return
        call check(error, value_equal(list_tail(l, 1_int64), list(value_type(name="b"), value_type(name="c"))), "tail")
        if (allocated(error)) return
        v = list_tail(list(value_type(name="a")), 1_int64)
        call check(error,.not. allocated(v%a), "tail of one element")
        if (allocated(error)) return
        call cons(value_type(name="z"), l, v, ok)
        call check(error, ok .and. list_length(v) == 4, "cons")
        if (allocated(error)) return
        call cons(value_type(name="z"), value_type(name="y"), v, ok)
        call check(error,.not. ok, "only onto an array")
        if (allocated(error)) return
        call check(error, value_eq(list_elt(l, 3_int64), value_type(name="c")), "third element")
        if (allocated(error)) return
        v = list_elt(l, 10_int64)
        call check(error,.not. allocated(v%a), "past the end")
    end subroutine test_lists

    subroutine test_symbol_values(error)
        type(error_type), allocatable, intent(out) :: error
        type(value_type) :: v
        type(status_type) :: st
        logical :: bound

        call boundp(value_type(name="tv-var"), bound, st)
        call check(error,.not. bound, "fresh symbol bound")
        if (allocated(error)) return
        call symbol_value(value_type(name="tv-var"), v, st)
        call check(error, st%code, FM_VOID_VARIABLE)
        if (allocated(error)) return
        call set(value_type(name="tv-var"), value_type(42_int64), st)
        call symbol_value(value_type(name="tv-var"), v, st)
        call check(error, st%ok() .and. xfixnum(v) == 42, "set/symbol-value")
        if (allocated(error)) return
        ! Part of data-tests.el binding-test-makunbound (no `let` yet).
        call makunbound(value_type(name="tv-var"), st)
        call boundp(value_type(name="tv-var"), bound, st)
        call check(error,.not. bound, "makunbound")
        if (allocated(error)) return
        call symbol_value(value_type(.true.), v, st)
        call check(error, value_eq(v, value_type(.true.)), "t evaluates to t")
        if (allocated(error)) return
        call symbol_value(value_type(name=":kw"), v, st)
        call check(error, value_eq(v, value_type(name=":kw")), "keyword evaluates to itself")
    end subroutine test_symbol_values

    !> data-tests.el: binding-test-set-constant-{t,nil,keyword,itself}.
    subroutine test_set_constant(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st

        call set(value_type(.true.), value_type(name="bob"), st)
        call check(error, st%code, FM_SETTING_CONSTANT)
        if (allocated(error)) return
        call set(value_type(), value_type(name="bob"), st)
        call check(error, st%code, FM_SETTING_CONSTANT)
        if (allocated(error)) return
        call set(value_type(name=":keyword"), value_type(name="bob"), st)
        call check(error, st%code, FM_SETTING_CONSTANT)
        if (allocated(error)) return
        call set(value_type(name=":keyword"), value_type(name=":keyword"), st)
        call check(error, st%ok(), "setting a keyword to itself")
        if (allocated(error)) return
        call makunbound(value_type(.true.), st)
        call check(error, st%code, FM_SETTING_CONSTANT)
    end subroutine test_set_constant

    !> fns-tests.el: plist-get/odd-number-of-elements.
    subroutine test_plist_get_odd(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, vtag(plist_get(list(value_type(name=":foo"), value_type(1_int64), value_type(name=":bar")), &
                                         value_type(name=":bar"))) == V_NULL, "degenerate plist")
    end subroutine test_plist_get_odd

    !> fns-tests.el: plist-put/odd-number-of-elements (bug#27726).
    subroutine test_plist_put_odd(error)
        type(error_type), allocatable, intent(out) :: error
        type(value_type) :: plist
        type(status_type) :: st

        plist = list(value_type(name=":foo"), value_type(1_int64), value_type(name=":bar"))
        call plist_put(plist, value_type(name=":zot"), value_type(2_int64), st)
        call check(error, st%code, FM_WRONG_TYPE_ARGUMENT)
        if (allocated(error)) return
        call check(error, st%detail, "expected array")
    end subroutine test_plist_put_odd

    !> fns-tests.el: test-plist (all four blocks, tests included).
    subroutine test_plist(error)
        type(error_type), allocatable, intent(out) :: error
        type(value_type) :: plist
        type(status_type) :: st

        plist = list(value_type(name=":a"), value_type(text="b"))
        call plist_put(plist, value_type(name=":b"), value_type(text="c"), st)
        call check(error, value_equal(plist_get(plist, value_type(name=":b")), value_type(text="c")), "block 1 get")
        if (allocated(error)) return
        call check(error, value_equal(plist_member(plist, value_type(name=":b")), &
                                      list(value_type(name=":b"), value_type(text="c"))), "block 1 member")
        if (allocated(error)) return
        ! eq never matches a fresh string: the pair is appended.
        plist = list(value_type(text="1"), value_type(text="2"), value_type(text="a"), value_type(text="b"))
        call plist_put(plist, value_type(text="a"), value_type(text="c"), st)
        call check(error, value_equal(plist, list(value_type(text="1"), value_type(text="2"), value_type(text="a"), &
                                                  value_type(text="b"), value_type(text="a"), value_type(text="c"))), &
                   "block 2 put")
        if (allocated(error)) return
        call check(error, vtag(plist_get(plist, value_type(text="a"))) == V_NULL, "block 2 get")
        if (allocated(error)) return
        call check(error, vtag(plist_member(plist, value_type(text="a"))) == V_NULL, "block 2 member")
        if (allocated(error)) return
        plist = list(value_type(text="1"), value_type(text="2"), value_type(text="a"), value_type(text="b"))
        call plist_put(plist, value_type(text="a"), value_type(text="c"), st, value_equal)
        call check(error, value_equal(plist, list(value_type(text="1"), value_type(text="2"), value_type(text="a"), &
                                                  value_type(text="c"))), "block 3 put")
        if (allocated(error)) return
        call check(error, value_equal(plist_get(plist, value_type(text="a"), value_equal), value_type(text="c")), &
                   "block 3 get")
        if (allocated(error)) return
        call check(error, value_equal(plist_member(plist, value_type(text="a"), value_equal), &
                                      list(value_type(text="a"), value_type(text="c"))), "block 3 member")
        if (allocated(error)) return
        plist = list(value_type(name=":a"), value_type(1_int64), value_type(name=":b"), value_type(2_int64), &
                     value_type(name=":c"), value_type(3_int64))
        call plist_put(plist, value_type(text=":a"), value_type(4_int64), st, is_string_greater)
        call check(error, value_equal(plist, list(value_type(name=":a"), value_type(1_int64), value_type(name=":b"), &
                                                  value_type(4_int64), value_type(name=":c"), value_type(3_int64))), &
                   "block 4 put")
        if (allocated(error)) return
        call check(error, value_equal(plist_get(plist, value_type(text=":b"), is_string_greater), &
                                      value_type(3_int64)), "block 4 get")
        if (allocated(error)) return
        call check(error, value_equal(plist_member(plist, value_type(text=":c"), is_string_less), plist), &
                   "block 4 member")
        if (allocated(error)) return
        call check(error, vtag(plist_get(plist, value_type(text=":a"), is_string_less)) == V_NULL .and. &
                   vtag(plist_member(plist, value_type(text=":a"), is_string_less)) == V_NULL .and. &
                   vtag(plist_get(plist, value_type(text=":c"), is_string_greater)) == V_NULL .and. &
                   vtag(plist_member(plist, value_type(text=":c"), is_string_greater)) == V_NULL, "block 4 misses")
    end subroutine test_plist

    subroutine test_get_put(error)
        type(error_type), allocatable, intent(out) :: error
        type(value_type) :: v
        type(status_type) :: st

        call put(value_type(name="tv-cmd"), value_type(name="disabled"), value_type(.true.), st)
        call get(value_type(name="tv-cmd"), value_type(name="disabled"), v, st)
        call check(error, value_eq(v, value_type(.true.)), "get after put")
        if (allocated(error)) return
        call get(value_type(name="tv-cmd"), value_type(name="other"), v, st)
        call check(error,.not. allocated(v%a), "missing property")
        if (allocated(error)) return
        call put(value_type(1_int64), value_type(name="p"), value_type(), st)
        call check(error, st%code, FM_WRONG_TYPE_ARGUMENT)
    end subroutine test_get_put
end module test_values_mod
