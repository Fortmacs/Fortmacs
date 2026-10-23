! test_keymap_fm.f90 -- tests for keymap_fm.
! Includes cases ported from GNU Emacs 31.1 test/src/keymap-tests.el.
!
! Copyright (C) 2015-2026 Free Software Foundation, Inc.
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

module test_keymap_fm
    use, intrinsic :: iso_fortran_env, only: int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_fm, only: format_message
    use error_fm, only: status_type, FM_GENERIC_ERROR, FM_WRONG_TYPE_ARGUMENT
    use handle_fm, only: handle_type, null_handle
    use lisp_fm, only: fixnump, list, make_fixnum, make_keymap_ref, make_string, nil, nilp, t_value, &
                       v_list, value_type, vtag, xfixnum, xhandle, xstring
    use lread_fm, only: sym
    use data_fm, only: value_eq
    use fns_fm, only: value_equal
    use buffer_fm, only: get_buffer_create, set_buffer, kill_buffer
    use keymap_fm, only: copy_keymap, current_global_map, current_local_map, define_key, free_keymap, &
                         key_binding, key_description, key_parse, keymap_full_p, keymap_live_p, &
                         keymap_parent, keymap_prompt, keymapp, lookup_key, make_keymap, &
                         make_sparse_keymap, push_key_description, set_keymap_parent, &
                         single_key_description, use_local_map, key_valid_p, keymap_set, &
                         keymap_global_set
    use subr_fm, only: global_set_key
    implicit none(type, external)
    private

    public :: collect_keymap_fm

    integer(int64), parameter :: META = int(z'8000000', int64), &
                                 CTL = int(z'4000000', int64), &
                                 HYPER = int(z'1000000', int64)

contains

    subroutine collect_keymap_fm(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)
        testsuite = [ &
                    new_unittest("keymap-make-keymap", test_make_keymap), &
                    new_unittest("keymap-keymapp", test_keymapp), &
                    new_unittest("keymap-keymap-parent", test_keymap_parent), &
                    new_unittest("keymap-copy-keymap", test_copy_keymap), &
                    new_unittest("keymap-define-key/undefined", test_define_undefined), &
                    new_unittest("keymap-define-key/keyboard-macro", test_define_macro), &
                    new_unittest("keymap-define-key/keymap", test_define_keymap), &
                    new_unittest("keymap-lookup-key", test_lookup_key), &
                    new_unittest("keymap-lookup-key/list-of-keymaps", test_lookup_list), &
                    new_unittest("keymap-lookup-key/too-long", test_lookup_too_long), &
                    new_unittest("keymap-lookup-key/accept-default", test_accept_default), &
                    new_unittest("keymap-removal-inherit", test_removal_inherit), &
                    new_unittest("keymap-unset-test-remove-and-inheritance", test_unset_inherit), &
                    new_unittest("keymap-prefix-composition", test_prefix_composition), &
                    new_unittest("keymap-meta", test_meta), &
                    new_unittest("keymap-non-prefix-error", test_non_prefix), &
                    new_unittest("keymap-modifier-order", test_modifier_order), &
                    new_unittest("keymap-key-parse", test_key_parse), &
                    new_unittest("keymap--key-description", test_key_description), &
                    new_unittest("keymap-key-binding", test_key_binding), &
                    new_unittest("keymap-free", test_free), &
                    new_unittest("subr-test-key-valid-p", test_key_valid_p), &
                    new_unittest("keymap-set-consistency", test_keymap_set), &
                    new_unittest("keymap-global-set", test_keymap_global_set) &
                    ]
    end subroutine collect_keymap_fm

    !> kbd: the key sequence for `text`, which must parse.
    function kbd(text) result(key)
        character(len=*), intent(in) :: text
        type(value_type), allocatable :: key(:)
        type(status_type) :: st
        call key_parse(text, key, st)
        if (.not. st%ok()) allocate (key(0))
    end function kbd

    function ch(c) result(key)
        character, intent(in) :: c
        type(value_type) :: key(1)
        key(1) = make_fixnum(int(iachar(c), int64))
    end function ch

    function lookup(map, key) result(val)
        type(value_type), intent(in) :: map
        type(value_type), intent(in) :: key(:)
        type(value_type) :: val
        type(status_type) :: st
        call lookup_key(map, key, val, st)
    end function lookup

    function ref(km) result(v)
        type(handle_type), intent(in) :: km
        type(value_type) :: v
        v = make_keymap_ref(km)
    end function ref

    subroutine free(km)
        type(handle_type), intent(in) :: km
        type(status_type) :: st
        call free_keymap(km, st)
    end subroutine free

    subroutine test_make_keymap(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: full, sparse, prompted
        call make_keymap(full)
        call make_sparse_keymap(sparse)
        call make_sparse_keymap(prompted, make_string("Prompt"))
        call check(error, keymap_full_p(full) .and. .not. keymap_full_p(sparse), "full")
        if (allocated(error)) return
        call check(error, nilp(keymap_prompt(full)), "no prompt")
        if (allocated(error)) return
        call check(error, xstring(keymap_prompt(prompted)), "Prompt")
        call free(full)
        call free(sparse)
        call free(prompted)
    end subroutine test_make_keymap

    subroutine test_keymapp(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: a, b
        call make_keymap(a)
        call make_sparse_keymap(b)
        call check(error, keymapp(ref(a)) .and. keymapp(ref(b)), "keymaps")
        if (allocated(error)) return
        call check(error,.not. keymapp(list(sym("foo"), sym("bar"))), "(foo bar)")
        if (allocated(error)) return
        call check(error,.not. keymapp(nil()), "nil")
        if (allocated(error)) return
        call check(error, keymapp(list(ref(a), ref(b))), "composite")
        call free(a)
        if (allocated(error)) return
        call check(error,.not. keymapp(ref(a)), "freed keymap")
        call free(b)
    end subroutine test_keymapp

    subroutine test_keymap_parent(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: map, parent, h
        type(status_type) :: st
        call make_keymap(map)
        call make_sparse_keymap(parent)
        h = keymap_parent(map)
        call check(error, h%index == 0, "no parent")
        if (allocated(error)) return
        call set_keymap_parent(map, parent, st)
        h = keymap_parent(map)
        call check(error, st%ok() .and. h%index == parent%index, "parent")
        if (allocated(error)) return
        call set_keymap_parent(parent, map, st)
        call check(error, st%code == FM_GENERIC_ERROR .and. &
                   st%detail == "Cyclic keymap inheritance", "cycle")
        if (allocated(error)) return
        call set_keymap_parent(map, null_handle(), st)
        h = keymap_parent(map)
        call check(error, st%ok() .and. h%index == 0, "unset parent")
        call free(map)
        call free(parent)
    end subroutine test_keymap_parent

    subroutine test_copy_keymap(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: map, copy, sub, subcopy
        type(status_type) :: st
        type(value_type) :: v
        call make_keymap(map)
        call define_key(map, kbd("a"), sym("foo"), st)
        call define_key(map, kbd("C-c x"), sym("bar"), st)
        call copy_keymap(map, copy, st)
        call check(error, st%ok() .and. copy%index /= map%index, "is-not-eq")
        if (allocated(error)) return
        call check(error, value_eq(lookup(ref(copy), kbd("a")), sym("foo")) .and. &
                   value_eq(lookup(ref(copy), kbd("C-c x")), sym("bar")), "is-equal")
        if (allocated(error)) return
        ! Sub-keymaps are copied, so changing one map leaves the other alone.
        v = lookup(ref(map), kbd("C-c"))
        sub = xhandle(v)
        v = lookup(ref(copy), kbd("C-c"))
        subcopy = xhandle(v)
        call check(error, sub%index /= subcopy%index, "sub-keymap copied")
        if (allocated(error)) return
        call define_key(copy, kbd("C-c x"), sym("baz"), st)
        call check(error, value_eq(lookup(ref(map), kbd("C-c x")), sym("bar")), "independent")
        if (allocated(error)) return
        ! The copy owns its sub-keymap copy.
        call free(copy)
        call check(error,.not. keymap_live_p(subcopy) .and. keymap_live_p(sub), "owned")
        call free(map)
    end subroutine test_copy_keymap

    subroutine test_define_undefined(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: map
        type(status_type) :: st
        call make_keymap(map)
        call define_key(map, ch("a"), nil(), st)
        call check(error, st%ok() .and. nilp(lookup(ref(map), ch("a"))))
        call free(map)
    end subroutine test_define_undefined

    subroutine test_define_macro(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: map
        type(status_type) :: st
        call make_keymap(map)
        call define_key(map, ch("a"), make_string("abc"), st)
        call check(error, value_equal(lookup(ref(map), ch("a")), make_string("abc")))
        call free(map)
    end subroutine test_define_macro

    subroutine test_define_keymap(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: map, map2
        type(status_type) :: st
        call make_keymap(map)
        call make_keymap(map2)
        call define_key(map, ch("a"), ref(map2), st)
        call define_key(map2, ch("b"), sym("foo"), st)
        call check(error, value_eq(lookup(ref(map), kbd("a b")), sym("foo")))
        call free(map)
        call free(map2)
    end subroutine test_define_keymap

    subroutine test_lookup_key(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: map
        type(status_type) :: st
        type(value_type) :: v
        type(value_type), allocatable :: none(:)
        call make_keymap(map)
        call define_key(map, ch("a"), sym("foo"), st)
        call check(error, value_eq(lookup(ref(map), ch("a")), sym("foo")), "a")
        if (allocated(error)) return
        call check(error, nilp(lookup(ref(map), ch("b"))), "b")
        if (allocated(error)) return
        allocate (none(0))
        call check(error, value_eq(lookup(ref(map), none), ref(map)), "empty key")
        if (allocated(error)) return
        call lookup_key(sym("foo"), ch("a"), v, st)
        call check(error, st%code == FM_WRONG_TYPE_ARGUMENT, "not a keymap")
        call free(map)
    end subroutine test_lookup_key

    subroutine test_lookup_list(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: map1, map2
        type(status_type) :: st
        type(value_type) :: maps
        call make_keymap(map1)
        call make_keymap(map2)
        call define_key(map1, ch("a"), sym("foo"), st)
        call define_key(map2, ch("b"), sym("bar"), st)
        maps = list(ref(map1), ref(map2))
        call check(error, value_eq(lookup(maps, ch("a")), sym("foo")), "a")
        if (allocated(error)) return
        call check(error, value_eq(lookup(maps, ch("b")), sym("bar")), "b")
        if (allocated(error)) return
        call check(error, nilp(lookup(maps, ch("c"))), "c")
        call free(map1)
        call free(map2)
    end subroutine test_lookup_list

    subroutine test_lookup_too_long(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: map
        type(status_type) :: st
        type(value_type) :: v
        call make_keymap(map)
        call define_key(map, kbd("C-c f"), sym("foo"), st)
        v = lookup(ref(map), kbd("C-c f x"))
        call check(error, fixnump(v), "fixnum")
        if (allocated(error)) return
        call check(error, xfixnum(v), 2_int64)
        call free(map)
    end subroutine test_lookup_too_long

    !> Default (t) bindings apply only with accept_default.
    subroutine test_accept_default(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: map
        type(status_type) :: st
        type(value_type) :: key(1), v
        call make_sparse_keymap(map)
        key(1) = t_value()
        call define_key(map, key, sym("self_insert_command"), st)
        call define_key(map, ch("a"), sym("foo"), st)
        call check(error, nilp(lookup(ref(map), ch("b"))), "ignored")
        if (allocated(error)) return
        call lookup_key(ref(map), ch("b"), v, st, accept_default=.true.)
        call check(error, value_eq(v, sym("self_insert_command")), "default")
        if (allocated(error)) return
        call lookup_key(ref(map), ch("a"), v, st, accept_default=.true.)
        call check(error, value_eq(v, sym("foo")), "specific wins")
        if (allocated(error)) return
        ! M-b has no ESC map here, so only the default can apply.
        call lookup_key(ref(map), kbd("M-b"), v, st, accept_default=.true.)
        call check(error, value_eq(v, sym("self_insert_command")), "meta default")
        call free(map)
    end subroutine test_accept_default

    subroutine test_removal_inherit(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: parent, child
        type(status_type) :: st
        integer :: pass
        do pass = 1, 2
            call make_sparse_keymap(parent)
            call make_keymap(child)
            call set_keymap_parent(child, parent, st)
            call define_key(parent, ch("a"), sym("foo"), st)
            call define_key(child, ch("a"), sym("bar"), st)
            call check(error, value_eq(lookup(ref(child), ch("a")), sym("bar")), "bar")
            if (allocated(error)) return
            if (pass == 1) then
                ! Set to nil: shadows the parent.
                call define_key(child, ch("a"), nil(), st)
                call check(error, nilp(lookup(ref(child), ch("a"))), "nil")
            else
                ! Remove: the parent shows through.
                call define_key(child, ch("a"), nil(), st, remove=.true.)
                call check(error, value_eq(lookup(ref(child), ch("a")), sym("foo")), "foo")
            end if
            call free(child)
            call free(parent)
            if (allocated(error)) return
        end do
    end subroutine test_removal_inherit

    subroutine test_unset_inherit(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: map, parent
        type(status_type) :: st
        call make_sparse_keymap(map)
        call make_sparse_keymap(parent)
        call set_keymap_parent(map, parent, st)
        ! Removing an unset key should not add a key.
        call define_key(parent, kbd("u"), sym("undo"), st)
        call define_key(map, kbd("u"), nil(), st, remove=.true.)
        call check(error, value_eq(lookup(ref(map), kbd("u")), sym("undo")), "undo")
        if (allocated(error)) return
        ! Non-removed child bindings should shadow parent.
        call define_key(map, kbd("u"), sym("identity"), st)
        call define_key(map, kbd("u"), nil(), st)
        call check(error, nilp(lookup(ref(map), kbd("u"))), "shadowed")
        if (allocated(error)) return
        call define_key(map, kbd("u"), nil(), st, remove=.true.)
        call check(error, value_eq(lookup(ref(map), kbd("u")), sym("undo")), "from parent")
        call free(map)
        call free(parent)
    end subroutine test_unset_inherit

    !> A prefix key bound in both child and parent composes the two
    !> prefix maps, and a nil in the child's still shadows the parent's.
    subroutine test_prefix_composition(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: parent, child
        type(status_type) :: st
        type(value_type) :: v
        call make_sparse_keymap(parent)
        call make_sparse_keymap(child)
        call set_keymap_parent(child, parent, st)
        call define_key(parent, kbd("C-x C-f"), sym("find_file"), st)
        call define_key(parent, kbd("C-x C-s"), sym("save_buffer"), st)
        call define_key(child, kbd("C-x k"), sym("kill_buffer"), st)
        call check(error, value_eq(lookup(ref(child), kbd("C-x k")), sym("kill_buffer")), "child")
        if (allocated(error)) return
        call check(error, value_eq(lookup(ref(child), kbd("C-x C-f")), sym("find_file")), "parent")
        if (allocated(error)) return
        v = lookup(ref(child), kbd("C-x"))
        call check(error, vtag(v) == V_LIST .and. keymapp(v), "composite")
        if (allocated(error)) return
        call define_key(child, kbd("C-x C-s"), nil(), st)
        call check(error, nilp(lookup(ref(child), kbd("C-x C-s"))), "nil shadows")
        if (allocated(error)) return
        call check(error, nilp(lookup(ref(parent), kbd("C-x k"))), "parent unchanged")
        call free(child)
        call free(parent)
    end subroutine test_prefix_composition

    !> Meta characters live in the ESC prefix map.
    subroutine test_meta(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: map
        type(status_type) :: st
        type(value_type) :: key(2)
        call make_sparse_keymap(map)
        call define_key(map, kbd("M-x"), sym("execute_extended_command"), st)
        call check(error, value_eq(lookup(ref(map), kbd("M-x")), &
                                   sym("execute_extended_command")), "M-x")
        if (allocated(error)) return
        call check(error, value_eq(lookup(ref(map), kbd("ESC x")), &
                                   sym("execute_extended_command")), "ESC x")
        if (allocated(error)) return
        call check(error, keymapp(lookup(ref(map), kbd("ESC"))), "ESC map")
        if (allocated(error)) return
        key(1) = make_fixnum(27_int64)
        key(2) = make_fixnum(META + iachar("y"))
        call define_key(map, key, sym("foo"), st)
        call check(error, value_eq(lookup(ref(map), kbd("ESC ESC y")), sym("foo")), "ESC M-y")
        call free(map)
    end subroutine test_meta

    subroutine test_non_prefix(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: map
        type(status_type) :: st
        call make_sparse_keymap(map)
        call define_key(map, kbd("C-c"), sym("foo"), st)
        call define_key(map, kbd("C-c f"), sym("bar"), st)
        call check(error, st%code == FM_GENERIC_ERROR, "error")
        if (allocated(error)) return
        call check(error, st%detail, "Key sequence C-c f starts with non-prefix key C-c")
        if (allocated(error)) return
        call define_key(map, kbd("ESC"), sym("foo"), st)
        call define_key(map, kbd("M-a"), sym("bar"), st)
        call check(error, st%detail, "Key sequence M-a starts with non-prefix key ESC")
        call free(map)
    end subroutine test_non_prefix

    !> Symbol events with modifiers are stored in canonical order.
    subroutine test_modifier_order(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: map
        type(status_type) :: st
        type(value_type) :: key(1)
        call make_sparse_keymap(map)
        key(1) = sym("M-C-home")
        call define_key(map, key, sym("foo"), st)
        key(1) = sym("C-M-home")
        call check(error, value_eq(lookup(ref(map), key), sym("foo")), "C-M-home")
        if (allocated(error)) return
        call check(error, value_eq(lookup(ref(map), kbd("C-M-<home>")), sym("foo")), "kbd")
        call free(map)
    end subroutine test_modifier_order

    subroutine test_key_parse(error)
        type(error_type), allocatable, intent(out) :: error
        type(value_type), allocatable :: k(:)
        type(status_type) :: st
        k = kbd("C-x C-f")
        call check(error, size(k) == 2, "C-x C-f size")
        if (allocated(error)) return
        call check(error, xfixnum(k(1)) == 24 .and. xfixnum(k(2)) == 6, "C-x C-f")
        if (allocated(error)) return
        k = kbd("M-x")
        call check(error, xfixnum(k(1)), META + 120)
        if (allocated(error)) return
        k = kbd("C-M-<return>")
        call check(error, value_eq(k(1), sym("C-M-return")), "C-M-<return>")
        if (allocated(error)) return
        k = kbd("<f1> SPC")
        call check(error, value_eq(k(1), sym("f1")) .and. xfixnum(k(2)) == 32, "<f1> SPC")
        if (allocated(error)) return
        k = kbd("RET TAB ESC DEL NUL LFD")
        call check(error, all(xfixnum(k) == [13, 9, 27, 127, 0, 10]), "named")
        if (allocated(error)) return
        k = kbd("C-<RET>")
        call check(error, xfixnum(k(1)), CTL + 13)
        if (allocated(error)) return
        k = kbd("3*a")
        call check(error, size(k) == 3 .and. all(xfixnum(k) == 97), "3*a")
        if (allocated(error)) return
        k = kbd("C-%")
        call check(error, xfixnum(k(1)), CTL + 37)
        if (allocated(error)) return
        k = kbd("^C ^@")
        call check(error, xfixnum(k(1)) == 3 .and. xfixnum(k(2)) == 0, "^C")
        if (allocated(error)) return
        k = kbd("\101")
        call check(error, xfixnum(k(1)), 65_int64)
        if (allocated(error)) return
        k = kbd("abc")
        call check(error, size(k) == 3 .and. xfixnum(k(3)) == 99, "abc")
        if (allocated(error)) return
        k = kbd("M-10")
        call check(error, size(k) == 2 .and. xfixnum(k(2)) == META + 48, "M-10")
        if (allocated(error)) return
        k = kbd("C-x REM comment"//achar(10)//"x")
        call check(error, size(k) == 2 .and. xfixnum(k(2)) == 120, "REM")
        if (allocated(error)) return
        k = kbd("o"//char(195)//char(161))
        call check(error, size(k) == 2 .and. xfixnum(k(2)) == 225, "multibyte")
        if (allocated(error)) return
        k = kbd("<mouse 1>")
        call check(error, value_eq(k(1), sym("mouse 1")), "<as df>")
        if (allocated(error)) return
        call key_parse("C-xy", k, st)
        call check(error, st%detail, "C- must prefix a single character, not xy")
    end subroutine test_key_parse

    subroutine test_key_description(error)
        type(error_type), allocatable, intent(out) :: error
        type(value_type) :: k1(1), k2(2), k3(3), prefix(1)
        k1(1) = sym("right")
        prefix(1) = make_fixnum(24_int64)
        call check(error, key_description(k1, prefix), "C-x <right>")
        if (allocated(error)) return
        k1(1) = sym("M-H-right")
        call check(error, key_description(k1, prefix), "C-x M-H-<right>")
        if (allocated(error)) return
        ! Treat latin-1 correctly vs meta.  (Bug#59305)
        ! Element by element: function results in an array constructor leak.
        k3(1) = make_fixnum(111_int64)
        k3(2) = make_fixnum(108_int64)
        k3(3) = make_fixnum(225_int64)
        call check(error, key_description(k3), "o l "//char(195)//char(161))
        if (allocated(error)) return
        call check(error, single_key_description(sym("home")), "<home>")
        if (allocated(error)) return
        call check(error, single_key_description(sym("home"), .true.), "home")
        if (allocated(error)) return
        call check(error, single_key_description(sym("C-s-home")), "C-s-<home>")
        if (allocated(error)) return
        k2(1) = make_fixnum(27_int64)
        k2(2) = make_fixnum(120_int64)
        call check(error, key_description(k2), "M-x")
        if (allocated(error)) return
        k2(2) = make_fixnum(27_int64)
        call check(error, key_description(k2), "ESC ESC")
        if (allocated(error)) return
        k2(2) = sym("f1")
        call check(error, key_description(k2), "ESC <f1>")
        if (allocated(error)) return
        call check(error, push_key_description(0_int64), "C-@")
        if (allocated(error)) return
        call check(error, push_key_description(1_int64), "C-a")
        if (allocated(error)) return
        call check(error, push_key_description(META + 9), "C-M-i")
        if (allocated(error)) return
        call check(error, push_key_description(CTL + HYPER + 32), "C-H-SPC")
        if (allocated(error)) return
        call check(error, push_key_description(int(z'400000', int64)), "A-C-@")
        if (allocated(error)) return
        call check(error, key_description(kbd("C-x C-f M-q <f5> DEL")), "C-x C-f M-q <f5> DEL")
    end subroutine test_key_description

    !> Local maps take precedence over the global map.
    subroutine test_key_binding(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: global, local, buf, h
        type(status_type) :: st
        type(value_type) :: v
        global = current_global_map()
        call check(error, keymap_live_p(global) .and. keymap_full_p(global), "global")
        if (allocated(error)) return
        call define_key(global, kbd("C-x C-f"), sym("find_file"), st)
        call define_key(global, kbd("C-f"), sym("forward_char"), st)
        call get_buffer_create("*keymap-test*", buf, st)
        call set_buffer(buf, st)
        call make_sparse_keymap(local)
        call use_local_map(local, st)
        h = current_local_map()
        call check(error, st%ok() .and. h%index == local%index, "local")
        if (allocated(error)) return
        call define_key(local, kbd("C-f"), sym("local-forward"), st)
        call key_binding(kbd("C-f"), v, st)
        call check(error, value_eq(v, sym("local-forward")), "local wins")
        if (allocated(error)) return
        call key_binding(kbd("C-x C-f"), v, st)
        call check(error, value_eq(v, sym("find_file")), "global")
        if (allocated(error)) return
        call key_binding(kbd("C-f C-f"), v, st)
        call check(error, nilp(v), "too long is nil")
        if (allocated(error)) return
        call use_local_map(null_handle(), st)
        call key_binding(kbd("C-f"), v, st)
        call check(error, value_eq(v, sym("forward_char")), "no local map")
        call free(local)
        call kill_buffer(buf, st)
    end subroutine test_key_binding

    !> free_keymap frees the prefix maps define_key made.
    subroutine test_free(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: map, prefix
        type(status_type) :: st
        type(value_type) :: v
        call make_sparse_keymap(map)
        call define_key(map, kbd("C-c a"), sym("foo"), st)
        v = lookup(ref(map), kbd("C-c"))
        prefix = xhandle(v)
        call check(error, keymap_live_p(prefix), "prefix live")
        if (allocated(error)) return
        call free_keymap(map, st)
        call check(error, st%ok() .and. .not. keymap_live_p(prefix), "prefix freed")
        if (allocated(error)) return
        call free_keymap(map, st)
        call check(error, st%code == FM_WRONG_TYPE_ARGUMENT, "stale")
    end subroutine test_free

    subroutine test_key_valid_p(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), parameter :: n_tilde = char(195)//char(177), u_umlaut = char(195)//char(188)
        character(len=16), allocatable :: valid(:), invalid(:)
        integer :: k
        valid = [character(len=16) :: "f", "X", "X f", &
                 "<F2>", "<f1> <f2> TAB", "<f1> RET", "<f1> SPC", "<f1>", "<return>", &
                 "C-x", "C-x a", "C-;", "C-a", "C-c SPC", "C-c TAB", "C-c c", "C-x 4 C-f", &
                 "C-x C-f", "C-M-<down>", "C-RET", "C-SPC", "C-TAB", "C-<down>", "C-c C-c C-c", &
                 "M-a", "M-<DEL>", "C-M-a", "M-ESC", "M-RET", "M-SPC", "M-TAB", "M-x a", &
                 "M-<up>", "M-c M-c M-c", &
                 "s-SPC", "s-a", "s-x a", "s-c s-c s-c", &
                 "S-a", "S-x a", "S-c S-c S-c", &
                 "H-<RET>", "H-DEL", "H-a", "H-x a", "H-c H-c H-c", &
                 "A-H-a", "A-SPC", "A-TAB", "A-a", "A-c A-c A-c", &
                 "C-M-a", "C-M-<up>", &
                 "DEL", "ESC C-a", "ESC", "LFD", "NUL", "RET", "SPC", "TAB", &
                 n_tilde, u_umlaut, char(195)//char(182), char(196)//char(159), &
                 char(213)//char(161), "C-"//u_umlaut, "M-"//u_umlaut, "H-"//u_umlaut, &
                 "s-<return>", "C-M-<return>", "<mouse-1>", "<Scroll_Lock>"]
        invalid = [character(len=16) :: " X", "a  b", "foobar", "return", "[f1]", "< right >", &
                   "<C-M-down>", "M-C-a", "S-H-a", achar(9), "^M", achar(127), achar(0), &
                   "\177", "\000", "C-x \150", u_umlaut//u_umlaut//char(195)//char(182)//char(195)//char(182), &
                   "<s-return>", "<C-M-return>", "c-x", "C-xx", "M-xx", "M-x<TAB>"]
        call check(error,.not. key_valid_p(""), "empty")
        if (allocated(error)) return
        do k = 1, size(valid)
            call check(error, key_valid_p(trim(valid(k))), "valid: "//trim(valid(k)))
            if (allocated(error)) return
        end do
        do k = 1, size(invalid)
            ! Trimming would lose the significant spaces of " X".
            if (k == 1) then
                call check(error,.not. key_valid_p(" X"), "invalid:  X")
            else
                call check(error,.not. key_valid_p(trim(invalid(k))), "invalid: "//trim(invalid(k)))
            end if
            if (allocated(error)) return
        end do
    end subroutine test_key_valid_p

    !> A string definition is a key to type: a keyboard macro, which is a
    !> list of events here.
    subroutine test_keymap_set(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: k
        type(value_type) :: a
        type(status_type) :: st
        call make_sparse_keymap(k)
        call keymap_set(k, "a", make_string("a"), st)
        call check(error, st%ok(), "keymap_set")
        if (allocated(error)) return
        a = make_fixnum(97_int64)
        call check(error, value_equal(lookup(ref(k), kbd("a")), list(a)), "a")
        if (allocated(error)) return
        call keymap_set(k, "C-c b", "backward_char", st)
        call check(error, value_eq(lookup(ref(k), kbd("C-c b")), sym("backward_char")), "a command name")
        if (allocated(error)) return
        call keymap_set(k, "b", make_string("C-xx"), st)
        call check(error, st%detail, """C-xx"""//format_message(" is not a valid key definition; see `key_valid_p'"))
        call free(k)
    end subroutine test_keymap_set

    subroutine test_keymap_global_set(error)
        type(error_type), allocatable, intent(out) :: error
        type(value_type) :: global
        type(status_type) :: st
        global = ref(current_global_map())
        call keymap_global_set("C-c x", "forward_char", st)
        call check(error, st%ok() .and. value_eq(lookup(global, kbd("C-c x")), sym("forward_char")), &
                   "keymap_global_set")
        if (allocated(error)) return
        call keymap_global_set("C-c x", nil(), st)
        call check(error, nilp(lookup(global, kbd("C-c x"))), "unbound with nil")
        if (allocated(error)) return
        call keymap_global_set("C-xx", "forward_char", st)
        call check(error, st%detail, """C-xx"""//format_message(" is not a valid key definition; see `key_valid_p'"))
        if (allocated(error)) return
        call global_set_key(kbd("C-c y"), "backward_char", st)
        call check(error, st%ok() .and. value_eq(lookup(global, kbd("C-c y")), sym("backward_char")), &
                   "global_set_key")
        call define_key(current_global_map(), kbd("C-c y"), nil(), st, remove=.true.)
        call define_key(current_global_map(), kbd("C-c x"), nil(), st, remove=.true.)
    end subroutine test_keymap_global_set
end module test_keymap_fm
