! test_derived_fm.f90 -- tests for hooks (subr_fm, eval_fm), derived
! major modes (derived_fm), Text, Prog, Fundamental and Special modes.
! Includes cases ported from GNU Emacs 31.1 test/lisp/subr-tests.el and
! test/lisp/files-tests.el.
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

!> Buffers are global state; every test creates its buffers and kills
!> them before returning.
module test_derived_fm
    use, intrinsic :: iso_fortran_env, only: int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_fm, only: status_type
    use handle_fm, only: handle_type, null_handle
    use lisp_fm, only: value_type, nil, nilp, list, list_length, list_elt, t_value, xstring
    use lread_fm, only: sym
    use buffer_fm, only: get_buffer_create, kill_buffer, set_buffer, buffers, cur
    use data_fm, only: symbol_value, set, set_default, value_eq, setq_local, local_variable_p
    use callint_fm, only: defcommand
    use eval_fm, only: run_hooks
    use subr_fm, only: add_hook, remove_hook, provided_mode_derived_p, derived_mode_p
    use keymap_fm, only: current_local_map, keymap_parent, keymap_set, key_binding, key_parse
    use derived_fm, only: define_derived_mode, derived_mode_begin, derived_mode_end, derived_mode_map
    use syntax_fm, only: syntax_table, standard_syntax_table, char_syntax
    use text_mode_fm, only: text_mode, syms_of_text_mode, text_mode_syntax_table
    use prog_mode_fm, only: prog_mode, syms_of_prog_mode
    use simple_fm, only: fundamental_mode, special_mode, syms_of_simple_modes
    use files_fm, only: normal_mode, auto_mode_spec, file_name_sans_versions, add_auto_mode
    implicit none(type, external)
    private

    public :: collect_derived_fm

    !> The hook functions that ran, in order.
    character(len=:), allocatable :: trace

contains

    subroutine collect_derived_fm(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)
        testsuite = [ &
                    new_unittest("subr-tests-add-hook-depth", test_add_hook_depth), &
                    new_unittest("run-hooks-local-and-global", test_run_hooks_local), &
                    new_unittest("remove-hook", test_remove_hook), &
                    new_unittest("text-mode", test_text_mode), &
                    new_unittest("derived-mode-hooks-order", test_hooks_order), &
                    new_unittest("provided-mode-derived-p", test_provided_mode_derived_p), &
                    new_unittest("special-mode", test_special_mode), &
                    new_unittest("fundamental-mode-kills-locals", test_fundamental), &
                    new_unittest("auto-mode-spec", test_auto_mode_spec), &
                    new_unittest("file-name-sans-versions", test_sans_versions), &
                    new_unittest("files-tests--bug75961", test_normal_mode) &
                    ]
    end subroutine collect_derived_fm

    logical function same(a, b)
        type(handle_type), intent(in) :: a, b
        same = a%index == b%index .and. a%generation == b%generation
    end function same

    subroutine hook_a(status)
        type(status_type), intent(out) :: status
        status = status_type()
        trace = trace//"a"
    end subroutine hook_a

    subroutine hook_b(status)
        type(status_type), intent(out) :: status
        status = status_type()
        trace = trace//"b"
    end subroutine hook_b

    subroutine hook_text(status)
        type(status_type), intent(out) :: status
        status = status_type()
        trace = trace//"T"
    end subroutine hook_text

    subroutine hook_child(status)
        type(status_type), intent(out) :: status
        status = status_type()
        trace = trace//"C"
    end subroutine hook_child

    subroutine hook_after(status)
        type(status_type), intent(out) :: status
        status = status_type()
        trace = trace//"!"
    end subroutine hook_after

    !> A mode derived from text-mode.
    recursive subroutine child_mode(status)
        type(status_type), intent(out) :: status
        logical :: saved
        call derived_mode_begin("test-derived-child-mode", saved, status)
        if (.not. status%ok()) return
        trace = trace//"c"
        call derived_mode_end("test-derived-child-mode", saved, status)
    end subroutine child_mode

    !> The names in the hook value of NAME, joined by spaces.
    function hook_names(name) result(s)
        character(len=*), intent(in) :: name
        character(len=:), allocatable :: s
        type(value_type) :: v, e
        type(status_type) :: st
        integer(int64) :: i
        call symbol_value(sym(name), v, st)
        s = ""
        do i = 1, list_length(v)
            e = list_elt(v, i)
            if (value_eq(e, t_value())) then
                s = s//" t"
            else
                s = s//" "//symbol_text(e)
            end if
        end do
        s = adjustl(s)
        s = trim(s)
    end function hook_names

    function symbol_text(v) result(s)
        type(value_type), intent(in) :: v
        character(len=:), allocatable :: s
        integer :: k
        s = "?"
        do k = 1, 10
            if (value_eq(v, sym("f"//digits(k)))) s = "f"//digits(k)
        end do
    end function symbol_text

    function digits(k) result(s)
        integer, intent(in) :: k
        character(len=:), allocatable :: s
        character(len=4) :: buf
        write (buf, "(i0)") k
        s = trim(buf)
    end function digits

    !> subr-tests.el: subr-tests-add-hook-depth.
    subroutine test_add_hook_depth(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        call set_default(sym("subr-tests--hook"), nil(), st)
        call add_hook("subr-tests--hook", "f1", st)
        call add_hook("subr-tests--hook", "f2", st)
        call check(error, hook_names("subr-tests--hook"), "f2 f1")
        if (allocated(error)) return
        call add_hook("subr-tests--hook", "f3", st, depth=90)
        call check(error, hook_names("subr-tests--hook"), "f2 f1 f3")
        if (allocated(error)) return
        call add_hook("subr-tests--hook", "f4", st, depth=50)
        call check(error, hook_names("subr-tests--hook"), "f2 f1 f4 f3")
        if (allocated(error)) return
        call add_hook("subr-tests--hook", "f5", st, depth=-50)
        call check(error, hook_names("subr-tests--hook"), "f5 f2 f1 f4 f3")
        if (allocated(error)) return
        call add_hook("subr-tests--hook", "f6", st)
        call check(error, hook_names("subr-tests--hook"), "f5 f6 f2 f1 f4 f3")
        if (allocated(error)) return
        ! Make sure t is equivalent to 90.
        call add_hook("subr-tests--hook", "f7", st, depth=90)
        call add_hook("subr-tests--hook", "f8", st, depth=90)
        call check(error, hook_names("subr-tests--hook"), "f5 f6 f2 f1 f4 f3 f7 f8")
        if (allocated(error)) return
        ! Make sure nil is equivalent to 0.
        call add_hook("subr-tests--hook", "f9", st, depth=0)
        call add_hook("subr-tests--hook", "f10", st)
        call check(error, hook_names("subr-tests--hook"), "f5 f10 f9 f6 f2 f1 f4 f3 f7 f8")
    end subroutine test_add_hook_depth

    !> A local hook value with t runs the global functions there.
    subroutine test_run_hooks_local(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: a, b
        type(status_type) :: st
        call defcommand("test-derived-hook-a", hook_a)
        call defcommand("test-derived-hook-b", hook_b)
        call get_buffer_create("test-derived-hooks-A", a, st)
        call get_buffer_create("test-derived-hooks-B", b, st)
        call set_buffer(a, st)
        call set_default(sym("test-derived--hook"), nil(), st)
        call add_hook("test-derived--hook", "test-derived-hook-a", st)
        call add_hook("test-derived--hook", "test-derived-hook-b", st, local=.true.)
        call check(error, local_variable_p(sym("test-derived--hook")), "local hook")
        if (allocated(error)) return
        trace = ""
        call run_hooks(["test-derived--hook"], st)
        call check(error, st%ok(), "run-hooks")
        if (allocated(error)) return
        call check(error, trace, "ba", "local, then global")
        if (allocated(error)) return
        call set_buffer(b, st)
        trace = ""
        call run_hooks(["test-derived--hook"], st)
        call check(error, trace, "a", "global only in B")
        call kill_buffer(a, st)
        call kill_buffer(b, st)
    end subroutine test_run_hooks_local

    !> remove-hook drops a function, and a local value of just t.
    subroutine test_remove_hook(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: a
        type(status_type) :: st
        call get_buffer_create("test-derived-remove", a, st)
        call set_buffer(a, st)
        call set_default(sym("test-derived--hook2"), nil(), st)
        call add_hook("test-derived--hook2", "f1", st)
        call add_hook("test-derived--hook2", "f2", st, local=.true.)
        call remove_hook("test-derived--hook2", "f2", st, local=.true.)
        call check(error,.not. local_variable_p(sym("test-derived--hook2")), "local value killed")
        if (allocated(error)) return
        call check(error, hook_names("test-derived--hook2"), "f1")
        if (allocated(error)) return
        call remove_hook("test-derived--hook2", "f1", st)
        call check(error, hook_names("test-derived--hook2"), "")
        call kill_buffer(a, st)
    end subroutine test_remove_hook

    !> text-mode sets the mode, its keymap and syntax table.
    subroutine test_text_mode(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: a, map
        type(status_type) :: st
        type(value_type) :: v
        call syms_of_text_mode()
        call get_buffer_create("test-derived-text", a, st)
        call set_buffer(a, st)
        call text_mode(st)
        call check(error, st%ok(), "text-mode")
        if (allocated(error)) return
        call check(error, buffers(cur)%major_mode, "text-mode")
        if (allocated(error)) return
        call symbol_value(sym("mode-name"), v, st)
        call check(error, xstring(v), "Text")
        if (allocated(error)) return
        map = current_local_map()
        call check(error, same(map, derived_mode_map("text-mode")), "text-mode-map")
        if (allocated(error)) return
        call check(error, same(syntax_table(), text_mode_syntax_table), "syntax table")
        if (allocated(error)) return
        call check(error, char_syntax(iachar("'")) == "w", "' is a word constituent")
        if (allocated(error)) return
        call check(error, derived_mode_p([character(len=9) :: "text-mode"]), "derived-mode-p")
        if (allocated(error)) return
        call symbol_value(sym("text-mode-variant"), v, st)
        call check(error, value_eq(v, t_value()), "text-mode-variant")
        if (allocated(error)) return
        call fundamental_mode(st)
        call check(error, same(syntax_table(), standard_syntax_table()), "standard syntax table")
        if (allocated(error)) return
        call check(error, same(current_local_map(), null_handle()), "no local map")
        call kill_buffer(a, st)
    end subroutine test_text_mode

    !> A derived mode runs its parent's hook, then its own, then
    !> after-change-major-mode-hook, all after the bodies.
    subroutine test_hooks_order(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: a, map
        type(status_type) :: st
        type(value_type) :: binding
        type(value_type), allocatable :: key(:)
        call syms_of_text_mode()
        call define_derived_mode("test-derived-child-mode", "text-mode", "Child")
        call defcommand("test-derived-child-mode", child_mode)
        call defcommand("test-derived-hook-text", hook_text)
        call defcommand("test-derived-hook-child", hook_child)
        call defcommand("test-derived-hook-after", hook_after)
        call add_hook("text-mode-hook", "test-derived-hook-text", st)
        call add_hook("test-derived-child-mode-hook", "test-derived-hook-child", st)
        call add_hook("after-change-major-mode-hook", "test-derived-hook-after", st)
        call get_buffer_create("test-derived-order", a, st)
        call set_buffer(a, st)
        trace = ""
        call child_mode(st)
        call check(error, st%ok(), "child mode")
        if (allocated(error)) return
        call check(error, trace, "cTC!", "body, parent hook, own hook, after-change")
        if (allocated(error)) return
        call check(error, buffers(cur)%major_mode, "test-derived-child-mode")
        if (allocated(error)) return
        map = derived_mode_map("test-derived-child-mode")
        call check(error, same(keymap_parent(map), derived_mode_map("text-mode")), "keymap parent")
        if (allocated(error)) return
        ! A key bound in text-mode-map is active in the child.
        call keymap_set(derived_mode_map("text-mode"), "C-c t", "forward-char", st)
        call key_parse("C-c t", key, st)
        call key_binding(key, binding, st)
        call check(error, value_eq(binding, sym("forward-char")), "inherited binding")
        call keymap_set(derived_mode_map("text-mode"), "C-c t", nil(), st)
        call remove_hook("text-mode-hook", "test-derived-hook-text", st)
        call remove_hook("test-derived-child-mode-hook", "test-derived-hook-child", st)
        call remove_hook("after-change-major-mode-hook", "test-derived-hook-after", st)
        call kill_buffer(a, st)
    end subroutine test_hooks_order

    !> subr-tests.el: provided-mode-derived-p (without aliases).
    subroutine test_provided_mode_derived_p(error)
        type(error_type), allocatable, intent(out) :: error
        call syms_of_prog_mode()
        call define_derived_mode("subr-tests--parent-mode", "prog-mode", "P")
        call define_derived_mode("subr-tests--derived-mode-1", "prog-mode", "D1")
        call define_derived_mode("subr-tests--derived-mode-2", "subr-tests--parent-mode", "D2")
        call check(error, provided_mode_derived_p("subr-tests--derived-mode-1", [character(len=9) :: "prog-mode"]))
        if (allocated(error)) return
        call check(error, provided_mode_derived_p("subr-tests--parent-mode", &
                                                  [character(len=23) :: "subr-tests--parent-mode"]))
        if (allocated(error)) return
        call check(error, provided_mode_derived_p("subr-tests--derived-mode-2", &
                                                  [character(len=23) :: "subr-tests--parent-mode"]))
        if (allocated(error)) return
        call check(error, provided_mode_derived_p("subr-tests--derived-mode-2", [character(len=9) :: "prog-mode"]))
        if (allocated(error)) return
        call check(error, provided_mode_derived_p("prog-mode", [character(len=16) :: "fundamental-mode"]))
        if (allocated(error)) return
        call check(error,.not. provided_mode_derived_p("prog-mode", [character(len=9) :: "text-mode"]))
    end subroutine test_provided_mode_derived_p

    !> special-mode makes the buffer read-only with special-mode-map.
    subroutine test_special_mode(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: a
        type(status_type) :: st
        type(value_type) :: binding
        type(value_type), allocatable :: key(:)
        call syms_of_simple_modes()
        call get_buffer_create("test-derived-special", a, st)
        call set_buffer(a, st)
        call special_mode(st)
        call check(error, st%ok(), "special-mode")
        if (allocated(error)) return
        call check(error, buffers(cur)%read_only, "read-only")
        if (allocated(error)) return
        call key_parse("q", key, st)
        call key_binding(key, binding, st)
        call check(error, value_eq(binding, sym("quit-window")), "q quits")
        call kill_buffer(a, st)
    end subroutine test_special_mode

    !> fundamental-mode kills local variables (not permanent ones).
    subroutine test_fundamental(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: a
        type(status_type) :: st
        call syms_of_simple_modes()
        call get_buffer_create("test-derived-fundamental", a, st)
        call set_buffer(a, st)
        call setq_local("test-derived--var", t_value(), st)
        call fundamental_mode(st)
        call check(error,.not. local_variable_p(sym("test-derived--var")), "local killed")
        if (allocated(error)) return
        call check(error, buffers(cur)%major_mode, "fundamental-mode")
        call kill_buffer(a, st)
    end subroutine test_fundamental

    !> The -*- line.
    subroutine test_auto_mode_spec(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), parameter :: LF = char(10)
        call check(error, auto_mode_spec("-*- Text -*-"//LF//"body"), "text-mode")
        if (allocated(error)) return
        call check(error, auto_mode_spec(LF//"  ;; -*- mode: Prog; fill-column: 60 -*-"), "prog-mode")
        if (allocated(error)) return
        call check(error, auto_mode_spec("x -*- coding: utf-8; Mode: text -*-"), "text-mode")
        if (allocated(error)) return
        call check(error, auto_mode_spec("#!/bin/sh"//LF//"# -*- text -*-"), "text-mode")
        if (allocated(error)) return
        call check(error, auto_mode_spec("plain"//LF//"# -*- text -*-"), "")
        if (allocated(error)) return
        call check(error, auto_mode_spec("-*- coding: utf-8 -*-"), "")
        if (allocated(error)) return
        call check(error, auto_mode_spec("-*- unclosed"), "")
    end subroutine test_auto_mode_spec

    !> file-name-sans-versions.
    subroutine test_sans_versions(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, file_name_sans_versions("foo.txt~"), "foo.txt")
        if (allocated(error)) return
        call check(error, file_name_sans_versions("foo.txt.~12~"), "foo.txt")
        if (allocated(error)) return
        call check(error, file_name_sans_versions("foo.txt"), "foo.txt")
    end subroutine test_sans_versions

    !> files-tests.el: files-tests--bug75961 (the first part: normal-mode
    !> picks text-mode from auto-mode-alist and runs its hook).
    subroutine test_normal_mode(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: a
        type(status_type) :: st
        call syms_of_text_mode()
        call defcommand("test-derived-hook-text", hook_text)
        call get_buffer_create("test-derived-normal", a, st)
        call set_buffer(a, st)
        buffers(cur)%filename = "/tmp/foo.text"
        call normal_mode(st)
        call check(error, derived_mode_p([character(len=9) :: "text-mode"]), "text-mode from the file name")
        if (allocated(error)) return
        call add_hook("text-mode-hook", "test-derived-hook-text", st)
        trace = ""
        buffers(cur)%filename = "/tmp/foo.TXT"
        call normal_mode(st)
        call remove_hook("text-mode-hook", "test-derived-hook-text", st)
        call check(error, trace, "T", "hook ran (case-insensitive match)")
        if (allocated(error)) return
        call add_auto_mode("\.prog\'", "prog-mode")
        call syms_of_prog_mode()
        buffers(cur)%filename = "/tmp/foo.prog"
        call normal_mode(st)
        call check(error, buffers(cur)%major_mode, "prog-mode")
        if (allocated(error)) return
        buffers(cur)%filename = "/tmp/foo.other"
        call normal_mode(st)
        call check(error, buffers(cur)%major_mode, "fundamental-mode")
        deallocate (buffers(cur)%filename)
        call kill_buffer(a, st)
    end subroutine test_normal_mode
end module test_derived_fm
