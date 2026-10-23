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
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_fm, only: status_type
    use handle_fm, only: handle_type, null_handle
    use lisp_fm, only: value_type, nil
    use lread_fm, only: sym
    use buffer_fm, only: get_buffer_create, kill_buffer, set_buffer, buffers, cur, fill_column
    use data_fm, only: value_eq
    use eval_fm, only: hook_type
    use subr_fm, only: after_change_major_mode_hook
    use keymap_fm, only: current_local_map, keymap_parent, keymap_set, key_binding, key_parse
    use derived_fm, only: major_mode_type, derived_mode_p, provided_mode_derived_p
    use modes_fm, only: major_modes
    use syntax_fm, only: syntax_table, standard_syntax_table, char_syntax
    use text_mode_fm, only: text_mode, syms_of_text_mode, text_mode_syntax_table, text_mode_variant, text_mode_hook
    use prog_mode_fm, only: syms_of_prog_mode
    use simple_fm, only: fundamental_mode, special_mode, syms_of_simple_modes
    use files_fm, only: normal_mode, auto_mode_spec, file_name_sans_versions, add_auto_mode, require_final_newline, &
                        mode_require_final_newline, FINAL_NEWLINE_SAVE
    implicit none(type, external)
    private

    public :: collect_derived_fm

    !> The hook functions that ran, in order.
    character(len=:), allocatable :: trace

    !> Hooks of the hook tests.
    type(hook_type), target :: subr_tests_hook, test_hook, test_hook2, child_mode_hook, depth_hook

    !> Modes of the derived-mode tests.
    type(major_mode_type), target :: child_mode_obj, parent_mode_obj, derived_mode_1_obj, derived_mode_2_obj

contains

    subroutine collect_derived_fm(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)
        testsuite = [ &
                    new_unittest("subr-tests-add-hook-depth", test_add_hook_depth), &
                    new_unittest("add-hook-remembers-depth", test_depth_memory), &
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

    subroutine f1(status)
        type(status_type), intent(out) :: status
        status = status_type()
        trace = trace//" f1"
    end subroutine f1

    subroutine f2(status)
        type(status_type), intent(out) :: status
        status = status_type()
        trace = trace//" f2"
    end subroutine f2

    subroutine f3(status)
        type(status_type), intent(out) :: status
        status = status_type()
        trace = trace//" f3"
    end subroutine f3

    subroutine f4(status)
        type(status_type), intent(out) :: status
        status = status_type()
        trace = trace//" f4"
    end subroutine f4

    subroutine f5(status)
        type(status_type), intent(out) :: status
        status = status_type()
        trace = trace//" f5"
    end subroutine f5

    subroutine f6(status)
        type(status_type), intent(out) :: status
        status = status_type()
        trace = trace//" f6"
    end subroutine f6

    subroutine f7(status)
        type(status_type), intent(out) :: status
        status = status_type()
        trace = trace//" f7"
    end subroutine f7

    subroutine f8(status)
        type(status_type), intent(out) :: status
        status = status_type()
        trace = trace//" f8"
    end subroutine f8

    subroutine f9(status)
        type(status_type), intent(out) :: status
        status = status_type()
        trace = trace//" f9"
    end subroutine f9

    subroutine f10(status)
        type(status_type), intent(out) :: status
        status = status_type()
        trace = trace//" f10"
    end subroutine f10

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
        call child_mode_obj%begin(saved, status)
        if (.not. status%ok()) return
        trace = trace//"c"
        call child_mode_obj%finish(saved, status)
    end subroutine child_mode

    !> A mode with an empty body (the provided-mode-derived-p modes).
    recursive subroutine empty_mode(status)
        type(status_type), intent(out) :: status
        status = status_type()
    end subroutine empty_mode

    !> The functions of HOOK in the order they run, by name.
    function hook_names(hook) result(s)
        type(hook_type), intent(in) :: hook
        character(len=:), allocatable :: s
        type(status_type) :: st
        trace = ""
        call hook%run(st)
        s = adjustl(trace)
        s = trim(s)
    end function hook_names

    !> subr-tests.el: subr-tests-add-hook-depth.
    subroutine test_add_hook_depth(error)
        type(error_type), allocatable, intent(out) :: error
        call subr_tests_hook%add(f1)
        call subr_tests_hook%add(f2)
        call check(error, hook_names(subr_tests_hook), "f2 f1")
        if (allocated(error)) return
        call subr_tests_hook%add(f3, depth=90)
        call check(error, hook_names(subr_tests_hook), "f2 f1 f3")
        if (allocated(error)) return
        call subr_tests_hook%add(f4, depth=50)
        call check(error, hook_names(subr_tests_hook), "f2 f1 f4 f3")
        if (allocated(error)) return
        call subr_tests_hook%add(f5, depth=-50)
        call check(error, hook_names(subr_tests_hook), "f5 f2 f1 f4 f3")
        if (allocated(error)) return
        call subr_tests_hook%add(f6)
        call check(error, hook_names(subr_tests_hook), "f5 f6 f2 f1 f4 f3")
        if (allocated(error)) return
        ! Make sure t is equivalent to 90.
        call subr_tests_hook%add(f7, depth=90)
        call subr_tests_hook%add(f8, depth=90)
        call check(error, hook_names(subr_tests_hook), "f5 f6 f2 f1 f4 f3 f7 f8")
        if (allocated(error)) return
        ! Make sure nil is equivalent to 0.
        call subr_tests_hook%add(f9, depth=0)
        call subr_tests_hook%add(f10)
        call check(error, hook_names(subr_tests_hook), "f5 f10 f9 f6 f2 f1 f4 f3 f7 f8")
    end subroutine test_add_hook_depth

    !> subr.el keeps a function's depth (hook--depth-alist) after it is
    !> removed: added again without a depth, it goes back there.
    subroutine test_depth_memory(error)
        type(error_type), allocatable, intent(out) :: error
        call depth_hook%add(f1, depth=50)
        call depth_hook%add(f2)
        call check(error, hook_names(depth_hook), "f2 f1")
        if (allocated(error)) return
        call depth_hook%remove(f1)
        call depth_hook%add(f3)
        call depth_hook%add(f1)
        call check(error, hook_names(depth_hook), "f3 f2 f1")
    end subroutine test_depth_memory

    !> A buffer's own functions run first, then the global ones in the
    !> place of `t`.
    subroutine test_run_hooks_local(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: a, b
        type(status_type) :: st
        call get_buffer_create("test-derived-hooks-A", a, st)
        call get_buffer_create("test-derived-hooks-B", b, st)
        call set_buffer(a, st)
        call test_hook%add(f1)
        call test_hook%add(f2, local=.true.)
        call check(error, test_hook%local_p(), "local hook")
        if (allocated(error)) return
        call check(error, hook_names(test_hook), "f2 f1", "local, then global")
        if (allocated(error)) return
        call set_buffer(b, st)
        call check(error, hook_names(test_hook), "f1", "global only in B")
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
        call test_hook2%add(f1)
        call test_hook2%add(f2, local=.true.)
        call test_hook2%remove(f2, local=.true.)
        call check(error,.not. test_hook2%local_p(), "local value killed")
        if (allocated(error)) return
        call check(error, hook_names(test_hook2), "f1")
        if (allocated(error)) return
        call test_hook2%remove(f1)
        call check(error, hook_names(test_hook2), "")
        call kill_buffer(a, st)
    end subroutine test_remove_hook

    !> text-mode sets the mode, its keymap, syntax table and variables.
    subroutine test_text_mode(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: a
        type(status_type) :: st
        call syms_of_text_mode()
        call get_buffer_create("test-derived-text", a, st)
        call set_buffer(a, st)
        call text_mode(st)
        call check(error, st%ok(), "text_mode")
        if (allocated(error)) return
        call check(error, buffers(cur)%major_mode, "text_mode")
        if (allocated(error)) return
        call check(error, buffers(cur)%mode_name, "Text")
        if (allocated(error)) return
        call check(error, same(current_local_map(), major_modes%text_mode%map), "text_mode_map")
        if (allocated(error)) return
        call check(error, same(syntax_table(), text_mode_syntax_table), "syntax table")
        if (allocated(error)) return
        call check(error, char_syntax(iachar("'")) == "w", "' is a word constituent")
        if (allocated(error)) return
        call check(error, derived_mode_p(major_modes%text_mode), "derived_mode_p")
        if (allocated(error)) return
        call check(error, text_mode_variant%value .and. text_mode_variant%local_p(), "text_mode_variant")
        if (allocated(error)) return
        call check(error, require_final_newline%value == FINAL_NEWLINE_SAVE .and. require_final_newline%local_p(), &
                   "require_final_newline")
        if (allocated(error)) return
        call fundamental_mode(st)
        call check(error, same(syntax_table(), standard_syntax_table()), "standard syntax table")
        if (allocated(error)) return
        call check(error, same(current_local_map(), null_handle()), "no local map")
        if (allocated(error)) return
        call check(error,.not. text_mode_variant%value .and. .not. require_final_newline%local_p(), "locals killed")
        if (allocated(error)) return
        call check(error, mode_require_final_newline%value == FINAL_NEWLINE_SAVE, "mode_require_final_newline")
        call kill_buffer(a, st)
    end subroutine test_text_mode

    !> A derived mode runs its parent's hook, then its own, then
    !> after-change-major-mode-hook, all after the bodies.
    subroutine test_hooks_order(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: a
        type(status_type) :: st
        type(value_type) :: binding
        type(value_type), allocatable :: key(:)
        call syms_of_text_mode()
        call child_mode_obj%define("test-derived-child-mode", "Child", child_mode, child_mode_hook, &
                                   parent=major_modes%text_mode)
        call text_mode_hook%add(hook_text)
        call child_mode_hook%add(hook_child)
        call after_change_major_mode_hook%add(hook_after)
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
        call check(error, same(keymap_parent(child_mode_obj%map), major_modes%text_mode%map), "keymap parent")
        if (allocated(error)) return
        call check(error, derived_mode_p(major_modes%text_mode), "derived from text-mode")
        if (allocated(error)) return
        ! A key bound in text-mode-map is active in the child.
        call keymap_set(major_modes%text_mode%map, "C-c t", "forward_char", st)
        call key_parse("C-c t", key, st)
        call key_binding(key, binding, st)
        call check(error, value_eq(binding, sym("forward_char")), "inherited binding")
        call keymap_set(major_modes%text_mode%map, "C-c t", nil(), st)
        call text_mode_hook%remove(hook_text)
        call child_mode_hook%remove(hook_child)
        call after_change_major_mode_hook%remove(hook_after)
        call kill_buffer(a, st)
    end subroutine test_hooks_order

    !> subr-tests.el: provided-mode-derived-p (without aliases).
    subroutine test_provided_mode_derived_p(error)
        type(error_type), allocatable, intent(out) :: error
        call syms_of_prog_mode()
        call parent_mode_obj%define("subr-tests--parent-mode", "P", empty_mode, parent=major_modes%prog_mode)
        call derived_mode_1_obj%define("subr-tests--derived-mode-1", "D1", empty_mode, parent=major_modes%prog_mode)
        call derived_mode_2_obj%define("subr-tests--derived-mode-2", "D2", empty_mode, parent=parent_mode_obj)
        call check(error, provided_mode_derived_p("subr-tests--derived-mode-1", [character(len=9) :: "prog_mode"]))
        if (allocated(error)) return
        call check(error, provided_mode_derived_p("subr-tests--parent-mode", &
                                                  [character(len=23) :: "subr-tests--parent-mode"]))
        if (allocated(error)) return
        call check(error, provided_mode_derived_p("subr-tests--derived-mode-2", &
                                                  [character(len=23) :: "subr-tests--parent-mode"]))
        if (allocated(error)) return
        call check(error, provided_mode_derived_p("subr-tests--derived-mode-2", [character(len=9) :: "prog_mode"]))
        if (allocated(error)) return
        ! define-derived-mode takes fundamental-mode as no parent.
        call check(error,.not. provided_mode_derived_p("prog_mode", [character(len=16) :: "fundamental_mode"]))
        if (allocated(error)) return
        call check(error,.not. provided_mode_derived_p("prog_mode", [character(len=9) :: "text_mode"]))
        if (allocated(error)) return
        call check(error, derived_mode_2_obj%derived_from(major_modes%prog_mode), "derived_from")
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
        call check(error, st%ok(), "special_mode")
        if (allocated(error)) return
        call check(error, buffers(cur)%read_only, "read_only")
        if (allocated(error)) return
        call key_parse("q", key, st)
        call key_binding(key, binding, st)
        call check(error, value_eq(binding, sym("quit_window")), "q quits")
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
        call fill_column%set_local(60)
        call check(error, fill_column%local_p() .and. fill_column%value == 60, "local value")
        if (allocated(error)) return
        call fundamental_mode(st)
        call check(error,.not. fill_column%local_p() .and. fill_column%value == 70, "local killed")
        if (allocated(error)) return
        call check(error, buffers(cur)%major_mode, "fundamental_mode")
        call kill_buffer(a, st)
    end subroutine test_fundamental

    !> The -*- line.
    subroutine test_auto_mode_spec(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), parameter :: LF = char(10)
        call check(error, auto_mode_spec("-*- Text -*-"//LF//"body"), "text_mode")
        if (allocated(error)) return
        call check(error, auto_mode_spec(LF//"  ;; -*- mode: Prog; fill-column: 60 -*-"), "prog_mode")
        if (allocated(error)) return
        call check(error, auto_mode_spec("x -*- coding: utf-8; Mode: text -*-"), "text_mode")
        if (allocated(error)) return
        call check(error, auto_mode_spec("#!/bin/sh"//LF//"# -*- text -*-"), "text_mode")
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
        call get_buffer_create("test-derived-normal", a, st)
        call set_buffer(a, st)
        buffers(cur)%filename = "/tmp/foo.text"
        call normal_mode(st)
        call check(error, derived_mode_p(major_modes%text_mode), "text-mode from the file name")
        if (allocated(error)) return
        call text_mode_hook%add(hook_text)
        trace = ""
        buffers(cur)%filename = "/tmp/foo.TXT"
        call normal_mode(st)
        call text_mode_hook%remove(hook_text)
        call check(error, trace, "T", "hook ran (case-insensitive match)")
        if (allocated(error)) return
        call add_auto_mode("\.prog\'", "prog_mode")
        call syms_of_prog_mode()
        buffers(cur)%filename = "/tmp/foo.prog"
        call normal_mode(st)
        call check(error, buffers(cur)%major_mode, "prog_mode")
        if (allocated(error)) return
        buffers(cur)%filename = "/tmp/foo.other"
        call normal_mode(st)
        call check(error, buffers(cur)%major_mode, "fundamental_mode")
        deallocate (buffers(cur)%filename)
        call kill_buffer(a, st)
    end subroutine test_normal_mode
end module test_derived_fm
