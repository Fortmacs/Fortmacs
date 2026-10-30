! test_make_mode_fm.f90 -- tests for Makefile mode (make_mode_fm).
!
! Copyright (C) 2019-2026 Free Software Foundation, Inc.
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

!> Ports test/lisp/progmodes/make-mode-tests.el (make-mode-tests--bug17400),
!> plus tests of the keywords, motion, pickup, completion, backslashes,
!> filling, the save checks and auto-mode-alist.
module test_make_mode_fm
    use, intrinsic :: iso_fortran_env, only: int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_fm, only: status_type
    use handle_fm, only: handle_type
    use lisp_fm, only: value_type, nilp, consp, symbolp, symbols, xsymbol, xstring, list_elements, list_length, list_elt
    use lread_fm, only: sym
    use buffer_fm, only: buffers, cur, get_buffer_create, set_buffer, kill_buffer, erase_buffer
    use insdel_fm, only: insert
    use editfns_fm, only: point, point_min, point_max, goto_char, buffer_string
    use cmds_fm, only: forward_line, beginning_of_line, end_of_line
    use search_fm, only: search_forward, search_backward, re_search_backward
    use textprop_fm, only: get_text_property
    use bindings_fm, only: init_bindings
    use modes_fm, only: minor_modes
    use font_lock_fm, only: font_lock_ensure
    use files_fm, only: set_auto_mode
    use fill_fm, only: fill_paragraph
    use fileio_fm, only: make_temp_name
    use minibuf_table_fm, only: capf_data_type, COMPLETION_FINISHED
    use make_mode_fm, only: makefile_mode, makefile_gmake_mode, makefile_next_dependency, makefile_previous_dependency, &
                            makefile_pickup_targets, makefile_pickup_macros, makefile_target_table, makefile_macro_table, &
                            makefile_has_prereqs, makefile_name_type, makefile_insert_target, makefile_insert_macro, &
                            makefile_format_macro_ref, makefile_do_macro_insertion, makefile_backslash_region, &
                            makefile_completions_at_point, makefile_cleanup_continuations_function, &
                            makefile_cleanup_continuations, makefile_add_log_defun, makefile_use_curly_braces_for_macros_p, &
                            makefile_tab_after_target_colon
    implicit none(type, external)
    private

    public :: collect_make_mode_fm

    character(len=*), parameter :: LF = achar(10), TAB = achar(9)

    abstract interface
        subroutine mode_proc(status)
            import :: status_type
            implicit none(type, external)
            type(status_type), intent(out) :: status
        end subroutine mode_proc
    end interface

contains

    subroutine collect_make_mode_fm(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("make-mode-tests--bug17400", test_bug17400), &
                    new_unittest("make-mode-font-lock", test_font_lock), &
                    new_unittest("make-mode-gmake-font-lock", test_gmake_font_lock), &
                    new_unittest("make-mode-dependency-motion", test_motion), &
                    new_unittest("make-mode-pickup", test_pickup), &
                    new_unittest("make-mode-insert", test_insert), &
                    new_unittest("make-mode-completion", test_completion), &
                    new_unittest("make-mode-backslash-region", test_backslash), &
                    new_unittest("make-mode-fill-paragraph", test_fill), &
                    new_unittest("make-mode-cleanup-continuations", test_cleanup), &
                    new_unittest("make-mode-add-log-defun", test_add_log), &
                    new_unittest("make-mode-auto-mode-alist", test_auto_mode), &
                    new_unittest("make-temp-name", test_make_temp_name) &
                    ]
    end subroutine collect_make_mode_fm

    !> A fresh buffer holding TEXT (with-temp-buffer, insert).
    subroutine temp_buffer(text, h)
        character(len=*), intent(in) :: text
        type(handle_type), intent(out) :: h
        type(status_type) :: st

        call init_bindings()
        call get_buffer_create(" *make-test*", h, st)
        call set_buffer(h, st)
        call erase_buffer(st)
        call insert(text, st)
    end subroutine temp_buffer

    !> TEXT in a fresh buffer in MODE, fontified.
    subroutine fontified(text, mode, h)
        character(len=*), intent(in) :: text
        procedure(mode_proc) :: mode
        type(handle_type), intent(out) :: h
        type(status_type) :: st

        call temp_buffer(text, h)
        call mode(st)
        call minor_modes%font_lock_mode%enable(st)
        call font_lock_ensure(st)
    end subroutine fontified

    !> The face at POS (the first of a list), "" if none.
    function face_at(pos) result(name)
        integer(int64), intent(in) :: pos
        character(len=:), allocatable :: name
        type(value_type) :: v
        type(value_type), allocatable :: elts(:)
        type(status_type) :: st

        name = ""
        call get_text_property(pos, sym("face"), v, st)

        if (consp(v)) then
            call list_elements(v, elts)
            v = elts(1)
        end if

        if (nilp(v)) return
        if (symbolp(v)) name = symbols(xsymbol(v))%name
    end function face_at

    !> Point after (search-forward S).
    integer(int64) function after(s) result(pos)
        character(len=*), intent(in) :: s
        integer(int64) :: found
        type(status_type) :: st

        call search_forward(s, found, st)
        pos = point()
    end function after

    subroutine leave(h)
        type(handle_type), intent(in) :: h
        type(status_type) :: st

        call kill_buffer(h, st)
    end subroutine leave

    !> Whether NAME is in NAMES.
    logical function has(names, name)
        type(makefile_name_type), intent(in) :: names(:)
        character(len=*), intent(in) :: name
        integer :: k

        has = .false.

        do k = 1, size(names)
            if (names(k)%name == name .and. len(names(k)%name) == len(name)) has = .true.
        end do
    end function has

    subroutine test_bug17400(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        integer(int64) :: found, bar_pos, foo_pos
        type(status_type) :: st

        call fontified(LF//"foo:"//LF//TAB//"bar; : baz"//LF, makefile_mode, h)
        call re_search_backward("bar", found, st)
        bar_pos = point()
        call re_search_backward("foo", found, st)
        foo_pos = point()
        ! Make sure we don't confuse "bar;:" for a target!
        call check(error, face_at(bar_pos) /= face_at(foo_pos), "bar; has the face of a target")
        call leave(h)
    end subroutine test_bug17400

    subroutine test_font_lock(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        integer(int64) :: pos

        call fontified("CC = gcc"//LF//"all: main.o"//LF//TAB//"@$(CC) -o $@ main.o"//LF//"include foo.mk"//LF//" "//TAB// &
                       "x"//LF, makefile_mode, h)

        checks: block
            call check(error, face_at(1_int64), "font_lock_variable_name_face")
            if (allocated(error)) exit checks
            call goto_char(point_min())
            pos = after("all")
            call check(error, face_at(pos - 1), "makefile_targets")
            if (allocated(error)) exit checks
            pos = after("@")
            call check(error, face_at(pos - 1), "font_lock_type_face")
            if (allocated(error)) exit checks
            pos = after("$(C")
            call check(error, face_at(pos), "font_lock_variable_name_face")
            if (allocated(error)) exit checks
            pos = after("$@")
            call check(error, face_at(pos - 1), "font_lock_constant_face")
            if (allocated(error)) exit checks
            pos = after("include")
            call check(error, face_at(pos - 1), "font_lock_keyword_face")
            if (allocated(error)) exit checks
            call check(error, face_at(pos + 1), "font_lock_variable_name_face")
            if (allocated(error)) exit checks
            ! Spaces that precede a tab.
            pos = after(LF//" ")
            call check(error, face_at(pos - 1), "makefile_space")
        end block checks

        call leave(h)
    end subroutine test_font_lock

    subroutine test_gmake_font_lock(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        integer(int64) :: pos

        call fontified("ifneq ($(X),)"//LF//"Y := $(shell ls -l)"//LF//"endif"//LF, makefile_gmake_mode, h)

        checks: block
            call check(error, face_at(1_int64), "font_lock_keyword_face")
            if (allocated(error)) exit checks
            call check(error, face_at(3_int64), "font_lock_negation_char_face")
            if (allocated(error)) exit checks
            call goto_char(point_min())
            pos = after("shell")
            call check(error, face_at(pos - 1), "font_lock_function_name_face")
            if (allocated(error)) exit checks
            pos = after("ls")
            call check(error, face_at(pos - 1), "makefile_shell")
        end block checks

        call leave(h)
    end subroutine test_gmake_font_lock

    subroutine test_motion(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        logical :: found
        type(status_type) :: st

        call temp_buffer("X = 1"//LF//"all: a b"//LF//TAB//"echo x: y"//LF//LF//"clean:"//LF//TAB//"rm -f a"//LF, h)
        call makefile_mode(st)
        call goto_char(point_min())

        checks: block
            call makefile_next_dependency(found)
            call check(error, found .and. point() == 7, "first dependency")
            if (allocated(error)) exit checks
            call makefile_next_dependency(found)
            call check(error, found .and. point() == 28, "second dependency")
            if (allocated(error)) exit checks
            call makefile_next_dependency(found)
            call check(error,.not. found .and. point() == 28, "no more dependencies")
            if (allocated(error)) exit checks
            call makefile_previous_dependency(found)
            call check(error, found .and. point() == 7, "previous dependency")
            if (allocated(error)) exit checks
            call makefile_previous_dependency(found)
            call check(error,.not. found .and. point() == 7, "no previous dependency")
        end block checks

        call leave(h)
    end subroutine test_motion

    subroutine test_pickup(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(makefile_name_type), allocatable :: names(:)
        type(status_type) :: st

        call temp_buffer("CC = gcc"//LF//"export LDFLAGS := -s"//LF//"all: prog"//LF//"prog clean:"//LF, h)
        call makefile_mode(st)

        checks: block
            call makefile_pickup_macros(st)
            names = makefile_macro_table()
            call check(error, size(names) == 2 .and. has(names, "CC") .and. has(names, "LDFLAGS"), "macros")
            if (allocated(error)) exit checks
            ! Newest first.
            call check(error, names(1)%name, "LDFLAGS")
            if (allocated(error)) exit checks
            call makefile_pickup_targets(st)
            names = makefile_target_table()
            call check(error, size(names) == 3 .and. has(names, "all") .and. has(names, "prog") .and. has(names, "clean"), &
                       "targets")
            if (allocated(error)) exit checks
            names = makefile_has_prereqs()
            ! (A target followed by another one counts as having prerequisites.)
            call check(error, size(names) == 2 .and. has(names, "all") .and. has(names, "prog"), "prerequisites")
        end block checks

        call leave(h)
    end subroutine test_pickup

    subroutine test_insert(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(makefile_name_type), allocatable :: names(:)
        type(status_type) :: st

        call temp_buffer("", h)
        call makefile_mode(st)

        checks: block
            call makefile_insert_target("install", st)
            call check(error, buffer_string(), "install:"//TAB)
            if (allocated(error)) exit checks
            names = makefile_target_table()
            call check(error, has(names, "install"), "remembered target")
            if (allocated(error)) exit checks
            call insert(LF, st)
            call makefile_insert_macro("PREFIX", st)
            call check(error, buffer_string(), "install:"//TAB//LF//"PREFIX = ")
            if (allocated(error)) exit checks
            call check(error, makefile_format_macro_ref("CC"), "$(CC)")
            if (allocated(error)) exit checks
            call check(error, makefile_format_macro_ref("(CC)"), "$(CC)")
            if (allocated(error)) exit checks
            makefile_use_curly_braces_for_macros_p = .true.
            call check(error, makefile_format_macro_ref("CC"), "${CC}")
            makefile_use_curly_braces_for_macros_p = .false.
            if (allocated(error)) exit checks
            call erase_buffer(st)
            call makefile_do_macro_insertion("@", st)
            call makefile_do_macro_insertion("CC", st)
            call check(error, buffer_string(), "$@$(CC)")
            if (allocated(error)) exit checks
            call erase_buffer(st)
            makefile_tab_after_target_colon = .false.
            call makefile_insert_target("x", st)
            makefile_tab_after_target_colon = .true.
            call check(error, buffer_string(), "x: ")
        end block checks

        call leave(h)
    end subroutine test_insert

    subroutine test_completion(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(capf_data_type) :: data
        type(status_type) :: st

        call temp_buffer("CC = gcc"//LF//"all: prog"//LF//"x: $(C", h)
        call makefile_mode(st)
        call makefile_pickup_macros(st)
        call makefile_pickup_targets(st)

        checks: block
            call goto_char(point_max())
            call makefile_completions_at_point(data, st)
            call check(error, data%found .and. data%start == point_max() - 1 .and. data%end == point_max(), "bounds")
            if (allocated(error)) exit checks
            ! Macros only after "$(".
            call check(error, list_length(data%table%list) == 1, "macros only")
            if (allocated(error)) exit checks
            call check(error, xstring(list_elt(data%table%list, 1_int64)), "CC")
            if (allocated(error)) exit checks
            ! The exit function closes the reference.
            call insert("C", st)
            call data%exit_function("CC", COMPLETION_FINISHED)
            call check(error, buffer_string(), "CC = gcc"//LF//"all: prog"//LF//"x: $(CC)")
            if (allocated(error)) exit checks
            ! Targets and macros elsewhere.
            call insert(" p", st)
            call makefile_completions_at_point(data, st)
            call check(error, list_length(data%table%list) == 3, "targets and macros")
        end block checks

        call leave(h)
    end subroutine test_completion

    subroutine test_backslash(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        character(len=:), allocatable :: expected
        type(status_type) :: st

        call temp_buffer("SRC = a.c"//LF//"  b.c"//LF//"  c.c"//LF, h)
        call makefile_mode(st)

        checks: block
            call makefile_backslash_region(point_min(), point_max(), .false., st)
            expected = "SRC = a.c"//repeat(TAB, 5)//"\"//LF//"  b.c"//repeat(TAB, 6)//"\"//LF//"  c.c"//LF
            call check(error, buffer_string(), expected)
            if (allocated(error)) exit checks
            call makefile_backslash_region(point_min(), point_max(), .true., st)
            call check(error, buffer_string(), "SRC = a.c"//LF//"  b.c"//LF//"  c.c"//LF)
        end block checks

        call leave(h)
    end subroutine test_backslash

    subroutine test_fill(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        character(len=:), allocatable :: text
        integer :: k
        type(status_type) :: st

        text = "OBJS ="

        do k = 1, 15
            text = text//" file"//achar(iachar("a") + k - 1)//".o"
        end do

        call temp_buffer(text//LF, h)
        call makefile_mode(st)
        call goto_char(point_min())
        call fill_paragraph(st)

        checks: block
            call check(error, st%ok(), "fill_paragraph failed")
            if (allocated(error)) exit checks
            ! The assignment is filled into backslashed lines.
            call goto_char(point_min())
            call end_of_line()
            call check(error, buffer_string() /= text//LF, "not filled")
            if (allocated(error)) exit checks
            call check(error, index(buffer_string(), "\"//LF) > 0, "no backslashes")
        end block checks

        call leave(h)
    end subroutine test_fill

    subroutine test_cleanup(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        logical :: done
        type(status_type) :: st

        call temp_buffer("A = x \  "//LF//"  y"//LF, h)
        call makefile_mode(st)

        checks: block
            call makefile_cleanup_continuations_function(done, st)
            call check(error, buffer_string(), "A = x \  "//LF//"  y"//LF)
            if (allocated(error)) exit checks
            makefile_cleanup_continuations = .true.
            call makefile_cleanup_continuations_function(done, st)
            makefile_cleanup_continuations = .false.
            call check(error,.not. done, "the save goes on")
            if (allocated(error)) exit checks
            call check(error, buffer_string(), "A = x \"//LF//"  y"//LF)
        end block checks

        call leave(h)
    end subroutine test_cleanup

    subroutine test_add_log(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        character(len=:), allocatable :: name
        integer(int64) :: found
        type(status_type) :: st

        call temp_buffer("CFLAGS = -O2 \"//LF//"  -g"//LF//LF//"all : prog"//LF//TAB//"cc -o prog"//LF//LF//"# x"//LF, h)
        call makefile_mode(st)

        checks: block
            call goto_char(point_min())
            call search_forward("-g", found, st)
            call makefile_add_log_defun(name)
            call check(error, allocated(name), "variable not found")
            if (allocated(error)) exit checks
            call check(error, name, "CFLAGS")
            if (allocated(error)) exit checks
            call search_forward("cc -o", found, st)
            call makefile_add_log_defun(name)
            call check(error, allocated(name), "target not found")
            if (allocated(error)) exit checks
            call check(error, name, "all")
            if (allocated(error)) exit checks
            call goto_char(point_max())
            call makefile_add_log_defun(name)
            call check(error,.not. allocated(name), "found in a comment")
        end block checks

        call leave(h)
    end subroutine test_add_log

    subroutine test_auto_mode(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call temp_buffer("", h)

        checks: block
            buffers(cur)%filename = "/tmp/x/Makefile"
            call set_auto_mode(st)
            call check(error, buffers(cur)%major_mode, "makefile_gmake_mode")
            if (allocated(error)) exit checks
            buffers(cur)%filename = "/tmp/x/Makefile.am"
            call set_auto_mode(st)
            call check(error, buffers(cur)%major_mode, "makefile_automake_mode")
            if (allocated(error)) exit checks
            buffers(cur)%filename = "/tmp/x/Imakefile"
            call set_auto_mode(st)
            call check(error, buffers(cur)%major_mode, "makefile_imake_mode")
            if (allocated(error)) exit checks
            buffers(cur)%filename = "/tmp/x/Makeppfile.mk"
            call set_auto_mode(st)
            call check(error, buffers(cur)%major_mode, "makefile_makepp_mode")
            if (allocated(error)) exit checks
            buffers(cur)%filename = "/tmp/x/rules.mk"
            call set_auto_mode(st)
            call check(error, buffers(cur)%major_mode, "makefile_gmake_mode")
        end block checks

        deallocate (buffers(cur)%filename)
        call leave(h)
    end subroutine test_auto_mode

    subroutine test_make_temp_name(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: name
        type(status_type) :: st

        call make_temp_name("/tmp/mktmp.", name, st)

        checks: block
            call check(error, st%ok(), "make_temp_name failed")
            if (allocated(error)) exit checks
            call check(error, len(name) == len("/tmp/mktmp.") + 6, "length")
            if (allocated(error)) exit checks
            call check(error, name(1:11), "/tmp/mktmp.")
        end block checks
    end subroutine test_make_temp_name
end module test_make_mode_fm
